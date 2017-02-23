;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_coh_mean
; written by       : Maxim Neumann
; last revision    : 08/2006
; Multibaseline coherence calculation using boxcar filter
;------------------------------------------------------------------------
; The contents of this file are subject to the Mozilla Public License
; Version 1.1 (the "License"); you may not use this file except in
; compliance with the License. You may obtain a copy of the License at
; http://www.mozilla.org/MPL/
;
; Software distributed under the License is distributed on an "AS IS"
; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
; License for the specific language governing rights and limitations
; under the License.
;
; The Initial Developer of the Original Code is the RAT development team.
; All Rights Reserved.
;------------------------------------------------------------------------

pro polin_coh_mean,CALLED = called, SMMX = smmx, SMMY = smmy, coh_complex=coh_complex
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 514) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,matrix=matrix

  if n_elements(smmx) eq 0 then smmx = matrix? 1: 7  ; Default values
  if n_elements(smmy) eq 0 then smmy = matrix? 1: 7  ; Default values
  if n_elements(coh_complex) eq 0 then coh_complex=1 ; Default values


  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4, $
                        TITLE='Coherence using boxcar filter',/modal,/tlb_kill_request_events,/tlb_frame_attr)
     field1 = CW_FIELD(main,VALUE=smmx,/integer, $
                       TITLE='Filter boxsize X   : ',XSIZE=3)
     field2 = CW_FIELD(main,VALUE=smmy,/integer, $
                       TITLE='Filter boxsize Y   : ',XSIZE=3)
     cohcompl=cw_bgroup(main," complex coherence",/nonexclusive,set_value=coh_complex)
     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main,/REALIZE,default_button=but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['COHERENCE ESTIMATION USING BOXCAR FILTER',$
                       ' ',$
                       'RAT module written by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,field1,GET_VALUE=smmx ; read widget fields
     widget_control,field2,GET_VALUE=smmy
     widget_control,cohcompl,GET_VALUE=coh_complex
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; Error Handling
  if smmx le 0 or smmy le 0 then begin ; Wrong box sizes ?
     error = DIALOG_MESSAGE("Boxsizes has to be >= 1", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

  newtype = 530L ; only one coherence type for mb-sar! (complex & float)
  newvar  = coh_complex? 6L: 4L

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [4L,pol,n_bl,file.xdim,file.ydim,newvar]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  overlap = smmy / 2
  calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last
  ypos1 = 0                     ; block start
  ypos2 = bs - overlap          ; block end

;smooth box
  smm_box  = [1,1,smmx,smmy]
  byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos

; pop up progress window
  progress,Message='MB coherence estimation...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                        ,type=file.var,/nozero)
     oblock= make_array([pol,n_bl,file.xdim,blocksizes[i]],type=newvar,/nozero)
     readu,ddd,block
; -------- THE FILTER ----------
     NaN = (where(~finite(block)))[0] ne -1
     if ~matrix then $
        for bl=0,n_bl-1 do begin
        tracks = mb_ind(bl)
        coh = smooth(block[*,tracks[0],*,*]*conj(block[*,tracks[1],*,*]),smm_box,/ed, NaN=NaN)/ $
              sqrt(smooth(abs(block[*,tracks[0],*,*])^2,smm_box,/edge_truncate, NaN=NaN)* $
                   smooth(abs(block[*,tracks[1],*,*])^2,smm_box,/edge_truncate, NaN=NaN))
        oblock[*,bl,*,*]=coh_complex? coh: abs(coh)
     endfor

     if matrix then $
        for bl=0,n_bl-1 do begin
        tracks = mb_ind(bl)
        coh = smooth(mm_diag(block[tracks[0]*pol:(tracks[0]+1)*pol-1,tracks[1]*pol:(tracks[1]+1)*pol-1,*,*]),smm_box[1:*],/edge_truncate, NaN=NaN)/ $
              sqrt(smooth(mm_diag(block[tracks[0]*pol:(tracks[0]+1)*pol-1,tracks[0]*pol:(tracks[0]+1)*pol-1,*,*]),smm_box[1:*],/edge_truncate, NaN=NaN)* $
                   smooth(mm_diag(block[tracks[1]*pol:(tracks[1]+1)*pol-1,tracks[1]*pol:(tracks[1]+1)*pol-1,*,*]),smm_box[1:*],/edge_truncate, NaN=NaN))
        oblock[*,bl,*,*]=coh_complex? coh: abs(coh)
     endfor
; -------- THE FILTER ----------
     if i eq anz_blocks-1 then ypos2 = bs_last

     writeu,eee,oblock[*,*,*,ypos1:ypos2-1]
     ypos1 = overlap
     point_lun,-ddd,file_pos
     point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim $
               * file.xdim * byt[file.var]
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = 4L
  file.vdim = pol
  file.zdim = n_bl
  file.var  = newvar
  file.type = newtype

  evolute,'Multibaseline coherence estimation. Boxcar: '+strcompress(smmx,/R)+'x'+strcompress(smmy,/R)

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
