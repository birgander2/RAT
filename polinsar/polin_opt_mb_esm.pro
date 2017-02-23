;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_opt_mb_esm
; written by       : Maxim Neumann
; last revision    : 08/2006
; Simultaneously Multibaseline Coherence Optimization (Multiple NR's)
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


pro polin_opt_mb_esm,CALLED = called, sm_file=sm_file, SMMX = smmx, SMMY = smmy
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 514) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,matrix=is_matrix

  if ~is_matrix then begin
     polin_k2m,/called,/gui,smmx=sx,smmy=sy ;; reform to matrix
  endif
  newtype = 532L

  if n_elements(smmx) eq 0 then smmx = is_matrix? 1: 7  ; Default values
  if n_elements(smmy) eq 0 then smmy = is_matrix? 1: 7  ; Default values

  SMs = n_elements(sm_file) ne 0 
  if n_elements(sm_file) eq 0 then sm_file = file_dirname(file.name,/M)+'sm_mb_esm_'+file_basename(file.name)

  if ~keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4, $
                        TITLE='Coherence Optimization',/modal,/tlb_kill_request_events,/tlb_frame_attr)
     field1 = CW_FIELD(main,VALUE=smmx,/integer, $
                       TITLE='Filter boxsize X   : ',XSIZE=3)
     field2 = CW_FIELD(main,VALUE=smmy,/integer, $
                       TITLE='Filter boxsize Y   : ',XSIZE=3)
     main2 = widget_base(main,/column,/frame)
     box1  = cw_bgroup(main2," save optimal scattering mechanism vectors",/nonexclusive,set_value=sms)
     line1 = WIDGET_BASE(main2,column=2,sensitive=sms)
     text1 = CW_FIELD(line1,VALUE=sm_file,/string,XSIZE=60,TITLE='Target name :');,noedit=~sms)
     brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)
;     cohcompl=cw_bgroup(main," complex coherence",/nonexclusive,set_value=coh_complex)
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
           infotext = ['MULTIBASELINE COHERENCE OPTIMIZATION',$
                       ' ',$
                       'RAT module written by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Information')
        endif
        widget_control,box1,get_value=sms
        widget_control,line1,sensitive=sms
;           widget_control,line4,sensitive=1-random
        if event.id eq brow1 then begin ; Info Button clicked
           path = config.workdir
           sm_file = DIALOG_PICKFILE(TITLE='Provide target file for scattering mechanisms',DIALOG_PARENT=wid.base, $
                                     FILTER = '*.rat',PATH=path, GET_PATH=path)
           if strlen(sm_file) gt 0 then begin
              config.workdir = path
              widget_control,text1,set_value=sm_file
           endif
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,field1,GET_VALUE=smmx ; read widget fields
     widget_control,field2,GET_VALUE=smmy
     widget_control,text1,get_value=sm_file
 ;    widget_control,cohcompl,GET_VALUE=coh_complex
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

  if smmx gt 1 || smmy gt 1 then $
     speck_polmean, /CALLED, SMMX = smmx, SMMY = smmy

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [4L,pol,n_bl,head[3:*]]
  srat,outputfile,eee,header=head,info=info,type=newtype

  if sms then begin
     smtype = 535L
     smhead = [4L,pol,pol,head[3:*]]
     srat,sm_file,fff,header=smhead,info=info,type=smtype
  endif

; calculating preview size and number of blocks
  bs = config.blocksize / file.vdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Multibaseline ESM coherence optimization...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                        ,type=file.var,/nozero)
     readu,ddd,block

; -------- THE FILTER ----------
     block = mb_opt_nr(block,sm=sm, pol=pol, tracks_nr=n_tr, bl_nr=n_bl)
; -------- THE FILTER ----------
     writeu,eee, block
     if sms then writeu,fff,sm
  endfor
  free_lun,ddd,eee
  if sms then free_lun,fff

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.zdim = n_bl
  file.vdim = pol
  file.type = newtype

  evolute,'Multibaseline coherence optimization with ESM (MB-ESM).'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
