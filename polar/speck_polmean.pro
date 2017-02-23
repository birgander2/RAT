;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_polmean
; last revision : 17.Mar.2003
; written by    : Andreas Reigber
; Boxcar (Mean) filter for polarimetric data (+POLInSAR +POLIN)
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



pro speck_polmean,CALLED = called, GUI=GUI, SMMX = smmx, SMMY = smmy
  common rat, types, file, wid, config
;  
  if ~((file.type ge 200 and file.type lt 210) or (file.type ge 220 and file.type le 230) or $
       (file.type ge 500 and file.type le 510) or (file.type ge 510 and file.type le 514)) then begin
     error_button = DIALOG_MESSAGE(['Data have to be in','vector or matrix format'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

; start speckle filtering
  if n_elements(smmx) eq 0 then smmx=3
  if n_elements(smmy) eq 0 then smmy=3

  if ~keyword_set(called) || keyword_set(GUI) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Polarimetric Boxcar Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     field1 = CW_FIELD(main,VALUE=smmx,/integer,TITLE='Filter boxsize X   : ',XSIZE=3)
     field2 = CW_FIELD(main,VALUE=smmy,/integer,TITLE='Filter boxsize Y   : ',XSIZE=3)
     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['POLARIMETRIC BOXCAR FILTER',$
                       ' ',$
                       'RAT module written 2003 by Andreas Reigber']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,field1,GET_VALUE=smmx ; read widget fields
     widget_control,field2,GET_VALUE=smmy
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; Error Handling
  if (smmx le 0 or smmy le 0) and not keyword_set(called) then begin ; Wrong box sizes ?
     error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  
; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; transform polarimetric vector to matrix
  if (file.type ge 200 and file.type le 210) || (file.type ge 500 and file.type lt 510) then begin
     if not keyword_set(called) then $
        dummy = DIALOG_MESSAGE(["Data are in vector form. They have", $
                                "to be converted to matrix form first."], $
                               DIALOG_PARENT = wid.base, /information)
     if ~(file.type ge 500 && file.type le 503) then $
        k_to_m,/called $
     else polin_k2m,/called
     if wid.cancel eq 1 then return
  endif

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=head,info=info,type=type	
  
; calculating preview size and number of blocks
  
  bs = config.blocksize
  overlap = (smmy + 1) / 2
  calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

  ypos1 = 0                     ; block start
  ypos2 = bs - overlap          ; block end

  byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos

;smooth box

  smm_box = [1,1,smmx,smmy]

; pop up progress window

  progress,Message='Boxcar Polarimetric Speckle Filter...',/cancel_button

;start block processing

  for i=0,anz_blocks-1 do begin   
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block
; -------- THE FILTER ----------
     block = smooth(block,smm_box,/edge_truncate, /nan)
; -------- THE FILTER ----------
     if i eq anz_blocks-1 then ypos2 = bs_last
     writeu,eee,block[*,*,*,ypos1:ypos2-1]
     ypos1 = overlap
     point_lun,-ddd,file_pos
     point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
  endfor
  free_lun,ddd,eee

; update file information
  
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  
  evolute,'Speckle filtering (Polarimetric Boxcar): '+strcompress(smmx,/R)+'x'+strcompress(smmy,/R)

; generate preview

  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy
end
