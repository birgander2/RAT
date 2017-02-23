;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: subap_nonstat
; written by       : Maxim Neumann
; last revision    : 23. September 2005
; (Non-) Stationarity analysis of subapertures. (log(L))
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

pro subap_nonstat,CALLED=called
  common rat, types, file, wid, config

; check if array is usable - get new type
  if file.type eq 300L || $
     (file.type ge 200L && file.type le 210L) || $
     (file.type ge 500L && file.type le 501L) || $
     (file.type ge 601L && file.type le 605L) then begin
     error = DIALOG_MESSAGE(["This is wrong data type. ", $
                             "Needs "+types[610L],' ', $
                             "Proceed automatically with generation?"], $
                            DIALOG_PARENT = wid.base, TITLE='Proceed with covariance matrices generation?',/question)
     if error eq 'Yes' then subap_k2m,SMMX=smmx,SMMY=smmy $
     else return
  endif

; check if array is usable - get new type
  if file.type eq 610L then newtype = 630L $
  else begin
     error_button = DIALOG_MESSAGE(['Data has to be',types[610L]] $
                                   , DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endelse

  if n_elements(smmx) gt 0 && n_elements(smmy) gt 0 then $
     nrL = smmx*smmy $
  else nrL = 2*4

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Stationarity analysis.',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     field1 = CW_FIELD(main,VALUE=nrL,/integer,TITLE='Number of looks: ',XSIZE=3)
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
           infotext = ['SUBAPERTURE STATIONARITY ANALYSIS',$
                       ' ',$
                       'RAT module written 09/2005 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,field1,GET_VALUE=nrl ; read widget fields
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; read / write header
  newvar = 4L
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=[2L,file.xdim,file.ydim,newvar],info=info,type=newtype
; pop up progress window

  progress,Message='Subapertures stationarity analysis...',/cancel_button

;;; filter pre-calculations
  R  = file.zdim
  p  = sqrt(file.vdim)
  nt = R*nrL

  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block

     block  = reform(block,[p,p,R,file.xdim,blocksizes[i]],/overwrite)
     d     = REAL_PART(block_det(block))
     Tt    = total(block,3)/R   ; mean cov-matrices
     dt    = REAL_PART(block_det(Tt))
     d     = reform(d,/overwrite) & dt = reform(dt,/overwrite)
     oblock= nrL*total(alog(d),1) - nt*alog(dt) ; log(L)

     writeu,eee,oblock
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.type = newtype
  file.var  = newvar
  file.dim  = 2L
  file.vdim = 1
  file.zdim = 1

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy
end
