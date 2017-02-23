;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calib_xsym
; written by    : Andreas Reigber
; last revision : 26. April 2004
; modified      : extended to polinsar & polin (mn,08/2006)
; Symmetrisation of cross-polar components
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

pro calib_xsym,CALLED=called,METHOD = method
  common rat, types, file, wid, config
  compile_opt idl2

; check if array is usable - get new type
  type_all   = [200L,210,220,221,500,501,510,511]
  type_polin = type_all[where(type_all/100L eq 5)]
  type_matrix= [220L,221,510,511]
  type_pauli = [210L,221,501,511]

  polin   = total(type_polin  eq file.type) ne 0
  matrix  = total(type_matrix eq file.type) ne 0
  pauli   = total(type_pauli  eq file.type) ne 0

  if total(type_all eq file.type) eq 0 then begin
     error_button = DIALOG_MESSAGE(['Data have to be in ','HV basis'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif
  if (polin && file.vdim mod 4 ne 0) || (~polin && file.zdim mod 4 ne 0) then begin
     error_button = DIALOG_MESSAGE(['Scattering vector with','4 elements required'] $
                                   , DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif


;   if file.type eq 200L || file.type eq 210L || file.type eq 220L || file.type eq 221L then $
;      polin = 0 $
;   else if file.type eq 500L || file.type eq 501L || file.type eq 510L || file.type eq 511 then $
;      polin = 1 $
;   else if file.type eq 800 || file.type eq 801 || file.type eq 810 || file.type eq 811 then $
;      polin = 1 $
;   else begin
;      error_button = DIALOG_MESSAGE(['Data have to be in ','HV basis'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
;      return
;   endelse
;   if n_elements(polin) eq 0 then polin=0

;   if (polin && file.vdim mod 4 ne 0) || (~polin && file.zdim mod 4 ne 0) then begin
;      error_button = DIALOG_MESSAGE(['Scattering vector with','4 elements required'] $
;                                    , DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
;      return
;   endif

;   if (file.type ge 200L && file.type le 210L) || (file.type ge 500L && file.type le 509L) || (file.type ge 800 && file.type le 803) then $
;      matrix = 0 $
;   else $
;      matrix = 1
;   if (file.type eq 200L || file.type eq 220L || file.type eq 500L || file.type eq 510L || file.type eq 800 || file.type eq 810) then $
;      pauli = 0 $
;   else $
;      pauli = 1


; GUI
  if n_elements(method) eq 0 then method = 0
  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Cross-polar symmetrisation',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     field2 = widget_label(main,value='Select symmetrisation method:')
     field1 = CW_BGROUP(main,['Average HV / VH','Take only HV','Take only VH'],/column,/exclusive,set_value=method)
     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_ok, tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['CROSS-POLAR SYMMETRISATION',$
                       ' ',$
                       'RAT module written 04/2004 by Andreas Reigber', $
                       'and extended 12/2005 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,field1,GET_VALUE=method ; read widget fields
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; here we go
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=header,info=info,type=type
  if matrix then header[1:2] = header[1:2]/4L*3L $
  else header[1] = 3L
  srat,outputfile,eee,header=header,info=info,type=type
;  srat,outputfile,eee,header=[3l,3l,file.xdim,file.ydim,file.var],info=info,type=type

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

;;; in Pauli matrix basis, the only one 'reasonable' and 'easy'
;;; possibility for xsym is just to take the HV+VH term, which is the
;;; third one.
;   if polin then begin
;      n_tr=matrix? file.vdim / 4: file.zdim
;      choice = [0,1,2]
;      ch1    = 2L + lindgen(n_tr)*4L
;      ch2    = 3L + lindgen(n_tr)*4L
;      if method eq 2 then choice=[0,1,3]
;      choice = mm_v2m(choice,n_tr) + mm_s2v(lindgen(n_tr)*4L,3)
;      choice = reform(choice,n_elements(choice))
;   endif
  if polin then begin
     n_tr=matrix? file.vdim / 4: file.zdim
     choice = [0,1,2]
     ch1    = 2L + lindgen(n_tr)*4L
;     ch2    = 3L + lindgen(n_tr)*4L
     ch2    = ch1
     choice = mm_v2m(choice,n_tr) + mm_s2v(lindgen(n_tr)*4L,3)
     choice = reform(choice,n_elements(choice))
  endif

; pop up progress window
  progress,Message='Symmetrisation of Cross-Polar channels...',/cancel_button

  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var,/nozero)
     readu,ddd,block

     if ~polin then begin
        if ~matrix then case method of ; scattering vector
           0: begin
              block[*,2,*,*] = (block[*,2,*,*]+block[*,3,*,*])/2.
              block=block[*,0:2,*,*]
           end
           1: block=block[*,0:2,*,*]
           2: block=block[*,[0,1,3],*,*]
        endcase else if matrix then $ ; polsar matrix
           case method of 
           0: begin
              block[*,2,*,*] = (block[*,2,*,*]+block[*,3,*,*])/2.
              block[2,*,*,*] = (block[2,*,*,*]+block[3,*,*,*])/2.
              block=block[0:2,0:2,*,*]
           end
           1: block=block[0:2,0:2,*,*]
           2: block=block[[0,1,3],[0,1,3],*,*]
        endcase
        if ~pauli then block[*,2,*,*] *= sqrt(2.)
        if ~pauli && matrix then block[2,*,*,*] *= sqrt(2)
     endif else begin           ; mb-polinsar
        if method eq 0           then block[ch1,*,*,*] = (block[ch1,*,*,*]+block[ch2,*,*,*])/2.
        if method eq 0 && matrix then block[*,ch1,*,*] = (block[*,ch1,*,*]+block[*,ch2,*,*])/2.
        if ~pauli           then block[ch1,*,*,*] *= sqrt(2.)
        if ~pauli && matrix then block[*,ch1,*,*] *= sqrt(2.)
        if ~matrix then block=block[choice,*,*,*] $
        else block=block[choice,choice,*,*]
     endelse

     writeu,eee,block
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.zdim = (header[0] eq 4?header[2]:header[1])
  file.vdim = (header[0] eq 4?header[1]:1)

  evolute,'Symmetrisation of Cross-Polar channels'
  ignore = set_par('polarizations',3L)
  
; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
