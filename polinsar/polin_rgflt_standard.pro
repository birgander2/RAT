;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_rgflt_standard
; last revision : 08/2006
; written by    : Maxim Neumann
; Multibaseline range filter (standard)
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

pro polin_rgflt_standard,CALLED = called
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 503) then begin
     error = DIALOG_MESSAGE("This is not a multibaseline scattering vector", DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  n_tr = file.zdim
  bw   = 16.
  rs   = 19.2
  ss   = findgen(n_tr)/(n_tr-1)*rs/4.
  text3= lonarr(n_tr,/nozero)

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Spectral range filtering',/floating,/tlb_kill_request_events,/tlb_frame_attr)

     text1 = CW_FIELD(main,VALUE=bw,/floating,XSIZE=6,TITLE='Range bandwidth           [MHz] :')
     text2 = CW_FIELD(main,VALUE=rs,/floating,XSIZE=6,TITLE='Range sampling frequency  [MHz] :')
     for i=0,n_tr-1 do $
        text3[i] = CW_FIELD(main,VALUE=ss[i],/floating,XSIZE=6,TITLE='Spectral shift of track '+strcompress(i,/R)+' [MHz] :')
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
           infotext = ['MULTIBASELINE SPECTRAL RANGE FILTER V1.0',$
                       ' ',$
                       'RAT module written by A. Reigber and M. Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

     widget_control,text1,get_value=bw
     widget_control,text2,get_value=rs
     for i=0,n_tr-1 do begin
        widget_control,text3[i],get_value=sstmp
        ss[i]=sstmp
     endfor

     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; calculate parameters

  empty = (1-bw/rs)*file.xdim   ; empty spectral range (in pixels)

  minss    = min(ss,MAX=maxss)
  diffss   = maxss-minss
  cut      = fix(diffss/rs*file.xdim) ; how much to cut (in pixels) at the ends
  cutLeft  = fix(cut* (ss-minss)/diffss)
  cutRight = cut - cutLeft
  fltX     = fltarr(file.xdim,n_tr)+1.
  for i=0,n_tr-1 do begin
     fltX[0:empty/2+cutLeft[i],i] = 0.0
     fltX[file.xdim-empty/2-cutRight[i]:*,i] = 0.0
  endfor
  fltX = shift(fltX,file.xdim/2,0)

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=head,info=info,type=type		

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Spectral range filtering...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block

; -------- THE FILTER ----------
     for j=0,file.zdim-1 do $           ;; tracks
        for p=0,file.vdim-1 do $        ;; polarization
           for k=0,blocksizes[i]-1 do $ ;; azimuth
              block[p,j,*,k] = fft(fft(reform(block[p,j,*,k]),-1)*fltX[*,j],+1)
; -------- THE FILTER ----------

     writeu,eee,block
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile

  evolute,'Standard Range Filtering for Multibaselines'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
