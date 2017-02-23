;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calc_polsnr
; written by    : Andreas Reigber
; last revision : March 2008
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



pro calc_polsnr,CALLED=called,BOXSIZE=boxsize,PROFILE=profile,SAVEPROFILE=saveprofile
  common rat, types, file, wid, config, tiling

  if not keyword_set(boxsize) then boxsize = 7l            ; Default values
  if not keyword_set(profile) then profile = 1             ; Default values
  if not keyword_set(saveprofile) then saveprofile = ''    ; Default values
; check if array is usable

  if file.type lt 220 or file.type gt 229 then begin
     error_button = DIALOG_MESSAGE(['Data have to be spatially averaged','polarimetric matrices like [C] or [T].',' ','(try a speckle filter now ?)'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif
  if file.vdim ne 4 then begin
     error_button = DIALOG_MESSAGE(['Data need to include both','HV and VH channels!',' ','(try a speckle filter now ?)'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Polarimetric SNR Calculation',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     field1 = CW_FIELD(main,VALUE=1,/integer,TITLE='Internal smooth   : ',XSIZE=3)
     field2 = cw_bgroup(main,[' Yes ',' No '],label_left='Range profile ',set_value=profile,/exclusive,/row)
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
           infotext = ['POLARIMETRIC SNR CALCULATION',$
                       ' ',$
                       'RAT module written 03/2008 by Andreas Reigber']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,field1,GET_VALUE=boxsize                           ; read widget fields
     widget_control,field2,GET_VALUE=profile                           ; read widget fields
     widget_control,main,/destroy                                      ; remove main widget
     if event.id ne but_ok then return                                 ; OK button _not_ clicked
  endif

; go

  WIDGET_CONTROL,/hourglass

; undo function

  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

  head = 1l
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=[2l,file.xdim,file.ydim,4l],info=info,type=50l

; calculating preview size and number of blocks

  tiling_init,overlap=(boxsize+1)/2

; pop up progress window

  progress,Message='PolSAR SNR...',/cancel_button

; calculating eigenvectors

  if profile eq 0 then pline = complexarr(4,4,file.xdim)
  index = reverse(indgen(file.zdim))
  for i=0,tiling.nr_blocks-1 do begin
     progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
     if wid.cancel eq 1 then return

     tiling_read,ddd,i,block
     if boxsize gt 1 then block = smooth(block,[1,1,boxsize,boxsize],/edge_truncate)

     if profile eq 0 then pline += (total(block,4) / (*tiling.blocksizes)[i])

     oblock = fltarr(1,1,file.xdim,(*tiling.blocksizes)[i])
     for k=0,file.xdim-1 do begin
        for l=0,(*tiling.blocksizes)[i]-1 do begin
           foo = (float(la_eigenql(reform(block[*,*,k,l]))))[index]
           oblock[0,0,k,l] = 10*alog10(total(foo[0:2]-foo[3])/3.0/foo[3])
        endfor
     endfor
     tiling_write,eee,i,temporary(oblock)
     tiling_jumpback,ddd
  endfor
  free_lun,ddd,eee
  rat_finalise,outputfile,finalfile,CALLED=CALLED
  evolute,'PolSAR SNR, boxsize : '+strcompress(boxsize,/R)

  if not keyword_set(called) and profile eq 0 then begin ; Graphical interface

     snrline = fltarr(file.xdim)
     for k=0,file.xdim-1 do begin
        foo = (float(la_eigenql(reform(pline[*,*,k]))))[index]
        snrline[k] = 10*alog10(total(foo[0:2]-foo[3])/3.0/foo[3])
     endfor

     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Range SNR profile',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     dr1  = widget_draw(main,XSIZE=800,ysize=500)
     buttons  = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_save = WIDGET_BUTTON(buttons,VALUE=' Save ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE,tlb_get_size=toto

     tek_color
     plot,snrline,title='SNR',ytitle="dB",xtitle="x [pixels]"
     loadct,0,/silent

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_save then begin
           path = config.workdir
           saveprofile = dialog_pickfile(title='Select output file',dialog_parent=wid.base, filter = '*.rat',path=path,get_path=path)
           if strlen(saveprofile) gt 0 then config.workdir = path
        endif
     endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,main,/destroy ; remove main widget
     widget_control,wid.draw,get_value=index
     wset,index
  endif
  if strlen(saveprofile) gt 0 then srat,saveprofile,snrline

end
