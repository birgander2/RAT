;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: measure_value
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

pro measure_value, MATRIXVIEW=MATRIXVIEW
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag

  x=0  & y=0
  xo=1 & yo=1
  if	config.os eq 'windows' then newline = string(13B) + string(10B)
  if	config.os eq 'unix' then newline = string(10B)

  widget_control,wid.draw, draw_motion_events = 1
  device,/cursor_crosshair
  names = channel_names
  maxle = max(strlen(names))

  if keyword_set(MATRIXVIEW) then goto, matrix_view

;;;; LIST VIEW
list_view:
  xo++ ; important to actualize the data
  main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Measure value/location',/floating,/tlb_kill_request_events,/tlb_frame_attr)
  text = WIDGET_TEXT(main,XSIZE=50, YSIZE=6+(file.vdim*file.zdim*file.mult))	
  buttons = WIDGET_BASE(main,column=4,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_matr = WIDGET_BUTTON(buttons,VALUE=' MatrixView ',xsize=80,sensitive=(file.zdim gt 1?1:0))
  but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  WIDGET_CONTROL, main, /REALIZE, tlb_get_size=toto
  widget_control, main

  infostr = ''
  for i=0,file.vdim-1 do for j=0,file.zdim-1 do for k=0,maxle-strlen(channel_names[i*file.zdim+j]) do names[i*file.zdim+j] += ' '  
  
  repeat begin                  ; Event loop
     wait,0.01
	  event = widget_event(main,/nowait)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['Measure value/location',$
                    ' ',$
                    'RAT module written 08/2004 by Andreas Reigber']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
     end
     res = widget_event(wid.draw,/nowait)
     if res.id eq wid.draw then begin
        x   = res.x
        y   = res.y
     endif
     if x ne xo || y ne yo then begin
        xf  = round(float(x) / wid.draw_scale)  < file.xdim-1 > 0
        yf  = round(float(y) / wid.draw_scale)  < file.ydim-1 > 0

			if file.mult eq 1 then begin
				rrat,file.name,inblock,block=[xf,yf,1,1]
				inblock = reform(inblock,file.mult,file.vdim,file.zdim)
   		endif else begin
				mfiles  = extract_mtnames(file.name)
				inblock = make_array(file.mult,file.vdim,file.zdim,type=file.var)
				for i=0,file.mult-1 do begin
					rrat,mfiles[i],minblock,block=[xf,yf,1,1] 
					inblock[i,*,*] = minblock
				endfor
			endelse
	     xo  = x & yo  = y
		 
		;calculate real-world co-ordinates if transform is defined
		err = get_par('transform',transform)
		if err eq 0 then begin
			geo_calc,xf,yf,xreal,yreal,transform,0
		endif
        
        infostr  = "Display coordinates : "+strcompress(x)+strcompress(y)
        infostr += newline
        infostr += "Data coordinates    : "+strcompress(xf)+strcompress(yf)
        infostr += newline
		if err eq 0 then begin
			infostr += "Real coordinates    : "+strcompress(xreal)+strcompress(yreal)
			infostr += newline
		endif
		infostr +=newline
        if file.var eq 6 or file.var eq 9 then begin
           infostr += "Data amplitude / phase value(s)"
           infostr += newline + newline
           for m=0,file.mult-1 do for i=0,file.vdim-1 do for j=0,file.zdim-1 do infostr += names[i*file.zdim+j] + ' : ' + strcompress(abs(inblock[m*file.zdim*file.vdim+i*file.zdim+j])) + ' /' + strcompress(atan(inblock[m*file.zdim*file.vdim+i*file.zdim+j],/phase)*!radeg) + ' [deg]' + newline
        endif else begin
           infostr += "Data values(s)      : "
           infostr += newline + newline
           for m=0,file.mult-1 do for i=0,file.vdim-1 do for j=0,file.zdim-1 do infostr += names[i*file.zdim+j] + ' : ' + strcompress(inblock[m*file.zdim*file.vdim+i*file.zdim+j]) + newline
        endelse
        widget_control, text, SET_VALUE = infostr
     endif

  endrep until (event.id eq but_ok) || (event.id eq but_canc) || (event.id eq but_matr) || tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  widget_control,main,/destroy  ; remove main widget

;;;; MATRIX VIEW
  if event.id eq but_matr then begin
matrix_view:
     xo++                       ; important to actualize the data
     mainM = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Measure value/location',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     textM = WIDGET_TEXT(mainM,XSIZE=116, YSIZE=(file.var eq 6 || file.var eq 9?5+file.vdim*2:3+file.vdim))
     buttonsM = WIDGET_BASE(mainM,column=4,/frame)
     but_okM   = WIDGET_BUTTON(buttonsM,VALUE=' OK ',xsize=80,/frame)
     but_listM = WIDGET_BUTTON(buttonsM,VALUE=' ListView ',xsize=80)
     but_cancM = WIDGET_BUTTON(buttonsM,VALUE=' Cancel ',xsize=60)
     but_infoM = WIDGET_BUTTON(buttonsM,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, mainM, /REALIZE, default_button = but_cancM,tlb_get_size=toto
     widget_control, mainM
     
     repeat begin               ; Event loop
      wait,0.01
       event = widget_event(mainM,/nowait)
        if event.id eq but_infoM then begin ; Info Button clicked
           infotext = ['Measure value/location',$
                       ' ',$
                       'RAT module written 08/2004 by Andreas Reigber', $
                       'and extended 02/2005 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = mainM, TITLE='Information')
        endif
        res = widget_event(wid.draw,/nowait)
        if res.id eq wid.draw then begin
           x= res.x & y= res.y
        endif
        if x ne xo || y ne yo then begin
           xf  = round(float(x) / wid.draw_scale)  < file.xdim-1 > 0
           yf  = round(float(y) / wid.draw_scale)  < file.ydim-1 > 0
           rrat,file.name,inblock,block=[xf,yf,1,1]
           xo = x & yo = y
           infostr  = "Display coordinates: "+string(FORMAT='(i6,i6 )',x,y)+'    '
           infostr += "Data coordinates: "+string(FORMAT='(i6,i6 )',xf,yf)+newline
           if file.var eq 6 or file.var eq 9 then begin
              infostr += 'Amplitude'+newline+'v\z'+string(FORMAT='(8(I14))',indgen(file.zdim))+newline
              for v=0,file.vdim-1 do $
                 infostr += string( FORMAT='((I3),8(f14.3))',v,abs(inblock[v*file.zdim+indgen(file.zdim)]) ) +newline
              infostr += 'Phase in [deg]'+newline+'v\z'+string(FORMAT='(8(I14))',indgen(file.zdim))+newline
              for v=0,file.vdim-1 do $
                 infostr += string( FORMAT='((I3),8(f14.3))',v,atan(inblock[v*file.zdim+indgen(file.zdim)],/phase)*!radeg ) +newline
           endif else begin
              infostr += newline+'v\z'+string(FORMAT='(8(I14))',indgen(file.zdim))+newline
              for v=0,file.vdim-1 do $
                 infostr += string(FORMAT='((I3),8(G14.3))',v,inblock[v*file.zdim+indgen(file.zdim)] ) +newline
           endelse
           widget_control, textM, SET_VALUE = infostr
        endif

     endrep until (event.id eq but_okM) || (event.id eq but_cancM) || (event.id eq but_listM) || tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,mainM,/destroy ; remove main widget
     if event.id eq but_listM then goto, list_view
     
  endif

  widget_control,wid.draw, draw_motion_events = 0

end
