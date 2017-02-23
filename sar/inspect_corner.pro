;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: inspect_corner
; written by    : Andreas Reigber (TUB)
; last revision : 6.August.2003
; Corner reflector analysis
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

pro inspect_corner
	common rat, types, file, wid, config

; ---- Does image has an amplitude to analyse?

	if file.type ne 100 and file.type ne 101 then begin
		error = DIALOG_MESSAGE("Complex or amplitude SAR image required", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return	
	endif
	
; ---- Draw white box with the mouse to select corner

	widget_control,wid.draw,draw_button_events=1, draw_motion_events = 1
	
	device,/cursor_crosshair
	repeat res = widget_event(wid.draw) until res.press eq 1
	x1 = res.x
	y1 = res.y
	xmin = res.x
	ymin = res.y
	savim = bytarr(1,1)

	repeat begin
		res = widget_event(wid.draw) 
		x2 = res.x
		y2 = res.y
		
		tv,savim,xmin-1,ymin-1
		
		xmin = min([x1,x2])
		xmax = max([x1,x2])
		ymin = min([y1,y2])
		ymax = max([y1,y2])
		difx = xmax - xmin + 2
		dify = ymax - ymin + 2

		savim = tvrd(xmin-1,ymin-1,difx,dify)
		plots,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/device,color=255
	endrep until res.release eq 1
	tv,savim,xmin-1,ymin-1
	device,/cursor_original
	
	widget_control,wid.draw, draw_button_events = 0, draw_motion_events = 0
	
; ---- End draw box	

; ---- Get coordinates of maximum in the selected box (device corrdinates)

	dummy   = max(savim,pos)
	xc = xmin - 1 + (pos mod (size(savim))[1])
	yc = ymin - 1 + (pos / (size(savim))[1])

; ---- Transform them to data coordinates
	
	xc = round(xc / wid.draw_scale) 
	yc = round(yc / wid.draw_scale) 
	
	if xc lt 40 then xc = 40
	if xc ge file.xdim-40 then xc = file.xdim-40 
	if yc lt 40 then yc = 40
	if yc ge file.ydim-40 then yc = file.ydim-40 
	
	
; ---- Estimate all the parameters

; -> center the corner

	rrat,file.name,arr,block=[xc-40,yc-40,81,81]
	arr = reform(arr)
	pmax  = max(abs(arr),pos)
	dx = (pos mod 81) - 40
	dy = (pos / 81) - 40
	
	rrat,file.name,arr,block=[xc-20+dx,yc-20+dy,41,41]
	arr = reform(arr)
	pmax  = max(abs(arr),pos)  ; peak amplitude
	ppha  = atan(arr[pos],/phase)  ; peak phase
	
; -> oversampling
	
	savim  = congrid(abs(arr),410,410,cubic=-0.5) > 0
	dummy  = max(savim,pos)  
	dx = (pos mod 410) - 205
	dy = (pos / 410) - 205
	
	if abs(dx) ge 54 or abs(dy) ge 54 then begin
		error = DIALOG_MESSAGE("No clear point target found!", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return	
	endif
	
	savim = shift(savim,-dx,-dy)
	
	savim2 = savim[205-150:205+149,205-150:205+149]	
	savim  = savim[205-128:205+127,205-128:205+127]	

; -> estimate resolution and PSLR

	xprof = reform(abs(savim[*,128]))
	yprof = reform(abs(savim[128,*]))
	profx = xprof / max(xprof)
	profy = yprof / max(yprof)
	
	px1 = 128
	px2 = 128
	while profx[px1] gt 0.5 and px1 lt 255 do px1++
	while profx[px2] gt 0.5 and px2 gt 0   do px2--
	resx1 = (px1-px2)/10.0
	while profx[px1] lt profx[px1-1] and px1 lt 255 do px1++
	while profx[px2] lt profx[px2+1] and px2 gt 0   do px2--
	resx2 = (px1-px2)/20.0
	while profx[px1] gt profx[px1-1] and px1 lt 255 do px1++
	while profx[px2] gt profx[px2+1] and px2 gt 0   do px2--
	pslrx1 = -alog10(1.0 / profx[px1]) * 20
	pslrx2 = -alog10(1.0 / profx[px2]) * 20

	py1 = 128
	py2 = 128
	while profy[py1] gt 0.5 and py1 lt 255 do py1++
	while profy[py2] gt 0.5 and py2 gt 0   do py2--
	resy1 = (py1-py2)/10.0
	while profy[py1] lt profy[py1-1] and py1 lt 255 do py1++
	while profy[py2] lt profy[py2+1] and py2 gt 0   do py2--
	resy2 = (py1-py2)/20.0
	while profy[py1] gt profy[py1-1] and py1 lt 255 do py1++
	while profy[py2] gt profy[py2+1] and py2 gt 0   do py2--
	pslry1 = -alog10(1.0 / profy[py1]) * 20
	pslry2 = -alog10(1.0 / profy[py2]) * 20

; ---- Generate GUI

 
	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Point target analysis',/floating,/tlb_kill_request_events,/tlb_frame_attr)
	sub  = WIDGET_BASE(main,column=2,/frame)
	sub1 = WIDGET_BASE(sub,row=2,/frame)
	sub2 = WIDGET_BASE(sub,row=2,/frame)
	
	dr2  = widget_draw(sub1,XSIZE=300,ysize=150)
	dr3  = widget_draw(sub2,XSIZE=300,ysize=150)
	dr1  = widget_draw(sub2,XSIZE=300,ysize=300)
	tx1  = widget_text(sub1,XSIZE=48,ysize=18)

	buttons  = WIDGET_BASE(main,column=2,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

	WIDGET_CONTROL, main, /REALIZE, default_button = but_ok
	
	widget_control, main, tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, XOFFSET=pos[0], yoffset=pos[1]
	
; --- Output results in GUI

	widget_control,dr1,get_value=index
	wset,index
	loadct,33,/silent
	tvscl,alog(1.0+savim2)
	loadct,0,/silent

	widget_control,dr2,get_value=index
	wset,index
	plot,xprof,title='Impulse response X',position=[0.01,0.01,0.99,0.85],xstyle=1
	widget_control,dr3,get_value=index
	wset,index
	plot,yprof,title='Impulse response Y',position=[0.01,0.01,0.99,0.85],xstyle=1

	if	config.os eq 'windows' then newline = strcompress(13B) + string(10B)
	if	config.os eq 'unix' then newline = string(10B)

	text1 = $
	'ESTIMATED POINT TARGET PARAMETERS:' + newline + $
	newline + $	
	'maximum amplitude              : ' + strcompress(pmax) + newline + $
	'maximum intensity              : ' + strcompress(pmax^2) + newline 
	
	if file.var eq 6 or file.var eq 9 then $
	text1 = text1 + 'peak phase                     : ' + strcompress(ppha*!radeg) +' deg'	+ newline 
		
	text1 = text1 + newline + $
	'position x of the point target : ' + strcompress(xc) + newline + $
	'resolution x (from -3db width) : ' + strcompress(resx1) +' pixel'+ newline + $
	'resolution x (from 1st minima) : ' + strcompress(resx2) +' pixel'+ newline + $
	'PSLR x (left)                  : ' + strcompress(pslrx1) + ' dB' + newline + $
	'PSLR x (right)                 : ' + strcompress(pslrx2) + ' dB'  + newline + $
	newline + $
	'position y of the point target : ' + strcompress(yc) + newline + $
	'resolution y (from -3db width) : ' + strcompress(resy1) +' pixel'+ newline + $
	'resolution y (from 1st minima) : ' + strcompress(resy2) +' pixel'+ newline + $
	'PSLR y (left)                  : ' + strcompress(pslry1)  + ' dB' + newline + $
	'PSLR y (right)                 : ' + strcompress(pslry2)  + ' dB' 

	widget_control, tx1, SET_VALUE = text1

; ---- Event loop

	repeat begin
		event = widget_event(main)
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['POINT TARGET ANALYSIS',$
			' ',$
			'RAT module written 08/2003 by Andreas Reigber',$
			' ',$
			'Warning: This routine is working correctly only in case of',$
			'a bright target in dark surrounding. Otherwise the automatic',$
			'target search and parameter estimation will fail.']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
	endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,main,/destroy                        ; remove main widget

; switch back to main draw widget

	widget_control,wid.draw,get_value=index
	wset,index
end
