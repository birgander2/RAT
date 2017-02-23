;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: inspect area
; written by    : Andreas Reigber (TUB)
; last revision : 21.August.2003
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

pro inspect_area
	common rat, types, file, wid, config

; ---- Does image has an amplitude to analyse?

	if file.type ne 100 and file.type ne 101 then begin
		error = DIALOG_MESSAGE("Complex or amplitude SAR image required", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return	
	endif

; ---- Define region of interest

	widget_control,wid.draw,draw_button_events=1, draw_motion_events = 1
	res = cw_defroi(wid.draw,image_size=[file.xdim,file.ydim],/restore)
	widget_control,wid.draw,draw_button_events=0, draw_motion_events = 0
	
	if (size(res))[0] eq 0 then return
	if (size(res))[0] eq 1 and (size(res))[1] eq 1 then return

; ---- Generate GUI
	
	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Distributed target analysis',/floating,/tlb_kill_request_events,/tlb_frame_attr)
	
	sub  = WIDGET_BASE(main,column=2,/frame)
	tx1  = widget_text(sub,XSIZE=48,ysize=20)
	dr1  = widget_draw(sub,XSIZE=400,ysize=400)

	buttons  = WIDGET_BASE(main,column=2,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

	WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]


; --- Output results in GUI

	widget_control,dr1,get_value=index
	wset,index
	
	xminf = floor(min(res mod file.xdim) / wid.draw_scale)
	xmaxf = floor(max(res mod file.xdim) / wid.draw_scale)
	yminf = floor(min(res  /  file.xdim) / wid.draw_scale)
	ymaxf = floor(max(res  /  file.xdim) / wid.draw_scale)
	
	rrat,file.name,arr,block=[xminf,yminf,xmaxf-xminf,ymaxf-yminf]
	arr = abs(reform(arr))
	
	show_arr  = congrid(arr,400,400) > 0
	mm  = total(show_arr)/n_elements(show_arr)	
	show_arr = bytscl(show_arr,0,config.sar_scale*mm)
	
	xind = res mod file.xdim
	yind = res / file.xdim
	xmin = min(xind)
	xmax = max(xind)
	ymin = min(yind)
	ymax = max(yind)

	check = bytarr(xmax-xmin,ymax-ymin)
	check[xind-xmin,yind-ymin] = 1
	
	show_check = congrid(check,400,400)
	show_index = where(show_check eq 0,dummy)
	if dummy gt 0 then show_arr[show_index] = show_arr[show_index] / 2.0
	tv,show_arr

	check = congrid(check,xmaxf-xminf,ymaxf-yminf)
	index = where(check eq 1,nrofpixels)
	if nrofpixels gt 0 then begin
		pixels = arr[index]
		
		pmax  = max(pixels)
		pmean = mean(pixels)
		pvar  = variance(pixels)
	
		imax  = max(pixels^2)
		imean = mean(pixels^2)
		ivar  = variance(pixels^2)
	
		penl  = imean^2 / ivar > 1.0

		if	config.os eq 'windows' then newline = strcompress(13B) + string(10B)
		if	config.os eq 'unix' then newline = string(10B)
	
		text1 = $
		'DISTRIBUTED TARGET PARAMETERS:' + newline + $
		newline + $	
		'number of pixels selected      : ' + strcompress(nrofpixels) + newline + $
		newline + $	
		'maximum amplitude              : ' + strcompress(pmax) + newline + $
		'mean amplitude                 : ' + strcompress(pmean) + newline  + $
		'variance amplitude             : ' + strcompress(pvar) + newline   + $
		newline + $	
		'maximum intensity              : ' + strcompress(imax) + newline  + $
		'mean intensity                 : ' + strcompress(imean) + newline  + $
		'variance intensity             : ' + strcompress(ivar) + newline   + $
		newline + $	
		'effective number of looks      : ' + strcompress(penl)
		
		widget_control, tx1, SET_VALUE = text1
	endif
; event_loop

	repeat begin
		event = widget_event(main)
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['POINT TARGET ANALYSIS',$
			' ',$
			'RAT module written 08/2003 by Andreas Reigber']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
	endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,main,/destroy                        ; remove main widget

; switch back to main draw widget

	widget_control,wid.draw,get_value=index
	wset,index

end
