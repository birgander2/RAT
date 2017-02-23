;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: mousebox
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
pro mousebox,xmin,xmax,ymin,ymax
	common rat, types, file, wid, config

; ---- Draw white box with the mouse to select corner

	widget_control,wid.draw,draw_button_events=1, draw_motion_events = 1
	geo = widget_info(wid.draw,/geometry)
	
	device,/cursor_crosshair
	repeat res = widget_event(wid.draw) until res.press eq 1
	x1 = res.x
	y1 = res.y
	xmin = res.x
	ymin = res.y
	
	savim = tvrd(xmin,ymin,2,2,true=1)
	
	yo = y1
	
	repeat begin
		res = widget_event(wid.draw) 
		x2 = res.x
		y2 = res.y


		tv,savim,xmin,ymin,true=1
		
		xmin = min([x1,x2]) > 0
		xmax = max([x1,x2]) < !d.x_size-1
		ymin = min([y1,y2]) > 0
		ymax = max([y1,y2]) < !d.y_size-1
		difx = xmax - xmin + 1 > 1
		dify = ymax - ymin + 1 > 1

		widget_control,wid.draw,get_draw_view=scroll	
		if (y2 lt scroll[1]) and (scroll[1]-10 ge 0) then widget_control,wid.draw,set_draw_view=scroll - [0,10]
		if (y2 gt scroll[1]+geo.ysize) and (scroll[1]+10 le (geo.draw_ysize-geo.ysize)) then widget_control,wid.draw,set_draw_view=scroll + [0,10]

		savim = tvrd(xmin,ymin,difx,dify,true=1)
		plots,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/device,color=255
		
		yo = y2
	endrep until res.release eq 1

	tv,savim,xmin,ymin,true=1	
	device,/cursor_original
	
	widget_control,wid.draw, draw_button_events = 0, draw_motion_events = 0
	
; ---- End draw box	

	if xmax lt xmin then begin
		dummy = xmax
		xmax = xmin
		xmin = dummy
	endif
	
	if ymax lt ymin then begin
		dummy = ymax
		ymax = ymin
		ymin = dummy
	endif
	
end
