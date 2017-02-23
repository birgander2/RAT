;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: cw_rat_draw
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
pro cw_rat_draw_ev,ev
  	widget_control, ev.id, get_uvalue = state, /no_copy
	if ev.press eq 8 then begin
		widget_control,ev.id,get_draw_view=tmp
		state.dv = tmp
		widget_control,ev.id,set_draw_view=state.dv + [0,10]
	endif
	if ev.press eq 16 then begin
		widget_control,ev.id,get_draw_view=tmp
		state.dv = tmp
		widget_control,ev.id,set_draw_view=state.dv - [0,10]
	endif
	if ev.press eq 1 and ev.clicks eq 1 then begin
		state.flag_press = 1
		state.flag_press_pos = [ev.x, ev.y]
	endif
	if ev.release eq 1 then begin
		state.flag_press = 0
		device,/cursor_original
	endif
	if state.flag_press eq 1 then begin
		device,cursor_standard=52
		widget_control,ev.id,get_draw_view=tmp
		state.dv = tmp
		geo = widget_info(ev.id,/geometry)
		scroll_test = (state.dv + (state.flag_press_pos - [ev.x,ev.y]))[1]
		if scroll_test ge 0 and scroll_test le (geo.draw_ysize-geo.ysize) then $
		state.dv = state.dv + (state.flag_press_pos - [ev.x,ev.y])
		widget_control,ev.id,set_draw_view=state.dv
	endif
  	widget_control, ev.id, set_uvalue = state, /no_copy
end

function cw_rat_draw,parent,xsize,ysize,XSCROLL=xscroll,YSCROLL=yscroll, SCROLL=scroll, retain=retain,color_model=color_model

	state = { flag_press: 0,$
				 flag_press_pos: [0,0],$
				 dv: [0,0]$
	}
	main = widget_draw(parent,xsize=xsize,ysize=ysize,x_scroll_size=xscroll,y_scroll_size=yscroll, scroll=scroll, $
	                   retain=retain,color_model=color_model,event_pro='cw_rat_draw_ev')
  	widget_control, main, set_uvalue = state, /no_copy
	return,main
end
