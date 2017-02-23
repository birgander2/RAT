;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: color_update_current_table
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
pro color_update_current_table,data=ind
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag
	if xregistered('tool_box') ne 0 then begin
  		event = LookupManagedWidget('tool_box')
		widget_control, event, get_uvalue=state, /no_copy
  		tvlct,r,g,b,/get
		modifyct,(*state.color_list_name).pos[ind],(*state.color_list_name).table_name[ind],r,g,b,file=config.prefdir+'user_color.tbl'
		color_display_current_table,state.color_draw,(*state.color_list_name).pos[ind],(*state.color_list_name).ncolor[ind]
		widget_control, event, set_uvalue=state, /no_copy
	endif
end
