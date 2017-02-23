;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: color_display_current_table
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
pro color_display_current_table,used_window,color_table_index,nb_color=nb_color
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames
	
	tvlct,r,g,b,/get
	widget_control,used_window,get_value=tmp
	wset,tmp
	
	
	if color_table_index ge 0 then begin
		; define the color table to be displayed
		new_r = reform(palettes[color_table_index,*,0])
		new_g = reform(palettes[color_table_index,*,1])
		new_b = reform(palettes[color_table_index,*,2])
		
	endif else begin
		new_r = bytarr(256) + 192
		new_g = bytarr(256) + 192
		new_b = bytarr(256) + 192
	endelse

	; Get the number of color
	ind = where(new_r ne 0 or new_b ne 0 or new_g ne 0, nb_color)
	 
	if nb_color eq 0 then begin
		new_r = bytarr(256) + 192
		new_g = bytarr(256) + 192
		new_b = bytarr(256) + 192
		ind = where(new_r ne 0 or new_b ne 0 or new_g ne 0, nb_color)
	endif else nb_color = ind[nb_color-1]+1
	
	
	
	
	tvlct,new_r,new_g,new_b
	
	
	
	;loadct,table,file=config.prefdir+'user_color.tbl',/silent
	tv,congrid((bytarr(50)+1)##bindgen(nb_color),340,40)
	tvlct,r,g,b
	widget_control,wid.draw,get_value=tmp
	wset,tmp
end
