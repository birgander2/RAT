;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: tool_box_update
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
pro tool_box_update;, select_channel=select_channel,data_management=data_management,color_table=color_table
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag,palettes,pnames
		
	if xregistered('tool_box') ne 0 then begin
  		event = LookupManagedWidget('tool_box')
;  		tab = widget_info(widget_info(event,/child),/tab_current)
;  		if keyword_set(data_management) then tab=1
;  		if keyword_set(color_table) then tab=2
;        widget_control, id, /destroy
;  		case tab of
;  			0:data_management
;  			1:data_management,/data_management
;  			2:data_management,/color_table
;  		endcase
		
		widget_control, event, get_uvalue=state, /no_copy

;*******************************************************************************
		; GENERATE THE NEW SELECT CHANNEL TREE
		layer_image = reverse(read_tiff(config.imagedir+'multi_layer_image.tif',interleave=2),2)
		for m=0,(size(*state.select_root))[1]-1 do widget_control, (*state.select_root)[m], /destroy
		
		select_root = lonarr(file.mult)
		if file.mult eq 1 then main_text = file_basename(file.name) else main_text = file_basename(extract_mtnames(file.name))
		for m=0,file.mult-1 do begin
			select_root[m] = widget_tree(state.select_tree, value=main_text[m], /folder, /expanded, bitmap=layer_image)
		endfor
		ptr_free,state.select_root
		state.select_root = ptr_new(select_root)

		; --> Display the Name of the channel
		file_image = reverse(read_tiff(config.imagedir+'file_image.tif',interleave=2),2)
		leaf = lonarr(file.mult*file.vdim*file.zdim)
		for m=0,file.mult-1 do begin
			for i=0,file.vdim-1 do begin
				for j=0,file.zdim-1 do begin
					index = m*file.zdim*file.vdim+i*file.zdim+j 
					leaf[index] = widget_tree((*state.select_root)[m], value=channel_names[i*file.zdim+j],bitmap=file_image)
				endfor
			endfor
		endfor
		ptr_free,state.select_leaf
		state.select_leaf = ptr_new(leaf)
		for m=0,file.mult-1 do widget_control, (*state.select_root)[m], /realize
;*******************************************************************************

;*******************************************************************************
		; SELECT THE CORRECT CHOICE FOR BUTTON
		; --> Analysis of channel settings (one or more channel)
		if (color_flag eq 0) or (file.dim eq 2)  and not (file.mult gt 1) then begin
			rgb_flag = 0 
			state.select_chnr1 = channel_selec[0]
			state.select_chnr2 = channel_selec[0]
			state.select_chnr3 = channel_selec[0]
			widget_control, state.select_button_choose,set_value=0
			widget_control, state.select_text_gray, set_value=channel_names[state.select_chnr1 mod (file.zdim*file.vdim)]
			if file.dim eq 2 then widget_control, state.select_sub11, map=0
		endif else begin
			rgb_flag = 1		
			state.select_chnr1 = channel_selec[0]
			state.select_chnr2 = channel_selec[1]
			state.select_chnr3 = channel_selec[2]
			widget_control, state.select_sub11, map=1
			widget_control, state.select_button_choose,set_value=1
			widget_control,state.select_text_R,set_value=channel_names[state.select_chnr1 mod (file.zdim*file.vdim)]
			widget_control,state.select_text_G,set_value=channel_names[state.select_chnr2 mod (file.zdim*file.vdim)]
			widget_control,state.select_text_B,set_value=channel_names[state.select_chnr3 mod (file.zdim*file.vdim)]
		endelse
		widget_control,state.select_sub_chosen_band[0],map=1-rgb_flag
		widget_control,state.select_sub_chosen_band[1],map=rgb_flag
;*******************************************************************************

;*******************************************************************************
		; DISPLAY NEW DATA FILE MANAGEMENT
		if state.data_main_name_realize then widget_control,state.data_main_name,/destroy
		if file_test(config.workdir+'data_management.txt') then begin
			tool_box_data_management_file_read,config.workdir+'data_management.txt',state
			widget_control, state.data_main_name, /realize
		endif else state.data_main_name_realize = 0
		widget_control,state.data_sub21,map=1 - file_test(config.workdir+'data_management.txt')
		widget_control,state.data_sub22,map=file_test(config.workdir+'data_management.txt')
;*******************************************************************************

;*******************************************************************************
		; Update the color table management tool box
		widget_control,(*state.color_leaf_wid)[0],/destroy
		for i=0,n_elements(*state.color_branch_wid)-1 do widget_control,(*state.color_branch_wid)[i],/destroy
		tool_box_color_table_management_read,state
  		color_display_current_table,state.color_draw_current,0
		widget_control,state.data_preview_draw,get_value=index
 		color_display_current_table,state.color_draw_selected,-100
		widget_control,state.data_preview_draw,get_value=index
		widget_control,state.color_button_base_add,map=0
		widget_control,state.color_button_base_modify,map=0
		widget_control,state.color_button_base_load,map=0
		widget_control,state.color_button_base_save,map=0
		widget_control,state.color_button_base_remove,map=0
		widget_control,state.color_draw_label,set_value="Defined color table"
;*******************************************************************************
		
		widget_control,state.data_preview_draw,get_value=index
		rat_display_small_preview,'default',index,state.data_preview_text,100,150
		widget_control,wid.draw,get_value=index
		wset,index
		case widget_info(state.wTab,/tab_current) of
			0:widget_control,state.but_appl,set_value='Apply',sensitive=1
			1:widget_control,state.but_appl,set_value='Load',sensitive=1
			2:widget_control,state.but_appl,set_value='Apply',sensitive=0
			else:
		endcase
		
		widget_control, event, set_uvalue=state, /no_copy	
	endif	
end
