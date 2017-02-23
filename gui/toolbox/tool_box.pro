;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: tool_box
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
@xmanager




;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; SELECT THE APPROPRIATE EVENT OF THE TOOL BOX
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
pro tool_box_event, event 
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames
	

	; to get all the state structure
	widget_control, event.top, get_uvalue=state, /no_copy

	; --> Display the name of the second button depending of the widget tab
	case widget_info(state.wTab,/tab_current) of
		0:widget_control,state.but_appl,set_value='Apply',sensitive=1
		1:widget_control,state.but_appl,set_value='Load',sensitive=1
		2:widget_control,state.but_appl,set_value='Apply',sensitive=1
		else:
	endcase
		 
	
;*******************************************************************************
;*******************************************************************************

;--------------------------- SELECT CHANNEL TAB EVENT --------------------------
	
;*******************************************************************************
;*******************************************************************************
;*******************************************************************************
	; select a channel and put it in the appropriate field
 	if event.id ge (*state.select_leaf)[0] and event.id le (*state.select_leaf)[file.mult*file.zdim*file.vdim-1] then begin
 		chnr = where(*state.select_leaf eq event.id)
		mes = channel_names[chnr]
  		case state.select_flag_display of
			0 : begin
				widget_control,state.select_text_gray,set_value=mes
				state.select_chnr1 = chnr
				state.select_chnr2 = chnr
				state.select_chnr3 = chnr
				channel_selec = [state.select_chnr1] 
				color_flag = 0
				generate_preview,/nodefault,/noupdate;;; obsolete: ,/redisplay
			end
			1 : begin
				case state.select_flag_rgb of
					0 : begin
						widget_control,state.select_text_R,set_value=mes
						state.select_chnr1 = chnr
					end
					1 : begin
						widget_control,state.select_text_G,set_value=mes
						state.select_chnr2 = chnr
					end	
					2 : begin
						widget_control,state.select_text_B,set_value=mes
						state.select_chnr3 = chnr
					end
				endcase
				state.select_flag_rgb = (++state.select_flag_rgb) mod 3
				widget_control, state.select_button_rgb, set_value=state.select_flag_rgb
			end
  		endcase
	endif
;*******************************************************************************

;*******************************************************************************
	; Select the appropriate field for the rgb field
	if state.select_flag_display eq 1 then begin
		if event.id eq state.select_button_rgb then begin
			widget_control, state.select_button_rgb, get_value=dummy
			state.select_flag_rgb = dummy
		endif
	endif
;*******************************************************************************
		
;*******************************************************************************
	; if reset to default is pressed
		if event.id eq state.select_button_default then begin
			if file.dim ne 2 or file.mult gt 1 then begin
				channel_default
				rgb_flag = 1
				state.select_chnr1 = channel_selec[0]
				state.select_chnr2 = channel_selec[1]
				state.select_chnr3 = channel_selec[2]
				widget_control,state.select_button_choose,set_value=1
				event.id = state.select_button_choose
			endif else begin
				widget_control,state.select_button_choose,set_value=0
				event.id = state.select_button_choose
			endelse
			flag_reset = 1
		endif else flag_reset = 0
;*******************************************************************************

;*******************************************************************************
	; display the appropriate widget depending of the choose button 
	if event.id eq state.select_button_choose then begin
		widget_control,state.select_button_choose,get_value=dummy
		state.select_flag_display = dummy
		widget_control,state.select_sub_chosen_band[0],map=1-state.select_flag_display
		widget_control,state.select_sub_chosen_band[1],map=state.select_flag_display
		if state.select_flag_display eq 1 then begin
			widget_control,state.select_text_R,set_value=channel_names[state.select_chnr1 mod (file.zdim*file.vdim)]
			widget_control,state.select_text_G,set_value=channel_names[state.select_chnr2 mod (file.zdim*file.vdim)]
			widget_control,state.select_text_B,set_value=channel_names[state.select_chnr3 mod (file.zdim*file.vdim)]
		endif
		if dummy eq 0 then begin
			event.id = state.but_appl
			widget_control,state.select_text_gray,set_value=channel_names[state.select_chnr1 mod (file.zdim*file.vdim)]
		endif
	endif
;*******************************************************************************

;*******************************************************************************
	; If apply is pressed and widget tab is select channel
		if (event.id eq state.but_appl and widget_info(state.wTab,/tab_current) eq 0) or (flag_reset eq 1) then begin
			if  (state.select_flag_display eq 0) then begin
				channel_selec = [state.select_chnr1] 
				color_flag = 0
			endif else begin
				channel_selec = [state.select_chnr1,state.select_chnr2,state.select_chnr3] 
				color_flag = 1
			endelse
				generate_preview,/nodefault,/noupdate ;;; ,/redisplay
		endif
;*******************************************************************************
		
;  ;*******************************************************************************
;  	; If color table is pressed
;  		if event.id eq state.select_but_color_table then begin  
;  			xloadct,updatecallback='select_channels_new_color_tab',updatecbdata=1,/use_current
;  		end
;  ;*******************************************************************************
		
;*******************************************************************************
;*******************************************************************************

;--------------------------- DATA MANAGEMENT TAB EVENT -------------------------
	
;*******************************************************************************
;*******************************************************************************

;*******************************************************************************	
	; double click on a file
	if state.data_main_name_realize then begin
		n_elem = n_elements(*state.data_leaf_wid)
		if event.id ge (*state.data_leaf_wid)[0] and event.id le (*state.data_leaf_wid)[n_elem-1] then begin
			if tag_names(event,/structure_name) eq 'WIDGET_TREE_SEL' then begin
				; display a preview if existing
				ind = where(event.id eq (*state.data_leaf_wid))
				if ind ne -1 then begin
					filename = config.workdir+(*state.data_leaf_filename)[ind]
					widget_control,state.data_preview_draw,get_value=draw_index
					rat_display_small_preview,filename,draw_index,state.data_preview_text,100,150
				endif else begin
					widget_control,state.data_preview_draw,get_value=draw_index
					rat_display_small_preview,'default',draw_index,state.data_preview_text,100,150
				endelse

				if event.clicks eq 2 then begin
					ind = where(event.id eq (*state.data_leaf_wid))
					if ind ne -1 then begin
						filename = config.workdir+(*state.data_leaf_filename)[ind]
						if filename ne file.name then begin
							widget_control, event.top, set_uvalue=state, /no_copy
							open_rat,inputfile = filename
							return
						endif
					endif
				endif
			endif 
		endif else begin
					widget_control,state.data_preview_draw,get_value=draw_index
					rat_display_small_preview,'default',draw_index,state.data_preview_text,100,150
		endelse

	endif
;*******************************************************************************
	
;*******************************************************************************
	; Select a file and display it
	if (event.id eq state.but_appl and widget_info(state.wTab,/tab_current) eq 1 and state.data_main_name_realize) then begin

		; check if a file is selected
		tmp = widget_info(state.data_root,/tree_select)
		ind = where(tmp eq (*state.data_leaf_wid))
		if ind ne -1 then begin
			
			; test if the file exist
			filename = config.workdir+(*state.data_leaf_filename)[ind]
			if file_test(filename) then begin
				
				; test what is the current file
				if filename ne file.name then begin
					widget_control, event.top, set_uvalue=state, /no_copy
					open_rat,inputfile = filename
					return
				endif else error_button = dialog_message('This file is already open',dialog_parent=wid.base,/error)
			endif	else error_button = DIALOG_MESSAGE(['This file does not exist'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		endif
		
	endif
;*******************************************************************************

;*******************************************************************************
	; ---- CREATE A NEW DATA MANAGEMENT FILE ----
	if event.id eq state.data_but_new then begin
	
		path = config.workdir
		filename = config.workdir+'data_management.txt'
		; draw the widget
		main = widget_base(group_leader=wid.base,title='Create a data management file',/modal,/column)
		name = cw_field(main,value='',/string,title='Main data information: ',xsize=50)
		buttons  = widget_base(main,/row,/frame)
		but_ok   = widget_button(buttons,value=' Ok ',xsize=80,/frame,uvalue='ok')
		but_canc = widget_button(buttons,value=' Cancel ',xsize=80)
		widget_control, main, /realize,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]
		repeat begin
  			event_add = widget_event(main)
  		endrep until (event_add.id eq but_ok) or (event_add.id eq but_canc)

		; Get the information
		widget_control,name,get_value=info

		; Destroy the main widget
		widget_control, main, /destroy
		
		if info eq '' and event_add.id eq but_ok then begin
			error_button = dialog_message(['You have to give a title !!!'],dialog_parent=wid.base,/error)
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		if event_add.id eq but_ok then begin
			; Create the file
			openw,ddd,filename,/get_lun
			printf,ddd,info
			free_lun,ddd
			widget_control, event.top, set_uvalue=state, /no_copy
			tool_box_update
			return
		endif
		
	endif
;*******************************************************************************


;*******************************************************************************
	; ---- REMOVE A FILE FROM THE THE DATA MANAGEMENT ----
	if event.id eq state.data_but_rem then begin
		
		; check if there is a data_management file
		filename = config.workdir+'data_management.txt'

		; check if a file is selected
		tmp = widget_info(state.data_root,/tree_select)
		ind = where(tmp eq *state.data_leaf_wid)
		
		if ind ne -1 then begin
			
			leaf_type = (*state.data_leaf_type)
			leaf_info = (*state.data_leaf_info)
			leaf_filename = (*state.data_leaf_filename)
			texte = 'Do you want to remove "'+leaf_info[ind]+': '+types[leaf_type[ind]]+'"'
			question_button = dialog_message(texte,dialog_parent=wid.base,/question)
		
			if question_button eq 'No' then begin
				widget_control, event.top, set_uvalue=state, /no_copy
				return
			endif
			
			
			leaf_type[ind] = 0
			
			; order the file
			ind_sort = sort(leaf_type)
			leaf_type = leaf_type[ind_sort]
			leaf_info = leaf_info[ind_sort]
			leaf_filename = leaf_filename[ind_sort]
			flag_empty = 0
			if n_elements(leaf_type) gt 1 then leaf_type = leaf_type[1:*] else flag_empty = 1
			if n_elements(leaf_info) gt 1 then leaf_info = leaf_info[1:*]
			if n_elements(leaf_filename) gt 1 then leaf_filename = leaf_filename[1:*]
			
			; insert the file in data management
			openr,ddd,filename,/get_lun
			line = ''
			readf,ddd,line
			free_lun,ddd
			openw,ddd,filename,/get_lun
			printf,ddd,line
			if flag_empty ne 1 then begin
				for ii = 0,n_elements(leaf_type)-1 do begin
					printf,ddd,strcompress(leaf_type[ii],/rem)+':'+leaf_info[ii]+':'+leaf_filename[ii]
				endfor
			endif
			free_lun,ddd
			widget_control, event.top, set_uvalue=state, /no_copy
			tool_box_update
			return
		endif
	endif
;*******************************************************************************

;*******************************************************************************
	; ---- ADD A FILE TO THE DATA MANAGEMENT ----
	if event.id eq state.data_but_add then begin
	
		; check if there is a data_management file
		filename = config.workdir+'data_management.txt'

		; Ask for the file
		path = config.workdir
		fileaddname = cw_rat_dialog_pickfile(title='Choose a rat file',dialog_parent=wid.base, filter = '*.rat',/must_exist,path=path)
		
		if fileaddname eq '' then begin
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		leaf_type = (*state.data_leaf_type)
		leaf_info = (*state.data_leaf_info)
		leaf_filename = (*state.data_leaf_filename)
		
		; check if the file is already in the list
		ind_tmp = where(file_basename(fileaddname) eq leaf_filename, count)
		if count ne 0 then begin
			error_button = dialog_message(['File already in the data management list'],dialog_parent=wid.base,/error)
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif

		; Check if the file is correct
		headfile=1l
		rrat,fileaddname,ddd,header=headfile,info=infofile,type=typefile
		if (typefile lt 100) or (typefile eq 390) then begin
			error_button = dialog_message(['Not a valid file'],dialog_parent=wid.base,/error)			
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
			

		; draw the widget
try_again:
		main = widget_base(group_leader=wid.base,title='add a file',/modal,/column)
		name = cw_field(main,value='',/string,title='File information: ',xsize=50)
		buttons  = widget_base(main,/row,/frame)
		but_ok   = widget_button(buttons,value=' Ok ',xsize=80,/frame,uvalue='ok')
		but_canc = widget_button(buttons,value=' Cancel ',xsize=80)
		
		widget_control, main, /realize,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]
		
	 	; Wait for an event
		repeat begin
  			event_add = widget_event(main)
  		endrep until (event_add.id eq but_ok) or (event_add.id eq but_canc)
		
		; Get the information
		widget_control,name,get_value=info

		; Destroy the main widget
		widget_control, main, /destroy
		
		if event_add.id eq but_canc then begin
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		if info eq '' then begin
			error_button = dialog_message(['You have to give File information !!!'],dialog_parent=wid.base,/error)
			goto,try_again
		endif
		
		if event_add.id eq but_ok then begin
			
			; add the file in the other file 
			leaf_type = [leaf_type,typefile]
			leaf_info = [leaf_info,info]
			leaf_filename = [leaf_filename,file_basename(fileaddname)]
			
			; order the file
			ind_sort = sort(leaf_type)
			leaf_type = leaf_type[ind_sort]
			leaf_info = leaf_info[ind_sort]
			leaf_filename = leaf_filename[ind_sort]
			flag_empty = 0
			
			ind = where(leaf_type ge 100, count)
			if count ne 0 then begin
				leaf_type = leaf_type[ind]
				leaf_info = leaf_info[ind]
				leaf_filename = leaf_filename[ind]
			endif
			
;  			if n_elements(leaf_type) gt 1 then leaf_type = leaf_type[1:*] else flag_empty = 1
;  			if n_elements(leaf_info) gt 1 then leaf_info = leaf_info[1:*]
;  			if n_elements(leaf_filename) gt 1 then leaf_filename = leaf_filename[1:*]
			
			; insert the file in data management
			openr,ddd,filename,/get_lun
			line = ''
			readf,ddd,line
			free_lun,ddd
			openw,ddd,filename,/get_lun
			printf,ddd,line
			if flag_empty ne 1 then begin
				for ii = 0,n_elements(leaf_type)-1 do begin
					printf,ddd,strcompress(leaf_type[ii],/rem)+':'+leaf_info[ii]+':'+leaf_filename[ii]
				endfor
			endif
			free_lun,ddd
			widget_control, event.top, set_uvalue=state, /no_copy
			tool_box_update
			return
		endif
	endif
;*******************************************************************************

;*******************************************************************************
	;---- CHANGE FILE INFORMATION ----
	if event.id eq state.data_but_ren then begin
		
		filename = config.workdir+'data_management.txt'
		tmp = widget_info(state.data_root,/tree_select)
		
		leaf_type = (*state.data_leaf_type)
		leaf_info = (*state.data_leaf_info)
		leaf_filename = (*state.data_leaf_filename)
		
		; check if it is the main data information
		if tmp eq state.data_main_name then title = 'Change main data information' else begin
			ind = where(tmp eq *state.data_leaf_wid)
			if ind ne -1 then title = 'Change file information' else begin
				widget_control, event.top, set_uvalue=state, /no_copy
				return
			endelse
		endelse
		
		main = widget_base(group_leader=wid.base,title=title,/modal,/column)
		name = cw_field(main,value='',/string,title='New information: ',xsize=50)
		buttons  = widget_base(main,/row,/frame)
		but_ok   = widget_button(buttons,value=' Ok ',xsize=80,/frame,uvalue='ok')
		but_canc = widget_button(buttons,value=' Cancel ',xsize=80)
		widget_control, main, /realize,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]
		repeat begin
  			event_add = widget_event(main)
  		endrep until (event_add.id eq but_ok) or (event_add.id eq but_canc)

		; Get the information
		widget_control,name,get_value=info

		; Destroy the main widget
		widget_control, main, /destroy
		
		if info eq '' and event_add.id eq but_ok then begin
			error_button = dialog_message(['You have to give an information !!!'],dialog_parent=wid.base,/error)
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		if event_add.id eq but_ok then begin
			ind = where(tmp eq *state.data_leaf_wid)
			if ind ne -1 then leaf_info[ind] = info
			
			; Make change in the file in data management
			if tmp ne state.data_main_name then begin
				openr,ddd,filename,/get_lun
				line = ''
				readf,ddd,line
				free_lun,ddd
			endif
			openw,ddd,filename,/get_lun
			if tmp eq state.data_main_name then printf,ddd,info else printf,ddd,line
			for ii = 0,n_elements(leaf_type)-1 do begin
				printf,ddd,strcompress(leaf_type[ii],/rem)+':'+leaf_info[ii]+':'+leaf_filename[ii]
			endfor
			free_lun,ddd
			widget_control, event.top, set_uvalue=state, /no_copy
			tool_box_update
			return
		
		endif
				
	endif

;*******************************************************************************

;*******************************************************************************
;*******************************************************************************

;--------------------------- COLOR TABLE EVENT -------------------------
	
;*******************************************************************************
;*******************************************************************************
;  
;*******************************************************************************

;*******************************************************************************
; --> Selected part is a folder
	for i=0,n_elements(*state.color_branch_wid)-1 do begin
		if event.id eq (*state.color_branch_wid)[i] then begin
			widget_control,state.color_button_base_add,map=0
			widget_control,state.color_button_base_modify,map=0
			widget_control,state.color_button_base_load,map=0
			widget_control,state.color_button_base_save,map=0
			widget_control,state.color_button_base_remove,map=0
			widget_control,state.color_draw_label,set_value="Defined color table"
			widget_control,state.but_appl,sensitive=1
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
	endfor

;*******************************************************************************
; --> Select a table and display it
 	if event.id ge (*state.color_leaf_wid)[0] and event.id le (*state.color_leaf_wid)[n_elements(pnames)-1] then begin
	
 		; Calculate the number of system palette
		number_system_palette = 0
		for ii=1,n_elements(pnames)-1 do begin
			dummy = strsplit(pnames[ii],':',/extract)
			if strcmp(dummy[0],"S") then number_system_palette++
		endfor

		
		index_color_table = where(*state.color_leaf_wid eq event.id)
;  		if file.dim eq 2l $
;  		  then widget_control,state.but_appl,set_value='Apply',sensitive=1 $
;  		  else widget_control,state.but_appl,set_value='Apply',sensitive=1
	
		if index_color_table eq 0 then begin
			widget_control,state.color_button_base_add,map=1
			widget_control,state.color_button_base_modify,map=0
			widget_control,state.color_button_base_load,map=0
			widget_control,state.color_button_base_save,map=0
			widget_control,state.color_button_base_remove,map=0
		endif
		if index_color_table ge 1 and index_color_table le number_system_palette then begin
			widget_control,state.color_button_base_add,map=0
			widget_control,state.color_button_base_modify,map=0
			widget_control,state.color_button_base_load,map=0
			widget_control,state.color_button_base_save,map=0
			widget_control,state.color_button_base_remove,map=0
		endif
		if index_color_table gt number_system_palette then begin
			widget_control,state.color_button_base_add,map=0
			widget_control,state.color_button_base_modify,map=1
			widget_control,state.color_button_base_load,map=1
			widget_control,state.color_button_base_save,map=1
			widget_control,state.color_button_base_remove,map=1
		endif
		;display the new table
		color_display_current_table,state.color_draw_selected,index_color_table,nb_color=nb_color
		if nb_color gt 100 then begin
			widget_control,state.color_draw_label,set_value="Defined color table: linear mode "
		endif else begin
			widget_control, state.color_draw_label, set_value="Defined color table: "+strcompress(nb_color,/rem)+" classes"
		endelse
	endif
;  

;*******************************************************************************
; --> Create a new user color table
	if event.id eq state.color_button_new then begin
		
		; create a dialog widget
		main = widget_base(group_leader=wid.base,title='Create an user color table',/modal,/column)
		name = cw_field(main,value='User defined '+strcompress(n_elements(pnames),/rem),/string,title='Color table name: ',xsize=50)
		buttons  = widget_base(main,/row,/frame)
		but_ok   = widget_button(buttons,value=' Ok ',xsize=80,/frame,uvalue='ok')
		but_canc = widget_button(buttons,value=' Cancel ',xsize=80)
		widget_control, main, /realize,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin
  			event_add = widget_event(main)
  		endrep until (event_add.id eq but_ok) or (event_add.id eq but_canc)
		
		widget_control,name,get_value=info_name
		widget_control, main,/destroy
		
		if event_add.id eq but_canc then begin
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		tvlct,old_r,old_g,old_b,/get
		error_button = dialog_message(['Please start the color choice at the index 0','For linear palette, use the button predefined palettes'],dialog_parent=wid.base,/error)
		xpalette,/block
		tvlct,new_r,new_g,new_b,/get
		tvlct,old_r,old_g,old_b
		
		if info_name eq "" then info_name = "User defined "+strcompress(n_elements(pnames),/rem)
		
		;--> add information to the palettes and pnames
		pnames = [pnames,"U:"+info_name]
		array_color_tmp = bytarr(1,256,3)
		array_color_tmp[0,*,0] = new_r
		array_color_tmp[0,*,1] = new_g
		array_color_tmp[0,*,2] = new_b
		palettes = [palettes,array_color_tmp]
		
		; --> save palettes.rat and palettes.txt
		srat,config.palettes,palettes
		openw,ddd,config.pnames,/get_lun
		for ii=0,n_elements(pnames)-1 do begin
			printf,ddd,pnames[ii]
		endfor
		free_lun,ddd
		
		; --> close and update and exit
		widget_control, event.top, set_uvalue=state, /no_copy
		tool_box_update
		return


	endif

;*******************************************************************************
; --> modify a user color table
	if event.id eq state.color_button_modify then begin
		
		tmp = widget_info(state.color_root,/tree_select)
		
		index = where(tmp eq *state.color_leaf_wid)
		table_r = reform(palettes[index,*,0])
		table_g = reform(palettes[index,*,1])
		table_b = reform(palettes[index,*,2])
	
		ind = where(table_r ne 0 or table_g ne 0 or table_b ne 0, ncouleur)
		ncouleur = ind[ncouleur-1]+1

		; create a dialog widget
		main = widget_base(group_leader=wid.base,title='Modify an user color table',/modal,/column)
		dummy = strsplit(pnames[index],':',/extract)
		name = cw_field(main,value=dummy[1],/string,title='Color table name: ',xsize=50)
		buttons  = widget_base(main,/row,/frame)
		but_ok   = widget_button(buttons,value=' Ok ',xsize=80,/frame,uvalue='ok')
		but_canc = widget_button(buttons,value=' Cancel ',xsize=80)
		widget_control, main, /realize,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin
  			event_add = widget_event(main)
  		endrep until (event_add.id eq but_ok) or (event_add.id eq but_canc)
		
		widget_control,name,get_value=info_name
		widget_control, main,/destroy

		if event_add.id eq but_canc then begin
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		tvlct,old_r,old_g,old_b,/get
		tvlct,table_r,table_g,table_b
		xpalette,/block
		tvlct,new_r,new_g,new_b,/get
		tvlct,old_r,old_g,old_b
		
		if info_name eq "" then info_name = "User defined "+strcompress(index,/rem)
		
		;--> add information to the palettes and pnames
		pnames[index] = "U:"+info_name
		palettes[index,*,0] = new_r
		palettes[index,*,1] = new_g
		palettes[index,*,2] = new_b
		
		; --> save palettes.rat and palettes.txt
		srat,config.palettes,palettes
		openw,ddd,config.pnames,/get_lun
		for ii=0,n_elements(pnames)-1 do begin
			printf,ddd,pnames[ii]
		endfor
		free_lun,ddd
		
		widget_control, event.top, set_uvalue=state, /no_copy
		tool_box_update
		return


	endif


;*******************************************************************************
; --> remove a user color table
	if event.id eq state.color_button_remove then begin
		
		tmp = widget_info(state.color_root,/tree_select)
		
		index = where(tmp eq *state.color_leaf_wid)

		dummy = strsplit(pnames[index],':',/extract)
		texte = 'Do you want to remove "'+dummy[1]+'"'
		question_button = dialog_message(texte,dialog_parent=wid.base,/question)
		
		if question_button eq 'No' then begin
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		
		;--> remove information to the palettes and pnames
		if n_elements(pnames)-1 eq index then begin
			pnames = reform(pnames[0:index-1])
			palettes = reform(palettes[0:index-1,*,*])
		endif else begin
			pnames = [pnames[0:index-1],pnames[index+1:*]]
			palettes = [palettes[0:index-1,*,*],palettes[index+1:*,*,*]]
		endelse
		if index lt n_elements(pnames) then begin
			for ii=index[0],n_elements(pnames)-1 do begin
				if strcmp(pnames[ii],"U:User defined "+strcompress(ii+1,/rem)) then pnames[ii] = "U:User defined "+strcompress(ii,/rem)
			endfor
		endif

		; --> save palettes.rat and palettes.txt
		srat,config.palettes,palettes
		openw,ddd,config.pnames,/get_lun
		for ii=0,n_elements(pnames)-1 do begin
			printf,ddd,pnames[ii]
		endfor
		free_lun,ddd
		
		widget_control, event.top, set_uvalue=state, /no_copy
		tool_box_update
		return

	endif

;*******************************************************************************
; --> add the current table to the user defined
	if event.id eq state.color_button_add then begin
		
		tmp = widget_info(state.color_root,/tree_select)
		
		index = where(tmp eq *state.color_leaf_wid)

		texte = 'Do you want to add the current table to the user defined?'
		question_button = dialog_message(texte,dialog_parent=wid.base,/question)
		
		if question_button eq 'No' then begin
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		
		;--> add information to the palettes and pnames
		; create a dialog widget
		main = widget_base(group_leader=wid.base,title='Create a user color table',/modal,/column)
		name = cw_field(main,value='User defined '+strcompress(n_elements(pnames),/rem),/string,title='Color table name: ',xsize=50)
		buttons  = widget_base(main,/row,/frame)
		but_ok   = widget_button(buttons,value=' Ok ',xsize=80,/frame,uvalue='ok')
		but_canc = widget_button(buttons,value=' Cancel ',xsize=80)
		widget_control, main, /realize,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin
  			event_add = widget_event(main)
  		endrep until (event_add.id eq but_ok) or (event_add.id eq but_canc)
		
		widget_control,name,get_value=info_name
		widget_control, main,/destroy
		
		if event_add.id eq but_canc then begin
			widget_control, event.top, set_uvalue=state, /no_copy
			return
		endif
		
		;--> add information to the palettes and pnames
		pnames = [pnames,"U:"+info_name]
		palettes = [palettes,palettes[0,*,*]]

		; --> save palettes.rat and palettes.txt
		srat,config.palettes,palettes
		openw,ddd,config.pnames,/get_lun
		for ii=0,n_elements(pnames)-1 do begin
			printf,ddd,pnames[ii]
		endfor
		free_lun,ddd
		
		widget_control, event.top, set_uvalue=state, /no_copy
		tool_box_update
		return

	endif
	
; --> Apply case
	if event.id eq state.but_appl  and widget_info(state.wTab,/tab_current) eq 2 then begin
		
		tmp = widget_info(state.color_root,/tree_select)
		index = where(tmp eq *state.color_leaf_wid)

		; --> Allocate the chosen color table to the first palette array
		if index ne -1 then palettes[0,*,*] = palettes[index,*,*]
		
		generate_preview,/nodefault,/noupdate ;;; ,/redisplay
		widget_control, event.top, set_uvalue=state, /no_copy
		tool_box_update
		return
		
	endif

;  
;  ;*******************************************************************************
;  	;Modify a color table
;  	if event.id eq state.color_but_mod then begin
;  		ind = widget_info(state.color_list_table,/list_select)
;  		if (*state.color_list_name).ncolor[ind] lt 256 then begin
;  			tvlct,ro,go,bo,/get
;  			loadct,(*state.color_list_name).pos[ind],file=config.prefdir+'user_color.tbl',/silent
;  			xpalette,/block;updatecallback='color_update_current_table',updatecbdata=ind;,/block
;    			tvlct,r,g,b,/get
;  			modifyct,(*state.color_list_name).pos[ind],(*state.color_list_name).table_name[ind],r,g,b,file=config.prefdir+'user_color.tbl'
;  			color_display_current_table,state.color_draw,(*state.color_list_name).pos[ind],(*state.color_list_name).ncolor[ind]
;  			tvlct,ro,go,bo
;  			if file.type eq (*state.color_list_name).type[ind] then generate_preview,/redisplay,/nodefault,/noupdate
;  		endif else begin
;  			tvlct,ro,go,bo,/get
;  			loadct,(*state.color_list_name).pos[ind],file=config.prefdir+'user_color.tbl',/silent
;  			xloadct,/use_current,/block
;  			tvlct,r,g,b,/get
;  			modifyct,(*state.color_list_name).pos[ind],(*state.color_list_name).table_name[ind],r,g,b,file=config.prefdir+'user_color.tbl'
;  			color_display_current_table,state.color_draw,(*state.color_list_name).pos[ind],(*state.color_list_name).ncolor[ind]
;  			tvlct,ro,go,bo
;  		endelse
;  	endif
;  ;*******************************************************************************
;  
;  ;*******************************************************************************
;  	;Restore the colortable original
;  	if event.id eq state.color_but_res then begin
;  		ind = widget_info(state.color_list_table,/list_select)
;  		result = dialog_message(['Are you sure to restore the original color table?'],dialog_parent=wid.base,/question) eq 'Yes'
;  		if result then begin
;  			tvlct,ro,go,bo,/get
;  			loadct,(*state.color_list_name).pos[ind],file=config.prefdir+'default_color.tbl',/silent
;  			tvlct,r,g,b,/get
;  			modifyct,(*state.color_list_name).pos[ind],(*state.color_list_name).table_name[ind],r,g,b,file=config.prefdir+'user_color.tbl'
;  			color_display_current_table,state.color_draw,(*state.color_list_name).pos[ind],(*state.color_list_name).ncolor[ind]
;  			tvlct,ro,go,bo
;  		endif
;  	endif
;  ;*******************************************************************************



;--------------------------------------------------------------------------------------------	
;--------------------------------------------------------------------------------------------
; EXIT THE WIDGET 	
;--------------------------------------------------------------------------------------------	
;--------------------------------------------------------------------------------------------

	; if OK button is pressed, then destroy the widget
	if event.id eq state.but_ok then widget_control,state.main,/destroy $
	else widget_control, event.top, set_uvalue=state, /no_copy
	
end 

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; MAIN PROGRAM
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

pro tool_box,select_channel=select_channel, data_management=data_management,color_table=color_table
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames
	
	
;---- TEST IF A DATASET IS LOADED ----
	if file.type eq 0 then begin
		error_button = DIALOG_MESSAGE(['Please load data'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
		
;---- TEST IF THE WIDGET IS STILL OPEN AND PUT IT FRONT ----
	if xregistered('tool_box') ne 0 then begin
	  	event = LookupManagedWidget('tool_box')
		widget_control, event, get_uvalue=state, /no_copy
		if keyword_set(select_channel) then widget_control,state.wTab,set_tab_current=0
		if keyword_set(data_management) then widget_control,state.wTab,set_tab_current=1
		if keyword_set(color_table) then widget_control,state.wTab,set_tab_current=2
		widget_control, event, set_uvalue=state, /no_copy
		return
	endif

;---- DEFINE THE STRUCTURE TO USE WITH EVENT ----
	state = {$
		main:0l,$
		wTab:0l,$
		
		;concerning the select channel
		select_wTab            : 0l,$
		select_tree            : 0l,$
		select_root            : ptr_new(),$
		select_leaf            : ptr_new(),$
		select_sub1            : 0l,$
		select_sub11           : 0l,$
		select_button_choose   : 0l,$
		select_button_default  : 0l,$
		select_sub2            : 0l,$
		select_sub_chosen_band : lonarr(2),$
		select_label           : 0l,$
		select_text_gray       : 0l,$
		select_sub3            : 0l,$
		select_but_color_table : 0l,$
		select_flag_display    : 0l,$
		select_flag_rgb        : 0l,$
		select_sub_L           : 0l,$
		select_button_rgb      : 0l,$
		select_sub_R           : 0l,$
		select_text_R          : 0l,$
		select_text_G          : 0l,$
		select_text_B          : 0l,$
		select_chnr1           : 0,$
		select_chnr2           : 0,$
		select_chnr3           : 0,$
		
		; widget concerning data management
	   data_wTab              : 0l,$ ; tab data management
		data_root              : 0l,$ ; define the main tree
		data_main_name         : 0l,$ ;define the name of the dataset
		data_main_name_realize : 0l,$ ;define the name of the dataset
		data_branch_wid        : ptr_new(),$ ; branch type folder
		data_leaf_wid          : ptr_new(),$ ; concerning the type and name associate to the dataset
		data_leaf_type         : ptr_new(),$ ; put the associated file type
		data_leaf_info         : ptr_new(),$ ; put the associated file info
		data_leaf_filename     : ptr_new(),$ ; put the associated file name
		data_foldertype        : ['Single channel SAR',$
			                       'Polarimetric SAR',$
			                       'Interferometric SAR',$
			                       'Classification results',$
			                       'Polarimetric interferometric SAR'],$
		data_ind_leaf          : 0l,$
		data_sub2              : 0l,$
		data_sub21             : 0l,$
		data_sub22             : 0l,$
		data_but_new           : 0l,$
		data_but_add           : 0l,$
		data_but_rem           : 0l,$
		data_but_ren           : 0l,$
		data_preview_draw      : 0l,$ ; draw a preview image if needed
		data_preview_text      : 0l,$ ; draw a preview image if needed
	
		; concerning the colortable management
		color_wTab               : 0l,$
		color_root               : 0l,$ ; define the main tree
		color_list_name          : ptr_new(),$
		color_main_name          : 0l,$ ;define the name of the dataset
		color_main_name_realize  : 0l,$ ;define the name of the dataset
		color_branch_wid         : ptr_new(),$ ; branch type folder
		color_leaf_wid           : ptr_new(),$ ; indicate the wid of user palettes
		color_leaf_name          : ptr_new(),$ ; put the associated file info
		color_foldertype         : ['System color tables',$
			                         'User defined color tables'],$
		color_draw_current       : 0l,$ ;display the current color palette
		color_draw_label         : 0l,$
		color_draw_selected      : 0l,$ ; display the selected palette 
		color_button_base        : 0l,$
		color_button_base_new    : 0l,$
		color_button_new         : 0l,$ ; define a new user palette
		color_button_base_add    : 0l,$
		color_button_add         : 0l,$ ; add the current palette to the user palette
		color_button_base_modify : 0l,$
		color_button_modify      : 0l,$ ; modify a user palette
		color_button_base_load   : 0l,$
		color_button_load        : 0l,$ ; load a saved color table
		color_button_base_save   : 0l,$
		color_button_save        : 0l,$ ; save a user color palette
		color_button_base_remove : 0l,$ ; remove a user color palette
		color_button_remove      : 0l,$ ; remove a user color palette
		
		; concerning the last button
		buttons:0l,$
		but_ok:0l,$
		buttons_appl:0l,$
		but_appl:0l,$
		but_info:0l $
		}
	
;---- DEFINE THE WIDGET ----

	geom = widget_info(wid.base, /geometry)
	state.main = widget_base(/column, title='Tool box',xoffset=geom.xoffset+geom.xsize+1,group_leader=wid.base)
		
	; --> Define the tab
	state.wTab = widget_tab(state.main)
	
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;									SELECT CHANNEL TO DISPLAY
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
	state.select_wTab = widget_base(state.wTab, title='Select channel', /column)
	
	; --> Define the select channel tree
	state.select_tree = widget_tree(state.select_wTab,ysize=360)
		
	; --> Define the main text for the tree
;	main_text = (file.info ne '') ? file.info : file_basename(file.name)
	
	layer_image = reverse(read_tiff(config.imagedir+'multi_layer_image.tif',interleave=2),2)
	
	if file.mult eq 1 then main_text = file_basename(file.name) else main_text = file_basename(extract_mtnames(file.name))
	select_root = lonarr(file.mult)
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

	; --> Analysis of channel settings (one or more channel)
	if (color_flag eq 0) or (file.dim eq 2) and not (file.mult gt 1) then begin
		rgb_flag = 0
		state.select_chnr1 = channel_selec[0]
		state.select_chnr2 = channel_selec[0]
		state.select_chnr3 = channel_selec[0]
	endif else begin
		rgb_flag = 1
		state.select_chnr1 = channel_selec[0]
		state.select_chnr2 = channel_selec[1]
		state.select_chnr3 = channel_selec[2]
	endelse

	; --> display normal - rgb - Default

	state.select_sub1 = widget_base(state.select_wTab,/row)
	state.select_sub11 = widget_base(state.select_sub1,map=(file.dim eq 2 and file.mult eq 1)?0:1)
	state.select_button_choose  = cw_bgroup(state.select_sub11,['One channel','RGB Color'],/exclusive,/row,set_value=rgb_flag,/no_release)
	state.select_button_default = widget_button(state.select_sub1,value=' Reset to default ')
			
	; --> display the chosen band
	state.select_sub2 = widget_base(state.select_wTab)
	
	; --> display depending if it is one mono/multi band
	dummy = widget_base(state.select_sub2,/column,/frame,map=1-rgb_flag)
		state.select_label= widget_label(dummy,value='Selected Band',/align_center)
		state.select_text_gray = widget_text(dummy,xsize=50,/frame,value=channel_names[state.select_chnr1 mod (file.zdim*file.vdim)])
		; define the color table button
		state.select_sub3 = widget_base(dummy,/row)
;  		state.select_but_color_table = widget_button(state.select_sub3,VALUE=' Select color tables... ')
		state.select_sub_chosen_band[0] = dummy
	
	dummy = widget_base(state.select_sub2,/row,/frame,map=rgb_flag)
		; define the three rgb exclusive button
		state.select_sub_L = widget_base(dummy,/column)
		state.select_button_rgb = cw_bgroup(state.select_sub_L,['R','G','B'],/exclusive,set_value=0,/no_release,space=13)

		; define the text for rgb image band
		state.select_sub_R = widget_base(dummy,/column)
		state.select_text_R = widget_text(state.select_sub_R,xsize=40,/frame,value=channel_names[state.select_chnr1 mod (file.zdim*file.vdim)])
		state.select_text_G = widget_text(state.select_sub_R,xsize=40,/frame,value=channel_names[state.select_chnr2 mod (file.zdim*file.vdim)])
;		state.select_text_B = widget_text(state.select_sub_R,xsize=40,/frame,value=channel_names[(file.zdim ge 3?state.select_chnr3:0)])
		state.select_text_B = widget_text(state.select_sub_R,xsize=40,/frame,value=channel_names[state.select_chnr3 mod (file.zdim*file.vdim)])
		state.select_sub_chosen_band[1] = dummy
	
	state.select_flag_display = rgb_flag

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;										DATA MANAGEMENT TAB
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

	state.data_wTab = widget_base(state.wTab, title='Data management', /column)

	; Test if a data_management file exist
	path = config.workdir
	filename = path+'data_management.txt'
	
	state.data_sub2 = widget_base(state.data_wTab,/row,/toolbar,xsize=330)
		state.data_sub21 = widget_base(state.data_sub2,/row,/toolbar,map=1 - file_test(filename))
			state.data_but_new = widget_button(state.data_sub21, value=config.imagedir+'new.bmp',/bitmap,tooltip='Create new data management file')
		state.data_sub22 = widget_base(state.data_sub2,/row,/toolbar,map=file_test(filename))
			state.data_but_add = widget_button(state.data_sub22, value=config.imagedir+'plus.bmp',/bitmap,tooltip='Add a file to the project')
			state.data_but_rem = widget_button(state.data_sub22, value=config.imagedir+'minus.bmp',/bitmap,tooltip='Remove a file from the project')
			state.data_but_ren = widget_button(state.data_sub22, value=config.imagedir+'prop.bmp',/bitmap,tooltip='Change information')
	
	; --> Define the data tree management
	state.data_root = widget_tree(state.data_wTab,ysize=340)
	
	if file_test(filename) then tool_box_data_management_file_read,filename,state
	

	; preview data
	data_preview_base = widget_base(state.data_wTab,/row)
		state.data_preview_draw = widget_draw(data_preview_base,xsize=100,ysize=150)
		;data_preview_subbase = widget_base(data_preview_base,/column,/frame,xsize=221)
;  			label1 = widget_label(data_preview_subbase,value='File info: toto.rat',/align_left)
;  			label1 = widget_label(data_preview_subbase,value='POLTOM - track 05 - HH - hamming - n',/align_left)
;  			label1 = widget_label(data_preview_subbase,value='o calib',/align_left)
;  			label2 = widget_label(data_preview_subbase,value='SAR complex image',/align_left)
;  			label3 = widget_label(data_preview_subbase,value='1390 x 6640   (complex)',/align_left)
;  			label3 = widget_label(data_preview_subbase,value='complex',/align_left,/sunken_frame)
;  	text = ['File info:',$
;  	        '----------','',$
;  			  'name: toto.rat','','POLTOM - track 05 - HH - hamming - n','o calib','',$
;  		'dimensions: 5 x 4 x 1390 x 6640','[complex]']
		state.data_preview_text = widget_text(data_preview_base,xsize=36,ysize=5)


;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;									  COLOR TABLE DEFINITION
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

	; --> Define the color table management tab
	state.color_wTab = widget_base(state.wTab, title='Color table management', /column)
	
	; --> Define the current color table used
	label = widget_label(state.color_wTab,value="Current color table",/align_left)
	state.color_draw_current = widget_draw(state.color_wTab,xsize=340,ysize=40)
	
	; --> Define the tree with color tables
	label = widget_label(state.color_wTab,value="",/align_left)
	label = widget_label(state.color_wTab,value="",/align_left)
	state.color_draw_label = widget_label(state.color_wTab,value="Defined color table",/align_left,xsize=300)
	state.color_draw_selected = widget_draw(state.color_wTab,xsize=340,ysize=40)
	
	; --> read and display all color table
	state.color_root = widget_tree(state.color_wTab,ysize=360)
	tool_box_color_table_management_read,state
	
	; --> Define the buttons
	state.color_button_base = widget_base(state.color_wTab,/row,/toolbar,xsize=330)
		state.color_button_base_new = widget_base(state.color_button_base,/row,/toolbar,/map)
;			state.color_button_new = widget_button(state.color_button_base_new, value=config.imagedir+'color_palette_new.bmp',/bitmap,tooltip='Create new color table')
			state.color_button_new = widget_button(state.color_button_base_new, value=' New ',tooltip='Create new color table')
		state.color_button_base_add = widget_base(state.color_button_base,/row,/toolbar,map=0)
;			state.color_button_add = widget_button(state.color_button_base_add, value=config.imagedir+'color_palette_add.bmp',/bitmap,tooltip='Add to the user defined color table')
			state.color_button_add = widget_button(state.color_button_base_add, value=' Add ',tooltip='Add to the user defined color table')
		state.color_button_base_modify = widget_base(state.color_button_base,/row,/toolbar,map=0)
;			state.color_button_modify = widget_button(state.color_button_base_modify, value=config.imagedir+'color_palette_prop.bmp',/bitmap,tooltip='Edit color table')
			state.color_button_modify = widget_button(state.color_button_base_modify, value=' Edit ',tooltip='Edit color table')
		state.color_button_base_load = widget_base(state.color_button_base,/row,/toolbar,map=0)
;			state.color_button_load = widget_button(state.color_button_base_load, value=config.imagedir+'color_palette_load.bmp',/bitmap,tooltip='Load a color table from a file')
			state.color_button_load = widget_button(state.color_button_base_load, value=' Load ',tooltip='Load a color table from a file')
		state.color_button_base_save = widget_base(state.color_button_base,/row,/toolbar,map=0)
;			state.color_button_save = widget_button(state.color_button_base_save, value=config.imagedir+'color_palette_save.bmp',/bitmap,tooltip='Save a color table to a file')
			state.color_button_save = widget_button(state.color_button_base_save, value=' Save ',tooltip='Save a color table to a file')
		state.color_button_base_remove = widget_base(state.color_button_base,/row,/toolbar,map=0)
;			state.color_button_remove = widget_button(state.color_button_base_remove, value=config.imagedir+'color_palette_minus.bmp',/bitmap,tooltip='Remove a color table')
			state.color_button_remove = widget_button(state.color_button_base_remove, value=' Delete ',tooltip='Remove a color table')
	

;  	; read the user_color_info
;  	color_arr = ''
;  	line = ''
;  	openr,ddd,config.pnames,/get_lun
;  	while ~ eof(ddd) do begin
;  		readf,ddd,line
;  		color_arr = [color_arr,line]
;  	endwhile
;  	free_lun,ddd
;  	color_arr = color_arr[1:*]
;  	n_elem = n_elements(color_arr)
;  	color_list_name = color_arr
;  	;{$
;  	;	type:lonarr(n_elem),$
;  	;	pos:lonarr(n_elem),$
;  	;	ncolor:lonarr(n_elem),$
;  	;	color_type:lonarr(n_elem),$
;  	;	table_name:strarr(n_elem)}
;  ;  	loadct,file=config.prefdir+'user_color.tbl',get_name=dummy
;  ;  	for ii=0,n_elem-1 do begin
;  ;  		line = strsplit(color_arr[ii],':',/extract)
;  ;  		color_list_name.type[ii] = line[0]
;  ;  		color_list_name.pos[ii] = line[1]
;  ;  		color_list_name.ncolor[ii] = line[2]
;  ;  		color_list_name.color_type[ii] = line[3]
;  ;  		color_list_name.table_name[ii] = dummy[line[1]]
;  ;  	endfor
;  ;  	ptr_free,state.color_list_name
;  ;  	state.color_list_name = ptr_new(color_list_name)
;  	
;  	state.color_wTab = widget_base(state.wTab, title='Color table management',/column)
;  	
;  	; define the current color table
;  	label = widget_label(state.color_wTab,value=color_list_name[0],/align_left)
;  	current_color_draw = widget_draw(state.color_wTab,xsize=340,ysize=20,/frame)
;  	
;  	label = widget_label(state.color_wTab,value='NOT YET IMPLEMENTED',/align_center)
;    	state.color_draw = widget_draw(state.color_wTab,xsize=340,ysize=20,/frame)
;  	
;  	
;   ;	state.color_list_table = widget_list(state.color_wTab,ysize=17,value=color_list_name.table_name)
;   	state.color_list_table = widget_list(state.color_wTab,ysize=17,value=color_list_name)
;  	widget_control,state.color_list_table,set_list_select=0
;  	state.color_buttons = widget_base(state.color_wTab,/row,/frame)
;  	state.color_but_mod = widget_button(state.color_buttons,value='Modify')
;  	state.color_but_res = widget_button(state.color_buttons,value='Restore default')
;  	
;  	
;  	state.color_but_ch = cw_bgroup(state.color_buttons,['linear','# colors'],/exclusive,/row,set_value=color_list_name.ncolor[0] lt 256)
;  	state.color_sub = widget_base(state.color_wTab,/row,/frame,map=color_list_name.ncolor[0] lt 256) ; penser au map
;  	state.color_label = widget_label(state.color_sub,value='# of colors: ')
;  	state.color_sub1 = widget_base(state.color_sub,/column,ysize=15,/toolbar)
;  	state.color_but_up = widget_button(state.color_sub1,value=config.imagedir+'shift_up.bmp',/bitmap,ysize=12)
;  	state.color_but_down=widget_button(state.color_sub1,value=config.imagedir+'shift_down.bmp',/bitmap,ysize=12)
;  	state.color_nb_color=widget_text(state.color_sub,value='0',xsize=3)
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;										 BUTTON ON THE BOTTOM
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
	state.buttons  = WIDGET_BASE(state.main,/row,/frame)
	state.but_ok   = WIDGET_BUTTON(state.buttons,VALUE=' Close ',xsize=80,/frame,uvalue='ok')
	state.but_appl = WIDGET_BUTTON(state.buttons,VALUE='Apply',xsize=80)
	state.but_info = WIDGET_BUTTON(state.buttons,VALUE=' Info ',xsize=60)
	

	widget_control, state.main, /realize,set_uvalue=state
	;if file.dim eq 2 then widget_control,state.select_button_choose,sensitive=0
	if keyword_set(select_channel) then begin
		widget_control,state.wTab,set_tab_current=0
		widget_control,state.but_appl,set_value='Apply',sensitive=1
	endif
	if keyword_set(data_management) then begin
		widget_control,state.wTab,set_tab_current=1
		widget_control,state.but_appl,set_value='Load',sensitive=1
	endif
	if keyword_set(color_table) then begin
		widget_control,state.but_appl,sensitive=1
		widget_control,state.wTab,set_tab_current=2
		widget_control,state.but_appl,set_value='Apply'
	endif
  	color_display_current_table,state.color_draw_current,0
	widget_control,state.data_preview_draw,get_value=index
 	color_display_current_table,state.color_draw_selected,-100
	widget_control,state.data_preview_draw,get_value=index
	
	rat_display_small_preview,'default',index,state.data_preview_text,100,150
	widget_control,wid.draw,get_value=index
	wset,index
	xmanager,'tool_box', state.main, /no_block
	
end 
