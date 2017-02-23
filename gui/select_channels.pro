;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: select_channels
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
pro select_channels
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag
	
	if file.dim eq 2 then begin
		error_button = DIALOG_MESSAGE(['Not a multilayer file'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	main = widget_base(group_leader=wid.base,/COLUMN, TITLE='Select channels',/modal) 
	tree = widget_tree(main,xsize=200,ysize=32*file.vdim*file.zdim>100) 
	main_text = (file.info ne '') ? file.info : file_basename(file.name)
	root = widget_tree(tree, value=main_text, /FOLDER, /EXPANDED,bitmap=multi_layer_image()) 
	
	leaf  = lonarr(file.zdim*file.vdim)
	
	for i=0,file.vdim-1 do begin
		for j=0,file.zdim-1 do begin
			index =i*file.zdim+j 
			leaf[index] = widget_tree(root, value=channel_names[index],bitmap=file_image())
		endfor
	endfor

; Analysis of channel settings
	
	if color_flag eq 0 then begin
		rgb_flag = 0 
		chnr1 = channel_selec[0]
		chnr2 = channel_selec[0]
		chnr3 = channel_selec[0]
	endif else begin
		rgb_flag = 1		
		chnr1 = channel_selec[0]
		chnr2 = channel_selec[1]
		chnr3 = channel_selec[2]
	endelse
	
;display gray - rgb - I don't know

	sub1 = widget_base(main,column = 2)
	button_choose  = cw_bgroup(sub1,['Single channel','RGB Color'],/exclusive,/row,set_value=rgb_flag,/no_release)
	button_default = widget_button(sub1,value=' Reset to default ',/align_center,ysize = 35)
			
; display the choosen band
	
	sub2 = widget_base(main,scr_ysize=150)
	if rgb_flag eq 0 then begin
		sub_choosen_band = widget_base(sub2,/column,/frame)
		label = widget_label(sub_choosen_band,value='Selected Band',/align_center)
		text_gray = widget_text(sub_choosen_band,xsize=50,/frame,value=channel_names[chnr1])
		flag_display = 0
	endif else begin	
		sub_choosen_band = widget_base(sub2,/row,/frame)
		sub_L = widget_base(sub_choosen_band,/column)
		button_rgb = cw_bgroup(sub_L,['R','G','B'],/exclusive,set_value=0,/no_release,space=13)
		sub_R = widget_base(sub_choosen_band,/column)
		text_R = widget_text(sub_R,xsize=45,/frame,value=channel_names[chnr1])
		text_G = widget_text(sub_R,xsize=45,/frame,value=channel_names[chnr2])
		text_B = widget_text(sub_R,xsize=45,/frame,value=channel_names[chnr3])
		flag_display = 1
	endelse
	
	
	
	buttons  = WIDGET_BASE(main,/row,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_appl = WIDGET_BUTTON(buttons,VALUE=' Apply ',xsize=80,/frame)
	but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)


	; Realize the widgets and run XMANAGER to manage them. 
	WIDGET_CONTROL, main, /REALIZE,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]

	; Initialisation of different variables
	flag_rgb = 0
	
	repeat begin                                        ; Event loop
		event = widget_event(main)
		if event.id ge leaf[0] and event.id le leaf[file.zdim*file.vdim-1] then begin
			chnr = where(leaf eq event.id)
;			mes = main_text+': '+channel_names[chnr]
			mes = channel_names[chnr]
  			case flag_display of
				0 : begin
					widget_control,text_gray,set_value=mes
					chnr1 = chnr
					chnr2 = chnr
					chnr3 = chnr
				end
				1 : begin
					case flag_rgb of
						0 : begin
							widget_control,text_R,set_value=mes
							chnr1 = chnr
						end
						1 : begin
							widget_control,text_G,set_value=mes
							chnr2 = chnr
						end	
						2 : begin
							widget_control,text_B,set_value=mes
							chnr3 = chnr
						end
					endcase
					flag_rgb = (flag_rgb+1) mod 3
					widget_control, button_rgb, set_value=flag_rgb
				end
  			endcase
		endif

		if flag_display eq 1 then begin
			if event.id eq button_rgb then widget_control, button_rgb, get_value=flag_rgb
		endif
		
		if event.id eq button_default then begin
			channel_default
			rgb_flag = 1		
			chnr1 = channel_selec[0]
			chnr2 = channel_selec[1]
			chnr3 = channel_selec[2]
			widget_control,button_choose,set_value=1
			event.id = button_choose
		endif


		if event.id eq button_choose then begin
			widget_control,button_choose,get_value=flag_display
			case flag_display of 
				0 : begin
					widget_control,root,sensitive=1
					WIDGET_CONTROL, sub_choosen_band, /DESTROY
					sub_choosen_band = widget_base(sub2,/column,/frame)
					label = widget_label(sub_choosen_band,value='Selected Band',/align_center)
					text_gray = widget_text(sub_choosen_band,xsize=50,/frame,value=channel_names[chnr1])
					WIDGET_CONTROL, sub_choosen_band, /REALIZE
				end
				1 : begin
					widget_control,root,sensitive=1
					WIDGET_CONTROL, sub_choosen_band, /DESTROY
					sub_choosen_band = widget_base(sub2,/row,/frame)
					sub_L = widget_base(sub_choosen_band,/column)
					button_rgb = cw_bgroup(sub_L,['R','G','B'],/exclusive,set_value=0,/no_release,space=13)
					sub_R = widget_base(sub_choosen_band,/column)
					text_R = widget_text(sub_R,xsize=45,/frame,value=channel_names[chnr1])
					text_G = widget_text(sub_R,xsize=45,/frame,value=channel_names[chnr2])
					text_B = widget_text(sub_R,xsize=45,/frame,value=channel_names[chnr3])
					WIDGET_CONTROL, sub_choosen_band, /REALIZE
				end
;  				2 : begin
;  					widget_control,root,sensitive=0
;  					WIDGET_CONTROL, sub_choosen_band, /DESTROY
;  					sub_choosen_band = widget_base(sub2,/column)
;  					WIDGET_CONTROL, sub_choosen_band, /REALIZE
;  				end
			endcase
		endif
		
		if event.id eq but_appl then begin
			if  (flag_display eq 0) then begin
				channel_selec = [chnr1] 
				color_flag = 0
			endif else begin
				channel_selec = [chnr1,chnr2,chnr3] 
				color_flag = 1
			endelse
			generate_preview,/nodefault ;;; ,/redisplay
		endif
		
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['Channel selection',$
			' ',$
			'RAT module written 05/2004 by S. Guillaso and A. Reigber']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
	endrep until (event.id eq but_ok) or (event.id eq but_canc)
	
	WIDGET_CONTROL, main, /DESTROY
	
	if (event.id eq but_ok) and (flag_display eq 0) then begin
		channel_selec = [chnr1] 
		color_flag = 0
		generate_preview,/nodefault  ;;; ,/redisplay
	endif
	if (event.id eq but_ok) and (flag_display eq 1) then begin
		channel_selec = [chnr1,chnr2,chnr3] 
		color_flag = 1
		generate_preview,/nodefault  ;;; ,/redisplay
	endif
	
end
