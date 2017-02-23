;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: whatisthis
; written by    : Andreas Reigber
; last revision : 9.Feb.2003
; Editing of file type and info string
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
pro whatisthis
	common rat, types, file, wid, config
;  	filetypes = types
;  	filetypes[1:99] = ''
;  	fileindex = where(filetypes ne '')
;  	startindex = where(fileindex eq file.type)
;  	filetypes = filetypes(fileindex)
;		drop   = widget_droplist(sub,value=filetypes,uvalue=filetypes, title="Data type : ",/align_left)

	;--> Define the pull down menu function of the types given by definitions
	index = where(types ne '', count)
	desc = ['1\Data Type: ']
	; Test data for generic data
	desc = [desc,'1\Generic']
	ind = where(index ge 50 and index lt 99, count)
	if count ne 0 then begin
		for ii=0,count-2 do begin
			desc = [desc,'0\'+types[index[ind[ii]]]]
		endfor
		desc = [desc,'2\'+types[index[ind[count-1]]]]
	endif

	; Test data for single channel SAR
	desc = [desc,'1\Single Channel SAR']
	ind = where(index ge 100 and index lt 199, count)
	if count ne 0 then begin
		for ii=0,count-2 do begin
			desc = [desc,'0\'+types[index[ind[ii]]]]
		endfor
		desc = [desc,'2\'+types[index[ind[count-1]]]]
	endif

	; Test data for Polarimetric SAR
	desc = [desc,'1\Polarimetric SAR']
	ind = where(index ge 200 and index le 299,count)
	if count ne 0 then begin
		for ii=0,count-2 do begin
			desc = [desc,'0\'+types[index[ind[ii]]]]
		endfor
		desc = [desc,'2\'+types[index[ind[count-1]]]]
	endif

	; Test data for Interferometric SAR
	desc = [desc,'1\Interferometric SAR']
	ind = where(index ge 300 and index le 399, count)
	if count ne 0 then begin
		for ii=0,count-2 do begin
			desc = [desc,'0\'+types[index[ind[ii]]]]
		endfor
		desc = [desc,'2\'+types[index[ind[count-1]]]]
	endif

	; Test data for Classification results
	desc = [desc,'1\Classification results']
	ind = where(index ge 400 and index le 499, count)
	if count ne 0 then begin
		for ii=0,count-2 do begin
			desc = [desc,'0\'+types[index[ind[ii]]]]
		endfor
		desc = [desc,'2\'+types[index[ind[count-1]]]]
	endif

	; Test data for Polarimetric interferometric SAR
	desc = [desc,'1\Polarimetric interferometric SAR']
	ind = where(index ge 500 and index le 599, count)
	if count ne 0 then begin
		for ii=0,count-2 do begin
			desc = [desc,'0\'+types[index[ind[ii]]]]
		endfor
		desc = [desc,'2\'+types[index[ind[count-1]]]]
	endif
		
	; Test data for Polarimetric interferometric SAR
; 	desc = [desc,'1\Multibaseline polarimetric interferometric SAR']
; 	ind = where(index ge 800 and index le 899, count)
; 	if count ne 0 then begin
; 		for ii=0,count-2 do begin
; 			desc = [desc,'0\'+types[index[ind[ii]]]]
; 		endfor
; 		desc = [desc,'2\'+types[index[ind[count-1]]]]
; 	endif




	;--> Generate the widget
	main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='What is this?',/floating,/tlb_kill_request_events,/tlb_frame_attr)

	field1 = CW_FIELD(main,VALUE=file.info,/string,TITLE='Content   : ',XSIZE=80)

	sub    = WIDGET_BASE(main,/row,/frame)
	drop   = cw_pdmenu(sub,desc,/return_name)
	label = widget_label(sub,value=types[file.type],/dynamic_resize)
;		drop2  = cw_bgroup(sub,'redisplay image',label_left='         redisplay image ',/nonexclusive)
	drop2  = cw_bgroup(main,['redisplay image','save info'],/nonexclusive,/row)


	
	
	redisp = 0
	
	;widget_control,drop,set_droplist_select=startindex
	
	buttons = WIDGET_BASE(main,/row,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		
	WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]

	repeat begin                                        ; Event loop
		event = widget_event(main)
		if event.id eq drop then widget_control,label,set_value=event.value
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	if event.id eq but_ok then begin
		widget_control,field1,GET_VALUE=info                ; read widget fields
		file.info = info

		;-> get new file information type
		widget_control,label,get_value=new_type
		ind = where(types eq new_type,count)
		if count ne 0 then file.type = ind
		update_info_box
		widget_control,drop2,GET_VALUE=redisp
		
		widget_control,main,/destroy                     ; remove main widget
		if redisp[0] eq 1 then begin
			WIDGET_CONTROL,/hourglass
			generate_preview,/recalculate
		endif
		if redisp[1] eq 1 then begin
			srat,file.name,update_info=[info, string(ind)]
		endif
	endif else widget_control,main,/destroy    

end
