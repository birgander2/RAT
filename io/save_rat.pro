;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: save_rat
; written by    : Andreas Reigber
; last revision : 14.Feb.2003
; Save current data set in RAT format
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

pro save_rat, OUTPUTFILE = outputfile, ADD_TOOL_BOX=add_tool_box, CALLED=called
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	if not keyword_set(outputfile) then begin
		path = config.workdir
                default_filters = ['*.rat','*.rat;*.mrat','*.mrat','*.rit']
		outputfile = cw_rat_DIALOG_PICKFILE(TITLE='Save RAT file', DIALOG_PARENT=wid.base, FILTER = default_filters, PATH=path, GET_PATH=path,/OVERWRITE_PROMPT,/write,add_tool_box=add_tool_box)
		if strlen(outputfile) gt 0 then config.workdir = path
             endif else path = file_dirname(outputfile)

	if not strcmp(outputfile,path) then begin
		if not strmatch(outputfile, '*.rat', /FOLD_CASE) then outputfile = outputfile + '.rat'

; change mousepointer
	
		WIDGET_CONTROL,/hourglass

; copy file to destination
		
		inputfile  = file.name
		
; read / write header

		head = 1l
		rrat,inputfile,ddd,header=head,info=info,type=type,multi=multi	
		srat,outputfile+'.part',eee,header=head,info=file.info,type=file.type,multi=multi		

; calculating preview size and number of blocks

		bs = config.blocksize
		calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
		blocksizes = intarr(anz_blocks)+bs
		blocksizes[anz_blocks-1] = bs_last
	
; pop up progress window

		progress,Message='Saving RAT file...'

;do the transform
		if file.mult gt 1 then begin  ; Multifile (contains only file names)
			for index=0,file.mult-1 do begin
				file_name = ''
				readu,ddd,file_name
				writeu,eee,file_name
;print,file_name
			endfor
		endif else begin ; Normal file with data
			for i=0,anz_blocks-1 do begin  
				progress,percent=(i+1)*100.0/anz_blocks
				block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
				readu,ddd,block
				writeu,eee,block
			endfor
		endelse
		free_lun,ddd,eee

		;--> Check if there is a preview image
		rrat,inputfile,image,/preview
		if (size(image))[0] ne 0 then srat,outputfile+'.part',image,/preview

		file_move,outputfile+'.part',outputfile,/overwrite
		file.name = outputfile

; save rit parameters (palette inclusive)
                save_rit,filename=outputfile

  		progress,/destroy
		
; update tool_box if necessary
		
		if keyword_set(add_tool_box) then begin
			filename = file_dirname(outputfile)+'/data_management.txt'
			
			;--> Generate the data_management file if necessary
			if not file_test(filename) then begin
				test_out = 0
				while not test_out do begin
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
						test_out = 0
					endif else test_out = 1
				endwhile
	
				if event_add.id eq but_ok then begin
					; Create the file
					openw,ddd,filename,/get_lun
					printf,ddd,info
					free_lun,ddd
				endif else return
			endif
			
			;--> Read the data management file
			line          = ''
			leaf_type     = [0l]
			leaf_info     = ['']
			leaf_filename = ['']
			
			openr,ddd,filename,/get_lun
			readf,ddd,line
			
			;read the first line
			main_info = line
			
			;read the entire file
			while ~ eof(ddd) do begin
				readf,ddd,line
				dummy = strsplit(line,':',/extract)
				ind_br = fix(dummy[0] /100.) - 1
	
				; add a file and analyse it if not exist delete it
				if file_test(file_dirname(outputfile)+'/'+dummy[2]) then begin
					leaf_type     = [leaf_type,long(dummy[0])]
					leaf_info     = [leaf_info,dummy[1]]
					leaf_filename = [leaf_filename,dummy[2]]
				endif
			endwhile
			free_lun,ddd
			
			if n_elements(leaf_type)     gt 1 then leaf_type     = leaf_type[1:*]
			if n_elements(leaf_info)     gt 1 then leaf_info     = leaf_info[1:*]
			if n_elements(leaf_filename) gt 1 then leaf_filename = leaf_filename[1:*]
			
			;--> Check if the file already exist
			ind_tmp = where(file_basename(file.name) eq leaf_filename, count)
			if count ne 0 then begin
				error_button = dialog_message(['File already in the data management list'],dialog_parent=wid.base,/question)
				if error_button eq 'No' then return
				test_out = 0
				while not test_out do begin
					main = widget_base(group_leader=wid.base,title='Change main data information',/modal,/column)
					name = cw_field(main,value='',/string,title='File information: ',xsize=50)
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
					if event_add.id eq but_canc then return
	
					if info eq '' and event_add.id eq but_ok then begin
						error_button = dialog_message(['You have to give File information !!!'],dialog_parent=wid.base,/error)
						test_out = 0
					endif else test_out = 1
				endwhile
				leaf_info[ind_tmp] = info
				
			endif else begin
				test_out = 0
				while not test_out do begin
					main = widget_base(group_leader=wid.base,title='add a file',/modal,/column)
					name = cw_field(main,value='',/string,title='File information: ',xsize=50)
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
					if event_add.id eq but_canc then return
	
					if info eq '' and event_add.id eq but_ok then begin
						error_button = dialog_message(['You have to give File information !!!'],dialog_parent=wid.base,/error)
						test_out = 0
					endif else test_out = 1
				endwhile
				
				;add the file
				leaf_type = [leaf_type,file.type]
				leaf_info = [leaf_info,info]
				leaf_filename = [leaf_filename,file_basename(file.name)]
			
				; order the file
				ind_sort = sort(leaf_type)
				leaf_type = leaf_type[ind_sort]
				leaf_info = leaf_info[ind_sort]
				leaf_filename = leaf_filename[ind_sort]
				
				ind = where(leaf_type ge 100, count)
				if count ne 0 then begin
					leaf_type = leaf_type[ind]
					leaf_info = leaf_info[ind]
					leaf_filename = leaf_filename[ind]
				endif
			endelse
			
			; insert the file in data management
			openr,ddd,filename,/get_lun
			line = ''
			readf,ddd,line
			free_lun,ddd
			openw,ddd,filename,/get_lun
			printf,ddd,line
			for ii = 0,n_elements(leaf_type)-1 do begin
				printf,ddd,strcompress(leaf_type[ii],/rem)+':'+leaf_info[ii]+':'+leaf_filename[ii]
			endfor
			free_lun,ddd
			tool_box_update
		endif
	endif	


; 	if file.dim eq 2 and not array_equal(reform(palettes[0,*,*]),[[bindgen(256)],[bindgen(256)],[bindgen(256)]]) ne 0 then begin
; 		colour = reform(palettes[0,*,*])
; 		cfile  = strmid(outputfile,0,strpos(outputfile,'.rat'))+'.pal'
; 		srat,cfile,colour,type=499
;              endif


		file.window_name = file_basename(file.name)
		widget_control,wid.base,base_set_title='RAT - Radar Tools: '+file.window_name

end
