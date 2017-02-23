;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;                      GET DIRECTORIES AND FILES 
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
pro cw_rat_dialog_pickfile_prev_get_dir_info,filter,dirs,files,directory
	
	; --> search directory in the current directory
	dirs = file_search(/test_directory)
	dirs = ['.','..',dirs]
	
	if filter ne 'AllFiles' then begin
		files = (directory eq 0) ? file_search(filter) : file_search(filter,/test_directory) 
	endif else begin
		dummy = file_search(/mark_directory,count=count)
		if count ne 0 then begin
			ind = where(strmatch(dummy,'*/') eq 0,count)
			if count ne 0 then files = dummy[ind] else files =''
		endif else files = ''
		if directory ne 0 then files = dirs
	endelse
		
end

;-------------------------------------------------------------------------------
;                     READ THE DIRECTORIES DATABASE FILE 
;-------------------------------------------------------------------------------
pro cw_rat_dialog_pickile_prev_read_dirdb,dirdb_file,dirdb_arr,ind_dirdb_arr
	dirdb_arr = ['']
	line = ''
	ii=0
	if file_test(dirdb_file) then begin ;--> if file exists read it
		openr, ddd, dirdb_file, /get_lun
		while ~eof(ddd) and ii lt 15 do begin
			readf, ddd, line
			dirdb_arr = [dirdb_arr,line]
			ii++
		endwhile
		free_lun, ddd
	endif
	cd, current= cur_dir
	ind = where(dirdb_arr eq cur_dir, count)
	if count eq 0 then begin ; --> add the current directory and initialise index
		dirdb_arr[0] = cur_dir
		ind_dirdb_arr = ind
	endif else begin ;--> remove the first line and initialise index
		ind_dirdb_arr = ind-1
		dirdb_arr = dirdb_arr[1:*]
	endelse
end

;-------------------------------------------------------------------------------
;                     READ THE DIRECTORIES DATABASE FILE 
;-------------------------------------------------------------------------------
pro cw_rat_dialog_pickile_prev_update_dirdb,dirdb_file,dirdb_arr
	cd,current=cur_dir
	ind = where(dirdb_arr eq cur_dir, count)
	if count eq 0 then begin ;--> if file doesn't exist add it
		if n_elements(dirdb_arr) eq 15 then dirdb_arr = dirdb_arr[0:13] ;--> remove the last directory
		dirdb_arr = [cur_dir,dirdb_arr] ;--> add the new file
	endif
	
	; write into the database file
	openw, ddd, dirdb_file, /get_lun
	for ii=0, n_elements(dirdb_arr)-1 do begin
		printf,ddd,dirdb_arr[ii]
	endfor
	
	free_lun, ddd
end

;-------------------------------------------------------------------------------
;                               MAIN FUNCTION 
;-------------------------------------------------------------------------------
function cw_rat_dialog_pickfile_prev, $
	DIALOG_PARENT=dialog_parent, $
	DIRECTORY=directory, $
	FILTER=filter,$
	GET_PATH=get_path,$
	MUST_EXIST=must_exist,$
	OVERWRITE_PROMPT=overwrite_prompt,$
	PATH=path,$
	READ=read, $
	TITLE=title, $
	WRITE=write
	
	common rat, types, file, wid, config

; ---- WINDOWS CASE: USE NORMAL DIALOG_PICKFILE ----
	if config.os eq 'windows' then begin
		if not keyword_set(dialog_parent) then dialog_parent=0
		if not keyword_set(directory) then directory=0
		if not keyword_set(filter) then filter=0
		if not keyword_set(get_path) then get_path=0
		if not keyword_set(must_exist) then must_exist=0
		if not keyword_set(overwrite_prompt) then overwrite_prompt=0
		if not keyword_set(path) then path=0
		if not keyword_set(read) then read=0
		if not keyword_set(title) then title=0
		if not keyword_set(write) then write=0
		return,dialog_pickfile(dialog_parent=dialog_parent, directory=directory, filter=filter, get_path=get_path, must_exist=must_exist, overwrite_prompt=overwrite_prompt, path=path, read=read, title=title, write=write)
	endif
	
; ---- DEFINE SOME VARIABLES ----
	dirdb_file = config.prefdir+'rat_dirdb' ;--> directories database file
	
; ---- KEYWORD INITIALISATION ----
	if not keyword_set(title) then begin
		title='Please Select a File'
		if keyword_set(read) then title = title + ' for Reading'
		if keyword_set(write) then title = title + ' for Writing'
	endif
	if not keyword_set(directory) then directory=0
	if not keyword_set(filter) then filter = 'AllFiles'
	if not keyword_set(dialog_parent) then dialog_parent=0
	if keyword_set(path) then cd,path,current=org_dir else cd,current=org_dir
	cd,current=cur_dir

; ---- CHECK DIRECTORIES DATABASE ----
	cw_rat_dialog_pickile_prev_read_dirdb, dirdb_file, dirdb_arr, ind_dirdb_arr
	
; ---- DEFINE THE WIDGET ----
	; define the main widget
  	main = widget_base(group_leader=dialog_parent, /column, title=title)

	; define the recent directories
	label = widget_label(main, value='Recent directories', /align_left)
	;dirdb_drop = widget_combobox(main, value=dirdb_arr)
	dirdb_drop = widget_droplist(main,value=dirdb_arr)
	;widget_control, dirdb_drop, set_combobox_select = ind_dirdb_arr
	
	; if asked: display different filter
	if n_elements(filter) gt 1 then filter_drop = widget_droplist(main,value=filter,title='Filters:')
	
	; display the top directory 
	label = widget_label(main,value='Directory', /align_left)
	dir_text = widget_text(main,value=cur_dir,/editable,/kbrd_focus_event)
	
	; Display the two columns
	sub1 = widget_base(main,/row)
	
	; Left Column: filter and directory
	sub_L = widget_base(sub1, /column, xsize=135)
	
	; Display filter
	label = widget_label(sub_L, value='Filter', /align_left)
	dummy = (filter[0] eq 'AllFiles') ? '*' : filter[0]
	filter_text = widget_text(sub_L, value=dummy, /editable, /kbrd_focus_event)
	
	; Read information about files and directories
	cw_rat_dialog_pickfile_prev_get_dir_info,filter[0],dirs,files,directory			
	
	; Display directory
	label = widget_label(sub_L, value='Directory', /align_left)
	dirs_list = widget_list(sub_L, value=dirs, ysize=5)
	
	; Right column: files or directories
	sub_R = widget_base(sub1, /column)
	
	dummy = (directory ne 0) ? 'Directories' : 'Files'
	label = widget_label(sub_R, value=dummy, /align_left)
	files_list = widget_list(sub_R,value=files, ysize=9,scr_xsize=240)
	
	; Display the bottom files name selected
	label = widget_label(main, value='Selection', /align_left)
	selected_text = widget_text(main,/editable,/kbrd_focus_event)
	
	; Display the bottom button
	button = widget_base(main,/row,/frame)
	but_ok = widget_button(button,value=' OK ', /frame, xsize=50)
	but_filter = widget_button(button, value=' Filter ')
	but_cancel = widget_button(button, value=' Cancel ')
	
	; Realize the widget
	widget_control, main, /realize,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]

	flag_filt = 0 ; --> for the filter button, associate to a filter or a dir
	filter_index = 0
	repeat begin
		event = widget_event(main)
		
		; ---- CASE: RECENT DIRECTORIES ----
		if event.id eq dirdb_drop then begin
			if file_test(dirdb_arr[event.index],/directory) then begin ;--> Go to this directory
				cd, dirdb_arr[event.index]
				widget_control, filter_text, get_value=filter_tmp
				filter_tmp = (filter_tmp eq '' or filter_tmp eq '*') ? 'AllFiles' : filter_tmp
				cw_rat_dialog_pickfile_prev_get_dir_info,filter_tmp,dirs,files,directory
				widget_control, selected_text, set_value=''
				widget_control, dirs_list, set_value=dirs
				widget_control, files_list, set_value=files
				cd,current=cur_dir
				widget_control, dir_text, set_value=cur_dir
			endif
		endif

		; ---- CASE: FILTER (DROPLIST) ----
		if n_elements(filter) gt 1 then begin
			if event.id eq filter_drop then begin
				flag_filt = 1
				filter_index = event.index
				widget_control, filter_text, set_value=filter[filter_index]
				cw_rat_dialog_pickfile_prev_get_dir_info,filter[filter_index],dirs,files,directory
				widget_control, files_list, set_value=files
				widget_control, selected_text, set_value=''
			endif
		endif
		
		; ---- CASE: FILTER (TEXT) ----
		if event.id eq filter_text then begin
			flag_filt = 1
			widget_control, but_filter, set_value=' Filter '
			if tag_names(event,/structure_name) eq 'WIDGET_TEXT_CH' then begin
				widget_control, filter_text, get_value=dummy
				dummy = (dummy eq '' or dummy eq '*') ? 'AllFiles' : dummy
				cw_rat_dialog_pickfile_prev_get_dir_info,dummy,dirs,files,directory
				dummy = (dummy eq 'AllFiles') ? '*' : dummy
				widget_control, filter_text, set_value=dummy
				widget_control, files_list, set_value=files
				widget_control, selected_text, set_value=''
			endif
		endif
		
		; ---- CASE: DIRECTORY (TEXT) ----
		if event.id eq dir_text then begin
			flag_filt = 0
			widget_control, but_filter, set_value=' Go To '
			if tag_names(event,/structure_name) eq 'WIDGET_TEXT_CH' then begin
				widget_control, dir_text, get_value=dummy
				if file_test(dummy,/directory) then begin
					cd,dummy
					widget_control, filter_text, get_value=filter_tmp
					filter_tmp = (filter_tmp eq '' or filter_tmp eq '*') ? 'AllFiles' : filter_tmp
					cw_rat_dialog_pickfile_prev_get_dir_info,filter_tmp,dirs,files,directory
					widget_control, selected_text, set_value=''
					widget_control, dirs_list, set_value=dirs
					widget_control, files_list, set_value=files
					cd,current=cur_dir
				endif
			endif
		endif
		
		; ---- CASE: DIRECTORY (LIST) ----
		if event.id eq dirs_list then begin
			flag_filt = 0
			widget_control, but_filter, set_value=' Go To '
			if event.clicks eq 2 then begin
				new_path = cur_dir+'/'+dirs[event.index]
				cd,new_path
				widget_control, filter_text, get_value=filter_tmp
				filter_tmp = (filter_tmp eq '' or filter_tmp eq '*') ? 'AllFiles' : filter_tmp
				cw_rat_dialog_pickfile_prev_get_dir_info,filter_tmp,dirs,files,directory
				widget_control, selected_text, set_value=''
				widget_control, dirs_list, set_value=dirs
				widget_control, files_list, set_value=files
				cd,current=cur_dir
				widget_control, dir_text, set_value=cur_dir
			endif
		endif
		
		; ---- CASE: FILES ----
		if event.id eq files_list then begin
			widget_control,selected_text, set_value=files[event.index]
			if event.clicks eq 2 then event.id = but_ok
		endif
		
		; ---- CASE: SELECTED FILES ----
		if event.id eq selected_text then begin
			if tag_names(event,/structure_name) eq 'WIDGET_TEXT_CH' then event.id = but_ok
		endif
		
		; ---- CASE: FILTER OR GO TO ----
		if event.id eq but_filter then begin
			case flag_filt of
				0: begin
					dummy = widget_info(dirs_list,/list_select)
					if dummy ne -1 then begin
						new_path = cur_dir+'/'+dummy
						cd,new_path
						widget_control, filter_text, get_value=filter_tmp
						filter_tmp = (filter_tmp eq '' or filter_tmp eq '*') ? 'AllFiles' : filter_tmp
						cw_rat_dialog_pickfile_prev_get_dir_info,filter_tmp,dirs,files,directory
						widget_control, selected_text, set_value=''
						widget_control, dirs_list, set_value=dirs
						widget_control, files_list, set_value=files
						cd,current=cur_dir
						widget_control, dir_text, set_value=cur_dir
					endif
				end
				1: begin
					widget_control, filter_text, get_value=dummy
					dummy = (dummy eq '' or dummy eq '*') ? 'AllFiles' : dummy
					cw_rat_dialog_pickfile_prev_get_dir_info,dummy,dirs,files,directory
					dummy = (dummy eq 'AllFiles') ? '*' : dummy
					widget_control, filter_text, set_value=dummy
					widget_control, files_list, set_value=files
					widget_control, selected_text, set_value=''
				end
			endcase
		endif
		
		; ---- CASE: OVERWRITE_PROMPT ----
		if keyword_set(overwrite_prompt) and keyword_set(write) and (event.id eq but_ok) then begin
			widget_control, selected_text, get_value=filename
			filename = cur_dir+'/'+filename
			if file_test(filename) then begin
				mess = [filename+' already exists.','Do you want to replace it?']
				res = (dialog_message(mess,title=title,/question,dialog_parent=main)) eq 'No'
				if res then event.id = label
			endif
		endif

		; ---- CASE: MUST EXIST ----
		if keyword_set(must_exist) and (event.id eq but_ok) then begin
			widget_control, selected_text, get_value=filename
			filename = cur_dir+'/'+filename
			if not file_test(filename) then event.id = label
		endif
	endrep until (event.id eq but_ok) or (event.id eq but_cancel)
	
; ---- CASE: BUTTON OK ----
	if event.id eq but_ok then begin
		
		; get the filename
		widget_control,selected_text, get_value=filename
		cd,current=cur_dir
		
		; update the recent directories database
		cw_rat_dialog_pickile_update_dirdb,dirdb_file,dirdb_arr
		
		
		if filename eq '' then begin
			filename = cur_dir+'/'
		endif else begin
			filename = cur_dir+'/'+filename
			if directory ne 0 then filename += '/'
		endelse
		get_path = cur_dir+'/'
	endif else begin
		filename = ''
		get_path = ''
	endelse

	widget_control, main, /destroy
	cd,org_dir
	return,filename[0]
end
