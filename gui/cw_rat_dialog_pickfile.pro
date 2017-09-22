;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; CW_RAT_DIALOG_PICKFILE_REMOVE_INFO_DATABASE: Remove old information from database 
;  pro cw_rat_dialog_pickfile_remove_info_database,DatabaseArray,DatabaseIndex
;  	NumberElementArray = n_elements(DatabaseArray)
;  	case DatabaseIndex of
;  		1: DatabaseArray =
;  	endcase
;  end
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

;-------------------------------------------------------------------------------
; CW_RAT_DIALOG_PICKFILE_MKDIR: make a new directory in the current directory 
;-------------------------------------------------------------------------------
function cw_rat_dialog_pickfile_mkdir,parent
	
	out = 0
	repeat begin
		main = widget_base(group_leader=parent,title='Make a new directory',/modal,/column)
		name = cw_field(main,value='',/string,title='Name of new directory: ',xsize=60)
		buttons  = widget_base(main,/row,/frame)
		but_ok   = widget_button(buttons,value=' Ok ',xsize=80,/frame,uvalue='ok')
		but_canc = widget_button(buttons,value=' Cancel ',xsize=80)

		widget_control, main, /realize, tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		; Wait for an event
		repeat begin
			event_add = widget_event(main)
		endrep until (event_add.id eq but_ok) or (event_add.id eq but_canc)
	
		widget_control,name,get_value=info
		widget_control, main, /destroy
	
		if event_add.id eq but_ok then begin
			if info ne '' then begin
				cd,current=CurrentDirectory
				CurrentDirectory += '/'
				if file_test(CurrentDirectory+info,/directory) then begin
					out = 0
					error_button = dialog_message(['This directory already exist !!!'],dialog_parent=parent,/error)
				endif else begin
					if info eq '' then begin
						out = 0
						error_button = dialog_message(['Please Give a valid name !!!'],dialog_parent=parent,/error)
					endif else begin
						file_mkdir,info
						return,CurrentDirectory+info+'/'
					endelse
				endelse
			endif
		endif else return,''
	endrep until out eq 1
     end

;-------------------------------------------------------------------------------
;                      GET DIRECTORIES AND FILES 
;-------------------------------------------------------------------------------
function cw_rat_dialog_pickfile_get_dir_info,filterTemp,directory
	
	; --> search directory in the current directory
	dirs = file_search(/test_directory,count=count_dirs)+'/'
        
;        if n_elements(filterTemp) lt 2 then filter=filterTemp else filter=strsplit(filterTemp,';',/EXTRACT)
        filter=strsplit(filterTemp,';',/EXTRACT)

	if n_elements(filter) gt 1 || filter ne 'All Files' then begin
		files = (directory eq 0) ? file_search(filter) : file_search(filter,/test_directory) 
	endif else begin
		dummy = file_search(/mark_directory,count=count)
		if count ne 0 then begin
			ind = where(strmatch(dummy,'*/') eq 0,count)
			if count ne 0 then files = dummy[ind] else files =''
		endif else files = ''
	endelse
	if directory eq 0 then begin
		if count_dirs gt 0 then return,[dirs,files] else return,files
	endif else begin
		if count_dirs gt 0 then return,dirs else return,['']
	endelse
		

     end

;-------------------------------------------------------------------------------
; Get and display information in the new directory 
;-------------------------------------------------------------------------------
function cw_rat_dialog_pickfile_display_new_info,$
	CurrentDirectory,$
	FirstLineDirectoryDatabaseDrop,$
	SecondLineList,$
	ThirdLineSelDrop,$
	FourthLineFilterDrop,$
	DirectoryDatabaseArray,$
	FileDatabaseArray,$
	directory
	
	;Go to the new directory
	cd,CurrentDirectory
	
	;--> Take the correct filter
	FilterTemp = widget_info(FourthLineFilterDrop, /COMBOBOX_GETTEXT)
	if FilterTemp eq '' or FilterTemp eq '*' then FilterTemp = 'All Files'
	
	;--> Get information in the new directory
	FilesArray = cw_rat_dialog_pickfile_get_dir_info(FilterTemp,directory)
	
	; Display in the selection field combobox
	if directory ne 0 then FileDatabaseArray[0] = file_basename(CurrentDirectory) else FileDatabaseArray[0] = '*'
	widget_control,ThirdLineSelDrop,set_value=FileDatabaseArray

	; Display directory in Directory list
  	DirectoryDatabaseArray[0] = CurrentDirectory
	widget_control,FirstLineDirectoryDatabaseDrop,set_value=DirectoryDatabaseArray,set_combobox_select=0

	; Display information in the list file
	widget_control,SecondLineList,set_value=FilesArray
	
	; Return FilesArray information
	return,FilesArray
	
end


;-------------------------------------------------------------------------------
;                     READ THE DIRECTORIES DATABASE FILE 
;-------------------------------------------------------------------------------
function cw_rat_dialog_pickile_read_dirdb,DirectoryDatabaseFilename
	
	;--> Initialisation of variables
	DirectoryDatabaseArray = ['']
	line = ''
	ii=0
	
	;--> Read the Directory Database file if the file exist
	if file_test(DirectoryDatabaseFilename) then begin ;--> if file exists read it
		openr, ddd, DirectoryDatabaseFilename, /get_lun
		while ~eof(ddd) and ii lt 15 do begin
			readf, ddd, line
			DirectoryDatabaseArray = [DirectoryDatabaseArray,line]
			ii++
		endwhile
		free_lun, ddd
	endif
	
	;--> Put the current directory to the index 0 of DirectoryDatabaseArray
	cd, current= cur_dir
	cur_dir +='/'
	DirectoryDatabaseArray[0] = cur_dir
	return,DirectoryDatabaseArray

end

;-------------------------------------------------------------------------------
; CW_RAT_DIALOG_PICKILE_UPDATE_DIRDB: update the directories database file
;-------------------------------------------------------------------------------
pro cw_rat_dialog_pickile_update_dirdb,DirectoryDatabaseFilename,DirectoryDatabaseArray

	if n_elements(DirectoryDatabaseArray) gt 1 then begin
		
		subarray = DirectoryDatabaseArray[1:*]
		
		;--> Check if the current directory exist
		ind = where(DirectoryDatabaseArray[0] eq subarray, count)
		
		if count eq 0 then begin
			
			;--> Remove the last directory
			if n_elements(subarray) eq 15 then subarray = subarray[0:13]
			
			;--> Add the new directory
			subarray = [DirectoryDatabaseArray[0],subarray]
		endif 
		
	endif else subarray = DirectoryDatabaseArray[0]
	
	; write into the database file
	openw, ddd, DirectoryDatabaseFilename, /get_lun
	for ii=0, n_elements(subarray)-1 do printf,ddd,subarray[ii]
	free_lun, ddd
end

;-------------------------------------------------------------------------------
; CW_RAT_DIALOG_PICKILE_READ_FILEDB: read the last selected file database 
;-------------------------------------------------------------------------------
function cw_rat_dialog_pickile_read_filedb,FileDatabaseFilename
	
	;--> Initialisation of variables
	FileDatabaseArray = ['*']
	line = ''
	ii=0
	
	;--> Read the Directory Database file if the file exist
	if file_test(FileDatabaseFilename) then begin ;--> if file exists read it
		openr, ddd, FileDatabaseFilename, /get_lun
		while ~eof(ddd) and ii lt 15 do begin
			readf, ddd, line
			FileDatabaseArray = [FileDatabaseArray,line]
			ii++
		endwhile
		free_lun, ddd
	endif
	return,FileDatabaseArray
end

;-------------------------------------------------------------------------------
; CW_RAT_DIALOG_PICKILE_UPDATE_FILEDB: update the last selected file database 
;-------------------------------------------------------------------------------
pro cw_rat_dialog_pickile_update_filedb,FileDatabaseFilename,FileDatabaseArray

	cd,current=CurrentDirectory
	CurrentDirectory += '/'
	FileDatabaseArray[0] = CurrentDirectory+FileDatabaseArray[0]
	
	;--> check the number of element of FileDatabaseArray
	if n_elements(FileDatabaseArray) gt 1 then begin
		subarray = FileDatabaseArray[1:*]
		ind = where(FileDatabaseArray[0] eq subarray, count)
		if count eq 0 then begin
			;--> Remove the last file if needed
			if n_elements(subarray) eq 15 then subarray = subarray[0:13]
			;--> Add the new file if needed
			subarray = [FileDatabaseArray[0],subarray]
		endif
	endif else subarray = FileDatabaseArray
	
	;--> Save into the database file
	openw,ddd,FileDatabaseFilename,/get_lun
	for ii=0,n_elements(subarray)-1 do printf,ddd,subarray[ii]
	free_lun,ddd
end


;-------------------------------------------------------------------------------
;                               MAIN FUNCTION 
;-------------------------------------------------------------------------------
function cw_rat_dialog_pickfile, $
	DIALOG_PARENT=dialog_parent, $
	DIRECTORY=directory, $
	FILTER=filter,$
	GET_PATH=get_path,$
	MUST_EXIST=must_exist,$
	OVERWRITE_PROMPT=overwrite_prompt,$
	PATH=path,$
	READ=read, $
	TITLE=title, $
	WRITE=write,$
	NOPREVIEW=nopreview, $
	ADD_TOOL_BOX=add_tool_box
	
	common rat, types, file, wid, config

; ---- WINDOWS CASE: USE NORMAL DIALOG_PICKFILE ----
	if config.os eq 'windows' then begin
		if not keyword_set(dialog_parent) then dialog_parent=0
		if not keyword_set(directory) then directory=''
		if n_elements(filter) eq 0 then filter=''
		if not keyword_set(get_path) then get_path=0
		if not keyword_set(must_exist) then must_exist=0
		if not keyword_set(overwrite_prompt) then overwrite_prompt=0
		if not keyword_set(path) then path=''
		if not keyword_set(read) then read=0
		if not keyword_set(title) then title=''
		if not keyword_set(write) then write=0
		add_tool_box = 0;
		return,dialog_pickfile(dialog_parent=dialog_parent, directory=directory, filter=filter, get_path=get_path, must_exist=must_exist, overwrite_prompt=overwrite_prompt, path=path, read=read, title=title, write=write)
	endif

; ---- IDL VERSION LESS THAN 6.1 ----
	if float(strmid(!version.release,0,3)) lt 6.1 then begin
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
		if not keyword_set(nopreview) then nopreview=0		
		add_tool_box = 0;
		return,cw_rat_dialog_pickfile_prev(dialog_parent=dialog_parent, directory=directory, filter=filter, get_path=get_path, must_exist=must_exist, overwrite_prompt=overwrite_prompt, path=path, read=read, title=title, write=write)
	endif

; ---- KEYWORD INITIALISATION ----
	if not keyword_set(title) then begin
		title='Please Select a File'
		if keyword_set(read) then title = title + ' for Reading'
		if keyword_set(write) then title = title + ' for Writing'
	endif
	if not keyword_set(directory) then directory=0
	if n_elements(filter) eq 0 then filter = 'All Files' ; no filter then take all file
	if not keyword_set(dialog_parent) then dialog_parent=0
	if keyword_set(path) then cd,path,current=OriginalDirectory else cd,current=OriginalDirectory
	cd,current=CurrentDirectory
	CurrentDirectory += path_sep()


; ---- DEFINE SOME VARIABLES ----
	DirectoryDatabaseFilename = config.prefdir+'rat_dirdb' ;--> directories database file
	FileDatabaseFilename = config.prefdir+'rat_filedb' ;--> files database file (the 15 last files downloaded)
	
; ---- CHECK DIRECTORIES DATABASE ----
  	DirectoryDatabaseArray = cw_rat_dialog_pickile_read_dirdb(DirectoryDatabaseFilename)

; ---- CHECK FILES DATABASE ----
  	FileDatabaseArray = cw_rat_dialog_pickile_read_filedb(FileDatabaseFilename)


;---------------------------------------------------------------------
; DEFINE THE WIDGET
;---------------------------------------------------------------------
	;--> The global main base of the widget
	main = widget_base(group_leader=dialog_parent,/column,title=title,/modal)
	
	;--> Define the first line with button and recent directory
	FirstLineBase = widget_base(main,/row)
		FirstLineButtonBase = widget_base(FirstLineBase,/row,/toolbar)
			FirstLineParentButton    = widget_button(FirstLineButtonBase,value=config.imagedir+'up.bmp',/bitmap,tooltip='Parent directory')
			FirstLinePreviousButton  = widget_button(FirstLineButtonBase,value=config.imagedir+'prev.bmp',/bitmap,tooltip='Previous')
			FirstLineNextButton      = widget_button(FirstLineButtonBase,value=config.imagedir+'next.bmp',/bitmap,tooltip='Next')
			FirstLineReloadButton    = widget_button(FirstLineButtonBase,value=config.imagedir+'reload.bmp',/bitmap,tooltip='Reload current')
			FirstLineMkdirButton     = widget_button(FirstLineButtonBase,value=config.imagedir+'mkdir.bmp',/bitmap,tooltip='Create new directory')
		FirstLineDirectoryDatabaseDrop = widget_combobox(FirstLineBase, value=DirectoryDatabaseArray,/editable,xsize=460)
	
	; Read information about files and directories
	FilesArray = cw_rat_dialog_pickfile_get_dir_info(filter[0],directory)

	;--> Second line with list and preview
	SecondLineBase = widget_base(main,/row)
		SecondLineList = widget_list(SecondLineBase,value=FilesArray,xsize=35)
		SecondLinePreviewBase = widget_base(SecondLineBase,/column,frame=5)
			SecondLinePreviewUpBase = widget_base(SecondLinePreviewBase,/row)
				SecondLinePreviewUpDraw = widget_draw(SecondLinePreviewUpBase,xsize=100,ysize=150)
				SecondLinePreviewUpText = widget_text(SecondLinePreviewUpBase,xsize=36,ysize=5)
			SecondLinePreviewDownBase = widget_base(SecondLinePreviewBase,/row,/base_align_center)
				SecondLinePreviewDownDisplayButton = cw_bgroup(SecondLinePreviewDownBase,'Display preview',/nonexclusive,set_value=1)
				SecondLinePreviewDownDisplayAddDataManagementBase = widget_base(SecondLinePreviewDownBase,/row,/base_align_center)
				SecondLinePreviewDownDisplayAddDataManagementButton = cw_bgroup(SecondLinePreviewDownDisplayAddDataManagementBase,'Add to data management',/nonexclusive,set_value=0)
				
	;--> Third line with Selection and OK
	ThirdLineBase = widget_base(main,/row)
		ThirdLineLabel = widget_label(ThirdLineBase,value='Selection: ',/align_left)
		ThirdLineSelDrop = widget_combobox(ThirdLineBase,value=FileDatabaseArray,/editable,xsize=470)
		ThirdLineOKButton = widget_button(ThirdLineBase,value=' OK ',xsize=50,/frame)
	
	;--> Fourth line with filter and cancel
	FourthLineBase = widget_base(main,/row)
		FourthLineLabel = widget_label(FourthLineBase,value='Filters:   ',/align_left)
		FourthLineFilterDrop = widget_combobox(FourthLineBase,value=filter,/editable,xsize=470)
		FourthLineCancelButton = widget_button(FourthLineBase,value=' Cancel ',xsize=52)

	; Realize the widget
	widget_control, main, /realize,tlb_get_size=toto, default_button = FourthLineCancelButton
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]
	
	;display the default preview
	widget_control,SecondLinePreviewUpDraw,get_value=draw_index
	rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150

	
; ---- CHECK IF DISPLAY PREVIEW IS AVAILABLE ----
	if (filter[0] ne '*.rat' && filter[0] ne '*.rat;*.mrat' && filter[0] ne '*.mrat') or keyword_set(nopreview) then begin
		widget_control,SecondLinePreviewUpBase,map=0
		widget_control,SecondLinePreviewDownDisplayButton,sensitive=0,set_value=0
		widget_control,SecondLinePreviewDownDisplayAddDataManagementButton,sensitive=0,set_value=0
	endif

; ---- BUTTON MAKE DIRECTORY ONLY IF WRITE IS SELECTED ----
	if keyword_set(write) then begin
		widget_control,FirstLineMkdirButton,sensitive=1
		widget_control,SecondLinePreviewDownDisplayAddDataManagementBase,map=1
	endif else begin
		widget_control,FirstLineMkdirButton,sensitive=0
		widget_control,SecondLinePreviewDownDisplayAddDataManagementBase,map=0
	endelse

; ---- INITIALISATION OF PREVIOUS AND NEXT BUTTON ----
	PreviousDirectory = ['']
	NextDirectory = ['']
	widget_control,FirstLinePreviousButton,sensitive=0
	widget_control,FirstLineNextButton,sensitive=0

;--> initialise the add_tool_box button
	add_tool_box=0
	
	;--> Redefine the dimension of all widget to have a better display
	
;  	flag_filt = 0 ; --> for the filter button, associate to a filter or a dir
;  	filter_index = 0

;  	
	repeat begin
		event = widget_event(main)
			
		;--> DISPLAY PREVIEW
		if event.id eq SecondLinePreviewDownDisplayButton then widget_control,SecondLinePreviewUpBase,map=event.select
		
		;--> BUTTON PARENT DIRECTORY
		if event.id eq FirstLineParentButton then begin
			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			
			cd,current=CurrentDirectory
			CurrentDirectory += '/'
			;--> Update Previous Directory
			PreviousDirectory = [PreviousDirectory,CurrentDirectory]
			NextDirectory = ['']
			if file_test(file_dirname(CurrentDirectory),/directory) then begin
				CurrentDirectory = file_dirname(CurrentDirectory)+'/'
				FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
				widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
				
			endif
		endif
			
		;--> BUTTON PREVIOUS
		if event.id eq FirstLinePreviousButton then begin
			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			
			cd,current=CurrentDirectory
			CurrentDirectory += '/'
			NextDirectory = [NextDirectory,CurrentDirectory]
			;--> Go to the previous directory
			CurrentDirectory = PreviousDirectory[n_elements(PreviousDirectory)-1]
			FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
			widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
			
			;--> Update Previous and Next
			PreviousDirectory = PreviousDirectory[0:n_elements(PreviousDirectory)-2]
		endif
		
		;--> BUTTON NEXT
		if event.id eq FirstLineNextButton then begin
			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			
			cd,current=CurrentDirectory
			CurrentDirectory += '/'
			PreviousDirectory = [PreviousDirectory,CurrentDirectory]
			;--> Go to the previous directory
			CurrentDirectory = NextDirectory[n_elements(NextDirectory)-1]
			FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
			widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
			
			;--> Update Previous and Next
			NextDirectory = NextDirectory[0:n_elements(NextDirectory)-2]
		endif
		
		
		;--> BUTTON RELOAD
		if event.id eq FirstLineReloadButton then begin
			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			
			cd,current=CurrentDirectory
			CurrentDirectory += '/'
			FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
			widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
		endif
		
		;--> BUTTON MKDIR
		if event.id eq FirstLineMkdirButton then begin
			NewDir = cw_rat_dialog_pickfile_mkdir(main)
			if NewDir ne '' then begin
				cd,current=CurrentDirectory
				CurrentDirectory += '/'
				;display the default preview
				rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
				
				;--> Update Previous Directory
				PreviousDirectory = [PreviousDirectory,CurrentDirectory]
				NextDirectory = ['']
				
				CurrentDirectory = NewDir
				FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
				widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
			endif
		endif
		
		;--> CASE: RECENT DIRECTORIES
		if event.id eq FirstLineDirectoryDatabaseDrop then begin
			;--> Check if it is a correct directory

			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			
			if file_test(event.str,/directory) then begin
				cd,current=CurrentDirectory
				CurrentDirectory += '/'
				;--> Update Previous Directory
				PreviousDirectory = [PreviousDirectory,CurrentDirectory]
				NextDirectory = ['']
				
				CurrentDirectory = event.str
				FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
				widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
			endif
		endif
		
		;--> CASE: LIST
		if event.id eq SecondLineList then begin
			
			cd,current=CurrentDirectory
			CurrentDirectory += '/'

			; it is a directory --> Go to there
			if file_test(CurrentDirectory+FilesArray[event.index],/directory) then begin

				;--> Display default
				rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
				
				;--> Update Previous Directory
				cd,current=CurrentDirectory
				CurrentDirectory += '/'
				PreviousDirectory = [PreviousDirectory,CurrentDirectory]
				NextDirectory = ['']
				
				CurrentDirectory = CurrentDirectory+FilesArray[event.index]
  				FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
				widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
			endif else begin ;--> Should be a file
				FileDatabaseArray[0] = file_basename(CurrentDirectory+'/'+FilesArray[event.index])
				widget_control,ThirdLineSelDrop,set_value=FileDatabaseArray
				; Display the preview if needed
				widget_control,SecondLinePreviewDownDisplayButton,get_value=DisplayPreview
				if DisplayPreview eq 1 then begin
					widget_control,SecondLinePreviewUpDraw,get_value=draw_index
					text=' '
					rat_display_small_preview,CurrentDirectory+'/'+FilesArray[event.index],draw_index,SecondLinePreviewUpText,100,150
				endif
				if event.clicks eq 2 then event.id = ThirdLineOKButton ;--> Take the file
			endelse
		endif
		
		;--> CASE: SELECTION
		if event.id eq ThirdLineSelDrop then begin
			if event.index eq -1 or event.index eq 0 then begin
				event.id = ThirdLineOKButton
			endif else begin
				
				if file_test(FileDatabaseArray[event.index]) then begin
					;--> Display default
					rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
	
					;--> Update Previous Directory
					cd,current=CurrentDirectory
					CurrentDirectory += '/'
					PreviousDirectory = [PreviousDirectory,CurrentDirectory]
					NextDirectory = ['']
	
					CurrentDirectory = file_dirname(FileDatabaseArray[event.index])+ '/'
 					FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
					widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
					FileDatabaseArray[0] = file_basename(FileDatabaseArray[event.index])
					widget_control,ThirdLineSelDrop,set_value=FileDatabaseArray
					widget_control,SecondLinePreviewDownDisplayButton,get_value=DisplayPreview
					if DisplayPreview eq 1 then begin
						widget_control,SecondLinePreviewUpDraw,get_value=draw_index
						rat_display_small_preview,FileDatabaseArray[event.index],draw_index,SecondLinePreviewUpText,100,150
					endif
				endif
			endelse
		endif
		
		;--> CASE: FILTER
		if event.id eq FourthLineFilterDrop then begin
			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			
			if event.str eq '*.rat' || event.str eq '*.mrat' || event.str eq '*.rat;*.mrat' then begin
				widget_control,SecondLinePreviewUpBase,map=1
				widget_control,SecondLinePreviewDownDisplayButton,sensitive=1,set_value=1
				widget_control,SecondLinePreviewDownDisplayAddDataManagementButton,sensitive=1,set_value=0
			endif else begin
				widget_control,SecondLinePreviewUpBase,map=0
				widget_control,SecondLinePreviewDownDisplayButton,sensitive=0,set_value=0
				widget_control,SecondLinePreviewDownDisplayAddDataManagementButton,sensitive=0,set_value=0
			endelse
			cd,current=CurrentDirectory
			FilesArray = cw_rat_dialog_pickfile_display_new_info(CurrentDirectory,FirstLineDirectoryDatabaseDrop,SecondLineList,ThirdLineSelDrop,FourthLineFilterDrop,DirectoryDatabaseArray,FileDatabaseArray,directory)
			widget_control,FirstLineParentButton,sensitive=1-(CurrentDirectory eq '/')
		endif

		; ---- CASE: OVERWRITE_PROMPT ----
		if keyword_set(overwrite_prompt) and keyword_set(write) and (event.id eq ThirdLineOKButton) then begin
			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			
			;widget_control, selected_text, get_value=filename
			cd,current=CurrentDirectory
			filename = widget_info(ThirdLineSelDrop, /COMBOBOX_GETTEXT)
			filename = CurrentDirectory+'/'+filename
			if file_test(filename) then begin
				mess = [filename+' already exists.','Do you want to replace it?']
				res = (dialog_message(mess,title=title,/question,dialog_parent=main)) eq 'No'
				if res then event.id = ThirdLineLabel
			endif
		endif

		; ---- CASE: MUST EXIST ----
		if keyword_set(must_exist) and (event.id eq ThirdLineOKButton) then begin
			;display the default preview
			rat_display_small_preview,'default',draw_index,SecondLinePreviewUpText,100,150
			cd,current=CurrentDirectory
			filename = widget_info(ThirdLineSelDrop, /COMBOBOX_GETTEXT)
			if filename eq '*' then filename = ' '
			filename = CurrentDirectory+'/'+filename
			if not file_test(filename) then event.id = ThirdLineLabel
		endif
		
		
		;--> Test of previous and next directory
		case n_elements(PreviousDirectory) of
			1:widget_control,FirstLinePreviousButton,sensitive=0
			else:widget_control,FirstLinePreviousButton,sensitive=1
		endcase
		
		case n_elements(NextDirectory) of
			1:widget_control,FirstLineNextButton,sensitive=0
			else:widget_control,FirstLineNextButton,sensitive=1
		endcase

		;--> ADD TOOL BOX
		if event.id eq SecondLinePreviewDownDisplayAddDataManagementButton then add_tool_box = event.select

	endrep until (event.id eq ThirdLineOKButton) or (event.id eq FourthLineCancelButton)
	
; ---- CASE: BUTTON OK ----
	if event.id eq ThirdLineOKButton then begin
		
		; get the filename
		filename = widget_info(ThirdLineSelDrop, /COMBOBOX_GETTEXT)
		cd,current=CurrentDirectory
		
		; update the recent directories database
		cw_rat_dialog_pickile_update_dirdb,DirectoryDatabaseFilename,DirectoryDatabaseArray
		cw_rat_dialog_pickile_update_filedb,FileDatabaseFilename,FileDatabaseArray
		
		
		if filename eq '' or filename eq '*' then begin
			filename = CurrentDirectory+'/'
		endif else begin
			filename = CurrentDirectory+'/'+filename
			if directory ne 0 then filename += '/'
		endelse
		get_path = CurrentDirectory+'/'
	endif else begin
		filename = ''
		get_path = ''
	endelse

	widget_control, main, /destroy
	cd,OriginalDirectory
	widget_control,wid.draw,get_value=index
	wset,index
	return,filename
end
