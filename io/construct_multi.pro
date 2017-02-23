;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : consruct_multi.pro
; Author        : Marco Saemmang (Technical University Berlin, Germany)
; Last revision : March 2006
; Modified      : mn, 08/2006
; Module produces a new multifile or opens an existing multifile for
; changing order and number of files.
; Tests for allowed files are included.
; PRO construct_multi - dialog for selecting, changing etc.
; module for creating a new RAT-multifile or
; for change an existing multifile
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

pro construct_multi, CALLED = called
	common rat, types, file, wid, config
	common mt_analysis_new, files

		; create a structur for changing vars
		value ={text : ['Please add at least 2 Files'], $	; text before first file is opened
				count : 0l, $								; filecounter
				info_window : 0l, $							; window id for information-text
				wid_multi : 0l	$							; window_id
				}

		; pointer to structur - better for changing -> no SET_VALUE commands necessary
		value = PTR_NEW(value, /NO_COPY)

		IF KEYWORD_SET(called) THEN BEGIN
			; open multifile if current file is not a multifile
			IF file.mult LE 1 THEN BEGIN
				; ask for open or abort
				info = DIALOG_MESSAGE(['Current file is not a multifile!', 'Will you open a multifile?'], DIALOG_PARENT = wid.base, $
				TITLE ='Question', /QUESTION)
				; user wants to return
				IF info EQ 'No' THEN BEGIN
					RETURN
				; try to open a multifile
				ENDIF ELSE BEGIN
					inputfile = DIALOG_PICKFILE(TITLE = 'Open multifile', DIALOG_PARENT = (*value).wid_multi, $
					FILTER = '*.rat', /READ, /FIX_FILTER, /MUST_EXIST, PATH = config.workdir, GET_PATH = path)
					; return if no file was opened
					IF STRLEN(inputfile) EQ 0 THEN RETURN
					; open RAT-file and check if is a multifile or not
					rrat, inputfile, multifile, header = header, info = info, type = type, multi = multi
					; return if selected file is not a multifile
					IF multi LE 1 THEN BEGIN
						info = DIALOG_MESSAGE('Selected file is not a multifile!', DIALOG_PARENT = wid.base, TITLE = 'Information')
						FREE_LUN, multifile
				        RETURN
					ENDIF
				ENDELSE
			ENDIF ELSE BEGIN
				info = DIALOG_MESSAGE(['Will you change the current file?', 'Click NO for open another file'], $
				DIALOG_PARENT = wid.base, TITLE = 'Question', /QUESTION)
				IF info EQ 'No' THEN BEGIN
					; ask for filename
					inputfile = DIALOG_PICKFILE(TITLE = 'Open another multifile', DIALOG_PARENT = (*value).wid_multi, $
					FILTER = '*.rat', /READ, /FIX_FILTER, /MUST_EXIST, PATH = config.workdir, GET_PATH = path)
					IF STRLEN(inputfile) EQ 0 THEN RETURN
				ENDIF ELSE BEGIN
					; take currentfilename for multifile
					inputfile = file.name
				ENDELSE
				; open multifile for names, number and values
				rrat, inputfile, multifile, header = header, info = info, type = type, multi = multi
			ENDELSE
			; structure for subfile-details
			files = REPLICATE({name : '', info : '', typ : 0l, var : 0l, dim : 0l, x : 0l, y : 0l, z : 0l, v : 0l}, multi)

			; read subfile-names and save details in structure
			FOR index = 0, multi-1 DO BEGIN
				temp = ''
				READU, multifile, temp
				files[index].name = temp
				rrat, files[index].name, temp, header = header, info = info, type = type, multi = nr_files
				FREE_LUN, temp
				files[index].dim = header[0]
				files[index].x = header[header[0]-1]
				files[index].y = header[header[0]]
				IF files[index].dim GT 2 THEN files[index].z = header[header[0]-2]
				IF files[index].dim GT 3 THEN files[index].v = header[header[0]-3]
				files[index].var = header[header[0]+1]
				files[index].info = info
				files[index].typ = type
				; check if the files fit
				IF nr_files GT 1 THEN BEGIN
					FREE_LUN, multifile
					info = DIALOG_MESSAGE(['File ' + STRING(index+1) + ' is a multifile!', 'You need a datafile!'], $
					DIALOG_PARENT = wid.base, TITLE = 'Error')
					RETURN
				ENDIF
				IF files[index].dim NE files[0].dim THEN BEGIN
					FREE_LUN, multifile
					info = DIALOG_MESSAGE(['Dimensions of file ' + STRING(index+1) + ' are', 'not correct!'], $
					DIALOG_PARENT = wid.base, TITLE = 'Error')
					RETURN
				ENDIF
				IF (files[index].var NE files[0].var) OR (files[index].typ NE files[0].typ) THEN BEGIN
					FREE_LUN, multifile
					info = DIALOG_MESSAGE(['Datatype of file ' + STRING(index+1) + ' is', 'not correct!'], $
					DIALOG_PARENT = wid.base, TITLE = 'Error')
					RETURN
				ENDIF
			ENDFOR
			FREE_LUN, multifile
			(*value).count = multi
		; make structure (anonymous) for creating a new arrangement
		ENDIF ELSE BEGIN
			; count = 0l
			files = {name : ' ', info : ' ', typ : 0l, var : 0l, dim : 0l, x : 0l, y : 0l, z : 0l, v : 0l}
		ENDELSE

		(*value).wid_multi = WIDGET_BASE(GROUP_LEADER = wid.base, TLB_FRAME_ATTR = 1, $
		COLUMN = 2, TITLE = 'Create New Project', /TLB_KILL_REQUEST_EVENTS, XSIZE = 350, YSIZE = 250, /MODAL)

		; left for buttons - right for filenames
		leftbase = WIDGET_BASE((*value).wid_multi, /COLUMN, /FRAME)
		rightbase = WIDGET_TEXT((*value).wid_multi, /ALIGN_CENTER, XSIZE = 25, YSIZE = 13, /SCROLL, $
		UNAME = 'filelist')

		; buttons for creating and analysing the project
		add_button = WIDGET_BUTTON(leftbase, VALUE = 'Add file', UVALUE = 'add_but')
		up_button = WIDGET_BUTTON(leftbase, VALUE = 'Up', UVALUE = 'up_but')
		down_button = WIDGET_BUTTON(leftbase, VALUE = 'Down', UVALUE = 'down_but')
		del_button = WIDGET_BUTTON(leftbase, VALUE = 'Del last', UVALUE = 'del_but')
		info_button = WIDGET_BUTTON(leftbase, VALUE = 'Info', UVALUE = 'info')
		canc_button = WIDGET_BUTTON(leftbase, VALUE = 'Cancel', UVALUE = 'can_but')
		run_button = WIDGET_BUTTON(leftbase, VALUE = 'OK', UVALUE = 'run_but', FRAME=2)

		; set value to change the file-list in the info-window
		(*value).info_window = rightbase

		WIDGET_CONTROL, (*value).wid_multi, /REALIZE, XOFFSET = 375, YOFFSET = 250, SET_UVALUE = value
		IF (*value).count EQ 0 THEN WIDGET_CONTROL, rightbase, SET_VALUE = (*value).text
		IF (*value).count GT 0 THEN BEGIN
			; newline definition for infobox
			IF	config.os EQ 'windows' THEN newline = STRING(13B) + STRING(10B)
			IF	config.os EQ 'unix' THEN newline = STRING(10B)
			infotext = ''
			; create the current file-list
			FOR counter = 1,(*value).count DO BEGIN
				infotext = infotext + files[counter-1].name + newline
			ENDFOR
			WIDGET_CONTROL, (*value).info_window, SET_VALUE = infotext
		ENDIF

		XMANAGER, 'construct_multi', (*value).wid_multi
		PTR_FREE, value
END


; event-handler - reactions in case of widget (button) activity
PRO construct_multi_event, event
	COMMON rat, types, file, wid, config
	COMMON mt_analysis_new, files

	; newline definition for infobox
	IF	config.os EQ 'windows' THEN newline = STRING(13B) + STRING(10B)
	IF	config.os EQ 'unix' THEN newline = STRING(10B)
	infotext = ''

		; close the dialog by using the window_close_box
		IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			RETURN
		ENDIF

		; get the pointer of the file-array
		WIDGET_CONTROL, event.top, GET_UVALUE = value
		; check for uvalue
		WIDGET_CONTROL, event.id, GET_UVALUE = uvalue

		CASE uvalue OF
		'add_but': BEGIN
				; test-var to control open or about
				test = 0l
				; ask for name and path for inputfile
				inputfile = DIALOG_PICKFILE(TITLE = 'Add file', DIALOG_PARENT = (*value).wid_multi, $
				FILTER = '*.rat', /READ, /FIX_FILTER, /MUST_EXIST, PATH = config.workdir, GET_PATH = path)

				IF STRLEN(inputfile) NE 0 THEN BEGIN
					; get the path for current workdir
					config.workdir = path
					; test if user can open file
					test = FILE_TEST(inputfile, /READ)
					IF test EQ 0 THEN BEGIN
						d_info = DIALOG_MESSAGE('Can not open file!', DIALOG_PARENT = (*value).wid_multi, $
						TITLE = 'Information')
						RETURN
					ENDIF

					; read the projectfile-details and write details in the filearray
					rrat, inputfile, datafile, header = header, info = info, type = type
					files[(*value).count].dim = header[0]
                                        files[(*value).count].x = header[header[0]-1]
                                        files[(*value).count].y = header[header[0]]
                                        files[(*value).count].z = header[0] ge 3 ? header[header[0]-2] : 1L
                                        files[(*value).count].v = header[0] ge 4 ? header[header[0]-3] : 1L
					files[(*value).count].var = header[header[0]+1]
					files[(*value).count].typ = type
					files[(*value).count].info = info
					files[(*value).count].name = inputfile

					FREE_LUN, datafile

					; close if type ne 100 or 101 - show notice
                                        IF (type NE 100) && (type NE 101) && (type NE 103) $
                                           && ~(type ge 200 && type le 210) && ~(type ge 220 && type le 222) $
                                        then begin
                                           d_info = DIALOG_MESSAGE(['At the moment this modul works only with',$
                                                                    'scalar amplitude or complex images,', $
                                                                    'and with polarimetric scattering vectors', $
                                                                    'and covariance matrices.'], DIALOG_PARENT = (*value).wid_multi, $
                                                                   TITLE = 'Information')
						RETURN
					ENDIF

					; compare the following files with the first
					IF test NE 0 AND (*value).count GT 0 THEN BEGIN
						; message if file is already in project
						IF MAX(STRCMP(inputfile, files[0:(*value).count-1].name, /FOLD_CASE)) EQ 1 THEN BEGIN
							d_info = DIALOG_MESSAGE('File is already included.', DIALOG_PARENT = (*value).wid_multi, $
							TITLE = 'Information')
							RETURN
						ENDIF
						; message if the dimensions are not equal
						IF files[0].dim NE files[(*value).count].dim THEN BEGIN
							d_info = DIALOG_MESSAGE('File has wrong dimensions.', DIALOG_PARENT = (*value).wid_multi, $
							TITLE = 'Information')
							RETURN
						ENDIF
						; message if the types are not equal
						IF files[0].typ NE files[(*value).count].typ THEN BEGIN
							d_info = DIALOG_MESSAGE('File has wrong / inconsistent data type.', DIALOG_PARENT = (*value).wid_multi, $
							TITLE = 'Information')
							RETURN
						ENDIF
						; message if the type of numbers are not equal
						IF files[0].var NE files[(*value).count].var THEN BEGIN
							d_info = DIALOG_MESSAGE('File has wrong / inconsistent variable type.', DIALOG_PARENT = (*value).wid_multi, $
							TITLE = 'Information')
							RETURN
						ENDIF
						; message if the 3rd dimension is not equal
						IF files[0].dim GT 2 THEN BEGIN
							IF files[0].z NE files[(*value).count].z THEN BEGIN
								d_info = DIALOG_MESSAGE('File has wrong / inconsistent number of layers.', $
								DIALOG_PARENT = (*value).wid_multi, TITLE = 'Information')
								RETURN
							ENDIF
						ENDIF
						; message if the 4th dimension is not equal
						IF files[0].dim GT 3 THEN BEGIN
							IF files[0].v NE files[(*value).count].v THEN BEGIN
								d_info = DIALOG_MESSAGE('File has wrong / inconsistent number of layers.', $
								DIALOG_PARENT = (*value).wid_multi, TITLE = 'Information')
								RETURN
							ENDIF
						ENDIF
					ENDIF
					; add 1 to counter and resize the file array
					(*value).count +=1
					files = [files,files[0]]
				ENDIF
			END ; end 'add_but'

		'up_but': BEGIN
					; show notice that only one or no file/s is/are included
					IF (*value).count LT 2 THEN BEGIN
						d_info = DIALOG_MESSAGE('Which order you will change?', DIALOG_PARENT = (*value).wid_multi, $
						TITLE = 'Information')
						RETURN
					ENDIF ELSE BEGIN
						temp = files[0]
						FOR index = 0, (*value).count-2 DO files[index] = files[index+1]
						files[(*value).count-1] = temp
					ENDELSE
			END

		'down_but': BEGIN
					; show notice that only one or no file/s is/are included
					IF (*value).count LT 2 THEN BEGIN
						d_info = DIALOG_MESSAGE('Which order you will change?', DIALOG_PARENT = (*value).wid_multi, $
						TITLE = 'Information')
						RETURN
					ENDIF ELSE BEGIN
						temp = files[(*value).count-1]
						FOR index = 0, (*value).count-2 DO files[(*value).count-index-1] = files[(*value).count-index-2]
						files[0] = temp
					ENDELSE
			END

		'del_but': BEGIN
					; show notice that no files are included
					IF (*value).count LT 1 THEN BEGIN
						d_info = DIALOG_MESSAGE('No files to delete', DIALOG_PARENT = (*value).wid_multi, $
						TITLE = 'Information')
						RETURN
					ENDIF ELSE BEGIN
						(*value).count -=1
					ENDELSE
			END ; end 'del_but'

		'run_but': BEGIN
					; jump back if not 2 or more files are selected
					IF (*value).count LT 2 THEN BEGIN
						d_info = DIALOG_MESSAGE('At least 2 files needed', DIALOG_PARENT = (*value).wid_multi, $
						TITLE = 'Information')
						RETURN
					; start the analyze-process if 2 or more files are selected
					; with a warning that base-files could be changed
					ENDIF
					WIDGET_CONTROL, EVENT.TOP, /DESTROY
					; set the numbers of datafiles and run the file-creator
					file.mult = (*value).count
					save_multi, files[0:((*value).count-1)]
                                        open_rit,/EMPTY
                                        evolute,'Construct Multi-data.'

					RETURN
			END ; end 'run_but'

		'can_but': BEGIN
					WIDGET_CONTROL, EVENT.TOP, /DESTROY
					RETURN
			END ; end 'can_but'

		'info'	 : BEGIN
				dialog_text = ['Module for creating',$
				'A New Project',$
				'RAT module written 2005/06 by Marco Saemmang']
				info = DIALOG_MESSAGE(dialog_text, DIALOG_PARENT = (*value).wid_multi, TITLE='Information', /default_no)
			END ; end 'info'

			ELSE:
		ENDCASE

	; show the current file-list
	IF (*value).count EQ 0 THEN BEGIN
		WIDGET_CONTROL, (*value).info_window, SET_VALUE = (*value).text
	ENDIF ELSE BEGIN
		; create the current file-list
		FOR counter = 1,(*value).count DO BEGIN
			infotext = infotext + files[counter-1].name + newline
		ENDFOR
		WIDGET_CONTROL, (*value).info_window, SET_VALUE = infotext
	ENDELSE
END
