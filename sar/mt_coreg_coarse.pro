;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : mt_coreg_coarse
; Author        : Marco Saemmang (Technical University Berlin, Germany)
; Last revision : March 2006
; Read a multifile and estimate the offset for coregistration.
; If user wants, RAT coregists the subfiles.
; If user wants, RAT resizes the subfiles to a common size an saves them.
; FUNCTION coreg_coarse  - for estimate single offsets
; PRO mt_common_size     - for finding common size and resizing the subfiles
; PRO mt_coreg_coarse    - dialog for estimating, shifting and resizing
; module for coregistrating (coarse) and resizing multifiles
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

FUNCTION coreg_coarse, arr1, arr2
	bsx = (SIZE(arr1))[1]
	bsy = (SIZE(arr1))[2]
	auxcor = ABS(FFT(FFT(ABS(arr1),-1)*CONJ(FFT(ABS(arr2),-1)),+1))
	aux = MAX(auxcor, auxpos)
	offsetx = auxpos MOD bsx
	offsety = auxpos  /  bsx
	IF (offsetx GT bsx/2) THEN offsetx = offsetx-bsx
	IF (offsety GT bsy/2) THEN offsety = offsety-bsy
	RETURN,[offsetx,offsety]
END

PRO mt_common_size, file_name
COMMON rat, types, file, wid, config

	; open multifile for names and number
	rrat, file_name, multifile, header = header, info = info, type = type, multi = nr_files
	mfiles = MAKE_ARRAY(nr_files, /STRING)
	temp = ''
	FOR index = 0, nr_files-1 DO BEGIN
		READU, multifile, temp
		mfiles[index] = temp
	ENDFOR
	FREE_LUN, multifile

	; make array for start and end for x and y - xs, xe, ys, ye
	offsets = MAKE_ARRAY(nr_files, 4, /LONG)
	; read block to find start and end in x
	; find good blocksize
	bs_x = 0l
	bs_y = 0l
	IF config.blocksize LE header[header[0]-1] THEN bs_x = config.blocksize ELSE bs_x = FLOOR(header[header[0]-1]/2)
	IF config.blocksize LE header[header[0]] THEN bs_y = config.blocksize ELSE bs_y = FLOOR(header[header[0]]/2)
	; find startposition for reading block
	start_x = LONG(FLOOR((header[header[0]-1]-bs_x)/2))
	start_y = LONG(FLOOR((header[header[0]]-bs_y)/2))
	; calculate blocksize for reading block in y with using rrat
	calc_blocks_normal, header[header[0]], bs_y, nr_blocks, bs_last
	blocksizes = INTARR(nr_blocks) + bs_y
	blocksizes[nr_blocks-1] = bs_last

	; show status for estimate the common size
	progress, MESSAGE = 'Estimate common size ...'

	; check subfiles
	FOR index = 0, nr_files-1 DO BEGIN
		; status for progress
		progress, percent = (index+1)*100.0/nr_files
		; value for offset in x and y
		pos_xs = 0l
		pos_xe = header[header[0]-1]-1
		pos_ys = 0l
		pos_ye = header[header[0]]-1
		; read block for searching in x and search first row ne 0
		rrat, mfiles[index], arr_x, block = [0, start_y, header[header[0]-1], bs_y]
		arr_comp = TRANSPOSE(MAKE_ARRAY(bs_y, type = header[header[0]+1]))
		WHILE ARRAY_EQUAL(arr_x[pos_xs, *], arr_comp) EQ 1 DO pos_xs++
		WHILE ARRAY_EQUAL(arr_x[pos_xe, *], arr_comp) EQ 1 DO pos_xe--
		offsets[index, 0] = pos_xs
		offsets[index, 1] = pos_xe
		; read block for searching in y and search first column ne 0 - use rrat
		arr_y = MAKE_ARRAY(bs_x, header[header[0]], type = header[header[0]+1])
		arr_comp = MAKE_ARRAY(bs_x, type = header[header[0]+1])
		FOR blockindex = 0, nr_blocks-1 DO BEGIN
			rrat, mfiles[index], temp, block = [start_x, LONG(TOTAL(blocksizes[0:blockindex])-blocksizes[blockindex]), bs_x, blocksizes[blockindex]]
			arr_y[*, (blockindex*bs_y):(blockindex*bs_y+blocksizes[blockindex]-1)] = temp
		ENDFOR
		WHILE ARRAY_EQUAL(arr_y[*, pos_ys], arr_comp) EQ 1 DO pos_ys++
		WHILE ARRAY_EQUAL(arr_y[*, pos_ye], arr_comp) EQ 1 DO pos_ye--
		offsets[index, 2] = pos_ys
		offsets[index, 3] = pos_ye
	ENDFOR
	progress, /DESTROY

	; find common start- and endposition
	pos_xs = MAX(offsets[*,0])
	pos_xe = MIN(offsets[*,1])
	pos_ys = MAX(offsets[*,2])
	pos_ye = MIN(offsets[*,3])

	; resize the files
	IF (pos_xs NE 0) OR (pos_ys NE 0) OR (pos_xe NE header[header[0]-1]-1) OR (pos_ye NE header[header[0]]-1) THEN BEGIN

		; show window for status of resizing
		progress, MESSAGE = 'Resizing files ....'

		; calculate dx and dy - new delta  (difference) in x and y
		dx = pos_xe - pos_xs + 1
		dy = pos_ye - pos_ys + 1

		; new blocksize for new dimension
		calc_blocks_normal, dy, config.blocksize, nr_blocks, bs_last
		blocksizes = INTARR(nr_blocks) + config.blocksize
		blocksizes[nr_blocks-1] = bs_last
		IF bs_last EQ 0 THEN nr_blocks -=1

		; resize files
		FOR index = 0, nr_files-1 DO BEGIN
			; save changes in workfile1 temporary
			outputfile = config.tempdir + config.workfile1
			; show resize-status
			progress, percent = (index+1)*100.0/nr_files
			; read original subfile-header
			rrat, mfiles[index], temp, header = header, type = type, info = info
			FREE_LUN, temp

			; only the files with other dimensions
			IF (header[header[0]-1] NE dx) OR (header[header[0]] NE dy) THEN BEGIN
				; set new values for subfile-header and write modified header
				header[header[0]-1] = dx
				header[header[0]] = dy
				srat, outputfile, subfile, header = header, type = type, info = info
				; resize blockwise
				FOR blockindex = 0, nr_blocks-1 DO BEGIN
					rrat, mfiles[index], temp, block = [pos_xs, pos_ys + LONG(TOTAL(blocksizes[0:blockindex])-blocksizes[blockindex]), dx, blocksizes[blockindex]]
					WRITEU, subfile, temp
				ENDFOR
				;close modified subfile
				FREE_LUN, subfile
				FILE_MOVE, config.tempdir + config.workfile1, mfiles[index], /OVERWRITE, /ALLOW_SAME
			ENDIF
		ENDFOR

		; destroy status display
		progress, /DESTROY

		; create new multifile-header - save changes
		rrat, file.name, temp, header = header, type = type, info = info, multi = multi
		FREE_LUN, temp
		header[header[0]-1] = dx
		header[header[0]] = dy
		srat, config.tempdir + config.workfile1, temp, header = header, type = type, info = 'coregistered and resized multifile', multi = multi
		WRITEU, temp, mfiles
		FREE_LUN, temp
		FILE_MOVE, config.tempdir + config.workfile1, file.name, /ALLOW_SAME, /OVERWRITE

		; set values for file. variables
		file.window_name = FILE_BASENAME(file.name)
		file.xdim = dx
		file.ydim = dy
		widget_control,wid.base,base_set_title='RAT - Radar Tools: '+file.window_name

		generate_preview, /recalculate
		update_info_box
	ENDIF
END

PRO mt_coreg_coarse, called = called, offset_x = offset_x, offset_y = offset_y
	COMMON rat, types, file, wid, config

	; check the file.type is a multifile
	IF file.mult LE 1 THEN BEGIN
		d_info = DIALOG_MESSAGE('This is not a multifile!', DIALOG_PARENT = wid.base, TITLE = 'Information', /error)
		RETURN
	ENDIF

	; open multifile for details
	; read header
	header = 1l
	rrat, file.name, inputfile, header = header, info = info, type = type, multi = number
	; read filenames
	names = MAKE_ARRAY(number, /STRING)
	temp_name = ''
	FOR counter = 0, number-1 DO BEGIN
		READU, inputfile, temp_name
		names[counter] = temp_name
		; remove write-protection (subfiles)
		FILE_CHMOD, temp_name, /A_WRITE
	ENDFOR
	; close multifile
	FREE_LUN, inputfile
	; remove write-protection (multifile)
	FILE_CHMOD, file.name, /A_WRITE

	; set offsets if not handed over
	IF NOT KEYWORD_SET(offset_x) THEN offset_x = MAKE_ARRAY(number, /INTEGER)
	IF NOT KEYWORD_SET(offset_y) THEN offset_y = MAKE_ARRAY(number, /INTEGER)

	IF NOT KEYWORD_SET(called) THEN BEGIN
		; create widgets for coregistration-dialog
		main = WIDGET_BASE(GROUP_LEADER = wid.base, ROW = 3, TITLE = 'Image coregistration (coarse)',/floating,/tlb_kill_request_events,/tlb_frame_attr)

		base_man = WIDGET_BASE(main, ROW = 8, /FRAME, XSIZE = 300)
		base_est = WIDGET_BASE(main, ROW = 4, /FRAME, XSIZE = 300)
		base_but = WIDGET_BASE(main, COLUMN = 3, /FRAME, XSIZE = 300)

		; create subwindow for manual offset
		field_man = WIDGET_LABEL(base_man, VALUE = 'Look or set values for offset')
		master_txt = WIDGET_LABEL(base_man, VALUE = 'Masterfile')
		master_name = WIDGET_TEXT(base_man, VALUE = FILE_BASENAME(names[0]), /ALIGN_CENTER, XSIZE = 38)
		slave_txt = WIDGET_LABEL(base_man, VALUE = 'Offset for file')
		field_name = WIDGET_TEXT(base_man, VALUE = FILE_BASENAME(names[1]), /ALIGN_CENTER, XSIZE = 38)

		field_x = CW_FIELD(base_man, VALUE = offset_x[0], /INTEGER, TITLE = 'Offset in x : ', XSIZE = 5)
		field_y = CW_FIELD(base_man, VALUE = offset_y[0], /INTEGER, TITLE = 'Offset in y : ', XSIZE = 5)
		; fix index-value for open dialog
		index = 2
		; temp for get_value in the repeat-loop
		temp = 0
		slider = WIDGET_SLIDER(base_man, VALUE = index, MAXIMUM = number, MINIMUM = 1, TITLE = ' File ')

		; create subwindow for estimate
		field_est = WIDGET_LABEL(base_est, VALUE = 'Let RAT estimate offset values:')
		field_boxx = CW_FIELD(base_est, VALUE = 128, /INTEGER, TITLE = 'Max offset in x to check :', XSIZE =5 )
		field_boxy = CW_FIELD(base_est, VALUE = 1024, /INTEGER, TITLE = 'Max offset in y to check :', XSIZE = 5)
		but_est = WIDGET_BUTTON(base_est, VALUE = ' Estimate (Memory-Intensiv) ', /ALIGN_CENTER, XSIZE = 292, /FRAME)

		; create subwindow for buttons
		but_ok = WIDGET_BUTTON(base_but, VALUE = ' OK ', XSIZE = 140)
		but_cancel = WIDGET_BUTTON(base_but, VALUE = ' Cancel ', XSIZE = 75)
		but_info = WIDGET_BUTTON(base_but, VALUE = ' Info ', XSIZE = 75)

		;realize dialog-box
		WIDGET_CONTROL, main, /REALIZE, default_button = but_cancel, tlb_get_size = toto
		; center the dialog-box
		pos = CENTER_BOX(toto[0], drawysize = toto[1])
		WIDGET_CONTROL, main, xoffset = pos[0], yoffset = pos[1]

		; loop for reaction of dialog activities
		REPEAT BEGIN
			IF index EQ 1 THEN BEGIN
				WIDGET_CONTROL, field_x, SENSITIV = 0
				WIDGET_CONTROL, field_y, SENSITIV = 0
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, field_x, SENSITIV = 1
				WIDGET_CONTROL, field_y, SENSITIV = 1
			ENDELSE
			; set the actual values for offsets and filename
			WIDGET_CONTROL, field_x, SET_VALUE = offset_x[index-1]
			WIDGET_CONTROL, field_y, SET_VALUE = offset_y[index-1]
			WIDGET_CONTROL, field_name, SET_VALUE = FILE_BASENAME(names[index-1])

			event = WIDGET_EVENT(main)
			; destroy widget if kill_request-button
			IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
				WIDGET_CONTROL, event.top, /DESTROY
				RETURN
			ENDIF
			; close if cancel-button is changed
			IF event.id EQ but_cancel THEN BEGIN
				WIDGET_CONTROL, event.top, /DESTROY
				RETURN
			ENDIF
			; text for info-button
			IF event.id EQ but_info THEN BEGIN
				infotext = ['COARSE IMAGE COREGISTRATION', $
				' ', $
				'RAT module written January 2006 by Marco Saemmang.']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			END
			; calculate offset
			IF event.id EQ but_est THEN BEGIN
				WIDGET_CONTROL, /hourglass
				; get values for boxsize
				WIDGET_CONTROL, field_boxx, GET_VALUE = maxoff_x
				WIDGET_CONTROL, field_boxy, GET_VALUE = maxoff_y

				; find good blocksize automaticly (for fast calculate)
				bs_x = 2l
				WHILE 2l^bs_x LT maxoff_x DO bs_x++
				WHILE 2l^bs_x GT header[header[0]-1] DO bs_x--
				bs_x = 2l^bs_x
				bs_y = 2l
				WHILE 2l^bs_y LT maxoff_y DO bs_y++
				WHILE 2l^bs_y GT header[header[0]] DO bs_y--
				bs_y = 2l^bs_y

				; open progress window for showing status of estimate
				progress, MESSAGE = 'Estimate offsets ...', /CANCEL_BUTTON

				; create arrays for reading and calculating
				arr_base = MAKE_ARRAY(bs_x, bs_y, TYPE = header[header[0]+1])
				arr_comp = MAKE_ARRAY(bs_x, bs_y, TYPE = header[header[0]+1])

				progress, percent = 100.0/number, /CHECK_CANCEL
				; calculate number of blocks and blocksizes for reading
				calc_blocks_normal, bs_y, config.blocksize, nr_blocks, bs_last
				blocksizes = INTARR(nr_blocks)+config.blocksize
				blocksizes[nr_blocks-1] = bs_last

				; read basearray fisrt
				FOR counter = 0l, nr_blocks-1 DO BEGIN
					; if cancel-button is activated then back
					IF wid.cancel EQ 1 THEN RETURN
					; position for reading in y - x is constant
					pos_y = FLOOR(header[header[0]]/2-bs_y/2+counter*config.blocksize)
					rrat, names[0], temp, BLOCK = [header[header[0]]/2-bs_x/2,pos_y,bs_x,blocksizes[counter]]
					; block to array
					arr_base[0,counter*config.blocksize] = temp
				ENDFOR
				; read single comparrays and calculate offsets
				FOR filecounter = 1, number-1 DO BEGIN
					progress, percent = (1+filecounter)*100.0/number, /CHECK_CANCEL
					FOR counter = 0l, nr_blocks-1 DO BEGIN
						; if cancel-button is activated then back
						IF wid.cancel EQ 1 THEN RETURN
						; position for reading in y - x is constant
						pos_y = FLOOR(header[header[0]]/2-bs_y/2+counter*config.blocksize)
						rrat, names[filecounter], temp, BLOCK = [header[header[0]]/2-bs_x/2,pos_y,bs_x,blocksizes[counter]]
						; block to array
						arr_comp[0,counter*config.blocksize] = temp
					ENDFOR
					ret = coreg_coarse(arr_base, arr_comp)
					offset_x[filecounter] = ret[0]
					offset_y[filecounter] = ret[1]
				ENDFOR

				; destroy status-display
				progress, /DESTROY

				; set calculated offsets
				WIDGET_CONTROL, field_x, SET_VALUE = offset_x[index-1]
				WIDGET_CONTROL, field_y, SET_VALUE = offset_y[index-1]
			ENDIF
			; get new values for offset if the are changed
			WIDGET_CONTROL, field_x, GET_VALUE = temp
			offset_x[index-1] = temp
			WIDGET_CONTROL, field_y, GET_VALUE = temp
			offset_y[index-1] = temp
			; ask for position of slider-widget
			WIDGET_CONTROL, slider, GET_VALUE = index
		ENDREP UNTIL (event.id EQ but_ok)
		; read the last values for offset if changed
		WIDGET_CONTROL, field_x, GET_VALUE = temp
		offset_x[index-1] = temp
		WIDGET_CONTROL, field_y, GET_VALUE = temp
		offset_y[index-1] = temp
	; set values if they are not given
	ENDIF ELSE BEGIN
		offset_x[0,number-1] = 0
		offset_y[0,number-1] = 0
	ENDELSE
	; destroy dialog-box
	WIDGET_CONTROL, main, /DESTROY

	; save new header of the multifile - names will be the same
	srat, file.name, newfile, update_info = ['coregistered multifile', STRING(type)]

	FOR counter = 1, number-1 DO BEGIN
		; set file.mult back to 1, because reading single subfiles
		file.mult = 1l
		; shift if the offset is not zero
		IF MAX(ABS([offset_x[counter], offset_y[counter]])) GT 0 THEN BEGIN
			; name to save the shifted subfile temporary
			outputfile = config.tempdir + config.workfile1
			; only shifting if the subfile was not shifted before
			IF MAX(STRCMP(names[counter],names[0:counter-1])) EQ 0 THEN BEGIN
				; show progress window
				progress, MESSAGE = 'Shifting file...'+STRING(counter+1), /CANCEL_BUTTON

				; read and write header for subfile
				head = 0l
				rrat, names[counter], infile, header = head, info = info, type = type
				srat, outputfile, outfile, header = head, info = info, type = type

				; calculate number of blocks, overlap, last blocksize etc.
				overlap = FLOOR(ABS(offset_y[counter])) + 1
				bs = config.blocksize > 4 * overlap
				calc_blocks_overlap, head[head[0]], bs, overlap, nr_blocks, bs_last
				blocksizes = INTARR(nr_blocks)+bs
				blocksizes[nr_blocks-1] = bs_last

				; start and end for block
				ypos1 = 0
				ypos2 = bs - overlap

				; bytes for calculating position in file
				byt = [0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]

				; start shifting - block processing
				FOR i = 0, nr_blocks-1 DO BEGIN
					; show progress status
					progress, percent = (i+1)*100.0/nr_blocks, /CHECK_CANCEL

					;abort if user wants
					IF wid.cancel EQ 1 THEN BEGIN
						FREE_LUN, infile, outfile
						info = DIALOG_MESSAGE('Files could be damaged!', DIALOG_PARENT = main, TITLE = 'WARNING')
						RETURN
					ENDIF

					; read block for block
					block = MAKE_ARRAY(head[head[0]-1], blocksizes[i], type = head[head[0]+1])
					READU, infile, block

; ----------------- THE FILTER ----------------
					block = SHIFT(block, offset_x[counter], offset_y[counter])
					; set the array-elements to 0 which are out of array really
					; in x
					IF offset_x[counter] GT 0 THEN block[0:offset_x[counter]-1,*] = 0*block[0:offset_x[counter]-1,*]
					IF offset_x[counter] LT 0 THEN block[(head[head[0]-1]-1+offset_x[counter]):(head[head[0]-1]-1),*] = $
					0*block[(head[head[0]-1]-1+offset_x[counter]):(head[head[0]-1]-1),*]
					; in y
					IF offset_y[counter] GT 0 THEN block[*,0:ABS(offset_y[counter])-1] = 0*block[*,0:ABS(offset_y[counter])-1]
					IF offset_y[counter] LT 0 THEN block[*,blocksizes[i]-ABS(offset_y[counter])-1:blocksizes[i]-1] = $
					0*block[*,blocksizes[i]-ABS(offset_y[counter])-1:blocksizes[i]-1]
; ----------------- THE FILTER ----------------

					; change y-dim if last block
					IF i EQ nr_blocks-1 THEN ypos2 = bs_last
					; write shifted (and resized) block in new subfile
					WRITEU, outfile, block[*,ypos1:ypos2-1]
					ypos1 = overlap
					; find and change file position for reading
					POINT_LUN, -infile, file_pos
					POINT_LUN, infile, file_pos - 2 * overlap * head[head[0]-1] * byt[head[head[0]+1]]
				ENDFOR
			FREE_LUN, infile, outfile
			FILE_MOVE, outputfile, names[counter], /OVERWRITE, /ALLOW_SAME
			ENDIF
		ENDIF
	ENDFOR

	;fill the file.variables with common values
	file.mult = number
	file.info = 'coregistered multifile'
	file.window_name = FILE_BASENAME(file.name)
	widget_control,wid.base,base_set_title='RAT - Radar Tools: ' + file.window_name

	generate_preview, /recalculate
	update_info_box

	; ask for resizing the subfiles - common size will be found
	info = DIALOG_MESSAGE(['Should RAT resize the whole project', 'to a common size?'], $
	DIALOG_PARENT = wid.base, TITLE ='Common size', /QUESTION)
	IF info EQ 'Yes' THEN mt_common_size, file.name
END
