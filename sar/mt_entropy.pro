;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : mt_entropy
; Author        : Marco Saemmang (Technical University Berlin, Germany)
; Last revision : March 2006
; Module calculates a ratio-image.
; PRO mt_entropy - dialog for calculating the entropy
;				 - different kinds can be selected
;				 - works with complex, too
; module for calculating the entropy
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

PRO mt_entropy
COMMON rat, types, file, wid, config

	; read RAT-file for details
	rrat, file.name, m_file, header = header, type = type, info = info, multi = multi, mt = subfile

	; check if it is a multifile or multilayer file
	IF multi LE 1 AND file.dim NE 3 THEN BEGIN
		d_info = DIALOG_MESSAGE(['This is not a multifile or', 'a multilayer file!'], DIALOG_PARENT = wid.base, TITLE = 'Information', /error)
		CLOSE, /ALL
		RETURN
	ENDIF

	; abort if subfiles are more than 2-dimensional
	IF (multi GT 1) AND (file.dim GT 2) THEN BEGIN
		d_info = DIALOG_MESSAGE(['Module works only with', '2-dimensional subfiles!'], DIALOG_PARENT = wid.base, TITLE = 'Information', /error)
		CLOSE, /ALL
		RETURN
	ENDIF

	; abort if subfiles are more than 3-dimensional	multilayer
	IF (multi EQ 1) AND (file.dim GT 3) THEN BEGIN
		d_info = DIALOG_MESSAGE(['Module works only with', '3-dimensional multilayerfiles!'], DIALOG_PARENT = wid.base, TITLE = 'Information', /error)
		CLOSE, /ALL
		RETURN
	ENDIF

	; main-widget
	main = WIDGET_BASE(GROUP_LEADER = wid.base, /COLUMN, TITLE = 'Calculate entropy',/floating,/tlb_kill_request_events,/tlb_frame_attr)

	; sub-widget for slider, names, labels etc.
	base_select = WIDGET_BASE(main, /COLUMN, /FRAME, XSIZE = 300)
	base_piorbo = WIDGET_BASE(main, /COLUMN, /FRAME, XSIZE = 300)
	base_but = WIDGET_BASE(main, /ROW, /FRAME, XSIZE = 300)

	; text to describe the module
	sel_label = WIDGET_LABEL(base_select, VALUE = 'Type of entropy')
	selector = 0
	select_but = CW_BGROUP(base_select, ['Entropy between 2', 'Entropy between all'], SET_VALUE = selector, /NO_RELEASE, /EXCLUSIVE)

	; pixel or box
	box_label = WIDGET_LABEL(base_piorbo, VALUE = 'Pixel or Box')
	pixorbox = 0
	piorbo = CW_BGROUP(base_piorbo, ['Entropy between pixels', 'Entropy between mean of box', 'Blockentropy (mosaic)'], SET_VALUE = pixorbox, /NO_RELEASE, /EXCLUSIVE)
	boxsize = 3
	box = CW_FIELD(base_piorbo, VALUE = boxsize, /INTEGER, TITLE = 'Block-/Boxsize', XSIZE = 2)

	; buttons for ok, info and exit
	but_ok = WIDGET_BUTTON(base_but, VALUE = 'OK', XSIZE = 150)
	but_cancel = WIDGET_BUTTON(base_but, VALUE = ' Cancel ', XSIZE = 70)
	but_info = WIDGET_BUTTON(base_but, VALUE = ' Info ', XSIZE = 70)

	; realize dialog-box
	WIDGET_CONTROL, main, /REALIZE, default_button = but_cancel, tlb_get_size = toto
	; center the dialog-box
	pos = CENTER_BOX(toto[0], drawysize = toto[1])
	WIDGET_CONTROL, main, xoffset = pos[0], yoffset = pos[1]

	REPEAT BEGIN
		event = WIDGET_EVENT(main)
		; destroy widget if kill_request-button
		IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			CLOSE, /ALL
			RETURN
		ENDIF
		; close if cancel-button is changed
		IF event.id EQ but_cancel THEN BEGIN
			WIDGET_CONTROL, event.top, /DESTROY
			CLOSE, /ALL
			RETURN
		ENDIF
		; text for info-button
		IF event.id EQ but_info THEN BEGIN
			infotext = ['Module for calculating', $
			'the entropy.', $
			' ', 'RAT module written March 2006', 'by Marco Saemmang.']
			notice = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		END
		; get selector position
		WIDGET_CONTROL, select_but, GET_VALUE = selector
		WIDGET_CONTROL, piorbo, GET_VALUE = pixorbox
	ENDREP UNTIL (event.id EQ but_ok)
	; get boxsize
	WIDGET_CONTROL, box, GET_VALUE = boxsize

	WIDGET_CONTROL, main, /DESTROY

	; if box the boxsize min. 2x2 or greater than size
	IF pixorbox GE 1 THEN BEGIN
		IF boxsize LE 2 OR boxsize GT MIN([file.xdim, file.ydim]) THEN BEGIN
			error = WIDGET_MESSAGE('Boxsize has to be 3<=box<=min(x,y)', DIALOG_PARENT = wid.base, TITLE ='Error', /ERROR)
			CLOSE, /ALL
			RETURN
		ENDIF
	ENDIF

	; if user wants entropy between 2 files
	IF selector EQ 0 THEN BEGIN

		; start-index for master and slave
		master = 1
		slave = 2

		; main-widget
		main = WIDGET_BASE(GROUP_LEADER = wid.base, ROW = 2, TITLE = 'Select files/layers',/floating,/tlb_kill_request_events,/tlb_frame_attr)

		; sub-widget for slider, names, labels etc.
		base_change = WIDGET_BASE(main, /COLUMN, /FRAME, XSIZE = 300)
		base_but = WIDGET_BASE(main, /ROW, /FRAME, XSIZE = 300)

		; slider, text, etc. in first sub-widget
		field_1 = WIDGET_LABEL(base_change, VALUE = 'Fist is file/layer')
		slider_1 = WIDGET_SLIDER(base_change, VALUE = master, MAXIMUM = multi*file.zdim, MINIMUM = 1)
		IF multi GT 1 THEN name_1 = WIDGET_TEXT(base_change, VALUE = FILE_BASENAME(subfile[master-1].file1), XSIZE = 38)
		field_2 = WIDGET_LABEL(base_change, VALUE = 'Second is file/layer')
		slider_2 = WIDGET_SLIDER(base_change, VALUE = slave, MAXIMUM = multi*file.zdim, MINIMUM = 1)
		IF multi GT 1 THEN name_2 = WIDGET_TEXT(base_change, VALUE = FILE_BASENAME(subfile[slave-1].file1), XSIZE = 38)

		; buttons for ok, info and exit
		but_ok = WIDGET_BUTTON(base_but, VALUE = 'OK', XSIZE = 216)
		but_cancel = WIDGET_BUTTON(base_but, VALUE = ' Cancel ', XSIZE = 75)

		; realize dialog-box
		WIDGET_CONTROL, main, /REALIZE, default_button = but_cancel, tlb_get_size = toto
		; center the dialog-box
		pos = CENTER_BOX(toto[0], drawysize = toto[1])
		WIDGET_CONTROL, main, xoffset = pos[0], yoffset = pos[1]

		REPEAT BEGIN
			REPEAT BEGIN
				event = WIDGET_EVENT(main)
				; destroy widget if kill_request-button
				IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
					WIDGET_CONTROL, event.top, /DESTROY
					CLOSE, /ALL
					RETURN
				ENDIF
				; close if cancel-button is changed
				IF event.id EQ but_cancel THEN BEGIN
					WIDGET_CONTROL, event.top, /DESTROY
					CLOSE, /ALL
					RETURN
				ENDIF
				; get sliderposition and change filenames
				WIDGET_CONTROL, slider_1, GET_VALUE = master
				WIDGET_CONTROL, slider_2, GET_VALUE = slave
				IF multi GT 1 THEN WIDGET_CONTROL, name_1, SET_VALUE = FILE_BASENAME(subfile[master-1].file1)
				IF multi GT 1 THEN WIDGET_CONTROL, name_2, SET_VALUE = FILE_BASENAME(subfile[slave-1].file1)
			ENDREP UNTIL (event.id EQ but_ok)
			; no calculating if master eq slave
			IF master EQ slave THEN BEGIN
				infotext = ['The entropy between equal', 'files or layers is 1.', 'Control your selection!']
				notice = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE = 'Information')
			ENDIF
		ENDREP UNTIL (master NE slave)
		; close box after selected
		WIDGET_CONTROL, main, /DESTROY

	ENDIF ; if selector EQ 0

	; undo function
	undo_prepare, outputfile, finalfile, CALLED = CALLED

	; open outputfile to write the blocks of difference
	IF (file.var NE 6) AND (file.var NE 9) THEN BEGIN
		srat, outputfile, outfile, header = [2l, file.xdim, file.ydim, 4l], type = LONG(125), info = 'Entropy ('+types[type]+')'
	ENDIF ELSE BEGIN
		srat, outputfile, outfile, header = [2l, file.xdim, file.ydim, 6l], type = LONG(125), info = 'Entropy ('+types[type]+')'
	ENDELSE

	; if pixel
	IF (pixorbox EQ 0) THEN BEGIN
		overlap = 0
		bl_size = config.blocksize
	ENDIF
	; if mean of a box
	IF (pixorbox EQ 1) THEN BEGIN
		overlap = (boxsize + 1) / 2
		bl_size = config.blocksize
		smooth_box = [1, boxsize, boxsize]
	ENDIF
	; if mosaic
	IF (pixorbox EQ 2) THEN BEGIN
		overlap = 0
		bl_size = boxsize
		rest_x = file.xdim MOD boxsize
		nr_in_x = (file.xdim - rest_x)/boxsize
	ENDIF

	calc_blocks_overlap, file.ydim, bl_size, overlap, nr_blocks, bs_last
	blocksizes = INTARR(nr_blocks) + bl_size
	blocksizes[nr_blocks-1] = bs_last
	IF bs_last EQ 0 THEN BEGIN
		nr_blocks -=1
		bs_last = bl_size
	ENDIF

	; position for start and end of current block
	ypos1 = 0
	ypos2 = bl_size - overlap
	; bytelength for jumps
	byt = [0, 1, 4, 8, 4, 8, 8, 0, 0, 16, 0, 0, 4, 4, 8, 8]

	; progress window
	progress, MESSAGE = 'Calculating entropy ...'

	FOR index = 0, nr_blocks-1 DO BEGIN
		progress, percent = (index+1)*100.0/nr_blocks
		; blocks for reading and writing
		inblock = MAKE_ARRAY(file.zdim, file.xdim, blocksizes[index], type = file.var)
		array_in = MAKE_ARRAY(file.mult*file.zdim, file.xdim, blocksizes[index], type = file.var)
		IF (file.var NE 6) AND (file.var NE 9) THEN BEGIN
			array_out = MAKE_ARRAY(file.xdim, blocksizes[index], /FLOAT)
		ENDIF ELSE BEGIN
			array_out = MAKE_ARRAY(file.xdim, blocksizes[index], /COMPLEX)
		ENDELSE

		; read block (z*x*y) or blocks (multi*x*y)
		FOR i = 0, multi-1 DO BEGIN
			READU, m_file[i], inblock
			IF file.zdim EQ 1 THEN array_in[i, *, *] = inblock ELSE array_in = inblock
			; correct the position (if overlap)
			POINT_LUN, -m_file[i], file_pos
			POINT_LUN, m_file[i], file_pos - 2 * overlap * file.zdim * file.xdim * byt[file.var]
		ENDFOR

		IF index EQ nr_blocks-1 THEN ypos2 = bs_last
		IF index NE 0 THEN ypos1 = overlap

		; if user wants mean of a box (like smoothing)
		IF pixorbox EQ 1 THEN array_in = SMOOTH(array_in, smooth_box, /EDGE_TRUNCATE)

		; if user wants mosaic
		IF pixorbox EQ 2 THEN BEGIN
			FOR index_z = 0, file.mult*file.zdim-1 DO BEGIN
				FOR index_x = 0, nr_in_x-1 DO BEGIN
					array_in[index_z, index_x*boxsize:(index_x*boxsize)+boxsize-1, *] = TOTAL(array_in[index_z, index_x*boxsize:(index_x*boxsize)+boxsize-1, *]) / (boxsize*blocksizes[index])
				ENDFOR
				IF rest_x GT 0 THEN BEGIN
					array_in[index_z, index_x*boxsize:(index_x*boxsize)+rest_x-1, *] = TOTAL(array_in[index_z, index_x*boxsize:(index_x*boxsize)+rest_x-1, *]) / (rest_x*blocksizes[index])
				ENDIF
			ENDFOR
		ENDIF

		; if only two files are selected
		IF selector EQ 0 THEN BEGIN
			; create smaller array
			array_in = [array_in[master-1, *, *], array_in[slave-1, *, *]]
			; set number to 2
			number = 2
		ENDIF ELSE BEGIN
			number = multi
		ENDELSE

; ------- FILTER ------- BEGIN

		; if pixel by pixel and file.var not complex
		IF (file.var NE 6) AND (file.var NE 9) THEN BEGIN
			; transform to float
			array_in = FLOAT(array_in)
			; claculate sum
			array_out = TOTAL(array_in, 1)
			; calculate lambda (lambda = xi/sum(xi))
			FOR i = 0, number-1 DO array_in[i, *, *] = REFORM(array_in[i, *, *]) / array_out
			; entropy -> H = -sum(lambda*log[number] lambda)
			array_in = array_in * ALOG(array_in)/ALOG(number)
			array_out = (-1)*TOTAL(array_in, 1)
		; pixel by pixel and complex
		ENDIF ELSE BEGIN
			; transform to complex
			array_in = COMPLEX(array_in)
			; calculate entropy pixel by pixel, because eigenvalues must be calculated for each vektor
			FOR index_y = 0, blocksizes[index]-1 DO BEGIN
				FOR index_x = 0, file.xdim-1 DO BEGIN
				; calculate eigenvalues of the covarianzmatrix
				values = REFORM(array_in[*, index_x, index_y])
				; covarianzmatrix
				cov_mat = values # (CONJ(values))
				; eigenvalues of cov-matrix
				cov_mat = ELMHES(cov_mat)
				eigenv = FLOAT(HQR(cov_mat))
				; eigenvalues / sum(eigenvalues) = pi
				eigenv = eigenv / TOTAL(eigenv)
				; parts of H
				eigenv = eigenv * ALOG(eigenv) / ALOG(number)
				; H = -SUM(parts)
				entrop = (-1) * TOTAL(eigenv)
				array_out[index_x, index_y] = entrop
				ENDFOR
			ENDFOR
		ENDELSE

		; find infinite elements
		posi = WHERE(FINITE(array_out) EQ 0, nr)
		IF nr GT 0 THEN BEGIN
			IF (file.var NE 6) AND (file.var NE 9) THEN BEGIN
				array_out[posi] = 0.0
			ENDIF ELSE BEGIN
				array_out[posi] = COMPLEX(0.0, 0.0)
			ENDELSE
		ENDIF

; ------- FILTER ------- END

		; write array into outputfile
		WRITEU, outfile, array_out[*, ypos1:ypos2-1]
	ENDFOR

	progress, /DESTROY
	CLOSE, /ALL

	; change arrow to hourglass
	WIDGET_CONTROL, /hourglass
	; move workfile1 to selected file
	FILE_MOVE, outputfile, finalfile, /OVERWRITE

	;fill the file.variables with common values
	IF file.dim GT 2 THEN file.zdim = 1l
	file.dim <= 2l
	file.name = finalfile
	file.window_name = FILE_BASENAME(file.name)
	file.info = 'Entropy image ('+types[type]+')'
	file.type = LONG(125)
	file.mult = 1l
	IF (file.var NE 6) AND (file.var NE 9) THEN BEGIN
		file.var = 4l
	ENDIF ELSE BEGIN
		file.var = 6l
	ENDELSE
	; set new file.window_name
	widget_control, wid.base, base_set_title = 'RAT - Radar Tools: ' + file.window_name

	generate_preview, /recalculate
	update_info_box

	RETURN
END
