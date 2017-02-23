;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : mt_prop_of_change
; Author        : Marco Saemmang (Technical University Berlin, Germany)
; Last revision : March 2006
; Module calculates a ratio-image.
; PRO mt_prop_of_change - dialog for calculating propability of change (statitics)
;			   			- different kinds can be selected
; module for calculating the probability of change
; different types are selectable
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

PRO mt_prop_of_change
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
	main = WIDGET_BASE(GROUP_LEADER = wid.base, /COLUMN, TITLE = 'Propability of change',/floating,/tlb_kill_request_events,/tlb_frame_attr)

	; sub-widget for slider, names, labels etc.
	base_select = WIDGET_BASE(main, /COLUMN, /FRAME, XSIZE = 340)
	base_piorbo = WIDGET_BASE(main, /COLUMN, /FRAME, XSIZE = 340)
	base_but = WIDGET_BASE(main, /ROW, /FRAME, XSIZE = 340)

	; group of types
	sel_label = WIDGET_LABEL(base_select, VALUE = 'Type')
	selector = 0
	select_but = CW_BGROUP(base_select, ['Propability - (Product(Xi) / (Mean(X)^n))', 'Propability - (Product(Xi) / (Mean(X)^n))^L',$
										 'Probability - coefficient of variation'], SET_VALUE = selector, /NO_RELEASE, /EXCLUSIVE)
	; pixel or box
	box_label = WIDGET_LABEL(base_piorbo, VALUE = 'Pixel or Box')
	pixorbox = 0
	piorbo = CW_BGROUP(base_piorbo, ['Propability between pixels', 'Propability between mean of box', 'Blockpropability (mosaic)'], SET_VALUE = pixorbox, /NO_RELEASE, /EXCLUSIVE)
	boxsize = 3
	box = CW_FIELD(base_piorbo, VALUE = boxsize, /INTEGER, TITLE = 'Block-/Boxsize', XSIZE = 2)

	; buttons for ok, info and exit
	but_ok = WIDGET_BUTTON(base_but, VALUE = 'OK', XSIZE = 180)
	but_cancel = WIDGET_BUTTON(base_but, VALUE = ' Cancel ', XSIZE = 75)
	but_info = WIDGET_BUTTON(base_but, VALUE = ' Info ', XSIZE = 75)

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
				infotext = ['Modul for calculating', $
				'the propability of change.', $
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

	; ask for classifying
	classify = 0
	IF selector EQ 2 THEN BEGIN
		classify = WIDGET_MESSAGE(['Should RAT makes a classification?', '(Homogeneous areas will be 0 and', 'heterogeneous areas will be 1.)'], $
								DIALOG_PARENT = wid.base, TITLE = 'Question', /QUESTION)
		IF classify EQ 'No' THEN classify = 0 ELSE classify = 1
	ENDIF

	;ask for number of looks and limit for classifying
	IF (selector EQ 1) OR (classify EQ 1) THEN BEGIN
		; main-widget
		IF selector EQ 1 THEN BEGIN
			main = WIDGET_BASE(GROUP_LEADER = wid.base, /COLUMN, TITLE = 'Number of looks',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		ENDIF ELSE BEGIN
			main = WIDGET_BASE(GROUP_LEADER = wid.base, /COLUMN, TITLE = 'Values for classification',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		ENDELSE

		; sub-widget for slider, names, labels etc.
		base_nr = WIDGET_BASE(main, /COLUMN, /FRAME, XSIZE = 230)
		base_but = WIDGET_BASE(main, /ROW, /FRAME, XSIZE = 230)

		; ask for number of looks
		nr_label = WIDGET_LABEL(base_nr, VALUE = 'Number of looks')
		nr_looks = 1.0
		nr_box = CW_FIELD(base_nr, VALUE = nr_looks, /FLOAT, TITLE = 'Number (L) ', XSIZE = 5)

		; ask for limit
		IF classify EQ 1 THEN BEGIN
			li_label = WIDGET_LABEL(base_nr, VALUE = 'Limit for selection')
			limit = 0.01
			li_box = CW_FIELD(base_nr, VALUE = limit, /FLOAT, TITLE = 'Limit      ', XSIZE = 5)
			text = WIDGET_LABEL(base_nr, VALUE = '')
			text_1 = WIDGET_LABEL(base_nr, VALUE = 'If classify is selected -')
			text_2 = WIDGET_LABEL(base_nr, VALUE = 'homogeneous areas will be 0 and')
			text_3 = WIDGET_LABEL(base_nr, VALUE = 'heterogeneous areas will be 1.')
		ENDIF

		; buttons for ok, info and exit
		but_ok = WIDGET_BUTTON(base_but, VALUE = 'OK', XSIZE = 160)
		but_cancel = WIDGET_BUTTON(base_but, VALUE = ' Cancel ', XSIZE = 62)

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

		ENDREP UNTIL (event.id EQ but_ok)
		; get value for ENL and limit
		WIDGET_CONTROL, nr_box, GET_VALUE = nr_looks
		IF classify EQ 1 THEN BEGIN
			WIDGET_CONTROL, li_box, GET_VALUE = limit
		ENDIF

		WIDGET_CONTROL, main, /DESTROY

		; limit must be greater than 0.0
		IF (classify EQ 1) THEN BEGIN
			IF (limit LT 0.0) THEN BEGIN
				error = WIDGET_MESSAGE('Limit has to be >=0.0', DIALOG_PARENT = wid.base, TITLE ='Error', /ERROR)
				CLOSE, /ALL
				RETURN
			ENDIF
		ENDIF
		; number of looks must be greater than or equal 1.0
		IF nr_looks LT 1.0 THEN BEGIN
			error = WIDGET_MESSAGE('Number of looks has to be >=1.0', DIALOG_PARENT = wid.base, TITLE ='Error', /ERROR)
			CLOSE, /ALL
			RETURN
		ENDIF
		; if smoothing is selected - nr of looks will be changed
		IF pixorbox EQ 1 THEN nr_looks = nr_looks*boxsize^2

	ENDIF

	; check if the subfiles include complex data
	IF file.var EQ 6 OR file.var EQ 9 THEN BEGIN
		CLOSE, /ALL
		info = DIALOG_MESSAGE(["Image is complex and has to", "be converted to float first!", "Convert?"], DIALOG_PARENT = wid.base, $
		TITLE = 'Question', /QUESTION)
		IF info EQ "No" THEN RETURN
		complex2abs, /called
		rrat, file.name, m_file, header = header, type = type, info = info, multi = multi, mt = subfile
	ENDIF

	; undo function
	undo_prepare, outputfile, finalfile, CALLED = CALLED

	; open outputfile to write the blocks of difference
	srat, outputfile, outfile, header = [2l, file.xdim, file.ydim, 4l], type = LONG(124), info = 'Propability of change ('+types[type]+')'

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

	; window for status
	progress, MESSAGE = 'Calculating propability ...'

	FOR index = 0, nr_blocks-1 DO BEGIN
		progress, percent = (index+1)*100.0/nr_blocks
		; blocks for reading and writing
		inblock = MAKE_ARRAY(file.zdim, file.xdim, blocksizes[index], type = file.var)
		array_in = MAKE_ARRAY(file.mult*file.zdim, file.xdim, blocksizes[index], type = file.var)
		array_out = MAKE_ARRAY(file.xdim, blocksizes[index], /FLOAT)

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

; ------- FILTER ------- BEGIN

		; array to float
		array_in = FLOAT(array_in)

		; normal type (product xi / (mean(xi))^n) - alog to avoid overflows
		IF selector EQ 0 THEN BEGIN
			array_out = 1.0 - EXP(TOTAL(ALOG(array_in), 1) - multi*ALOG(TOTAL(array_in, 1)/multi))
		ENDIF
		; like normal type, but with number of looks
		IF selector EQ 1 THEN BEGIN
			array_out = EXP(TOTAL(ALOG(array_in), 1) - multi*ALOG(TOTAL(array_in, 1)/multi))
			array_out = 1.0 - (array_out^nr_looks)
		ENDIF
		; coefficient of variation
		IF selector EQ 2 THEN BEGIN
			array_in = DOUBLE(array_in)
			; means
			means = (TOTAL(array_in, 1)) / multi
			; calculate the corrections
			FOR i = 0, multi-1 DO BEGIN
				array_in[i, *, *] = REFORM(array_in[i, *, *]) - means
			ENDFOR
			array_in = array_in^2
			array_out = TOTAL(array_in, 1)
			array_out = array_out / (multi-1)
			array_out = SQRT(array_out)
			; quotient
			array_out = FLOAT(array_out / means)
			; if classify is selected
			IF classify EQ 1 THEN BEGIN
				; all points LE calculated limit = 0 - others will be 1
				temp = limit+(1/SQRT(nr_looks))
				posi = WHERE(array_out LE temp, nr)
				IF nr GT 0 THEN array_out[posi] = 0.0
				posi = WHERE(array_out GT temp, nr)
				IF nr GT 0 THEN array_out[posi] = 1.0
			ENDIF
		ENDIF

		; find infinite elements
		posi = WHERE(FINITE(array_out) EQ 0, nr)
		IF nr GT 0 THEN array_out[posi] = 0.0

; ------- FILTER ------- END

		; write array into outputfile
		WRITEU, outfile, array_out[*, ypos1:ypos2-1]
	ENDFOR ;index

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
	file.info = 'Propability of change image ('+types[type]+')'
	file.type = LONG(124)
	file.mult = 1l
	file.var = 4l
	; set new file.window_name
	widget_control, wid.base, base_set_title = 'RAT - Radar Tools: ' + file.window_name

	generate_preview, /recalculate
	update_info_box

	RETURN
END
