;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : mt_coreg_patch
; Author        : Marco Saemmang (Technical University Berlin, Germany)
; Last revision : March 2006
; Read a multifile and estimate the offset for coregistration.
; If user wants, RAT coregists the subfiles with patches.
; FUNCTION coreg_patch      - for estimate single offsets
; PRO mt_coreg_patch        - dialog for estimating, shifting
; module for coregistrating multifiles with patches
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

FUNCTION coreg_patch, arr1, arr2
	; first step is coarse coregistration
	; no. of rows and columns are the same
	bs = (SIZE(arr1))[1]
	auxcor = ABS(FFT(FFT(arr1, -1) * CONJ(FFT(arr2, -1)), +1))
	; find maximum and position of maximum
	aux    = MAX(auxcor, auxpos)

	; values for offsets from position of max and size
	; it works, because auxpos and bs are (long)int
	offsetx = auxpos MOD bs
	offsety = auxpos  /  bs
	IF (offsetx GT bs/2) THEN offsetx = offsetx-bs
	IF (offsety GT bs/2) THEN offsety = offsety-bs

	; if values for coarse coregistration < 32 then calculate values for
	; fine coregistration - 32 is fixed for fast calculating
	IF ABS(offsetx) LT bs/2-32 AND ABS(offsety) LT bs/2-32 THEN BEGIN
		; take out a central part from arr1
		arr1 = arr1[bs/2-32:bs/2+31, bs/2-32:bs/2+31]
		; take out the 'same' part from arr2
		arr2 = arr2[bs/2-32-offsetx:bs/2-offsetx+31, bs/2-offsety-32:bs/2-offsety+31]
		sizx = 32
		sizy = 32
		osize = 512

		; go into frequency-space
		out1 = FFT(arr1, -1)
		out2 = FFT(arr2, -1)

		fbild1 = COMPLEXARR(osize, osize)
		fbild2 = COMPLEXARR(osize, osize)
		fbild1[0,0] = SHIFT(out1, sizx/2, sizy/2)
		fbild2[0,0] = SHIFT(out2, sizx/2, sizy/2)
		fbild1 = SHIFT(fbild1, -sizx/2, -sizy/2)
		fbild2 = SHIFT(fbild2, -sizx/2, -sizy/2)
		fbild  = fbild1 * CONJ(fbild2)
		bild   = ABS(FFT(fbild, +1))
		; find max
		aux = MAX(bild, pos)
		; calculate x and y
		xoff = pos MOD osize
		yoff = pos  /  osize
		IF xoff GT osize / 2 THEN xoff = xoff - osize
		IF yoff GT osize / 2 THEN yoff = yoff - osize
		xpos = xoff * 1.0 / (FLOAT(osize) / sizx)
		ypos = yoff * 1.0 / (FLOAT(osize) / sizy)
	ENDIF ELSE BEGIN
		xpos = 0.0
		ypos = 0.0
	ENDELSE

	RETURN, [offsetx+xpos, offsety+ypos]
END

PRO mt_coreg_warp, called = called, offset_x = offset_x, offset_y = offset_y
	COMMON rat, types, file, wid, config

	; check the file.type is a multifile
	IF file.mult LE 1 THEN BEGIN
		d_info = DIALOG_MESSAGE('This is not a multifile', DIALOG_PARENT = wid.base, TITLE = 'Information', /error)
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
	ENDFOR
	; close multifile
	FREE_LUN, inputfile

	; set offsets if not handed over
	IF NOT KEYWORD_SET(offset_x) THEN offset_x = MAKE_ARRAY(number, /INTEGER)
	IF NOT KEYWORD_SET(offset_y) THEN offset_y = MAKE_ARRAY(number, /INTEGER)

	IF NOT KEYWORD_SET(called) THEN BEGIN
		; create widgets for coregistration-dialog
		main = WIDGET_BASE(GROUP_LEADER = wid.base, ROW = 2, TITLE = 'Image coregistration (patches)',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		base_man = WIDGET_BASE(main, ROW = 3, /FRAME, XSIZE = 260)
		base_but = WIDGET_BASE(main, COLUMN = 3, /FRAME, XSIZE = 260)

		; create subwindow for inputs
		field_patx = CW_FIELD(base_man, VALUE = 10, /INTEGER, TITLE = 'No. of patches in x : ', XSIZE = 5)
		field_paty = CW_FIELD(base_man, VALUE = 10, /INTEGER, TITLE = 'No. of patches in y : ', XSIZE = 5)
		field_pats = CW_FIELD(base_man, VALUE = 128, /INTEGER, TITLE = 'Size of patches     : ', XSIZE = 5)

		; create subwindow for buttons
		but_ok = WIDGET_BUTTON(base_but, VALUE = ' OK ', XSIZE = 120)
		but_cancel = WIDGET_BUTTON(base_but, VALUE = ' Cancel ', XSIZE = 65)
		but_info = WIDGET_BUTTON(base_but, VALUE = ' Info ', XSIZE = 65)

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
				RETURN
			ENDIF
			; close if cancel-button is changed
			IF event.id EQ but_cancel THEN BEGIN
				WIDGET_CONTROL, event.top, /DESTROY
				RETURN
			ENDIF
			; text for info-button
			IF event.id EQ but_info THEN BEGIN
				infotext = ['IMAGE COREGISTRATION WITH PATCHES', $
				' ', $
				'RAT module written January 2006 by Marco Saemmang.']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			END
		ENDREP UNTIL (event.id EQ but_ok)
		; get values for details of patches
		WIDGET_CONTROL, field_patx, GET_VALUE = pat_no_x
		WIDGET_CONTROL, field_paty, GET_VALUE = pat_no_y
		WIDGET_CONTROL, field_pats, GET_VALUE = pat_size
		; close patches-details box
		WIDGET_CONTROL, main, /DESTROY
	ENDIF ELSE BEGIN
		; set values if they are not given
		IF ~KEYWORD_SET(pat_no_x) THEN pat_no_x = 10
		IF ~KEYWORD_SET(pat_no_y) THEN pat_no_y = 10
		IF ~KEYWORD_SET(pat_size) THEN pat_size = 128
	ENDELSE

	; set number of patches to 2 - if no of patches is lt 2 the interpol-fct. doesnt work
	pat_no_x >= 2
	pat_no_y >= 2
	; find a good patchsize if patchsize * number greater than x- or y-size
	; first change no. of patches in x and/or y
	WHILE (pat_no_x*pat_size GT header[header[0]-1]) AND (pat_no_x GT 2) DO pat_no_x--
	WHILE (pat_no_y*pat_size GT header[header[0]]) AND (pat_no_y GT 2) DO pat_no_y--
	; change size of patch
	IF (pat_no_x*pat_size GT header[header[0]-1]) OR (pat_no_y*pat_size GT header[header[0]]) THEN BEGIN
		min_size = MIN([header[header[0]],header[header[0]-1]])
		pat_min = MIN([pat_no_x, pat_no_y])
		temp = 1
		WHILE 2^temp*pat_min LT min_size DO temp++
		pat_size = 2^temp
		; now find new number of patches in and y
		pat_no_x = 2
		WHILE ((pat_no_x+1)*pat_size LT header[header[0]-1]) DO pat_no_x++
		pat_no_y = 2
		WHILE ((pat_no_y+1)*pat_size LT header[header[0]]) DO pat_no_y++
	ENDIF

	; calculate size of jumps in x and y
	diff_pos_x = (header[header[0]-1] - pat_no_x) / pat_no_x
	diff_pos_y = (header[header[0]] - pat_no_y) / pat_no_y
	; make arrays for offsets
	xo = FLTARR(number, pat_no_x, pat_no_y)
	yo = FLTARR(number, pat_no_x, pat_no_y)
	xp = FLTARR(pat_no_x, pat_no_y)
	yp = FLTARR(pat_no_x, pat_no_y)

	; status-window
	progress, MESSAGE = 'Estimating offsets ...', /CANCEL_BUTTON

	; loop over files
	FOR filecounter = 1, number-1 DO BEGIN
		; loop over no. of patches in y
		FOR index_y = 0, pat_no_y-1 DO BEGIN
			; start-position in y
			ypos = index_y * diff_pos_y
			; read block from master and slave[filecounter]
			rrat, names[0], arr_base, block = [0, ypos, header[header[0]-1], pat_size]
			rrat, names[filecounter], arr_comp, block = [0, ypos, header[header[0]-1], pat_size]
			; loop over no. of patches in x
			FOR index_x = 0, pat_no_x-1 DO BEGIN
				; show status
				progress, percent = ((filecounter-1)*pat_no_x*pat_no_y+index_y*pat_no_x+(index_x+1))*100.0/((number-1)*pat_no_x*pat_no_y), /CHECK_CANCEL
				; return if cancel-button
				IF wid.cancel EQ 1 THEN RETURN

				; extract patches from block
				xpos = index_x * diff_pos_x
				arr1 = ABS(arr_base[xpos:xpos+pat_size-1, *])
				arr2 = ABS(arr_comp[xpos:xpos+pat_size-1, *])

				; call function for calculating offsets
				off_values = coreg_patch(arr1, arr2)
				; offsets
				xo[filecounter, index_x, index_y] = off_values[0]
				yo[filecounter, index_x, index_y] = off_values[1]
				IF filecounter EQ 1 THEN BEGIN
					; center of patch (for showing the arrows)
					xp[index_x, index_y] = xpos + pat_size/2
					yp[index_x, index_y] = ypos + pat_size/2
				ENDIF
			ENDFOR
		ENDFOR
	ENDFOR
	; destroy status-window
	progress, /DESTROY

	IF ~KEYWORD_SET(called) THEN BEGIN
		; bigger array for drawing
		xo_draw = FLTARR(number, pat_no_x+2, pat_no_y+2)
		yo_draw = FLTARR(number, pat_no_x+2, pat_no_y+2)
		x_val = INDGEN(pat_no_x+2)
		y_val = INDGEN(pat_no_y+2)
		xo_unfit = xo
		yo_unfit = yo

		; copy offset-arrays into bigger arrays for generation vectorfield
		FOR index = 0, number-1 DO BEGIN
			xo_draw[index, 1, 1] = xo[index, *, *]
			yo_draw[index, 1, 1] = yo[index, *, *]
		ENDFOR

		; (start)value for oversampling
		over_xd = 2
		over_yd = 2

		; show offset-details and give chance to change manual
		main = WIDGET_BASE(GROUP_LEADER = wid.base, TITLE = 'Coregistration of images with patches', /COLUMN,/floating,/tlb_kill_request_events,/tlb_frame_attr)

		base_file = WIDGET_BASE(main, ROW = 6, /FRAME)
		base_draw = WIDGET_DRAW(main, XSIZE = 450, YSIZE = 400)
		base_edit = WIDGET_BASE(main, /COLUMN, /FRAME)
		base_samp = WIDGET_BASE(main, ROW = 2, /FRAME)
		base_but = WIDGET_BASE(main, COLUMN = 3, /FRAME)

		; create subwindow for filenames and slider
		field_man = WIDGET_LABEL(base_file, VALUE = 'Look or set values for offset')
		master_txt = WIDGET_LABEL(base_file, VALUE = 'Masterfile')
		master_name = WIDGET_TEXT(base_file, VALUE = FILE_BASENAME(names[0]), XSIZE = 50)
		slave_txt = WIDGET_LABEL(base_file, VALUE = 'Offset for file')
		field_name = WIDGET_TEXT(base_file, VALUE = FILE_BASENAME(names[1]), XSIZE = 50)
		slider_index = 2
		slider = WIDGET_SLIDER(base_file, VALUE = slider_index, MAXIMUM = number, MINIMUM = 1, TITLE = ' File ')

		; array for button-text and control-value (fitted=1 or not=0)
		fit_text = ['Fit offsets (current slave-file)', 'Undo fitting (current slave-file)']
		fit_test = INTARR(number)
		fit_but = WIDGET_BUTTON(base_edit, VALUE = fit_text[0], XSIZE = 446)
		edit_but = WIDGET_BUTTON(base_edit, VALUE = 'Edit values for offset (current slave-file)', XSIZE = 446)

		samp_x = CW_FIELD(base_samp, VALUE = over_xd, /INTEGER, TITLE = 'Oversampling in x : ', XSIZE = 4)
		samp_y = CW_FIELD(base_samp, VALUE = over_yd, /INTEGER, TITLE = 'Oversampling in y : ', XSIZE = 4)

		but_ok = WIDGET_BUTTON(base_but, VALUE = 'Start Coregistration', XSIZE = 210)
		but_default = WIDGET_BUTTON(base_but, VALUE = 'Default Values', XSIZE = 116)
		but_cancel = WIDGET_BUTTON(base_but, VALUE = 'Cancel', XSIZE = 116)

		; realize dialog-box
		WIDGET_CONTROL, main, /REALIZE, default_button = but_cancel, tlb_get_size = toto
		; center the dialog-box
		pos = CENTER_BOX(toto[0], drawysize = toto[1])
		WIDGET_CONTROL, main, xoffset = pos[0], yoffset = pos[1]
		; set draw-window
		WIDGET_CONTROL, base_draw, GET_VALUE = w_index
		WSET, w_index

		; draw vectorfield
		VELOVECT, REFORM(xo_draw[slider_index-1, *, *]), REFORM(yo_draw[slider_index-1, *, *]), $
			x_val, y_val, TITLE = 'Offsets as vectorfield', TICKS = 1

		REPEAT BEGIN
			event = WIDGET_EVENT(main)
			; destroy widget if kill_request-button
			IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
				WIDGET_CONTROL, event.top, /DESTROY
				; set draw window back to RATs draw window
				WIDGET_CONTROL, wid.draw, GET_VALUE = w_index
				WSET, w_index
				RETURN
			ENDIF
			; close if cancel-button is changed
			IF event.id EQ but_cancel THEN BEGIN
				WIDGET_CONTROL, event.top, /DESTROY
				; set draw window back to RATs draw window
				WIDGET_CONTROL, wid.draw, GET_VALUE = w_index
				WSET, w_index
				RETURN
			ENDIF
			; funktion for fitting the offset-matrix (like smoothing) and undo
			IF event.id EQ fit_but THEN BEGIN
				; fit if no fitting before
				IF fit_test[slider_index-1] EQ 0 THEN BEGIN
					; set fit_test to 1
					fit_test[slider_index-1] = 1
					; fit both (offset for x and offset for y) separate
					temp_x = REFORM(xo_unfit[slider_index-1, *, *])
					; find infinite values
					posi = WHERE(FINITE(temp_x) EQ 0, nr)
					IF nr GT 0 THEN temp_x[posi] = xo[slider-1, 0, 0] * 0
					temp_y = REFORM(yo_unfit[slider_index-1, *, *])
					posi = WHERE(FINITE(temp_y) EQ 0, nr)
					IF nr GT 0 THEN temp_y[posi] = yo[slider-1, 0, 0] * 0
					; use sfit-function if no. of patches in x any y GE 4
					IF (pat_no_x GE 4) AND (pat_no_y GE 4) THEN BEGIN
						temp_x = SFIT(temp_x, 3)
						temp_y = SFIT(temp_y, 3)
					; use svdfit-function if no. of patches in x and y LT 4
					ENDIF ELSE BEGIN
						; smoothing rows
						IF pat_no_x GT 1 THEN BEGIN
							x_ind = INDGEN(pat_no_x)
							FOR i = 0, pat_no_y-1 DO BEGIN
								; for offset in x (rows)
								temp = REFORM(temp_x[*, i])
								aux	= WHERE(ABS(temp) LE 2*MEDIAN(ABS(temp)), nr)
								IF nr GE (pat_no_x/2+1) THEN BEGIN
									coef = SVDFIT(x_ind[aux], temp[aux], a = [0,0,0,0])
									temp = coef[3]*(x_ind^3) + coef[2]*(x_ind^2) + coef[1]*x_ind + coef[0]
								ENDIF
								IF (WHERE(FINITE(temp) EQ 0) EQ -1) THEN temp_x[*, i] = temp
								; for offset in y (rows)
								temp = REFORM(temp_y[*, i])
								aux	= WHERE(ABS(temp) LE 2*MEDIAN(ABS(temp)), nr)
								IF nr GE (pat_no_x/2+1) THEN BEGIN
									coef = SVDFIT(x_ind[aux], temp[aux], a = [0,0,0,0])
									temp = coef[3]*(x_ind^3) + coef[2]*(x_ind^2) + coef[1]*x_ind + coef[0]
								ENDIF
								IF (WHERE(FINITE(temp) EQ 0) EQ -1) THEN temp_y[*, i] = temp
							ENDFOR
						ENDIF
						; smoothing columns
						IF pat_no_y GT 1 THEN BEGIN
							x_ind = INDGEN(pat_no_y)
							FOR i = 0, pat_no_x-1 DO BEGIN
								; for offset in x (columns)
								temp = REFORM(temp_x[i, *])
								aux = WHERE(ABS(temp) LE 2*MEDIAN(ABS(temp)), nr)
								IF nr GE (pat_no_y/2+1) THEN BEGIN
									coef = SVDFIT(x_ind[aux], temp[aux], a = [0,0,0,0])
									temp = coef[3]*(x_ind^3) + coef[2]*(x_ind^2) + coef[1]*x_ind + coef[0]
								ENDIF
								IF (WHERE(FINITE(temp) EQ 0) EQ -1) THEN temp_x[i, *] = temp
								; for offset in y (columns)
								temp = REFORM(temp_y[i, *])
								aux	= WHERE(ABS(temp) LE 2*MEDIAN(ABS(temp)), nr)
								IF nr GE (pat_no_y/2+1) THEN BEGIN
									coef = SVDFIT(x_ind[aux], temp[aux], a = [0,0,0,0])
									temp = coef[3]*(x_ind^3) + coef[2]*(x_ind^2) + coef[1]*x_ind + coef[0]
								ENDIF
								IF (WHERE(FINITE(temp) EQ 0) EQ -1) THEN temp_y[i, *] = temp
							ENDFOR
						ENDIF
					ENDELSE
					; take smoothed values if all values are correct numbers
					xo_draw[slider_index-1, 1:pat_no_x, 1:pat_no_y] = temp_x
					yo_draw[slider_index-1, 1:pat_no_x, 1:pat_no_y] = temp_y
				; undo fitting
				ENDIF ELSE BEGIN
					; set fit_test back to 0
					fit_test[slider_index-1] = 0
					; set values back to unfitted values
					xo_draw[slider_index-1, 1, 1] = xo_unfit[slider_index-1, *, *]
					yo_draw[slider_index-1, 1, 1] = yo_unfit[slider_index-1, *, *]
				ENDELSE
			ENDIF
			; set values back to default
			IF event.id EQ but_default THEN BEGIN
				; set all controls for fitting to 0
				fit_test[*] = 0
				; copy offset-arrays into bigger arrays for generation vectorfield
				FOR index = 0, number-1 DO BEGIN
					xo_draw[index, 1, 1] = xo[index, *, *]
					yo_draw[index, 1, 1] = yo[index, *, *]
					xo_unfit = xo
					yo_unfit = yo
				ENDFOR
				WIDGET_CONTROL, samp_x, SET_VALUE = over_xd
				WIDGET_CONTROL, samp_y, SET_VALUE = over_yd
			ENDIF
			; change offsets in x and y by user
			IF event.id EQ edit_but THEN BEGIN
				; copy offset-matrix temporary for changing
				temp_x = xo_draw[slider_index-1, *, *]
				temp_y = yo_draw[slider_index-1, *, *]

				; main widget for change offsets
				offbase = WIDGET_BASE(GROUP_LEADER = main, TITLE ='Change offsets', /MODAL, /COLUMN, /TLB_KILL_REQUEST_EVENTS)
				; subwidgets for changing and buttons
				off_pos = WIDGET_BASE(offbase, /COLUMN, /FRAME)
				off_val = WIDGET_BASE(offbase, /COLUMN, /FRAME)
				off_help = WIDGET_BASE(offbase, /COLUMN, /FRAME)
				off_but = WIDGET_BASE(offbase, /ROW, /FRAME)
				; generate subbases for single patch-offsets
				; base for changing position
				text_pos_x = WIDGET_LABEL(off_pos, VALUE = ' Position of patch in x: ')
				; shows slider if number of patches is greater 1
				pos_x = 1
				pos_y = 1
				IF pat_no_x EQ 1 THEN text_x = WIDGET_LABEL(off_pos, VALUE = ' Only 1 column. ') $
					ELSE slider_x = WIDGET_SLIDER(off_pos, VALUE = pos_x, MAXIMUM = pat_no_x, MINIMUM = 1)
				blank = WIDGET_LABEL(off_pos, VALUE = ' ')
				text_pos_y = WIDGET_LABEL(off_pos, VALUE = ' Position of patch in y: ')
				IF pat_no_y EQ 1 THEN text_y = WIDGET_LABEL(off_pos, VALUE = ' Only 1 row. ') $
					ELSE slider_y = WIDGET_SLIDER(off_pos, VALUE = pos_y, MAXIMUM = pat_no_y, MINIMUM = 1)
				; base for changing offsets
				off_x = CW_FIELD(off_val, VALUE = temp_x[0, pos_x, pos_y], /FLOATING, TITLE = 'Offset in x : ', XSIZE = 4)
				off_y = CW_FIELD(off_val, VALUE = temp_y[0, pos_x, pos_y], /FLOATING, TITLE = 'Offset in y : ', XSIZE = 4)
				; base for help-buttons
				find_max = WIDGET_BUTTON(off_help, VALUE = 'Find longest offset-vector!', XSIZE = 202)
				; buttons for ok and cancel
				off_ok = WIDGET_BUTTON(off_but, VALUE = 'Ok', XSIZE = 100)
				off_cancel = WIDGET_BUTTON(off_but, VALUE = 'Cancel', XSIZE = 100)

				; realize dialog-box
				WIDGET_CONTROL, offbase, /REALIZE, default_button = off_cancel, tlb_get_size = toto
				; center the dialog-box
				pos = CENTER_BOX(toto[0], drawysize = toto[1])
				WIDGET_CONTROL, offbase, xoffset = pos[0], yoffset = pos[1]

				REPEAT BEGIN
					event_off = WIDGET_EVENT(offbase)
					; destroy widget if kill_request-button
					IF TAG_NAMES(event_off, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BREAK
					; close if cancel-button is changed
					IF event_off.id EQ off_cancel THEN BREAK
					; get last values
					temp = 0.0
					WIDGET_CONTROL, off_x, GET_VALUE = temp
					temp_x[0, pos_x, pos_y] = temp
					WIDGET_CONTROL, off_y, GET_VALUE = temp
					temp_y[0, pos_x, pos_y] = temp
					; get slider position if pat_no gt 1
					IF pat_no_x GT 1 THEN WIDGET_CONTROL, slider_x, GET_VALUE = pos_x ELSE pos_x = 1
					IF pat_no_y GT 1 THEN WIDGET_CONTROL, slider_y, GET_VALUE = pos_y ELSE pos_y = 1
					; set new values
					WIDGET_CONTROL, off_x, SET_VALUE = temp_x[0, pos_x, pos_y]
					WIDGET_CONTROL, off_y, SET_VALUE = temp_y[0, pos_x, pos_y]
					; find biggest offset
					IF event_off.id EQ find_max THEN BEGIN
						; find position of longest offsetvector
						temp = SQRT(temp_x^2+temp_y^2)
						temp = MAX(temp, max_pos)
						pos_x = max_pos MOD (pat_no_x+2)
						pos_y = max_pos / (pat_no_x+2)
						IF pos_x EQ 0 THEN pos_x = 1
						IF pos_y EQ 0 THEN pos_y = 1
						WIDGET_CONTROL, find_max, SET_VALUE = ' Find again! '
						; set slider to right position if slider is displayed
						IF pat_no_x GT 1 THEN WIDGET_CONTROL, slider_x, SET_VALUE = pos_x
						IF pat_no_y GT 1 THEN WIDGET_CONTROL, slider_y, SET_VALUE = pos_y
						WIDGET_CONTROL, off_x, SET_VALUE = temp_x[0, pos_x, pos_y]
						WIDGET_CONTROL, off_y, SET_VALUE = temp_y[0, pos_x, pos_y]
					ENDIF
					; take new values for offset if ok-button is selected
					IF event_off.id EQ off_ok THEN BEGIN
						xo_draw[slider_index-1, *, *] = temp_x
						yo_draw[slider_index-1, *, *] = temp_y
						; ****
						xo_unfit[slider_index-1, *, *] = temp_x[0, 1:pat_no_x, 1:pat_no_y]
						yo_unfit[slider_index-1, *, *] = temp_y[0, 1:pat_no_x, 1:pat_no_y]
					ENDIF
				ENDREP UNTIL (event_off.id EQ off_ok)
				; close patch-window
				WIDGET_CONTROL, offbase, /DESTROY
				; makes base-widget sensitiv
				WIDGET_CONTROL, main, SENSITIV = 1
			ENDIF

			; get slider position
			WIDGET_CONTROL, slider, GET_VALUE = slider_index
			; clear draw window
			ERASE
			; lock edit-buttons if masterfile
			IF slider_index EQ 1 THEN BEGIN
				WIDGET_CONTROL, edit_but, SENSITIV = 0
				WIDGET_CONTROL, field_name, SET_VALUE = 'Masterfile - no possibility to change offsets!'
				WIDGET_CONTROL, fit_but, SET_VALUE = fit_text[fit_test[slider_index-1]]
				WIDGET_CONTROL, fit_but, SENSITIV = 0
			; unlock edit-buttons and draw vector field
			ENDIF ELSE BEGIN
				; draw vectorfield
				VELOVECT, REFORM(xo_draw[slider_index-1, *, *]), REFORM(yo_draw[slider_index-1, *, *]), $
				x_val, y_val, TITLE = 'Offsets as vectorfield', TICKS = 1
				WIDGET_CONTROL, edit_but, SENSITIV = 1
				WIDGET_CONTROL, fit_but, SET_VALUE = fit_text[fit_test[slider_index-1]]
				WIDGET_CONTROL, fit_but, SENSITIV = 1
				WIDGET_CONTROL, field_name, SET_VALUE = FILE_BASENAME(names[slider_index-1])
			ENDELSE
			; lock edit button if offsets were fitted
			IF (fit_test[slider_index-1] EQ 1) AND (slider_index GT 1) THEN WIDGET_CONTROL, edit_but, SENSITIV = 0

		ENDREP UNTIL (event.id EQ but_ok)
		; get values for oversampling in x and y
		WIDGET_CONTROL, samp_x, GET_VALUE = over_x
		WIDGET_CONTROL, samp_y, GET_VALUE = over_y
		; close main widget
		WIDGET_CONTROL, main, /DESTROY
		; set draw window back to RATs draw window
		WIDGET_CONTROL, wid.draw, GET_VALUE = w_index
		WSET, w_index
		; fill the xo- and yo-matrix with new (changed) values
		FOR index = 1, number-1 DO BEGIN
			xo[index, 0, 0] = xo_draw[index, 1:pat_no_x, 1:pat_no_y]
			yo[index, 0, 0] = yo_draw[index, 1:pat_no_x, 1:pat_no_y]
		ENDFOR
	ENDIF

	; change arrow to hourglass while calculating
	WIDGET_CONTROL, /hourglass
	progress, MESSAGE = 'Warping slave images ...'
	FOR file_index = 1, number-1 DO BEGIN
		progress, PERCENT = file_index * 100.0 / number
		; change picture if necessary
		IF (MAX(ABS(xo[file_index, *, *])) NE 0.0) OR (MAX(ABS(yo[file_index, *, *])) NE 0.0) THEN BEGIN
			IF MAX(STRCMP(names[file_index],names[0:file_index-1])) EQ 0 THEN BEGIN
				; change name of changed file and open for read and write
				header = 1l
				rrat, names[file_index], slave_in, header = header, info = info, type = type
				outputfile = config.tempdir + config.workfile1
				srat, outputfile, slave_out, header = header, info = info, type = type

				;calculating number of blocks, blocksizes etc.
				bs = config.blocksize
				overlap = 2 * (FLOOR(MAX(yo[file_index, *, *]) + 1))
				calc_blocks_overlap, header[header[0]], bs, overlap, nr_blocks, bs_last
				blocksizes = INTARR(nr_blocks) + bs
				blocksizes[nr_blocks-1] = bs_last
				; start- and endposition for block
				ypos1 = 0
				ypos2 = bs - overlap
				; bytelength for size of jumps
				byt = [0, 1, 4, 8, 4, 8, 8, 0, 0, 16, 0, 0, 4, 4, 8, 8]

				rgbin = LINDGEN(header[header[0]-1])
				rgbin_i = INTERPOL(LINDGEN(pat_no_x), xp[*, 0], rgbin)

				; search for good blocksize in x and y automatically
				IF over_x GT 1 THEN BEGIN
					bsx = 2
					WHILE 2l^bsx LT header[header[0]-1] DO bsx++
					bsx1 = 2^bsx
					bsx2 = over_x * bsx1
				ENDIF
				IF over_y GT 1 THEN BEGIN
					bsy = 2
					WHILE 2l^bsy LT bs DO bsy++
					bsy1 = 2^bsy
					bsy2 = over_y * bsy1
				ENDIF

				FOR index = 0, nr_blocks - 1 DO BEGIN

					block = MAKE_ARRAY([header[header[0]-1], blocksizes[index]], type = header[header[0]+1])
					READU, slave_in, block

; ---------- THE FILTER -------------

					azbin = LINDGEN(blocksizes[index]) + FLOOR(TOTAL(blocksizes[0:index])) - blocksizes[index] - 2 * index * overlap
					azbin_i = INTERPOL(LINDGEN(pat_no_y), yp[0, *], azbin)

					; resampling in range
					res = INTERPOLATE(-REFORM(xo[file_index, *, *]), rgbin_i, azbin_i, /GRID, CUBIC = -0.5)

					IF over_x EQ 1 THEN BEGIN
						res += (DBLARR(blocksizes[index]) + 1) ## DINDGEN(header[header[0]-1])
						FOR k = 0, blocksizes[index]-1 DO block[*, k] = INTERPOLATE(block[*, k], res[*, k], CUBIC = -0.5)
					ENDIF ELSE BEGIN
						FOR k = 0, blocksizes[index]-1 DO BEGIN
							line	= COMPLEXARR(bsx1)
							line[0]	= block[*, k]
							fline	= FFT(line, -1)
							ofline	= COMPLEXARR(bsx2)
							ofline[0]	= fline[0:bsx1/2-1]
							ofline[bsx2 - bsx1/2] = fline[bsx1/2:*]
							oline	= FFT(ofline, +1)

							res2	= DINDGEN(bsx2)
							res2[0:over_x * header[header[0]-1] - 1] += REBIN(over_x * res[*, k], over_x * header[header[0]-1])
							oline	= INTERPOLATE(oline, res2, CUBIC = -0.5)

							ofline	= FFT(oline, -1)
							fline	= [ofline[0:bsx1/2-1], ofline[bsx2-bsx1/2:*]]
							line	= FFT(fline, +1)
							block[*, k] = line[0:header[header[0]-1]-1]
						ENDFOR
					ENDELSE

					; resampling in azimuth
					res = INTERPOLATE(-REFORM(yo[file_index, *, *]), rgbin_i, azbin_i, /GRID, CUBIC = -0.5)

					IF over_y EQ 1 THEN BEGIN
						res += DINDGEN(blocksizes[index]) ## (DBLARR(header[header[0]-1]) + 1)
						FOR k = 0, header[header[0]-1]-1 DO block[k, *] = INTERPOLATE(block[k, *], res[k, *], CUBIC = -0.5)
					ENDIF ELSE BEGIN
						FOR k = 0, header[header[0]-1]-1 DO BEGIN
							line	= COMPLEXARR(bsy1)
							line[0] = REFORM(block[k, *])
							fline	= FFT(line, -1)
							ofline	= COMPLEXARR(bsy2)
							ofline[0]	= fline[0:bsy1/2-1]
							ofline[bsy2-bsy1/2]	= fline[bsy1/2:*]
							oline	= FFT(ofline, +1)

							res2	= DINDGEN(bsy2)
							res2[0:over_x * blocksizes[index]-1] += REBIN(over_y * res[k, *], over_y * blocksizes[index])
							oline	= INTERPOLATE(oline, res2, CUBIC = -0.5)

							ofline	= FFT(oline, -1)
							fline	= [ofline[0:bsy1/2-1], ofline[bsy2-bsy1/2:*]]
							line	= FFT(fline, +1)
							block[k, *]	= line[0:blocksizes[index]-1]
						ENDFOR
					ENDELSE

; ------- THE FILTER --------

					IF index EQ nr_blocks-1 THEN ypos2 = bs_last
					; only values gt than 0 (type 100)
					IF (file.type EQ 100) OR (file.type EQ 103) THEN block >= 0.0
					WRITEU, slave_out, block[*, ypos1:ypos2-1]
					ypos1 = overlap
					POINT_LUN, -slave_in, file_pos
					POINT_LUN, slave_in, file_pos - 2 * overlap * header[header[0]-1] * byt[header[header[0]+1]]

				ENDFOR
				FREE_LUN, slave_out, slave_in
				; overwrite old subfile with warped subfile
				FILE_MOVE, outputfile, names[file_index], /ALLOW_SAME, /OVERWRITE
			ENDIF ; end if strcmp(....
		ENDIF ; end if (max(abs(xo....
	ENDFOR
	progress, /DESTROY

	; save new header of the multifile - names will be the same
	srat, file.name, temp, update_info = ['coregistered multifile', STRING(type)]

	file.info = 'coregistered multifile'
	file.window_name = FILE_BASENAME(file.name)
	widget_control,wid.base,base_set_title='RAT - Radar Tools: ' + file.window_name

	generate_preview, /recalculate
	update_info_box
END
