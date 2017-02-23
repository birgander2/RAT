;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module	: mt_all_in_one
; Author 		: Marco Saemmang (Technical University Berlin, Germany)
; Last revision : March 2006
; Module copies the subfiles from a multifile into a common file.
; All subfiles are separat rat-files with own previews.
; A multifile with names and preview exist. After using this
; module all subfiles are copied into one rat-file.
; module for save all subfiles in one rat-file (no multifile)
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

PRO mt_allinone
COMMON rat, types, file, wid, config

	; check the file.type is a multifile
	IF file.mult LT 2 THEN BEGIN
		d_info = DIALOG_MESSAGE('This is not a multifile', DIALOG_PARENT = wid.base, TITLE = 'Information', /error)
		RETURN
	ENDIF

	; at this time only for 2-dimsional sub-arrays
	IF file.dim NE 2 THEN BEGIN
		d_info = DIALOG_MESSAGE(['At this time the All-In-One-module', 'works only with 2-dimensional', 'subfiles!'], $
		DIALOG_PARENT = wid.base, TITLE = 'Information', /error)
		RETURN
	ENDIF

	undo_prepare, outputfile, finalfile, CALLED = CALLED

	; open multifile for names and other details
	rrat, file.name, multifile, header = header, info = info, type = type, multi = multi, mt = subfile
	; create and save new header
	srat, outputfile, outfile, header = [3l, LONG(multi), header[header[0]-1], header[header[0]], header[header[0]+1]], info = 'combined multifile', type = type

	; define arrays to read and write different types
	temp = MAKE_ARRAY(header[header[0]-1], type = header[header[0]+1])
	array_out = MAKE_ARRAY(header[header[0]-1] * multi, type = header[header[0]+1])
	ind = INDGEN(header[header[0]-1]) * multi

	progress, MESSAGE = 'Creating new file ...'
	; loop over dimensions
	FOR index = 0, header[header[0]]-1 DO BEGIN
		progress, percent = (index+1)*100.0/(header[header[0]])
		; loop over the subfiles
		FOR i = 0, multi-1 DO BEGIN
			READU, subfile[i].lun1, temp
			array_out[i+ind] = temp
		ENDFOR
		WRITEU, outfile, array_out
	ENDFOR
	progress, /DESTROY

	; close all opened files
	CLOSE, /ALL

	; change arrow to hourglass
	WIDGET_CONTROL, /hourglass
	; move files for undo
	FILE_MOVE, outputfile, finalfile, /OVERWRITE

	; fill the file.variables with new values
	file.dim = 3l
	file.zdim = multi
	file.name = finalfile
	file.window_name = FILE_BASENAME(file.name)
	file.info = 'Multilayerfile'
	file.type = file.type
	file.mult = 1l
	file.var = file.var

	; set new file.window_name
	widget_control, wid.base, base_set_title = 'RAT - Radar Tools: ' + file.window_name

	generate_preview, /recalculate
	update_info_box

	RETURN
END
