;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_template
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
pro open_template,INPUTFILE = inputfile
	common rat, types, file, wid, config
        common channel

	if not keyword_set(inputfile) then begin    ; GUI for file selection
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open MYSENSOR file', $
		DIALOG_PARENT=wid.base, FILTER = '*.dat', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

; change mousepointer
	
		WIDGET_CONTROL,/hourglass                ; switch mouse cursor

; undo function
		
		undo_prepare,outputfile,finalfile,CALLED=CALLED
		open_rit,/EMPTY ; no parameters are set: delete the odl ones!

; converting format to RAT
	
		nrx  = 0l
		nry  = 0l
		openr,ddd,inputfile,/xdr,/get_lun        ; open input file
		readu,ddd,anzy                           ; reading range size
		readu,ddd,anzx                           ; reading azimuth size
		srat,outputfile,eee,header=[2l,nrx,nry,6l],type=101l     ; write RAT file header

		bs_y  = config.blocksize                 ; get standard RAT blocksize
		block = complexarr(anzx,bs_y)            ; define block
		nr_y  = anzy  /  bs_y                    ; calc nr. of blocks
		re_y  = anzy mod bs_y                    ; calc size of last block
		
		progress,message='Reading MYSENSOR file...'
		
		for i=0,nr_y-1 do begin                  ; read and write blocks
			progress,percent=((i+1)*100)/nr_y     ; display progress bar
			readu,ddd,block
			writeu,eee,block
		endfor
		if re_y gt 0 then begin                  ; read and write last block
			block = complexarr(anzx,re_y)
			readu,ddd,block
			writeu,eee,block
		endif		
		free_lun,ddd,eee                         ; close all files
			
; set internal variables of RAT
	
		file_move,outputfile,finalfile,/overwrite
		file.name = finalfile
		file.info = ''                           ; Put here a string describing your data
		file.type = 101l                         ; data type (101 = single channel complex)
		file.dim  = 2l                           ; single channel data (two-dimensional array)
		file.xdim = nrx                          ; range image size
		file.ydim = nry                          ; azimuth image size
		file.zdim = 1l                           ; nr. of layers (set to 1 if not needed)
		file.vdim = 1l                           ; nr. of layers of layers (set to 1 if not needed)
		file.var  = 6l                           ; IDL variable type (6 = complex, 4 = floating point)

; update file generation history (evolution)
	
		evolute,'Import SAR data from MYSENSOR.'
 	
; reset palette information

		palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
		palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

		file.window_name = 'Untitled.rat'
		generate_preview
		update_info_box
	
	endif	
end
