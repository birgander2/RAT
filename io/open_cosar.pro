;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_cosar
; written by : Tishampati Dhar (Apogee)
; last revision : 31st . October 2007
; Open files in DLR-COSAR format
;--------------------------------------
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
pro open_cosar,INPUTFILE = inputfile
	common rat, types, file, wid, config
    common channel, channel_names, channel_selec, color_flag, palettes,pnames

	if not keyword_set(inputfile) then begin    ; GUI for file selection
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open TerraSAR-X file', $
		DIALOG_PARENT=wid.base, FILTER = '*.cos', /MUST_EXIST,PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin
	; change mousepointer

		WIDGET_CONTROL,/hourglass                ; switch mouse cursor

	; undo function

		undo_prepare,outputfile,finalfile,CALLED=CALLED
		open_rit,/EMPTY ; no parameters are set: delete the old ones!

	; converting format to RAT

		nrx  = 0l
		nry  = 0l
		openr,ddd,inputfile,/get_lun,/swap_if_little_endian
	; open input file
		datasize = lonarr(7)
		readu,ddd,datasize
	;read header CSAR
		print,string(datasize)
		nrx = datasize[2]                         ; reading range size
		nry = datasize[3]                         ; reading azimuth size
		rtnb = datasize[5]
	; reading total number of bytes in rangeline

		ident = bytarr(8)
	;reading file identifier
		readu,ddd,ident
		print,string(ident)

		anzx = nrx
		anzy = nry

		;Set file pointer to end of burst annotation line blocks
		filler = lonarr(2)
		point_lun,ddd,rtnb
		;Read Azimuth Relative Indices
		asri = lonarr(anzx)
		readu,ddd,filler
		readu,ddd,asri
		;Read Azimuth First Valids
		asfv = lonarr(anzx)
		readu,ddd,filler
		readu,ddd,asfv
		;Read Azimuth Last Valids
		aslv = lonarr(anzx)
		readu,ddd,filler
		readu,ddd,aslv


		srat,outputfile,eee,header=[2l,nrx,nry,6l],type=101l
	; write RAT file header

		bs_y  = config.blocksize                 ; get standard RAT blocksize
		block = intarr(anzx*2+4,bs_y)            ; define block
		nr_y  = anzy  /  bs_y                    ; calc nr. of blocks
		re_y  = anzy mod bs_y                    ; calc size of last block

		progress,message='Reading TerraSAR-X Cosar file...'

		for i=0,nr_y-1 do begin                  ; read and write blocks
			progress,percent=((i+1)*100.)/nr_y     ; display progress bar
			readu,ddd,block
			;drop RSFV and RSLV
			subblock = block[4:anzx*2+3,*]
			refblock = reform(subblock,2,anzx,bs_y)
			;convert to complex
			complexblock = complexarr(anzx,bs_y)
			complexblock = complex(reform(refblock[0,*,*]),reform(refblock[1,*,*]))
			writeu,eee,complexblock
		endfor

		if re_y gt 0 then begin                  ; read and write last block
			block = intarr(anzx*2+4,re_y)
			readu,ddd,block
			;drop RSFV and RSLV
			subblock = block[4:anzx*2+3,*]
			refblock = reform(subblock,2,anzx,re_y)
			;convert to complex
			complexblock = complexarr(anzx,re_y)
			complexblock = complex(reform(refblock[0,*,*]),reform(refblock[1,*,*]))
			writeu,eee,complexblock
		endif
		free_lun,ddd,eee                         ; close all files

	; set internal variables of RAT

		file_move,outputfile,finalfile,/overwrite
		file.name = finalfile
		file.info = 'TerraSAR-X COSAR'           ; Put here a string describing your data
		file.type = 101l                         ; data type (101 = single channel complex)
		file.dim  = 2l                           ; single channel data (two-dimensional array)
		file.xdim = nrx                          ; range image size
		file.ydim = nry                          ; azimuth image size
		file.zdim = 1l                           ; nr. of layers (set to 1 if not needed)
		file.vdim = 1l                           ; nr. of layers of layers (set to 1 if not needed)
		file.var  = 6l                           ; IDL variable type (6 = complex, 4 = floating point)

	; update file generation history (evolution)

		evolute,'Import SAR data from TerraSAR-X.'

	; reset palette information

		palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
		palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

	; generate preview

		file.window_name = 'Untitled.rat'
		generate_preview
		update_info_box

	endif
end