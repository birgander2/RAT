;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_2lcmp
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
pro open_2lcmp,INPUTFILE = inputfile
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	if not keyword_set(inputfile) then begin
		path = config.workdir
		inputfile = dialog_pickfile(TITLE='Open file', $
		DIALOG_PARENT=wid.base, FILTER = '*.dat', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

; change mousepointer
	
		WIDGET_CONTROL,/hourglass

; undo function
                undo_prepare,outputfile,finalfile,CALLED=CALLED
                open_rit,/EMPTY ; no parameters are set: delete the old ones!

; converting format to RAT
	
		anzx  = 0l
		anzy  = 0l
		openr,ddd,inputfile,/xdr,/get_lun
		readu,ddd,anzx
		readu,ddd,anzy
		srat,config.tempdir+config.workfile1,eee,header=[2l,anzx,anzy,6l],type=0l

		bs_y  = config.blocksize
		zeile = complexarr(anzx,bs_y)
		nr_y  = anzy  /  bs_y
		re_y  = anzy mod bs_y
		for i=0,nr_y-1 do begin
			readu,ddd,zeile
			writeu,eee,zeile
		endfor
		if re_y gt 0 then begin
			zeile = complexarr(anzx,re_y)
			readu,ddd,zeile
			writeu,eee,zeile
		endif		
		free_lun,ddd,eee
	
; read header

		head = 1l
		rrat,config.tempdir+config.workfile1,ins1,header=head,info=info
		free_lun,ins1
		
; analyse header
	
		file.name = config.tempdir+config.workfile1
		file.info = ''
		file.type = 0l
		file.dim  = head[0]
		file.xdim = anzx
		file.ydim = anzy
		file.zdim = 1l
		file.vdim = 1l
		file.var  = 6l

; read palette information
	
		palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
		palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; update file generation history (evolution)
                evolute,'Import SAR data from MYSENSOR.'

; generate preview

		file.window_name = 'Untitled.rat'
		generate_preview
		update_info_box
	
; whatisthis ?

		whatisthis
	
	endif	
end
