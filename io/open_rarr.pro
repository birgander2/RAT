;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_rarr
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
pro open_rarr,INPUTFILE = inputfile
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	if not keyword_set(inputfile) then begin
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open rarr file', DIALOG_PARENT=wid.base, $
		FILTER = '*.dat', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

; set file type

		whatisthis	

; change mousepointer
	
		WIDGET_CONTROL,/hourglass
; undo function
                undo_prepare,outputfile,finalfile,CALLED=CALLED
                open_rit,/EMPTY ; no parameters are set: delete the old ones!

; copy rarr file to working directory

		rarr,inputfile,arr,info=info
		srat,config.tempdir+config.workfile1,arr,info=info
		arr = 0.0
		
; read RAT header

		head = 1l
		rrat,config.tempdir+config.workfile1,ins1,header=head,info=info,type=type		
		free_lun,ins1	
		
; analyse header
	
		file.name = config.tempdir+config.workfile1
;		file.info = info
;		file.type = 0l
		file.dim  = head[0]
		if file.dim eq 2 then begin
			file.xdim = head[1]
			file.ydim = head[2]
			file.zdim = 1l
			file.vdim = 1l
			file.var  = head[3]
		endif
		if file.dim eq 3 then begin
			file.xdim = head[2]
			file.ydim = head[3]
			file.zdim = head[1]
			file.vdim = 1l
			file.var  = head[4]
		endif
		if file.dim eq 4 then begin
			file.xdim = head[3]
			file.ydim = head[4]
			file.zdim = head[1]
			file.vdim = head[2]
			file.var  = head[5]
		endif

; read palette information
	
	palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
	palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview
	
	file.window_name = 'Untitled.rat'
	generate_preview
	update_info_box
	
	endif	
end
