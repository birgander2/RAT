;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_emisar
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
function emi2cpl,arr
	siz = (size(arr))[1]
	ind = indgen(siz/2)*2
	sign = fix(ishft(arr and 2l^15 ,-15))           ; bit 15
	expo = fix(ishft(arr and uint(32640),-7))-127   ; bit 14-8
	valu = 1.0+(arr and uint(127))/128.             ; bit 0-7
	flt  = float(valu) * 2.^float(expo)	
   aux  = where(sign eq 1,nr)
	if nr gt 0 then flt[aux] *= -1.0
	return,complex(flt[ind,*],flt[ind+1,*])
end

pro open_emisar,INPUTFILE = inputfile
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='EMISAT import',/floating,/tlb_kill_request_events,/tlb_frame_attr )
	butt = cw_bgroup(main,[' Load data in vector format',' Load data in matrix format'],set_value=0,row=2,/exclusive)		
	buttons  = WIDGET_BASE(main,column=3,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
	WIDGET_CONTROL, main, /REALIZE, default_button = but_canc, tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]

	repeat begin
		event = widget_event(main)
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['EMISAR IMPORT',$
			' ',$
			'RAT module written 01/2005 by Andreas Reigber']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,butt,GET_VALUE=channels
	widget_control,main,/destroy
	if event.id ne but_ok then return                   ; OK button _not_ clicked

	if not keyword_set(inputfile) then begin    ; GUI for file selection
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open EMISAR readme file', $
		DIALOG_PARENT=wid.base, FILTER = '*read_me*', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

; change mousepointer
	
		WIDGET_CONTROL,/hourglass                ; switch mouse cursor

; undo function
                undo_prepare,outputfile,finalfile,CALLED=CALLED
                open_rit,/EMPTY ; no parameters are set: delete the old ones!

; analysing readme file
		
		if channels eq 0 then secstr = 'scattering matrix'
		if channels eq 1 then secstr = 'covariance matrix'
		anz_rg = 0
		anz_az = 0
				
		rstr  = ''
		openr,ddd,inputfile,/get_lun
		repeat begin
			readf,ddd,rstr
			if strcmp(strlowcase(rstr),secstr,15) then begin
				repeat begin
					readf,ddd,rstr
						if strcmp(strlowcase(rstr),'    samples per line',15) then begin 
							rstr = strmid(rstr,(strsplit(rstr,':'))[1])
							rstr = strmid(rstr,0,(strsplit(rstr,'('))[1]-1)
							anz_rg = floor(float(strcompress(rstr,/remove)))
						endif				
						if strcmp(strlowcase(rstr),'    lines per file',15) then begin	
							rstr = strmid(rstr,(strsplit(rstr,':'))[1])
							rstr = strmid(rstr,0,(strsplit(rstr,'('))[1]-1)
							anz_az = floor(float(strcompress(rstr,/remove)))
						endif			
						if channels eq 0 then begin
							if strcmp(strmid(rstr,strlen(rstr)-5),'hh.pp') then hhfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-5),'vv.pp') then vvfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-5),'hv.pp') then hvfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-5),'vh.pp') then vhfile = strcompress(rstr,/remove)
						endif			
						if channels eq 1 then begin
							if strcmp(strmid(rstr,strlen(rstr)-7),'hhhh.co') then hhhhfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-7),'vvvv.co') then vvvvfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-7),'hvhv.co') then hvhvfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-7),'hhhv.co') then hhhvfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-7),'hhvv.co') then hhvvfile = strcompress(rstr,/remove)
							if strcmp(strmid(rstr,strlen(rstr)-7),'hvvv.co') then hvvvfile = strcompress(rstr,/remove)
						endif			
				endrep until anz_rg ne 0 and anz_az ne 0
			endif
		endrep until eof(ddd)
		free_lun,ddd

; converting format to RAT
	
		if channels eq 0 then srat,config.tempdir+config.workfile1,eee,header=[3l,4l,anz_rg,anz_az,6l],type=200l  
		if channels eq 1 then srat,config.tempdir+config.workfile1,eee,header=[4l,3l,3l,anz_rg,anz_az,6l],type=220l    

		progress,message='Reading EMISAR file...'
		if channels eq 0 then begin
			openr,hhin,path+hhfile,/get_lun,/swap_if_little_endian
			openr,vvin,path+vvfile,/get_lun,/swap_if_little_endian
			openr,hvin,path+hvfile,/get_lun,/swap_if_little_endian
			openr,vhin,path+vhfile,/get_lun,/swap_if_little_endian
	
	
			bs_y  = config.blocksize                 ; get standard RAT blocksize
			nr_y  = anz_az  /  bs_y                    ; calc nr. of blocks
			re_y  = anz_az mod bs_y                    ; calc size of last block
			block  = uintarr(2*anz_rg,bs_y)
			oline = complexarr(4,anz_rg,bs_y)

			for i=0,nr_y-1 do begin                  ; read and write blocks
				progress,percent=(i+1)*100./nr_y     ; display progress bar
				readu,hhin,block
				oline[0,*,*] = emi2cpl(block)
				readu,vvin,block
				oline[1,*,*] = emi2cpl(block)
				readu,hvin,block
				oline[2,*,*] = emi2cpl(block)
				readu,vhin,block
				oline[3,*,*] = emi2cpl(block)
				writeu,eee,oline
			endfor
			if re_y gt 0 then begin                  ; read and write last block
				block  = uintarr(2*anz_rg,re_y)
				oline = complexarr(4,anz_rg,re_y)
				readu,hhin,block
				oline[0,*,*] = emi2cpl(block)
				readu,vvin,block
				oline[1,*,*] = emi2cpl(block)
				readu,hvin,block
				oline[2,*,*] = emi2cpl(block)
				readu,vhin,block
				oline[3,*,*] = emi2cpl(block)
				writeu,eee,oline
			endif		
			free_lun,eee,hhin,vvin,hvin,vhin
		endif
			
		if channels eq 1 then begin
			openr,hhhhin,path+hhhhfile,/get_lun,/swap_if_big_endian
			openr,vvvvin,path+vvvvfile,/get_lun,/swap_if_big_endian
			openr,hvhvin,path+hvhvfile,/get_lun,/swap_if_big_endian
			openr,hhhvin,path+hhhvfile,/get_lun,/swap_if_big_endian
			openr,hhvvin,path+hhvvfile,/get_lun,/swap_if_big_endian
			openr,hvvvin,path+hvvvfile,/get_lun,/swap_if_big_endian

			bs_y  = config.blocksize                   ; get standard RAT blocksize
			nr_y  = anz_az  /  bs_y                    ; calc nr. of blocks
			re_y  = anz_az mod bs_y                    ; calc size of last block

			rblock  = fltarr(anz_rg,bs_y)
			cblock  = complexarr(anz_rg,bs_y)
			oblock  = complexarr(3,3,anz_rg,bs_y)
			for i=0,nr_y-1 do begin                  ; read and write blocks
				progress,percent=(i+1)*100./nr_y      ; display progress bar
				readu,hhhhin,rblock
				oblock[0,0,*,*] = rblock
				readu,vvvvin,rblock
				oblock[1,1,*,*] = rblock
				readu,hvhvin,rblock
				oblock[2,2,*,*] = rblock
				readu,hhhvin,cblock
				oblock[2,0,*,*] = cblock
				oblock[0,2,*,*] = conj(cblock)
				readu,hhvvin,cblock
				oblock[1,0,*,*] = cblock
				oblock[0,1,*,*] = conj(cblock)
				readu,hvvvin,cblock
				oblock[1,2,*,*] = cblock
				oblock[2,1,*,*] = conj(cblock)
				writeu,eee,oblock
			endfor
			if re_y gt 0 then begin                  ; read and write last block
				rblock  = fltarr(anz_rg,re_y)
				cblock  = complexarr(anz_rg,re_y)
				oblock  = complexarr(3,3,anz_rg,re_y)
				readu,hhhhin,rblock
				oblock[0,0,*,*] = rblock
				readu,vvvvin,rblock
				oblock[1,1,*,*] = rblock
				readu,hvhvin,rblock
				oblock[2,2,*,*] = rblock
				readu,hhhvin,cblock
				oblock[2,0,*,*] = cblock
				oblock[0,2,*,*] = conj(cblock)
				readu,hhvvin,cblock
				oblock[1,0,*,*] = cblock
				oblock[0,1,*,*] = conj(cblock)
				readu,hvvvin,cblock
				oblock[1,2,*,*] = cblock
				oblock[2,1,*,*] = conj(cblock)
				writeu,eee,oblock
			endif		
			free_lun,eee,hhhhin,vvvvin,hvhvin,hhhvin,hhvvin,hvvvin
		endif
			
; set internal variables of RAT
	
		file.name = config.tempdir+config.workfile1
		file.info = file_basename(strmid(inputfile,0,strpos(inputfile,'_read_me'))) ; Put here a string describing your data
		file.type = 200l                         ; data type (101 = single channel complex)
		file.dim  = 3l                           ; single channel data (two-dimensional array)
		file.xdim = anz_rg                          ; range image size
		file.ydim = anz_az                          ; azimuth image size
		file.zdim = 4l                           ; nr. of layers (set to 1 if not needed)
		file.vdim = 1l                           ; nr. of layers of layers (set to 1 if not needed)
		file.var  = 6l                           ; IDL variable type (6 = complex, 4 = floating point)
 		
		if channels eq 1 then begin
			file.dim  = 4l                           ; single channel data (two-dimensional array)
			file.xdim = anz_rg                          ; range image size
			file.ydim = anz_az                          ; azimuth image size
			file.zdim = 3l                           ; nr. of layers (set to 1 if not needed)
			file.vdim = 3l                           ; nr. of layers of layers (set to 1 if not needed)
			file.var  = 6l                           ; IDL variable type (6 = complex, 4 = floating point)
			file.type = 220l                         ; data type (101 = single channel complex)
		endif

; update file generation history (evolution)
                evolute,'Import SAR data from EMISAR.'

; read palette information
	
	palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
	palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

	file.window_name = 'Untitled.rat'
	generate_preview
	update_info_box
	
	endif	
end

