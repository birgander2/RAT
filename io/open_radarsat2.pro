;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_radarsat2
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
pro open_radarsat2,INPUTFILE = inputfile
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='RADARSAT-2 import',/floating,/tlb_kill_request_events,/tlb_frame_attr )
	butt = cw_bgroup(main,[' Load quadpol data set',' Load single-pol data set'],set_value=0,row=2,/exclusive)
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
			infotext = ['RADARSAT-2 IMPORT',$
			' ',$
			'RAT module written 08/2005 by Andreas Reigber']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,butt,GET_VALUE=channels
	widget_control,main,/destroy
	if event.id ne but_ok then return                   ; OK button _not_ clicked

	if not keyword_set(inputfile) then begin    ; GUI for file selection
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open RADARSAT-2 file', $
		DIALOG_PARENT=wid.base, FILTER = '*.tif', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

; change mousepointer

		WIDGET_CONTROL,/hourglass                ; switch mouse cursor
; undo function
                undo_prepare,outputfile,finalfile,CALLED=CALLED
                open_rit,/EMPTY ; no parameters are set: delete the old ones!

; Get all the files

		if channels eq 0 then begin
			filex = file_basename(inputfile)
			filex = strsplit(filex,'_',/extract)

			file_hh = path+filex[0]+'_HH.tif'
			file_vv = path+filex[0]+'_VV.tif'
			file_hv = path+filex[0]+'_HV.tif'
			file_vh = path+filex[0]+'_VH.tif'

			if not (file_test(file_hh) and file_test(file_vv) and file_test(file_hv) and file_test(file_vh)) then begin
				channels = 1
				dummy = DIALOG_MESSAGE(["Not all of the quad-pol files found !","Loading single-pol instead"], DIALOG_PARENT = wid.base, TITLE='Error',/information)
			endif
		endif

; Load all the files
		;need to modify for tiled reading to get around memory constraints
		if channels eq 0 then begin

			info = query_tiff(file_hh,tinfo,geotiff=para)

			anz_rg = tinfo.dimensions[0]
			anz_az = tinfo.dimensions[1]
			t = 200l
			;write rat header
			srat,config.tempdir+config.workfile1,lun_out,header=[3l,4l,anz_rg,anz_az,6l],type=t

			progress,Message='Decoding RADARSAT-2 Polarimetric ...'
			;loop through tiff files and write rat file
			arr = complexarr(4,anz_rg,1)
			for i=0l,anz_az-1 do begin
				progress,percent=i*100.0/anz_az
				sub_rect = [0,i,anz_rg,1]
				dummy = read_tiff(file_hh,geotiff=para,sub_rect=sub_rect)
				arr[0,*,*] = complex(reform(dummy[0,*,*]),reform(dummy[1,*,*]))
				dummy = read_tiff(file_vv,geotiff=para,sub_rect=sub_rect)
				arr[1,*,*]  = complex(reform(dummy[0,*,*]),reform(dummy[1,*,*]))
				dummy = read_tiff(file_hv,geotiff=para,sub_rect=sub_rect)
				arr[2,*,*]  = complex(reform(dummy[0,*,*]),reform(dummy[1,*,*]))
				dummy = read_tiff(file_vh,geotiff=para,sub_rect=sub_rect)
				arr[3,*,*]  = complex(reform(dummy[0,*,*]),reform(dummy[1,*,*]))
				writeu,lun_out,arr
			endfor
			;close file handle
			free_lun,lun_out
		endif else begin

			info = query_tiff(inputfile,tinfo,geotiff=para)

			anz_rg = tinfo.dimensions[0]
			anz_az = tinfo.dimensions[1]
			t = 101l
			;write rat header
			srat,config.tempdir+config.workfile1,lun_out,header=[2l,anz_rg,anz_az,6l],type=t

			progress,Message='Decoding RADARSAT-2 Single Channel ...'
			;loop through tiff files and write rat file
			arr = complexarr(1,anz_rg,1)
			for i=0l,anz_az-1 do begin
				progress,percent=i*100.0/anz_az
				sub_rect = [0,i,anz_rg,1]
				dummy = read_tiff(inputfile,geotiff=para,sub_rect=sub_rect)
				arr[0,*,*] = complex(reform(dummy[0,*,*]),reform(dummy[1,*,*]))
				writeu,lun_out,arr
			endfor
			;close file handle
			free_lun,lun_out
		endelse

; set internal variables of RAT

		file.name = config.tempdir+config.workfile1
		file.info = 'RADARSAT-2 Data'            ; Put here a string describing your data
		file.type = 200l                         ; data type (101 = single channel complex)
		file.dim  = 3l                           ; single channel data (two-dimensional array)
		file.xdim = anz_rg                       ; range image size
		file.ydim = anz_az                       ; azimuth image size
		file.zdim = 4l                           ; nr. of layers (set to 1 if not needed)
		file.vdim = 1l                           ; nr. of layers of layers (set to 1 if not needed)
		file.var  = 6l                           ; IDL variable type (6 = complex, 4 = floating point)

		if channels eq 1 then begin
			file.dim  = 2l                           ; single channel data (two-dimensional array)
			file.xdim = anz_rg                          ; range image size
			file.ydim = anz_az                          ; azimuth image size
			file.zdim = 1l                           ; nr. of layers (set to 1 if not needed)
			file.vdim = 1l                           ; nr. of layers of layers (set to 1 if not needed)
			file.var  = 6l                           ; IDL variable type (6 = complex, 4 = floating point)
			file.type = 101l                         ; data type (101 = single channel complex)
		endif

; update file generation history (evolution)

		evolute,'Import SAR data from RADARSAT2.'

; read palette information

		palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
		palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

		file.window_name = 'Untitled.rat'
		generate_preview
		update_info_box

	endif
end

