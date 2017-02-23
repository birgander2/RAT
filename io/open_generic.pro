;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_generic
; written by    : Thomas Weser
; last revision : 23. Januar 2004
; define reads generic formats
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
pro open_generic,INPUTFILE = inputfile, PATH = path
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames
	common gui, widgets

; only for drawing the widgtes
	widgets={$
		filesel			:0l,$
		but_filesel		:0l,$
		filesel_label		:0l,$
		filesel_info		:0l,$
	     	headline		:01,$
		typelist_field		:0l,$
		but_new			:0l,$
		but_del			:0l,$
		description_field	:0l,$
		intype_field		:0l,$
		faktor_field		:0l,$
		height_field		:0l,$
		width_field		:0l,$
		xstart_field		:0l,$
		ystart_field		:0l,$
		xend_field		:0l,$
		yend_field		:0l,$
		cut_region_field	:0l,$
		xdrflag_field		:0l,$
		offset_data_field	:0l,$
		offset_header_field	:0l,$
		filetype_field		:0l,$
		blocksize_field		:0l,$
		convert_to_float_field	:0l,$
		prefix_field 		:0l,$
		channel_search_field	:0l,$
		but_load		:0l,$
		but_canc		:0l,$
		but_save		:0l$
		}



; ----------------------------------------------------------------------------------------
; extract the data and file types from definitions
	data_types = types[1:15]
	data_index = where(data_types ne '')
	data_types = data_types(data_index)

	filetypes = types
	filetypes[1:99] = ''
	fileindex = where(filetypes ne '')
	filetypes = filetypes(fileindex)

;define default format
	default_format ={format,$
	description 	:'Default setting',$
	intype 		:2l,$
	height		:0l,$
	width		:0l,$
	xstart		:1l,$
	ystart		:1l,$
	xend		:0l,$
	yend		:0l,$
	cut_region	:0l,$
	faktor		:1.0,$
	xdrflag		:1l,$
	offset_data	:0l,$
	offset_header	:0l,$
	filetype	:100l,$
	blocksize	:0l,$
	convert_to_float:1l,$
	channel_search	:0l$
		}
current_setting={format}



;------------------------------------------------------------

; read saved formats from file
	savefile = '~/.rat_generic_formats'
	if config.os eq 'windows' then savefile = getenv("USERPROFILE") + "\rat_generic_formats"
	
	if FILE_TEST(savefile) then restore,savefile else saved_formats=[default_format]


; define the gui
	main = WIDGET_BASE(GROUP_LEADER=wid.base,column=1,TITLE='Open generic files',/base_align_center,/floating,/tlb_kill_request_events,/tlb_frame_attr )
	  main_filesel=WIDGET_BASE(main,row=2,/base_align_center)
	    main_deco1 =WIDGET_BASE(main_filesel,column=2,/base_align_center)
	      widgets.filesel       = CW_FIELD(main_deco1,/string, TITLE='File:',XSIZE=50)
	      widgets.but_filesel   = WIDGET_BUTTON(main_filesel,VALUE='Select image ',xsize=120)
	    main_deco2 =WIDGET_BASE(main_filesel,column=2,/base_align_center)
	      widgets.filesel_label  = WIDGET_LABEL(main_deco2,Value='     File size:',xSIZE=100)
	      widgets.filesel_info  = WIDGET_LABEL(main_deco2,Value=' - - ',xSIZE=200,/align_center)
	  main_top = WIDGET_BASE(main,column=2,/frame,/base_align_center)
	     main_types = WIDGET_BASE(main_top,row=3,/base_align_center,/frame)
	     main_headline =  WIDGET_BASE(main_types,/base_align_center,/frame)
	     	widgets.headline = WIDGET_LABEL(main_headline,Value='Saved settings',xSIZE=145,/ALIGN_CENTER)
	        widgets.typelist_field = WIDGET_LIST(main_types,XSIZE=20,YSIZE=20)
	        control =  WIDGET_BASE(main_types,column=2,/base_align_center)
	           widgets.but_new = WIDGET_BUTTON(control,VALUE=' NEW ',xsize=60)
	           widgets.but_del = WIDGET_BUTTON(control,VALUE=' DELETE ',xsize=60,SENSITIVE=0)
	     main_pref           = WIDGET_BASE(main_top,row=11,/base_align_bottom)
	        widgets.description_field = CW_FIELD(main_pref,/string, TITLE=                                           'Description:',XSIZE=30)
		main_inputdata		    = WIDGET_BASE(main_pref,column=2,/base_align_left)
		  widgets.offset_data_field = CW_FIELD(main_inputdata,/long, TITLE=                                    'Data offset : ',XSIZE=7)
		  widgets.intype_field      = WIDGET_droplist(main_inputdata,value=data_types,uvalue=data_types, title='Data type  :',/align_left)
		  widgets.faktor_field = CW_FIELD(main_inputdata,/float, TITLE=                                    'Divide by : ',XSIZE=7)
		main_imagesize              = WIDGET_BASE(main_pref,column=2,/base_align_center)
		  main_imagedim             = WIDGET_BASE(main_imagesize,row=3,/base_align_left)
		      label		    = WIDGET_LABEL(main_imagedim,Value='Image size:',xSIZE=100,ysize=50,/align_left)
		      widgets.width_field   = CW_FIELD(main_imagedim,/long, TITLE= 'Width :',XSIZE=7)
		      widgets.height_field    = CW_FIELD(main_imagedim,/long, TITLE= 'Height:',XSIZE=7)
		  main_imagecut             = WIDGET_BASE(main_imagesize,row=3,/base_align_center,/frame)
		    main_row1		    = WIDGET_BASE(main_imagecut,column=2,/base_align_center)
		      widgets.cut_region_field= CW_BGROUP(main_row1,' ',label_left=                                 'Cut region:',/nonexclusive)
		      label		    = WIDGET_LABEL(main_row1,Value='  ',xSIZE=200,/align_center)
		    main_row2		    = WIDGET_BASE(main_imagecut,column=2,/base_align_center)
		      widgets.xstart_field    = CW_FIELD(main_row2,/long, TITLE='Xstart:',XSIZE=7)
		      widgets.xend_field	    = CW_FIELD(main_row2,/long, TITLE='Xend:',XSIZE=7)
		    main_row3		    = WIDGET_BASE(main_imagecut,column=2,/base_align_center)
		      widgets.ystart_field    = CW_FIELD(main_row3,/long, TITLE='Ystart:',XSIZE=7)
		      widgets.yend_field	    = CW_FIELD(main_row3,/long, TITLE='Yend:',XSIZE=7)
		widgets.filetype_field      = WIDGET_droplist(main_pref,value=filetypes,uvalue=filetypes, title=  'Rat file type:',/align_left)
 		widgets.offset_header_field = CW_FIELD(main_pref,/long, TITLE=                                    'Header lenght: ',XSIZE=7)
		widgets.blocksize_field     = CW_BGROUP(main_pref,' ',label_left=                                 'Read by row  :',/nonexclusive)
		widgets.xdrflag_field       = CW_BGROUP(main_pref,' ',label_left=                                 'Read with xdr:',/nonexclusive)
	  buttons = WIDGET_BASE(main,column=3,/frame)
	     widgets.but_load   = WIDGET_BUTTON(buttons,VALUE='Load image ',xsize=120,/frame,SENSITIVE=0)
	     widgets.but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
	     widgets.but_save = WIDGET_BUTTON(buttons,VALUE=' Save format ',xsize=120,SENSITIVE=0)

	WIDGET_CONTROL, main, /REALIZE, default_button = but_canc

; update widgets with all known formats
	update_widgets,saved_formats,0,data_index,fileindex,filetypes

	repeat begin    ; Event loop
		event_loop:
		event = widget_event(main)
		status=0l

		; save formats to file
		if (event.id eq widgets.but_save) then begin

			list_select=WIDGET_INFO(widgets.typelist_field,/list_select)
			save_widgets,saved_formats,list_select,data_index,fileindex,current_setting,0
			update_widgets,saved_formats,list_select,data_index,fileindex,filetypes
			save,filename=savefile,saved_formats
			WIDGET_CONTROL,widgets.but_del,SENSITIVE=0
			WIDGET_CONTROL,widgets.but_save,SENSITIVE=0
		endif

		; start a new setting
		if event.id eq widgets.but_new then begin

 			saved_formats=[saved_formats,default_format]
			list_select=WIDGET_INFO(widgets.typelist_field,/list_select)
			count=size(saved_formats,/N_ELEMENTS)
			if list_select lt 0 then list_select=0
			save_widgets,saved_formats,list_select,data_index,fileindex,current_setting,0
			update_widgets,saved_formats,list_select,data_index,fileindex,filetypes
			WIDGET_CONTROL,widgets.but_del,SENSITIVE=1
			WIDGET_CONTROL,widgets.but_save,SENSITIVE=1
			WIDGET_CONTROL,widgets.typelist_field,set_list_select=count-1
		endif

		; delete a setting
		if event.id eq widgets.but_del then begin

			list_select=WIDGET_INFO(widgets.typelist_field,/list_select)
			count=size(saved_formats,/N_ELEMENTS)

			if list_select eq (count-1) then begin
				saved_formats=[extrac(saved_formats,0,count-1)]
				list_select--
			endif else saved_formats=[extrac(saved_formats,0,list_select),extrac(saved_formats,list_select+1,count-list_select-1)]

			update_widgets,saved_formats,list_select,data_index,fileindex,filetypes
			WIDGET_CONTROL,widgets.but_del,SENSITIVE=0
			WIDGET_CONTROL,widgets.but_save,SENSITIVE=0
		end

		; change displaied settings
		if event.id eq widgets.typelist_field then begin
			list_select=WIDGET_INFO(widgets.typelist_field,/list_select)
			if list_select eq 0 then begin
				WIDGET_CONTROL,widgets.but_del,SENSITIVE=0
				WIDGET_CONTROL,widgets.but_save,SENSITIVE=0
			endif else begin
				WIDGET_CONTROL,widgets.but_del,SENSITIVE=1
				WIDGET_CONTROL,widgets.but_save,SENSITIVE=1
			endelse
			update_widgets,saved_formats,list_select,data_index,fileindex,filetypes
			WIDGET_CONTROL,widgets.typelist_field,set_list_select=list_select
		end

		; select a file
		if event.id eq widgets.but_filesel then begin
			inputfile = cw_rat_dialog_pickfile(TITLE='Open E-SAR file', $
			DIALOG_PARENT=wid.base, /MUST_EXIST, PATH= config.workdir, GET_PATH=path)
			if strlen(inputfile) gt 0 then begin
				config.workdir = path
				WIDGET_CONTROL,widgets.filesel,SET_VALUE=inputfile
				WIDGET_CONTROL,widgets.but_load,SENSITIVE=1
				openr,dummy,inputfile,/get_lun
				s=fstat(dummy)
				free_lun,dummy
				WIDGET_CONTROL,widgets.filesel_info,SET_VALUE=strtrim(string(s.size))+' bytes'
			endif
		endif

		; load the selected file with current settings
		if event.id eq widgets.but_load then begin
			WIDGET_CONTROL,/hourglass
			save_widgets,saved_formats,0,data_index,fileindex,current_setting,1
			generic_check_settings,current_setting,checked=status
			if status eq 1 then generic_load_image,inputfile,current_Setting,loading_ok=status
		endif

		; to cut a region 
		if event.id eq widgets.cut_region_field then begin
			WIDGET_CONTROL,widgets.cut_region_field,get_value =val
			if val eq 0 then begin
				WIDGET_CONTROL,widgets.xstart_field,SENSITIVE=0
				WIDGET_CONTROL,widgets.ystart_field,SENSITIVE=0
				WIDGET_CONTROL,widgets.xend_field,SENSITIVE=0
				WIDGET_CONTROL,widgets.yend_field,SENSITIVE=0
			endif else begin
				WIDGET_CONTROL,widgets.xstart_field,SENSITIVE=1
				WIDGET_CONTROL,widgets.ystart_field,SENSITIVE=1
				WIDGET_CONTROL,widgets.xend_field,SENSITIVE=1
				WIDGET_CONTROL,widgets.yend_field,SENSITIVE=1
			endelse

		end


	endrep until ((event.id eq widgets.but_load ) and (status eq 1)) or (event.id eq widgets.but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

	widget_control,main,/destroy                        ; remove main widget


end
;--------------------------------------------------------------------------------------



;--------------------------------------------------------------------------------------
pro update_widgets,saved_formats,list_select,data_index,fileindex,filetypes
	common rat, types, file, wid, config
	common gui, widgets

; calculate startindex
	startindex_intype = where(data_index eq saved_formats[list_select].intype-1)
	startindex_filetype = where(fileindex eq saved_formats[list_select].filetype)


	count =size(saved_formats,/N_ELEMENTS)
	array=strarr(count)
	for i=0,count-1 do begin
		array[i]=saved_formats[i].description
	end
	WIDGET_CONTROL,widgets.typelist_field,set_value=array
	WIDGET_CONTROL,widgets.description_field,SET_VALUE=saved_formats[list_select].description
	WIDGET_CONTROL,widgets.intype_field,set_droplist_select=startindex_intype
	WIDGET_CONTROL,widgets.faktor_field,set_value=saved_formats[list_select].faktor
	WIDGET_CONTROL,widgets.height_field,SET_VALUE=saved_formats[list_select].height
	WIDGET_CONTROL,widgets.width_field,SET_VALUE=saved_formats[list_select].width
	WIDGET_CONTROL,widgets.cut_region_field,SET_VALUE=saved_formats[list_select].cut_region
	if saved_formats[list_select].cut_region eq 0 then begin
		WIDGET_CONTROL,widgets.xstart_field,SENSITIVE=0
		WIDGET_CONTROL,widgets.ystart_field,SENSITIVE=0
		WIDGET_CONTROL,widgets.xend_field,SENSITIVE=0
		WIDGET_CONTROL,widgets.yend_field,SENSITIVE=0
	endif else begin
		WIDGET_CONTROL,widgets.xstart_field,SENSITIVE=1
		WIDGET_CONTROL,widgets.ystart_field,SENSITIVE=1
		WIDGET_CONTROL,widgets.xend_field,SENSITIVE=1
		WIDGET_CONTROL,widgets.yend_field,SENSITIVE=1
	endelse
	WIDGET_CONTROL,widgets.xstart_field,SET_VALUE=saved_formats[list_select].xstart
	WIDGET_CONTROL,widgets.ystart_field,SET_VALUE=saved_formats[list_select].ystart
	WIDGET_CONTROL,widgets.xend_field,SET_VALUE=saved_formats[list_select].xend
	WIDGET_CONTROL,widgets.yend_field,SET_VALUE=saved_formats[list_select].yend
	WIDGET_CONTROL,widgets.xdrflag_field,set_value=saved_formats[list_select].xdrflag
	WIDGET_CONTROL,widgets.offset_data_field,set_value=saved_formats[list_select].offset_data
	WIDGET_CONTROL,widgets.offset_header_field,set_value=saved_formats[list_select].offset_header
	WIDGET_CONTROL,widgets.filetype_field,set_droplist_select=startindex_filetype
	WIDGET_CONTROL,widgets.blocksize_field,set_value=saved_formats[list_select].blocksize


end
;--------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------
pro save_widgets,saved_formats,list_select,data_index,fileindex,setting,flag
	common rat, types, file, wid, config
	common gui, widgets

	widget_control,widgets.description_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].description =val $
	else setting.description = val
	index = Widget_Info(widgets.intype_field, /DropList_Select)
	if flag eq 0 then saved_formats[list_select].intype = data_index[index]+1 $
	else setting.intype = data_index[index]+1
	widget_control,widgets.height_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].height = val $
	else setting.height = val
	widget_control,widgets.width_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].width = val $
	else setting.width = val
	widget_control,widgets.cut_region_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].cut_region = val $
	else setting.cut_region = val
	widget_control,widgets.faktor_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].faktor = val $
	else setting.faktor = val
	widget_control,widgets.xstart_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].xstart = val $
	else setting.xstart = val
	widget_control,widgets.ystart_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].ystart = val $
	else setting.ystart = val
	widget_control,widgets.xend_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].xend = val $
	else setting.xend = val
	widget_control,widgets.yend_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].yend = val $
	else setting.yend = val
	widget_control,widgets.xdrflag_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].xdrflag = val $
	else setting.xdrflag = val
	widget_control,widgets.offset_data_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].offset_data = val $
	else setting.offset_data = val
	widget_control,widgets.offset_header_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].offset_header = val $
	else setting.offset_header = val
	index = Widget_Info(widgets.filetype_field, /DropList_Select)
	if flag eq 0 then saved_formats[list_select].filetype = fileindex[index] $
	else setting.filetype = fileindex[index]
	widget_control,widgets.blocksize_field,GET_VALUE=val
	if flag eq 0 then saved_formats[list_select].blocksize = val $
	else setting.blocksize = val

end
;--------------------------------------------------------------------------------------




;--------------------------------------------------------------------------------------

pro generic_check_settings,cs,checked=status

	if cs.cut_region eq 0 then begin
		cs.xstart=0
		cs.ystart=0
		cs.xend=cs.width
		cs.yend=cs.height
	end

	;totdo
;	if cs.width lt cs.xend then begin
 ;		e=DIALOG_MESSAGE(["Xend <= Width !"], TITLE='Input error',/error)
;		status=0l
;	endif else
status=1l
end
;--------------------------------------------------------------------------------------




;--------------------------------------------------------------------------------------

pro generic_load_image,inputfile,cs,loading_ok=status
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

;------------------------------------------------------------
;establish error handler

	catch, error_number
	if error_number ne 0 then begin
		error = DIALOG_MESSAGE(["Could not read image !","Change settings"], DIALOG_PARENT = wid.base, TITLE='Unrecognized Format',/error)
		catch ,/cancel
		free_lun,dummy
		status=0l
		return
	end

; undo function
                undo_prepare,outputfile,finalfile,CALLED=CALLED
                open_rit,/EMPTY ; no parameters are set: delete the old ones!

; open file
	if cs.xdrflag eq 1 then openr,dummy,inputfile,/get_lun,/xdr $
	else  		  	openr,dummy,inputfile,/get_lun,/swap_if_big_endian

; don`t read header
	point_lun,dummy,cs.offset_header

	anzy=cs.yend-cs.ystart
	anzx=cs.xend-cs.xstart
	case cs.blocksize of
		1	:	begin	; read by line
					anz_blocks=anzy
					bs_last=1
					bs=1
				end
		else	:	begin
					bs = config.blocksize
					calc_blocks_normal,anzy,bs,anz_blocks,bs_last
				end
	endcase
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last


; set rat outtype
	case cs.intype of
		6l	:	outype=6l
		9l	:	outype=9l
		else	:	outype=4l
	endcase

	srat,config.tempdir+config.workfile1,eee,header=[2l,anzx,anzy,outype],type=0l

; now read the image(s)
	if (cs.ystart ne 0 ) then begin
		block =make_array([cs.width,cs.ystart],type=cs.intype)
		readu,dummy,block
	endif
	for i=0,anz_blocks-1 do begin
		block = make_array([cs.width,blocksizes[i]],type=cs.intype)
		readu,dummy,block
		if (cs.intype ne 6l) and (cs.intype ne 9l) then block=float(block)+cs.offset_data $
		else block+=cs.offset_data
		block=block/cs.faktor
		if (cs.xstart ne 0) or (cs.xend ne (cs.width-cs.xstart)) then begin
			if blocksizes[i] ne 1 then block=extrac(block,cs.xstart,0,cs.xend-cs.xstart,blocksizes[i]) $
			else block=extrac(block,cs.xstart,cs.xend-cs.xstart)
		end
		writeu,eee,block
	endfor

	free_lun,dummy,eee
	zdim=1
	dim=2


; read header

	head = 1l
	rrat,config.tempdir+config.workfile1,ins1,header=head,info=info
	free_lun,ins1

; analyse header

	file.name = config.tempdir+config.workfile1
	file.info = info
	file.type= cs.filetype
	file.dim  = dim
	file.xdim = anzx
	file.ydim = anzy
	file.zdim = zdim
	file.vdim = 1l
	file.var  = outype

; update file generation history (evolution)
	
	evolute,'Import SAR data from "generic".'

; read palette information
	
	palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
	palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

	file.window_name = 'Untitled.rat'
	generate_preview
	update_info_box

	status=1l
end
