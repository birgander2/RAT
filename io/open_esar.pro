;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_esar
; written by    : Thomas Weser (TUB)
; last revision : 16. Januar 2004
; Open files in DLR E-SAR format
;
; revision      : mn, oct'09
; open multiple files in batch mode
; (just provide list of correct files; not many checks are done)
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

pro open_esar, CALLED=CALLED, INPUTFILE = inputfile, PATH = path
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

;;; if more than 1 inputfiles are provided
   if n_elements(inputfile) gt 1 then begin
      readrgb = n_elements(inputfile)
      result  = inputfile
      rgb = indgen(readrgb)
      inputfile = file_basename(result[0])
      path = file_dirname(result[0])
   endif

; define known formats

	image_types = [$
	;SingleLook Complex Image, Ground Range (Int)
	{$
		search_string:'int_slc_geo',	$
		intype:2l,		$
		outype:6l,		$
		xdrflag:0l,		$
		offset:0l,		$
		filetype:101l,		$
		blocksize:1l,		$
		convert_to_float:0l,	$
		prefix:['i'],		$
		channel_search:1l	$
		},$

	;SingleLook Complex Image, SLANT  Range (Float)
	 {$
		search_string:'_slc',	$
		intype:6l,		$
		outype:6l,		$
		xdrflag:1l,		$
		offset:0l,		$
		filetype:101l,		$
		blocksize:0l,		$
		convert_to_float:0l,	$
		prefix:['i'],		$
		channel_search:1l	$
		},$

	 {$
		search_string:'_flt',	$
		intype:4l,		$
		outype:4l,		$
		xdrflag:1l,		$
		offset:0l,		$
		filetype:100l,		$
		blocksize:0l,		$
		convert_to_float:0l,	$
		prefix:['i'],		$
		channel_search:1l	$
		},$

	; MutiLook Detected Image , GEOCODED     (Integer)
	 {$
		search_string:'_int_geo',	$
		intype:2l,		$
		outype:4l,		$
		xdrflag:0l,		$
		offset:32768l,		$
		filetype:100l,		$
		blocksize:0l,		$
		convert_to_float:1l,	$
		prefix:['i'],		$
		channel_search:0l	$
		},$

	; MutiLook Detected Image , GROUND Range (Integer)
	 {$
		search_string:'_intg',	$
		intype:2l,		$
		outype:4l,		$
		xdrflag:0l,		$
		offset:32768l,		$
		filetype:100l,		$
		blocksize:0l,		$
		convert_to_float:1l,	$
		prefix:['i'],		$
		channel_search:0l	$
		},$

	; MutiLook Detected Image , SLANT  Range (Integer)
	 {$
		search_string:'_int',	$
		intype:2l,		$
		outype:4l,		$
		xdrflag:0l,		$
		offset:32768l,		$
		filetype:100l,		$
		blocksize:0l,		$
		convert_to_float:1l,	$
		prefix:['i'],		$
		channel_search:0l	$
		},$

	; interferometric coherence
	 {$
		search_string:'coh',	$
		intype:1l,		$
		outype:4l,		$
		xdrflag:1l,		$
		offset:1l,		$
		filetype:310l,		$
		blocksize:1l,		$
		convert_to_float:1l,	$
		prefix:['coh'],		$
		channel_search:0l	$
		},$

	; interferometric image pair
	 {$
		search_string:'if',	$
		intype:4l,		$
		outype:4l,		$
		xdrflag:1l,		$
		offset:0l,		$
		filetype:302l,		$
		blocksize:0l,		$
		convert_to_float:1l,	$
		prefix:['if'],		$
		channel_search:0l	$
		}$

	; ????
;	 {$
;		search_string:'dem',	$
;		intype:2l,		$
;		outype:4l,		$
;		xdrflag:0l,		$
;		offset:0l,		$
;		filetype:100l,		$
;		blocksize:0l,		$
;		convert_to_float:1l,	$
;		prefix:['dem'],		$
;:		channel_search:0l	$
;		}$
			]

;------------------------------------------------------------
;establish error handler
;	catch, error_number
;	if error_number ne 0 then begin
;		error = DIALOG_MESSAGE(["Could not read image !",!err.msg], DIALOG_PARENT = wid.base, TITLE='Unrecognized Format',/error)
;		catch ,/cancel
;		return
;	end

	;don't print math exceptions
	;!EXCEPT=0

;------------------------------------------------------------

; stop

	if not (keyword_set(inputfile) or keyword_set(path))  then begin
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open E-SAR file', $
		DIALOG_PARENT=wid.base, FILTER = '*.dat', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

; change mousepointer

		WIDGET_CONTROL,/hourglass

; undo function
		undo_prepare,outputfile,finalfile,CALLED=CALLED
		open_rit,/EMPTY ; no parameters are set: delete the old ones!

; converting Rolf's format to RAT
;		filename = StrMid( inputfile, StrLen(Path) )
      filename = file_basename(inputfile)

		;search for known formats
		for i=0, (size(image_types,/N_ELEMENTS)-1) do begin
			found = STREGEX([filename],image_types[i].search_string)
			if found GT -1 then begin
				imageformat = i	;save the typ
				;now check the prefix
				for k= 0,(size(image_types[i].prefix,/N_ELEMENTS)-1) do begin
					tmp = StrMid( filename,0, max(strlen(image_types[i].prefix[k])) )
					found = STREGEX([tmp],image_types[i].prefix[k])
					if found gt -1 then begin
						imageprefix=k ; save the prefix
						goto, format_found
					end
				end
			end
		end

		if found eq -1 then begin
			error = DIALOG_MESSAGE(["Format not recognized."], DIALOG_PARENT = wid.base, TITLE='Unrecognized Format',/error)
			return
		end
;--------------------------------------------------------------------------
		format_found:

		; set parameters for detected image
		intype =  image_types[imageformat].intype
		outype = image_types[imageformat].outype
		offset = image_types[imageformat].offset
		xdrflag = image_types[imageformat].xdrflag
		file.type = image_types[imageformat].filetype
		blocksize = image_types[imageformat].blocksize
		convert_to_float=image_types[imageformat].convert_to_float
		channel_search = image_types[imageformat].channel_search

;--------------------------------------------------------------------------

      if n_elements(result) le 1 then begin
; load more channels if possible
		rgb=intarr(1)
		rgb[0]=0
		result=strarr(1)
		result[0]=inputfile
		readrgb=1
		if channel_search eq 1 then begin
			channelpos= STREGEX([filename],'_ch*')
			if channelpos ne -1 then begin
				searchstring=	path+ $
			     		StrMid(filename ,0, channelpos+3)+'*'+ $
				     	StrMid(filename ,channelpos+4,Strlen(filename))
					result_tmp=file_search( searchstring,COUNT=anz)
					count_files=size(result_tmp,/N_ELEMENTS)
					count_channels=count_files
					if count_files gt 4 then count_channels=4
					if count_files gt 2 then begin
						result=result_tmp
						main = WIDGET_BASE(GROUP_LEADER=wid.base,column=1,TITLE='Load more channels ',/base_align_center,/floating,/tlb_kill_request_events,/tlb_frame_attr )
 						main_top = WIDGET_BASE(main,row=count_files+1,/frame)
						main_label=WIDGET_BASE(main_top,column=count_channels+1)
						str=''
						for i=0,strlen(result(0)) do str+=' '
						label=WIDGET_LABEL(main_label,VALUE=str)
						label_HH=WIDGET_LABEL(main_label,VALUE='HH ')
						label_VV=WIDGET_LABEL(main_label,VALUE=' VV ')
						label_HV=WIDGET_LABEL(main_label,VALUE=' HV ')
						if count_channels eq 4 then label_VH=WIDGET_LABEL(main_label,VALUE=' VH')
						labels=intarr(count_files)
						button=intarr(count_channels,count_files)
						main_labels=intarr(count_files)
						for i=0,count_files-1 do begin
							main_labels[i]=WIDGET_BASE(main_top,column=count_channels+1)
							labels[i]=WIDGET_LABEL(main_labels[i],VALUE=result[i])
							button(0,i) = CW_BGROUP(main_labels[i],'',/nonexclusive)
							button(1,i) = CW_BGROUP(main_labels[i],'',/nonexclusive)
							button(2,i) = CW_BGROUP(main_labels[i],'',/nonexclusive)
							if count_channels eq 4 then button(3,i) = CW_BGROUP(main_labels[i],'',/nonexclusive)
						endfor
						buttons = WIDGET_BASE(main,column=2,/frame)
	     					but_ok   = WIDGET_BUTTON(buttons,VALUE=' Load selected channels ',xsize=180,/frame)
	    					but_canc = WIDGET_BUTTON(buttons,VALUE=' Load single file ',xsize=160)

						WIDGET_CONTROL, main, /REALIZE, default_button = but_canc
						repeat begin
							event = WIDGET_EVENT(main)
							anz=0
							if event.id eq but_ok then begin
								drop=intarr(count_channels,count_files)
								rgb=intarr(count_channels)
								rgb+=-1
								for i=0,(count_files-1) do begin
									WIDGET_CONTROL,long(button(0,i)),GET_VALUE=tmp1
									WIDGET_CONTROL,long(button(1,i)),GET_VALUE=tmp2
									WIDGET_CONTROL,long(button(2,i)),GET_VALUE=tmp3
									if count_channels eq 4 then begin
										WIDGET_CONTROL,long(button(3,i)),GET_VALUE=tmp4
										drop(*,i)=[tmp1,tmp2,tmp3,tmp4]
									endif else drop(*,i)=[tmp1,tmp2,tmp3]
									row=where(drop(*,i) ne 0)
									test=size(row,/N_ELEMENTS)
										if test ne 1 then begin
											anz=0
											break
										endif
									if row ne -1 then rgb(where(drop(*,i) ne 0))=i
									anz+=drop(0,i)
									anz+=drop(1,i)
									anz+=drop(2,i)
									if count_channels eq 4 then anz+=drop(3,i)
								end

								if count_channels eq 4 then begin
									if ((anz ne 4) or (rgb(0)eq-1 ) or (rgb(1) eq -1) or (rgb(2) eq -1) or ( rgb(3) eq -1 )) $
									then begin
								 		error = DIALOG_MESSAGE(["select 4 channels (1 x HH, 1 x VV, 1 x HV, 1 x VH)"], DIALOG_PARENT = wid.base, TITLE='RGB import',/error)
										anz=0
									end
								endif else begin
									if ((anz ne 3) or(rgb(0)eq -1)or(rgb(1)eq -1)or(rgb(2)eq -1)) $
								 	then begin
								 		error = DIALOG_MESSAGE(["select 3 channels (1 x HH, 1 x VV, 1 x HV)"], DIALOG_PARENT = wid.base, TITLE='RGB import',/error)
										anz=0
									endif
								endelse
							end

						endrep until (event.id eq but_ok and anz eq count_channels) or event.id eq but_canc or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
						if event.id eq but_ok and anz eq count_channels then readrgb=count_channels

						widget_control,main,/destroy
					end
			end
		end
      end

; open the image(s)
		files= lonarr(readrgb)
		anzx  = lonarr(readrgb)
		anzy  = lonarr(readrgb)
		dummy=0l
		dummy1=0l
		dummy2=0l
		for i=0,readrgb-1 do begin
			if xdrflag eq 1 then begin
				openr,dummy,result(rgb(i)),/get_lun,/xdr ;,/swap_if_big_endian
				files(i)=dummy
			endif else begin
				openr,dummy,result(rgb(i)),/get_lun,/swap_if_big_endian
				files(i)=dummy
			endelse
			dummy=files(i)
			readu,dummy,dummy1
			readu,dummy,dummy2

			anzx(i)=dummy1
			anzy(i)=dummy2

;			if (anzx(i) lt 1)or (anzy(i) lt 1) then begin
;				anzx(i) = swap_endian(anzx(i),/swap_if_little_endian)
;				anzy(i) = swap_endian(anzy(i),/swap_if_little_endian)
;			endif

			if anzx[i] lt 0 or anzy[i]  lt 0 or anzx[i] gt 1000000l or anzy[i] gt 1000000l  then begin
				bits=[0,1,2,8,4,8,8,0,0,16,0,0,4,4,8,8]
				main = WIDGET_BASE(GROUP_LEADER=wid.base,column=1,TITLE='No Header ?',/modal,/base_align_center)
	   			main_top = WIDGET_BASE(main,row=6,/frame)
				dummy=files(i)
				inf = fstat(dummy)
				label1 =WIDGET_LABEL(main_top,VALUE="No header found in image (tape data?)")
				label2 =WIDGET_LABEL(main_top,VALUE="found     : "+types[file.type])
				label3 =WIDGET_LABEL(main_top,VALUE="data type : "+types[intype]+' ('+strtrim(string(bits[intype]))+' bytes)')
				label4 =WIDGET_LABEL(main_top,VALUE="image size: "+strtrim(string(inf.size)))
				anzx_field = CW_FIELD(main_top,/long, TITLE='Image Width  : ',XSIZE=7)
				anzy_field = CW_FIELD(main_top,/long, TITLE='Image Height : ',XSIZE=7)
				buttons = WIDGET_BASE(main,column=2,/frame)
	     			but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	    			but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)

				WIDGET_CONTROL, main, /REALIZE, default_button = but_canc

				repeat begin; Event loop
					event = widget_event(main)
					if event.id eq but_ok then begin
						widget_control,anzx_field,GET_VALUE=tmpx
						widget_control,anzy_field,GET_VALUE=tmpy
						anzx(i)=tmpx
						anzy(i)=tmpy
						if (inf.size/anzy(i)/bits[intype]) ne anzx(i) then begin
							error = DIALOG_MESSAGE(["Worng size of image ",$
									strtrim(string(anzx(i)))+' * '+strtrim(string(anzy(i)))+ ' * '+types[intype]+' ('+strtrim(string(bits[intype])+' bytes)')+' = '+$
									strtrim(string(anzx(i)*anzy(i)*bits[intype]))+' ne '+string(inf.size)], DIALOG_PARENT = wid.base, TITLE='Unrecognized Format',/error)
						end
					endif else begin
						widget_control,main,/destroy
						return
					end
				endrep until (event.id eq but_ok) and (inf.size/anzy(i)/bits[intype] eq anzx(i))
				free_lun,dummy
				if xdrflag eq 1 then begin
					openr,dummy,result(rgb(i)),/get_lun,/xdr ;,/swap_if_big_endian
					files(i)=dummy
				endif else begin
					openr,dummy,result(rgb(i)),/get_lun,/swap_if_big_endian
					files(i)=dummy
				endelse
				widget_control,main,/destroy
			endif
		endfor

		WIDGET_CONTROL,/hourglass



; calculating preview size and number of blocks


		case blocksize of
			1	:	begin	; read by line
						anz_blocks=anzy(0)
						bs_last=1
						bs=1
					end
			else	:	begin
						bs = config.blocksize
						calc_blocks_normal,anzy(0),bs,anz_blocks,bs_last
					end
		endcase
		blocksizes = intarr(anz_blocks)+bs
		blocksizes[anz_blocks-1] = bs_last

		if imageformat eq 0 then anzx=anzx/2

; now read the image(s)
		if readrgb gt 1 then begin ;3 or 4 channels
			srat,config.tempdir+config.workfile1,eee,header=[3l,readrgb,anzx(0),anzy(0),outype],type=200l

			for i=0,anz_blocks-1 do begin

				block = make_array([anzx(0),blocksizes[i]],type=intype)
				if imageformat eq 0 then begin
					block = make_array([anzx(0)*2,blocksizes[i]],type=intype)
				endif

				oblock = make_array([readrgb,anzx(0),blocksizes[i]],type=outype)
				for j=0,readrgb-1 do begin
					dummy=files(j)
					readu,dummy,block
					if convert_to_float eq 1 then block=float(block)+offset
					if imageformat eq 0 then begin
						refblock = reform(block,2,anzx[0],blocksizes[i])
						oblock[j,*,*] = complex(reform(refblock[0,*,*]),reform(refblock[1,*,*]))
					endif else begin
						oblock[j,*,*] = block
					endelse
				endfor
;  				if readrgb eq 2 then begin
;  					dummy=make_array([anzx(0),blocksizes[i]],type=intype)
;  					oblock[2,*,*] = dummy
;  					endif
				writeu,eee,oblock
			endfor
			dummy=files(0)
			free_lun,dummy,eee
			zdim=readrgb
			dim=3
			file.type=200l
		endif else begin ; one channel

			srat,config.tempdir+config.workfile1,eee,header=[2l,anzx(0),anzy(0),outype],type=0l

			for i=0,anz_blocks-1 do begin
				block = make_array([anzx(0),blocksizes[i]],type=intype)
				if imageformat eq 0 then begin
					block = make_array([anzx(0)*2,blocksizes[i]],type=intype)
				endif
				dummy=files(0)
				readu,dummy,block
				if convert_to_float eq 1 then block=float(block)+offset else block+=offset
				if file.type eq 310l then block=block/255
				if imageformat eq 0 then begin
						refblock = reform(block,2,anzx[0],blocksizes[i])
						block = complex(reform(refblock[0,*,*]),reform(refblock[1,*,*]))
				endif
				writeu,eee,block
			endfor
			dummy=files(0)
			free_lun,dummy,eee
			zdim=1
			dim=2
		endelse

; generate info string

		prefix  = image_types[imageformat].prefix
		postfix = image_types[imageformat].search_string
		if prefix eq postfix then postfix = '.dat'
		info = strmid(filename,strlen(prefix))
		info = strmid(info,0,strpos(info,postfix))

;stop

		file.name = config.tempdir+config.workfile1
		file.info = info
		file.dim  = dim
		file.xdim = anzx(0)
		file.ydim = anzy(0)
		file.zdim = zdim
		file.vdim = 1l
		file.var  = outype

; update file generation history (evolution)

		evolute,'Import SAR data from ESAR.'

; read palette information

		palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
		palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

		file.window_name = 'Untitled.rat'
		generate_preview
		update_info_box

	endif
	close,/all

end


