;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rm_flatearth_airborne
; written by    : Stephane Guillaso (TUB)
; last revision : 29.april.2004
; Remove flatearth phase component in the airborne case
; using a parameter file
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

PRO rm_flatearth_airborne
	common rat, types, file, wid, config

;------------------------------------------------------------------------
; Error handling:
;   -> check if file is a interferometric or interferogram
;------------------------------------------------------------------------
	if (file.type lt 300) or (file.type gt 302) then begin
		mes = 'Data have to be interferometric image pair or interferogram'
		error_button = dialog_message(mes, dialog_parent=wid.base, title='Error', /error)
		return
	endif
	

;------------------------------------------------------------------------
; - Create the insar structure
;------------------------------------------------------------------------
	insar = {$
		polarisation_channel         : "", $
		master_track_name            : "", $
		slave_track_name             : "", $
		info                         : "", $
		wavelength                   : 0.d,$
		chirp_bandwidth              : 0.d,$
		range_sampling               : 0.d,$
		range_sampling_rate          : 0.d,$
		range_delay                  : 0.d,$
		terrain_elevation            : 0.d,$
		speed_of_light               : 2.99708e+08,$
		xdim_or                      : 0l,$ ;original size of data
		ydim_or                      : 0l,$
		xdim                         : 0l,$ ;current size of data
		ydim                         : 0l,$ 
		xmin                         : 0l,$
		xmax                         : 0l,$
		ymin                         : 0l,$
		ymax                         : 0l,$
		range_baseline               : 0.d,$
		height_baseline              : 0.d,$
		altitude_above_ground_master : 0.d,$
		altitude_above_ground_slave  : 0.d,$
		range_bin_first_master       : 0l,$
		range_bin_first_slave        : 0l,$
		baseline                     : 0.d,$
		alpha                        : 0.d$
	}

;------------------------------------------------------------------------
; - set the input airborne parameter file name to an empty string
;------------------------------------------------------------------------
	inputfile = ''

;------------------------------------------------------------------------
; - Generate the graphical user interface
;------------------------------------------------------------------------
	; define the main widget
	main = widget_base(group_leader=wid.base, row=3, title='Remove flatearth phase - Airborne case',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		; Get interferometric config file
		sub_input_param_file = widget_base(main,column=2)
			text_param_file = cw_field(sub_input_param_file,value=inputfile,/string,xsize=60,title='Airborne system file:')
			brow_param_file = widget_button(sub_input_param_file,value='browse',ysize=35)
		
		; Indicate a group buttons if you want to perform a symmetric or not flatearth phase component removal
		button_name = ['symmetric','only slave track']
		button3 = cw_bgroup(main,button_name, /exclusive, /return_index)
	
		; Generate the three bottom button OK - Cancel - Info
		buttons = widget_base(main, column=3, /frame)
			but_ok   = widget_button(buttons, value=' ok ',     xsize=80, /frame)
			but_canc = widget_button(buttons, value=' cancel ', xsize=60)
			but_info = widget_button(buttons, value=' info ',   xsize=60)

	; Realise the main widget and define the default button
	widget_control, main, /realize, default_button = but_ok,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]


;------------------------------------------------------------------------
; - If file is not interferometric image pair, 
;   desactivate the symmetric operation
;------------------------------------------------------------------------
	if (file.type eq 301) or (file.type eq 302) then widget_control, button3, sensitive=0
	
	
;------------------------------------------------------------------------
; - Loop events to catch the information provided by widget
;------------------------------------------------------------------------
	repeat begin
		
		; Get an event from the main widget
		event = widget_event(main)
		
		; get the parameter file
		if event.id eq brow_param_file then begin
			path = config.workdir
			inputfile = dialog_pickfile(title='Open airborne system file', dialog_parent=wid.base, filter='*.par', /must_exist, path=path, get_path=path)
			if strlen(inputfile) gt 0 then config.workdir = path
 			widget_control, text_param_file, set_value=inputfile
		endif
		
		; If info button is choosen
		if event.id eq but_info then begin
			infotext = ['REMOVE FLATEARTH PHASE - AIRBORNE CASE',$
			' ',$
			'RAT module written 03-04/2004 by Stephane Guillaso',$
			' ']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
		
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'


;------------------------------------------------------------------------
; - Get information provided by widget
;   user choose symmetric or not flatearth phase component removal
;------------------------------------------------------------------------
	widget_control, button3, get_value=symetric
	
;------------------------------------------------------------------------
; - Destroy the widget display
;------------------------------------------------------------------------
	widget_control, main, /destroy
	
;------------------------------------------------------------------------
; - Test if we have choose the cancel button. If yes exit the procedure
;------------------------------------------------------------------------
	if event.id ne but_ok then return

;------------------------------------------------------------------------
; - Test about the airborne system file
;------------------------------------------------------------------------
	; is a airborne system file indicated
	if inputfile eq '' then begin
		error = dialog_message("Please select airborne system file", dialog_parent=wid.base, title='Error', /error)
		return
	endif else begin
		; read the header airborne parameter file
		openr,ddd,inputfile,/xdr,/get_lun
		type = 0l & readu,ddd,type
		if type ne 399 then begin
			error = dialog_message("Wrong airborne system file", dialog_parent=wid.base, title='Error', /error)
			free_lun,ddd
			return
		endif
	endelse

;------------------------------------------------------------------------
; - If the airborne system file is OK, read it and close it
;------------------------------------------------------------------------
	readu,ddd,insar
	free_lun,ddd
	
;------------------------------------------------------------------------
; - Make a test about the image dimension and these indicate 
;   by the airborne system file and propose to change the value if needed
;------------------------------------------------------------------------
	if file.xdim ne insar.xdim then begin
		mes = ['The range size of the image and those given by',$
			    'airborne system parameter are different',$
			    ' ',$
			    'Would you like to correct it ? -- Do not forget to save it again']
		tmp = dialog_message(mes, /question, dialog_parent=wid.base)
		; if "no" is choose quit the procedure
		if tmp eq 'Yes' then parameter_information,wid.base,insar=insar else return
	endif
		
;------------------------------------------------------------------------
; - If everything "OK", the flatearth phase component is generated
;   from the parameter file using the following function
;------------------------------------------------------------------------
	airborne_insar_info_gen, insar, fe=fe
	
;------------------------------------------------------------------------
; - Transform the arrow mouse pointer into a hourglass
;------------------------------------------------------------------------
	widget_control, /hourglass
	
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
	
;------------------------------------------------------------------------
; - Read and write header file
;------------------------------------------------------------------------
	head = 1l
	rrat,file.name,ddd,header=head, info=info, type=type
	srat,outputfile,eee,header=head, info=info, type=type
	
;------------------------------------------------------------------------
; - As the procedure works by block, this calculate all
;   information needed to work with block
;------------------------------------------------------------------------
	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
	blocksizes = intarr(anz_blocks) + bs
	blocksizes[anz_blocks-1] = bs_last
	
;------------------------------------------------------------
; Pop up progress window (FIXED)
;------------------------------------------------------------
	progress, message='Removing flatearth phase component...',/cancel_button
	
;------------------------------------------------------------
; Start block processing (FIXED)
;------------------------------------------------------------
	for ii=0,anz_blocks-1 do begin
		
		; display the progression of the loop according to the block used
		progress,percent=(ii+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		
		;create & read block
		block = make_array([file.zdim,file.xdim,blocksizes[ii]],type=file.var)
		readu,ddd,block
		
		; generate an array with the flatearth phase information
		fe_array = (fltarr(blocksizes[ii])+1.0) ## fe[insar.xmin:insar.xmax]
		
		; test about the symmetric case
		if (symetric eq 0) and (file.type eq 300) then begin
			block[0,*,*] = reform(block[0,*,*]) * exp(complex(0,   fe_array/2))
			block[1,*,*] = reform(block[1,*,*]) * exp(complex(0, - fe_array/2))
		endif else begin
			if file.type eq 300 then block[1,*,*] = reform(block[1,*,*]) * exp(complex(0,-fe_array))
			if file.type eq 301 then block[0,*,*] = reform(block[0,*,*]) * exp(complex(0, fe_array))
			if file.type eq 302 then block[0,*,*] = atan(exp(complex(0,block[0,*,*])) * exp(complex(0,-fe_array)),/phase)
		endelse
		
		; write block
		writeu,eee,block

	endfor ; end of the for about block
	
;------------------------------------------------------------
; Close opening files
;------------------------------------------------------------
	free_lun,ddd,eee
	
	
;------------------------------------------------------------
; Update file information (FIXED)
;------------------------------------------------------------
	file_move,outputfile,finalname,/overwrite
	file.name = finalname
	
;------------------------------------------------------------
; Generate and display preview image and info-text (FIXED)
;------------------------------------------------------------
	generate_preview
	update_info_box
	

;------------------------------------------------------------
; Return to main program (FIXED)
;------------------------------------------------------------
	widget_control,wid.draw,get_value=index
	wset,index
	
END

