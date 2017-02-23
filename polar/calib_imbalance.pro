;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: phase_channel_calibration
; written by    : Stéphane Guillaso (TUB)
; last revision : 29.March.2004
; Perform  Co-polar Phase and Channel Polarimetric Calibration
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


pro calib_imbalance,phi_co=phi_co,amp_co=amp_co

;------------------------------------------------------------
; Global variables of RAT (FIXED)
; 
; Important:
;
; file.xdim = horizontal size of image
; file.ydim = vertical size of image
; file.zdim = number of layers, 1st matrix dimension
; file.vdim = used in matrix represenation for 2nd matrix dimension
; file.var  = IDL variable type, following the values returned by the size() command
; file.type = RAT data type, for details have a look in definitions.pro
;------------------------------------------------------------

	common rat, types, file, wid, config

;------------------------------------------------------------
; Error Handling 1 (EXAMPLE)
; -> check if the input file is suitable for the routine
;------------------------------------------------------------

; ---- Is it a lexicographic image?
	IF file.type NE 200 THEN BEGIN
		error = DIALOG_MESSAGE("scattering vector, lexicographic basis is required",DIALOG_PARENT=wid.base,TITLE='Error',/ERROR)
		RETURN
	ENDIF

;------------------------------------------------------------
; Graphical interface mode (FIXED+EXAMPLE)
; -> used only if the keyword /called is not set
;    this is important as possibly other routines want to use
;    your routine as a batch process
;------------------------------------------------------------

	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Co-channel phase and amplitude polarimetric calibration',/floating,/tlb_kill_request_events,/tlb_frame_attr)
	field1   = CW_FIELD(main,/FLOATING,TITLE='Co-Channel Phase     Imbalance --> arg(HH VV*): ',XSIZE=8)
	field2   = CW_FIELD(main,/FLOATING,TITLE='Co-Channel Amplitude Imbalance --> |HH|/|VV|:   ',XSIZE=8)
	buttons  = WIDGET_BASE(main,column=3,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
	WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]

	IF KEYWORD_SET(phi_co) THEN WIDGET_CONTROL,field1,SET_VALUE=phi_co
	IF KEYWORD_SET(amp_co) THEN WIDGET_CONTROL,field2,SET_VALUE=amp_co
	
	repeat begin
		event = widget_event(main)
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['Co-channel Phase and Amplitude Polarimetric Calibration',$
				' ',$
				'RAT module written 03/2004 by Stephane Guillaso']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		endif
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,field1,GET_VALUE=phi_co
	widget_control,field2,GET_VALUE=amp_co
	widget_control,main,/destroy
	if event.id ne  but_ok then return                    ; OK button _not_ clicked

	;------------------------------------------------------------
	; Error Handling 2 (EXAMPLE)
	; -> check validity of parameters
	;------------------------------------------------------------

	if amp_co le 0 then begin                                   ; Wrong box size ?
		error = DIALOG_MESSAGE("Co-Channel Amplitude Imbalance must be > 0", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	phi_co = -phi_co / !RADEG ;co-channel phase imbalance
	amp_co = 1/amp_co         ;co-channel amplitude imbalance

;------------------------------------------------------------
; change mousepointer to hourglass (FIXED)
;------------------------------------------------------------

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED


	;READ/WRITE HEADER FILE
	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
				
	;CALCULATING PREVIOUS SIZE & NUMBER OF BLOCKS
	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
	blocksizes = intarr(anz_blocks) + bs
	blocksizes[anz_blocks-1] = bs_last

	;POP UP PROGRESS WINDOW
	progress, MESSAGE='Co-Channel Imbalance Calibration...',/cancel_button
	

	;START BLOCK PROCESSING
	for ii=0,anz_blocks-1 do begin   
		progress,percent=(ii+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.zdim,file.xdim,blocksizes[ii]],type=file.var)
		readu,ddd,block
		
		block[0,*,*] = REFORM(block[0,*,*]) *      amp_co  * EXP(COMPLEX(0,phi_co   ))
		block[2,*,*] = REFORM(block[2,*,*]) * SQRT(amp_co) * EXP(COMPLEX(0,phi_co/2.))
		IF file.zdim EQ 4 THEN block[3,*,*] = REFORM(block[3,*,*]) * SQRT(amp_co) * EXP(COMPLEX(0,phi_co/2.))
		
		;WRITE BLOCK
		writeu,eee,block

	endfor
	free_lun,ddd,eee

	progress,/DESTROY
;------------------------------------------------------------
; Update file information (FIXED)
; -> If the parameters of the output data are not identical
;    to the input data, here all the new data parameters are
;    to be set. Take care to write also a correct file header
;------------------------------------------------------------
	
	IF FILE_TEST(file.name+'.system') THEN BEGIN
		file_copy, file.name+'.system',outputfile+'.system',/OVERWRITE
		file_move, outputfile+'.system',finalfile+'.system',/OVERWRITE
	ENDIF
	file.name = finalfile
	file_move,outputfile,finalfile,/overwrite
	
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

end
