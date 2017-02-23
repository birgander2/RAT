;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: parameter_information
; written by    : Stephane Guillaso (TUB)
; last revision : 24.march.2004
; Display a widget to give all parameter information needed
; to apply interferometry for airborne case
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

pro parameter_information,GroupLeader,insar=insar
	common rat, types, file, wid, config

;------------------------------------------------------------------------
; - Create the insar structure if not passed by argument
;------------------------------------------------------------------------
	if not keyword_set(insar) then begin
		insar = {$
			polarisation_channel         : "",$
			master_track_name            : "",$
			slave_track_name             : "",$
			info                         : "",$
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
	endif


;------------------------------------------------------------------------
; - Generate the graphical user interface
;------------------------------------------------------------------------
	ec_tl = 80
	ec_tl2 = 174
	titre = 'Airborne Parameter Information'
		
	main = widget_base(group_leader=groupleader,/column,title=titre,/floating,/tlb_kill_request_events,/tlb_frame_attr)
	
		
		;separate the work base into two columns
		sub = widget_base(main,/row)
		
			; First column
			sub_left = widget_base(sub,/column)
				
				; Information about the track
				sub_left_up = widget_base(sub_left,/column,/frame)
					labelL11 = widget_label(sub_left_up,value='TRACKS INFORMATIONS',/align_center)
 					field_polar  = cw_field(sub_left_up,title='Polarisation Channel: ',/string,xsize=11)
					subl3 = widget_base(sub_left_up,/row)
						labell31 = widget_label(subl3,value='Master',scr_xsize=ec_tl,/align_center)
						labell32 = widget_label(subl3,value='',scr_xsize=ec_tl2,/align_center)
						labell33 = widget_label(subl3,value='Slave',scr_xsize=ec_tl,/align_center)
					subl4 = widget_base(sub_left_up,/row)
						field_track1 = widget_text(subl4,/editable,scr_xsize=ec_tl)
						labell42 = widget_label(subl4,value='Name',scr_xsize=ec_tl2,/align_center)
						field_track2 = widget_text(subl4,/editable,scr_xsize=ec_tl)
					subl5 = widget_base(sub_left_up,/row)
						field_aag1 = widget_text(subl5,/editable,scr_xsize=ec_tl)
						labell42 = widget_label(subl5,value='Altitude Above Ground',scr_xsize=ec_tl2,/align_center)
						field_aag2 = widget_text(subl5,/editable,scr_xsize=ec_tl)
					subl6 = widget_base(sub_left_up,/row)
						field_rbf1 = widget_text(subl6,/editable,scr_xsize=ec_tl)
						labell42 = widget_label(subl6,value='Range Bin First',scr_xsize=ec_tl2,/align_center)
						field_rbf2 = widget_text(subl6,/editable,scr_xsize=ec_tl)
				
				; information about the image size
				sub_left_down = widget_base(sub_left,/column,/frame)
					labelld1 = widget_label(sub_left_down,value='DIMENSIONS',/align_center)
					subld = widget_base(sub_left_down,/row)
						subld1 = widget_base(subld,/column)
							field_xdimor = cw_field(subld1,title='Original X Size: ',xsize=5)
							field_xdim   = cw_field(subld1,title='Actual X Size:   ',xsize=5)
							field_xmin   = cw_field(subld1,title='X Start:         ',xsize=5)
							field_xmax   = cw_field(subld1,title='X End:           ',xsize=5)
						subld2 = widget_base(subld,/column)
							field_ydimor = cw_field(subld2,title='Original Y Size: ',xsize=5)
							field_ydim   = cw_field(subld2,title='Actual Y Size:   ',xsize=5)
							field_ymin   = cw_field(subld2,title='Y Start:         ',xsize=5)
							field_ymax   = cw_field(subld2,title='Y End:           ',xsize=5)
				
				; add additional information if you need
				sub_info = widget_base(sub_left,/column,/frame)
					labeli1 = widget_label(sub_info,value='ADDITIONAL INFORMATION',/align_center)
					field_info = widget_text(sub_info,/editable,scr_ysize=80,scr_xsize=347)
		
				
			; Second column
		   sub_rigth = widget_base(sub,/column)
		   	
		   	; airborne system parameter
		   	sub_rigth_up = widget_base(sub_rigth,/column,/frame)
		   		labelr21 = widget_label(sub_rigth_up,value='SYSTEM PARAMETERS',/align_center)
  		   		field_wave   = cw_field(sub_rigth_up,title='Wavelength:          ',/float,xsize=11)
  		   		field_chbd   = cw_field(sub_rigth_up,title='Chirp Bandwidth:     ',/float,xsize=11)
  		   		field_splg   = cw_field(sub_rigth_up,title='Speed of Light:      ',/float,xsize=11,value=insar.speed_of_light)
  		   		field_rs 	 = cw_field(sub_rigth_up,title='Range Sampling:      ',/float,xsize=11)
  		   		field_rsr	 = cw_field(sub_rigth_up,title='Range Sampling Rate: ',/float,xsize=11)
  		   		field_rd 	 = cw_field(sub_rigth_up,title='Range Delay:         ',/float,xsize=11)
  		   		field_te 	 = cw_field(sub_rigth_up,title='Terrain Elevation:   ',/float,xsize=11)
		   	
		   	; baseline information
		   	sub_rigth_down = widget_base(sub_rigth,/column,/frame)
		   		labelrd1 = widget_label(sub_rigth_down,value='BASELINE INFORMATION',/align_center)
		   		field_baserg = cw_field(sub_rigth_down,title='Range Baseline:      ',xsize=11)
		   		field_baseht = cw_field(sub_rigth_down,title='Height Baseline:     ',xsize=11)
		   		field_base   = cw_field(sub_rigth_down,title='Baseline:            ',xsize=11,/noedit)
		   		field_alpha  = cw_field(sub_rigth_down,title='Alpha:               ',xsize=11,/noedit)
					
					; button to calculate and display
					subrd = widget_base(sub_rigth_down,/row)
						button_calc = widget_button(subrd,value='Calculate...')
						button_disp = widget_button(subrd,value='Display...')
				
   	; generate the three bottom button ok - cancel - Info
   	buttons    = widget_base(main,/row,/frame)
   		but_ok	  = widget_button(buttons,value=' OK ',xsize=80,/frame)
   		but_read   = widget_button(buttons,value=' Read From File... ')
   		but_save   = widget_button(buttons,value=' Save Data... ')
   		but_info   = widget_button(buttons,value=' Info ',xsize=60)

	; Realise the main widget and define the default button
	widget_control, main, /realize, default_button = but_ok,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]


;------------------------------------------------------------------------
; - Display information if there is information
;------------------------------------------------------------------------
	if insar.xdim NE 0 then begin
		widget_control,field_polar, set_value=insar.polarisation_channel
		widget_control,field_track1,set_value=insar.master_track_name
		widget_control,field_track2,set_value=insar.slave_track_name
		widget_control,field_info,  set_value=insar.info
		widget_control,field_wave,  set_value=insar.wavelength
		widget_control,field_chbd,  set_value=insar.chirp_bandwidth
		widget_control,field_rs,	 set_value=insar.range_sampling
		widget_control,field_rsr,   set_value=insar.range_sampling_rate
		widget_control,field_rd,	 set_value=insar.range_delay
		widget_control,field_te,	 set_value=insar.terrain_elevation
		widget_control,field_splg,  set_value=insar.speed_of_light
		widget_control,field_xdimor,set_value=insar.xdim_or
		widget_control,field_ydimor,set_value=insar.ydim_or
		widget_control,field_xdim,  set_value=insar.xdim
		widget_control,field_ydim,  set_value=insar.ydim
		widget_control,field_xmin,  set_value=insar.xmin
		widget_control,field_xmax,  set_value=insar.xmax
		widget_control,field_ymin,  set_value=insar.ymin
		widget_control,field_ymax,  set_value=insar.ymax
		widget_control,field_baserg,set_value=insar.range_baseline
		widget_control,field_baseht,set_value=insar.height_baseline
		widget_control,field_aag1,  set_value=strcompress(insar.altitude_above_ground_master,/rem)
		widget_control,field_aag2,  set_value=strcompress(insar.altitude_above_ground_slave,/rem)
		widget_control,field_rbf1,  set_value=strcompress(insar.range_bin_first_master,/rem)
		widget_control,field_rbf2,  set_value=strcompress(insar.range_bin_first_slave,/rem)
		widget_control,field_base,  set_value=insar.baseline
		widget_control,field_alpha, set_value=insar.alpha
	endif

;------------------------------------------------------------------------
; - Define the current path directory
;------------------------------------------------------------------------
	path = config.workdir

;------------------------------------------------------------------------
; - Loop events to catch the information provided by widget
;------------------------------------------------------------------------
	repeat begin
		
		; Get an event from the main widget
		event = widget_event(main)
		
		; Retrieve the different parameters
		dummy = ""
		widget_control,field_polar, get_value=dummy & insar.polarisation_channel = dummy
		widget_control,field_track1,get_value=dummy & insar.master_track_name = dummy
		widget_control,field_track2,get_value=dummy & insar.slave_track_name = dummy
		widget_control,field_info,  get_value=dummy & insar.info = dummy
		dummy = 0.d
		widget_control,field_wave,  get_value=dummy & insar.wavelength = dummy
		widget_control,field_chbd,  get_value=dummy & insar.chirp_bandwidth = dummy
		widget_control,field_rs,    get_value=dummy & insar.range_sampling = dummy
		widget_control,field_rsr,   get_value=dummy & insar.range_sampling_rate = dummy
		widget_control,field_rd,    get_value=dummy & insar.range_delay = dummy
		widget_control,field_te,    get_value=dummy & insar.terrain_elevation = dummy
		widget_control,field_splg,  get_value=dummy & insar.speed_of_light = dummy
		widget_control,field_baserg,get_value=dummy & insar.range_baseline = dummy
		widget_control,field_baseht,get_value=dummy & insar.height_baseline = dummy
		widget_control,field_base,  get_value=dummy & insar.baseline = dummy
		widget_control,field_alpha, get_value=dummy & insar.alpha = dummy
		widget_control,field_aag1,  get_value=dummy & insar.altitude_above_ground_master = dummy
		widget_control,field_aag2,  get_value=dummy & insar.altitude_above_ground_slave = dummy
		dummy = 0l
		widget_control,field_xdimor,get_value=dummy & insar.xdim_or = dummy
		widget_control,field_ydimor,get_value=dummy & insar.ydim_or = dummy
		widget_control,field_xdim,  get_value=dummy & insar.xdim = dummy
		widget_control,field_ydim,  get_value=dummy & insar.ydim = dummy
		widget_control,field_xmin,  get_value=dummy & insar.xmin = dummy
		widget_control,field_xmax,  get_value=dummy & insar.xmax = dummy
		widget_control,field_ymin,  get_value=dummy & insar.ymin = dummy
		widget_control,field_ymax,  get_value=dummy & insar.ymax = dummy
		widget_control,field_rbf1,  get_value=dummy & insar.range_bin_first_master = dummy
		widget_control,field_rbf2,  get_value=dummy & insar.range_bin_first_slave = dummy
		
		; If button save if called
		if event.id eq but_save then begin
			fichname = dialog_pickfile(dialog_parent=main,filter='*.par',title='save airborne system file',path = path, get_path=path )
			if (strlen(fichname) gt 0) then begin
				openw,xxx,fichname,/xdr,/get_lun
				writeu,xxx,399l,insar
				free_lun,xxx
				info = dialog_message('data saved ...', dialog_parent=main)
			endif else info = dialog_message('data not saved, try again...', dialog_parent=main)
		endif
		
		; If read button is called
		if event.id eq but_read then begin
			fichname = dialog_pickfile(dialog_parent=main, filter='*.par', title='read airborne system file', path=path, get_path=path )
			if (strlen(fichname) gt 0) and file_test(fichname) then begin
				openr,xxx,fichname,/xdr,/get_lun
				type = 0l & readu,xxx,type
				if type eq 399 then begin
					readu,xxx,insar
					; ---- display information
					widget_control,field_polar, set_value=insar.polarisation_channel
					widget_control,field_track1,set_value=insar.master_track_name           
					widget_control,field_track2,set_value=insar.slave_track_name            
					widget_control,field_info,  set_value=insar.info                        
					widget_control,field_wave,  set_value=insar.wavelength                  
					widget_control,field_chbd,  set_value=insar.chirp_bandwidth             
					widget_control,field_rs,    set_value=insar.range_sampling              
					widget_control,field_rsr,   set_value=insar.range_sampling_rate         
					widget_control,field_rd,    set_value=insar.range_delay                 
					widget_control,field_te,    set_value=insar.terrain_elevation           
					widget_control,field_splg,  set_value=insar.speed_of_light	  
					widget_control,field_xdimor,set_value=insar.xdim_or 
					widget_control,field_ydimor,set_value=insar.ydim_or
					widget_control,field_xdim,  set_value=insar.xdim              
					widget_control,field_ydim,  set_value=insar.ydim  
					widget_control,field_xmin,  set_value=insar.xmin
					widget_control,field_xmax,  set_value=insar.xmax 
					widget_control,field_ymin,  set_value=insar.ymin                        
					widget_control,field_ymax,  set_value=insar.ymax                        
					widget_control,field_baserg,set_value=insar.range_baseline              
					widget_control,field_baseht,set_value=insar.height_baseline             
					widget_control,field_aag1,  set_value=strcompress(insar.altitude_above_ground_master,/rem)
					widget_control,field_aag2,  set_value=strcompress(insar.altitude_above_ground_slave,/rem)
					widget_control,field_rbf1,  set_value=strcompress(insar.range_bin_first_master,/rem)      
					widget_control,field_rbf2,  set_value=strcompress(insar.range_bin_first_slave,/rem)     
					widget_control,field_base,  set_value=insar.baseline                    
					widget_control,field_alpha, set_value=insar.alpha
				endif else info = dialog_message('not a airborne parameter file...', dialog_parent=main)
				free_lun,xxx      
			endif
		endif
		
		; the calcul
		if event.id eq button_calc then begin
			if (insar.range_baseline ne 0) and (insar.height_baseline ne 0) then begin
				insar.baseline = sqrt(insar.range_baseline^2 + insar.height_baseline^2)
				insar.alpha    = atan(insar.height_baseline,insar.range_baseline)
				widget_control,field_base,  set_value=insar.baseline                    
				widget_control,field_alpha, set_value=insar.alpha                       
			endif else begin
				message = 'range baseline and height baseline must be not equal to zero'
				info = dialog_message(message,/error, dialog_parent = main, title='errors')
			endelse 
		endif
		
		if event.id eq button_disp then begin
			if (insar.wavelength ne 0) and						 $
		 		(insar.chirp_bandwidth ne 0) and 				 $
				(insar.range_sampling ne 0) and					 $
				(insar.range_sampling_rate ne 0) and			 $
				(insar.range_delay ne 0) and						 $
				(insar.terrain_elevation ne 0) and				 $
				(insar.speed_of_light ne 0) and					 $
				(insar.range_baseline ne 0) and					 $
				(insar.height_baseline ne 0) and 				 $
				(insar.baseline ne 0) and							 $
				(insar.alpha ne 0) and								 $
				(insar.altitude_above_ground_master ne 0) and $
				(insar.altitude_above_ground_slave ne 0) and  $
				(insar.xdim_or ne 0) then display_information_system,groupleader,insar else begin
					mes = 'all fields must be filled and not equal to zero.'
					info = dialog_message(mes,/error, dialog_parent = main, title='errors')
			endelse 
		endif
		
		; Display the info
		if event.id eq but_info then begin
			infotext = ['AIRBORNE PARAMETER ANALYSIS',$
			' ',$
			'RAT module written 03/2004 by Stephane Guillaso',$
			' ']
			info = dialog_message(infotext, dialog_parent = main, title='information')
		end
		
	endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

;------------------------------------------------------------------------
; - Destroy the widget display
;------------------------------------------------------------------------
	widget_control,main, /destroy

;------------------------------------------------------------
; Return to main program (FIXED)
;------------------------------------------------------------
	widget_control,wid.draw,get_value=index
	wset,index

end
