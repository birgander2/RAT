;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: display_information_system
; written by    : Stephane Guillaso (TUB)
; last revision : 24.march.2004
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
PRO display_information_system,GroupLeader,insar
	common rat, types, file, wid, config

;------------------------------------------------------------------------
; - Generate the graphical user interface
;------------------------------------------------------------------------
	titre = 'Display Airborne Parameter Information'
	
	; Create the menu
	mm = replicate({flags:0, name:''},8)
	mm.flags = [1,1,0,0,0,0,0,2]
	mm.name = ['File','Save','Flatearth phase','Look angle','Ambiguity height','Normal baseline','Critical baseline','Reference range distance']

	main = WIDGET_BASE(GROUP_LEADER=GroupLeader,/column,TITLE=titre,/floating,/tlb_kill_request_events,/tlb_frame_attr)
		sub_menu = widget_base(main,/row,/frame)
			menu = cw_pdmenu(sub_menu,mm,/return_full_name)
		sub2 = WIDGET_BASE(main,/row)
			draw21 = WIDGET_DRAW(sub2,XSIZE = 400,YSIZE = 300)
			draw22 = WIDGET_DRAW(sub2,XSIZE = 400,YSIZE = 300)
		sub_option = widget_base(main,/row,/align_right)
			button_choose = cw_bgroup(sub_option,['Look Angle','Reference range distance'],/exclusive,/row,set_value=0)
		sub3 = WIDGET_BASE(main,/row)
			draw31 = WIDGET_DRAW(sub3,XSIZE = 400,YSIZE = 300)
			draw32 = WIDGET_DRAW(sub3,XSIZE = 400,YSIZE = 300)
	buttons    = WIDGET_BASE(main,/row,/frame)
		but_ok     = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_info   = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

	WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]

	
;------------------------------------------------------------------------
; - Generate information about airborne system 
;   using the following function
;------------------------------------------------------------------------
	airborne_insar_info_gen,insar, fe=fe, theta=theta, h_amb=hb, b_n = nb, bnc = cnb, r_ref=r_ref
	
;------------------------------------------------------------------------
; - Display all information on the 4 draw area
;------------------------------------------------------------------------
	; Display the flatearth phase component
	WIDGET_CONTROL, draw21, GET_VALUE=index
	WSET, index
	plot,fe,xtitle='range position pixel',ytitle='fe (rad)',TITLE='Flatearth phase'
	
	; Display the Look angle component
	WIDGET_CONTROL, draw22, GET_VALUE=index
	WSET, index
	plot,theta,xtitle='range position pixel',ytitle='theta (rad)',TITLE='Look Angle'

	; Display the Ambiguity height component
	WIDGET_CONTROL, draw31, GET_VALUE=index
	WSET, index
	plot,hb,xtitle='range position pixel',ytitle='height (m)',TITLE='Ambiguity height'

	; Display the normal baseline and critical normal baseline
	WIDGET_CONTROL, draw32, GET_VALUE=index
	WSET, index
	minmin = min([nb,abs(cnb)],max=maxmax)
	plot,nb,xtitle='range position pixel',ytitle='normal baseline (m)',yrange=[minmin,maxmax],$
		     TITLE='Normal baseline - Critical normal baseline'
	oplot,abs(cnb),linestyle=2
;------------------------------------------------------------------------
; - Loop events to catch the information provided by widget
;------------------------------------------------------------------------
	repeat begin

		; Get an event from the main widget
		event = widget_event(main)
		
		if event.id eq button_choose then begin
			case event.value of
				0 : begin
					WIDGET_CONTROL, draw22, GET_VALUE=index
					WSET, index
					plot,theta,xtitle='range position pixel',ytitle='theta (rad)',TITLE='Look Angle'
				end
				1 : begin
					WIDGET_CONTROL, draw22, GET_VALUE=index
					WSET, index
					plot,r_ref,xtitle='range position pixel',ytitle='distance (m)',TITLE='Reference range distance'
				end
			endcase
		endif
		
		if event.id eq menu then begin
			case event.value of
				'File.Save.Flatearth phase' : begin
					path=config.workdir
					fichname = dialog_pickfile(dialog_parent=main,filter='*.rat',title='save flatearth phase file',path = path, get_path=path )
					if (strlen(fichname) gt 0) then begin
						srat,fichname,fe
						info = dialog_message('data saved ...', dialog_parent=main)
					endif else info = dialog_message('data not saved, try again...', dialog_parent=main)
				end
				'File.Save.Look angle' : begin
					path=config.workdir
					fichname = dialog_pickfile(dialog_parent=main,filter='*.rat',title='save Look angle file',path = path, get_path=path )
					if (strlen(fichname) gt 0) then begin
						srat,fichname,theta
						info = dialog_message('data saved ...', dialog_parent=main)
					endif else info = dialog_message('data not saved, try again...', dialog_parent=main)
				end
				'File.Save.Ambiguity height' : begin
					path=config.workdir
					fichname = dialog_pickfile(dialog_parent=main,filter='*.rat',title='save Ambiguity height file',path = path, get_path=path )
					if (strlen(fichname) gt 0) then begin
						srat,fichname,hb
						info = dialog_message('data saved ...', dialog_parent=main)
					endif else info = dialog_message('data not saved, try again...', dialog_parent=main)
				end
				'File.Save.Normal baseline' : begin
					path=config.workdir
					fichname = dialog_pickfile(dialog_parent=main,filter='*.rat',title='save Normal baseline file',path = path, get_path=path )
					if (strlen(fichname) gt 0) then begin
						srat,fichname,nb
						info = dialog_message('data saved ...', dialog_parent=main)
					endif else info = dialog_message('data not saved, try again...', dialog_parent=main)
				end
				'File.Save.Critical baseline' : begin
					path=config.workdir
					fichname = dialog_pickfile(dialog_parent=main,filter='*.rat',title='save Critical baseline file',path = path, get_path=path )
					if (strlen(fichname) gt 0) then begin
						srat,fichname,cnb
						info = dialog_message('data saved ...', dialog_parent=main)
					endif else info = dialog_message('data not saved, try again...', dialog_parent=main)
				end
				'File.Save.Reference range distance' : begin
					path=config.workdir
					fichname = dialog_pickfile(dialog_parent=main,filter='*.rat',title='save Reference range distance file',path = path, get_path=path )
					if (strlen(fichname) gt 0) then begin
						srat,fichname,r_ref
						info = dialog_message('data saved ...', dialog_parent=main)
					endif else info = dialog_message('data not saved, try again...', dialog_parent=main)
				end
			endcase
		endif

		; If info button is choosen
		if event.id eq but_info then begin               
			infotext = ['DISPLAY INFORMATIONS',$
			' ',$
			'RAT module written 05/2004 by Stephane Guillaso',$
			' ']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
		
	endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

;------------------------------------------------------------------------
; - Destroy the widget display
;------------------------------------------------------------------------
	widget_control,main,/destroy

END
