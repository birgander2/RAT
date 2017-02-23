;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: wiz_k2eaa
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
pro wiz_k2eaa, called=called
	common rat, types, file, wid, config

	if file.type ne 200 then begin
		error_button = DIALOG_MESSAGE(['Input data have to be a polarimetric','vector in lexicographic basis'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
	
	ospeck = 0
	
	main = widget_base(GROUP_LEADER=wid.base,title='WIZARD: Scattering vector -> Entopy / Alpha / Anisotropy',row=3,/floating,/tlb_kill_request_events,/tlb_frame_attr)
	main_s2  = widget_base(main)
	main_s3  = widget_base(main_s2,row=5,/frame)
	w_title2 = widget_label(main_s3,value='Speckle Filtering')
	w_speck  = cw_bgroup(main_s3,['Boxcar','Lee','Refined Lee'],label_left='Method : ',/row,/exclusive,set_value=0)
	w_smmx1  = CW_FIELD(main_s3,VALUE=7,/integer,  TITLE='Filter boxsize X      : ',XSIZE=3)
	w_smmx2  = CW_FIELD(main_s3,VALUE=7,/integer,  TITLE='Filter boxsize Y      : ',XSIZE=3)

	main_s1  = widget_base(main,row=3,/frame)
	w_title1 = widget_label(main_s1,value='Covariance matrix generation')
	w_presum1= CW_FIELD(main_s1,VALUE=3,/integer,  TITLE='Presumming X          : ',XSIZE=3)
	w_presum2= CW_FIELD(main_s1,VALUE=3,/integer,  TITLE='Presumming X          : ',XSIZE=3)
	
	buttons  = WIDGET_BASE(main,column=3,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
	
	widget_control, main, /REALIZE, default_button = but_ok, /update

	repeat begin
		event = widget_event(main)
		
		widget_control,w_speck,GET_VALUE=speck
		if speck ne ospeck then begin
			ospeck = speck
			widget_control, main_s3, /destroy
			case speck of
				0: begin
					main_s3  = widget_base(main_s2,row=5,/frame)
					w_title2 = widget_label(main_s3,value='Speckle Filtering')
					w_speck  = cw_bgroup(main_s3,['Boxcar','Lee','Refined Lee'],label_left='Method : ',/row,/exclusive,set_value=0)
					w_smmx1  = CW_FIELD(main_s3,VALUE=7,/integer,  TITLE='Filter boxsize X      : ',XSIZE=3)
					w_smmx2  = CW_FIELD(main_s3,VALUE=7,/integer,  TITLE='Filter boxsize Y      : ',XSIZE=3)
				end
				1: begin
					main_s3  = widget_base(main_s2,row=5,/frame)
					w_title2 = widget_label(main_s3,value='Speckle Filtering')
					w_speck  = cw_bgroup(main_s3,['Boxcar','Lee','Refined Lee'],label_left='Method : ',/row,/exclusive,set_value=1)
					w_smmx1  = CW_FIELD(main_s3,VALUE=7,/integer,  TITLE='Filter boxsize        : ',XSIZE=3)
					w_smmx2  = CW_FIELD(main_s3,VALUE=1.0,/float,  TITLE='Effective No of Looks : ',XSIZE=3)
				end
				2: begin
					main_s3  = widget_base(main_s2,row=5,/frame)
					w_title2 = widget_label(main_s3,value='Speckle Filtering')
					w_speck  = cw_bgroup(main_s3,['Boxcar','Lee','Refined Lee'],label_left='Method : ',/row,/exclusive,set_value=2)
					w_smmx1  = CW_FIELD(main_s3,VALUE=7,/integer,  TITLE='Filter boxsize        : ',XSIZE=3)
					w_smmx2  = CW_FIELD(main_s3,VALUE=1.0,/float,  TITLE='Effective No of Looks : ',XSIZE=3)
					w_smmx3  = CW_FIELD(main_s3,VALUE=6.0,/float,  TITLE='Threshold             : ',XSIZE=3)
				end
			endcase
		endif
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,w_presum1,get_value=presum1
	widget_control,w_presum2,get_value=presum2
	widget_control,w_speck,GET_VALUE=speck
	case speck of
		0: begin
			widget_control,w_smmx1,get_value=smmx1	
			widget_control,w_smmx2,get_value=smmx2
		end
		1: begin
			widget_control,w_smmx1,get_value=smmx1	
			widget_control,w_smmx2,get_value=smmx2
		end
		2: begin
			widget_control,w_smmx1,get_value=smmx1	
			widget_control,w_smmx2,get_value=smmx2
			widget_control,w_smmx3,get_value=smmx3
		end
	endcase
	widget_control, main, /destroy
	if event.id ne but_ok then return    

; undo function
  wid.cancel = 0l
  undo_prepare,outputfile,finalfile,CALLED=CALLED
   

; start batch
	
	case speck of
		0: speck_polmean,smmx=smmx1,smmy=smmx2,/called
		1: speck_pollee,smm=smmx1,looks=smmx2,/called
		2: speck_polreflee,smm=smmx1,looks=smmx2,threshold=smmx3,/called
	endcase
	if wid.cancel eq 1 then return
	image_presumming,smmx=presum1,smmy=presum2,/called
	if wid.cancel eq 1 then return
	c_to_t,/called
	if wid.cancel eq 1 then return
	decomp_entalpani,/called
	if wid.cancel eq 1 then return

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
	
end
