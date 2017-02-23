;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: preferences
; written by    : Andreas Reigber (TUB)
; last revision : 21. 4. 2004
; Setting of RAT preferences
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
pro preferences
	common rat, types, file, wid, config
	mainx = WIDGET_BASE(GROUP_LEADER=wid.base,column=1,TITLE='RAT preferences',/base_align_center,/floating,/tlb_kill_request_events,/tlb_frame_attr)
;	main = WIDGET_BASE(mainx,column=1,/base_align_left)
        maintab = WIDGET_TAB(mainx)
        main    = WIDGET_BASE(maintab,title="  Directories & Display  ",/frame,column=1,/base_align_left)
	
	sval1  = config.tempbase
	sval2  = config.workdir
	sval3  = wid.base_xsize
	sval4  = wid.base_ysize
	sval5  = config.blocksize
	sval6  = config.sar_scale
	sval7  = config.pha_gamma

	field1 = CW_FIELD(main,VALUE=config.tempbase,/string, TITLE='Temporary directory        : ',XSIZE=50)
	field2 = CW_FIELD(main,VALUE=config.workdir,/string, TITLE ='Default working directory  : ',XSIZE=50)
	field3 = CW_FIELD(main,VALUE=wid.base_xsize,/integer,TITLE ='Size of RAT window in x    : ',XSIZE=4)
	field4 = CW_FIELD(main,VALUE=wid.base_ysize,/integer,TITLE ='Size of RAT window in y    : ',XSIZE=4)
	field5 = CW_FIELD(main,VALUE=config.blocksize,/integer,TITLE='Blocksize for processing   : ',XSIZE=4)

	sub6   = WIDGET_BASE(main,column=2)
	field6 = CW_FIELD(sub6,VALUE=config.sar_scale,/floating,TITLE='SAR image contrast scaling : ',XSIZE=4)
        field6a= cw_bgroup(sub6,' ',label_left='        logarithmic scale ', $
                           /nonexclusive,set_value=config.log_scale)
        widget_control,field6,sensitive=~config.log_scale

	sub    = WIDGET_BASE(main,column=2)
	field7 = CW_FIELD(sub,VALUE=config.pha_gamma,/floating,TITLE='Phase image gamma factor   : ',XSIZE=4)
	field8 = cw_bgroup(sub ,' ',label_left='        redisplay image   ',/nonexclusive)



        main2   = WIDGET_BASE(maintab,title="  Control  ",/frame,column=1,/base_align_left)
	field10= CW_FIELD(main2,VALUE=config.pdfviewer,/string, TITLE='Viewer for pdf files       : ',XSIZE=50)

	field9 = cw_bgroup(main2,[' On ',' Off '],label_left='RAT debug mode              ',set_value=~config.debug,/exclusive,/row)
        field11= cw_bgroup(main2,[' On ',' Off '],label_left='Image preview at start      ',set_value=~config.show_preview,/exclusive,/row)




	buttons = WIDGET_BASE(mainx,column=3,/frame)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
	but_save = WIDGET_BUTTON(buttons,VALUE=' Save ',xsize=60)
	redisp   = 0
	exitfl   = 0
	
	WIDGET_CONTROL, mainx, /REALIZE, default_button = but_canc,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, mainx, xoffset=pos[0], yoffset=pos[1]

	repeat begin                                        ; Event loop
		event = widget_event(mainx)
                if event.id eq field6a then begin
                   widget_control,field6a,GET_VALUE=val
                   config.log_scale = val
                   widget_control,field6,sensitive=~config.log_scale
                endif
		if event.id eq but_save or event.id eq but_ok then begin
			widget_control,field1,GET_VALUE=val
			if strmid(val,strlen(val)-1,1) ne path_sep() then val=val+path_sep()
			config.tempbase = val
			widget_control,field2,GET_VALUE=val
			if strmid(val,strlen(val)-1,1) ne path_sep() then val=val+path_sep()
			config.workdir = val
			widget_control,field3,GET_VALUE=val
			wid.base_xsize = val
			widget_control,field4,GET_VALUE=val
			wid.base_ysize = val
			widget_control,field5,GET_VALUE=val
			config.blocksize = val
			widget_control,field6,GET_VALUE=val
			config.sar_scale = val
			widget_control,field7,GET_VALUE=val
			config.pha_gamma = val
			widget_control,field8,GET_VALUE=redisp
			widget_control,field9,GET_VALUE=val
			config.debug = ~val
                        widget_control,field10,GET_VALUE=val
                        config.pdfviewer = val
			widget_control,field11,GET_VALUE=val
			config.show_preview = ~val

			if event.id eq but_save then save,filename=config.pref,config,wid

			if sval1 ne config.tempbase then begin
				dummy = DIALOG_MESSAGE(["You changed the temporary directory.","This requires to restart RAT.","Shutting down now...."], DIALOG_PARENT = mainx, TITLE='Information',/information)
				exitfl = 1
			endif
			if sval3 ne wid.base_xsize or sval4 ne wid.base_xsize then begin
				dummy = DIALOG_MESSAGE(["Changes of the window size take","effect after restarting RAT"], DIALOG_PARENT = mainx, TITLE='Information',/information)
			endif	
		endif	
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' or (exitfl eq 1)
	widget_control,mainx,/destroy                        ; remove main widget
	
	if exitfl eq 1 then exit_rat

	if redisp eq 1 then begin
		WIDGET_CONTROL,/hourglass
		generate_preview,/recalculate
		update_info_box
	endif
	
end
