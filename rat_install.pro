;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Installation routine for new installations / updates
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

@definitions
@io/rrat

function cw_rat_install_filesel_get_value,id
	
  	widget_control, id, get_uvalue = state, /no_copy
	filename = state.filename
  	widget_control, id, set_uvalue = state, /no_copy
	
	return,filename
	
end
pro cw_rat_install_filesel_set_value,id,value
	
  	widget_control, id, get_uvalue = state, /no_copy
	widget_control,state.text,set_value= value
  	widget_control, id, set_uvalue = state, /no_copy
	
end


pro cw_rat_install_filesel_test_file,ev
  common rat_install, prefdir
	
  	widget_control, ev.handler, get_uvalue = state, /no_copy

	path = state.path
	old_path = path
	
	state.filename= dialog_pickfile(title=state.label, /directory, dialog_parent=state.base,/must_exist,path=path,get_path=path)
	if state.filename eq prefdir then begin
		infotext = ["You cannot use the "+prefdir+"directory","","please, choose another one" ]
		info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = base, /error)
		state.filename="<please choose a valid directory>"
	endif else state.path = path
	widget_control,state.text,set_value=state.filename
  	widget_control, ev.handler, set_uvalue = state, /no_copy

end

function cw_rat_install_filesel, parent, label=label, filename=filename, path=path
	
	if not keyword_set(filename) then filename=''
	if not keyword_set(label) then label=' '
	if not keyword_set(path) then path=' '
	
	state = { label: label,$
				 filename: filename,$
				 base:0l,$
				 path:path,$
				 text:0l $
	}
	; generate the widget base
	base = widget_base(parent, /row,event_pro='cw_rat_install_filesel_test_file',func_get_value='cw_rat_install_filesel_get_value',pro_set_value='cw_rat_install_filesel_set_value')
	state.base = base
		state.text = cw_field(state.base,value=filename,/string,xsize=50,title=label,/noedit)
		dummy = widget_base(state.base,/align_center)
			but = widget_button(dummy,value='browse...',ysize=30)
  	WIDGET_CONTROL, base, set_uvalue = state,  /no_copy
	return,base
end

; ==============================================================================
; ==============================================================================
pro rat_install
	common rat, types, file, wid, config
   common rat_install
	os = strlowcase(!version.os_family)
	case os of
		'unix': begin
			;print,'UNIX operating system detected'
			homedir = getenv("HOME")
			prefdir = homedir+'/.rat/'
			newline = string(10B)
			font = '6x13bold'
		end
		'windows': begin
			;print,'WINDOWS operating system detected'
			homedir = getenv("USERPROFILE")
			prefdir = homedir+'\rat\'
			newline = strcompress(13B) + string(10B)
			widget_control,default_font='Courier New*15'
			font = 'Courier New*15*bold'
		end
		else: begin
	     	error = DIALOG_MESSAGE("ERROR: Unknown operating system !!!",/error)
			return
		end
   endcase
   sourcedir = file_dirname((routine_info('rat_install',/source)).path,/mark)
   prefdir0 = prefdir
   prefdir1 = sourcedir+'preferences'+path_sep()+'installed'+path_sep()
;------------------------------------------------------------------
; - Star the graphical interface to install rat
;------------------------------------------------------------------

; --> INTRODUCTION

INTRODUCTION:
	base = widget_base(title='RAT Installation',/column,/tlb_kill_request_events,/tlb_frame_attr)
		base2 = widget_base(base,/row)
			base2left = widget_base(base2,/column)
				dummy = widget_label(base2left,value = "--> INTRODUCTION            ",font=font)
				dummy = widget_label(base2left,value = "--> LICENSE                 ",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALATION DIRECTORY   ",sensitive=0)
				dummy = widget_label(base2left,value = "--> PRE-INSTALLATION SUMMARY",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALLATION            ",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALL COMPLETE        ",sensitive=0)
			base2right = widget_base(base2,/column)
				mes  = "RAT_INSTALL will guide you through " + newline
				mes += "the installation of the RAT software" + newline + newline
				mes += "Click the 'Next' button to proceed" + newline
				mes += "to the next screen. If you want to" + newline
				mes += "change something on a previous screen" + newline	
				mes += "click the 'Previous' button" + newline + newline
				mes += "You may cancel the installation at any" + newline
				mes += "time by clicking the 'Cancel' button"
				dummy = widget_text(base2right,value=mes,SCR_XSIZE=300,YSIZE=17)
		buttons  = WIDGET_BASE(base,column=3,/frame)
			but_prev   = WIDGET_BUTTON(buttons,VALUE='Previous',xsize=80,sensitive=0)
			but_next   = WIDGET_BUTTON(buttons,VALUE='Next',xsize=80)
			but_canc = WIDGET_BUTTON(buttons,VALUE='Cancel',xsize=80)
	
	WIDGET_CONTROL, base, /REALIZE, default_button = but_next, XOFFSET=200,yoffset=100
	
	repeat begin
		event = widget_event(base)
	endrep until (event.id eq but_next) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	if (event.id eq  but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' then begin
		infotext = ['Are you sure you want to cancel',$
			' RAT INSTALLATION ?']
		info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = base, /question)
		widget_control,base,/destroy
		if info eq 'Yes' then return else goto,INTRODUCTION
	endif	
	widget_control,base,/destroy
	
; --> LICENSE

LICENSE:
	base = widget_base(title='RAT Installation',/column,/tlb_kill_request_events,/tlb_frame_attr)
		base2 = widget_base(base,/row)
			base2left = widget_base(base2,/column)
				dummy = widget_label(base2left,value = "--> INTRODUCTION            ")
				dummy = widget_label(base2left,value = "--> LICENSE                 ",font=font)
				dummy = widget_label(base2left,value = "--> INSTALATION DIRECTORY   ",sensitive=0)
				dummy = widget_label(base2left,value = "--> PRE-INSTALLATION SUMMARY",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALLATION            ",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALL COMPLETE        ",sensitive=0)
			base2right = widget_base(base2,/column)
				mes  = 'RAT - Terms of Use' + newline
				mes += '----------------------------------------------------------------------' + newline
				mes += 'The RAT source code and binary releases are licenced under the Mozilla' + newline
				mes += 'Public License (MPL) version 1.1. This allows the use of the code in  ' + newline
				mes += 'a wide variety of other free and commercial software projects, while  ' + newline
				mes += 'maintaining copyleft on the written code.                             ' + newline
				mes += '                                                                      ' + newline
				mes += 'For more details see http://www.mozilla.org/MPL/MPL-1.1.html          ' + newline
				mes += 'or the file LICENCE in the base directory of the source distribution. ' + newline
				mes += '                                                                      ' + newline
				mes += 'The software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  ' + newline
				mes += 'ANY KIND, either express or implied. See the license for the specific ' + newline
				mes += 'language governing rights and limitations under the license.          '
				dummy = widget_text(base2right,value=mes,SCR_XSIZE=500,YSIZE=17,/scroll)
		buttons  = WIDGET_BASE(base,column=3,/frame)
			but_prev   = WIDGET_BUTTON(buttons,VALUE='Previous',xsize=80)
			but_next   = WIDGET_BUTTON(buttons,VALUE='Next',xsize=80)
			but_canc = WIDGET_BUTTON(buttons,VALUE='Cancel',xsize=80)
	
	WIDGET_CONTROL, base, /REALIZE, default_button = but_next, XOFFSET=200,yoffset=100
	
	repeat begin
		event = widget_event(base)
	endrep until (event.id eq but_next) or (event.id eq but_canc) or (event.id eq but_prev) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	if event.id eq  but_canc or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' then begin
		infotext = ['Are you sure you want to cancel',$
			' RAT INSTALLATION ?']
		info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = base, /question)
		widget_control,base,/destroy
		if info eq 'Yes' then return else goto,LICENSE
	endif
; --> AGREE ?
	if event.id eq but_next then begin
		infotext = ['To continue installing RAT software,',$
			'you must agree to the term of the',$
			'software license agreement.',$
			' ',$
			' ',$
			' ',$
			'Click "Yes" to continue',$
			' or click "No" to cancel the installation']
		info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = base, title="License Agreement",/question)
		if info eq 'No' then begin
			widget_control,base,/destroy
			return
		endif
	endif
	widget_control,base,/destroy
	if event.id eq but_prev then goto,INTRODUCTION

; --> INSTALLATION DIR

INSTALLATION_DIR:
   prefdir = prefdir0
	base = widget_base(title='RAT Installation',/column,/tlb_kill_request_events,/tlb_frame_attr)
		base2 = widget_base(base,/row)
			base2left = widget_base(base2,/column)
				dummy = widget_label(base2left,value = "--> INTRODUCTION            ")
				dummy = widget_label(base2left,value = "--> LICENSE                 ")
				dummy = widget_label(base2left,value = "--> INSTALATION DIRECTORY   ",font=font)
				dummy = widget_label(base2left,value = "--> PRE-INSTALLATION SUMMARY",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALLATION            ",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALL COMPLETE        ",sensitive=0)
			base2right = widget_base(base2,/column)
				mes  = "INSTALLATION DIRECTORY " + newline
				mes += '----------------------------------------------------------------------' + newline
            mes += 'By default, RAT will install the configuration files into ' +newline
            mes += '      '+prefdir0+newline
            mes += "If you prefer to install the configuration files into the source " + newline
            mes += "directory, please check the box below." + newline + newline
            mes += "Note that in the later case, if you have previously installed the " + newline
            mes += "configuration files into " + newline
            mes += '      '+prefdir0+newline
            mes += 'you have to delete this directory.' + newline
            mes += '(it will be done automatically if present)' + newline+newline
				mes += '----------------------------------------------------------------------' + newline
 				dummy = widget_text(base2right,value=mes,SCR_XSIZE=300,YSIZE=13)
            choice_dir = ['Install into "'+prefdir0+'"','Install into "'+prefdir1+'" and delete "'+prefdir0+'"']
            but_dir = cw_bgroup(base2right,choice_dir,/EXCLUSIVE,/COLUMN,set_value=0)
         buttons  = WIDGET_BASE(base,column=3,/frame)
			but_prev   = WIDGET_BUTTON(buttons,VALUE='Previous',xsize=80,sensitive=0)
			but_next   = WIDGET_BUTTON(buttons,VALUE='Next',xsize=80)
			but_canc = WIDGET_BUTTON(buttons,VALUE='Cancel',xsize=80)
	
	WIDGET_CONTROL, base, /REALIZE, default_button = but_next, XOFFSET=200,yoffset=100
	
	repeat begin
		event = widget_event(base)
	endrep until (event.id eq but_next) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
   widget_control,but_dir, get_value=val
   if val eq 1 then prefdir = prefdir1
	if (event.id eq  but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' then begin
		infotext = ['Are you sure you want to cancel',$
			' RAT INSTALLATION ?']
		info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = base, /question)
		widget_control,base,/destroy
		if info eq 'Yes' then return else goto,INSTALLATION_DIR
	endif	
	widget_control,base,/destroy

; --> UPDATE / NEW INSTALLATION
PRE_INSTALL:
	base = widget_base(title='RAT Installation',/column,/tlb_kill_request_events,/tlb_frame_attr)
		base2 = widget_base(base,/row)
			base2left = widget_base(base2,/column)
				dummy = widget_label(base2left,value = "--> INTRODUCTION            ")
				dummy = widget_label(base2left,value = "--> LICENSE                 ")
				dummy = widget_label(base2left,value = "--> INSTALATION DIRECTORY   ")
				dummy = widget_label(base2left,value = "--> PRE-INSTALLATION SUMMARY",font=font)
				dummy = widget_label(base2left,value = "--> INSTALLATION            ",sensitive=0)
				dummy = widget_label(base2left,value = "--> INSTALL COMPLETE        ",sensitive=0)
			base2right = widget_base(base2,/column)
				wid_text = widget_text(base2right,SCR_XSIZE=500,YSIZE=17,/scroll)
		buttons  = WIDGET_BASE(base,column=3,/frame)
			but_prev   = WIDGET_BUTTON(buttons,VALUE='Previous',xsize=80)
			but_next   = WIDGET_BUTTON(buttons,VALUE='Next',xsize=80,sensitive=0)
			but_canc = WIDGET_BUTTON(buttons,VALUE='Cancel',xsize=80)
	
	WIDGET_CONTROL, base, /REALIZE, default_button = but_next, XOFFSET=200,yoffset=100
	
	;--> Check the installation
	mes  = 'PRE INSTALLATION SUMMARY' + newline
	mes += '------------------------' + newline + newline
	widget_control,wid_text,set_value=mes
	wait,0.5
	
	; * operating system
	mes += 'Operating system: '+strupcase(os) + newline + newline
	widget_control,wid_text,set_value=mes
	wait,0.5
	
	; * is an installation ?
	if file_test(prefdir) then begin
		mes += 'There is a previous version of RAT installed'+newline
		mes += 'in: '+prefdir+newline+newline
		mes += 'Click on "Next" to update your RAT version' 
		widget_control,wid_text,set_value=mes
		widget_control,but_next,set_value='Next',sensitive=1
		wait,0.5
	endif else if prefdir eq prefdir1 && prefdir ne prefdir0 && file_test(prefdir0,/dir,/write) then begin
      mes += 'You have chosen to install the configuration files into'+newline
      mes += prefdir + newline
      mes += 'Delete the available directory '+newline
      mes += prefdir0
		mes += 'Click on "Next" to update your RAT version' 
		widget_control,wid_text,set_value=mes
		widget_control,but_next,set_value='Next',sensitive=1
		wait,0.5
   endif else begin
      mes += 'You will make a RAT Installation'+newline+newline
		mes += 'Click on "Next" to install RAT' 
		widget_control,wid_text,set_value=mes
		widget_control,but_next,set_value='Next',sensitive=1
		wait,0.5
	endelse



	
	repeat begin
		event = widget_event(base)
	endrep until (event.id eq but_next) or (event.id eq but_canc) or (event.id eq but_prev) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	if event.id eq but_canc or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' then begin
		infotext = ['Are you sure you want to cancel',$
			' RAT INSTALLATION ?']
		info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = base, /question)
		widget_control,base,/destroy
		if info eq 'Yes' then return else goto,PRE_INSTALL
	endif
	if event.id eq but_prev then begin
		widget_control,base,/destroy
		goto,LICENSE
	endif

	
	if os eq 'windows' then begin
		dummy = DIALOG_MESSAGE(["WINDOWS operating system detected!","","Would you like that RAT installs Linux ?"], DIALOG_PARENT = base, TITLE='Linux Installation ?',/question)
		if dummy eq 'Yes' then begin
			toto = dialog_message(["You have made a good choice!","","Unfortunatly, you have to do that by yourself:","RAT works much better on UNIX systems!"],/information,dialog_parent=base)
		endif else begin
			base2 = widget_base(title='WARNING',/column,group_leader=base)
				dummy = widget_label(base2,                        value = "Bad choice... RAT might format your harddisk !!!")
				label1 = widget_text(base2,font="courier new*15",xsize=70,ysize=2,value=' ')
				b2  = WIDGET_BASE(base2,/row)
				bn   = WIDGET_BUTTON(b2,VALUE=' Next ',sensitive=0,/frame)
			WIDGET_CONTROL, base2, /REALIZE, default_button = bn, XOFFSET=500,yoffset=300
			mes = "#"
			for i=0,30 do begin
				widget_control,label1,set_value=mes
				wait,0.05
				mes += "#"
			endfor
			widget_control,label1,set_value=mes+newline+"...not really, but RAT works much better on UNIX systems"
			widget_control,bn,sensitive=1
			repeat begin
			event = widget_event(base2)
			endrep until (event.id eq bn)
			widget_control,base2,/destroy
		endelse
	endif

	; * Delete default prefdir
   if prefdir eq prefdir1 && prefdir ne prefdir0 && file_test(prefdir0,/dir,/write) then begin
      prefdir0 = file_dirname(prefdir0,/m)+file_basename(prefdir0) ; important for win machines.
      file_delete,/quiet,/rec,prefdir0
   endif

   config1 = prefdir + 'preferences'
   config2 = prefdir + 'preferences.old'
	if file_test(config1) then begin
		file_move,config1,config2,/overwrite
		restore,config2
		config_old = config
		wid_old = wid	
		wait,0.5
		definitions,/update_pref
		restore,config1
	endif else begin
		file_mkdir,prefdir
		file_mkdir,prefdir+'icons'
		definitions,/update_pref
		restore,config1
		config_old = config
		config_old.tempbase = "<please choose a TMP directory>"
		wid_old = wid
	endelse
	
	;--> Design the preference widget
	mainx = WIDGET_BASE(GROUP_LEADER=base,/column, TITLE='RAT preferences',/modal,/base_align_center,/tlb_kill_request_events,/tlb_frame_attr)
		main = WIDGET_BASE(mainx,/column,/base_align_left)
			dummy = widget_label(main,value='An existing temporary directory is required for RAT working correctly !!!')
			field1 = cw_rat_install_filesel(main,                       label="Temporary directory        : ", filename=config_old.tempdir, path=config_old.tempdir)
			field2 = cw_rat_install_filesel(main,                       label="Default working directory  : ", filename=config_old.workdir, path=config_old.workdir)
			field3 = CW_FIELD(main,VALUE=wid_old.base_xsize,/integer,   TITLE='Size of RAT window in x    : ',XSIZE=4)
			field4 = CW_FIELD(main,VALUE=wid_old.base_ysize,/integer,   TITLE='Size of RAT window in y    : ',XSIZE=4)
			field5 = CW_FIELD(main,VALUE=config_old.blocksize,/integer, TITLE='Blocksize for processing   : ',XSIZE=4)
			field6 = CW_FIELD(main,VALUE=config_old.sar_scale,/floating,TITLE='SAR image contrast scaling : ',XSIZE=4)
			field7 = CW_FIELD(main,VALUE=config.pha_gamma,/floating,     TITLE='Phase image gamma factor   : ',XSIZE=4)


	
		buttons = WIDGET_BASE(mainx,column=3,/frame)
			but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	
	WIDGET_CONTROL, mainx, /REALIZE, default_button = but_ok, xoffset=350,yoffset=250

	repeat begin                                        ; Event loop
		event = widget_event(mainx)
		if event.id eq but_ok then begin
			widget_control,field1,GET_VALUE=val
			if not file_test(val) then begin
				info = dialog_message('Please choose a valid directory',/error,dialog_parent=mainx)
				widget_control,field1,set_value="<Please choose a valid directory>"
				event.id=mainx
			endif else begin
  				;config_old.tempdir = val
  				config_old.tempdir  = val
			endelse
			widget_control,field2,GET_VALUE=val
			if not file_test(val) then begin
				info = dialog_message('Please choose a valid directory',/error,dialog_parent=mainx)
				event.id=mainx
			endif else begin
   			config_old.workdir = val
			endelse
			widget_control,field3,GET_VALUE=val
			wid_old.base_xsize = val
			widget_control,field4,GET_VALUE=val
			wid_old.base_ysize = val
			widget_control,field5,GET_VALUE=val
			config_old.blocksize = val
			widget_control,field6,GET_VALUE=val
			config_old.sar_scale = val
			widget_control,field7,GET_VALUE=val
			config_old.pha_gamma = val		
			
		endif	
	endrep until  (event.id eq but_ok)
	widget_control,mainx,/destroy                        ; remove main widget
	widget_control,base,/destroy
	
; --> INSTALLATION
	base = widget_base(title='RAT Installation',/column,/tlb_kill_request_events,/tlb_frame_attr)
		base2 = widget_base(base,/row)
			base2left = widget_base(base2,/column)
				dummy = widget_label(base2left,value = "--> INTRODUCTION            ")
				dummy = widget_label(base2left,value = "--> LICENSE                 ")
				dummy = widget_label(base2left,value = "--> INSTALATION DIRECTORY   ")
				dummy = widget_label(base2left,value = "--> PRE-INSTALLATION SUMMARY")
				dummy = widget_label(base2left,value = "--> INSTALLATION            ",font=font)
				dummy = widget_label(base2left,value = "--> INSTALL COMPLETE        ",sensitive=0)
			base2right = widget_base(base2,/column)
				wid_text = widget_text(base2right,SCR_XSIZE=500,YSIZE=17,/scroll)
		buttons  = WIDGET_BASE(base,column=3,/frame)
			but_prev   = WIDGET_BUTTON(buttons,VALUE='Previous',xsize=80,sensitive=0)
			but_next   = WIDGET_BUTTON(buttons,VALUE='Next',xsize=80,sensitive=0)
			;but_canc = WIDGET_BUTTON(buttons,VALUE='Cancel',xsize=80,sensitive=0)
	
	WIDGET_CONTROL, base, /REALIZE, default_button = but_next, XOFFSET=200,yoffset=100
	
	;--> Installation
	mes  = 'INSTALL RAT' + newline
	mes += '-----------' + newline + newline
	widget_control,wid_text,set_value=mes
	wait,0.5
   

	; * Create the home dir
	mes += 'Installing in: '+ prefdir
	widget_control,wid_text,set_value=mes
	wait,0.5
	mes += '     OK' + newline
	
	; * Copie the color palettes
	mes += 'Copy the color palettes '
	file_copy,sourcedir+'preferences'+path_sep()+'*.*',prefdir,/overwrite
	widget_control,wid_text,set_value=mes
	wait,0.5
	mes += '     OK' + newline
	
	; * Copie the color palettes
	mes += 'Copy the icons '
	file_copy,sourcedir+'icons'+path_sep()+'*.*',prefdir+'icons'+path_sep(),/overwrite
	widget_control,wid_text,set_value=mes
	wait,0.5
	mes += '     OK' + newline
	
	;--> Test about config
	name_tag_old = tag_names(config_old)
	name_tag = tag_names(config)
	version  = config.version
	
	; * test config
	mes += 'Copy config options'
	widget_control,wid_text,set_value=mes
	for ii=0,n_tags(config_old)-1 do begin
		ind = where(name_tag_old[ii] eq name_tag,count)
		if count ne 0 then config.(ind) = config_old.(ii)
	endfor
	config.tempbase = config.tempdir
	config.version  = version

;;; update the rat script for starting rat from the shell (mn, 24.08.07)
        if os eq 'unix' then begin
           wait,0.5
           mes += '     OK' + newline
           mes += 'Update the starting script "rat"'
           widget_control,wid_text,set_value=mes
           openw, 1,sourcedir+'rat'
           printf,1,'cd '+sourcedir
           printf,1,'idl << final'
           printf,1,'.compile rat'
           printf,1,'rat,startfile="$1",/block'
           printf,1,'final'
           printf,1,'echo $1'
           close, 1
           spawn,'chmod a+x '+sourcedir+'rat'
        endif

	wait,0.5
	mes += '     OK' + newline
	;--> Test about wid
	name_tag_old = tag_names(wid_old)
	name_tag = tag_names(wid)
	mes += 'Copy window options'
	widget_control,wid_text,set_value=mes
	for ii=0,n_tags(wid_old)-1 do begin
		ind = where(name_tag_old[ii] eq name_tag,count)
		if count ne 0 then wid.(ind) = wid_old.(ii)
	endfor
	wait,0.5
	mes += '     OK' + newline
	mes += 'Save preferences'
	widget_control,wid_text,set_value=mes

	save,filename=config1,config,wid
	wait,0.5
	mes += '     OK' + newline + newline
	mes += 'Please Click on Next to finish the procedure' + newline
	mes += 'or on Previous to do another changes'
	widget_control,wid_text,set_value=mes
	
	widget_control,but_prev,sensitive=1
	widget_control,but_next,sensitive=1
	
	repeat begin
		event = widget_event(base)
	endrep until (event.id eq but_next) or (event.id eq but_prev) 
	widget_control,base,/destroy
	if event.id eq but_prev then goto,PRE_INSTALL


; --> SUCCES ? QUIT
	base = widget_base(title='RAT Installation',/column,/tlb_kill_request_events,/tlb_frame_attr)
		base2 = widget_base(base,/row)
			base2left = widget_base(base2,/column)
				dummy = widget_label(base2left,value = "--> INTRODUCTION            ")
				dummy = widget_label(base2left,value = "--> LICENSE                 ")
				dummy = widget_label(base2left,value = "--> INSTALATION DIRECTORY   ")
				dummy = widget_label(base2left,value = "--> PRE-INSTALLATION SUMMARY")
				dummy = widget_label(base2left,value = "--> INSTALLATION            ")
				dummy = widget_label(base2left,value = "--> INSTALL COMPLETE        ",font=font)
			base2right = widget_base(base2,/column)
				mes  = "The RAT installation is succesful" + newline + newline
				mes += "Please click on Finish to exit the installation procedure"
				wid_text = widget_text(base2right,value=mes,SCR_XSIZE=500,YSIZE=17)
		buttons  = WIDGET_BASE(base,column=3,/frame)
			but_next   = WIDGET_BUTTON(buttons,VALUE='Finish',xsize=80)
			;but_canc = WIDGET_BUTTON(buttons,VALUE='Cancel',xsize=80,sensitive=0)
	WIDGET_CONTROL, base, /REALIZE, default_button = but_next, XOFFSET=200,yoffset=100
	repeat begin
		event = widget_event(base)
	endrep until (event.id eq but_next)
	widget_control,base,/destroy
		
end
