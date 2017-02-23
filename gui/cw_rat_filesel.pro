;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: cw_rat_filesel
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
function cw_rat_filesel_get_value,id
	common rat, types, file, wid, config
	
  	widget_control, id, get_uvalue = state, /no_copy
	filename = state.filename
  	widget_control, id, set_uvalue = state, /no_copy
	
	return,filename
	
end


pro cw_rat_filesel_test_file,ev
	compile_opt hidden, strictarr
	common rat, types, file, wid, config
	
  	widget_control, ev.handler, get_uvalue = state, /no_copy

	path = config.workdir
	
	state.filename= cw_rat_dialog_pickfile(title=state.label, dialog_parent=state.base, filter='*.rat',/must_exist,path=path,get_path=path)
	
	;test if the file is correct
	if state.filename ne '' then begin
		head=1l
		rrat,state.filename,ddd,header=head,info=info,type=type
		free_lun,ddd
	
		if type ne state.type then begin
			error = dialog_message('wrong '+types[state.type]+' file',dialog_parent = state.base,/error)
			state.filename = ''
		endif
	endif
	
	widget_control,state.text,set_value=state.filename
  	widget_control, ev.handler, set_uvalue = state, /no_copy

end

function cw_rat_filesel, parent, label=label, type=type
	common rat, types, file, wid, config
	
	if not keyword_set(filename) then filename=''
	if not keyword_set(label) then label=' '
	
	state = { label: label,$
				 filename: filename,$
				 type:type,$
				 base:0l,$
				 text:0l$
	}
	; generate the widget base
	base = widget_base(parent, /row,event_pro='cw_rat_filesel_test_file',func_get_value='cw_rat_filesel_get_value')
	state.base = base
		state.text = cw_field(state.base,value=filename,/string,xsize=50,title=label)
		dummy = widget_base(state.base,/align_center)
			but = widget_button(dummy,value='browse...',ysize=30)
  	WIDGET_CONTROL, base, set_uvalue = state,  /no_copy
	return,base
end
	
	
