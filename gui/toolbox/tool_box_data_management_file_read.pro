;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; READ AND INTERPRETATE THE DATA MANAGEMENT FILE
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
pro tool_box_data_management_file_read,filename,state
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag
	line = ''
	ind_branch = -1
	ind_branch_test = -1
	ind_leaf = 0
	branch_wid = [0l]
	leaf_wid = [0l]
	leaf_type = [0l]
	leaf_info = ['']
	leaf_filename = ['']
	
	openr,ddd,filename,/get_lun
	readf,ddd,line
	state.data_main_name = widget_tree(state.data_root,value=line,/folder,/expanded)
	while ~ eof(ddd) do begin
		readf,ddd,line
		dummy = strsplit(line,':',/extract)
		ind_br = fix(dummy[0] /100.) - 1
		
			; add a file and analyse it if not exist delete it
		if file_test(config.workdir+dummy[2]) then begin
			
			; add a branch if needed
			if ind_br gt ind_branch_test then begin
				ind_branch_test = ind_br
				ind_branch++
				branch_wid =[branch_wid, widget_tree(state.data_main_name, value=state.data_foldertype[ind_br], /folder, /expanded)]
			endif
			
			leaf_wid = [leaf_wid,widget_tree(branch_wid[ind_branch+1],value=dummy[1]+': '+types[dummy[0]])]
			leaf_type = [leaf_type,long(dummy[0])]
			leaf_info = [leaf_info,dummy[1]]
			leaf_filename = [leaf_filename,dummy[2]]
		endif
	endwhile
	free_lun,ddd
	ptr_free,state.data_branch_wid,state.data_leaf_wid,state.data_leaf_type,state.data_leaf_info,state.data_leaf_filename
	if n_elements(branch_wid) gt 1 then branch_wid = branch_wid[1:*]
	if n_elements(leaf_wid) gt 1 then leaf_wid = leaf_wid[1:*] 
	if n_elements(leaf_type) gt 1 then leaf_type = leaf_type[1:*]
	if n_elements(leaf_info) gt 1 then leaf_info = leaf_info[1:*]
	if n_elements(leaf_filename) gt 1 then leaf_filename = leaf_filename[1:*]
	state.data_branch_wid    = ptr_new(branch_wid)
	state.data_leaf_wid      = ptr_new(leaf_wid) 
	state.data_leaf_type     = ptr_new(leaf_type)
	state.data_leaf_info     = ptr_new(leaf_info)
	state.data_leaf_filename = ptr_new(leaf_filename)
	state.data_main_name_realize = 1
	
end
