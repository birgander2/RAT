;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rat_update
; last revision : 10.Oct.2007
; written by    : Andreas Reigber
;
; Updates RAT's internal structures and windows after finishing a module
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

pro rat_finalise,outputfile,finalfile,PALETTE=palette,CALLED=called
	common rat, types, file, wid, config, tiling
	common channel, channel_names, channel_selec, color_flag, palettes, pnames
	
	file.name = finalfile
	file_move,outputfile,finalfile,/overwrite
	head = 1l
	rrat,file.name, ddd,header=head,info=info,type=type		
	file.dim  = head[0]
	file.ydim = head[head[0]]	
	file.xdim = head[head[0]-1]
	if file.dim ge 3 then file.zdim = head[head[0]-2] else file.zdim = 1
	if file.dim ge 4 then file.vdim = head[head[0]-3] else file.vdim = 1
	file.var  = head[head[0]+1]
	file.type = type
	file.info = info
	free_lun,ddd
	
	if not keyword_set(palette) then palette = 2
	palettes[0,*,*] = palettes[palette,*,*]  
	palettes[1,*,*] = palettes[palette,*,*]  

	if ~keyword_set(called) then begin
		generate_preview
		update_info_box
   endif else progress,/destroy
	
end
