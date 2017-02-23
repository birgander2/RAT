;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: set_type
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
pro set_type,files
	common rat, types, file, wid, config
	
	nfiles = n_elements(files)
	for i=0,nfiles-1 do begin
	
		openr,ddd,files[i],/get_lun,/xdr
		dim  = 0l
		var  = 0l
		readu,ddd,dim
		siz=lonarr(dim)
		readu,ddd,siz
		readu,ddd,var
		free_lun,ddd
		
		openw,ddd,files[i],/get_lun,/xdr,/append
		point_lun,ddd,0l
		writeu, ddd, dim
		writeu, ddd, siz
		writeu, ddd, var
		writeu,ddd,file.type
		free_lun,ddd
	endfor
	
end
