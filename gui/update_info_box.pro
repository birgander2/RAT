;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: update_info_box
; written by    : Andreas Reigber
; last revision : 14.Mar.2003
; Print file information at the bottom of the main window
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

pro update_info_box
	common rat, types, file, wid, config
	
	dimstr = strcompress(file.xdim)+' x'+strcompress(file.ydim)
	if file.dim ge 3 then dimstr = strcompress(file.zdim)+' x' + dimstr
	if file.dim ge 4 then dimstr = strcompress(file.vdim)+' x' + dimstr
	
	if	config.os eq 'windows' then newline = string(13B) + string(10B)
	if	config.os eq 'unix' then newline = string(10B)
	
	
;	infostr = file.info + '  ('+file.name+')'+ ' ('+config.undofile+')'+newline $
	infostr = file.info + newline $
	+ types[file.type] + newline $
	+ 'Dimensions :'+dimstr
	
	if file.mult gt 1 then infostr += ' , '+strcompress(file.mult,/rem)+' files'
	infostr += '  ('+types[file.var]+')'
	widget_control, wid.info, SET_VALUE = infostr
end
