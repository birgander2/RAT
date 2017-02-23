;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: atanc
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
function atanc,_arr
	s = size(_arr)
	type = s[s[0]+1]
	if type eq 6 or type eq 9 then $
		if float(strmid(!version.release,0,3)) lt 5.6 then $
			return,atan(imaginary(_arr),real_part(_arr)) $
			else return,atan(_arr,/phase) 
	return,atan(_arr) 
end
