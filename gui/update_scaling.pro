;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: update_scaling
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
pro update_scaling
	common rat, types, file, wid, config

; wid.draw_presum = 0  means floating point type presumming
;                 = 1  means phase type presumming (i.e. complex)
;
; wid.draw_bytes  = 0  means unknown scaling       (min -> max)
;                 = 1  means SAR amplitude scaling (0 -> mean)
;                 = 2  means phase scaling         (-pi -> +pi)
;                 = 3  means coherence scaling     (0.0 -> 1.0)
;                 = 4  means alpha angle scaling   (0.0 -> +pi/2)

	wid.draw_presu = 0 
	wid.draw_bytes = 1 
	
	if (file.type eq 302) or (file.type eq 102) then begin  ; phase images
		wid.draw_presu = 1   
		wid.draw_bytes = 2 
	endif
	
end
