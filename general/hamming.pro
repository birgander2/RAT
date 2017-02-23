;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; hamming.pro
; written by Andreas Reigber
; Calculates the Hamming weighting function 
; Usage:
; hamming,N   N=desired length of array
; KEYWORDS:
; /ALPHA :  Specifies alpha of the hamming function
; /DOUBLE:  Double precision
; /CENTER:  Center around 0
; /TOTAL:   Zero Padding to length total. 
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

function hamming,len,ALPHA=alpha,DOUBLE=double,CENTER=center,TOTAL=total
	if not keyword_set(alpha) then alpha=0.54
	if not keyword_set(double) then $
		ham=alpha-(1-alpha)*cos(2*!pi*findgen(len)/len) $
	else $
		ham=alpha-(1-alpha)*cos(2*!dpi*dindgen(len)/len)
	if keyword_set(center) then ham=shift(ham,len/2)
	if keyword_set(total) then begin
		if not keyword_set(center) then ham=shift(ham,len/2)
		tot = fltarr(total)
		tot[0:len/2-1] = ham[0:len/2-1]
		tot[total-len/2:*] = ham[len-len/2:*]
		ham = tot
	endif
	return,ham
end
