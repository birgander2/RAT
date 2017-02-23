;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: block_trace
; written by    : Andreas Reigber (TUB)
; last revision : 24. March 2004
; blockwise span calculation (for RAT arrays)
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



function block_trace,arr, REAL_VALUED=real
	dim = (size(arr))[1]
	out = arr[0,0,*,*]
	for i=1,dim-1 do out += arr[i,i,*,*]
        out = reform(out,/overwrite)
        if keyword_set(REAL) && $
           (size(out,/type) eq 6 || size(out,/type) eq 9) $
        then out = real_part(out)
	return, out
end

