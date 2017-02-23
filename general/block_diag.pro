;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: block_diag
; written by    : Maxim Neumann (TUB)
; last revision : 06. December 2004
; blockwise matrix diagonal calculation (for RAT arrays)
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

function block_diag,arr, OVERWRITE=OVERWRITE

  dim = (size(arr))[1]
  if keyword_set(OVERWRITE) then begin
     for i=1,dim-1 do arr[i,0,*,*,*,*] = reform(arr[i,i,*,*,*,*])
     return, reform(arr[*,0,*,*,*,*])
  endif else begin
     out = reform(arr[*,0,*,*,*,*])
     for i=1,dim-1 do out[i,*,*,*,*] = reform(arr[i,i,*,*,*,*])
     return, out
  endelse

end
