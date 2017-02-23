;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: block_transpose
; written by    : Maxim Neumann (TUB)
; last revision : 06. December 2004
; blockwise matrix transpose (for RAT arrays)
; only for 3- and 4- dimensional arrays!
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

function block_transpose,arr, OVERWRITE=OVERWRITE

  siz = size(arr)
  if siz[0] eq 3 then arr = reform(arr,[1,siz[1:3]],/OV)
  if keyword_set(OVERWRITE) then begin
     arr = transpose(arr,[1,0,2,3])
     return, arr
  endif else begin
     out = transpose(arr,[1,0,2,3])
     if siz[0] eq 3 then arr = reform(arr,siz[1:3],/OV)
     return, out
  endelse

end
