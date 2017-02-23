;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; matrix diagonalization over all dimensions
;;;   Maxim Neumann - 03/2006
;;;   based on rat-block-routines by A. Reigber and S. Guillaso
;;; mm_diag(a[*,*,0])	<==> diag_matrix(a[*,*,0])
;;; with the assumtion that the first two dimensions are equal
;;; it is not intended to construct diagonal matrices from vectors
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

function mm_diag,a, OVERWRITE=OVERWRITE

  siz = size(a)
  dim = min(siz[1:2])
  newsiz = siz[0] lt 3? dim : [dim,siz[3:siz[0]]]
  if keyword_set(OVERWRITE) then begin
     for i=1,dim-1 do a[i,0,*,*,*,*,*,*] = a[i,i,*,*,*,*,*,*]
     return, reform(a[*,0,*,*,*,*,*,*],newsiz,/overwrite)
  endif else begin
     out = reform(a[*,0,*,*,*,*,*,*])
     for i=1,dim-1 do out[i,*,*,*,*,*,*] = a[i,i,*,*,*,*,*,*]
     return, reform(out,newsiz)
  endelse

end
