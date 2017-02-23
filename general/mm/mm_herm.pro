;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; make Hermian matrices over all dimensions
;;;   Maxim Neumann - 03/2006
;;; mm_herm(a[*,*,0])	<==>  conj(transpose(a[*,*,0]))
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


function mm_herm,a

  n_dim   = size(a,/N_DIM)
  case n_dim of
     0: return, conj(a)
     1: return, conj(transpose(a))
     2: return, conj(transpose(a))
     else: begin
        dims    = indgen(n_dim)
        dims[0] = 1
        dims[1] = 0
        return, conj(transpose(a,dims))
     endelse
  endcase

end
