;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; vector normalization over all dimensions
;;;   Maxim Neumann - 03/2006
;;; mm_vNormalize(a)	<==>  a[*,0]/norm(a[*,0])
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

function mm_vNormalize, a

  return, a/mm_s2v(sqrt(total(abs(a)^2,1)),(size(a,/dim))[0])

end
