;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; scalars to vectors over all dimensions
;;;   Maxim Neumann - 03/2006
;;;   based on rat-block-routines by A. Reigber and S. Guillaso
;;; scalar block --> vector block
;     a --> [ a a a ]  <==>  sb[x,y] == vb[*,x,y]
;;; same as mm_v2m(a,/T)
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


function mm_s2v, a, dim, OVERWRITE=OVERWRITE

  siz=size(a)
  if n_elements(dim) eq 0 then dim=siz[1]

  return, reform(make_array(dim,type=siz[siz[0]+1],value=1)# $
                 a[*],[dim,siz[1:siz[0]]])

end
