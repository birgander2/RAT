;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; outer product of vectors over all dimensions
;;;   Maxim Neumann - 03/2006
;;;   based on rat-block-routines by A. Reigber and S. Guillaso
;;; mm_xprod(a1,a2)  	<==>  a1 # a2
;;; mm_xprod(a1)       	<==>  a1 # a1
;;; mm_xprod(a1,/conj)	<==>  a1 # conj(a1)
;;; mm_xprod(a1,/first)	<==>  conj(a1) # a1
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


function mm_xprod,a1,a2, CONJUGATE_SECOND=CONJUGATE_SECOND, $
                  FIRST_CONJUGATE=FIRST_CONJUGATE

  siz = size(a1)
  if siz[0] eq 0 then message,'provide arrays for mm_oprod!'
  if n_elements(a2) ne 0 && ~array_equal(siz[1:siz[0]],size(a2,/dim)) then $
     message,'the dimensionality of arrays should be the same!'
  dim  = siz[1]
  type = siz[siz[0]+1]
  if siz[0] eq 1 then out=make_array([dim,dim],type=type) $
  else out= make_array([dim,dim,siz[2:siz[0]]],type=type)
  if n_elements(a2) gt 0 then begin
     if keyword_set(CONJUGATE_SECOND) then $
        for i=0,dim-1 do for j=0,dim-1 do $
           out[i,j,*,*,*,*,*,*] = a1[j,*,*,*,*,*,*] * conj(a2[i,*,*,*,*,*,*,*]) $
     else if keyword_set(FIRST_CONJUGATE) then $
        for i=0,dim-1 do for j=0,dim-1 do $
           out[i,j,*,*,*,*,*,*] = conj(a1[j,*,*,*,*,*,*]) * a2[i,*,*,*,*,*,*,*] $
     else for i=0,dim-1 do for j=0,dim-1 do $
        out[i,j,*,*,*,*,*,*] = a1[j,*,*,*,*,*,*] * a2[i,*,*,*,*,*,*,*]
  endif else if keyword_set(CONJUGATE_SECOND) then $
     for i=0,dim-1 do for j=0,dim-1 do $
        out[i,j,*,*,*,*,*,*] = a1[j,*,*,*,*,*,*] * conj(a1[i,*,*,*,*,*,*,*]) $
  else if keyword_set(FIRST_CONJUGATE) then $
     for i=0,dim-1 do for j=0,dim-1 do $
        out[i,j,*,*,*,*,*,*] = conj(a1[j,*,*,*,*,*,*]) * a1[i,*,*,*,*,*,*,*] $
  else $
     for i=0,dim-1 do for j=0,dim-1 do $
        out[i,j,*,*,*,*,*,*] = a1[j,*,*,*,*,*,*] * a1[i,*,*,*,*,*,*,*]

  return,out
end
