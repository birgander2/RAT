;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;      mm_:   MATRIX MATHEMATICS routines                     ;;;
;;;             for fast mathematical operations                ;;;
;;;             of complex matrices/vectors/scalars             ;;;
;;;             over all dimensions                             ;;;
;;;             utilising mathematical matrix notation          ;;;
;;; matrix trace over all dimensions
;;;   Maxim Neumann - 03/2006
;;;   based on rat-block-routines by A. Reigber and S. Guillaso
;;; mm_trace(a)     <==>  trace(a[*,*,0])
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



function mm_trace, A, REAL_VALUED=real
  siz = size(A)
  if siz[siz[0]+2] eq 1 then return, A

  trace = A[0,0,*,*,*,*,*,*]
  for i=1,siz[1]-1 do $
     trace += A[i,i,*,*,*,*,*,*]

  if keyword_set(REAL) && $
     (size(A,/type) eq 6 || size(A,/type) eq 9) $
  then trace = real_part(trace)

  return, reform(trace,/overwrite)
end
