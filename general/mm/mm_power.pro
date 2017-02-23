;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; matrix power over all dimensions
;;;   Maxim Neumann - 08/2006
;;;   based on sqrtm.pro (version 2.2)
;;; power over complex-hermitian or real-symmetric matrices
;;;   mm_power,M,power,SQRT=SQRT,INVERT=INVERT
;;;   <==> M^power  where power=any real (pos/neg) number
;;;   /SQRT   		power  = 1/2  (sqrt has higher priority then power!)
;;;   /INV    		power *= -1
;;;   /OVERWRITE 	
;;;   result is complex!
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


function mm_power, A, power, SQRT=SQRT_KEY, INVERT=INVERT, OVERWRITE=OVERWRITE
  eps = (MACHAR()).EPS
  siz = size(A)
  n   = [siz[0]le 2?lonarr(6)+1:[siz[3:siz[0]],lonarr(8-siz[0])+1]]

  if n_elements(power) eq 0 then power  =  1.
  if keyword_set(SQRT_KEY)  then power  =  0.5
  if keyword_set(INVERT)    then power *= -1.

  if ~keyword_set(OVERWRITE) then M=complexarr(siz[1:siz[0]],/nozero)

  for q=0,n[5]-1 do $
     for p=0,n[4]-1 do $
        for l=0,n[3]-1 do $
           for k=0,n[2]-1 do $
              for j=0,n[1]-1 do $
                 for i=0,n[0]-1 do begin
     eval  = complex(la_eigenql(A[*,*,i,j,k,l,p,q],eigenvectors=evec))
     if keyword_set(OVERWRITE) then $
        A[*,*,i,j,k,l,p,q]= matrix_multiply(conj(evec) # diag_matrix(eval^power), evec, /BTRANSPOSE) $
     else $
        M[*,*,i,j,k,l,p,q]= matrix_multiply(conj(evec) # diag_matrix(eval^power), evec, /BTRANSPOSE)
  endfor
  
  return, keyword_set(OVERWRITE)? A: M
end
