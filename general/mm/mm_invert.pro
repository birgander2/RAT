;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; square matrix inversion over all dimensions
;;;   Maxim Neumann - 03/2006
;;;   based on rat-block-routines by A. Reigber and S. Guillaso
;;; mm_invert(a[*,*,0])	<==>  la_invert(a[*,*,0])
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


function mm_invert, A

  siz = size(A)
  if  siz[siz[0]+2] eq 1 then return, 1/A
  inv = make_array(siz[1:siz[0]],type=siz[siz[0]+1],/nozero)

  case siz[1] of
     2: begin
        det              =   mm_determ(A)
        inv[0,0,*,*,*,*,*,*] =   A[1,1,*,*,*,*,*,*]/det
        inv[1,0,*,*,*,*,*,*] = - A[1,0,*,*,*,*,*,*]/det
        inv[0,1,*,*,*,*,*,*] = - A[0,1,*,*,*,*,*,*]/det
        inv[1,1,*,*,*,*,*,*] =   A[0,0,*,*,*,*,*,*]/det
     end
     3: begin
        det              = mm_determ(A)
        inv[0,0,*,*,*,*,*,*] = (A[1,1,*,*,*,*,*,*]*A[2,2,*,*,*,*,*,*] - A[2,1,*,*,*,*,*,*]*A[1,2,*,*,*,*,*,*])/det
        inv[1,0,*,*,*,*,*,*] = (A[2,0,*,*,*,*,*,*]*A[1,2,*,*,*,*,*,*] - A[1,0,*,*,*,*,*,*]*A[2,2,*,*,*,*,*,*])/det
        inv[2,0,*,*,*,*,*,*] = (A[1,0,*,*,*,*,*,*]*A[2,1,*,*,*,*,*,*] - A[2,0,*,*,*,*,*,*]*A[1,1,*,*,*,*,*,*])/det
        inv[0,1,*,*,*,*,*,*] = (A[2,1,*,*,*,*,*,*]*A[0,2,*,*,*,*,*,*] - A[0,1,*,*,*,*,*,*]*A[2,2,*,*,*,*,*,*])/det
        inv[1,1,*,*,*,*,*,*] = (A[0,0,*,*,*,*,*,*]*A[2,2,*,*,*,*,*,*] - A[2,0,*,*,*,*,*,*]*A[0,2,*,*,*,*,*,*])/det
        inv[2,1,*,*,*,*,*,*] = (A[2,0,*,*,*,*,*,*]*A[0,1,*,*,*,*,*,*] - A[0,0,*,*,*,*,*,*]*A[2,1,*,*,*,*,*,*])/det
        inv[0,2,*,*,*,*,*,*] = (A[0,1,*,*,*,*,*,*]*A[1,2,*,*,*,*,*,*] - A[1,1,*,*,*,*,*,*]*A[0,2,*,*,*,*,*,*])/det
        inv[1,2,*,*,*,*,*,*] = (A[0,2,*,*,*,*,*,*]*A[1,0,*,*,*,*,*,*] - A[0,0,*,*,*,*,*,*]*A[1,2,*,*,*,*,*,*])/det
        inv[2,2,*,*,*,*,*,*] = (A[0,0,*,*,*,*,*,*]*A[1,1,*,*,*,*,*,*] - A[1,0,*,*,*,*,*,*]*A[0,1,*,*,*,*,*,*])/det
     end
     else: $
        for n=0,(siz[0]lt 8?0:siz[8]-1) do $
           for m=0,(siz[0]lt 7?0:siz[7]-1) do $
              for l=0,(siz[0]lt 6?0:siz[6]-1) do $
                 for k=0,(siz[0]lt 5?0:siz[5]-1) do $
                    for j=0,(siz[0]lt 4?0:siz[4]-1) do $
                       for i=0,(siz[0]lt 3?0:siz[3]-1) do $
                          inv[*,*,i,j,k,l,m,n] = la_invert(A[*,*,i,j,k,l,m,n],status=ignore)
  endcase

  return, inv
end
