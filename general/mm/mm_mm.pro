;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;;;	 mm_:	MATRIX MATHEMATICS routines			;;;
;;;		for fast mathematical operations		;;;
;;;		of complex matrices/vectors/scalars		;;;
;;;		over all dimensions				;;;
;;;		utilising mathematical matrix notation		;;;
;;; matrix multiplication over all dimensions
;;;   Maxim Neumann - 03/2006
;;;   based on rat-block-routines by A. Reigber and S. Guillaso
;;; mm_mm,a,b	<==>  a[*,*,0] # b[*,*,0]
;;; it is equivalent to matrix_multiply(A,B) == A#B
;;; (while block_mm(A,B) == A##B) !!!
;;; if 1-dim array is given, it will be extended to 2-dim!
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

function mm_mm,A,B
  compile_opt idl2

; DIMENSION OF A and B
  if size(A,/n_dim) eq 1 then A=reform(A,[n_elements(A),1],/overwrite)
  if size(B,/n_dim) eq 1 then B=reform(B,[n_elements(B),1],/overwrite)
  siz1   = SIZE(A) & siz2   = SIZE(B)
  x1 = siz1[1]    & x2 = siz2[1]
  y1 = siz1[2]    & y2 = siz2[2]

  if x2 ne y1 then $
     message,'ERROR in bl_mm.pro: the dimensions do not fit ' + $
             'for matrix multiplication!'
  if siz1[0] gt 2 && siz2[0] gt 2 && ~array_equal(siz1[3:*],siz2[3:*]) then $
     message,'ERROR in bl_mm.pro: one of the arguments should be ' + $
             'a single matrix; or the dimensions should be equal!'

  new_dim      = (siz1[0] gt siz2[0] ? size(A,/dim) : size(B,/dim))
  new_dim[0:1] = [x1,y2]
  C            = make_array(new_dim,type=size(A,/type)>size(B,/type))

  if siz1[0] eq 2 then $
     for y=0,y2-1 do $
        for x=0,x1-1 do $
           for k=0,x2-1 do $
              C[x,y,*,*,*,*,*,*] += A[x,k]*B[k,y,*,*,*,*,*,*] $
  else if siz2[0] eq 2 then $
     for y=0,y2-1 do $
        for x=0,x1-1 do $
           for k=0,x2-1 do $
              C[x,y,*,*,*,*,*,*] += A[x,k,*,*,*,*,*,*]*B[k,y] $
  else $
     for y=0,y2-1 do $
        for x=0,x1-1 do $
           for k=0,x2-1 do $
              C[x,y,*,*,*,*,*,*] += A[x,k,*,*,*,*,*,*]*B[k,y,*,*,*,*,*,*]

  return, C
end
