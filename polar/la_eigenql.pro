;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; TECHNICAL UNIVERSITY BERLIN
; DEPARTMENT OF PHOTOGRAMMETRY UND CARTOGRAPHY
; (c)2001 by Andreas Reigber  (anderl@fpk.tu-berlin.de)
; Calculates eigenvalues and eigenvectors of a 3x3 complex hermitian matrix
; This routine exists natively in IDL >= 5.6
; WARNING: Only for 3x3 matrices !!!!
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


function la_eigenql,matrix,EIGENVECTORS = eigen_vec

;---------------------------------------------------------------------
; Calculating eigenvalues using characteristic polynom
; --------------------------------------------------------------------

	eigen_val=complexarr(3)                 
	eigen_vec=complexarr(3,3)                 

	e=complexarr(4)
	e[3]=-1
	e[2]=matrix[0,0]+matrix[1,1]+matrix[2,2]
	e[1]=matrix[0,1]*matrix[1,0]+matrix[0,2]*matrix[2,0]+matrix[1,2]*matrix[2,1]-matrix[1,1]*matrix[2,2]-matrix[0,0]*matrix[2,2]-matrix[0,0]*matrix[1,1]
	e[0]=matrix[0,0]*matrix[1,1]*matrix[2,2]+matrix[1,0]*matrix[2,1]*matrix[0,2]+matrix[2,0]*matrix[0,1]*matrix[1,2]-matrix[0,0]*matrix[1,2]*matrix[2,1]-matrix[1,0]*matrix[0,1]*matrix[2,2]-matrix[2,0]*matrix[1,1]*matrix[0,2]
	eigen_val = fz_roots(e,/double)             
	eigen_val = eigen_val[sort(abs(eigen_val))]

; --------------------------------------------------------------------

	
;-- 1st eigenvector

	a = matrix 
	a[0,0] = a[0,0] - eigen_val[0]
	a[1,1] = a[1,1] - eigen_val[0]
	a[2,2] = a[2,2] - eigen_val[0]
	
	a2   = [[a[1,1],a[2,1]],[a[1,2],a[2,2]]]
	b    = [-a[0,1],-a[0,2]]
	esol = lu_complex(a2,b,/double)
	ev   = [1,esol]
	ev   = ev / norm(ev)
	eigen_vec[*,0] = ev

;-- 2nd eigenvector
	
	a = matrix
	a[0,0] = a[0,0] - eigen_val[1]
	a[1,1] = a[1,1] - eigen_val[1]
	a[2,2] = a[2,2] - eigen_val[1]

	a2   = [[a[1,1],a[2,1]],[a[1,2],a[2,2]]]
	b    = [-a[0,1],-a[0,2]]
	esol = lu_complex(a2,b,/double)
	ev   = [1,esol]
	ev   = ev / norm(ev)
	eigen_vec[*,1] = ev
	
;-- 3rd eigenvector
	
	a = matrix
	a[0,0] = a[0,0] - eigen_val[2]
	a[1,1] = a[1,1] - eigen_val[2]
	a[2,2] = a[2,2] - eigen_val[2]
	
	a2   = [[a[1,1],a[2,1]],[a[1,2],a[2,2]]]
	b    = [-a[0,1],-a[0,2]]
	esol = lu_complex(a2,b,/double)
	ev   = [1,esol]
	ev   = ev / norm(ev)
	eigen_vec[*,2] = ev

;--------------------------------------------
	return,eigen_val

end
