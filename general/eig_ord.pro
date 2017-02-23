;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;+
; NAME:
;	EIG_ORD
; PURPOSE:
;	This function calculate and order eigenvalue/vectors of a hermitian matrix
;  Rxx
; CATEGORY:
;	Mathematics.
; CALLING SEQUENCE:
;	EIG_ORD, Rxx, sens, V, D
; INPUTS:
;	Rxx:	a hermitian matrix.
;	sens: 1 to an increasing order, -1 to a decreasing order
; OUTPUTS:
;	V:	eigenvector matrix.
;	D: eigenvalue vector
; MODIFICATION HISTORY:
; 	Written by:	Stephane Guillaso.
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
PRO eig_ord,Rxx,sens,V,D

; Calcul des valeurs/vecteurs propres
 D = la_eigenql(Rxx,eigenvectors=V)

V = transpose(V)
; Inverse l'ordre des valeurs/vecteurs propres si demande'e
if sens eq -1 then begin
  D = reverse(D)
  V = reverse(V)
endif

; Enleve la phase des premiers termes des vecteurs propres
xdim = (size(V))[1]
for ii=0,xdim-1 do begin
  phi = atan(V(ii,0),/phase)
  V(ii,*) = V(ii,*) * exp(complex(0,-phi))
endfor
	
END
