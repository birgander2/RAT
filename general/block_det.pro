;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: det
; written by    : Stéphane Guillaso (TUB)
; last revision : 24.February.2004
; determinant of 2x2, 3x3 and 4x4 [T] or [C] matrix
; can handle up to 5xdimensional block-matrices (mn)
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

FUNCTION block_det,in,REFORM=REFORM

	; DIMENSION OF IN
	cc = SIZE(in)
	vdim = cc[1]

        IF (vdim gt 4) THEN BEGIN
           if cc[0] eq 2 then det=la_determ(in, /check) $
           else begin
              det = fltarr(cc[3:cc[0]])
              for k=0,(cc[0]lt 5?0:cc[5]-1) do $
                 for j=0,(cc[0]lt 4?0:cc[4]-1) do $
                    for i=0,(cc[0]lt 3?0:cc[3]-1) do $
                       det[i,j,k] = la_determ(in[*,*,i,j,k], /check)
           endelse
           RETURN, det
        ENDIF
	
	CASE vdim OF
		2:	det = $
				    in[0,0,*,*,*] * in[1,1,*,*,*] $
				  - in[1,0,*,*,*] * in[0,1,*,*,*]
	
		3:	det = $
				  in[0,0,*,*,*] * in[1,1,*,*,*] * in[2,2,*,*,*] $
				- in[0,0,*,*,*] * in[2,1,*,*,*] * in[1,2,*,*,*] $
				- in[0,1,*,*,*] * in[1,0,*,*,*] * in[2,2,*,*,*] $
				+ in[0,1,*,*,*] * in[2,0,*,*,*] * in[1,2,*,*,*] $
				+ in[0,2,*,*,*] * in[1,0,*,*,*] * in[2,1,*,*,*] $
				- in[0,2,*,*,*] * in[2,0,*,*,*] * in[1,1,*,*,*]
		4:	det = $	
				  in[0,0,*,*,*] * in[1,1,*,*,*] * in[2,2,*,*,*] * in[3,3,*,*,*] $ 
				- in[0,0,*,*,*] * in[1,1,*,*,*] * in[3,2,*,*,*] * in[2,3,*,*,*] $
				- in[0,0,*,*,*] * in[1,2,*,*,*] * in[2,1,*,*,*] * in[3,3,*,*,*] $
				+ in[0,0,*,*,*] * in[1,2,*,*,*] * in[3,1,*,*,*] * in[2,3,*,*,*] $
				+ in[0,0,*,*,*] * in[1,3,*,*,*] * in[2,1,*,*,*] * in[3,2,*,*,*] $
				- in[0,0,*,*,*] * in[1,3,*,*,*] * in[3,1,*,*,*] * in[2,2,*,*,*] $
				- in[0,1,*,*,*] * in[1,0,*,*,*] * in[2,2,*,*,*] * in[3,3,*,*,*] $
				+ in[0,1,*,*,*] * in[1,0,*,*,*] * in[3,2,*,*,*] * in[2,3,*,*,*] $
				+ in[0,1,*,*,*] * in[1,2,*,*,*] * in[2,0,*,*,*] * in[3,3,*,*,*] $
				- in[0,1,*,*,*] * in[1,2,*,*,*] * in[3,0,*,*,*] * in[2,3,*,*,*] $
				- in[0,1,*,*,*] * in[1,3,*,*,*] * in[2,0,*,*,*] * in[3,2,*,*,*] $
				+ in[0,1,*,*,*] * in[1,3,*,*,*] * in[3,0,*,*,*] * in[2,2,*,*,*] $
				+ in[0,2,*,*,*] * in[1,0,*,*,*] * in[2,1,*,*,*] * in[3,3,*,*,*] $
				- in[0,2,*,*,*] * in[1,0,*,*,*] * in[3,1,*,*,*] * in[2,3,*,*,*] $
				- in[0,2,*,*,*] * in[1,1,*,*,*] * in[2,0,*,*,*] * in[3,3,*,*,*] $
				+ in[0,2,*,*,*] * in[1,1,*,*,*] * in[3,0,*,*,*] * in[2,3,*,*,*] $
				+ in[0,2,*,*,*] * in[1,3,*,*,*] * in[2,0,*,*,*] * in[3,1,*,*,*] $
				- in[0,2,*,*,*] * in[1,3,*,*,*] * in[3,0,*,*,*] * in[2,1,*,*,*] $
				- in[0,3,*,*,*] * in[1,0,*,*,*] * in[2,1,*,*,*] * in[3,2,*,*,*] $
				+ in[0,3,*,*,*] * in[1,0,*,*,*] * in[3,1,*,*,*] * in[2,2,*,*,*] $
				+ in[0,3,*,*,*] * in[1,1,*,*,*] * in[2,0,*,*,*] * in[3,2,*,*,*] $
				- in[0,3,*,*,*] * in[1,1,*,*,*] * in[3,0,*,*,*] * in[2,2,*,*,*] $
				- in[0,3,*,*,*] * in[1,2,*,*,*] * in[2,0,*,*,*] * in[3,1,*,*,*] $
				+ in[0,3,*,*,*] * in[1,2,*,*,*] * in[3,0,*,*,*] * in[2,1,*,*,*]
             ENDCASE
        if keyword_set(REFORM) then return, reform(det) $
        else return, det
END
