;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: block_mm
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
FUNCTION block_mm,in1,in2
	
	; DIMENSION OF IN1 and IN2
	cc1   = SIZE(in1) & cc2   = SIZE(in2)
	vdim1 = cc1[1]    & vdim2 = cc2[1]
	zdim1 = cc1[2]    & zdim2 = cc2[2]
        type = size(in1,/type)>size(in2,/type)

	IF vdim1 NE zdim2 THEN BEGIN
		PRINT,'oups, error'
		RETURN,0
	ENDIF
	
	case 1 of
	
	cc1[0] GT cc2[0]: BEGIN
		IF cc1[0] EQ 2 THEN BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz] += in1[kk,zz] * in2[vv,kk]
		ENDIF
		IF cc1[0] EQ 3 THEN BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,cc1[3],type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz,*] += in1[kk,zz,*] * in2[vv,kk]
		ENDIF
		IF cc1[0] EQ 4 THEN BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,cc1[3],cc1[4],type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz,*,*] += in1[kk,zz,*,*] * in2[vv,kk]
		ENDIF
	END
	
	cc1[0] LT cc2[0]: BEGIN
		if cc2[0] EQ 2 then BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz] += in1[kk,zz] * in2[vv,kk]
		ENDIF
		IF cc2[0] EQ 3 THEN BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,cc2[3],type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz,*] += in1[kk,zz] * in2[vv,kk,*]
		ENDIF
		IF cc2[0] EQ 4 THEN BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,cc2[3],cc2[4],type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz,*,*] += in1[kk,zz] * in2[vv,kk,*,*]
		ENDIF
	END
	
	else: begin
		if cc2[0] EQ 2 then BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz] += in1[kk,zz] * in2[vv,kk]
		ENDIF
		IF cc2[0] EQ 3 THEN BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,cc2[3],type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz,*] += in1[kk,zz,*] * in2[vv,kk,*]
		ENDIF
		IF cc2[0] EQ 4 THEN BEGIN
			out = MAKE_ARRAY(vdim2,zdim1,cc2[3],cc2[4],type=type)
			FOR vv = 0,vdim2-1 DO $
				FOR zz=0,zdim1-1 DO $
					FOR kk=0,vdim1-1 DO $
						out[vv,zz,*,*] += in1[kk,zz,*,*] * in2[vv,kk,*,*]
		ENDIF
	end
	endcase
	
	
	RETURN,out
	
END
