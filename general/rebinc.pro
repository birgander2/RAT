;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; rebinc.pro
; written by Andreas Reigber
; Rebin for complex arrays (1,2 and 3 dimensions), with
; automatic smoothing for better sidelobe suppression.
; KEYWORDS:
; /NOSMOOTH:  Do not apply smoothing (sometimes helpful)
; MODIFICATION HISTORY:
;   06-Jun-2001, Martin Keller, DLR, extended to 3D-arrays of the
;        size (layers, xsize, ysize). The first dimension of the
;        matrix isn't affected by rebinc.
;   07-Jun-2001: now it is fully 3d capable (Andreas Reigber)
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

function rebinc,arr_in,xres,yres,zres,vres,NOSMOOTH = nosmooth
	arr = arr_in
	on_error,2
	s=size(arr)
   if ((s[0] eq 0) or (s[0] gt 3)) then message, 'Array must have 1, 2, or 3 dimensions.'
   if ((s[s[0]+1] ne 6) and (s[s[0]+1] ne 9)) then message, 'Array not complex.'
	case s[0] of
		1: begin
			xfak=s[1]/xres
			if xfak gt 1 and not keyword_set(nosmooth) then arr=smooth(arr,xfak)
			re = rebin(float(arr),xres)
			im = rebin(imaginary(arr),xres)
			return,complex(re,im)
		endcase
		2: begin
			xfak=(s[1]/xres)-1
			yfak=(s[2]/yres)-1
			if xfak gt 1 and not keyword_set(nosmooth) then for i=0,s[2]-1 do arr[0,i]=smooth(arr[*,i],xfak)
			if yfak gt 1 and not keyword_set(nosmooth) then for i=0,s[1]-1 do arr[i,0]=smooth(arr[i,*],yfak)
			re = rebin(float(arr),xres,yres)
			im = rebin(imaginary(arr),xres,yres)
			return,complex(re,im)
		endcase
		3: begin
			xfak=(s[1]/xres)-1
			yfak=(s[2]/yres)-1
			zfak=(s[3]/zres)-1
			if xfak gt 1 and not keyword_set(nosmooth) then for i=0,s[2]-1 do for j=0,s[3]-1 do arr[0,i,j]=smooth(arr[*,i,j],xfak)
			if yfak gt 1 and not keyword_set(nosmooth) then for i=0,s[1]-1 do for j=0,s[3]-1 do arr[i,0,j]=smooth(arr[i,*,j],yfak)
			if zfak gt 1 and not keyword_set(nosmooth) then for i=0,s[1]-1 do for j=0,s[2]-1 do arr[i,j,0]=smooth(arr[i,j,*],zfak)
			re = rebin(float(arr),xres,yres,zres)
			im = rebin(imaginary(arr),xres,yres,zres)
			return,complex(re,im)
		endcase
	endcase
end
