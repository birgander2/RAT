;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: display_default_preview
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
pro display_default_preview,xsize,ysize
	image = bytarr(xsize,ysize)
	tvlct,r_old,g_old,b_old,/get
	r = [192,bytarr(254)]
	g = [192,bytarr(254)]
	b = [192,bytarr(254)]
	tvlct,r,g,b
	tv,image
	tvlct,r_old,g_old,b_old
end
