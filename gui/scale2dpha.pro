;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: scale2dpha
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
function scale2dpha,inarr,xdim,ydim
	siz  = size(inarr)
	if siz[0] eq 2 then begin
		anzx = siz[1]
		anzy = siz[2]
	endif else begin
		anzx = siz[1]
		anzy = 1
	endelse
  if siz[0] gt 1 then begin
		if xdim le anzx and ydim le anzy then begin
			smmx = floor(anzx / xdim) < anzx-1
			smmy = floor(anzy / ydim) < anzy-1
			arr  = atan(congrid(smooth(exp(complex(0,inarr)),[smmx,smmy],/edge_truncate),xdim,ydim),/phase)
		endif else begin
			arr  = atan(congrid(exp(complex(0,inarr)),xdim,ydim),/phase)
		endelse
  endif else begin
 		if xdim le anzx then begin
			smmx = floor(anzx / xdim) < anzx-1
			arr  = atan(congrid(smooth(exp(complex(0,inarr)),[smmx],/edge_truncate),xdim),/phase)
		endif else begin
			arr  = atan(congrid(exp(complex(0,inarr)),xdim),/phase)
		endelse
 endelse
	return,arr
end
