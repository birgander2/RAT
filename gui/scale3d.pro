;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: scale3d
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
function scale3d,inarr,xdim,ydim
	siz  = size(inarr)
	dim  = siz[1]
	anzx = siz[2]
	anzy = siz[3]
	if xdim le anzx and ydim le anzy then begin
		smmx = floor(anzx / xdim) 
		smmy = floor(anzy / ydim)
		arr  = congrid(smooth(inarr,[1,smmx,smmy],/edge_truncate),dim,xdim,ydim) 
	endif else begin
		arr  = congrid(inarr,dim,xdim,ydim) 
	endelse
	return,arr
end
