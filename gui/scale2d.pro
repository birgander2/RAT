;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: scale2d
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
function scale2d,inarr,xdim,ydim
	common rat, types, file, wid, config

		siz  = size(inarr)
if siz[0] eq 2 then begin
		anzx = siz[1]
		anzy = siz[2]
	endif else begin
		anzx = siz[1]
		anzy = 1
	endelse

  if anzx eq xdim && anzy eq ydim then return, inarr

  if siz[0] gt 1 then begin
	  if xdim le anzx and ydim le anzy and not (file.type ge 400 and file.type lt 500) then begin
    	smmx = floor(anzx / xdim) < (anzx-1)
      smmy = floor(anzy / ydim) < (anzy-1)
      if (anzx mod xdim eq 0 || xdim mod anzx eq 0) && (anzy mod ydim eq 0 || ydim mod anzy eq 0) $
      then arr  = rebin  (smooth(inarr,[smmx,smmy],/edge_truncate),xdim,ydim) $
      else arr  = congrid(smooth(inarr,[smmx,smmy],/edge_truncate),xdim,ydim) 
    endif else begin
      if (anzx mod xdim eq 0 || xdim mod anzx eq 0) && (anzy mod ydim eq 0 || ydim mod anzy eq 0) $
      then arr  = rebin  (inarr,xdim,ydim) $
      else arr  = congrid(inarr,xdim,ydim) 
    endelse
   endif else begin
	  if xdim le anzx and not (file.type ge 400 and file.type lt 500) then begin
    	smmx = floor(anzx / xdim) < (anzx-1)
      if (anzx mod xdim eq 0 || xdim mod anzx eq 0) $
      then arr  = rebin  (smooth(inarr,[smmx],/edge_truncate),xdim) $
      else arr  = congrid(smooth(inarr,[smmx],/edge_truncate),xdim) 
    endif else begin
      if (anzx mod xdim eq 0 || xdim mod anzx eq 0) $
      then arr  = rebin  (inarr,xdim) $
      else arr  = congrid(inarr,xdim) 
    endelse
   endelse
	return,arr
end
