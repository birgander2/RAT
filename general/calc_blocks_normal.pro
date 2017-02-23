;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calc_blocks_normal
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
pro calc_blocks_normal,ydim,bs,nrblocks,bs_last
	nrblocks = 1
	ypos = bs
	while (ypos lt ydim) do begin
	  ypos = ypos + bs
	  nrblocks = nrblocks + 1
	endwhile
	bs_last = ydim - (nrblocks-1) * bs
end
