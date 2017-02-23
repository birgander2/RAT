;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calc_blocks_overlap
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
pro calc_blocks_overlap,ydim,bs,overlap,nrblocks,bs_last

recalculate:

	if 2*overlap gt bs then begin		
		bs *= 2
	endif

	if bs ge ydim then begin
		nrblocks = 1
		bs = ydim
		bs_last = ydim
		return
	endif

	nrblocks = 1
	ypos1 = 0                
	ypos2 = bs - overlap
	while (ypos2 lt ydim) do begin
		ypos1 = ypos2 - overlap
		ypos2 = ypos1 + bs
		nrblocks++
		if ypos2 ge ydim then break 
		ypos2 -= overlap
	endwhile
	bs_last = ydim - ypos1

; last block too small?
	if bs_last le overlap then begin
		overlap++
		goto,recalculate
	endif

end
