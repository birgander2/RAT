;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: block_xprod
; written by    : Andreas Reigber (TUB)
; last revision : 24.February.2004
; blockwise outer product of vectors
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

function block_xprod,arr1,arr2
	common rat, types, file, wid, config
        siz  = size(arr1)
	dim  = siz[1]
        xdim = siz[2]
	ydim = siz[3]
        type = siz[4]

	out = make_array([dim,dim,xdim,ydim],type=type)
	for i=0,dim-1 do for j=0,dim-1 do out[i,j,*,*] = arr1[j,*,*] * arr2[i,*,*]
	return,out

end
