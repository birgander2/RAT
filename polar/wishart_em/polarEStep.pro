;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polarEStep
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
function polarEStep, covarBlock, classCovar, classWeight, lookNum

covarSize = (size(covarBlock))[1]
blockRes = (size(covarBlock))[3:4]
classNum = n_elements(classWeight)

invClassCovar = block_inv(classCovar)
lnClassDet = alog(block_det(classCovar))
lnDetBlock = alog(block_det(covarBlock))
logWeight = alog(classWeight)

logEBlock = fltarr([classNum, blockRes])
for i=0,classNum-1 do begin
    logEBlock[i,*,*] = (lookNum-covarSize)*lnDetBlock - lookNum*(lnClassDet[i] + block_trace(real_part(block_mm(reform(invClassCovar[*,*,i]),covarBlock))))
end
logEBlock *= lookNum

infInd = where(finite(logEBlock) eq 0, nr)
if (nr gt 0) then logEBlock[infInd] = min(logEBlock,/nan)

return, logEBlock

end
