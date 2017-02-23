;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: reduceDynRange
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
pro reduceDynRange, covarBlock

covarSize = (size(covarBlock))[1]
covarOffset = covarSize^2
blockRes = (size(covarBlock))[3:4]

normBlock = abs(block_trace(covarBlock))
normBlock = alog(1.0+normBlock)/normBlock

dataInd = covarOffset*lindgen(blockRes)
for i=0,covarOffset-1 do begin
    covarBlock[i+dataInd] = normBlock*covarBlock[i+dataInd]
end

end

