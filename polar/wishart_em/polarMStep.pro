;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polarMStep
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
pro polarMStep, covarBlock, eBlock, newCovar, newWeight, convergenceShock

covarSize = (size(covarBlock))[1]
covarOffset = covarSize^2
blockRes = (size(covarBlock))[3:4]
classNum = (size(eBlock))[1]

eNorm = max(eBlock,dimension=1,/nan)
for i=0,classNum-1 do begin
    eBlock[i,*,*] = exp(eBlock[i,*,*] - eNorm)
end

eNorm = 1.0/total(eBlock,1)
for i=0,classNum-1 do begin
    eBlock[i,*,*] *= eNorm
end
eBlock = eBlock^convergenceShock

dataInd = covarOffset*lindgen(blockRes)
for i=0,classNum-1 do begin
    for j=0,covarOffset-1 do begin
        newCovar[j+covarOffset*i] += total(eBlock[i,*,*] * covarBlock[j+dataInd])
    end
    newWeight[i] += total(eBlock[i,*,*])
end

end
