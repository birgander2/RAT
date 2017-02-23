;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: mStep
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
pro mStep, covarBlock, coherBlock, eBlock, newCovar, newCoherAbs, newCoherPh, newWeight, CONVSHOCK=convShock

covarSize = (size(covarBlock))[1]
covarOffset = covarSize^2
coherSize = (size(coherBlock))[1]
blockRes = (size(covarBlock))[3:4]
classNum = (size(eBlock))[1]

if (keyword_set(convShock)) then eBlock *= convShock

eNorm = max(eBlock,dimension=1,/nan)
for i=0,classNum-1 do begin
    eBlock[i,*,*] = exp(eBlock[i,*,*] - eNorm)
end

; ignore = max(eBlock, classif, dimension=1)
; classif = classif mod classNum

eNorm = 1.0/total(eBlock,1)
for i=0,classNum-1 do begin
    ; classInd = classNum * long(where(classif eq i, nr))    
    ; if (nr gt 0) then eBlock[i+classInd] = 1.0
    eBlock[i,*,*] *= eNorm
end

infInd = where(finite(eBlock) eq 0, nr)
if (nr gt 0) then stop

absCoher = abs(coherBlock)

normCoher = coherBlock * conj(shift(coherBlock,[1,0,0]))
normCoher /= abs(normCoher)

; for i=0,coherSize-1 do begin
;     j = (i+1) mod coherSize
;     normCoher[i,*,*] = coherBlock[i,*,*]*conj(coherBlock[j,*,*])
;     normCoher[i,*,*] /= abs(normCoher[i,*,*])
; end

infInd = where(finite(normCoher) eq 0, nr)
if (nr gt 0) then normCoher[infInd] = 0.0

        
dataInd = lindgen(blockRes)
for i=0,classNum-1 do begin
    for j=0,covarOffset-1 do begin
        newCovar[j+covarOffset*i] += total(eBlock[i,*,*] * covarBlock[j+covarOffset*dataInd])
    end
    for j=0,coherSize-1 do begin
        newCoherAbs[j,i] += total(eBlock[i,*,*] * absCoher[j,*,*])
        newCoherPh[j,i] += total(eBlock[i,*,*] * normCoher[j,*,*])

        infInd = where(finite(newCoherAbs) eq 0, nr1)
        infInd = where(finite(newCoherPh) eq 0, nr2)

        if ((nr1 + nr2) gt 0) then stop
    end
    newWeight[i] += total(eBlock[i,*,*])
end

end
