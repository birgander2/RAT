;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: finaliseEstimate
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
pro finaliseEstimate, estimBlock, newEstimBlock, energyDensity, lastDensity, temperature, randSeed

covarSize = (size(estimBlock))[1]
covarOffset = covarSize^2
blockRes = (size(energyDensity))[1:2]

energyGradient = energyDensity-lastDensity
metropolisThresh = exp(-energyGradient/temperature)
infInd = where(finite(energyDensity) eq 0, nr)
if (nr gt 0) then metropolisThresh[infInd] = 2.0

acceptInd = where(randomu(randSeed,blockRes[0],blockRes[1]) lt metropolisThresh, nr)
if (nr le 0) then return
print, '% accepted:'
print, 100.0*nr/product(blockRes[0:1])

lastDensity[acceptInd] = energyDensity[acceptInd]
energyDensity = lastDensity

acceptInd = covarOffset*(long(acceptInd))
for i=0,covarOffset-1 do begin
    estimBlock[i+acceptInd] = newEstimBlock[i+acceptInd]
end

end
