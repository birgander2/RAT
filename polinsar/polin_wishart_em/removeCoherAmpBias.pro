;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: removeCoherAmpBias
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
function removeCoherAmpBias, classCoher, lookNum, wisdom, ITERATION_NUM=iteration_num

classNum = (size(classCoher))[1]
coherChannels = (size(classCoher))[2]

if (not keyword_set(iteration_num)) then iteration_num = 50

coherAmp = abs(classCoher)

ampLim = fltarr(2,classNum,coherChannels)
ampLim[1,*,*] = 0.999
a = [1.5,lookNum,lookNum]
b = [lookNum+0.5,1.0]

for i=0,iteration_num-1 do begin
    ampGuess = 0.5*total(ampLim,1)
    lnMeanCoher = (lngamma(lookNum)+lngamma(1.5)-lngamma(lookNum+0.5)) + lookNum*alog(1-ampGuess^2) + lnGenHyp(a,b,ampGuess^2,wisdom)
    meanCoher = exp(float(lnMeanCoher))
    
    smallInd = where(meanCoher lt coherAmp, nr)
    if (nr gt 0) then ampLim[2*smallInd] = ampGuess[smallInd]

    bigInd = where(meanCoher gt coherAmp, nr)
    if (nr gt 0) then ampLim[1+2*bigInd] = ampGuess[bigInd]
end

return, 0.5*total(ampLim,1)*exp(complex(0,atan(classCoher,/phase)))

end
