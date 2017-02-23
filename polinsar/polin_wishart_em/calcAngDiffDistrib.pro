;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calcAngDiffDistrib
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
function calcAngDiffDistrib, classAbsCoher, classAng, lookNum, distLen, wisdom

angShift = 2*!pi*((findgen(distLen)+1)/(distLen+1) - 0.5)

lngLooks = lnGamma(lookNum)

z = complexarr(2,2)

z[0,0] = lookNum*alog(1-classAbsCoher[0]^2) - alog(2*!pi)
z[0,1] = z[0,0] + lnGamma(0.5) + lnGamma(lookNum+0.5) - lnGamma(lookNum)

z[1,0] = lookNum*alog(1-classAbsCoher[1]^2) - alog(2*!pi)
z[1,1] = z[1,0] + lnGamma(0.5) + lnGamma(lookNum+0.5) - lnGamma(lookNum)


A = [1.0,lookNum,lookNum]
B = [0.5,lookNum]

C = [1.5,lookNum+0.5,lookNum+0.5]
D = [1.5,lookNum+0.5]

result = complexarr(distLen,4)

r0 = integrateHypGeom(A,B,A,B, classAbsCoher, classAng, angShift, [0,0], wisdom)
result[*,0] = z[0,0]+z[1,0] + r0

result[*,1] = z[0,0]+z[1,1] + integrateHypGeom(A,B,C,D, classAbsCoher, classAng, angShift, [0,1], wisdom)
result[*,2] = z[0,1]+z[1,0] + integrateHypGeom(C,D,A,B, classAbsCoher, classAng, angShift, [1,0], wisdom)

r3 = integrateHypGeom(C,D,C,D, classAbsCoher, classAng, angShift, [1,1], wisdom)
result[*,3] = z[0,1]+z[1,1] + r3

lnTotal = complexarr(distLen)-!values.f_infinity
for i=0,3 do begin
    logShift = real_part(result[*,i]) > real_part(lnTotal)
    lnTotal = alog(exp(lnTotal-logShift)+exp(result[*,i]-logShift))+logShift
end

negInd = where(abs(imaginary(lnTotal)) gt 0.5, nr)
if (nr gt 0) then lnTotal[negInd] = min(real_part(lnTotal))

return, lnTotal

end
