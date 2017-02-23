;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: approxAngDiffDistrib
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
function approxAngDiffDistrib, classAbsCoher, classAng, lookNum, distLen, hypGeomEPS

angles = 2*!pi*(findgen(distLen)/distLen - 0.5)

lngLooks = lnGamma(lookNum)

angDist = complexarr(2,distLen)
A = [1.0,lookNum,lookNum]
B = [0.5,lookNum]    
C = [1.5,lookNum+0.5,lookNum+0.5]
D = [1.5,lookNum+0.5]

classAng[0] -= !pi

for i=0,1 do begin
    z0 = lookNum*alog(1-classAbsCoher[i]^2) - alog(2*!pi)
    z1 = z0 + lnGamma(0.5) + lnGamma(lookNum+0.5) - lnGamma(lookNum)

    cosDiff = classAbsCoher[i]*cos(angles-classAng[i])

    term1 = z0 + lnHypGeom(A,B,cosDiff^2,epsilon=hypGeomEPS)
    term2 = z1 + lnHypGeom(C,D,cosDiff^2,epsilon=hypGeomEPS) + alog(complex(cosDiff))

    logShift = real_part(term1) > real_part(term2)
    angDist[i,*] = alog(exp(term1-logShift)+exp(term2-logShift))+logShift
end

angDiffDist = fltarr(1,distLen);
for i=0,distLen-1 do begin
    lnMult = angDist[0,*] + shift(angDist[1,*],[0,i])
    lnShift = max(lnMult)
    angDiffDist[i] = alog(total(exp(lnMult-lnShift)))+lnShift
end

; angDiffDist = fft(fft(real_part(exp(angDist[0,*]))>0,-1)*fft(real_part(exp(angDist[1,*]))>0,-1),1)

angDiffDist -= alog(total(exp(angDiffDist))/distLen)

return, angDiffDist

end
