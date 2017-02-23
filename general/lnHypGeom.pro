;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: lnHypGeom
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
function lnHypGeom, a, b, z, EPSILON=epsilon

zLen = n_elements(z)
origSize = size(reform(z))
origSize = origSize[1:origSize[0]]
valid = [min(a),min(b)] ge 0
coefNum = valid*[n_elements(a),n_elements(b)]

if (not keyword_set(epsilon)) then epsilon = 0

if (not valid[0]) then a = 1
if (not valid[1]) then b = 1

z = reform(z,[zLen,1])
absZ = abs(z)
absOrder = sort(absZ)

z = z[absOrder]
absZ = absZ[absOrder]
argZ = atan(complex(z),/phase)
lnAbsZ = alog(absZ)

lnA = total(lngamma(a))
lnB = total(lngamma(b))

workBuf = fltarr(zLen,6)

k = long(0)
for t=0,zLen-1 do begin
    while (lnAbsZ[t] + valid[0]*total(alog(a+k)) - valid[1]*total(alog(b+k)) - alog(k+1) > 0) do begin
        k += 1
    end

    workBuf[t,0] = k
    workBuf[t,1] = k*lnAbsZ[t] - lngamma(k+1)
    workBuf[t,1] += valid[0]*(total(lngamma(a+k)) - lnA)
    workBuf[t,1] -= valid[1]*(total(lngamma(b+k)) - lnB)
end

lnK = lngamma(workBuf[*,0]+1)
lnA0 = fltarr(zLen)
lnB0 = fltarr(zLen)
for i=0,coefNum[0]-1 do begin
    lnA0 += lngamma(a(i)+workBuf[*,0])
end
for i=0,coefNum[1]-1 do begin
    lnB0 += lngamma(b(i)+workBuf[*,0])
end

deltaK = 0
deltaFn = epsilon+1
lastCorrect = fltarr(zLen,2)
while (deltaFn gt epsilon) do begin
    lastCorrect[*,*] = workBuf[*,4:5]

    deltaK += 1
    for i=0,1 do begin
        k = workBuf[*,0]+(2*i-1)*deltaK
        workBuf[*,i+2] = (2*i-1)*deltaK*lnAbsZ + lnK - lngamma(k+1)

        for j=0,coefNum[0]-1 do begin
            workBuf[*,i+2] += lngamma((a(j)+k)>1)
        end
        workBuf[*,i+2] -= lnA0

        for j=0,coefNum[1]-1 do begin
            workBuf[*,i+2] -= lngamma((b(j)+k)>1)
        end
        workBuf[*,i+2] += lnB0
        
        workBuf[*,i+2] = exp(workBuf[*,i+2])        
    end
    
    workBuf[*,4] += cos(argZ*deltaK)*(workBuf[*,3]+workBuf[*,2])
    workBuf[*,5] += sin(argZ*deltaK)*(workBuf[*,3]-workBuf[*,2])

    deltaFn = max(abs(workBuf[*,4:5]-lastCorrect))
end
workBuf[*,4] += 1

result = complexarr(zLen)
result[*] = 0.5*alog(total(workBuf[*,4:5]^2,2))+workBuf[*,1]
result[*] += complex(0,argZ*workBuf[*,0]+atan(workBuf[*,5],workBuf[*,4]));

if (valid[0] lt 1) then a = -1
if (valid[1] lt 1) then b = -1

result[absOrder] = result[*]

return, reform(result,origSize)

end
