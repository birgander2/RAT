;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: hypGeom
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

; Copyright (c) 2005 Marc Jaeger (jaeger at cs.tu-berlin.de)
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.



; DESCRIPTION
; Computes the generalised hypergeometric function by direct complex
; contour integration using the Runge-Kutta-Fehlberg algorithm.
; 
; 
; USAGE
; result = hypGeom(a, b, z [, /RELEPS=value] [, /ABSEPS=value])
;
; a:   parameter vector (e.g. [2,2,1.5], 2 or -1).
;      Negative 'a' implies no value.
; b:   parameter vector (e.g. [2,2,1.5], 2 or -1).
;      Negative 'b' implies no value.
; z:   Complex array of arbitrary size and dimensionality.
;
; Keyword RELEPS: The desired relative accuracy (maximum fractional error)
;                 Default value 1e-7.
; Keyword ABSEPS: The desired absolute accuracy (maximum absolute error)
;                 Default value 1e-3*RELEPS.
; 
; 
; EXAMPLES
; 0F1({},{1},z):       r = hypGeom(-1,1,z)
; 1F2({1},{2,1},z):    r = hypGeom(1,[2,1],z)
;
; IDL> print, hypGeom([1,2],2,2)
; (    -1.000000,  2.53785e-07)


function hGMultiplyTerm, c, terms

termLen = (size(terms))[1]
newTerms = fltarr(termLen*3,3)

termInd = 0
for i=0,termLen-1 do begin
    newTerms[termInd,*] = [reform(terms[i,0:1]),terms[i,2]*c]
    newTerms[termInd+1,*] = [reform(terms[i,0:1]),terms[i,2]*terms[i,1]]
    newTerms[termInd+2,*] = [reform(terms[i,0:1])+1,terms[i,2]]
    termInd += 3
end

newTerms = newTerms[0:termInd-1,*]
return, newTerms
end



pro initHGPolyInfo, a, b, lhsTerms, rhsTerms

valid = [min(a) gt 0, min(b) gt 0]
coefNum = valid*[n_elements(a), n_elements(b)]

aTerms = reform([0.,0.,1.],[1,3])
if (valid[0]) then begin
    for i=0,coefNum[0]-1 do aTerms = hGMultiplyTerm(a[i], aTerms)
end
aTerms[*,1] += 1

bTerms = reform([0.,0.,1.],[1,3])
if (valid[1]) then begin
    for i=0,coefNum[1]-1 do bTerms = hGMultiplyTerm(b[i]-1, bTerms)
end
bTerms = hGMultiplyTerm(0,bTerms)
bTerms[*,2] *= -1

termList = [aTerms,bTerms]
termNum = (size(termList))[1]
lastInd = termNum-1
maxDeriv = max(termList[*,0])
maxInd = 0

for i=0,termNum-1 do begin
    for j=i+1,lastInd do begin
        if (j gt lastInd) then break
        if (max(abs(termList[i,0:1]-termList[j,0:1])) lt 0.5) then begin
            termList[i,2] += termList[j,2]
            termList[j,*] = termList[lastInd,*]
            lastInd -= 1
            j -= 1
        end
    end
    
    if (i gt lastInd) then break

    if (termList[i,2] eq 0) then begin
        termList[i,*] = termList[lastInd,*]
        lastInd -= 1
        i -= 1
        continue
    end
    
    if (termList[i,0] eq maxDeriv) then begin
        tmp = termList[maxInd,*]
        termList[maxInd,*] = termList[i,*]
        termList[i,*] = tmp
        maxInd += 1
    end
end
termList[*,1] -= min(termList[0:lastInd,1])

lhsTerms = termList[0:maxInd-1,*]
lhsTerms[*,2] *= -1
order = sort(lhsTerms[*,1])
lhsTerms = lhsTerms[order,*]
firstPow = lhsTerms[0,1]
lhsTerms[*,1] -= shift(lhsTerms[*,1],1)
lhsTerms[0,1] = firstPow

rhsTerms = termList[maxInd:lastInd,*]
order = sort(rhsTerms[*,1])
rhsTerms = rhsTerms[order,*]
firstPow = rhsTerms[0,1]
rhsTerms[*,1] -= shift(rhsTerms[*,1],1)
rhsTerms[0,1] = firstPow

end



pro updateHGTopGrad, lhsTerms, rhsTerms, deriv, z
termNum = [(size(lhsTerms))[1],(size(rhsTerms))[1]]
topCoef = lhsTerms[0,0]
zLen = n_elements(z)

deriv[topCoef,*] = 0.0
zFact = complexarr(zLen)+1.0
for i=0,termNum[1]-1 do begin
    for j=0,rhsTerms[i,1]-1 do zFact *= z
    deriv[topCoef,*] += rhsTerms[i,2]*zFact*deriv[rhsTerms[i,0],*]
end

lhs = complexarr(zLen)
zFact[*] = 1.0
for i=0,termNum[0]-1 do begin
    for j=0,lhsTerms[i,1]-1 do zFact *= z
    lhs += zFact*lhsTerms[i,2]
end

deriv[topCoef,*] /= lhs

end



function calcHGSeriesDeriv, a, b, z

zLen = n_elements(z)
valid = [min(b) gt 0, min(a) gt 0]
coefNum = max(valid*[n_elements(b)+1, n_elements(a)])>1

deriv = complexarr([coefNum+1, zLen])
derivIncr = fltarr(zLen)+1

lnZ = alog(complex(z))

termNum = 1
deriv[0,*] = 1.0
lnFactor = 0.0
if (valid[1]) then lnFactor += total(alog(a))
if (valid[0]) then lnFactor -= total(alog(b))

while (max(derivIncr) gt 0) do begin
    derivIncr[*,*] = 0

    lnDiffMod = 0.0
    for i=0,(coefNum<termNum) do begin
        zMult = (i eq termNum) ? 0 : (termNum-i)*lnZ
        newTerm = exp(lnFactor + zMult + lnDiffMod)
        derivIncr += abs(deriv[i,*]-(deriv[i,*]+newTerm))
        deriv[i,*] += newTerm
        lnDiffMod += alog(termNum-i)
    end

    if (valid[1]) then lnFactor += total(alog(a+termNum))
    if (valid[0]) then lnFactor -= total(alog(b+termNum))

    termNum += 1
    lnFactor -= alog(termNum)
end

return, deriv
end




function hypGeom, a, b, z, RELEPS=relEps, ABSEPS=absEps

if (not keyword_set(relEps)) then relEps = 1e-7
if (not keyword_set(absEps)) then absEps = relEps * 1e-3

origSize = size(z)
if (origSize[0] eq 0) then origSize = 1 else origSize = origSize[1:origSize[0]]
z = reform(z,n_elements(z))

zLen = n_elements(z)
valid = [min(b) gt 0, min(a) gt 0]
coefNum = max(valid*[n_elements(b)+1, n_elements(a)])>1

initHGPolyInfo, a, b, lhsTerms, rhsTerms

initZ = complexarr(4)
for i=0,3 do initZ[i] = 0.5*exp(complex(0, 0.5*i*!pi))
initDeriv = calcHGSeriesDeriv(a,b,initZ)

reZ = real_part(z)
imZ = imaginary(z)
masks = [[reZ ge 0 and reZ lt 1.0], [reZ ge 1.0 and imZ ge 0], [reZ lt 0], [reZ ge 1.0 and imZ lt 0],[abs(z) gt 0.5]]
masks = reform(masks,[zLen,5])

dz = z
z[*] = 0.0
deriv = complexarr([coefNum+1,zLen])
for i=0,3 do begin
    masks[*,i] *= masks[*,4]
    z += masks[*,i]*initZ[i]
    for j=0,coefNum do begin
        deriv[j,*] += masks[*,i]*initDeriv[j+(coefNum+1)*i]
    end
end

absInd = where((1-masks[*,4]) gt 0.5, nr)
if (nr gt 0) then begin
    absZ = dz[absInd]
    absDeriv = calcHGSeriesDeriv(a,b,absZ)
    z[absInd] = absZ
    absInd *= coefNum+1
    for j=0,coefNum do begin
        deriv[absInd+j] += absDeriv[j,*]
    end
end
dz -= z

aCoef = [0.0,0.2,0.3,0.6,1.0,0.875]
bCoef = [[0,0,0,0,0],[0.2,0,0,0,0],[3./40,9./40,0,0,0],[0.3,-0.9,1.2,0,0],[-11./54,2.5,-70./27,35./27,0],[1631./55296,175./512,575./13824,44275./110592,253./4096]]
cCoef = [37./378,0,250./621,125./594,0,512./1771]
cErr = [2825./27648,0,18575./48384,13525./55296,277./14336,0.25]
cErr = cCoef-cErr

step = fltarr(zLen)+1e-3
s = fltarr(zLen)
delta = fltarr([coefNum,zLen])

kBuf = complexarr([5,coefNum+1,zLen])
k = complexarr([coefNum+1,zLen])

newDeriv = complexarr([coefNum,zLen])
newStep = fltarr(zLen)
sRestrain = 0

doneMask = bytarr(zLen)
doneNr = 0

while (min(doneMask) lt 1) do begin
    newDeriv[*,*] = deriv[0:coefNum-1,*]
    delta[*,*] = 0
    kBuf[*,*,*] = 0
    zStep = dz*step

    for i=0,5 do begin
        k[*,*] = 0.0
        for j=0,i-1 do k[0:coefNum-1,*] += bCoef[j,i]*kBuf[j,1:coefNum,*]
        for j=0,coefNum-1 do k[j,*] *= zStep
        k += deriv

        updateHGTopGrad, lhsTerms, rhsTerms, k, z + aCoef[i]*zStep
        if (i lt 5) then kBuf[i,*,*] = k

        for j=0,coefNum-1 do begin
            delta[j,*] += zStep*cErr[i]*k[j+1,*]
            newDeriv[j,*] += zStep*cCoef[i]*k[j+1,*]
        end
    end

    newStep[*] = !values.f_infinity
    for i=0,coefNum-1 do begin
        e0 = (relEps*(abs(deriv[i,*])+abs(step*deriv[i+1,*])))>absEps
        newStep <= abs(step)*(e0/abs(delta[i,*]))^0.2
    end

    updateMask = (newStep ge step)
    if (doneNr gt 0) then doneMask[doneInd] >= updateMask[doneInd]

    z += updateMask*zStep
    s += updateMask*step
    for i=0,coefNum-1 do begin
        deriv[i,*] *= (1-updateMask)
        deriv[i,*] += updateMask*newDeriv[i,*]
    end
    updateHGTopGrad, lhsTerms, rhsTerms, deriv, z
    step[*] = newStep

    doneInd = where((s+step) gt 1.0, doneNr)
    if (doneNr gt 0) then step[doneInd] = 1.0-s[doneInd]
end

zeroInd = where(z eq 0.0, nr)
if (nr gt 0) then deriv[0,zeroInd] = 1.0

return, reform(deriv[0,*], origSize)

end
