;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: updateEstimate
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
pro updateEstimate, covarBlock, estimBlock, lookBuf, newEstimBlock, clique, energyDensity, lookNum, singularCovar

covarSize = (size(covarBlock))[1]
covarOffset = covarSize^2
blockRes = (size(lookBuf))[1:2]
dataInd = covarOffset*lindgen(blockRes)

cliqueInd = where(clique gt 0)
cliqueX = 1-(cliqueInd mod 3)
cliqueY = 1-floor(cliqueInd / 3)

energy = fltarr(blockRes)
; sumLooks = fltarr(blockRes)
; estimDet = float(lookBuf*alog(block_det(estimBlock)))
; logLooks = alog(lookBuf)

; sumLooks = fltarr(blockRes)
estimDet = reform(float(alog(block_det(estimBlock)))) ;reform(float(lookNum*alog(block_det(estimBlock))))
; logLooks = alog(lookNum)

estim = complexarr([covarSize,covarSize,blockRes])
for i=0,1 do begin
    shiftV = [0,0,cliqueX[i],cliqueY[i]]

    estim += shift(estimBlock,shiftV)

    shiftLooks = lookNum; shift(lookBuf,shiftV[2:3])
    energy += shift(estimDet,shiftV[2:3])
    ; energy -= covarSize*shiftLooks*logLooks ;shift(logLooks,shiftV[2:3])
    ; sumLooks += shiftLooks
end

if (singularCovar) then begin
    energy -= 2*float(alog(block_det(estim))); sumLooks*float(alog(block_det(estim)))
    ; energy += covarSize*sumLooks*alog(sumLooks)

    origInten = float(block_diag(covarBlock))
    estimInten = 0.5*float(block_diag(estim))
    energy += total(alog(origInten)+alog(estimInten),1)
    energy -= 2*total(alog(origInten+estimInten),1)
end else begin
    energy += float(alog(block_det(covarBlock))); lookNum*float(alog(block_det(covarBlock)))
    ; energy -= covarSize*lookNum*alog(lookNum)
    energy -= 3*float(alog(block_det(estim+covarBlock))) ;(sumLooks+lookNum)*float(alog(block_det(estim+covarBlock)))
    ; energy += covarSize*(sumLooks+lookNum)*alog(sumLooks+lookNum)
end

; energy -= sumLooks*float(alog(block_det(estim)))
; energy += (covarSize*sumLooks)*alog(sumLooks)

energy = -energy

; tva, energy, win=0, xsize=300, ysize=300
; tva, finite(energy),win=1, xsize=300, ysize=300, /n
print, min(energy,/nan), max(energy,/nan), total(finite(energy))/product(blockRes)
;print, clique

relativeLooks = lookBuf/lookNum
for i=0,covarOffset-1 do begin
    estim[i+dataInd] /= 2.0 + 1.0/relativeLooks
    estim[i+dataInd] += covarBlock[i+dataInd]/(1+2*relativeLooks)
end

;  energy = fltarr(blockRes)

;  estimInv = block_inv(estim)

;  for i=0,1 do begin
;      shiftV = [0,0,cliqueX[i],cliqueY[i]]
;      energy += float(block_trace(real_part(block_mm(estimInv,shift(estimBlock,shiftV)))))
;  end

;  energy *= lookBuf
;  energy += lookNum * float(block_trace(real_part(block_mm(estimInv,covarBlock))))
;  energy += (lookNum+2*lookBuf) * alog(float(block_det(estim)))

; a = energy
; infInd = where(finite(a) eq 0, nr)
; if (nr gt 0) then a[infInd] = min(a,/nan)
; tva, a, win=2

infInd = where(finite(energy) eq 0, nr)
if (nr gt 0) then energy[infInd] = !values.f_infinity

updateMask = (energy lt energyDensity) + (1-finite(energy)) * (randomu(seed,blockRes) lt 1.0/12)
updateInd = covarOffset*long(where(updateMask, nr))

if (nr le 0) then return

for i=0,covarOffset-1 do begin
    newEstimBlock[i+updateInd] = estim[i+updateInd]
end

energyDensity <= energy

end
