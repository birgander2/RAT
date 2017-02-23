;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: eStep
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
function eStep, covarBlock, coherBlock, classCovar, classLogTables, lookNum, coherHist, useFlags

covarSize = (size(covarBlock))[1]
coherChannels = (size(coherBlock))[1]
blockRes = (size(covarBlock))[3:4]
classNum = (size(classCovar))[3]
logTableLen = (size(classLogTables))[1]
coherHistLen = (size(coherHist))[1]

invClassCovar = block_inv(classCovar)
lnClassDet = alog(block_det(classCovar))

coherAbs = (round((logTableLen+1)*abs(coherBlock) - 1)>0)<(logTableLen-1)

coherPhase = intarr([coherChannels,blockRes])
for i=0,coherChannels-1 do begin
    j = (i+1) mod coherChannels
    coherPhase[i,*,*] = (round(logTableLen*(0.5+atan(coherBlock[i,*,*]*conj(coherBlock[j,*,*]),/phase)/(2*!pi)))>0)<(logTableLen-1)
end

for i=0,coherChannels-1 do begin
    coherHist[*,i,0] += histogram(coherAbs[i,*,*],min=0,max=(logTableLen-1),nbins=coherHistLen)
    coherHist[*,i,1] += histogram(coherPhase[i,*,*],min=0,max=(logTableLen-1),nbins=coherHistLen)
end

lnDetBlock = alog(block_det(covarBlock))
lnCoherBlock = alog(coherBlock)

logEBlock = fltarr([classNum, blockRes])
for i=0,classNum-1 do begin

    if (useFlags[1] eq 1) then begin
        logEBlock[i,*,*] = lnDetBlock - lnClassDet[i] - block_trace(real_part(block_mm(reform(invClassCovar[*,*,i]),covarBlock)))
        logEBlock[i,*,*] *= lookNum
    end

    for j=0,coherChannels-1 do begin
        absTableOffset = j*logTableLen + i*logTableLen*coherChannels*2
        phaseTableOffset = absTableOffset + logTableLen*coherChannels

        if (useFlags[2] eq 1) then logEBlock[i,*,*] += classLogTables[coherAbs[j,*,*] + absTableOffset]

        if (useFlags[3] eq 1) then logEBlock[i,*,*] += classLogTables[coherPhase[j,*,*] + phaseTableOffset]
    end
end

infInd = where(finite(logEBlock) eq 0, nr)
if (nr gt 0) then logEBlock[infInd] = min(logEBlock,/nan)
print, 'Nans:', nr
; if (nr gt 0) then stop

return, logEBlock

end
