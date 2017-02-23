;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; calculates log(pFq(a1,...,ap; b1,...,bq; x))
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

function lnGenHyp, a, b, x, wisdom

lngammaA = total(complex(lngamma(a)))
lngammaB = total(complex(lngamma(b)))
lnX = complex(alog(x))

for i=0,wisdom-1 do begin
    termNum = wisdom-i-1

    lnFact = lngamma(termNum+1)

    term = termNum*lnX
    term += total(lngamma(a+termNum))-lngammaA
    term -= lnFact + total(lngamma(b+termNum))-lngammaB

    if (i ne 0) then begin
        logShift = real_part(lnSeriesTotal)>real_part(term)
        lnSeriesTotal = alog(exp(lnSeriesTotal-logShift)+exp(term-logShift))+logShift
    end else begin
        lnSeriesTotal = term
    end
end

return, lnSeriesTotal
end
