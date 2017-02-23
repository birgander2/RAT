;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: makeGenHypWisdom
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
function makeGenHypWisdom, lookNum, a, b, logMinTerm
; Compute the number of terms we need to sum in the generalised
; hypergeometric function. This only works because 0<=x<=1!

maxWisdom = long(50000)
wisdom = long(1)

lnTerm = 0.0
lnFact = 0.0
lnX = alog(0.95)
while((real_part(lnTerm) gt logMinTerm) and (wisdom lt maxWisdom)) do begin
    lnFact += alog(wisdom)
    lnTerm = total(lngamma(a+wisdom)-lngamma(a)) + wisdom*lnX
    lnTerm -= total(lngamma(b+wisdom)-lngamma(b)) + lnFact
    wisdom += 1l
end

return, wisdom
end
