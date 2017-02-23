;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: integrateHypGeom
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
function integrateHypGeom, A,B,C,D, absCoher, classAng, angShift, expShift, wisdom

angNum = n_elements(angShift)

lnTotal = complexarr(angNum)-!values.f_infinity

lngA = total(lnGamma(complex(A)))
lngB = total(lnGamma(complex(B)))
lngC = total(lnGamma(complex(C)))
lngD = total(lnGamma(complex(D)))

lnCoh = alog(absCoher)
ln2 = alog(2.0)

offset1 = -classAng[0]
offset2 = angShift - classAng[1]

for i=0,wisdom do begin
    diag = wisdom-i
    for j=0,diag do begin
        m = diag-j
        n = j

        uL = 2*m+expShift[0]
        vL = 2*n+expShift[1]

        lnTerm = total(lnGamma(A+m))+lngB
        lnTerm -= lngA+total(lnGamma(B+m))+lnGamma(m+1)

        lnTerm += total(lnGamma(C+n))+lngD
        lnTerm -= lngC+total(lnGamma(D+n))+lnGamma(n+1)
        
        lnTerm += total([uL,vL] * lnCoh)
        lnFact = complex(total(lnGamma([uL,vL]+1)))

        lnInt = complexarr(angNum)-!values.f_infinity

        for u=0,uL do begin
            for v=0,vL do begin
                lnBinom = lnFact
                lnBinom -= total(lnGamma([u,v]+1))+total(lnGamma(1+[uL,vL]-[u,v]))
                
                lnIntTerm = complex(0,offset1*(2*u-uL) + offset2*(2*v-vL))+lnBinom
                
                if ((uL+vL) eq 2*(u+v)) then begin
                    lnIntTerm += alog(2*!pi)
                end else begin
                    lnIntTerm += complex(0,!pi/2) - alog(complex((uL+vL)-2*(u+v),0))
                    lnIntTerm += alog(exp(2*!pi*complex(0,2*(u+v)-(uL+vL))) - 1.0)
                end
                
                if (min(finite(lnIntTerm)) eq 0) then stop
                
                logShift = real_part(lnInt) > real_part(lnIntTerm)
                lnInt = alog(exp(lnInt-logShift)+exp(lnIntTerm-logShift))+logShift
                
                if (min(finite(lnInt)) eq 0) then stop
            end
        end

        lnInt += lnTerm - total([uL,vL])*ln2

        logShift = real_part(lnTotal) > real_part(lnInt)
        lnTotal = alog(exp(lnTotal-logShift)+exp(lnInt-logShift))+logShift

        if (min(finite(lnTotal)) eq 0) then stop
    end
end

return, lnTotal
end
