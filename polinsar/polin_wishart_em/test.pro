;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: test
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
pro test

testLen = 4096
testAng = 2*!pi*findgen(testLen)/(testLen-1)
debug = complexarr(testLen) - !values.f_infinity

ln2 = alog(2)

uL = 4
vL = 3

offset1 = 0.0 ;0.1 + !pi/4
offset2 = 0.0 ;0.8 + !pi/4

expShift = [0,0]
lnFact = complex(total(lnGamma([uL,vL]+1)))

lnInt = -!values.f_infinity

for u=0,uL do begin
    for v=0,vL do begin
        lnBinom = lnFact
        lnBinom -= total(lnGamma([u,v]+1))+total(lnGamma(1+[uL,vL]-[u,v]))
        
        debugTerm = complex(0,(testAng-offset1)*(2*u-uL) + (testAng-offset2)*(2*v-vL))
        debugTerm += lnBinom - (uL+vL)*alog(2.0)
        debugShift = real_part(debug) > real_part(debugTerm)
        debug = alog(exp(debug-debugShift)+exp(debugTerm-debugShift))+debugShift

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

lnInt -= total([uL,vL])*ln2

testData = cos(testAng+offset1)^(uL+expShift[0]) * cos(testAng+offset2)^(vL+expShift[1])

testInt = 2*!pi * total(testData)/testLen

print, 'diff'
print, real_part(lnInt)-alog(testInt)
print, 'ratio'
print, real_part(lnInt)/alog(testInt)

stop

; lookNum = 88.92
; x = 0.25
; wisdom = 22529

; ; s = lnGenHyp([lookNum,lookNum],[1],x,wisdom)

; a=[complex(1,1),1]
; b=[complex(2,-1),3,3]
; x=1.5

; s = lnGenHyp(a,b,x,4000)

; print, s, exp(s)

end
