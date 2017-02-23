;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: optimisePalette
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
pro iterateClassValues, h, s, v, valRange

classNum = n_elements(h)
convergenceLim = 1e-6
iterationLim = 80000

hsv = [[reform(h)/180.0],[reform(s)],[reform(v)]]

maxMotion = convergenceLim*10
motionNorm = 0.01
stdDev = sqrt(3)/10
iterations = 0
while (maxMotion gt convergenceLim and iterations lt iterationLim) do begin
    maxMotion = 0.0

    newV = fltarr(classNum)
    for i=0,classNum-1 do begin
        motion = fltarr(classNum-1)
        for j=1,classNum-1 do begin
            testInd = (i+j) mod classNum
            motion[j-1] = exp(-total((hsv[i,*]-hsv[testInd,*])^2)/(stdDev^2))
            motion[j-1] *= motionNorm*(2*(hsv[i,2] gt hsv[testInd,2])-1)
        end
        motion = total(motion)
        newV[i] = hsv[i,2]+motion
    end
    vLim = [min(newV),max(newV)]
    newV = (newV-vLim[0])/(vLim[1]-vLim[0])
    newV = valRange[0] + newV*(valRange[1]-valRange[0])
    maxMotion = max(abs(hsv[*,2]-newV))
    hsv[*,2] = newV
    iterations += 1
end

h = 180.0*hsv[*,0]
s = hsv[*,1]
v = hsv[*,2]

end



function optimisePalette, classHist, classCovar

satRange = [0.25,0.9]
valRange = [0.3,1.0]

channelColor = [[1.0,0,0],[0,1.0,0],[0.15,0.25,0.6]]

classNum = n_elements(classHist)
covarSize = (size(classCovar))[1]

classColors = reform(classCovar,[covarSize,covarSize,classNum,1])
decomp_fredur, block=classColors
classColors = alog(1.0+reform(classColors)) ## channelColor
classColors *= 255.0/max(classColors)

classHist = alog(1.0+classHist)
histRange = [min(classHist),max(classHist)]
classHist = (classHist-histRange[0])/(histRange[1]-histRange[0])

color_convert, classColors[0,*], classColors[1,*], classColors[2,*], h, s, v, /rgb_hsv

s[*] = (1.0-classHist)*satRange[1] + classHist*satRange[0]
v[*] = (1.0-classHist)*valRange[1] + classHist*valRange[0]

iterateClassValues, h, s, v, valRange

color_convert, h, s, v, r, g, b, /hsv_rgb

palette = fltarr(256,3)
palette[0:classNum-1,0] = r
palette[0:classNum-1,1] = g
palette[0:classNum-1,2] = b

return, palette

end
