;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calcCliqueConfig
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
function calcCliqueConfig

cliqueConf = intarr(3,3,12)
rot = [1,2,5,0,4,8,3,6,7]
ind = indgen(9)

for i=0,3 do begin
    for j=0,2 do begin
        conf = intarr(3,3)
        conf[ind[1]] = 1
        conf[ind[j+6]] = 1
        cliqueConf[*,*,i*3+j] = conf
    end
    ind = rot[ind]
end

return, cliqueConf
end
