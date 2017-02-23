;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calcEffectiveNumLooks
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
function calcEffectiveNumLooks, covarBlock, winSize

covarSize = (size(covarBlock))[1]
blockRes = (size(covarBlock))[3:4]

lookBuf = fltarr(blockRes)

for i=0,covarSize-1 do begin
    looks = float(reform(covarBlock[i,i,*,*]))
    looks = smooth(looks^2,winSize,/edge_truncate)/(smooth(looks,winSize,/edge_truncate)^2)
    lookBuf += 1.0 / (looks - 1.0)
end

return, (lookBuf / covarSize)
end
