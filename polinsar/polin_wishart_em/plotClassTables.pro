;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: plotClassTables
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
pro plotClassTables, logTables, coherHist

coherChannels = (size(logTables))[2]
classNum = (size(logTables))[4]

!p.multi = [-1,2*coherChannels,classNum+1]

window, 0, xsize=500, ysize=400

for i=0,coherChannels-1 do begin
    plot, coherHist[*,i,0]
    plot, coherHist[*,i,1]
end

tables = exp(logTables)
infInd = where(finite(tables) eq 0, nr)
if (nr gt 0) then tables[infInd] = 0.0 

for i=0,classNum-1 do begin
    for j=0,coherChannels-1 do begin
        plot, reform(tables[*,j,0,i])

        print, 'tabTotal', total(tables[*,j,0,i])

        plot, reform(tables[*,j,1,i])
    end
end

!p.multi=[-1,0,0]

end

