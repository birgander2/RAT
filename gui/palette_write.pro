;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: palette_write
; written by    : Maxim Neumann
; last revision : 07.Oct.2005
; writes the palette to a file
; filename == "abc.rat" or "abc.pal" or "abc.rit"
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

pro palette_write, pal, filename=filename
  common rat, types, file, wid, config

  if n_elements(filename) eq 0 then filename = file.name 
  f = strmid(filename,0,strlen(filename)-4)

  ritfile = f+'.rit'
  rit_write,ritfile,'palette',pal,stat=ritstat

  palfile = f+'.pal'
  file_delete,palfile,/QUIET
end
