;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: palette_read
; written by    : Maxim Neumann
; last revision : 07.Oct.2005
; reads out the palette from a file, if no file available, returns the 
; standard palette
; filename == "abc.rat" or "abc.pal" or "abc.rit"
;             if not given, the name will be taken from file.name
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

function palette_read, filename=filename
  common rat, types, file, wid, config

  if n_elements(filename) eq 0 then filename=file.name
  f = strmid(filename,0,strlen(filename)-4)

  ritfile = f+'.rit'
  rit_read,ritfile,'palette',pal,stat=ritstat
  if ritstat eq 0 && n_elements(pal) eq 256*3 $
  then return,pal
 
  palfile = f+'.pal'
  if file_test(palfile,/READ) then begin
     rrat,palfile,pal
     if n_elements(pal) eq 256*3 then begin
        rit_write,ritfile,'palette',pal
        file_delete,palfile,/QUIET
        return,pal
     endif
  endif

  pal = [[bindgen(256)],[bindgen(256)],[bindgen(256)]]
  return, pal
end
