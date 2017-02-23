;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_coh_rgrow
; written by       : Maxim Neumann
; last revision    : 08/2006
; Multibaseline coherence calculation using region growing
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

pro polin_coh_rgrow,CALLED = called
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 513) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

  speck_polidan, /CALLED, /GUI
  polin_coh_mean, smmx=1,smmy=1, /CALLED

  evolute,'Multibaseline coherence estimation. 1x1'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy
end
