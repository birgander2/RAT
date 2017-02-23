;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: view_ratpdf
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
pro view_ratpdf, file=pdffile, path=path, GUIDE=GUIDE
  common rat ;, types, file, wid, config

  if n_elements(path) eq 0 then path=config.docdir
  if keyword_set(GUIDE) then pdffile='user_guide_v1.0.pdf'

  if ~file_test(path+pdffile, /READ) then begin
     res = dialog_message(['No User Guide could be found.', $
                           ' ', $
                           'Please check the online documentation', $
                           'at the RAT homepage (see Help->Contact).'], dialog_parent=main, /ERROR)
     return
  endif

  if config.os eq 'unix' then $
     spawn,config.pdfviewer+' '+path+pdffile+' &' $
  else if config.os eq 'windows' then $
     spawn,config.pdfviewer+' '+path+pdffile,/nowait,/noshell

end
