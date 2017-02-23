;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: evolute
; written by    : Maxim Neumann
; last revision : 17.Oct.2005
; PARAMETERS:
;   curr_step: will be added to the evolution of the file
;   small:     will be added to the file.info text (optional)
; EXAMPLE:
;   evolute,'RefLee Speckle box'+strcompress(smm),SMALL='rLee'
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


pro evolute,curr_step, SMALL=small, NO_TIMESTAMP=NO_TIMESTAMP
  common rat
  common rit, parstruct, evolution
  
  if ~keyword_set(NO_TIMESTAMP) then $
     curr_step = strjoin(curr_step)+' TIMESTAMP: '+systime() $
  else curr_step=strjoin(curr_step)
  if n_elements(evolution) eq 1 && strlen(evolution) eq 0 then $
     evolution = [curr_step] $
  else evolution = [evolution, curr_step]

  if n_elements(small) eq 1 then file.info += ' '+small

  if config.batch then print,'Finished: '+curr_step
end
