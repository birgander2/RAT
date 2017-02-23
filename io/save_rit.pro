;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: save_rit
; written by    : Maxim Neumann
; last revision : 17.Oct.2005
; Write the parstruct values to the rit file
; PARAMETERS:
; EXAMPLE:
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

pro save_rit, FILENAME=FILENAME,UNDO_PREPARE=UNDO_PREPARE
  common rat
  common channel, channel_names, channel_selec, color_flag, palettes, pnames
  common rit, pars,evolution             ; pars== parameters structure ==parstruct

  if n_elements(FILENAME) eq 0 then FILENAME = file.name
  if strmid(filename,strlen(filename)-4) eq '.rat'  then base=file_basename(filename, '.rat')
  if strmid(filename,strlen(filename)-4) eq '.rit'  then base=file_basename(filename, '.rit')
  if strmid(filename,strlen(filename)-5) eq '.mrat' then base=file_basename(filename,'.mrat')
;  rit_file = file_dirname(filename,/mark)+base+'.rit'
  rit_file = strmid(FILENAME,0,strlen(FILENAME)-4)+'.rit'


;;; system parameters
;   PN        = ptr_new()
;   par_nr    = n_tags(pars)
;   par_names = tag_names(pars)
;   for i=0,par_nr-1 do $
;      if pars.(i) ne PN then $
;         rit_write,RIT_FILE,par_names[i],*pars.(i), STATUS=STATUS
;;; evolution
;   if n_elements(evolution) ne 1 || strlen(evolution[0]) ne 0 then $
;      for i=0,n_elements(evolution)-1 do $
;         rit_write,RIT_FILE,'RAT_EVOLUTION_'+strcompress(i,/R),evolution[i]

  if file_test(rit_file) then file_delete,/quiet,rit_file

;;; do nothing if no write rights!
  if ~file_test(file_dirname(rit_file),/write) then return

  if n_elements(evolution) ne 1 || strlen(evolution[0]) ne 0 then $
     rit_write,RIT_FILE,RAT_PAR=pars,RAT_EVO=evolution,RAT_PALETTE=reform(palettes[0,*,*]) $
  else rit_write,RIT_FILE,RAT_PAR=pars,RAT_PALETTE=reform(palettes[0,*,*])



;;; save rit_file and timestamp information
;;; if ~.k(UNDO_PREPARE) then
  
;;; palette  --  save palette information
;;; !always! - because also multi-channel images can have own palettes for
;;;            single channel view
;;;          - and the change from color- to b/w- palette should also be saved
;  palette_write,reform(palettes[0,*,*]),filename=rit_file

end
