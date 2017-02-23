;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: redo
; written by    : Maxim Neumann
; last revision : 18.Oct.2005
; Redo last operation
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



pro redo
  common rat, types, file, wid, config

  if config.redofile ne '' then begin

     config.undofile = file.name
     widget_control,wid.button_undo,set_value=config.imagedir+'undo.bmp',/BITMAP

     if strcmp(file.window_name,file_basename(config.redofile)) $
     then tmp = file.window_name $
     else tmp = file.window_name+' (modified)'

     open_rat,INPUTFILE=config.redofile,/FROM_UNDO
     widget_control,wid.base,base_set_title='RAT - Radar Tools: '+tmp

     config.redofile = ''
     widget_control,wid.button_redo,set_value=config.imagedir+'redo2.bmp',/BITMAP
  endif


end
