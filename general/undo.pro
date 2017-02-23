;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: undo
; last revision : 28. March 2004
; written by    : Andreas Reigber, Stephane Guillaso
; Undo last operation               
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



pro undo
  common rat, types, file, wid, config

  if config.undofile ne '' then begin

     config.redofile = file.name
     save_rit,filename=config.redofile,/UNDO_PREPARE
     widget_control,wid.button_redo,set_value=config.imagedir+'redo.bmp',/BITMAP


;      if config.undofile eq config.tempdir+config.workfile3 then begin
;         file_move,config.undofile,config.tempdir+config.workfile1,/overwrite
;         ritfile = strmid(config.undofile,0,strlen(config.undofile)-4)+'.rit'
;         if file_test(ritfile,/READ,/WRITE) then $
;            file_move,ritfile,strmid(config.tempdir+config.workfile1,0,strlen(config.tempdir+config.workfile1)-4)+'.rit',/overwrite
;         config.undofile = config.tempdir+config.workfile1 
;      endif

     if strcmp(file.window_name,file_basename(config.undofile)) $
     then tmp = file.window_name $
     else tmp = file.window_name+' (modified)'
     tmp_name = file.window_name

     open_rat,INPUTFILE=config.undofile,/FROM_UNDO
     widget_control,wid.base,base_set_title='RAT - Radar Tools: '+tmp
     file.window_name = tmp_name

     config.undofile = ''
     widget_control,wid.button_undo,set_value=config.imagedir+'undo2.bmp',/BITMAP
  endif

end
