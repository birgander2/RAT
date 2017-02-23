;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_wiz_2haawish
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
pro polin_wiz_2haawish, called=called
  common rat, types, file, wid, config

  if ~(file.type ge 500L && file.type le 513L) then begin
     error = DIALOG_MESSAGE(["This is wrong data type. Needs single-baseline PolInSAR ",$
                             "scattering vectors or covariance or coherency matrices"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

  polin_get_info, tracks=n_tr, baselines=n_bl, pol=pol, matrix=is_matrix
  if n_tr gt 2 then begin
     error = DIALOG_MESSAGE(["This is wrong data type. Needs single-baseline(!) PolInSAR ",$
                             "scattering vectors or covariance or coherency matrices"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

; undo function
  wid.cancel = 0l
  undo_prepare,outputfile,finalfile,CALLED=CALLED

  wiz_speckle,/called
  if wid.cancel eq 1 then return

  if file.type eq 510L || file.type eq 512L then begin
     file_copy,file.name,config.tempdir+'tmp_C6.rat',/overwrite
     polin_c2t,/called
     file_copy,file.name,config.tempdir+'tmp_T6.rat',/overwrite
  endif else begin
     file_copy,file.name,config.tempdir+'tmp_T6.rat',/overwrite 
     polin_c2t,/called
     file_copy,file.name,config.tempdir+'tmp_C6.rat',/overwrite
     file_copy,config.tempdir+'tmp_T6.rat',file.name,/overwrite
     file.type++
  endelse
  if wid.cancel eq 1 then return
  t6zdim = file.zdim & t6vdim=file.vdim & t6type=file.type

  polin_extract_polsar,/called,channel=0
  if wid.cancel eq 1 then return
  decomp_entalpani,/called
  if wid.cancel eq 1 then return
  classif_eaa,/called
  if wid.cancel eq 1 then return

  file_copy,file.name,config.tempdir+'tmp_haa.rat',/overwrite
;  file_move,config.tempdir+'tmp_T6.rat',file.name,/overwrite
;  file.zdim=t6zdim & file.vdim=t6vdim & file.type=t6type

;  polin_classif_wishart,initfile=config.tempdir+'tmp_haa.rat',/called
  classif_wishart,TFile=config.tempdir+'tmp_T6.rat',InitFile=config.tempdir+'tmp_haa.rat',/called
  if wid.cancel eq 1 then return

  pclass_fredur,covarfile=config.tempdir+'tmp_C6.rat',/called
  if wid.cancel eq 1 then return

; remove temporary files
  file_delete,config.tempdir+'tmp_T6.rat',/quiet
  file_delete,config.tempdir+'tmp_C6.rat',/quiet
  file_delete,config.tempdir+'tmp_haa.rat',/quiet

  evolute,'POLINSAR k-means Wishart classification with HAA-initialization'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy

end
