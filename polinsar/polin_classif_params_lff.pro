;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module:     polin_classif_params_lff
; written by    : Maxim Neumann
; last revision : 17.5.5
; Quader Classification of PolInSAR A1/A2 (LFF) parameters.
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



pro polin_classif_params_lff,CALLED=called
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag, palettes, pnames

; check if array is usable
  if (file.type ge 500 && file.type le 511) || file.type eq 530 || file.type eq 531 then begin
     error = DIALOG_MESSAGE(["This is wrong data type. ", $
                             "Needs LFF A1/A2 coherence parameters!", $
                             "Proceed automatically with LFF parameter generation?"], $
                            DIALOG_PARENT = wid.base, TITLE='Proceed with parameter generation?',/question)
     if error eq 'Yes' then polin_params_lff,/CALLED $
     else return
  endif

  if file.type ne 540 then begin
     error = DIALOG_MESSAGE(["This is wrong data type. ", $
                             "Needs LFF coherence parameters!"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif


; change mousepointer
  WIDGET_CONTROL,/hourglass
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED


  newtype = 451L
  newvar  = 2L
; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [2L,file.xdim,file.ydim,newvar]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='A1/A2 coherence parameter classification...',/cancel_button

; start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
;     oblock = make_array([1,1,file.xdim,blocksizes[i]],type=newvar)
     readu,ddd,block
; -------- THE FILTER ----------
     a1 = (block[0,0,*,*]gt 0.05 and block[0,0,*,*]le 0.25) + (block[0,0,*,*]gt 0.25)*2
     a2 = (block[1,0,*,*]gt 0.2  and block[1,0,*,*]le 0.5 ) + (block[1,0,*,*]gt 0.5 )*2
     oblock = a1 + 3*a2
; -------- THE FILTER ----------
     writeu,eee,fix(oblock)
  endfor
  free_lun,ddd,eee
; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = 2L
  file.vdim = 1L
  file.zdim = 1L
  file.var  = newvar
  file.type = newtype

; set palette
  palettes[0,*,*] = palettes[5,*,*] ; palettes 5 = 10 classes
  palettes[1,*,*] = palettes[5,*,*] ; to actual and suggestion

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy

end
