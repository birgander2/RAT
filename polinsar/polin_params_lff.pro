;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_params_lff
; written by       : Maxim Neumann
; last revision    : 5.5.5
; PolIn- coherence parameters from Laurent Ferro-Famil
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



pro polin_params_lff,CALLED = called
  common rat, types, file, wid, config

  if file.type eq 510 || file.type eq 511 || file.type eq 500 $
     || file.type eq 501 then begin
     error = DIALOG_MESSAGE(["This is wrong data type. ", $
                             "Needs optimized PolInSAR coherences!"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  
  if file.type ne 532 then begin
     error = DIALOG_MESSAGE(["This is wrong data type. ", $
                             "Needs PolInSAR optimized coherences!"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED
  
  n_bl = file.zdim
;  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl, matrix=is_matrix
  newtype = 540L
  newvar  = 4L
  pars    = 4L ; 4 parameters

; read / write header
;  head = 1l
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [4L,pars,n_bl,file.xdim,file.ydim,newvar]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Coherence parameters calculation...',/cancel_button
  alog3 = alog(3)

;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                         ,type=file.var)
     oblock = make_array([pars,n_bl,file.xdim,blocksizes[i]] $
                         ,type=newvar)
     readu,ddd,block
; -------- THE FILTER ----------
     block = abs(block)
     oblock[0,*,*,*] = (block[0,*,*,*]-block[1,*,*,*])/block[0,*,*,*]
     oblock[1,*,*,*] = (block[0,*,*,*]-block[2,*,*,*])/(block[0,*,*,*]+block[2,*,*,*])
     oblock[3,*,*,*] = (block[1,*,*,*]-block[2,*,*,*])/(block[1,*,*,*]+block[2,*,*,*])
     blocksum = reform(total(block,1))
     block[0,*,*,*] /= blocksum ; A1
     block[1,*,*,*] /= blocksum ; A2
     block[2,*,*,*] /= blocksum ; Aint
     oblock[2,*,*,*] = -total(block*alog(block),1)/alog3 ; Hint
; -------- THE FILTER ----------
     writeu,eee,oblock
  endfor
  free_lun,ddd,eee
; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = 4L
  file.vdim = pars
  file.zdim = n_bl
  file.var  = newvar
  file.type = newtype

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy
end
