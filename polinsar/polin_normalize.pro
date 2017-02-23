;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_normalize
; written by       : Maxim Neumann
; last revision    : 08/2006
; Modify the cov/coh matrices
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


pro polin_normalize,CALLED = called
  common rat, types, file, wid, config

  if ~(file.type ge 510 && file.type le 513) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

  polin_get_info, tracks=n_tr, baselines=n_bl, pol=pol, matrix=is_matrix
;   pol = file.vdim mod 3 eq 0? 3L: 4L
;   n_tr = file.vdim/pol
  newtype = 514L

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize/file.vdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Normalization of matrices...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                        ,type=file.var,/nozero)
     readu,ddd,block

; -------- THE FILTER ----------
     R = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     for tr=0,n_tr-1 do $
        R[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1,*,*]=mm_power(block[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1,*,*],/INV,/SQRT)
     block = mm_mm(R,block)
     block = mm_mm(block,R)
; -------- THE FILTER ----------

     writeu,eee, block
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.type = newtype

  evolute,'Matrix normalization.'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
