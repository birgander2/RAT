;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_c2t
; written by       : Andreas Reigber
; last modified by : Maxim Neumann
; last revision    : 12.Oct.2004
; Transforms between covariance and coherency matrices
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

pro polin_c2t,CALLED=called;, TO_PAULI=TO_PAULI, TO_LEX=TO_LEX
  common rat, types, file, wid, config

; check if array is usable

  if ~(file.type ge 510 && file.type le 513) then begin
     error_button = DIALOG_MESSAGE(['Data has to be a','[C] or a [T] matrix'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

  case file.type of 
     510L : newtype = 511L
     511L : newtype = 510L
     512L : newtype = 513L
     513L : newtype = 512L
     else:
  endcase
  polin_get_info, tracks=n_tr, baselines=n_bl, pol=pol, matrix=is_matrix
;  pol  = file.zdim mod 3 eq 0? 3L : 4L
;  n_tr = file.zdim / pol

  WIDGET_CONTROL,/hourglass
; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=[4l,file.zdim,file.zdim,file.xdim,file.ydim,file.var],info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize / file.zdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; calculating matrix D for the unitary transform between [C] and [T]
  j      = complex(0,1)
  D3     = 1./sqrt(2.) * [ $
           [ 1,  1,   0     ], $
           [ 1, -1,   0     ], $
           [ 0,  0, sqrt(2.)]]
  D4     = 1./sqrt(2.) * [ $
           [ 1, 1, 0, 0 ], $
           [ 1,-1, 0, 0 ], $
           [ 0, 0, 1, j ], $
           [ 0, 0, 1,-j ]]
  if pol eq 3 then D1p = D3 else D1p = D4
  D = complexarr(pol*n_tr,pol*n_tr)
  for i=0,n_tr-1 do D[i*pol:(i+1)*pol-1,i*pol:(i+1)*pol-1]=D1p
  if newtype gt file.type then begin ; from lex to pauli
     Di = conj(transpose(D))
     m1 = 'C' & m2 = 'T'
  endif else begin              ; from pauli to lex
     Di = D
     D  = conj(transpose(Di))
     m1 = 'T' & m2 = 'C'
  endelse

; pop up progress window
  progress,Message='['+m1+'] -> ['+m2+']...',/cancel_button

; calculating transform
  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var,/NOZERO)
     readu,ddd,block

     for k=0,file.xdim-1 do $
        for l=0,blocksizes[i]-1 do $
           block[*,*,k,l] = D # block[*,*,k,l] # Di
     writeu,eee,block
;     writeu,eee,mm_mm(mm_mm(D,block),Di) ;; quite slow! (08/2006)
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.type = newtype
  file.dim  = 4L
  file.vdim = file.zdim

  evolute,'Transform: '+m1+' -> '+m2

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy
end
