;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: m_to_k
; written by       : Maxim Neumann
; last revision    : 02/2007
; Transforms covariance matrices into its polarimtric vectors
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

pro m_to_k,CALLED=called
  common rat, types, file, wid, config

; check if array is usable

  if ~((file.type ge 220 && file.type le 221) || (file.type ge 510 && file.type le 513)) then begin
     error_button = DIALOG_MESSAGE(['Data have to be in','matrix format.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif
;  if not ((file.type ge 200 and file.type lt 211) or (file.type ge 500 and file.type lt 510)) then begin
;     error_button = DIALOG_MESSAGE(['Data have to be in','vector format.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
;     return
;  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type

  
; calculating new dimensions
  if file.type eq 220 then newtype = 208l
  if file.type eq 221 then newtype = 210l
  if file.type ge 220 and file.type le 221 then begin
     vdim = 1L
     zdim = file.zdim
     dim  = 3L
     srat,outputfile,eee,header=[dim,zdim,file.xdim,file.ydim,file.var],info=info,type=newtype
  endif
;   if file.type ge 510 and file.type lt 511 then begin
;      newtype = file.type - 10L
;      vdim = 2L
;      zdim = file.zdim / 2L
;      dim  = 4L
;      srat,outputfile,eee,header=[dim,vdim,zdim,file.xdim,file.ydim,file.var],info=info,type=newtype
;   endif
  if file.type ge 510 and file.type le 513 then begin
     newtype = file.type - 10L
     dim  = 4L
     vdim = file.vdim mod 3 eq 0? 3L : 4L
     zdim = file.zdim/vdim
     srat,outputfile,eee,header=[dim,vdim,zdim,file.xdim,file.ydim,file.var],info=info,type=newtype
  endif

; pop up progress window
  progress,Message='Matrix -> Vector...',/cancel_button

; calculating span
  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var,/nozero)
     readu,ddd,block

; filter
     amp = sqrt(block_diag(block))
     pha = -atan(block[*,0,*,*],/phase) & block=-1
     oblock = amp * complex(cos(pha),sin(pha)) & amp=-1 & pha=-1

;      if file.type ge 510 and file.type lt 513 then begin
;         oblock = reform(oblock,zdim,vdim,file.xdim,blocksizes[i],/OVERWRITE)
;         oblock = transpose(oblock,[1,0,2,3])
;      endif
     if file.type ge 510 and file.type le 513 then $
        oblock = reform(oblock,vdim,zdim,file.xdim,blocksizes[i],/OVERWRITE)

     writeu,eee,oblock
  endfor
  free_lun,ddd,eee

; update file information

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.type = newtype
  file.dim  = dim
  file.vdim = vdim
  file.zdim = zdim
  evolute,'Transform Matrix to Vector. (no reconstruction of the absolute phase!)'
; generate preview

  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
