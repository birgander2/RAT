;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_interf_pha
; written by    : Maxim Neumann
; last revision : 18.Oct.2004
; Extracts interferometric phase from complex interferograms or PolInSAR data
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

pro polin_interf_pha,CALLED=called
  common rat, types, file, wid, config

; check if array is complex

  if ~(file.type ge 500 && file.type le 514) $
  then begin
     error_button = DIALOG_MESSAGE('Wrong input data type', DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  newtype = 52L
;    if (file.type eq 500) or (file.type eq 501) then head = [3L,file.zdim,file.xdim,file.ydim,4L] $
;    else if type eq 510 || type eq 511 then head = [4l,file.vdim/2,file.zdim/2,file.xdim,file.ydim,4L] $
;    else 
  if type ge 500 && type le 514 then begin
     polin_get_info,pol=pol,tracks=n_tr,baseline=n_bl,matrix=is_matrix
     head=[4L,pol,n_bl,file.xdim,file.ydim,4L]
  endif else begin
     if file.dim eq 4 then head = [4l,file.vdim,file.zdim,file.xdim,file.ydim,4L]
     if file.dim eq 3 then head = [3l,file.zdim,file.xdim,file.ydim,4L]
     if file.dim eq 2 then head = [2l,file.xdim,file.ydim,4L]
  endelse
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Extracting interferometric phase...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]], $
                        type=file.var,/nozero)
     readu,ddd,block
; -------- THE FILTER ----------
;       if file.type ge 500 && file.type le 503 $
;       then writeu,eee,atan(block[0,*,*,*] * conj(block[1,*,*,*]),/phase) $
;       else if file.type ge 510 && file.type le 514 $
;       then writeu,eee,atan(block[file.vdim/2:file.vdim-1,file.vdim/2:file.vdim-1,*,*],/phase) $
;      else 
     if file.type ge 500 && file.type le 514 then begin
        oblock=fltarr(pol,n_bl,file.xdim,blocksizes[i],/nozero)
        if ~is_matrix then $
           for bl=0,n_bl-1 do begin
           tracks = mb_ind(bl)
           oblock[*,bl,*,*]=atan(block[*,tracks[0],*,*]*conj(block[*,tracks[1],*,*]),/phase)
        endfor else for bl=0,n_bl-1 do begin
           tracks = mb_ind(bl)
           oblock[*,bl,*,*]=atan(mm_diag(block[tracks[0]*pol:(tracks[0]+1)*pol-1,tracks[1]*pol:(tracks[1]+1)*pol-1,*,*]),/phase)
        endfor
        writeu,eee,oblock
     endif
;          block=reform(block,/overwrite,file.vdim*file.zdim,1,file.xdim,blocksizes[i])
;          writeu,eee,atan(mm_xprod(reform(block,file.vdim*file.zdim,file.xdim,blocksizes[i],/overwrite),/conj),/phase)
;       endif else if file.type ge 510 && file.type le 514 then writeu,eee,atan(block,/phase)
;      if file.type eq 301 or file.type eq 311 or file.type eq 53 or file.type eq 54 or file.type eq 55 then writeu,eee,atan(block,/phase)
; -------- THE FILTER ----------
  endfor
  free_lun,ddd,eee

; update file information

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = head[0]

  if file.type ge 500 && file.type le 514 then begin
     file.vdim = pol
     file.zdim = n_bl
  endif
  file.var  = 4l
  file.type = newtype
  evolute,'Interferometric Phase Extraction'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif

end
