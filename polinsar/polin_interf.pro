;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_interf
; written by       : Maxim Neumann
; last revision    : 15.Oct.2004
; Generate interferogram from PolInSar vector
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

pro polin_interf,CALLED=called
  common rat, types, file, wid, config

; right type? --- calculate new type
 
  if file.type lt 500 || file.type gt 514 then begin
     error_button = DIALOG_MESSAGE(['Data have to be a PolInSAR vector'], $
                    DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  newtype = 52L
  polin_get_info,pol=pol,tracks=n_tr,baseline=n_bl,matrix=is_matrix
  head=[4L,pol,n_bl,file.xdim,file.ydim,6L]
  srat,outputfile,eee,header=head,info=info,type=newtype
  
; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last
  
; pop up progress window
  progress,Message='Calculating PolInSAR interferograms...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]], $
                        type=file.var,/nozero)
     readu,ddd,block
; -------- THE FILTER ----------
     oblock=complexarr(pol,n_bl,file.xdim,blocksizes[i],/nozero)
     if ~is_matrix then for bl=0,n_bl-1 do begin
        tracks = mb_ind(bl)
        oblock[*,bl,*,*]=block[*,tracks[0],*,*]*conj(block[*,tracks[1],*,*])
     endfor else for bl=0,n_bl-1 do begin
        tracks = mb_ind(bl)
        oblock[*,bl,*,*]=mm_diag(block[tracks[0]*pol:(tracks[0]+1)*pol-1,tracks[1]*pol:(tracks[1]+1)*pol-1,*,*])
     endfor
;     writeu,eee,block[0,*,*,*] * conj(block[1,*,*,*])
; -------- THE FILTER ----------
        writeu,eee,oblock
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite

  file.name = finalfile
  file.dim  = 4l
  file.vdim = pol
  file.zdim = n_bl
  file.type = newtype
  file.var  = 6L

  evolute,'Complex Interferogram Extraction'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
     error_button = DIALOG_MESSAGE(['This preview shows only the amplitude' + $
                                    ' of the complex interferograms!'], $
                                   DIALOG_PARENT = wid.base, TITLE='Message')
  endif
end
