;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_deco_SDH
; written by	: Maxim Neumann
; last revision	: 09/2006
; Single-Track Sphere-Diplane-Helix decomposition
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


pro polin_deco_SDH,CALLED = called
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 513) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,matrix=matrix
  get_polar_basis,CIRC=CIRC,ERROR=error
  if error then begin
     error = DIALOG_MESSAGE("The polarimetric basis type of the data type could not be determined. Return!", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  if ~circ then begin
     if 'OK' eq dialog_message(["Performing necessary preprocessing step:","Tranformation to Circular Basis"],/cancel, $
                                DIALOG_PARENT = wid.base, TITLE='Information') $
     then polin_basis,1,/LEX,/called $
     else return
  endif

  newtype = 213L
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [4L,3,n_tr,head[3:*]]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize / file.vdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Sphere-Diplane-Helix Decomposition...',/cancel_button

  ind = lindgen(n_tr)*pol

  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var,/nozero)
     oblock = make_array([3l,n_tr,file.xdim,blocksizes[i]],type=4l)
     readu,ddd,block

     if matrix then begin
        block		 =  abs(mm_diag(block,/overwrite))
        oblock[0,*,*,*]  =  sqrt((block[ind+2,*,*])) ; sphere = RL
        oblock[1,*,*,*]  =  sqrt((block[ind,*,*]))<sqrt((block[ind+1,*,*])) ; diplane = min(|LL|,|RR|)
        oblock[2,*,*,*]  =  abs(sqrt((block[ind,*,*])) - sqrt((block[ind+1,*,*]))) ; helix = ||RR| - |LL||
     endif else begin
        oblock[0,*,*,*]  =  abs(block[2,*,*,*]) ; sphere = RL
        oblock[1,*,*,*]  =  min(abs(block[[0,1],*,*,*]),dim=1) ; diplane = min(|LL|,|RR|)
        oblock[2,*,*,*]  =  abs(abs(block[0,*,*,*]) - abs(block[1,*,*,*])) ; helix = ||RR| - |LL||
     endelse

     writeu,eee,oblock
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.type = newtype
  file.zdim = n_tr
  file.vdim = 3
  file.var  = 4l

  evolute,'Polarimetric Sphere-Diplane-Helix Decomposition.'
  
; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif	
end
