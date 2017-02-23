;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_cor
; written by       : Maxim Neumann
; last revision    : 08/2006
; Multibaseline (kind of) correlation calculation
; /TOTAL    correlation over baselines and polarizations
; (default) correlation over baselines for every polarization
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

pro polin_cor,CALLED = called, TOTAL=TOTAL, SMMX = smmx, SMMY = smmy
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 514) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,matrix=matrix

  if file.type ge 500 && file.type le 509 then begin
     polin_k2m,/called,/gui,smmx=sx,smmy=sy ;; reform to matrix
     if sx*sy le pol then $
        speck_polmean,/called,/gui
  endif
  newtype = 56L
  newvar  = 4L
  plist   = indgen(n_tr)*pol

  if file.type ne 514 then $
     polin_normalize,/called
  if file.type ne 514 then return


; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  if keyword_set(TOTAL) then $
     head = [2L,file.xdim,file.ydim,newvar] $
  else head = [3L,pol,file.xdim,file.ydim,newvar]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Multibaseline correlation coefficient...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                        ,type=file.var,/nozero)
     readu,ddd,block

; -------- THE FILTER ----------
     if keyword_set(TOTAL) then begin
        for y=0,blocksizes[i]-1 do for x=0,file.xdim-1 do $
           block[0,0,x,y]=la_eigenql(block[*,*,x,y],status=ignore)
        block=1-product(real_part(block[*,0,*,*]),1)^(1./(pol*n_tr))
     endif else begin
        for p=0,pol-1 do for y=0,blocksizes[i]-1 do for x=0,file.xdim-1 do $
           block[p,0,x,y]=product(la_eigenql(block[plist+p,plist+p,x,y],status=ignore))
        block=1-real_part(block[0:p-1,0,*,*])^(1./n_tr)
     endelse
     block = float(block)
; -------- THE FILTER ----------
     writeu,eee, block
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = head[0]
  file.zdim = keyword_set(total)? 1L: pol
  file.vdim = 1L
  file.var  = newvar
  file.type = newtype

  evolute,'Multibaseline correlation estimation.'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
