;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_deco_FreeDur
; written by	: Maxim Neumann
; last revision	: 09/2006
; Single-Track Freeman-Durden decomposition
; [Double-Bounce, Volume, Surface]
; method=0 : standard 
; method=1 : adjust the power for bad points (more correct) +more DB
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

function polin_deco_freedur_func, C, METHOD=METHOD
  if n_elements(method) eq 0 then method=0
  eps = (MACHAR()).EPS & eps=0.
  siz = size(C) & dims=siz[0]gt 2? siz[3:siz[0]]: 1
  res = fltarr((siz[0]gt 2?[3,dims]:3),/nozero)
  fv  = double(3./2.*C[2,2,*,*]) & fs=fv & fd=fv ; volume

  shh  = (double(C[0,0,*,*])-fv) >0
  svv  = (double(C[1,1,*,*])-fv) >0
  shhvv= (C[1,0,*,*]-fv/3.)

  bad  = where(abs(shhvv)^2 gt shh*svv, bad_nr) ; preconditioning (shhvv bigger than possible)
  if bad_nr gt 0 then $
     shhvv[bad] *= (sqrt(abs(shh[bad]*svv[bad]))/abs(shhvv[bad]))
  rshhvv  = double(shhvv)
  beta	  = dblarr(dims)+1.
  alpha   = dcomplex(dblarr(dims)-1.)

  pos  = where(rshhvv ge 0,pos_nr,compl=neg,ncompl=neg_nr)
  if pos_nr gt 0 then begin     ; alpha = -1.  ; surface
     beta[pos] = double((shh[pos]+shhvv[pos])/(svv[pos]+shhvv[pos])) > eps
     fs[pos] = ((svv[pos]+rshhvv[pos])/(1.+beta[pos])) > eps
     fd[pos] = (svv[pos]-fs[pos]) > eps
  endif
  if neg_nr gt 0 then begin     ; beta  = +1. ; double bounce
     fs[neg] = ((shh[neg]*svv[neg]-abs(shhvv[neg])^2)/(shh[neg]+svv[neg]-2.*rshhvv[neg])) > eps
     fd[neg] = (svv[neg]-fs[neg]) > eps
     alpha[neg]= dcomplex((rshhvv[neg]-fs[neg])/fd[neg],imaginary(shhvv[neg])/fd[neg])
  endif

  if method eq 1 then begin
     P   = mm_trace(C)
     bad = [where(rshhvv le eps),where(shh le eps),where(svv le eps)] & bad=bad[sort(bad)] & bad=bad[uniq(bad)]
     if ~(n_elements(bad) eq 1 && bad[0] eq -1) then begin
        if bad[0] eq -1 then bad=bad[1:*]
        alpha[bad]=0 & beta[bad]=0 & fd[bad]=0 & fs[bad]=0
        pos  = where(rshhvv[bad] gt 0,pos_nr,compl=neg,ncompl=neg_nr)
        if pos_nr gt 0 then fs[bad[pos]]= double(P[bad[pos]]-8./3.*fv[bad[pos]])>0
        if neg_nr gt 0 then fd[bad[neg]]= double(P[bad[neg]]-8./3.*fv[bad[neg]])>0
     endif
  end

  res[1,*,*] = float(8./3.*fv)  ; volume
  res[0,*,*] = float((1.+abs(alpha)^2)*fd) ; double bounce
  res[2,*,*] = float((1+abs(beta)^2)*fs) ; surface

  index = where(finite(res) eq 0,nr) ; eliminate unvalid (nan) pixels
  if nr gt 0 then res[index] = 0.0

  return, res
end


; docformat = 'rst'
;+
; Polarimetric Freeman-Durden for multibaseline data
; [Double-Bounce, Volume, Surface]
;
; :Keywords:
;    called: in, optional, type="flag"
;       call routine without GUI in batchmode
;    method: in, optional, type=int
;       0: standard, 1: adjust the power for bad points (more correct) +more DB
;    
; :Params:
;
; :Author: Maxim Neumann
; 
; :Categories: PolInSAR
;
; :Copyright:
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
;-
pro polin_deco_freedur,CALLED = called, METHOD=METHOD
  common rat, types, file, wid, config
  compile_opt idl2

  if file.type eq 220 then begin
     pol = file.zdim
     n_tr = 1 & n_bl = 0 & matrix = 1
  endif else begin
     if ~(file.type ge 500 && file.type le 513) then begin
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
     if file.type ne 510 && 'OK' eq $
        dialog_message(["Performing necessary preprocessing step:","Tranformation to Lexicographic HV basis"],/cancel, DIALOG_PARENT = wid.base, TITLE='Information') $
     then polin_basis,0,/LEX,/called
     if file.type ne 510 then $
        return
  endelse

  newtype = 211L
  WIDGET_CONTROL,/hourglass

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [4L,3,n_tr,head[3:4],4L]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize / file.vdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Freeman-Durden decomposition...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                        ,type=file.var,/nozero)
     oblock= make_array([3,n_tr,file.xdim,blocksizes[i]],type=4L,/nozero)
     readu,ddd,block

; -------- THE FILTER ----------
     for tr=0,n_tr-1 do $
        oblock[*,tr,*,*] = polin_deco_freedur_func(block[pol*tr:pol*(tr+1)-1,pol*tr:pol*(tr+1)-1,*,*],METHOD=method)
; -------- THE FILTER ----------
     writeu,eee, oblock
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.zdim = n_tr
  file.vdim = 3L
  file.var  = 4L
  file.type = newtype

  evolute,'Single-track Freeman-Durden decomposition.'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
