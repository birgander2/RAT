;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_calib_natural
; written by	: Maxim Neumann
; last revision	: 09/2006
; Brute-force polarimetric calibration for natural media.
; /REFLECTION_SYM
;  - set <HH HV> = 0  (reflection symmetry assumption and enforcement)
;  - set <HV VV> = 0  (reflection symmetry assumption)
; /POL_STATIONARITY
;  - set all Tii to be equal (WPS)
; /VOL_CALIB
;  - adjust backscatter power for natural media after Freeman!
;    (i.e. |HH|²>=3|HV|² && |VV|²>=3|HV|² ) (perhaps this is a bit too strong?)
;    (i.e. C00 >= 3/2C22 && C11 >= 3/2C22 && C01 >= 1/2C22 )
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


pro polin_calib_natural,CALLED = called, REFLECTION_SYM=REFLECTION_SYM, VOL_CALIB=VOL_CALIB, $
                        POL_STATIONARITY=POL_STATIONARITY
  common rat, types, file, wid, config

  if ~keyword_set(REFLECTION_SYM) && ~keyword_set(POL_STATIONARITY) && ~keyword_set(VOL_CALIB) then return

  if file.type ne 510 && file.type ne 511 then begin
     ignore=dialog_message('Needs coherency or covariance matrix!',/error,dialog_parent=wid.base,title='Error')
     return
  endif

  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl

  if pol ne 3 then begin
     ignore=dialog_message('Works only for reciprocal data!',/error,dialog_parent=wid.base,title='Error')
     return
  endif


; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=head,info=info,type=type

; calculating preview size and number of blocks
  bs = config.blocksize / file.vdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Enforcing media assumptions...',/cancel_button

  ind = lindgen(n_tr)*pol
  D   = 1./sqrt(2.) * [ $
        [ 1,  1,   0     ], $
        [ 1, -1,   0     ], $
        [ 0,  0, sqrt(2.)]]
;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var,/nozero)
     readu,ddd,block

; -------- THE FILTER ----------
     if keyword_set(REFLECTION_SYM) $
     then for tr=0,n_tr-1 do begin
        block[tr*pol+2,[ind,ind+1],*,*]=complex(0,0)
        block[[ind,ind+1],tr*pol+2,*,*]=complex(0,0)
     endfor
     if keyword_set(POL_STATIONARITY) then begin
        Tmean = block[0:pol-1, 0:pol-1, *, *]
        for tr=1,n_tr-1 do $
           Tmean += block[tr*pol:tr*pol+2, tr*pol:tr*pol+2, *,*]
        Tmean /= n_tr
        for tr=0,n_tr-1 do $
           block[tr*pol:tr*pol+2, tr*pol:tr*pol+2, *,*] = Tmean
     endif
     if keyword_set(VOL_CALIB) then begin
        for tr=0,n_tr-1 do begin
           if file.type eq 510 then $ ; lex
              max2HV = (2./3.*abs(block[tr*pol,tr*pol,*,*])) < (2./3.*abs(block[tr*pol+1,tr*pol+1,*,*])) $ ;< (2.*abs(block[tr*pol,tr*pol+1,*,*])) $
           else $
              max2HV = (1./3.*abs(block[tr*pol,tr*pol,*,*]+block[tr*pol+1,tr*pol+1,*,*]+block[tr*pol,tr*pol+1,*,*]+block[tr*pol+1,tr*pol,*,*])) < $
                       (1./3.*abs(block[tr*pol,tr*pol,*,*]+block[tr*pol+1,tr*pol+1,*,*]-block[tr*pol,tr*pol+1,*,*]-block[tr*pol+1,tr*pol,*,*])) ; < $
;                    (      abs(block[tr*pol,tr*pol,*,*]-block[tr*pol+1,tr*pol+1,*,*]+block[tr*pol,tr*pol+1,*,*]-block[tr*pol+1,tr*pol,*,*]))

;         else begin $            ; pauli
;            Cblock = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var,/nozero)
;            for k=0,file.xdim-1 do $
;               for l=0,blocksizes[i]-1 do $
;                  Cblock[*,*,k,l] = D # block[*,*,k,l] # D
;            max2HV = (2./3.*abs(cblock[tr*pol,tr*pol,*,*])) < (2./3.*abs(cblock[tr*pol+1,tr*pol+1,*,*])) < (2.*abs(cblock[tr*pol,tr*pol+1,*,*]))
;         endelse
        endfor
        coeff  = complex(sqrt(max2HV / abs(block[tr*pol+2,tr*pol+2,*,*])) < 1.)
        for j=0,n_tr*pol-1 do begin
           block[tr*pol+2,j,*,*] *= coeff
           block[j,tr*pol+2,*,*] *= coeff
        endfor
     endif
; -------- THE FILTER ----------
     writeu,eee, block
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile

  if keyword_set(REFLECTION_SYM) then $
     evolute,'Enforcing media assumptions: "Brute-force" reflection symmetry assumption and enforcement.'
  if keyword_set(POL_STATIONARITY) then $
     evolute,'Enforcing media assumptions: "Brute-force" polarimetric stationarity assumption and enforcement.'
  if keyword_set(VOL_CALIB) then $
     evolute,'Enforcing media assumptions: "Brute-force" volume backscatter power adjustment (Freeman model).'

; generate preview
  if ~keyword_set(called) && keyword_set(VOL_CALIB) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy
end
