;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_deco_abgd
; written by	: Maxim Neumann
; last revision	: 09/2006
; Single-Track Alpha, Beta, Gamma, Delta angles
; result: [ [Alpha[0:2],Beta[0:2],Gamma[0:2],Delta[0:2]], track, x, y]
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


pro polin_deco_abgd, CALLED = called,MEAN=mean
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 513) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl
  if n_elements(method) eq 0 then method=0

  if file.type ge 500 && file.type le 509 then begin
     polin_k2m,/called,/gui,smmx=sx,smmy=sy ;; reform to matrix
     if sx*sy le pol then $
        speck_polmean,/called,/gui
  endif

  if file.type ne 511 && 'Yes' eq $
     dialog_message(["Performing necessary preprocessing step:","Tranformation to Pauli representation"],/cancel, DIALOG_PARENT = wid.base, TITLE='Information') $
  then polin_basis,0,/PAULI
  if file.type ne 511 then $
     return

  newtype = 234L

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [4L,keyword_set(mean)?4:3*4,n_tr,head[3:4],4L]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize / file.vdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Alpha, Beta, Gamma, Delta ...',/cancel_button

  index=[2,1,0]
  eps = (MACHAR()).EPS
;start block processing
  for i=0,anz_blocks-1 do begin ; loop normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                        ,type=file.var,/nozero)
     oblock= make_array([3*5,n_tr,file.xdim,blocksizes[i]],type=4L,/nozero)
     readu,ddd,block

; -------- THE FILTER ----------
     for y=0,blocksizes[i]-1 do $
        for x=0,file.xdim-1 do $
           for tr=0,n_tr-1 do begin
        eval = la_eigenql(block[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1,x,y],eigenvectors=evec)
        oblock[0:2, tr,x,y] = acos(abs(evec[0,index]))
        oblock[3:5, tr,x,y] = acos((abs(evec[1,index])/(sin(oblock[0:2,tr,x,y]))))
        oblock[6:8, tr,x,y] = atan(evec[2,index],/phase)
        oblock[9:11,tr,x,y] = atan(evec[1,index],/phase)
        if keyword_set(mean) then $
           oblock[12:14,tr,x,y] = real_part(eval)/total(real_part(eval))
     endfor
     oblock[3:5,*,*,*] >= 0.
     if keyword_set(mean) then $
        for e=0,3 do begin
        oblock[e*3:e*3+2,*,*,*] *= oblock[12:14,*,*,*]
        oblock[e,*,*,*] = oblock[e*3,*,*,*]+oblock[e*3+1,*,*,*]+oblock[e*3+2,*,*,*]
     endfor
     writeu,eee,keyword_set(mean)?oblock[0:3,*,*,*]:oblock[0:11,*,*,*]
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.zdim = n_tr
  file.vdim = keyword_set(mean)?4:3*4
  file.type = newtype
  file.var  = 4L

  evolute,'Alpha, Beta, Gamma, Delta.'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
