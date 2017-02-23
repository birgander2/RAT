;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_coh_esprit
; written by       : Maxim Neumann & Stephane Guillaso
; last revision    : 10. January 2005
; PolInterferometric coherence phase optimization with ESPRIT
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

FUNCTION rat_esprit_single_phase,T6,M,d
; for rat modified function st_esprit_single_phase
  eval = la_eigenql(T6,eigenvectors=evec)
  evec = reverse(transpose(evec))
  evec = evec[0:d-1,*]
  F12 = [evec[*,0:M-1],evec[*,M:2*M-1]]
  F12H = adj(F12) ## (F12)
  eval = la_eigenql(F12H,eigenvectors=evec)
  evec = reverse(transpose(evec))
  E12 = evec(d:2*d-1,0:d-1)
  E22 = evec(d:2*d-1,d:2*d-1)
  if d ne 1 then BEGIN
     psi = - E12 ## LA_INVERT(E22)
     phi = CONJ(LA_EIGENPROBLEM(psi))
  endif else BEGIN
     phi = -E12 / E22
  ENDELSE
  RETURN,phi
END

FUNCTION rat_esprit_phases,T6=T6,M,d
; for rat modified function esprit_phases
  n = reverse((reverse(size(T6,/dim)))[0:1])
  IF ~keyword_set(M) THEN M = (size(T6))[1]/2
  IF ~keyword_set(d) THEN d = M-1
  esprit = complexarr(d,n[0],n[1])
  FOR i=0,n[0]-1 DO FOR j=0,n[1]-1 DO $
    esprit[*,i,j] = rat_esprit_single_phase(reform(t6[*,*,i,j]),M,d)
  return, esprit
END

function rat_cohpha_ESPRIT, C, d, COMPLEX=COMPLEX   ; d==number of scatterers
; for rat modified function cohpha_ESPRIT
;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;
  siz = size(C)
  M   = siz[1]/2                ; M==p==number of polarizations
;  if n_elements(d) ne 1 then d=M-1 else d=1>d<(M-1) ; d==number of scatterers
  if n_elements(d) ne 1 then d=M-1 else d=1>d<(2*M) ; d==number of scatterers
  if siz[0] eq 2 then n = lonarr(4)+1 $
  else if siz[0] ge 3 && siz[0] le 8 then n = [siz[3:siz[0]],lonarr(9-siz[0])+1] $
  else Message,'error in cohpha_ESPRIT'
  esprit = complexarr([d,n],/nozero)
;;;;;;;;;;;; COMPUTATION ;;;;;;;;;;;;;;;;
  for l=0,n[3]-1 do for k=0,n[2]-1 do for j=0,n[1]-1 do for i=0,n[0]-1 do begin
     R = C[*,*,i,j,k,l]        ; R==Rxx
     eval = la_eigenql(R,eigenvectors=evec)
     F12  = [[evec[0:M-1,2*M-d:*]],[evec[M:2*M-1,2*M-d:*]]]
     F12h = conj(transpose(F12)) # F12
     eval = la_eigenql(F12h,eigenvectors=evec)
     G    = evec[*,0:d-1] ; null space over F12
     if d eq 1 then esprit[0,i,j,k,l]= -G[0]/G[1] $
     else begin
        psi = -G[0:d-1,*] # la_invert(G[d:*,*])
        esprit[*,i,j,k,l] = la_eigenproblem(psi)
     endelse
  endfor
  esprit = reform(esprit,/overwrite)
  if keyword_set(COMPLEX) then return, esprit/abs(esprit) $
  else return, atan(esprit,/phase)
end

pro polin_coh_esprit,CALLED = called, SMMX = smmx, SMMY = smmy, ESPRIT_SCATTERERS=ESPRIT_SCATTERERS
   common rat, types, file, wid, config

; if necessary, then generate covariance or coherence matrix
   if file.type ge 500 && file.type le 509 then begin
      errormsg = DIALOG_MESSAGE(["The scattering vectors need to be transformed to covariance or coherence matrices!","Proceed automatically with vector-->matrix transformation?"],/question,DIALOG_PARENT = wid.base, TITLE='Proceed with matrix generation?')
      if errormsg eq 'Yes' then polin_k2m,/CALLED,SMMX=1,SMMY=1 $
      else return
   endif

   if ~(file.type ge 510 && file.type lt 519) then begin
      error = DIALOG_MESSAGE(["This is wrong data type. Needs PolInSAR ",$
                              "covariance or coherency matrix"], $
                             DIALOG_PARENT = wid.base, TITLE='Error',/error)
      return
   endif

   polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl, matrix=is_matrix
   if n_tr ne 2 then begin
      error = DIALOG_MESSAGE(["This is wrong data type. Needs single-baseline(!) PolInSAR ",$
                              "covariance or coherency matrix"], $
                             DIALOG_PARENT = wid.base, TITLE='Error',/error)
      return
   endif



   real_vdimh = file.vdim/2
   but_esprit_labels = ['1',(real_vdimh lt 4?'2':['2','3'])]

   if not keyword_set(called) then begin ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4, $
                         TITLE='Esprit Coherence Phase - Eigenvalue Decmoposition', $
                         /modal,/tlb_kill_request_events,/tlb_frame_attr)
      esprit_nr  = real_vdimh-1
      field1 = CW_FIELD(main,VALUE=7,/integer, $
                        TITLE='Filter boxsize X   : ',XSIZE=3)
      field2 = CW_FIELD(main,VALUE=7,/integer, $
                        TITLE='Filter boxsize Y   : ',XSIZE=3)
      but_esprit = CW_BGROUP(main,but_esprit_labels,/exclusive,/row, $
                             set_value=(esprit_nr-1), $
                             LABEL_LEFT='ESPRIT dominant scatterers:')
      but_compl  = CW_BGROUP(main,['Phase','Complex Value'],/exclusive,/row, $
                             set_value=0, $
                             LABEL_LEFT='Return:  ')
      buttons   = WIDGET_BASE(main,column=3,/frame)
      but_ok    = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
      but_canc  = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
      but_info  = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
      WIDGET_CONTROL, main,/REALIZE,default_button=but_canc,tlb_get_size=toto
      pos = center_box(toto[0],drawysize=toto[1])
      widget_control, main, xoffset=pos[0], yoffset=pos[1]

      repeat begin              ; Event loop
         event = widget_event(main)
         if event.id eq but_info then begin ; Info Button clicked
            infotext = ['ESPRIT COHERENCE PHASES',$
                        ' ',$
                        'RAT module written 12/2004 by Maxim Neumann and Stephane Guillaso']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                  TITLE='Information')
         end
      endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
      widget_control,field1,GET_VALUE=smmx ; read widget fields
      widget_control,field2,GET_VALUE=smmy
      widget_control,but_esprit,GET_VALUE=val
      esprit_nr = LONG(val+1)
      widget_control,but_compl,GET_VALUE=coh_complex
      widget_control,main,/destroy ; remove main widget
      if event.id ne but_ok then return ; OK button _not_ clicked
   endif else begin             ; Routine called with keywords
      if not keyword_set(smmx) then smmx = 1 ; Default values
      if not keyword_set(smmy) then smmy = 1
      if n_elements(ESPRIT_SCATTERERS) gt 0 && ESPRIT_SCATTERERS le real_vdimh then $
         esprit_nr = LONG(ESPRIT_SCATTERERS) $
      else esprit_nr = LONG(real_vdimh-1)
   endelse

; change mousepointer
   WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

   if coh_complex then begin
      newtype = 55L
      newvar  = 6L
   endif else begin
      newtype = 52L
      newvar  = 4L
   endelse
   newzdim = LONG(esprit_nr)

; read / write header
   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type
   head = [3L,newzdim,file.xdim,file.ydim,newvar]
   srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
   bs = config.blocksize/file.zdim
   overlap = smmy / 2
   calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last
   blocksizes = intarr(anz_blocks)+bs
   blocksizes[anz_blocks-1] = bs_last
   ypos1 = 0                    ; block start
   ypos2 = bs - overlap         ; block end

;smooth box
   smm_box  = [1,1,smmx,smmy]
   byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos

; pop up progress window
   progress,Message='ESPRIT coherence phases...',/cancel_button


;start block processing
   for i=0,anz_blocks-1 do begin ; loop normal blocks
      progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

      block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
                         ,type=file.var)
      readu,ddd,block
; -------- THE FILTER ----------
      block1 = smooth(block,smm_box,/edge_truncate)
      if i eq anz_blocks-1 then ypos2 = bs_last
      coh = rat_cohpha_esprit(block1[*,*,*,ypos1:ypos2-1],esprit_nr,COMPLEX=coh_complex)
;      coh = rat_esprit_phases(T=block1[*,*,*,ypos1:ypos2-1],real_vdimh,esprit_nr)
;      if ~coh_complex then coh = atan(coh,/phase)
; -------- THE FILTER ----------
      writeu,eee,coh
      ypos1 = overlap
      point_lun,-ddd,file_pos
      point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim $
         * file.xdim * byt[file.var]
   endfor
   free_lun,ddd,eee

; update file information
   file_move,outputfile,finalfile,/overwrite
   file.name = finalfile
   file.dim  = 3l
   file.vdim = 1L
   file.zdim = newzdim
   file.var  = newvar
   file.type = newtype

   evolute,'Extraction of ESPRIT phases as '+(keyword_set(coh_complex)?'complex':'real')+ $
           '. boxcar:'+strjoin(strcompress([smmx,smmy]))+' det.scatterers:'+strcompress(esprit_nr)

; generate preview
   if not keyword_set(called) then begin
      generate_preview
      update_info_box
   endif else progress,/destroy
end
