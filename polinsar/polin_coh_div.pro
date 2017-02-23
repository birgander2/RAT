;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_coh_div
; written by       : Maxim Neumann
; last revision    : Nov.2005
; PolInterferometric coherence phase diversity
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

function rat_cohpha_DIV, T6, COMPLEX=COMPLEX   ; d==number of scatterers
;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;
  siz = size(T6)
  M   = siz[1]/2                ; M==p==number of polarizations
  if siz[0] eq 2 then n = lonarr(4)+1 $
  else if siz[0] ge 3 && siz[0] le 8 then n = [siz[3:siz[0]],lonarr(9-siz[0])+1] $
  else Message,'error in cohpha_DIV'
  cd = fltarr([M,n],/nozero)
  i=0 & j=0 & k=0 & l=0
;;;;;;;;;;;; COMPUTATION ;;;;;;;;;;;;;;;;
  for l=0,n[3]-1 do for k=0,n[2]-1 do for j=0,n[1]-1 do for i=0,n[0]-1 do begin
;     R = C[*,*,i,j,k,l]         ; R==Rxx
;     T12   = T6[0:M-1,M:*,i,j,k,l] ; R==Rxx
     T12   = T6[M:*,0:M-1,i,j,k,l] ; R==Rxx
     Phi   = !pi/8. - atan(trace(T12),/phase)
;     phi   = 0
     T12m  = T12 * complex(cos(Phi),sin(Phi))
     A     =  T12m + adj(T12m)
     B     = (T12m - adj(T12m)) * complex(0,-1)
     eval  = la_eigenql(A,B,STATUS=status)
     cd[*,i,j,k,l] = atan(1./eval)-phi
;stop
  endfor
  cd = reform(cd,/overwrite)
  if keyword_set(COMPLEX) then return, complex(cos(cd),sin(cd)) $
  else return, cd
end


pro polin_coh_div,CALLED = called, SMMX = smmx, SMMY = smmy
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


   if not keyword_set(called) then begin ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4, $
                         TITLE='Coherence Phase Diversity', $
                         /modal,/tlb_kill_request_events,/tlb_frame_attr)
      field1 = CW_FIELD(main,VALUE=7,/integer, $
                        TITLE='Filter boxsize X   : ',XSIZE=3)
      field2 = CW_FIELD(main,VALUE=7,/integer, $
                        TITLE='Filter boxsize Y   : ',XSIZE=3)
      but_compl  = CW_BGROUP(main,['Phase as float','Complex value'],/exclusive,/row, $
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
            infotext = ['COHERENCE PHASE DIVERSITY',$
                        ' ',$
                        'RAT module written 11/2005 by Maxim Neumann']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                  TITLE='Information')
         end
      endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
      widget_control,field1,GET_VALUE=smmx ; read widget fields
      widget_control,field2,GET_VALUE=smmy
      widget_control,but_compl,GET_VALUE=coh_complex
      widget_control,main,/destroy ; remove main widget
      if event.id ne but_ok then return ; OK button _not_ clicked
   endif else begin             ; Routine called with keywords
      if not keyword_set(smmx) then smmx = 1 ; Default values
      if not keyword_set(smmy) then smmy = 1
      coh_complex = 0
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

; read / write header
   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type
   head = [3L,file.zdim/2,file.xdim,file.ydim,newvar]
   srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
   bs = config.blocksize
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
   progress,Message='Coherence phase diversity...',/cancel_button


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
      coh = rat_cohpha_div(block1[*,*,*,ypos1:ypos2-1],complex=coh_complex)
;       coh = rat_cohpha_esprit(block1[*,*,*,ypos1:ypos2-1],esprit_nr,COMPLEX=coh_complex)
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
   file.zdim /= 2
   file.var  = newvar
   file.type = newtype

   evolute,'Coherence phase diversity. Return: '+(keyword_set(coh_complex)?'complex':'real')+ $
           '. Boxcar:'+strjoin(strcompress([smmx,smmy]))

; generate preview
   if not keyword_set(called) then begin
      generate_preview
      update_info_box
   endif else progress,/destroy
end
