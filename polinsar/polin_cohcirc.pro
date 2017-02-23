;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_cohcirc
; written by    : Maxim Neumann (TUB)
; last revision : 10. January 2005
; Draws a complex coherence circle for the picked point and shows
; different properties
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

; function coh_shape, CovarianceMatrix, BOUNDARY_POINTS=bp, FILTERED_POWER = fpow, NOSORT=nosort
;   if n_elements(bp)  eq 0 then bp  = 181
;   C        = reform(CovarianceMatrix)
;   Cdim     = (size(C))[1]
;   C11 = C[      0:Cdim/2-1,       0:Cdim/2-1, *,*,* ]
;   C22 = C[ Cdim/2:Cdim-1,    Cdim/2:Cdim-1,   *,*,* ]
;   C12 = C[ Cdim/2:Cdim-1,         0:Cdim/2-1, *,*,* ]
;   boundary = complexarr(bp*2)
;   dphi     = 2.*!pi/float(bp)
;   polnr    = Cdim/2
;   T        = ( C11 + C22 ) /2.
;   O12      = C12

;   if keyword_set(fpow) then begin
;      mju = float(fpow) / 100. /float(polnr)
;      T   = T   + diag_matrix(mju*trace(T)   + complexarr(polnr))
;      O12 = O12 + diag_matrix(mju*(trace(O12)) + complexarr(polnr))
;   endif

;   for i = 0,bp-1 do begin
;      phi  = float(i)*dphi & e_phi = complex(cos(phi),sin(phi))
;      Ophi = e_phi*O12
;      A    = (Ophi + transpose(conj(Ophi))) / 2.
;      eval = real_part(la_eigenproblem(A,T,E=w,status=status)) ; if T is positive definite, then use better la_eigenql!

;      if status then begin
;         print, 'ERROR in cohshape: la_eigenproblem was not able to compute the eigenvalues...'
;         continue
;      endif

; ;;;; compute the coherences for given polarizations...
;      minev = min(eval,minpos)
;      maxev = max(eval,maxpos)
;      w1   = w[*,minpos]
;      w2   = w[*,maxpos]
;      pol  = w1
;      d1 = (conj(pol) ## C11 ## transpose(pol))
;      d2 = (conj(pol) ## C22 ## transpose(pol))
;      d3 = (conj(pol) ## C12 ## transpose(pol))
;      coh1 = d3 / sqrt(abs(d1)*abs(d2))
;      pol  = w2
;      d1 = (conj(pol) ## C11 ## transpose(pol))
;      d2 = (conj(pol) ## C22 ## transpose(pol))
;      d3 = (conj(pol) ## C12 ## transpose(pol))
;      coh2 = d3 / sqrt(abs(d1)*abs(d2))
;      boundary[i*2] = [coh1,coh2]
;   endfor
;   if ~keyword_set(NOSORT) then begin
;      m = mean(boundary)
;      s = sort(atan(boundary-m,/ph))
;      boundary = boundary[s]
;   endif
;   return, boundary
; end

function projcol,v,p1,p2,p3,HHVVHV=hhvvhv
; v is normalized!!!
  if keyword_set(hhvvhv) then val = (byte(256.*abs(v)))[[2,0,1]] $
  else val = (byte(256.*abs([v # p1,v#p2,v#p3])))[[2,0,1]]
  val = val[0]/40. + 6.3*(val[1]/40. + 6.3*val[2]/40.) ; because no possibity for RGB!
;  val = val[0]/3 + val[1]/3 + val[2]/3) ; because no possibity for RGB!
  return, BYTE(val)
end

FUNCTION arg, c
  return, atan(c,/phase)
END

FUNCTION argmin, x, minimum=minimum
  m=min(x,pos)
  IF arg_present(minimum) THEN minimum=m
  return, pos
END

FUNCTION evlinfit, x,y,norm_angle, ABC=ABC,EVZ=evz
; EIGENVALUE - LINEFIT  (01.09.04 by mn)
; input : x,y      - one dimensional real arrays
; if norm_angle is given, the line normal angle is returned in radians
; keyword ABC      - return [a,b,c]: line equation: 0 = a*x + b*y + c
; if no keyword:   - return [A,B]:   line equation: y = A + B*x

  xmean=mean(x) & ymean=mean(y)
  b = [[x - xmean],[y - ymean]]
  A = b ## transpose(b)
  eigenvalues = la_eigenql(A,eigenvectors=eigenvectors)
  ab = eigenvectors[*,argmin(eigenvalues)]
  c = -[xmean,ymean] ## transpose(ab)
  IF keyword_set(evz) THEN $
    evz = {evzt,mean:[xmean,ymean],eval:eigenvalues,evec:eigenvectors}
  IF n_params() EQ 3 $
    THEN NORM_ANGLE = atan(ab[1],ab[0])
  IF keyword_set(ABC) $
    THEN return, [ab,c] $
  ELSE return, [-c/ab[1], - ab[0]/ab[1]]
END

pro plot_cohlinfit,coh,color
  xline = findgen(2)*2.-1.
  if n_elements(coh) ge 2 then begin
     l=evlinfit(real_part(coh),imaginary(coh))
     oplot,xline,l[0]+xline*l[1],thick=1,color=color
  endif
end

FUNCTION sqrtm, M, NO_TEST=no_test
; square root of matrix M
; M should be diagonalizable, respectively symmetric/hermitian!?

  eval = la_eigenql(M,eigenvectors=evec)
  L = diag_matrix(sqrt(complex(eval)))
  A = evec # L # la_invert(evec)
  return, transpose(A)
END

pro get_CT,V,C,T,VDIMH=vdimh
  common rat, types, file
  if vdimh eq 3 then D  = [[1,1,0],[1,-1,0],[0,0,sqrt(2)]] / sqrt(2)
  if vdimh eq 4 then $
    D  = [[1,1,0,0],[1,-1,0,0],[0,0,1,1],[0,0,complex(0,-1),complex(0,1)]] / sqrt(2)
  DM = [ [D,complexarr(vdimh,vdimh)], [complexarr(vdimh,vdimh),D] ]
  Di = adj(D)
  DMi= adj(DM)

  n = [(size(V))[3],(size(V))[4]]
  if file.type eq 500 then begin
     kl = V
     kl6 = [reform(kl[*,0,*,*]),reform(kl[*,0,*,*])]
     C = block_xprod(kl6,conj(kl6))
     T=C
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       T[*,*,i,j] = DMi # C[*,*,i,j] #DM
  endif
  if file.type eq 501 then begin
     kp  = V
     kp6 = [reform(kp[*,0,*,*]),reform(kp[*,0,*,*])]
     T = block_xprod(kp6,conj(kp6))
     C = T
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       C[*,*,i,j] = DM # T[*,*,i,j] # DMi
  endif
  if file.type eq 510 then begin
     C = V
     T = C
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       T[*,*,i,j] = DMi # C[*,*,i,j] #DM
  endif
  if file.type eq 511 then begin
     T = V
     C = T
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       C[*,*,i,j] = DM # T[*,*,i,j] # DMi
  endif
end

pro get_lpCT,V,kl,kp,C,T,VDIMH=vdimh
  common rat, types, file
  if vdimh eq 3 then begin
     D  = [[1,1,0],[1,-1,0],[0,0,sqrt(2)]] / sqrt(2)
     DM = [ [D,complexarr(3,3)], [complexarr(3,3),D] ]
  endif
  if vdimh eq 4 then begin
     D  = [[1,1,0,0],[1,-1,0,0],[0,0,1,1],[0,0,complex(0,-1),complex(0,1)]] / sqrt(2)
     DM = [ [D,complexarr(4,4)], [complexarr(4,4),D] ]
  endif
  Di = adj(D)
  DMi= adj(DM)

  n = [(size(V))[3],(size(V))[4]]
  if file.type eq 500 then begin
     kl = V
     kp = kl
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       kp[*,*,i,j]  = D# kl[*,*,i,j] 
;     kp = kl # D
     kp6 = [reform(kp[*,0,*,*]),reform(kp[*,1,*,*])]
     T = block_xprod(kp6,conj(kp6))
;     T = kp6 # conj(kp6)
     C = T
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       C[*,*,i,j] = DM # T[*,*,i,j] # DMi
  endif
  if file.type eq 501 then begin
     kp  = V
     kp6 = [reform(kp[*,0,*,*]),reform(kp[*,1,*,*])]
     T = block_xprod(kp6,conj(kp6))
;     T   = kp6 # conj(kp6)
     C = T
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       C[*,*,i,j] = DM # T[*,*,i,j] # DMi
;     C = DM # T # DMi
     kl = kp
     for i=0,n[0]-1 do for j=0,n[1]-1 do $
       kl[*,*,i,j]  = kp[*,*,i,j] # Di
  endif
end

function MCoh,M
; returns the complex coherence of matrix C or T
  vdimh = (size(M))[1] / 2
  if vdimh eq 3 then begin
     ind1=[0,1,2]
     ind2=[3,4,5]
  endif
  if vdimh eq 4 then begin
     ind1=[0,1,2,3]
     ind2=[4,5,6,7]
  endif
  coh = M[ind2,ind1] / sqrt(M[ind1,ind1]*M[ind2,ind2])
  return,coh
end

function OCoh, M,SINGLEW=singlew
; return the optimal coherence after Kostas
  vdimh = (size(M))[1] / 2
  T11i = la_invert(M[0:vdimh-1,0:vdimh-1])
  T22i = la_invert(M[vdimh:vdimh*2-1,vdimh:vdimh*2-1])
  O12  = M[vdimh:vdimh*2-1,0:vdimh-1]

  a = T11i ## O12
  b = T22i ## adj(O12)
  eval1 = real_part(la_eigenproblem(a ## b,eigenvectors=w1))
  eval2 = real_part(la_eigenproblem(b ## a,eigenvectors=w2))
  sort1 = reverse(sort(eval1))
  sort2 = reverse(sort(eval2))
  w1= w1[*,sort1]
  w2= w2[*,sort2]
  dpha = cadd_arr(fltarr(vdimh),atan(total(w1*conj(w2),1),/pha))
  w2 *= complex(cos(dpha),sin(dpha))
  if keyword_set(singlew) then begin
     w1 = (w1+w2)/complex(2.,0.)
     w2 = w1
  endif
  d1 = (conj(w1) ## M[0:vdimh-1,0:vdimh-1] ## transpose(w1))
  d2 = (conj(w2) ## M[vdimh:vdimh*2-1,vdimh:vdimh*2-1] ## transpose(w2))
  d3 = (conj(w1) ## O12 ## transpose(w2))
  coh = diag_matrix(d3) / sqrt(diag_matrix(d1)*diag_matrix(d2))
  return, coh
end

function Ecoh, M,d ; Esprit
  vdimh = (size(M))[1]/2
  IF n_elements(d) eq 0 THEN d = vdimh-1
  eval = la_eigenql(M,eigenvectors=evec)
;;; important??? sorting of eigenvalues -- the biggest is at 0, etc.!
  evec = reverse(transpose(evec))
  evec = evec[0:d-1,*]
  F12 = [evec[*,0:vdimh-1],evec[*,vdimh:2*vdimh-1]]
  F12H = adj(F12) ## (F12)
  eval = la_eigenql(F12H,eigenvectors=evec)
;;; important??? sorting of eigenvalues -- the biggest is at 0, etc.!
  evec = reverse(transpose(evec))
  E12 = evec(d:2*d-1,0:d-1)
  E22 = evec(d:2*d-1,d:2*d-1)
  if d ne 1 then BEGIN
     psi = - E12 ## LA_INVERT(E22)
     phi = CONJ(LA_EIGENPROBLEM(psi))
  endif else phi = -E12 / E22
  alpha = atan(phi,/phase)
  coh = complex(cos(alpha),sin(alpha))
  return,coh
end

FUNCTION num_radius_3d,A,th0,angle
;;; input: matrix A and initial theta0
;  th = th0
  th2 = -float(th0[0])
  eps = !pi/180./10.
  cnt = 0L
  REPEAT BEGIN
     th = -th2
     Ath = A * exp(complex(0,th))
     Hth = (Ath + adj(Ath)) / 2.
     l   = LA_EIGENQL(Hth, eigenvectors = w)
     lsort = SORT(l)
     l   = l[REVERSE(lsort)]
     w   = w[*,REVERSE(lsort)]
     wmax= w[*,0]
     th2 = (atan(conj(wmax) ## transpose(A) ## transpose(wmax),/phase))[0]
     IF cnt++>10 THEN break
  ENDREP UNTIL abs(th + th2) LE eps
  IF arg_present(angle) THEN $
    angle = ATAN(DIAG_MATRIX(CONJ(w) ## TRANSPOSE(A) ## TRANSPOSE(w)),/phase)
  return, l
END
FUNCTION CCoh, M
  return, coh_nr(M)
; Colin coherence
;   vdimh = (size(M))[1]/2
;   T11 = M[0:vdimh-1,0:vdimh-1]
;   T22 = M[vdimh:vdimh*2-1,vdimh:vdimh*2-1]
;   T12 = M[vdimh:vdimh*2-1,0:vdimh-1]
;   T   = (T11+T22)/2.
;   sqTi   = la_invert(sqrtm(T,/no_test))
;   A   = transpose( sqTi # t12 # sqTi )
;   lambda = num_radius_3d(A,!pi/3,phi)
;   coh = lambda * complex(cos(phi),sin(phi))
;   sort1 = reverse(sort(abs(coh)))
;   return, coh[sort1]
END








pro polin_cohcirc
  common rat, types, file, wid, config

;   if file.type ne 510 and file.type ne 511 and file.type ne 500 $
;     and file.type ne 501 and file.type ne 531 then begin
;  if file.type ne 500 && file.type ne 501 && file.type ne 510 && file.type ne 511 $
  if file.type ne 510 && file.type ne 511 $
  then begin
     error = DIALOG_MESSAGE(["This is wrong data type. Needs PolInSAR ",$
                             "covariance or coherency matrix"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

  polin_get_info, POL_NR=pol, TRACKS_NR=n_tr, BASELINES_NR=n_bl, MATRIX=is_matrix
  if n_tr ne 2 then begin
     error = DIALOG_MESSAGE(["This is wrong data type. Needs single-baseline(!) PolInSAR ",$
                             "covariance or coherency matrix"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif


  if file.type eq 500 or file.type eq 501 then real_vdimh = file.vdim
  if file.type eq 510 or file.type eq 511 then real_vdimh = file.zdim/2
  vdimh = real_vdimh


  max_box_max = (file.xdim < file.ydim) - 1 ; borders for max_box
  if (max_box_max/2)*2 eq max_box_max then max_box_max++
  max_box_min = 3

  max_box   = 101<max_box_max   ; should be odd!
;  enter_newpos = 0
  newpos_type = 0
  first_start = 1


;;;; ---- INITIAL OPTIONS ---- ;;;;
  main = WIDGET_BASE(GROUP_LEADER=wid.base,row=6, $
                     TITLE='Complex coherence analysis - Start',/modal,/tlb_kill_request_events,/tlb_frame_attr)
  start_label=WIDGET_LABEL(main,VALUE='Click in the image to choose the position!')
  fld_max_box = CW_FIELD(main,VALUE=max_box,/integer, $
                         TITLE='Max smooth box(between '+strcompress(max_box_min)+ $
                         ' and '+strcompress(max_box_max)+'): ',XSIZE=5)
  if vdimh eq 4 then but_vdimh = cw_bgroup(main,['3','4'],/exclusive,/ROW,set_value=[0],$
                                           LABEL_LEFT='Number of polariaztion channels: ')
  but_newpos_type = CW_BGROUP(main,['Pick new position', $
                                    'Enter new position by hand'], $
                              SET_VALUE=newpos_type,/EXCLUSIVE,/ROW)
  buttons = WIDGET_BASE(main,/column,/BASE_ALIGN_CENTER,XSIZE=400)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  WIDGET_CONTROL, main,/REALIZE,default_button=but_ok
  repeat event = widget_event(main) until (event.id eq but_ok)
  WIDGET_CONTROL,but_newpos_type,GET_VALUE=newpos_type
  widget_control,fld_max_box,GET_VALUE=val ; read widget fields
  if (val ge max_box_min) && (val le max_box_max) then max_box=val
  if vdimh eq 4 then begin
     widget_control,but_vdimh,GET_VALUE=val
     vdimh = val + 3
  endif
  widget_control,main,/destroy  ; remove main widget
  if event.id ne but_ok then return ; OK button _not_ clicked






; some constants and variables
  if (max_box/2)*2 eq max_box then max_box++
  mboxh     = (max_box-1)/2     ; half of the max_box
  coh_size      = 500
;  box_xyratio   = 10.

  box_val = 5 < max_box > max_box_min
  box = [box_val,box_val]
  smooth_lim_plus = max_box
  smooth_lim_moins = 1
  smooth = box
  smooth_type = 0
  smooth_type_labels = ['linear','square','cube']

  zoom_factor    =  1.
  zoom_lim_plus  = 20.
  zoom_lim_moins =  1.

  img_size     = max_box
  img_zoom     = 2
  img_zoom_min = 1
  img_zoom_max = 20

  esprit_d       = 2            ; esprit dominant scatterers
  esprit_vdimh   = vdimh > real_vdimh
  optcoh_singlew = 0            ; use both optcoh mechanism vectors
  linpoints      = 3

  random_coh_nr     = 10000L
  random_coh_nr_min = 100L
  random_coh_nr_max = 500000L
  random_coh_col_type = 0
  random_coh_col_labels = ['histogram','lexico-pol','pauli-pol']

  mainright_xsize = 300


  update_random_coh = 0
  img_color     = 1
  show_pha      = 0


  but_coh_labels = ['Lexicographic','Pauli','Optimized (2vec)','ESPRIT', $
                    'Numerical Optimized','Custom 1 (ground phase)', $
                    'Custom 2 (canopy top)','Custom 3 (canopy bottom)', $
                    'Boundary Shape','Random']
  new_coh_choice = [1,1,1,1,1,0,0,0,0,0] ; for but_coh_labels!
  new_lin_choice = [0,0,0,0,0,0,0,0,0,0] ; for but_lin
  new_lab_choice = [1,0]
  rand_pol_mask  = intarr(vdimh) ; for but_rand[]

  xpos = file.xdim/2
  ypos = file.ydim/2




  repeat begin
     get_newpos    = 0
     update_smooth = 1
     update_img    = 1
     update_coh    = 1
     box = smooth-1             ; needs to be differnt at the beginning!! do not change!
     if new_coh_choice[9] then update_random_coh = 1

; get position
     if newpos_type eq 1 then begin
        main = WIDGET_BASE(GROUP_LEADER=(first_start?wid.base:main1),row=6, $
                           TITLE='Enter the position for the coherence ' + $
                           'analysis',/modal,/tlb_kill_request_events,/tlb_frame_attr)
        fld_posX = CW_FIELD(main,VALUE=xpos,/integer,XSIZE=5, $
                            TITLE='X in ['+strcompress(mboxh)+ $
                            ':'+strcompress(file.xdim-1-mboxh)+']: ')
        fld_posY = CW_FIELD(main,VALUE=ypos,/integer,XSIZE=5, $
                            TITLE='Y in ['+strcompress(mboxh)+ $
                            ':'+strcompress(file.ydim-1-mboxh)+']: ')
        buttons = WIDGET_BASE(main,/row,/BASE_ALIGN_CENTER,XSIZE=400)
        but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
        but_newpos = WIDGET_BUTTON(buttons,VALUE=' Pick new position ')
        WIDGET_CONTROL, main,/REALIZE,default_button=but_ok
        repeat event = widget_event(main) until (event.id eq but_ok)|| $
                       (event.id eq but_newpos) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
        if event.id eq but_newpos then newpos_type = 0 $
        else begin
           WIDGET_CONTROL,fld_posX,GET_VALUE=xpos
           WIDGET_CONTROL,fld_posY,GET_VALUE=ypos
           pos = [xpos,ypos]
        endelse
        widget_control,main,/destroy ; remove main widget
     endif
     if newpos_type eq 0 then begin ; after newpos_type eq 1 !!!
        if ~first_start then begin
           WIDGET_CONTROL,main1, MAP=0
           widget_control,wid.draw,get_value=index
           wset,index
        endif
        print,'Click on a position in the draw area...'
        pos = get_rat_draw_position()
        if ~first_start then $
           WIDGET_CONTROL,main1, MAP=1
     endif
     if newpos_type eq 2 then pos = [xpos,ypos]

     xpos= pos[0] > mboxh < (file.xdim-1-mboxh)
     ypos= pos[1] > mboxh < (file.ydim-1-mboxh)
     if pos[0] ne xpos or pos[1] ne ypos then $
        error=DIALOG_MESSAGE(['The chosen position is too close to image border!' $
                              ,string(pos,/print),'is replaced by', $
                              string(xpos,' ',ypos,/print)], $
                             DIALOG_PARENT = (first_start?wid.base:main1), $
                             TITLE='Warning')
     print,'Chosen position: ',pos,xpos,ypos

     rrat,file.name,origin,block=[xpos-mboxh,ypos-mboxh,max_box,max_box]

     if vdimh ne real_vdimh then begin
        get_CT,origin,Corig4,Torig4,VDIMH=real_vdimh
        if file.type eq 500 || file.type eq 501 then $
           origin = origin[0:vdimh-1,*,*,*] $
        else begin
           x0b= 0          & x0e= vdimh-1
           x1b= real_vdimh & x1e= real_vdimh + vdimh-1
           origin = [[origin[x0b:x0e,x0b:x0e,*,*],origin[x1b:x1e,x0b:x0e,*,*]],$
                     [origin[x0b:x0e,x1b:x1e,*,*],origin[x1b:x1e,x1b:x1e,*,*]]]
        endelse
     endif

;  get_lpCT,origin,klorig,kporig,Corig,Torig,VDIMH=vdimh
     get_CT,origin,Corig,Torig,VDIMH=vdimh

; read custom coherences for the poltom data set track02/01
     custom_dataset = 1
     if custom_dataset eq 0 then begin
        dpath = '/dat/projekte/sar/tomosar/POLTOM_data/'
        cust1 = dpath+'ground_phase.rat'
        cust2 = dpath+'forest_top_canopy_phase.rat'
        cust3 = dpath+'forest_bottom_canopy_phase.rat'
        cust4 = dpath+'building_phase.rat'
     endif
     if custom_dataset eq 1 then begin
        dpath = '/sar/data/tomosar/'
        cust1 = dpath+'ground_phase_sm.rat'
        cust2 = dpath+'forest_top_canopy_phase_sm.rat'
        cust3 = dpath+'forest_bottom_canopy_phase_sm.rat'
        cust4 = dpath+'building_phase_sm.rat'
     endif
     if file_test(cust1) then begin
        rrat,cust1,tmpcoh,header=cust1head
        free_lun,tmpcoh
        if (cust1head[cust1head[0]]eq file.ydim) $
           && (cust1head[cust1head[0]-1]eq file.xdim) $
           && (cust1head[cust1head[0]+1] eq 6) then begin
           rrat,cust1,customcoh1,block=[xpos,ypos,1,1]
           if file_test(cust2) then $
              rrat,cust2,customcoh2,block=[xpos,ypos,1,1]
           if file_test(cust3) then $
              rrat,cust3,customcoh3,block=[xpos,ypos,1,1]
           if file_test(cust4) then $
              rrat,cust4,customcoh4,block=[xpos,ypos,1,1]
        endif
     endif






     if first_start then begin
        first_start = 0
;;;; ---- GENERATE GUI ---- ;;;;
        main1= WIDGET_BASE(GROUP_LEADER=wid.base,row=2, $
                           TITLE='Complex coherence analysis',/tlb_kill_request_events,/tlb_frame_attr)
;;;;    CHANGE THIS IF YOU WANT SCROLL-BARS IN THE X/Y-DIRECTION
;;;;    SET THE Y_SCROLL_SIZE APPROPRIATLY!!
;;;;        main1= WIDGET_BASE(GROUP_LEADER=wid.base,row=2, $
;;;;                           TITLE='Complex coherence analysis',/scroll,y_scroll_size=900,x_scroll_size=900)
        main2= WIDGET_BASE(main1,column=3)
        mainleft = WIDGET_BASE(main2,row=9)

;;;;; COH
        wdg_coh1 = cw_rat_draw(mainleft,coh_size,coh_size,XSCROLL=coh_size-20,YSCROLL=coh_size-20)
        sub_zoom_button = WIDGET_BASE(mainleft,COLUMN=3,/base_align_center)
        but_zoom_m =WIDGET_BUTTON(sub_zoom_button,VALUE=config.imagedir+ $
                                  'zoom_out.bmp',/bitmap)
        text_zoom = WIDGET_LABEL(sub_zoom_button,VALUE='Coherence Zoom Factor:   '$
                                 +STRCOMPRESS(zoom_factor,/REM)+'x')
        but_zoom_p =WIDGET_BUTTON(sub_zoom_button,VALUE=config.imagedir+ $
                                  'zoom_in.bmp',/bitmap)
;;;;; COH OPTIONS
        cohopt_grp = WIDGET_BASE(mainleft,ROW=5,/FRAME,XSIZE=coh_size)
        but_linpoints_labels = ['2',(vdimh lt 4?'3':['3','4'])]
        but_linpoints=CW_BGROUP(cohopt_grp,but_linpoints_labels,/EXCLUSIVE, $
                                /ROW,SET_VALUE=linpoints-2, $
                                LABEL_LEFT='Number of points for line fitting(except random):')
        but_opt = CW_BGROUP(cohopt_grp,['1','2'],/exclusive,/row, $
                            set_value=(~optcoh_singlew), $
                            LABEL_LEFT='Optimal coherence projection vectors:')
        esprit_grp = WIDGET_BASE(cohopt_grp,ROW=2)
        but_esprit_labels = ['1',(real_vdimh lt 4?'2':['2','3'])]
        but_esprit = CW_BGROUP(esprit_grp,but_esprit_labels,/exclusive,/row, $
                               set_value=(esprit_d-1), $
                               LABEL_LEFT='ESPRIT dominant scatterers:')
        if real_vdimh eq 4 && vdimh eq 3 then $
           but_esprit_vdimh = CW_BGROUP(esprit_grp,['3','4'],/exclusive,/row, $
                                        set_value=(esprit_vdimh-3), $
                                        LABEL_LEFT='Channels for ESPRIT-computation:')
        random_field   = CW_FIELD(cohopt_grp,VALUE=random_coh_nr,/LONG, $
                                  XSIZE=7,TITLE='Number of random polarizations:')
        but_random_col = CW_BGROUP(cohopt_grp,SET_VALUE=random_coh_col_type,random_coh_col_labels, $
                                   /exclusive,/row,LABEL_LEFT='Type of random coh representation:')
;;;;; TEXT INFO
        text_pos=WIDGET_LABEL(mainleft,VALUE='Position: '+STRCOMPRESS(xpos,/REM)$
                              +', '+ STRCOMPRESS(ypos,/REM))
        main_text2=WIDGET_LABEL(mainleft,VALUE='Number of used polarizations: '+$
                                STRCOMPRESS(vdimh,/REM)+' of '+strcompress(real_vdimh,/REM) )
;      main_text3=WIDGET_LABEL(mainleft,VALUE='Number of available polarizations: '+ $
;                               STRCOMPRESS(real_vdimh,/REM))
        main_text4= WIDGET_LABEL(mainleft,VALUE='Maximum smoothbox width: '+ $
                                 STRCOMPRESS(max_box,/REM))

        mainright   = WIDGET_BASE(main2,ROW=9)

;      mainTab = WIDGET_TAB(main2)
;      mainright   = WIDGET_BASE(mainTab,TITLE='Main Options',ROW=9)
;      mainfarright= WIDGET_BASE(mainTab,TITLE='Random Coherence Generation Options', $
;                                ROW=3)

;;;;; IMG GRP
        img_grp = WIDGET_BASE(mainright,ROW=3,/FRAME,XSIZE=mainright_xsize)
        wdg_img1 = cw_rat_draw(img_grp,img_size*2,img_size*2,$
                               XSCROLL=img_size*2-20,YSCROLL=img_size*2-20)

        img_visio_grp = WIDGET_BASE(img_grp,COLUMN=2)
        but_magpha =cw_bgroup(img_visio_grp,['Amplitude','Phase'],/exclusive, $
                              column=2, $
                              set_value=show_pha)
        but_imgcol = CW_BGROUP(img_visio_grp,['3 Channels'],/NONEXCLUSIVE, $
                               SET_VALUE=[img_color])
        imgzoom_grp = WIDGET_BASE(img_grp,COLUMN=3,/base_align_center)
        but_imgzoom_m=WIDGET_BUTTON(imgzoom_grp,/BITMAP, $
                                    VALUE=config.imagedir+'zoom_out.bmp')
        text_imgzoom =WIDGET_LABEL(imgzoom_grp,VALUE='  Image Zoom Factor:  ' $
                                   +STRCOMPRESS(img_zoom,/REM)+'x')
        but_imgzoom_p=WIDGET_BUTTON(imgzoom_grp,/BITMAP,$
                                    VALUE=config.imagedir+'zoom_in.bmp')

;;;;; SMOOTH GRP
        big_smooth_grp = WIDGET_BASE(mainright,ROW=2,/FRAME,TITLE='Smooth Properties', $
                                     XSIZE=mainright_xsize)
        but_smooth_type = CW_BGROUP(big_smooth_grp,/ROW,smooth_type_labels, $
                                    SET_VALUE=smooth_type,/EXCLUSIVE, $
                                    LABEL_LEFT='Smoothing type:')
        smooth_tab = WIDGET_TAB(big_smooth_grp)
        smooth_grp = WIDGET_BASE(smooth_tab,COLUMN=3,TITLE='Square Smooth')
        but_smooth_m=WIDGET_BUTTON(smooth_grp,VALUE=' <')
        slider_smooth = WIDGET_SLIDER(smooth_grp,MAXIMUM=smooth_lim_plus, $
                                      MINIMUM=smooth_lim_moins,VALUE=mean(smooth), $
                                      TITLE='Smooth Box',XSIZE=mainright_xsize-100)
        but_smooth_p=WIDGET_BUTTON(smooth_grp,VALUE='> ')
        cust_smooth_grp = WIDGET_BASE(smooth_tab,ROW=2,TITLE='Custom Smooth')
        slider_smooth_x = WIDGET_SLIDER(cust_smooth_grp,MAXIMUM=smooth_lim_plus, $
                                        MINIMUM=smooth_lim_moins,VALUE=smooth[0], $
                                        TITLE='Smooth Box X',XSIZE=mainright_xsize-100)
        slider_smooth_y = WIDGET_SLIDER(cust_smooth_grp,MAXIMUM=smooth_lim_plus, $
                                        MINIMUM=smooth_lim_moins,VALUE=smooth[1], $
                                        TITLE='Smooth Box Y',XSIZE=mainright_xsize-100)

;;;;; COH AND RANDOM TAB
        mainTab = WIDGET_TAB(mainright)
        coh_grp_tmp = WIDGET_BASE(mainTab,TITLE='Coherence Selection',ROW=2, $
                                  /FRAME,XSIZE=mainright_xsize)
        rand_grp = WIDGET_BASE(mainTab,ROW=2,/FRAME,XSIZE=mainright_xsize, $
                               TITLE='Random Coherence Generation')

;;;;; COHERENCES
        coh_grp = WIDGET_BASE(coh_grp_tmp,COLUMN=3)
        but_coh =CW_BGROUP(coh_grp,but_coh_labels,/COLUMN,SET_VALUE=new_coh_choice, $
                           /nonexclusive,LABEL_TOP='Coherences')
        but_lin=CW_BGROUP(coh_grp,/COLUMN,SET_VALUE=new_lin_choice,LABEL_TOP='LinFit', $
                          /NONEXCLUSIVE,[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '])
;                                    'linfit','linfit','linfit','linfit', $
;                                    'linfit','linfit'])
        but_lab=CW_BGROUP(coh_grp,/COLUMN,SET_VALUE=new_lab_choice,LABEL_TOP='Labels', $
                          /NONEXCLUSIVE,[' ',' '])


;;;;; RANDOM COHERENCES
        rand_val_grp = WIDGET_BASE(rand_grp,COLUMN=5)
        but_rand_labels = ['Complex','Amplitude','Phase','Real','Imaginary', $
                           'Real positive','Zero']
        but_rand_empty_labels=[' ',' ',' ',' ',' ',' ',' ']
        but_rand = lonarr(vdimh)
        for i=0,vdimh-2 do but_rand[i] =cw_bgroup(rand_val_grp,/COLUMN, $
                                                  SET_VALUE=rand_pol_mask[i], $
                                                  but_rand_empty_labels,/exclusive, $
                                                  LABEL_TOP=STRCOMPRESS(i+1,/REM))
        but_rand[vdimh-1] =cw_bgroup(rand_val_grp,/COLUMN,SET_VALUE=rand_pol_mask[vdimh-1],$
                                     /exclusive,but_rand_labels, $
                                     LABEL_TOP=strcompress(vdimh,/REM))

;;;;; BOTTOM BUTTONS
        mc_buttons      = WIDGET_BASE  (main1,column=9,/frame)
        mc_but_ok = WIDGET_BUTTON(mc_buttons,VALUE=' OK ',xsize=80,/frame)
        mc_but_info     = WIDGET_BUTTON(mc_buttons,VALUE=' Info ',xsize=60)
        mc_but_newpos   = WIDGET_BUTTON(mc_buttons,VALUE=' Pick new position ')
        mc_but_enterpos = WIDGET_BUTTON(mc_buttons,VALUE=' Enter new position ')
        text_move =WIDGET_LABEL(mc_buttons,VALUE=' Move one pixel: ')
;      but_newpos_u = WIDGET_BUTTON(mc_buttons,VALUE=' 1 Pixel Up ')
;      but_newpos_d = WIDGET_BUTTON(mc_buttons,VALUE=' 1 Pixel Down ')
;      but_newpos_r = WIDGET_BUTTON(mc_buttons,VALUE=' 1 Pixel to the Right ')
;      but_newpos_l = WIDGET_BUTTON(mc_buttons,VALUE=' 1 Pixel to the Left ')
        but_newpos_l = WIDGET_BUTTON(mc_buttons,/BITMAP,$
                                     VALUE=config.imagedir+'shift_left.bmp')
        but_newpos_u = WIDGET_BUTTON(mc_buttons,/BITMAP,$
                                     VALUE=config.imagedir+'shift_up.bmp')
        but_newpos_d = WIDGET_BUTTON(mc_buttons,/BITMAP,$
                                     VALUE=config.imagedir+'shift_down.bmp')
        but_newpos_r = WIDGET_BUTTON(mc_buttons,/BITMAP,$
                                     VALUE=config.imagedir+'shift_right.bmp')



;     WIDGET_CONTROL, main1, /REALIZE, default_button = but_ok,tlb_get_size=toto
        WIDGET_CONTROL, main1, /REALIZE,tlb_get_size=toto
        pos = center_box(toto[0],drawysize=toto[1])
        widget_control, main1, xoffset=pos[0], yoffset=pos[1]
        widget_control,wdg_coh1,draw_button_events=1, draw_motion_events = 1
        widget_control,wdg_img1,draw_button_events=1, draw_motion_events = 1

        widget_control,wdg_img1,GET_VALUE=win_img1
        widget_control,wdg_coh1,GET_VALUE=win_coh1
        widget_control,but_coh, GET_VALUE=new_coh_choice
        widget_control,but_lin, GET_VALUE=new_lin_choice
; ----

     endif else begin
;     WIDGET_CONTROL,main1, MAP=1
        WIDGET_CONTROL,text_pos,SET_VALUE='Position: '+STRCOMPRESS(xpos,/REM)$
                       +', '+ STRCOMPRESS(ypos,/REM)
     endelse







;;;; ---- EVENT LOOP ---- ;;;;
     repeat begin

        if update_smooth then begin
           box = smooth
           C = Corig & T = Torig
           for i=0,smooth_type do begin
              C  = smooth(C, [1,1,box[0],box[1]])
              T  = smooth(T, [1,1,box[0],box[1]])
           endfor
;           C  = smooth(Corig, [1,1,box[0],box[1]])
;           T  = smooth(Torig, [1,1,box[0],box[1]])
           C  = reform(C[*,*,mboxh,mboxh])
           T  = reform(T[*,*,mboxh,mboxh])
           if vdimh ne real_vdimh then begin
              T4 = Torig4
              for i=0,smooth_type do $
                 T4  = smooth(T4, [1,1,box[0],box[1]])
              T4 = reform(T4[*,*,mboxh,mboxh])
           endif
           T11 = T[0:vdimh-1,0:vdimh-1]
           T22 = T[vdimh:vdimh*2-1,vdimh:vdimh*2-1]
           T12 = T[vdimh:vdimh*2-1,0:vdimh-1]
           C11 = C[0:vdimh-1,0:vdimh-1]
           C22 = C[vdimh:vdimh*2-1,vdimh:vdimh*2-1]
           C12 = C[vdimh:vdimh*2-1,0:vdimh-1]
           imgmag=(abs(reform(origin[0,0:2,*,*])))
           if file.type eq 500 || file.type eq 501 then $
              imgpha=atanc(reform(origin[0:2,0,*,*]*conj(origin[0:2,1,*,*]))) $
           else imgpha=atanc(reform([origin[vdimh,0,*,*],origin[vdimh+1,1,*,*],origin[vdimh+2,2,*,*]]))
           for i=0,smooth_type do begin
              imgmag=smooth(imgmag,[1,box[0],box[1]])
              imgpha=smooth(imgpha,[1,box[0],box[1]])
           endfor
           if new_coh_choice[9] then $
              update_random_coh = 1
           update_img = 1
           update_coh = 1
           update_smooth = 0
        endif



        if update_img then begin
           WIDGET_CONTROL,wdg_img1,DRAW_XSIZE=img_size*img_zoom, $
                          DRAW_YSIZE=img_size*img_zoom
           wset,win_img1 & loadct,0,/SILENT
           if show_pha then begin
              if img_color then img=rebin(imgpha,3,img_size*img_zoom, $
                                          img_size*img_zoom)$
              else img = rebin(reform(imgpha[0,*,*]),img_size*img_zoom, $
                               img_size*img_zoom)
              img = bytscl(img,-!pi,!pi)
           endif else begin
              if img_color then img=rebin(imgmag,3,img_size*img_zoom, $
                                          img_size*img_zoom)$
              else img = rebin(reform(imgmag[0,*,*]),img_size*img_zoom, $
                               img_size*img_zoom)
              mm=mean(img)
              img=bytscl(img,0,2.5*mm)
           endelse
           if img_color then tv,img,true=1 $
           else tv,img
           update_img = 0
        endif




        if update_coh || update_random_coh then begin
           WIDGET_CONTROL,wdg_coh1,DRAW_XSIZE=coh_size*zoom_factor, $
                          DRAW_YSIZE=coh_size*zoom_factor
           wset,win_coh1 & loadct,0,/silent & ucplot


           if update_random_coh then begin ; random coherence
              loadct,33,/silent
              rand_coh   = complexarr(random_coh_nr)
              rand_coh_color = intarr(random_coh_nr)
              coh_hist_dim = 201
              coh_hist_speed = 4
              coh_hist   = intarr(coh_hist_dim,coh_hist_dim)
              pol = complexarr(vdimh,random_coh_nr)
              for i=0,vdimh-1 do begin
                 r  = 100 *randomu(seed,random_coh_nr)
                 phi= 1000*randomu(seed,random_coh_nr)
                 case rand_pol_mask[i] of 
                    0: pol[i,*] = r*complex(cos(phi),sin(phi))
                    1: pol[i,*] = r
                    2: pol[i,*] = complex(cos(phi),sin(phi))
                    3: pol[i,*] = REAL_PART(r*complex(cos(phi),sin(phi)))
                    4: pol[i,*] = complex(0,IMAGINARY(r*complex(cos(phi), $
                                                                sin(phi))))
                    5: pol[i,*] = abs(REAL_PART(r*complex(cos(phi),sin(phi))))
                    6:          ; do nothing! - zero
                 endcase
              endfor

              loadct,(random_coh_col_type?34:33),/silent
              for i=0L,random_coh_nr-1 do begin
                 pol[*,i] /= (sqrt(abs(pol[*,i] ## adj(pol[*,i]))))[0]
                 d1 = (conj(pol[*,i]) ## C11 ## transpose(pol[*,i]))
                 d2 = (conj(pol[*,i]) ## C22 ## transpose(pol[*,i]))
                 d3 = (conj(pol[*,i]) ## C12 ## transpose(pol[*,i]))
                 coh = d3 / sqrt(abs(d1)*abs(d2))
                 coh_r=byte(real_part(coh)*(coh_hist_dim-1)/2+(coh_hist_dim-1)/2)
                 coh_i=byte(imaginary(coh)*(coh_hist_dim-1)/2+(coh_hist_dim-1)/2)
                 coh_hist[coh_r,coh_i] += coh_hist_speed
                 rand_coh[i] = coh
                 rand_coh_color[i] = 255-coh_hist[coh_r,coh_i]
                 case random_coh_col_type of 
                    0: begin
                       plots,real_part(coh),imaginary(coh),psym =3, $
                             color=rand_coh_color[i]
                    end
                    1: begin
                       plots,real_part(coh),imaginary(coh),psym =3, $
                             color=projcol(reform(pol[0:2,i]),/hhvv)
                    end
                    2: begin
                       plots,real_part(coh),imaginary(coh),psym =3, $
                             color=projcol(reform(pol[0:2,i]),[1,1,0],[1,-1,0],[0,0,1])
                    end
                 end
              endfor
              r=-1 & phi=-1 & coh_hist=-1 ; delvar!
              update_random_coh = 0
           endif else if new_coh_choice[9] then begin
              loadct,(random_coh_col_type?34:33),/silent
              for i=0L,random_coh_nr-1 do $
                 case random_coh_col_type of 
                 0: begin
                    plots,real_part(rand_coh[i]),imaginary(rand_coh[i]),psym =3, $
                          color=rand_coh_color[i]
                 end
                 1: begin
                    plots,real_part(rand_coh[i]),imaginary(rand_coh[i]),psym =3, $
                          color=projcol(reform(pol[0:2,i]),/hhvv)
                 end
                 2: begin
                    plots,real_part(rand_coh[i]),imaginary(rand_coh[i]),psym =3, $
                          color=projcol(reform(pol[0:2,i]),[1,1,0],[1,-1,0],[0,0,1])
                 end
              end
;                plots,real_part(rand_coh[i]),imaginary(rand_coh[i]),psym=3, $
;                color=rand_coh_color[i]
           endif

           tek_color
           if new_coh_choice[0] then begin
              coh = MCoh(C)
              oplot,real_part(coh),imaginary(coh),psym = 1,thick=2,symsize=2, $
                    color=1
              if new_lin_choice[0] then plot_cohlinfit, $
                 coh[0:linpoints-1],1
              if new_lab_choice[0] then xyouts, real_part(coh),imaginary(coh),color=1, $
                                                (vdimh eq 3? ['HH','VV','HV']:['HH','VV','HV','VH'])
;               print,'LEXICO - ','Abs: '+strcompress(abs(coh))+' Pha: ' $
;                 +strcompress(arg(coh))
           endif
           if new_coh_choice[1] then begin
              coh = MCoh(T)
              oplot,real_part(coh),imaginary(coh),psym = 2,thick=2,symsize=2,color=3
              if new_lin_choice[1] then plot_cohlinfit, $
                 coh[0:(linpoints-1)],3
; 			names = (zdim EQ 3) ? ['HH+VV','HH-VV','2HV'] :
; 			['HH+VV','HH-VV','HV+VH','i*(HV-VH)']
              if new_lab_choice[1] then xyouts, real_part(coh),imaginary(coh),color=3, $
                                                (vdimh eq 3? ['HH+VV','HH-VV','2HV']:['HH+VV','HH-VV','HV+VH','i*(HV-VH)'])
;               print,'PAULI - ','Abs: '+strcompress(abs(coh))+' Pha: ' $
;                 +strcompress(arg(coh))
           endif
           if new_coh_choice[2] then begin
              coh = OCoh(T,SINGLEW=optcoh_singlew)
              oplot,real_part(coh),imaginary(coh),psym = 4,thick=2,symsize=2,color=4
              if new_lin_choice[2] then plot_cohlinfit, $
                 coh[0:(linpoints-1)],4
;               print,'OPTIMAL-KOSTAS - ','Abs: '+strcompress(abs(coh))+' Pha: '$
;                 +strcompress(arg(coh))
           endif
           if new_coh_choice[3] then begin
              if (esprit_vdimh ne vdimh) then $
                 coh = ECoh(T4,esprit_d) $
              else coh = ECoh(T,esprit_d)
              oplot,real_part(coh),imaginary(coh),psym = 5,thick=2,symsize=2,color=5
              if new_lin_choice[3] then plot_cohlinfit, $
                 coh[0:(linpoints-1)<(n_elements(coh)-1)],5
;               print,'OPTIMAL-ESPRIT - ','Abs: '+strcompress(abs(coh))+' Pha: '$
;                 +strcompress(arg(coh))
           endif
           if new_coh_choice[4] then begin
              coh = CCoh(T)
              oplot,real_part(coh),imaginary(coh),psym = 6,thick=2,symsize=2,color=6
              if new_lin_choice[4] then plot_cohlinfit, $
                 coh[0:(linpoints-1)],6
;               print,'OPTIMAL-COLIN - ','Abs: '+strcompress(abs(coh))+' Pha: '$
;                 +strcompress(arg(coh))
           endif
           if new_coh_choice[5] then begin
              if n_elements(customcoh1) gt 0 then begin
                 coh = customcoh1
                 oplot,real_part(coh),imaginary(coh),psym = 6,thick=2, $
                       symsize=3,color=7
                 if new_lin_choice[5] then plot_cohlinfit, $
                    coh[0:(linpoints-1)<(n_elements(coh)-1)],7
              endif else begin
                 error=DIALOG_MESSAGE('The support is not implemented yet!', $
                                      DIALOG_PARENT=main1)
                 WIDGET_CONTROL,but_coh,GET_VALUE=val
                 val[5] = 0
                 WIDGET_CONTROL,but_coh,SET_VALUE=val
                 new_coh_choice[5] = 0
              endelse
           endif
           if new_coh_choice[6] then begin
              if n_elements(customcoh2) gt 0 then begin
                 coh = customcoh2
                 oplot,real_part(coh),imaginary(coh),psym = 2,thick=2, $
                       symsize=3,color=8 
                 if new_lin_choice[6] then plot_cohlinfit, $
                    coh[0:(linpoints-1)<(n_elements(coh)-1)],8
              endif else begin
                 error=DIALOG_MESSAGE('The support is not implemented yet!', $
                                      DIALOG_PARENT=main1)
                 WIDGET_CONTROL,but_coh,GET_VALUE=val
                 val[6] = 0
                 WIDGET_CONTROL,but_coh,SET_VALUE=val
                 new_coh_choice[6] = 0
              endelse
           endif
           if new_coh_choice[7] then begin
              if n_elements(customcoh3) gt 0 then begin
                 coh = customcoh3
                 oplot,real_part(coh),imaginary(coh),psym = 4,thick=2, $
                       symsize=3,color=9 
                 if new_lin_choice[7] then plot_cohlinfit, $
                    coh[0:(linpoints-1)<(n_elements(coh)-1)],9
              endif else begin
                 error=DIALOG_MESSAGE('The support is not implemented yet!', $
                                      DIALOG_PARENT=main1)
                 WIDGET_CONTROL,but_coh,GET_VALUE=val
                 val[7] = 0
                 WIDGET_CONTROL,but_coh,SET_VALUE=val
                 new_coh_choice[7] = 0
              endelse
           endif
           if new_coh_choice[8] then begin
              boundary = coh_shape(C)
              boundary = [boundary,boundary[0]]
              plots,real_part(boundary),imaginary(boundary),thick=1,color=10
              if new_lin_choice[8] then plot_cohlinfit,boundary,10

;               if n_elements(customcoh4) gt 0 then begin  ;;;;;;;;;FOR TOMOGRAPHY
;                  coh = customcoh4
;                  oplot,real_part(coh),imaginary(coh),psym = 5,thick=2, $
;                    symsize=3,color=10 
;                  if new_lin_choice[8] then plot_cohlinfit, $
;                    coh[0:(linpoints-1)<(n_elements(coh)-1)],10
;               endif else begin
;                  error=DIALOG_MESSAGE('The support is not implemented yet!', $
;                                       DIALOG_PARENT=main1)
;                  WIDGET_CONTROL,but_coh,GET_VALUE=val
;                  val[8] = 0
;                  WIDGET_CONTROL,but_coh,SET_VALUE=val
;                  new_coh_choice[8] = 0
;               endelse
           endif
           if new_coh_choice[9] && new_lin_choice[9] $
           then plot_cohlinfit,rand_coh,2
           
           update_coh = 0
        endif




        event = widget_event(main1)
        switch event.id of

           but_zoom_p: begin
              zoom_factor = (zoom_factor + 0.5) < zoom_lim_plus
              WIDGET_CONTROL,text_zoom,SET_VALUE='Coherence Zoom Factor: ' $
                             +STRCOMPRESS(zoom_factor,/REM)+'x'
              update_coh = 1
           break & end
           but_zoom_m: begin
              zoom_factor = (zoom_factor - 0.5) > zoom_lim_moins
              WIDGET_CONTROL,text_zoom,SET_VALUE='Coherence Zoom Factor: ' $
                             +STRCOMPRESS(zoom_factor,/REM)+'x'
              update_coh = 1
           break & end

           but_magpha: begin
              widget_control,but_magpha,get_value=val
              if val ne show_pha then begin
                 show_pha = val
                 update_img = 1
                 print,'Show ',(val?'Phase':'Amplitude')
              endif
           break & end
           but_imgcol: begin
              WIDGET_CONTROL,but_imgcol,GET_VALUE=img_color
              update_img = 1
           break & end
           but_imgzoom_p: begin
              img_zoom = (img_zoom+1) < img_zoom_max
              WIDGET_CONTROL,text_imgzoom,SET_VALUE='Image Zoom Factor: ' $
                             +STRCOMPRESS(img_zoom,/REM)+'x'
              update_img = 1
           break & end
           but_imgzoom_m: begin
              img_zoom = (img_zoom-1) > img_zoom_min
              WIDGET_CONTROL,text_imgzoom,SET_VALUE='Image Zoom Factor: ' $
                             +STRCOMPRESS(img_zoom,/REM)+'x'
              update_img = 1
           break & end

           slider_smooth: begin
              WIDGET_CONTROL,slider_smooth,GET_VALUE=val
              val = val > smooth_lim_moins < smooth_lim_plus
              if ~ARRAY_EQUAL(smooth,[val,val]) then begin
                 if (val/2)*2 eq val then begin
                    val++
                    WIDGET_CONTROL,slider_smooth,SET_VALUE=val
                 endif
                 WIDGET_CONTROL,slider_smooth_x,SET_VALUE=val
                 WIDGET_CONTROL,slider_smooth_y,SET_VALUE=val
                 smooth = [val,val]
                 update_smooth=1
                 print,'Update square smooth box and visualization...'
              endif
           break & end
           but_smooth_p: begin
              WIDGET_CONTROL,slider_smooth,GET_VALUE=val
              val = (val+2) > smooth_lim_moins < smooth_lim_plus
              if ~ARRAY_EQUAL(smooth,[val,val]) then begin
                 if (val/2)*2 eq val then val++
                 WIDGET_CONTROL,slider_smooth,SET_VALUE=val
                 WIDGET_CONTROL,slider_smooth_x,SET_VALUE=val
                 WIDGET_CONTROL,slider_smooth_y,SET_VALUE=val
                 smooth = [val,val]
                 update_smooth=1
                 print,'Update square smooth box and visualization...'
              endif
           break & end
           but_smooth_m: begin
              WIDGET_CONTROL,slider_smooth,GET_VALUE=val
              val = (val-2) > smooth_lim_moins < smooth_lim_plus
              if ~ARRAY_EQUAL(smooth,[val,val]) then begin
                 if (val/2)*2 eq val then val++
                 smooth = [val,val]
                 WIDGET_CONTROL,slider_smooth,SET_VALUE=val
                 WIDGET_CONTROL,slider_smooth_x,SET_VALUE=val
                 WIDGET_CONTROL,slider_smooth_y,SET_VALUE=val
                 update_smooth=1
                 print,'Update square smooth box and visualization...'
              endif
           break & end
           slider_smooth_x: begin
              WIDGET_CONTROL,slider_smooth_x,GET_VALUE=val
              val = val > smooth_lim_moins < smooth_lim_plus
              if smooth[0] ne val then begin
                 if (val/2)*2 eq val then begin
                    val++
                    WIDGET_CONTROL,slider_smooth_x,SET_VALUE=val
                 endif
                 smooth[0] = val
                 WIDGET_CONTROL,slider_smooth,SET_VALUE=mean(smooth)
                 update_smooth=1
                 print,'Update custom smooth box and visualization...'
              endif
           break & end
           slider_smooth_y: begin
              WIDGET_CONTROL,slider_smooth_y,GET_VALUE=val
              val = val > smooth_lim_moins < smooth_lim_plus
              if smooth[1] ne val then begin
                 if (val/2)*2 eq val then begin
                    val++
                    WIDGET_CONTROL,slider_smooth_y,SET_VALUE=val
                 endif
                 smooth[1] = val
                 WIDGET_CONTROL,slider_smooth,SET_VALUE=mean(smooth)
                 update_smooth=1
                 print,'Update custom smooth box and visualization...'
              endif
           break & end
           but_smooth_type: begin
              WIDGET_CONTROL,but_smooth_type,GET_VALUE=val
              if val ne smooth_type then begin
                 smooth_type = val
                 update_smooth=1
                 print,'Recalculate image and coherence with the new smooth type...'
              endif
           break & end

           but_coh: begin
              widget_control,but_coh,get_value=val
              if val[9] && (~new_coh_choice[9]) $
              then update_random_coh=1
              new_coh_choice = val
              update_coh = 1
              print, 'Update coherence choice...'
           break & end

           but_lin: begin
              WIDGET_CONTROL,but_lin,GET_VALUE=new_lin_choice
              update_coh = 1
              print, 'Update linfit...'
           break & end

           but_lab: begin
              WIDGET_CONTROL,but_lab,GET_VALUE=new_lab_choice
              update_coh = 1
              print, 'Update labels...'
           break & end

           but_opt: begin
              widget_control,but_opt,get_value=val
              if (~val) ne optcoh_singlew then begin
                 optcoh_singlew = ~val
                 update_coh = 1
                 print,'Update optimal coherence mechanism number...'
              endif
           break & end


           but_esprit: begin
              WIDGET_CONTROL,but_esprit,GET_VALUE=val
              if ++val ne esprit_d then begin
                 esprit_d = val
                 update_coh = 1
                 print, 'Update ESPRIT coherence...'
              endif
              if esprit_vdimh eq esprit_d then begin
                 WIDGET_CONTROL,but_esprit,SET_VALUE=((--esprit_d)-1)
                 update_coh = 0
                 print,'This value is not allowed --> please increase the ' + $
                       'number of usable channels for ESPRIT in the next row!'
              endif
           break & end

           but_random_col: begin
              WIDGET_CONTROL,but_random_col,GET_VALUE=val
              if val ne random_coh_col_type then begin
                 random_coh_col_type = val
                 update_coh = 1
              end
           break & end
           
           but_rand[0]: 
           but_rand[1]:
           but_rand[2]:
           but_rand[vdimh-1]: begin
              WIDGET_CONTROL,but_rand[0],GET_VALUE=val0
              WIDGET_CONTROL,but_rand[1],GET_VALUE=val1
              WIDGET_CONTROL,but_rand[2],GET_VALUE=val2
              if vdimh ge 4 then begin
                 WIDGET_CONTROL,but_rand[3],GET_VALUE=val3
                 val = [val0,val1,val2,val3]
              endif else val = [val0,val1,val2]
              if ~ARRAY_EQUAL(val,rand_pol_mask) then begin
                 rand_pol_mask = val
                 if new_coh_choice[9] then begin
                    update_random_coh = 1
                    update_coh = 1
                    print,'Update random polarization states...'
                 endif
              endif
           break & end

           but_linpoints: begin
              WIDGET_CONTROL,but_linpoints,GET_VALUE=val
              if (val+2) ne linpoints then begin
                 linpoints = val+2
                 update_coh = 1
                 print,'Recalculate lines for linfit...'
              endif
           break & end

           mc_but_info: begin   ; Info Button clicked
              infotext = ['COMPLEX COHERENCE CIRCLE',' ',$
                          'RAT module written 10/2004 by Maxim Neumann']
              info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main1,/INFORMATION)
           break & end

           mc_but_newpos: begin
              get_newpos   = 1
              newpos_type  = 0
              print,'Pick a new position on the image...'
           break & end
           mc_but_enterpos:begin
              get_newpos   = 1
              newpos_type  = 1
           break & end
           but_newpos_u:begin
              ypos ++
              get_newpos   = 1
              newpos_type  = 2
           break & end
           but_newpos_d:begin
              ypos --
              get_newpos   = 1
              newpos_type  = 2
           break & end
           but_newpos_r:begin
              xpos ++
              get_newpos   = 1
              newpos_type  = 2
           break & end
           but_newpos_l:begin
              xpos --
              get_newpos   = 1
              newpos_type  = 2
           break & end


        endswitch

        if (vdimh ne real_vdimh) && (event.id eq but_esprit_vdimh) then begin
           WIDGET_CONTROL,but_esprit_vdimh,GET_VALUE=val
           if (val+3) ne esprit_vdimh then begin
              esprit_vdimh = val+3
              if esprit_vdimh eq esprit_d then $
                 WIDGET_CONTROL,but_esprit,SET_VALUE=((--esprit_d)-1)
              update_coh = 1
              print, 'Update ESPRIT channels and coherence...'
           endif
        endif

; random coh number
        WIDGET_CONTROL,random_field,GET_VALUE=val
        val = random_coh_nr_min > val < random_coh_nr_max
        if val ne random_coh_nr then begin
           random_coh_nr = val
           WIDGET_CONTROL,random_field,SET_VALUE=random_coh_nr
           if new_coh_choice[9] then update_random_coh = 1
        endif
        
     endrep until (event.id eq mc_but_ok) || get_newpos or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')

;  if get_newpos then polin_cohcirc ; do it again!
  endrep until ~get_newpos

  widget_control,main1,/destroy ; remove main widget
; switch back to main draw widget
  widget_control,wid.draw,get_value=index
  wset,index

end
