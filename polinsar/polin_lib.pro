;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_lib
; written by	: Maxim Neumann
; last revision	: 08/2006
; POLINSAR-Library	: Collection of supplementary routines for working
; 		  	  with POLINSAR datasets
; Contains	: coh_rand
; 		: ucplot
; 		: coh_shape
; 		: coh_nr
;		: cc_opt
;		: cc
;		: undefine
;		: mb_ind
; 		: mb_opt
; 		: mb_opt_nr
; 		: mb_numrad
;		: mb_nr_baselines
;		: mb_c2t
;		: mb_cc
;		: mb_sb
;		: triplot
;		: ortho_basis
;		: alogB
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

pro undefine, a,b,c,d,e,f,g,h,i
  if n_elements(a) ne 0 then tmp = temporary(a)
  if n_elements(b) ne 0 then tmp = temporary(b)
  if n_elements(c) ne 0 then tmp = temporary(c)
  if n_elements(d) ne 0 then tmp = temporary(d)
  if n_elements(e) ne 0 then tmp = temporary(e)
  if n_elements(f) ne 0 then tmp = temporary(f)
  if n_elements(g) ne 0 then tmp = temporary(g)
  if n_elements(h) ne 0 then tmp = temporary(h)
  if n_elements(i) ne 0 then tmp = temporary(i)
end
function cadd_arr,arr1,arr2
   s1 = (size(arr1))[1]
   s2 = (size(arr2))[1]
   res1 = (fltarr(s2) + 1) ## arr1
   res2 = arr2 ## (fltarr(s1) + 1)
   return,res1+res2
end
pro testnanq, arr, PRINTOUT=printout, ARRNAME=arrname, NO_REMOVE=no_remove
  err=where(finite(arr) eq 0,cnt)
  if cnt ne 0 then begin
     if keyword_set(printout) then $
       print,'Attention! In '+(keyword_set(arrname)?arrname:'arr')+' are ',cnt," nan's detected!"
     if ~keyword_set(no_remove) then begin
        meanval = mean(arr,/NAN) ; mean value without nan's
        arr(err)= meanval
        if keyword_set(printout) then $
          print,cnt," nan's replaced with "+strcompress(meanval,/R)+" in "+arrname
     endif
  endif
end
function mm_mm,A,B
  if size(A,/n_dim) eq 1 then A=reform(A,[n_elements(A),1],/overwrite)
  if size(B,/n_dim) eq 1 then B=reform(B,[n_elements(B),1],/overwrite)
  siz1   = SIZE(A) & siz2   = SIZE(B)
  x1 = siz1[1]    & x2 = siz2[1]
  y1 = siz1[2]    & y2 = siz2[2]
  if x2 ne y1 then $
     message,'ERROR in mm_mm.pro: the dimensions do not fit ' + $
             'for matrix multiplication!'
  if siz1[0] gt 2 && siz2[0] gt 2 && ~array_equal(siz1[3:*],siz2[3:*]) then $
     message,'ERROR in mm_mm.pro: one of the arguments should be ' + $
             'a single matrix; or the dimensions should be equal!'
  new_dim      = (siz1[0] gt siz2[0] ? size(A,/dim) : size(B,/dim))
  new_dim[0:1] = [x1,y2]
  C            = make_array(new_dim,type=size(A,/type))
  if siz1[0] eq 2 then $
     for y=0,y2-1 do $
        for x=0,x1-1 do $
           for k=0,x2-1 do $
              C[x,y,*,*,*,*,*,*] += A[x,k]*B[k,y,*,*,*,*,*,*] $
  else if siz2[0] eq 2 then $
     for y=0,y2-1 do $
        for x=0,x1-1 do $
           for k=0,x2-1 do $
              C[x,y,*,*,*,*,*,*] += A[x,k,*,*,*,*,*,*]*B[k,y] $
  else $
     for y=0,y2-1 do $
        for x=0,x1-1 do $
           for k=0,x2-1 do $
              C[x,y,*,*,*,*,*,*] += A[x,k,*,*,*,*,*,*]*B[k,y,*,*,*,*,*,*]
  return, C
end
pro get_polar_basis, ellipt,orient,pauliB, CIRCULAR_BASIS=CIRCULAR_BASIS, LINEAR_BASIS=LINEAR_BASIS, HV_BASIS=HV_BASIS, ERROR=ERROR
  common rat, types, file, wid, config
  error = 0
;  txt_pre = [' -> HV',' -> circular',' -> linear 45deg']
  ellipt_pre = round(100*[0., !pi/4.,  0.   ])
  orient_pre = round(100*[0.,  0,     !pi/4.])
  if (file.type ge 200L && file.type le 209L) || file.type eq 220L || file.type eq 222L || $
     file.type eq 500L || file.type eq 502L || file.type eq 510L || file.type eq 512L then $
     pauliB = 0 $
  else pauliB = 1
  if file.type eq 200L || file.type eq 220L || $
     file.type eq 500L || file.type eq 510L || file.type eq 501L || file.type eq 511L then begin
     ellipt = 0
     orient = 0
  endif else begin
     st1 = get_par('polbasis_ellipticity',ellipt)
     st2 = get_par('polbasis_orientation',orient)
     if st1 ne 0 || st2 ne 0 then begin
        error_button = DIALOG_MESSAGE(['No polarimetric basis angles are given!'], DIALOG_PARENT = wid.base, TITLE='Error',/E)
        ERROR=1
     endif
  endelse
  CIRCULAR_BASIS=0 & LINEAR_BASIS=0 & HV_BASIS=0
  if ~error then case 1 of
     round(100*ellipt) eq ellipt_pre[0] && round(100*orient) eq orient_pre[0]: HV_BASIS		= 1
     round(100*ellipt) eq ellipt_pre[1] && round(100*orient) eq orient_pre[1]: CIRCULAR_BASIS	= 1
     round(100*ellipt) eq ellipt_pre[2] && round(100*orient) eq orient_pre[2]: LINEAR_BASIS	= 1
     else:
  endcase
end
;; 17.08.06
function coh_rand, T6, Ncoh, POLARIATIONS=pol
  if n_elements(Ncoh) eq 0 then Ncoh=200
  siz = size(T6,/dim)
  case n_elements(siz) of
     4: n=siz[2:3]
     3: n=[siz[2],1]
     2: n=[1L,1L]
  endcase
  if n_elements(pol) eq 0 || ~array_equal(size(pol,/dim),[3,Ncoh]) then begin 
     pol = complex(randomu(seed,3,Ncoh)*2-1,randomu(seed,3,Ncoh)*2-1)
     for i=0L,Ncoh-1 do pol[*,i] /= norm(pol[*,i])
  endif
  coh = complexarr(Ncoh,n[0],n[1],/nozero)
  cpol = conj(pol)
  if Ncoh le 150 then $
     for j=0L,n[1]-1 do for i=0L,n[0]-1 do begin
     d1 = MATRIX_MULTIPLY(pol,cpol ## t6[0:2,0:2,i,j],/A)
     d2 = MATRIX_MULTIPLY(pol,cpol ## t6[3:5,3:5,i,j],/A)
     d3 = MATRIX_MULTIPLY(pol,cpol ## t6[0:2,3:5,i,j],/A)
     coh[*,i,j] = diag_matrix(d3) / $
                  sqrt(abs(diag_matrix(d1))*abs(diag_matrix(d2)))
  endfor else $
     for j=0L,n[1]-1 do for i=0L,n[0]-1 do for k=0L,Ncoh-1 do begin
     d1 = MATRIX_MULTIPLY(pol[*,k],cpol[*,k] ## t6[0:2,0:2,i,j],/A)
     d2 = MATRIX_MULTIPLY(pol[*,k],cpol[*,k] ## t6[3:5,3:5,i,j],/A)
     d3 = MATRIX_MULTIPLY(pol[*,k],cpol[*,k] ## t6[0:2,3:5,i,j],/A)
     coh[k,i,j] = d3 / sqrt(abs(d1)*abs(d2))
  endfor
  return, reform(coh,/overwrite)
end
pro ucplot, NOERASE=NOERASE, TITLE=TITLE, ISO=ISO, xrange=xrange,yrange=yrange
  circ_r = fltarr(1000) + 1.0
  circ_t = findgen(1000) / 1000 * 2 * !pi
  circ_r = [circ_r,fltarr(100)+0.75,fltarr(100)+0.50,fltarr(100)+0.25]
  circ_t = [circ_t,findgen(100)/100*2*!pi,findgen(100)/100*2*!pi, $
            findgen(100)/100*2*!pi]
  circ_r = [circ_r,findgen(100)/50-1.0,findgen(100)/50-1.0]
  circ_t = [circ_t,fltarr(100),fltarr(100)+!Pi/2]
  plot,circ_r,circ_t,/polar,psym=3, NOERASE=NOERASE, TITLE=TITLE, ISO=ISO, xrange=xrange, yrange=yrange,charsize=0.1
end
function coh_shape, CovarianceMatrix, BOUNDARY_POINTS=bp_total, FILTERED_POWER = fpow, MJU=mju, NOSORT=nosort, USE_ORIGINAL_MATRIX=USE_ORIGINAL_MATRIX
  if n_elements(bp_total) eq 0 then bp_total=361
  bp = bp_total/2
  C        = reform(CovarianceMatrix)
  Cdim     = (size(C))[1]
  C11 = C[      0:Cdim/2-1,    0:Cdim/2-1]
  C22 = C[ Cdim/2:Cdim-1,   Cdim/2:Cdim-1]
  C12 = C[      0:Cdim/2-1, Cdim/2:Cdim-1]
  boundary = complexarr(bp*2)
  dphi     = 2.*!pi/float(bp)
  polnr    = Cdim/2
  O12      = C12
  T        = ( C11 + C22 ) /2.
  if n_elements(fpow) ne 0 then $
     mju = float(fpow)/100./float(polnr)
  if n_elements(mju) ne 0 then begin
     T   += diag_matrix(mju*trace(T)   + complexarr(polnr))
     O12 += diag_matrix(mju*trace(O12) + complexarr(polnr))
  endif
  for i = 0,bp-1 do begin
     phi  = float(i)*dphi
     e_phi = complex(cos(phi),sin(phi))
     Ophi = e_phi*O12
     A    = (Ophi + adj(Ophi)) / 2.
     eval = real_part(la_eigenql(A,T,E=w,status=status)) ; if T is positive definite, then use better la_eigenql!
     if status then begin
        print, 'ERROR in cohshape: la_eigenproblem was not able to compute the eigenvalues...'
        continue
     endif
     minev = min(eval,minpos)
     maxev = max(eval,maxpos)
     w1   = w[*,minpos]
     w2   = w[*,maxpos]
     if ~keyword_set(USE_ORIGINAL_MATRIX) then begin
        coh1 = (conj(w1) ## O12 ## transpose(w1)) / abs(conj(w1) ## T ## transpose(w1))
        coh2 = (conj(w2) ## O12 ## transpose(w2)) / abs(conj(w2) ## T ## transpose(w2))
     endif else begin
        pol  = w1
        d1 = (conj(pol) ## C11 ## transpose(pol))
        d2 = (conj(pol) ## C22 ## transpose(pol))
        d3 = (conj(pol) ## C12 ## transpose(pol))
        coh1 = d3 / sqrt(abs(d1)*abs(d2))
        pol  = w2
        d1 = (conj(pol) ## C11 ## transpose(pol))
        d2 = (conj(pol) ## C22 ## transpose(pol))
        d3 = (conj(pol) ## C12 ## transpose(pol))
        coh2 = d3 / sqrt(abs(d1)*abs(d2))
     endelse
     boundary[i*2] = [coh1,coh2]
  endfor
  if ~keyword_set(NOSORT) then begin
     m = mean(boundary)
     s = sort(atan(boundary-m,/ph))
     boundary = boundary[s]
  endif
  if bp*2 ne bp_total then boundary=[boundary,boundary[0]]
  return, boundary
end
function cc, T, w1, w2, MAGNITUDE=magnitude, PHASE=phase, $
             NR_MB=N_TR
  siz = size(T)
  md  = siz[1] ;;; matrix dimension
  if n_elements(w1) eq 0 then begin
     case md of
        3: c = mm_diag(T) ;; PI
        6: if n_elements(n_tr) eq 0 || n_tr eq 2 then $
           c = mm_diag(T[0:2,3:*,*,*,*,*]) $ ;; T6
               /sqrt(mm_diag(T[0:2,0:2,*,*,*,*])*mm_diag(T[3:*,3:*,*,*,*,*]))
        else: begin
           if n_elements(N_TR) eq 0 && md mod 3 eq 0 then $
              N_TR = md/3 ;;; Multibaselines 3-pol!!!
           if n_elements(N_TR) eq 0 then $
              message,'ERROR in cc.pro: ' + $
                      'wrong dimensions of the coherency matrix'
           p   = siz[1]/N_TR
           NRX = 1 & for i=2,N_TR-1 do NRX += i
           c   = complexarr([p,(siz[0]eq 2?NRX:[NRX,siz[3:siz[0]]])],/noz)
           k   = 0
           for i=0,N_TR-1 do $
              for j=i+1,N_TR-1 do $
                 c[*,k++,*,*] = mm_diag(T[i*p:(i+1)*p-1,j*p:(j+1)*p-1,*,*,*])$
              /sqrt(mm_diag(T[i*p:(i+1)*p-1,i*p:(i+1)*p-1,*,*])* $
                    mm_diag(T[j*p:(j+1)*p-1,j*p:(j+1)*p-1,*,*,*]))
        end
     endcase
  endif else begin
     if n_elements(w2) eq 0 then w2=w1
     if n_elements(n_tr) eq 0 || n_tr eq 2 then begin
        p = md/2
        if md eq 3 then c = mm_diag(mm_mm(mm_mm(adj(w1),T),w2)) $
        else c = mm_diag(mm_mm(mm_mm(adj(w1),T[0:p-1,p:*,*,*,*,*]),w2)) $
                 /sqrt(mm_diag(mm_mm(mm_mm(adj(w1),T[0:p-1,0:p-1,*,*,*,*]),w1))* $
                       mm_diag(mm_mm(mm_mm(adj(w2),T[p:*,p:*,*,*,*,*]),w2)))
;         else: message,'ERROR in cc.pro: ' + $
;                       'wrong dimensions of the coherency matrix'
     endif
  endelse
  if keyword_set(PHASE)     then return, atan(c,/phase)
  if keyword_set(MAGNITUDE) then return, abs(c)
  return, c
end
function cc_opt, T, SINGLE_SM=SINGLE_SM, EIGENVALUES=EIGENVALUES, $
                 MAGNITUDE=m, SM = SM,  POL_NR=pol, TRACKS_NR=n_tr, BL_NR=n_bl
 ;; mn, 27.08.2006
  rmnanq,T
  if keyword_set(m) then EIGENVALUES = 1
  siz = size(T)
  if n_elements(pol) eq 0 then pol = 3
  n_tr = 2
  coh = make_array((siz[0]le 2?pol:[pol,siz[3:siz[0]]]),type=(keyword_set(EIGENVALUES)?4:6),/nozero)
  if arg_present(SM) then sm = make_array([pol,pol,(siz[0]le 2?n_tr:[n_tr,siz[3:siz[0]]])],type=siz[siz[0]+1],/nozero)
  case siz[1] of
     pol: begin
        A = T
        B = mm_herm(T)
     end
     2*pol: begin  ;; no test either T11 or T22 are invertible!
        A = mm_mm(mm_invert(T[0:pol-1,0:pol-1,*,*,*,*]),T[0:pol-1,pol:2*pol-1,*,*,*,*])
        B = mm_mm(mm_invert(T[pol:2*pol-1,pol:2*pol-1,*,*,*,*]),mm_herm(T[0:pol-1,pol:2*pol-1,*,*,*,*]))
     end
  endcase
  for l=0,(siz[0]lt 6?0:siz[6]-1) do $
     for k=0,(siz[0]lt 5?0:siz[5]-1) do $
        for j=0,(siz[0]lt 4?0:siz[4]-1) do $
           for i=0,(siz[0]lt 3?0:siz[3]-1) do $
              begin
     eval1 = real_part(la_eigenproblem(transpose(A[*,*,i,j,k,l] # B[*,*,i,j,k,l]),eigenvectors=w1,status=st1))
     if keyword_set(eigenvalues) then begin
        coh[0,i,j,k,l] = eval1[reverse(sort(eval1))]
        continue
     endif
     eval2 = real_part(la_eigenproblem(transpose(B[*,*,i,j,k,l] # A[*,*,i,j,k,l]),eigenvectors=w2,status=st2))
     if st1 ne 0 || st2 ne 0 then begin
        coh[*,i,j,k,l] = 0.
        continue
     endif
     w1= w1[*,reverse(sort(eval1))]
     w2= w2[*,reverse(sort(eval2))]
     dpha = mm_v2m(atan(total(w1*conj(w2),1),/pha),pol,/T)
     w2   = w2 * complex(cos(dpha),sin(dpha))
     if keyword_set(SINGLE_SM) then begin
        w1 += w2
        w2 = mm_vnormalize(w1)
        w1 = w2
     endif
     case siz[1] of 
        pol: coh[*,i,j,k,l] = diag_matrix(conj(transpose(w1))#T[*,*,i,j,k,l]#w2)
        2*pol: coh[0,i,j,k,l] = diag_matrix(        conj(transpose(w1))#T[0:pol-1,pol:*,i,j,k,l]#w2) $
                            /sqrt(diag_matrix(( conj(transpose(w1))#T[0:pol-1,0:pol-1,i,j,k,l]#w1) $
                                              *(conj(transpose(w2))#T[pol:*,pol:*,i,j,k,l]#w2)))
     endcase
     if keyword_set(SINGLE_SM) then coh[0,i,j,k,l] = coh[reverse(sort(abs(coh[*,i,j,k,l]))),i,j,k,l]
     if n_elements(SM) ne 0 then begin
        SM[*,*,0,i,j,k,l] = w1
        SM[*,*,1,i,j,k,l] = w2
     endif
  endfor
  if keyword_set(EIGENVALUES) then return, sqrt(coh)
  if keyword_set(m) then return, abs(coh)
  return, coh
end
function NumRad, A, th0, $ ;; mn, 27.08.2006
                 eval=eval, evec=evec, $
                 EPS=EPS, USE_THETA_THRESHOLD=USE_THETA_THRESHOLD, $
                 MAX_ITER=MAX_ITER, THETA_RETURN=th, DEBUG=DEBUG
  case size(A,/N_DIM) of
     2: 
     1: return, n_elements(A)eq 1? A: stop
     0: return, A
     else: stop
  endcase
  if n_elements(use_theta_threshold) eq 0 then use_theta_threshold=0
  if n_elements(EPS) eq 0 then eps = !pi/180./100.
  if n_elements(MAX_ITER) eq 0 then max_iter = 100
  if n_elements(th0) eq 0 then th0=atan(trace(A),/phase)
  i=0 & th2=th0
  repeat begin
     th   = -th2
     Ath  = complex(cos(th),sin(th)) * A
     Hth  = (Ath + conj(transpose(Ath))) / 2.
     eval = la_eigenql(transpose(Hth), eigenvectors=evec)
     se   = reverse(sort(abs(eval)))
     maxN = conj(transpose(evec[*,se[0]])) # A # evec[*,se[0]]
     th2  = atan(maxN[0],/phase)
     if keyword_set(DEBUG) then $
        print,strcompress(i)+' th: '+strcompress(th)+' th2: '+strcompress(th2)+ $
              ' evalMAX: '+strcompress(abs(eval[se[0]]))+'evalNew: '+strcompress(abs(maxN))+' evalALL:'+strjoin(strcompress(eval))
  endrep until max(eval) ge abs(maxN) || ++i ge MAX_ITER || (use_theta_threshold? abs(th+th2) le EPS: 0)
  eval = eval[se]               ; in descending order now! (eMAX=eval[0])
  evec = evec[*,se]             ; in descending order now! (xMAX=evec[*,0])
  if keyword_set(DEBUG) then $
     print,strcompress(i)+'iterations th0: '+strcompress(th0)+' th2: '+strcompress(th2)+ $
           ' numRAD: '+strcompress(abs(eval[0]))+' evalALL:'+strjoin(strcompress(eval))
  return, abs(eval[0])
end
function coh_NR, T6, MAGNITUDE=magnitude, SM=SM, DEBUG=DEBUG, $ ;; mn, 27.08.2006
                 POL_NR=pol, TRACKS_NR=n_tr, BL_NR=n_bl
;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;
  siz  = size(T6)
  if n_elements(pol) eq 0 then pol = siz[1]/2 
  p = pol                       ; M==p==number of polarizations
  n_tr = 2
  if arg_present(SM) then sm = make_array([p,(siz[0]le 2?p:[p,siz[3:siz[0]]])],type=siz[siz[0]+1],/nozero)

  if siz[0] eq 2 then n = lonarr(4)+1 $
  else if siz[0] ge 3 && siz[0] le 8 then $
     n = [siz[3:siz[0]],lonarr(9-siz[0])+1] $
  else Message,'error in coh_1MC.pro'
  coh = complexarr([p,n],/nozero)
;;;;;;;;;;;; COMPUTATION ;;;;;;;;;;;;;;;;
  for l=0,n[3]-1 do for k=0,n[2]-1 do for j=0,n[1]-1 do for i=0,n[0]-1 do begin
     T11 = T6[0:p-1,0:p-1,i,j,k,l]
     T22 = T6[p:*  ,p:*  ,i,j,k,l]
     T12 = T6[0:p-1,p:*  ,i,j,k,l]
     Tsi = (T11+T22)/2.
     Tsi = mm_power(Tsi,/sqrt,/invert,/overwrite)
     A   = Tsi # T12 # Tsi      ; A is not hermitian
     nr0 = numrad(A,evec=evec,DEBUG=DEBUG)  ;,th=th)
     if pol eq 3 then begin
        x0  = evec[*,0]
        PB  = evec
        A2  = conj(transpose(PB)) # A # PB
        A2[*,0] = 0.
        A2[0,*] = 0.
        nr1 = numrad(A2,evec=evec,eval=eval,DEBUG=DEBUG) ;,/debug)
        x1  = PB # evec[*,0]
        x2  = PB # evec[*,1]
        w0  = Tsi # x0
        w1  = Tsi # x1
        w2  = Tsi # x2
        coh[0,i,j,k,l] = transpose(conj(w0)) # T12 # (w0) $
                         / sqrt(transpose(conj(w0)) # T11 # (w0)) $
                         / sqrt(transpose(conj(w0)) # T22 # (w0)) 
        coh[1,i,j,k,l] = transpose(conj(w1)) # T12 # (w1) $
                         / sqrt(transpose(conj(w1)) # T11 # (w1)) $
                         / sqrt(transpose(conj(w1)) # T22 # (w1)) 
        coh[2,i,j,k,l] = transpose(conj(w2)) # T12 # (w2) $
                         / sqrt(transpose(conj(w2)) # T11 # (w2)) $
                         / sqrt(transpose(conj(w2)) # T22 # (w2)) 
     if n_elements(SM) ne 0 then sm[*,*,i,j,k,l] = $
        [[w0]/norm(w0),[w1]/norm(w1),[w2]/norm(w2)]
     endif else if pol eq 2 then begin
        x0  = evec[*,0]
        x1  = evec[*,1]
        w0  = Tsi # x0
        w1  = Tsi # x1
        coh[0,i,j,k,l] = transpose(conj(w0)) # T12 # (w0) $
                         / sqrt(transpose(conj(w0)) # T11 # (w0)) $
                         / sqrt(transpose(conj(w0)) # T22 # (w0)) 
        coh[1,i,j,k,l] = transpose(conj(w1)) # T12 # (w1) $
                         / sqrt(transpose(conj(w1)) # T11 # (w1)) $
                         / sqrt(transpose(conj(w1)) # T22 # (w1)) 
        if n_elements(SM) ne 0 then sm[*,*,i,j,k,l] = $
           [[w0]/norm(w0),[w1]/norm(w1)]
     endif else message, 'Error in coh_NR'
  endfor
  return, reform(keyword_set(magnitude)?abs(coh):coh)
end
function coh_NR_old, T6
  siz = size(T6)
  p   = siz[1]/2
  if siz[0] eq 2 then n = lonarr(4)+1 $
  else if siz[0] ge 3 && siz[0] le 8 then $
     n = [siz[3:siz[0]],lonarr(9-siz[0])+1] $
  else Message,'error in coh_1MC.pro'
  coh = complexarr([p,n],/nozero)
  for l=0,n[3]-1 do for k=0,n[2]-1 do for j=0,n[1]-1 do for i=0,n[0]-1 do begin
     T11 = T6[0:p-1,0:p-1,i,j,k,l]
     T22 = T6[p:*  ,p:*  ,i,j,k,l]
     T12 = T6[0:p-1,p:*  ,i,j,k,l]
     T   = (T11+T22)/2.
     Ts  = sqrtm(T)
     Tsi = la_invert(Ts)
     A   = Tsi # T12 # adj(Tsi) ; A is not hermitian
     nr0 = numrad(A,evec=evec)
     x0  = evec[*,0]
     PB  = evec
     A1  = adj(PB) # A # PB
     A2  = A1
     A2[*,0] = 0.
     A2[0,*] = 0.
     nr1 = numrad(A2,evec=evec,eval=eval)
     x1  = PB # evec[*,0]
     x2  = PB # evec[*,1]
     w0  = Ts # x0
     w1  = Ts # x1
     w2  = Ts # x2
     coh[0,i,j,k,l] = transpose(conj(w0)) # T12 # (w0) $
                      / sqrt(transpose(conj(w0)) # T11 # (w0)) $
                      / sqrt(transpose(conj(w0)) # T22 # (w0)) 
     coh[1,i,j,k,l] = transpose(conj(w1)) # T12 # (w1) $
                      / sqrt(transpose(conj(w1)) # T11 # (w1)) $
                      / sqrt(transpose(conj(w1)) # T22 # (w1)) 
     coh[2,i,j,k,l] = transpose(conj(w2)) # T12 # (w2) $
                      / sqrt(transpose(conj(w2)) # T11 # (w2)) $
                      / sqrt(transpose(conj(w2)) # T22 # (w2)) 
  endfor
  return, reform(coh)
end
function ortho_basis, v, NORMAL=normal1, ORTHONORMAL=normal2, UNITARY=normal3
  normal = keyword_set(normal1)||keyword_set(normal2)||keyword_set(normal3)
  n = (size(v,/dim))[0]
  u = v
  w = v[*,0]
  for i=1,n-1 do begin
     w[*] = 0
     for j=0,i-1 do w += ((conj(transpose(u[*,j]))#v[*,i]) $
        /(conj(transpose(u[*,j]))#u[*,j]))[0]*u[*,j]
     u[*,i] -= w
  endfor
  if normal then u /= (complexarr(n)+1)#sqrt(diag_matrix(conj(transpose(u))#u))
  return, u
end
pro triplot, x,y,z,xmax=xmax,ymax=ymax,zmax=zmax,NO_ISO=NO_ISO,title=title, $ ; 9.9.6
             UNITARY=UNITARY, RGB=RGB, ANGLE=ANGLE, AXES=AXES, SQUARED=SQUARED
  if n_elements(xmax) eq 0 then xmax=max(abs(x))
  if n_elements(ymax) eq 0 then ymax=max(abs(y))
  if n_elements(zmax) eq 0 then zmax=max(abs(z))
  if keyword_set(UNITARY) then begin
     xmax=1. & ymax=1. & zmax=1.
  endif
  if ~keyword_set(NO_ISO) then begin
     xmax>=ymax>zmax & ymax=xmax & zmax=xmax
  endif
  data = abs([[x[*]],[y[*]],[z[*]]])
  if keyword_set(SQUARED) then begin
     xmax^=2 & ymax^=2 & zmax^=2
     data^=2
  endif
  if n_elements(angle) eq 0 then angle=-5./7*!pi ; -4./6
  angles = !pi*[0,4./6,-4./6,0] + angle
  if n_elements(AXES) eq 0 then axes=['x','y','z']
  base   = complex(cos(angles),sin(angles))
  plot, float(base),imaginary(base),xstyle=29,ystyle=29,/iso
  plots,float(base/2),imaginary(base/2),linestyle=2
  plots,float(base*.25),imaginary(base/4),linestyle=1
  plots,float(base*.75),imaginary(base*.75),linestyle=1
  base = base[0:2]
  xyouts,float(base),imaginary(base),axes,charsize=2
  tmax=total(data,2)
  pnts  = base[0]/tmax*data[*,0] + base[1]/tmax*data[*,1] + base[2]/tmax*data[*,2]
  plots,float(pnts),imaginary(pnts),psym=6
end
function alogB,x,B
  return, alog(x)/alog(B)
end
function alog3,x
  return, alog(x)/alog(3.)
end
function ha_border_courve, complex=compl, DEGREES_ALPHAS=deg, ALL=all
  m=findgen(1001)/1000.
  a1 = (m*!pi)/(1.+2.*m)
  a2 = (!pi/(1.+2.*m)) < (!pi/2.)
  h1 = -1./(1.+2.*m)*alog3(m^(2.*m)/(1.+2.*m)^(2.*m+1))
  h2 = m
  k  = min(where(a2 lt !pi/2))
  mm = m[k:*]
  h2[k:*] = -1./(1.+2.*mm)*alog3((2.*mm-1)^(2.*mm-1.)/(1.+2.*mm)^(2.*mm+1))
 
  if keyword_set(ALL) then k=0
  hac = [[h1,reverse(h2[k:*])],[a1,reverse(a2[k:*])]]
  return, keyword_set(compl)? complex(hac[*,0],hac[*,1]): $
          (keyword_set(deg)? [[hac[*,0]],[hac[*,1]*!radeg]]: hac)
end
function MB_sb, Tx, ind, ind2, POL=POL, THIRD_DIM=THIRD_DIM
  siz = size(Tx,/dim)
  n1 = n_elements(ind) & n2 = n_elements(ind2)
  if n_elements(POL) eq 0 then pol=siz[0] mod 3 eq 0? 3L: 4L
;;; !!!! no alternatives anymore !!!! 6x6 is also considered as MB
;;; !!!! use keyword /THIRD_DIM instead !!!
;;; second alternative representation: 6x6xmbNx...
  if keyword_set(THIRD_DIM) && array_equal([siz[0:1],n1,n2],[pol*2,pol*2,1,0]) then $
     return, Tx[*,*,ind,*,*,*,*,*]
  case 1 of
     array_equal([n1,n2],[1,1]): i=[ind,ind2]
     array_equal([n1,n2],[2,0]): i=ind
     array_equal([n1,n2],[1,0]): i=mb_ind(ind)
     else: message,'error in mb_2sb: wrong indices'
  endcase
  T6=Tx[0:pol*2-1,0:pol*2-1,*,*,*,*,*,*]
  T6[0:pol-1,0:pol-1,*,*,*,*,*,*]= Tx[i[0]*pol:(i[0]+1)*pol-1, $
                                      i[0]*pol:(i[0]+1)*pol-1,*,*,*,*,*,*]
  T6[pol:*,pol:*,*,*,*,*,*,*]    = Tx[i[1]*pol:(i[1]+1)*pol-1, $
                                      i[1]*pol:(i[1]+1)*pol-1,*,*,*,*,*,*]
  T6[0:pol-1,pol:*,*,*,*,*,*,*]  = Tx[i[0]*pol:(i[0]+1)*pol-1, $
                                      i[1]*pol:(i[1]+1)*pol-1,*,*,*,*,*,*]
  T6[pol:*,0:pol-1,*,*,*,*,*,*]  = Tx[i[1]*pol:(i[1]+1)*pol-1, $
                                      i[0]*pol:(i[0]+1)*pol-1,*,*,*,*,*,*]
  return, T6
end
function mb_ind, ind1, ind2, FIRST=FIRST, SECOND=SECOND,TOGETHER_STRING=TOGETHER_STRING
  n1=n_elements(ind1) & n2=n_elements(ind2)
  if ind1[0] lt 0 then message,'error in mb_ind: ind1 is <0 !!!'
  case n1+n2 of
     1: begin ;; ind1d ==> ind2d
        i=-1
        for tr2=1,ind1[0]+1 do $
           for tr1=0,tr2-1 do $
              if ++i eq ind1[0] then return, $
           keyword_set(FIRST)?tr1:keyword_set(SECOND)?tr2: $
           keyword_set(TOGETHER_STRING)?strcompress(tr1,/r)+'x'+strcompress(tr2,/r):[tr1,tr2]
     end
     2: begin ;; ind2d ==> ind1d
        if n1 eq 2 then ind2=ind1[1]
        if ind2 lt 0 then message,'error in mb_ind: ind2 is <0 !!!'
        i=-1
        for tr2=1,ind2[0] do $
           for tr1=0,tr2-1 do $
              if tr2 eq ind2[0] && tr1 eq ind1[0] then return, ++i else ++i
     end
     else: begin
        if n2 eq 0 && n1 gt 2 then begin ;; list of ind1d ==> 2d-list of ind2d
           ret=lonarr(2,n1,/nozero)
           for n=0,n1-1 do begin
              i=-1
              for tr2=1,ind1[n]+1 do $
                 for tr1=0,tr2-1 do $
                    if ++i eq ind1[n] then ret[*,n]=[tr1,tr2]
           endfor
           return, ret
        endif else $
           message,'error in mb_ind: provide max. two elements!'
        end
  endcase
  print,'error in mb_ind: track and baseline indices not allowed!'
  return,-1
end
function mb_nr_baselines, nr_tracks
;;; <==> return, total(lindgen(nr_tracks))
  return, nr_tracks*(nr_tracks-1L)/2L
end
function mb_opt, T, SINGLE_SM=SINGLE_SM, MAGNITUDE=m, CRITERIA=criteria, CONSTRAINT=CONSTRAINT, $
                 GLOBALLY_ORTHOGONAL_BASIS=GLOBALLY_ORTHOGONAL_BASIS, $
                 SM = SM,  POL_NR=pol, TRACKS_NR=n_tr, BL_NR=n_bl
;; /single_sm -- only one average scattering vector is taken to compute result!
;;            -- else both eigenvectors are used.
;; /MAGNITUDE -- /mag
;; CRITERIA =	1: SUMCOR
;; 		6: SUMCOR (diag=0) (DEFAULT)
;; CONSTRAINT =	1: wi wj	  = 1
;; 		2: SUM(wi wj)	  = 1
;; 		3: wi Tij wj 	  = 1
;; 		4: SUM(wi Tij wj) = 1 (DEFAULT)
;; /GLOB (DEFAULT) : build orthogonal scattering mechanism decomposition (w's are orthogonal!)
;;		   (/single is not considered then!)
;; SM=sm 	 : return scattering mechanisms (w) in this argument!

  GLOBALLY_ORTHOGONAL_BASIS = n_elements(GLOBALLY_ORTHOGONAL_BASIS) eq 0? 1: GLOBALLY_ORTHOGONAL_BASIS
  if n_elements(criteria) eq 0 then criteria   = 6
  if n_elements(constraint)   eq 0 then constraint = 4
  method = [criteria,constraint]

  siz  = size(T)
  if n_elements(pol) eq 0 then pol  = siz[1] mod 3 eq 0? 3L: 4L
  if n_elements(n_tr) eq 0 then n_tr = siz[1]/pol
  if n_elements(n_bl) eq 0 then n_bl = mb_nr_baselines(n_tr)
  coh = make_array((siz[0]le 2?pol*n_Bl:[pol*n_Bl,siz[3:siz[0]]]),type=(keyword_set(EIGENVALUES)?4:6),/nozero)
  wTr = complexarr(pol,pol,n_tr,/nozero)
  U    = make_array(pol*n_tr,pol*n_tr,type=siz[siz[0]+1])
  if arg_present(SM) then sm = make_array([pol,pol,n_tr,(siz[0]le 2?1:siz[3:siz[0]])],type=siz[siz[0]+1],/nozero)

  if total(method eq [1,2]) ne 2 then begin
     B   = complexarr(siz[1:siz[0]])
     for i=0,n_tr-1 do $
        B[i*pol:i*pol+pol-1,i*pol:i*pol+pol-1,*,*,*,*]=T[i*pol:i*pol+pol-1,i*pol:i*pol+pol-1,*,*,*,*]
     Bbx = complexarr(siz[1:2])
     Bi  = complexarr(siz[1:2])
  endif

  for l=0,(siz[0]lt 6?0:siz[6]-1) do $
     for k=0,(siz[0]lt 5?0:siz[5]-1) do $
        for j=0,(siz[0]lt 4?0:siz[4]-1) do $
           for i=0,(siz[0]lt 3?0:siz[3]-1) do $
              begin
     case 2 of
        total(method eq [1,2]): eval = la_eigenql(transpose(T[*,*,i,j,k,l])/(n_Tr-1),					      eigenvectors=w,status=status)
        total(method eq [1,4]): eval = la_eigenql(transpose(T[*,*,i,j,k,l])/(n_Tr-1),		    transpose(B[*,*,i,j,k,l]),eigenvectors=w,status=status)
        total(method eq [6,2]): eval = la_eigenql(transpose(T[*,*,i,j,k,l]-B[*,*,i,j,k,l])/(n_Tr-1),			      eigenvectors=w,status=status)
        total(method eq [6,4]): eval = la_eigenql(transpose(T[*,*,i,j,k,l]-B[*,*,i,j,k,l])/(n_Tr-1),transpose(B[*,*,i,j,k,l]),eigenvectors=w,status=status)
        else: stop
     endcase
     eval = reverse(eval)
     w    = reverse(w,2) & w=w[*,0:pol-1]
     if status ne 0 then begin
        coh[*,i,j,k,l] = 0.
        continue
     endif

     if GLOBALLY_ORTHOGONAL_BASIS then begin
        if pol ge 3 then begin
           for tr=0,n_tr-1 do begin
              U3 = ortho_basis(w[tr*pol:(1+tr)*pol-1,0:pol-1],/unitary)
              U[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1]= U3
           endfor
           Tbx   = conj(transpose(U)) # T[*,*,i,j,k,l] # U
           Tbx[lindgen(n_tr)*pol,*] = 0.
           Tbx[*,lindgen(n_tr)*pol] = 0.
           if total(method eq [1,2]) ne 2 then begin
              for tr=0,n_tr-1 do $
                 Bbx[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1]=Tbx[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1]
              for tr=0,n_tr-1 do $
                 Bi[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1]=la_invert(Tbx[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1], status=status)
           endif
           case 2 of
              total(method eq [1,2]): eval2 = la_eigenql(transpose(Tbx)/(n_Tr-1),		eigenvectors=evec,status=status)
              total(method eq [1,4]): eval2 = la_eigenql(transpose(Bi#Tbx)/(n_Tr-1),	eigenvectors=evec,status=status)
              total(method eq [6,2]): eval2 = la_eigenql(transpose(Tbx-Bbx)/(n_Tr-1),	eigenvectors=evec,status=status)
              total(method eq [6,4]): eval2 = la_eigenql(transpose(Bi#(Tbx-Bbx))/(n_Tr-1),	eigenvectors=evec,status=status)
              else: stop
           endcase
           wbx = reverse(evec,2)
           for tr=0,n_tr-1 do begin
              w[pol*tr:pol*tr+pol-1,1:2] = U[pol*tr:pol*tr+pol-1,pol*tr:pol*tr+pol-1] # wbx[pol*tr:pol*tr+pol-1,0:1]
              w[pol*tr:pol*tr+pol-1,*] = ortho_basis(w[pol*tr:pol*tr+pol-1,*],/unitary)
           endfor
           for tr=0,n_tr-1 do $
              wTr[*,*,tr] = w[tr*pol:tr*pol+pol-1,0:pol-1]
        endif else $ ;; pol = 2
           for tr=0,n_tr-1 do $
              wTr[*,*,tr] = mm_vnormalize(w[tr*pol:tr*pol+pol-1,0:pol-1])
        for tr=1,n_Tr-1 do $
           wTr[*,*,tr] *= exp(complex(0,mm_v2m(atan(total(wTr[*,*,0]*conj(wTr[*,*,tr]),1),/pha),pol,/TRANSP)))
        for bl=0,n_Bl-1 do $
           coh[bl*pol,i,j,k,l] = cc(mb_sb(T[*,*,i,j,k,l],bl, pol=pol),wTr[*,0:pol-1,(mb_ind(bl,/F))],wTr[*,0:pol-1,(mb_ind(bl,/S))])
     endif else begin
        if keyword_set(m) && n_bl lt n_tr then begin
           coh[0,i,j,k,l] = ((eval))[0:n_Bl*pol-1]
           continue
        endif else begin
           if keyword_set(SINGLE_SM) then wTr = complexarr(pol,pol,n_tr,/nozero)
           for tr=0,n_Tr-1 do $
              wTr[0,0,tr]   = mm_vnormalize(w[tr*pol:tr*pol+pol-1,0:pol-1])
           for tr=1,n_Tr-1 do $
              wTr[*,*,tr] *= exp(complex(0,mm_v2m(atan(total(wTr[*,*,0]*conj(wTr[*,*,tr]),1),/pha),pol,/TRANSP)))
           if keyword_set(SINGLE_SM) then begin
              wTr = total(wTr,3)/n_tr
              for bl=0,n_Bl-1 do $
                 coh[bl*pol,i,j,k,l] = cc(mb_sb(T[*,*,i,j,k,l],bl, pol=pol),wTr[*,0:pol-1])
           endif else $
              for bl=0,n_Bl-1 do $
                 coh[bl*pol,i,j,k,l] = cc(mb_sb(T[*,*,i,j,k,l],bl, pol=pol),wTr[*,0:pol-1,(mb_ind(bl,/F))],wTr[*,0:pol-1,(mb_ind(bl,/S))])
        endelse
     endelse
     if n_elements(SM) gt 0 then sm[*,*,*,i,j,k,l]=wTr
  endfor
  if keyword_set(m) then return, abs(coh)
  return, coh
end
function mb_NumRad, A, th0, $
                 eval=max_eval, evec=max_evec, $
                 EPS=EPS, USE_THETA_THRESHOLD=USE_THETA_THRESHOLD, $
                 MAX_ITER=MAX_ITER, THETA_RETURN=th, DEBUG=DEBUG
  n_bl= (size(A))[3]
  pol = (size(A))[2]
  if n_elements(use_theta_threshold) eq 0 then use_theta_threshold=0
  if n_elements(EPS) eq 0 then eps = !pi/180./100.
  if n_elements(MAX_ITER) eq 0 then max_iter = 100
  if n_elements(th0) ne n_bl then th0=atan(mm_trace(A),/phase)
  i=0 & th2=th0
  Ath = A
  max_eval=-100000.
  no_update=0
  repeat begin
     th = -th2
     for bl=0,n_bl-1 do $
        Ath[*,*,bl] = complex(cos(th[bl]),sin(th[bl])) * A[*,*,bl]
     Hth  = (Ath + mm_herm(Ath)) / 2.
     if n_bl ne 1 then $
        Hth  = total(Hth,3)/n_bl
     eval = la_eigenql(transpose(Hth), eigenvectors=evec, status=status, failed=failed, method=0)
     if status eq 0 then $
        se   = reverse(sort(eval)) $
     else begin
        se = pol-1
        if n_elements(evec[0, *]) eq pol && product(failed-se) ne 0 then $
           status = 0 $
        else begin
           eval = la_eigenql(transpose(Hth), eigenvectors=evec, status=status, method=1)
           if status ne 0 then $
              eval = la_eigenql(transpose(Hth), eigenvectors=evec, status=status, method=2)
           if status eq 0 then se = pol-1 $
           else begin
              max_evec = evec[*, 0]
              return, 0
           endelse
        endelse
     endelse 
     for bl=0,n_bl-1 do $
        th2[bl] = atan(conj(transpose(evec[*,se[0]])) # A[*,*,bl] # evec[*,se[0]],/phase)
     if max_eval[0] lt eval[se[0]] then begin
        max_eval = eval[se]
        max_evec = evec[*,se]
        no_update=0
     endif else no_update++
     if keyword_set(DEBUG) then $
        print,strcompress(i)+' th: '+strjoin(strcompress(th))+' th2: '+strjoin(strcompress(th2))+ $
              ' evalMAX: '+strcompress(abs(eval[se[0]]))+' evalALL:'+strjoin(strcompress(eval))
  endrep until no_update ge 1 || ++i ge MAX_ITER || (use_theta_threshold? max(abs(th+th2)) le EPS: 0)
  if keyword_set(DEBUG) then $
     print,strcompress(i)+'iterations th0: '+strjoin(strcompress(th0))+' th2: '+strjoin(strcompress(th2))+ $
           ' numRAD: '+strcompress((max_eval[0]))+' evalALL:'+strjoin(strcompress(max_eval))
  return, abs(eval[0])
end
function mb_opt_nr, T, MAGNITUDE=magnitude, $
                    ORTHOGONAL_BASIS=ORTHOGONAL_BASIS, $
                    GLOBALLY_ORTHOGONAL_BASIS=GLOBALLY_ORTHOGONAL_BASIS, $
                    NORMALIZED=NORMALIZED, DEBUG=DEBUG, $
                    SM = SM,  POL_NR=pol, TRACKS_NR=n_tr, BL_NR=n_bl

  ORTHOGONAL_BASIS	    = n_elements(ORTHOGONAL_BASIS)	    eq 0? 0: ORTHOGONAL_BASIS
  GLOBALLY_ORTHOGONAL_BASIS = n_elements(GLOBALLY_ORTHOGONAL_BASIS) eq 0? 1: GLOBALLY_ORTHOGONAL_BASIS
  siz  = size(T)
  if n_elements(pol) eq 0 then pol  = siz[1] mod 3 eq 0? 3: stop
  if n_elements(n_tr) eq 0 then n_tr = siz[1] / pol
  if n_elements(n_bl) eq 0 then n_bl = mb_nr_baselines(n_tr)
  n    = [siz[2:siz[0]],lonarr(8-siz[0])+1] & n=n[1:*]
  coh  = make_array([pol,siz[0]eq 2?n_bl:[n_bl,siz[3:siz[0]]]],type=siz[siz[0]+1],/nozero)
  A    = make_array(pol,pol,n_bl,type=siz[siz[0]+1],/nozero)
  U    = make_array(pol*n_tr,pol*n_tr,type=siz[siz[0]+1])
  if n_bl eq 1 then A=reform(A,pol,pol,n_bl,/overwrite)
  bltr = mb_ind(indgen(n_bl))
  if arg_present(SM) then sm = make_array([pol,pol,(siz[0]le 2?1:siz[3:siz[0]])],type=siz[siz[0]+1],/nozero)

  for q=0,n[5]-1 do $
     for p=0,n[4]-1 do $
        for l=0,n[3]-1 do $
           for k=0,n[2]-1 do $
              for j=0,n[1]-1 do $
                 for i=0,n[0]-1 do begin
     Tx   = T[*,*,i,j,k,l,p,q]
     Tsi  = make_array(pol,pol,type=siz[siz[0]+1])
     for tr=0,n_tr-1 do $
        Tsi += Tx[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1]
     Tsi /= n_tr
     Tsi = mm_power(Tsi,/sqrt,/invert)
     for bl=0,n_bl-1 do $
        A[*,*,bl]=Tsi # Tx[bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1,bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1] # Tsi
     nr0 = mb_NumRad(A,evec=evec,DEBUG=DEBUG)
     w   = Tsi # evec

     if ORTHOGONAL_BASIS then begin
        PB  = evec
        for bl=0,n_bl-1 do $
           A[*,*,bl]= conj(transpose(PB)) # A # PB
        A[*,0,*] = 0.
        A[0,*,*] = 0.
        nr1 = mb_NumRad(A,evec=evec,DEBUG=DEBUG)
        w[*,1] = Tsi # PB # evec[*,0]
        w[*,2] = Tsi # PB # evec[*,1]
     endif else if GLOBALLY_ORTHOGONAL_BASIS then begin
        PB = ortho_basis(w,/unitary)
        for tr=0,n_tr-1 do U[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1]= PB
        Tbx   = conj(transpose(U)) # T[*,*,i,j,k,l,p,q] # U
        Tbx[lindgen(n_tr)*pol,*] = 0.
        Tbx[*,lindgen(n_tr)*pol] = 0.
        Tsi  = make_array(pol,pol,type=siz[siz[0]+1])
        for tr=0,n_tr-1 do $
           Tsi += Tbx[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1]
        Tsi /= n_tr
        Tsi = mm_power(Tsi,/sqrt,/invert)
        for bl=0,n_bl-1 do $
           A[*,*,bl]=Tsi # Tbx[bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1,bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1] # Tsi
        nr1 = mb_NumRad(A,evec=evec,DEBUG=DEBUG)
        w[*,1] = pb # Tsi # evec[*,0]
        w = ortho_basis(w,/unitary)
     endif
     if n_elements(SM) gt 0 then sm[*,*,i,j,k,l,p,q]=w

     wH = conj(transpose(w))
     for bl=0,n_bl-1 do $
        coh[0,bl,i,j,k,l,p,q] = diag_matrix((wH # Tx[bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1,bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1] # w) $
                                            /sqrt((wH # Tx[bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1,bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1] # w) $
                                                  *(wH # Tx[bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1,bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1] # w)))
     if keyword_set(DEBUG) then $
        print,'Maximal NR0       : '+strjoin(strcompress(nr0)), $
              'Maximal coherences: '+strjoin(strjoin(strcompress(abs(coh[0,*,i,j,k,l,p,q])))), $
              'AS1               : '+strcompress(mean(abs(coh[0,*,i,j,k,l,p,q])))
  endfor
;stop
  return, keyword_set(magnitude)? abs(coh): coh
end
function mb_opt_nr_old, T, MAGNITUDE=magnitude, $
                    ORTHOGONAL_BASIS=ORTHOGONAL_BASIS, $
                    NORMALIZED=NORMALIZED, DEBUG=DEBUG

  ORTHOGONAL_BASIS = n_elements(ORTHOGONAL_BASIS) eq 0? 1: ORTHOGONAL_BASIS
  siz  = size(T)
  pol  = siz[1] mod 3 eq 0? 3: stop
  n_tr = siz[1] / pol
  n_bl = mb_nr_baselines(n_tr)
  n    = [siz[2:siz[0]],lonarr(8-siz[0])+1] & n=n[1:*]
  coh  = make_array([pol,siz[0]eq 2?n_bl:[n_bl,siz[3:siz[0]]]],type=siz[siz[0]+1],/nozero)
  A    = make_array(pol,pol,n_bl,type=siz[siz[0]+1],/nozero)
  bltr = mb_ind(indgen(n_bl))
  for q=0,n[5]-1 do $
     for p=0,n[4]-1 do $
        for l=0,n[3]-1 do $
           for k=0,n[2]-1 do $
              for j=0,n[1]-1 do $
                 for i=0,n[0]-1 do begin
     Tx   = T[*,*,i,j,k,l,p,q]
     Tsi  = make_array(pol,pol,type=siz[siz[0]+1])
     for tr=0,n_tr-1 do $
        Tsi += Tx[tr*pol:(tr+1)*pol-1,tr*pol:(tr+1)*pol-1]
     Tsi /= n_tr
     Tsi = mm_power(Tsi,/sqrt,/invert)
     for bl=0,n_bl-1 do $
        A[*,*,bl]=Tsi # Tx[bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1,bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1] # Tsi
     nr0 = mb_NumRad(A,evec=evec,DEBUG=DEBUG)
     w   = Tsi # evec
     if ORTHOGONAL_BASIS then begin
        PB  = evec
        for bl=0,n_bl-1 do $
           A[*,*,bl]= conj(transpose(PB)) # A # PB
        A[*,0,*] = 0.
        A[0,*,*] = 0.
        nr1 = mb_NumRad(A,evec=evec,DEBUG=DEBUG)
        w[*,1] = Tsi # PB # evec[*,0]
        w[*,2] = Tsi # PB # evec[*,1]
     endif
     wH = conj(transpose(w))
     for bl=0,n_bl-1 do $
        coh[0,bl,i,j,k,l,p,q] = diag_matrix((wH # Tx[bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1,bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1] # w) $
                                            /sqrt((wH # Tx[bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1,bltr[0,bl]*pol:(bltr[0,bl]+1)*pol-1] # w) $
                                                  *(wH # Tx[bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1,bltr[1,bl]*pol:(bltr[1,bl]+1)*pol-1] # w)))
     if keyword_set(DEBUG) then $
        print,'Maximal NR0       : '+strjoin(strcompress(nr0)), $
              'Maximal coherences: '+strjoin(strjoin(strcompress(abs(coh[0,*,i,j,k,l,p,q])))), $
              'AS1               : '+strcompress(mean(abs(coh[0,*,i,j,k,l,p,q])))
  endfor
  return, keyword_set(magnitude)? abs(coh): coh
end
function mb_C2T, C, REVERSED=REVERSED, OVERWRITE=OVERWRITE
  siz  = size(C)
  pol  = siz[1] mod 3 eq 0? 3L: 4L
  n_tr = siz[1] /pol
  n    = siz[0]le 2? lonarr(6)+1: [siz[3:siz[0]],lonarr(8-siz[0])+1]
  j      = complex(0,1)
  D3     = 1./sqrt(2.) * [ $
           [ 1,  1,   0     ], $
           [ 1, -1,   0     ], $
           [ 0,  0, sqrt(2.)]]
  D4     = 1./sqrt(2.) * [ $
           [ 1, 1, 0, 0 ], $
           [ 1,-1, 0, 0 ], $
           [ 0, 0, 1, j ], $
           [ 0, 0, 1,-j ]]
  if pol eq 3 then D1p = D3 else D1p = D4
  D = complexarr(pol*n_tr,pol*n_tr)
  for i=0,n_tr-1 do D[i*pol:(i+1)*pol-1,i*pol:(i+1)*pol-1]=D1p
  if ~keyword_set(REVERSED) then $ ; from lex to pauli
     Di = conj(transpose(D)) $
  else begin                    ; from pauli to lex
     Di = D
     D  = conj(transpose(Di))
  endelse
  if keyword_set(OVERWRITE) then $
     for q=0,n[5]-1 do $
        for p=0,n[4]-1 do $
           for l=0,n[3]-1 do $
              for k=0,n[2]-1 do $
                 for j=0,n[1]-1 do $
                    for i=0,n[0]-1 do $
                       C[*,*,i,j,k,l,p,q] = D # C[*,*,i,j,k,l,p,q] # Di $
  else begin
     T = complexarr(siz[1:siz[0]],/nozero)
     for q=0,n[5]-1 do $
        for p=0,n[4]-1 do $
           for l=0,n[3]-1 do $
              for k=0,n[2]-1 do $
                 for j=0,n[1]-1 do $
                    for i=0,n[0]-1 do $
                       T[*,*,i,j,k,l,p,q] = D # C[*,*,i,j,k,l,p,q] # Di
  endelse
  return, keyword_set(OVERWRITE)? C: T
end
function MB_cc, T, w, MAGNITUDE=magnitude, PHASE=phase
  siz  = size(T)
  pol  = siz[1]mod 3 eq 0? 3L: 4L
  n_tr = siz[1]/pol
  n_bl = n_tr*(n_tr-1)/2
  cc   = complexarr(siz[0]le 2?[pol,n_bl]:[pol,n_bl,siz[3:siz[0]]],/nozero)
  tracks=mb_ind(indgen(n_bl))
  if n_elements(w) eq 0 then $
     for bl=0,n_bl-1 do $
        cc[*,bl,*,*,*,*,*,*] $
     = mm_diag(T[tracks[0,bl]*pol:(tracks[0,bl]+1)*pol-1, $
                 tracks[1,bl]*pol:(tracks[1,bl]+1)*pol-1,*,*]/ $
               sqrt(T[tracks[0,bl]*pol:(tracks[0,bl]+1)*pol-1, $
                      tracks[0,bl]*pol:(tracks[0,bl]+1)*pol-1,*,*]* $
                    T[tracks[1,bl]*pol:(tracks[1,bl]+1)*pol-1, $
                      tracks[1,bl]*pol:(tracks[1,bl]+1)*pol-1,*,*]))
  if n_elements(w) ne 0 then begin
     wsiz=size(w)
     for bl=0,n_bl-1 do begin
        if wsiz[1] eq pol && wsiz[2] eq n_tr then begin ;; diff pol's
           w1 = w[*,tracks[0,bl],*,*,*,*,*,*]
           w2 = w[*,tracks[1,bl],*,*,*,*,*,*]
           w1=mm_herm(w1)
           if wsiz[0] eq 2 then w2=reform(w2,pol,1,/overwrite)
        endif else begin ;; same polarization
           w2 = reform(w,[pol,wsiz[0]eq 1?1:[1,wsiz[2:wsiz[0]]]])
           w1 = mm_herm(w2)
        endelse
        cc[*,bl,*,*,*,*,*,*] $
           = mm_diag(mm_mm(mm_mm(w1,T[tracks[0,bl]*pol:(tracks[0,bl]+1)*pol-1,$
               tracks[1,bl]*pol:(tracks[1,bl]+1)*pol-1,*,*]),w2)/ $
             sqrt(mm_mm(mm_mm(w1,T[tracks[0,bl]*pol:(tracks[0,bl]+1)*pol-1, $
               tracks[0,bl]*pol:(tracks[0,bl]+1)*pol-1,*,*]),w1)*$
             mm_mm(mm_mm(w2,T[tracks[1,bl]*pol:(tracks[1,bl]+1)*pol-1, $
               tracks[1,bl]*pol:(tracks[1,bl]+1)*pol-1,*,*]),w2)))
     endfor
  endif
  return, keyword_set(MAGNITUDE)?abs(cc): $
          keyword_set(PHASE)?atan(cc,/phase): cc 
end
function automatic_file_select, path=path, choice=choice, CASE_SENSITIVE=CASE_SENSITIVE
  if n_elements(path) eq 0 then path=file_dirname(choice[0],/M)
  f = file_search(path+'*')
  for i=0,n_elements(choice)-1 do $
     for j=0,n_elements(f)-1 do $
        if strcmp(file_basename(f[j]),file_basename(choice[i]),FOLD=~keyword_set(CASE_SENSITIVE)) then return, path+choice[i]
  return, ''
end
