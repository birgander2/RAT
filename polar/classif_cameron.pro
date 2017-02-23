;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: decomp_pauli
; written by    : Andreas Reigber
; last revision : 14.Feb.2003
; Calculates polarimetric Pauli decomposition
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

function dist_cam,z,zref
	return,abs(1+conj(z)*zref)/sqrt(1+abs(z)^2)/sqrt(1+abs(zref)^2)
end

;  function calcul_matrix_rotation,psi
;  
;  	;calcul size psi
;  	xdim = (size(psi))[1]
;  	ydim = (size(psi))[2]
;  	
;  	;calcul rotation matrix
;  	MatR = dblarr(4,4,xdim,ydim)
;  	
;  	MatR[0,0,*,*] = cos(psi) * cos(psi) & MatR[1,0,*,*] = - cos(psi) * sin(psi) & MatR[2,0,*,*] = - sin(psi) * cos(psi) & MatR[3,0,*,*] =   sin(psi) * sin(psi)
;  	MatR[0,1,*,*] = cos(psi) * sin(psi) & MatR[1,1,*,*] =   cos(psi) * cos(psi) & MatR[2,1,*,*] = - sin(psi) * sin(psi) & MatR[3,1,*,*] = - sin(psi) * cos(psi)
;  	MatR[0,2,*,*] = sin(psi) * cos(psi) & MatR[1,2,*,*] = - sin(psi) * sin(psi) & MatR[2,2,*,*] =   cos(psi) * cos(psi) & MatR[3,2,*,*] = - cos(psi) * sin(psi)
;  	MatR[0,3,*,*] = sin(psi) * sin(psi) & MatR[1,3,*,*] =   sin(psi) * cos(psi) & MatR[2,3,*,*] =   cos(psi) * sin(psi) & MatR[3,3,*,*] =   cos(psi) * cos(psi)
;  	
;  	
;  	return,MatR
;  ;  	SizeVec1 = size(Vec1)
;  ;  	SizeVec2 = size(Vec2)
;  ;  	
;  ;  	
;  ;  	
;  ;  	if (size(Vec1))[0] eq 1 and (size(Vec2))[0] eq 1 then return,total(Vec1*conj(Vec2))
;  end

pro classif_cameron,CALLED=called
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

; check if array is usable
	close,/all
	if file.type ne 200 then begin
		error_button = DIALOG_MESSAGE(['Data have to be a polarimetric vector in','lexicographic basis'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
	
; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,2l],info=info,type=410l

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='Cameron Decomposition...',/cancel_button

; calculating Cameron decomposition
	
	;define the reciprocity matrix
	Prec = [$
		[1,0,  0,  0],$
		[0,0.5,0.5,0],$
		[0,0.5,0.5,0],$
		[0,  0,  0,1]]
	
	;Define the Pauli basis vector
	VecSa = (1./sqrt(2.)) * [1,0,0,1]
	VecSb = (1./sqrt(2.)) * [1,0,0,-1]
	VecSc = (1./sqrt(2.)) * [0,1,1,0]
	VecSd = (1./sqrt(2.)) * [0,-1,1,0]
	
	; --> define some variables
	HH = 0
	HV = 1
	VH = 2
	VV = 3
	
	;--> Define z of basic scatterer
	z_trihedral =  1.0
	z_diplane   = -1.0
	z_dipole    =  0.0
	z_cylinder  =  0.5
	z_narrow    = -0.5
	z_wave_up   =  complex( 0.0, 1.0)
	z_wave_dn   =  complex( 0.0,-1.0)
		
	for i=0,anz_blocks-1 do begin
		
		;define theInput block
		block = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
		
		
		; define some variables
		Svec = dcomplexarr(file.zdim,file.xdim,blocksizes[i]) ;--> Scattering vector
		
		;Define the output block	
		Vec = dcomplexarr(file.zdim,file.xdim,blocksizes[i]) ;--> Scattering vector
		;NormVec = dblarr(file.xdim,blocksizes[i]) ;--> Norm of the scattering vector
		NormVec = dcomplexarr(file.zdim,file.xdim,blocksizes[i])
		VecRec = dcomplexarr(file.zdim,file.xdim,blocksizes[i])
		NormVecRec = dcomplexarr(file.zdim,file.xdim,blocksizes[i])
		;xsi = dblarr(file.xdim,blocksizes[i])
		
		SinXsi = dblarr(file.xdim,blocksizes[i])
		SinXsi2 = dblarr(file.xdim,blocksizes[i])
		CosXsi2 = dblarr(file.xdim,blocksizes[i])
		TanXsi = dblarr(file.xdim,blocksizes[i])
		tau = dblarr(file.xdim,blocksizes[i])
		VecSp = dcomplexarr(file.zdim,file.xdim,blocksizes[i]) ;--> Scattering vector
		VecSD = dcomplexarr(file.zdim,file.xdim,blocksizes[i]) ;--> Scattering vector
		MatR = dblarr(file.zdim,file.zdim,file.xdim,blocksizes[i])
		VecL = dcomplexarr(file.zdim,file.xdim,blocksizes[i])
		VecLP = dcomplexarr(file.zdim,file.xdim,blocksizes[i])
		VecLM = dcomplexarr(file.zdim,file.xdim,blocksizes[i])
		VecTmp = dcomplexarr(file.xdim,blocksizes[i])

		out   = make_array([file.xdim,blocksizes[i]],type=2l)
		
		readu,ddd,block
		
		; --> Update progress bar with cancel button
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		;--> define scattering vector
		Svec[HH,*,*] = block[0,*,*] ;SHH
		Svec[HV,*,*] = block[2,*,*] ;SHV
		Svec[VH,*,*] = block[3,*,*] ;SVH
		Svec[VV,*,*] = block[1,*,*] ;SVV
		
		;                            -->               -->
		;--> Calculate the projection VecRec = [Prec] . Svec
		VecRec[0,*,*] = Prec[0,0]*Svec[HH,*,*] + Prec[1,0]*Svec[HV,*,*] + Prec[2,0]*Svec[VH,*,*] + Prec[3,0]*Svec[VV,*,*]
		VecRec[1,*,*] = Prec[0,1]*Svec[HH,*,*] + Prec[1,1]*Svec[HV,*,*] + Prec[2,1]*Svec[VH,*,*] + Prec[3,1]*Svec[VV,*,*]
		VecRec[2,*,*] = Prec[0,2]*Svec[HH,*,*] + Prec[1,2]*Svec[HV,*,*] + Prec[2,2]*Svec[VH,*,*] + Prec[3,2]*Svec[VV,*,*]
		VecRec[3,*,*] = Prec[0,3]*Svec[HH,*,*] + Prec[1,3]*Svec[HV,*,*] + Prec[2,3]*Svec[VH,*,*] + Prec[3,3]*Svec[VV,*,*]
		
		;                            -->
		;--> Calculate the norm of || VecRec ||
		NormVecRec = sqrt( abs(reform(VecRec[1,*,*])^2 + reform(VecRec[1,*,*])^2 + reform(VecRec[2,*,*])^2 + reform(VecRec[3,*,*])^2 ))
		
		;--> Extraction of the reciprocity parameter THrec
		THrec = acos(NormVecRec)
				
		;--> Calcul of aa,bb,gg,dd : [S] = aa[Sa] + bb[Sb] + gg[Sc] + dd[Sd]
		r2 = (1./sqrt(2))
		aa = r2 * (Svec[HH,*,*] + Svec[VV,*,*])
		bb = r2 * (Svec[HH,*,*] - Svec[VV,*,*])
		gg = r2 * (Svec[HV,*,*] + Svec[VH,*,*])
		dd = r2 * (Svec[VH,*,*] - SVec[HV,*,*])
		
		;--> Estimation of xsi
		test1 = reform(real_part(bb * conj(gg) + conj(bb) * gg))
		test2 = reform(abs(bb)^2 - abs(gg)^2)
		SinXSI = test1 / sqrt(test1^2 + test2^2)
		CosXSI = test2 / sqrt(test1^2 + test2^2)	
		rmnanq,SinXSI
		rmnanq,CosXSI
		
		indxsi = where (((abs(test1) lt (aa^2 + bb^2 + gg^2)*1e-6) and ( abs(test2) lt (aa^2 + bb^2 + gg^2)*1e-6)),count)
		
		if count ne 0 then begin
			SinXSI[indxsi] = 0.
			CosXSI[indxsi] = 1.
		endif
		
		xsi = atan(sinxsi,cosxsi)

		indxsipos = where(CosXSI le 0, count)
		if count ne 0 then xsi[indxsipos] = !pi - asin(SinXSI[indxsipos])
		
		;--> Calcul de S'
		VecSp[0,*,*] = cos(xsi/2)*VecSb[0] + sin(xsi/2)*VecSc[0]
		VecSp[1,*,*] = cos(xsi/2)*VecSb[1] + sin(xsi/2)*VecSc[1]
		VecSp[2,*,*] = cos(xsi/2)*VecSb[2] + sin(xsi/2)*VecSc[2]
		VecSp[3,*,*] = cos(xsi/2)*VecSb[3] + sin(xsi/2)*VecSc[3]
		
		ee = reform(total(Svec*conj(VecSp),1))
		
		VecSD[0,*,*] = aa*VecSa[0] + ee*reform(VecSp[0,*,*])
		VecSD[1,*,*] = aa*VecSa[1] + ee*reform(VecSp[1,*,*])
		VecSD[2,*,*] = aa*VecSa[2] + ee*reform(VecSp[2,*,*])
		VecSD[3,*,*] = aa*VecSa[3] + ee*reform(VecSp[3,*,*])
		
		
		;--> Estimation of tau: symmetric
		TauNum = total(VecRec*conj(VecSD),1)
		TauDen = (sqrt(total(abs(VecRec)^2,1)) * sqrt(total(abs(VecSD)^2,1)));+1e-6
		tau = acos(abs(TauNum/TauDen))
		
		;--> Diagonalisation angle: psi
		psi = -(1./4.) * xsi
		
		;--> Diagonalisation
		Sd_1 = cos(psi)^2 * reform(VecRec[0,*,*]) + $
		       sin(2*psi) * reform(VecRec[1,*,*]) + $
		       sin(psi)^2 * reform(VecRec[3,*,*])
		
		Sd_2 = sin(psi)^2 * reform(VecRec[0,*,*]) + $
		       sin(2*psi) * reform(VecRec[1,*,*]) + $
		       cos(psi)^2 * reform(VecRec[3,*,*])
				 
		ind_2_gt_1 = where(abs(Sd_2) gt abs(Sd_1), cc)
		if cc ne 0 then begin
			psi[ind_2_gt_1] += !pi/2.
			ind_tmp = where(psi gt (!pi/2),count)
			psi[ind_tmp] -= !pi
		endif
		
		;--> calcul of z
		z = Sd_1/Sd_2
		if cc ne 0 then z[ind_2_gt_1] = Sd_2[ind_2_gt_1]/Sd_1[ind_2_gt_1]
		
		;--> Calcul of distance
		dd_trihedral = dist_cam(z,z_trihedral)
		dd_diplane   = dist_cam(z,z_diplane)
		dd_dipole	 = dist_cam(z,z_dipole)
		dd_cylinder  = dist_cam(z,z_cylinder)
		dd_narrow	 = dist_cam(z,z_narrow)
		dd_wave_up   = dist_cam(z,z_wave_up)
		dd_wave_dn   = dist_cam(z,z_wave_dn)

		out *= 0
		
		ind_trihedral = where((dd_trihedral gt dd_diplane)   and (dd_trihedral gt dd_dipole)	 and (dd_trihedral gt dd_cylinder)  and (dd_trihedral gt dd_narrow)    and (dd_trihedral gt dd_wave_up)   and (dd_trihedral gt dd_wave_dn),   cc_trihedral)
		ind_diplane   = where((dd_diplane	gt dd_trihedral) and (dd_diplane   gt dd_dipole)	 and (dd_diplane   gt dd_cylinder)  and (dd_diplane	gt dd_narrow)    and (dd_diplane   gt dd_wave_up)   and (dd_diplane   gt dd_wave_dn),   cc_diplane  )
		ind_dipole    = where((dd_dipole 	gt dd_diplane)   and (dd_dipole    gt dd_trihedral) and (dd_dipole	 gt dd_cylinder)  and (dd_dipole 	gt dd_narrow)    and (dd_dipole    gt dd_wave_up)   and (dd_dipole	 gt dd_wave_dn),   cc_dipole   )
		ind_cylinder  = where((dd_cylinder  gt dd_diplane)   and (dd_cylinder  gt dd_dipole)	 and (dd_cylinder  gt dd_trihedral) and (dd_cylinder  gt dd_narrow)    and (dd_cylinder  gt dd_wave_up)   and (dd_cylinder  gt dd_wave_dn),   cc_cylinder )
		ind_narrow    = where((dd_narrow 	gt dd_diplane)   and (dd_narrow    gt dd_dipole)	 and (dd_narrow	 gt dd_cylinder)  and (dd_narrow 	gt dd_trihedral) and (dd_narrow    gt dd_wave_up)   and (dd_narrow	 gt dd_wave_dn),   cc_narrow   )
		ind_wave_up   = where((dd_wave_up	gt dd_diplane)   and (dd_wave_up   gt dd_dipole)	 and (dd_wave_up   gt dd_cylinder)  and (dd_wave_up	gt dd_narrow)    and (dd_wave_up   gt dd_trihedral) and (dd_wave_up   gt dd_wave_dn),   cc_wave_up  )
		ind_wave_dn   = where((dd_wave_dn	gt dd_diplane)   and (dd_wave_dn   gt dd_dipole)	 and (dd_wave_dn   gt dd_cylinder)  and (dd_wave_dn	gt dd_narrow)    and (dd_wave_dn   gt dd_wave_up)   and (dd_wave_dn   gt dd_trihedral), cc_wave_dn  )
		
		if cc_trihedral ne 0 then out[ind_trihedral] = 1
		if cc_diplane   ne 0 then out[ind_diplane]   = 2
		if cc_dipole    ne 0 then out[ind_dipole]    = 3
		if cc_cylinder  ne 0 then out[ind_cylinder]  = 4
		if cc_narrow    ne 0 then out[ind_narrow]    = 5
		if cc_wave_up   ne 0 then out[ind_wave_up]   = 6
		if cc_wave_dn   ne 0 then out[ind_wave_dn]   = 7
		
		;test if not reciprocal
		ind_nr = where(THrec gt (!pi/4), cc)
		if cc ne 0 then out[ind_nr] = 0
		
		;test if not symetric
		ind_ns = where(tau gt (!pi/0), cc)
		if cc ne 0 then out[ind_ns] = 0
		
				
				
				
		;rmnanq,PsiP
		writeu,eee,out
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 410l
	file.dim  = 2l
	file.vdim = 1l
	file.zdim = 1l
	file.var  = 2l
	
; set palette
	
	palettes[0,*,*] = palettes[7,*,*]   ; palettes 7 = system 10 classes
	palettes[1,*,*] = palettes[7,*,*]   ; to actual and suggestion
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
