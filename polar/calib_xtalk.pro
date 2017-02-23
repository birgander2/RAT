;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calib_xtalk
; written by    : Stephane Guillaso
; last revision : 20. September 2004
; Estimate and remove cross-talk parameters
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

pro calib_xtalk
	common rat, types, file, wid, config

; ---- CHECK IF ARRAY IS USABLE ----

	if file.type ne 200 then begin
		error_button = DIALOG_MESSAGE(['Data has to be a','lexicographic scattering vector'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	if file.zdim ne 4 then begin
		error_button = DIALOG_MESSAGE(['Data has to be','4 parameter vector','HH,VV,HV,VH'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

;------------------------------------------------------------
; change mousepointer to hourglass (FIXED)
;------------------------------------------------------------

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; ---- SAVE THE ACTUAL LEXICOGRAPHIC SCATTERING VECTOR ----
;  	file_copy,file.name,config.tempdir+'k4_tmp.rat',/overwrite

; ---- CALCULATE THE COVARIANCE MATRIX ----	
;  	k_to_m,/called

; ---- FILTER THE COVARIANCE MATRIX ----
;  	speck_polmean,/called,smmx=2,ymmx=32

; ---- DEFINE SOME PARAMETERS ----
	smmx = 2
	smmy = 32
	filter = fltarr(smmx,smmy)+1.
	hh=0 & hv=2 & vh=3 & vv=1
	eps=1e-10
	xc_limit = 0.3 ; HH-HV and VV-VH correlation limit
	
;  	file_alpha = config.tempdir+'alpha.rat'
;  	file_alpha1 = config.tempdir+'alpha1.rat'
;  	file_alpha2 = config.tempdir+'alpha2.rat'
;  	file_u = config.tempdir+'u.rat'
;  	file_v = config.tempdir+'v.rat'
;  	file_w = config.tempdir+'w.rat'
;  	file_z = config.tempdir+'z.rat'

; ---- CALCULATING PREVIOUS SIZE & NUMBER OF BLOCKS (USING OVERLAP) ----
;  	bs = config.blocksize
;  	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
;  	blocksizes = intarr(anz_blocks) + bs
;  	blocksizes[anz_blocks-1] = bs_last
	bs = config.blocksize
	overlap = (smmy + 1) / 2
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end

	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos

; ---- READ/WRITE HEADER FILE ----
	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type	
	
;  	srat,file_alpha,aaa,header=[2l,file.xdim,file.ydim,file.var]

;-------------------------------------------------------------------------------
;       							   CROSS-TALK ESTIMATION
;-------------------------------------------------------------------------------
	
; ---- DEFINE SOME VARIABLES ----
;	alpha_rg = dblarr(file.xdim)
;	alpha1_rg = dblarr(file.xdim)
;	alpha2_rg = dblarr(file.xdim)
	alpha = complexarr(file.xdim,file.ydim)
	alpha1 = complexarr(file.xdim,file.ydim)
	alpha2 = complexarr(file.xdim,file.ydim)
	u = complexarr(file.xdim,file.ydim)
	v = complexarr(file.xdim,file.ydim)
	w = complexarr(file.xdim,file.ydim)
	z = complexarr(file.xdim,file.ydim)
	mask_cross = fltarr(file.xdim)
;  	u_rg = dblarr(file.xdim)
;  	v_rg = dblarr(file.xdim)
;  	w_rg = dblarr(file.xdim)
;  	z_rg = dblarr(file.xdim)
;  	mask_alpha_nelem = 0l
;  	mask_cross_nelem = 0l
	

; ---- POP UP PROGRESS WINDOW ----
	progress, MESSAGE='X-talk estimation...',/cancel_button

; ---- START BLOCK PROCESSING ----
	for ii=0,anz_blocks-1 do begin
		
		;print,ii+1,'/',anz_blocks
		progress,percent=(ii+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		if ii eq anz_blocks-1 then ypos2 = bs_last
		
		size_bloc = ypos2-ypos1
		
		; - define a block to read data
		C = make_array([4,4,file.xdim,blocksizes[ii]],type=file.var)
		K = make_array([file.zdim,file.xdim,blocksizes[ii]],type=file.var)
		
		
		; - read the scattering vector
		readu,ddd,K
		for xx=0,file.xdim-1 do for yy=0,blocksizes[ii]-1 do C[*,*,xx,yy] = transpose(k[*,xx,yy]) ## conj(k[*,xx,yy])

; ---- FILTER THE COVARIANCE MATRIX ----
		for vvv=0,file.zdim-1 do begin
			for zzz=0,file.zdim-1 do begin
				C[vvv,zzz,*,*] = convol(reform(C[vvv,zzz,*,*]),filter,n_elements(filter))
			endfor
		endfor

		
; ---- CROSS CORRELATION CHECK ----
		; HH-HV correlation
		hhhv_c = abs(reform(C[hh,hv,*,*]))
		hhhh_c = abs(reform(C[hh,hh,*,*]))
		hvhv_c = abs(reform(C[hv,hv,*,*]))
		cor_hhhv = hhhv_c/sqrt(hhhh_c*hvhv_c+eps)
	
		; VV-VH correlation
		vvvh_c = abs(reform(C[vv,vh,*,*]))
		vvvv_c = abs(reform(C[vv,vv,*,*]))
		vhvh_c = abs(reform(C[vh,vh,*,*]))
		cor_vvvh = vvvh_c/sqrt(vvvv_c*vhvh_c+eps)
		
; ---- CHECK CROSS-CORRELATION ----
		ind_cross = where(median(cor_hhhv,3) gt xc_limit and median(cor_vvvh,3) gt xc_limit, count)
		mask_cross_tmp = fltarr(file.xdim,blocksizes[ii])+1.
		if count ne 0 then mask_cross_tmp[ind_cross] = 0
		mask_cross = [[mask_cross],[mask_cross_tmp[*,ypos1:ypos2-1]]]

; ---- CROSS-TALK RATIO ESTIMATION ----
		; estimation of delta (page 91)
		delta = abs(reform(C[hh,hh,*,*]))*abs(reform(C[vv,vv,*,*])) - abs(reform(C[hh,vv,*,*]))^2 > eps
	
		; estimation of cross-talk ration (page 91)
		u_tmp = reform(C[vv,vv,*,*]*C[hh,vh,*,*] - C[hh,vv,*,*]*C[vv,vh,*,*])/delta ; eq. 22
		v_tmp = reform(C[hh,hh,*,*]*C[vv,vh,*,*] - C[hh,vh,*,*]*C[vv,hh,*,*])/delta ; eq. 23
		z_tmp = reform(C[vv,vv,*,*]*C[hh,hv,*,*] - C[hh,vv,*,*]*C[vv,hv,*,*])/delta ; eq. 24
		w_tmp = reform(C[hh,hh,*,*]*C[vv,hv,*,*] - C[hh,hv,*,*]*C[vv,hh,*,*])/delta ; eq. 25

; ---- CHANNEL IMBALANCE RATIO ESTIMATION ----'
		X = reform(C[vh,hv,*,*]) - z_tmp*reform(C[vh,hh,*,*]) - w_tmp*reform(C[vh,vv,*,*]) ; eq. 27
	
		; first alpha: alpha1
		alpha1_tmp = (reform(C[vh,vh,*,*]) - u_tmp*reform(C[vh,hh,*,*]) - v_tmp*reform(C[vh,vv,*,*])) / (X + eps) ; eq. 26
		beta1  = abs(X)^2 / (reform(C[vh,vh,*,*]) - u_tmp*reform(C[vh,hh,*,*]) - v_tmp*reform(c[vh,vv,*,*]) + eps) ; eq. 28

		; second alpha: alpha2
		alpha2_tmp = conj(X) / (reform(C[hv,hv,*,*]) - conj(z_tmp)*reform(C[hh,hv,*,*]) - conj(w_tmp)*reform(C[vv,hv,*,*]) +eps) ; eq. 29
		beta2  = reform(C[hv,hv,*,*]) - conj(z_tmp)*reform(C[hh,hv,*,*]) - conj(w_tmp)*reform(C[vv,hv,*,*]) ; eq. 30
	
;  ; ---- CHECK CHANNEL IMBALANCE RATIO ----
;  		diff = abs(conj(alpha1_tmp)*float(beta1) - conj(alpha2_tmp)*float(beta2))
	
; ---- GENERATE THE TRUE ALPHA ----
		abs_alpha = (abs(alpha1_tmp*alpha2_tmp)-1+sqrt((abs(alpha1_tmp*alpha2_tmp)-1)^2 + 4*abs(alpha2_tmp)^2))/(2*abs(alpha2_tmp))
		alpha_tmp = abs_alpha * exp(complex(0,atan(alpha2_tmp+eps,/phase)))
		
		;--> out
		alpha[*,ii*size_bloc:ii*size_bloc+size_bloc-1]  = alpha_tmp[*,ypos1:ypos2-1]
		alpha1[*,ii*size_bloc:ii*size_bloc+size_bloc-1] = alpha1_tmp[*,ypos1:ypos2-1]
		alpha2[*,ii*size_bloc:ii*size_bloc+size_bloc-1] = alpha2_tmp[*,ypos1:ypos2-1]

		u[*,ii*size_bloc:ii*size_bloc+size_bloc-1]  = u_tmp[*,ypos1:ypos2-1]
		v[*,ii*size_bloc:ii*size_bloc+size_bloc-1]  = v_tmp[*,ypos1:ypos2-1]
		w[*,ii*size_bloc:ii*size_bloc+size_bloc-1]  = w_tmp[*,ypos1:ypos2-1]
		z[*,ii*size_bloc:ii*size_bloc+size_bloc-1]  = z_tmp[*,ypos1:ypos2-1]
	
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
	endfor
	free_lun,ddd
	
	;--> Generate the alpha mask
	mask_alpha = fltarr(file.xdim,file.ydim)
	
	;--> alpha range profile estimation
	dummy = where(finite(alpha) ne 0, count)
	if count ne 0 then begin
		alpha_stat = moment(abs(alpha[dummy]), sdev=alpha_stddev, mdev=alpha_mdev)
		alpha_ind  = where( abs(alpha) le alpha_stat[0]+alpha_mdev and $
	   	              	  abs(alpha) ge alpha_stat[0]-alpha_mdev)
		mask_alpha[alpha_ind] = 1.
	endif
	rmnanq,alpha
	alpha_rg = total(alpha*mask_alpha,2)/total(mask_alpha,2)
	rmnanq,alpha_rg
	alpha_abs = abs(alpha_rg)
	alpha_phi = atan(alpha_rg,/phase)
	
; ---- ESTIMATION POLYNOMIALE DE ALPHA ----
	range_line = findgen(file.xdim)
	
	; absolute value of alpha
	alpha_abs_coef = poly_fit(range_line,alpha_abs,3)
	alpha_abs_fit  = poly(range_line,alpha_abs_coef)
	
	; phase value of alpha
	alpha_phi_coef = poly_fit(range_line,alpha_phi,3)
	alpha_phi_fit  = fltarr(file.xdim)+alpha_phi_coef[0]
	
	; alpha fit
	alpha_fit = alpha_abs_fit * exp(complex(0,alpha_phi_fit))
	
	alpha1_rg = total(alpha1*mask_alpha,2)/(total(mask_alpha,2)+eps)
	alpha2_rg = total(alpha2*mask_alpha,2)/(total(mask_alpha,2)+eps)

	;--> check cross-correlation
	mask_cross = mask_cross[*,1:*]
	
	u_rg = total((smooth(u,3,/edge_truncate))*mask_cross,2)/(total(mask_cross,2)+eps)
	u_fit = complexarr(file.xdim)+mean(u_rg)

	v_rg = total((smooth(v,3,/edge_truncate))*mask_cross,2)/(total(mask_cross,2)+eps)
	v_fit = complexarr(file.xdim)+mean(v_rg)

	w_rg = total((smooth(w,3,/edge_truncate))*mask_cross,2)/(total(mask_cross,2)+eps)
	w_fit = complexarr(file.xdim)+mean(w_rg)

	z_rg = total((smooth(z,3,/edge_truncate))*mask_cross,2)/(total(mask_cross,2)+eps)
	z_fit = complexarr(file.xdim)+mean(z_rg)
	progress,/destroy
	
;-------------------------------------------------------------------------------
;                               CROSS-TALK REMOVAL
;-------------------------------------------------------------------------------

	;--> Cross-talk calibration
	;--------------------------
	den_x = 1./(alpha_fit+eps)
	
	cal_m00=dcomplex(  1  		                  ) * den_x
	cal_m01=dcomplex( -u_fit                     ) * den_x
	cal_m02=dcomplex( -alpha_fit * z_fit         ) * den_x
	cal_m03=dcomplex(  alpha_fit * z_fit * u_fit ) * den_x

	cal_m10=dcomplex( -w_fit                     ) * den_x
	cal_m11=dcomplex(  1 			               ) * den_x
	cal_m12=dcomplex(  alpha_fit * z_fit * w_fit ) * den_x
	cal_m13=dcomplex( -alpha_fit * z_fit  	      ) * den_x

	cal_m20=dcomplex( -v_fit             ) * den_x
	cal_m21=dcomplex(  u_fit * v_fit     ) * den_x
	cal_m22=dcomplex(  alpha_fit		    ) * den_x
	cal_m23=dcomplex( -alpha_fit * u_fit ) * den_x

	cal_m30=dcomplex(  v_fit * w_fit     ) * den_x
	cal_m31=dcomplex( -v_fit  	          ) * den_x
	cal_m32=dcomplex( -alpha_fit * w_fit ) * den_x
	cal_m33=dcomplex(  alpha_fit         ) * den_x
	
	;--> Read/Write header information
	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type	
	srat,outputfile,eee,header=head,info=info,type=type
	
	;--> generate the progress bar
	progress,message='Cross-talk removal',/cancel_button
	
	for ii=0,file.ydim-1 do begin
		
		progress,percent=(ii+1)*100.0/file.ydim,/check_cancel
		if wid.cancel eq 1 then return

		
		;--> Create block for data
		k_vec = make_array([4,file.xdim],type=file.var)
		
		;--> read data
		readu,ddd,k_vec
		
		;--> Remove cross-talk
		hh_l_cal=reform(cal_m00 * k_vec[hh,*] + cal_m10 * k_vec[vh,*] + cal_m20 * k_vec[hv,*] + cal_m30 * k_vec[vv,*])
		hv_l_cal=reform(cal_m02 * k_vec[hh,*] + cal_m12 * k_vec[vh,*] + cal_m22 * k_vec[hv,*] + cal_m32 * k_vec[vv,*])
 		vh_l_cal=reform(cal_m01 * k_vec[hh,*] + cal_m11 * k_vec[vh,*] + cal_m21 * k_vec[hv,*] + cal_m31 * k_vec[vv,*])
		vv_l_cal=reform(cal_m03 * k_vec[hh,*] + cal_m13 * k_vec[vh,*] + cal_m23 * k_vec[hv,*] + cal_m33 * k_vec[vv,*])
		
		;--> Affect to original data
		k_vec[hh,*] = hh_l_cal
		k_vec[hv,*] = hv_l_cal
		k_vec[vh,*] = vh_l_cal
		k_vec[vv,*] = vv_l_cal
		
		;--> Write data
		writeu,eee,k_vec
		
	endfor
	free_lun,ddd,eee
	
; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
