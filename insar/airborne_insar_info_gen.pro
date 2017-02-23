;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: airborne_insar_info_gen
; written by    : Stephane Guillaso (TUB)
; last revision : 26.march.2004
; generate flatearth phase, look angle, ambiguity height, normal baseline
; critical normal baseline
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

PRO airborne_insar_info_gen, insar, fe=fe, theta=theta_r, h_amb=h_amb, b_n=B_normal, bnc=B_normal_c, r_ref=r_ref
	common rat, types, file, wid, config

;------------------------------------------------------------
; Get some parameters needed by the procedure
;------------------------------------------------------------
	bl_y_ref = insar.range_baseline
	bl_z_ref = insar.height_baseline
	lambda   = insar.wavelength
	xdim     = insar.xdim_or
	rdelay   = insar.range_delay*1e-6
	rbin     = insar.range_sampling
	dt       = 1.0d/(insar.range_sampling_rate*1e-6)
	terrain  = insar.terrain_elevation
	c0       = insar.speed_of_light
	W_r      = insar.chirp_bandwidth
	
;------------------------------------------------------------
; Retrieve information provided for master track
;------------------------------------------------------------
	h01      = insar.altitude_above_ground_master
	r_ref    = rdelay * c0/2.0 + dindgen(xdim)*rbin
	first1   = insar.range_bin_first_master
	r_ref[0:first1-1] = r_ref[first1]
	r_ref = double(r_ref) 
	
;------------------------------------------------------------
; Retrieve information provided for slave track
;------------------------------------------------------------
	h02      = insar.altitude_above_ground_slave
	r02      = rdelay * c0 / 2.0 + dindgen(xdim) * rbin
	first2   = insar.range_bin_first_slave	
	r02[0:first2-1] = r02[first2]
	midbin  = (xdim-first2)/2+first2
	t_mid   = rdelay+midbin*dt
	
;------------------------------------------------------------
; Calcul of the different parameters
;------------------------------------------------------------
	; Calcul of the look angle theta
	theta_r = -acos(h02/r02)
	
	; Calcul of the baseline and angle between baseline and horizontal line
	bl_z_ref=double(bl_z_ref)
	bl_y_ref=double(bl_y_ref)
	bl = insar.baseline
	alpha = insar.alpha
	
	; Calcul the distance between the slave sensor and a target
	r_neu = sqrt( (h01+bl_z_ref)^2 + (sqrt(r_ref^2 - h01^2) - bl_y_ref)^2 )
	
	; Calcul the flatearth phase component
	fe = 4 * !pi / lambda * ( r_ref - r_neu )
	
	; Remove some "nan" number inside the flatearth phase component
	rmnanq,fe

	; Extraction de la baseline normale
	B_normal = abs(bl*cos(theta_r - alpha))

	; Calcul de la baseline critique
	B_normal_c = abs((lambda * W_r * 1e6 * r_ref * tan(theta_r)) / c0)
	
	; Ambiguity height
	h_amb = abs((lambda/2) * (r_ref * sin(theta_r))/(B_normal))

END
