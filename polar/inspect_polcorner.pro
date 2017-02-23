;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: inspect_polcorner
; written by    : Stéphane Guillaso (TUB)
; last revision : 30.March.2004
; Polarimetric corner reflector analysis
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

FUNCTION corner_analyse,arr,xmin_or,xmin,ymin_or,ymin,value
	COMMON rat, types, file, wid, config
	
	; ---- INTERNAL VARIABLES
	eps = 1.e-6
	
	; ---- SIZE OF THE INPUT ARRAY
	xdim_arr = (SIZE(arr))[2]
	ydim_arr = (SIZE(arr))[3]
	
	; ---- FIND THE MAXIMUM OF THE PEAK
	; HH polarisation
	tmp = MAX(ABS(REFORM(arr[0,*,*])),pos)
	hh_xpos = pos MOD xdim_arr
	hh_ypos = pos  /  xdim_arr
	
	; VV polarisation
	tmp = MAX(ABS(REFORM(arr[1,*,*])),pos)
	vv_xpos = pos MOD xdim_arr
	vv_ypos = pos  /  xdim_arr
	
	; ---- UPDATE THE GLOBAL POSITION
	; HH polarisation
	global_hh_xpos = hh_xpos + xmin_or + xmin
	global_hh_ypos = hh_ypos + ymin_or + ymin
	
	; VV polarisation
	global_vv_xpos = vv_xpos + xmin_or + xmin
	global_vv_ypos = vv_ypos + ymin_or + ymin

	; ---- TEST THE POSITION OF THE MAXIMUM (BORDER PROBLEM)
	box_dim= 10        ; Box size the the analyse window

	; Calculation of the analyse window boxsize for HH polarisation
	xbox = box_dim
	ybox = box_dim * 2
	
	xbox_min = hh_xpos - xbox
	xbox_max = hh_xpos + xbox

	; Test with 0 value, if yes, give the distance  between 0 and the position of the max position
	xbox1 = (xbox_min LT 0) ? hh_xpos : xbox
	; Test with the opposite border, if yes give the distance between the border and the max position
	xbox2 = (xbox_max GE xdim_arr) ? xdim_arr - hh_xpos - 1 : xbox
	;Take the minimum between xbox1 and xbox2
	xbox_hh=min([xbox1,xbox2])

	ybox_min = hh_ypos - ybox
	ybox_max = hh_ypos + ybox
	ybox1 = (ybox_min LT 0)        ? hh_ypos : ybox
	ybox2 = (ybox_max GE ydim_arr) ? ydim_arr- hh_ypos - 1 : ybox
	ybox_hh = min([ybox1,ybox2])
	
	; Calculation of the analyse window boxsize for VV polarisation
	xbox_min = vv_xpos - xbox
	xbox_max = vv_xpos + xbox
	xbox1 = (xbox_min LT 0) ? vv_xpos : xbox
	xbox2 = (xbox_max GE xdim_arr) ? xdim_arr - vv_xpos - 1 : xbox
	xbox_vv=min([xbox1,xbox2])

	ybox_min = vv_ypos - ybox
	ybox_max = vv_ypos + ybox
	ybox1 = (ybox_min LT 0)        ? vv_ypos : ybox
	ybox2 = (ybox_max GE ydim_arr) ? ydim_arr- vv_ypos - 1 : ybox
	ybox_vv = min([ybox1,ybox2])

	; Take the minimum between the HH boxsize and VV boxsize
	xbox = min([xbox_hh,xbox_vv])
	ybox = min([ybox_hh,ybox_vv])
	
	IF (xbox EQ 0) OR (ybox EQ 0) THEN BEGIN
		RETURN,0
	ENDIF
	
	; Cut with respect of VV maximum
	s_hh = REFORM(arr[0,vv_xpos - xbox : vv_xpos + xbox, vv_ypos - ybox : vv_ypos + ybox])
	s_vv = REFORM(arr[1,vv_xpos - xbox : vv_xpos + xbox, vv_ypos - ybox : vv_ypos + ybox])
	s_hv = REFORM(arr[2,vv_xpos - xbox : vv_xpos + xbox, vv_ypos - ybox : vv_ypos + ybox]) / 2.
	IF file.zdim EQ 4 THEN $
		s_vh = REFORM(arr[3,vv_xpos - xbox : vv_xpos + xbox, vv_ypos - ybox : vv_ypos + ybox]) / 2. $
	ELSE s_vh = s_hv
	
	; Size of the new array
	xdim_arr = (SIZE(s_vv))[1]
	ydim_arr = (SIZE(s_vv))[2]

	; ---- CALCULATE THE POSITION OF THE PEAK MAXIMUM
	; HH polarisation
	tmp = MAX(ABS(s_hh),pos)
	hh_xpos_max = pos MOD xdim_arr
	hh_ypos_max = pos  /  xdim_arr
	
	; VV polarisation
	tmp = MAX(ABS(s_vv),pos)
	vv_xpos_max = pos MOD xdim_arr
	vv_ypos_max = pos  /  xdim_arr
	
	; ---- OVERSAMPLE OF A FACTOR OF OVER_F
	over_f = 8.
	
	so_hh = CONGRID(s_hh, (xbox*2+1) * over_f, (ybox*2+1) * over_f, CUBIC=-0.5)
	so_vv = CONGRID(s_vv, (xbox*2+1) * over_f, (ybox*2+1) * over_f, CUBIC=-0.5)
	so_hv = CONGRID(s_hv, (xbox*2+1) * over_f, (ybox*2+1) * over_f, CUBIC=-0.5)
	so_vh = CONGRID(s_vh, (xbox*2+1) * over_f, (ybox*2+1) * over_f, CUBIC=-0.5)
  	
	; ---- CALCULATE THE POSITION OF THE PEAK MAXIMUM OF THE OVERSAMPLED DATA
	xdim_arr = (SIZE(so_vv))[1]
	ydim_arr = (SIZE(so_vv))[2]
	
	; HH polarisation
	tmp = MAX(ABS(so_hh),pos)
	hh_xpos_max_over = pos MOD xdim_arr
	hh_ypos_max_over = pos  /  xdim_arr
	
	; VV polarisation
	tmp = MAX(ABS(so_vv),pos)
	vv_xpos_max_over = pos MOD xdim_arr
	vv_ypos_max_over = pos  /  xdim_arr
	
					 
	; Test of the position of the peak maximum +/- 5 pixel and the size of the analysis window
	IF ((hh_ypos_max_over - 5) LE 0) OR ((hh_ypos_max_over + 5) GE ydim_arr) OR $
	   ((vv_ypos_max_over - 5) LE 0) OR ((vv_ypos_max_over + 5) GE ydim_arr) THEN BEGIN
		RETURN,0	
	ENDIF

	; Channel Power and Phase Imbalance Estimation
	top_hh = so_hh[vv_xpos_max_over, hh_ypos_max_over - 5 : hh_ypos_max_over + 5]
	top_vv = so_vv[vv_xpos_max_over, vv_ypos_max_over - 5 : vv_ypos_max_over + 5]
	top_hv = so_hv[vv_xpos_max_over, vv_ypos_max_over - 5 : vv_ypos_max_over + 5]
	top_vh = so_vh[vv_xpos_max_over, vv_ypos_max_over - 5 : vv_ypos_max_over + 5]

	;--Channel-Imbalance Estimation

	top_hh_av = MEAN(top_hh)
	top_vv_av = MEAN(top_vv)
	top_hv_av = MEAN(top_hv)
	top_vh_av = MEAN(top_vh)

	power_hh = 20 * ALOG10(ABS(top_hh_av) + eps) 	      ;HH-Power in dB
	power_hv = 20 * ALOG10(ABS(top_hv_av) + eps) 	      ;HV-Power in dB
	power_vh = 20 * ALOG10(ABS(top_vh_av) + eps) 	      ;VH-Power in dB
	power_vv = 20 * ALOG10(ABS(top_vv_av) + eps) 	      ;VV-Power in dB

	pow_hhvv = ABS(top_hh_av) / ABS(top_vv_av + eps)		;HH/VV
	pow_hhhv = ABS(top_hh_av) / ABS(top_hv_av + eps)		;HH/HV
	pow_vvvh = ABS(top_vv_av) / ABS(top_vh_av + eps)		;VV/VH
	pow_hvvh = ABS(top_hv_av) / ABS(top_vh_av + eps)		;HV/VH

	pdb_hhvv = 20 * ALOG10(pow_hhvv + eps)
	pdb_hhhv = 20 * ALOG10(pow_hhhv + eps)
	pdb_vvvh = 20 * ALOG10(pow_vvvh + eps)
	pdb_hvvh = 20 * ALOG10(pow_hvvh + eps)

	;--Phase-Imbalance Estimation

	pha_hhvv = ATAN(top_hh_av * CONJ(top_vv_av),/PHASE) * !radeg  ;HH/VV
	pha_hhhv = ATAN(top_hh_av * CONJ(top_hv_av),/PHASE) * !radeg  ;HH/HV
	pha_vvvh = ATAN(top_vv_av * CONJ(top_vh_av),/PHASE) * !radeg  ;VV/VH
	pha_hvvh = ATAN(top_hv_av * CONJ(top_vh_av),/PHASE) * !radeg  ;HV/VH
		
; --> Co and Cross Pol Channel Polarisation Signature

	psi_st=5                                ;Steps in Chi (Orientation) [-90 90]
	chi_st=5                                ;Steps in Phi (Ellipticity) [-45 45]
	p_co=fltarr(90/chi_st+1,180/psi_st+1)		     ;co-pol	output array
	p_cr=fltarr(90/chi_st+1,180/psi_st+1)		     ;cross-pol output array 
	ideal_co=fltarr(90/chi_st+1,180/psi_st+1)	     ;ideal co-pol	output array
	ideal_cr=fltarr(90/chi_st+1,180/psi_st+1)	     ;ideal cross-pol output array 

	;Co-pol. Signature

	for psi=-90,90,psi_st do begin                               ;orientation angle
 		for chi=-45,45,chi_st do begin                              ;ellipticity angle

  			psi_t=psi*!dpi/180                                 ;transmit     grad --> rad
  			chi_t=chi*!dpi/180                                 ;  pol.       grad --> rad

  			psi_r=psi_t                                        ;receive  
  			chi_r=chi_t                                        ;  pol.      

  			a_t=COMPLEX(COS(psi_t)*COS(chi_t), sin(psi_t)*sin(chi_t)) 
  			b_t=complex(sin(psi_t)*cos(chi_t),-cos(psi_t)*sin(chi_t)) 
  			a_r=complex(cos(psi_r)*cos(chi_r), sin(psi_r)*sin(chi_r)) 
  			b_r=complex(sin(psi_r)*cos(chi_r),-cos(psi_r)*sin(chi_r)) 
    
			IF file.zdim EQ 4 THEN $
			  p_co((chi+45)/chi_st,(psi+90)/psi_st) = $
			    ABS(top_hh_av*a_t*a_r+top_hv_av*a_r*b_t+top_vh_av*b_r*a_t+top_vv_av*b_r*b_t )^2 $
			ELSE $
			  p_co((chi+45)/chi_st,(psi+90)/psi_st) = $
			    ABS(top_hh_av*a_t*a_r+ 2 * top_hv_av*a_r*b_t+top_vv_av*b_r*b_t )^2
			
			ideal_co((chi+45)/chi_st,(psi+90)/psi_st)=ABS(1.*a_t*a_r+1.*b_r*b_t )^2
 
 		endfor 
	endfor

	;Cross-pol. Signature

		for psi=-90,90,psi_st do begin                               ;orientation angle
 			for chi=-45,45,chi_st do begin                              ;ellipticity angle

  				psi_t=psi*!dpi/180                                 ;transmit     grad --> rad
  				chi_t=chi*!dpi/180                                 ;  pol.       grad --> rad

  				psi_r=psi_t+!dpi/2                                 ;receive  
  				chi_r=-chi_t                                       ;  pol.      

  				a_t=complex(cos(psi_t)*cos(chi_t), sin(psi_t)*sin(chi_t)) 
  				b_t=complex(sin(psi_t)*cos(chi_t),-cos(psi_t)*sin(chi_t)) 

  				a_r=complex(cos(psi_r)*cos(chi_r), sin(psi_r)*sin(chi_r)) 
  				b_r=complex(sin(psi_r)*cos(chi_r),-cos(psi_r)*sin(chi_r)) 
  
				IF file.zdim EQ 4 THEN $
				  p_cr((chi+45)/chi_st,(psi+90)/psi_st) = $
				    ABS(top_hh_av*a_t*a_r+top_hv_av*a_r*b_t+top_vh_av*b_r*a_t+top_vv_av*b_r*b_t )^2 $
				ELSE $
				  p_cr((chi+45)/chi_st,(psi+90)/psi_st) = $
				    ABS(top_hh_av*a_t*a_r+ 2 * top_hv_av*a_r*b_t+top_vv_av*b_r*b_t )^2
				
				ideal_cr((chi+45)/chi_st,(psi+90)/psi_st)=ABS(1.*a_t*a_r+1.*b_r*b_t )^2

 			endfor
		endfor

	p_co=p_co/max(p_co)                                     ;Normalization
	p_cr=p_cr/max(p_cr)  
	ideal_co = ideal_co/max(ideal_co)
	ideal_cr = ideal_cr/max(ideal_cr)
	
	; OUTPUT STRUCTURE CREATION
	value = { hh_xpos_max 	: global_hh_xpos + hh_xpos_max_over / over_f - xbox,$
				 hh_ypos_max	: global_hh_ypos + hh_ypos_max_over / over_f - ybox,$
				 vv_xpos_max 	: global_vv_xpos + vv_xpos_max_over / over_f - xbox,$
				 vv_ypos_max 	: global_vv_ypos + vv_ypos_max_over / over_f - ybox,$
				 power_hh 		: power_hh,$
				 power_vv 		: power_vv,$
				 power_hv 		: power_hv,$
				 power_vh 		: power_vh,$
				 pow_hhvv 		: pow_hhvv,$
				 pow_hvvh 		: pow_hvvh,$
				 pow_hhhv 		: pow_hhhv,$
				 pow_vvvh 		: pow_vvvh,$
				 pdb_hhvv 		: pdb_hhvv,$
				 pdb_hvvh 		: pdb_hvvh,$
				 pdb_hhhv 		: pdb_hhhv,$
				 pdb_vvvh 		: pdb_vvvh,$
				 pha_hhvv 		: pha_hhvv,$
				 pha_hvvh 		: pha_hvvh,$
				 pha_hhhv 		: pha_hhhv,$
				 pha_vvvh 		: pha_vvvh,$
				 so_hh    		: so_hh,$
				 so_vv         : so_vv,$
				 so_hv         : so_hv,$
				 so_vh         : so_vh,$
				 hh_xpos_max_over : hh_xpos_max_over,$
				 hh_ypos_max_over : hh_ypos_max_over,$
				 vv_xpos_max_over : vv_xpos_max_over,$
				 vv_ypos_max_over : vv_ypos_max_over,$
				 top_hh : top_hh,$
				 top_vv : top_vv,$
				 p_co : p_co,$
				 p_cr : p_cr,$
				 ideal_co : ideal_co,$
				 ideal_cr : ideal_cr}
	RETURN,1
END 
PRO display_polcorner_information,GroupLeader,value
	COMMON rat, types, file, wid, config
	
	; ---- DRAW THE WIDGET
	main = WIDGET_BASE(GROUP_LEADER=GroupLeader,row=2,TITLE='Polarimetric Corner Information Display',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		wTab = WIDGET_TAB(main)

		;Create the first tab
		wT1 = WIDGET_BASE(wTab,TITLE='CR Impulse Response',ROW=3)
			label1 = WIDGET_LABEL(wT1,VALUE='Corner Reflector Impulse Response (Oversampled)',SCR_XSIZE=800)
			wT11 = WIDGET_BASE(wT1,COLUMN=2)
				draw111 = WIDGET_DRAW(wT11,XSIZE=400,YSIZE=300)
				draw121 = WIDGET_DRAW(wT11,XSIZE=400,YSIZE=300) 
			wT12 = WIDGET_BASE(wT1,COLUMN=2)
				draw112 = WIDGET_DRAW(wT12,XSIZE=400,YSIZE=300)
				draw122 = WIDGET_DRAW(wT12,XSIZE=400,YSIZE=300) 
				
		;Create the second tab
		wT2 = WIDGET_BASE(wTab,TITLE='Azimuth/Range CR Profile',ROW=3)
			label2 = WIDGET_LABEL(wT2,VALUE='Azimuth/Range Corner Reflector Profiles (Intensity/Phase)',SCR_XSIZE=800)
			wT21 = WIDGET_BASE(wT2,COLUMN=2)
				draw211 = WIDGET_DRAW(wT21,XSIZE=400,YSIZE=300)
				draw221 = WIDGET_DRAW(wT21,XSIZE=400,YSIZE=300) 
			wT22 = WIDGET_BASE(wT2,COLUMN=2)
				draw212 = WIDGET_DRAW(wT22,XSIZE=400,YSIZE=300)
				draw222 = WIDGET_DRAW(wT22,XSIZE=400,YSIZE=300) 
			
		;Create the third tab
		wT3 = WIDGET_BASE(wTab,TITLE='CR Peak Phase',ROW=3)
			label3 = WIDGET_LABEL(wT3,VALUE='Corner Reflector Peak Phase (Oversampled)',SCR_XSIZE=800)
			wT31 = WIDGET_BASE(wT3,COLUMN=2)
				draw311 = WIDGET_DRAW(wT31,XSIZE=400,YSIZE=300)
				draw321 = WIDGET_DRAW(wT31,XSIZE=400,YSIZE=300) 
			wT32 = WIDGET_BASE(wT3,COLUMN=2)
				draw312 = WIDGET_DRAW(wT32,XSIZE=400,YSIZE=300)
			
		;Create the second tab
		wT4 = WIDGET_BASE(wTab,TITLE='Co- and Cross-Polar Signature',ROW=4)
			label4 = WIDGET_LABEL(wT4,VALUE='Real/Ideal Co- and Cross-Polarimetric Signatures',SCR_XSIZE=800)
			wT41 = WIDGET_BASE(wT4,COLUMN=2)
				draw411 = WIDGET_DRAW(wT41,XSIZE=400,YSIZE=200)
				draw421 = WIDGET_DRAW(wT41,XSIZE=400,YSIZE=200) 
			wT42 = WIDGET_BASE(wT4,COLUMN=2)
				draw412 = WIDGET_DRAW(wT42,XSIZE=400,YSIZE=200)
				draw422 = WIDGET_DRAW(wT42,XSIZE=400,YSIZE=200) 
			wT43 = WIDGET_BASE(wT4,COLUMN=2)
				draw413 = WIDGET_DRAW(wT43,XSIZE=400,YSIZE=200)
				draw423 = WIDGET_DRAW(wT43,XSIZE=400,YSIZE=200) 
			
		
	buttons = WIDGET_BASE(main,column=2)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
	WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]



	WIDGET_CONTROL,draw111,GET_VALUE=index & WSET,index
		SHADE_SURF,ABS(value.so_hh),XSTYLE=1,YSTYLE=1,XTITLE='Range', YTITLE='Azimuth', ZTITLE='Amplitude',TITLE='HH'
	WIDGET_CONTROL,draw121,GET_VALUE=index & WSET,index
		SHADE_SURF,ABS(value.so_vv),XSTYLE=1,YSTYLE=1,XTITLE='Range', YTITLE='Azimuth', ZTITLE='Amplitude',TITLE='VV'
	WIDGET_CONTROL,draw112,GET_VALUE=index & WSET,index
		SHADE_SURF,ABS(value.so_hv),XSTYLE=1,YSTYLE=1,XTITLE='Range', YTITLE='Azimuth', ZTITLE='Amplitude',TITLE='HV'
	IF file.zdim EQ 4 THEN BEGIN
		WIDGET_CONTROL,draw122,GET_VALUE=index & WSET,index
			SHADE_SURF,ABS(value.so_vh),XSTYLE=1,YSTYLE=1,XTITLE='Range', YTITLE='Azimuth', ZTITLE='Amplitude',TITLE='VH'
	ENDIF
	
	WIDGET_CONTROL,draw211,GET_VALUE=index & WSET,index
		tmp_vv = ABS(value.so_vv[value.vv_xpos_max_over,*])
		tmp_hh = ABS(value.so_hh[value.hh_xpos_max_over,*])
		maxmax = MAX([tmp_vv,tmp_hh])
		PLOT,tmp_vv,YRANGE=[0,maxmax],$
			         XSTYLE=1,TITLE='Azimuth Corner Reflector - Intensity',XTITLE='Azimuth',YTITLE='Intensity'
		LOADCT,33,/SILENT
		OPLOT,tmp_hh,COLOR=150
		LOADCT,0,/SILENT
	WIDGET_CONTROL,draw221,GET_VALUE=index & WSET,index
		tmp_vv = ABS(value.so_vv[*,value.vv_ypos_max_over])
		tmp_hh = ABS(value.so_hh[*,value.hh_ypos_max_over])
		maxmax = MAX([tmp_vv,tmp_hh])
		PLOT,tmp_vv,YRANGE=[0,maxmax],$
		         XSTYLE=1,TITLE='Range Corner Reflector - Intensity',XTITLE='Range',YTITLE='Intensity'
		LOADCT,33,/SILENT
		OPLOT,tmp_hh,COLOR=150
		LOADCT,0,/SILENT
	WIDGET_CONTROL,draw212,GET_VALUE=index & WSET,index
		PLOT,ATAN(value.so_vv[value.vv_xpos_max_over,*],/PHASE),YRANGE = [-!pi,!pi],$
		          XSTYLE=1,TITLE='Azimuth Corner Reflector - Phase',XTITLE='Azimuth',YTITLE='Phase'
		LOADCT,33,/SILENT
		OPLOT,ATAN(value.so_hh[value.hh_xpos_max_over,*],/PHASE),COLOR=150
		LOADCT,0,/SILENT
	WIDGET_CONTROL,draw222,GET_VALUE=index & WSET,index
		PLOT,ATAN(value.so_vv[*,value.vv_ypos_max_over],/PHASE),YRANGE = [-!pi,!pi],$
		          XSTYLE=1,TITLE='Range Corner Reflector - Phase',XTITLE='Range',YTITLE='Phase'
		LOADCT,33,/SILENT
		OPLOT,ATAN(value.so_hh[*,value.hh_ypos_max_over],/PHASE),COLOR=150
		LOADCT,0,/SILENT

	WIDGET_CONTROL,draw311,GET_VALUE=index & WSET,index
		tmp = ATAN(value.top_hh,/PHASE)*!RADEG
		PLOT,tmp,YRANGE = [MIN(tmp)-10,MAX(tmp)+10],TITLE='HH',XSTYLE=1,YSTYLE=1,CHARSIZE=2
	WIDGET_CONTROL,draw321,GET_VALUE=index & WSET,index
		tmp = ATAN(value.top_vv,/PHASE)*!RADEG
		PLOT,tmp,YRANGE = [MIN(tmp)-10,MAX(tmp)+10],TITLE='VV',XSTYLE=1,YSTYLE=1,CHARSIZE=2
	WIDGET_CONTROL,draw312,GET_VALUE=index & WSET,index
		tmp = ATAN(value.top_hh * CONJ(value.top_vv),/PHASE)*!RADEG
		PLOT,tmp,YRANGE = [MIN(tmp)-10,MAX(tmp)+10],TITLE='HH-VV',XSTYLE=1,YSTYLE=1,CHARSIZE=2

	WIDGET_CONTROL,draw411,GET_VALUE=index & WSET,index
		SURFACE,value.p_co,xticks=2,xtickname=[45,0,-45],xtitle='ORIENTATION CHI',xstyle=1,$
 		    yticks=2,ytickname=[90,0,-90],ytitle='ELLIPTICITY PSI',ystyle=1,$
 		    zrange=[0,1],ztitle='NORMALIZED POWER',title='Co-pol',$
 		    az=47,charsize=2.
	WIDGET_CONTROL,draw421,GET_VALUE=index & WSET,index
		SURFACE,value.p_cr,xticks=2,xtickname=[45,0,-45],xtitle='ORIENTATION CHI',xstyle=1,$
 		    yticks=2,ytickname=[90,0,-90],ytitle='ELLIPTICITY PSI',ystyle=1,$
 		    zrange=[0,1],ztitle='NORMALIZED POWER',title=' Cross-pol',$
 		    az=47,charsize=2.
	WIDGET_CONTROL,draw412,GET_VALUE=index & WSET,index
		SURFACE,value.ideal_co,xticks=2,xtickname=[45,0,-45],xtitle='ORIENTATION CHI',xstyle=1,$
 		    yticks=2,ytickname=[90,0,-90],ytitle='ELLIPTICITY PSI',ystyle=1,$
 		    zrange=[0,1],ztitle='NORMALIZED POWER',title=' Ideal Co-pol',$
 		    az=47,charsize=2.
	WIDGET_CONTROL,draw422,GET_VALUE=index & WSET,index
		SURFACE,value.ideal_cr,xticks=2,xtickname=[45,0,-45],xtitle='ORIENTATION CHI',xstyle=1,$
 		    yticks=2,ytickname=[90,0,-90],ytitle='ELLIPTICITY PSI',ystyle=1,$
 		    zrange=[0,1],ztitle='NORMALIZED POWER',title=' Ideal Cross-pol',$
 		    az=47,charsize=2.
	WIDGET_CONTROL,draw413,GET_VALUE=index & WSET,index
		CONTOUR,value.p_co,nlevels=25,xstyle=1,ystyle=1
	WIDGET_CONTROL,draw423,GET_VALUE=index & WSET,index
		CONTOUR,value.p_cr,nlevels=25,xstyle=1,ystyle=1
;  
	REPEAT BEGIN
		event = WIDGET_EVENT(main)
		; ---- Info Button
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['POLARIMETRIC POINT TARGET ANALYSIS - POLARIMETRIC CALIBRATION',$
			' ',$
			'RAT module written 03/2004 by Stephane Guillaso']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		endif
		
	ENDREP UNTIL (event.id EQ but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	
	WIDGET_CONTROL,main,/DESTROY
	
END
PRO inspect_polcorner
	COMMON rat, types, file, wid, config
	
; ---- Is it a lexicographic image?
	IF file.type NE 200 THEN BEGIN
		error = DIALOG_MESSAGE("scattering vector, lexicographic basis is required",DIALOG_PARENT=wid.base,TITLE='Error',/ERROR)
		RETURN
	ENDIF
	
	
	; Display select region message

	message = ['Select a region with corner reflectors',$
	           'on the main windows']
	tmp = DIALOG_MESSAGE(message,DIALOG_PARENT=wid.base)


	; ---- Draw white box with the mouse to select corner

	; Control the main widget in order to select a part of a scene
	WIDGET_CONTROL, wid.draw,DRAW_BUTTON_EVENTS=1, DRAW_MOTION_EVENTS = 1
	
	mousebox,xmin,xmax,ymin,ymax
	
	xmin = round(xmin / wid.draw_scale)
	xmax = round(xmax / wid.draw_scale)
	ymin = round(ymin / wid.draw_scale)
	ymax = round(ymax / wid.draw_scale)

  	rrat,file.name,arrx,block=[xmin,ymin,xmax-xmin+1,ymax-ymin+1]
	savim = REFORM(ABS(arrx[0,*,*]))
	xsize_im = (SIZE(savim))[1]
	ysize_im = (SIZE(savim))[2]
	xmin_or = xmin
	ymin_or = ymin
	xmax_or = xmax
	ymax_or = ymax
	
	case file.type of
		 102: trick=2  ; phase
		 301: trick=1  ; complex interferogram
		 302: trick=2  ; phase
		else: trick=0  ; normal
	endcase
	
	; Get the size of inblock
;  	tmp = file.zdim
;  	file.dim = 2l
;  	file.zdim = 1l
;  	outblock_bw = scale_block(reform(arrx[1,*,*]),xsize_im,ysize_im,trick)
;  	file.dim = 3l
;  	file.zdim = tmp
	outblock_col = scale_block(arrx,xsize_im,ysize_im,trick)
	
	; Draw the widget
	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Polarimetric Point Target Analysis',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		sub = WIDGET_BASE(main,COLUMN=2,/FRAME)
;			wdraw = WIDGET_DRAW(sub,/SCROLL,X_SCROLL_SIZE=300,Y_SCROLL_SIZE=300,XSIZE=xsize_im,YSIZE=ysize_im)
			wdraw = cw_rat_draw(sub,xsize_im,ysize_im,XSCROLL=300,YSCROLL=300)
			sub_right = WIDGET_BASE(sub,ROW=3)
				label1 = WIDGET_TEXT(sub_right,VALUE='',SCR_XSIZE=600,YSIZE=1,/ALIGN_CENTER)
				label2 = WIDGET_TEXT(sub_right,VALUE='',SCR_XSIZE=600,YSIZE=17)
				but    = WIDGET_BASE(sub_right,/row)
;  					text_tmp = widget_label(but,value="display:")
;  					but_im_color = cw_bgroup(but,['b/w','colour'],/exclusive, set_value=0,/row)
					but_anc  = WIDGET_BUTTON(but,VALUE='Corner Reflector Analyse')
					but_disp = WIDGET_BUTTON(but,VALUE='Display Information')
					but_cal  = WIDGET_BUTTON(but,VALUE='Perform Calibration')

	buttons  = WIDGET_BASE(main,column=2)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

	WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]
	widget_control, wdraw, draw_button_events=1, draw_motion_events=1

	
	WIDGET_CONTROL,wdraw,GET_VALUE=index
	WSET,index
;  	file.type = 101l
;  	dummy = file.dim
;  	file.dim = 2

	image = float2bytes(outblock_col)
	rat_tv,image
;  	file.type = 200l
;  	file.dim = dummy
	
	;TVSCL,BYTSCL(savim,0,2.5*MEAN(savim))
	
	output=0
	flag = 0
	repeat begin
		event = widget_event(main)
		
		; ---- display color or bw image
;  		if event.id eq but_im_color then begin
;  			widget_control, but_im_color, get_value=toto
;  			case toto of
;  				0 : begin
;  					WIDGET_CONTROL,wdraw,GET_VALUE=index
;  					WSET,index
;  					file.type = 101l
;  					dummy = file.dim
;  					file.dim = 2
;  					image = float2bytes(outblock_bw)	
;  					rat_tv,image
;  					file.type = 200l
;  					file.dim = dummy
;  				end
;  				1 : begin
;  					image = float2bytes(outblock_col)	
;  					rat_tv,image
;  				end
;  			endcase
;  		endif
		
		; ---- Corner Reflector Analyse
		IF event.id EQ but_anc THEN BEGIN
			IF flag EQ 1 THEN TV,savim,xmin-1,ymin-1,true=1
			flag =  1
			; Update label 1
			WIDGET_CONTROL,label1,SET_VALUE='Select the Corner Reflector in the left window'
			; Update label 2
			WIDGET_CONTROL,label2,SET_VALUE=''
			WIDGET_CONTROL, wdraw,event_pro=''
			DEVICE,/CURSOR_CROSSHAIR
			REPEAT res = WIDGET_EVENT(wdraw) UNTIL res.press EQ 1
			;First point and definition of the image array
			x1    = res.x
			y1    = res.y
			xmin  = res.x
			ymin  = res.y
			savim = bytarr(3,2,2)
	
			; draw the rectangle into the image
			REPEAT BEGIN
				res = WIDGET_EVENT(wdraw) 
				x2 = res.x
				y2 = res.y

				TV,savim,xmin-1,ymin-1,true=1
		
				xmin = min([x1,x2])
				xmax = max([x1,x2])
				ymin = min([y1,y2])
				ymax = max([y1,y2])
				difx = xmax - xmin + 2
				dify = ymax - ymin + 2

				savim = TVRD(xmin-1,ymin-1,difx,dify,true=1)
				PLOTS,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/device ,color=255
			ENDREP UNTIL res.release eq 1
			DEVICE,/CURSOR_ORIGINAL
			WIDGET_CONTROL, wdraw,event_pro='cw_rat_draw_ev'
			WIDGET_CONTROL,label1,SET_VALUE='Corner Reflector Analyse'
			
			arrx_sub = arrx[*,xmin:xmax,ymin:ymax]
			output = corner_analyse(arrx_sub,xmin_or,xmin,ymin_or,ymin,value)
			IF output EQ 1 THEN BEGIN
				OPENW,tmp_id,config.tempdir+'tmp.txt',/GET_LUN
				PRINTF,tmp_id,'COREGISTRATION'
				PRINTF,tmp_id,' '
				PRINTF,tmp_id,'# Peak-Position       HH             VV'
				PRINTF,tmp_id,format='(A12,5f15.3)','    Range   ',value.hh_xpos_max,value.vv_xpos_max
				PRINTF,tmp_id,format='(A12,5f15.3)','   Azimuth  ',value.hh_ypos_max,value.vv_ypos_max
				PRINTF,tmp_id,''
				PRINTF,tmp_id,'SIGNAL ANALYSIS'
				PRINTF,tmp_id,''
				IF file.zdim EQ 4 THEN BEGIN
					PRINTF,tmp_id,'# Power                HH             VV             HV             VH'
					PRINTF,tmp_id,format='(A12,5f15.3)','    [  -  ] ',value.power_hh, value.power_vv, value.power_hv, value.power_vh
					PRINTF,tmp_id, ' '
					PRINTF,tmp_id,'# Amplitude Imbalance HH/VV         HV/VH           HH/HV          VV/VH'
					PRINTF,tmp_id,format='(A12,5f15.3)','    [ A/A ]  ',value.pow_hhvv, value.pow_hvvh, value.pow_hhhv, value.pow_vvvh
					PRINTF,tmp_id,format='(A12,5f15.3)','    [ dB. ]  ',value.pdb_hhvv, value.pdb_hvvh, value.pdb_hhhv, value.pdb_vvvh
					PRINTF,tmp_id, ' '
					PRINTF,tmp_id,'# Phase Imbalance     HH/VV         HV/VH           HH/HV          VV/VH'
					PRINTF,tmp_id,format='(A12,5f15.3)','    [ Deg ]  ',value.pha_hhvv, value.pha_hvvh, value.pha_hhhv, value.pha_vvvh
				ENDIF ELSE BEGIN
					PRINTF,tmp_id,'# Power                HH             VV             HV'
					PRINTF,tmp_id,format='(A12,5f15.3)','    [  -  ] ',value.power_hh, value.power_vv, value.power_hv
					PRINTF,tmp_id, ' '
					PRINTF,tmp_id,'# Amplitude Imbalance HH/VV         HH/HV          VV/HV'
					PRINTF,tmp_id,format='(A12,5f15.3)','    [ A/A ]  ',value.pow_hhvv, value.pow_hhhv, value.pow_vvvh
					PRINTF,tmp_id,format='(A12,5f15.3)','    [ dB. ]  ',value.pdb_hhvv, value.pdb_hhhv, value.pdb_vvvh
					PRINTF,tmp_id, ' '
					PRINTF,tmp_id,'# Phase Imbalance     HH/VV         HH/HV          VV/HV'
					PRINTF,tmp_id,format='(A12,5f15.3)','    [ Deg ]  ',value.pha_hhvv, value.pha_hhhv, value.pha_vvvh
				ENDELSE
				FREE_LUN,tmp_id
				if	config.os eq 'windows' then newline = strcompress(13B) + string(10B)
				if	config.os eq 'unix' then newline = string(10B)

				; File reading for formatting the text
				text1 ='' & dummy = ''
				OPENR,tmp_id,config.tempdir+'tmp.txt',/GET_LUN
	
				FOR iii=0,15 DO BEGIN
					READF,tmp_id,dummy
					text1 = text1 + dummy + newline
				ENDFOR
				READF,tmp_id,dummy
				text1 = text1 + dummy
	
				FREE_LUN,tmp_id
				WIDGET_CONTROL,label2, SET_VALUE = text1
				
			ENDIF ELSE WIDGET_CONTROL,label2, SET_VALUE = 'Not Available...'
			
		ENDIF
		
		IF event.id EQ but_disp THEN BEGIN
			IF output EQ 0 THEN BEGIN
				message = 'You have to analyse a Corner Reflector first'
				error = DIALOG_MESSAGE(message,DIALOG_PARENT=main,TITLE='Error',/ERROR)
			ENDIF ELSE BEGIN
				display_polcorner_information,main,value
			ENDELSE
		ENDIF
		
		; ---- Info Button
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['POLARIMETRIC POINT TARGET ANALYSIS - POLARIMETRIC CALIBRATION',$
			' ',$
			'RAT module written 03/2004 by Stephane Guillaso',$
			' ',$
			'Warning: This routine is working correctly only in case of',$
			'a bright target in dark surrounding. Otherwise the automatic',$
			'target search and parameter estimation will fail.']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		endif
	endrep until (event.id eq but_ok) OR (event.id EQ but_cal) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,main,/destroy                        ; remove main widget

; switch back to main draw widget
	IF (event.id EQ but_cal) THEN $
		IF output EQ 1 $
		THEN calib_imbalance,phi_co=value.pha_hhvv,amp_co=value.pow_hhvv $
		ELSE calib_imbalance

	widget_control,wid.draw,get_value=index
	wset,index
END

