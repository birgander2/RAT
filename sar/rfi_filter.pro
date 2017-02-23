;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rfi_filter
; last revision : 1.October.2004
; written by    : Andreas Reigber
; Filtering of radio interferences      
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

function focrfi,arr,v0,f0,prf,rs,r0,rad,thr

	anz_rg = (size(arr))[1]
	anz_az = (size(arr))[2]

	c0     = 299707760   ;Lichtgeschwindigkeit (5 Grad, L-Band, Luft)
	fa  = shift(findgen(anz_az)/anz_az * prf - prf/2,anz_az/2)
	kx  = 2*!dpi*fa/v0
	lam = c0/f0/1e9
	ph0 = (sqrt( 16 * !pi^2 / lam^2 + kx^2) - (4*!pi/lam))

; Azimuth defocusing

	for i=0,anz_rg-1 do arr[i,*] = fft(arr[i,*],-1)
	for i=0,anz_rg-1 do begin
		r = r0 + i*rs
		arr[i,*] *= exp(complex(0,r*ph0))
	endfor
	for i=0,anz_rg-1 do arr[i,*] = fft(arr[i,*],+1)

; Range FFT	

	for i=0,anz_az-1 do arr[*,i] = fft(arr[*,i],-1)

; RFI detection & removal

	x1 = smooth(abs(arr),7,/edge_truncate)^2
	x2 = (smooth(abs(arr)^2,7,/edge_truncate) - x1  ) / x1
	aux = where(finite(x2) eq 0,nr)  
	if nr gt 0 then x2[aux] = 0.0
	x1 = bytarr(anz_rg,anz_az)
	aux = where(x2 gt thr*median(x2),nr)
	if nr gt 0 then x1[aux] = 1
 	disc = SHIFT(DIST(2*rad+1), rad, rad) LE rad 
	x2 = dilate(x1,disc)
	aux = where(x2 gt 0,nr)
	if nr gt 0 then arr[aux] = complex(0,0)

; Range IFFT	

	for i=0,anz_az-1 do arr[*,i] = fft(arr[*,i],+1)

; Azimuth focusing	

	for i=0,anz_rg-1 do arr[i,*] = fft(arr[i,*],-1)
	for i=0,anz_rg-1 do begin
		r = r0 + i*rs
		arr[i,*] *= exp(complex(0,-r*ph0))
	endfor
	for i=0,anz_rg-1 do arr[i,*] = fft(arr[i,*],+1)

	return,arr
end


pro rfi_filter,CALLED = called
	common rat, types, file, wid, config
	
	if (file.type ne 101 and file.type ne 200 and file.type ne 500) then begin
		error_button = DIALOG_MESSAGE(['Data has to be a complex SLC image or','a polsar/polinsar lexicographic scattering vector!'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='RFI Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		main_sub1 = WIDGET_BASE(main,row=5,/frame)
		field1   = CW_FIELD(main_sub1,VALUE='89.0',/float,TITLE='Platform velocity [m/s] : ',XSIZE=5)
		field2   = CW_FIELD(main_sub1,VALUE='1.3',/float,TITLE='Center frequency  [GHz] : ',XSIZE=5)
		field3   = CW_FIELD(main_sub1,VALUE='100.0',/float,TITLE='PRF               [Hz]  : ',XSIZE=5)
		field4   = CW_FIELD(main_sub1,VALUE='1.5',/float,TITLE='Range spacing     [m]   : ',XSIZE=5)
		field5   = CW_FIELD(main_sub1,VALUE='3742.0',/float,TITLE='First range       [m]   : ',XSIZE=5)
		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' Estimate ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
;  		main_sub2 = WIDGET_BASE(main,row=2,/frame)
;  		field6   = CW_FIELD(main_sub2,VALUE='2.0',/float,TITLE='Detection threshold     : ',XSIZE=3)
;  		field7   = CW_FIELD(main_sub2,VALUE='3',/float,TITLE='Notch filter strength   : ',XSIZE=3)
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

	
		repeat begin
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['RFI FILTER',$
				'Warning: Might be memory intensive ',$
				' ',$
				'RAT module written 10/2005 by Andreas Reigber',$
				' ',$
				'further information: A. Reigber and L. Ferro-Famil:', $ 
 				'Interference Suppression in Synthesized SAR Images', $
 				'IEEE Geosc. and Rem. Sens. Letters, Vol. 2(1)45-49, 2005']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=v0
		widget_control,field2,GET_VALUE=f0
		widget_control,field3,GET_VALUE=prf
		widget_control,field4,GET_VALUE=rs
		widget_control,field5,GET_VALUE=r0
;  		widget_control,field6,GET_VALUE=thr
;  		widget_control,field7,GET_VALUE=rad
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		info = DIALOG_MESSAGE("Batch mode not yet implemented", DIALOG_PARENT = main, TITLE='Information')
		return
	endelse

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function

   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
		
	progress,Message='RFI detection...'

; some parameters	

	c0     = 299707760   ;Lichtgeschwindigkeit (5 Grad, L-Band, Luft)
	fa  = shift(findgen(file.ydim)/file.ydim * prf - prf/2,file.ydim/2)
	kx  = 2*!dpi*fa/v0
	lam = c0/f0/1e9
	ph0 = (sqrt( 16 * !pi^2 / lam^2 + kx^2) - (4*!pi/lam))
	amp = fltarr(file.xdim,file.ydim) 
	thr = 2.5
	rad = 4
	
; and start

	block = make_array([file.vdim,file.zdim,file.xdim,file.ydim],type=file.var)
	readu,ddd,block
	free_lun,ddd
	
	for v=0,file.vdim-1 do begin
		for z=0,file.zdim-1 do begin
			progress,percent=(v*file.zdim+z)*100.0/file.zdim/file.vdim

; Azimuth defocusing

			for i=0,file.xdim-1 do block[v,z,i,*] = fft(block[v,z,i,*],-1)
			for i=0,file.xdim-1 do begin
				r = r0 + i*rs
				block[v,z,i,*] *= exp(complex(0,r*ph0))
			endfor
			for i=0,file.xdim-1 do block[v,z,i,*] = fft(block[v,z,i,*],+1)

; Range FFT	

			for i=0,file.ydim-1 do block[v,z,*,i] = fft(block[v,z,*,i],-1)
						
;			block[i,j,*,*] = focrfi(reform(block[i,j,*,*]),v0,f0,prf,rs,r0,rad,thr)
			amp += shift(reform(abs(block[v,z,*,*])),file.xdim/2)
		endfor
	endfor
	progress,/destroy

	if not keyword_set(called) then begin             ; Graphical interface 2
	
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='RFI Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		dr1   = widget_draw(main,XSIZE=500,ysize=500)

		sl1   = widget_base(main,column=3)
		label1  = widget_label(sl1,value=' detection threshold : ',/align_left)
		slide1  = widget_slider(sl1,/suppress_value,xsize=200,value=(thr-1.0)*100.0/5.0)
		value1  = widget_label(sl1,value=strcompress(thr,/remove))

		sl2   = widget_base(main,column=3)
		label2  = widget_label(sl2,value=' notch radius        : ',/align_left)
		slide2  = widget_slider(sl2,/suppress_value,xsize=200,value=(rad-1.0)*100.0/10.0)
		value2  = widget_label(sl2,value=strcompress(rad,/remove))

		but_upd = WIDGET_BUTTON(main,VALUE=' Update window ',xsize=100,/frame)
		
		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' Start filtering ',xsize=100,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)

		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]
	
		widget_control,dr1,get_value=index
		wset,index
		
		amps = scale2d(amp,500,500)
		tv,bytscl(amps,0.0,2.5*mean(amps))

		repeat begin                                        ; Event loop
			event = widget_event(main)
			widget_control,slide1,get_value=s1
			widget_control,slide2,get_value=s2
			thr = 1.0 + s1 / 100.0 * 5.0
			rad = 1 + floor(s2 / 100.0 * 10.0)
			
			widget_control,value1,set_value=strcompress(thr,/remove)
			widget_control,value2,set_value=strcompress(rad,/remove)
			
			if event.id eq but_upd then begin
				; RFI detection & removal
				widget_control,slide1,get_value=s1
				widget_control,slide2,get_value=s2
				thr = 1.0 + s1 / 100.0 * 5.0
				rad = 1 + floor(s2 / 100.0 * 10.0)

				mas = fltarr(file.xdim,file.ydim)+1
				x1 = smooth(abs(amp),7,/edge_truncate)^2
				x2 = (smooth(abs(amp)^2,7,/edge_truncate) - x1  ) / x1
				aux = where(finite(x2) eq 0,nr)  
				if nr gt 0 then x2[aux] = 0.0
				x1 = bytarr(file.xdim,file.ydim)
				aux = where(x2 gt thr*median(x2),nr)
				if nr gt 0 then x1[aux] = 1
			 	disc = shift(dist(2*rad+1), rad, rad) le rad 
				x2 = dilate(x1,disc)
				aux = where(x2 gt 0,nr)
				if nr gt 0 then mas[aux] = 0.0
		
				amps = scale2d(amp*mas,500,500)
				tv,bytscl(amps,0.0,2.5*mean(amps))
			endif
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,slide1,get_value=s1
		widget_control,slide2,get_value=s2
		thr = 1.0 + s1 / 100.0 * 5.0
		rad = 1 + floor(s2 / 100.0 * 10.0)
		
		widget_control,main,/destroy                        ; remove main widget

; switch back to main draw widget
		widget_control,wid.draw,get_value=index
		wset,index

		if event.id ne but_ok then return                   ; OK button _not_ clicked
	
	endif 

	WIDGET_CONTROL,/hourglass
	progress,Message='RFI removal...'

	mas = fltarr(file.xdim,file.ydim)+1
	x1 = smooth(abs(amp),7,/edge_truncate)^2
	x2 = (smooth(abs(amp)^2,7,/edge_truncate) - x1  ) / x1
	aux = where(finite(x2) eq 0,nr)  
	if nr gt 0 then x2[aux] = 0.0
	x1 = bytarr(file.xdim,file.ydim)
	aux = where(x2 gt thr*median(x2),nr)
	if nr gt 0 then x1[aux] = 1
	disc = shift(dist(2*rad+1), rad, rad) le rad 
	x2 = dilate(x1,disc)
	aux = where(x2 gt 0,nr)
	if nr gt 0 then mas[aux] = 0.0
	mas = shift(mas,-file.xdim/2,0)

	srat,outputfile,eee,header=head,info=info,type=type		

	for v=0,file.vdim-1 do begin
		for z=0,file.zdim-1 do begin
			progress,percent=(v*file.zdim+z)*100.0/file.zdim/file.vdim
			
			
; Range IFFT	

			for i=0,file.ydim-1 do block[v,z,*,i] = fft(block[v,z,*,i] * mas[*,i],+1)

; Azimuth focusing	

			for i=0,file.xdim-1 do block[v,z,i,*] = fft(block[v,z,i,*],-1)
			for i=0,file.xdim-1 do begin
				r = r0 + i*rs
				block[v,z,i,*] *= exp(complex(0,-r*ph0))
			endfor
			for i=0,file.xdim-1 do block[v,z,i,*] = fft(block[v,z,i,*],+1)

		endfor
	endfor
	writeu,eee,block
	progress,/destroy
	free_lun,eee
	
; update file information

	file.name = finalfile
	file_move,outputfile,finalfile,/overwrite

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end


