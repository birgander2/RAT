;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: coreg_patch
; written by    : Andreas Reigber
; last revision : 13. Oct 2004
; Interferometric coregistration based on amplitude correlation
; and using multiple patches over the image
; - requires coarse registration before
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

function coreg_ampsub,arr1,arr2,quality
   compile_opt idl2
	bs = (size(arr1))[1]
	auxcor = abs(fft(fft(arr1,-1)*conj(fft(arr2,-1)),+1))
	aux    = max(auxcor, auxpos )
	
	offsetx = auxpos mod bs
	offsety = auxpos  /  bs
	if (offsetx gt bs/2) then offsetx = offsetx-bs
	if (offsety gt bs/2) then offsety = offsety-bs
	
;	if abs(offsetx) lt 3 and abs(offsety) lt 3 then stop
	
	if abs(offsetx) lt bs/2-32 and abs(offsety) lt bs/2-32 then begin
		arr1 = arr1[bs/2-32:bs/2+31,bs/2-32:bs/2+31]
		arr2 = arr2[bs/2-32-offsetx:bs/2-offsetx+31,bs/2-offsety-32:bs/2-offsety+31]
		sizx = 32
		sizy = 32	
		osize = 512
	
		out1 = fft(arr1,-1)
		out2 = fft(arr2,-1)
	
		fbild1 = complexarr(osize,osize)
		fbild2 = complexarr(osize,osize)
		fbild1[0,0] = shift(out1,sizx/2,sizy/2)
		fbild2[0,0] = shift(out2,sizx/2,sizy/2)
		fbild1 = shift(fbild1,-sizx/2,-sizy/2)
		fbild2 = shift(fbild2,-sizx/2,-sizy/2)
		fbild  = fbild1 * conj(fbild2)
		bild   = abs(fft(fbild,1))
		
		aux=max(bild,pos)
		
	   xoff = pos mod osize
	   yoff = pos  /  osize
	   if xoff gt osize / 2 then xoff = xoff - osize
	   if yoff gt osize / 2 then yoff = yoff - osize
		xpos = xoff * 1.0 / (float(osize) / sizx)
		ypos = yoff * 1.0 / (float(osize) / sizy)
	endif else begin
		xpos = 0.0
		ypos = 0.0
	endelse


	return,[offsetx+xpos,offsety+ypos]



end

pro coreg_patch,CALLED = called, OFFSETX = offsetx, OFFSETY = offsety
   compile_opt idl2
	common rat, types, file, wid, config

	if file.type ne 300 and file.type ne 301 and file.type ne 302 then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram ", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Patched image registration',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=10,/integer,TITLE='No. of patches in x : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=10,/integer,TITLE='No. of patches in y : ',XSIZE=3)
		field3 = CW_FIELD(main,VALUE=128,/integer,TITLE='Size of patches     : ',XSIZE=3)
		buttons = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin                                        ; Event loop
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['PATCHED IMAGE COREGISTRATION',$
				' ',$
				'RAT module written 10/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=patx					; read widget fields
		widget_control,field2,GET_VALUE=paty
		widget_control,field3,GET_VALUE=pats
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(patx) then patx = 10				  ; Default values
		if not keyword_set(paty) then paty = 10
		if not keyword_set(pats) then pats = 128
	endelse
	patx >= 1
	paty >= 1
	pats >= 1

; change mousepointer

	WIDGET_CONTROL,/hourglass

	progress,Message='Estimating offsets...',/cancel_button
		
	stpx = (file.xdim - pats) / patx
	stpy = (file.ydim - pats) / paty
	xo = fltarr(patx,paty)
	yo = fltarr(patx,paty)
	xp = fltarr(patx,paty)
	yp = fltarr(patx,paty)
	
	
	for j=0L,paty-1 do begin
		ypos = j*stpy
		rrat,file.name,arr,block=[0,ypos,file.xdim,pats]
		for i=0L,patx-1 do begin
			progress,percent=(i+j*patx)*100.0/(patx*paty),/check_cancel
			if wid.cancel eq 1 then return

			xpos = i*stpx
			arr1 = abs(reform(arr[0,xpos:xpos+pats-1,*]))
			arr2 = abs(reform(arr[1,xpos:xpos+pats-1,*]))
			
			ret = coreg_ampsub(arr1,arr2,q)
			xo[i,j] = ret[0]
			yo[i,j] = ret[1]
			xp[i,j] = xpos+pats/2
			yp[i,j] = ypos+pats/2
		endfor
	endfor     
	amp = abs(complex(xo,yo))
	aux = where(amp lt 2*median(amp),anz)
	if anz gt 0 then begin
		xo  = min_curve_surf(xo[aux],aux mod patx, aux / patx, gs=[1,1],bounds=[0,0,patx-1,paty-1])
		yo  = min_curve_surf(yo[aux],aux mod patx, aux / patx, gs=[1,1],bounds=[0,0,patx-1,paty-1])
	endif
	progress,/destroy

	if not keyword_set(called) then begin             ; Graphical interface
	
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Patched image registration',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		dr1   = widget_draw(main,XSIZE=500,ysize=400)

		field1 = CW_FIELD(main,VALUE=2,/integer,TITLE='Oversampling factor in x : ',XSIZE=2)
		field2 = CW_FIELD(main,VALUE=2,/integer,TITLE='Oversampling factor in y : ',XSIZE=2)
		
		buttons1 = WIDGET_BASE(main,column=2,/frame)
		edx   = WIDGET_BUTTON(buttons1,VALUE=' Edit x offsets ')
		edy   = WIDGET_BUTTON(buttons1,VALUE=' Edit y offsets ')

		buttons2 = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons2,VALUE=' Start coregistration ',/frame)
		but_canc = WIDGET_BUTTON(buttons2,VALUE=' Cancel ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]
	
		widget_control,dr1,get_value=index
		wset,index
		velovect,xo,yo,title='Coregistration result'

		repeat begin                                        ; Event loop
			event = widget_event(main)
			if event.id eq edx then begin               ; Info Button clicked
				xvaredit,xo,x_scroll_size=10,y_scroll_size=10
				velovect,xo,yo,title='Coregistration result'
			end
			if event.id eq edy then begin               ; Info Button clicked
				xvaredit,yo,x_scroll_size=10,y_scroll_size=10
				velovect,xo,yo,title='Coregistration result'
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,get_value=overx
		widget_control,field2,get_value=overy
		
		widget_control,main,/destroy                        ; remove main widget
		widget_control,wid.draw,get_value=index
		wset,index
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	
; switch back to main draw widget

	
	endif 

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	rrat,file.name,ddd,header=head,info=info,type=type
	srat,outputfile,eee,header=head,info=info,type=type
		
; calculating preview size and number of blocks

	bs = config.blocksize
	overlap = 2*(floor(max(abs(yo))+1))
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end

	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
	
; pop up progress window

	progress,Message='Warping slave image...'

;start block processing
	
	rgbin   = lindgen(file.xdim)
	rgbin_i = interpol(lindgen(patx),xp[*,0],rgbin)

	if overx gt 1 then begin
		bsx = 2                      ; search automatically for good blocksize
		while 2l^bsx lt file.xdim do bsx++  
		bsx1 = 2UL^bsx
		bsx2 = overx*bsx1
	endif
	if overy gt 1 then begin
		bsy = 2                      ; search automatically for good blocksize
		while 2l^bsy lt bs do bsy++  
		bsy1 = 2UL^bsy
		bsy2 = overy*bsy1
	endif
	
	for i=0,anz_blocks-1 do begin   
		progress,percent=(i+1)*100.0/anz_blocks

		block = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		arr = reform(block[1,*,*])

; -------- THE FILTER ----------
		azbin   = lindgen(blocksizes[i])+floor(total(blocksizes[0:i]))-blocksizes[i]-2*i*overlap
		azbin_i = interpol(lindgen(paty),yp[0,*],azbin)

; PART1 : Resampling in Range

		res     = interpolate(-xo,rgbin_i,azbin_i,/grid,cubic=-0.5)	

		if overx eq 1 then begin
			res    += (dblarr(blocksizes[i]) + 1) ## dindgen(file.xdim) 
			for k=0L,blocksizes[i]-1 do arr[*,k] = interpolate(arr[*,k],res[*,k],cubic=-0.5)
		endif else begin

			for k=0L,blocksizes[i]-1 do begin
				line = complexarr(bsx1)
				line[0]   = arr[*,k]
				fline     = fft(line,-1)
				ofline    = complexarr(bsx2)
				ofline[0] = fline[0:bsx1/2-1]
				ofline[bsx2-bsx1/2] = fline[bsx1/2:*]
				oline     = fft(ofline,+1)
				
				res2      = dindgen(bsx2) 
				res2[0:overx * file.xdim-1]  += rebin(overx * res[*,k], overx * file.xdim)
				oline     = interpolate(oline,res2,cubic=-0.5)
				
				ofline    = fft(oline,-1)
				fline     = [ofline[0:bsx1/2-1],ofline[bsx2-bsx1/2:*]]
				line      = fft(fline,+1)
				arr[*,k]  = line[0:file.xdim-1]
			endfor
		endelse
		
		
; PART2 : Resampling in Azimuth
		
		res     = interpolate(-yo,rgbin_i,azbin_i,/grid,cubic=-0.5)	

		if overy eq 1 then begin
			res    += dindgen(blocksizes[i])  ## (dblarr(file.xdim)+1) 
			for k=0,file.xdim-1 do arr[k,*] = interpolate(arr[k,*],res[k,*],cubic=-0.5)
		endif else begin
			for k=0L,file.xdim-1 do begin			
				line = complexarr(bsy1)
				line[0]   = reform(arr[k,*])
				fline     = fft(line,-1)
				ofline    = complexarr(bsy2)
				ofline[0] = fline[0:bsy1/2-1]
				ofline[bsy2-bsy1/2] = fline[bsy1/2:*]
				oline     = fft(ofline,+1)
				
				res2      = dindgen(bsy2) 
				res2[0:overx * blocksizes[i]-1]  += rebin(overy * res[k,*], overy * blocksizes[i])
				oline     = interpolate(oline,res2,cubic=-0.5)
				
				ofline    = fft(oline,-1)
				fline     = [ofline[0:bsy1/2-1],ofline[bsy2-bsy1/2:*]]
				line      = fft(fline,+1)
				arr[k,*]  = line[0:blocksizes[i]-1]
			endfor
		endelse

; -------- THE FILTER ----------

		block[1,*,*] = arr
		if i eq anz_blocks-1 then ypos2 = bs_last
		writeu,eee,block[*,*,ypos1:ypos2-1]
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
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
