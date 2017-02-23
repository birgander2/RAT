;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_polreflee
; last revision : 17.Mar.2003
; written by    : Andreas Reigber
; Lee's refined speckle filter (polarimetric version)               
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

function lee_pol_ref,arr,smm,LOOKS=looks,THRESHOLD=threshold,METHOD=method,SINGLE=single
	compile_opt idl2

	if not keyword_set(looks) then looks=1
	if not keyword_set(threshold) then threshold=0.5
	if not keyword_set(method) then method=0

	siz    = size(arr)	
	anzz = siz[2]
	anzx = siz[3]
	anzy = siz[4]

	delta = (smm-1)/2
	dsh   = (delta+1)/2
	thr   = threshold
	sig2  = 1.0/looks
	sfak  = 1.0+sig2

	if keyword_set(single) then begin
		out = fltarr(1,anzz,anzx,anzy) 
		amp = total(total(arr,1),1)  ; Calculate span
		anzz2 = anzz
	endif else begin
		out = complexarr(anzz,anzz,anzx,anzy)
		amp = real_part(block_trace(arr))   ; Calculate span
		anzz2 = anzz^2
	endelse
	
; ---------------------------------------------
; TURNING BOX 
; ---------------------------------------------

	cbox = fltarr(9,smm,smm)
	chbox = fltarr(smm,smm)
	chbox[*,0:delta] = 1.
	cvbox = fltarr(smm,smm)
	for i=0,smm-1 do cvbox[0:i,i] = 1.
	
	cbox[0,*,*] = rotate(chbox,1)
	cbox[1,*,*] = rotate(cvbox,3)
	cbox[2,*,*] = rotate(chbox,2)
	cbox[3,*,*] = rotate(cvbox,0)
	cbox[4,*,*] = rotate(chbox,3)
	cbox[5,*,*] = rotate(cvbox,1)
	cbox[6,*,*] = rotate(chbox,0)
	cbox[7,*,*] = rotate(cvbox,2)
;	cbox[8,*,*] += 1.
        for i=0, delta do for j=delta-i, delta+i do cbox[8, i:smm-i-1, j] = 1
	
	for i=0,8 do cbox[i,*,*] /= total(cbox[i,*,*])

	dbox = fltarr(1,1,smm,smm)
	
	ampf1 = fltarr(9,anzx,anzy)
	ampf2 = fltarr(9,anzx,anzy)
	for i=0,8 do begin
		box  = reform(cbox[i,*,*])
		ampf1[i,*,*] = convol(amp^2,box,/center,/edge_truncate)
		ampf2[i,*,*] = convol(amp,box,/center,/edge_truncate)^2
	endfor
	
	case method of
; ---------------------------------------------
; GRADIENT ESTIMATION METHOD 1 
; ---------------------------------------------
		0: begin
			xs = [+2,+2, 0,-2,-2,-2, 0,+2]
			ys = [0 ,+2,+2,+2, 0,-2,-2,-2]
	
			samp = smooth(amp,delta)
			grad = fltarr(9,anzx,anzy)
			for i=0,7 do grad[i,*,*] = abs(shift(samp,xs[i],ys[i])/samp   - 1.0)
			mag = max(grad,dir,dim=1)
			dir mod= 9
			aux = where(mag lt thr,nr)
			if nr gt 0 then dir[aux] = 8
		end
; ---------------------------------------------
; GRADIENT ESTIMATION METHOD 2 
; ---------------------------------------------
		1: begin		
			samp = smooth(amp,delta)
			grad = fltarr(4,anzx,anzy)
			grad[0,*,*] = (shift(samp,+dsh,0)+shift(samp,+dsh,+dsh)+shift(samp,+dsh,-dsh)-shift(samp,-dsh,0)-shift(samp,-dsh,+dsh)-shift(samp,-dsh,-dsh)) ; vertical
			grad[1,*,*] = (shift(samp,+dsh,+dsh)+shift(samp,0,+dsh)+shift(samp,+dsh,0)-shift(samp,-dsh,-dsh)-shift(samp,0,-dsh)-shift(samp,-dsh,0) )
			grad[2,*,*] = (shift(samp,0,+dsh)+shift(samp,+dsh,+dsh)+shift(samp,-dsh,+dsh)-shift(samp,0,-dsh)-shift(samp,-dsh,-dsh)-shift(samp,+dsh,-dsh)) ; horizontal
			grad[3,*,*] = -(shift(samp,+dsh,-dsh)+shift(samp,+dsh,0)+shift(samp,0,-dsh)-shift(samp,-dsh,+dsh)-shift(samp,0,+dsh)-shift(samp,-dsh,0) )
			mag = max(grad,dir,dim=1,/absolute)
			dir mod= 4
			ind = where(mag le 0.0,nind)
			if nind gt 0 then dir[ind] += 4
		end
; ---------------------------------------------
; GRADIENT ESTIMATION METHOD 3 
; ---------------------------------------------
		2: begin
			samp = smooth(amp,delta)
			grad = fltarr(4,anzx,anzy)
			grad[0,*,*] = (shift(samp,+dsh,0)+shift(samp,+dsh,+dsh)+shift(samp,+dsh,-dsh)-shift(samp,-dsh,0)-shift(samp,-dsh,+dsh)-shift(samp,-dsh,-dsh)) ; vertical
			grad[1,*,*] = (shift(samp,+dsh,+dsh)+shift(samp,0,+dsh)+shift(samp,+dsh,0)-shift(samp,-dsh,-dsh)-shift(samp,0,-dsh)-shift(samp,-dsh,0) )
			grad[2,*,*] = (shift(samp,0,+dsh)+shift(samp,+dsh,+dsh)+shift(samp,-dsh,+dsh)-shift(samp,0,-dsh)-shift(samp,-dsh,-dsh)-shift(samp,+dsh,-dsh)) ; horizontal
			grad[3,*,*] = -(shift(samp,+dsh,-dsh)+shift(samp,+dsh,0)+shift(samp,0,-dsh)-shift(samp,-dsh,+dsh)-shift(samp,0,+dsh)-shift(samp,-dsh,0) )
			mag = max(grad,dir,dim=1,/absolute)
			dir mod= 4
			
			for i=0,3 do begin
				dummy1 = (ampf1[i,*,*] - ampf2[i,*,*]) / ampf2[i,*,*]
				dummy2 = (ampf1[i+4,*,*] - ampf2[i+4,*,*]) / ampf2[i+4,*,*]
				ind = where((dummy2 lt dummy1) and (dir eq i),nind)
				if nind gt 0 then dir[ind] += 4
			endfor
		end
; ---------------------------------------------
; GRADIENT ESTIMATION METHOD 4 
; ---------------------------------------------
		3: begin
			grad = fltarr(8,anzx,anzy)
			for i=0,7 do grad[i,*,*] = (ampf1[i,*,*] - ampf2[i,*,*]) / ampf2[i,*,*]
			mag = min(grad,dir,dim=1)
			dir mod= 8
		end
	endcase
;box  = reform(cbox[l,*,*]) & print,reverse(box,2)
;---------------------------------------------
; FILTERING	
;---------------------------------------------
        pre1 = lonarr(anzz2)+1
	pre2 = lindgen(anzz2)
	pre3 = lonarr(anzz2)

	for l=0,8 do begin                            ; loop over the different boxes
		box  = reform(cbox[l,*,*])
		dbox[0,0,*,*] = box
		grad = reform(ampf1[l,*,*])
		mamp = reform(ampf2[l,*,*])
		aux  = where(dir eq l,nr)
		if nr gt 0 then begin
			vary = grad[aux] - mamp[aux]
 			varx = (vary - mamp[aux]*sig2)/sfak > 0
			k    = varx / vary
			mamp = convol(arr,dbox,/center,/edge_truncate)

			i4d = (pre1 ## (aux*anzz2)) + (pre2 ## (lonarr(nr)+1))
			k4d = (pre1 ## k) ;+ (pre3 ## (fltarr(nr)+1))
			out[i4d] = mamp[i4d] + (arr[i4d] - mamp[i4d] ) * k4d			

		endif
	endfor

	err = where(finite(out) eq 0,nr)
   if nr gt 0 then out[err] = 0.0
	return,out
end



pro speck_polreflee,CALLED = called, SMM = smm, LOOKS = looks, THRESHOLD = threshold, METHOD_FLAG=mth_flag
	common rat, types, file, wid, config
	compile_opt idl2

	if not ((file.type ge 200 and file.type lt 210) or (file.type ge 220 and file.type le 230) or $
		 (file.type eq 100 or file.type eq 101 or file.type eq 103) or $
                (file.type ge 500 and file.type le 510) or (file.type ge 510 and file.type le 520)) $
        then begin
           error_button = DIALOG_MESSAGE(['Wrong data type'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
           return
        endif

;;; default values
	if n_elements(smm) eq 0 then smm=7
	if n_elements(looks) eq 0 then looks=1.
	if n_elements(threshold) eq 0 then threshold=6.0
	if n_elements(mth_flag) eq 0 then mth_flag=0

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Refined Polarimetric Lee Speckle Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=smm,/integer,  TITLE='Filter boxsize        : ',XSIZE=3)
		field2   = CW_FIELD(main,VALUE=strcompress(string(looks,f='(f9.1)'),/R),/float,TITLE='Effective No of Looks : ',XSIZE=3)
		field3   = CW_FIELD(main,VALUE=strcompress(string(threshold,f='(f9.1)'),/R),/float,TITLE='Threshold  (dB)       : ',XSIZE=3)
		field4   = CW_BGROUP(main,["Lee original (RoA)","Lee modified","Lee modified + coef. of var.","Coefficient of variation"],/frame,set_value=mth_flag,row=4,label_top='Mask selection method:',/exclusive)
		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]


		repeat begin
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['POLARIMETRIC REFINED LEE SPECKLE FILTER',$
				'also called refined LLMMSE filtering ',$
				' ',$
				'RAT module written 01/2002 by Andreas Reigber',$
				'updated 11/2005 by Andreas Reigber',$
				' ',$
				'further information:',$
				'J.S.Lee et al.: "Speckle Reduction in Multipolarization,',$
				'Multifrequency SAR Imagery", Trans. on Geoscience and Remote',$
				'Sensing, Vol. 29, No. 4, pp. 535-544, 1991']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=smm
		widget_control,field2,GET_VALUE=looks
		widget_control,field3,GET_VALUE=threshold
		widget_control,field4,GET_VALUE=mth_flag
		widget_control,main,/destroy
		if event.id ne but_ok then return                   ; OK button _not_ clicked
             endif
	threshold = alog10(threshold)

; Error Handling

	if smm le 0 and not keyword_set(called) then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; transform polarimetric vector to matrix

	if (file.type ge 200 and file.type le 210) or (file.type ge 500 and file.type lt 510) $
        then begin
           if not keyword_set(called) then dummy = DIALOG_MESSAGE(["Data are in vector form. They have","to be converted to matrix form first."], DIALOG_PARENT = wid.base, /information)
           if file.type ge 500 && file.type le 503 then polin_k2m,/called $
           else k_to_m,/called
           if wid.cancel eq 1 then return
        endif

; handling of single channel
	
	if file.type eq 100 or file.type eq 101 or file.type eq 103 then begin
		ampflag = 0
		if file.type eq 100 then ampflag = 1
		if file.type eq 101 then begin
			error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
			if error eq "Cancel" then return else complex2abs,/called
			if wid.cancel eq 1 then return
			ampflag = 1
		endif
	endif

	
; read / write header

	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; calculating preview size and number of blocks
		
	bs = config.blocksize
	overlap = (smm + 1) / 2
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end

	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
 
; pop up progress window

	progress,Message='Refined Lee speckle filter...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
; -------- THE FILTER ----------
		if file.type eq 100 or file.type eq 101 or file.type eq 103 then begin
			block = reform(block,file.vdim,file.zdim,file.xdim,blocksizes[i])
			if ampflag eq 1 then block = block^2
			block = lee_pol_ref(block,smm,LOOKS=looks,THRESHOLD=threshold,METHOD=mth_flag,/single)
			if ampflag eq 1 then block = sqrt(block)
		endif else begin
			block = lee_pol_ref(block,smm,LOOKS=looks,THRESHOLD=threshold,METHOD=mth_flag)
		endelse
; -------- THE FILTER ----------
		if i eq anz_blocks-1 then ypos2 = bs_last
		writeu,eee,block[*,*,*,ypos1:ypos2-1]
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
	endfor
	free_lun,ddd,eee

; update file information
	
	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile

        evolute,'Speckle filtering (Polarimetric RefLee): boxsize'+strcompress(smm,/R)+' looks: '+strcompress(looks,/R)+' Threshold: '+strcompress(threshold,/R)+ $
                ' Method: '+strcompress(mth_flag,/R)

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
