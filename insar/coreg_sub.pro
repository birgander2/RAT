;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: coreg_sub
; written by    : Andreas Reigber
; last revision : 13. Oct 2004
; Interferometric coregistration based on amplitude correlation
; one value for the whole image only
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


function coreg_over,arr1,arr2
	osize = 512
   sizx = (size(arr1))[1]
   sizy = (size(arr1))[2]
   out1 = complexarr(sizx/2+1,sizy)
   out2 = complexarr(sizx/2+1,sizy)

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
	xpos = xoff * 1.0 / (osize / sizx)
	ypos = yoff * 1.0 / (osize / sizy)

	return,[xpos,ypos]
end

pro coreg_sub,CALLED = called, OFFSETX = offsetx, OFFSETY = offsety
	common rat, types, file, wid, config

	if file.type ne 300 and file.type ne 301 and file.type ne 302 then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram ", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if not keyword_set(offsetx) then offsetx = 0.0 				 ; Default values
	if not keyword_set(offsety) then offsety = 0.0
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Subpixel image registration',/modal)
		field1 = CW_FIELD(main,VALUE=offsetx,/floating,TITLE='Offset in x : ',XSIZE=5)
		field2 = CW_FIELD(main,VALUE=offsety,/floating,TITLE='Offset in y : ',XSIZE=5)
		but_estimate = WIDGET_BUTTON(main,VALUE=' Estimate (memory intensive)')
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
				infotext = ['SUBPIXEL IMAGE COREGISTRATION',$
				' ',$
				'RAT module written 2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		if (event.id eq but_estimate) then begin
			WIDGET_CONTROL,/hourglass

			ret = fltarr(2,4)
			bs  = 64
			rrat,file.name,arr,block=[file.xdim/4,file.ydim/4,bs,bs]
			ret[*,0] = coreg_over(abs(reform(arr[0,*,*])),abs(reform(arr[1,*,*])))
			rrat,file.name,arr,block=[file.xdim*3/4,file.ydim/4,bs,bs]
			ret[*,1] = coreg_over(abs(reform(arr[0,*,*])),abs(reform(arr[1,*,*])))
			rrat,file.name,arr,block=[file.xdim/4,file.ydim*3/4,bs,bs]
			ret[*,2] = coreg_over(abs(reform(arr[0,*,*])),abs(reform(arr[1,*,*])))
			rrat,file.name,arr,block=[file.xdim*3/4,file.ydim*3/4,bs,bs]
			ret[*,3] = coreg_over(abs(reform(arr[0,*,*])),abs(reform(arr[1,*,*])))
			ret = median(ret,dim=2)
			offsetx = ret[0]
			offsety = ret[1]

			widget_control,field1,set_value=offsetx
			widget_control,field2,set_value=offsety
		endif
		
		endrep until (event.id eq but_ok) or (event.id eq but_canc)
		widget_control,field1,GET_VALUE=offsetx             ; read widget fields
		widget_control,field2,GET_VALUE=offsety
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(offsetx) then offsetx = 0              ; Default values
		if not keyword_set(offsety) then offsety = 0
	endelse
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; calculating preview size and number of blocks

	bs = config.blocksize > 4*floor(abs(offsety) + 1) 
	overlap = floor(abs(offsety)) + 1

	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end

	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
	
; pop up progress window

	progress,Message='Shifting...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block

; -------- THE FILTER ----------
		arr2 = reform(block[1,*,*])
		if (offsetx ne 0) then for j=0,blocksizes[i]-1 do arr2[0,j] = interpolate(arr2[*,j],findgen(file.xdim)-offsetx,cubic = - 0.5) 
		if (offsety ne 0) then for j=0,file.xdim-1 do arr2[j,*] = interpolate(reform(arr2[j,*]),findgen(blocksizes[i])-offsety,cubic = - 0.5) 
		block[1,*,*] = arr2
; -------- THE FILTER ----------

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
