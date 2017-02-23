;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rm_flatearth_fft
; written by    : Andreas Reigber
; last revision : 14.Mar.2003
; Linear flat-earth removal including estimation of dominant fringe
; frequency
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
 
pro rm_flatearth_fft,CALLED = called, FRINGEX = fringex, FRINGEY = fringey
	common rat, types, file, wid, config
   compile_opt idl2

	if file.type ne 300 and file.type ne 301 and file.type ne 302 then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram ", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if not keyword_set(fringex) then fringex = 0              ; Default values
	if not keyword_set(fringey) then fringey = 0
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Flat-earth removal',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=fringex,/integer,TITLE='No. of fringes in x : ',XSIZE=6)
		field2 = CW_FIELD(main,VALUE=fringey,/integer,TITLE='No. of fringes in y : ',XSIZE=6)
		but_estimate = WIDGET_BUTTON(main,VALUE=' Estimate (memory intensive) ')
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
				infotext = ['INSAR FLAT-EARTH REMOVAL',$
				' ',$
				'RAT module written 2003 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		if (event.id eq but_estimate) then begin
			WIDGET_CONTROL,/hourglass
			
			bsx = 2                      ; search automatically for good blocksize
			while 2l^bsx lt file.xdim and bsx lt 12 do bsx++
			bsx = 2^--bsx
			bsy = 2                      ; search automatically for good blocksize
			while 2l^bsy lt file.ydim and bsy lt 12 do bsy++
			bsy = 2^--bsy

			rrat,file.name,arr,block=[file.xdim/2-bsx/2,file.ydim/2-bsy/2,bsx,bsy]
			if file.type eq 300 then arr = reform(arr[0,*,*] * conj(arr[1,*,*] ))	
			if file.type eq 302 then arr = exp(complex(0,arr))			
			aux = max(abs(fft(arr,-1)),pos)
			fringex = pos mod bsx
			fringey = pos  /  bsx
			if fringex gt bsx/2 then fringex = fringex - bsx
			if fringey gt bsy/2 then fringey = fringey - bsy
			
			fringex *= (float(file.xdim)/bsx)
			fringey *= (float(file.xdim)/bsx)
			
			arr = 0
			widget_control,field1,set_value=-fringex
			widget_control,field2,set_value=-fringey
		endif
		
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		inputfile  = file.name
		widget_control,field1,GET_VALUE=fringex             ; read widget fields
		widget_control,field2,GET_VALUE=fringey
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(smmx) then fringex = 0              ; Default values
		if not keyword_set(smmy) then fringey = 0
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

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last
		
;do the transform

	corrx = findgen(file.xdim)/file.xdim*2*!pi*fringex
	corry = findgen(file.ydim)/file.ydim*2*!pi*fringey

; pop up progress window

	progress,Message='Removing flat-earth...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block

; -------- THE FILTER ----------
		corr = (fltarr(blocksizes[i]) + 1) ## corrx + corry[i*bs:i*bs+blocksizes[i]-1] ## (fltarr(file.xdim) + 1)
		if file.type eq 302 then block = exp(complex(0,block))
		block[0,0,*,*] = block[0,0,*,*] * exp(complex(0,corr))
		if file.type eq 302 then block = atan(block,/phase)
; -------- THE FILTER ----------

		writeu,eee,block 
	endfor
	free_lun,ddd,eee

; update file information
	
	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
        file.info += ' (FFT-fe removed)'

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
