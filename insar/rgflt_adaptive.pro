;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rgflt_adaptive
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
pro rgflt_adaptive,CALLED = called
	common rat, types, file, wid, config

	if file.type ne 300 and not (file.type ge 500 and file.type lt 510) then begin                   
		error = DIALOG_MESSAGE("This is not an interferometric pair ", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
;  	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Adaptive spectral range filtering',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		text1 = CW_FIELD(main,VALUE=100.0,/floating,XSIZE=6,TITLE='Range bandwidth          [MHz] :')
		text2 = CW_FIELD(main,VALUE=100.0,/floating,XSIZE=6,TITLE='Range sampling frequency [MHz] :')
		draw1 = widget_draw(main,XSIZE=300,ysize=200)
		but_br = WIDGET_BUTTON(main,VALUE=' Select flat-earth file ',/frame)
		
		sub0 = widget_base(main,row=4,/frame)
		sub1 = widget_base(sub0,/row)
			RemHamButton = cw_bgroup(sub1,set_value = 1,['Remove Hamming   '],/nonexclusive)
			RemHamValText = cw_field(sub1,value=0.54,title=' ',/float,xsize=5)
		sub2 = widget_base(sub0,/row)
			AddHamButton = cw_bgroup(sub2,set_value = 1,['Apply New Hamming'],/nonexclusive)
			AddHamValText = cw_field(sub2,value=0.54,title=' ',/float,xsize=5)
		sub3 = widget_base(sub0,/row)
			RemFeButton = cw_bgroup(sub3,set_value = 1,['Remove Flatearth Phase'],/nonexclusive)
		
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
				infotext = ['ADAPTIVE SPECTRAL RANGE FILTER',$
				' ',$
				'RAT module written 11/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			endif
			if event.id eq but_br then begin
				path = config.workdir
				fefile = DIALOG_PICKFILE(TITLE='Open RAT file', DIALOG_PARENT=main, FILTER = '*.rat', /MUST_EXIST, PATH=path)
                                if fefile eq '' then continue
				rrat,fefile,fe
				widget_control,draw1,get_value=index
				wset,index
				plot,fe
				widget_control,wid.draw,get_value=index
				wset,index
			endif
			
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		
		widget_control,text1,get_value=bw
		widget_control,text2,get_value=rs
		widget_control,RemHamButton,get_value=but1
		widget_control,AddHamButton,get_value=but2
		widget_control,RemFeButton,get_value=but3
		widget_control,RemHamValText,get_value=alpha1
		widget_control,AddHamValText,get_value=alpha2

		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif

; calculate parameters

;	cut = abs((deriv(fe))[file.xdim/2]/2/!pi*file.xdim)
	cut = floor(max(abs(deriv(fe)))/2/!pi*file.xdim)
;	stop
	cor = exp(complex(0,float(fe/2)))
	empty = (1-bw/rs)*file.xdim  ; empty spectral range (in pixels)
	cut += empty

	flt = fltarr(file.xdim)+1.0
	if cut ge 1 then begin
		flt[0:cut/2-1] = 0.0
		flt[file.xdim-cut/2:*] = 0.0
	endif
	flt = shift(flt,file.xdim/2)
	
	ham = 1 / hamming(file.xdim-empty,total=file.xdim,alpha=alpha1)
	rmnanq,ham
	
	if but2 eq 1 then flt *= hamming(file.xdim-cut,total=file.xdim,alpha=alpha2)
;	stop
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
		
; pop up progress window

	progress,Message='Spectral range filtering...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block

; -------- THE FILTER ----------
		if (file.type ge 500 and file.type lt 510) then begin
			for j=0,file.zdim-1 do begin
				for k=0,blocksizes[i]-1 do begin
					if but1 eq 1 then line = fft(fft(reform(block[0,j,*,k]),-1) * ham,+1) else line = reform(block[0,j,*,k])
					block[0,j,*,k] = fft(fft(line * cor,-1)*flt,+1)
					if but3 eq 0 then block[0,j,*,k] *= conj(cor)
					if but1 eq 1 then line = fft(fft(reform(block[1,j,*,k]),-1) * ham,+1) else line = reform(block[1,j,*,k])
					block[1,j,*,k] = fft(fft(line * conj(cor),-1)*flt,+1)
					if but3 eq 0 then block[1,j,*,k] *= cor
				endfor
			endfor
		endif else begin
			block = reform(block)
			for k=0,blocksizes[i]-1 do begin
				if but1 eq 1 then line = fft(fft(reform(block[0,*,k]),-1) * ham,+1) else line = reform(block[0,*,k])
				block[0,*,k] = fft(fft(line * cor,-1)*flt,+1)
				if but3 eq 0 then block[0,*,k] *= conj(cor)

				if but1 eq 1 then line = fft(fft(reform(block[1,*,k]),-1) * ham,+1) else line = reform(block[1,*,k])
				block[1,*,k] = fft(fft(line * conj(cor),-1)*flt,+1)
				if but3 eq 0 then block[1,*,k] *= cor
			endfor
		endelse
; -------- THE FILTER ----------

		writeu,eee,block 
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
