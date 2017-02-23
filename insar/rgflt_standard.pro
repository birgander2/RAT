;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rgflt_standard
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
pro rgflt_standard,CALLED = called
	common rat, types, file, wid, config

        if file.type ne 300 and not (file.type ge 500 and file.type lt 510) then begin
           error = DIALOG_MESSAGE("This is not an interferometric pair ", DIALOG_PARENT = wid.base, TITLE='Error',/error)
           return
        endif

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Spectral range filtering',/floating,/tlb_kill_request_events,/tlb_frame_attr)

		text1 = CW_FIELD(main,VALUE=16.0,/floating,XSIZE=6,TITLE='Range bandwidth          [MHz] :')
		text2 = CW_FIELD(main,VALUE=19.20768 ,/floating,XSIZE=6,TITLE='Range sampling frequency [MHz] :')
		text3 = CW_FIELD(main,VALUE=7.0 ,/floating,XSIZE=6,TITLE='Spectral shift (slave)   [MHz] :')

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
				infotext = ['SPECTRAL RANGE FILTER V1.0',$
				' ',$
				'RAT module written 11/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

		widget_control,text1,get_value=bw
		widget_control,text2,get_value=rs
		widget_control,text3,get_value=ss

		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif

; calculate parameters

	empty = (1-bw/rs)*file.xdim  ; empty spectral range (in pixels)
	cut   = ss/rs*file.xdim      ; how much to cut (in pixels)
	flt = fltarr(file.xdim)+1.0
	if empty ge 1 then begin
		flt[0:empty/2-1] = 0.0
		flt[file.xdim-empty/2:*] = 0.0
	endif

	if ss ge 0 then begin
		flt1 = flt
		flt1[empty/2:empty/2+abs(cut)] = 0.0
		flt2 = flt
		flt2[file.xdim-empty/2-abs(cut):*] = 0.0
	endif else begin
		flt2 = flt
		flt2[empty/2:empty/2+abs(cut)] = 0.0
		flt1 = flt
		flt1[file.xdim-empty/2-abs(cut):*] = 0.0
	endelse
	flt1 = shift(flt1,file.xdim/2)
	flt2 = shift(flt2,file.xdim/2)

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
					block[0,j,*,k] = fft(fft(reform(block[0,j,*,k]),-1)*flt1,+1)
					block[1,j,*,k] = fft(fft(reform(block[1,j,*,k]),-1)*flt2,+1)
				endfor
			endfor
		endif else begin
			block = reform(block)
			for k=0,blocksizes[i]-1 do begin
				block[0,*,k] = fft(fft(reform(block[0,*,k]),-1)*flt1,+1)
				block[1,*,k] = fft(fft(reform(block[1,*,k]),-1)*flt2,+1)
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
