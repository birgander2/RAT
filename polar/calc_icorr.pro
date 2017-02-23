;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calc_icorr
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
pro calc_icorr,CALLED = called,channel1=channel1,channel2=channel2
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag

	if not (file.type eq 100 or file.type eq 101 or file.type eq 600 or file.type eq 601 or (file.type ge 200 and file.type le 250)) then begin
		error_button = DIALOG_MESSAGE(['Wrong file type','SAR image, scattering vector','covariance required'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
	
	if not (file.zdim gt 1 or file.vdim gt 1) then begin
		error_button = DIALOG_MESSAGE(['Vector information required','to calculate ratios'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
	
	if (file.type ge 200 and file.type le 210)  or file.type eq 600 or file.type eq 601 then matrixflag = 0 else matrixflag = 1

	smmx = 1
	smmy = 1

	if not keyword_set(called) then begin             ; Graphical interface
		ch = channel_names

		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Interchannel correlation calculation',/floating,/tlb_kill_request_events,/tlb_frame_attr)		

		if matrixflag eq 0 then begin
			sub  = widget_base(main,column=2)
			sub1 = widget_base(sub,row=3,/frame)
			text = widget_label(sub1,value='Select master channel:')
			butt1 = cw_bgroup(sub1,ch,/exclusive,column=1,set_value=0)		
			sub2 = widget_base(sub,row=3,/frame)
			text = widget_label(sub2,value='Select slave channel :')
			butt2 = cw_bgroup(sub2,ch,/exclusive,column=1,set_value=1)
			field1 = CW_FIELD(main,VALUE=7,/integer,TITLE='Filter boxsize X   : ',XSIZE=3)
			field2 = CW_FIELD(main,VALUE=7,/integer,TITLE='Filter boxsize Y   : ',XSIZE=3)
		endif
		if matrixflag eq 1 then begin
			text = widget_label(main,value='Select channel:')
			butt1 = cw_bgroup(main,ch,/exclusive,column=file.vdim,set_value=0)		
		endif

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
				infotext = ['INTERCHANNEL CORRELATION CALCULATION',$
				' ',$
				'RAT module written 10/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,butt1,GET_VALUE=channel1
		if matrixflag eq 0 then begin
			widget_control,butt2,GET_VALUE=channel2
			widget_control,field1,GET_VALUE=smmx                ; read widget fields
			widget_control,field2,GET_VALUE=smmy
		endif 
		widget_control,main,/destroy
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; calculate new type

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,4l],info=info,type=310l
	
; calculating preview size and number of blocks
		
	overlap = smmy / 2 
	bs = config.blocksize
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end
	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos

; pop up progress window

	progress,Message='Extracting interchannel correlation...',/cancel_button

; calculating span
	
	if matrixflag eq 1 then begin
		channel2 = channel1 / file.zdim
		channel1 = channel1 mod file.zdim
	endif
	smm = [1,1,smmx,smmy]
	
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block

		if file.type eq 601 then begin
			oblock = abs(smooth(block[channel1 mod file.vdim,channel1 / file.vdim,*,*] * conj(block[channel2 mod file.vdim,channel2 / file.vdim,*,*]),smm,/edge_truncate) / $
			sqrt(smooth(abs(block[channel1 mod file.vdim,channel1 / file.vdim,*,*])^2,smm,/edge_truncate)*smooth(abs(block[channel2 mod file.vdim,channel2 / file.vdim,*,*])^2,smm,/edge_truncate)))

		endif else begin
			if matrixflag eq 0 then oblock = abs(smooth(block[0,channel1,*,*] * conj(block[0,channel2,*,*]),smm,/edge_truncate))/sqrt(smooth(abs(block[0,channel1,*,*])^2,smm,/edge_truncate)*smooth(abs(block[0,channel2,*,*])^2,smm,/edge_truncate))
			if matrixflag eq 1 then oblock = abs(block[channel1,channel2,*,*]) / sqrt(abs(block[channel1,channel1,*,*]) * abs(block[channel2,channel2,*,*]))
		endelse
		aux = where(finite(oblock) eq 0,nr)
		if nr gt 0 then oblock[aux] = 0.0

; block processing
		if i eq anz_blocks-1 then ypos2 = bs_last

		writeu,eee,oblock[*,*,*,ypos1:ypos2-1]
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.var  = 4l
	file.dim  = 2l
	file.vdim = 1l 
	file.zdim = 1l 
	file.type = 310l
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif

end
