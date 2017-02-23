;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: extract_channels
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
pro extract_channels,CALLED = called,channel=channel
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag

; ---- Is it a multilayer image?

	if (file.mult gt 1) then begin
		error = dialog_message(['This is a multitemporal data set.',$
		                        'The individual channels can be accessed',$
										'directly by loading the respective files.'],DIALOG_PARENT=wid.base,TITLE='Error',/ERROR)
		return
	endif
	
	if (file.dim eq 2) then begin
		error = dialog_message("Not a multilayer file",DIALOG_PARENT=wid.base,TITLE='Error',/ERROR)
		return
	endif
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Channel extraction',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		text = widget_label(main,value='Select channel to extract:')
		
;  		ch = strarr(file.vdim,file.zdim)
;  		for i=0,file.vdim*file.zdim-1 do begin
;  			if file.vdim gt 1 then ch[i] = ' Channel '+strcompress((i mod file.vdim)+1,/remove)+'/'+strcompress((i / file.vdim)+1,/remove)
;  			if file.vdim eq 1 then ch[i] = ' Channel '+strcompress(i+1,/remove)
;  		endfor
		ch = channel_names
		
;  		case file.type of
;  			200 : ch = (file.zdim EQ 2) ?  ['HH','VV'] : ((file.zdim EQ 3) ? ['HH','VV','HV']:['HH','VV','HV','VH'])
;  			201 : ch = ch
;  			202 : ch = ch
;  			208 : ch = ch
;  			209 : ch = ch
;  			210 : ch = (file.zdim EQ 3) ? ['HH+VV','HH-VV','2HV'] : ['HH+VV','HH-VV','HV+VH','i*(HV-VH)']
;  			211 : ch = ['Double Bounce','Volume','Surface']
;  			212 : ch = ch
;  			213 : ch = ['Sphere','Diplane','Helix']
;  			214 : ch = ch
;  			215 : ch = ['Entropy','Alpha','Anisotropy']
;  			220 : ch = [['C_11','C_21','C_31'],['C_12','C_22','C_32'],['C_13','C_23','C_33']]
;  			221 : ch = [['T_11','T_21','T_31'],['T_12','T_22','T_32'],['T_13','T_23','T_33']]
;  			300 : ch = ['Master track','Slave track']
;  			404 : ch = ['forest','surface','double']
;  			else : begin
;  				error = dialog_message("Not a multilayer file",DIALOG_PARENT=wid.base,TITLE='Error',/ERROR)
;  				return
;  			end
;  		endcase
;  		if file.type eq 200 then ch = (file.zdim EQ 2) ?  ['HH','VV'] : ((file.zdim EQ 3) ? ['HH','VV','HV']:['HH','VV','HV','VH'])
;  		if file.type eq 210 then ch = (file.zdim EQ 3) ? ['HH+VV','HH-VV','2HV'] : ['HH+VV','HH-VV','HV+VH','i*(HV-VH)']
;  		if file.type eq 211 then ch = ['Double Bounce','Volume','Surface']
;  		if file.type eq 213 then ch = ['Sphere','Diplane','Helix']
;  		if file.type eq 215 then ch = ['Entropy','Alpha','Anisotropy']
;  ;		if file.type eq 220 then ch = 
;  		if file.type eq 300 then ch = ['Master track','Slave track']
;  		if file.type eq 404 then ch = ['forest','surface','double']
		
		butt = cw_bgroup(main,ch,/exclusive,row=file.zdim)
		
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
				infotext = ['CHANNEL EXTRACTION',$
				' ',$
				'RAT module written 04/2003 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' 
		widget_control,butt,GET_VALUE=channel
		widget_control,main,/destroy
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; calculate new type

	newtype = 50l
	if file.var eq 6 or file.var eq 9 then newtype = 53l
	if file.type ge 200 and file.type lt 210 then newtype = 101l
	if file.type ge 300 then newtype = 101l
	if file.type eq 233 then newtype = 230l + channel
	if file.type eq 220 then newtype = 103l
	if file.type eq 221 then newtype = 103l
	if file.type eq 404 then newtype = 404l + channel
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,file.var],info=info,type=newtype
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='Extracting channel...',/cancel_button

; calculating span
	
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		
		; Case of the terrain classification
		IF file.type EQ 404 THEN BEGIN
			oblock = block * 0
			IF channel EQ 0 THEN BEGIN
				dummy = WHERE(block EQ 1,count)
				IF count GT 0 THEN oblock[dummy] = 1
			ENDIF
			IF channel EQ 1 THEN BEGIN
				dummy = WHERE((block EQ 3) OR (block EQ 5), count)
				IF count GT 0 THEN oblock[dummy] = 3
			ENDIF
			IF channel EQ 2 THEN BEGIN
				dummy = WHERE((block EQ 2) OR (block EQ 4), count)
				IF count GT 0 THEN oblock[dummy] = 2
			ENDIF
		ENDIF ELSE BEGIN
			if file.vdim eq 1 then oblock = block[0l,channel,*,*]
			if file.vdim gt 1 then oblock = block[channel mod file.vdim,channel / file.vdim,*,*]
		ENDELSE
		writeu,eee,oblock
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.dim  = 2l
	file.vdim = 1l 
	file.zdim = 1l 
	file.type = newtype 
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif

end
