;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rm_flatearth_file
; written by    : Andreas Reigber
; last revision : 12.December.2003
; Flat-earth removal from file
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

pro rm_flatearth_file,CALLED = called, INPUTFILE1 = inputfile1, INPUTFILE2 = inputfile2
	common rat, types, file, wid, config

	if file.type ne 300 and file.type ne 301 and file.type ne 302 then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram ", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Flat-earth removal from file',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		line1 = WIDGET_BASE(main,column=3)		
		text1 = CW_FIELD(line1,VALUE=inputfile1,/string,XSIZE=50,TITLE='Flat-earth file (x) :')
		brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)

		line2 = WIDGET_BASE(main,column=3)		
		text2 = CW_FIELD(line2,VALUE=inputfile2,/string,XSIZE=50,TITLE='Flat-earth file (y) :')
		brow2 = WIDGET_BUTTON(line2,VALUE=' browse ',ysize=35)

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
				infotext = ['FLAT-EARTH REMOVAL FROM FILE',$
				' ',$
				'RAT module written 12/2003 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
			if event.id eq brow1 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile1 = DIALOG_PICKFILE(TITLE='Open flat-earth file', DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
				if strlen(inputfile1) gt 0 then begin
					config.workdir = path
					rrat,inputfile1,fex
					aux = size(fex)
					if aux[0] ne 1 or aux[1] ne file.xdim then begin
						error = DIALOG_MESSAGE("Array size does not correspond to image size", DIALOG_PARENT = wid.base, TITLE='Error',/error)
						fex = 0
						inputfile=''
					endif 
					widget_control,text1,set_value=inputfile1
				endif
			endif
			if event.id eq brow2 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile2 = DIALOG_PICKFILE(TITLE='Open flat-earth file', DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
				if strlen(inputfile2) gt 0 then begin
					config.workdir = path
					rrat,inputfile2,fey
					aux = size(fey)
					if aux[0] ne 1 or aux[1] ne file.ydim then begin
						error = DIALOG_MESSAGE("Array size does not correspond to image size", DIALOG_PARENT = wid.base, TITLE='Error',/error)
						delvar,fey
						inputfile=''
					endif 
					widget_control,text2,set_value=inputfile2
				endif
			endif
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,text1,get_value=inputfile1
		widget_control,text2,get_value=inputfile2
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif

	if strlen(inputfile1) gt 0 and file_test(inputfile1) then rrat,inputfile1,fex else fex = fltarr(file.xdim)
	if strlen(inputfile2) gt 0 and file_test(inputfile2) then rrat,inputfile2,fey else fey = fltarr(file.ydim)

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

	progress,Message='Removing flat-earth...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block

; -------- THE FILTER ----------
		if file.type eq 302 then block = exp(complex(0,block))
		for k=0,blocksizes[i]-1 do block[0,*,k] = block[0,*,k] * exp(complex(0,fex))
		for k=0,file.xdim-1 do block[0,k,*] = block[0,k,*] * exp(complex(0,fey[total(blocksizes[0:i])-blocksizes[i]:total(blocksizes[0:i])-1]))		
		if file.type eq 302 then block = atan(block,/phase)
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
