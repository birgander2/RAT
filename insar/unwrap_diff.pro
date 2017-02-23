;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: unwrap_diff
; written by    : Andreas Reigber
; last revision : 03.July.2003
; Difference phase calculation
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

pro unwrap_diff,CALLED = called, FILE1 = file1, FILE2 = file2
	common rat, types, file, wid, config

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Least-squares phase unwrapping',/floating,/tlb_kill_request_events,/tlb_frame_attr)

		line1 = WIDGET_BASE(main,column=3)		
		text1 = CW_FIELD(line1,VALUE=inputfile1,/string,XSIZE=50,TITLE='Phase 1   :')
		brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)

		line2 = WIDGET_BASE(main,column=2)		
		text2 = CW_FIELD(line2,VALUE=inputfile2,/string,XSIZE=50,TITLE='Phase 2   :')
		brow2 = WIDGET_BUTTON(line2,VALUE=' browse ',ysize=35)

		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin                                        ; Event loop
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['CALCULATE DIFFERENCE PHASE',$
				' ',$
				'RAT module written 07/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
			if event.id eq brow1 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile1 = DIALOG_PICKFILE(TITLE='Open RAT file 1', DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
				if strlen(inputfile1) gt 0 then config.workdir = path
				widget_control,text1,set_value=inputfile1
			endif
			if event.id eq brow2 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile2 = DIALOG_PICKFILE(TITLE='Open RAT file 2', DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
				if strlen(inputfile2) gt 0 then config.workdir = path
				widget_control,text2,set_value=inputfile2
			endif
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; Error Handling

	if inputfile1 eq '' or inputfile2 eq '' then begin
		error = DIALOG_MESSAGE("Please select two files", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head1 = 1l
	rrat,inputfile1,ddd1,header=head1,info=info,type=type1		
	head2 = 1l
	rrat,inputfile2,ddd2,header=head2,info=info,type=type2		
	
	if (head1[0] ne head2[0]) or (head1[1] ne head2[1]) or (head1[2] ne head2[2]) or (head1[3] ne head2[3]) then begin
		error = DIALOG_MESSAGE("Files not matching", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	if ((type1 ne 302) and (type1 ne 303)) or ((type2 ne 302) and (type2 ne 303)) then begin
		error = DIALOG_MESSAGE("Wrong file types", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

	head = [2l,head1[1],head1[2],4l]
	srat,outputfile,eee,header=head,info=info,type=302l		

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last
	
; pop up progress window

	progress,Message='Calculate phase difference...',/cancel_button

; Calc difference

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block1 = make_array([file.xdim,blocksizes[i]],type=file.var)
		block2 = make_array([file.xdim,blocksizes[i]],type=file.var)
		readu,ddd1,block1
		readu,ddd2,block2
; -------- THE FILTER ----------
		writeu,eee,atan(exp(complex(0,block1 - block2)),/phase)
; -------- THE FILTER ----------
	endfor
	free_lun,ddd1,ddd2,eee
	
; update file information
	
	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.dim  = 2l
	file.zdim = 1l
	file.var  = 4l
	file.type = 302l

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
