;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: construct_insar
; last revision : 11.Feb.2003
; written by    :  Andreas Reigber
; Combines two (single channel) RAT files into one interferometric pair                   
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



pro construct_insar,NOGUI=nogui
	common rat, types, file, wid, config
	
	if not keyword_set(nogui) then begin             ; Graphical interface
		inputfile1 = ''
		inputfile2 = ''
		tryagain:
		
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Construct InSAR pair',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		line1 = WIDGET_BASE(main,column=3)		
		text1 = CW_FIELD(line1,VALUE=inputfile1,/string,XSIZE=50,TITLE='RAT-file 1 (Master track) :')
		brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)

		line2 = WIDGET_BASE(main,column=2)		
		text2 = CW_FIELD(line2,VALUE=inputfile2,/string,XSIZE=50,TITLE='RAT-file 2 (Slave track)  :')
		brow2 = WIDGET_BUTTON(line2,VALUE=' browse ',ysize=35)

		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
;		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc, tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin                                        ; Event loop
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['2*RAT --> INSAR PAIR',$
				' ',$
				'RAT module written 2003 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
			if event.id eq brow1 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile1 = cw_rat_dialog_pickfile(TITLE='Open RAT file 1', DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
				if strlen(inputfile1) gt 0 then config.workdir = path
				widget_control,text1,set_value=inputfile1
			endif
			if event.id eq brow2 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile2 = cw_rat_dialog_pickfile(TITLE='Open RAT file 2', DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
				if strlen(inputfile2) gt 0 then config.workdir = path
				widget_control,text2,set_value=inputfile2
			endif
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
		WIDGET_CONTROL, /DESTROY, main
		if event.id ne but_ok then return     
	endif 

	if inputfile1 eq '' or inputfile2 eq '' then begin
		error = DIALOG_MESSAGE("Please select two files", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		goto,tryagain
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

	outputfile = config.tempdir+config.workfile2
	finalfile  = config.tempdir+config.workfile1

	head1 = 1l
	head2 = 1l
	rrat,inputfile1,ddd1,header=head1,info=info1,type=type1		
	if ddd1 eq -1 then begin
		error = DIALOG_MESSAGE("Not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	rrat,inputfile2,ddd2,header=head2,info=info2,type=type2		
	if ddd2 eq -1 then begin
		error = DIALOG_MESSAGE("Not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

	xdim1 = head1[1]
	xdim2 = head2[1]
	ydim1 = head1[2]
	ydim2 = head2[2]
	var1  = head1[3]
	var2  = head2[3]
	var   = 6l
	
	if head1[0] ne 2 or head2[0] ne 2 then begin
		error = DIALOG_MESSAGE("Data are not two-dimensional", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	if var1 ne 6 or var2 ne 6 then begin
		error = DIALOG_MESSAGE("Data not complex", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if xdim1 le xdim2 then xdim = xdim1 else xdim = xdim2 
	if ydim1 le ydim2 then ydim = ydim1 else ydim = ydim2 

	srat,outputfile,eee,header=[3l,2l,xdim,ydim,var],info=info1,type=300l		

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last
  
  progress,Message='Construct InSAR-image...'
	
;do the transform

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks
		block1 = make_array([xdim1,blocksizes[i]],type=var)
		block2 = make_array([xdim2,blocksizes[i]],type=var)
		oblock = make_array([2,xdim,blocksizes[i]],type=var)
		readu,ddd1,block1
		readu,ddd2,block2
		
		oblock[0,*,*] = block1[0:xdim-1,*]		
		oblock[1,*,*] = block2[0:xdim-1,*]		
		writeu,eee,oblock
	endfor
	free_lun,ddd1,ddd2,eee

; update file information
	file.name  = finalfile
	file.dim   = 3l
	file.xdim  = xdim
	file.ydim  = ydim
	file.zdim  = 2l
	file.vdim  = 1l
	file.var   = var
	file.type  = 300l
	file_move,outputfile,finalfile,/overwrite

        open_rit,/EMPTY
        evolute,'Construct InSAR from '+file_basename(inputfile1)+' and '+file_basename(inputfile2)

; generate preview
	file.window_name = 'Untitled.rat'
	generate_preview
	update_info_box

end
