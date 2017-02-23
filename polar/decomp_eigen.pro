;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: decomp_eigen
; written by    : Andreas Reigber
; last revision : 22.April.2004
; Calculates polarimetric Pauli decomposition
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



pro decomp_eigen,CALLED=called,SMM=smm
	common rat, types, file, wid, config

; check if array is usable

	if file.type lt 220 or file.type gt 229 then begin
		error_button = DIALOG_MESSAGE(['Data has to be a spatially averaged','polarimetric matrix like [C] or [T].',' ','(try a speckle filter now ?)'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Polarimetric Eigendecomposition',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=1,/integer,TITLE='Internal smooth   : ',XSIZE=3)
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
				infotext = ['POLARIMETRIC EIGENDECOMPOSITION',$
				' ',$
				'RAT module written 04/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=smm                ; read widget fields
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(smm) then smm = 1              ; Default values
	endelse

; go

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
	
; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[4l,file.vdim+1,file.zdim,file.xdim,file.ydim,6l],info=info,type=214l	

; calculating preview size and number of blocks

	if smm gt 1 then begin
		bs = config.blocksize
		overlap = (smm + 1) / 2
		calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
		blocksizes = intarr(anz_blocks)+bs
		blocksizes[anz_blocks-1] = bs_last
		ypos1 = 0                       ; block start
		ypos2 = bs - overlap            ; block end
		byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
	endif else begin
		bs = config.blocksize
		calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
		blocksizes = intarr(anz_blocks)+bs
		blocksizes[anz_blocks-1] = bs_last
	endelse	
	
; pop up progress window

	progress,Message='Eigenvector Decomposition...',/cancel_button

; calculating eigenvectors

	index = reverse(indgen(file.zdim))
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block   = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		oblock  = make_array([file.vdim+1,file.zdim,file.xdim,blocksizes[i]],type=6l)
		readu,ddd,block
		if smm gt 1 then block = smooth(block,[1,1,smm,smm],/edge_truncate)
		for k=0,file.xdim-1 do begin
			for l=0,blocksizes[i]-1 do begin
				oblock[file.vdim,*,k,l]       = (la_eigenql(reform(block[*,*,k,l]),eigenvectors=ev))[index]
				oblock[0:file.vdim-1,*,k,l]   = ev[*,index]
			endfor
		endfor
		if smm gt 1 then begin
			if i eq anz_blocks-1 then ypos2 = bs_last
			writeu,eee,oblock[*,*,*,ypos1:ypos2-1]
			ypos1 = overlap
			point_lun,-ddd,file_pos
			point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
		endif else writeu,eee,oblock
	endfor
	free_lun,ddd,eee
	
; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 214l
	file.vdim = file.vdim + 1
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
