;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: k_to_m
; written by       : Andreas Reigber
; last modified by : Maxim Neumann
; last revision    : 21. October 2004
; Transforms polarimtric vectors into its covariance matrices
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

pro k_to_m,CALLED=called,SMMX=smmx,SMMY=smmy
	common rat, types, file, wid, config

; check if array is usable

	if not ((file.type ge 200 and file.type lt 211) or (file.type ge 500 and file.type lt 510)) then begin
		error_button = DIALOG_MESSAGE(['Data have to be in','vector format.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	if not keyword_set(smmx) then smmx = 1    ; Routine called with keywords  
	if not keyword_set(smmy) then smmy = 1    ; Default values         

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Vector -> Matrix Transform',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=smmx,/integer,TITLE='Presumming X   : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=smmy,/integer,TITLE='Presumming Y   : ',XSIZE=3)
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
				infotext = ['VECTOR TO MATRIX TRANSFORM',$
				' ',$
				'RAT module written 11/2004 by Andreas Reigber & Maxim Neumann',$
				'minor updates 01/2007 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=smmx                ; read widget fields
		widget_control,field2,GET_VALUE=smmy
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif

; Error Handling

	if smmx le 0 or smmy le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED


; calculating preview size and number of blocks

	bs = config.blocksize
	if bs ne bs / smmy * smmy then bs =  (bs / smmy + 1) * smmy 
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	if bs_last ne bs_last / smmy * smmy then bs_last =  bs_last / smmy * smmy 
	blocksizes[anz_blocks-1] = bs_last
	if bs_last eq 0 then begin
		anz_blocks -= 1
		blocksizes = blocksizes[0:anz_blocks-1]
	endif

; calculating new dimensions

	xend = file.xdim / smmx * smmx - 1
	xnew = file.xdim / smmx
	ynew = total(blocksizes)/smmy
	dnew = file.zdim
	if file.type ge 500 and file.type lt 510 then dnew = file.vdim*file.zdim

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	if file.type ge 200 and file.type lt 210 then newtype = 220l
	if file.type eq 210 then newtype = 221l
	if file.type ge 500 and file.type lt 510 then newtype = file.type + 10
	
	srat,outputfile,eee,header=[4l,dnew,dnew,xnew,ynew,file.var],info=info,type=newtype

; pop up progress window

	progress,Message='Vector -> Matrix...',/cancel_button

; calculating span
	
	for i=0L,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
                if file.type ge 500 and file.type lt 510 then block = reform(block,[file.vdim*file.zdim,file.xdim,file.ydim],/overwrite)
		block = reform(block,/overwrite)
		oblock = block_xprod(block,conj(block))
		
; presumming
				
		if smmx gt 1 then oblock = oblock[*,*,0:xend,*]
		oblock = complex(rebin(real_part(oblock),dnew,dnew,xnew,blocksizes[i]/smmy),rebin(imaginary(oblock),dnew,dnew,xnew,blocksizes[i]/smmy))

		writeu,eee,oblock
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = newtype
	file.dim  = 4l
	file.xdim = xnew
	file.ydim = ynew
	file.zdim = dnew
	file.vdim = dnew

        evolute,'Vector to Matrix transformation. Presumming: '+strcompress(smmx)+', '+strcompress(smmy)

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
