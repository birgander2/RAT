;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: image_resize
; last revision : 12.Feb.2003
; written by    : Andreas Reigber
; Resizes images             
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



pro image_resize,CALLED = called, SMMX = smmx, SMMY = smmy
	common rat, types, file, wid, config

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Resize Image',/modal)
		field1 = CW_FIELD(main,VALUE=file.xdim,/integer,TITLE='New size in X   : ',XSIZE=4)
		field2 = CW_FIELD(main,VALUE=file.ydim,/integer,TITLE='New size in Y   : ',XSIZE=4)
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
				infotext = ['RESIZE IMAGE',$
				' ',$
				'RAT module written 2003 Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) 
		widget_control,field1,GET_VALUE=smmx                ; read widget fields
		widget_control,field2,GET_VALUE=smmy
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(smmx) then smmx = file.xdim      ; Default values
		if not keyword_set(smmy) then smmy = file.ydim 
	endelse

; Error Handling

	if smmx le 1 or smmy le 1 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Image sizes have to be > 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; scaling factors
		
	xnew   = smmx 		
	scalex = xnew*1.0 / file.xdim			
	ynew   = smmy
	scaley = ynew*1.0 / file.ydim			
	smmx = 1 / scalex
	smmy = 1 / scaley
	
	xposn = findgen(xnew) / xnew * file.xdim
	yposn = findgen(ynew) / ynew * file.ydim

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	head[file.dim-1] = xnew		
	head[file.dim]   = ynew	
	srat,outputfile,eee,header=head,info=info,type=type		
		
; calculating preview size and number of blocks
		
	bs = config.blocksize
	overlap = floor((smmy + 3) / 2)
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end
	ypos  = 0

	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
 
;smooth box

	smm_box = [1,1,smmx,smmy]

; pop up progress window

	progress,Message='Resize image...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
; -------- THE FILTER ----------
		if file.type eq 102 or file.type eq 302 then block = exp(complex(0,block))   ; go to complex for phase values
		block = smooth(block,smm_box,/edge_truncate)
		if i eq anz_blocks-1 then ypos2 = bs_last
		aux   = where(yposn le ypos + ypos2,anzn)
		if anzn ne 0 then begin
			var = file.var
			if file.type eq 102 or file.type eq 302 then var = 6			
			oblock = make_array([file.vdim,file.zdim,xnew,anzn],type=var)
			for j=0,file.vdim-1 do for k=0,file.zdim-1 do oblock[j,k,*,*] = interpolate(reform(block[j,k,*,*]),xposn,yposn[aux]-ypos,/grid,cubic=-0.5)
			yposn[aux] = 2*max(yposn)
		endif
		if file.type eq 102 or file.type eq 302 then oblock = atan(oblock,/phase)
; -------- THE FILTER ----------
		writeu,eee,oblock
		ypos1 = overlap
		ypos  = ypos + bs - 2*overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
	endfor
	free_lun,ddd,eee

; update file information
	
	file.name = finalfile
	file.xdim = xnew
	file.ydim = ynew
	file_move,outputfile,finalfile,/overwrite

; generate preview

	if not keyword_set(called) then begin
		update_info_box
		generate_preview
	endif
	
end
