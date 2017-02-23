;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_frost
; last revision : 29.Oct 2003
; written by    : Andre Lehmann
; Frost speckle filter
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

function frost, amp, smm, damp = damp
	if not keyword_set(looks) then looks=1

	siz     = size(amp)
	block_x = siz[1]
	block_y = siz[2]
	rand    = (smm-1)/2
   smm2    = smm^2
	out     = amp
	fbox    = mbox(smm,block_x)

	m_arr = dblarr(smm,smm)
	t_arr = dblarr(smm,smm)
	for i = 0, smm - 1 do begin
		for j = 0, smm - 1 do begin
			t_arr[i,j] = sqrt((rand - i)^2 + (rand - j)^2)
		endfor
	endfor

	for i = rand, block_x - rand - 1 do begin
		for j = rand, block_y - rand - 1 do begin
			pos   = i+j*block_x
			box   = amp[pos+fbox]
			mean  = mean(box)
			vary  = total((box-mean)^2)/smm2
			alpha = damp * (vary / mean^2)
			m_arr = alpha * exp(- alpha * t_arr)
			out[pos] = total(m_arr * box) / total(m_arr)
		endfor
	endfor
	err = where(finite(out) eq 0,anz)
   if anz ne 0 then out[err]=0.0
	return,(out > 0)
end

function mbox,n,anz
	box = intarr(n*n)
	for i=0,n-1 do box[n*i] = (findgen(n)-n/2)+anz*(i-n/2)
	return,box
end

pro speck_frost,CALLED = called, BOXSIZE = boxsize, DAMP=damp
	common rat, types, file, wid, config, tiling

	if not keyword_set(boxsize) then boxsize = 7                ; Default values
	if not keyword_set(damp) then damp = 1.0
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Frost Speckle Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=boxsize,/integer,   TITLE='Filter boxsize      : ',XSIZE=3)
		field2   = CW_FIELD(main,VALUE=damp,/float,TITLE='Damping factor      : ',XSIZE=3)
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
				infotext = ['FROST SPECKLE FILTER',$
				' ',$
				'RAT module written 01/2003 by Andre Lehmann',$
				' ',$
				'further information:']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=boxsize
		widget_control,field2,GET_VALUE=damp
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; Error handling

	if boxsize lt 3 then begin                                 ; Wrong box size ?
		error = DIALOG_MESSAGE("Boxsize has to be >= 3", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	if damp lt 0.001 then begin                              ; Damp to small ?
		error = DIALOG_MESSAGE("Damping factor has to be > 0", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
 
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; handling of complex and amplitude input data
	
	ampflag = 0
	if file.var eq 6 or file.var eq 9 then begin             ; Wrong variable type?
		error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
		if error eq "Cancel" then return else complex2abs,/called
		if wid.cancel eq 1 then return
		ampflag = 1
	endif
	if file.type eq 100 then ampflag = 1
	
; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	srat,outputfile,eee,header=head,info=info,type=type

; Initialise tiling & progess bar

	tiling_init,overlap=(boxsize+1)/2
	progress,Message='Frost Speckle Filter...',/cancel_button

;start block processing

	for i = 0, tiling.nr_blocks-1 do begin
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return

		tiling_read,ddd,i,block
; -------- THE FILTER ----------
		if ampflag eq 1 then block = block^2
		for j=0,file.vdim-1 do for k=0,file.zdim-1 do block[j,k,*,*] = frost(reform(block[j,k,*,*]),boxsize,damp=damp)
		if ampflag eq 1 then block = sqrt(block)
; -------- THE FILTER ----------
		tiling_write,eee,i,temporary(block)
		tiling_jumpback,ddd
	endfor
	free_lun,ddd,eee

; update everything
	rat_finalise,outputfile,finalfile,CALLED=called
	evolute,'Frost speckle filtering, Damp: '+strcompress(damp,/R)+' Boxsize: '+strcompress(boxsize,/R)

end
