;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: text_varcoef
; last revision : 15. July 2004
; written by    : Andreas Reigber
; Calculates the variation coefficient                
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



pro text_varcoef,CALLED = called, BOXSIZEX = boxsizex, BOXSIZEY = boxsizey
	common rat, types, file, wid, config, tiling

	if not keyword_set(boxsizex) then boxsizex = 7l             ; Default values
	if not keyword_set(boxsizey) then boxsizey = 7l             ; Default values
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Variation coefficient',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=boxsizex,/integer,TITLE='Filter boxsize X   : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=boxsizey,/integer,TITLE='Filter boxsize Y   : ',XSIZE=3)
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
				infotext = ['VARIATION COEFFICIENT',$
				' ',$
				'RAT module written 07/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=boxsizex                ; read widget fields
		widget_control,field2,GET_VALUE=boxsizey
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; Error Handling

	if boxsizex le 0 or boxsizey le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; Complex input data ????

	if file.var eq 6 or file.var eq 9 then begin         
		error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
		if error eq "Cancel" then return
		complex2abs,/called
	endif

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; Initialise tiling & progess bar

	tiling_init,overlap=(boxsizey+1)/2
	progress,Message='Variation coefficient...',/cancel_button

;start block processing

	smm_box = [1,1,boxsizex,boxsizey]

	for i=0,tiling.nr_blocks-1 do begin   
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return
		tiling_read,ddd,i,block
; -------- THE FILTER ----------
		dummy = smooth(block,smm_box,/edge_truncate)^2
		block = (smooth(block^2,smm_box,/edge_truncate) - dummy  ) / dummy
		index = where(finite(block) eq 0,nr)  
		if nr gt 0 then block[index] = 0.0
; -------- THE FILTER ----------
		tiling_write,eee,i,temporary(block)
		tiling_jumpback,ddd
	endfor
	free_lun,ddd,eee

; update everything

	rat_finalise,outputfile,finalfile,CALLED=called
	evolute,'Variation coefficient. Boxsize in X: '+strcompress(boxsizex,/R)+' Boxsize in Y: '+strcompress(boxsizey,/R)

end
