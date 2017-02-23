;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_mean
; last revision : 12.Feb.2003
; written by    : Bert Wolf
;                 Matthias Weller
;                 Andreas Reigber
; Boxcar (Mean) filter for all variable types                    
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



pro speck_mean,CALLED = called, BOXSIZEX = boxsizex, BOXSIZEY = boxsizey
	common rat, types, file, wid, config, tiling

	if not keyword_set(boxsizex) then boxsizex = 7                ; Default values
	if not keyword_set(boxsizey) then boxsizey = 7                ; Default values

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Boxcar Filter',/modal)
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
				infotext = ['BOXCAR FILTER',$
				' ',$
				'RAT module written 2003 by Bert Wolf,',$
				'Matthias Weller and Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) 
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
		if wid.cancel eq 1 then return
	endif

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; Initialise tiling & progess bar

	tiling_init,overlap=(boxsizey+1)/2
	progress,Message='Boxcar Speckle Filter...',/cancel_button
 
;start block processing

	for i=0,tiling.nr_blocks-1 do begin   
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return

		tiling_read,ddd,i,block
; -------- THE FILTER ----------
		block = smooth(block,[1,1,boxsizex,boxsizey],/edge_truncate)   
; -------- THE FILTER ----------
		tiling_write,eee,i,temporary(block)
		tiling_jumpback,ddd
	endfor
	free_lun,ddd,eee

; update everything

	rat_finalise,outputfile,finalfile,CALLED=called
	evolute,'Boxcar speckle filtering, Boxsize X : '+strcompress(boxsizex,/R)+' Boxsize Y : '+strcompress(boxsizey,/R)

end
