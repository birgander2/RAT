;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: slant2ground
; written by    : Andreas Reigber
; last revision : 14.Mar.2003
; Generic slant-range to ground-range projection
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

pro slant2ground,CALLED=called, HEIGHT = height, RANGE = range, PSIN = psin, PSOUT = psout
	common rat, types, file, wid, config, tiling

	if not keyword_set(height) then height = 5000.0             ; Default values
	if not keyword_set(range)  then range  = 7000.0             ; Default values
	if not keyword_set(psin)   then psin   = 3.0             ; Default values
	if not keyword_set(psout)  then psout  = 3.0             ; Default values

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Slant Range Projection',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=height,/floating,TITLE='Sensor height          [m] : ',XSIZE=7)
		field2 = CW_FIELD(main,VALUE=range ,/floating,TITLE='Minimum range distance [m] : ',XSIZE=7)
		field3 = CW_FIELD(main,VALUE=psin  ,/floating,TITLE='Range pixel spacing    [m] : ',XSIZE=7)
		field4 = CW_FIELD(main,VALUE=psout ,/floating,TITLE='Output pixel spacing   [m] : ',XSIZE=7)
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
				infotext = ['Slant-range to ground range projection',$
				' ',$
				'RAT module written 2003 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=h0               ; read widget fields
		widget_control,field2,GET_VALUE=rd
		widget_control,field3,GET_VALUE=psin
		widget_control,field4,GET_VALUE=psout
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; calculate interpolation function
	
	range = rd + dindgen(file.xdim)*psin
	theta = acos(h0 / range)
	yin   = sqrt(range^2 - h0^2)
	aux   = where(finite(yin) eq 0,anz)
	if anz gt 0 then yin[aux] = 0.0
	
	y0    = yin[0]
	y1    = yin[file.xdim-1]
	anz_out = floor((y1 - y0) / psout)

	yout  = y0 + findgen(anz_out)*psout
	
	tin   = findgen(file.xdim)
	tout  = interpol(tin,yin,yout)

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   
	undo_prepare,outputfile,finalfile,CALLED=CALLED


; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	head[head[0]-1] = anz_out
	srat,outputfile,eee,header=head,info=info,type=type		
	
; Initialise tiling & progess bar

	tiling_init
	progress,Message='Slant-Range to Ground Range...',/cancel_button

;start block processing

	for i=0,tiling.nr_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return
		tiling_read,ddd,i,block
		oblock = make_array([file.vdim,file.zdim,anz_out,(*tiling.blocksizes)[i]],type=file.var)
		for k=0,file.vdim-1 do for l=0,file.vdim-1 do for j=0,(*tiling.blocksizes)[i]-1 do $
			oblock[k,l,*,j] = interpolate(reform(block[k,l,*,j]),tout,cubic=-0.5)
		tiling_write,eee,i,temporary(oblock)
	endfor
	free_lun,ddd,eee

; update everything

	rat_finalise,outputfile,finalfile,CALLED=called
	evolute,'Slant range to ground range projection'
end
