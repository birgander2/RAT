;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: spectral_watch
; last revision : 15.Jan.2004
; written by    : Stéphane Guillaso
; check spectrum SAR data               
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

PRO spectral_watch, CALLED=called, spectrum_X=fl_rg, spectrum_Y=fl_az, data=arr
	COMMON rat, types, file, wid, config
	
; --> Is it a complex SAR image ?
	if file.type ne 101 then begin
		error = dialog_message("SAR complex image is required",dialog_parent=wid.base,title='Error',/error)
		return
	endif
	
; --> Image size extraction
	xdim = file.xdim
	ydim = file.ydim
	
;------------------------------------------------------------------------
; - Transform the arrow mouse pointer into a hourglass
;------------------------------------------------------------------------
	widget_control, /hourglass

; --> Image reading
	rrat,file.name,arr

	progress,Message='Spectrum Extraction...',/cancel_button
; --> Azimuth spectrum calculation
	fl_az = FLTARR(ydim)
	FOR col=0,xdim-1 DO BEGIN
		;print,(col+1)*100.0/(xdim+ydim);,/check_cancel
		if col mod 100 eq 0 then begin
			progress,percent=(col+1)*100.0/(xdim+ydim),/check_cancel
			if wid.cancel eq 1 then return
		endif

		fl_az = fl_az + ABS(FFT(arr[col,*],-1))
	ENDFOR
	fl_az = fl_az / xdim

; --> Range spectrum calculation
	fl_rg = FLTARR(xdim)
	FOR lig=0,ydim-1 DO BEGIN
		;print,(lig+xdim+1)*100.0/(xdim+ydim);,/check_cancel
		if lig mod 100 eq 0 then begin
			progress,percent=(lig+xdim+1)*100.0/(xdim+ydim),/check_cancel
			if wid.cancel eq 1 then return
		endif
		
		fl_rg = fl_rg + ABS(FFT(arr[*,lig],-1))
	ENDFOR
	fl_rg = fl_rg / ydim
	progress,/destroy
;------------------------------------------------------------------------
; If called thus exit 
;------------------------------------------------------------------------
	if keyword_set(called) then return

; --> Generate GUI
	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Spectrum analysis',/floating,/tlb_kill_request_events,/tlb_frame_attr)
	sub_main = WIDGET_BASE(main,COLUMN=2,/frame)
		sub_m1 = WIDGET_BASE(sub_main,ROW=2)
		sub_m2 = WIDGET_BASE(sub_main,ROW=2)
	
	lab1 = WIDGET_LABEL(sub_m1,value='X spectrum analysis : ',/align_left,SCR_XSIZE=300)
	drw1 = WIDGET_DRAW(sub_m1,XSIZE=300,YSIZE=300)

	lab2 = WIDGET_LABEL(sub_m2,value='Y spectrum analysis : ',/align_left,SCR_XSIZE=300)
	drw2 = WIDGET_DRAW(sub_m2,XSIZE=300,YSIZE=300)
	
	buttons  = WIDGET_BASE(main,column=2)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

	WIDGET_CONTROL,main,/realize
	
	widget_control, main, tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, XOFFSET=pos[0], yoffset=pos[1]

	; Display the result
	WIDGET_CONTROL,drw2,get_value=index
	wset,index
	PLOT,fl_az
	
	WIDGET_CONTROL,drw1,get_value=index
	wset,index
	PLOT,fl_rg
	
	; loop event for OK and Info button
	repeat begin
		event = widget_event(main)

		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['SAR IMAGE SPECTRUM ANALYSIS',$
			' ',$
			'RAT module written 01/2004 by Stephane Guillaso'$
			]
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
	endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,main,/destroy                        ; remove main widget

; switch back to main draw widget

	WIDGET_CONTROL,wid.draw,get_value=index
	wset,index

end


