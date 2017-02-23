;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: pnoise_qef
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
pro fluc_sub,pha,upha,a1,alpha1,a2
   cum=exp(complex(0,pha-upha))
   if a1 gt 1 then cum=smooth(cum,a1,/edge_truncate)
   if alpha1 gt 1 then $
        cum=atan(median(imaginary(cum),alpha1),median(float(cum),alpha1)) $
   else    $ 
           cum = atan(cum)
   ergo_m,cum,ucum
   upha=upha+ucum
   if a2 gt 1 then upha=smooth(upha,a2,/edge_truncate)
end
pro ergo_m,_pha,rho
   s = size(_pha)
   xsize=s(1)
   ysize=s(2)
   pha=fltarr(xsize+1,ysize+1)
   pha(0,0)=_pha
   pha(xsize,*)=pha(xsize-1,*)
   pha(*,ysize)=pha(*,ysize-1)
   xsize=xsize+1
   ysize=ysize+1
     
   rho = atan(exp(complex(0,shift(pha,-1,0)-pha)),/phase) 
   rho[xsize-1,*]=atan(exp(complex(0,pha[xsize-2,*]-pha[xsize-1,*])),/phase) 
   x2 = atan(exp(complex(0,pha-shift(pha,1,0))  ),/phase)
   x2[0,*]=atan(exp(complex(0,pha[0,*]-pha[1,*])),/phase) 
   rho = rho - x2
   x2 = atan(exp(complex(0,shift(pha,0,-1)-pha)  ),/phase)
   x2[*,ysize-1]=atan(exp(complex(0,pha[*,ysize-2]-pha[*,ysize-1])),/phase) 
   rho = rho + x2
   x2 = atan(exp(complex(0,pha-shift(pha,0,1)) ),/phase) 
   x2[*,0]=atan(exp(complex(0,pha[*,0]-pha[*,1])),/phase) 
   rho = rho - x2
   for i=0,xsize-1 do rho(i,*) = float((fft([(rho[i,*])[*],(reverse((rho[i,*])[*]))[1:ysize-2]],-1))[0:ysize-1])
   for i=0,ysize-1 do rho(*,i) = float((fft([(rho[*,i])[*],(reverse((rho[*,i])[*]))[1:xsize-2]],-1))[0:xsize-1])
   index = findgen(xsize)*!pi/(xsize-1)
   x2 = cos(index) # (fltarr(ysize) + 1)
   index = findgen(ysize)*!pi/(ysize-1)
   x2 = x2 + (fltarr(xsize) + 1) # cos(index) 
   x2(0,0) = x2(0,0) + 0.00001
   rho = rho / (2*(x2 - 2))   
   for i=0,xsize-1 do rho(i,*) = float((fft([(rho[i,*])[*],(reverse((rho[i,*])[*]))(1:ysize-2)],1))[0:ysize-1])
   for i=0,ysize-1 do rho(*,i) = float((fft([(rho[*,i])[*],(reverse((rho[*,i])[*]))(1:xsize-2)],1))[0:xsize-1])
   rho = rho[0:xsize-2,0:ysize-2]
end

pro pnoise_qef,CALLED = called, SMM=smm
	common rat, types, file, wid, config

	if file.type ne 301 and file.type ne 302 then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='GLSME phase noise filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=5,/integer,TITLE='Filter stength : ',XSIZE=3)
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
				infotext = ['GLSME phase noise filter',$
				' ',$
				'RAT module written 2004 by N. Nadow']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=smm                ; read widget fields
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(smm) then smmx = 5              ; Default values
	endelse

; Error Handling

	if smm le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Boxsize has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED


; pop up progress window

	progress,Message='reading data...',/cancel_button

; read complete image

	rrat,file.name,arr,info=info,type=type	
	if file.type eq 301 then arr = atan(arr,/phase)
	
; Filtering

   res = 0
	progress,Message='GLSME estimation...',/cancel_button

	fluc_sub,arr,res,21,3,11
	progress,percent=25,/check_cancel
		if wid.cancel eq 1 then return

	fluc_sub,arr,res,9,3,11
	progress,percent=50,/check_cancel
		if wid.cancel eq 1 then return

	fluc_sub,arr,res,3,3,11
	progress,percent=75,/check_cancel
		if wid.cancel eq 1 then return

 	fluc_sub,arr,res,1,3,1
	progress,percent=100,/check_cancel
		if wid.cancel eq 1 then return

	progress,Message='filtering...',/cancel_button
   res = smooth(res,11,/edge_truncate)
 	progress,percent=25,/check_cancel
		if wid.cancel eq 1 then return

	dummy = exp(complex(0,arr-res))
	progress,percent=50,/check_cancel
		if wid.cancel eq 1 then return

   dummy = complex(median(float(dummy),smm),median(imaginary(dummy),smm))
	progress,percent=75,/check_cancel
		if wid.cancel eq 1 then return

   res = atan(dummy*exp(complex(0,res)),/phase)
	progress,percent=100,/check_cancel
		if wid.cancel eq 1 then return

; save complete image

	srat,outputfile,res,info=info,type = 302l

; update file information
	
	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 302l
	file.var  = 4l

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
