;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: unwrap_ls
;
; written by    : Andreas Reigber
; last revision : 01.July.2003
;------------------------------------------------------------------------
; Least-squares phase unwrapping
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
; The Initial Developer of the Original Code is Andreas Reigber.
; All Rights Reserved.
;------------------------------------------------------------------------

function lsphu,pha
   s = size(pha)
   xsize=s[1]
   ysize=s[2]    
   uw = atan(exp(complex(0,shift(pha,-1,0)-pha)),/phase) 
   uw[xsize-1,*]=atan(exp(complex(0,pha[xsize-2,*]-pha[xsize-1,*])),/phase) 
   x2 = atan(exp(complex(0,pha-shift(pha,1,0))),/phase)
   x2[0,*]=atan(exp(complex(0,pha[0,*]-pha[1,*])),/phase) 
   uw = uw - x2   
   x2 = atan(exp(complex(0,shift(pha,0,-1)-pha)),/phase)
   x2[*,ysize-1]=atan(exp(complex(0,pha[*,ysize-2]-pha[*,ysize-1])),/phase) 
   uw = uw + x2
   x2 = atan(exp(complex(0,pha-shift(pha,0,1))),/phase) 
   x2[*,0]=atan(exp(complex(0,pha[*,0]-pha[*,1])),/phase) 
   uw = uw - x2     
   for i=0,xsize-1 do uw[i,*] = float((fft([(uw[i,*])[*],(reverse((uw[i,*])[*]))[1:ysize-2]],-1))[0:ysize-1])
   for i=0,ysize-1 do uw[*,i] = float((fft([(uw[*,i])[*],(reverse((uw[*,i])[*]))[1:xsize-2]],-1))[0:xsize-1])
   index = findgen(xsize)*!pi/(xsize-1)
   x2 = cos(index) # (fltarr(ysize) + 1)
   index = findgen(ysize)*!pi/(ysize-1)
   x2 = x2 + (fltarr(xsize) + 1) # cos(index) 
   x2(0,0) = x2(0,0) + 0.00001
   uw = uw / (2*(x2 - 2))   
   for i=0,xsize-1 do uw[i,*] = float((fft([(uw[i,*])[*],(reverse((uw[i,*])[*]))[1:ysize-2]],1))[0:ysize-1])
   for i=0,ysize-1 do uw[*,i] = float((fft([(uw[*,i])[*],(reverse((uw[*,i])[*]))[1:xsize-2]],1))[0:xsize-1])
	return,uw
end

pro unwrap_ls,CALLED = called, ITER=iter,SMM1 = smm1,SMM2 = smm2
	common rat, types, file, wid, config

	if (file.type ne 300) and (file.type ne 301) and (file.type ne 302) then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Least-squares phase unwrapping',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=3,/integer,TITLE='Iterations                  : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=3,/integer,TITLE='Smooth between iterations   : ',XSIZE=3)
		field3 = CW_FIELD(main,VALUE=1,/integer,TITLE='Smooth on output            : ',XSIZE=3)
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
				infotext = ['LEAST-SQUARES PHASE UNWRAPPING',$
				' ',$
				'RAT module written 06/2004 by Andreas Reigber',$
				' ',$
				'more information:',$
				'M.D. Pritt and J.S. Shipman: Least-Squares Two Dimensional Phase Unwrapping',$
				'using FFTs, IEEE Trans. Geosc. Rem. Sens. 32(3), pp.706-708, 1994']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=iter               ; read widget fields
		widget_control,field2,GET_VALUE=smm1              ; read widget fields
		widget_control,field3,GET_VALUE=smm2              ; read widget fields
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(iter) then iter = 3              ; Default values
		if not keyword_set(smm1) then smm1 = 3           ; Default values
		if not keyword_set(smm2) then smm2 = 1           ; Default values
	endelse

; Error Handling

	if iter le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Nr. of iterations has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if smm1 le 0 or smm2 le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Smoothing has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; pop up progress window

	progress,Message='LS phase unwrapping...',/cancel_button

; read complete image

	rrat,file.name,arr,info=info,type=type	

; unwrap image

; ADD iterations
; ADD transforms for complex, pair, etc.

	if file.type eq 300 then arr = reform(atan(arr[0,*,*]*conj(arr[1,*,*]),/phase))
	if file.type eq 301 then arr = atan(arr,/phase)
	
	pha = arr
	res = fltarr(file.xdim,file.ydim)
	for i=0,iter-1 do begin
		progress,percent=(i+1)*100.0/(iter+1),/check_cancel
		if wid.cancel eq 1 then return

		if i lt iter-1 then $
			pha = atan(smooth(exp(complex(0,arr-res)),smm1,/edge_truncate),/phase) $
		else $ 
			pha = atan(exp(complex(0,arr-res)),/phase)
		res += lsphu(pha)
	endfor
	res += min(res)
	if smm2 gt 1 then res = smooth(res,smm2,/edge_truncate)
; save complete image

	srat,outputfile,res,info=info,type=303l
	
; update file information
	
	file_move,outputfile,finalfile,/overwrite

	file.name = finalfile
	file.dim  = 2l
	file.zdim = 1l
	file.var  = 4l
	file.type = 303l

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
