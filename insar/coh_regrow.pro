;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: coh_regrow
; written by    : Andreas Reigber
; last revision : 20.Dec.2004
; Interferometric coherence calculation using region growing
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

function coh_grow,arr1,arr2,NRMAX=nrmax,NRMIN=nrmin,LOOKS=looks,AMPLITUDE=amplitude
	common rat, types, file, wid, config
	
	if not keyword_set(nrmax) then nrmax = 50
	if not keyword_set(looks) then looks = 1.0
	
	amp = abs(arr1)+abs(arr2)
	anzx = (size(amp))[1]
	anzy = (size(amp))[2]
	
	coh = fltarr(anzx,anzy)
	if arg_present(amplitude) then amplitude = fltarr(anzx,anzy)

	box3 = intarr(9)
	for i=0,2 do box3[3*i] = (findgen(3)-1)+anzx*(i-1)
	box  = [box3[0:3],box3[5:*]]
	limit = 1.0/sqrt(looks)

	for i=1,anzx-2 do begin
		progress,percent=(i+1)*100.0/(anzx-2),/check_cancel
		if wid.cancel eq 1 then return,-1
		
		for j=1,anzy-2 do begin
			pos   = [j*anzx+i]
			an    = [j*anzx+i]
			anbg  = 0
			rand  = 0
			nrold = 1
			nrnew = 1
			seed  = median(amp[pos[0]+box3])
			repeat begin 
				nrold = nrnew
				for k=0,nrold-1 do rand = [rand,an[k]+box]
				rand = rand[1:*]
				rand = rand[uniq(rand,sort(rand))]
				check = abs(amp[rand]-seed)/seed
				index = where(check lt limit,anz1,complement=bgindex,ncomplement=anz2)
				if anz1 gt 0 then begin
					an = [an,rand[index]]
					an = an[uniq(an,sort(an))]
					nrnew = n_elements(an)
				endif
				if anz2 gt 0 then anbg = [anbg,rand[bgindex]]
			endrep until (nrnew ge nrmax) or (nrold eq nrnew)

			anbg  = anbg[uniq(anbg,sort(anbg))]
			if n_elements(anbg) gt 1 then begin
  				seed  = mean(amp[an])
				anbg  = anbg[1:*]
				check = abs(amp[anbg]-seed)/seed
				index = where(check lt 2*limit,anz)
				if anz gt 0 then an = [an,anbg[index]]
			endif
			
			s1 = arr1[an]
			s2 = arr2[an]
			coh[pos] = abs(mean(s1*conj(s2)))/sqrt(mean(abs(s1)^2)*mean(abs(s2)^2))
			if arg_present(amplitude) then amplitude[pos] = mean(amp[an])
		endfor
	endfor
	return,coh
end

pro coh_regrow,CALLED = called, NMAX = nmax, NLOOK=nlook
	common rat, types, file, wid, config

	if (file.type ne 300) then begin                   
		error = DIALOG_MESSAGE(["Wrong data type!","only for InSAR pairs"], DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='region growing coherence estimation',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=50,/integer,TITLE='Maximum number of pixels : ',XSIZE=3)
		field3 = CW_FIELD(main,VALUE=1.0,/floating,TITLE='No. of looks             : ',XSIZE=3)
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
				infotext = ['REGION GROWING COHERENCE ESTIMATION V1.0',$
				' ',$
				'RAT module written 12/2004 by Andreas Reigber',$
				' ',$
				'further information: ', $
				'G. Vasile, E. Trouve, M. Ciuc, P. Bolon and V. Buzuloiu:', $
				'Improving Coherence Estimation for High-resolution ', $
				'Polarimetric SAR Interferometry, Proc. of IGARSS, 2004']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=nmax                ; read widget fields
		widget_control,field3,GET_VALUE=nlook
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(nmax) then nmax = 50              ; Default values
		if not keyword_set(nlook) then nlook = 1.0
	endelse

; Error Handling

	if nmax le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("No. of pixels has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read file

	rrat,file.name,arr,info=info,type=type	

; pop up progress window

	progress,Message='Coherence estimation (region growing)...',/cancel_button

;start block processing
	
	arr1 = reform(arr[0,*,*])
	arr2 = reform(arr[1,*,*])
	cpha = atan(smooth(exp(complex(0,atan(arr1*conj(arr2),/phase))),2*sqrt(nmax)),/phase)
	arr2 *= exp(complex(0,cpha))
	coh = coh_grow(arr1,arr2,NRMAX=nmax,NRMIN=nmin,LOOKS=nlook)

	if n_elements(coh) eq 1 then return
	
; save file

	srat,outputfile,coh,info=info,type=310l	

; update file information
	
	file_move,outputfile,finalfile,/overwrite

	file.name = finalfile
	file.dim  = 2l
	file.zdim = 1l
	file.var  = 4l
	file.type = 310l

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
