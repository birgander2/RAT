;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_polidan
; written by    : Andreas Reigber
; last revision : 24.Nov.2005
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

function polidan,arr,NRMAX=nrmax,LOOKS=looks,SINGLE=single,NRMIN=nrmin
   common rat, types, file, wid, config
   
   if not keyword_set(nrmax) then nrmax = 50
   if not keyword_set(looks) then looks = 1.0
   
   sig2  = 1.0/looks
   sfak  = 1.0+sig2
   
   if keyword_set(single) then begin
      amp  = total(total(arr,1),1) ; Calculate span
      span = amp
      anzz = 1
   endif else begin             ; covariance matrix data
      amp = make_array(file.zdim,file.xdim,file.ydim,/float)
      for i=0,file.zdim-1 do amp[i,*,*] = arr[i,i,*,*]
      span = total(amp,1)
      anzz = file.zdim
   endelse

   siz  = size(amp)
   anzx = siz[siz[0]-1]
   anzy = siz[siz[0]]
   nmat = file.vdim*file.zdim
   pol  = lindgen(nmat)
   if keyword_set(single) then out = fltarr(file.vdim,file.zdim,file.xdim,file.ydim) else out = complexarr(file.vdim,file.zdim,file.xdim,file.ydim)
   
   if file.zdim gt 1 and not keyword_set(single) then begin ; multilayer span
      box3 = intarr(file.zdim,9)
      for j=0,file.zdim-1 do for i=0,2 do box3[j,3*i:3*i+2]=(findgen(3)-1)*file.zdim+anzx*file.zdim*(i-1)+j
   endif else begin             ; single layer
      box3 = intarr(1,9)
      for i=0,2 do box3[3*i] = (findgen(3)-1)+anzx*(i-1)
   endelse
   box = intarr(9)
   for i=0,2 do box[3*i] = (findgen(3)-1)+anzx*(i-1)
   box  = [box[0:3],box[5:*]]

   for i=1,anzx-2 do begin
      progress,percent=(i+1)*100.0/(anzx-2),/check_cancel
      if wid.cancel eq 1 then return,-1
      
      for j=1,anzy-2 do begin
         limit = file.zdim/sqrt(looks)*2/3
         pos   = [j*anzx+i]
         an    = [j*anzx+i]
         nrold = 1
         nrnew = 1
grow_further:
         seed  = median(amp[file.zdim*pos[0]+box3],dim=2)

         repeat begin 
            nrold = nrnew
            rand  = 0
            for k=0,nrold-1 do rand = [rand,an[k]+box]
            rand = rand[1:*]
            rand = rand[uniq(rand,sort(rand))]
            check = 0
            mran = file.zdim*rand
            for k=0,anzz-1 do check += abs(amp[mran+k]-seed[k])/seed[k]
            index = where(check lt limit,anz1,complement=bgindex,ncomplement=anz2)

            if anz1 gt 0 then begin
               an = [an,rand[index]]
               an = an[uniq(an,sort(an))]
               nrnew = n_elements(an)
            endif
         endrep until (nrnew ge nrmax) or (nrold eq nrnew)
         if anz2 gt 0 then anbg = rand[bgindex]

         if nrnew lt nrmax && anz2 ge 1 then begin
            anbg  = rand[bgindex]
            anbg  = anbg[uniq(anbg,sort(anbg))]
            for k=0,anzz-1 do seed[k] = mean(amp[file.zdim*an+k])
            check = 0
            mran = file.zdim*anbg
            for k=0,anzz-1 do check += abs(amp[mran+k]-seed[k])/seed[k]
            index = where(check lt limit*3,anz)
            if anz gt 0 then an = [an,anbg[index]]
         endif
         nele = n_elements(an)
         if nele lt nrmin then begin
            limit += 0.2
            nrnew = nele
            if limit lt 10*file.zdim then goto, grow_further
         endif

         ibox = span(an)
         imean= mean(ibox)
         vary = total((ibox-imean)^2)/(nele-1) 
         varx = (vary - imean^2*sig2) / sfak > 0
         k    = varx / vary

         win   = (lonarr(nmat) + 1) ## (an*nmat) + pol ## (lonarr(nele) + 1)
         covec = arr[win] ; covec contains all complex values of the window			
         meanc = total(covec,1)  / nele ; meanc contains the averaged complex values for each polarisation
         cov   = arr[pos[0]*nmat+pol]
         out[*,*,i,j] = meanc + (cov - meanc) * k
      endfor
   endfor
   return,out
end

pro speck_polidan,CALLED = called, GUI=GUI, NMAX=nmax,NMIN=nmin,LOOKS=nlook
	common rat, types, file, wid, config

;  	if not ((file.type eq 100 or file.type eq 101 or file.type eq 103) or (file.type ge 200 and file.type lt 210) or (file.type ge 220 and file.type le 230) or $
;  	    (file.type ge 500 and file.type le 510) or (file.type ge 510 and file.type le 520) $
;             || (file.type ge 800 && file.type le 813)) then begin
;  		error_button = DIALOG_MESSAGE(['Wrong data type'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
;  		return
;  	endif
;  	
	if ~keyword_set(called) || keyword_set(GUI)  then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=6,TITLE='IDAN-LLMMSE speckle filter ',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=50,/integer,  TITLE='Size of adaptive neighbourhood : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=25,/integer,  TITLE='Minimal size of  neighbourhood : ',XSIZE=3)
		field3 = CW_FIELD(main,VALUE=1.0,/floating,TITLE='No. of looks                   : ',XSIZE=3)
                note1 = WIDGET_LABEL(main,VALUE='NOTE 1: This filter is powerful but rather slow!')
                note2 = WIDGET_LABEL(main,VALUE='NOTE 2: It can be also quite memory intensive.')
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
				infotext = ['IDAN-LLMMSE SPECKLE FILTER V1.0',$
				' ',$
				'RAT module written 12/2005 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=nmax               ; read widget fields
		widget_control,field2,GET_VALUE=nmin
		widget_control,field3,GET_VALUE=nlook
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(nmax) then nmax = 50              ; Default values
		if n_elements(nmin) eq 0 then nmin = 25
		if not keyword_set(nlook) then nlook = 1.0
	endelse

; Error Handling

	if nmax le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("No. of pixels has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
    
;	nmin = 0 < nmin > nmax

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function

	undo_prepare,outputfile,finalfile,CALLED=CALLED

; transform polarimetric vector to matrix

        if (file.type ge 200 and file.type le 210) or (file.type ge 500 and file.type lt 510) then begin
           if not keyword_set(called) then dummy = DIALOG_MESSAGE(["Data are in vector form. They have","to be converted to matrix form first."], DIALOG_PARENT = wid.base, /information)
           if file.type ge 500 && file.type le 503 then polin_k2m,/called,/GUI $
           else k_to_m,/called
           if wid.cancel eq 1 then return
	endif
	
; handling of single channel
	
	if file.type eq 100 or file.type eq 101 or file.type eq 103 then begin
		ampflag = 0
		if file.type eq 100 then ampflag = 1
		if file.type eq 101 then begin
			error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
			if error eq "Cancel" then return else complex2abs,/called
			if wid.cancel eq 1 then return
			ampflag = 1
		endif
	endif

; read file

	rrat,file.name,arr,info=info,type=type	

; pop up progress window

	progress,Message='IDAN-LLMMSE speckle filter...',/cancel_button

;start block processing

; -------- THE FILTER ----------
	if file.type eq 100 or file.type eq 101 or file.type eq 103 then begin
		arr = reform(arr,file.vdim,file.zdim,file.xdim,file.ydim)
		if ampflag eq 1 then arr = arr^2
		arr = polidan(arr,NRMAX=nmax,LOOKS=nlook,/single,NRMIN=nmin)
		if ampflag eq 1 then arr = sqrt(arr)
		arr = reform(arr)
	endif else begin
		arr = polidan(arr,NRMAX=nmax,LOOKS=nlook,NRMIN=nmin)
	endelse
; -------- THE FILTER ----------
; save file

	srat,outputfile,arr,info=info,type=type

; update file information
	
	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile

	evolute,'Speckle filtering (IDAN-Region Growing): nmax: '+strcompress(nmax,/r)+' nmin: '+strcompress(nmin,/r)+' looks: '+strcompress(nlook)

; generate preview

	if ~keyword_set(called) then begin
		generate_preview
		update_info_box
	endif else progress,/destroy
end
