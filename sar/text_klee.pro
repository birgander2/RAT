;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: text_klee
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
function klee,arr,boxsize,LOOKS=looks,THRESHOLD=threshold,METHOD=method,AMPLITUDE=amplitude
	if not keyword_set(looks) then looks=1
	if not keyword_set(threshold) then threshold=0.5
	if not keyword_set(method) then method=0
	
	siz    = size(arr)	
	anzx = siz[3]
	anzy = siz[4]
	
	amp   = reform(arr)
	out   = fltarr(anzx,anzy)
	
	sig2  = 1.0/looks
	sfak  = 1.0+sig2
	
	case method of
	
		0: begin
			m2arr  = smooth(amp^2,boxsize)
			marr   = smooth(amp,boxsize)
			vary   = m2arr - marr^2 
			varx   = (vary - marr^2*sig2)/sfak > 0
			k      = varx / vary
			if keyword_set(amplitude) then k *= (((amp-marr)/(amp+marr))>0)
			out    = k
		end
		
		1: begin
			sigma = float(boxsize)/3.0
			len = 6*sigma
			x   = shift((findgen(len)-len/2),len/2)
			box = fltarr(len,len)
			for i=0,len-1 do box[*,i]  = 1.0/sigma/sqrt(2*!pi)*exp(-0.5*x^2/sigma^2)
			for i=0,len-1 do box[i,*] *= 1.0/sigma/sqrt(2*!pi)*exp(-0.5*x^2/sigma^2)
			box = (shift(box,len/2,len/2))[1:*,1:*]
			m2arr  = convol(amp^2,box,/center,/normalize)
			marr   = convol(amp,box,/center,/normalize)
			vary   = m2arr - marr^2 
			varx   = (vary - marr^2*sig2)/sfak > 0
			k      = varx / vary
			if keyword_set(amplitude) then k *= (((amp-marr)/(amp+marr))>0)
			out    = k
		end
		
		2: begin
			delta = (boxsize-1)/2
			dsh   = (delta+1)/2
			
			cbox = fltarr(9,boxsize,boxsize)
			chbox = fltarr(boxsize,boxsize)
			chbox[*,0:delta] = 1./boxsize/delta
			cvbox = fltarr(boxsize,boxsize)
			for i=0,boxsize-1 do cvbox[0:i,i] = 1.
	
			cbox[0,*,*] = rotate(chbox,1)
			cbox[1,*,*] = rotate(cvbox,3)
			cbox[2,*,*] = rotate(chbox,2)
			cbox[3,*,*] = rotate(cvbox,0)
			cbox[4,*,*] = rotate(chbox,3)
			cbox[5,*,*] = rotate(cvbox,2)
			cbox[6,*,*] = rotate(chbox,0)
			cbox[7,*,*] = rotate(cvbox,1)
			cbox[8,*,*] += 1.
	
			for i=0,8 do cbox[i,*,*] /= total(cbox[i,*,*])

			samp = smooth(amp,delta)
			grad = fltarr(4,anzx,anzy)
			grad[0,*,*] = (shift(samp,+dsh,0)+shift(samp,+dsh,+dsh)+shift(samp,+dsh,-dsh)-shift(samp,-dsh,0)-shift(samp,-dsh,+dsh)-shift(samp,-dsh,-dsh)) ; vertical
			grad[1,*,*] = (shift(samp,+dsh,+dsh)+shift(samp,0,+dsh)+shift(samp,+dsh,0)-shift(samp,-dsh,-dsh)-shift(samp,0,-dsh)-shift(samp,-dsh,0) )
			grad[2,*,*] = (shift(samp,0,+dsh)+shift(samp,+dsh,+dsh)+shift(samp,-dsh,+dsh)-shift(samp,0,-dsh)-shift(samp,-dsh,-dsh)-shift(samp,+dsh,-dsh)) ; horizontal
			grad[3,*,*] = -(shift(samp,+dsh,-dsh)+shift(samp,+dsh,0)+shift(samp,0,-dsh)-shift(samp,-dsh,+dsh)-shift(samp,0,+dsh)-shift(samp,-dsh,0) )
			mag = max(grad,dir,dim=1,/absolute)
			dir mod= 4
			ind = where(mag le 0.0,nind)
			if nind gt 0 then dir[ind] += 4

			for i=0,8 do begin  ; filtering
				box = reform(cbox[i,*,*])
				grad = convol(amp^2,box,/center,/edge_truncate)
				mamp = convol(amp,box,/center,/edge_truncate)
				aux  = where(dir eq i,nr)
				if nr gt 0 then begin
					vary = grad[aux] - mamp[aux]^2
					varx = (vary - (mamp[aux]^2)*sig2)/sfak > 0
					k    = varx / vary
					if keyword_set(amplitude) then k *= (((amp[aux]-mamp[aux])/(amp[aux]+mamp[aux]))>0)
					out[aux]  = k
				endif
			endfor

		end
	endcase
	
	err = where(finite(out) eq 0,nr)
   if nr gt 0 then out[err] = 0.0
	return,out
end

pro text_klee,CALLED = called, BOXSIZE = boxsize, LOOKS = looks, METHOD=method, AMPLITUDE=amplitude
	common rat, types, file, wid, config, tiling

	if not keyword_set(boxsize) then boxsize = 7                ; Default values
	if not keyword_set(looks) then looks = 1.0
	if not keyword_set(method) then method = 0
	if not keyword_set(amplitude) then amplitude = 1

	; Check inputfile

	if file.type ne 100 and file.type ne 101 and file.type ne 103 then begin
		error = DIALOG_MESSAGE("Intensity or amplitude SAR image required",$
		DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Texture inhomogenity',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=boxsize,/integer,  TITLE='Filter boxsize        : ',XSIZE=3)
		field2   = CW_FIELD(main,VALUE='1.0',/float,TITLE='Effective No of Looks : ',XSIZE=3)
		field3   = cw_bgroup(main,[' No ',' Yes '],label_left=' Use amplitude damping ',set_value=amplitude,/exclusive,/row)
		field4   = CW_BGROUP(main,["Boxcar","Gauss","refined Lee"],/frame,set_value=method,row=3,label_top='Mask selection:',/exclusive)
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
				infotext = ['TEXTURE INHOMOGENITY',$
				' ',$
				'RAT module written 06/2006 by Andreas Reigber',$
				' ',$
				'this function estimates the k-factor of the Lee speckle filter',$
				'and uses it as a descriptor for local image inhomogenity.',$
				'',$
				'Amplitude damping: See P. Leducq, PhD, University of Rennes, 2006']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=boxsize
		widget_control,field2,GET_VALUE=looks
		widget_control,field3,GET_VALUE=amplitude
		widget_control,field4,GET_VALUE=method
		
		widget_control,main,/destroy
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; Error Handling

	if boxsize le 0 and not keyword_set(called) then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   
	undo_prepare,outputfile,finalfile,CALLED=CALLED

; handling of single channel
	
	ampflag = 0
	if file.type eq 100 then ampflag = 1
	if file.type eq 101 then begin
		error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
		if error eq "Cancel" then return else complex2abs,/called
		if wid.cancel eq 1 then return
		ampflag = 1
	endif
	
; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=51l		
		
; Initialise tiling & progess bar

	tiling_init,overlap=(boxsize+1)/2
	progress,Message='Texture inhomogenity...',/cancel_button

;start block processing

	for i=0,tiling.nr_blocks-1 do begin   
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return
		tiling_read,ddd,i,block

;		if ampflag eq 1 then block = block^2
		for j=0,file.vdim-1 do for k=0,file.zdim-1 do block[j,k,*,*] = klee(block[j,k,*,*],boxsize,LOOKS=looks,METHOD=method,AMPLITUDE=amplitude)

		tiling_write,eee,i,temporary(block)
		tiling_jumpback,ddd
	endfor
	free_lun,ddd,eee

; update everything

	rat_finalise,outputfile,finalfile,CALLED=called
	evolute,'Texture inhomogenity: boxsize'+strcompress(boxsize,/R)+' looks: '+strcompress(looks,/R)+ ' Method: '+strcompress(method,/R)

end
