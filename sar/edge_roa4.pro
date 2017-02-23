;------------------------------------------------------------------------
; RAT - Radar Tools
;-----------------------------------------------------------------
; ROA4
;
; 07/2007 by Andreas Reigber
;------------------------------------------------------------------
; Maximum Gradient edge detector for SAR images
; using spliting of window into 4 possible halfs
;------------------------------------------------------------------
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

function roa4,amp,delta
	siz = size(amp)	
	nrx = siz[1]
	nry = siz[2]
	samp = smooth(amp,delta)
	grad = fltarr(4,nrx,nry)
	dsh   = (delta+1)/2
	grad[0,*,*] = ((shift(samp,+dsh,0)+shift(samp,+dsh,+dsh)+shift(samp,+dsh,-dsh)) / (shift(samp,-dsh,0)+shift(samp,-dsh,+dsh)+shift(samp,-dsh,-dsh))) ; vertical
	grad[1,*,*] = ((shift(samp,+dsh,+dsh)+shift(samp,0,+dsh)+shift(samp,+dsh,0)) / (shift(samp,-dsh,-dsh)+shift(samp,0,-dsh)+shift(samp,-dsh,0)))		 ; diagonal 1
	grad[2,*,*] = ((shift(samp,0,+dsh)+shift(samp,+dsh,+dsh)+shift(samp,-dsh,+dsh)) / (shift(samp,0,-dsh)+shift(samp,-dsh,-dsh)+shift(samp,+dsh,-dsh))) ; horizontal
	grad[3,*,*] = ((shift(samp,+dsh,-dsh)+shift(samp,+dsh,0)+shift(samp,0,-dsh)) / (shift(samp,-dsh,+dsh)+shift(samp,0,+dsh)+shift(samp,-dsh,0)))		 ; disgonal 2
        xxx = reform(grad[0,*,*])

	grad[0,*,*] = max([grad[0,*,*],1/grad[0,*,*]],dim=1)
	grad[1,*,*] = max([grad[1,*,*],1/grad[1,*,*]],dim=1)
	grad[2,*,*] = max([grad[2,*,*],1/grad[2,*,*]],dim=1)
	grad[3,*,*] = max([grad[3,*,*],1/grad[3,*,*]],dim=1)
	mag = sqrt(grad[0,*,*]^2 + grad[1,*,*]^2 + grad[2,*,*]^2 + grad[3,*,*]^2)
	
	aux = where(finite(mag) eq 0,nr)  ; remove possible division by zero
	if nr gt 0 then mag[aux] = 0.0
	return,xxx
end


pro edge_roa4,CALLED = called, BOXSIZE = boxsize
	common rat, types, file, wid, config, tiling
	
	if not keyword_set(boxsize) then boxsize = 3  
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='RoA Edge Detector',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=3,/integer,TITLE='Filter boxsize     : ',XSIZE=3)
		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['RATIO OF AVERAGE (ROA) EDGE DETECTION (4 directions)',$
				'This filter uses 4 ratios: horizontal, vertical and the 2 diagonals',$
				' ',$
				'For more information, see A. Bovik: On detecting edges in speckled imagery,',$
				'IEEE Transactions on Acoustics, Speech and Signal Processing, 36(10), pp. 1618-1627, 1988',$
				' ',$
				'RAT module written 08/2007 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=boxsize
		widget_control,main,/destroy
		if event.id ne but_ok then return                    ; OK button _not_ clicked
	endif 

; Error Handling

	if boxsize lt 3 then begin                                   ; Wrong box size ?
		error = DIALOG_MESSAGE("Boxsizes has to be >= 3", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
  
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; handling of complex and amplitude input data

	if file.var eq 6 or file.var eq 9 then begin             ; Wrong variable type?
		error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
		if error eq "Cancel" then return else complex2abs,/called
	endif

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; Initialise tiling & progess bar

	tiling_init,overlap=(boxsize+1)
	progress,Message='RoA Edge Detector...',/cancel_button

;start block processing

	for i=0,tiling.nr_blocks-1 do begin   
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return

		tiling_read,ddd,i,block
; -------- THE FILTER ----------
		for j=0,file.vdim-1 do for k=0,file.zdim-1 do block[j,k,*,*] = roa4(reform(block[j,k,*,*]),boxsize)    
; -------- THE FILTER ----------
		tiling_write,eee,i,temporary(block)
		tiling_jumpback,ddd
	endfor
	free_lun,ddd,eee

; update everything

	rat_finalise,outputfile,finalfile,CALLED=called
	evolute,'RoA Edge Detector. Boxsize: '+strcompress(boxsize,/R)

end



