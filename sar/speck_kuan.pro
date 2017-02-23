;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_kuan
; last revision : 18.Nov.2003
; written by    : Andre Lehmann
;                 Andreas Reigber
; Lee's speckle filter               
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


function kuan,amp,smm,LOOKS=looks
	if not keyword_set(looks) then looks = 1.0
	siz    = size(amp)	
	anz_rg = siz[1]
	anz_az = siz[2]

	delta = (smm-1)/2
	sig2  = 1.0 / looks
	sfak  = 1.0 + sig2
   zfak  = smm^2
	out   = amp-amp
   fbox  = mbox(smm,anz_rg)

	samp  = (amp - smooth(amp,smm,/edge_truncate))^2
	
	x = (findgen(smm) - (smm-1)/2.0)*4.0
	gauss = 1/(sqrt(2*!pi)*smm)*exp(-(x^2)/(2*smm^2))
	res1 = (fltarr(smm) + 1) ## gauss
   res2 = gauss ## (fltarr(smm) + 1)
	gauss2d = res1 * res2
	gauss2d = gauss2d / mean(gauss2d)/zfak 
	
	marr = smooth(amp,smm)
	vary = convol(samp,gauss2d,/center) > 0
	varx = (vary - marr^2*sig2)/sfak > 0
	k    = varx / vary > 0
	out  = marr + (amp-marr) * k
	
	return,(out > 0)
end

function mbox,n,anz
	box = intarr(n*n)
	for i=0,n-1 do box[n*i] = (findgen(n)-n/2)+anz*(i-n/2)
	return,box
end



pro speck_kuan,CALLED = called, BOXSIZE = boxsize, LOOKS=looks
	common rat, types, file, wid, config, tiling

	if not keyword_set(boxsize) then boxsize = 7l             ; Default values
	if not keyword_set(looks) then looks = 1.0

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Lee Speckle Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=boxsize,/integer,  TITLE='Filter boxsize        : ',XSIZE=3)
		field2   = CW_FIELD(main,VALUE=looks,/float,TITLE='Effective No of Looks : ',XSIZE=3)
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
				infotext = ['KUAN SPECKLE FILTER',$
				' ',$
				'RAT module written 11/2003 by Andre Lehmann',$
				' ',$
				'further information:',$
				' ']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=boxsize
		widget_control,field2,GET_VALUE=looks
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif 

; Error handling

	if boxsize lt 3 then begin                                 ; Wrong box size ?
		error = DIALOG_MESSAGE("Boxsize has to be >= 3", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if looks lt 1.0 then begin                              ; Looks to small ?
		error = DIALOG_MESSAGE("Looks have to be >= 1.0", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function

   undo_prepare,outputfile,finalfile,CALLED=CALLED

; handling of complex and amplitude input data
	
	ampflag = 0
	if file.var eq 6 or file.var eq 9 then begin             ; Wrong variable type?
		error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
		if error eq "Cancel" then return else complex2abs,/called
		if wid.cancel eq 1 then return
		ampflag = 1
	endif
	if file.type eq 100 then ampflag = 1

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; Initialise tiling & progess bar

	tiling_init,overlap=(boxsize+1)/2
	progress,Message='Kuan Speckle Filter...',/cancel_button

;start block processing

	for i=0,tiling.nr_blocks-1 do begin   
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return

		tiling_read,ddd,i,block
; -------- THE FILTER ----------
		if ampflag eq 1 then block = block^2
		for j=0,file.vdim-1 do for k=0,file.zdim-1 do block[j,k,*,*] = kuan(reform(block[j,k,*,*]),boxsize,looks=looks)
		if ampflag eq 1 then block = sqrt(block)
; -------- THE FILTER ----------
		tiling_write,eee,i,temporary(block)
		tiling_jumpback,ddd
	endfor
	free_lun,ddd,eee

; update everything

	rat_finalise,outputfile,finalfile,CALLED=called
	evolute,'Speckle filtering (Kuan). Looks: '+strcompress(looks,/R)+' Boxsize: '+strcompress(boxsize,/R)

end


