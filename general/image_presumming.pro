; docformat = 'rst'
;+
; Presumming of images (averaging of adjacent pixels to reduce image size).
; The current type of image is considered to perform the averaging in
; an optimal way. Attention when presumming complex images: This is probably
; not what you want to do!
;
; :Keywords:
;    smmx: in, optional, type=integer
;       the horizontal presumming factor
;    smmy: in, optional, type=integer
;       the vertical presumming factor
;    called: in, optional, type=flag
;       run in batch mode without gui
;       
;
; :Author: RAT team
; :Categories: general
;
; :Copyright:
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
;-
pro image_presumming,CALLED = called, SMMX = smmx, SMMY = smmy
	common rat, types, file, wid, config

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Image Presumming',/modal)
		field1 = CW_FIELD(main,VALUE=2,/integer,TITLE='Presumming factor in X   : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=2,/integer,TITLE='Presumming factor in Y   : ',XSIZE=3)
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
				infotext = ['IMAGE PRESUMMING',$
				' ',$
				'RAT module written 2003 Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) 
		widget_control,field1,GET_VALUE=smmx                ; read widget fields
		widget_control,field2,GET_VALUE=smmy
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(smmx) then smmx = 1     			 ; Default values
		if not keyword_set(smmy) then smmy = 1 
	endelse

; Error Handling

	if smmx lt 1 or smmy lt 1 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Presumming has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

   WIDGET_CONTROL,/hourglass

; undo function

   undo_prepare,outputfile,finalfile,CALLED=CALLED

; scaling factors
		
	xnew   = floor(file.xdim/smmx) 		
	ynew   = floor(file.ydim/smmy) 		
	rebx   = xnew * smmx - 1
	
; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	head[file.dim-1] = xnew		
	head[file.dim]   = ynew	
	srat,outputfile,eee,header=head,info=info,type=type		
		
; pop up progress window

	progress,Message='Presumming image...',/cancel_button

;do the transform

	for i=0,ynew-1 do begin   
		progress,percent=(i+1)*100.0/ynew,/check_cancel
		if wid.cancel eq 1 then return

		block = make_array([file.vdim,file.zdim,file.xdim,smmy],type=file.var)
		readu,ddd,block
		
		if (file.type ge 400 and file.type lt 500) then begin
			block = congrid(reform(block[*,*,*,0]),xnew)
		endif else begin
			if file.type eq 102 or file.type eq 302 then block = exp(complex(0,block))
			if smmy gt 1 then block = total(block,4)/smmy		
			if smmx gt 1 then begin
				var = (size(block))[4]
				if	var eq 6 or var eq 9 then  block = rebinc(block[*,*,0:rebx],file.vdim,file.zdim,xnew,/nosmooth) $
				else block = rebin(block[*,*,0:rebx],file.vdim,file.zdim,xnew) 
			endif
			if file.type eq 102 or file.type eq 302 then block = atan(block,/phase)
		endelse
		writeu,eee,block
	endfor
	free_lun,ddd,eee

; update everything

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute, 'Image presumming: '+strcompress(smmx, /r)+'x'+strcompress(smmy, /r)

end
