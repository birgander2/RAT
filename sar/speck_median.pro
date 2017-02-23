; docformat = 'rst'
;+
; Median filter for real variable types (complex median does not exist).
; May also used for improving classification results
;
; :Keywords:
;    boxsize: in, optional, type=integer
;       default window size for filtering (default 7)
;    called: in, optional, type=flag
;       run in batch mode without gui
;
; :Author: RAT team
; :Categories: SAR, speckle filter
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
pro speck_median,CALLED = called, BOXSIZE = boxsize
   common rat, types, file, wid, config, tiling
   compile_opt idl2
   
   if not keyword_set(boxsize) then boxsize = 7                  ; Default values

   if not keyword_set(called) and not config.batch then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Median Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=boxsize,/integer,TITLE='Filter boxsize     : ',XSIZE=3)
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
				infotext = ['MEDIAN FILTER',$
				' ',$
				'RAT module written 01/2003 by Bert Wolf,',$
				'Matthias Weller and Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=boxsize
		widget_control,main,/destroy
		if event.id ne  but_ok then return                    ; OK button _not_ clicked
   endif
	
; Error Handling

	if boxsize le 1 then begin                                   ; Wrong box size ?
		error = DIALOG_MESSAGE("Boxsizes has to be > 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function

   undo_prepare,outputfile,finalfile,CALLED=CALLED

; Complex input data ????

	if file.var eq 6 or file.var eq 9 then begin         
		error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
		if error eq "Cancel" then return
		complex2abs,/called
		if wid.cancel eq 1 then return
	endif

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; Initialise tiling & progess bar

	tiling_init,overlap=(boxsize+1)/2
	progress,Message='Median Speckle Filter...',/cancel_button

;start block processing

   for i=0,tiling.nr_blocks-1 do begin   
		progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
		if wid.cancel eq 1 then return

		tiling_read,ddd,i,block
; -------- THE FILTER ----------
		for j=0,file.vdim-1 do for k=0,file.zdim-1 do block[j,k,*,*] = median(reform(block[j,k,*,*]),boxsize)    
; -------- THE FILTER ----------
		tiling_write,eee,i,temporary(block)
		tiling_jumpback,ddd
   endfor
   free_lun,ddd,eee

; update everything

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute,'Speckle filtering (Median). Boxsize: '+strcompress(boxsize,/R)

end
