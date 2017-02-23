;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: unwrap_rewrap
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
pro unwrap_rewrap,CALLED=called
	common rat, types, file, wid, config

; check if array is complex

	if (file.type ne 303) then begin
		error_button = DIALOG_MESSAGE('Unwrapped phase required', DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	head = [2l,file.xdim,file.ydim,4l]
	srat,outputfile,eee,header=head,info=info,type=302l		
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last
	
; pop up progress window

	progress,Message='Extracting phase...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
; -------- THE FILTER ----------
 		writeu,eee,atan(exp(complex(0,block)),/phase)
; -------- THE FILTER ----------
	endfor
	free_lun,ddd,eee

; update file information
	
	file_move,outputfile,finalfile,/overwrite

	file.name = finalfile
	file.dim  = 2l
	file.zdim = 1l
	file.var  = 4l
	file.type = 302l

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
	
end
