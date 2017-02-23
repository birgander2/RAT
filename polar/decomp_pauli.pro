;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: decomp_pauli
; written by    : Andreas Reigber
; last revision : 14.Feb.2003
; Calculates polarimetric Pauli decomposition
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



pro decomp_pauli,CALLED=called
	common rat, types, file, wid, config

; check if array is usable

	if file.type eq 220 or file.type eq 221 then begin
		error_button = DIALOG_MESSAGE(['Data have to be a polarimetric vector.',' ',$
		                               'Hint: To transform a covariance matrix into a',$
												 'coherency matrix, you have to use the function',$
												 'PolSAR -> Transform -> [C] to [T]'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
	if file.type ne 200 then begin
		error_button = DIALOG_MESSAGE(['Data have to be a polarimetric vector in',$
		                               'lexicographic basis or a covariance matrix'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif


	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=210l	

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='Pauli Decomposition...',/cancel_button

; calculating Pauli decomposition
	
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		block[0,*,*] = block[0,*,*] + block[1,*,*] 
		block[1,*,*] = block[0,*,*] - 2*block[1,*,*] 
		if file.zdim eq 3 then block[2,*,*] = sqrt(2)*block[2,*,*]
		if file.zdim eq 4 then begin
			block[2,*,*] = block[2,*,*] + block[3,*,*] 
			block[3,*,*] = (2*block[3,*,*] - block[2,*,*])*COMPLEX(0,1) 
		endif 
		writeu,eee,(1./SQRT(2.))*block
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 210l
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
