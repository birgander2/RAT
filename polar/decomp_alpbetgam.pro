;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: decomp_alpbetgam
; written by    : Andreas Reigber
; last revision : 9.Feb.2005
; Calculates alpha beta and gamma angles
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



pro decomp_alpbetgam
	common rat, types, file, wid, config

; check if array is usable

	if file.type ne 214 and file.type ne 220 and file.type ne 221 then begin
		error_button = DIALOG_MESSAGE(['Input data have to be a [C] matrix','[T] matrix or an eigendecomposition.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

; ----------------------

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
	
	if file.type eq 220 then begin             ; Wrong variable type?
		error = DIALOG_MESSAGE(["Performing necessary preprocessing step:","Tranformation to Pauli representation"], /cancel, DIALOG_PARENT = wid.base, TITLE='Information')
		if error eq "Cancel" then return else c_to_t,/called
	endif

	if file.type eq 221 then begin             ; Wrong variable type?
		error = DIALOG_MESSAGE(["Performing necessary preprocessing step:","Eigenvector Decomposition"], /cancel, DIALOG_PARENT = wid.base, TITLE='Information')
		if error eq "Cancel" then return else decomp_eigen,/called
	endif

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[3l,4*(file.zdim < 3),file.xdim,file.ydim,4l],info=info,type=215l		
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='alpha beta gamma delta...',/cancel_button

	index = indgen(file.zdim < 3)
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=6l)
		oblock = make_array([4*(file.zdim < 3),file.xdim,blocksizes[i]],type=4l)
		readu,ddd,block
		for k=0,file.xdim-1 do begin
			for l=0,blocksizes[i]-1 do begin	
				ew = reform(abs(block[file.vdim-1,*,k,l]))
				ev = reform(abs(block[0:file.vdim-2,*,k,l]))
				if file.zdim eq 4 then begin
					ew = ew[0:2]
					ev = ev[0:2,0:2]
				endif
				oblock[0:2,k,l]  = acos(abs(ev[0,index]))
				oblock[3:5,k,l]  = acos(abs(ev[1,index])/sin(oblock[0:2,k,l]))
				oblock[6:8,k,l]  = atan(ev[2,index],/phase)
				oblock[9:11,k,l] = atan(ev[1,index],/phase)
			endfor
		endfor
		
		aux = where(finite(oblock) eq 0,anz)  ; eliminate nan's
   	if anz ne 0 then oblock[aux]=0.0
		
		writeu,eee,oblock
	endfor
	free_lun,ddd,eee
	
; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 234l
	file.vdim = 1l
	file.zdim = 12l
	file.dim  = 3l
	file.var  = 4l
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
