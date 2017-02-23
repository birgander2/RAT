;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: pol_to_span
; written by    : Andreas Reigber
; last revision : 14.Feb.2003
; Calculates span/trace of polarimetric vectors/matrices
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



pro pol_to_span,CALLED = called
	common rat, types, file, wid, config

; check if array is usable

	if not (file.type eq 100 or file.type eq 101 or file.type eq 600 or file.type eq 601 or file.type eq 610 or file.type eq 615 or (file.type ge 200 and file.type le 250)) then begin
		error_button = DIALOG_MESSAGE(['Wrong file type','SAR image, scattering vector','covariance required'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
	if file.zdim eq 1 and file.vdim eq 1 then begin
		error_button = DIALOG_MESSAGE(['Nothing to average!','Vector/matrix information required'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif


	WIDGET_CONTROL,/hourglass
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED


; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,4l],info=info,type=103l	
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='Calculate span...',/cancel_button

; calculating span
	
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		if file.dim eq 3 then oblock = total(abs(block)^2,2)       ; polarimetric vector
		if file.dim eq 4 then begin                               
			case file.type of
				600 or 601: begin   ; subapertures    
					oblock = total(total(abs(block)^2,1),1) 
				end
				610: begin          ; Covariance matrices for every subaperture
					for j=0,sqrt(file.zdim)-1 do oblock += total(reform(abs(block[j*sqrt(file.zdim)+1,*,*,*])),1)
				end
				else: begin                                        ; subaperture or polarimetric matrix
					oblock = make_array([file.xdim,blocksizes[i]],type=4l)
					for j=0,file.zdim-1 do oblock += reform(abs(block[j,j,*,*]))
				end
			endcase	
		endif
		writeu,eee,oblock
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 103l
	file.dim  = 2l
	file.zdim = 1l
	file.vdim = 1l
	file.var  = 4l
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
