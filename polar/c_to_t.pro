;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: c_to_t
; written by    : Andreas Reigber
; last revision : 19.Jan.2004
; Transforms between covariance and coherency matrices
; Bugs: possibly has to be changed for partial polarimetry
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

pro c_to_t,CALLED=called
	common rat, types, file, wid, config

; check if array is usable

	if file.type lt 220 or file.type gt 221 then begin
		error_button = DIALOG_MESSAGE(['Data has to be a','[C] or a [T] matrix'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	if file.type eq 220 then newtype = 221l
	if file.type eq 221 then newtype = 220l
	srat,outputfile,eee,header=[4l,file.zdim,file.zdim,file.xdim,file.ydim,file.var],info=info,type=newtype
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; calculating matrix D for the unitary transform between [C] and [T]

	if file.zdim eq 3 then begin
		d  = [[1,1,0],[1,-1,0],[0,0,sqrt(2)]] / sqrt(2)
		di = adj(d)
	endif

	if file.zdim eq 4 then begin
		d  = [[1,1,0,0],[1,-1,0,0],[0,0,1,1],[0,0,complex(0,-1),complex(0,1)]] / sqrt(2)
		di = adj(d)
	endif	

	if newtype eq 220l then begin
		m1 = 'T'
		m2 = 'C'
	endif	else begin
		d  = adj(d)
		di = adj(di)
		m1 = 'C'
		m2 = 'T'
	endelse

; pop up progress window

	progress,Message='['+m1+'] -> ['+m2+']...',/cancel_button

; calculating transform

	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		oblock = make_array([file.zdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block

		for k=0,file.xdim-1 do begin
			for l=0,blocksizes[i]-1 do begin
				vec = block[*,*,k,l]
				oblock[*,*,k,l] = d # vec # di
			endfor
		endfor

		writeu,eee,oblock
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = newtype
	file.dim  = 4l
	file.vdim = file.zdim 

        evolute,'Transform:  [C] <--> [T]'
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
