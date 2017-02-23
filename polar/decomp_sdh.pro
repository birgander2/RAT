;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: decomp_sdh
; written by    : Andreas Reigber
; last revision : 14.Feb.2003
; Calculates polarimetric sphere diplane helix decomposition
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



pro decomp_sdh,CALLED = called
	common rat, types, file, wid, config

; check if array is usable

	if file.type ne 209 then begin
		error_button = DIALOG_MESSAGE(['Data has to be a','polarimetric vector in','circular basis'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[3l,3l,file.xdim,file.ydim,4l],info=info,type=210l

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='Sphere-Diplane-Helix Decomposition...',/cancel_button

; calculating span
	
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
		oblock = make_array([3l,file.xdim,blocksizes[i]],type=4l)
		readu,ddd,block

		oblock[0,*,*]  =  abs(block[2,*,*])  ; sphere = RL
		
		oblock[1,*,*]  =  min(abs(block[[0,1],*,*]),dim=1)  ; diplane = min(|LL|,|RR|)

		oblock[2,*,*]  =  abs(abs(block[0,*,*]) - abs(block[1,*,*]))  ; helix = ||RR| - |LL||

		writeu,eee,oblock
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 213l
   file.var  = 4l
   file.zdim = 3L

        evolute,'Polarimetric Sphere-Diplane-Helix Decomposition.'
	
; generate preview
	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
