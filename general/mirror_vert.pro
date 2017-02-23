;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; mirror_vert.pro   (RAT module)
; mirrors a data set vertically
; 02/2003 by Andreas Reigber
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
pro mirror_vert,CALLED=called
	common rat, types, file, wid, config

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last
	
; pop up progress window

	progress,Message='Mirror vertically...',/cancel_button

; read blocks
	
	for i=0,anz_blocks-1 do begin   ; read blocks
		progress,percent=(i+1)*100.0/anz_blocks/2,/check_cancel
		if wid.cancel eq 1 then return
		filename = config.tempdir+'tempfile_'+strcompress(i,/remove)+'.rat'
		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		srat,filename,reverse(block,4)
	endfor
	for i=anz_blocks-1,0,-1 do begin ; write blocks
		progress,percent=(anz_blocks-i+1)*100.0/anz_blocks/2+50,/check_cancel
		if wid.cancel eq 1 then return

		filename = config.tempdir+'tempfile_'+strcompress(i,/remove)+'.rat'
		rrat,filename,block
		writeu,eee,block
		file_delete,filename,/quiet,/allow_nonexistent
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	
; generate preview

	if not keyword_set(called) then generate_preview
	
end
