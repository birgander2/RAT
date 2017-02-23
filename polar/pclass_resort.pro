;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Resort classification indices
; IDEA: Give large classes in small indices and smaller ones in the bigger indices
;       In predefined palettes, the first colours are optimised for this (not so colourful)
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

pro pclass_resort
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag
	
	if not (file.type ge 400 and file.type lt 499) then begin
		error_button = DIALOG_MESSAGE(['Input data have to be a classification'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
	
; calculate new type

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

; calculating histograms
	
	progress,Message='Calculating histograms...',/cancel_button
	hist = lon64arr(65536)
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		aux = histogram(block,min=0)
		hist[0:n_elements(aux)-1] += aux
	endfor
	free_lun,ddd

	sclass = reverse(sort(hist))
	nclass = 65535
	while hist[nclass] eq 0 do --nclass

	progress,Message='Resorting classes...',/cancel_button
	rrat,file.name,ddd,header=head,info=info,type=type
	srat,outputfile,eee,header=head,info=info,type=type

	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.xdim,blocksizes[i]],type=file.var)
		oblock  = make_array([file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		for j=0,nclass do begin
			aux = where(block eq sclass[j],nr)
			if nr gt 0 then oblock[aux] = j
		endfor
		writeu,eee,oblock
	endfor
	free_lun,ddd,eee
	
; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif

	
end
