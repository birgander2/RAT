;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: convent_entropy_alpha
; written by    : Andreas Reigber, Guido Bethke
; last revision : 10.May.2005
; Conventional Entropy-Alpha-Classification
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

pro classif_ea,CALLED=called
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

; check if array is usable

	if file.type ne 233 then begin
		error_button = DIALOG_MESSAGE(['Input data has to be an Entropy/Alpha decomposition'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif


	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,2l],info=info,type=400l

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='H/a classification',/cancel_button

	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.zdim,file.xdim,blocksizes[i]],type=4l)
		oblock = make_array([file.xdim,blocksizes[i]],type=2l)
		readu,ddd,block
		block[1,*,*] *=  !radeg
		
		ent = reform(block[0,*,*])
		alp = reform(block[1,*,*])
		
		index_1 = where ((ent ge 0.9) and (ent le 1.0) $
		    	and (alp ge 55.0) and (alp le 90.0),anz_1)
	    	if anz_1 gt 0 then oblock[index_1] = 1

		index_2 = where ((ent ge 0.9) and (ent le 1.0) $
		    	and (alp ge 40.0) and (alp lt 55.0),anz_2)
		if anz_2 gt 0 then oblock[index_2] = 2

		index_3 = where ((ent ge 0.9) and (ent le 1.0) $
		    	and (alp ge 00.0) and (alp lt 40.0),anz_3)
		if anz_3 gt 0 then oblock[index_3] = 3

		index_4 = where ((ent ge 0.5) and (ent lt 0.9) $
		    	and (alp ge 50.0) and (alp le 90.0),anz_4)
		if anz_4 gt 0 then oblock[index_4] = 4

		index_5 = where ((ent ge 0.5) and (ent lt 0.9) $
		    	and (alp ge 40.0) and (alp lt 50.0),anz_5)
		if anz_5 gt 0 then oblock[index_5] = 5

		index_6 = where ((ent ge 0.5) and (ent lt 0.9) $
		    	and (alp ge 00.0) and (alp lt 40.0),anz_6)
		if anz_6 gt 0 then oblock[index_6] = 6

		index_7 = where ((ent ge 0.0) and (ent lt 0.5) $
		    	and (alp  ge 47.5) and (alp le 90.0),anz_7)
		if anz_7 gt 0 then oblock[index_7] = 7

		index_8 = where ((ent ge 0.0) and (ent lt 0.5) $
		    	and (alp ge 42.5) and (alp lt 47.5),anz_8)
		if anz_8 gt 0 then oblock[index_8] = 8

		index_9 = where ((ent ge 0.0) and (ent lt 0.5) $
		    	and (alp ge 00.0) and (alp lt 42.5),anz_9)
		if anz_9 gt 0 then oblock[index_9] = 9

		index_0 = where ((ent lt 0.0) and (ent gt 1.0) $
		    	and (alp lt 00.0) and (alp gt 90.0),anz_0)
		if anz_0 gt 0 then oblock[index_0] = 0
			    
		aux = where(finite(oblock) eq 0,anz)  ; eliminate nan's
		if anz gt 0 then oblock[aux] = 0

		writeu,eee,oblock
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 400l
	file.vdim = 1l
	file.zdim = 1l
	file.dim  = 2l
	file.var  = 2l

; set palette
	
	palettes[0,*,*] = palettes[5,*,*]   ; palettes 5 = classification
	palettes[1,*,*] = palettes[5,*,*]   ; to actual and suggestion

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	

end
