;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: convent_entropy_alpha_anisotropy
; written by    : Andreas Reigber and Guido Bethke
; last revision : 10.May.2005
; Conventional Entropy-Alpha-Anisotropy-Classification
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



pro classif_eaa,CALLED=called
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
	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,2l],info=info,type=401l

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='H/a/A classification',/cancel_button

	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.zdim,file.xdim,blocksizes[i]],type=4l)
		oblock = make_array([file.xdim,blocksizes[i]],type=2l)
		readu,ddd,block
		block[1,*,*] *= !radeg

		ent = reform(block[0,*,*])
		alp = reform(block[1,*,*])
		ani = reform(block[2,*,*])

		index_1 = where ((ent ge 0.9) and (ent le 1.0) $
			and (alp ge 55.0) and (alp le 90.0) $
			and (ani ge 0.5) and (ani le 1.0), anz_1)
		if anz_1 gt 0 then oblock[index_1] = 1

		index_2 = where ((ent ge 0.9) and (ent le 1.0) $
			and (alp ge 55.0) and (alp le 90.0) $
			and (ani ge 0.0) and (ani lt 0.5), anz_2)
		if anz_2 gt 0 then oblock[index_2] = 2

		index_3 = where ((ent ge 0.9) and (ent le 1.0) $
			and (alp ge 40.0) and (alp lt 55.0) $
			and (ani ge 0.5) and (ani le 1.0), anz_3)
		if anz_3 gt 0 then oblock[index_3] = 3
			
		index_4 = where ((ent ge 0.9) and (ent le 1.0) $
			and (alp ge 40.0) and (alp lt 55.0) $
			and (ani ge 0.0) and (ani lt 0.5), anz_4)
		if anz_4 gt 0 then oblock[index_4] = 4

		index_5 = where ((ent ge 0.9) and (ent le 1.0) $
			and (alp ge 00.0) and (alp lt 40.0) $
			and (ani ge 0.5) and (ani le 1.0), anz_5)
		if anz_5 gt 0 then oblock[index_5] = 5

		index_6 = where ((ent ge 0.9) and (ent le 1.0) $
			and (alp ge 00.0) and (alp lt 40.0) $
			and (ani ge 0.0) and (ani lt 0.5), anz_6)
		if anz_6 gt 0 then oblock[index_6] = 6

		index_7= where ((ent ge 0.5) and (ent lt 0.9) $
			and (alp ge 50.0) and (alp le 90.0) $
			and (ani ge 0.5) and (ani le 1.0), anz_7)
		if anz_7 gt 0 then oblock[index_7] = 7

		index_8 = where ((ent ge 0.5) and (ent lt 0.9) $
			and (alp ge 50.0) and (alp le 90.0) $
			and (ani ge 0.0) and (ani lt 0.5), anz_8)
		if anz_8 gt 0 then oblock[index_8] = 8

		index_9 = where ((ent ge 0.5) and (ent lt 0.9) $
			and (alp ge 40.0) and (alp lt 50.0) $
			and (ani ge 0.5) and (ani le 1.0), anz_9)
		if anz_9 gt 0 then oblock[index_9] = 9

		index_10 = where ((ent ge 0.5) and (ent lt 0.9) $
			and (alp ge 40.0) and (alp lt 50.0) $
			and (ani ge 0.0) and (ani lt 0.5), anz_10)
		if anz_10 gt 0 then oblock[index_10] = 10

		index_11 = where ((ent ge 0.5) and (ent lt 0.9) $
			and (alp ge 00.0) and (alp lt 40.0) $
			and (ani ge 0.5) and (ani le 1.0), anz_11)
		if anz_11 gt 0 then oblock[index_11] = 11

		index_12 = where ((ent ge 0.5) and (ent lt 0.9) $
			and (alp ge 00.0) and (alp lt 40.0) $
			and (ani ge 0.0) and (ani lt 0.5), anz_12)
		if anz_12 gt 0 then oblock[index_12] = 12

		index_13 = where ((ent ge 0.0) and (ent lt 0.5) $
			and (alp ge 47.5) and (alp le 90.0) $
			and (ani ge 0.5) and (ani le 1.0), anz_13)
		if anz_13 gt 0 then oblock[index_13] = 13

		index_14 = where ((ent ge 0.0) and (ent lt 0.5) $
			and (alp ge 47.5) and (alp le 90.0) $
			and (ani ge 0.0) and (ani lt 0.5), anz_14)
		if anz_14 gt 0 then oblock[index_14] = 14

		index_15 = where ((ent ge 0.0) and (ent lt 0.5) $
			and (alp ge 42.5) and (alp lt 47.5) $
			and (ani ge 0.5) and (ani le 1.0), anz_15)
		if anz_15 gt 0 then oblock[index_15] = 15

		index_16 = where ((ent ge 0.0) and (ent lt 0.5) $
			and (alp ge 42.5) and (alp lt 47.5) $
			and (ani ge 0.0) and (ani lt 0.5), anz_16)
		if anz_16 gt 0 then oblock[index_16] = 16

		index_17 = where ((ent ge 0.0) and (ent lt 0.5) $
			and (alp ge 00.0) and (alp lt 42.5) $
			and (ani ge 0.5) and (ani le 1.0), anz_17)
		if anz_17 gt 0 then oblock[index_17] = 17

		index_18 = where ((ent ge 0.0) and (ent lt 0.5) $
			and (alp ge 00.0) and (alp lt 42.5) $
			and (ani ge 0.0) and (ani lt 0.5), anz_18)
		if anz_18 gt 0 then oblock[index_18] = 18

		index_0 = where ((ent lt 0.0) and (ent gt 1.0) $
			and (alp lt 00.0) and (alp gt 90.0) $
			and (ani lt 0.0) and (ani gt 1.0),anz_0)
		if anz_0 gt 0 then oblock[index_0] = 0

		aux = where(finite(oblock) eq 0,anz)  ; eliminate nan's
		if anz gt 0 then oblock[aux] = 0

		writeu,eee,oblock
	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 401l
	file.vdim = 1l
	file.zdim = 1l
	file.dim  = 2l
	file.var  = 2l

; set palette
	
	palettes[0,*,*] = palettes[5,*,*]   ; palettes 5 = classification
	palettes[1,*,*] = palettes[5,*,*]   ; copy to actual and suggestion

; generate preview

        if ~keyword_set(called) then begin
           generate_preview
           update_info_box
        endif else progress,/destroy

; feature space plot

;   	loadct,0,/silent
;   	r = [0,0,0,100,125,255,255,100,125,100,125,200,225,150,175,125,150,100,125,bytarr(237)]
;   	g = [0,100,125,125,150,255,255,150,175,200,225,200,225,150,175,100,125,150,175,bytarr(237)]
;   	b = [0,0,0,0,0,255,255,150,175,75,100,75,100,150,175,50,75,200,225,bytarr(237)]
;   	tvlct, r, g, b
;   	plot, [0,90], [0,0], line=2, xrange=[0,1], yrange=[0,90], $
;   	xtitle='die x-AChse', ytitle='die y-Achse', ystyle=1, color=145
;   	if anz_1 gt 0 then oplot, ent[index_1], alp[index_1], psym = 3, color = 1
;   	if anz_2 gt 0 then oplot, ent[index_2], alp[index_2], psym = 3, color = 2
;   	if anz_3 gt 0 then oplot, ent[index_3], alp[index_3], psym = 3, color = 3
;   	if anz_4 gt 0 then oplot, ent[index_4], alp[index_4], psym = 3, color = 4
;   	if anz_5 gt 0 then oplot, ent[index_5], alp[index_5], psym = 3, color = 5
;   	if anz_6 gt 0 then oplot, ent[index_6], alp[index_6], psym = 3, color = 6
;   	if anz_7 gt 0 then oplot, ent[index_7], alp[index_7], psym = 3, color = 7
;   	if anz_8 gt 0 then oplot, ent[index_8], alp[index_8], psym = 3, color = 8
;   	if anz_9 gt 0 then oplot, ent[index_9], alp[index_9], psym = 3, color = 9
;   	if anz_10 gt 0 then oplot, ent[index_10], alp[index_10], psym = 3, color = 10
;   	if anz_11 gt 0 then oplot, ent[index_11], alp[index_11], psym = 3, color = 11
;   	if anz_12 gt 0 then oplot, ent[index_12], alp[index_12], psym = 3, color = 12
;   	if anz_13 gt 0 then oplot, ent[index_13], alp[index_13], psym = 3, color = 13
;   	if anz_14 gt 0 then oplot, ent[index_14], alp[index_14], psym = 3, color = 14
;   	if anz_15 gt 0 then oplot, ent[index_15], alp[index_15], psym = 3, color = 15
;   	if anz_16 gt 0 then oplot, ent[index_16], alp[index_16], psym = 3, color = 16
;   	if anz_17 gt 0 then oplot, ent[index_17], alp[index_17], psym = 3, color = 17
;   	if anz_18 gt 0 then oplot, ent[index_18], alp[index_18], psym = 3, color = 18
;   	loadct,0,/silent

end
