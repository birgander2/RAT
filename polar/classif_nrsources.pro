;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: classif_nrsources
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
PRO classif_nrsources, CALLED = called
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	
	; Check if array is usable should be a terrain classification
	IF file.type NE 404 THEN BEGIN
		error_button = DIALOG_MESSAGE(['Data have to be an physical classification'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		RETURN
	ENDIF
	
	; ----------------------
	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
	
	; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,2l],info=info,type=408l		
	
	; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	; pop up progress window
	progress,Message='Number of scattering mechanisms...',/cancel_button

	FOR i=0,anz_blocks-1 DO BEGIN
		
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = MAKE_ARRAY([file.xdim,blocksizes[i]],type=2l)
		oblock = MAKE_ARRAY([file.xdim,blocksizes[i]],type=2l)
		READU,ddd,block
		aux = WHERE(block EQ 1,count)
		IF count GT 0 THEN oblock[aux] = 3
		aux = WHERE(block EQ 2 OR block EQ 4,count)
		IF count GT 0 THEN oblock[aux] = 2
		aux = WHERE(block EQ 3 OR block EQ 5,count)
		IF count GT 0 THEN oblock[aux] = 1
		WRITEU,eee,oblock		
	ENDFOR
	FREE_LUN,ddd,eee
	
	; update file information

	FILE_MOVE,outputfile,finalfile,/OVERWRITE
		
	file.name = finalfile
	file.type = 408
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
END
