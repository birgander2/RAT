;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_pollee
; last revision : 24. March 2004
; written by    : Andreas Reigber
; Lee's speckle filter (polarimetric version)
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

function lee_pol,arr,smm,LOOKS=looks
	if not keyword_set(looks) then looks=1.0

	siz    = size(arr)
	anzz = siz[2]
	anzx = siz[3]
	anzy = siz[4]
	anzz2 = anzz^2

	sig2  = 1.0/looks
	sfak  = 1.0+sig2

	out   = complexarr(anzz,anzz,anzx,anzy)
	box   = fltarr(smm,smm)+1.0/smm/smm
	amp   = real_part(block_trace(arr))

	m2arr  = smooth(amp^2,smm)
	marr   = smooth(amp,smm)
	vary   = m2arr - marr^2
	varx   = (vary - (marr^2)*sig2)/sfak > 0
	k      = varx / vary
	aux = where(finite(k) eq 0,nr)
	if nr gt 0 then k[aux] = 0.0

	for i=0,anzz-1 do begin
		for j=0,anzz-1 do begin
			out[i,j,*,*] = convol(reform(arr[i,j,*,*]),box,/center,/edge_truncate)
			out[i,j,*,*] += (arr[i,j,*,*] - out[i,j,*,*]) * k
		endfor
	endfor
	return,out
end

pro speck_pollee,CALLED = called, SMM = smm, LOOKS = looks
	common rat, types, file, wid, config

	if ~((file.type ge 200 and file.type lt 210) or (file.type ge 220 and file.type le 230) or $
             (file.type ge 500 and file.type le 510) or (file.type ge 510 and file.type le 520)) then begin
           error_button = DIALOG_MESSAGE(['Data have to be in','vectors or matrix format'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
           return
        endif

        if n_elements(smm) eq 0 then smm = 7 ; Default values
        if n_elements(looks) eq 0 then looks = 1.0

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Polarimetric Lee Speckle Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1   = CW_FIELD(main,VALUE=smm,/integer,  TITLE='Filter boxsize        : ',XSIZE=3)
		field2   = CW_FIELD(main,VALUE=strcompress(/r,string(looks,f='(f9.1)')),/float,TITLE='Effective No of Looks : ',XSIZE=3)
		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]


		repeat begin
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['POLARIMETRIC LEE SPECKLE FILTER',$
				' ',$
				'RAT module written 01/2002 by Andreas Reigber',$
				' ',$
				'further information:',$
				'J.S.Lee et al.: "Speckle Reduction in Multipolarization,',$
				'Multifrequency SAR Imagery", Trans. on Geoscience and Remote',$
				'Sensing, Vol. 29, No. 4, pp. 535-544, 1991']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=smm
		widget_control,field2,GET_VALUE=looks
		widget_control,main,/destroy
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif
; Error Handling

	if smm le 0 and not keyword_set(called) then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
	undo_prepare,outputfile,finalfile,CALLED=CALLED

; transform polarimetric vector to matrix

	if (file.type ge 200 and file.type le 210) || (file.type ge 500 and file.type lt 510) then begin
           if not keyword_set(called) then dummy = DIALOG_MESSAGE(["Data are in vector form. They have","to be converted to matrix form first."], DIALOG_PARENT = wid.base, /information)
           if file.type ge 500 && file.type le 503 then polin_k2m,/called $
           else k_to_m,/called
           if wid.cancel eq 1 then return
        endif
	
; read / write header

	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=head,info=info,type=type		
		
; calculating preview size and number of blocks
		
	bs = config.blocksize
	overlap = (smm + 1) / 2
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end

	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
 
; pop up progress window

	progress,Message='Polarimetric Lee Speckle Filter...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
; -------- THE FILTER ----------
		block = lee_pol(block,smm,looks=looks)
; -------- THE FILTER ----------
		if i eq anz_blocks-1 then ypos2 = bs_last
		writeu,eee,block[*,*,*,ypos1:ypos2-1]
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
	endfor
	free_lun,ddd,eee

; update file information
	
	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile

        evolute,'Speckle filtering (Polarimetric Lee): boxsize'+strcompress(smm,/R)+' looks: '+strcompress(looks,/R)


; generate preview
	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
