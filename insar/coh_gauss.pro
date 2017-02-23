;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: coh_gauss
; written by    : Andreas Reigber
; last revision : 14.Mar.2003
; Interferometric coherence calculation using gaussian filter
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

pro coh_gauss,CALLED = called, SMMX = smmx, SMMY = smmy
	common rat, types, file, wid, config

	if (file.type ne 300) and (file.type ne 301) and (file.type ne 302) then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
	if (file.type ne 300) then begin                   
		error = DIALOG_MESSAGE(["This is not an interferometric pair !!","Only an approximation can be calculated"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
		if error eq "Cancel" then return
	endif
	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Coherence using gaussian filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=7,/integer,TITLE='Filter boxsize X   : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=7,/integer,TITLE='Filter boxsize Y   : ',XSIZE=3)
		topo   = cw_bgroup(main," use topography compensation",/nonexclusive)
		buttons = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin                                        ; Event loop
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['COHERENCE USING GAUSSIAN FILTER',$
				' ',$
				'RAT module written 2003 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=smmx                ; read widget fields
		widget_control,field2,GET_VALUE=smmy
		widget_control,topo,GET_VALUE=topo_corr
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(smmx) then smmx = 3              ; Default values
		if not keyword_set(smmy) then smmy = 3
	endelse

; Error Handling

	if smmx le 0 or smmy le 0 then begin                   ; Wrong box sizes ?
		error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
	
; change mousepointer

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type	
	head = [2l,file.xdim,file.ydim,4l]
	srat,outputfile,eee,header=head,info=info,type=type		

; calculating preview size and number of blocks
		
	bs = config.blocksize
	if topo_corr eq 0 then overlap = smmy  else overlap = 4 * smmy 
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end

;smooth box

	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
		
; pop up progress window

	progress,Message='Coherence estimation (Gauss)...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   ; loop normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
; -------- THE FILTER ----------
		if topo_corr eq 0 then begin

			if file.type eq 300 then coh = abs(g_smooth(reform(block[0,0,*,*]*conj(block[0,1,*,*])),smmx,smmy)) / sqrt(g_smooth(reform(abs(block[0,0,*,*]))^2,smmx,smmy) * g_smooth(reform(abs(block[0,1,*,*]))^2,smmx,smmy))
			if file.type eq 301 then coh = abs(g_smooth(reform(block[0,0,*,*]),smmx,smmy))/g_smooth(reform(abs(block[0,0,*,*])),smmx,smmy)
			if file.type eq 302 then coh = abs(g_smooth(reform(exp(complex(0,block[0,0,*,*]))),smmx,smmy))
		endif else begin
			if file.type eq 300 then begin
				c_pha = atan(g_smooth(reform(exp(complex(0,atan(block[0,0,*,*]*conj(block[0,1,*,*]),/phase)))),2*smmx,2*smmy),/phase)
				coh   = abs(g_smooth(reform(block[0,0,*,*]*conj(block[0,1,*,*])) * exp(complex(0,-c_pha)),smmx,smmy)) / sqrt(g_smooth(reform(abs(block[0,0,*,*]))^2,smmx,smmy) * g_smooth(reform(abs(block[0,1,*,*]))^2,smmx,smmy))
			endif
			if file.type eq 301 then begin
				c_pha = atan(g_smooth(reform(exp(complex(0,atan(block[0,0,*,*],/phase)))),2*smmx,2*smmy),/phase)
				coh = abs(g_smooth(reform(block[0,0,*,*]) * exp(complex(0,-c_pha)) ,smmx,smmy))/g_smooth(reform(abs(block[0,0,*,*])),smmx,smmy)
			endif
			if file.type eq 302 then begin
				c_pha = atan(g_smooth(reform(exp(complex(0,block[0,0,*,*]))),2*smmx,2*smmy),/phase)
				coh = abs(g_smooth(reform(exp(complex(0,block[0,0,*,*]))) * exp(complex(0,-c_pha)) ,2*smmx,2*smmy))
			endif
		endelse
; -------- THE FILTER ----------
		if i eq anz_blocks-1 then ypos2 = bs_last

		writeu,eee,coh[*,ypos1:ypos2-1]
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
	endfor
	free_lun,ddd,eee

; update file information
	
	file_move,outputfile,finalfile,/overwrite

	file.name = finalfile
	file.dim  = 2l
	file.zdim = 1l
	file.var  = 4l
	file.type = 310l

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
