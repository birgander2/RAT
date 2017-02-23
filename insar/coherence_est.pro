;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: coh_mean
; written by    : Andreas Reigber
; last revision : 14.Mar.2003
; Interferometric coherence calculation using boxcar filter
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

pro coherence_est,CALLED = called, SMMX = smmx, SMMY = smmy
	common rat, types, file, wid, config

;  	if (file.type ne 300) and (file.type ne 301) and (file.type ne 302) then begin                   
;  		error = DIALOG_MESSAGE("This is not an interferogram", DIALOG_PARENT = wid.base, TITLE='Error',/error)
;  		return
;  	endif
;  ;  
;  	if (file.type ne 300) then begin                   
;  		error = DIALOG_MESSAGE(["This is not an interferometric pair !!","Only an approximation can be calculated"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
;  		if error eq "Cancel" then return
;  	endif
;  
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Coherence estimation',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		field1 = CW_FIELD(main,VALUE=7,/integer,TITLE='Filter boxsize X   : ',XSIZE=3)
		field2 = CW_FIELD(main,VALUE=7,/integer,TITLE='Filter boxsize Y   : ',XSIZE=3)
		topo   = cw_bgroup(main," use topography compensation",/nonexclusive)
      coco   = cw_bgroup(main," complex coherence",/nonexclusive)
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
				infotext = ['COHERENCE USING BOXCAR FILTER',$
				' ',$
				'RAT module written 2003 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,field1,GET_VALUE=smmx                ; read widget fields
		widget_control,field2,GET_VALUE=smmy
		widget_control,topo,GET_VALUE=topo_corr
		widget_control,coco,GET_VALUE=coh_cmpl
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
	newdim  = file.dim
	newvdim = file.vdim
	newzdim = file.zdim
	if coh_cmpl eq 0 then begin
		newvar = 4l 
		newtype = 310l
	endif else begin
		newvar = 6l
		newtype = 311l
	endelse

	if file.dim eq 2 then head = [2l,file.xdim,file.ydim,newvar]
	if file.dim eq 3 then head = [3l,file.zdim,file.xdim,file.ydim,newvar]
	if file.dim eq 4 then head = [4l,file.vdim,file.zdim,file.xdim,file.ydim,newvar]

	if file.type eq 300 then begin
		newdim  = 2
		newvdim = 1
		newzdim = 1
		head = [2,file.xdim,file.ydim,newvar]
	endif
	if file.type eq 500 or file.type eq 501 then begin
		newdim  = 3
		newvdim = 1
		newzdim = file.zdim
		head = [3,newzdim,file.xdim,file.ydim,newvar]
	endif
	if file.type ge 510 and file.type le 520 then begin
		newdim  = 4
		newvdim = file.vdim/2
		newzdim = file.zdim/2
		head = [4,newvdim,newzdim,file.xdim,file.ydim,newvar]
	endif

	srat,outputfile,eee,header=head,info=info,type=type		

; calculating preview size and number of blocks
		
	bs = config.blocksize
	if topo_corr eq 0 then overlap = smmy / 2 else overlap = 2 * smmy 
	calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	ypos1 = 0                       ; block start
	ypos2 = bs - overlap            ; block end

;smooth box

	smm_box  = [1,1,smmx,smmy]
	if topo_corr eq 1 then smm_box2 = [1,1,2*smmx,2*smmy]
	byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos

; pop up progress window

	progress,Message='Coherence estimation (Boxcar)...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   ; loop normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
; -------- THE FILTER ----------
		if file.type eq 300 then begin
			calcflag = 0
			arr1 = block[0,0,*,*]
			arr2 = block[0,1,*,*]
		endif
		if file.type eq 301 or file.type eq 53 or file.type eq 54  or file.type eq 55 then begin
			calcflag = 1
			arr1 = block
		endif
		if file.type eq 302 or file.type eq 52 then begin
			calcflag = 1
			arr1 = exp(complex(0,block))
		endif
		if file.type eq 500 or file.type eq 501 then begin
			calcflag = 0
			arr1 = block[0,*,*,*]
			arr2 = block[1,*,*,*]
		endif
		if file.type ge 510 and file.type lt 520 then begin
			calcflag = 2
			arr1 = block[file.vdim/2:file.vdim-1,0:file.zdim/2-1,*,*]
			arr2 = make_array([file.vdim/2,file.zdim/2,file.xdim,blocksizes[i]],type=4l)
			arr3 = make_array([file.vdim/2,file.zdim/2,file.xdim,blocksizes[i]],type=4l)
			for k=0,file.vdim/2-1 do begin
				for l=0,file.zdim/2-1 do begin
					arr2[k,l,*,*] = abs(block[file.zdim/2+k,file.zdim/2+k,*,*])
					arr3[k,l,*,*] = abs(block[l,l,*,*])
				endfor
			endfor
		endif

; BOXCAR ----------------------------------------

		if topo_corr eq 0 then begin
			case calcflag of
				0: coh = smooth(arr1*conj(arr2),smm_box) / sqrt(smooth(abs(arr1)^2,smm_box) * smooth(abs(arr2)^2,smm_box))
				1: coh = smooth(arr1,smm_box)/sqrt(smooth(abs(arr1)^2,smm_box))
				2: coh = smooth(arr1,smm_box)/sqrt(smooth(arr2,smm_box) * smooth(arr3,smm_box))
			endcase
		endif else begin
			case calcflag of
				0: begin
					c_pha = atan(smooth(exp(complex(0,atan(arr1*conj(arr2),/phase))),smm_box2),/phase)
					coh = smooth(arr1*conj(arr2)*exp(complex(0,-c_pha)),smm_box)/sqrt(smooth(abs(arr1)^2,smm_box) * smooth(abs(arr2)^2,smm_box))
				end
				1: begin
					c_pha = atan(smooth(exp(complex(0,atan(arr1,/phase))),smm_box2),/phase)
					coh = smooth(arr1*exp(complex(0,-c_pha)),smm_box)/sqrt(smooth(abs(arr1)^2,smm_box))
				end
				2: begin
					
					end
			endcase
		endelse

; BOXCAR ----------------------------------------

		if coh_cmpl eq 0 then coh = abs(coh)

; -------- THE FILTER ----------
		if i eq anz_blocks-1 then ypos2 = bs_last

		writeu,eee,coh[*,*,*,ypos1:ypos2-1]
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]

	endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite

	file.name = finalfile
	file.dim  = newdim
	file.zdim = newzdim
	file.vdim = newvdim
	file.var  = newvar
	file.type = newtype

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end

