;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rm_flatearth_geometry
; written by    : Andreas Reigber
; last revision : 
; Flat-earth removal from geometry
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

pro rm_flatearth_geometry,CALLED = called, INPUTFILE1 = inputfile1, INPUTFILE2 = inputfile2
	common rat, types, file, wid, config

	if file.type ne 300 and file.type ne 301 and file.type ne 302 then begin                   
		error = DIALOG_MESSAGE("This is not an interferogram ", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif
;  	
	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Flat-earth removal from geometry',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		text1 = CW_FIELD(main,VALUE=790000.0,/floating,XSIZE=6,TITLE='Platform height  [m] :')
		text2 = CW_FIELD(main,VALUE=500.0000,/floating,XSIZE=6,TITLE='Reference height [m] :')
		text3 = CW_FIELD(main,VALUE=5.400000,/floating,XSIZE=6,TITLE='Range delay     [ms] :')
		text4 = CW_FIELD(main,VALUE=7.80397 ,/floating,XSIZE=6,TITLE='Range spacing    [m] :')
		text5 = CW_FIELD(main,VALUE=150.0000,/floating,XSIZE=6,TITLE='Baseline horiz.  [m] :')
		text6 = CW_FIELD(main,VALUE=0.000000,/floating,XSIZE=6,TITLE='Baseline vert.   [m] :')
		text7 = CW_FIELD(main,VALUE=5.623565,/floating,XSIZE=6,TITLE='Wavelength      [cm] :')

		draw1 = widget_draw(main,XSIZE=300,ysize=200)

		buttons1 = WIDGET_BASE(main,column=2)
		but_plot = WIDGET_BUTTON(buttons1,VALUE=' Plot ',xsize=80)
		but_save = WIDGET_BUTTON(buttons1,VALUE=' Save ',xsize=60)

		buttons2 = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons2,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons2,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons2,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]


		repeat begin                                        ; Event loop
			widget_control,text1,get_value=h0
			widget_control,text2,get_value=href
			widget_control,text3,get_value=rd
			widget_control,text4,get_value=rs
			widget_control,text5,get_value=bh
			widget_control,text6,get_value=bv
			widget_control,text7,get_value=la
			
			c0 = 3e8
			lam = la*1e-2
			re = 6370000.0d + href   ; earth radius + reference height
			rg = rd*1e-3*c0/2+dindgen(file.xdim)*rs
				
			y = (2*re^2+2*re*h0+h0^2-rg^2)/(2*re+2*h0)
			x = sqrt(re^2-y^2)
				
			rg2 = sqrt((x+bh)^2+(y-re-h0+bv)^2)
			fex = -4*!dpi/lam*(rg-rg2)
			fex -= min(fex)

			widget_control,draw1,get_value=index
			wset,index
			plot,rg/1e3,fex/2/!pi,xtitle='range [km]',ytitle='phase cycles'

			widget_control,wid.draw,get_value=index
			wset,index
			event = widget_event(main)
			
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['FLAT-EARTH REMOVAL FROM GEOMETRY V1.0',$
				'this routine accounts for the curvature of the earth',$
				' ',$
				'RAT module written 11/2004 by Andreas Reigber']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
			if event.id eq but_save then begin                  ; Info Button clicked
				path = config.workdir
				inputfile = cw_rat_dialog_pickfile(TITLE='Save flat-earth file', DIALOG_PARENT=wid.base, FILTER = '*.rat', PATH=path, GET_PATH=path)
				if strlen(inputfile) gt 0 then begin
					config.workdir = path
					srat,inputfile,fex,type=390l
				endif				
			endif
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif

; change mousepointer

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

	progress,Message='Removing flat-earth...',/cancel_button

;start block processing

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return


		block = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block

; -------- THE FILTER ----------
		if file.type eq 302 then block = exp(complex(0,block))
		for k=0,blocksizes[i]-1 do block[0,*,k] = block[0,*,k] * exp(complex(0,-fex))
		if file.type eq 302 then block = atan(block,/phase)
; -------- THE FILTER ----------

		writeu,eee,block 
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
