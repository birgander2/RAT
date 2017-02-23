;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: zoom_region
; written by    : Andreas Reigber (TUB)
; last revision : 14. Mai 2004
; Enlarge region selectable by the mouse
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

pro zoom_region
	common rat, types, file, wid, config

	mousebox,xmin,xmax,ymin,ymax
	
	xmin = round(xmin / wid.draw_scale)
	xmax = round(xmax / wid.draw_scale)
	ymin = round(ymin / wid.draw_scale)
	ymax = round(ymax / wid.draw_scale)

	if ymax-ymin gt xmax-xmin then begin
		xmax = xmin + (ymax-ymin) 
		if xmax gt file.xdim then begin
			sub   = xmax-file.xdim+1
			xmax -= sub
			xmin -= sub
		endif
	endif else begin
		ymax = ymin + (xmax-xmin)
		if ymax gt file.ydim then begin
			sub   = ymax-file.ydim+1
			ymax -= sub
			ymin -= sub
		endif
	endelse
	
	if xmax-xmin le 1 then if xmax lt file.xdim-(2-xmax+xmin) then xmax+=(2-xmax+xmin) else xmin-=(2-xmax+xmin)
	if ymax-ymin le 1 then if ymax lt file.ydim-(2-ymax+ymin) then ymax+=(2-ymax+ymin) else ymin-=(2-ymax+ymin)
	
	case file.type of
		 102: trick=2  ; phase
		 302: trick=2  ; phase
		else: trick=0  ; normal
	endcase
	
	if file.mult eq 1 then begin
		rrat,file.name,inblock,block=[xmin,ymin,xmax-xmin,ymax-ymin]
		outblock = scale_block(inblock,400,400,trick)
		image = float2bytes(outblock)	
   endif else begin
		mfiles  = extract_mtnames(file.name)
		outblock = make_array(file.mult,file.vdim,file.zdim,400,400,type=file.var)
		for i=0,file.mult-1 do begin
			rrat,mfiles[i],inblock,block=[xmin,ymin,xmax-xmin,ymax-ymin]
			outblock[i,*,*,*,*] = scale_block(inblock,400,400,trick)
		endfor
		image = make_array(file.mult,file.vdim,file.zdim,400,400,type=1)
	
		for m=0,file.mult-1 do for i=0,file.vdim-1 do for j=0,file.zdim-1 do image[m,*,*,*,*] = float2bytes(reform(outblock[m,*,*,*,*]))
		image = reform(image)
	endelse

;	outblock = scale_block(inblock,xdim,ydim,trick)

	sz = SIZE(image)
	dim  = sz[0]
	xdim = sz[sz[0]-1]
	ydim = sz[sz[0]]

	zoom_factor = floor(400.0 / xdim) > 1
	zoom_lim_plus = 20
	zoom_lim_moins = zoom_factor

; ---- Generate GUI
 	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Zoom region',/floating,/tlb_kill_request_events,/tlb_frame_attr)
;  	dr1  = WIDGET_DRAW(main,xsize=400,ysize=400,x_scroll_size=380,y_scroll_size=380,/scroll,/button_events,/motion_events,event_pro='cw_rat_draw_ev')
	dr1 = cw_rat_draw(main,400,400,XSCROLL=380,YSCROLL=380)
	
	sub_zoom_button = WIDGET_BASE(main,COLUMN=3)
		but_zoom_p = WIDGET_BUTTON(sub_zoom_button,VALUE=config.imagedir+'zoom_in.bmp',/bitmap)
;		but_zoom_p = WIDGET_BUTTON(sub_zoom_button,VALUE='zoom (+)')
		but_zoom_m = WIDGET_BUTTON(sub_zoom_button,VALUE=config.imagedir+'zoom_out.bmp',/bitmap)
;		but_zoom_m = WIDGET_BUTTON(sub_zoom_button,VALUE='zoom (-)')
		text_zoom = WIDGET_LABEL(sub_zoom_button,VALUE='zoom factor: '+STRCOMPRESS(zoom_factor,/REM)+'x')
  	buttons  = WIDGET_BASE(main,column=2,/frame)
  	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
;  
  	WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
	pos = center_box(toto[0],drawysize=toto[1])
	widget_control, main, xoffset=pos[0], yoffset=pos[1]
	widget_control,dr1,draw_button_events=1, draw_motion_events = 1

	
; ----

	rat_tv,image

; ---- Event loop
	update_flag = 0
	repeat begin
		event = widget_event(main)
		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['ZOOM REGION' ,$
			' ',$
			'RAT module written 08/2003 by Andreas Reigber']
			info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		end
		
		IF event.id EQ but_zoom_p THEN BEGIN
			zoom_factor = zoom_factor + 1 < zoom_lim_plus
			WIDGET_CONTROL,text_zoom,SET_VALUE='zoom factor: '+STRCOMPRESS(zoom_factor,/REM)+'x'
			update_flag = 1
		ENDIF
		IF event.id EQ but_zoom_m THEN BEGIN
			;zoom_factor = (zoom_factor EQ zoom_lim_moins) ? zoom_lim_moins : zoom_factor -1
			zoom_factor = zoom_factor - 1 > zoom_lim_moins
			WIDGET_CONTROL,text_zoom,SET_VALUE='zoom factor: '+STRCOMPRESS(zoom_factor,/REM)+'x'
			update_flag = 1
		ENDIF
		
		if update_flag eq 1 then begin		
			case dim of 
				2: new_image = congrid(image,xdim*zoom_factor,ydim*zoom_factor)
				3: begin
					new_image = make_array(file.zdim,xdim*zoom_factor,ydim*zoom_factor,type=4l)
					for i=0,file.zdim-1 do new_image[i,*,*] = congrid(reform(image[i,*,*]),xdim*zoom_factor,ydim*zoom_factor)
				end
				4: begin
					new_image = make_array(file.vdim,file.zdim,xdim*zoom_factor,ydim*zoom_factor,type=4l)
					for i=0,file.vdim-1 do for j=0,file.zdim-1 do new_image[i,j,*,*] = congrid(reform(image[i,j,*,*]),xdim*zoom_factor,ydim*zoom_factor)
				end
			endcase

			WIDGET_CONTROL,dr1,DRAW_XSIZE=xdim*zoom_factor,DRAW_YSIZE=ydim*zoom_factor 
			WIDGET_CONTROL,dr1,get_value=index
			WSET,index
			rat_tv,new_image
			update_flag = 0
		endif
		
	endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	widget_control,main,/destroy                        ; remove main widget

; switch back to main draw widget

	widget_control,wid.draw,get_value=index
	wset,index


end
