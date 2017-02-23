;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; dialog_resize.pro
; written by    : Maxim Neumann
; last revision : 08/2006
; Dialog for getting resize parameters
; Input		: xdim, ydim, title
; Output	: change={0,1}, resize={0,1}
; 		  result = [ [xstart,xend,xdim], [ystart,yend,ydim] ]
; 		         = [ {start,end,dim}; {x,y} ]
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

function dialog_resize, XDIM=XDIM, YDIM=YDIM, TITLE=TITLE, $
                        CHANGE=CHANGE, RESIZE=RESIZE, X_NEW=x, Y_NEW=y
  common rat

  if n_elements(XDIM)  eq 0 then xdim  = file.xdim
  if n_elements(YDIM)  eq 0 then ydim  = file.ydim
  if n_elements(TITLE) eq 0 then title = 'Change x/y dimensions'
  x=[0,xdim-1,xdim]
  y=[0,ydim-1,ydim]
  xstart = x[0]
  xend   = x[1]
  ystart = y[0]
  yend   = y[1]
  change = 0
  resize=0

  main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE=title,/floating,/tlb_kill_request_events,/tlb_frame_attr,/KBRD_FOCUS_EVENTS)

  f_xstart = cw_field(main,title='New x-start       (0) : ',/int,value=xstart,XSIZE=6,/RETURN_EVENTS)
  f_xend   = cw_field(main,title='New x-end   '+string('('+strcompress(xdim-1,/R)+')',f='(a8)')+' : ',/int,value=xend,XSIZE=6,/RETURN_EVENTS)
  f_ystart = cw_field(main,title='New y-start       (0) : ',/int,value=ystart,XSIZE=6,/RETURN_EVENTS)
  f_yend   = cw_field(main,title='New y-end   '+string('('+strcompress(ydim-1,/R)+')',f='(a8)')+' : ',/int,value=yend,XSIZE=6,/RETURN_EVENTS)
  text     = widget_label(main,/FRAME,value="Size of the new image without resizing the new area: "+ $
                          strcompress(xend-xstart+1,/R)+' x '+strcompress(yend-ystart+1,/R)+'          ')
  b_resize  = cw_bgroup(main,['Resize the new area'],set_value=0,/nonexcl)
  f_xdim   = cw_field(main,title='New x-dim             : ',/int,value=xdim,XSIZE=6,/RETURN_EVENTS)
  f_ydim   = cw_field(main,title='New y-dim             : ',/int,value=ydim,XSIZE=6,/RETURN_EVENTS)

  buttons  = WIDGET_BASE(main,column=4,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

  WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
;	     WIDGET_CONTROL, main, /REALIZE,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=pos[0], yoffset=pos[1]
  if ~resize then begin
     widget_control,f_xdim,SENSITIVE=0,set_value=xend-xstart+1
     widget_control,f_ydim,SENSITIVE=0,set_value=yend-ystart+1
  endif

  repeat begin                  ; Event loop
     event = widget_event(main)
     widget_control,f_xstart,get_val=xstart & xstart >= 0
     widget_control,f_xend,get_val=xend & xend <= xdim-1
     widget_control,f_xend,set_val=xend
     widget_control,f_ystart,get_val=ystart & ystart >= 0
     widget_control,f_yend,get_val=yend & yend <= ydim-1
     widget_control,f_yend,set_val=yend
     widget_control, text,set_val="Size of the new image without resizing the new area: "+strcompress(xend-xstart+1,/R)+' x '+strcompress(yend-ystart+1,/R)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['Generic resize/resample dialog',$
                    ' ',$
                    'RAT module written 2006 by Maxim Neumann',' ', $
                    'In case you change x-dim or y-dim from the default, the new array will be resampled']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
     endif
     if event.id eq b_resize then begin
        widget_control,b_resize,get_value=resize
        if resize then begin
           widget_control,f_xdim,/SENSITIVE,set_value=xend-xstart+1
           widget_control,f_ydim,/SENSITIVE,set_value=yend-ystart+1
        endif else begin
           widget_control,f_xdim,SENSITIVE=0,set_value=xend-xstart+1
           widget_control,f_ydim,SENSITIVE=0,set_value=yend-ystart+1
        endelse
     endif
  endrep until (event.id eq but_ok) || (event.id eq but_canc) || tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  if event.id eq but_ok then begin
     widget_control,f_xstart,get_value=tmp & x[0]=tmp>0
     widget_control,f_xend  ,get_value=tmp & x[1]=tmp<(xdim-1)
     widget_control,f_ystart,get_value=tmp & y[0]=tmp>0
     widget_control,f_yend  ,get_value=tmp & y[1]=tmp<(ydim-1)
     if resize then begin
        widget_control,f_xdim  ,get_value=tmp & x[2]=tmp
        widget_control,f_ydim  ,get_value=tmp & y[2]=tmp
        if x[2] eq x[1]-x[0]+1 && y[2] eq y[1]-y[0]+1 then resize=0
     endif else begin
        x[2]=x[1]-x[0]+1
        y[2]=y[1]-y[0]+1
     endelse
     if ~array_equal([x,y],[0,xdim-1,xdim,0,ydim-1,ydim]) then change=1
  endif else begin
     x=[0,xdim-1,xdim]
     y=[0,ydim-1,ydim]
  endelse
  WIDGET_CONTROL, /DESTROY, main

  return, [[x],[y]]
end
