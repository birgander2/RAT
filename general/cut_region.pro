; docformat = 'rst'
;+
; Cut out/Crop a rectangular part of the currently loaded image
;
; :Keywords:
;    xmin: in, optional, type=integer
;       the horizontal start of the region to cut out
;    xmax: in, optional, type=integer
;       the horizontal end of the region to cut out
;    ymin: in, optional, type=integer
;       the vertical start of the region to cut out
;    ymax: in, optional, type=integer
;       the vertical end of the region to cut out
;    called: in, optional, type=flag
;       run in batch mode without gui
;       
;
; :Author: RAT team
; :Categories: general
;
; :Copyright:
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
;-
pro cut_region, CALLED=CALLED, XMIN=xminf, XMAX=xmaxf, YMIN=yminf, YMAX=ymaxf

   common rat, types, file, wid, config, tiling
   compile_opt idl2

   if n_elements(xminf) eq 0 then xminf=0
   if n_elements(xmaxf) eq 0 then xmaxf=file.xdim-1
   if n_elements(yminf) eq 0 then yminf=0
   if n_elements(ymaxf) eq 0 then ymaxf=file.ydim-1


   if not keyword_set(CALLED) then begin 

;------------------------------------------------------------------------
; - set the input airborne parameter file name to an empty string
;------------------------------------------------------------------------
     inputfile = ''
     select_param_file = 0
     
;------------------------------------------------------------------------
; - Create the insar structure
;------------------------------------------------------------------------
     insar = {$
             polarisation_channel         : "", $
             master_track_name            : "", $
             slave_track_name             : "", $
             info                         : "", $
             wavelength                   : 0.d,$
             chirp_bandwidth              : 0.d,$
             range_sampling               : 0.d,$
             range_sampling_rate          : 0.d,$
             range_delay                  : 0.d,$
             terrain_elevation            : 0.d,$
             speed_of_light               : 2.99708e+08,$
             xdim_or                      : 0l,$ ;original size of data
             ydim_or                      : 0l,$
             xdim                         : 0l,$ ;current size of data
             ydim                         : 0l,$ 
             xmin                         : 0l,$
             xmax                         : 0l,$
             ymin                         : 0l,$
             ymax                         : 0l,$
             range_baseline               : 0.d,$
             height_baseline              : 0.d,$
             altitude_above_ground_master : 0.d,$
             altitude_above_ground_slave  : 0.d,$
             range_bin_first_master       : 0l,$
             range_bin_first_slave        : 0l,$
             baseline                     : 0.d,$
             alpha                        : 0.d$
     }


; ---- draw box

     mousebox,xmin,xmax,ymin,ymax
     
     xminf = floor(xmin / wid.draw_scale)
     xmaxf = floor(xmax / wid.draw_scale)
     yminf = floor(ymin / wid.draw_scale)
     ymaxf = floor(ymax / wid.draw_scale)

; ---- save Box content
;  	
;  	difx = xmax - xmin + 2
;  	dify = ymax - ymin + 2
;  	savim = tvrd(xmin-1,ymin-1,difx,dify,true=1)
;  	plots,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/device,color=255
     
     difx = xmax - xmin  + 1
     dify = ymax - ymin  + 1
     savim = tvrd(xmin,ymin,difx,dify,true=1)
     plots,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/device,color=255

; ---- generate GUI

     difxf = xmaxf - xminf + 1
     difyf = ymaxf - yminf + 1

     
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Cut region',/modal)
     sub  = WIDGET_BASE(main,column=2,/frame)
     sub1  = WIDGET_BASE(sub,row=4,/frame)
     sub2  = WIDGET_BASE(sub,row=4,/frame)
     field1   = CW_FIELD(sub1,VALUE=xminf,/integer,TITLE='X start  : ',XSIZE=5,/return_events)
     field3   = CW_FIELD(sub1,VALUE=xmaxf,/integer,TITLE='X end    : ',XSIZE=5,/return_events)
     field2   = CW_FIELD(sub1,VALUE=yminf,/integer,TITLE='Y start  : ',XSIZE=5,/return_events)
     field4   = CW_FIELD(sub1,VALUE=ymaxf,/integer,TITLE='Y end    : ',XSIZE=5,/return_events)
     field5   = CW_FIELD(sub2,VALUE=difxf,/integer,TITLE='X size   : ',XSIZE=5,/return_events)
     field6   = CW_FIELD(sub2,VALUE=difyf,/integer,TITLE='Y size   : ',XSIZE=5,/return_events)

     button_param_file = CW_BGROUP(main,['Update airborne system file'],ypad=5,/row,/nonexclusive)
;	sub4  = WIDGET_BASE(sub3,row=4,/frame)
;	update   = WIDGET_BUTTON(sub4,VALUE=' Update box ',/frame)

;  	sub_input_param_file = widget_base(main,column=3)
;  		button_param_file = CW_BGROUP(sub_input_param_file,['Update airborne system file'],ypad=5,/row,/nonexclusive)
;  		text_param_file = cw_field(sub_input_param_file,value=inputfile,/string,xsize=60, title='')
;  		brow_param_file = widget_button(sub_input_param_file,value='browse',ysize=35)

     buttons  = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

     WIDGET_CONTROL, main, /REALIZE, tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     
;------------------------------------------------------------------------
; - by default airborne system file is not active
;------------------------------------------------------------------------
;  	widget_control,text_param_file,sensitive=select_param_file
;  	widget_control,brow_param_file,sensitive=select_param_file
     

     repeat begin
        event = widget_event(main, bad_id = closed)
        
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['CUT OUT REGION V1.2' ,$
                       ' ',$
                       'RAT module written 08/2003 by Andreas Reigber']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif

        if event.id eq field1 or event.id eq  field2 or event.id eq field3 or event.id eq field4 then begin ; Info Button clicked
           widget_control,field1,GET_VALUE=xminf
           widget_control,field2,GET_VALUE=yminf
           widget_control,field3,GET_VALUE=xmaxf
           widget_control,field4,GET_VALUE=ymaxf
           
           if xminf lt 0 or xminf ge file.xdim then xminf = 0
           if xmaxf lt 0 or xmaxf ge file.xdim then xmaxf = file.xdim - 1
           if yminf lt 0 or yminf ge file.ydim then yminf = 0
           if ymaxf lt 0 or ymaxf ge file.ydim then ymaxf = file.ydim - 1
           
           difxf = xmaxf - xminf + 1
           difyf = ymaxf - yminf + 1

           widget_control,field1,SET_VALUE=xminf
           widget_control,field2,SET_VALUE=yminf
           widget_control,field3,SET_VALUE=xmaxf
           widget_control,field4,SET_VALUE=ymaxf
           widget_control,field5,SET_VALUE=difxf
           widget_control,field6,SET_VALUE=difyf
        endif
        if event.id eq field5 or event.id eq field6 then begin              
           widget_control,field1,GET_VALUE=xminf
           widget_control,field2,GET_VALUE=yminf
           widget_control,field5,GET_VALUE=difxf
           widget_control,field6,GET_VALUE=difyf

           xmaxf = xminf + difxf - 1
           ymaxf = yminf + difyf - 1
           
           if xmaxf ge file.xdim then begin
              xmaxf = file.xdim - 1
              difxf = xmaxf - xminf + 1
           endif
           if ymaxf ge file.ydim then begin
              ymaxf = file.ydim - 1 
              difyf = ymaxf - yminf + 1
           endif
           
           widget_control,field3,SET_VALUE=xmaxf
           widget_control,field4,SET_VALUE=ymaxf
           widget_control,field5,SET_VALUE=difxf
           widget_control,field6,SET_VALUE=difyf		
        endif

; Draw new white box
        geo = widget_info(wid.draw,/geometry)

        tv,savim,xmin,ymin,true=1
        xmin = floor(xminf * wid.draw_scale) > 0
        xmax = floor(xmaxf * wid.draw_scale) < geo.draw_xsize-1
        ymin = floor(yminf * wid.draw_scale) > 0
        ymax = floor(ymaxf * wid.draw_scale) < geo.draw_ysize-1
        difx = xmax - xmin + 1
        dify = ymax - ymin + 1

        savim = tvrd(xmin,ymin,difx,dify,true=1)
        plots,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/device,color=255

     endrep until (event.id eq but_ok) or (event.id eq but_canc) or closed 

; Delete white box

     tv,savim,xmin,ymin,true=1  ; remove white box

     IF event.id NE but_ok then BEGIN
        widget_control,main,/destroy
        return                  ; OK button _not_ clicked
     ENDIF


;------------------------------------------------------------------------
; - Test about the airborne system file
;------------------------------------------------------------------------
; Ready with selecting region
     widget_control,button_param_file,GET_VALUE=update_system
     widget_control,field1,GET_VALUE=xminf
     widget_control,field2,GET_VALUE=yminf
     widget_control,field3,GET_VALUE=xmaxf
     widget_control,field4,GET_VALUE=ymaxf

     if xminf lt 0 or xminf ge file.xdim then xminf = 0
     if xmaxf lt 0 or xmaxf ge file.xdim then xmaxf = file.xdim - 1
     if yminf lt 0 or yminf ge file.ydim then yminf = 0
     if ymaxf lt 0 or ymaxf ge file.ydim then ymaxf = file.ydim - 1

     if not closed then widget_control,main,/destroy ; remove main widget

  endif

;------------------------------------------------------------------------
; - Transform the arrow mouse pointer into a hourglass
;------------------------------------------------------------------------
  
   widget_control, /hourglass
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

   rrat,file.name, ddd,header=head,info=info,type=type		
   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type
   head[head[0]-1] = xmaxf-xminf+1	
   head[head[0]]   = ymaxf-yminf+1
   byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos
   point_lun,-ddd,file_pos
   point_lun,ddd,file_pos + long64(yminf) * file.vdim * file.zdim * file.xdim * byt[file.var]
   srat,outputfile,eee,header=head,info=file.info,type=file.type

; calculating preview size and number of blocks

  bs = config.blocksize
  calc_blocks_normal,ymaxf-yminf+1,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

  progress,Message='Cutting...',/cancel_button
  for i=0,anz_blocks-1 do begin ; normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block
     block = block[*,*,xminf:xmaxf,*] 
     writeu,eee,block
  endfor
  free_lun,ddd,eee

; update everything

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute,'Cut out region: ['+strcompress(xminf,/r)+':'+strcompress(xmaxf,/r)+';'+strcompress(yminf,/r)+':'+strcompress(ymaxf,/r)+']'

end
