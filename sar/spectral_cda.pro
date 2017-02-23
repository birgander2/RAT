; docformat = 'rst'
;+
; Performs CDA (Complex Dual Apodization) sidelobe suppression / resolution enhancement.
; Needs complex image data as input. Warning: The resulting images don't
; follow the usual SAR image statistics and are mainly for visual
; interpretation!
;
; :Keywords:
;    type: in, optional, type=integer
;       force this RAT type (default: file.type)
;    xmax: in, optional, type=integer
;       centre of the data spectrum in x
;    xbw: in, optional, type=integer
;       bandwidth of the data spectrum in x
;    ymax: in, optional, type=integer
;       centre of the data spectrum in y
;    ybw: in, optional, type=integer
;       bandwidth of the data spectrum in y
;    called: in, optional, type="flag"
;       call routine without GUI in batchmode
;
; :Author: Andreas Reigber
; :Categories: spectral tools, image enhancment
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
PRO spectral_cda, CALLED=called, XMAX=xmax, YMAX=ymax, XBW=xbw, YBW=ybw
   COMMON rat, types, file, wid, config, tiling
   compile_opt idl2
;------------------------------------------------------------------------
; Error handling:
;   -> check if file is a complex SAR image
;------------------------------------------------------------------------

   if file.type ne 101 then begin
      error = dialog_message("SAR complex image is required",dialog_parent=wid.base,title='Error',/error)
      return
   endif
   
;------------------------------------------------------------------------
; - Calculate the spectrum in X and Y direction
;------------------------------------------------------------------------

   progress,Message='Spectral estimation...',/cancel_button
   rrat,file.name,arr
   
   spec_x = fltarr(file.xdim)
   spec_y = fltarr(file.ydim)

   for i=0l,file.xdim-1 do begin
      if i mod 100 eq 0 then progress,percent=i*100.0/(file.xdim+file.ydim),/check_cancel
      arr[i,*] = fft(arr[i,*],-1)
   endfor
   for i=0l,file.ydim-1 do begin
      if i mod 100 eq 0 then progress,percent=(i+file.xdim)*100.0/(file.xdim+file.ydim),/check_cancel 
      arr[*,i] = fft(arr[*,i],-1)
   endfor
   spec_x = total(abs(arr),2)/file.ydim
   spec_y = total(abs(arr),1)/file.xdim
   progress,/destroy

   if not keyword_set(xmax) then foo = max(spec_x,xmax)/10.0
   if not keyword_set(xbw) then begin
      bar = shift(spec_x,-xmax+file.xdim/2)
      xbw = 1
      while bar[file.xdim/2-xbw] gt foo and bar[file.xdim/2+xbw] gt foo and xbw lt file.xdim/2-1 do xbw++
      xbw *= 2
   endif

   if not keyword_set(ymax) then foo = max(spec_y,ymax)/10.0
   if not keyword_set(ybw) then begin
      bar = shift(spec_y,-ymax+file.ydim/2)
      ybw = 1
      while bar[file.ydim/2-ybw] gt foo and bar[file.ydim/2+ybw] gt foo and ybw lt file.ydim/2-1 do ybw++
      ybw *= 2
   endif

;------------------------------------------------------------------------
; - Generate the graphical user interface
;------------------------------------------------------------------------

   if not keyword_set(called) then begin
      main = widget_base(group_leader=wid.base, /column, title='Complex Dual Apodisation',/floating,/tlb_kill_request_events,/tlb_frame_attr)
      
      StepBase = widget_base(main, /row, /frame)
      StepText = widget_label(StepBase,value='STEP 1: Select spectral boundaries in X')
      
      DrawBase = widget_base(main,/column, /frame)
      DrawWin  = widget_draw(DrawBase, xsize=500, ysize=300)
      DrawSub  = widget_base(DrawBase, /row)
      DrawLabel1 = widget_label(DrawSub, value='Select the area of the weighting function: ',/align_left)
;      DrawIO   = cw_bgroup(DrawSub, ['In','Out'],/row, /exclusive, set_value=1)
      DrawSlider1 = widget_slider(DrawBase,title='Select centre of spectrum :'  ,minimum=0,maximum=file.xdim-1,value=xmax)
      DrawSlider2 = widget_slider(DrawBase,title='Select bandwidth of spectrum:',minimum=1,maximum=file.xdim-1,value=xbw)

      buttons = widget_base(main, column=3, /frame)
      but_ok   = widget_button(buttons, value=' Next -> ',xsize=80, /frame)
      but_canc = widget_button(buttons, value=' Cancel ', xsize=60)
                                ;     but_info = widget_button(buttons, value=' Info ',xsize=60)
      
      widget_control, main, /realize, tlb_get_size=toto
      pos = center_box(toto[0],drawysize=toto[1])
      widget_control, main, xoffset=pos[0], yoffset=pos[1]

      widget_control,DrawWin,get_value=index
      wset,index
      
      plot_cdaspec,spec_x,xmax,xbw,file.xdim

      repeat begin
         event = widget_event(main)
         
         widget_control,DrawSlider1,get_value=xmax
         widget_control,DrawSlider2,get_value=xbw

         plot_cdaspec,spec_x,xmax,xbw,file.xdim
         
         if  (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' then begin
            widget_control,main,/destroy
            return
         endif
      endrep until (event.id eq but_ok)
      
      widget_control,but_ok,set_value=' START '
      widget_control,StepText,set_value='STEP 2: Select spectral boundaries in Y'
      widget_control,DrawSlider1,set_value=ymax,set_slider_max=file.ydim-1
      widget_control,DrawSlider2,set_value=ybw,set_slider_max=file.ydim-1
      
      plot_cdaspec,spec_y,ymax,ybw,file.ydim
      
      repeat begin
         event = widget_event(main)

         widget_control,DrawSlider1,get_value=ymax
         widget_control,DrawSlider2,get_value=ybw
         
         plot_cdaspec,spec_y,ymax,ybw,file.ydim    
         
         if  (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' then begin
            widget_control,main,/destroy
            return
         endif
      endrep until (event.id eq but_ok)
      
      widget_control,main,/destroy
      
   endif 

;------------------------------------------------------------------------
; undo function
;------------------------------------------------------------------------

   undo_prepare,outputfile,finalfile,CALLED=CALLED
   
;------------------------------------------------------------------------
; - Transform the arrow mouse pointer into a hourglass
;------------------------------------------------------------------------
   
   widget_control, /hourglass
   
;------------------------------------------------------------------------
; - Here we go !!!
;------------------------------------------------------------------------
   

   mask_x = fltarr(file.xdim)
   mask_x[(file.xdim-xbw)/2:(file.xdim+xbw)/2] = 1.0
   mask_x = shift(mask_x,-file.xdim/2+xmax)
   corr_x = 1/ts_smooth(spec_x,100) * mask_x
   
   
   mask_y = fltarr(file.ydim)
   mask_y[(file.ydim-ybw)/2:(file.ydim+ybw)/2] = 1.0
   mask_y = shift(mask_y,-file.ydim/2+ymax)
   corr_y = 1/ts_smooth(spec_y,100) * mask_y
   
   for i=0,file.xdim-1 do arr[i,*] *= corr_y
   for i=0,file.ydim-1 do arr[*,i] *= corr_x
   arr = shift(temporary(arr),-xmax,-ymax)

   arrh = arr
   
   han_x = fltarr(file.xdim)
   hanning = 1 - cos(2*!pi*findgen(xbw)/xbw)
   han_x[0]= hanning
   han_x   = shift(han_x,-xbw/2)

   han_y = fltarr(file.ydim)
   hanning = 1 - cos(2*!pi*findgen(ybw)/ybw)
   han_y[0]= hanning
   han_y   = shift(han_y,-ybw/2)

   progress,Message='Spectral adjustments...',/cancel_button
   for i=0l,file.xdim-1 do begin
      if i mod 100 eq 0 then progress,percent=i*100.0/(file.xdim+file.ydim),/check_cancel
      arr[i,*] = fft(arr[i,*],+1)
      arrh[i,*] = fft(arrh[i,*]*han_y,+1)
   endfor
   for i=0l,file.ydim-1 do begin
      if i mod 100 eq 0 then progress,percent=(i+file.xdim)*100.0/(file.xdim+file.ydim),/check_cancel 
      arr[*,i] = fft(arr[*,i],+1)
      arrh[*,i] = fft(arrh[*,i]*han_x,+1)
   endfor

   progress,Message='CDA processing...',/cancel_button
   im = fltarr(file.xdim)
   re = fltarr(file.xdim)
   for i=0l,file.ydim-1 do begin
      if i mod 100 eq 0 then progress,percent=i*100.0/(file.ydim)
      c1 = imaginary(arr[*,i])
      c2 = imaginary(arrh[*,i])
      aux = where(c1 ge 0 and c2 ge 0,nr)
      if nr gt 0 then im[aux] = c1[aux] < c2[aux] 
      aux = where(c1 lt 0 and c2 lt 0,nr)
      if nr gt 0 then im[aux] = c1[aux] > c2[aux] 

      c1 = real_part(arr[*,i])
      c2 = real_part(arrh[*,i])
      aux = where(c1 ge 0 and c2 ge 0,nr)
      if nr gt 0 then re[aux] = c1[aux] < c2[aux] 
      aux = where(c1 lt 0 and c2 lt 0,nr)
      if nr gt 0 then re[aux] = c1[aux] > c2[aux] 
      arr[*,i] = complex(re,im)
   endfor

   progress,Message='Saving result...',/cancel_button
   
   srat,outputfile,arr,info=file.info,type=file.type		

; update everything

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute,'CDA processing. Spectral parameters: '+strcompress(xmax)+strcompress(xbw)+strcompress(ymax)+strcompress(ybw)

end

pro plot_cdaspec,spec,sl1,sl2,dim
   
   tek_color
   xx = findgen(dim)

   plot,spec,yrange=[min(spec),max(spec)]
   plots,[sl1,sl1],[0,max(spec)],color=3
   spec_min = (sl1 - sl2/2)
   spec_max = (sl1 + sl2/2)
   case 1 of
      spec_min lt 0: begin
         plots,xx[spec_min+dim:dim-1],spec[spec_min+dim:dim-1],color=2,thick=3
         plots,xx[0:spec_max],spec[0:spec_max],color=2,thick=3
         plots,[spec_min+dim,spec_min+dim],[0,max(spec)/2],color=2
         plots,[spec_max,spec_max],[0,max(spec)/2],color=2
      end
      spec_max ge dim: begin
         plots,xx[spec_min:dim-1],spec[spec_min:dim-1],color=2,thick=3
         plots,xx[0:spec_max mod dim],spec[0:spec_max mod dim],color=2,thick=3
         plots,[spec_max mod dim,spec_max mod dim],[0,max(spec)/2],color=2
         plots,[spec_min,spec_min],[0,max(spec)/2],color=2
      end
      else:begin
         plots,[spec_min,spec_min],[0,max(spec)/2],color=2
         plots,[spec_max,spec_max],[0,max(spec)/2],color=2
         plots,xx[spec_min:spec_max],spec[spec_min:spec_max],color=2,thick=3
      end
   endcase

end

