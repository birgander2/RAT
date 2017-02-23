; docformat = 'rst'
;+
; Removes range antenna patterns through simple profiling
;
; :Keywords:
;    boxsize: in, optional, type=integer
;       default box size for filtering the profiles
;
; :Author: Andreas Reigber
; :Categories: SAR, antenna correction
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
pro amp_rgant, CALLED = called, BOXSIZE = boxsize
   common rat, types, file, wid, config, tiling
   compile_opt idl2

   if not keyword_set(boxsize) then boxsize = file.xdim / 8 ; Default values
   
; Error handling

   if boxsize lt 3 then begin   ; Wrong box size ?
      error = DIALOG_MESSAGE("Boxsize has to be >= 3", DIALOG_PARENT = wid.base, TITLE='Error',/error)
      return
   endif

; change mousepointer

   WIDGET_CONTROL,/hourglass

; read / write header

   head = 1l
   rrat,file.name, ddd,header=head,info=info,type=type          
   
; Get profiles
   
   profile = fltarr(file.zdim,file.xdim)
   progress,Message='Estimating pattern...',/cancel_button
   for i=0l,file.ydim-1 do begin
      progress,percent=(i+1)*100.0/file.ydim,/check_cancel
      block = make_array(file.vdim,file.zdim,file.xdim,type=file.var)
      readu,ddd,block
      if file.vdim eq 1 then profile += abs(block) / file.ydim else for j=0,file.vdim-1 do profile[j,*] += sqrt(abs(block[j,j,*])) / file.ydim
   endfor
   free_lun,ddd

   
; Smooth profiles
   
   for i=0,file.zdim-1 do profile[i,*] /= mean(profile[i,*])
   sm_profile = profile
   for i=0,file.zdim-1 do sm_profile[i,*] = ts_smooth(reform(profile[i,*]),boxsize)
   progress,/destroy
   
; GUI
   
   if not keyword_set(called) then begin ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Antenna pattern',/floating,/tlb_kill_request_events,/tlb_frame_attr)
      dr1  = widget_draw(main,XSIZE=800,ysize=500)
      field1 = CW_FIELD(main,VALUE=boxsize,/integer,  TITLE='Filter boxsize        : ',XSIZE=5)

      buttons      = WIDGET_BASE(main,column=4,/frame)
      but_ok       = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
      but_smooth   = WIDGET_BUTTON(buttons,VALUE=' Smooth ',xsize=80,/frame)
      but_canc     = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
      but_info     = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
      WIDGET_CONTROL, main, /REALIZE,tlb_get_size=toto

      tek_color
      plot,profile[0,*]
      for i=0,file.zdim-1 do oplot,profile[i,*],color=i+2
      for i=0,file.zdim-1 do oplot,sm_profile[i,*],color=i+2,thick=2
      loadct,0,/silent
 
      repeat begin              ; Event loop
         event = widget_event(main)
         
         if event.id eq but_smooth then begin
            widget_control,/hourglass
            widget_control,field1,GET_VALUE=boxsize
            
            sm_profile = profile
            for i=0,file.zdim-1 do sm_profile[i,*] = ts_smooth(reform(profile[i,*]),boxsize)
            tek_color
            plot,profile[0,*]
            for i=0,file.zdim-1 do oplot,profile[i,*],color=i+2
            for i=0,file.zdim-1 do oplot,sm_profile[i,*],color=i+2,thick=2
            loadct,0,/silent
         endif
         if event.id eq but_info then begin ; Info Button clicked
            infotext = ['ANTENNA PATTERN REMOVAL',$
                        ' ',$
                        'RAT module written 11/2008 by Andreas Reigber',$
                        'It just performs a vertical profiling of the amplitude', $
                        'and corrects for this pattern. VERY BASIC APPROACH!']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
         end
        
      endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
      widget_control,main,/destroy ; remove main widget
      widget_control,wid.draw,get_value=index
      wset,index
      if event.id ne but_ok then return
   endif

; Remove profiles
   
   widget_control,/hourglass

; undo function
   
   undo_prepare,outputfile,finalfile,CALLED=CALLED
   
   head = 1l
   rrat,file.name, ddd,header=head,info=info,type=type          
   srat,outputfile,eee,header=head,info=info,type=type          
   
   progress,Message='Removing pattern...',/cancel_button
   for i=0l,file.ydim-1 do begin
      progress,percent=(i+1)*100.0/file.ydim,/check_cancel
      block = make_array(file.vdim,file.zdim,file.xdim,type=file.var)
      readu,ddd,block
      if file.vdim eq 1 then for j=0,file.zdim-1 do block[0,j,*] /= sm_profile[j,*]
      if file.vdim gt 1 then for k=0,file.vdim-1 do for l=0,file.zdim-1 do block[k,l,*] = block[k,l,*] / sm_profile[k,*] / sm_profile[l,*] 
      writeu,eee,block
   endfor

   free_lun,ddd,eee

; update everything

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute,'Removed range antenna pattern. Boxsize: '+strcompress(boxsize,/R)

end
















