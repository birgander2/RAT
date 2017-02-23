; docformat = 'rst'
;+
; Wrapper for generating and displaying the data preview in the main
; window, i.e. it updates the image window in a data-sensitiv way
;
; :Author: RAT Team
; :Categories: Infrastructure
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
pro generate_preview, NODEFAULT = nodefault, COLOR_TABLE=color_table, NOUPDATE=noupdate,RECALCULATE=recalculate, WINDOW_TITLE=window_title ;;; obsolete: REDISPLAY = redisplay
   compile_opt idl2
   common rat, types, file, wid, config
   common channel, channel_names, channel_selec, color_flag
   forward_function extract_mtnames

   if keyword_set(window_title) then begin
      file.window_name = file_basename(file.name)
      widget_control,wid.base,base_set_title='RAT - Radar Tools: '+file.window_name
   endif else begin
      widget_control,wid.base,base_set_title='RAT - Radar Tools: '+file.window_name+ ' (modified)'
   endelse	

   if ~file_test(file.name) then return
;       if ~file_test(file.name,/READ) then return

;;;-----------------------------------
;;;		NO PREVIEW
;;;-----------------------------------
   if ~config.show_preview then begin
      read_png,config.imagedir+'rat.png',image
      isiz = size(image)
      idim = isiz[isiz[0]-1:isiz[0]]
      ipos = 0>fix(([wid.base_xsize,wid.base_ysize]-idim)/2)
;           widget_control,wid.draw, DRAW_YSIZE = idim[1] ; update scroll bar
      widget_control,wid.draw,get_value = win_draw
      wset,win_draw
      erase,190
      if size(image,/n_dim) le 2 then tv,image,ipos[0],ipos[1] else tv,image,ipos[0],ipos[1],true=1
      progress,/destroy
      if ~keyword_set(nodefault) then channel_default
      if not keyword_set(noupdate) then tool_box_update
      return
   endif


;---------------------------------
; Multiple files
;---------------------------------
   if file.mult gt 1 then begin
      mfiles = extract_mtnames(file.name,anz=nfiles)

;;; not necessary to load more than three channels for preview !? (mn)
;                  if file.vdim*file.zdim ge 3 then nfiles=1 $
;                  else nfiles <= 3

      for i=0,nfiles-1 do begin
         rrat,mfiles[i],image,/preview

         dim = size(image)
         if dim[0] ne 0 then begin
            xdim = dim[dim[0]-1]
            if xdim ne wid.base_xsize then recalculate=1
         endif else recalculate=1
         
         if keyword_set(recalculate) then begin
                                ;--> Generate the preview file
            preview,mfiles[i],config.tempdir+config.lookfile,TYPE=mtype
            rrat,config.tempdir+config.lookfile,image
            image = float2bytes(image,type=mtype)
            srat,mfiles[i],image,/preview
            progress,/destroy
         endif

         dim = size(image)
         xdim = dim[dim[0]-1]
         ydim = dim[dim[0]]
         if dim[0] ge 3 then zdim = dim[dim[0]-2] else zdim = 1
         if dim[0] ge 4 then vdim = dim[dim[0]-3] else vdim = 1
         
         if i eq 0 then mimage = bytarr(nfiles*zdim*vdim,xdim,ydim)

         mimage[i*zdim*vdim:(i+1)*zdim*vdim-1,0:xdim-1,0:ydim-1] = image

      endfor
      image = mimage
      srat,file.name,image,/preview
   endif else begin
;---------------------------------
; Single file
;---------------------------------
      rrat,file.name,image,/preview
      dim = size(image)
      if dim[0] ne 0 then begin
         xdim = dim[dim[0]-1]
         if xdim ne wid.base_xsize then recalculate=1
      endif else recalculate=1 
      if keyword_set(recalculate) then begin
                                ;--> Generate the preview file
         preview,file.name,DIRECT=image
         progress,Message='Finalizing preview...'
                                ;--> Read image and transform to byte
;         rrat,config.tempdir+config.lookfile,image
         image = float2bytes(temporary(image),/OVERWRITE)

         dim = size(image)
         xdim = dim[dim[0]-1]
         ydim = dim[dim[0]]
         srat,file.name,image,/preview
         progress,/destroy
      endif
   endelse
;---------------------------------
; Select channels
;---------------------------------

   if not keyword_set(nodefault) then channel_default

;---------------------------------
; Plotting
;---------------------------------

   dim  = size(image)
   xdim = dim[dim[0]-1]
   ydim = dim[dim[0]]

   wid.draw_scale = float(xdim) / file.xdim
   wid.draw_ysize = ydim

   widget_control,wid.draw, DRAW_YSIZE = ydim ; update scroll bar

   if keyword_set(color_table) then rat_tv,image else rat_tv,image
   if not keyword_set(noupdate) then tool_box_update
end
