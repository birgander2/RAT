;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rat_display_small_preview
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
pro rat_display_small_preview,filename,draw_index,text_index,xsize,ysize
   common rat, types, file, wid, config
   common channel, channel_names, channel_selec, color_flag, palettes, pnames

   if filename eq 'default' then begin
      wset,draw_index
      display_default_preview,xsize,ysize
      widget_control,text_index,set_value=''
      widget_control,wid.draw,get_value=index
      wset,index
      return
   endif

   rrat,filename,image,/preview,filetype=filetype

; read palette information
   colour_save = reform(palettes[0,*,*]) ; save actual colour palette

   palettes[0,*,*] = palette_read(FILENAME=filename)
   
;         cfile  = strmid(filename,0,strpos(filename,'.rat'))+'.pal' ; optional palette file existing?
; 	if file_test(cfile) then begin
; 		colour = reform(palettes[0,*,*])
; 		rrat,cfile,colour,type=499
; 		palettes[0,*,*] = colour		; update actual and variable palette
; 	endif else begin
; 		palettes[0,*,0] = bindgen(256)    ; set actual palette to b/w linear
; 		palettes[0,*,1] = bindgen(256)
; 		palettes[0,*,2] = bindgen(256)
; 	endelse
   
; -------
   
   preview_dim = size(image)
   if preview_dim[0] ge 2 then begin

                                ;select channel to diplay
      case preview_dim[0] of
         2: begin
            dim = preview_dim[0]
            arr_size = [1,1,preview_dim[1],preview_dim[2]]
         end
         3: begin
            dim = preview_dim[0]
            arr_size = [1,preview_dim[1],preview_dim[2],preview_dim[3]]
         end
         4: begin
            dim = preview_dim[0]
            arr_size = [preview_dim[1],preview_dim[2],preview_dim[3],preview_dim[4]]
         end
      endcase
      
      channel_default,type=filetype,select=select,dim=dim,arr_size=arr_size,c_flag=c_flag
      
      if preview_dim[0] eq 3 then select <= preview_dim[1]-1
      if preview_dim[0] eq 4 then select <= (preview_dim[1]*preview_dim[2]-1)
      
                                ; congrid the image
      case preview_dim[0] of

         2:	begin
            if filetype ge 400 and filetype le 499 then begin
               new_image = congrid(image,xsize,ysize)
            endif else begin
               new_image = congrid(image,xsize,ysize,cubic=-0.5)
            endelse
         end
         3: begin
            if c_flag eq 0 then begin
               new_image = congrid(reform(image[select[0],*,*]),xsize,ysize,cubic=-0.5)
            endif else begin
               new_image = fltarr(3,xsize,ysize)
               new_image[0,*,*] = congrid(reform(image[select[0],*,*]),xsize,ysize,cubic=-0.5)
               new_image[1,*,*] = congrid(reform(image[select[1],*,*]),xsize,ysize,cubic=-0.5)
               if preview_dim[1] gt 2 then new_image[2,*,*] = congrid(reform(image[select[2],*,*]),xsize,ysize,cubic=-0.5)
            endelse
         end
         4: begin
            if c_flag eq 0 then begin
               new_image = reform(image[select[0] / preview_dim[2], select[0] mod preview_dim[2],*,*])
            endif else begin
               new_image = fltarr(3,xsize,ysize)
               new_image[0,*,*] = congrid(reform(image[select[0] / preview_dim[2], select[0] mod preview_dim[2],*,*]),xsize,ysize,cubic=-0.5)
               new_image[1,*,*] = congrid(reform(image[select[1] / preview_dim[2], select[1] mod preview_dim[2],*,*]),xsize,ysize,cubic=-0.5)
               new_image[2,*,*] = congrid(reform(image[select[2] / preview_dim[2], select[2] mod preview_dim[2],*,*]),xsize,ysize,cubic=-0.5)
            endelse
         end
      endcase
      wset,draw_index
      rat_tv,new_image,type=filetype,c_flag=c_flag,select=[0,1,2],dim=dim,arr_size=arr_size,/small_preview
      widget_control,wid.draw,get_value=index
      wset,index
   endif else begin
      wset,draw_index
      display_default_preview,xsize,ysize
      widget_control,wid.draw,get_value=index
      wset,index
   endelse

   palettes[0,*,*] = colour_save ; restore actual colour palette

   header = 1l
   rrat,filename,in,header=header,info=info,type=type
   free_lun,in

                                ;--> traitement d'info
   if strlen(info) gt 36 then begin
      a = strmid(info,0,36)
      b = strmid(info,36)
      info = [a,b]
   endif
   dim = header[0]
   case dim of
      2 : begin
         dimstr = strcompress(header[1])+' x'+strcompress(header[2])
         type_data = types[header[3]]
      end
      3 : begin
         dimstr = strcompress(header[1])+' x'+strcompress(header[2]) + ' x'+strcompress(header[3])
         type_data = types[header[4]]
      end
      4 : begin
         dimstr = strcompress(header[1])+' x'+strcompress(header[2]) + ' x'+strcompress(header[3])+' x'+strcompress(header[4])
         type_data = types[header[5]]
      end
      else:begin
         text = [$
                  'File info:',$
                  '----------',$
                  '',$
                  'name: '+file_basename(filename),$
                  '',$
                  'This file is not a valid rat data',$
                  'It is maybe a flat earth phase file!!']
         widget_control,text_index,set_value=text
         return
      end
   endcase
   text = ['File info:',$
           '----------',$
           '',$
           'name: '+file_basename(filename),$
           '',$
           info,$
           '',$
           'dimensions: '+dimstr,$
           '['+type_data+']']
   widget_control,text_index,set_value=text
   
end
