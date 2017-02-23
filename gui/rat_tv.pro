;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; KEYWORD
; - TYPE:     replace the current file.type
; - C_FLAG:   replace the current color_flag
; - SELECT:   replace the current channel_selec
; - DIM:      replace the current file.dim
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

pro rat_tv,arr,SAVE=save,PNG=png,jpg=jpg,TIF=tif,type=type,c_flag=c_flag,select=select,dim=dim,small_preview=small_preview,ARR_SIZE=arr_size
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames
	
	if not keyword_set(type) then begin
		type=file.type
		c_flag = color_flag
	endif

	if not keyword_set(dim) then dim=file.dim
	if not keyword_set(type) then type=file.type
	if not keyword_set(arr_size) then begin
		if file.mult eq 1 then begin
			arr_size = [file.vdim,file.zdim,file.xdim,file.ydim]
		endif else begin
			arr_size = [file.mult,file.vdim*file.zdim,file.xdim,file.ydim]
		endelse
	endif

	if n_elements(select) eq 0 then select = channel_selec

	dim  = (size(arr))[0]
	xdim = (size(arr))[dim-1]
	ydim = (size(arr))[dim]
	if dim ge 3 then zdim = (size(arr))[dim-2] else zdim = 1
	if dim ge 4 then vdim = (size(arr))[dim-3] else vdim = 1

	rgb_flag = 0

	if not keyword_set(small_preview) then begin
		case dim of
	
			2:	image = arr
			3: begin
				arr = reform(arr,zdim,xdim,ydim)
				if c_flag eq 0 then begin
					image = reform(arr[select[0] < (zdim-1),*,*])
				endif else begin
					image = fltarr(3,xdim,ydim)
					if n_elements(select) lt 3 then select = [select,intarr(3-n_elements(select))]
					image[0,*,*] = reform(arr[select[0]<(zdim-1),*,*])
					image[1,*,*] = reform(arr[select[1]<(zdim-1),*,*])
					image[2,*,*] = reform(arr[select[2]<(zdim-1),*,*])
					rgb_flag = 1
				endelse
			end
			4: begin
				arr = reform(arr,vdim,zdim,xdim,ydim)
				if c_flag eq 0 then begin
					image = reform(arr[select[0] / zdim, select[0] mod zdim,*,*])
				endif else begin
					image = fltarr(3,xdim,ydim)
					if n_elements(select) lt 3 then select = [select,intarr(3-n_elements(select))]
					image[0,*,*] = reform(arr[select[0] mod vdim, select[0] / vdim, *,*])
					image[1,*,*] = reform(arr[select[1] mod vdim, select[1] / vdim, *,*])
					if file.zdim ge 3 then image[2,*,*] = reform(arr[select[2] mod vdim, select[2] / vdim, *,*]) $
					else image[2,*,*]=image[0,*,*]
					rgb_flag = 1
				endelse
			end
		endcase
	endif else image = arr
	
; ----
	color_type = 0    ; 0 = indexed b/w,  1 = indexed color,  2 = true color (24bit)
	dim  = size(image)
	case 1 of

; single channel images (possibility of palette colour)

		dim[0] eq 2: begin
			r = reform(palettes[0,*,0])
			g = reform(palettes[0,*,1])
			b = reform(palettes[0,*,2])
			tvlct, r, g, b
			color_type = 1
		end	                           

; multi channel images

		dim[0] eq 3 and dim[1] eq 3: color_type = 2             ; RGB image
		
		else: begin
			dummy = dialog_message(["A stupid error occured in the RAT_TV subroutine.","Trying to continue..."], DIALOG_PARENT = wid.base, TITLE='Error',/error)
		endelse

	endcase

;--------------------------------------------------------------------
; output routines (screen and file)
;--------------------------------------------------------------------

	if not keyword_set(save) then begin
		if color_type lt 2 then tv,image else tv,image,true=1   ; screen output

; from here file output 
	endif else begin                                              
		tvlct,r,g,b,/get
																						
		if keyword_set(PNG) then begin
			if color_type ne 2 then write_png,save,image,r,g,b $
			                   else write_png,save,image
		endif
		
		if keyword_set(JPG) then begin                    
			if color_type eq 0 then write_jpeg,save,image
			if color_type eq 1 then begin                     ; no support for indexed color in jpg, so convert it if necessary
				ra = r[image]
				ga = g[image]
				ba = b[image]
				image = [[[ra]],[[ga]],[[ba]]]
				help,image
				write_jpeg,save,image,true=3
			endif
			if color_type eq 2 then write_jpeg,save,image,true=1
		endif
		
		if keyword_set(TIF) then begin
			if color_type ne 2 then write_tiff,save,image,orientation=0,red=r,green=g,blue=b $
			                   else write_tiff,save,image,orientation=0
		endif

	endelse

	loadct,0,/silent

end
