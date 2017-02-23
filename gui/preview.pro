; docformat = 'rst'
;+
; Wrapper for calculating the floating point preview image of the data, which
; is later on scaled by float2bytes...
;
; :Params:
;    infile : in, required, type="RAT filename"
;       the input data to be previewed
;    outfile : out, optional, type="RAT filename"
;       a file containing the preview image
;
; :Keywords:
;    direct : in, optional, type="named variable"
;       don't write a file, put result in this variable instead. outfile will
;       be ignored if set
;    full : in, optional, type="flag"
;       don't scale the image, produce full resolution preview instead
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
pro preview,infile,outfile,FULL=full,TRICK=trick,TYPE=type,DIRECT=direct
   compile_opt idl2   
   common rat, types, file, wid, config

   head = 1l
   rrat,infile,in,header=head,info=info,type=type		

;---------------------------------
; Scaling tricks
; 0 = take abs()
; 1 = float as it is
; 2 = float phase (but considering wrapping)
; 3 = complex -> phase
;---------------------------------
   case type of
      52:  trick=2              ; phase
      55:  trick=4              ; complex phase
      102: trick=2              ; phase
      235: trick=1
      302: trick=2              ; phase
      303: trick=1              ; unwrapped phase
      320: trick=2              ; phase
      524: trick=2              ; phase
      525: trick=2              ; phase
      else: trick=0             ; normal
   endcase

   if file.dim eq 4 and trick eq 1 then trick = 0

   if not keyword_set(full) then begin ; Caluculate small preview image?
      xprev = wid.base_xsize    ; x-size preview window
      scale = float(xprev) / file.xdim ; scaling factor for preview
      wid.draw_scale = scale
   endif else begin             ; or full resolution ?
      xprev = file.xdim
      scale = 1.0
   endelse

; calculating preview size and number of blocks

   bs = config.blocksize
   calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 

   blocksizes = intarr(anz_blocks)+bs
   blocksizes[anz_blocks-1] = bs_last

   yprev = round(file.ydim * scale)
   scalesizes = intarr(anz_blocks)
   for i=0,anz_blocks-1 do begin
      scalesizes[i] = blocksizes[i] * scale
      if total(scalesizes[0:i]) lt total(blocksizes[0:i]) * scale then scalesizes[i] = scalesizes[i]+1
   endfor
   if scalesizes[anz_blocks-1] eq 0 then anz_blocks -= 1

   if file.zdim gt 3 then dim = 3 else dim = file.zdim

; reading and transforming data blockwise

   if not arg_present(direct) then begin
      if file.dim eq 2 then srat,outfile,out,header=[2l,xprev,yprev,4l],info='preview'
      if file.dim eq 3 then srat,outfile,out,header=[3l,file.zdim,xprev,yprev,4l],info='preview' 		
      if file.dim eq 4 then srat,outfile,out,header=[4l,file.vdim,file.zdim,xprev,yprev,4l],info='preview' 		
   endif else begin
      direct = fltarr(file.vdim,file.zdim,xprev,total(scalesizes))
      opos   = 0l
   endelse

   progress,Message='Calculating preview...'
   for i=0,anz_blocks-1 do begin
      progress,percent=(i+1)*100.0/anz_blocks
      inblock = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
      readu,in,inblock

;;; to test (in some cases it is important!)
      rmnanq,inblock

      if keyword_set(full) then outblock = scale_block(inblock,xprev,scalesizes[i],trick,/full) $
      else outblock = scale_block(inblock,xprev,scalesizes[i],trick)

      if not arg_present(direct) then  writeu,out,outblock else begin
         direct[*,*,*,opos:opos+scalesizes[i]-1]=outblock
         opos += scalesizes[i]
      endelse
   endfor

   if not arg_present(direct) then free_lun,in,out

end
