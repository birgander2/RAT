;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_envi
; last revision : 1.October 2003
; written by    : Andreas Reigber
; Reads data in ENVI standard format (at least in most of the cases)
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



pro open_envi,INPUTFILE = inputfile, SUFFIX=suffix, FSAR=fsar
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

   if not keyword_set(suffix) then suffix = '*.hdr'
   if keyword_set(fsar) then begin
      suffix = '*.hdr'
      title  = 'Open F-SAR file'
   endif else begin
      suffix = '*.*'
      title  = 'Open ENVI file'
   endelse
   
	if not keyword_set(inputfile) then begin
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE=title, $
		DIALOG_PARENT=wid.base, FILTER = suffix, /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

; change mousepointer

		WIDGET_CONTROL,/hourglass

; undo function
      undo_prepare,outputfile,finalfile,CALLED=CALLED
      open_rit,/EMPTY ; no parameters are set: delete the old ones!

; analyse HDR file

;---------------------------------
; detect bin and header filenames
;---------------------------------

      suffix = strmid(inputfile,2,/reverse)
      if suffix eq 'hdr' then begin
         hdrfile = inputfile
         binfile = strmid(inputfile,0,strlen(inputfile)-4)
         if not file_test(binfile) then begin
            print,'ERROR: '+binfile+' not found!'
            return
         endif
      endif else begin
         binfile = inputfile
         hdrfile = inputfile + '.hdr'
         if not file_test(binfile) then begin
            print,'ERROR: '+hdrfile+' not found!'
            return
         endif
      endelse
      rstr  = ''
      openr,ddd,hdrfile,/get_lun
      readf,ddd,rstr
      if rstr ne 'ENVI' then begin
         error = DIALOG_MESSAGE("This is not an ENVI standard file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
         free_lun,ddd
         return
      endif

; set file type

	  whatisthis

;---------------------------------
; analyse header file
;---------------------------------

      hdrline = ''
      openr,lun,hdrfile,/get_lun
      while not eof(lun) do begin
         readf,lun,hdrline
         tags = strtrim(strsplit(hdrline,'=',/extract),2)
         tags[0] = strlowcase(tags[0])
         if tags[0] eq 'samples'       then nrx = fix(tags[1])
         if tags[0] eq 'lines'         then nry = fix(tags[1])
         if tags[0] eq 'bands'         then bands = fix(tags[1])
         if tags[0] eq 'header offset' then offset = fix(tags[1])
         if tags[0] eq 'data type'     then type = fix(tags[1])
         if tags[0] eq 'byte order'    then xdrflag = fix(tags[1])
         if tags[0] eq 'interleave'    then order = tags[1]
         if tags[0] eq 'description'   then begin
            info = hdrline
            while strpos(hdrline,'}') eq -1 do begin
               readf,lun,hdrline
               info += hdrline
            endwhile
            info = strmid(info,strpos(info,'{')+1)
            info = strtrim(strmid(info,0,strpos(info,'}')),2)
         endif
      endwhile
      free_lun,lun

      file.info = info
      file.xdim = nrx
      file.ydim = nry
      file.var  = type
      offset    = offset
      interl    = order
      endian    = xdrflag
      if bands eq 1 then file.dim  = 2l else file.dim = 3l 
      if bands eq 1 then file.zdim = 1l else file.zdim = bands
      file.vdim = 1l

; Byte data? If yes convert to integer

      invar = file.var
      if file.var eq 1 then file.var = 2
   ;
; Read and convert data

      progress,message='Converting...'
      
      if file.dim eq 2 then begin  ; single channel data (no interleave conversion)
         srat,outputfile,eee,header=[2l,file.xdim,file.ydim,file.var],type=file.type
         openr,ddd,binfile,/get_lun
         point_lun,ddd,offset

         for i=0l,file.ydim-1 do begin
            progress,percent=((i+1)*100.)/file.ydim
            block = make_array([file.vdim,file.zdim,file.xdim,1l],type=invar)
            readu,ddd,block
            if endian eq 1 then block = swap_endian(block) else block = block
            if invar eq 1 then block = fix(block)
            writeu,eee,block
         endfor
      endif

      if file.dim eq 3 then begin ; multi channel data
         openr,ddd,binfile,/get_lun
         srat,outputfile,eee,header=[3l,file.zdim,file.xdim,file.ydim,file.var],type=file.type
         point_lun,ddd,offset
         case interl of

; band interleave

            'bsq': begin
               block  = make_array([file.xdim,file.ydim],type=invar)
               oblock = make_array([file.zdim,file.xdim,file.ydim],type=file.var)
               for i=0l,file.zdim-1 do begin
                  progress,percent=((i+1)*100.)/file.zdim
                  readu,ddd,block
                  if endian eq 1 then block = swap_endian(block)
                  oblock[i,*,*] = block
               endfor
               if invar eq 1 then oblock = fix(oblock)
               writeu,eee,oblock
            end

; pixel interleave
				
            'bip': begin
               for i=0l,file.ydim-1 do begin
                  progress,percent=((i+1)*100)/file.ydim
                  block  = make_array([file.zdim,file.xdim],type=invar)
                  readu,ddd,block
                  if endian eq 1 then block = swap_endian(block)
                  if invar eq 1 then block = fix(block)
                  writeu,eee,block
               endfor
            end

; line interleave
            
            'bil': begin
               error = DIALOG_MESSAGE("line interleave not yet supported", DIALOG_PARENT = wid.base, TITLE='Error',/error)
               return
            end
				
            else: begin
               error = DIALOG_MESSAGE("No valid interleave found", DIALOG_PARENT = wid.base, TITLE='Error',/error)
               return
            end
         endcase
      endif
      free_lun,ddd,eee
      file.name = config.tempdir+config.workfile1

; update file generation history (evolution)

      file.window_name = 'Untitled.rat'
      rat_finalise,outputfile,finalfile
      if keyword_set(fsar) then evolute,'Import F-SAR data.' $
                           else evolute,'Import data in ENVI format.'


	endif
end
