;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_convair
; written 08/2005 by Andreas Reigber
; extended 12/2007 by Maxim Neumann
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
; function  readconvair,file,sx,sy
; 	openr,ddd,file,/xdr,/get_lun
; 	arr = complexarr(sx,sy)
; 	readu,ddd,arr
; 	free_lun,ddd
; 	return,arr
; end

;;; channels: [0: quad-pol; 1: single-pol]
pro open_convair,INPUTFILE = inputfile, CALLED=CALLED, channels=channels
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

        if ~(keyword_set(CALLED) && n_elements(inputfile) ne 0 && file_test(inputfile,/READ) && n_elements(channels) ne 0 ) then begin
           main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='CONVAIR import',/floating,/tlb_kill_request_events,/tlb_frame_attr )
           butt = cw_bgroup(main,[' Load quad-pol data set (MPG)',' Load single-pol data set (MGP)'],set_value=0,row=2,/exclusive)		
           buttons  = WIDGET_BASE(main,column=3,/frame)
           but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
           but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
           but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
           WIDGET_CONTROL, main, /REALIZE, default_button = but_canc, tlb_get_size=toto
           pos = center_box(toto[0],drawysize=toto[1])
           widget_control, main, xoffset=pos[0], yoffset=pos[1]

           repeat begin
              event = widget_event(main)
              if event.id eq but_info then begin ; Info Button clicked
                 infotext = ['CONVAIR IMPORT',$
                             ' ',$
                             'RAT module written 08/2005 by Andreas Reigber', $
                             '           extended 12/2007 by Maxim Neumann']
                 info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
              endif
           endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
           widget_control,butt,GET_VALUE=channels
           widget_control,main,/destroy
           if event.id ne but_ok then return ; OK button _not_ clicked

           if not keyword_set(inputfile) then begin ; GUI for file selection
              path = config.workdir
              inputfile = cw_rat_dialog_pickfile(TITLE='Open CONVAIR file', $
                                                 DIALOG_PARENT=wid.base, FILTER = '*.hdr', /MUST_EXIST, PATH=path, GET_PATH=path)
              if strlen(inputfile) gt 0 then config.workdir = path
           endif
        endif else $
           path = file_dirname(inputfile,/mark)

	if strlen(inputfile) gt 0 then begin
           inputfile_hdr=inputfile
; change mousepointer
	
		WIDGET_CONTROL,/hourglass                ; switch mouse cursor

; undo function
                undo_prepare,outputfile,finalfile,CALLED=CALLED
                open_rit,/EMPTY ; no parameters are set: delete the old ones!

; Get all the files
		
		if channels eq 0 then begin
			filex = file_basename(inputfile)
			subfile1 = strsplit(filex,'hhpol',/extract,/regex,count=nr1)
			subfile2 = strsplit(filex,'vvpol',/extract,/regex,count=nr2)
			subfile3 = strsplit(filex,'hvpol',/extract,/regex,count=nr3)
			subfile4 = strsplit(filex,'vhpol',/extract,/regex,count=nr4)
			
			if nr1 eq 2 then filex = subfile1
			if nr2 eq 2 then filex = subfile2
			if nr3 eq 2 then filex = subfile3
			if nr4 eq 2 then filex = subfile4
			
			filex[1] = (strsplit(filex[1],'.',/extract))[0]

			file_hh = path+filex[0]+'hhpol'+filex[1]+'.img'
			file_vv = path+filex[0]+'vvpol'+filex[1]+'.img'
			file_hv = path+filex[0]+'hvpol'+filex[1]+'.img'
			file_vh = path+filex[0]+'vhpol'+filex[1]+'.img'
                        inputfile=[file_hh,file_vv,file_hv,file_vh]
			
			if not (file_test(file_hh) and file_test(file_vv) and file_test(file_hv) and file_test(file_vh)) then begin
				channels = 1
				dummy = DIALOG_MESSAGE(["Not all of the quad-pol files found !","Loading single-pol instead"], DIALOG_PARENT = wid.base, TITLE='Error',/information)
			endif
		endif
		if channels eq 1 then begin
			filex = file_basename(inputfile)
			filex = strsplit(filex,'.',/extract)
			file_xx = path+filex[0]+'.img'
                        inputfile=[file_xx]
		endif

; Analyse info file
                progress,message='Reading CONVAIR header file ...'
		rstr  = ''
		infotext = 'unknown content'
		anz_rg = 0
		anz_az = 0
		openr,ddd,inputfile_hdr,/get_lun
		repeat begin
			readf,ddd,rstr
			rstr = strsplit(rstr,' ',/extract)
			if strcmp(strlowcase(rstr[0]),'mission_id') and (size(rstr))[1] ge 2 then infotext = rstr[1]
			if strcmp(strlowcase(rstr[0]),'number_lines')   then ny = floor(float(strcompress(rstr[1],/remove)))
			if strcmp(strlowcase(rstr[0]),'number_samples') then nx = floor(float(strcompress(rstr[1],/remove)))
			rstr  = ''
		endrep until eof(ddd)
		free_lun,ddd

; Load all the files

                nrch   = channels eq 0? 4: 1
                ddd    = lonarr(nrch)
                for ch=0,nrch-1 do begin
                   get_lun,tmp
                   ddd[ch] = tmp
                   openr,ddd[ch],inputfile[ch],/xdr ; open input file
;                   readu,ddd[ch],header   ;; no header in .img files?
                endfor

                newtype=channels eq 0? 200L: 101L
                newdims=channels eq 0? [3L,4L,nx,ny,6L]: [2L,nx,ny,6L]
                nrx = nx
                nry = ny
                info = infotext
                outputfile=config.tempdir+config.workfile1
                srat,outputfile,eee,header=newdims,type=newtype,info=info ; write RAT file header

                bs_y  = config.blocksize/nrch ; get standard RAT blocksize
                block    = complexarr(     nrx,bs_y,/nozero) ; define block
                blockALL = complexarr(nrch,nrx,bs_y,/nozero)
                nr_y  = nry  /  bs_y ; calc nr. of blocks
                re_y  = nry mod bs_y ; calc size of last block
  
                progress,message='Reading CONVAIR data file...'
  
                for i=0,nr_y-1 do begin ; read and write blocks
                   progress,percent=((i+1)*100)/nr_y ; display progress bar
                   for ch=0,nrch-1 do begin
                      readu,ddd[ch],block
                      blockALL[ch,*,*]=block
                   endfor
                   writeu,eee,blockALL
                endfor
                if re_y gt 0 then begin ; read and write last block
                   block   = complexarr(     nrx,re_y,/nozero)
                   blockALL= complexarr(nrch,nrx,re_y,/nozero)
                   for ch=0,nrch-1 do begin
                      readu,ddd[ch],block
                      blockALL[ch,*,*] = block
                   endfor
                   writeu,eee,blockALL
                endif
                for ch=0,nrch-1 do $
                   free_lun,ddd[ch]
                free_lun,eee    ; close all files

;                 if channels eq 0 then begin
; 			dummy = transpose(readconvair(file_hh,nx,ny))
; 			siz   = size(dummy)
; 			anz_rg = siz[1]
; 			anz_az = siz[2]
; 			arr = complexarr(4,anz_rg,anz_az)
; 			arr[0,*,*] = dummy
; 			arr[1,*,*] = transpose(readconvair(file_vv,nx,ny))
; 			arr[2,*,*] = transpose(readconvair(file_hv,nx,ny))
; 			arr[3,*,*] = transpose(readconvair(file_vh,nx,ny))
; 			t = 200l
; 		endif else begin
; 			arr = transpose(readconvair(file_xx,nx,ny))
; 			siz   = size(arr)
; 			anz_rg = siz[1]
; 			anz_az = siz[2]
; 			t = 101l
; 		endelse		
; 		srat,config.tempdir+config.workfile1,arr,type=t

; set internal variables of RAT
	
		file.name = config.tempdir+config.workfile1
		file.info = infotext                    ; Put here a string describing your data
		file.type = newtype                         ; data type (101 = single channel complex)
		file.dim  = 3l                           ; single channel data (two-dimensional array)
		file.xdim = nrx                       ; range image size
		file.ydim = nry                       ; azimuth image size
		file.zdim = 4l                           ; nr. of layers (set to 1 if not needed)
		file.vdim = 1l                           ; nr. of layers of layers (set to 1 if not needed)
		file.var  = 6L                           ; IDL variable type (6 = complex, 4 = floating point)
 		
		if channels eq 1 then begin
			file.dim  = 2l                           ; single channel data (two-dimensional array)
			file.zdim = 1l                           ; nr. of layers (set to 1 if not needed)
		endif

; update file generation history (evolution)
                evolute,'Import SAR data from CONVAIR.'

; read palette information
	
		palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
		palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

		file.window_name = 'Untitled.rat'
		generate_preview
		update_info_box
	
	endif	
end

