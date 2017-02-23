;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_polsarpro
; VERSION 2.0
; last revision : 06. November 2004
; written by    : Andreas Reigber
; Reads data in POLSARPRO format           
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


;;; for the moment batch call only for data in vector or matrix format!
pro open_polsarpro, CALLED=CALLED, CONFIG_FILE=CONFIG_FILE
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag, palettes, pnames
  compile_opt idl2

  if n_elements(CONFIG_FILE) ne 0 then begin
     inputfile = CONFIG_FILE
     path = file_dirname(inputfile, /M)
     channels = 1
  endif

  if ~keyword_set(CALLED) then begin
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='POLSAR import',/floating,/tlb_kill_request_events,/tlb_frame_attr )
     butt = cw_bgroup(main,[' Load single channel data set ',' Load data in vector or matrix format'],set_value=1,row=2,/exclusive)		
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
           infotext = ['POLSARPRO IMPORT',$
                       ' ',$
                       'RAT module written 11/2004 by Andreas Reigber']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,butt,GET_VALUE=channels
     widget_control,main,/destroy
     if event.id ne but_ok then return ; OK button _not_ clicked

     path = config.workdir
     inputfile = cw_rat_dialog_pickfile(TITLE='Select POLSARPRO config.txt file', $
                                        DIALOG_PARENT=wid.base, /MUST_EXIST, FILTER = '*.txt', PATH=path, GET_PATH=path)
  endif
  if strlen(inputfile) gt 0 then config.workdir = path
  
  if strlen(inputfile) gt 0 then begin

; change mousepointer
     
     WIDGET_CONTROL,/hourglass
; undo function
     undo_prepare,outputfile,finalfile,CALLED=CALLED
     open_rit,/EMPTY            ; no parameters are set: delete the old ones!

; analyse file structures
     
     rstr  = ''
     openr,ddd,inputfile,/get_lun
     repeat begin
        readf,ddd,rstr
        case strlowcase(rstr) of
           'nrow':  begin
              readf,ddd,rstr
              file.ydim = strcompress(rstr,/remove)
           end
           'nlig':  begin
              readf,ddd,rstr
              file.ydim = strcompress(rstr,/remove)
           end
           'ncol':  begin
              readf,ddd,rstr
              file.xdim = strcompress(rstr,/remove)
           end
           'polartype':  begin
              readf,ddd,rstr
              if rstr ne 'full' then begin
                 error = DIALOG_MESSAGE("Partial polarimetric mode not supported up to now", DIALOG_PARENT = wid.base, TITLE='Error',/error)
                 return
              endif
           end
           else: rstr = ''
        endcase
     endrep until eof(ddd)
     free_lun,ddd

                                ; calculating preview size and number of blocks
     
     bs = config.blocksize
     calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
     blocksizes = intarr(anz_blocks)+bs
     blocksizes[anz_blocks-1] = bs_last
     
     spstr = strsplit(path,path_sep(),/extract)
     direc = spstr[(size(spstr))[1]-1]
     
     if channels eq 1 then begin
        
        file.dim = 4
        file.var = 6
        case direc of
           'C3': begin
              file.vdim = 3
              file.zdim = 3
              file.type = 220l
           end
           'T3': begin
              file.vdim = 3
              file.zdim = 3
              file.type = 221l
           end
           'C4': begin
              file.vdim = 4
              file.zdim = 4
              file.type = 220l
           end
           'T4': begin
              file.vdim = 4
              file.zdim = 4
              file.type = 221l
           end
           else: begin          ; Sinclair
              file.vdim = 1
              file.zdim = 4
              file.type = 200l
              file.dim  = 3
           end
        endcase
        
; Read and convert data
        
        head = [file.dim,file.zdim,file.zdim,file.xdim,file.ydim,file.var]
        if file.dim eq 3 then head = [head[0],head[2:*]]
        srat,config.tempdir+config.workfile1,eee,header=head,type=file.type

; --------------------------		
; SCATTERING VECTOR
; --------------------------		

        if file.type eq 200 then begin
           file1 = path+'s11.bin'
           file2 = path+'s22.bin'
           file3 = path+'s12.bin'
           file4 = path+'s21.bin'
           if (file_test(file1)+file_test(file2)+file_test(file3)+file_test(file4)) ne 4 then begin
              error = DIALOG_MESSAGE("Some files are missing", DIALOG_PARENT = wid.base, TITLE='Error',/error)
              return
           endif
           
           openr,ddd1,file1,/get_lun
           openr,ddd2,file2,/get_lun
           openr,ddd3,file3,/get_lun
           openr,ddd4,file4,/get_lun
           
           for i=0,anz_blocks-1 do begin
              oblock = make_array([file.zdim,file.xdim,blocksizes[i]],type=file.var)
              block  = make_array([file.xdim,blocksizes[i]],type=file.var)
              readu,ddd1,block
              oblock[0,*,*] = block
              readu,ddd2,block
              oblock[1,*,*] = block
              readu,ddd3,block
              oblock[2,*,*] = block
              readu,ddd4,block
              oblock[3,*,*] = block
              writeu,eee,oblock
           endfor
           free_lun,ddd1,ddd2,ddd3,ddd4,eee
        endif
        
; --------------------------		
; COVARIANCE & COHERENCY MATRIX
; --------------------------		

        if file.type eq 220 or file.type eq 221 then begin                              
           
           if file.type eq 220 then begin
              file1 = path+'C11.bin'
              file2 = path+'C22.bin'
              file3 = path+'C33.bin'
              file4 = path+'C12_real.bin'
              file5 = path+'C12_imag.bin'
              file6 = path+'C13_real.bin'
              file7 = path+'C13_imag.bin'
              file8 = path+'C23_real.bin'
              file9 = path+'C23_imag.bin'
              
              if file.zdim eq 4 then begin
                 file10 = path+'C14_real.bin' 
                 file11 = path+'C14_imag.bin' 
                 file12 = path+'C24_real.bin' 
                 file13 = path+'C24_imag.bin' 
                 file14 = path+'C34_real.bin' 
                 file15 = path+'C34_imag.bin' 
                 file16 = path+'C44.bin' 
              endif
           endif else begin
              file1 = path+'T11.bin'
              file2 = path+'T22.bin'
              file3 = path+'T33.bin'
              file4 = path+'T12_real.bin'
              file5 = path+'T12_imag.bin'
              file6 = path+'T13_real.bin'
              file7 = path+'T13_imag.bin'
              file8 = path+'T23_real.bin'
              file9 = path+'T23_imag.bin'
              
              if file.zdim eq 4 then begin
                 file10 = path+'T14_real.bin' 
                 file11 = path+'T14_imag.bin' 
                 file12 = path+'T24_real.bin' 
                 file13 = path+'T24_imag.bin' 
                 file14 = path+'T34_real.bin' 
                 file15 = path+'T34_imag.bin' 
                 file16 = path+'T44.bin' 
              endif
           endelse
           
           if (file_test(file1)+file_test(file2)+file_test(file3)+file_test(file4)+file_test(file5)+$
               file_test(file6)+file_test(file7)+file_test(file8)+file_test(file9)) ne 9 then begin
              error = DIALOG_MESSAGE("Some files are missing", DIALOG_PARENT = wid.base, TITLE='Error',/error)
              return
           endif
           if file.zdim eq 4 then begin
              if (file_test(file10)+file_test(file11)+file_test(file12)+file_test(file13)+file_test(file14)+file_test(file15)+file_test(file16)) ne 7 then begin
                 error = DIALOG_MESSAGE("Some files are missing", DIALOG_PARENT = wid.base, TITLE='Error',/error)
                 return
              endif
           endif
           
           openr,ddd11,file1,/get_lun
           openr,ddd22,file2,/get_lun
           openr,ddd33,file3,/get_lun
           openr,ddd12r,file4,/get_lun
           openr,ddd12i,file5,/get_lun
           openr,ddd13r,file6,/get_lun
           openr,ddd13i,file7,/get_lun
           openr,ddd23r,file8,/get_lun
           openr,ddd23i,file9,/get_lun
           if file.zdim eq 4 then begin
              openr,ddd14r,file10,/get_lun
              openr,ddd14i,file11,/get_lun
              openr,ddd24r,file12,/get_lun
              openr,ddd24i,file13,/get_lun
              openr,ddd34r,file14,/get_lun
              openr,ddd34i,file15,/get_lun
              openr,ddd44,file16,/get_lun
           endif
           
           

           for i=0,anz_blocks-1 do begin
              oblock = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
              block  = make_array([file.xdim,blocksizes[i]],type=4l)
              blocki = make_array([file.xdim,blocksizes[i]],type=4l)
              
              
              if file.type eq 220 and file.zdim eq 3 then begin
                 readu,ddd11,block ; HH-HH
                 oblock[0,0,*,*] = block
                 readu,ddd33,block ; VV-VV
                 oblock[1,1,*,*] = block
                 readu,ddd22,block ; XX-XX
                 oblock[2,2,*,*] = block   
                 
                 readu,ddd12r,block ; HH-XX
                 readu,ddd12i,blocki
                 oblock[0,2,*,*] = complex(block,blocki)
                 oblock[2,0,*,*] = complex(block,-blocki)
                 readu,ddd13r,block ; HH-VV
                 readu,ddd13i,blocki
                 oblock[0,1,*,*] = complex(block,blocki)
                 oblock[1,0,*,*] = complex(block,-blocki)
                 readu,ddd23r,block ; XX-VV
                 readu,ddd23i,blocki
                 oblock[1,2,*,*] = complex(block,-blocki)
                 oblock[2,1,*,*] = complex(block,blocki)
              endif
              if file.type eq 220 and file.zdim eq 4 then begin
                 readu,ddd11,block ; HH-HH
                 oblock[0,0,*,*] = block
                 readu,ddd33,block ; VV-VV
                 oblock[1,1,*,*] = block
                 readu,ddd22,block ; XX1-XX1
                 oblock[2,2,*,*] = block   
                 readu,ddd44,block ; XX2-XX2
                 oblock[3,3,*,*] = block   
                 

                 readu,ddd12r,block ; HH-XX1
                 readu,ddd12i,blocki
                 oblock[0,2,*,*] = complex(block,blocki)
                 oblock[2,0,*,*] = complex(block,-blocki)
                 readu,ddd13r,block ; HH-XX2
                 readu,ddd13i,blocki
                 oblock[0,3,*,*] = complex(block,blocki)
                 oblock[3,0,*,*] = complex(block,-blocki)
                 readu,ddd14r,block ; HH-VV
                 readu,ddd14i,blocki
                 oblock[0,1,*,*] = complex(block,blocki)
                 oblock[1,0,*,*] = complex(block,-blocki)
                 readu,ddd23r,block ; XX1-XX2
                 readu,ddd23i,blocki
                 oblock[2,3,*,*] = complex(block,blocki)
                 oblock[3,2,*,*] = complex(block,-blocki)
                 readu,ddd24r,block ; XX1-VV
                 readu,ddd24i,blocki
                 oblock[1,2,*,*] = complex(block,-blocki)
                 oblock[2,1,*,*] = complex(block,blocki)
                 readu,ddd34r,block ; XX2-VV
                 readu,ddd34i,blocki
                 oblock[1,3,*,*] = complex(block,-blocki)
                 oblock[3,1,*,*] = complex(block,blocki)
              endif	
              if file.type eq 221 and file.zdim eq 3 then begin
                 readu,ddd11,block ; P1-P1
                 oblock[0,0,*,*] = block
                 readu,ddd22,block ; P2-P2
                 oblock[1,1,*,*] = block
                 readu,ddd33,block ; XX-XX
                 oblock[2,2,*,*] = block   
                 
                 readu,ddd12r,block ; P1-P2
                 readu,ddd12i,blocki
                 oblock[0,1,*,*] = complex(block,blocki)
                 oblock[1,0,*,*] = complex(block,-blocki)
                 readu,ddd13r,block ; P1-XX
                 readu,ddd13i,blocki
                 oblock[0,2,*,*] = complex(block,blocki)
                 oblock[2,0,*,*] = complex(block,-blocki)
                 readu,ddd23r,block ; P2-XX
                 readu,ddd23i,blocki
                 oblock[1,2,*,*] = complex(block,blocki)
                 oblock[2,1,*,*] = complex(block,-blocki)
              endif
              if file.type eq 221 and file.zdim eq 4 then begin
                 readu,ddd11,block ; P1-P1
                 oblock[0,0,*,*] = block
                 readu,ddd22,block ; P2-P2
                 oblock[1,1,*,*] = block
                 readu,ddd33,block ; XX1-XX1
                 oblock[2,2,*,*] = block   
                 readu,ddd44,block ; XX2-XX2
                 oblock[3,3,*,*] = block   

                 readu,ddd12r,block ; P1-P2
                 readu,ddd12i,blocki
                 oblock[0,1,*,*] = complex(block,blocki)
                 oblock[1,0,*,*] = complex(block,-blocki)
                 readu,ddd13r,block ; P1-XX1
                 readu,ddd13i,blocki
                 oblock[0,2,*,*] = complex(block,blocki)
                 oblock[2,0,*,*] = complex(block,-blocki)
                 readu,ddd14r,block ; P1-XX2
                 readu,ddd14i,blocki
                 oblock[0,3,*,*] = complex(block,blocki)
                 oblock[3,0,*,*] = complex(block,-blocki)
                 readu,ddd23r,block ; P2-XX1
                 readu,ddd23i,blocki
                 oblock[1,2,*,*] = complex(block,blocki)
                 oblock[2,1,*,*] = complex(block,-blocki)
                 readu,ddd24r,block ; P2-XX2
                 readu,ddd24i,blocki
                 oblock[1,3,*,*] = complex(block,blocki)
                 oblock[3,1,*,*] = complex(block,-blocki)
                 readu,ddd34r,block ; XX1-XX2
                 readu,ddd34i,blocki
                 oblock[2,3,*,*] = complex(block,blocki)
                 oblock[3,2,*,*] = complex(block,-blocki)
              endif
              writeu,eee,oblock
           endfor
           free_lun,ddd11,ddd33,ddd22,ddd12r,ddd12i,ddd13r,ddd13i,ddd23r,ddd23i,eee
           if file.zdim eq 4 then free_lun,ddd14r,ddd14i,ddd24r,ddd24i,ddd34r,ddd34i,ddd44
        endif
        
     endif else begin           ; Single channel images
        path = config.workdir
        inputfile = dialog_pickfile(TITLE='Select POLSARPRO file to load', $
                                    DIALOG_PARENT=wid.base, /MUST_EXIST, FILTER = '*.bin', PATH=path, GET_PATH=path)
        if strlen(inputfile) gt 0 then config.workdir = path
        
        if strlen(inputfile) gt 0 then begin

; Check for complex file
           file.dim  = 2l
           file.vdim = 1l
           file.zdim = 1l				
           file.var  = 4l				
           
; single complex file ? 
           
           size = (file_info(inputfile)).size
           if size eq file.xdim*file.ydim*8 then file.var = 6l

;double complex file (real / imaginary) ? 
           
           fname = file_basename(inputfile)
           if strmatch(fname,'*real*',/fold_case) or strmatch(fname,'*imag*',/fold_case) then begin
              twofiles = 1
              if strmatch(fname,'*real*',/fold_case) then begin
                 file1 = inputfile
                 file2 = strmid(inputfile,0,strpos(inputfile,'_real.bin')) + '_imag.bin'			
              endif else begin
                 file1 = strmid(inputfile,0,strpos(inputfile,'_imag.bin')) + '_real.bin'				
                 file2 = inputfile
              endelse
              file.var = 6l
           endif else twofiles = 0

; angular value ?
           
           if strmatch(fname,'*alpha*',/fold_case) or strmatch(fname,'*beta*',/fold_case)  or strmatch(fname,'*delta*',/fold_case) or strmatch(fname,'*gamma*',/fold_case) then $
              scaling = 1/!radeg else scaling = 1.0 	
           

; Read and convert data

           head = [2l,file.xdim,file.ydim,file.var]
           srat,config.tempdir+config.workfile1,eee,header=head

           if twofiles eq 0 then begin
              openr,ddd,inputfile,/get_lun
              for i=0,anz_blocks-1 do begin
                 block  = make_array([file.xdim,blocksizes[i]],type=file.var)
                 readu,ddd,block
                 writeu,eee,block*scaling
              endfor
              free_lun,ddd,eee
           endif else begin
              openr,dddr,file1,/get_lun
              openr,dddi,file2,/get_lun
              for i=0,anz_blocks-1 do begin
                 blockr  = make_array([file.xdim,blocksizes[i]],type=4l)
                 blocki  = make_array([file.xdim,blocksizes[i]],type=4l)
                 readu,dddr,blockr
                 readu,dddi,blocki
                 writeu,eee,complex(blockr,blocki)
              endfor
              free_lun,dddr,dddi,eee
           endelse
           whatisthis			
        endif
     endelse
     
     file.name = config.tempdir+config.workfile1
     
; update file generation history (evolution)
     evolute,'Import SAR data from POLSARPRO.'

; read palette information
     
     palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
     palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview
     
     file.window_name = 'Untitled.rat'
     generate_preview
     update_info_box
     
  endif
end
