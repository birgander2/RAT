pro open_palsar_ersdac_ceos,INPUTFILE = inputfile, PATH = path

 common rat, types, file, wid, config 
 common channel, channel_names, channel_selec, color_flag, palettes, pnames 


if not (keyword_set(inputfile) or keyword_set(path)) then begin 
    path = config.workdir 
    inputfile = cw_rat_dialog_pickfile(TITLE='Open ERSDAC CEOS file', $ 
    DIALOG_PARENT=wid.base, FILTER = ['*.dat', '*.raw'], /MUST_EXIST, PATH=path, GET_PATH=path) 
    if strlen(inputfile) gt 0 then config.workdir = path 
endif 

if inputfile EQ '' then begin
    void = Dialog_Message('You must select input file')
    return
endif

 ;/* change mousepointer 
 
 WIDGET_CONTROL,/hourglass 
 
 filename_base = StrMid( inputfile, StrLen(Path) )
 StringSet = Strsplit( filename_base, '-',/extract)
 
 
;
;/* check if data is ERSDAC format or not
;
ceosFlg = ersdac_chk_ceos(inputfile)

if ceosflg eq -1 then begin
  void = dialog_message('The data could not be imported!!')
  return
endif
 

;/* Read Header Files

;/* Get Header Info
flg_data = ersdac_palsar_get_headerinfo(  $
    inputfile,              $
    Header1=header_info1)

if flg_data EQ -1 then begin
    void=Dialog_Message(!Error_State.Msg)
    return
endif

;/* Check if CEOS format

if StrUpCase(StrTrim(header_info1.form_doc, 2)) NE $
    'CEOS-SAR-CCT' then begin
    void = Dialog_Message('The data could not be imported!!')
    return
endif




;/* Check Level and Set Data Params

  level = STRUPCASE(STRMID(header_info1.file_name, 0, 14))
  offset = long(String(header_info1.rec_length))  ;Offset
  lines = long(String(header_info1.num_line))    ;Number of Lines
  ns = long(String(header_info1.num_group))   ;Number of Samples
  nb = long(String(header_info1.sample_num))    ;Number of Bands
  prefix=long(String(header_info1.prefix))                  
  frame_val = Long(String(header_info1.sar_data_rec_len)) $
    - Long(String(header_info1.byte_count))
  file_type = 10                  ;CEOS Generic
  data_type_code = StrUpCase(StrTrim(header_info1.data_type_code, 2))

    case level of

    'ALOS-1-SAR-RAW':begin
     ; 'Reading Level 1.0 Raw Products'
        case data_type_code of
        'CI*2':data_type=1
        'CI*4':data_type=2
        endcase
     end

    'ALOS-1-SAR-SGF':begin
     ; 'Reading Level 1.5 SGF'
      data_type=12    ;unsigned
      end
      
    'ALOS-1-SAR-SSG':begin
      ;'Reading Level 1.5 SSG'
      data_type=12    ;unsigned
      end
      
    'ALOS-1-SAR-SGP':begin
      ; 'Reading Level 4.1 SGP'
      data_type=12    ;unsigned
      end
      
    'ALOS-1-SAR-PSG':begin
      ; 'Reading Level 4.1 PSG'
      data_type=12    ;unsigned
      end
      
    'ALOS-1-SAR-SCN':begin
      ; 'Reading Level 4.2 SCN'
      data_type= 12   ;unsigned
      end
      
    'ALOS-1-SAR-SCG':begin
      ; 'Reading Level 4.2 SCG'
      data_type= 12   ;unsigned
      end
      
    else:begin
      ; 'Other'
      data_type= 12   ;unsigned
      end
    endcase

;/*open the image(s)

file.xdim = ns
file.ydim = lines
file.zdim = nb
if (nb eq 1) then begin
file.dim = 2l
endif else begin
file.dim=3l
endelse
file.vdim = 1l
file.type = data_type
file.var = data_type
file.info = level
channels=0l

openR, ddd, inputfile, /GET_LUN, /swap_if_little_endian

;/*check if create polar and stokes files(L4.1)

if  (nb eq 4) or (nb eq 9) then begin
    polar_flg=ersdac_palsar_create_polar_stokes(inputfile,CHANNELS=channels,POLAR=ofile_polar,STOKES=ofile_stokes)
    if polar_flg eq -1 then begin
    return
    endif
endif

;/*read Level 4.1
if (level eq 'ALOS-1-SAR-SGP') || (level eq 'ALOS-1-SAR-PSG') then begin
    ;nb = 1
    if (channels eq 0) then begin
        srat, config.tempdir+config.workfile1, lun_out, header=[file.dim,file.zdim,file.xdim,file.ydim,file.var], type = file.type 
        
        linebuffer=uintarr(ns*nb)
        polvectorbuffer=uintarr(nb,ns)

        progress,Message='Decoding ALOS-PALSAR...'
        for i=0l,lines-1 do begin
            progress,percent = i*100.0/lines

            point_lun,ddd,offset+(lines-1-i)*(frame_val+2*ns*nb)+frame_val
            ;point_lun,ddd,offset+i*(2*ns)-1;+prefix-1
            readu,ddd,linebuffer
            for k=0,nb-1 do begin
                polvectorbuffer[k,*] = linebuffer[k:ns*nb-1:nb]
            endfor
            writeu,lun_out,polvectorbuffer
        endfor
    endif else begin
    
    case nb of
    ;nb=4 
    4:begin
         srat, ofile_polar, lun_polar, header=[3l,2,file.xdim,file.ydim,file.var], type = file.type 
         srat, ofile_stokes, lun_stokes, header=[2l,file.xdim,file.ydim,6l], type = 6l
         
            linebuffer=uintarr(ns*nb)
            polarbuffer=uintarr(2,ns)
            stokesbuffer=complexarr(ns)
            
            progress,Message='Decoding ALOS-PALSAR...'
            for i=0l,lines-1 do begin
                progress,percent = i*100.0/lines
                point_lun,ddd,offset+(lines-1-i)*(frame_val+2*ns*nb)+frame_val
                ;point_lun,ddd,offset+i*(2*ns)-1;+prefix-1
                readu,ddd,linebuffer
                for k=0,1 do begin
                    polarbuffer[k,*] = linebuffer[k:ns*nb-1:nb]
                endfor
                stokesbuffer=complex(fix(linebuffer[2:ns*nb-1:nb]),fix(linebuffer[3:ns*nb-1:nb]))
                writeu,lun_polar,polarbuffer
                writeu,lun_stokes,stokesbuffer
            endfor
            free_lun,ddd
            free_lun,lun_polar
            free_lun,lun_stokes
            progress,/destroy
            file.name = ofile_polar
            file.zdim=2l
             file.window_name = ofile_polar
         end
         ;nb=9
         9:begin
             srat, ofile_polar, lun_polar, header=[3l,3,file.xdim,file.ydim,file.var], type = file.type 
             srat, ofile_stokes, lun_stokes, header=[3l,3,file.xdim,file.ydim,6l], type = 6l
         
            linebuffer=uintarr(ns*nb)
            polarbuffer=uintarr(3,ns)
            stokesbuffer=complexarr(3,ns)

            progress,Message='Decoding ALOS-PALSAR...'
            for i=0l,lines-1 do begin
                progress,percent = i*100.0/lines
                point_lun,ddd,offset+(lines-1-i)*(frame_val+2*ns*nb)+frame_val
                ;point_lun,ddd,offset+i*(2*ns)-1;+prefix-1
                readu,ddd,linebuffer
                for k=0,2 do begin
                    polarbuffer[k,*] = linebuffer[k:ns*nb-1:nb]
                endfor
                stokesbuffer[0,*]=complex(fix(linebuffer[3:ns*nb-1:nb]),fix(linebuffer[4:ns*nb-1:nb]))
                stokesbuffer[1,*]=complex(fix(linebuffer[5:ns*nb-1:nb]),fix(linebuffer[6:ns*nb-1:nb]))
                stokesbuffer[2,*]=complex(fix(linebuffer[7:ns*nb-1:nb]),fix(linebuffer[8:ns*nb-1:nb]))
                writeu,lun_polar,polarbuffer
                writeu,lun_stokes,stokesbuffer
            endfor
            free_lun,ddd
            free_lun,lun_polar
            free_lun,lun_stokes
            progress,/destroy
            file.name = ofile_polar
            file.zdim=3l
            file.window_name = ofile_polar
         end
         else:begin
         void=DIALOG_MESSAGE('This file seems not to be Level4.1')
         return
         end
         endcase         
        endelse  
;*/
endif else begin
;/*read Level 1.0
         if (level eq  'ALOS-1-SAR-RAW') then begin
             frame_val=frame_val+100 
             ;prefix=frame_val
             linebuffer=intarr(ns*nb)
             polvectorbuffer=intarr(nb,ns)
         srat, config.tempdir+config.workfile1, lun_out, header=[file.dim,file.zdim,file.xdim,file.ydim,file.var], type = file.type              
         endif else begin
             linebuffer=uintarr(ns*nb)
             polvectorbuffer=uintarr(nb,ns)
             srat, config.tempdir+config.workfile1, lun_out, header=[file.dim,file.xdim,file.ydim,file.var], type = file.type 
         endelse
 
    
         progress,Message='Decoding ALOS-PALSAR...'
         for i=0l,lines-1 do begin
    
            progress,percent = i*100.0/lines
            point_lun,ddd,offset+(lines-1-i)*(frame_val+((data_type gt 1)+1)*ns*nb)+frame_val
            ;point_lun,ddd,offset+i*(2*ns)-1;+prefix-1
            readu,ddd,linebuffer
            for k=0,nb-1 do begin
                polvectorbuffer[k,*] = linebuffer[k:ns*nb-1:nb]
            endfor
            writeu,lun_out,polvectorbuffer
        endfor
endelse



 ;/* close open input handles
free_lun,ddd
if (channels eq 0) then begin
      free_lun,lun_out
      file.name = config.tempdir+config.workfile1
      file.window_name = 'Untitled.rat'
endif

 ;/* read palette information
 
palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
  
 ;/* generate preview
update_info_box
generate_preview,/window_title
   
close,/all

end