;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
 ; RAT Module: open_alos 
 ; written by : Tishampati Dhar / Michael Ward (Apogee) 
 ; last revision : 30th . January 2006 
 ; Open files in JAXA ALOS format 
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
 
pro open_palsar,CALLED=CALLED, INPUTFILE = inputfile, PATH = path, $
                do_xpol=do_xpol, do_k2m=do_k2m, load_pol=load_pol
  common rat, types, file, wid, config 
  common channel, channel_names, channel_selec, color_flag, palettes, pnames 
  
  
;;;------------------------------------------------------------ 
;establish error handler 
; catch, error_number 
; if error_number ne 0 then begin 
; error = DIALOG_MESSAGE(["Could not read image !",!err.msg], DIALOG_PARENT = wid.base, TITLE='Unrecognized Format',/error) 
; catch ,/cancel 
; return 
; end 
;don't print math exceptions 
;!EXCEPT=0 
;------------------------------------------------------------ 
  
  if n_elements(inputfile) ne 0 && file_test(inputfile) then begin
    path = file_dirname(inputfile,/mark)
    channels = keyword_set(load_pol)
    if n_elements(do_xpol) eq 0 then do_xpol=0
    if n_elements(do_k2m)  eq 0 then do_k2m =0
  endif else begin
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='ALOS-PALSAR import',/floating,/tlb_kill_request_events,/tlb_frame_attr )
    butt = cw_bgroup(main,[' Load single data set',' Load multiple files (polarimetric data)'],set_value=0,row=2,/exclusive)		
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
        infotext = ['ALOS-PALSAR IMPORT',$
                    'Level 1.1 and 1.5 products only',$
                    ' ',$
                    'RAT module written 01/2006 by Tishampati Dhar / Michael Ward (Apogee)',$
                    'extended 02/2006 by Andreas Reigber']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
      end
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
    widget_control,butt,GET_VALUE=channels
    widget_control,main,/destroy
    if event.id ne but_ok then return ; OK button _not_ clicked
    
    if not (keyword_set(inputfile) or keyword_set(path)) then begin 
      path = config.workdir 
      inputfile = cw_rat_dialog_pickfile(TITLE='Open ALOS-CEOS file', $ 
                                         DIALOG_PARENT=wid.base, FILTER = 'IMG*1*', /MUST_EXIST, PATH=path, GET_PATH=path) 
      if strlen(inputfile) gt 0 then config.workdir = path 
    endif 
  endelse
  
  if strlen(inputfile) gt 0 then begin 
;;; change mousepointer 
    WIDGET_CONTROL,/hourglass 
    
    filename_base = StrMid( inputfile, StrLen(Path) )
    StringSet = Strsplit( filename_base, '-',/extract)
    
; HEADER analysis
    header = bytarr(720)
    openr,dummy,path+filename_base,/get_lun,/swap_if_big_endian 
    readu,dummy,header 
    if header[55] eq 65 then begin
      mess = dialog_message('ALOS-PALSAR level 1.0 data not supported',/error,dialog_parent=wid.base)
      free_lun,dummy 
      return
    endif
    if header[55] eq 66 then level = 1.1 else level = 1.5
    lines  = long(string(header[180:185])) ; nr of data lines
    bps    = long(string(header[224:227])) ; bytes per sample
    prefix = long(string(header[276:279])) ; prefix at beginning of line
    pixels = (long(string(header[186:191]))-prefix) /  bps ; nr of pixels per line  
    offset = header[11]+2l^8*header[10]+2l^16*header[9]+2l^24*header[8] + 1

;	lines = long(string(header[236:243])) - 1       
;	pixels = long(string(header[248:255]))          

    file.xdim = pixels 
    file.ydim = lines 
    free_lun,dummy 
    
; Generating other filenames 
    
    symm = 0
    if channels eq 0 then begin
      file.dim  = 2l 
      file.zdim = 1l 
      file.vdim = 1l 
      if bps eq 8 then file.type = 101l else file.type = 100l  
      if bps eq 8 then file.var = 6l else file.var = 4l 
      file.info = Stringset[1]+'-'+Stringset[2] 
      nr_channels = 1 
    endif else begin
      filename = strarr(4)
      filetest = 'IMG-HH-' + Stringset[2] + '-' + Stringset[3]
      if file_test(path+filetest) then filename[0] = filetest
      filetest = 'IMG-VV-' + Stringset[2] + '-' + Stringset[3]
      if file_test(path+filetest) then filename[1] = filetest
      filetest = 'IMG-HV-' + Stringset[2] + '-' + Stringset[3]
      if file_test(path+filetest) then filename[2] = filetest
      filetest = 'IMG-VH-' + Stringset[2] + '-' + Stringset[3]
      if file_test(path+filetest) then filename[3] = filetest

      Leader  = 'LED-' + Stringset[1] + '-' + Stringset[3]
      Trailer = 'TRL-' + Stringset[1] + '-' + Stringset[3]
      Volume  = 'VOL-' + Stringset[1] + '-' + Stringset[3]
      
      
      index = where(filename ne '',nr)
      if nr gt 0 then filename = filename[index]
      nr_channels = (size(filename))[1]
      
      if nr_channels lt 2 then begin
        mess = dialog_message(['No multiple files found.','This is not polarimetric data !!'],title = 'ALOS import',/error,dialog_parent=wid.base)
        return
      endif
      if nr_channels eq 2 && ~keyword_set(called) then begin
        mess = dialog_message(['Only two channels found (partial polarimetric data).','RAT is not supporting analysis of partial polarimetric data.','WARNING: errors are very likely to occur'],title = 'ALOS import',dialog_parent=wid.base)
      endif
      if nr_channels eq 3 then begin
        mess = dialog_message(['Not enough channel found / a file is missing','WARNING: errors are very likely to occur'],title = 'ALOS import',dialog_parent=wid.base)
      endif
      if nr_channels gt 1 and level eq 1.5 && ~keyword_set(called) then begin
        mess = dialog_message(['You are loading polarimetric amplitude data. RAT is not supporting such data and it is',$
                               'generally recommended to use coherent complex data for polarimetric analysis !!','WARNING: errors are very likely to occur !'],title = 'ALOS import',/error,dialog_parent=wid.base)
      endif
      if nr_channels eq 4 $     ; and level eq 1.1
      then begin                ; ask for crosspolar symmetrisation
        if n_elements(do_xpol) eq 0 then begin 
          mess = dialog_message(['Perform cross-polar symmetrisation?','','Hint: HV / VH will be averaged.','This is usually recommendable.'],title = 'ALOS import',dialog_parent=wid.base,/question)
          if mess eq "Yes" then symm = 1
        endif else symm=keyword_set(do_xpol)
      endif
      
      file.dim  = 3l 
      file.zdim = nr_channels-symm 
      file.vdim = 1l 
      if nr_channels gt 2 then begin
        file.type = 200l 
      endif else begin
        file.type = 280l
      endelse
      if bps eq 8 then file.var = 6l else file.var = 4l
      file.info = Stringset[2] 
    endelse
    
    
;TODO:Need to extend for dual-polarization in fine beam mode 
;TODO:Need to extend to parse leader,trailer,volume 
;TODO:Need to extend for skipping blank pixels/padding 
    
                                ; open the image(s) 

    if ~keyword_set(channels) then begin 
      openr,ddd,path+filename_base,/get_lun,/swap_if_little_endian 
      srat,config.tempdir+config.workfile1,lun_out,header=[2l,pixels,lines,file.var],type=file.type
    endif else begin
      ddd = lonarr(nr_channels)
      for i=0,nr_channels-1 do begin
        openr,dddumm,path+filename[i],/get_lun,/swap_if_little_endian
        ddd[i] = dddumm
      endfor
      srat,config.tempdir+config.workfile1,lun_out,header=[3l,nr_channels-symm,pixels,lines,file.var],type=file.type
    endelse
    
    if bps eq 8 then begin
      linebuffer      = complexarr(pixels) 
      polvectorbuffer = complexarr(nr_channels,pixels) 
    endif else begin
      linebuffer      = uintarr(pixels) 
      polvectorbuffer = fltarr(nr_channels,pixels) 
    endelse

    progress,Message='Decoding ALOS-PALSAR...'
    rev_img = level eq 1.5 ;; reverse y-direction
    start_line = keyword_set(rev_img)? lines-1L: 0L
    end_line   = keyword_set(rev_img)? 0L: lines-1L
    incr_line  = keyword_set(rev_img)? -1: 1
    for i=start_line,end_line,incr_line do begin 
      progress,percent=i*100.0/lines
      for k=0,nr_channels-1 do begin
        point_lun,ddd[k],offset+i*(prefix+bps*pixels)+prefix-1
        readu,ddd[k],linebuffer 
        polvectorbuffer[k,*] = linebuffer / 10000.0
      endfor
      if symm eq 1 then begin
        polvectorbuffer[2,*] = (polvectorbuffer[2,*] + polvectorbuffer[3,*]) / sqrt(2.0)
        writeu,lun_out,polvectorbuffer[0:2,*] 
      endif else writeu,lun_out,polvectorbuffer 
    endfor 
    
                                ; close open input handles 

    for i=0,nr_channels-1 do free_lun,ddd[i]
    free_lun,lun_out
    
    file.name = config.tempdir+config.workfile1 

; read palette information 

    palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
    palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
    
; directly transform to C / T matrix ?
    
    mess = "No"
    progress,/destroy
    if nr_channels eq 4 $       ; and level eq 1.1
    then begin                  ; ask for crosspolar symmetrisation
      if n_elements(do_k2m) eq 0 then begin 
      mess = dialog_message(['Directly transform to covariance matrix representation?',' ',$
                             'Hint: Choose presumming factors, which correct','for different resolutions in range / azimuth.'],title = 'ALOS import',dialog_parent=wid.base,/question)
    endif else mess=(['No','Yes'])[keyword_set(do_k2m)] 
      if mess eq "Yes" then begin
        k_to_m,smmx=1,smmy=8
      endif
    endif 

; generate preview 
    
    if mess eq "No" then begin
      file.window_name = 'untitled.rat' 
      update_info_box 
      generate_preview,/window_title
    endif
    
  endif 
  close,/all 
  
end
