;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: construct_polsar
; last revision : 12.Feb.2003
; written by    : Andreas Reigber
; Combines RAT files into a polarimetric vector image            
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



pro construct_polsar,NOGUI=nogui, called=called, $
                     files=files, xx_factor=xx_factor
;;; files should be in the order: [ HH, VV, HV, VH] (or a subset)
;;; xx_factor is by default 1. if only HV is given, but not symmetrized
;;;           otherwise, xx_factor is 1/sqrt(2)  
  common rat, types, file, wid, config
  compile_opt idl2, strictArrSubs
  
  if n_elements(files) ne 0 && n_elements(xx_factor) eq 0 then xx_factor=1.0
  
  if ~keyword_set(nogui) && ~keyword_set(called) && n_elements(files) eq 0 $
  then begin
;;; Graphical interface
    inputfile1 = ''
    inputfile2 = ''
    inputfile3 = ''
    inputfile4 = ''
    tryagain:

;  		main  = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Construct PolSAR vector',/modal)
    main  = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Construct PolSAR vector',/floating,/tlb_kill_request_events,/tlb_frame_attr)
    
    line1 = WIDGET_BASE(main,column=3)		
    text1 = CW_FIELD(line1,VALUE=inputfile1,/string,XSIZE=50,TITLE='RAT-file for HH :')
    brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)

    line2 = WIDGET_BASE(main,column=2)		
    text2 = CW_FIELD(line2,VALUE=inputfile2,/string,XSIZE=50,TITLE='RAT-file for VV :')
    brow2 = WIDGET_BUTTON(line2,VALUE=' browse ',ysize=35)

    line3 = WIDGET_BASE(main,column=2)		
    text3 = CW_FIELD(line3,VALUE=inputfile3,/string,XSIZE=50,TITLE='RAT-file for HV :')
    brow3 = WIDGET_BUTTON(line3,VALUE=' browse ',ysize=35)

    line4 = WIDGET_BASE(main,column=2)		
    text4 = CW_FIELD(line4,VALUE=inputfile4,/string,XSIZE=50,TITLE='RAT-file for VH :')
    brow4 = WIDGET_BUTTON(line4,VALUE=' browse ',ysize=35)

    buttons  = WIDGET_BASE(main,column=3,/frame)
    but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
    but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
    but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
;		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
    WIDGET_CONTROL, main, /REALIZE, default_button = but_canc, tlb_get_size=toto
    pos = center_box(toto[0],drawysize=toto[1])
    widget_control, main, xoffset=pos[0], yoffset=pos[1]

    repeat begin                ; Event loop
      event = widget_event(main)
      if event.id eq but_info then begin ; Info Button clicked
        infotext = ['POLSAR CONSTRUCTOR',$
                    ' ',$
                    'RAT module written 2003 by Andreas Reigber']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
      end
      if event.id eq brow1 then begin ; Info Button clicked
        path = config.workdir
        inputfile1 = cw_rat_dialog_pickfile(TITLE='Open file for HH', DIALOG_PARENT=main, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
        if strlen(inputfile1) gt 0 then config.workdir = path
        widget_control,text1,set_value=inputfile1
      endif
      if event.id eq brow2 then begin ; Info Button clicked
        path = config.workdir
        inputfile2 = cw_rat_dialog_pickfile(TITLE='Open file for VV', DIALOG_PARENT=main, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
        if strlen(inputfile2) gt 0 then config.workdir = path
        widget_control,text2,set_value=inputfile2
      endif
      if event.id eq brow3 then begin ; Info Button clicked
        path = config.workdir
        inputfile3 = cw_rat_dialog_pickfile(TITLE='Open file for HV', DIALOG_PARENT=main, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
        if strlen(inputfile3) gt 0 then config.workdir = path
        widget_control,text3,set_value=inputfile3
      endif
      if event.id eq brow4 then begin ; Info Button clicked
        path = config.workdir
        inputfile4 = cw_rat_dialog_pickfile(TITLE='Open file for VH', DIALOG_PARENT=main, FILTER = '*.rat', /MUST_EXIST, PATH=path, GET_PATH=path)
        if strlen(inputfile4) gt 0 then config.workdir = path
        widget_control,text4,set_value=inputfile4
      endif
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
    WIDGET_CONTROL, /DESTROY, main
    if event.id ne but_ok then return     
  endif else if n_elements(files) ne 0 then begin
    switch n_elements(files) of
      4: inputfile4=files[3]
      3: inputfile3=files[2]
      2: inputfile2=files[1]
      1: inputfile1=files[0]
    endswitch  
  endif
  
  
  case 1 of
    inputfile1 ne '' and inputfile2 ne '' and inputfile3 ne '' and inputfile4 ne '' : nr_of_channels = 4l
    inputfile1 ne '' and inputfile2 ne '' and inputfile3 ne '' and inputfile4 eq '' : nr_of_channels = 3l
    inputfile1 ne '' and inputfile2 ne '' : nr_of_channels = 2l
    else: begin
      error = DIALOG_MESSAGE("Must select at least HH and VV files", DIALOG_PARENT = wid.base, TITLE='Error',/error)
      goto,tryagain
    end
  endcase	
  
  
  if n_elements(xx_factor) eq 0 then begin 
    xx_factor = 1.0
    if nr_of_channels eq 3 then begin
      dummy = dialog_message( ["-------------- Only 3 channels have been specified --------------",$
                               " ",$
                               "Is the selected HV channel a normal HV channel (select YES) or is",$
                               "it already a correctly symmetrised cross-polar channel (select NO)",$
                               "including the amplitude factor sqrt(2) ?",$
                               " ",$
                               "If you select YES, a factor of sqrt(2) will be multiplied to the ",$
                               "HV channel to correct its total power." ],dialog_parent=wid.base,/question)	
      if dummy eq "Yes" then xx_factor = sqrt(2)
    endif
  endif
; change mousepointer

  WIDGET_CONTROL,/hourglass

  outputfile = config.tempdir+config.workfile2
  finalfile  = config.tempdir+config.workfile1

  head1 = 1l
  head2 = 1l
  head3 = 2l
  head4 = 2l
  var3 = 6l
  var4 = 6l
  rrat,inputfile1,ddd1,header=head1,info=info1,type=type1		
  if ddd1 eq -1 then begin
    error = DIALOG_MESSAGE("HH is not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
  endif
  rrat,inputfile2,ddd2,header=head2,info=info2,type=type2		
  if ddd2 eq -1 then begin
    error = DIALOG_MESSAGE("VV is not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
  endif
  if nr_of_channels ge 3 then begin
    rrat,inputfile3,ddd3,header=head3,info=info3,type=type3		
    if ddd3 eq -1 then begin
      error = DIALOG_MESSAGE("HV is not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
      return
    endif
    xdim3 = head3[1]
    ydim3 = head3[2]
    var3  = head3[3]
  endif
  if nr_of_channels ge 4 then begin
    rrat,inputfile4,ddd4,header=head4,info=info4,type=type4		
    if ddd2 eq -1 then begin
      error = DIALOG_MESSAGE("VH is not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
      return
    endif
    xdim4 = head4[1]
    ydim4 = head4[2]
    var4  = head4[3]
  endif
  
  xdim1 = head1[1]
  xdim2 = head2[1]
  ydim1 = head1[2]
  ydim2 = head2[2]
  var1  = head1[3]
  var2  = head2[3]
  var   = 6l
  
  if head1[0] ne 2 or head2[0] ne 2 or head3[0] ne 2 or head4[0] ne 2 then begin
    error = DIALOG_MESSAGE("Data are not two-dimensional", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
  endif
  if var1 ne 6 or var2 ne 6 or var3 ne 6 or var4 ne 6 then begin
    error = DIALOG_MESSAGE("Data not complex", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
  endif

  if nr_of_channels eq 3 then begin
    xdim4 = max([xdim1,xdim2,xdim3])
    ydim4 = max([ydim1,ydim2,ydim3])
  endif
  if nr_of_channels eq 2 then begin
    xdim3 = max([xdim1,xdim2])
    ydim3 = max([ydim1,ydim2])
    xdim4 = xdim3
    ydim4 = ydim3
  endif
  xdim = min([xdim1,xdim2,xdim3,xdim4])
  ydim = min([ydim1,ydim2,ydim3,ydim4])

  srat,outputfile,eee,header=[3l,nr_of_channels,xdim,ydim,var],info=info1,type=200l		

; calculating preview size and number of blocks

  bs = config.blocksize
  calc_blocks_normal,ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last
  
  progress,Message='Construct PolSAR-image...'
  
  
;do the transform

  for i=0,anz_blocks-1 do begin ; normal blocks
    progress,percent=(i+1)*100.0/anz_blocks
    block1 = make_array([xdim1,blocksizes[i]],type=var)
    block2 = make_array([xdim2,blocksizes[i]],type=var)
    if nr_of_channels ge 3 then block3 = make_array([xdim3,blocksizes[i]],type=var)
    if nr_of_channels ge 4 then block4 = make_array([xdim4,blocksizes[i]],type=var)
    
    oblock = make_array([nr_of_channels,xdim,blocksizes[i]],type=var)
    readu,ddd1,block1
    readu,ddd2,block2
    if nr_of_channels ge 3 then readu,ddd3,block3
    if nr_of_channels ge 4 then readu,ddd4,block4		
    oblock[0,*,*] = block1[0:xdim-1,*]		
    oblock[1,*,*] = block2[0:xdim-1,*]
    if nr_of_channels ge 3 then oblock[2,*,*] = block3[0:xdim-1,*]
    if nr_of_channels ge 4 then oblock[3,*,*] = block4[0:xdim-1,*]
    if nr_of_channels eq 3 then oblock[2,*,*] *= xx_factor
    writeu,eee,oblock
  endfor
  free_lun,ddd1,ddd2,eee
  if nr_of_channels ge 3 then free_lun,ddd3
  if nr_of_channels ge 4 then free_lun,ddd4

; update file information
  file.name  = finalfile
  file.dim   = 3l
  file.xdim  = xdim
  file.ydim  = ydim
  file.zdim  = nr_of_channels
  file.vdim  = 1l
  file.var   = 6l
  file.type  = 200l
  file_move,outputfile,finalfile,/overwrite

  open_rit,/EMPTY
  evolute,'Construct POLSAR data.'

; generate preview
  file.window_name = 'Untitled.rat'
  generate_preview
  update_info_box

end
