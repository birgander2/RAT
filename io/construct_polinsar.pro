;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: construct_polin
; written by    : Maxim Neumann
; last revision : 08/2006
; Combines RAT files into polarimetric multibaseline dataset
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



pro construct_polinsar,NOGUI=nogui, called=called, files=files, info_get=info_get, fe_get=fe_get, kz_get=kz_get, blp_get=blp_get
  common rat, types, file, wid, config

  if n_elements(info_get) eq 0 then info_get=0
  if n_elements(fe_get) eq 0 then fe_get=0
  if n_elements(kz_get) eq 0 then kz_get=0
  if n_elements(blp_get) eq 0 then blp_get=0
  afterwards=[info_get,fe_get,kz_get,blp_get] eq 1

  if ~keyword_set(nogui) && ~keyword_set(called)  then begin ; Graphical interface
     MAX_TR = 20
     cur_tr = 2
     files = strarr(MAX_TR)
     lines = lonarr(MAX_TR)
     texts = lonarr(MAX_TR)
     brows = lonarr(MAX_TR)

     tryagain:
     main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Construct MB-PolInSAR dataset', $
                        /floating,/tlb_kill_request_events,/tlb_frame_attr,/SCROLL,X_SCROLL=600,Y_SCROLL=500)
     but_tr = widget_droplist(main,value=strcompress(indgen(MAX_TR-1)+2),title='Number of tracks: ')
     for i=0,cur_tr-1 do begin
        lines[i] = WIDGET_BASE(main,column=2)
        texts[i] = CW_FIELD(lines[i],VALUE=files[i],/NOEDIT,/string,XSIZE=50,TITLE='Polarimetric scattering vector '+strcompress(i,/r)+':')
        brows[i] = WIDGET_BUTTON(lines[i],VALUE=' browse ',ysize=35)
     endfor

;      main2 = widget_base(main,/column,/frame)
;      tmp   = widget_label(main2,value="Afterwards")
     fld = cw_bgroup(main,label_top="Afterwards",/NONEXCLUSIVE,set_value=afterwards,/FRAME, $
                     ['Import system information','Construct and connect flat earth file', $
                      'Construct and connect vertical wavenumber file','Construct and connect perpendicular baseline lengths file'])

     buttons  = WIDGET_BASE(main,column=4,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
;      if cur_tr lt max_tr then $
;         but_add  = WIDGET_BUTTON(buttons,VALUE=' Add Tracks (+5)',xsize=120)

     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
;	     WIDGET_CONTROL, main, /REALIZE,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
     widget_control, but_tr, set_droplist_select=cur_tr-2

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['MB-POLINSAR CONSTRUCTOR',$
                       ' ',$
                       'RAT module written 2006 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
        for i=0,cur_tr-1 do begin
           if event.id eq brows[i] then begin ; Browse clicked
              path = config.workdir
              files[i] = cw_rat_dialog_pickfile(TITLE='Open RAT file', DIALOG_PARENT=wid.base, FILTER = '*.rat', $
                                                /MUST_EXIST, PATH=path, GET_PATH=path)
              if strlen(files[i]) gt 0 then config.workdir = path
              widget_control,texts[i],set_value=files[i]
           endif
        endfor
        if event.id eq but_tr then begin
           cur_tr = event.index+2
           widget_control, fld, get_value=afterwards
           WIDGET_CONTROL, /DESTROY, main
           goto, tryagain
        endif
;         if event.id eq but_add then begin
;            cur_tr = (cur_tr+5) < max_tr
;            WIDGET_CONTROL, /DESTROY, main
;            goto, tryagain
;         endif
     endrep until (event.id eq but_ok && total(files[0:cur_tr-1] eq '')eq 0) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control, fld, get_value=afterwards
     WIDGET_CONTROL, /DESTROY, main
     if event.id ne but_ok then return
  endif else begin
     cur_tr=n_elements(files)
     if cur_tr le 1 then message,"Please provide an array of at least two PolSAR RAT data files."
  endelse

; change mousepointer
  WIDGET_CONTROL,/hourglass
  outputfile = config.tempdir+config.workfile2
  finalfile  = config.tempdir+config.workfile1

  n_tr = cur_tr
  ddd  = lonarr(n_tr)

  rrat,files[0],dd,header=head0,info=info0,type=type0
  if dd eq -1 then begin
     error = DIALOG_MESSAGE('0: Not a RAT file', DIALOG_PARENT = wid.base, TITLE='Error',/error)
     free_lun, dd
     files[0]=''
     goto,tryagain
  endif
  ;need to extend to handle partial polarimetry datasets
  ;where only 2-channels are present - these need to be
  ;identfiable as HH/VV/HV etc.
  if type0 lt 200 || type0 gt 210 || head0[0] ne 3 then begin
     error = DIALOG_MESSAGE('0: Data has to be a polarimetric (3-pol) scattering vector', $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     files[0]=''
     free_lun, dd
     goto,tryagain
  endif
  free_lun, dd
  for i=0,n_tr-1 do begin
     rrat,files[i],dd,header=head,info=info,type=type & ddd[i]=dd
     if ddd[i] eq -1 then begin
        error = DIALOG_MESSAGE("Not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
        for j=i-1,0,-1 do $
           free_lun,ddd[i]
        files[i]=''
        goto,tryagain
     endif
     if ~array_equal(head,head0) || type ne type0 then begin
        error = DIALOG_MESSAGE("Data dimensions or data types do not match between track 0 and track "+strcompress(i,/R)+'!', $
                               DIALOG_PARENT = wid.base, TITLE='Error',/error)
        for j=i-1,0,-1 do $
           free_lun,ddd[i]
        files[i]=''
        goto,tryagain
     endif
  endfor

;   nrch1 = head1[1]              ; nr of pol-channels
;   nrch2 = head2[1]              ; nr of pol-channels
;   xdim1 = head1[2]
;   xdim2 = head2[2]
;   ydim1 = head1[3]
;   ydim2 = head2[3]
;   var1  = head1[4]
;   var2  = head2[4]
;   var   = 6l
;   nr_of_channels = head[1]

  pol   = head[1]
  xdim  = head[2]
  ydim  = head[3]
  var   = head[4]
  case type of
     200: newtype=500L
     209: newtype=502L
     210: newtype=501L
  endcase
  newinfo = 'mb: '+string(info0)+' + ...'
  srat,outputfile,eee,header=[4L,pol,n_tr,xdim,ydim,var],info=newinfo,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last
; pop up progress window
  progress,Message='Construct MB-PolInSAR-image...'

;do the transform
  for i=0,anz_blocks-1 do begin ; normal blocks
     progress,percent=(i+1)*100.0/anz_blocks

     oblock = make_array([pol,n_tr,xdim,blocksizes[i]],type=var)
     block = make_array([pol,xdim,blocksizes[i]],type=var)
     for tr=0,n_tr-1 do begin
        readu,ddd[tr],block
        oblock[*,tr,*,*] = block
     endfor
     writeu,eee,oblock
  endfor
  for i=0,n_tr-1 do free_lun,ddd[i]
  free_lun,eee

; update file information
  file.name  = finalfile
  file.dim   = 4l
  file.xdim  = xdim
  file.ydim  = ydim
  file.zdim  = n_tr
  file.vdim  = pol
  file.var   = var
  file.type  = newtype
  file.info  = newinfo
  file_move,outputfile,finalfile,/overwrite

  if file_test(files[0]) then $
    open_rit, file=files[0] $
  else open_rit,/EMPTY
  ignore = set_par('polarizations',pol)
  ignore = set_par('nr_tracks',n_tr)
  evolute,'Construct multibaseline polarimetic SAR data with '+ $
          strcompress(n_tr,/r)+' tracks and '+strcompress(pol,/r)+ $
          ' polarizations from '+strjoin(file_basename(files[0:n_tr-1]),', ')+'.'
; generate preview
  file.window_name = 'Untitled.rat'
  generate_preview
  update_info_box

  if afterwards[0] then import_info_generic, TRACKS=n_tr
  if afterwards[1] then construct_flatearth, TRACKS=n_tr, /CONNECT
  if afterwards[2] then construct_kz,        TRACKS=n_tr, /CONNECT
  if afterwards[3] then construct_blp,       TRACKS=n_tr, /CONNECT

end
