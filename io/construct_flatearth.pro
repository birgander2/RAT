;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: construct_flatearth
; written by    : Maxim Neumann
; last revision : 08/2006
; Construct flatearth file
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



pro construct_flatearth, called=called, CONNECT=CONNECT, TRACKS_NR=cur_tr, files=fe_files, $
                         include_master_track_at_pos=include_master_track_at_pos
  common rat, types, file, wid, config
  compile_opt idl2

;;; @param include_master_track_at_pos: implies to include a zero--valued fe
;;; phase for the master track at this given position between the
;;; provided files.


  MAX_TR = 20
  files = strarr(MAX_TR)
  if n_elements(fe_files) ne 0 then files[0] = fe_files
  lines = lonarr(MAX_TR)
  texts = lonarr(MAX_TR)
  brows = lonarr(MAX_TR)
  if n_elements(cur_tr)  eq 0 then cur_tr  = 1
  if n_elements(CONNECT) eq 0 then connect = 0
  conjugate = 0

  tryagain:
  main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Construct flatearth file', $
                     /floating,/tlb_kill_request_events,/tlb_frame_attr,/SCROLL,X_SCROLL=600,Y_SCROLL=500)
  but_tr = widget_droplist(main,value=strcompress(indgen(MAX_TR)+1),title='Number of tracks: ')
  for i=0,cur_tr-1 do begin
     lines[i] = WIDGET_BASE(main,column=2)
     texts[i] = CW_FIELD(lines[i],VALUE=files[i],/string,/NOEDIT,XSIZE=50,TITLE='Flat earth phase file for track '+strcompress(i,/r)+':')
     brows[i] = WIDGET_BUTTON(lines[i],VALUE=' browse ',ysize=35)
  endfor

  but_conj = cw_bgroup(main,label_left="Conjugate complex phases?",[' '],set_value=[conjugate],/nonexcl)
  but_add  = cw_bgroup(main,label_left="Connect this flatearth-file with the current dataset? ",[' '],set_value=[connect],/nonexcl)

  buttons  = WIDGET_BASE(main,column=4,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

  WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
;	     WIDGET_CONTROL, main, /REALIZE,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=pos[0], yoffset=pos[1]
  widget_control, but_tr, set_droplist_select=cur_tr-1

  repeat begin                  ; Event loop
     event = widget_event(main)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['FLAT-EARTH (FE) FILE CONSTRUCTOR',$
                    ' ',$
                    'RAT module written 2006 by Maxim Neumann']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
     endif
     for i=0,cur_tr-1 do begin
        if event.id eq brows[i] then begin ; Browse clicked
           path = config.workdir
           files[i] = cw_rat_dialog_pickfile(TITLE='Open RAT file with flat-earth phase content', DIALOG_PARENT=wid.base, FILTER = '*.rat', $
                                             /MUST_EXIST, PATH=path, GET_PATH=path)
           if strlen(files[i]) gt 0 then config.workdir = path
           widget_control,texts[i],set_value=files[i]
        endif
        if event.id eq texts[i] then begin
           widget_control,texts[i],get_value=file
           if file_test(file,/read) then files[i]=file $
           else widget_control,texts[i],set_value=files[i]
        endif
     endfor
     if event.id eq but_tr then begin
        cur_tr = event.index+1
        WIDGET_CONTROL, /DESTROY, main
        goto, tryagain
     endif
     if event.id eq but_add then $
        widget_control,but_add,get_value=connect
     if event.id eq but_conj then $
        widget_control,but_conj,get_value=conjugate
  endrep until (event.id eq but_ok && total(files[0:cur_tr-1] eq '') le 1) || (event.id eq but_canc) || tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  WIDGET_CONTROL, /DESTROY, main
  if event.id ne but_ok then return
  n_tr = cur_tr
  for i=0,n_tr-1 do $
     if ~file_test(files[i],/READ) then files[i]=''
  if total(files[0:n_tr-1] eq '') gt 1 || (n_tr eq 1 && files[0] eq '') then begin
     error = DIALOG_MESSAGE("Please provide more files", DIALOG_PARENT = wid.base, TITLE='Error',/error)
     goto,tryagain
  endif

  newtype = n_tr eq 1? 390L: 391L
  ddd  = lonarr(n_tr)
  i = files[0] eq ''
  rrat,files[i],dd,header=head0,info=info0,type=type0,var=var,n_dim=n_dim,dim=dim
  if dd eq -1 then begin
     error = DIALOG_MESSAGE(strcompress(i)+"Not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
     files[i] = ''
     free_lun, dd
     goto,tryagain
  endif
  free_lun, dd
  if n_dim ne 1 && n_dim ne 2 then begin
     error = DIALOG_MESSAGE(strcompress(i)+"Needs one- or two-dimensional arrays", DIALOG_PARENT = wid.base, TITLE='Error',/error)
     files[i] = ''
     goto,tryagain
  endif

  varch=-3
  if var ne 4 && var ne 5 then begin
     main=WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Edit Flat-Earth-Phase File', $
                      /floating,/tlb_kill_request_events,/tlb_frame_attr)
     varch = -2
     lab2=widget_label(main,value='The data type is not float or double. The type will be converted to float.')
     if var eq 6 || var eq 9 then $
        fld2=cw_bgroup(main,label_left='Use:',['Phase','Real Part','Imaginary Part','Amplitude'],set_value=0,/excl,/column)
     buttons  = WIDGET_BASE(main,column=4,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
     repeat begin
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['FLATEARTH-FILE CONSTRUCTOR',$
                       ' ',$
                       'RAT module written 2006 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     if n_elements(fld2) ne 0 then widget_control,fld2,get_value=varch
     WIDGET_CONTROL, /DESTROY, main
     if event.id ne but_ok then return
  endif

  for i=0,n_tr-1 do $
     if files[i] ne '' then begin
     rrat,files[i],dd,header=head,info=info,type=type & ddd[i]=dd
     if ddd[i] eq -1 then begin
        error = DIALOG_MESSAGE(strcompress(i)+" Not a RAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
        files[i] = ''
        for j=i,0,-1 do $
           free_lun,ddd[i]
        goto,tryagain
     endif
     if ~array_equal(head,head0) then begin
        error = DIALOG_MESSAGE(strcompress(i)+"Data dimensions or data types do not match between track 0 and track "+strcompress(i,/R)+'!', $
                               DIALOG_PARENT = wid.base, TITLE='Error',/error)
        files[i] = ''
        for j=i,0,-1 do $
           free_lun,ddd[i]
        goto,tryagain
     endif
     free_lun,ddd[i]
  endif

  path = config.workdir
  sfile = cw_rat_dialog_pickfile(TITLE='Save flat-earth-phase file', DIALOG_PARENT=wid.base, FILTER = '*.rat', $
                                 PATH=path, GET_PATH=path)
  if sfile eq '' then return ;; no save file


  if n_elements(dim) eq 1 then dim=[dim,1]
  fe = fltarr([n_tr,dim])
  for i=0,n_tr-1 do $
     if files[i] ne '' then begin
     rrat,files[i],fe1
     case varch of
        -3: fe[i,*,*]=fe1
        -2: fe[i,*,*]=float(fe1)
        0 : fe[i,*,*]=atan(fe1,/phase)
        1 : fe[i,*,*]=real_part(fe1)
        2 : fe[i,*,*]=imaginary(fe1)
        3 : fe[i,*,*]=abs(fe1)
        else:
     endcase
  endif

  if conjugate then $
     fe = -fe

  xdim=dim[0] & ydim=n_dim eq 1? 1: dim[1]
  newsiz = dialog_resize(title="Do you wish to resize the flat-earth-phase file?",xdim=xdim,ydim=ydim,change=change,resize=resize)
  if change then begin
     fe=fe[*,newsiz[0,0]:newsiz[1,0],newsiz[0,1]:newsiz[1,1]]
     if resize then begin
        if total(((size(fe))[2:3]>newsiz[2,*])mod((size(fe))[2:3]<newsiz[2,*])) eq 0 $
        then fe=rebin(fe,n_tr,newsiz[2,0],newsiz[2,1]) $
        else fe=congrid(fe,n_tr,newsiz[2,0],newsiz[2,1])
     endif
;     if resize then fe=congrid(fe,n_tr,newsiz[2,0],newsiz[2,1])
  endif

  fe=reform(fe,/overwrite)

  srat,sfile,fe,type=newtype,info=strcompress(n_dim,/r)+'-dim fe file for '+strcompress(n_tr,/R)+' tracks'
  if connect then begin
     ignore=set_par('fe_file',sfile)
     info = dialog_message("Don't forget to save your current dataset to save also the connection to the fe_file.",Dialog_parent=wid.base,/info)
  endif
end
