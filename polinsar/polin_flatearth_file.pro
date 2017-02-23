;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_flatearth_file
; written by       : Maxim Neumann
; last revision    : 08/2006
; Flat-earth removal from file
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

pro polin_flatearth_file,CALLED = called
  common rat, types, file, wid, config

  if ~((file.type ge 500 && file.type le 513)) then begin
     error = DIALOG_MESSAGE("This is not multibaseline data!", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,matrix=matrix,baselines=n_bl
  conj_fe= 0
  show_intrf = 0
  single_file = 1
  files = strarr(n_TR)
  lines = lonarr(n_TR)
  labels= lonarr(n_TR)
  texts = lonarr(n_TR)
  brows = lonarr(n_TR)
  ignore  = get_par('fe_file',fe_file)
  if ignore eq 0 && file_test(fe_file,/READ) then files[0]=fe_file

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,/column, $
                        TITLE='Flat-earth removal from file',/modal,/tlb_kill_request_events,/tlb_frame_attr)
     but_ch = cw_bgroup(main,['multiple files with flat earth phases','single multi-track file with flat earth phases'], $
                        label_left='Use ',/excl,set_value=single_file,/col)
     for i=0,n_tr-1 do begin
        lines[i] = WIDGET_BASE(main,column=3)
        labels[i]= WIDGET_LABEL(lines[i],VALUE='Flat-earth file in range '+(i ne 0 || ~single_file?strcompress(i,/R):' ')+': ')
        texts[i] = CW_FIELD(lines[i],VALUE=files[i],/string,XSIZE=50,/noedit,title='')
;         texts[i] = CW_FIELD(lines[i],VALUE=files[i],/string,XSIZE=50, $
;                             TITLE='Flat-earth file in range '+strcompress(i,/R)+':')
        brows[i] = WIDGET_BUTTON(lines[i],VALUE=' browse ',ysize=35)
     endfor
     conj_but = cw_bgroup(main,['Conjugate complex phases'],/nonexcl,set_value=conj_fe)
     intrfpha=cw_bgroup(main,"Generate interferometric phases afterwards",/nonexclusive,set_value=[show_intrf])
     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc, $
                     tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
     new_event={ID:0L,TOP:0L,HANDLER:0L}
     WIDGET_CONTROL, but_ch, /NO_COPY, SEND_EVENT=new_event
more:
     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_ch then begin
           widget_control,but_ch,get_value=single_file
           for i=1,n_tr-1 do begin
              widget_control,texts[i],sensitive=~single_file
              widget_control,brows[i],sensitive=~single_file
              widget_control,labels[i],sensitive=~single_file
           endfor
           widget_control,labels[0],set_value='Flat-earth file in range '+(~single_file?'0':' ')+': '
        endif
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['MB FLAT-EARTH REMOVAL FROM FILE',$
                       ' ',$
                       'RAT module written 08/2006 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Information')
        end
        path = config.workdir
        for i=0,n_tr-1 do if event.id eq brows[i] then begin
           files[i] = DIALOG_PICKFILE(TITLE='Open flat-earth file', $
                                      DIALOG_PARENT=wid.base, FILTER = '*.rat', $
                                      /MUST_EXIST, PATH=path, GET_PATH=path)
           if strlen(files[i]) gt 0 then begin
              config.workdir = path
              rrat,files[i],fextmp,head=siz & free_lun,fextmp
;              siz = size(fextmp)
              if (~single_file && (siz[0] ne 1 || siz[1] ne file.xdim)) || $
                 ( single_file && total(siz[0:2] eq [2,n_tr,file.xdim])ne 3) then begin
                 error = DIALOG_MESSAGE("Array size does not correspond " + $
                                        "to image size", $
                                        DIALOG_PARENT = wid.base, TITLE='Error',/error)
                 files[i]=''
              endif
              widget_control,texts[i],set_value=files[i]
           endif
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     if event.id eq but_ok && ~single_file && total(files eq '') gt 1 then goto,more
     if event.id eq but_ok && single_file && files[0] eq '' then goto,more
     widget_control,conj_but,get_value=conj_fe
     widget_control,intrfpha,GET_VALUE=show_intrf
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

  fex    = fltarr(n_tr,file.xdim) ; extend later to 2d-fe
  if single_file then begin
     rrat,files[0],fex
     if size(fex,/n_dim) ne 2 then $
        fex = fex[*,*,0,0,0,0,0]
  endif else for i=0,n_tr-1 do if file_test(files[i]) then begin
     rrat,files[i],fextmp
     fex[i,*] = fextmp[*,0]
  endif & fextmp=0
  if conj_fe then fex = -fex

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  info += ' file-FE'
  srat,outputfile,eee,header=head,info=info,type=type		

; calculating preview size and number of blocks
  bs = config.blocksize/file.zdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Removing flat-earth...',/cancel_button

; precompute flat earth
;  fex      = mm_s2v(fex,pol)
  fex      = -reform(mm_s2v(fex,pol),/overwrite,n_tr*pol,file.xdim)
;   if matrix then $
;      fex = mm_xprod(fex,/conj)
;     fex = mm_v2m(fex,pol*n_tr) * conj(mm_v2m(fex,/TR,pol*n_tr))
;      fex = mm_s2v(fex)
;      for i=0,n_tr*pol-1 do for j=0,n_tr*pol-1 do if i ne j then fex[i,j,*]=0
;  endif
  fe_range = complex(cos(fex),sin(fex)) & fex=0
  if matrix then $
     fe_range = mm_xprod(fe_range,/conj)

;start block processing
  for i=0,anz_blocks-1 do begin ; normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]], $
                        type=file.var)
     readu,ddd,block

; -------- THE FILTER ----------
     for y=0,blocksizes[i]-1 do block[*,*,*,y] *= fe_range
;      if matrix $
;      then for x=0,file.xdim-1 do block[*,*,i,*] = mm_mm(mm_mm(fe_range,block[*,*,i,*]),conj(fe_range)) $
;      else for y=0,blocksizes[i]-1 do block[*,*,*,y] *= fe_range
     
; -------- THE FILTER ----------

     writeu,eee,block
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.info += ' file-FE'

  evolute,'Remove flat earth phase...'

; generate preview
  if show_intrf then polin_interf_pha $
  else if ~keyword_set(called) then begin
;      generate_preview  ;;; because no visible change in amplitude!
     progress,/DESTROY
     update_info_box
  endif else progress,/destroy
;   if not keyword_set(called) then begin
;      generate_preview
;      update_info_box
;   endif

end
