;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_rgflt_adaptive
; last revision : 08/2006
; written by    : Maxim Neumann
; Multibaseline range filter (adaptive)
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

pro polin_rgflt_adaptive,CALLED = called, FE=FE, BANDWIDTH=bw, SAMPLING=rs, CONJ_FE=conj_fe
  common rat, types, file, wid, config

  if ~(file.type ge 500 && file.type le 503) then begin
     error = DIALOG_MESSAGE("This is not a multibaseline scattering vector", DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,matrix=is_matrix

  but1=1
  but2=1
  but3=1
  alpha1=0.54
  alpha2=0.54
  if n_elements(conj_fe) eq 0 then conj_fe = 0
  if n_elements(bw) eq 0 then bw=100.
  if n_elements(rs) eq 0 then rs=100.

  if ~keyword_set(called) then begin ; Graphical interface
     fe   = fltarr(file.xdim,n_tr)
     single_file = 1
     files = strarr(n_TR)
     lines = lonarr(n_TR)
     labels= lonarr(n_TR)
     texts = lonarr(n_TR)
     brows = lonarr(n_TR)
     ignore  = get_par('fe_file',fe_file)
     if ignore eq 0 && file_test(fe_file,/READ) then files[0]=fe_file

     main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Multibaseline adaptive spectral range filtering',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     
     text1 = CW_FIELD(main,VALUE=100.0,/floating,XSIZE=6,TITLE='Range bandwidth          [MHz] :')
     text2 = CW_FIELD(main,VALUE=100.0,/floating,XSIZE=6,TITLE='Range sampling frequency [MHz] :')
;     draw1 = widget_draw(main,XSIZE=300,ysize=200)
;     but_br = WIDGET_BUTTON(main,VALUE=' Select flat-earth file ',/frame)

     subFE = widget_base(main,/column,/frame)
     but_ch = cw_bgroup(subFE,['multiple files with flat earth phases','single multi-track file with flat earth phases'], $
                        label_left='Use ',/excl,set_value=single_file,/col)
     for i=0,n_tr-1 do begin
        lines[i] = WIDGET_BASE(subFE,column=3)
        labels[i]= WIDGET_LABEL(lines[i],VALUE='Flat-earth file in range '+(i ne 0 || ~single_file?strcompress(i,/R):' ')+': ')
        texts[i] = CW_FIELD(lines[i],VALUE=files[i],/string,XSIZE=50,/noedit,title='')
;         texts[i] = CW_FIELD(lines[i],VALUE=files[i],/string,XSIZE=50, $
;                             TITLE='Flat-earth file in range '+strcompress(i,/R)+':')
        brows[i] = WIDGET_BUTTON(lines[i],VALUE=' browse ',ysize=35)
     endfor
;      for i=0,n_tr-1 do begin
;         lines[i] = WIDGET_BASE(subFE,column=3)
;         texts[i] = CW_FIELD(lines[i],VALUE=files[i],/string,XSIZE=50, $
;                             TITLE='Flat-earth file '+strcompress(i,/R)+':')
;         brows[i] = WIDGET_BUTTON(lines[i],VALUE=' browse ',ysize=35)
;      endfor
     ConjButton = cw_bgroup(subFE,set_value = 0,['Conjugate Complex Phases  '],/nonexclusive)

     sub0 = widget_base(main,row=4,/frame)
     sub1 = widget_base(sub0,/row)
     RemHamButton = cw_bgroup(sub1,set_value = 1,['Remove Hamming   '],/nonexclusive)
     RemHamValText = cw_field(sub1,value=0.54,title=' ',/float,xsize=5)
     sub2 = widget_base(sub0,/row)
     AddHamButton = cw_bgroup(sub2,set_value = 1,['Apply New Hamming'],/nonexclusive)
     AddHamValText = cw_field(sub2,value=0.54,title=' ',/float,xsize=5)
     sub3 = widget_base(sub0,/row)
     RemFeButton = cw_bgroup(sub3,set_value = 1,['Remove Flatearth Phase'],/nonexclusive)

     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
     new_event={ID:0L,TOP:0L,HANDLER:0L}
     WIDGET_CONTROL, but_ch, /NO_COPY, SEND_EVENT=new_event

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
           infotext = ['MULTIBASELINE ADAPTIVE SPECTRAL RANGE FILTER',$
                       ' ',$
                       'RAT module written by A. Reigber & M. Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
        path = config.workdir
        for i=0,n_tr-1 do if event.id eq brows[i] then begin
           files[i] = DIALOG_PICKFILE(TITLE='Open RAT file', DIALOG_PARENT=main, FILTER = '*.rat', /MUST_EXIST, PATH=path)
           if file_test(files[i],/read) then begin
              rrat,files[i],ddd,head=head
              free_lun,ddd
              var = head[n_elements(head)-1] & siz=head
              if (~single_file && (siz[0] ne 1 || siz[1] ne file.xdim)) || $
                 ( single_file && total(siz[0:2] eq [2,n_tr,file.xdim])ne 3) then begin
                 error = DIALOG_MESSAGE("Array size does not correspond " + $
                                        "to image size", $
                                        DIALOG_PARENT = wid.base, TITLE='Error',/error)
                 files[i]=''
              endif
;            if head[1] ne file.xdim || (var ne 4 && var ne 5) then begin
;               info = DIALOG_MESSAGE('The x-dimension or the type do not match with the data', DIALOG_PARENT = main, TITLE='Error')
;               files[i]=''
;            endif
              widget_control,texts[i],set_value=files[i]
           endif
        endif
        
     endrep until (event.id eq but_ok && total(files eq '')le 1) || (event.id eq but_ok && single_file && files[0] ne '') || $
        (event.id eq but_canc) || tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     if ~((event.id eq but_canc) || $
          tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST') $
     then begin
        widget_control,text1,get_value=bw
        widget_control,text2,get_value=rs
        widget_control,RemHamButton,get_value=but1
        widget_control,AddHamButton,get_value=but2
        widget_control,RemFeButton,get_value=but3
        widget_control,RemHamValText,get_value=alpha1
        widget_control,AddHamValText,get_value=alpha2
        widget_control,ConjButton,get_value=conj_fe

        fe    = fltarr(n_tr,file.xdim) ; extend later to 2d-fe
        if single_file then begin
           rrat,files[0],fe
           if size(fe,/n_dim) ne 2 then $
              fe = fe[*,*,0,0,0,0,0]
        endif else for i=0,n_tr-1 do if file_test(files[i]) then begin
           rrat,files[i],fetmp
           fe[i,*] = fetmp[*,0]
        endif & fetmp=0
        fe =transpose(fe)

;      for i=0,n_tr-1 do begin
;         widget_control,texts[i],get_value=tmp
;         files[i]=tmp
;         if files[i] ne '' then begin
;            rrat,files[i],fetmp
;            fe[*,i] = fetmp[*,0]
;         endif
;      endfor & fetmp=0
     endif

     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
     
  endif else begin
     if n_elements(fe) eq 0 then begin
        ignore  = get_par('fe_file',fe_file)
        if ignore eq 0 && file_test(fe_file,/READ) then rrat,fe_file,fe
     endif
     if n_elements(fe) eq 0 then $
        message, "Please provide an appropriate Flat Earth file!" 
  endelse

; calculate parameters
  if conj_fe then fe = -fe
  if array_equal(size(fe, /dim), [n_tr, file.xdim]) then fe = transpose(fe)
  dfe = fe
  for i=0,n_tr-1 do dfe[*,i]=deriv(fe[*,i])
  cut = floor(max(abs(dfe))/2/!pi*file.xdim)
  cor = complex(cos(fe),sin(fe))
  empty = floor((1-bw/rs)*file.xdim)   ; empty spectral range (in pixels)
  cut += empty

;   cut = floor(max(abs(deriv(fe)))/2/!pi*file.xdim)
;   cor = exp(complex(0,float(fe/2)))
;   empty = (1-bw/rs)*file.xdim   ; empty spectral range (in pixels)
;   cut += empty

  flt = fltarr(file.xdim)+1.0
  if cut ge 1 then begin
     flt[0:cut/2-1] = 0.0
     flt[file.xdim-cut/2:*] = 0.0
  endif
  flt = shift(flt,file.xdim/2)

  ham = 1 / hamming(file.xdim-empty,total=file.xdim,alpha=alpha1)
  rmnanq,ham

  if but2 eq 1 then flt *= hamming(file.xdim-cut,total=file.xdim,alpha=alpha2)
;	stop

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=head,info=info,type=type

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Multibaseline spectral range filtering...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin ; normal blocks
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block

; -------- THE FILTER ----------
     for p=0,file.vdim-1 do $                ;; polarization
        for j=0,file.zdim-1 do $             ;; track
           for k=0,blocksizes[i]-1 do begin  ;; azimuth
        if but1 eq 1 then line = fft(fft(reform(block[p,j,*,k]),-1) * ham,+1) else line = reform(block[p,j,*,k])
        block[p,j,*,k] = fft(fft(line * conj(cor[*,j]),-1)*flt,+1)
        if but3 eq 0 then block[p,j,*,k] *= cor[*,j]
     endfor
; -------- THE FILTER ----------

     writeu,eee,block 
  endfor
  free_lun,ddd,eee

; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile

  evolute,'Multibaseline adaptive range spectral filtering. Cut '+strcompress(cut,/R)+' pixels im spectrum (from '+strcompress(file.xdim,/R)+').'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
