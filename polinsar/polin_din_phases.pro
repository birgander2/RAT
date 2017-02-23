;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_din_phases
; written by       : Maxim Neumann
; last revision    : 06/2007
; Differential phases
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

pro polin_din_phases,CALLED = called, kz_file=kz_file, heights=heights
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag


; right type? --- calculate new type
  if file.type ge 510 && file.type le 513 then newtype = (keyword_set(heights)?4L:52L) $
  else begin
     error_button = DIALOG_MESSAGE(['Data has to be a MB-POLInSAR data set!'], $
                                   DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endelse

  polin_get_info, tracks=n_tr, baselines=n_bl, pol=pol, matrix=is_matrix, use_kz=use_kz, kz_file=kz_linked
  if n_elements(kz_file) eq 0 then kz_file = kz_linked

  if n_tr lt 3 then begin
     error_button = DIALOG_MESSAGE(['PolDInSAR applications require at least 3 tracks!'], $
                                   DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif


  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=6, $
                        TITLE='PolDInSAR phases',/modal, $
                        /tlb_kill_request_events,/tlb_frame_attr)
     
     text = widget_label(main,value='Choose two different baselines to obtain the differential phase:')
     tmp  = strcompress(indgen(n_tr)+1,/R)
;      ch_groups = 'Tracks 1x'+tmp
;      for i=2,n_tr do ch_groups=[ch_groups,'Tracks '+strcompress(i,/R)+'x'+tmp]
     ch_groups = '1x'+tmp
     for i=2,n_tr do ch_groups=[ch_groups,strcompress(i,/R)+'x'+tmp]
     for i=0,n_tr-1 do ch_groups[i*n_tr+i]='          '
;     ch_groups = transpose(reform(ch_groups,n_tr,n_tr))

     blx1 = 1 & blx2 = 2
     butt     = cw_bgroup(main,/frame,ch_groups,/exclusive,row=n_tr,SET_VALUE=blx1, $
                          label_left='Choose combination of tracks for baseline 1: ')
     butt2    = cw_bgroup(main,/frame,ch_groups,/exclusive,row=n_tr,SET_VALUE=blx2, $
                          label_left='Choose combination of tracks for baseline 2: ')
     buttpol  = cw_bgroup(main,/frame,['1','2','3'],/excl,set_value=0, $
                          label_left='Choose polarization channel:')
     if keyword_set(HEIGHTS) then begin
        main2 = widget_base(main,/column)
        line1 = WIDGET_BASE(main2,column=2)
        text1 = CW_FIELD(line1,VALUE=kz_file,/string,XSIZE=60,TITLE='kz file :') ;,noedit=~sms)
        brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)
     endif
     buttons  = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
     
     repeat begin
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['PolDInSAR - Differential interferometric phases and heights',$
                       ' ',$
                       'RAT module written 06/2007 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
        if event.id eq butt then begin
           widget_control,butt,get_value=ch
           if ch/n_tr eq ch mod n_tr then $
              widget_control,butt,set_value=blx1 $
           else blx1 = ch
        endif
        if event.id eq butt2 then begin
           widget_control,butt2,get_value=ch
           if ch/n_tr eq ch mod n_tr then $
              widget_control,butt2,set_value=blx2 $
           else blx2 = ch
        endif
        if keyword_set(HEIGHTS) && event.id eq brow1 then begin ; Info Button clicked
           path = config.workdir
           kz_file = DIALOG_PICKFILE(TITLE='Provide kz file for this data set',DIALOG_PARENT=wid.base, $
                                     FILTER = '*.rat',/MUST_EXIST, PATH=path, GET_PATH=path)
           if strlen(kz_file) gt 0 then begin
              config.workdir = path
              widget_control,text1,set_value=kz_file
           endif
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) $
        or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,butt,GET_VALUE=blx1
     widget_control,butt2,GET_VALUE=blx2
     widget_control,buttpol,GET_VALUE=chosen_pol
     if keyword_set(HEIGHTS) then $
        widget_control,text1,get_value=kz_file
     widget_control,main,/destroy
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif
  blx1 = [blx1/n_tr, blx1 mod n_tr]
  blx2 = [blx2/n_tr, blx2 mod n_tr]

  if array_equal(blx1,blx2) then begin
     error_button = DIALOG_MESSAGE(['Please choose two different baselines!'], $
                                   DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

  if keyword_set(heights) && ~file_test(kz_file) then begin
     error_button = DIALOG_MESSAGE(['Please provide a valid kz file for all baselines (dimensions: [n_tr,x,y])!'], $
                                   DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif 

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=[2L,file.xdim,file.ydim,4L], $
       info=info,type=newtype
  if keyword_set(heights) then $
     rrat,kz_file,fff,header=kzhead

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  if keyword_set(heights) then $
     progress,Message='Extracting differential heights...',/cancel_button $
  else $
     progress,Message='Extracting differential phases...',/cancel_button

  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var,/nozero)
     readu,ddd,block
     if keyword_set(heights) then begin
        kz     = make_array([kzhead[1:kzhead[0]-1],blocksizes[i]],type=4,/nozero)
        readu,fff,kz
        writeu,eee,reform(atan(block[blx2[0]*pol+chosen_pol,blx2[1]*pol+chosen_pol,*,*],/phase)) $
               /reform(kz[blx2[1],*,*]-kz[blx2[0,*,*]]) - $
               reform(atan(block[blx1[0]*pol+chosen_pol,blx1[1]*pol+chosen_pol,*,*],/phase)) $
               /reform(kz[blx1[1],*,*]-kz[blx1[0,*,*]])
     endif else $
        writeu,eee,atan(block[blx2[0]*pol+chosen_pol,blx2[1]*pol+chosen_pol,*,*],/phase) - $
               atan(block[blx1[0]*pol+chosen_pol,blx1[1]*pol+chosen_pol,*,*],/phase)
  endfor
  free_lun,ddd,eee
  if keyword_set(heights) then free_lun, fff

; update file information

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = 2l
  file.vdim = 1l
  file.zdim = 1L
  file.var  = 4L
  file.type = newtype

  evolute,'Extract differential phases'

; generate preview
  if ~keyword_set(called) then begin
     generate_preview
     update_info_box
  endif

end
