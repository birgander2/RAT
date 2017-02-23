;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_extract_insar
; written by       : Maxim Neumann
; last revision    : 08/2006
; Extracts a InSAR image from MB image
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

pro polin_extract_insar,CALLED = called,channelvector=channelvector
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag

; right type? --- calculate new type
  if file.type ge 500 && file.type le 503 then newtype = 300L $
  else begin
     error_button = DIALOG_MESSAGE(['Data has to be a Multibaseline scattering vector'], $
                                   DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endelse

  polin_get_info, tracks=n_tr, baselines=n_bl, pol=pol, matrix=is_matrix

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5, $
                        TITLE='InSAR image extraction',/modal, $
                        /tlb_kill_request_events,/tlb_frame_attr)
     text = widget_label(main,value='Select an interferometric pair to extract:')
     tmp  = strcompress(indgen(n_tr)+1,/R)
     ch_groups = 'Tracks 1x'+tmp
     for i=2,n_tr do ch_groups=[ch_groups,'Tracks '+strcompress(i,/R)+'x'+tmp]
     for i=0,n_tr-1 do ch_groups[i*n_tr+i]='          '
;     ch_groups = transpose(reform(ch_groups,n_tr,n_tr))

     butt     = cw_bgroup(main,ch_groups,/exclusive,row=n_tr,SET_VALUE=1, $
                          label_left='Choose combination of tracks')
     buttpol  = cw_bgroup(main,['Pol 1','Pol 2','Pol 3'],/excl,set_value=0, $
                          label_left='Choose polarization:')
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
           infotext = ['INSAR IMAGE EXTRACTION FROM MB-POLINSAR IMAGE',$
                       ' ',$
                       'RAT module written 08/2006 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
        if event.id eq butt then begin
           widget_control,butt,get_value=ch
           if ch/n_tr eq ch mod n_tr then $
              widget_control,butt,set_value=1
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) $
        or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,butt,GET_VALUE=channelvector
     widget_control,buttpol,GET_VALUE=chosen_pol
     widget_control,main,/destroy
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif else $
     if n_elements(channelvector) eq 0 then channelvector = 1

  channelvector=[channelvector/n_tr,channelvector mod n_tr]

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=[3L,2L,file.xdim,file.ydim,file.var], $
       info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Extracting insar image...',/cancel_button

  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block

     writeu,eee,block[chosen_pol,channelvector,*,*]
  endfor
  free_lun,ddd,eee

; update file information

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = 3l
  file.vdim = 1l
  file.zdim = 2L
  file.type = newtype

  n_tr_new = 2
  pol_new  = 1
  ignore = set_par('polarizations',pol_new)
  ignore = set_par('nr_tracks',n_tr_new)
  evolute,'Extract InSAR image'

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif

end
