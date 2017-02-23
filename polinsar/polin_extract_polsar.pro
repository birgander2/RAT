;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_extract_polsar
; written by       : Maxim Neumann
; last revision    : 08/2006
; Extracts a PolSAR image from MB image
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

; docformat = 'rst'
;+
; Extract PolSAR image from MB-PolInSAR
;
; :Keywords:
;    called: in, optional, type="flag"
;       call routine without GUI in batchmode
;    no_gui: in, optional, type="flag"
;    channelvector: in, optional, type="int"
;       Track number to extract
;    all: in, optional, type="flag"
;       Average all tracks into one PolSAR image
;    
; :Params:
;
; :Author: Maxim Neumann
; 
; :Categories: PolInSAR
;
; :Copyright:
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
;-
pro polin_extract_polsar,CALLED = called, NO_GUI=no_gui, CHANNELVECTOR=channelvector, ALL=all
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag
  compile_opt idl2

; right type? --- calculate new type
  case file.type of
     500 : newtype = 200L
     501 : newtype = 210L
     502 : newtype = 209L
     503 : newtype = 209L
     510 : newtype = 220L
     511 : newtype = 221L
     512 : newtype = 222L
     513 : newtype = 222L
     else: begin
        error_button = DIALOG_MESSAGE(['Data has to be a PolInSAR data set'], $
                                      DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
        return
     endelse
  endcase

  polin_get_info, pol=pol, tracks=n_tr, baselines=n_bl, matrix=matrix
  if n_elements(channelvector) eq 0 then channelvector = 0
  if keyword_set(ALL) then channelvector = n_tr

  if ~keyword_set(called) && ~keyword_set(no_gui) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4, $
                        TITLE='PolSAR image extraction',/modal, $
                        /tlb_kill_request_events,/tlb_frame_attr)
     text = widget_label(main,value='Select polarimetric vector to extract:')
     if file.type ge 500 && file.type le 503 then $
        ch_groups = 'Vector '+strcompress(indgen(n_tr),/R) $
     else ch_groups = 'T'+strcompress(indgen(n_tr)+1,/R)+strcompress(indgen(n_tr)+1,/R)
     ch_groups = [ch_groups, 'Average all']
     butt = cw_bgroup(main,ch_groups,/exclusive,column=1,SET_VALUE=0)
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
           infotext = ['POLSAR IMAGE EXTRACTION FROM MB-SAR IMAGE',$
                       ' ',$
                       'RAT module written 08/2006 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) $
        or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,butt,GET_VALUE=channelvector
     widget_control,main,/destroy
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  if matrix then $
     srat,outputfile,eee,header=[4l,pol,pol,file.xdim,file.ydim,file.var], $
          info=info,type=newtype $
  else $
     srat,outputfile,eee,header=[3l,pol,file.xdim,file.ydim,file.var], $
          info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Extracting polsar image...',/cancel_button

; calculating span
  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block

     if channelvector eq n_tr then begin ; average all
        if matrix then begin
           for tr=1, n_tr-1 do $
              block[0:pol-1, 0:pol-1, *, *] += block[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1,*,*]
           writeu,eee,block[0:pol-1,0:pol-1,*,*]/n_tr
        endif else writeu,eee,total(block, 2)/n_tr
     endif else if matrix then $
        writeu,eee,block[channelvector*pol:channelvector*pol+pol-1,channelvector*pol:channelvector*pol+pol-1,*,*] $
     else $
        writeu,eee,block[*,channelvector,*,*]
  endfor
  free_lun,ddd,eee

; update file information

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  if matrix then begin
     file.dim  = 4l
     file.vdim = pol
     file.zdim = pol
  endif else begin
     file.dim  = 3l
     file.vdim = 1l
     file.zdim = pol
  endelse
  file.type = newtype
  n_tr_new = 1
  pol_new  = pol
  ignore = set_par('polarizations',pol_new)
  ignore = set_par('nr_tracks',n_tr_new)
  evolute,'Extract POLSAR image from MB-PolInSAR data: '+(channelvector eq n_tr?'average all '+strcompress(n_tr, /r)+ $
                                                          ' tracks':'track '+strcompress(channelvector, /r))

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else $
     channel_default

end
