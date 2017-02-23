;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: subap_extract_single
; written by    : Maxim Neumann
; last revision : 20.Sept.2005
; Extracts a single subaperture
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

pro subap_extract_single,CALLED = called
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag

; right type? --- calculate new type
  case file.type of
     600 : newtype = 101L ; scalar
     601 : newtype = 300L ; POL
     604 : newtype = 208L ; In
     605 : newtype = 500L ; POLIn
     else: begin
        error_button = DIALOG_MESSAGE(['Needs subapertures scalar or vector data'], $
                                      DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
        return
     endelse
  endcase
  n_ap = file.zdim
  ch   = channel_names
  n_ch = file.vdim

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Single subaperture extraction',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     text = widget_label(main,value='Select subaperture to extract:')
     if newtype eq 101L then $
        ch_groups = channel_names $
     else begin
        ch_groups = ['SubAp 0: '+strjoin(ch[0:(n_ch-1)],' - ')]
        for i=1,n_ap-1 do ch_groups = [ch_groups,'SubAp '+strcompress(i,/R)+': '+strjoin(ch[i*n_ch:(i+1)*n_ch-1],' - ')]
     endelse

     fld_resize = cw_bgroup(main,set_value=0,"Reduce size proportionaly to resolution loss", $
                            /nonexclusive)
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
           infotext = ['SINGLE SUBAPERTURE EXTRACTION',$
                       ' ',$
                       'RAT module written 09/2005 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
        if event.id eq fld_resize then begin
           infotext = ['Not yet implemented!']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Information')
           widget_control,fld_resize,set_value=0
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,fld_resize,GET_VALUE=resize ; one scattering mechanism
     widget_control,butt,GET_VALUE=channelvector
     widget_control,main,/destroy
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif 

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  head = 1l
  rrat,file.name,ddd,header=head,info=info,type=type
  newd   = (newtype eq 500L? 4L:(newtype eq 101L? 2L:3L))
  newv   = (newtype eq 500L? 2L:1L)
  newz   = (newtype eq 500L? file.vdim/2:file.vdim)
  newheader = [(newd eq 4?[newd,newv,newz]:(newd eq 3?[newd,newz]:[newd])),[file.xdim,file.ydim,file.var]]
  newinfo = info + ' extract:subap'+strcompress(channelvector,/R)
  srat,outputfile,eee,header=newheader,info=newinfo,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Extracting single subaperture...',/cancel_button

  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block

     if newtype eq 500L then begin
        block = reform(block[*,channelvector,*,*],[newz,newv,file.xdim,blocksizes[i]])
        block = transpose(block,[1,0,2,3])
        writeu,eee,block
     endif else $
        writeu,eee,block[*,channelvector,*,*]
  endfor
  free_lun,ddd,eee

; update file information

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = newd
  file.vdim = newv
  file.zdim = newz
  file.type = newtype
  file.info = newinfo

; generate preview

  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif

end
