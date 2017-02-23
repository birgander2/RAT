;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_basis_indiv
; written by       : Maxim Neumann
; last revision    : 08/2006
; Individual basis change, pixel per pixel
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
; Individual basis change, pixel per pixel
;
; :Keywords:
;    called: in, optional, type=flag
;       
;    sm_file: in, optional, type=string
;       Name of the file, which contains the scattering mechanisms
;    sb_extract: in, optional, type=int
;       Index of the demanded baseline
; 
; :Author: Maxim Neumann
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
pro polin_basis_indiv,CALLED = called, SM_FILE=sm_file, SB_EXTRACT=SB_EXTRACT
  common rat, types, file, wid, config
  compile_opt idl2

  if ~(file.type ge 510 && file.type le 513) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,matrix=is_matrix

  newtype = file.type
  if (newtype mod 10) lt 2 then newtype += 2


  if n_elements(sm_file) eq 0 then $
     sm_file = automatic_file_select(path=file_dirname(file.name,/M), $
                                     choice='sm_'+['mb_msm_','mb_esm_','sb_msm_','sb_esm_', $
                                                   'mb_','sb_','msm_','esm_',''] $
                                     +file_basename(file.name),CASE_SENSITIVE=0)

  if ~keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4, $
                        TITLE='Individual pixel per pixel basis transformation',/modal,/tlb_kill_request_events,/tlb_frame_attr)
     line1 = WIDGET_BASE(main,column=2)
     text1 = CW_FIELD(line1,VALUE=sm_file,/string,XSIZE=60,TITLE='Matrix of scattering mechanism vectors :')
     brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)
;     cohcompl=cw_bgroup(main," complex coherence",/nonexclusive,set_value=coh_complex)
     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main,/REALIZE,default_button=but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['Individual pixel per pixel basis transformation',$
                       ' ',$
                       'RAT module written by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Information')
        endif
        if event.id eq brow1 then begin ; Info Button clicked
           path = config.workdir
           sm_file = DIALOG_PICKFILE(TITLE='Provide target file for scattering mechanisms',DIALOG_PARENT=wid.base, $
                                     FILTER = '*.rat',/MUST_EXIST, PATH=path, GET_PATH=path)
           if strlen(sm_file) gt 0 then begin
              config.workdir = path
              widget_control,text1,set_value=sm_file
           endif
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,text1,get_value=sm_file
                                ;    widget_control,cohcompl,GET_VALUE=coh_complex
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

  if ~file_test(sm_file) then begin
     err= dialog_message(dialog_parent=wid.base,/error,['File '+sm_file+' does not exist or is not readable!','Return to main program!'])
     return
  endif

;;; testing the dimensions of sm_file
  bl = 0
  rrat,sm_file,fff,header=sm_head
  free_lun,fff
  if sm_head[0] eq 6 && n_tr gt 2 then begin
     if  n_elements(sb_extract) gt 0 && n_elements(sb_extract) lt n_bl then begin
        bl = sb_extract
        blx = mb_ind(bl)
     endif else begin
        err = dialog_message(['It is not possible and makes no sense to', $
                              'transform multibaseline datasets with ', $
                              'single-baselin optimized scattering mechanism vectors!', $
                              '', $
                              'Do you want to extract one single-baseline dataset?'], $
                             /question)
        if err eq 'No' then return
        main = WIDGET_BASE(GROUP_LEADER=wid.base,/column, $
                           TITLE='Choose baseline',/modal, $
                           /tlb_kill_request_events,/tlb_frame_attr)
        tmp  = strcompress(indgen(n_tr)+1,/R)
        ch_groups = '1x'+tmp
        for i=2,n_tr do ch_groups=[ch_groups,strcompress(i,/R)+'x'+tmp]
        for i=0,n_tr-1 do ch_groups[i*n_tr+i]='   '
;     ch_groups = transpose(reform(ch_groups,n_tr,n_tr))
        blx = 1
        text = widget_label(main,value='Choose the combination of tracks' + $
                            ' for the extracted baseline:')
        butt     = cw_bgroup(main,/frame,ch_groups,/exclusive,row=n_tr,SET_VALUE=blx)
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
              infotext = ['PolDInSAR - Individual pixel per pixel basis transformation',$
                          ' ',$
                          'RAT module written 06/2007 by Maxim Neumann']
              info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
           endif
           if event.id eq butt then begin
              widget_control,butt,get_value=ch
              if ch/n_tr ge ch mod n_tr then $
                 widget_control,butt,set_value=blx $
              else blx = ch
           endif
        endrep until (event.id eq but_ok) or (event.id eq but_canc) $
           or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
        widget_control,butt,GET_VALUE=blx
        widget_control,main,/destroy
        if event.id ne but_ok then return ; OK button _not_ clicked
        blx = [blx/n_tr, blx mod n_tr]
        bl  = mb_ind(blx[0],blx[1])
     endelse
  endif

; change mousepointer
     WIDGET_CONTROL,/hourglass

; undo function
     undo_prepare,outputfile,finalfile,CALLED=CALLED


; read / write header
     rrat,file.name,ddd,header=head,info=info,type=type
     if sm_head[0] ne 6 then $
        srat,outputfile,eee,header=head,info=info,type=newtype $
     else $ ;; single--baseline optimized
        srat,outputfile,eee,header=[4L,pol*[2L,2L],head[3:*]],info=info,type=newtype
     rrat,sm_file,fff,header=sm_head
     

; calculating preview size and number of blocks
     bs = config.blocksize / file.vdim
     calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
     blocksizes = intarr(anz_blocks)+bs
     blocksizes[anz_blocks-1] = bs_last

; pop up progress window
     progress,Message='Individual basis transformation...',/cancel_button

;start block processing
     for i=0,anz_blocks-1 do begin ; loop normal blocks
        progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
        if wid.cancel eq 1 then return

        block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]], $
                           type=file.var,/nozero)
        readu,ddd,block
        SM= complexarr([sm_head[1:sm_head[0]-1],blocksizes[i]],/nozero)
        if sm_head[0] eq 6 then $
           U = make_array([pol*[2,2],file.xdim,blocksizes[i]], $
                          type=file.var) $
        else $
           U = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]], $
                          type=file.var)
        readu,fff,sm
; -------- THE FILTER ----------
        case sm_head[0] of
           6: begin
              if n_tr gt 2 then $
                 block = mb_sb(block,bl, pol=pol)
              for tr=0,1 do $
                 if sm_head[3] eq 1 then $
                    U[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1,*,*] = sm[*,*,0,bl,*,*] $
                 else $
                    U[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1,*,*] = sm[*,*,tr,bl,*,*]
           end
           5: for tr=0,n_tr-1 do $
              U[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1,*,*] = sm[*,*,tr,*,*]
           4: for tr=0,n_tr-1 do $
              U[tr*pol:tr*pol+pol-1,tr*pol:tr*pol+pol-1,*,*] = sm
           else: stop
        endcase
        sm = -1
        block = mm_mm(mm_mm(mm_herm(U),block),U)
;     block = mm_mm(mm_mm(U,block),mm_herm(U))
; -------- THE FILTER ----------
        writeu,eee, block
     endfor
     free_lun,ddd,eee,fff

; update file information
     file_move,outputfile,finalfile,/overwrite
     file.name = finalfile
     file.type = newtype
     if sm_head[0] eq 6 then begin
        file.zdim = pol*2
        file.vdim = pol*2
        err=set_par('nr_tracks',2)
     endif
     

     evolute,'Multibaseline individual basis transformation.'

; generate preview
     if not keyword_set(called) then begin
        generate_preview
        update_info_box
     endif
  end
