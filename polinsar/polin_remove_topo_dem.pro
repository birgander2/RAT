;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_remove_topo_dem
; 
; written by    : Maxim Neumann
; last revision : 14. January 2008
;------------------------------------------------------------------------
; Removes the topography with the help of a given DEM (in slant range)
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


pro polin_remove_topo_dem,CALLED=called, dem=dem, kz=kz, file_dem=file_dem, file_kz=file_kz,  conj_pha=conj_pha
  common rat, types, file, wid, config
  compile_opt idl2

  if ~((file.type ge 500 && file.type le 503)) then begin
     error_button = DIALOG_MESSAGE('Wrong input data type. Needs PolInSAR scattering vectors.', DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,matrix=is_matrix

  show_intrf = 0
  if n_elements(file_dem) eq 0 then file_dem = ''
  if n_elements(file_kz) eq 0 then file_kz = ''
  if n_elements(conj_pha) eq 0 then conj_pha = 0
  
  if ~keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,/col, $
                        TITLE='Topography Removal with a DEM',/modal,/tlb_kill_request_events,/tlb_frame_attr)
     
     lines1 = WIDGET_BASE(main,column=3)
     labels1= WIDGET_LABEL(lines1,VALUE='DEM file (2D=[X;Y], float)         : ')
     texts1 = CW_FIELD(lines1,VALUE=file_dem,/string,XSIZE=50,/noedit,title='')
     brows1 = WIDGET_BUTTON(lines1,VALUE=' browse ',ysize=35)
     
     lines2 = WIDGET_BASE(main,column=3)
     labels2= WIDGET_LABEL(lines2,VALUE='Kz file (3D=[Datasets;X;Y], float): ')
     texts2 = CW_FIELD(lines2,VALUE=file_kz,/string,XSIZE=50,/noedit,title='')
     brows2 = WIDGET_BUTTON(lines2,VALUE=' browse ',ysize=35)
     
     conjButton = cw_bgroup(main,set_value = conj_pha,['Conjugate complex phases  '],/nonexclusive)
     intrfpha=cw_bgroup(main,"Generate interferometric phases afterwards",/nonexclusive,set_value=[show_intrf])
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
           infotext = ['TOPOGRAPHY REMOVAL WITH DEM',$
                       ' ',$
                       'RAT module written 2008 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Information')
        endif
        if event.id eq brows1 then begin
           path = config.workdir
           file_dem = DIALOG_PICKFILE(TITLE='Provide the DEM file',DIALOG_PARENT=wid.base, $
                                      FILTER = '*.rat',PATH=path, GET_PATH=path)
           if strlen(file_dem) gt 0 then begin
              config.workdir = path
              widget_control,texts1,set_value=file_dem
           endif
        endif
        if event.id eq brows2 then begin
           path = config.workdir
           file_kz = DIALOG_PICKFILE(TITLE='Provide the KZ file',DIALOG_PARENT=wid.base, $
                                      FILTER = '*.rat',PATH=path, GET_PATH=path)
           if strlen(file_kz) gt 0 then begin
              config.workdir = path
              widget_control,texts2,set_value=file_kz
           endif
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,intrfpha,GET_VALUE=show_intrf
     widget_control,conjButton,GET_VALUE=conj_pha
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

  if n_elements(file_dem) ne 0 &&  file_test(file_dem) then $
     rrat, file_dem, dem
  if n_elements(file_kz) ne 0 &&  file_test(file_kz) then $
     rrat, file_kz, kz

  if ~array_equal(size(kz, /dim), [n_tr, file.xdim, file.ydim]) then Message, 'ERROR: the sizes for the kz-file do not match. Requires [n_tr,xdim,ydim].'
  if ~array_equal(size(dem, /dim), [file.xdim, file.ydim]) then Message, 'ERROR: the sizes for the dem-file do not match. Requires [xdim,ydim].'
  cp = conj_pha? -1: 1


; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  info += ' dem-toporm'
  srat,outputfile,eee,header=head,info=info,type=type

; calculating preview size and number of blocks
  bs = config.blocksize / file.vdim
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
   progress,Message='Removing tomographic phase...',/cancel_button

;start block processing
   for i=0,anz_blocks-1 do begin ; normal blocks
      progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
      if wid.cancel eq 1 then return

      block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]], $
                         type=file.var,/NOZERO)
      readu,ddd,block
; -------- THE FILTER ----------
      if ~is_matrix then $
         for tr=0, n_tr-1 do $
            for ch=0, pol-1 do $
               block[ch, tr, *, *] *= exp(-complex(0, cp)*(kz[tr, *, i*bs:i*bs+blocksizes[i]-1] $
                                                         *dem[*, i*bs:i*bs+blocksizes[i]-1]))
; -------- THE FILTER ----------
      if i eq anz_blocks-1 then ypos2 = bs_last
      writeu,eee,block
   endfor
   free_lun,ddd,eee

; update file information

   file_move,outputfile,finalfile,/overwrite

   file.info = info
   file.name = finalfile

   evolute,'DEM topographic phase removed.'

; generate preview
   if show_intrf then polin_interf_pha $
   else if ~keyword_set(called) then begin
;      generate_preview  ;;; because no visible change in amplitude!
      progress,/DESTROY
      update_info_box
   endif

end
