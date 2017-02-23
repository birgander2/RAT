;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_k2m
; written by       : Maxim Neumann
; last revision    : 08/2006
; Transforms MB vectors into its covariance (or coherency) matrices
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

pro polin_k2m,CALLED=called, GUI=GUI, SMMX=smmx,SMMY=smmy
  common rat, types, file, wid, config

; check if array is usable - get new type
  case file.type of 
     500L: newtype = 510L
     501L: newtype = 511L
     502L: newtype = 512L
     503L: newtype = 513L
     else: begin
        error_button = DIALOG_MESSAGE(['Data has to be a','MB vector'] $
                                      , DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
        return
     end
  endcase

  if n_elements(smmx) eq 0 then smmx = 1 ; Default values
  if n_elements(smmy) eq 0 then smmy = 1

  if ~keyword_set(called) || keyword_set(GUI)  then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Vector -> Matrix Transform',/modal,/tlb_kill_request_events,/tlb_frame_attr)
     field1 = CW_FIELD(main,VALUE=smmx,/integer,TITLE='Presumming X   : ',XSIZE=3)
     field2 = CW_FIELD(main,VALUE=smmy,/integer,TITLE='Presumming Y   : ',XSIZE=3)
     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['VECTOR TO MATRIX TRANSFORM',$
                       ' ',$
                       'RAT module written 08/2006 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
     endrep until (event.id eq but_ok) or (event.id eq but_canc)  or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,field1,GET_VALUE=smmx ; read widget fields
     widget_control,field2,GET_VALUE=smmy
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif else begin              ; Routine called with keywords
  endelse

; Error Handling
  if smmx le 0 or smmy le 0 then begin ; Wrong box sizes ?
     error = DIALOG_MESSAGE("Boxsizes has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; calculating preview size and number of blocks
  bs = config.blocksize / file.zdim
  if bs ne bs / smmy * smmy then bs =  (bs / smmy + 1) * smmy 
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  if bs_last ne bs_last / smmy * smmy then bs_last =  bs_last / smmy * smmy
  blocksizes[anz_blocks-1] = bs_last
  if bs_last eq 0 then begin
     anz_blocks -= 1
     blocksizes = blocksizes[0:anz_blocks-1]
  endif

; calculating new dimensions
  xend = file.xdim / smmx * smmx - 1
  xnew = file.xdim / smmx
  ynew = total(blocksizes)/smmy
  dnew = file.vdim*file.zdim

; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=[4l,dnew,dnew,xnew,ynew,file.var],info=info,type=newtype
; pop up progress window
  progress,Message='Vector -> Matrix...',/cancel_button

  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return
     block  = make_array([file.vdim*file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block
     oblock = mm_xprod(block,/conj)
; presumming
     if smmx gt 1 then oblock = oblock[*,*,0:xend,*]
     oblock = complex(rebin(real_part(oblock),dnew,dnew,xnew,blocksizes[i]/smmy),rebin(imaginary(oblock),dnew,dnew,xnew,blocksizes[i]/smmy))
     writeu,eee,oblock
  endfor
  free_lun,ddd,eee

  xdim=file.xdim & ydim=file.ydim
; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.type = newtype
  file.dim  = 4l
  file.xdim = xnew
  file.ydim = ynew
  file.zdim = dnew
  file.vdim = dnew

  evolute,'Vector to matrix transform. presumming: '+strjoin(strcompress([smmx,smmy]))

; generate preview
  if ~keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy

; update connected files
  files=['fe_file','kz_file','bl_file']
  for i=0,n_elements(files)-1 do begin
     stat=get_par(files[i],filetmp)
     if stat eq 0 then begin
        rrat,filetmp,fe,head=head,type=type & free_lun,fe
        if (head[0]eq 1 && head[1]eq xdim) || $
           (head[0]eq 2 && head[1]eq xdim && head[2]eq ydim) || $
           (head[0]eq 2 && head[2]eq xdim) || $
           (head[0]eq 3 && head[2]eq xdim && head[3]eq ydim) then begin
           quest=dialog_message('Do you want simultaneously presum the connected '+files[i]+'?',/Question,dialog_parent=wid.base)
           if quest eq 'Yes' then begin
              rrat,filetmp,fe,type=type,info=info
              case 1 of
                 head[0]eq 1 && head[1]eq xdim				: fe=rebin(fe,xnew)
                 head[0]eq 2 && head[1]eq xdim && head[2]eq ydim	: fe=rebin(fe,xnew,ynew)
                 head[0]eq 2 && head[2]eq xdim				: fe=rebin(fe,head[1],xnew)
                 head[0]eq 3 && head[2]eq xdim && head[3]eq ydim	: fe=rebin(fe,head[1],xnew,ynew)
              endcase
              path = config.workdir
              sfile = cw_rat_dialog_pickfile(TITLE='Save new '+files[i], DIALOG_PARENT=wid.base, FILTER = '*.rat', $
                                             PATH=path, GET_PATH=path)
              if sfile ne '' then begin
                 srat,sfile,fe,type=type,info=info
                 if sfile ne filetmp then $
                    ignore=set_par(files[i],sfile)
              endif
           endif
        endif
     endif
  endfor


end
