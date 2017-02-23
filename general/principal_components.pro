;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: principal_componenets
; last revision : 11 Jan. 2005
; written by    : Maxim Neumann
; Principal Components Decomposition
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

;;; principal_component_analysis
pro rat_PCA, a,b, COVARIANCE=cov, EVAl=eval, EVEC=evec, NO_CALC=no_calc
  siz = size(a)
  xy  = siz[siz[0]-1] * siz[siz[0]]
  if siz[0] gt 2 then vz  = product(siz[1:siz[0]-2]) $
  else vz = 1
  a   = reform(a,vz,xy,/O)
  cov = correlate(a,/COVARIANCE)
  eval = eigenql(cov,eigenvec=evec)
  if ~no_calc then begin
     b = transpose(evec) # a
     b = reform(b,siz[1:siz[0]],/O)
  endif
  a = reform(a,siz[1:siz[0]],/O)
end

pro principal_components,CALLED = called
  common rat, types, file, wid, config

  keep_phase  = 0
  calculate_pc= 0

  if file.zdim*file.vdim le 1 then begin
     error_button = DIALOG_MESSAGE(['Data has to have multiple channels!'], $
                                   DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Principal Components Decomposition',/floating,/tlb_kill_request_events,/tlb_frame_attr)
     but_calc = CW_BGROUP(main,['Only PC info display','Calculate PC rotation'],/exclusive,/row,set_value=calculate_pc)
     text1    = WIDGET_LABEL(main,VALUE='Only the amplitude of channels is used for PC decomposition')
     if file.var eq 6 || file.var eq 9 then $
        but_pha= CW_BGROUP(main,['deleted','kept without changes'],/exclusive,/row, $
                           set_value=0,LABEL_LEFT='The phase should be: ')
     text2    = WIDGET_LABEL(main,VALUE='Attention: The computation requires the use of the whole data set at once!')
     buttons = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['PRINCIPAL COMPONENTS DECOMPOSITION',$
                       ' ',$
                       'RAT module written 2005 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     if file.var eq 6 || file.var eq 9 then $
        widget_control,but_pha,GET_VALUE=keep_phase
     widget_control,but_calc,GET_VALUE=calculate_pc
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif

; change mousepointer
  WIDGET_CONTROL,/hourglass


  if calculate_pc then begin
; undo function
     undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
     head = 1l
     rrat,file.name,ddd,header=head,info=info,type=type
     if file.var eq 6 && ~keep_phase then newvar = 4L $
     else if file.var eq 9 && ~keep_phase then newvar = 5L $
     else newvar = file.var
     if newvar eq 6 || newvar eq 9 then $
        newtype = 53L $
     else newtype = 50L
     head[head[0]+1] = newvar
     srat,outputfile,eee,header=head,info=info,type=newtype
  endif else begin
     rrat,file.name,ddd,header=head,info=info,type=type
  endelse

; calculating preview size and number of blocks
  bs = file.ydim
;   bs = config.blocksize
  overlap = 0
;   overlap = smmy / 2
  calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last
  ypos1 = 0                     ; block start
  ypos2 = bs - overlap          ; block end
  byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos
  
; pop up progress window
  progress,Message='Principal components decomposition...',/cancel_button

;start block processing
  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block
; -------- THE FILTER ----------
     rat_pca,abs(block),oblock,EVAL=eval,EVEC=evec,NO_CALC=~calculate_pc
; -------- THE FILTER ----------
     if i eq anz_blocks-1 then ypos2 = bs_last
     if calculate_pc then begin
        if keep_phase then begin
           pha    = atan(block,/phase)
           oblock = oblock * complex(cos(pha),sin(pha))
        endif
        writeu,eee,oblock[*,*,*,ypos1:ypos2-1]
     endif
     ypos1 = overlap
     point_lun,-ddd,file_pos
     point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
  endfor
  free_lun,ddd

  if	config.os eq 'windows' then newline = strcompress(13B) + string(10B)
  if	config.os eq 'unix' then newline = string(10B)
;  infostring = ['Eigenvalues: ',strcompress(eval),'Eigenvectors: ',strcompress(evec)]  
  main2 = widget_base(GROUP_LEADER=wid.base,row=6,TITLE='Principal components information',/floating,/tlb_kill_request_events,/tlb_frame_attr)
  text21 = widget_text(main2,value='Eigenvalues')
  evaltext = ''
  vzdim = file.vdim*file.zdim
  for i=0,n_elements(eval)-1 do evaltext += strcompress(i,/r)+': '+strcompress(eval[i],/r)+' ('+strcompress(eval[i]/total(eval),/r)+')'+newline
  text22 = widget_text(main2,value=evaltext,/scroll,ysize=(10<vzdim),xsize=30)
  evectext = ''
  for i=0,(size(evec,/dim))[0]-1 do for j=0,(size(evec,/dim))[1]-1 do evectext += strcompress(i,/r)+'v: '+strcompress(evec[j,i])+newline
  text23 = widget_text(main2,value='Eigenvectors')
  text24 = widget_text(main2,value=evectext,/scroll,ysize=(30<vzdim^2),xsize=30)
  buttons2 = WIDGET_BASE(main2,column=2,/frame)
  but_ok2   = WIDGET_BUTTON(buttons2,VALUE=' OK ',xsize=80,/frame)
  but_info2 = WIDGET_BUTTON(buttons2,VALUE=' Info ',xsize=60)
  WIDGET_CONTROL, main2, /REALIZE, default_button = but_canc,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main2, xoffset=pos[0], yoffset=pos[1]
;stop
  if calculate_pc then begin
     free_lun,eee
; update file information
     file.var  = newvar
     file.type = newtype
     file.name = finalfile
     file_move,outputfile,finalfile,/overwrite
; generate preview
     if not keyword_set(called) then begin
        generate_preview
        update_info_box
     endif
  endif else progress,/destroy

  repeat begin                  ; Event loop
     event = widget_event(main2)
     if event.id eq but_info2 then begin ; Info Button clicked
        infotext = ['PRINCIPAL COMPONENTS DECOMPOSITION',$
                    ' ',$
                    'RAT module written 2005 by Maxim Neumann']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main2, TITLE='Information')
     endif
  endrep until (event.id eq but_ok2) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  widget_control,main2,/destroy ; remove main widget

end
