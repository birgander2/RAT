;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : polin_analysis_pol.pro
; Author        : Maxim Neumann
; Modified      : 09/2006
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

pro polin_analysis_pol
  common rat
  common mb_tracks, pos, pol, n_tr, n_bl, is_coherency, newline, $
     gui, gsiz, pnt, pnt_save, $
     DPar,DPar_range,DPar_labels,Dcomb_labels,Dlabels,Dpar_offsets,Dsmooth,Dinfo,Dwin,Dtr_choice, Dmath



;;; initialization
  if file.type ne 511 && file.type ne 510 && file.type ne 220 && file.type ne 221 then begin
     error = DIALOG_MESSAGE(["This is wrong data type.",'Needs a covariance/coherency matrix [C/T]'], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  is_coherency = file.type eq 511 || file.type eq 221
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl,use_kz=use_kz,kz_file=kz_file,use_bl=use_bl,bl_file=bl_file
  if pol eq 4 then $
     if 'Yes' eq dialog_message(['This application expects symmectric cross-pol channels.', $
                                 'Proceed with cross-pol symmetrisation?'],dialog_parent=wid.base,title='Question',/q) $
     then begin
     calib_xsym
     pol = 3L
  endif else return
  if use_kz then begin
     rrat,kz_file,ddd,header=head & free_lun,ddd
     if head[0] ne 3 || head[1] ne n_tr || head[2] ne file.xdim || head[3] ne file.ydim then use_kz=0
  endif
  if use_bl then begin
     rrat,bl_file,ddd,header=head & free_lun,ddd
     if head[0] ne 3 || head[1] ne n_tr || head[2] ne file.xdim || head[3] ne file.ydim then use_bl=0
  endif
  newline=config.os eq 'unix'? string(10b): string(13b)+string(10b)

;;;; VARIABLES and CONSTANTS ;;;;
  newpos_type = 0

;;;; ---- INITIAL OPTIONS ---- ;;;;
  main = WIDGET_BASE(GROUP_LEADER=wid.base,row=6, $
                     TITLE='Multibaseline Track Parameter Analysis - Start',/modal,/tlb_kill_request_events,/tlb_frame_attr)
  start_label = WIDGET_LABEL(main,VALUE='Click in the image to choose the position!')
  but_newpos_type = CW_BGROUP(main,['Pick new position', $
                                    'Enter new position by hand'], $
                              SET_VALUE=newpos_type,/EXCLUSIVE,/ROW)
  buttons = WIDGET_BASE(main,/row,/BASE_ALIGN_CENTER,XSIZE=400)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  WIDGET_CONTROL, main,/REALIZE,default_button=but_ok
  repeat begin
     event = widget_event(main)
     if event.id eq but_info then $
        info = DIALOG_MESSAGE(['MULTIBASELINE TRACK PARAMETER ANALYSIS',' ',$
                               'RAT module written 09/2006 by Maxim Neumann'], $
                              DIALOG_PARENT = main,/INFORMATION)
  endrep until event.id eq but_ok || event.id eq but_canc or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
  WIDGET_CONTROL,but_newpos_type,GET_VALUE=newpos_type
  widget_control,main,/destroy  ; remove main widget
  if event.id ne but_ok then return ; OK button _not_ clicked

; get position
  if newpos_type eq 1 then begin
     xpos = file.xdim/2 & ypos=file.ydim/2
     main = WIDGET_BASE(GROUP_LEADER=(wid.base),row=6, $
                        TITLE='Enter the position for the MB-analysis',/modal,/tlb_kill_request_events,/tlb_frame_attr)
     fld_posX = CW_FIELD(main,VALUE=xpos,/integer,XSIZE=5, $
                         TITLE='X in [0..'+strcompress(file.xdim-1,/R)+']: ')
     fld_posY = CW_FIELD(main,VALUE=ypos,/integer,XSIZE=5, $
                         TITLE='Y in [0..'+strcompress(file.ydim-1,/R)+']: ')
     buttons = WIDGET_BASE(main,/row,/BASE_ALIGN_CENTER,XSIZE=400)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     but_newpos = WIDGET_BUTTON(buttons,VALUE=' Pick new position ')
     WIDGET_CONTROL, main,/REALIZE,default_button=but_ok
     repeat begin
        event = widget_event(main)
        if event.id eq but_info then $
           info = DIALOG_MESSAGE(['MULTIBASELINE TRACK PARAMETER ANALYSIS',' ',$
                                  'RAT module written 09/2006 by Maxim Neumann'], $
                                 DIALOG_PARENT = main,/INFORMATION)
        if event.id eq fld_posX then begin
           widget_control,fld_posX,get_value=xpos
           xpos = (0>xpos)<(file.xdim-1)
           widget_control,fld_posX,set_value=xpos
        endif
        if event.id eq fld_posY then begin
           widget_control,fld_posY,get_value=ypos
           ypos = (0>ypos)<(file.ydim-1)
           widget_control,fld_posY,set_value=ypos
        endif
     endrep until (event.id eq but_ok)|| event.id eq but_canc || $
        (event.id eq but_newpos) || (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     if event.id eq but_newpos then newpos_type = 0
     pos = [xpos,ypos]
     widget_control,main,/destroy ; remove main widget
  endif
  if newpos_type eq 0 then begin
     pos = get_rat_draw_position()
  endif






;;; Data variables and constants
  Dcomb_labels  = ['Entropy-Alpha','DB-Vol-Gr','Sph-Dip-Hel', 'Lexicographic','Pauli'] ; combination labels
  Dpar_labels 	= ['Entropy','Alpha','Anisotropy', 'FD DB','FD Vol','FD Gr','Sphere','Diplane','Helix','|HH|','|VV|','sqrt(2)|HV|','|HH+VV|','|HH-VV|', $
                   'Alpha'+strcompress(indgen(3)+1),'Beta'+strcompress(indgen(3)+1), $
                   'Gamma'+strcompress(indgen(3)+1),'Delta'+strcompress(indgen(3)+1)]
  Dwin_labels	= [[Dcomb_labels,'x: '+Dpar_labels],[Dcomb_labels,'y: '+Dpar_labels]]
  Dlabels       = {long:['Entropy Alpha decomposition','Freeman-Durden decomposition','Sphere-Diplane-Helix decomposition','Lexicographic ratios','Pauli ratios'], $
                   axes:[['','',''],['Double Bounce','Volume','Surface'],['Sphere','Diplane','Helix'],['HH','VV','sqrt(2)HV'],['HH+VV','HH-VV','sqrt(2)HV']]}
  tmp		= (where(Dwin_labels[*,0] eq 'x: '+Dpar_labels[0]))[0]
  Dpar_offsets  = {par:tmp, $
                   haa:(where(Dwin_labels[*,0] eq 'x: Entropy'))[0]-tmp,angles: (where(Dwin_labels[*,0] eq 'x: Alpha 1'))[0]-tmp, $
                   lex:(where(Dwin_labels[*,0] eq 'x: |HH|'))[0]-tmp,   pauli:  (where(Dwin_labels[*,0] eq 'x: |HH+VV|'))[0]-tmp, $
                   sdh:(where(Dwin_Labels[*,0] eq 'x: Sphere'))[0]-tmp, freeman:(where(Dwin_labels[*,0] eq 'x: FD DB'))[0]-tmp}
  Dpar        	= fltarr(n_elements(Dpar_labels),n_tr,/nozero)
  Dpar_range    = [ [.0,1.],[0,90.],[0.,1.], [-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],$
                  [0,90],[0,90],[0,90],[0,90],[0,90],[0,90],[-180,180],[-180,180],[-180,180],[-180,180],[-180,180],[-180,180]] ; haa
  Dsmooth	= {pre:1,post:1,max:1} & dsmooth.max=min([pos,[file.xdim,file.ydim]-pos])/2*2+1
  Dinfo		= ['Data Position: '+strjoin(strcompress(pos,/R),', '),'','','']
  Dwin_single	= {zoom:1.,x:0,y:0,info:'test'} ;; x,y are related to Dpar_labels and not to Dwin_labels!
  Dwin		= replicate(Dwin_single,3) & for i=1,2 do Dwin[i].y=i+1
  s2=sqrt(2.) & j=complex(0,1)
  Dtr_choice	= {List:'Track'+strcompress(indgen(n_tr)+1),Choice:intarr(n_tr)+1}
  Dmath         = {T2C:[[1.,1,0],[1,-1,0],[0,0,s2]]/s2,C2LR:[[1,-1,j*s2],[-1,1,j*s2],[j*s2,j*s2,0]], $
                   hab:ha_border_courve(/DEG)}

;;; Draw Window variables
  widget_control,wid.draw,get_value=win_tmp
  geo=widget_info(wid.draw,/geo)
  pnt={pos:[0,0], dim:[1,1], win:win_tmp, geo:geo}
  wset,pnt.win
  pnt_save = tvrd(pnt.pos[0],pnt.pos[1],pnt.dim[0],pnt.dim[1],true=1)

;;; Graphical variables and constants
  gzoom={main:0L,out:0L,in:0L,text:0L}
  gwin ={main:0L,draw:0L,zoom:gzoom,xy:0L,x:0L,y:0L,info:0L}
  gsmooth={main:0L,pre:0L,post:0L,preMore:0L,preLess:0L,postMore:0L,postLess:0L}
  gmove={main:0L,left:0L,right:0L,up:0L,down:0L,pick:0L,label:0L}
  gsiz={draw:300}
  gui={main: 0L, top:0L, middle:0L, tab:0L, middle_left:0L, control:0L, options:0L, buttons:0L, $
       draw:[gwin,gwin,gwin], $ ;; coh-wins: Draw,Zoom,Baseline,Info
       smooth:gsmooth, info:lonarr(n_elements(dinfo)),  $ ;; smooth: pre, post; info
       ch:0L, chTrack:0L, valTab:0L, $ ;; value table
       but_ok:0L, but_help:0L, but_info:0L, move:gmove, $
       redraw:0L, renew:0L } ;; for custom events!


;;;; ---- GENERATE GUI ---- ;;;;
;;;;    CHANGE THIS IF YOU WANT SCROLL-BARS IN THE X/Y-DIRECTION
;;;;    SET THE Y_SCROLL_SIZE APPROPRIATLY!!
;;;;        gui.main= WIDGET_BASE(GROUP_LEADER=wid.base,row=2, $
;;;;                           TITLE='Complex coherence analysis',/scroll,y_scroll_size=900,x_scroll_size=900)
  gui.main = WIDGET_BASE(/column, GROUP_LEADER=wid.base, $
                         TITLE='Multibaseline track parameter analysis',/tlb_kill_request_events,/tlb_frame_attr)
  gui.top    = WIDGET_BASE(gui.main,/row,/FRAME,/BASE_ALIGN_CENTER)
  gui.middle = WIDGET_BASE(gui.main,/row,/FRAME,/BASE_ALIGN_CENTER)
  gui.buttons= WIDGET_BASE(gui.main,/row,/BASE_ALIGN_CENTER)

  for i=0,2 do begin ;; DRAW AREA
     gui.draw[i].main 	   = WIDGET_BASE(gui.top,/column,/frame)
     gui.draw[i].draw 	   = CW_RAT_DRAW(gui.draw[i].main,gsiz.draw,gsiz.draw,xscroll=gsiz.draw-20,yscroll=gsiz.draw-20)
     gui.draw[i].info 	   = WIDGET_LABEL(gui.draw[i].main,/align_left,value=dwin[i].info,XSIZE=gsiz.draw)
     gui.draw[i].zoom.main = WIDGET_BASE(gui.draw[i].main,/row,/ALIGN_CENTER )
     gui.draw[i].zoom.out  = WIDGET_BUTTON(gui.draw[i].zoom.main,VALUE=config.imagedir+'zoom_out.bmp',/bitmap)
     gui.draw[i].zoom.text = WIDGET_LABEL (gui.draw[i].zoom.main,VALUE='Zoom: '+string(Dwin[i].zoom,f='(f0.1)')+'x')
     gui.draw[i].zoom.in   = WIDGET_BUTTON(gui.draw[i].zoom.main,VALUE=config.imagedir+'zoom_in.bmp',/bitmap)
     gui.draw[i].xy	   = WIDGET_BASE(gui.draw[i].main,/row)
     gui.draw[i].x         = WIDGET_DROPLIST(gui.draw[i].xy,VALUE=Dwin_labels[*,0],title='X:') ;,set_value=i)
     gui.draw[i].y         = WIDGET_DROPLIST(gui.draw[i].xy,VALUE=Dwin_labels[*,1],title='Y:') ;,set_value=i)
  endfor

  gui.tab		= WIDGET_TAB(gui.middle)
  gui.control		= WIDGET_BASE(gui.tab,/row,title='Control')
  gui.middle_left	= WIDGET_BASE(gui.control,/column)
  gui.smooth.main	= WIDGET_BASE(gui.middle_left,/column)
  gtmp			= WIDGET_LABEL(gui.smooth.main,/ALIGN_CENTER,VALUE='Simultaneous smooth')
  gui.smooth.pre	= WIDGET_SLIDER(gui.smooth.main,title="First  (linear) ",SCROLL=2,MIN=1,MAX=dsmooth.max,VALUE=dsmooth.pre)
  gui.smooth.post	= WIDGET_SLIDER(gui.smooth.main,title="Second (square) ",SCROLL=2,MIN=1,MAX=dsmooth.max,VALUE=dsmooth.post)
  gtmp			= WIDGET_LABEL(gui.middle_left,/ALIGN_CENTER,VALUE='System information')
  for i=0,n_elements(dinfo)-1 do $
     gui.info[i]        = WIDGET_LABEL(gui.middle_left,value=dinfo[i],/ALIGN_LEFT,XSIZE=gsiz.draw/3.*2.)


  gui.ch		= WIDGET_BASE(gui.control,/row,/FRAME,/BASE_ALIGN_CENTER)
  gui.chTrack		= CW_BGROUP(gui.ch,Dtr_choice.list,label_top='Tracks',/COLUMN,/nonex,set_value=dtr_choice.choice,xsize=200)

  gui.options		= WIDGET_BASE(gui.tab,/column,title='Options')

  gui.valTab		= WIDGET_TABLE(gui.middle,value=strcompress(transpose(DPar),/R),row_LABELS=Dpar_labels,column_LABELS='Track'+strcompress(indgen(n_tr)+1), $
                                       ALIGNMENT=1,COLUMN_WIDTHS=80,/ALIGN_CENTER,YSIZE=n_elements(Dpar_labels),XSIZE=3,Y_SCROLL_SIZE=12)

  gui.but_ok    = WIDGET_BUTTON(gui.buttons,VALUE=' OK ',xsize=80,/frame)
;  gui.but_canc  = WIDGET_BUTTON(gui.buttons,VALUE=' Cancel ',xsize=60)
  gui.but_info  = WIDGET_BUTTON(gui.buttons,VALUE=' Info ',xsize=60)
;  gui.but_help  = WIDGET_BUTTON(gui.buttons,VALUE=' Help ',xsize=60)
  gui.move.main = WIDGET_BASE(gui.buttons,/row,/FRAME)
  gui.move.label= WIDGET_LABEL(gui.move.main,value='New position: ')
  gui.move.left = WIDGET_BUTTON(gui.move.main,/BITMAP,VALUE=config.imagedir+'shift_left.bmp')
  gui.move.up   = WIDGET_BUTTON(gui.move.main,/BITMAP,VALUE=config.imagedir+'shift_up.bmp')
  gui.move.down = WIDGET_BUTTON(gui.move.main,/BITMAP,VALUE=config.imagedir+'shift_down.bmp')
  gui.move.right= WIDGET_BUTTON(gui.move.main,/BITMAP,VALUE=config.imagedir+'shift_right.bmp')
  gui.move.pick = WIDGET_BUTTON(gui.move.main,VALUE=' Pick ',xsize=60)

  gui.redraw    = WIDGET_BASE(gui.buttons)
  gui.renew     = WIDGET_BASE(gui.buttons)

  WIDGET_CONTROL, gui.main, /REALIZE, DEFAULT_BUTTON = gui.but_ok, tlb_get_size=toto
;  guipos = center_box(toto[0],drawysize=toto[1])
;  widget_control, gui.main, xoffset=guipos[0], yoffset=guipos[1]
  for i=0,2 do begin
     widget_control,gui.draw[i].draw,draw_button_events=1, draw_motion_events=1
     Dwin[i].x=i & Dwin[i].y=i
     widget_control,gui.draw[i].x,set_droplist_select=Dwin[i].x
     widget_control,gui.draw[i].y,set_droplist_select=Dwin[i].y
  endfor

  new_event={ID:0L,TOP:0L,HANDLER:0L,NAME:'renew'}

  XMANAGER, 'polin_analysis_tracks', gui.main, NO_BLOCK=~wid.block
  XMANAGER, 'polin_analysis_tracks', wid.base, NO_BLOCK=~wid.block, EVENT_HANDLER='polin_analysis_tracks_base_event'
  WIDGET_CONTROL, gui.renew, /NO_COPY, SEND_EVENT=new_event
end

pro polin_analysis_tracks_base_event, event
  common rat
  common mb_tracks
  new_event={ID:0L,TOP:0L,HANDLER:0L,VALUE:-1L}
  case event.id of
     wid.draw: begin
        if event.press eq 1 then begin
           pos = 0>round([event.x,event.y]/wid.draw_scale)<[file.xdim-1,file.ydim-1]
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.renew
        endif
     end
     else:
  endcase
end



pro polin_analysis_tracks_event, event
  common rat
  common mb_tracks

  new_event={ID:0L,TOP:0L,HANDLER:0L,VALUE:-1L}

  for i=0,2 do begin
     case event.id of
        gui.draw[i].zoom.out: if Dwin[i].zoom gt 1. then begin
           Dwin[i].zoom = (Dwin[i].zoom-0.5)>1.
           WIDGET_CONTROL,gui.draw[i].zoom.text,SET_VALUE='Zoom: '+strcompress(fix(dwin[i].zoom),/R)+'.'+strcompress(fix(dwin[i].zoom*10)mod 10,/R)+'x'
           curr_event=new_event & curr_event.VALUE=i
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=curr_event, gui.REDRAW
        end
        gui.draw[i].zoom.in: if Dwin[i].zoom lt 5. then begin
           Dwin[i].zoom = (Dwin[i].zoom+0.5)<5.
           WIDGET_CONTROL,gui.draw[i].zoom.text,SET_VALUE='Zoom: '+strcompress(fix(dwin[i].zoom),/R)+'.'+strcompress(fix(dwin[i].zoom*10)mod 10,/R)+'x'
           curr_event=new_event & curr_event.VALUE=i
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=curr_event, gui.REDRAW
        end
        gui.draw[i].x: if event.index ne Dwin[i].x then begin
           Dwin[i].x=event.index
           if event.index lt n_elements(Dcomb_labels) then $
              Dwin[i].y=event.index $
           else Dwin[i].y >= n_elements(Dcomb_labels) 
           curr_event=new_event & curr_event.VALUE=i
           widget_control,gui.draw[i].y,set_droplist_select=Dwin[i].y
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=curr_event, gui.REDRAW
        endif
        gui.draw[i].y: if event.index ne Dwin[i].y then begin
           Dwin[i].y=event.index
           if event.index lt n_elements(Dcomb_labels) then $
              Dwin[i].x=event.index $
           else Dwin[i].x >= n_elements(Dcomb_labels)
           curr_event=new_event & curr_event.VALUE=i
           widget_control,gui.draw[i].x,set_droplist_select=Dwin[i].x
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=curr_event, gui.REDRAW
        endif
        else:
     endcase
  endfor

  case event.id of
     gui.but_ok: begin
        WIDGET_CONTROL, gui.main, /DESTROY
;;; switch back to main draw widget
        wset,pnt.win
        tv,pnt_save,pnt.pos[0],pnt.pos[1],true=1
        tv,transpose(pnt_save,[1,2,0]),pnt.pos[0],pnt.pos[1],true=3
        XMANAGER, 'rat', wid.base, NO_BLOCK=~wid.block
        widget_control,wid.draw, draw_button_events = 0, draw_motion=0
     end
     gui.smooth.pre: if Dsmooth.pre ne event.value then begin
        Dsmooth.pre=event.value/2*2+1
        if Dsmooth.pre ne (event.value/2*2+1) then WIDGET_CONTROL,gui.smooth.pre,SET_VALUE=Dsmooth.pre
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.RENEW
     end
     gui.smooth.post: if Dsmooth.post ne event.value then begin
        Dsmooth.post=event.value/2*2+1
        if Dsmooth.post ne (event.value/2*2+1) then WIDGET_CONTROL,gui.smooth.post,SET_VALUE=Dsmooth.post
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.RENEW
     endif
     gui.chTrack: begin
        WIDGET_CONTROL, gui.chTrack, GET_VALUE=choice
        if ~array_equal(choice, Dtr_choice.choice) then begin
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.REDRAW
           Dtr_choice.choice = choice
        endif
     end
     gui.but_info: tmp = DIALOG_MESSAGE(['MULTIBASELINE TRACK PARAMETER ANALYSIS',' ','RAT module written 09/2006 by Maxim Neumann',' ', $
                                         'Notes:', $
                                         " - In case you have difficulties using this tool and interpreting abbreviations, ", + $
                                         '   contact the author and demand better documentation.'], $
                                        DIALOG_PARENT = gui.main,/INFORMATION)
     gui.move.up: if pos[1]+1 lt file.ydim then begin
        pos[1]++
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.renew
     endif
     gui.move.down: if pos[1] gt 0 then begin
        pos[1]--
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.renew
     endif
     gui.move.left: if pos[0] gt 0 then begin
        pos[0]--
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.renew
     endif
     gui.move.right: if pos[0]+1 lt file.xdim then begin
        pos[0]++
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.renew
     endif
     gui.move.pick: tmp = DIALOG_MESSAGE(['MULTIBASELINE TRACK PARAMETER ANALYSIS',' ','RAT module written 09/2006 by Maxim Neumann',' ', $
                                          'You can always pick a new position', $
                                          'by clicking on the map in the main rat window.'], $
                                         DIALOG_PARENT = gui.main,/INFORMATION)
     gui.renew: begin
        Dinfo[0]='Position: '+strjoin(strcompress(pos,/R),', ')
        WIDGET_CONTROL,gui.info[0],set_value=Dinfo[0]
        Dsmooth.max=min([pos,[file.xdim,file.ydim]-pos])/2*2+1
        Dsmooth.pre<=Dsmooth.max & Dsmooth.post<=Dsmooth.max
        WIDGET_CONTROL,gui.smooth.pre, set_slider_max=Dsmooth.max,set_value=Dsmooth.pre
        WIDGET_CONTROL,gui.smooth.post,set_slider_max=Dsmooth.max,set_value=Dsmooth.post

        rrat,file.name,M,block=[pos-Dsmooth.pre/2-Dsmooth.post/2,[1,1]*(Dsmooth.pre+Dsmooth.post-1)]
        for i=1,n_tr-1 do $
           M[0:pol-1,i*pol:(i+1)*pol-1,*,*]=M[i*pol:(i+1)*pol-1,i*pol:(i+1)*pol-1,*,*]
        M=M[0:pol-1,*,*,*]
        if Dsmooth.pre gt 1 then begin
           M=smooth(M,[1,1,Dsmooth.pre,Dsmooth.pre])
           x=(Dsmooth.pre+Dsmooth.post-1)/2 & x=x+[-Dsmooth.post,Dsmooth.post]/2 & 
           M=M[*,*,x[0]:x[1],x[0]:x[1]]
        endif
        if Dsmooth.post ne 1 then $
           M = total(total(M,3),3)/(Dsmooth.post^2)
        M  = reform(M,pol,pol,n_tr,/overwrite)

        index=[2,1,0]           ; descending eigenvalues
        for tr=0,n_tr-1 do begin
           if is_coherency then begin
              T = M
              C = Dmath.T2C  # T[*,*,tr] # Dmath.T2C
           endif else begin
              C = M
              T = Dmath.T2C  # C[*,*,tr] # Dmath.T2C
           endelse
           LR = Dmath.C2LR # C # conj(transpose(Dmath.C2LR))
           eval = la_eigenql(T[*,*,tr],eigenvectors=evec)
           pi = (eval / total(eval))[index]
           Dpar[Dpar_offsets.haa+0,tr]    = total(-pi*alog(pi)/alog(pol)) ; entropy
           Dpar[Dpar_offsets.haa+1,tr]    = total(pi*acos(abs(evec[0,index]))*!radeg) ; alpha
           Dpar[Dpar_offsets.haa+2,tr]    = (eval[1]-eval[0])/(eval[1]+eval[0]) ; anisotropy
           Dpar[Dpar_offsets.angles+0,tr] = reform(acos(abs(evec[0,index]))*!radeg) ; alphas
           Dpar[Dpar_offsets.angles+3,tr] = reform(acos(abs(evec[1,index])/sin(Dpar[Dpar_offsets.angles+0,tr]/!radeg))*!radeg)>0. ; betas
           Dpar[Dpar_offsets.angles+6,tr] = reform(atan(evec[2,index],/phase)*!radeg) ; gammas
           Dpar[Dpar_offsets.angles+9,tr] = reform(atan(evec[1,index],/phase)*!radeg) ; deltas
           Dpar[Dpar_offsets.lex,tr]	  = sqrt(abs(diag_matrix(C)))
           Dpar[Dpar_offsets.pauli,tr]	  = sqrt(abs([T[0,0,tr],T[1,1,tr]]))
           Dpar[Dpar_offsets.freeman,tr]  = polin_deco_freedur_func(C,m=1)
           Dpar[Dpar_offsets.sdh,tr]	  = [sqrt(abs(LR[2,2])),sqrt(abs(LR[0,0])<abs(LR[1,1])),abs(sqrt(abs(LR[0,0]))-sqrt(abs(LR[1,1])))]
        endfor

        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.REDRAW
        widget_control,gui.valTab,set_value=transpose(reform(string((Dpar),f='(f0.3)'),size((Dpar),/DIM)))
;;; redraw the pointer on the map
        wset,pnt.win & loadct,0,/silent
        tv,pnt_save,pnt.pos[0],pnt.pos[1],true=1
        pnt.pos=(round((pos-Dsmooth.pre/2-Dsmooth.post/2)*wid.draw_scale)-1)>0
        pnt.dim=[1,1]*round((Dsmooth.pre+Dsmooth.post-1)*wid.draw_scale)+2 & pnt.dim <= ([pnt.geo.draw_xsize,pnt.geo.draw_ysize]-pnt.pos)
        pnt_save = tvrd(pnt.pos[0],pnt.pos[1],pnt.dim[0],pnt.dim[1],true=1)
        tv,255-transpose(pnt_save,[1,2,0]),pnt.pos[0],pnt.pos[1],true=3
        x=pnt.pos[0]+[0,pnt.dim[0]-1] & y=pnt.pos[1]+[0,pnt.dim[1]-1]
        plots,/device,color=255,[x[0],x[1],x[1],x[0],x[0]],[y[0],y[0],y[1],y[1],y[0]]
        if ~strcmp(widget_info(wid.draw,/event_pro),'polin_analysis_tracks_base_event',/FOLD) then $
           widget_control,wid.draw,draw_button_events=1, draw_motion_events = 0,event_pro='polin_analysis_tracks_base_event'
     end
     gui.redraw: if total(Dtr_choice.choice) ne 0 then begin
        par = Dpar[*,where(Dtr_choice.choice)]
        for i=0,2 do if i eq event.value || event.value eq -1 then begin
           widget_control,gui.draw[i].draw,DRAW_XSIZE=gsiz.draw*Dwin[i].zoom, $
                          DRAW_YSIZE=gsiz.draw*Dwin[i].zoom,get_value=win
           wset,win
           loadct,0,/silent
           if dwin[i].x ge n_elements(Dcomb_labels) then begin
              x=Dwin[i].x-n_elements(Dcomb_labels) & y=Dwin[i].y-n_elements(Dcomb_labels)
              xrange=Dpar_range[*,x] & yrange=Dpar_range[*,y]
              if xrange[0] le xrange[1] then undefine,xrange
              if yrange[0] le yrange[1] then undefine,yrange
              plot,par[x,*],par[y,*],psym=4,thick=2,symsize=2,xrange=xrange,yrange=yrange,ystyle=19,xstyle=3
              widget_control,gui.draw[i].info,set_value=Dpar_labels[x]+' x '+Dpar_labels[y]
           endif else begin
              case dwin[i].x of
                 0: begin ;;; Entropy-Alpha
                    ea_bord = [[0,42.5],[0.5,42.5],[0.5,0],[0.5,90],[0.5,47.5],[0.,47.5],[0.5,47.5],[0.5,40], $
                               [1,40],[.9,40],[.9,0],[.9,90],[.9,55],[1,55],[.9,55],[.9,50],[.5,50]]
                    x=Dpar_offsets.haa & y=Dpar_offsets.haa+1
                    plot,/nodata,xrange=Dpar_range[*,x],yrange=Dpar_range[*,y],ystyle=17,xstyle=1,ea_bord & tek_color
                    plots,ea_bord[0,*],ea_bord[1,*],color=3,thick=2
                    plots,Dmath.hab[*,0],Dmath.hab[*,1],color=2,thick=2 & loadct,0,/silent
                    plots,par[x,*],par[y,*],psym=4,thick=2,symsize=2
                 end
                 1: triplot,par[Dpar_offsets.freeman+0,*],par[Dpar_offsets.freeman+1,*],par[Dpar_offsets.freeman+2,*],axes=DLabels.axes[*,dwin[i].x] ;;; freeman
                 2: triplot,par[Dpar_offsets.sdh+0,*],par[Dpar_offsets.sdh+1,*],par[Dpar_offsets.sdh+2,*],axes=DLabels.axes[*,dwin[i].x] ;;; kroegager
                 3: triplot,par[Dpar_offsets.lex+0,*],par[Dpar_offsets.lex+1,*],par[Dpar_offsets.lex+2,*],axes=DLabels.axes[*,dwin[i].x] ;;; lex
                 4: triplot,par[Dpar_offsets.pauli+0,*],par[Dpar_offsets.pauli+1,*],par[Dpar_offsets.lex+2,*],axes=DLabels.axes[*,dwin[i].x] ;;; pauli
              endcase
              widget_control,gui.draw[i].info,set_value=DLabels.long[dwin[i].x]
           endelse
        endif
     endif
     else:
  endcase
end
