;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : polin_analysis.pro
; Author        : Maxim Neumann
; Modified      : 08/2006
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

pro polin_analysis
  common rat
  common tool, pos, pol, n_tr, n_bl, newline, $
     gui,gsiz, Dbl_list,Dcoh_choice,Dopt_choice,Dcoh,Dsmooth,Dinfo, $
     Dtab, Dtab_labels_col,Dtab_labels_row, $
     use_kz,kz_file,use_bl,bl_file,kz,blp,z2pi, $
     coh, opt, rcoh, shape, $
     pnt, pnt_save

;;; initialization
  if file.type lt 510 || file.type gt 511 then begin
     error = DIALOG_MESSAGE(["This is wrong data type.",'Needs a MB-cov/coh-matrix'], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
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
                     TITLE='Multibaseline Analysis - Start',/modal,/tlb_kill_request_events,/tlb_frame_attr)
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
        info = DIALOG_MESSAGE(['MULTIBASELINE COHERENCE OPTIMIZATION ANALYSIS',' ',$
                               'RAT module written 08/2006 by Maxim Neumann'], $
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
           info = DIALOG_MESSAGE(['MULTIBASELINE COHERENCE OPTIMIZATION ANALYSIS',' ',$
                                  'RAT module written 08/2006 by Maxim Neumann'], $
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
  Dbl_list  = strcompress(indgen(n_bl),/R)+': tracks '+strjoin(strcompress(mb_ind(indgen(n_bl))+1,/R),'x')
  Dtab_labels_col = ['Lex','Pauli','SB-MSM','SB-MSM 1SM','SB-ESM', $
                     'MB-MSM','MB-MSM 1SM','MB-ESM']
;                      'SUMCOR(A)','SC(A) 1MC','SUMCOR(B)','SC(B) 1MC','SUMCOR(C)','SC(C) 1MC','SUMCOR(D)','SC(D) 1MC', $
;                      'SUMCOH NR','SC NR-ORTH']
  Dtab_labels_row = ['CohSum',' CS 1',' CS 2',' CS 3', $
                     'AmpSum',' AS 1',' AS 2',' AS 3']
  Dcoh_choice = {List:['Lex','Pauli','SB-MSM','SB-MSM 1SM','SB-ESM','Shape','Random'],Choice:[0,0,1,0,0,1,0]}
  Dopt_choice = {List:['MB-MSM','MB-MSM 1SM','MB-ESM'],Choice:[1,0,0]}
;   Dopt_choice = {List:['SUMCOR(A)','SUMCOR(A) 1MC','SUMCOR(B)','SUMCOR(B) 1MC','SUMCOR(C)','SUMCOR(C) 1MC','SUMCOR(D)','SUMCOR(D) 1MC', $
;                        'SUMCOH NR','SUMCOH NR-ORTH'],Choice:[1,0,0,0,0,0,0,0,0,1]}
  Dcoh_single = {zoom:1.,bl:0}
  Dcoh=replicate(dcoh_single,3) & for i=0,2 do Dcoh[i].bl=(i<(n_bl-1))
  Dsmooth={pre:1,post:1,max:1} & dsmooth.max=min([pos,[file.xdim,file.ydim]-pos])/2*2+1
  Dinfo = ['Data Position: '+strjoin(strcompress(pos,/R),', '),'                             ','                             ','                             ']
  Dtab  = complexarr(n_elements(Dtab_labels_col),n_elements(Dtab_labels_row))

;;; Draw Window variables
  widget_control, wid.draw,get_value=win_tmp
  geo=widget_info(wid.draw,/geo)
  pnt={pos:[0,0], dim:[1,1], win:win_tmp, geo:geo}
  wset,pnt.win
  pnt_save = tvrd(pnt.pos[0],pnt.pos[1],pnt.dim[0],pnt.dim[1],true=1)

;;; Graphical variables and constants
  gzoom={main:0L,out:0L,in:0L,text:0L}
  gcoh={main:0L,draw:0L,zoom:gzoom,bl:0L,info:0L}
  gsmooth={main:0L,pre:0L,post:0L,preMore:0L,preLess:0L,postMore:0L,postLess:0L}
  gmove={main:0L,left:0L,right:0L,up:0L,down:0L,pick:0L,label:0L}
  gsiz={draw:300}
  gui={main: 0L, top:0L, middle:0L, middle_left:0L, buttons:0L, $
       draw:[gcoh,gcoh,gcoh], $ ;; coh-wins: Draw,Zoom,Baseline,Info
       smooth:gsmooth, info:lonarr(n_elements(dinfo)),  $ ;; smooth: pre, post; info
       ch:0L, chCoh:0L, chOpt:0L, valTab:0L, $ ;; value table
       but_ok:0L, but_help:0L, but_info:0L, move:gmove, $
       redraw:0L, renew:0L } ;; for custom events!


;;;; ---- GENERATE GUI ---- ;;;;
;;;;    CHANGE THIS IF YOU WANT SCROLL-BARS IN THE X/Y-DIRECTION
;;;;    SET THE Y_SCROLL_SIZE APPROPRIATLY!!
;;;;        gui.main= WIDGET_BASE(GROUP_LEADER=wid.base,row=2, $
;;;;                           TITLE='Complex coherence analysis',/scroll,y_scroll_size=900,x_scroll_size=900)
  gui.main = WIDGET_BASE(/column, GROUP_LEADER=wid.base, $
                         TITLE='Multibaseline coherence analysis',/tlb_kill_request_events,/tlb_frame_attr)
  gui.top    = WIDGET_BASE(gui.main,/row,/FRAME,/BASE_ALIGN_CENTER)
  gui.middle = WIDGET_BASE(gui.main,/row,/FRAME,/BASE_ALIGN_CENTER)
  gui.buttons= WIDGET_BASE(gui.main,/row,/BASE_ALIGN_CENTER)

  for i=0,2 do begin ;; DRAW AREA
     gui.draw[i].main = WIDGET_BASE(gui.top,/column,/frame)
     gui.draw[i].draw = CW_RAT_DRAW(gui.draw[i].main,gsiz.draw,gsiz.draw,xscroll=gsiz.draw-20,yscroll=gsiz.draw-20)
     if use_kz || use_bl then $
        gui.draw[i].info   = WIDGET_LABEL(gui.draw[i].main,/align_left,value='Baseline '+strcompress(i),XSIZE=gsiz.draw)
     gui.draw[i].zoom.main = WIDGET_BASE(gui.draw[i].main,/row)
     gui.draw[i].bl        = WIDGET_DROPLIST(gui.draw[i].zoom.main,VALUE=Dbl_list) ;,set_value=i)
     gui.draw[i].zoom.out  = WIDGET_BUTTON(gui.draw[i].zoom.main,VALUE=config.imagedir+'zoom_out.bmp',/bitmap)
     gui.draw[i].zoom.text = WIDGET_LABEL (gui.draw[i].zoom.main,VALUE='Zoom: '+strcompress(fix(dcoh[i].zoom),/R)+'.'+strcompress(fix(dcoh[i].zoom*10)mod 10,/R)+'x')
     gui.draw[i].zoom.in   = WIDGET_BUTTON(gui.draw[i].zoom.main,VALUE=config.imagedir+'zoom_in.bmp',/bitmap)
  endfor

  gui.middle_left	= WIDGET_BASE(gui.middle,/column)
  gui.smooth.main	= WIDGET_BASE(gui.middle_left,/column)
  gtmp			= WIDGET_LABEL(gui.smooth.main,/ALIGN_CENTER,VALUE='Simultaneous smooth')
  gui.smooth.pre	= WIDGET_SLIDER(gui.smooth.main,title="PRE ",SCROLL=2,MIN=1,MAX=dsmooth.max,VALUE=dsmooth.pre)
  gui.smooth.post	= WIDGET_SLIDER(gui.smooth.main,title="POST",SCROLL=2,MIN=1,MAX=dsmooth.max,VALUE=dsmooth.post)
  gtmp			= WIDGET_LABEL(gui.middle_left,/ALIGN_CENTER,VALUE='System information')
  for i=0,n_elements(dinfo)-1 do $
     gui.info[i]        = WIDGET_LABEL(gui.middle_left,value=dinfo[i],/ALIGN_LEFT)

  gui.ch		= WIDGET_BASE(gui.middle,/row,/FRAME)
  gui.chCoh		= CW_BGROUP(gui.ch,dcoh_choice.list,label_top='SB Coherence',/COLUMN,/nonex,set_value=dcoh_choice.choice)
  gui.chOpt		= CW_BGROUP(gui.ch,dopt_choice.list,label_top='MB Optimization',/COLUMN,/nonex,set_value=dopt_choice.choice)
  gui.valTab		= WIDGET_TABLE(gui.middle,value=strcompress(abs(transpose(Dtab)),/R),COLUMN_LABELS=Dtab_labels_row,ROW_LABELS=Dtab_labels_col, $
                                       ALIGNMENT=1,COLUMN_WIDTHS=50)

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
  guipos = center_box(toto[0],drawysize=toto[1])
  widget_control, gui.main, xoffset=guipos[0], yoffset=guipos[1]
  for i=0,2 do begin
     widget_control,gui.draw[i].draw,draw_button_events=1, draw_motion_events=1
     widget_control,gui.draw[i].bl,set_droplist_select=Dcoh[i].bl
  endfor

  new_event={ID:0L,TOP:0L,HANDLER:0L,NAME:'renew'}

  XMANAGER, 'polin_analysis', gui.main, NO_BLOCK=~wid.block
  XMANAGER, 'polin_analysis', wid.base, NO_BLOCK=~wid.block, EVENT_HANDLER='polin_analysis_base_event'
  WIDGET_CONTROL, gui.renew, /NO_COPY, SEND_EVENT=new_event

end


pro polin_analysis_base_event, event
  common rat
  common tool
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


pro polin_analysis_event, event
  common rat
  common tool

  new_event={ID:0L,TOP:0L,HANDLER:0L,VALUE:-1L}

  for i=0,2 do begin
     case event.id of
        gui.draw[i].zoom.out: if Dcoh[i].zoom gt 1. then begin
           Dcoh[i].zoom = (Dcoh[i].zoom-0.5)>1.
           WIDGET_CONTROL,gui.draw[i].zoom.text,SET_VALUE='Zoom: '+strcompress(fix(dcoh[i].zoom),/R)+'.'+strcompress(fix(dcoh[i].zoom*10)mod 10,/R)+'x'
           curr_event=new_event & curr_event.VALUE=i
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=curr_event, gui.REDRAW
        end
        gui.draw[i].zoom.in: if Dcoh[i].zoom lt 5. then begin
           Dcoh[i].zoom = (Dcoh[i].zoom+0.5)<5.
           WIDGET_CONTROL,gui.draw[i].zoom.text,SET_VALUE='Zoom: '+strcompress(fix(dcoh[i].zoom),/R)+'.'+strcompress(fix(dcoh[i].zoom*10)mod 10,/R)+'x'
           curr_event=new_event & curr_event.VALUE=i
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=curr_event, gui.REDRAW
        end
        gui.draw[i].bl: if event.index ne Dcoh[i].bl then begin
           Dcoh[i].bl = event.index
           curr_event=new_event & curr_event.VALUE=i
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
	XMANAGER, 'rat', wid.base, no_block = ~wid.block
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
     gui.chCoh: begin
        WIDGET_CONTROL, gui.chCoh, GET_VALUE=choice
        if ~array_equal(choice, Dcoh_choice.choice) then begin
           if choice[6] && choice[6] ne Dcoh_choice.choice[6] then $
              WIDGET_CONTROL,   /NO_COPY, SEND_EVENT=new_event, gui.RENEW $
           else WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.REDRAW
           Dcoh_choice.choice = choice
        endif
     end
     gui.chOpt: begin
        WIDGET_CONTROL, gui.chOpt, GET_VALUE=choice
        if ~array_equal(choice, Dopt_choice.choice) then begin
           Dopt_choice.choice = choice
           WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.REDRAW
        endif
     end
     gui.but_info: tmp = DIALOG_MESSAGE(['MULTIBASELINE COHERENCE OPTIMIZATION ANALYSIS',' ','RAT module written 08/2006 by Maxim Neumann',' ', $
                                         'Notes:', $
                                         ' - It does not make much sense to (spatially) post-smooth optimized coherences', $
                                         '   since they are in different polarization basis.', $
                                         ' - MSM (1SM) means the initial optimization with multiple scattering mechanisms, ', $
                                         '   which are afterwards averaged to one scattering mechanism for coherence computation.', $
;                                          ' - In case you want to pick a new position, ', $
;                                          '   click please on the main map in the rat window.', $
                                         " - In case you have difficulties using this tool and interpreting abbreviations, ", $
                                         '   contact the author and demand better documentation.'], $
                                        DIALOG_PARENT = gui.main,/INFORMATION)
     gui.move.up: if pos[1]+1 lt file.ydim-1 then begin
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
     gui.move.right: if pos[0]+1 lt file.xdim-1 then begin
        pos[0]++
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.renew
     endif
     gui.move.pick: tmp = DIALOG_MESSAGE(['MULTIBASELINE COHERENCE OPTIMIZATION ANALYSIS',' ','RAT module written 08/2006 by Maxim Neumann',' ', $
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
;         WIDGET_CONTROL,gui.smooth.pre, set_value=Dsmooth.pre
;         WIDGET_CONTROL,gui.smooth.post,set_value=Dsmooth.post
        tracks = mb_ind(indgen(n_bl))
        if use_kz then begin
           rrat,kz_file,kz_tr,block=[pos,1,1]
           kz=kz_tr[tracks[1,*]]-kz_tr[tracks[0,*]] & z2pi=2.*!pi/kz
           WIDGET_CONTROL,gui.info[1],set_value='Kz:   ['+string(min(kz),max(kz),f='(f0.2,"..",f0.2)')+']'
           WIDGET_CONTROL,gui.info[2],set_value=string(min(z2pi),max(z2pi),f='("z2pi: [",f0.2,"m..",f0.2,"m]")')
;'z2pi: ['+strcompress(min(z2pi),/R)+'..'+strcompress(max(z2pi),/R)+']'
        endif
        if use_bl then begin
           rrat,bl_file,blp_tr,block=[pos,1,1]
           blp=blp_tr[tracks[1,*]]-blp_tr[tracks[0,*]]
           WIDGET_CONTROL,gui.info[3],set_value='Blp:  ['+strcompress(min(blp) ,/R)+'..'+strcompress(max(blp) ,/R)+']'
        endif

        rrat,file.name,origin,block=[pos-Dsmooth.pre/2-Dsmooth.post/2,[1,1]*(Dsmooth.pre+Dsmooth.post-1)]
        if Dsmooth.pre gt 1 then begin
           origin=smooth(origin,[1,1,Dsmooth.pre,Dsmooth.pre])
           x=(Dsmooth.pre+Dsmooth.post-1)/2 & x=x+[-Dsmooth.post,Dsmooth.post]/2 & 
           origin=origin[*,*,x[0]:x[1],x[0]:x[1]]
        endif
        C=file.type eq 510? origin: mb_c2t(origin,/REV)
        T=file.type eq 511? origin: mb_c2t(origin)
        if Dsmooth.post ne 1 then $
           Tpost = total(total(T,3),3)/(Dsmooth.post^2) $
        else Tpost = T
        coh   = complexarr(pol,n_bl,Dsmooth.post,Dsmooth.post,n_elements(Dcoh_choice.list)-1,/nozero)
        opt   = complexarr(pol,n_bl,Dsmooth.post,Dsmooth.post,n_elements(Dopt_choice.list),  /nozero)
        npts_shape = 91
        npts_rcoh  = 500
        shape = complexarr(npts_shape,n_bl,/nozero)
        if Dcoh_choice.choice[6] eq 1 then $ ;; random coherences
           rcoh  = complexarr(npts_rcoh,n_bl,Dsmooth.post,Dsmooth.post,4,/nozero)
        coh[*,*,*,*,0] = mb_cc(C)
        coh[*,*,*,*,1] = mb_cc(T)
        opt[*,*,*,*,0] = mb_opt(T)
        opt[*,*,*,*,1] = mb_opt(T,/single)
        opt[*,*,*,*,2] = mb_opt_nr(T,ORTHO=1)
;         opt[*,*,*,*,2] = mb_opt(T,constr=2)
;         opt[*,*,*,*,3] = mb_opt(T,constr=2,/single)
;         opt[*,*,*,*,4] = mb_opt(T,criter=1)
;         opt[*,*,*,*,5] = mb_opt(T,criter=1,/single)
;         opt[*,*,*,*,6] = mb_opt(T,criter=1,constr=2)
;         opt[*,*,*,*,7] = mb_opt(T,criter=1,constr=2,/single)
;         opt[*,*,*,*,8] = mb_opt_nr(T,ORTHO=0)
;         opt[*,*,*,*,9] = mb_opt_nr(T,ORTHO=1)
        for i=0,n_bl-1 do begin
           Ti = mb_sb(T,i)
           coh[*,i,*,*,2] = cc_opt(Ti)
           coh[*,i,*,*,3] = coh_nr(Ti)
           coh[*,i,*,*,4] = cc_opt(Ti,/single)
;           if Dcoh_choice.choice[4] eq 1 then $ ;; shape
           shape[*,i] =  coh_shape(mb_sb(Tpost,i),b=npts_shape)
           if Dcoh_choice.choice[6] eq 1 then $ ;; random coherences
              rcoh[*,i,*,*] = coh_rand(Ti,npts_rcoh)
        endfor
        coh   = total(total(coh,3),3)/(Dsmooth.post^2)
        opt   = total(total(opt,3),3)/(Dsmooth.post^2)
        if Dcoh_choice.choice[6] eq 1 then $ ;; random coherences
           rcoh  = total(total(rcoh,3),3)/(Dsmooth.post^2)
        WIDGET_CONTROL, /NO_COPY, SEND_EVENT=new_event, gui.REDRAW
        for col=0,n_elements(Dtab_labels_col)-1 do begin
           case Dtab_labels_col[col] of
              'Lex':	ctab = coh[*,*,0]
              'Pauli':	ctab = coh[*,*,1]
              'SB-MSM':		ctab = coh[*,*,2]
              'SB-MSM 1SM':	ctab = coh[*,*,4]
              'SB-ESM':		ctab = coh[*,*,3]
              'MB-MSM':		ctab = opt[*,*,0]
              'MB-MSM 1SM':	ctab = opt[*,*,1]
              'MB-ESM':		ctab = opt[*,*,2]
;               'SUMCOR(A)':	ctab = opt[*,*,0]
;               'SC(A) 1MC': 	ctab = opt[*,*,1]
;               'SUMCOR(B)':	ctab = opt[*,*,2]
;               'SC(B) 1MC': 	ctab = opt[*,*,3]
;               'SUMCOR(C)':	ctab = opt[*,*,4]
;               'SC(C) 1MC': 	ctab = opt[*,*,5]
;               'SUMCOR(D)':	ctab = opt[*,*,6]
;               'SC(D) 1MC': 	ctab = opt[*,*,7]
;               'SUMCOH NR':	ctab = opt[*,*,8]
;               'SC NR-ORTH':	ctab = opt[*,*,9]
              else: stop
           endcase
           Dtab[col,0] = mean(ctab)
           for i=0,pol-1 do $
              Dtab[col,i+1] = mean(ctab[i,*])
           Dtab[col,4] = mean(abs(ctab))
           for i=0,pol-1 do $
              Dtab[col,i+5] = mean(abs(ctab[i,*]))
           widget_control,gui.valTab,set_value=reform(string(abs(transpose(Dtab)),f='(f0.3)'),size(transpose(Dtab),/DIM))
;   Dtab_labels_row = ['Coherent Sum All',' " 1',' " 2',' " 3', $
;                      'Sum of Amplitudes /N_BL All','.. 1','.. 2','.. 3', $
;                      'Custom']
        endfor
;;; redraw the pointer on the map
        wset,pnt.win
        loadct,0,/silent
        tv,pnt_save,pnt.pos[0],pnt.pos[1],true=1
        pnt.pos=(round((pos-Dsmooth.pre/2-Dsmooth.post/2)*wid.draw_scale)-1)>0
        pnt.dim=[1,1]*round((Dsmooth.pre+Dsmooth.post-1)*wid.draw_scale)+2 & pnt.dim <= ([pnt.geo.draw_xsize,pnt.geo.draw_ysize]-pnt.pos)
        pnt_save = tvrd(pnt.pos[0],pnt.pos[1],pnt.dim[0],pnt.dim[1],true=1)
        tv,255-transpose(pnt_save,[1,2,0]),pnt.pos[0],pnt.pos[1],true=3
        x=pnt.pos[0]+[0,pnt.dim[0]-1] & y=pnt.pos[1]+[0,pnt.dim[1]-1]
        plots,/device,color=255,[x[0],x[1],x[1],x[0],x[0]],[y[0],y[0],y[1],y[1],y[0]]
;        plots,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/device,color=255
        if ~strcmp(widget_info(wid.draw,/event_pro),'polin_analysis_base_event',/FOLD) then $
           widget_control,wid.draw,draw_button_events=1, draw_motion_events = 0,event_pro='polin_analysis_base_event'
     end
     gui.redraw: for i=0,2 do if i eq event.value || event.value eq -1 then begin
        bl = Dcoh[i].bl
        widget_control,gui.draw[i].draw,DRAW_XSIZE=gsiz.draw*Dcoh[i].zoom, $
                       DRAW_YSIZE=gsiz.draw*Dcoh[i].zoom,get_value=win
        wset,win
        loadct,0,/silent
        ucplot,/iso,xrange=xrange,yrange=yrange
        if Dcoh_choice.choice[6] then begin $ ;; random coherences
           hf    = 100.
           coh_r = real_part(rcoh[*,bl])
           coh_i = imaginary(rcoh[*,bl])
           h = hist_2d(coh_r,coh_i,bin1=1./hf,bin2=1./hf,min1=-1,min2=-1,max1=1,max2=1)
           col = bytscl(h)
           loadct,33,/silent
           for j=0L,n_elements(rcoh[*,bl])-1 do $
              plots,coh_r[j],coh_i[j],psym=3,color=255-col[coh_r[j]*hf+hf,coh_i[j]*hf+hf]
        endif
        tek_color
;  Dcoh_choice = {List:['Lex','Pauli','Opt (SVD)','Opt (SVD) 1MC','Opt (NR)','Shape','Random'],Choice:[0,1,0,0,1,0]}
;  Dopt_choice = {List:['SUMCOR(A)','SUMCOR(A)
;  1MC','SUMCOR(B)'],Choice:[1]}
        if Dcoh_choice.choice[0] then $ ; lex
           plots,real_part(coh[*,bl,0]),imaginary(coh[*,bl,0]),psym=6,thick=2,symsize=2,color=4
        if Dcoh_choice.choice[1] then $ ; pauli
           plots,real_part(coh[*,bl,1]),imaginary(coh[*,bl,1]),psym=1,thick=2,symsize=2,color=2
        if Dcoh_choice.choice[2] then $ ; opt svd
           plots,real_part(coh[*,bl,2]),imaginary(coh[*,bl,2]),psym=4,thick=2,symsize=2,color=1
        if Dcoh_choice.choice[3] then $ ; opt svd
           plots,real_part(coh[*,bl,4]),imaginary(coh[*,bl,4]),psym=4,thick=2,symsize=2,color=2
        if Dcoh_choice.choice[4] then $ ; opt nr
           plots,real_part(coh[*,bl,3]),imaginary(coh[*,bl,3]),psym=4,thick=2,symsize=2,color=5
        if Dcoh_choice.choice[5] then $ ;; shape
           plots,real_part(shape[*,bl]), imaginary(shape[*,bl]),thick=1,                color=3
        for j=0,n_elements(Dopt_choice.list)-1 do $
           if Dopt_choice.choice[j] then $ ; sumcor(a)
              plots,real_part(opt[*,bl,j]),imaginary(opt[*,bl,j]),psym=5,thick=2,symsize=2,color=j+6
;         if Dopt_choice.choice[0] then $ ; sumcor(a)
;            plots,real_part(opt[*,bl,0]),imaginary(opt[*,bl,0]),psym=5,thick=2,symsize=2,color=8
;         if Dopt_choice.choice[1] then $ ; sumcor(a)
;            plots,real_part(opt[*,bl,1]),imaginary(opt[*,bl,1]),psym=5,thick=2,symsize=2,color=7
        loadct,0,/silent
        if use_kz || use_bl then $
           widget_control,gui.draw[i].info,set_value='Baseline '+strcompress(i,/R)+ $
                          (use_kz?' kz: '+string(kz[bl],f='(f0.2)')+' z2pi: '+string(z2pi[bl],f='(f0.2)')+'m':'')+ $
                          (use_bl?' blp: '+string(blp[bl],f='(f0.2)')+'m':'')
     endif
     else:                      ;help,event,/str
  endcase
end
