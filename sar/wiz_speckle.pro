;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: wiz_speckle
; written by    : Maxim Neumann
; last revision : Dec 2005
; Speckle filtering and presumming in a wizard
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


pro wiz_speckle, called=called
  common rat, types, file, wid, config

  data_type   = 0
  filter_type = ['Boxcar ','Lee    ','Refined Lee','IDAN-LLMMSE','Sim. Annealing','Gamma-MAP','Sigma  ','Median ','Gauss  ','Kuan   ','Frost  ']
  if (file.type ge 500L && file.type le 519L)  then begin
     data_type = 2              ; POLINSAR
     offer = [1,1,1,1,1,0,0,0,0,0,0]
  endif else if (file.type ge 200L && file.type le 210L) || (file.type ge 220L && file.type le 222L) then begin
     data_type = 1              ; POLAR
     offer = [1,1,1,1,1,0,0,0,0,0,0]
  endif else begin
     data_type = 0              ; SAR and others
     offer = [1,1,1,1,0,1,1,1,1,1,1]
  endelse
;  filter_ind  = [where(offer),where(~offer)]
  filter_ind  = where(offer)
  filter_grp  = lonarr(n_elements(filter_ind))
  fieldMaxNr  = 10
  field       = lonarr(fieldMaxNr,n_elements(filter_ind))-1
  field_value = ptrarr(fieldMaxNr)
  filter      = filter_ind[0]
  presumX     = 1
  presumY     = 1
  postsumX     = 1
  postsumY     = 1

  main = widget_base(GROUP_LEADER=wid.base,title='Presumming and Speckle Filtering',/COLUMN,XSIZE=700,/floating,/tlb_kill_request_events,/tlb_frame_attr)

  presum_title = widget_label(main,value='Presumming:')
  main_presum  = widget_base(main,col=2,/frame)
  w_presumX= CW_FIELD(main_presum,VALUE=presumX,/integer,  TITLE='Presumming X  : ',XSIZE=3)
  w_presumY= CW_FIELD(main_presum,VALUE=presumY,/integer,  TITLE='Presumming Y  : ',XSIZE=3)

  filter_title = widget_label(main,value='Speckle filter:')
  filter_tab = WIDGET_TAB(main)
  for i=0,n_elements(filter_grp)-1 do begin
     filter_grp[i]=widget_base(filter_tab,/column,title=filter_type[filter_ind[i]])
     if ~offer[filter_ind[i]] then widget_control,filter_grp[i],SENSITIVE=0
     case filter_ind[i] of
        0: begin                ; Boxcar
           field[0,i] = CW_FIELD(filter_grp[i],VALUE=7,/integer,TITLE='Filter boxsize X   : ',XSIZE=5)
           field[1,i] = CW_FIELD(filter_grp[i],VALUE=7,/integer,TITLE='Filter boxsize Y   : ',XSIZE=5)
        end
        1: begin                ; Lee
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=7,/integer,  TITLE='Filter boxsize        : ',XSIZE=5)
           field[1,i]   = CW_FIELD(filter_grp[i],VALUE='1.0',/float,TITLE='Effective No of Looks : ',XSIZE=5)
        end
        2: begin                ; Refined Lee
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=7,/integer,  TITLE='Filter boxsize        : ',XSIZE=5)
           field[1,i]   = CW_FIELD(filter_grp[i],VALUE='1.0',/float,TITLE='Effective No of Looks : ',XSIZE=5)
           field[2,i]   = CW_FIELD(filter_grp[i],VALUE='6.0',/float,TITLE='Threshold  (dB)       : ',XSIZE=5)
           if data_type ne 0 then $
              field[3,i]   = CW_BGROUP(filter_grp[i],["Lee original (RoA)","Lee modified","Lee modified + coef. of var.","Coefficient of variation"], $
                                       /frame,set_value=0,row=4,label_top='Mask selection method:',/exclusive)
        end
        3: begin                ; IDAN
           field[0,i] = CW_FIELD(filter_grp[i],VALUE=50,/integer,  TITLE='Size of adaptive neighbourhood : ',XSIZE=5)
           field[1,i] = CW_FIELD(filter_grp[i],VALUE=25,/integer,  TITLE='Minimal size of  neighbourhood : ',XSIZE=5)
           field[2,i] = CW_FIELD(filter_grp[i],VALUE=1.0,/floating,TITLE='No. of looks                   : ',XSIZE=5)
        end
        4: begin                ; Simulated Annealing
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=50,/integer,TITLE='Number of iterations:          ',XSIZE=7)
           field[1,i]   = CW_FIELD(filter_grp[i],VALUE=0.05,/float,TITLE='Cooling constant (beta):       ',XSIZE=7)
           field[2,i]   = CW_FIELD(filter_grp[i],VALUE=1.,/float,  TITLE='Initial temperature:           ',XSIZE=7)
           field[3,i]   = CW_FIELD(filter_grp[i],VALUE=1.,/float,  TITLE='Number of looks:               ',XSIZE=7)
           field[4,i]   = CW_BGROUP(filter_grp[i], ['Assume singular input covariances(number of looks < 3): '],set_value=[0], /nonexclusive)
        end
        5: begin                ; Gamma-MAP
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=7,/integer,  TITLE='Filter boxsize        : ',XSIZE=7)
           field[1,i]   = CW_FIELD(filter_grp[i],VALUE='1.0',/float,TITLE='Effective No of Looks : ',XSIZE=7)
        end
        6: begin                ; Sigma
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=7,/integer,TITLE='Filter boxsize  : ',XSIZE=7)
           field[1,i]   = CW_FIELD(filter_grp[i],VALUE=2.0,/float,TITLE='Sigma factor    : ',XSIZE=7)
           field[2,i]   = CW_FIELD(filter_grp[i],VALUE=1.0,/float,TITLE='Nr. of looks    : ',XSIZE=7)
        end
        7: begin                ; Median
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=7,/integer,TITLE='Filter boxsize        : ',XSIZE=7)
        end
        8: begin                ; Gauss
           field[0,i] = CW_FIELD(filter_grp[i],VALUE=7,/integer,  TITLE='Filter boxsize X      : ',XSIZE=7)
           field[1,i] = CW_FIELD(filter_grp[i],VALUE=7,/integer,  TITLE='Filter boxsize Y      : ',XSIZE=7)
        end
        9: begin                ; Kuan
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=7,/integer,TITLE='Filter boxsize        : ',XSIZE=7)
           field[1,i]   = CW_FIELD(filter_grp[i],VALUE=1.0,/float,TITLE='Effective No of Looks : ',XSIZE=7)
        end
        10: begin               ; Frost
           field[0,i]   = CW_FIELD(filter_grp[i],VALUE=7,/integer,TITLE='Filter boxsize        : ',XSIZE=7)
           field[1,i]   = CW_FIELD(filter_grp[i],VALUE=1.0,/float,TITLE='Damping factor        : ',XSIZE=7)
        end
        else:
     endcase
  endfor

  postsum_title = widget_label(main,value='Postsumming:')
  main_postsum  = widget_base(main,col=2,/frame)
  w_postsumX= CW_FIELD(main_postsum,VALUE=postsumX,/integer,  TITLE='Postsumming X  : ',XSIZE=3)
  w_postsumY= CW_FIELD(main_postsum,VALUE=postsumY,/integer,  TITLE='Postsumming Y  : ',XSIZE=3)


  if (file.type ge 200L && file.type le 210L) || (file.type ge 500L && file.type le 509L) then $
     attention_label=widget_text(main,value=['ATTENTION:', $
                                             'In order to continue with speckle filtering,', $
                                             'a covariance/coherency matrix will be generated from the scattering vectors!', $
                                             'If you disagree, press cancel!'],YSIZE=4)
  buttons  = WIDGET_BASE(main,column=3,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

;     widget_control, main, /REALIZE, default_button = but_ok, /update
  WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=pos[0], yoffset=pos[1]
another_filter:
  repeat begin
     event = widget_event(main)
     if event.id eq but_info $
     then begin                 ; Info Button clicked
        infotext = ['PRESUMMING AND SPECKLE FILTER WIZARD',' ',$
                    'RAT module written 12/2005 by Maxim Neumann and Andreas Reigber']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main,/INFORMATION)
     endif
  endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  tab_ind = widget_info(filter_tab,/TAB_CURRENT)
  filter = filter_ind[tab_ind]
  if event.id eq but_ok && ~offer[filter] then begin
     info = DIALOG_MESSAGE(['This speckle filter is not allowed for this data-type.','Please choose another one!'], $
                           DIALOG_PARENT = main, TITLE='Information')
     goto,another_filter
  endif
  widget_control,w_presumX,get_value=presumX
  widget_control,w_presumY,get_value=presumY
  widget_control,w_postsumX,get_value=postsumX
  widget_control,w_postsumY,get_value=postsumY
  for i = 0,fieldMaxNr-1 do if field[i,tab_ind] ne -1 then begin
     widget_control,field[i,tab_ind],get_value=tmp_value
     field_value[i] = ptr_new(tmp_value)
  endif
  widget_control, main, /destroy
  if event.id ne but_ok then return ; OK button _not_ clicked


; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; start batch
  if file.type ge 500L && file.type le 509L then begin
     polin_k2m,/called,smmx=presumX,smmy=presumY
     presumX = 1 & presumY = 1
  endif
  if file.type ge 200L && file.type le 210L then begin
     k_to_m,/called,smmx=presumX,smmy=presumY
     presumX = 1 & presumY = 1
  endif
  if presumX ne 1 || presumY ne 1 then $
     image_presumming,smmx=presumX,smmy=presumY,/called
  if wid.cancel eq 1 then return

  case filter of
;;; BOXCAR
     0: if data_type eq 0 $
     then speck_mean,/called,boxsizex=*field_value[0],boxsizey=*field_value[1] $
     else speck_polmean,/called,smmx=*field_value[0],smmy=*field_value[1]
;;; LEE
     1: if data_type eq 0 $
     then speck_lee,/called,boxsize=*field_value[0],looks=*field_value[1] $
     else speck_pollee,/called,smm=*field_value[0],looks=*field_value[1]
;;; REF-LEE
     2: if data_type eq 0 $
     then speck_polreflee,/called,smm=*field_value[0],looks=*field_value[1],threshold=*field_value[2] $
     else speck_polreflee,/called,smm=*field_value[0],looks=*field_value[1],threshold=*field_value[2],method=*field_value[3]
;;; IDAN
     3: speck_polidan,/called,NMAX=*field_value[0],NMIN=*field_value[1],LOOKS=*field_value[2]
;;; SIM-ANNEALING
     4: speck_pol_anneal,/called,IterNum=*field_value[0],cooling=*field_value[1],initTemp=*field_value[2],lookNum=*field_value[3],singularCovar=*field_value[4]
;;; GAMMA-MAP
     5: speck_gammamap,/called,BOXSIZE=*field_value[0],LOOKS=*field_value[1]
;;; SIGMA
     6: speck_sigma,/called,BOXSIZE=*field_value[0],SIGFAC=*field_value[1],LOOKS=*field_value[2]
;;; MEDIAN
     7: speck_median,/called,BOXSIZE=*field_value[0]
;;; GAUSS
     8: speck_gauss,/called,BOXSIZEX=*field_value[0],BOXSIZEY=*field_value[1]
;;; KUAN
     9: speck_kuan,/called,BOXSIZE=*field_value[0],LOOKS=*field_value[1]
;;; FROST
     10: speck_frost,/called,BOXSIZE=*field_value[0],LOOKS=*field_value[1]
  endcase
  ptr_free, field_value
  if wid.cancel eq 1 then return

  if postsumX ne 1 || postsumY ne 1 then $
     image_presumming,smmx=postsumX,smmy=postsumY,/called
  if wid.cancel eq 1 then return

; generate preview
  if ~keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy

end
