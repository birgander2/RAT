;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: parameter_info
; last revision : 11.11.2005
; written by    : Maxim Neumann
; Information about set parameters.
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

pro parameter_info,CALLED = called
  common rat, types, file, wid, config
  common rit, pars,evolution    ; pars== parameters structure ==parstruct




; change mousepointer
  WIDGET_CONTROL,/hourglass
  if	config.os eq 'windows' then newline = strcompress(13B) + string(10B)
  if	config.os eq 'unix' then newline = string(10B)



  main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Parameter Information',/floating,/tlb_kill_request_events,/tlb_frame_attr)
;  tx_base=WIDGET_BASE(main,column=2)
  lbl1    = widget_label(main,value='Data parameter information:')
  tx_par  = widget_text(main,XSIZE=50,ysize=30,/scroll)
  lbl2    = widget_label(main,value='Data evolution:')
  tx_evo  = widget_text(main,XSIZE=100,ysize=5,/scroll)
  buttons  = WIDGET_BASE(main,column=3,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  but_edit = WIDGET_BUTTON(buttons,VALUE=' Edit... ',xsize=80)
  WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, XOFFSET=pos[0], yoffset=pos[1]
  par1 = ''
  par2 = 'Not set parameters:'+newline
  evo  = ''

;;; system parameters
  PN        = ptr_new()
  par_nr    = n_tags(pars)
  par_names = tag_names(pars)
  for i=0,par_nr-1 do $
     if pars.(i) ne PN then $
        par1 += par_names[i]+': '+strjoin(strcompress((*pars.(i))[*]))+newline $
     else $
        par2 += par_names[i]+newline

;;; evolution
  if n_elements(evolution) ne 1 || strlen(evolution[0]) ne 0 then $
     for i=0,n_elements(evolution)-1 do $
        evo += strcompress(i,/R)+': '+evolution[i] +newline

  widget_control, tx_par, SET_VALUE = par1+newline+par2
  widget_control, tx_evo, SET_VALUE = evo


; ---- Event loop
  repeat begin
     event = widget_event(main)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['PARAMETER INFORMATION AND EVOLUTION OF THE DATA',$
                    ' ',$
                    'RAT module written 11/2005 by Maxim Neumann']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
     endif
  endrep until (event.id eq but_ok) || event.id eq but_edit || tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  widget_control,main,/destroy  ; remove main widget

  if event.id eq but_edit then $ ;; Edit button clicked
     parameter_edit

; switch back to main draw widget
  widget_control,wid.draw,get_value=index
  wset,index

;  progress,/destroy
end
