;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: import_info_esar
; written by    : Maxim Neumann
; last revision : 08/2006
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

pro import_info_esar, TRACKS_NR=n_tr, INPUTFILE_FIRST=inputfile0
  common rat
  common channel

  insar    = file.type ge 300 && file.type lt 400
  polin = file.type ge 500 && file.type lt 600

  if n_elements(n_tr) eq 0 then begin
     if polin then polin_get_info,tracks=n_tr $
     else if insar then n_tr=2 $
     else n_tr=1
  endif

  files=strarr(n_tr)
  lines=lonarr(n_tr)
  texts=lonarr(n_tr)
  brows=lonarr(n_tr)

  if n_elements(inputfile0) ne 0 then files[0]=inputfile0

;;; GUI
  if total(files eq '') ne 0 then begin
     main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Open ESAR system info file'+(n_tr gt 1?'s':''), $
                        /floating,/tlb_kill_request_events,/tlb_frame_attr)
     for i=0,n_tr-1 do begin
        lines[i] = WIDGET_BASE(main,column=2)
        texts[i] = CW_FIELD(lines[i],VALUE=files[i],/string,XSIZE=60,TITLE='ESAR system info file for track '+strcompress(i,/R)+':')
        brows[i] = WIDGET_BUTTON(lines[i],VALUE=' browse ',ysize=35)
     endfor
     buttons  = WIDGET_BASE(main,column=4,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['IMPORT ESAR SYSTEM INFO DATA',$
                       ' ',$
                       'RAT module written 2006 by Maxim Neumann',' ', $
                       'Note: Provide for every track one file,',' preferable with the same polarization']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
        for i=0,n_tr-1 do begin
           if event.id eq brows[i] then begin ; Browse clicked
              path = config.workdir
              files[i] = cw_rat_dialog_pickfile(TITLE='Open ESAR system info ".sav" file', DIALOG_PARENT=wid.base, FILTER = '*.sav', $
                                                /MUST_EXIST, PATH=path, GET_PATH=path)
              if strlen(files[i]) gt 0 && file_test(files[i],/READ) then config.workdir = path $
              else files[i] = ''
              widget_control,texts[i],set_value=files[i]
           endif
           if event.id eq texts[i] then begin
              widget_control,texts[i],get_value=file
              if file_test(file,/read) then files[i]=file $
              else widget_control,texts[i],set_value=files[i]
           endif
        endfor
     endrep until (event.id eq but_ok && total(files eq '') eq 0) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     WIDGET_CONTROL, /DESTROY, main
     if event.id ne but_ok then return
  endif

;;; Init-Info
  for i=0,n_tr-1 do begin
     restore,files[i],/RELAXED
     if i eq 0 then initlist = replicate(init,n_tr) $
     else initlist[i] = init
  endfor

;   rangeLine = double(init.range_delay)/double(1e6)*init.speed_of_light/2.+ $ 
;               init.range_sampling*findgen(n[0])

  tags = tag_names(init[0])
  if total(strcmp(tags,/FOLD_CASE,'wavelength')) then wavelength=init.wavelength
  if total(strcmp(tags,/FOLD_CASE,'ALTITUDE_ABOVE_GROUND')) then plane_height=initlist[*].ALTITUDE_ABOVE_GROUND
  if total(strcmp(tags,/FOLD_CASE,'FORW_VELOCITY')) then plane_velocity=mean(initlist[*].FORW_VELOCITY)
  if total(strcmp(tags,/FOLD_CASE,'range_delay')) then range_delay=init.range_delay
  if total(strcmp(tags,/FOLD_CASE,'Range_sampling')) then res_sr=init.range_sampling


  infotext = ['IMPORT ESAR SYSTEM INFO DATA',$
              'RAT module written 2006 by Maxim Neumann',' ', $
              'Example information which could be extracted:']

  if n_elements(wavelength) ne 0 then infotext=[infotext,'Wavelengt: '+strcompress(init.wavelength,/R)]
  if n_elements(plane_height) ne 0 then infotext=[infotext,'Height (cerca): '+strjoin(strcompress(initlist[*].ALTITUDE_ABOVE_GROUND,/R)+'m',', ')]
  if n_elements(plane_velocity) ne 0 then infotext=[infotext,'Plane velocity (cerca): '+strcompress(mean(initlist[*].FORW_VELOCITY),/R)+'m/s']
  if n_elements(range_delay) ne 0 then infotext=[infotext,'Range delay: '+strcompress(init.range_delay,/R)+' micro seconds']
  if n_elements(res_sr) ne 0 then infotext=[infotext,'Range sampling: '+strcompress(init.range_sampling,/R)]

  infotext=[infotext, $
            'Date of data acquisition: '+strcompress(init.date_of_data_acquis,/R), $
            'Squint: '+strjoin(strcompress(initlist[*].SQUINT,/R),', '), $
;              'Slant Range: between '+strcompress(min(rangeLine),/R)+'m and '+strcompress(max(rangeLine),/R)+'m', $
            'Antenna Depr. Angle: '+strcompress(init.ANTENNA_DEPR_ANGLE,/R), $
            'etc.']

  info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = wid.base, TITLE='ESAR extracted information',/INFO)

;;; additional information
  if n_elements(wavelength) ne 0 then ignore= set_par('wavelength',wavelength)
  if n_elements(plane_height) ne 0 then ignore= set_par('plane_height',plane_height)
  if n_elements(plane_velocity) ne 0 then ignore= set_par('plane_velocity',plane_velocity)
  if n_elements(range_delay) ne 0 then  ignore= set_par('range_delay',range_delay)
  if n_elements(res_sr) ne 0 then  ignore= set_par('res_sr',res_sr)
;  ignore= set_par('slant_range',rangeLine)

end
