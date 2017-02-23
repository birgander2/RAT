;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_data_info
; written by       : Maxim Neumann
; last revision    : 19. October 2004
; Data info for some PolInSAR data types
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

function mm,wert,MAGNITUDE=magnitude,PHASE=phase
  on_error,2
	ret = ''
  if keyword_set(magnitude) then begin
     wert2 = abs(wert)
     ret=[ret,'Minimum        :'+strcompress(min(wert2))]
     ret=[ret,'Maximum        :'+strcompress(max(wert2))]
     dummy = moment(wert2,/nan)
     ret=[ret,'Mittelwert     :'+strcompress(dummy[0])]
     ret=[ret,'Std-Abweichung :'+strcompress(sqrt(dummy[1]))]
  endif

  if keyword_set(phase) then begin
     wert2 = atan(wert,/phase)
     ret=[ret,'Minimum        :'+strcompress(min(wert2))]
     ret=[ret,'Maximum        :'+strcompress(max(wert2))]
     dummy = moment(wert2,/nan)
     ret=[ret,'Mittelwert     :'+strcompress(dummy[0])]
     ret=[ret,'Std-Abweichung :'+strcompress(sqrt(dummy[1]))]
  endif

  if ~keyword_set(magnitude) and ~keyword_set(phase) then begin
     ret=[ret,'Minimum        :'+strcompress(min(wert))]
     ret=[ret,'Maximum        :'+strcompress(max(wert))]
     dummy = moment(wert,/nan)
     ret=[ret,'Mittelwert     :'+strcompress(dummy[0])]
     ret=[ret,'Std-Abweichung :'+strcompress(sqrt(dummy[1]))]
  endif
	return,ret
end


pro polin_data_info,CALLED=called,SMMX=smmx,SMMY=smmy
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag

; check if array is usable - get new type
  if file.type ne 530 and file.type ne 531 then begin
     error_button = DIALOG_MESSAGE(['No info available for this data type!'] $
                                   , DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif else begin
     WIDGET_CONTROL,/hourglass
; pop up progress window
     progress,Message=' Calculating...',/cancel_button
     infostring = ''
     rrat,file.name,coh

     progress,percent=33,/check_cancel
     if wid.cancel eq 1 then return


     
     for v=0,file.vdim-1 do begin
        for z=0,file.zdim-1 do begin
           infostring=[infostring,'','Magnitude of coherence '+channel_names[v*file.zdim+z]]
           infostring=[infostring,mm(coh[v,z,*,*],/m)]
        endfor
     endfor
     
     progress,percent=66,/check_cancel
     if wid.cancel eq 1 then return

     if file.type eq 531 then begin
        for v=0,file.vdim-1 do begin
           for z=0,file.zdim-1 do begin
              infostring=[infostring,'','Phase of coherence '+channel_names[v*file.zdim+z]]
              infostring=[infostring,mm(coh[v,z,*,*],/p)]
           endfor
        endfor
     endif
     infostring=[infostring,'','Mean of Amplitude for all channels: ','']
     for v=0,file.vdim-1 do for z=0,file.zdim-1 do infostring=[infostring,channel_names[v*file.zdim+z]+' : '+strcompress(mean(abs(coh[v,z,*,*])))]
  endelse
  
  progress,/destroy
  
  infostring = infostring[1:*]
  main = widget_base(GROUP_LEADER=wid.base,row=3,TITLE='Information',/tlb_kill_request_events,/tlb_frame_attr)
  text = widget_text(main,value=infostring,/scroll,ysize=10)
  but_ok   = WIDGET_BUTTON(main,VALUE=' OK ',xsize=80,/frame)
  WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=pos[0], yoffset=pos[1]

  repeat begin                  ; Event loop
     event = widget_event(main)
  endrep until (event.id eq but_ok)  or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
  widget_control,main,/destroy  ; remove main widget
  

;   WIDGET_CONTROL,/hourglass
;   bs = config.blocksize
;   calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
;   blocksizes = intarr(anz_blocks)+bs
;   blocksizes[anz_blocks-1] = bs_last
;   head = 1l
;   rrat,file.name,ddd,header=head,info=info,type=type
;   progress,Message='some infos about current data... will be printed in the shell!'
;   for i=0,anz_blocks-1 do begin
;      progress,percent=(i+1)*100.0/anz_blocks
;      block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
;      readu,ddd,block
;   endfor
;   close, ddd

; no update - no preview !
end
