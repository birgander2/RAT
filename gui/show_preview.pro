;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: show_preview
; last revision : 02/2007
; written by    : Maxim Neumann
; To show or not to show the preview
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



pro show_preview, ON=ON, OFF=OFF
  common rat

  if keyword_set(ON) then begin ; switch on
     if ~config.show_preview then begin
        config.show_preview = ~config.show_preview
        generate_preview
        widget_control,wid.button_show_preview,set_value=config.imagedir+'show_preview_on.bmp',/BITMAP
     endif
  endif else if keyword_set(OFF) then begin ; switch off
     if config.show_preview then begin
        config.show_preview = ~config.show_preview
        generate_preview
        widget_control,wid.button_show_preview,set_value=config.imagedir+'show_preview_off.bmp',/BITMAP
     endif
  endif else begin ;;; toggle
;;; to provide this funtionality, include wid.button_show_preview into the system!
     config.show_preview = ~config.show_preview
     generate_preview
     if config.show_preview then $
        widget_control,wid.button_show_preview,set_value=config.imagedir+'show_preview_on.bmp',/BITMAP $
     else $
        widget_control,wid.button_show_preview,set_value=config.imagedir+'show_preview_off.bmp',/BITMAP
  endelse

end
