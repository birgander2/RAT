;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; written by    : Maxim Neumann (TUB)
; last revision : 21. October 2004
; gives back an array [xpos,ypos] of the clicked position in draw area
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

function get_rat_draw_position, POS_DEVICE=pos_device, NO_CURSOR_CHANGE=no_cursor_change
  common rat, types, file, wid, config

  widget_control,wid.draw,draw_button_events=1
;  geo = widget_info(wid.draw,/geometry)

  if ~keyword_set(no_cursor_change) then $
     device,/cursor_crosshair

  repeat res = widget_event(wid.draw) $
  until res.press eq 1

  if ~keyword_set(no_cursor_change) then $
     device,/cursor_original

  widget_control,wid.draw, draw_button_events = 0

  x = res.x > 0
  y = res.y > 0

  if arg_present(pos_device) then pos_device=[x,y]

  return, round([x,y] / wid.draw_scale)
end
