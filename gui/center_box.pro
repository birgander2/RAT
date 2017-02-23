;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: center_box
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
function center_box,drawsize,drawysize=drawysize
	common rat, types, file, wid, config

	geom = Widget_Info(wid.base, /geometry)
	posx = geom.xoffset + geom.xsize/2.0 - drawsize/2.0
	posy = geom.yoffset + geom.ysize/2.0 - 30
	IF KEYWORD_SET(drawysize) THEN posy = geom.yoffset + geom.ysize/2.0 - drawysize/2.0

	return,[posx,posy]
end
