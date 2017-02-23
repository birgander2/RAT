;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: license
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
pro license
	common rat, types, file, wid, config
	
	infostring = [ $
'RAT - Terms of Use',$
'----------------------------------------------------------------------',$
'The RAT source code and binary releases are licenced under the Mozilla',$
'Public License (MPL) version 1.1. This allows the use of the code in  ',$
'a wide variety of other free and commercial software projects, while ',$
'maintaining copyleft on the written code. ',$
'',$
'For more details see http://www.mozilla.org/MPL/MPL-1.1.html',$
'or the file LICENCE in the base directory of the source distribution.',$
'',$
'The software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ',$
'ANY KIND, either express or implied. See the license for the specific',$
'language governing rights and limitations under the license.']
 	
	dummy=DIALOG_MESSAGE(infostring,DIALOG_PARENT = wid.base, TITLE='License',/INFORMATION)
end



