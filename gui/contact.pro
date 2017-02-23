;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: contact
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
pro contact
	common rat, types, file, wid, config
	
	infostring = [ $
	'Contact the RAT developers:', $
	'', $
	'Mailing list     : https://lists.berlios.de/mailman/listinfo/radartools-users', $
	'RAT development  : https://developer.berlios.de/projects/radartools/', $
	'Bug reports      : https://developer.berlios.de/bugs/?group_id=8644', $
	'Main RAT website : http://www.cv.tu-berlin.de/rat/', $
	'',$
	'For questions concerning the usage of RAT, please use the mailing list.',$
	'If you want to contribute something, please use the development portal to',$
	'contact one of the main authors.']
	
	dummy=DIALOG_MESSAGE(infostring,DIALOG_PARENT = wid.base, TITLE='Contact information',/INFORMATION)
end
