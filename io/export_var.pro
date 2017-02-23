;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: export_var
; written by    : Maxim Neumann
; last revision : October 2007
;
; Export the current data set to an IDL variable.
;
; Call this function only from the shell and provide the variable, 
; where the data should be saved.
; ATTENTION: do not use this function, if your data is too big!
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




pro export_var, VAR, BLOCK=BLOCK, CALLED=CALLED
	common rat, types, file, wid, config

        rrat,file.name,var,BLOCK=BLOCK

end
