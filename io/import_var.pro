;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: import_var
; written by    : Maxim Neumann
; last revision : October 2007
;
; Import data from a given IDL variable 
; (corresponding to a given data type).
;
; Call this function only from the shell and provide the variable, 
; which contains the data, and at least the data type (for data type 
; info, see "definitions.pro")
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




pro import_var, VAR, TYPE=TYPE, INFO=info, CALLED=CALLED
	common rat, types, file, wid, config

        srat,config.tempdir+config.workfile1,var,type=type,info=info
        open_rat,inputfile=config.tempdir+config.workfile1

end
