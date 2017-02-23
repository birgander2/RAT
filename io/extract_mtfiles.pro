;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Modified      : mn, 08/2006
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

function extract_mtnames,multifile,ANZ=anz
  openr, ddd, multifile, /GET_LUN, /XDR
  dummy = 0l
  anz   = 0l
  info  = bytarr(80)
  dim   = 0l
  readu,ddd,dim
  for i=1,dim+4 do readu,ddd,dummy
  readu,ddd,anz
  readu,ddd,info

  files = strarr(anz)
  ifile = ""
  for i=1,anz do begin
     readu,ddd,ifile
     files[i-1] = ifile
  endfor
  free_lun,ddd
  if product(file_test(files)) eq 0 then $ ;; most probably relative names
     for i=0,anz-1 do $
        files[i] = filepath(ROOT=file_dirname(multifile),files[i])
  return,files
end
