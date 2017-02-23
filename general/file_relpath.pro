;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;+
; NAME:
; 	FILE_RELPATH
; PURPOSE:
; 	Relative path from one absolute path to another
; CALLING SEQUENCE:
; 	rel_path = FILE_RELPATH(file, source_path)
; INPUTS:
; 	file, source_path: absolute path names
;	if source_path is not given, then the current directory is
;	taken as the source_path
; OUTPUTS:
;	Relative path from source_path to file
; RESTRICTIONS:
;	file and source_path should be reachable by file system
; MODIFICATION HISTORY:
;	08/2006 Maxim [Yossarian] Neumann
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

function FILE_RELPATH, file, source
  sep  = path_sep()
  if n_elements(source) eq 0 then source = '.'
  to   = strsplit(file_search(file,  /FULLY_QUALIFY_PATH),sep,/EXTRACT)
  from = strsplit(file_search(source,/FULLY_QUALIFY_PATH),sep,/EXTRACT)
  n_to = n_elements(to) & n_from = n_elements(from)
  if n_from eq 1 && from[0] eq '' then n_from=0
  ind  = where(from ne to, cnt)
  same = cnt eq 0 ? n_from<n_to : ind[0]
  if same eq n_from then res = './' $
  else res = strjoin(replicate('..'+sep,n_from-same))
  if same ne n_to then res += strjoin(to[same:*],sep)
  return, res
end
