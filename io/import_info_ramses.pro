;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: import_info_ramses
; written by    : Maxim Neumann
; last revision : 08/2006
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

pro import_info_ramses, TRACKS_NR=n_tr, INPUTFILE_FIRST=inputfile0
  common rat
  common channel, channel_names, channel_selec, color_flag, palettes, pnames

  if n_elements(inputfile0) ne 0 then ent_file=inputfile0 $
  else begin

;;; GUI for file selection
     path = config.workdir
     ent_file = cw_rat_dialog_pickfile(TITLE='Open RAMSES system info file', $
                                       DIALOG_PARENT=wid.base, FILTER = '*.ent', /MUST_EXIST, PATH=path, GET_PATH=path)
  endelse

  if file_test(ent_file,/READ) then config.workdir = path else return

  name_tmpl = { VERSION:        FLOAT(1.00000), $
                DATASTART:      LONG(0), $
                DELIMITER:      BYTE(61), $
                MISSINGVALUE:   FLOAT(!VALUES.F_NAN), $
                COMMENTSYMBOL:  STRING('#'), $
                FIELDCOUNT:     LONG(2), $
                FIELDTYPES:     LONG([7,7]), $
                FIELDNAMES:     STRING(['names','values']), $
                FIELDLOCATIONS: LONG([0,6]), $ ; ???
                FIELDGROUPS:    LONG([0,1]) } ; ???
  d = READ_ASCII(ent_file,TEMPLATE=name_tmpl)

;;; get parameter information for RAMSES
  ind = where(strlowcase(strcompress(d.names,/R)) $
              eq strlowcase(strcompress('Nb_case_par_ligne_look',/R)),whereNr)
  X = LONG(d.values[ind[0]])
  ind = where(strlowcase(strcompress(d.names,/R)) $
              eq strlowcase(strcompress('Nb_ligne_look',/R)),whereNr)
  Y = LONG(d.values[ind[0]])
;;; additional information
  ind = where(strlowcase(strcompress(d.names,/R)) $
              eq strlowcase(strcompress('Resol_dist_proj',/R)),whereNr)
  err=set_par('res_gr',float(d.values[ind[0]]))
  ind = where(strlowcase(strcompress(d.names,/R)) $
              eq strlowcase(strcompress('Resol_dist_rad',/R)),whereNr)
  err=set_par('res_sr',float(d.values[ind[0]]))
  ind = where(strlowcase(strcompress(d.names,/R)) $
              eq strlowcase(strcompress('Resolution_doppler',/R)),whereNr)
  err=set_par('res_az',float(d.values[ind[0]]))

end
