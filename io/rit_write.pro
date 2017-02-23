;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rit_write
; written by    : Maxim Neumann
; last revision : 07.Oct.2005
; Writes/Updates an entry with name=value syntax in a given file
; PARAMETERS:
;   ritfile:  [string] If existing, the file should be readable and writabel!
;   name:     [string] something meaningful would be nice
;   value:    [any]    a scalar or array
;   status=st:   if ok, 0 is returned, else some error code
;   /NO_INFO: suppress to write the comment text into the file
; TODO:
;   implement support for structures, string arrays, pntr, objref
; EXAMPLE:
;   rit_write,'file.rit','flat-earth',FE
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

function rit_constants, COMMENT_TEXT=COMMENT_TEXT, NAME_TMPL=NAME_TMPL
  if keyword_set(COMMENT_TEXT) then $
     return, $
     [';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;',$
      ';;;;;               RAT INFORMATION TEXT (RITa) file            ;;;;;;',$
      ';;;;;                                                           ;;;;;;',$
      ';;;;;  Version 0.1                                    10.10.05  ;;;;;;',$
      ';;;;;  simple ini-file: in every line one entry: "name= value"  ;;;;;;',$
      ';;;;;  "value" includes type and dimension specific parameters  ;;;;;;',$
      ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;',$
      ';; - entry syntax:  NAMESTRING = TYPE N_DIM [DIMENSIONS] DATA       ;;',$
      ';; - if N_DIM (number of dimensions) is equal 0 (scalar value)      ;;',$
      ';;   then no DIMENSIONS are given!                                  ;;',$
      ';; - number of whitespaces (blanks and tabs) is not important       ;;',$
      ';;   they will be used as delimiter                                 ;;',$
      ';; - in case of string arrays: use \0 (backslash zero) as delimiter ;;',$
      ';; - use the equality sign "=" only after the entry name            ;;',$
      ';; - only the first compatible name will be used (and its value)    ;;',$
      ';; - the semicolor ";" is for commenting till the end of the line!  ;;',$
      ';; - all user written comments will be deleted by any automatic     ;;',$
      ';;   rit-file modification                                          ;;',$
      ';; - new scalar values should be added at the begining of the file  ;;',$
      ';; - new array values should be added at the end of the file        ;;',$
      ';;   (attention: also a 1-element array will be written at the end) ;;',$
      ';; ! IMPORTANT - prefer to use given methods for rit-file           ;;',$
      ';;               modification instead of hacking the file manually! ;;',$
      ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;',$
      ';;;  0-UNDEFINED  ;;;  1-BYTE      ;;;  2-INT      ;;;  3-LONG     ;;;',$
      ';;;  4-FLOAT      ;;;  5-DOUBLE    ;;;  6-COMPLEX  ;;;  7-STRING   ;;;',$
      ';;;  8-STRUCT     ;;;  9-DCOMPLEX  ;;; 10-POINTER  ;;; 11-OBJREF   ;;;',$
      ';;;  12 UINT      ;;; 13-ULONG     ;;; 14-LONG64   ;;; 15-ULONG64  ;;;',$
      ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;',$
      ';;;  NOT SUPPORTED DATA TYPES: 8 10 11 (STRUCT) (POINTER) (OBJREF) ;;;',$
      ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;',$
      ';;;  EXAMPLES:                                                     ;;;',$
      ';;;    speckle-filter = 8 0 lee-ref                                ;;;',$
      ';;;    flat-earth     = 4 1 812 fe0 fe1 fe2 ... fe811              ;;;',$
      ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;', $
      '']
  if keyword_set(NAME_TMPL) then $
     return, { VERSION:        FLOAT(1.00000), $
               DATASTART:      LONG(0), $
               DELIMITER:      BYTE(61), $
               MISSINGVALUE:   FLOAT(!VALUES.F_NAN), $
               COMMENTSYMBOL:  STRING(';'), $
               FIELDCOUNT:     LONG(2), $
               FIELDTYPES:     LONG([7,7]), $
               FIELDNAMES:     STRING(['names','values']), $
               FIELDLOCATIONS: LONG([0,6]), $ ; ???
               FIELDGROUPS:    LONG([0,1]) } ; ???
  return,''
end

function rit_line,name,value, FIRST_VALUE=FIRST_VALUE
  siz = size(value)
  type= size(value,/TYPE)
  if type ne 8L then begin
     val = STRJOIN([type, siz[0:siz[0]]],' ')
     if type eq 7L then $
        val += ' '+strjoin(value[*],'\0') $ ; for string-arrays
     else val = STRJOIN([val,fix(value[*],type=7L,/PRINT)],' ') ; for the byte->string case!
     if keyword_set(FIRST_VALUE) then return, strcompress(val)
     rline = strcompress(name)+' = '+strcompress(val)
  endif else begin
     str_nr    = n_tags(value)
     str_names = tag_names(value)
     rline = strcompress(name)+' = 8 0 STRUCT'
     for i=0,str_nr-1 do $
        rline = [rline,rit_line(name+'.'+str_names[i],value.(i))]
  endelse
  return,rline
end

pro rit_write_single, ritfile, name, value, STATUS=STATUS, NO_INFO=NO_INFO, DEBUG=DEBUG
;  on_error,2
  status=0
  comment_text = rit_constants(/COMMENT_TEXT)
  name_tmpl = rit_constants(/NAME_TMPL)

;;;;;;;;;;;;;; ERROR HANDLING ;;;;;;;;;;;;;;;;;
  if n_elements(name) eq 0 && n_elements(value) eq 0 then begin
     if keyword_set(DEBUG) then $
        print,'please provide a name and its value!'
     status=1
     return
  endif
  if n_elements(RITFILE) eq 0 then begin
     if keyword_set(DEBUG) then $
        print,'please provide a file name!'
     status=2
     return
  endif
  if file_test(RITFILE) && ~file_test(RITFILE,/WRITE,/READ) then begin
     if keyword_set(DEBUG) then $
        print,'the file exists but this user is not allowed to read/write it!'
     status=3
     return
  endif
  if size(value,/TYPE) eq 8L then begin
     if keyword_set(DEBUG) then $
        print,'the given value is a STRUCT!  ', $
              'no support of STRUCTs is implemented till now!'
     status=4
     return
  endif
  if size(value,/TYPE) eq 10 || size(value,/TYPE) eq 11 then begin
     if keyword_set(DEBUG) then $
        print,'the given value is a POINTER or OBJREF!  ', $
              'this datatype is not supported till now!'
     status=5
     return
  endif
;;;;;;;;;;;;;; ERROR HANDLING ;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;; file does not exist ;;;;;;;;;;;;;;;;;
  if ~FILE_TEST(RITFILE) then begin
;     print,'ERROR: could not find the rit file. ', $
;           'The file '+ritfile+' will be created'
     openw,LUN,RITFILE,/GET_LUN
     if ~keyword_set(NO_INFO)  then $
        printf,LUN,f='(1a)',comment_text
     printf,LUN,f='(1a)',rit_line(name,value)
  endif else begin
     d = READ_ASCII(RITFILE,TEMPLATE=name_tmpl)

;;;;;;;;;; file and data exists ;;;;;;;;;;;;;;;;;
     if size(d,/TYPE) eq 8L then begin
;;; find the entry for name
        openw,LUN,RITFILE,/GET_LUN
        if ~keyword_set(NO_INFO)  then $
           printf,LUN,f='(1a)',comment_text
        ind = where(strlowcase(strcompress(d.names,/R)) $
                    eq strlowcase(strcompress(name,/R)),whereNr)
        if whereNr lt 1 then begin
;;; write a new entry
;;; if value = scalar then at the beginning of the file
           if size(value,/N_DIM) eq 0 then $
              printf,LUN,f='(1a)',rit_line(name,value)
;              printf,LUN,f='(1a)',strcompress(name)+' = '+strcompress(val)
           printf,LUN,f='(1a)',strcompress(d.names)+' = '+strcompress(d.values)
;;; if value = array then at the end of the file
           if size(value,/N_DIM) ne 0 then $
              printf,LUN,f='(1a)',rit_line(name,value)
;              printf,LUN,f='(1a)',strcompress(name)+' = '+strcompress(val)
        endif else begin
;;; modify the name-entry
           d.values[ind[0]] = rit_line(name,value,/FIRST_VALUE)
           printf,LUN,f='(1a)',strcompress(d.names)+' = '+strcompress(d.values)
        endelse

;;;;;;;;;;;; file exists, but no data inside ;;;;;;;;;;;;;;;;;
     endif else begin 
        openw,LUN,RITFILE,/GET_LUN
        if ~keyword_set(NO_INFO)  then $
           printf,LUN,f='(1a)',comment_text
        printf,LUN,f='(1a)',rit_line(name,value)
     endelse
  endelse
  close,LUN
  free_lun,LUN
end


pro rit_write, ritfile, name, value, RAT_PAR=RAT_PAR, RAT_EVOLUTION=RAT_EVOLUTION, RAT_PALETTE=RAT_PALETTE, STATUS=STATUS, NO_INFO=NO_INFO, DEBUG=DEBUG
  common rat
  if config.debug then debug=1
;  on_error,2
  status=0
  comment_text = rit_constants(/COMMENT_TEXT)
  name_tmpl = rit_constants(/NAME_TMPL)

;;;;;;;;;;;;;; ERROR HANDLING ;;;;;;;;;;;;;;;;;
  if n_elements(RAT_PAR) eq 0 && n_elements(RAT_EVOLUTION) eq 0 && n_elements(RAT_PALETTE) eq 0 && (n_elements(name) eq 0 || n_elements(value) eq 0) then begin
     if keyword_set(DEBUG) then $
        print,'please provide a name and its value!'
     status=1
     return
  endif
  if n_elements(RITFILE) eq 0 then begin
     if keyword_set(DEBUG) then $
        print,'please provide a file name!'
     status=2
     return
  endif
  if file_test(RITFILE) && ~file_test(RITFILE,/WRITE,/READ) then begin
     if keyword_set(DEBUG) then $
        print,'the file exists but this user is not allowed to read/write it!'
     status=3
     return
  endif
  if n_elements(RAT_PAR) eq 0 && n_elements(RAT_EVOLUTION) eq 0 && n_elements(RAT_PALETTE) eq 0  && size(value,/TYPE) eq 8L then begin
     if keyword_set(DEBUG) then $
        print,'the given value is a STRUCT!  ', $
              'no support of STRUCTs is implemented till now!'
     status=4
     return
  endif
  if n_elements(RAT_PAR) eq 0 && n_elements(RAT_EVOLUTION) eq 0 && n_elements(RAT_PALETTE) eq 0  && $
     (size(value,/TYPE) eq 10 || size(value,/TYPE) eq 11) then begin
     if keyword_set(DEBUG) then $
        print,'the given value is a POINTER or OBJREF!  ', $
              'this datatype is not supported till now!'
     status=5
     return
  endif
;;;;;;;;;;;;;; ERROR HANDLING ;;;;;;;;;;;;;;;;;

  if n_elements(RAT_PAR) ne 0 then begin 
     PN        = ptr_new()
     par_nr    = n_tags(RAT_PAR)
     par_names = tag_names(rat_par)
  endif


  if file_test(RITFILE) then $
     d = READ_ASCII(RITFILE,TEMPLATE=name_tmpl)
  openw,LUN,RITFILE,/GET_LUN
  if ~keyword_set(NO_INFO)  then $
     printf,LUN,f='(1a)',comment_text

  if ~FILE_TEST(RITFILE) || size(d,/type) ne 8L then begin
;;;;;;;;;;;; file does not exists           ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; file exists and no data inside ;;;;;;;;;;;;;;;;;
;    printf,LUN,f='(1a)',';##################  RAT_EVOLUTION -- DEVELOPMENT OF THE FILE DATA  ##################;'
     if n_elements(RAT_EVOLUTION) ne 0 then $
        for i=0,n_elements(rat_evolution)-1 do $
           printf,LUN,f='(1a)',rit_line('RAT_EVOLUTION_'+strcompress(i,/R),rat_evolution[i])
;    printf,LUN,f='(1a)',';##################                 RAT PARAMETERS                  ##################;'
     if n_elements(RAT_PAR) ne 0 then $
        for i=0,par_nr-1 do $
           if rat_par.(i) ne PN then $
              printf,LUN,f='(1a)',rit_line(par_names[i],*rat_par.(i))
     if n_elements(name) gt 0 && n_elements(value) gt 0 then $
        printf,LUN,f='(1a)',rit_line(name,value)
     if n_elements(RAT_PALETTE) ne 0 $
        then printf,LUN,f='(1a)',rit_line('palette',RAT_PALETTE)
  endif else begin
;;;;;;;;;; file and data exists ;;;;;;;;;;;;;;;;;
;;; find the entry for name
     old = bytarr(n_elements(d.names))+1
     if n_elements(RAT_PAR) ne 0 then $
        for i=0,par_nr-1 do $
           old *= strlowcase(strcompress(d.names,/R)) ne strlowcase(strcompress(par_names[i],/R))
     if n_elements(RAT_EVOLUTION) ne 0 then $
        for i=0,n_elements(RAT_EVOLUTION)-1 do $
           old *= strlowcase(strcompress(d.names,/R)) ne 'rat_evolution_'+strcompress(i,/R)
     if n_elements(name) ne 0 && n_elements(value) ne 0 then $
        old *= strlowcase(strcompress(d.names,/R)) ne strlowcase(strcompress(name,/R))
     
;     printf,LUN,f='(1a)',';##################  RAT_EVOLUTION -- DEVELOPMENT OF THE FILE DATA  ##################;'
     if n_elements(RAT_EVOLUTION) ne 0 then $
        for i=0,n_elements(rat_evolution)-1 do $
           printf,LUN,f='(1a)',rit_line('RAT_EVOLUTION_'+strcompress(i,/R),rat_evolution[i])
;     printf,LUN,f='(1a)',';##################            RAT PARAMETERS  -- SCALARS           ##################;'
     if n_elements(RAT_PAR) ne 0 then $
        for i=0,par_nr-1 do $
           if rat_par.(i) ne PN && n_elements(rat_par.(i)) eq 1 then $
              printf,LUN,f='(1a)',rit_line(par_names[i],*rat_par.(i))
;     printf,LUN,f='(1a)',';##################            RAT PARAMETERS  -- ARRAYS            ##################;'
     if n_elements(RAT_PAR) ne 0 then $
        for i=0,par_nr-1 do $
           if rat_par.(i) ne PN && n_elements(rat_par.(i)) gt 1 then $
              printf,LUN,f='(1a)',rit_line(par_names[i],*rat_par.(i))
;    printf,LUN,f='(1a)',';##################                PARAMETERS                       ##################;'
     if n_elements(value) eq 1 then $
        printf,LUN,f='(1a)',rit_line(name,value)
     if total(old) gt 0 then $
        for i=0,n_elements(old)-1 do $
           if old[i] then $
              printf,LUN,f='(1a)',strcompress(d.names(i))+' = '+strcompress(d.values(i))
     if n_elements(value) gt 1 then $
        printf,LUN,f='(1a)',rit_line(name,value)
     if n_elements(RAT_PALETTE) ne 0 then $
        printf,LUN,f='(1a)',rit_line('palette',RAT_PALETTE)
  endelse
  close,LUN
  free_lun,LUN
end
