;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: rit_read
; written by    : Maxim Neumann
; last revision : 07.Oct.2005
; Reads an entry with name=value syntax from a given file
; PARAMETERS:
;   filename:    [string] An existing readable file
;   name:        [string] you should know it
;   value:       here the value of the entry will be written to
;   status=st:   if ok, 0 is returned, else some error code
;  /NAMES_PRINT: print out the names of all entries
;  /ALL_PRINT:   print out the names and the values of all entries
; TODO:
;   implement support for structures, string arrays, pntr, objref
; EXAMPLE:
;   rit_read,'file.rit','flat-earth',fe
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
pro rit_read, FILENAME, name, value, STATUS=STATUS, $
              NAMES_PRINT=NAMES_PRINT, ALL_PRINT=ALL_PRINT, DEBUG=DEBUG
;  on_error,2
  status=0

;;;;;;;;;;;;;; ERROR HANDLING ;;;;;;;;;;;;;;;;;
  if n_elements(FILENAME) eq 0 || ~FILE_TEST(FILENAME,/READ) then begin
     if keyword_set(DEBUG) then $
        print,'could not find the rit file or is not readable'
     status = 1
     return
  endif
  name_tmpl = { VERSION:        FLOAT(1.00000), $
                DATASTART:      LONG(0), $
                DELIMITER:      BYTE(61), $
                MISSINGVALUE:   FLOAT(!VALUES.F_NAN), $
                COMMENTSYMBOL:  STRING(';'), $
                FIELDCOUNT:     LONG(2), $
                FIELDTYPES:     LONG([7,7]), $
                FIELDNAMES:     STRING(['names','values']), $
                FIELDLOCATIONS: LONG([0,6]), $ ; ???
                FIELDGROUPS:    LONG([0,1]) } ; ???
  d = READ_ASCII(FILENAME,TEMPLATE=name_tmpl)
  if keyword_set(NAMES_PRINT) then $
     print, f='(1a)',d.names
  if keyword_set(ALL_PRINT) then $
     print, f='(1a)',strcompress(d.names)+' = '+strcompress(d.values)

  if n_elements(name) ne 0 && arg_present(value) then begin
     ind = where(strlowcase(strcompress(d.names,/R)) $
                 eq strlowcase(strcompress(name,/R)),whereNr)
     if whereNr lt 1 then begin
        if keyword_set(DEBUG) then $
           print,'no name could be found in file '+FILENAME
        status = 2
        return
     endif
     val   = strsplit(strcompress(d.values[ind[0]]),' ',/EXTRACT)
     type  = LONG(val[0])
     if type lt 1 and type gt 15 then begin
        if keyword_set(DEBUG) then $
           print,'wrong data type is given in the file '+filename+ $
                 ' for variable name '+name
        status = 3
        return
     endif
     if type eq 8L then begin
        if keyword_set(DEBUG) then $
           print,'the entry name "'+strcompress(name)+'" corresponds to a STRUCT!  ', $
                 'no value is returned!'
        status = 4
        return
     endif
;;;;;;;;;;;;;; ERROR HANDLING ;;;;;;;;;;;;;;;;;

     N_DIM = LONG(val[1])
     if type ne 6L && type ne 9L then begin
        if N_DIM eq 0 then begin
           if type eq 7L then value = STRJOIN(val[2:*],' ') $
           else value = (make_array(1,TYPE=type,VALUE=val[2]))[0]
        endif else begin
           DIM = LONG(val[2:N_DIM+1])
           value = make_array(DIM,TYPE=type,/NOZERO)
           if type eq 7L then value[*] = strsplit(strjoin(val[N_DIM+2:*],' '),'\\0',/REGEX,/EXTRACT) $
           else value[*] = val[N_DIM+2:N_DIM+1+product(DIM)]
        endelse
     endif else begin
        cval = strsplit(strjoin(val[2+N_DIM:*]),' ,()',/EXTRACT)
        if N_DIM eq 0 then begin
           if type eq 6L then value = complex(cval[0],cval[1]) $
           else value = dcomplex(cval[0],cval[1])
        endif else begin
           DIM = LONG(val[2:N_DIM+1])
           value = make_array([2,DIM],TYPE=type,/NOZERO)
           value[*] = cval[0:product(DIM)*2-1]
           if type eq 6L then value = $
              reform(complex(value[0,*,*,*,*,*,*],value[1,*,*,*,*,*,*])) $
           else value = $
              reform(dcomplex(value[0,*,*,*,*,*,*],value[1,*,*,*,*,*,*]))
        endelse
     endelse
  endif
end

