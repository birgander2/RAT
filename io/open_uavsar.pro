;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_uavsar
; written by    : Maxim Neumann
; last revision : Nov.2009
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

function get_field, fields, name
  return, fields.values[(where(strlowcase(strcompress(fields.names,/R)) $
                eq strlowcase(strcompress(name,/R)),whereNr))[0]]
end

function open_uavsar_info, inputfile, x=x,y=y, CHANNEL_TYPE=channel_type, info=info
  common rat
  common uavsar_info, slc_files, mlc_files, slcX, slcY, mlcX, mlcY, slc_available, mlc_available, path
  if ~file_test(inputfile,/READ) then return, 1
  name_tmpl = { VERSION:        FLOAT(1.00000), $
                DATASTART:      LONG(0), $
                DELIMITER:      BYTE(61), $
                MISSINGVALUE:   FLOAT(!VALUES.F_NAN), $
                COMMENTSYMBOL:  STRING(';'), $
                FIELDCOUNT:     LONG(2), $
                FIELDTYPES:     LONG([7,7]), $
                FIELDNAMES:     STRING(['names','values']), $
                FIELDLOCATIONS: LONG([0,18]), $ ; ???
                FIELDGROUPS:    LONG([0,1]) } ; ???
  d = READ_ASCII(inputfile,TEMPLATE=name_tmpl)
;;; splits the names string and compares only the first part till the
;;; first whitespace!
;;; might be modified later to include the whole description including
;;; the units.
  for i=0, n_elements(d.names)-1 do $
     d.names[i] = (strsplit(d.names[i],' ',/extract))[0]

  path = file_dirname(inputfile,/m)
;;; get parameter information for UAVSAR
  slcHH = get_field(d, 'slcHH')
  slcHV = get_field(d, 'slcHV')
  slcVV = get_field(d, 'slcVV')
  slcVH = get_field(d, 'slcVH')

  mlcHHHH = get_field(d, 'mlcHHHH')
  mlcHVHV = get_field(d, 'mlcHVHV')
  mlcVVVV = get_field(d, 'mlcVVVV')
  mlcHHHV = get_field(d, 'mlcHHHV')
  mlcHHVV = get_field(d, 'mlcHHVV')
  mlcHVVV = get_field(d, 'mlcHVVV')


  slcX = LONG(get_field(d,'slc_mag.set_cols'))
  slcY = LONG(get_field(d,'slc_mag.set_rows'))

  mlcX = LONG(get_field(d,'mlc_mag.set_cols'))
  mlcY = LONG(get_field(d,'mlc_mag.set_rows'))

  slc_files = [slcHH,slcVV,slcHV,slcVH]
  mlc_files = [mlcHHHH,mlcVVVV,mlcHVHV,mlcHHVV,mlcHHHV,mlcHVVV]

  slc_available = 1
  for i=0, 3 do slc_available *= file_test(path+slc_files[i],/read)
  mlc_available = 1
  for i=0, n_elements(mlc_files)-1 do mlc_available *= file_test(path+mlc_files[i],/read)

  grdHHHH = get_field(d, 'grdHHHH')
  grdHVHV = get_field(d, 'grdHVHV')
  grdVVVV = get_field(d, 'grdVVVV')
  grdHHHV = get_field(d, 'grdHHHV')
  grdHHVV = get_field(d, 'grdHHVV')
  grdHVVV = get_field(d, 'grdHVVV')
  grdX = LONG(get_field(d,'grd_mag.set_cols'))
  grdY = LONG(get_field(d,'grd_mag.set_rows'))
  grd_files = [grdHHHH,grdVVVV,grdHVHV,grdHHVV,grdHHHV,grdHVVV]
  grd_available = 1
  for i=0, n_elements(grd_files)-1 do grd_available *= file_test(path+grd_files[i],/read)

  return, ~(slc_available || mlc_available || grd_available)
end

pro open_uavsar, CALLED=CALLED, INPUTFILE = inputfile
  common rat
  common channel, channel_names, channel_selec, color_flag, palettes, pnames
  common uavsar_info

  if ~keyword_set(inputfile) then begin ; GUI for file selection
     path = config.workdir
     inputfile = cw_rat_dialog_pickfile(TITLE='Open UAVSAR file (provide .ann file)', $
                                        DIALOG_PARENT=wid.base, FILTER = '*.ann', /MUST_EXIST, PATH=path, GET_PATH=path)
     if strlen(inputfile) gt 0 then config.workdir = path
  endif
  if strlen(inputfile) le 0 then return

; change mousepointer
  WIDGET_CONTROL,/hourglass     ; switch mouse cursor

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
   open_rit,/EMPTY              ; no parameters are set: delete the odl ones!

; converting format to RAT
  
  err=open_uavsar_info(inputfile)
  if err then return
  path = file_dirname(inputfile,/m)

;;; multi-look complex data
  if mlc_available then begin
     n = [mlcX, mlcY]
     nrch = n_elements(mlc_files)
     newtype = 220L
     newdims = [4L,3L,3L,n,6L]
     info = file_basename(inputfile,'.ann')
     ddd    = lonarr(nrch)
     for ch=0,nrch-1 do begin
        get_lun,tmp
        ddd[ch] = tmp
        openr,ddd[ch],path+mlc_files[ch],xdr=0 ; open input file
;        readu,ddd[ch],header
     endfor
     srat,outputfile,eee,header=newdims,type=newtype,info=info ; write RAT file header

     nrx=n[0] & nry=n[1]

     bs_y  = config.blocksize/nrch             ; get standard RAT blocksize
     blockCmpl = complexarr(    nrx,bs_y,/nozero) ; define block
     blockFlt  = fltarr(    nrx,bs_y,/nozero) ; define block
     blockALL  = complexarr(3,3,nrx,bs_y,/nozero)
     nr_y  = nry  /  bs_y       ; calc nr. of blocks
     re_y  = nry mod bs_y       ; calc size of last block
     sqr2  = sqrt(2.)


     progress,message='Reading UAVSAR files...'
  
     for i=0,nr_y-1 do begin           ; read and write blocks
        progress,percent=((i+1)*100)/nr_y ; display progress bar
        readu,ddd[0],blockFlt
        blockALL[0,0,*,*]=blockFlt
        readu,ddd[1],blockFlt
        blockALL[1,1,*,*]=blockFlt
        readu,ddd[2],blockFlt
        blockALL[2,2,*,*]=blockFlt  *2.
        readu,ddd[3],blockCmpl
        blockALL[0,1,*,*]=blockCmpl
        readu,ddd[4],blockCmpl
        blockALL[0,2,*,*]=blockCmpl *sqr2
        readu,ddd[5],blockCmpl
        blockALL[2,1,*,*]=blockCmpl *sqr2

        blockALL[1,0,*,*]=conj(blockALL[0,1,*,*])
        blockALL[2,0,*,*]=conj(blockALL[0,2,*,*])
        blockALL[1,2,*,*]=conj(blockALL[2,1,*,*])

        writeu,eee,blockALL
     endfor
     if re_y gt 0 then begin    ; read and write last block
        blockFlt  = fltarr(     nrx,re_y,/nozero)
        blockCmpl = complexarr(     nrx,re_y,/nozero)
        blockALL  = complexarr(3,3 ,nrx,re_y,/nozero)

        readu,ddd[0],blockFlt
        blockALL[0,0,*,*]=blockFlt
        readu,ddd[1],blockFlt
        blockALL[1,1,*,*]=blockFlt
        readu,ddd[2],blockFlt
        blockALL[2,2,*,*]=blockFlt  *2.
        readu,ddd[3],blockCmpl
        blockALL[0,1,*,*]=blockCmpl
        readu,ddd[4],blockCmpl
        blockALL[0,2,*,*]=blockCmpl *sqr2
        readu,ddd[5],blockCmpl
        blockALL[2,1,*,*]=blockCmpl *sqr2

        blockALL[1,0,*,*]=conj(blockALL[0,1,*,*])
        blockALL[2,0,*,*]=conj(blockALL[0,2,*,*])
        blockALL[1,2,*,*]=conj(blockALL[2,1,*,*])

        writeu,eee,blockALL
     endif
     for ch=0,nrch-1 do $
        free_lun,ddd[ch]
     free_lun,eee               ; close all files

; set internal variables of RAT

     file_move,outputfile,finalfile,/overwrite
     file.name = finalfile
     file.info = info           ; Put here a string describing your data
     file.type = newtype        ; data type (101 = single channel complex)
     file.dim  = newdims[0]     ; 
     file.xdim = nrx            ; range image size
     file.ydim = nry            ; azimuth image size
     file.zdim = 3L             ; nr. of layers (set to 1 if not needed)
     file.vdim = 3L             ; nr. of layers of layers (set to 1 if not needed)
     file.var  = 6l             ; IDL variable type (6 = complex, 4 = floating point)
  end
; update file generation history (evolution)

  evolute,'Import UAVSAR data from: '+strjoin(file_basename(inputfile),', ')

; reset palette information

	palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
	palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

  file.window_name = 'Untitled.rat'
  generate_preview
;;; splits the names string and compares only the first part till the
;;; first whitespace!
;;; splits the names string and compares only the first part till the
;;; first whitespace!
;;; splits the names string and compares only the first part till the
;;; first whitespace!
;;; splits the names string and compares only the first part till the
;;; first whitespace!
  update_info_box

end
