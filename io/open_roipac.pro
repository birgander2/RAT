
;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_uavsar
; written by    : Maxim Neumann
; last revision : Nov.2010
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


pro open_roipac, CALLED=CALLED, INPUTFILE = inputfile
  common rat
  common channel, channel_names, channel_selec, color_flag, palettes, pnames
  
  
  if n_elements(inputfile) eq 0 then begin ; GUI for file selection
    path = config.workdir
    inputfile = cw_rat_dialog_pickfile(TITLE='Open ROI_pac file', $
                                       DIALOG_PARENT=wid.base, $
                                       FILTER = '*.rsc', $
                                       /MUST_EXIST, PATH=path, GET_PATH=path)
    if strlen(inputfile) gt 0 then config.workdir = path
  endif
  if strlen(inputfile) le 0 then return

; change mousepointer
  WIDGET_CONTROL,/hourglass     ; switch mouse cursor

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED
  open_rit,/EMPTY               ; no parameters are set: delete the odl ones!

; converting format to RAT, based on Maxim's read_roipac routine
;  file = inputfile
;;; BIL: .cor, .hgt, .unw, .msk, .trans
;;; BIP: .slc, .int, .amp, .slp
  post = strmid(inputfile,3,/reverse_offset)
  if post eq '.rsc' then begin
    inputfile=strmid(inputfile,0,strlen(inputfile)-4)
    post = strmid(inputfile,3,/reverse_offset)
  endif
  if total(post eq ['.cor','.hgt','.unw','.msk']) || $
    strmid(inputfile,5,/rev) eq '.trans' $
  then BIL = 1
  if ~keyword_set(BIL) then BIP=1
  cmplx = post eq '.int' || post eq '.slc'
  rsc_file = inputfile+'.rsc'
  openr,lun,/get_lun,rsc_file & s=''
  while ~EOF(lun) do begin
    readf,lun,s
    if strupcase(strmid(s,0,5))eq 'WIDTH' then $
      n0=Long((strsplit(s,/extr))[1])
    if strupcase(strmid(s,0,11))eq 'FILE_LENGTH' then $
      n1=Long((strsplit(s,/extr))[1])
  endwhile
  free_lun,lun
  siz=(file_info(inputfile)).size
  case siz of
    1L*n0*n1: s1=1
    2L*n0*n1: s2=1
    4L*n0*n1: s4=1
    8L*n0*n1: s8=1
  4L*(n0+1)*n1: begin & n0++ & s4=1 & end
  8L*(n0+1)*n1: begin & n0++ & s8=1 & end
  4L*n0*(n1+1): begin & n1++ & s4=1 & end
  8L*n0*(n1+1): begin & n1++ & s8=1 & end
    else: message,'::read_ROIpac:: data structure not recognized'
  endcase

  n = [n0,n1]
  newvar  = 6L
  case post of
    '.slc': newtype = 101L
    '.int': newtype = 301L
    '.cor': begin
      newtype = 56L
      newvar  = 4L
    end
    '.amp': begin
      newvar  = 4L
      newtype = 51L
      newdims = [3L,2L,n,newvar]
    end
    '.unw': begin
      newtype = 303L
      newvar  = 4L
    end
    '.hgt': begin
      newvar = 4L
      newtype = 50L
      newdims = [3L,2L,n,newvar]
    end
    'rans': begin
      newvar = 4L
      newtype = 4L
      newdims = [3L,2L,n,newvar]
    end
    '.dem': begin
      newvar = 2L
      newtype = 2L
    end
    else: begin
      print,'File type not recognized!'
      return
    end
  endcase
  if n_elements(newdims) eq 0 then $
    newdims = [2L,n,newvar]
  info = file_basename(inputfile,'.rsc')
  
  if keyword_set(s1) then dat = bytarr(n0,n1)
  if keyword_set(s2) then dat = intarr(n0,n1)
  if keyword_set(s4) then dat = fltarr(n0,n1)
  if keyword_set(s8) && keyword_set(BIL) then dat = fltarr(n0,2,n1)
  if keyword_set(s8) && keyword_set(BIP) then $
    dat = cmplx? complexarr(n0,n1): fltarr(2,n0,n1)

  openr,ddd,/get_lun,inputfile,xdr=0                             ; open input file
  srat,outputfile,eee,header=newdims,type=newtype,info=info ; write RAT file header
  
  progress,message='Reading ROI_pac file...'
  readu, ddd, dat
  if keyword_set(s8) && keyword_set(BIL) then $
    dat = transpose(temporary(dat),[1,0,2])
  if ~keyword_set(NO_YFLIP) then dat = reverse(dat,size(dat,/n_Dim),/overwrite)
  case post of
    '.cor': dat = reform(dat[1,*,*],/ov)
    '.unw': dat = reform(dat[1,*,*],/ov)
    else:
  endcase
  writeu,eee,dat
  
  free_lun,ddd,eee              ; close all files

; set internal variables of RAT

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.info = info              ; Put here a string describing your data
  file.type = newtype           ; data type (101 = single channel complex)
  file.dim  = newdims[0]        ; 
  file.xdim = n[0]               ; range image size
  file.ydim = n[1]               ; azimuth image size
  file.vdim = 1L                ; nr. of layers (set to 1 if not needed)
  file.zdim = file.dim eq 2?1L: 2L
  file.var  = newvar      ; IDL variable type (6 = complex, 4 = floating point)
; update file generation history (evolution)

  evolute,'Import ROI_pac data from: '+file_basename(inputfile)

; reset palette information
  palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
  palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

; generate preview

  file.window_name = 'Untitled.rat'
  generate_preview
  update_info_box

end
