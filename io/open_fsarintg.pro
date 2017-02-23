; docformat = 'rst'
;+
; Opens a file in F-SAR INTG format
;
; :Keywords:
;    inputfile: in, required, type=string
;       the name of the file to be opened
;
; :Author: Andreas Reigber
; :Categories: IO
;
; :Copyright:
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
;-
pro open_fsarintg,INPUTFILE = inputfile
   common rat, types, file, wid, config, tiling
   common channel, channel_names, channel_selec, color_flag, palettes, pnames
   compile_opt idl2

   if not keyword_set(inputfile) then begin
      path = config.workdir
      inputfile = cw_rat_dialog_pickfile(TITLE='Open F-SAR INTG file', $
                                         DIALOG_PARENT=wid.base, FILTER = '*.dat', /MUST_EXIST, PATH=path, GET_PATH=path)
      if strlen(inputfile) gt 0 then config.workdir = path
   endif

   if strlen(inputfile) gt 0 then begin

; change mousepointer
      
      WIDGET_CONTROL,/hourglass

; undo function
      
      undo_prepare,outputfile,finalfile,CALLED=CALLED
      open_rit,/EMPTY           ; no parameters are set: delete the odl ones!

; converting Rolf's format to RAT
      
      bytnr = 0l
      anzx  = 0l
      anzy  = 0l
      openr,ddd,inputfile,/xdr,/get_lun
      readu,ddd,bytnr
      readu,ddd,anzy
      readu,ddd,anzx
      inf = fstat(ddd)
      anzy = (inf.size - 12 ) / anzx / 8
      srat,config.tempdir+config.workfile1,eee,header=[2l,anzx,anzy,4l],type=100l

      bs_y  = config.blocksize
      zeile = complexarr(anzx,bs_y)
      nr_y  = anzy  /  bs_y
      re_y  = anzy mod bs_y
      
      progress,message='Reading F-SAR INTG file...'
      
      for i=0,nr_y-1 do begin
         progress,percent=((i+1)*100)/nr_y
         readu,ddd,zeile
         writeu,eee,zeile
      endfor
      if re_y gt 0 then begin
         zeile = complexarr(anzx,re_y)
         readu,ddd,zeile
         writeu,eee,zeile
      endif		
      free_lun,ddd,eee
      
; read header

      head = 1l
      rrat,config.tempdir+config.workfile1,ins1,header=head,info=info
      free_lun,ins1
      
; analyse header
      
      file.name = config.tempdir+config.workfile1
      file.info = info
      file.type = 101l
      file.dim  = head[0]
      if file.dim eq 2 then begin
         file.xdim = head[1]
         file.ydim = head[2]
         file.zdim = 1l
         file.vdim = 1l
         file.var  = head[3]
      endif
      if file.dim eq 3 then begin
         file.xdim = head[2]
         file.ydim = head[3]
         file.zdim = head[1]
         file.vdim = 1l
         file.var  = head[4]
      endif
      if file.dim eq 4 then begin
         file.xdim = head[3]
         file.ydim = head[4]
         file.zdim = head[1]
         file.vdim = head[2]
         file.var  = head[5]
      endif
      
      file.window_name = 'Untitled.rat'
      rat_finalise,outputfile,finalfile
      evolute,'Open F-SAR INTG file'
           
   endif	
end
