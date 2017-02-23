;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: save_polsarpro
; VERSION 2.0
; last revision : March 2009
; written by    : Maxim Neumann
; Export data in POLSARPRO format
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


; docformat = 'rst'
;+
; Export to POLSARPRO data format
;
;  Untill now, this procedure support only the export of covariance
;  and coherency matrices. 
;
;
; :Keywords:
;    called: in, optional, type="flag"
;       no functionality yet
;    
; :Params:
;
; :Author: Maxim Neumann
; 
; :Categories: 
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
pro save_polsarpro, CALLED=CALLED
  common rat, types, file, wid, config, tiling
  common channel, channel_names, channel_selec, color_flag, palettes, pnames
  compile_opt idl2

  output_path = ''

  if ~((file.type ge 220 &&  file.type le 222) || (file.type ge 510 &&  file.type le 513)) $
  then begin
     ignore=dialog_message(['Untill now, only the export of covariance [C]', $
                            'and coherency [T] matrices is supported.'], $
                           /error,dialog_parent=wid.base,title='Error')
    return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl
  n_ch = pol*n_tr
  switch file.type of
     220:
     222:
     510:
     512: begin
        root = 'C'
        break
     end
     221:
     511:
     513:begin
        root = 'T'
        break
     end
     else: $
        stop
  endswitch
  dir = root + strcompress(n_ch, /R)
  path = config.workdir
  outputfile = cw_rat_dialog_pickfile(TITLE='Provide the main directory ' + $
                                      'to save POLSARPro data.', $
                                      DIALOG_PARENT=wid.base, PATH=path, $
                                      GET_PATH=path, /write, /dir) 
  if strlen(outputfile) gt 0 then config.workdir = path
  if strlen(outputfile) eq 0 then $
     return
  save_path = outputfile+dir+path_sep()
  quest=dialog_message(['The data will be saved in the directory', $
                        save_path], $
                       /question,dialog_parent=wid.base,title='Question')
  if quest eq 'No' then return
  file_mkdir, save_path
  files = strcompress(indgen(n_ch)+1, /r)+strcompress(indgen(n_ch)+1, /r)
  for i=1, n_ch do for j=i+1, n_ch do $
     files = [files, strcompress(i, /r)+strcompress(j, /r)+['_real', '_imag']] 
  files = save_path+root+files+'.bin'

;  luns = lonarr(n_elements(files))
  if n_elements(files) gt 90 then begin
     ignore=dialog_message(['To many channels', 'Maximum is 90 channels'], $
                           /error,dialog_parent=wid.base,title='Error')
     return
  endif
  luns = lindgen(n_elements(files)) +10 
; write the config.txt file
  openw, ddd, save_path+'config.txt', /get_lun
  printf, ddd, 'Nrow'
  printf, ddd, strcompress(file.ydim, /r)
  printf, ddd, '---------'
  printf, ddd, 'Ncol'
  printf, ddd, strcompress(file.xdim, /r) 
  printf, ddd, '---------'
  printf, ddd, 'PolarCase'
  printf, ddd, (pol eq 3? 'monostatic': 'bistatic')
  printf, ddd, '---------'
  printf, ddd, 'PolarType'
  printf, ddd, 'full'
  free_lun, ddd

; change mousepointer
  WIDGET_CONTROL,/hourglass

; copy file to destination
  inputfile  = file.name
		
; read / write header
  rrat,inputfile,ddd,header=head,info=info,type=type

  tiling_init

  for i=0, n_elements(luns)-1 do begin
     openw, luns[i], files[i]
  endfor 

  progress,Message='Export to POLSARPro...',/cancel_button
   for i=0,tiling.nr_blocks-1 do begin
      progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
      if wid.cancel eq 1 then return

;------------------------------------------------------------
; Read tile from input file (FIXED)
; -> after reading the array is 4-dimensional. To get rid of
;    leading empty dimensions use the reform() command
;------------------------------------------------------------
      tiling_read,ddd,i,block

; -------- YOUR FILTER CALL----------
      for x=0, n_ch-1 do $
         writeu, luns[x], real_part(block[x, x, *, *])
      z = n_ch
      for x=0, n_ch-1 do for y=x+1, n_ch-1 do begin
         writeu, luns[z++], real_part(block[x, y, *, *])
         writeu, luns[z++], imaginary(block[x, y, *, *])
      endfor
; -------- YOUR FILTER CALL----------	

   endfor
   progress,/destroy

;------------------------------------------------------------
; Free LUNs (FIXED)
;------------------------------------------------------------
   free_lun,ddd
   for i=0, n_elements(luns)-1 do $
      free_lun, luns[i]

   evolute,'Export data to POLSARPro'
end
