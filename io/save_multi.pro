;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module    : save_multi.pro
; Author        : Marco Saemmang (Technical University Berlin, Germany)
; Last revision : March 2006
; Modified      : mn, 08/2006
; Read a multifile and estimate the offset for coregistration.
; If user wants, RAT coregists the subfiles.
; If user wants, RAT resizes the subfiles to a common size an saves them.
; PRO save_multi  - saves new multifile and changes the involved subfiles
;                   and resizes the files to a common size
;				  - creates multifile ( header and filenames ) with
; 					subfile-informations and previews
; module for saving new multifile and resizing subfiles
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

PRO save_multi, files
  COMMON rat, types, file, wid, config

;;; create new filenames to work with (at first in config.tempdir)
  multi_names = MAKE_ARRAY(file.mult, /STRING)
  FOR index = 0, file.mult-1 DO BEGIN
     multi_names[index] = config.tempdir + STRMID(FILE_BASENAME(files[index].name), 0, STRLEN(FILE_BASENAME(files[index].name))-4) + '_multi.rat'
  ENDFOR

;;; find max and min of x and y to check if resizing is necessary
  max_x = LONG(MAX(files[0:file.mult-1].x))
  max_y = LONG(MAX(files[0:file.mult-1].y))
  min_x = LONG(MIN(files[0:file.mult-1].x))
  min_y = LONG(MIN(files[0:file.mult-1].y))

;;; starts a procedure to change file-dimensions if necessary
  IF (min_x NE max_x) OR (min_y NE max_y) THEN BEGIN
     progress, MESSAGE = 'Resizing files ...'
                                ; loop to control the single datasets
     FOR index = 0, file.mult-1 DO BEGIN
                                ; show status of progress
        progress, percent = (index+1)*100.0/file.mult
                                ; change the file dimensions - fill with zero - if file is smaller than max
        IF (files[index].y NE max_y) OR (files[index].x NE max_x) THEN BEGIN
           rrat, files[index].name, inputfile, header = head, info = info, type = type
                                ; pay attention to save the common size of array
           head_temp = head
           head_temp[head[0]] = LONG(max_y)
           head_temp[head[0]-1] = LONG(max_x)
                                ; write the typical RAT-header in tempfile
           srat, multi_names[index], outputfile, header = head_temp, info = info, type = type
                                ; resize the orginal data to the common size
                                ; calculate number of y-blocks for copying
           calc_blocks_normal, head[head[0]], config.blocksize, nr_blocks, bs_last
           IF bs_last EQ 0 THEN BEGIN
              nr_blocks-=1
              bs_last = config.blocksize
           ENDIF
           blocksizes = INTARR(nr_blocks) + config.blocksize
           blocksizes[nr_blocks-1] = bs_last
                                ; get value for z and v
           size_z = 1l
           size_v = 1l
           IF head[0] GT 2 THEN size_z = head[head[0]-2]
           IF head[0] GT 3 THEN size_v = head[head[0]-3]

                                ; first fill columns with zero - if necessary
           IF max_x GT head[head[0]-1] THEN BEGIN
              FOR i_block = 0, nr_blocks-1 DO BEGIN
                 array_zero = REFORM(MAKE_ARRAY(size_v, size_z, (max_x-head[head[0]-1]), blocksizes[i_block], type = head[head[0]+1]))
                 array_in = REFORM(MAKE_ARRAY(size_v, size_z, head[head[0]-1], blocksizes[i_block], type = head[head[0]+1]))
                 READU, inputfile, array_in
                 WRITEU, outputfile, [array_in, array_zero]
              ENDFOR
                                ; else copy matrix blockwise
           ENDIF ELSE BEGIN
              FOR i_block = 0, nr_blocks-1 DO BEGIN
                 array_in = MAKE_ARRAY(size_v, size_z, head[head[0]-1], blocksizes[i_block], type = head[head[0]+1])
                 READU, inputfile, array_in
                 WRITEU, outputfile, array_in
              ENDFOR
           ENDELSE
                                ; now enlarge the matrix with rows of zero - if necessary
           IF max_y GT head[head[0]] THEN BEGIN
              diff = max_y - head[head[0]]
              array_zero = MAKE_ARRAY(size_v, size_z, max_x, type = head[head[0]+1])
              FOR i_line = 0, diff-1 DO BEGIN
                 WRITEU, outputfile, array_zero
              ENDFOR
           ENDIF
                                ; close input- and outputfile after resizing
           FREE_LUN, outputfile, inputfile
        ENDIF ELSE BEGIN
                                ; copy original file without changes for using in multifile-project
           FILE_COPY, files[index].name, multi_names[index], /ALLOW_SAME, /OVERWRITE
           FILE_CHMOD, multi_names[index], /A_WRITE
        ENDELSE
     ENDFOR
     progress, /DESTROY
;;; if dimensions are equal - RAT makes only copies to work with
  ENDIF ELSE BEGIN
     progress, MESSAGE = 'Copy files ...'
     FOR index = 0, file.mult-1 DO BEGIN
        progress, percent = (index+1)*100/file.mult
        FILE_COPY, files[index].name, multi_names[index], /ALLOW_SAME, /OVERWRITE
        FILE_CHMOD, multi_names[index], /A_WRITE
     ENDFOR
     progress, /DESTROY
  ENDELSE

;;; ask for a filename to save the multifile
  REPEAT BEGIN
     path = config.workdir
     new_mfile = DIALOG_PICKFILE(TITLE = 'Save multifile', DIALOG_PARENT = wid.base, FILTER = '*.mrat', $
                                 PATH = path, GET_PATH = path, /WRITE, /OVERWRITE_PROMPT, DEFAULT_EXTENSION = 'mrat')
;;; ask second time for quit without saving - otherwise RAT will close the multifile-creator
;;; and deletes the generated subfiles
     IF STRLEN(new_mfile) EQ 0 THEN BEGIN
        info = DIALOG_MESSAGE(['Quit without saving', 'multifile?'], DIALOG_PARENT = wid.base, TITLE ='Exit', /QUESTION)
        IF info EQ 'Yes' THEN BEGIN
           FOR index = 0, file.mult-1 DO FILE_DELETE, multi_names[index], /ALLOW_NONEXISTENT, /QUIET
           RETURN
        ENDIF
     ENDIF
  ENDREP UNTIL (STRLEN(new_mfile) GT 0)
;;; set actual path to config.workdir
  config.workdir = path
;;; add extension .mrat if necessary
  IF ~(STRMATCH(new_mfile, '*.mrat', /FOLD_CASE) || STRMATCH(new_mfile, '*.rat', /FOLD_CASE)) THEN new_mfile = new_mfile + '.mrat'



;;; destination of subfiles
  subdirpath = path + file_basename(new_mfile,'.mrat')+'_mrat'+PATH_SEP()
  main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5, $
                     TITLE='Destination of Subfiles',/modal,/tlb_kill_request_events,/tlb_frame_attr)
  tmp  = widget_label(main,value='What should RAT do with the subfiles?')
  subdest = cw_bgroup(main,['leave as it is (only for this working session)', $
                            'move to the directory of the multi-file: '+path, $
                            'move to subdirectory of the multi-file: '+subdirpath], $
                      /exclusive,set_value=2)
  tmp2 = widget_label(main,value='Save subfile links in the multi-file as ')
  rel_but = cw_bgroup(main,['absolute paths','relative paths (DEFAULT)'],/exclusive,set_value=1)
  buttons = WIDGET_BASE(main,column=3,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
;                but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  WIDGET_CONTROL, main,/REALIZE,default_button=but_ok,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=pos[0], yoffset=pos[1]
  repeat begin                  ; Event loop
     event = widget_event(main)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['Destination of subfiles of a multi-file',$
;                    'default is leave as it is (relative link)',$
                    '', $
                    'RAT module written by Marco Saemmang and Andreas Reigber', $
                    'and modified 08/2006 by Maxim Neumann']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                              TITLE='Information')
     endif
  endrep until (event.id eq but_ok) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
  widget_control,subdest,GET_VALUE=subdest_choice
  widget_control,rel_but,GET_VALUE=be_relative
  widget_control,main,/destroy  ; remove main widget
;                if event.id ne but_ok then return ; OK button _not_ clicked

  case subdest_choice of
     2: begin 
        new_files = subdirpath + file_basename(multi_names)
        if ~file_test(subdirpath,/dir) then $
           file_mkdir,subdirpath
     end
     1: new_files = path + file_basename(multi_names)
     else:
  endcase
  if subdest_choice gt 0 then begin
;;; check if file already exists (in target-path)
     tester = total(file_test(new_files))
;;; ask for overwriting
     IF tester ge 1 THEN BEGIN
        overw = DIALOG_MESSAGE(['Subfile already exists in selected path!', 'Overwrite?'], DIALOG_PARENT = wid.base, $
                               TITLE = 'Question', /QUESTION)
        IF overw EQ 'Yes' THEN BEGIN
;;; status for moving
           progress, MESSAGE = 'Moving files ...'
           FOR index = 0, file.mult-1 DO BEGIN
              progress, percent = (index+1)*100.0/file.mult
;;; remove fileprotection if file exists and is writeprotected
              IF FILE_TEST(new_files[index]) EQ 1 THEN BEGIN
                 IF FILE_TEST(new_files[index], /WRITE) NE 1 $
                 THEN FILE_CHMOD, (new_files[index]), /A_WRITE
              ENDIF
;;; in case of multi-used file - copy the first only
              IF FILE_TEST(multi_names[index]) EQ 1 THEN BEGIN
;;; only moving no copy
                 FILE_MOVE, multi_names[index], new_files[index], /ALLOW_SAME, /OVERWRITE
              ENDIF
;;; change name of subfile for saving into the multifile
              multi_names[index] = new_files[index]
           ENDFOR
           progress, /DESTROY
        ENDIF
     ENDIF ELSE BEGIN
;;; status for moving
        progress, MESSAGE = 'Moving files ...'
        FOR index = 0, file.mult-1 DO BEGIN
           progress, percent = (index+1)*100.0/file.mult
           if ~file_test(/dir,file_dirname(new_files[index])) $
           then file_mkdir,file_dirname(new_files[index])
           IF FILE_TEST(multi_names[index]) EQ 1 THEN BEGIN
;;; only moving no copy
              FILE_MOVE, multi_names[index], new_files[index], /ALLOW_SAME, /OVERWRITE
           ENDIF
;;; change name of subfile for saving into the multifile
           multi_names[index] = new_files[index]
        ENDFOR
        progress, /DESTROY
     ENDELSE
  endif








; 		; ask if the subfiles should be moved to the multifile-path
; 		info = DIALOG_MESSAGE(['Should RAT moves the subfiles into', 'the same path as the multifile?'], DIALOG_PARENT = wid.base, $
; 		TITLE = 'Question', /QUESTION)
; 		IF info EQ 'Yes' THEN BEGIN
; 			; check if file already exists (in target-path)
; 			tester = 0
; 			FOR index = 0, file.mult-1 DO BEGIN
; 				test = FILE_TEST(path + FILE_BASENAME(multi_names[index]))
; 				IF test GT 0 THEN tester = 1
; 			ENDFOR
; 			; ask for overwriting
; 			IF tester EQ 1 THEN BEGIN
; 				overw = DIALOG_MESSAGE(['Subfile already exists in selected path!', 'Overwrite?'], DIALOG_PARENT = wid.base, $
; 				TITLE = 'Question', /QUESTION)
; 				IF overw EQ 'Yes' THEN BEGIN
; 					; status for moving
; 					progress, MESSAGE = 'Moving files ...'
; 					FOR index = 0, file.mult-1 DO BEGIN
; 						progress, percent = (index+1)*100.0/file.mult
; 						; remove fileprotection if file exists and is writeprotected
; 						IF FILE_TEST(path + FILE_BASENAME(multi_names[index])) EQ 1 THEN BEGIN
; 							IF FILE_TEST(path + FILE_BASENAME(multi_names[index]), /WRITE) NE 1 THEN FILE_CHMOD, (path + FILE_BASENAME(multi_names[index])), /A_WRITE
; 						ENDIF
; 						; in case of multi-used file - copy the first only
; 						IF FILE_TEST(multi_names[index]) EQ 1 THEN BEGIN
; 							; only moving no copy
; 							FILE_MOVE, multi_names[index], path + FILE_BASENAME(multi_names[index]), /ALLOW_SAME, /OVERWRITE
; 						ENDIF
; 						; change name of subfile for saving into the multifile
; 						multi_names[index] = path + FILE_BASENAME(multi_names[index])
; 					ENDFOR
; 					progress, /DESTROY
; 				ENDIF
; 			ENDIF ELSE BEGIN
; 				; status for moving
; 				progress, MESSAGE = 'Moving files ...'
; 				FOR index = 0, file.mult-1 DO BEGIN
; 					progress, percent = (index+1)*100.0/file.mult
; 					IF FILE_TEST(multi_names[index]) EQ 1 THEN BEGIN
; 						; only moving no copy
; 						FILE_MOVE, multi_names[index], path + FILE_BASENAME(multi_names[index]), /ALLOW_SAME, /OVERWRITE
; 					ENDIF
; 					; change name of subfile for saving into the multifile
; 					multi_names[index] = path + FILE_BASENAME(multi_names[index])
; 				ENDFOR
; 				progress, /DESTROY
; 			ENDELSE
; 		ENDIF

;   FOR index = 0, file.mult-1 DO BEGIN
;      IF index EQ 0 THEN BEGIN
; ;;; open first rat-file for details
;         rrat, multi_names[index], file_lun, header = header, info = info, type = type
;         FREE_LUN, file_lun
; ;;; open new multifile to save header and filenames
;         OPENW, outputfile, new_mfile, /GET_LUN, /XDR
;         WRITEU, outputfile, header
;         WRITEU, outputfile, type
;         WRITEU, outputfile, LONG64(0)
;         WRITEU, outputfile, LONG(file.mult)
;         bytinfo = BYTARR(80)
;         bytinfo[0] = BYTE("combined multifile")
;         WRITEU, outputfile, bytinfo
;      ENDIF
; ;;; save names of involved subfiles
;      WRITEU, outputfile, STRING(multi_names[index])
;   ENDFOR




;;; create multifile with typical header, names, previews etc.
;;; and fill the file.variables

;;; open first rat-file for details
  rrat, multi_names[0], file_lun, header = header, info = info, type = type
  FREE_LUN, file_lun
;;; open new multifile to save header and filenames
  OPENW, outputfile, new_mfile, /GET_LUN, /XDR
  WRITEU, outputfile, header
  WRITEU, outputfile, type
  WRITEU, outputfile, LONG64(0)
  WRITEU, outputfile, LONG(file.mult)
  bytinfo = BYTARR(80)
  bytinfo[0] = BYTE("combined multifile")
  WRITEU, outputfile, bytinfo
;;; create relative file paths if necessary
  if be_relative then for i=0,file.mult-1 do $
     multi_names[i]=file_relpath(multi_names[i],file_dirname(new_mfile))
;;; save names of involved subfiles
  for i=0,file.mult-1 do $
     writeu, outputfile, string(multi_names[i])
;;; close multifile
  FREE_LUN, outputfile

;;; set config.- and file.parameters
  file.name = new_mfile
  file.info = 'combined multifile with '+STRCOMPRESS(STRING(file.mult))+' '+types[type]+'s'
  file.type = type
  file.dim  = header[0]
  IF file.dim EQ 2 THEN BEGIN
     file.xdim = header[1]
     file.ydim = header[2]
     file.zdim = 1l
     file.vdim = 1l
     file.var  = header[3]
  ENDIF
  IF file.dim EQ 3 THEN BEGIN
     file.xdim = header[2]
     file.ydim = header[3]
     file.zdim = header[1]
     file.vdim = 1l
     file.var  = header[4]
  ENDIF
  IF file.dim EQ 4 THEN BEGIN
     file.xdim = header[3]
     file.ydim = header[4]
     file.zdim = header[2]
     file.vdim = header[1]
     file.var  = header[5]
  ENDIF

;;; recalculate previews
  generate_preview
  update_info_box
  RETURN
END
