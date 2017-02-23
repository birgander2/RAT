;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: open_rat
; last revision :  29.Mar.2005
; written by    :  Andreas Reigber
; Reads RAT file
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

pro open_rat,INPUTFILE = inputfile,noupdate=noupdate,FROM_UNDO=FROM_UNDO, CALLED=called
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag, palettes, pnames

  if ~keyword_set(noupdate) then noupdate=0
  if ~keyword_set(inputfile) then begin
     path = config.workdir
;filters = ['*.jpg;*.jpeg', '*.tif;*.tiff', '*.png'] 
;file = DIALOG_PICKFILE(/READ, FILTER = filters) 
     default_filters = ['*.rat']
     inputfile = cw_rat_DIALOG_PICKFILE(TITLE='Open PyRAT file', DIALOG_PARENT=wid.base, $
                                        FILTER = default_filters, /MUST_EXIST, PATH=path, GET_PATH=path)
     if strlen(inputfile) gt 0 then config.workdir = path
  endif else if file_dirname(inputfile,/mark_directory) ne config.tempdir then config.workdir = file_dirname(inputfile,/mark_directory)

  if strlen(inputfile) gt 0 and FILE_TEST(inputfile) then begin

; change mousepointer
     WIDGET_CONTROL,/hourglass

; update config
     if ~keyword_set(FROM_UNDO) && strlen(file.name) le 2 then begin
        config.undofile = ''
        config.redofile = ''
        widget_control,wid.button_undo,set_value=config.imagedir+'undo2.bmp',/BITMAP
        widget_control,wid.button_redo,set_value=config.imagedir+'redo2.bmp',/BITMAP
     endif else if ~keyword_set(FROM_UNDO) && strlen(file.name) gt 2 then $
        undo_prepare,/OPEN_RAT

; read header
     head = 1l
     rrat,inputfile,ins1,header=head,info=info,type=type,multi=multi
     free_lun,ins1
     if ins1 eq -1 then begin
        error = DIALOG_MESSAGE("Not a PyRAT file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
        return
     endif

; check for obsolete data types!
     if type ge 500L && type le 503L && head[0] eq 4 && head[1] eq 2 && multi le 1 then begin
;;; obsolete POLINSAR data type for the scattering vectors
        if file_test(inputfile,/READ,/WRITE) then begin
;;; update data type without comments
           move_transpose_ratfile, inputfile, transp_dims=[1,0,2,3], new_type=type
           head[1]=head[2]
           head[2]=2
        endif else begin
           error = dialog_message(["Unfortunatly the given data type is obsolete.", $
                                   "RAT has not the privileges to update this data type.", $
                                   "Please provide read and write rights to update the given dataset!"])
           return
        endelse
     endif
;; for obsolete multibaseline data types (can be deleted after 12/2007, mn)
     if type ge 800L && type le 840L then $
        type -= 300L



; analyse header
     file.name = inputfile
     file.info = info
     file.mult = multi
     file.type = type
     file.dim  = head[0]
;		wid.draw_bytes = 0                 ; SAR-type bytscaling
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
        file.zdim = head[2]
        file.vdim = head[1]
        file.var  = head[5]
     endif

     generate_preview,/window_title
     update_info_box

  endif
end
