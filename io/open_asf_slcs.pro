;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
 ; RAT Module: open_asf_slcs
 ; written by : Franz Meyer / ASF Alaska
 ; last revision : 10th . April 2007
 ; Open SLC files in ASF SAR CEOS Format
 ; This program is able to handle ASFs RADARSAT-1, ERS-1/2, and JERS data
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

 pro open_asf_slcs,INPUTFILE = inputfile, PATH = path
 common rat, types, file, wid, config
 common channel, channel_names, channel_selec, color_flag, palettes, pnames



 ;------------------------------------------------------------
 ;establish error handler
 ; catch, error_number
 ; if error_number ne 0 then begin
 ; error = DIALOG_MESSAGE(["Could not read image !",!err.msg], DIALOG_PARENT = wid.base, TITLE='Unrecognized Format',/error)
 ; catch ,/cancel
 ; return
 ; end

 ;don't print math exceptions
 ;!EXCEPT=0

 ;------------------------------------------------------------

	if not (keyword_set(inputfile) or keyword_set(path)) then begin
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open ASF SAR SLC file', $
		DIALOG_PARENT=wid.base, FILTER = '*.D', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif

	if strlen(inputfile) gt 0 then begin

 ; change mousepointer

	WIDGET_CONTROL,/hourglass

	filename_base = StrMid( inputfile, StrLen(Path) )

;-----------------------------------------------------------------------------------------------------
; HEADER analysis
;-----------------------------------------------------------------------------------------------------

	header = bytarr(30000)
	openr,dummy,path+filename_base,/get_lun,/swap_if_big_endian
	readu,dummy,header
	if header[60] eq 78 then begin
		mess = dialog_message('Warning: data not calibrated',/error,dialog_parent=wid.base)
		free_lun,dummy
		return
	endif
	if header[60] eq 88 then level = 1.1 else level = 1.5
	lines  = long(string(header[180:185])) 	  ; nr of data lines
	bps    = long(string(header[224:227]))      ; bytes per sample
	prefix = long(string(header[276:279]))      ; prefix at beginning of line
	pixels = (long(string(header[186:191]))-prefix) /  bps  ; nr of pixels per line
	offset = header[11]+2l^8*header[10]+2l^16*header[9]+2l^24*header[8] + 1

	if header[48] EQ 82  THEN spacecraft = 'RADARSAT-1'
	if header[48] EQ 74  THEN spacecraft = 'JERS'
	if header[48] EQ 69 AND header[49] EQ 49  THEN spacecraft = 'ERS-1'
	if header[48] EQ 69 AND header[49] EQ 50  THEN spacecraft = 'ERS-2'
	if header[48] NE 82 AND header[48] NE 74 AND header[48] NE 69 THEN BEGIN
		mess = dialog_message('Unsupported Datatype',/error,dialog_parent=wid.base)
		return
	endif

	file.xdim = pixels
	file.ydim = lines
	free_lun,dummy

	file.dim  = 2l
	file.zdim = 1l
	file.vdim = 1l
	if bps eq 4 then file.type = 101l else file.type = 100l
	if bps eq 4 then file.var = 6l else file.var = 4l
	file.info = filename_base;Stringset[1]+'-'+Stringset[2]
	nr_channels = 1
;-----------------------------------------------------------------------------------------------------


;-----------------------------------------------------------------------------------------------------
; open the image(s)
;-----------------------------------------------------------------------------------------------------

	openr,ddd,path+filename_base,/get_lun,/swap_if_little_endian
	srat,config.tempdir+config.workfile1,lun_out,header=[2l,pixels,lines,file.var],type=file.type


	even_index = indgen(pixels)*2
	odd_index = indgen(pixels)*2+1
	test_img = fltarr(file.xdim,2000)

	if bps eq 4 then begin
		linebuffer      = intarr(pixels*2);complexarr(pixels)
		linebuffer_test = intarr(pixels*2)
		linebuffer2     = complexarr(pixels)
		linebuffer2_test= complexarr(pixels)
		polvectorbuffer = complexarr(file.zdim,pixels)
	endif else begin
		linebuffer      = bytarr(pixels)
		linebuffer_test = bytarr(pixels)
		linebuffer2_test= bytarr(pixels)
		linebuffer2     = fltarr(pixels)
		polvectorbuffer = fltarr(file.zdim,pixels)
	endelse

;-----------------------------------------------------------------------------------------------------


;-----------------------------------------------------------------------------------------------------
; Doing some simple data checks before the program starts freezing the screen
;-----------------------------------------------------------------------------------------------------

; Read a chunk of data
progress,Message='Preparing quality check...'
for i=0l,2000-1 do begin
	for k=0,nr_channels-1 do begin
		progress,percent = i*100.0/2000
		point_lun,ddd[k],offset+i*(prefix+bps*pixels)+prefix-1+(floor(lines/2)-1000)*(prefix+bps*pixels)
		readu,ddd[k],linebuffer_test
		if bps eq 4 then linebuffer2_test = complex(linebuffer_test(even_index),linebuffer_test(odd_index)) $
		else linebuffer2_test = linebuffer_test
		test_img[*,i] = linebuffer2_test
	endfor
endfor
progress,/destroy
a = WHERE(test_img[10000:400000] NE 0)

; ---- Generate GUI
wid_title = 'Quick and very simple quality check of file: ' + filename_base
main2 = WIDGET_BASE(row=2,TITLE=wid_title,/tlb_kill_request_events,/tlb_frame_attr)
sub2  = WIDGET_BASE(main2,column=2,/frame)
sub2_1 = WIDGET_BASE(sub2,row=1,/frame)
sub2_2 = WIDGET_BASE(sub2,row=1,/frame)

dr2_1  = widget_draw(sub2_1,XSIZE=300,ysize=300)
dr2_2  = widget_draw(sub2_2,XSIZE=300,ysize=300)

buttons2  = WIDGET_BASE(main2,column=2,/frame)
but_proceed2   = WIDGET_BUTTON(buttons2,VALUE=' Proceed ',xsize=80,/frame)
but_exit2 = WIDGET_BUTTON(buttons2,VALUE=' Exit ',xsize=80)
WIDGET_CONTROL, main2, /REALIZE, default_button = but_proceed2


widget_control,dr2_1,get_value=index
wset,index
tvscl,congrid(smooth((abs(test_img[1000:3000,*])),8),300,300)
loadct,0,/silent

widget_control,dr2_2,get_value=index
wset,index
if bps eq 4 then hist_result = histogram((abs(test_img[a])),nbins=100, locations=locations) $
else hist_result = histogram(smooth(abs(test_img[a]),3),nbins=100, locations=locations)
hist_result = histogram((abs(test_img[a])),nbins=100, locations=locations)
hist_result2 = histogram(smooth(abs(test_img[a]),8),nbins=100, locations=locations)
result_string_1 = 'min DN in tile:'+string(min(abs(test_img[a])))
result_string_2 = 'max DN in tile:'+string(max(abs(test_img[a])))
plot,locations/100,hist_result,title='Image Histogram',position=[0.01,0.2,0.99,0.85],xstyle=1,xtitle='DN/100',xcharsize=0.6
oplot,locations/100,hist_result2, color=160, thick=2.0, linestyle = 2
xyouts,50,230,result_string_1,/device,ALIGNMENT=0.0,charsize=0.5, charthick=1, color=[255,255,0]
xyouts,50,210,result_string_2,/device,ALIGNMENT=0.0,charsize=0.5, charthick=1, color=[255,255,0]
xyouts,210,90,'Single Look',/device,ALIGNMENT=0.0,charsize=0.5, charthick=2
xyouts,210,70,'Multi Look',/device,ALIGNMENT=0.0,charsize=0.5, charthick=2, color=150

repeat begin
event = widget_event(main2)
if event.id eq but_exit2 then begin               ; Exit Button clicked
	widget_control,main2,/destroy
	return
end
endrep until (event.id eq but_proceed2) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
widget_control,main2,/destroy
;-----------------------------------------------------------------------------------------------------


;-----------------------------------------------------------------------------------------------------
; Decoding the Image
;-----------------------------------------------------------------------------------------------------

	progress,Message='Decoding ' + spacecraft + '...'
	for i=0l,lines-1 do begin
	;print,i
		progress,percent = i*100.0/lines
		for k=0,nr_channels-1 do begin
			point_lun,ddd[k],offset+i*(prefix+bps*pixels)+prefix-1
			readu,ddd[k],linebuffer
			if bps eq 4 then linebuffer2 = complex(linebuffer(even_index),linebuffer(odd_index)) $
		 	else linebuffer2 = float(linebuffer)
			polvectorbuffer[k,*] = linebuffer2 / 10000.0
		endfor
		writeu,lun_out,polvectorbuffer
	endfor


 ; close open input handles

	for i=0,nr_channels-1 do free_lun,ddd[i]
	free_lun,lun_out

	file.name = config.tempdir+config.workfile1

 ; read palette information

	palettes[0,*,0] = bindgen(256) ; set actual palette to b/w linear
	palettes[0,*,1] = bindgen(256)
	palettes[0,*,2] = bindgen(256)
	palettes[1,*,*] = palettes[0,*,*] ; set variable palette to b/w linear

 ; generate preview

	file.window_name = 'Untitled.rat'
	update_info_box
	generate_preview,/window_title


endif
close,/all

end
