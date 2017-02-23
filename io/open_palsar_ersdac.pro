pro open_palsar_ersdac,INPUTFILE = inputfile, PATH = path 
common rat, types, file, wid, config 
common channel, channel_names, channel_selec, color_flag, palettes, pnames 

if not (keyword_set(inputfile) or keyword_set(path)) then begin 
    path = config.workdir 
		inputfile = cw_rat_dialog_pickfile(TITLE='Open SLC(Vexcel) file', $ 
		DIALOG_PARENT=wid.base, FILTER = '*.SLC', /MUST_EXIST, PATH=path, GET_PATH=path) 
		if strlen(inputfile) gt 0 then config.workdir = path 
endif 

if inputfile EQ '' then begin
	  void = Dialog_Message('You must select input file')
	  return
endif

 ; change mousepointer 
 
WIDGET_CONTROL,/hourglass 
 
filename_base = StrMid( inputfile, StrLen(Path) )
StringSet = Strsplit( filename_base, '-',/extract)



;/* Find Header

header_file = inputfile+'.par'
res = File_Search(header_file, Count=cnt,/Test_Read) 

if cnt NE 1 then begin
    header_file = cw_rat_dialog_pickfile(TITLE='Please select SLC header file', $ 
		DIALOG_PARENT=wid.base, FILTER = '*.PAR', /MUST_EXIST, PATH=path)

	  if header_file EQ '' then begin
		    void = Dialog_Message('No Header file exists!!')
		    return
    endif
endif

;/* Read Header Files
tmp = ''
slc_flg = 0
openR, lun, header_file, /GET_LUN

while ~ Eof(lun) do begin
    ReadF, lun, tmp

    if tmp[0] EQ '' then continue ; if line is null, skip 
	  strLine = strsplit(tmp, /Extract);separated by space

	  if (strLine[0] EQ 'SLCProduct') then slc_flg = 1

	  if (slc_flg EQ 1) then begin
		    case strLine[0] of
			  'NrPixels:':samples = Long(strLine[1])
			  'NrLines:':lines = Long(strLine[1])
			  'Format:':format = strLine[1:2]
			  'polarization:':pol = strLine[1]
			  else:
		    endcase
    endif

endwhile

    if slc_flg NE 1 then begin
		    void = dialog_message('This file is not ERSDAC SLC products!!')
		    free_lun, lun
    endif

free_lun, lun

;open the image(s)

file.xdim = samples
file.ydim = lines
file.zdim = 1l
file.dim = 2l
file.vdim = 1l
file.type = 101l
file.var = 6
file.info = 'ALOS-1-SLC-'+pol
nr_channels = 1

openR, ddd, inputfile, /GET_LUN, /swap_if_little_endian

srat, config.tempdir+config.workfile1, lun_out, header=[2,file.xdim,file.ydim,file.var], type = file.type 

linebuffer = complexarr(samples)
polvectorbuffer = complexarr(samples)

progress,Message = 'Decoding ALOS-PALSAR...'
for i=0l, lines-1 do begin
    progress,percent=i*100.0/lines
  	;for k=0, nr_channels-1 do begin
		point_lun,ddd,(lines-1- i)*8*samples
		readu,ddd,linebuffer
		polvectorbuffer[*]=linebuffer
  	;endfor
    writeu, lun_out, polvectorbuffer
endfor

progress,/destroy
;close open
free_lun, ddd
free_lun, lun_out
file.name = config.tempdir+config.workfile1

;read palette information
palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
 
;generate preview
file.window_name = 'untitle.rat'
update_info_box
generate_preview;,/window_title
 
end








 

