;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: save_generic
; written by    : Andreas Reigber and Stephane Guillaso
; last revision : 23.September 2003
; Save current data set as generic binary or ENVI format
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




pro save_generic, OUTPUTFILE = outputfile, ENVI = envi
	common rat, types, file, wid, config

	if not keyword_set(outputfile) then begin
		path = config.workdir
		if keyword_set(envi) then $
			outputfile = cw_rat_dialog_pickfile(TITLE='Save ENVI file', DIALOG_PARENT=wid.base, FILTER = '*.dat', PATH=path, GET_PATH=path,/overwrite_prompt,/write) $
		else $
			outputfile = cw_rat_dialog_pickfile(TITLE='Save generic binary', DIALOG_PARENT=wid.base, FILTER = '*.dat', PATH=path, GET_PATH=path,/overwrite_prompt,/write) 

		if strlen(outputfile) gt 0 then config.workdir = path
	endif

	if strlen(outputfile) gt 0 then begin

; change mousepointer
	
		WIDGET_CONTROL,/hourglass

; copy file to destination
		
		inputfile  = file.name
		
; read / write header

		head = 1l
		rrat,inputfile,ddd,header=head,info=info,type=type		
		openw,eee,outputfile+'.part',/get_lun,/xdr	

; calculating preview size and number of blocks

		bs = config.blocksize
		calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
		blocksizes = intarr(anz_blocks)+bs
		blocksizes[anz_blocks-1] = bs_last
	
; pop up progress window

	progress,Message='Exporting file...'

;do the transform

		for i=0,anz_blocks-1 do begin   ; normal blocks
			progress,percent=(i+1)*100.0/anz_blocks
			block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
			readu,ddd,block
			writeu,eee,block
		endfor
		free_lun,ddd,eee
		
; Write ENVI header file

		if keyword_set(envi) then begin
		

			openw,ddd,outputfile+'.hdr',/get_lun
			printf,ddd,'ENVI'
			printf,ddd,'description = {'
			printf,ddd,file.info+'.}'
			printf,ddd,'samples = '+strcompress(file.xdim,/remove)
			printf,ddd,'lines   = '+strcompress(file.ydim,/remove)
			printf,ddd,'bands   = '+strcompress(file.zdim * file.vdim,/remove)
			printf,ddd,'header offset = 0'
			if file.type ge 400 and file.type le 499 then $
				printf,ddd,'file type = ENVI Classification' else $
				printf,ddd,'file type = ENVI Standard'
			printf,ddd,'data type = '+strcompress((file.var EQ 2) ? 3 : file.var,/remove)
			printf,ddd,'interleave = bip'
			printf,ddd,'sensor type = Unknown'
			
			;test following the different classifiers
			if file.type ge 400 and file.type lt 500 then begin
			case file.type of
				400:begin ;Entropy / Alpha classification 
					printf,ddd,'classes = 10'
					printf,ddd,'class lookup = {'
					printf,ddd,'   0,   0,   0,   0, 100,   0, 100, 125, 000, 255, 255, 255, 100, 150, 150,'
					printf,ddd,' 100, 200, 075, 200, 200, 075, 150, 150, 150, 125, 100, 050, 100, 150, 200}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Class #1, Class #2, Class #3, Class #4, Class #5, Class #6,'
 					printf,ddd,'Class #7, Class #8, Class #9}'
				end
				401:begin ;Entropy Alpha Anisotropy classification 
					printf,ddd,'classes = 19'
					printf,ddd,'class lookup = {'
					printf,ddd,'   0,   0,   0,   0, 100,   0,   0, 125,   0, 100, 125,   0, 125, 150,   0,'
					printf,ddd,' 255, 255, 255, 255, 255, 255, 100, 150, 150, 125, 175, 175, 100, 200, 075,'
					printf,ddd,' 125, 255, 100, 200, 200,  75, 255, 255, 100, 150, 150, 150, 175, 175, 175,'
					printf,ddd,' 125, 100,  50, 150, 125,  75, 100, 150, 200, 125, 175, 225}' 
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Class #1, Class #2, Class #3, Class #4, Class #5, Class #6,'
 					printf,ddd,'Class #7, Class #8, Class #9, Class #10, Class #11, Class #12, Class #13,'
 					printf,ddd,' Class #14, Class #15, Class #16, Class #17, Class #18}'
				end
				402:begin ;Wishart Entropy Alpha Classification
					printf,ddd,'classes = 8'
					printf,ddd,'class lookup = {'
 					printf,ddd,' 255, 255, 255,   0, 130,   0, 255,   0,   0,   0, 255,   0,'
 					printf,ddd,'   0, 255, 255, 255, 130,   0, 255,   0, 255,   0,   0, 255}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Class #1, Class #2, Class #3, Class #4, Class #5, Class #6,'
 					printf,ddd,'Class #7, Class #8}'
				end
				403:begin ;Wishart Entropy Alpha Anisotropy classification 
					printf,ddd,'classes = 16'
					printf,ddd,'class lookup = {'
					printf,ddd,'   0, 130,   0,   0, 255,   0, 255, 130,   0, 134, 255, 134,'
					printf,ddd,' 134, 255, 255, 255,   0,   0, 190, 195,   0, 190,   0, 190, 134,   0, 134,'
					printf,ddd,' 255, 255,   0, 255,   0, 255,   0,   0, 255, 134, 130, 190, 190,   0,   0,'
					printf,ddd,'   0, 195, 190,   0,   0, 190}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Class #1, Class #2, Class #3, Class #4, Class #5, Class #6,'
 					printf,ddd,'Class #7, Class #8, Class #9, Class #10, Class #11, Class #12, Class #13,'
 					printf,ddd,' Class #14, Class #15, Class #16}'
				end
				404:begin ;Terrain classification 
					printf,ddd,'classes = 6'
					printf,ddd,'class lookup = {'
					printf,ddd,' 255, 255, 255,   0, 190,   0, 190,   0,   0,   0, 130, 255, 255,   0,   0,'
					printf,ddd,'   0,   0, 255}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Forest, Double 2, Double 1, Surface 2, Surface 1}'
				end
				405:begin ;forest classification 
					printf,ddd,'classes = 2'
					printf,ddd,'class lookup = {'
					printf,ddd,' 255, 255, 255,   0, 190,   0}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Forest}'
				end
				406:begin ;Surface classification 
					printf,ddd,'classes = 2'
					printf,ddd,'class lookup = {'
					printf,ddd,' 255, 255, 255,   0,   0, 255}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Surface}'
				end
				407:begin ;Double classification 
					printf,ddd,'classes = 2'
					printf,ddd,'class lookup = {'
					printf,ddd,' 255, 255, 255, 255,   0,   0}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, Double}'
				end
				408:begin ;Number of sources 
					printf,ddd,'classes = 4'
					printf,ddd,'class lookup = {'
					printf,ddd,' 255, 255, 255,   0,   0,   255, 255,   0,   0,   0, 255,   0}'
					printf,ddd,'class names = {'
					printf,ddd,'Unclassified, 1 mechanism, 2 mechanisms, 3 mechanisms}'
				end
				else : begin
					printf,ddd,'classes = 19'
					printf,ddd,'class lookup = {'
					printf,ddd,'   0,   0,   0, 255,   0,   0,   0, 255,   0,   0,   0, 255, 255, 255,   0,'
					printf,ddd,'   0, 255, 255, 255,   0, 255, 176,  48,  96,  46, 139,  87, 160,  32, 240,'
					printf,ddd,' 255, 127,  80, 127, 255, 212, 218, 112, 214, 160,  82,  45, 127, 255,   0,'
					printf,ddd,' 216, 191, 216, 238,   0,   0, 205,   0,   0, 139,   0,   0}'
					printf,ddd,'class names = {'
					printf,ddd,' Unclassified, Class #1, Class #2, Class #3, Class #4, Class #5, Class #6, '
					printf,ddd,' Class #7, Class #8, Class #9, Class #10, Class #11, Class #12, Class #13, '
					printf,ddd,' Class #14, Class #15, Class #16, Class #17, Class #18}'
				end
			endcase
			endif
			printf,ddd,'byte order = 1'
			if (file.var eq 6) then printf,ddd,'complex function = Power'
			free_lun,ddd
		endif

		
		file_move,outputfile+'.part',outputfile,/overwrite
		
		progress,/destroy
	endif	

end
