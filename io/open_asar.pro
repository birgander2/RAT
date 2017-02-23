;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;************************************************************
;* open_asar : programm converts ENVISAT-ASAR to RAT format *
;* Autor : OLIVER BACH, email: oli.mc@gmx.de                *
;* many,many thanx for helping me to Dr. Andreas Reigber    *
;* summer 2004                                              *
;************************************************************
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

pro open_envisat,INPUTFILE = inputfile
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	;*********************
	;Datei,Product-Analyse
	;*********************
	if not keyword_set(inputfile) then begin
		path = config.workdir
		inputfile = cw_rat_dialog_pickfile(TITLE='Open ENVISAT-ASAR file', DIALOG_PARENT=wid.base, FILTER = 'ASA*.N1', /MUST_EXIST, PATH=path, GET_PATH=path)
		if strlen(inputfile) gt 0 then config.workdir = path
	endif
	if strlen(inputfile) eq 0 then return
	
	filename = strmid(inputfile, strlen(path))
	prod=strsplit(filename,'_',/extract)
	
	case prod[1] of
		'CON' : begin
			mess = dialog_message(["This is not an ASAR picture file,","but a provessor-configuration file..."],title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'INS' : begin
			mess = dialog_message(["This is not an ASAR picture file","but an instrument-characterization file..."],title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'XCA' : begin
			mess = dialog_message(["This is not an ASAR picture file,","but an external calibration file..."],title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'XCH' : begin
			mess = dialog_message(["This is not an ASAR picture file,","but an external charakterisation file..."],title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'WVW' : begin
			mess = dialog_message("Wave Mode Spectra Level 2 Products are not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'APG' : begin
			mess = dialog_message("Alternating Polarization Ellipsoid Geocoded Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'APM' : begin
			mess = dialog_message("Alternating Polarization Medium Resolution Image Product detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'APP' : begin
			mess = dialog_message("Alternating Polarization Mode Precision Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'APS' : begin
			mess = dialog_message("Alternating Polarization Mode Single Look Complex detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'GM1' : begin
			mess = dialog_message("Global Monitoring Mode Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'IMG' : begin
			mess = dialog_message("Image Mode Ellipsoid Geocoded Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'IMM' : begin
			mess = dialog_message("Image Mode Medium Resolution Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'IMP' : begin
			mess = dialog_message("Image Mode Precision Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'IMS' : begin
			mess = dialog_message("Image Mode Single Look Complex detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'WSM' : begin
			mess = dialog_message("Wide Swath Medium Resolution Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'WVI' : begin
			mess = dialog_message("Wave Mode SLC Imagette and Imagette Cross Spectra are not yet supported",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'WVS' : begin
			mess = dialog_message("Wave Mode Imagette Cross Spectra are not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'APC' : begin
			mess = dialog_message("Alternating Polarization Level 0 (Copolar) is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'APH' : begin
			mess = dialog_message("Alternating Polarization Level 0 (Cross polar H) is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'APV' : begin
			mess = dialog_message("Alternating Polarization Level 0 (Cross polar V) is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'EC' : begin
			mess = dialog_message("Level 0 External Charcterization is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'MS' : begin
			mess = dialog_message("Level 0 Module Stepping Mode is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'WV' : begin
			mess = dialog_message("Wave Mode Level 0 is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
		'AP' : begin
			mess = dialog_message("Alternating Polarization Browse Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'GM' : begin
			vgl=strcmp(prod[2],'0P',2)
			if vgl eq 1 then begin
			mess = dialog_message("Global Monitoring Mode Level 0 is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			endif
			mess = dialog_message("Global Monitoring Mode Browse Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'IM' : begin
			vgl=strcmp(prod[2],'0P',2)
			if vgl eq 1 then begin
			mess = dialog_message("Image Mode Level 0 is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			endif
			mess = dialog_message("Image Mode Browse Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		'WS' : begin		
			vgl=strcmp(prod[2],'0P',2)
			if vgl eq 1 then begin
			mess = dialog_message("Wide Swath Mode Level 0 is not yet supported...",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			endif
			mess = dialog_message("Wide Swath Browse Image detected",title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			end
		else : begin
			mess = dialog_message(["Format not recognized...","Is this really an ENVISAT-ASAR file?"],title = 'RAT - ASAR',/information,dialog_parent=wid.base)
			return
			end
	endcase
	
; change mousepointer
	
	widget_control,/hourglass

; undo function
                undo_prepare,outputfile,finalfile,CALLED=CALLED
                open_rit,/EMPTY ; no parameters are set: delete the old ones!
	
	openr,lese,inputfile,/get_lun
	
	;******************************
	;lese MPH, SPH und DSDs (ascii)
	;******************************
	i=0l
	j=0l
	k=0l
	l=0l
	n=0l
	dsd=0l
	dattyp=''
	polm1=''
	app_flag = 0
	in = ''
	while not eof(lese) do begin
		readf,lese,in
	;Anzahl der Spalten
		vgl = strcmp(in,'LINE_LENGTH',11)
		if vgl eq 1 then begin
			pos = strsplit(in,'+',/extract)
			spal = long(pos[1])
		endif
	;MDS Daten-Typ
		vgl = strcmp(in,'DATA_TYP',8)
		if vgl eq 1 then begin
			pos = strsplit(in,'=',/extract)
			dattyp = pos[1]
		endif
	;Sample-Typ: Daten komplex oder nicht ?
		vgl = strcmp(in,'SAMPLE_TYPE',11)
		if vgl eq 1 then begin
			pos = strsplit(in,'"',/extract)          ;'
			samtyp = pos[1]
		endif
	;MDS1 Polarisation
		vgl = strcmp(in,'MDS1_TX_RX_POLAR',16)
		if vgl eq 1 then begin
			pos = strsplit(in,'=',/extract)
			polm1 = strsplit(pos[1],'"',/extract)         ;'
		endif
	;MDS2 Polarisation, falls MDS2 vorhanden
		vgl = strcmp(in,'MDS2_TX_RX_POLAR',16)
		if vgl eq 1 then begin
			pos = strsplit(in,'=',/extract)
			if strcmp(pos[1],'"   "') eq 0 then begin
				polm1 += " + " + strsplit(pos[1],'"',/extract)       ;'
				app_flag = 1
			endif
		endif
	;Anzahl der DSDs
		vgl = strcmp(in,'NUM_DSD',7)
		if vgl eq 1 then begin
			pos = strsplit(in,'+',/extract)
			dsd = long(pos[1])
			typ=string(make_array(dsd))
			offs=lonarr(dsd)
			lang=lonarr(dsd)
			anz=lonarr(dsd)
		endif
	;Beginn der DSRs
		vgl = strcmp(in,'DS_OFFSET',9)
		if vgl eq 1 then begin
			pos = strsplit(in,'+',/extract)
			offs[j] = long(pos[1])
			j=j+1
		endif
	;Groesse der DS
		vgl = strcmp(in,'DS_SIZE',7)
		if vgl eq 1 then begin
			pos = strsplit(in,'+',/extract)
			lang[k] = long(pos[1])
			k=k+1
		endif
	;Anzahl der DSR (Anzahl Zeilen)
		vgl = strcmp(in,'NUM_DSR',7)
		if vgl eq 1 then begin
			pos = strsplit(in,'+',/extract)
			anz[l] = long(pos[1])
			l=l+1
		endif
	;DS-Typ: z.B. Messung
		vgl = strcmp(in,'DS_TYPE',7)
		if vgl eq 1 then begin
			pos = strsplit(in,'=',/extract)
			typ[n]=pos[1]
			n=n+1
		endif
		abbr = strcmp(in,'DSR_SIZE',8)
		if abbr eq 1 then i=i+1
		if (abbr eq 1 and i eq dsd) then goto,ende1
	endwhile
	ende1:
	
	;*************
	;RAT-Parameter
	;*************
	dim   = 2l
	var   = 4l
	zdim  = 1l
	dummy = 0l
	
	j=0l
	for j=0,i-1 do begin
		if typ[j] eq 'M' then goto,ende2
	endfor
	ende2:

	l=0l
	case dattyp of
		'"UWORD"' : bin=make_array(spal,/uint)
		'"SWORD"' : bin=make_array(spal,/int)
		'"UBYTE"' : bin=make_array(spal,/byte)
		else : print,'SCHEISSE !!!'
	endcase
	tuep = 100l
	if samtyp eq 'COMPLEX ' then begin
		bin=make_array(spal*2,/int)
		tuep = 101l
		var  = 6l
		index1 = indgen(spal)*2
		index2 = indgen(spal)*2+1
	endif
	if app_flag eq 1 then begin
		dim  = 3l
		zdim = 2l
		if samtyp eq 'COMPLEX ' then tuep = 101l else tuep = 100l
		openr,lese2,inputfile,/get_lun
		point_lun,lese2,offs[j+1]+17
	endif

	;****************************************************************
	;lese DSR von MDS1 (binär) einiger Level1 und 
	;erstelle RAT-Datei *.N1-MDS1.rat oder nur *.rat, falls kein MDS2
	;****************************************************************

	openw,schr1,config.tempdir+config.workfile1,/get_lun,xdr=1
	writeu,schr1,dim
	if zdim gt 1 then writeu,schr1,zdim
	writeu,schr1,spal,anz[j]
	writeu,schr1,var
	writeu,schr1,tuep
	writeu,schr1,dummy,dummy,dummy
	info1=byte(filename + ', ' + polm1)
	writeu,schr1,info1
	k=0l

	point_lun,lese,offs[j]+17

	progress,Message='Decoding ENVISAT-ASAR...'
	for k=0l,anz[j]-1 do begin
		progress,percent=(k+1)*100.0/anz[j]
		
		readu,lese,bin
		point_lun,-lese,pos1
		point_lun, lese,pos1+17

		in1 = swap_endian(bin)		
		if var eq 6 then begin
			rp = in1[index1]
			ip = in1[index2]
			out1 = complex(rp,ip)
		endif else out1 = float(in1)
		
		if app_flag eq 1 then begin
			readu,lese2,bin
			point_lun,-lese2,pos1
			point_lun, lese2,pos1+17
			in2 = swap_endian(bin)		
			if var eq 6 then begin
				rp = in2[index1]
				ip = in2[index2]
				out2 = complex(rp,ip)
			endif else out2 = float(in2)
			out = make_array(2,spal,type=var)
			out[0,*] = out1
			out[1,*] = out2
		endif else out = out1
		writeu,schr1,out	
	endfor
	progress,/destroy
	free_lun,schr1

	;******************************************************************
	;habe fertitsch !?! dann nix wie auf den Bildschirm mit dem Sch*@!!
	;******************************************************************
	ende3:
	free_lun,lese
	if app_flag eq 1 then free_lun,lese2
	 
	file.info = filename + ', ' + polm1
	file.xdim = spal
	file.ydim = anz[j]
	file.zdim = zdim
	file.vdim = 1l
	file.var  = var
	file.dim  = dim
	file.type = tuep
	file.name = config.tempdir+config.workfile1
	

; update file generation history (evolution)
	
	evolute,'Import SAR data from ASAR.'

; read palette information
	
	palettes[0,*,*] = palettes[2,*,*] ; set variable palette to b/w linear
	palettes[1,*,*] = palettes[2,*,*] ; set variable palette to b/w linear

	file.window_name = 'Untitled.rat'
	generate_preview  ;,/window_title
	update_info_box

end
