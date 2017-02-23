;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; startup definition & initialisation
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
pro definitions, update_pref=update_pref
	common rat, types, file, wid, config, tiling
	common channel, channel_names, channel_selec, color_flag, palettes, pnames
	common rit, parstruct, evolution

	prefversion = 0.21 ; version .18 beta1
;;; please take note, that prefversion is closely related to version
;;; control at the beginning of the rat procedure. (adjust both
;;; accordingly: prefversion should never be smaller than the checked
;;; version)

	types=strarr(901)
	types[0]   = "Unkown type"
	types[1]   = "bytes"
	types[2]   = "integer"
	types[3]   = "long integer"
	types[4]   = "floating point"
	types[5]   = "double"
	types[6]   = "complex"
	types[9]   = "double complex"
	types[12]  = "unsigned integer"
	types[13]  = "unsigned long"
	types[14]  = "integer64"
	types[15]  = "unsigned integer64"

; 50-99 generic formats

	types[50]  = "generic amplitude"
	types[51]  = "generic amplitude (mean scaled)"
	types[52]  = "generic phase"
	types[53]  = "generic complex amplitude"
	types[54]  = "generic complex amplitude (mean scaled)"
	types[55]  = "generic complex phase"
	types[56]  = "generic correlation"         ;; values in range [0:1]
	types[57]  = "generic complex correlation" ;; values in range [0:1]
	types[58]  = "generic amplitude (histogram scaled)"

; 100-199 single channel SAR

	types[100] = "SAR amplitude image"
	types[101] = "SAR complex image"
	types[102] = "SAR phase image"
	types[103] = "SAR intensity image"
	types[110] = "SAR image after edge detection"
	types[120] = "Co-occurance texture features"
	types[121] = "Variation coefficient"
	types[122] = "Band ratio"
	types[123] = "Band difference"
	types[124] = "Propability of change"
	types[125] = "Band entropy"

; 200-299 polarimetric SAR

	types[200] = "scattering vector, lexicographic basis"
;	types[201] = "scattering vector, linear basis at 45 deg "
;	types[202] = "scattering vector, circular basis"
;	types[208] = "scattering vector"
	types[209] = "scattering vector, lexicographic arbitrary basis"
	types[210] = "Pauli decomposition"
	types[211] = "Freeman-Durden decomposition"
	types[212] = "Unknown decomposition"
	types[213] = "Sphere-Diplane-Helix decomposition"
	types[214] = "Eigenvector decomposition"
	types[216] = "Moriyama decomposition"
	types[220] = "covariance matrix [C]"
	types[221] = "coherency matrix [T]"
	types[222] = "covariance matrix [C], arbitrary basis"

	types[230] = "polarimetric entropy"
	types[231] = "polarimetric alpha angle"
	types[232] = "polarimetric anisotropy"
	types[233] = "Entropy / Alpha / Anisotropy"
	types[234] = "Alpha / Beta / Gamma / Delta angles"
   types[235] = "ERD parameters"
   types[236] = "Delta Mag / Delta Pha / Tau" ; particle scattering anisotropy (mag + pha), orientation randomness

	types[250] = "polarimetric span image"
	types[280] = "ENVISAT partial polarimetry scattering vector"

; 300-399 interferometric SAR

	types[300] = "interferometric image pair"
	types[301] = "complex interferogram"
	types[302] = "interferometric phase"
	types[303] = "unwrapped phase"
	types[310] = "interferometric coherence"
	types[311] = "complex interferometric coherence"
	types[320] = "shaded relief"
	types[390] = "Flat-earth phase" 		  ;; 1d or 2d
	types[391] = "Flat-earth phase (multiple tracks)" ;; 1d or 2d ;; especially for polin
	types[392] = "Wavenumber" 			  ;; 1d or 2d e.g. kz or ky
	types[393] = "Wavenumber (multiple tracks)" 	  ;; 1d or 2d ;; especially for polin
	types[394] = "Baseline" 			  ;; 1d or 2d e.g. perpendicular baseline (in meters)
	types[395] = "Baseline (multiple tracks)" 	  ;; 1d or 2d e.g. perpendicular baseline (in meters)

; 400-499 Classification results

	types[400] = "Entropy / Alpha classification"
	types[401] = "Entropy / Alpha / Anisotropy classification"
	types[402] = "Wishart Entropy / Alpha classification"
	types[403] = "Wishart Entropy / Alpha / Anisotropy classification"
	types[404] = "Physical classification"
	types[405] = "Forest classification"
	types[406] = "Surface classification"
	types[407] = "Double bounce classification"
	types[408] = "Number of scattering mechanisms"
	types[409] = "Lee category preserving classification"
	types[410] = "Cameron classification"
	types[411] = "Wishart EM classification"
        types[444] = "General classification"
	types[450] = "PolInSAR Wishart classification"
	types[451] = "PolInSAR A1/A2 coherence classification"
	types[499] = "Colour palette file"

; 500-599 Polarimetric Interferometric SAR
	types[500] = "PolInSAR scattering vector, lexicographic basis"
	types[501] = "PolInSAR scattering vector, Pauli basis"
	types[502] = "PolInSAR scattering vector, lexicographic arbitrary basis"
	types[503] = "PolInSAR scattering vector, Pauli arbitrary basis"
	types[510] = "PolInSAR covariance matrix"
	types[511] = "PolInSAR coherency matrix"
	types[512] = "PolInSAR covariance matrix, arbitrary basis"
	types[513] = "PolInSAR coherency matrix, arbitrary basis"
        types[514] = "PolInSAR normalized cov/coh matrix" ;; i.e. with identity block matrices along the diagonal
  	types[530] = "PolInSAR coherence"           ;; should be used for real and complex coherences!
;;; 	types[531] = "PolInSAR complex coherence"   ;; should not be used anymore!!! use always 530! obsolete
        types[532] = "POLInSAR optimized coherence" ;; special type for special rgb-channel rearange!
        types[535] = "POLInSAR scattering mechanims vectors" ;; SM's
	types[540] = "PolInSAR LFF coherence parameters (A1,A2,Hint,Aint)"

; 600-699 SAR Sub-Apertures
	types[600] = "Subaperture decomposition"
	types[601] = "Multi-channel subapertures"
	types[610] = "Covariance matrices for every subaperture"
	types[615] = "Subapertures covariance matrix"
	types[630] = "Subapertures stationarity [log(L)]"

; 700-799 Multitemporal & others
	types[700] = "Multitemporal data"

; 800-899 Polarimetric Multibaseline SAR
; 	types[800] = "MB scattering vector, lexicographic basis"
; 	types[801] = "MB scattering vector, Pauli basis"
; 	types[802] = "MB scattering vector, lexicographic arbitrary basis"
; 	types[803] = "MB scattering vector, Pauli arbitrary basis"
; 	types[810] = "MB covariance matrix"
; 	types[811] = "MB coherency matrix"
; 	types[812] = "MB covariance matrix, arbitrary basis"
; 	types[813] = "MB coherency matrix, arbitrary basis"
;       types[814] = "MB normalized cov/coh matrix" ;; i.e. with identity block matrices along the diagonal
;   	types[830] = "MB coherence"
;   	types[832] = "MB optimized coherence" ;; special type for special rgb-channels rearange!


;----------------------------------------------------
	channel_names = strarr(1)
	channel_selec = [0,1,2]
	color_flag    = 1
	palettes      = bytarr(256,256,3)
	pnames        = strarr(256)
;----------------------------------------------------

	file={$
		name : " " ,$
		window_name : " ",$
		info : " " ,$
		type : 0l  ,$
		var  : 0l  ,$
		dim  : 0l  ,$
		xdim : 0l  ,$
		ydim : 0l  ,$
		zdim : 0l  ,$
		vdim : 0l  ,$
		mult : 1l  $
	}


	wid={$
		base  : 0l ,$	      	;Main window widget ID
		draw  : 0l , $		;Draw window widget ID
		cancel : 0l , $		;cancel a routine
		block : 0b, $		;block rat widget or not
		plugins : ptr_new(), $	;plugin names
		info  : 0l , $        	;Info window widget ID
                button_undo: 0l , $   	;Undo button widget ID
                button_redo: 0l , $   	;Redo button widget ID
		button_show_preview: 1L, $ ; Preview Show
		prog1 : 0l , $        	;progress window widget ID
		prog2 : 0l , $        	;progress window widget ID
		prog3 : 0l , $        	;progress window text widget ID
		base_xsize : 700 ,$	;Size main window in x
		base_ysize : 700 ,$	;Size main window in y
		draw_ysize : 1000 ,$    ;Size scroll window in y
		draw_scale : 0.0 $    	;Preview image scaling factor
            }

	config = {$
		tempdir  : "", $
		tempbase : "", $
		workdir  : "", $
		pref     : "", $
		imagedir : "", $
		prefdir  : "", $
		palettes : "", $
		pnames   : "", $
		docdir   : "", $
		pdfviewer: "", $
		progress : ptr_new(), $ ; create a new pointer, will be used to store the progress bar status
		workfile1: "workfile1.rat", $
		workfile2: "workfile2.rat", $
		workfile3: "workfile3.rat", $
		lookfile : "lookfile.rat", $
		undofile : "",$
                redofile : "", $
                blocksize: 128l, $
		sar_scale: 2.5, $
		log_scale: 0, $
		pha_gamma: 1.5, $
		test     : 0.0, $
		os       : strlowcase(!version.os_family), $
;		file_filters: ['*.rat;*.mrat','*.rat','*.mrat','*.rit'], $
		version  : prefversion, $
		show_preview : 1, $ ; should a preview be shown ?
                batch    : 0, $ ; starting to implement the batch mode
                debug    : 0 $
	}

	tiling = {$
		nr_blocks  : 0l, $
		blocksizes : ptr_new(), $
		overlap    : 0l $
	}

	if config.os eq 'unix' then begin
		homedir  = getenv("HOME")
		tempbase = getenv("TMP")
		if tempbase eq '' then tempbase = homedir
		config.prefdir  = homedir+'/.rat/'
		config.pdfviewer= 'xpdf'
		;; config.tempbase = tempbase+'/'
		;; config.tempdir  = tempbase+'/'
		;; config.workdir  = homedir+'/'
		;; config.pref     = homedir+'/.rat/preferences'
		;; config.imagedir = homedir+'/.rat/icons/'
		;; config.palettes = homedir+'/.rat/palettes.rat'
		;; config.pnames   = homedir+'/.rat/palettes.txt'
	endif

	if config.os eq 'windows' then begin
		homedir = getenv("USERPROFILE")
		tempbase = getenv("TMP")
		if tempbase eq '' then tempbase = homedir
		config.prefdir  = homedir+'\rat\'
		config.pdfviewer= 'acrord32.exe'
	endif

;;; test either configuration is installed in the source dir.
   if ~file_test(config.prefdir,/dir,/read) then begin
      sourcedir = file_dirname((routine_info('definitions',/source)).path,/mark)
      prefdir1 = sourcedir+'preferences'+path_sep()+'installed'+path_sep()
      if file_test(prefdir1,/dir,/read) then config.prefdir = prefdir1
   endif

   config.tempbase = tempbase+path_sep()
   config.tempdir  = tempbase+path_sep()
   config.workdir  = homedir+path_sep()
   config.pref     = config.prefdir+'preferences'
   config.imagedir = config.prefdir+'icons'+path_sep()
   config.palettes = config.prefdir+'palettes.rat'
   config.pnames   = config.prefdir+'palettes.txt'

	if keyword_set(update_pref) then save,filename=config.pref,config,wid

	if FILE_TEST(config.pref) then begin
		wid_struct=wid
                config_struct=config
		restore,config.pref
		struct_assign,wid,wid_struct,/NOZERO ; for backwards compatibility (mn, 09/06)
		wid=wid_struct		; else the new fields in the structure would be deleted by restore!
                struct_assign,config,config_struct,/NOZERO ; for backwards compatibility (mn, 2/7)
                config=config_struct
                wid_struct = -1 & config_struct = -1
	endif else config.version = 0.0
;        config.docdir = file_dirname((routine_info('rat',/source)).path,/mark)+'doc'+path_sep()
        config.docdir = file_dirname((routine_info('definitions',/source)).path,/mark)+'doc'+path_sep()

	if keyword_set(update_pref) then config.version = prefversion

	if FILE_TEST(config.palettes) then rrat,config.palettes,palettes

	config.tempdir  = config.tempbase+'TMPRAT_'+strcompress(floor(1e9*randomu(s,1)),/remove)+path_sep()
	file_mkdir,config.tempdir,/noexpand_path

	if FILE_TEST(config.pnames) then begin
		str = ""
		pnames=""
		openr,ddd,config.pnames,/get_lun
		while ~ eof(ddd) do begin
			readf,ddd,str
			pnames = [pnames,str]
		endwhile
		pnames = pnames[1:*]
		free_lun,ddd
	endif

;;; load plugins
        plugin_dirs  = [config.prefdir,file_dirname((routine_info('definitions',/source)).path,/mark)]+'plugins'+path_sep()
        heap_free,wid.plugins
        wid.plugins=ptr_new()
        if ~strmatch(!path,"*"+strjoin(plugin_dirs+':')+"*") then !path=strjoin(plugin_dirs+':')+!path
;        if ~strmatch(!path,"*"+strjoin(':'+plugin_dirs)+"*") then $
;           !path=strjoin(':'+plugin_dirs)+!path
        plugins = [{pro_name:'',menu_name:'',menu_pos:''}]
        plugin_files = file_search(plugin_dirs,'rat_plugin_*.{sav,pro}',/FOLD_CASE,/TEST_READ)
        if (size(plugin_files))[0] ne 0 then $
           for i=0,n_elements(plugin_files)-1 do begin
           plugin_name = file_basename(strmid(plugin_files[i],0,strlen(plugin_files[i])-4))
           if strcmp(strmid(plugin_files[i],strlen(plugin_files[i])-3,3),'sav',/fold) then $
              restore,plugin_files[i] $
           else resolve_routine,plugin_name,/comp
;;; if an error occures at this place, then the plugin procedure has
;;; either the wrong name, or the file is in a further subdirectory.
;;; ==> adjust the plugin, or remove the file from the plugin directory!
           if total(strcmp(routine_info(),plugin_name,/Fold),/int) gt 0 then $
              call_procedure,plugin_name,PLUGIN_INFO=plugin_info
           for j=0,n_elements(plugin_info.menu_pos)-1 do $
              plugins = [plugins,{pro_name:plugin_name,menu_name:plugin_info.menu_name,menu_pos:strlowcase(plugin_info.menu_pos[j])}]
        endfor
        if n_elements(plugins) gt 1 then begin
           wid.plugins=ptr_new(plugins[1:*])
;;; delete similar entries
        sorted=sort((*wid.plugins)[*].menu_name+(*wid.plugins)[*].menu_pos)
        curr=uniq((*wid.plugins)[sorted].menu_name+(*wid.plugins)[sorted].menu_pos)
        *wid.plugins=(*wid.plugins)[curr]
     endif

;         plugin_sav_files = file_search(plugin_dirs,'rat_plugin_*.sav',/FOLD_CASE,/TEST_READ)
;         plugin_pro_files = file_search(plugin_dirs,'rat_plugin_*.pro',/FOLD_CASE,/TEST_READ)
;         if (size(plugin_sav_files))[0] ne 0 then $
;            for i=0,n_elements(plugin_sav_files)-1 do begin
;            restore,plugin_sav_files[i]
;            plugin_name = file_basename(plugin_sav_files[i],'.sav',/Fold)
;            if total(strcmp(routine_info(),plugin_name,/Fold),/int) gt 0 then $
;               plugins = [plugins,plugin_name]
;         endfor
;         if (size(plugin_pro_files))[0] ne 0 then $
;            for i=0,n_elements(plugin_pro_files)-1 do begin
;            plugin_name = file_basename(plugin_pro_files[i],'.pro',/Fold)
;            resolve_routine,plugin_name,/comp
;            if total(strcmp(routine_info(),plugin_name,/Fold),/int) gt 0 then begin
;               call_procedure,plugin_name,PLUGIN_INFO=plugin_info
;               plugins = [plugins,{pro_name:plugin_name,menu_name:plugin_info.menu_name,menu_pos:plugin_info.menu_pos}]
;            endif
;         endfor


	wid.base  = 0l
	wid.draw  = 0l
	wid.info  = 0l
	wid.prog1 = 0l
	wid.prog2 = 0l
	wid.prog3 = 0l

        parstruct = { $
                    polarizations:	ptr_new(), $ ; number of polarizations for multibaseline datasets
                    nr_tracks:		ptr_new(), $ ; number of tracks for multibaseline datasets
                    polbasis_ellipticity: ptr_new(), $ ; e.g. for POLInSAR
                    polbasis_orientation: ptr_new(), $ ; e.g. for POLInSAR
                    subap_x:		ptr_new(), $
                    subap_y:		ptr_new(), $
                    res_az:		ptr_new(), $ ; resolution of one pixel in azimuth (m)
                    res_gr:		ptr_new(), $ ; resolution of one pixel in ground range (m)
                    res_sr:		ptr_new(), $ ; resolution of one pixel in slant  range (m)
                    wavelength:		ptr_new(), $
                    range_delay:	ptr_new(), $
                    slant_range:	ptr_new(), $
                    plane_height:	ptr_new(), $
                    plane_velocity:	ptr_new(), $
                    flat_earth:		ptr_new(), $
                    fe_file:		ptr_new(), $ ; connection e.g. for polin algorithms
                    kz_file:		ptr_new(), $ ; connection e.g. for polin algorithms
                    bl_file:		ptr_new(), $ ; Baseline lengths
                    inc_file:		ptr_new(), $ ; Incidence angles ; connection e.g. for polin algorithms
                    rsl_file:		ptr_new(),  $ ; Slant ranges file
                    transform:		ptr_new()	$ ;X-Y to Geographic transform
                    }

        evolution = ['']

end
