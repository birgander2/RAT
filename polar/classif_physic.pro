;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: classif_physic
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
PRO classif_physic, CALLED = called
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames


	; Variables	
	lim_A  = 0.5 	;Limite change anisotropy
	lim_H1 = 0.88	;Limite change entropy 1
	lim_H2 = 0.5	;Limite change entropy 2
	eps    = 1.E-6
	
	alpha1 = 0
	alpha2 = 1
	beta1  = 2
	beta2  = 3
	p1     = 4
	p2     = 5
	
	nr_classes = 22


	;-----------------------------------------------
	; display the graphical interface
	;-----------------------------------------------
	IF NOT KEYWORD_SET(called) THEN BEGIN

		; Variable initialisation
		HAA_classif_filename  = ''
		eigen_decomp_filename = ''
		HAA_decomp_filename   = ''
		select_exclude_border = 0
		norg                  = 0
tryagain:
	
		; generate the main widget
		main = WIDGET_BASE(GROUP_LEADER=wid.base,ROW=2,TITLE='Physical classification',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		; Sub widget
		sub_main = WIDGET_BASE(main,ROW=2)
		
			; Sub file input
			sub_file_input = WIDGET_BASE(sub_main,ROW=3,/FRAME,XSIZE=wid_xsize)
			
				; Ask for the H/A/alpha Wishart Classification
				sub_input_HAA_classif = WIDGET_BASE(sub_file_input,COLUMN=2)
					text_HAA_classif = CW_FIELD(sub_input_HAA_classif,VALUE=HAA_classif_filename,/STRING,XSIZE=60,TITLE='H/A/alpha Wishart Classification: ')
					brow_HAA_classif = WIDGET_BUTTON(sub_input_HAA_classif,VALUE='browse',YSIZE=35)
	
				; Ask for the eigen decomposition
				sub_input_eigen_decomp = WIDGET_BASE(sub_file_input,COLUMN=2)
					text_eigen_decomp = CW_FIELD(sub_input_eigen_decomp,VALUE=eigen_decomp_filename,/STRING,XSIZE=60,TITLE='Eigen Decomposition:              ')
					brow_eigen_decomp = WIDGET_BUTTON(sub_input_eigen_decomp,VALUE='browse',YSIZE=35)
	
				; Ask for the H/A/alpha decompotision
				sub_input_HAA_decomp = WIDGET_BASE(sub_file_input,COLUMN=2)
					text_HAA_decomp = CW_FIELD(sub_input_HAA_decomp,VALUE=HAA_decomp_filename,/STRING,XSIZE=60,TITLE='H/A/alpha Decomposition:          ')
					brow_HAA_decomp = WIDGET_BUTTON(sub_input_HAA_decomp,VALUE='browse',YSIZE=35)
		
			; Sub diverse information
			sub_information = WIDGET_BASE(sub_main,ROW=1,/FRAME,XSIZE=wid_xsize)
			
				; Ask for exclude border
				sub_exclude_border = WIDGET_BASE(sub_information,COLUMN=2)
					button_exclude_border = CW_BGROUP(sub_exclude_border,      ['Exclude border: '],YPAD=5,/ROW,/NONEXCLUSIVE,SET_VALUE=[select_exclude_border])
					text_exclude_border = CW_FIELD(sub_exclude_border,VALUE=norg,/INTEGER,XSIZE=5,TITLE='')
				
		; Sub button widget
		buttons  = WIDGET_BASE(main,column=3,/frame)
			but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
			but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
			but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
			
		; Realise the User Interface
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]


		; Exclude border is not set by default
		WIDGET_CONTROL,text_exclude_border,SENSITIVE=select_exclude_border
		
		; Repeat loop event
		REPEAT BEGIN
		
			; Get event
			event = WIDGET_EVENT(main)
			
			; display the info message box
			IF event.id EQ but_info THEN BEGIN 
				infotext = ['Physical Classification',$
				' ',$
				'Version 1.0 - RAT module written 2004 by Stephane Guillaso',$
				'',$
				'further information:',$
				'L. Ferro-Famil et. al.: Classification and Interpretation of Polarimetric',$
				'Interferometric SAR Data, Proceedings of IGARSS 02, June 02, Toronto, Canada']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main,TITLE='Information')
			ENDIF
			
			; get the H/A/alpha Wishart Classification
			IF event.id EQ brow_HAA_classif THEN BEGIN
				path = config.workdir
				HAA_classif_filename = DIALOG_PICKFILE(TITLE='Open H/A/alpha Wishart Classification',DIALOG_PARENT=wid.base, FILTER = '*.rat',/MUST_EXIST,PATH=path,GET_PATH=path)
				IF STRLEN(HAA_classif_filename) GT 0 THEN config.workdir = path
 				WIDGET_CONTROL,text_HAA_classif,SET_VALUE=HAA_classif_filename
			ENDIF
			
			; get the eigen decomposition
			IF event.id EQ brow_eigen_decomp THEN BEGIN
				path = config.workdir
				eigen_decomp_filename = DIALOG_PICKFILE(TITLE='Open Eigen Decomposition',DIALOG_PARENT=wid.base, FILTER = '*.rat',/MUST_EXIST,PATH=path,GET_PATH=path)
				IF STRLEN(eigen_decomp_filename) GT 0 THEN config.workdir = path
 				WIDGET_CONTROL,text_eigen_decomp,SET_VALUE=eigen_decomp_filename
			ENDIF
			
			; get the H/A/alpha decomposition
			IF event.id EQ brow_HAA_decomp THEN BEGIN
				path = config.workdir
				HAA_decomp_filename = DIALOG_PICKFILE(TITLE='Open H/A/alpha Decomposition',DIALOG_PARENT=wid.base, FILTER = '*.rat',/MUST_EXIST,PATH=path,GET_PATH=path)
				IF STRLEN(HAA_decomp_filename) GT 0 THEN config.workdir = path
 				WIDGET_CONTROL,text_HAA_decomp,SET_VALUE=HAA_decomp_filename
			ENDIF
			
			; Exclude border information
			IF event.id EQ button_exclude_border THEN BEGIN
				select_exclude_border = event.select
				WIDGET_CONTROL,text_exclude_border,SENSITIVE=event.select
				IF event.select EQ 1 THEN WIDGET_CONTROL,text_exclude_border,SET_VALUE=3
				IF event.select EQ 0 THEN WIDGET_CONTROL,text_exclude_border,SET_VALUE=0
			ENDIF
			
		ENDREP UNTIL (event.id EQ but_ok) OR (event.id EQ but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		
		; Get all information value
		WIDGET_CONTROL,text_exclude_border,GET_VALUE=norg
		
		; Destroy the User Interface
  		WIDGET_CONTROL,main,/DESTROY
		
		; quit if not OK button
		IF event.id NE but_ok THEN	RETURN
				
		; Test if we have a H/A/alpha Wishart Classification file input
		IF HAA_classif_filename EQ '' THEN BEGIN
			error = DIALOG_MESSAGE("Please select a H/A/alpha Wishart Classification file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
			GOTO,tryagain
		ENDIF ELSE BEGIN
			; read the header file
			head_HAA_classif=1l
			rrat,HAA_classif_filename,ddd,header=head_HAA_classif,info=info_HAA_classif,type=type_HAA_classif
			free_lun,ddd
			IF type_HAA_classif NE 403 then begin
				error = DIALOG_MESSAGE("Wrong H/A/alpha Wishart Classification file", DIALOG_PARENT = wid.base,TITLE='Error',/error)
				HAA_classif_filename=''
				GOTO,tryagain
			ENDIF
		ENDELSE
		
		; Test if we have an eigendecomposition file input
		IF eigen_decomp_filename EQ '' THEN BEGIN
			error = DIALOG_MESSAGE("Please select a Eigen Decomposition file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
			GOTO,tryagain
		ENDIF ELSE BEGIN
			; read the header file
			head_eigen_decomp=1l
			rrat,eigen_decomp_filename,ddd,header=head_eigen_decomp,info=info_eigen_decomp,type=type_eigen_decomp
			free_lun,ddd
			IF type_eigen_decomp NE 214 then begin
				error = DIALOG_MESSAGE("Wrong Eigen Decomposition file", DIALOG_PARENT = wid.base,TITLE='Error',/error)
				eigen_decomp_filename=''
				GOTO,tryagain
			ENDIF
		ENDELSE
		
		; Test if we have an H/A/alpha decomposition file input
		IF HAA_decomp_filename EQ '' THEN BEGIN
			error = DIALOG_MESSAGE("Please select a H/A/alpha Decomposition file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
			GOTO,tryagain
		ENDIF ELSE BEGIN
			; read the header file
			head_HAA_decomp=1l
			rrat,HAA_decomp_filename,ddd,header=head_HAA_decomp,info=info_HAA_decomp,type=type_HAA_decomp
			free_lun,ddd
			IF type_HAA_decomp NE 233 then begin
				error = DIALOG_MESSAGE("Wrong H/A/alpha Decomposition file", DIALOG_PARENT = wid.base,TITLE='Error',/error)
				HAA_decomp_filename=''
				GOTO,tryagain
			ENDIF
		ENDELSE
		
		; Test if every size file correspond
		IF (head_HAA_classif[1] NE head_eigen_decomp[3]) OR (head_HAA_classif[2] NE head_eigen_decomp[4]) THEN BEGIN
			error = DIALOG_MESSAGE("Files sizes between H/A/alpha Wishart classification and Eigen Decomposition not corresponding", DIALOG_PARENT = wid.base,TITLE='Error',/error)
			HAA_classif_filename=''
			eigen_decomp_filename=''
			GOTO,tryagain
		ENDIF
		IF (head_HAA_classif[1] NE head_HAA_decomp[2]) OR (head_HAA_classif[2] NE head_HAA_decomp[3]) THEN BEGIN
			error = DIALOG_MESSAGE("Files sizes between H/A/alpha Wishart classification and H/A/alpha Decomposition not corresponding", DIALOG_PARENT = wid.base,TITLE='Error',/error)
			HAA_classif_filename=''
			eigen_decomp_filename=''
			HAA_decomp_filename=''
			GOTO,tryagain
		ENDIF
		
	ENDIF ; end if about KEYWORD_SET(CALLED)
	
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
	
	;-----------------------------------------------
	; Change the mouse pointer
	;-----------------------------------------------
	WIDGET_CONTROL,/hourglass
	
	;-----------------------------------------------
	; Define the information about the input
	;-----------------------------------------------
	xdim_in = head_HAA_classif[1]
	ydim_in = head_HAA_classif[2]
	var_in  = head_HAA_classif[3]
	
	;-----------------------------------------------
	; Define the information and type of the output
	;-----------------------------------------------
	type_terrain_class = 404
	
	;-----------------------------------------------
	; Calcul the block size
	;-----------------------------------------------
	bs = config.blocksize
	calc_blocks_normal,ydim_in,bs,anz_blocks,bs_last 
	blocksizes = INTARR(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last
	

	;-----------------------------------------------
	; Open files
	;-----------------------------------------------
	head_HAA_classif=1l
	head_eigen_decomp=1l
	head_HAA_decomp=1l
	rrat,HAA_classif_filename, ddd1,header=head_HAA_classif, info=info_HAA_classif, type=type_HAA_classif
	rrat,eigen_decomp_filename,ddd2,header=head_eigen_decomp,info=info_eigen_decomp,type=type_eigen_decomp
	rrat,HAA_decomp_filename,  ddd3,header=head_HAA_decomp,  info=info_HAA_decomp,  type=type_HAA_decomp
	
	;-----------------------------------------------
	; Some array creation
	;-----------------------------------------------
	cpt_classes_H_A     = MAKE_ARRAY([nr_classes,7],type=4l)
	cpt_classes_al1     = MAKE_ARRAY([nr_classes,3],type=4l)
	cpt_classes_al1_al2 = MAKE_ARRAY([nr_classes,3],type=4l)

	;-----------------------------------------------
	; Perform the terrain classification using blocks
	;-----------------------------------------------
	
	progress,Message='Terrain Classification',/cancel_button

	FOR kk=0,anz_blocks-1 DO BEGIN
	
		progress,percent=(kk+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		
		; Make the block array
		block_HAA_classif  = MAKE_ARRAY([xdim_in,blocksizes[kk]],type=head_HAA_classif[3])
		block_eigen_decomp = MAKE_ARRAY([head_eigen_decomp[1],head_eigen_decomp[2],xdim_in,blocksizes[kk]],type=head_eigen_decomp[5])
		block_HAA_decomp   = MAKE_ARRAY([head_HAA_decomp[1],xdim_in,blocksizes[kk]],type=head_HAA_decomp[4])
		class_H_A          = MAKE_ARRAY([xdim_in,blocksizes[kk]],type=head_HAA_classif[3])
		class_al1          = MAKE_ARRAY([xdim_in,blocksizes[kk]],type=head_HAA_classif[3])
		class_al1_al2      = MAKE_ARRAY([xdim_in,blocksizes[kk]],type=head_HAA_classif[3])
		tmp_block          = MAKE_ARRAY([6,xdim_in,blocksizes[kk]],type=4l)
		
		; Read the input block
		READU,ddd1,block_HAA_classif
		READU,ddd2,block_eigen_decomp
		READU,ddd3,block_HAA_decomp
		
		; Perform the Entropy/Anisotropy Classification
		; ---------------------------------------------
		ent = REFORM(block_HAA_decomp[0,*,*])
		ani = REFORM(block_HAA_decomp[2,*,*])
		
		; top right
		index_1 = WHERE((ent GT lim_H1) AND (ani GT lim_A),anz_1)
		IF anz_1 GT 0 THEN class_H_A[index_1] = 1
		
		; bottom right
		index_2 = WHERE((ent GT lim_H1) AND (ani LE lim_A),anz_2)
		IF anz_2 GT 0 THEN class_H_A[index_2] = 2
		
		; top middle
		index_3 = WHERE((ent GT lim_H2) AND (ent LE lim_H1) AND (ani GT lim_A),anz_3)
		IF anz_3 GT 0 THEN class_H_A[index_3] = 3
		
		; bottom middle
		index_4 = WHERE((ent GT lim_H2) AND (ent LE lim_H1) AND (ani LE lim_A),anz_4)
		IF anz_4 GT 0 THEN class_H_A[index_4] = 4
		
		; top left
		index_5 = WHERE((ent LE lim_H2) AND (ani GT lim_A),anz_5)
		IF anz_5 GT 0 THEN class_H_A[index_5] = 5
		
		; bottom left
		index_6 = WHERE((ent LE lim_H2) AND (ani LE lim_A),anz_6)
		IF anz_6 GT 0 THEN class_H_A[index_6] = 6
		
		; Calcul alpha_1, alpha_2, beta_1, beta_2, p1, p2
		; -----------------------------------------------
		FOR k=0,xdim_in-1 DO BEGIN
			FOR l=0,blocksizes[kk]-1 DO BEGIN	
				ew = REFORM(ABS(block_eigen_decomp[head_eigen_decomp[1]-1,*,k,l]))
				ev = REFORM(ABS(block_eigen_decomp[0:head_eigen_decomp[1]-2,*,k,l]))
				
				IF head_eigen_decomp[2] EQ 4 THEN BEGIN
					ew = ew[0:2]
					ev = ev[0:2,0:2]
				ENDIF
				
				; Calcul of the pseudo-probabilite
				pi = ew / total(ew)
				tmp_block[p1,k,l] = pi[0]
				tmp_block[p2,k,l] = pi[1]
				
				; Calcul of alpha1 and alpha 2
				tmp_block[alpha1,k,l] = ACOS(ABS(ev[0,0]))
				tmp_block[alpha2,k,l] = ACOS(ABS(ev[0,1]))
				
				; Calcul of beta1 and beta2
				tmp_block[beta1,k,l] = ATAN(ABS(ev[2,0]),ABS(ev[1,0]+eps))
				tmp_block[beta2,k,l] = ATAN(ABS(ev[2,1]),ABS(ev[1,1]+eps))
			ENDFOR
		ENDFOR
		
		class_al1[*,*] = 1
		index_7 = WHERE(REFORM(tmp_block[alpha1,*,*]) LT !pi/4,anz_7)
		IF anz_7 GT 0 THEN class_al1[index_7] = 2
		
		bid1 = REFORM(tmp_block[p1,*,*]) * COS(REFORM(tmp_block[alpha1,*,*])) + REFORM(tmp_block[p2,*,*]) * COS(REFORM(tmp_block[alpha2,*,*])) 
		bid2 = REFORM(tmp_block[p1,*,*]) * SIN(REFORM(tmp_block[alpha1,*,*])) * COS(REFORM(tmp_block[beta1,*,*])) + $
		       REFORM(tmp_block[p2,*,*]) * SIN(REFORM(tmp_block[alpha2,*,*])) * COS(REFORM(tmp_block[beta2,*,*]))
		
		class_al1_al2[*,*] = 1
		index_8 = WHERE(bid1 GT bid2,anz_8)
		IF anz_8 GT 0 THEN class_al1_al2[index_8] = 2
		
		; count the different classes
		IF kk EQ 0 THEN BEGIN
			wishart    = REFORM(block_HAA_classif[norg : (xdim_in-1) - norg,norg:*])
			HA_class   = REFORM(class_H_A[norg : (xdim_in-1) - norg,norg:*])
			a1_class   = REFORM(class_al1[norg : (xdim_in-1) - norg,norg:*])
			a1a2_class = REFORM(class_al1_al2[norg : (xdim_in-1) - norg,norg:*])
		ENDIF
		IF (kk GT 0) AND (kk LT anz_blocks-1) THEN BEGIN
			wishart    = REFORM(block_HAA_classif[norg :(xdim_in-1)-norg,*])
			HA_class   = REFORM(class_H_A[norg :(xdim_in-1)-norg,*])
			a1_class   = REFORM(class_al1[norg :(xdim_in-1)-norg,*])
			a1a2_class = REFORM(class_al1_al2[norg :(xdim_in-1)-norg,*])
		ENDIF
		IF kk EQ anz_blocks-1 THEN BEGIN
			wishart    = REFORM(block_HAA_classif[norg :(xdim_in-1)-norg,0:(blocksizes[kk]-1)-norg])
			HA_class   = REFORM(class_H_A[norg :(xdim_in-1)-norg,0:(blocksizes[kk]-1)-norg])
			a1_class   = REFORM(class_al1[norg :(xdim_in-1)-norg,0:(blocksizes[kk]-1)-norg])
			a1a2_class = REFORM(class_al1_al2[norg :(xdim_in-1)-norg,0:(blocksizes[kk]-1)-norg])
		ENDIF
		FOR class_wishart=0,nr_classes-1 DO BEGIN
			FOR class_other=1,6 DO BEGIN
				dummy = WHERE((wishart EQ class_wishart) AND (HA_class EQ class_other),count)
				cpt_classes_H_A[class_wishart,class_other] += count
			ENDFOR
			FOR class_other=1,2 DO BEGIN
				dummy = WHERE((wishart EQ class_wishart) AND (a1_class EQ class_other),count)
				cpt_classes_al1[class_wishart,class_other] += count
				dummy = WHERE((wishart EQ class_wishart) AND (a1a2_class EQ class_other),count)
				cpt_classes_al1_al2[class_wishart,class_other] += count
			ENDFOR
		ENDFOR

	ENDFOR ; End of the terrain classification block loop
	FREE_LUN,ddd1,ddd2,ddd3

	; Research of the maximum following the wishart classif
	dummy = MAX(cpt_classes_H_A,dim=2,pos)
	pos_H_A = pos / nr_classes

	dummy = MAX(cpt_classes_al1,dim=2,pos)
	pos_al1 = pos / nr_classes
	
	dummy = MAX(cpt_classes_al1_al2,dim=2,pos)
	pos_al1_al2 = pos / nr_classes

	rrat,HAA_classif_filename, ddd1,header=head_HAA_classif, info=info_HAA_classif, type=type_HAA_classif
	srat,outputfile,           eee, header=head_HAA_classif, info=info_HAA_classif, type=type_terrain_class

	FOR kk=0,anz_blocks-1 DO BEGIN
	
		; Make the block array
		block_HAA_classif  = MAKE_ARRAY([xdim_in,blocksizes[kk]],type=head_HAA_classif[3])
		oblock             = MAKE_ARRAY([xdim_in,blocksizes[kk]],type=head_HAA_classif[3])
		
		; Read the input block
		READU,ddd1,block_HAA_classif
		
		; Research following the classes which is what
		FOR class_wishart = 0, nr_classes - 1 DO BEGIN
			dummy = WHERE(block_HAA_classif EQ class_wishart, count)
			IF count NE 0 THEN BEGIN
				tmp_H_A = pos_H_A[class_wishart]
				tmp_al1 = pos_al1[class_wishart]
				tmp_al1_al2 = pos_al1_al2[class_wishart]
				
				; High entropy --> this means volume decorelation
				IF (tmp_H_A EQ 1) OR (tmp_H_A EQ 2) THEN oblock[dummy] = 1

				; Medium or low entropy and low anisotropy --> this means one scattering mechanism
				IF (tmp_H_A EQ 4) OR (tmp_H_A EQ 5) OR (tmp_H_A EQ 6) THEN $
					IF (tmp_al1 EQ 1) THEN oblock[dummy] = 2 ELSE oblock[dummy] = 3

				; Medium entropy and high anisotropy --> this means two scattering mechanism
				IF (tmp_H_A EQ 3) THEN $
					IF (tmp_al1_al2 EQ 1) THEN oblock[dummy] = 4 ELSE oblock[dummy] = 5
				
			ENDIF
		ENDFOR
		
		;Write the result
		WRITEU,eee,oblock
		
	ENDFOR
	
	FREE_LUN,ddd1,eee

	; update file information

	FILE_MOVE,outputfile,finalfile,/OVERWRITE
		
	file.name = finalfile
	file.type = type_terrain_class
	file.vdim = 1l
	file.zdim = 1l 
	file.xdim = xdim_in
	file.ydim = ydim_in
	file.dim  = 2l 
	file.var  = 2l

; set palette
	
	palettes[0,*,*] = palettes[5,*,*]   ; palettes 5 = classification
	palettes[1,*,*] = palettes[5,*,*]   ; to actual and suggestion

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
  	endif 	
END
