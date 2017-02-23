;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: classif_wishart_opt
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
pro classif_wishart_opt, TFILE = tfile, CFILE = cfile, AFILE = afile,CALLED = called
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	IF NOT KEYWORD_SET(called) THEN BEGIN             ; Graphical interface
		T_matrix_filename=''
		H_a_class_filename=''
		anisotropie_filename=''
		save_interm_filename=''
		select_anisotropie = 0
		save_interm = 0
		select_exclude_border = 0
		nbox = 0
		
	T_matrix_filename = ''
	H_a_class_filename = ''
	if file.type eq 220 or file.type eq 221 or file.type eq 222 || (file.type ge 510 && file.type le 513) then T_matrix_filename = file.name
	if file.type eq 233 then H_a_class_filename = file.name


tryagain:
		; display the widget in the center of the main widget
		main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='K-means Wishart (H/a/A) classification',/floating,/tlb_kill_request_events,/tlb_frame_attr)
			sub_main = WIDGET_BASE(main,/column)

				sub_file_input1 = WIDGET_BASE(sub_main,/column,/FRAME)

					sub_input_T_matrix = WIDGET_BASE(sub_file_input1,/row)
						text_T_matrix = CW_FIELD(sub_input_T_matrix,VALUE=T_matrix_filename,/STRING,XSIZE=60,TITLE='[C] / [T]-matrix:                  ')
						brow_T_matrix = WIDGET_BUTTON(sub_input_T_matrix,VALUE='browse',YSIZE=35)

					sub_input_Classif = WIDGET_BASE(sub_file_input1,/row)
						text_Classif = CW_FIELD(sub_input_Classif,VALUE=H_a_class_filename,/STRING,XSIZE=60, TITLE='H/a/A decomposition:               ')
						brow_Classif = WIDGET_BUTTON(sub_input_Classif,VALUE='browse',YSIZE=35)

					sub_save_intermediaire = WIDGET_BASE(sub_file_input1,/row)
						button_save_intermediaire = CW_BGROUP(sub_save_intermediaire, ['Save intermediate H/alpha classification:'],YPAD=5,/ROW,/NONEXCLUSIVE,SET_VALUE=[save_interm])
						text_save_intermediaire = CW_FIELD(sub_save_intermediaire,VALUE=save_interm_filename,/STRING,XSIZE=60,TITLE='')
						brow_save_intermediaire = WIDGET_BUTTON(sub_save_intermediaire,VALUE='browse',YSIZE=35)

				sub_information = WIDGET_BASE(sub_main,/row,/FRAME)
					text_choose_switch = CW_FIELD(sub_information,VALUE=5,/INTEGER,XSIZE=5,  TITLE='    % pixels switching class:    ')
					text_choose_max_iter = CW_FIELD(sub_information,VALUE=10,/INTEGER,XSIZE=5,TITLE='    Maximum number of iterations:')

			buttons  = WIDGET_BASE(main,/row,/frame)
				but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
				but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
				but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		
		; Anisotropie is not set by default
		WIDGET_CONTROL,text_save_intermediaire,SENSITIVE=save_interm
		WIDGET_CONTROL,brow_save_intermediaire,SENSITIVE=save_interm
				
		;REPEAT LOOP EVENT
		REPEAT BEGIN
					
			;get event
			event = WIDGET_EVENT(main)
			
			; display the info message box
			IF event.id EQ but_info THEN BEGIN 
				infotext = ['K-means Wishart (H/a/A) classification',$
				' ',$
				'Version 3.0 - RAT module written 01/2007 by Andreas Reigber',$
				'Version 2.0 - RAT module written 2004 by Stephane Guillaso & Andreas Reigber',$
				'Version 1.0 - RAT module written 2003 by Guido Bethke',$
				'',$
				'further information:',$
				'L. Ferro-Famil, E. Pottier, and J.-S. Lee: Unsupervised classification of ',$
				'multifrequency and fully polarimetric SAR images based on the H/A/Alpha-Wishart',$
				'classifier, IEEE Transactions on Geoscience and Remote Sensing, Vol. 39,',$
				'No. 11, pp. 2332-2342, 2001.']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main,TITLE='Information')
			ENDIF
			
			; get the coherency matrix filename
			IF event.id EQ brow_T_matrix THEN BEGIN
				path = config.workdir
				T_matrix_filename = DIALOG_PICKFILE(TITLE='Open [C]/[T]-matix file',DIALOG_PARENT=wid.base, FILTER = '*.rat',/MUST_EXIST,PATH=path,GET_PATH=path)
				IF STRLEN(T_matrix_filename) GT 0 THEN config.workdir = path
 				WIDGET_CONTROL,text_T_matrix,SET_VALUE=T_matrix_filename
			ENDIF
			
			; get the classification H/alpha filename
			IF event.id EQ brow_Classif THEN BEGIN
				path = config.workdir
				H_a_class_filename = DIALOG_PICKFILE(TITLE='Open H/alpha classification file',DIALOG_PARENT=wid.base, FILTER = '*.rat',/MUST_EXIST,PATH=path,GET_PATH=path)
				IF STRLEN(H_a_class_filename) GT 0 THEN config.workdir = path
 				WIDGET_CONTROL,text_Classif,SET_VALUE=H_a_class_filename
			ENDIF
			
			; Save intermediaire behaviour
			IF event.id EQ button_save_intermediaire THEN BEGIN
				WIDGET_CONTROL,text_save_intermediaire,SENSITIVE=event.select
				WIDGET_CONTROL,brow_save_intermediaire,SENSITIVE=event.select
				save_interm = event.select
			ENDIF
			
			IF event.id EQ brow_save_intermediaire THEN BEGIN
				path = config.workdir
				save_interm_filename = DIALOG_PICKFILE(TITLE='Write Wishart H/alpha classification file',DIALOG_PARENT=wid.base, FILTER = '*.rat',PATH=path,GET_PATH=path)
				if strlen(save_interm_filename) gt 0 then config.workdir = path
 				widget_control,text_save_intermediaire,set_value=save_interm_filename
			endif
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'widget_kill_request'
		
		; get all information value
		widget_control,text_choose_switch,  get_value=nb_switch & nb_switch = float(nb_switch)/100.0
		widget_control,text_choose_max_iter,get_value=max_iter
		
		; destroy the widget
  		widget_control, /destroy, main
		
		; quit if not ok button
		if event.id ne but_ok then	return
	
		; Test if we have a [T]-matrix file input
	
		if t_matrix_filename eq '' then begin
			error = DIALOG_MESSAGE("Please select a [C]/[T]-matrix file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
			goto,tryagain
		endif else begin
			; read the header file
			head_t_matrix=1l
			rrat,t_matrix_filename,ddd,header=head_t_matrix,info=info_t_matrix,type=type_t_matrix
			free_lun,ddd
			if type_t_matrix ne 220 && type_t_matrix ne 221 && type_t_matrix ne 222 && ~(type_t_matrix ge 510 && type_t_matrix le 513) then begin
				error = DIALOG_MESSAGE("[C] / [T] matrix file is of wrong type", DIALOG_PARENT = wid.base,TITLE='Error',/error)
				t_matrix_filename=''
				goto,tryagain
			endif
		endelse
		
		; test if we have a h/alpha classification file
		if h_a_class_filename eq '' then begin
			error = DIALOG_MESSAGE("Please select H/a/A decomposition file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
			goto,tryagain
		endif else begin
			; read the header file
			head_h_a_class=1l
			rrat,h_a_class_filename,ddd,header=head_h_a_class,info=info_h_a_class,type=type_h_a_class	
			free_lun,ddd
			if type_h_a_class ne 233 then begin
				error = DIALOG_MESSAGE("H/a/A decomposition file of wrong type", DIALOG_PARENT = wid.base,TITLE='Error',/error)
				h_a_class_filename=''
				goto,tryagain
			endif
		endelse

		; test if every size file correspond
		if (head_t_matrix[3] ne head_h_a_class[2]) or (head_t_matrix[4] ne head_h_a_class[3]) then begin
			error = DIALOG_MESSAGE("Image sizes between both files not corresponding", DIALOG_PARENT = wid.base,TITLE='Error',/error)
			t_matrix_filename=''
			h_a_class_filename=''
			goto,tryagain
		endif
		
		
		
						
	endif else begin ; /called is used   probably not working / never tested
		t_matrix_filename    = tfile
		h_a_class_filename   = cfile	
		select_exclude_border = 0
		save_interm = 0
		nbox = 0
		nb_switch = 0.1
		max_iter  = 10
		if keyword_set(afile) then begin
			anisotropie_filename = afile
			select_anisotropie   = 1
		endif
		head_T_matrix=1l
		rrat,T_matrix_filename,ddd,header=head_T_matrix,info=info_T_matrix,type=type_T_matrix
		head_H_a_class=1l
		rrat,H_a_class_filename,ddd,header=head_H_a_class,info=info_H_a_class,type=type_H_a_class	
	endelse
		
; undo function

   undo_prepare,outputfile,finalfile,CALLED=CALLED

; change mousepointer

	WIDGET_CONTROL,/hourglass

; ***** EXTRACT anisotropy *****
	
	file.name = H_a_class_filename
	file.type = 233
	file.var  = 4
	file.dim  = 3
	file.vdim = 1
	file.zdim = 3
	file.xdim = head_H_a_class[2]
	file.ydim = head_H_a_class[3]
	extract_channels,/called,channel=2
	file_move,file.name,config.tempdir+'anisotropy.rat',/overwrite	
	anisotropie_filename = config.tempdir+'anisotropy.rat'
	
; ***** EXTRACT H/a segmentation and anisotropy *****
	
	file.name = H_a_class_filename
	file.type = 233
	file.var  = 4
	file.dim  = 3
	file.vdim = 1
	file.zdim = 3
	file.xdim = head_H_a_class[2]
	file.ydim = head_H_a_class[3]
	classif_ea,/called
	file_move,file.name,config.tempdir+'class_ea.rat',/overwrite	
	H_a_class_filename = config.tempdir+'class_ea.rat'

; =================

	type_wishart_class = 403 
	
; Information about the input image

	vdim1 = head_T_matrix[1]
	zdim1 = head_T_matrix[2]
	xdim  = head_T_matrix[3]
	ydim  = head_T_matrix[4]
	var1  = head_T_matrix[5]
	
; Information about the output image

	vdim2 = 1
	zdim2 = 1
	var2  = 2l

; Calcul of the block size

	bs = config.blocksize
	calc_blocks_normal,ydim,bs,anz_blocks,bs_last 
	blocksizes = INTARR(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

;---------------------------------------------------------------------
; Here we go.... 
;---------------------------------------------------------------------
	nr_classes = 10
	
; Initialise class centers
	
	for round=0,1 do begin  ; First round Ha, second HaA
	
		head1 = 1l
		head2 = 1l
		head3 = 1l
		rrat,T_matrix_filename,ddd1,header=head1,info=info1,type=type1		
		rrat,H_a_class_filename,ddd2,header=head2,info=info2,type=type2		
		if round eq 1 then rrat,anisotropie_filename,ddd3,header=head3,info=info3,type=type3	
		if round eq 1 then nr_classes *= 2
		
		center = make_array(vdim1,zdim1,nr_classes,type=6)
		nr_per_class = lonarr(nr_classes)
		
		progress,Message='Initialising...',/cancel_button
		for i=0,anz_blocks-1 do begin
			progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
	
			block1 = make_array([vdim1,zdim1,xdim,blocksizes[i]],type=var1)
			block2 = make_array([xdim,blocksizes[i]],type=2l)
			if round eq 1 then block3 = make_array([xdim,blocksizes[i]],type=4l)
			readu,ddd1,block1
			readu,ddd2,block2
			if round eq 1 then readu,ddd3,block3
			for k=0,nr_classes-1 do begin
		    	if round eq 0 then aux = where(block2 eq k, count) else aux = where(block2 eq k/2 and block3 lt 0.5*((k mod 2)+1), count)
				if count gt 0 then for x=0,vdim1-1 do for y=0,zdim1-1 do center[x,y,k] += total((reform(block1[x,y,*,*]))[aux])
				nr_per_class[k] += count
			endfor
		endfor
		free_lun,ddd1,ddd2
		for k=0,nr_classes-1 do center[*,*,k] /= nr_per_class[k]
			
	
		classnumbers = indgen(nr_classes)
		
		head2 = 1l
		srat,outputfile,ddd2,header=[2l,xdim,ydim,2l],info=info1,type=444l		
		
		iter = 1
		for iterations=1,max_iter do begin    	
	
			if round eq 0 then $
				progress,Message='Wishart H/a clustering ('+strcompress(iter,/rem)+')',/cancel_button $
			else $
				progress,Message='Wishart H/a/A clustering ('+strcompress(iter,/rem)+')',/cancel_button 
			
; Eliminate empty classes
	
			aux = where(nr_per_class ne 0,nr)
			if nr gt 0 then begin
				nr_per_class = nr_per_class[aux]		
				center  = center[*,*,aux]
				nr_classes = nr			
			endif
			center_new       = make_array(vdim1,zdim1,nr_classes,/complex)
			nr_per_class_new = make_array(nr_classes,/long)
	
; Calculate class memberships
	
			head1 = 1l
			rrat,T_matrix_filename,ddd1,header=head1,info=info1,type=type1		
			for i=0,anz_blocks-1 do begin
				progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
			
				block1  = make_array([vdim1,zdim1,xdim,blocksizes[i]],/complex)
				inblock = make_array([xdim,blocksizes[i],vdim1,zdim1],/complex)
				ms      = make_array([xdim,blocksizes[i],nr_classes],/float)
				readu,ddd1,block1
				for k=0,vdim1-1 do for l=0,zdim1-1 do inblock[*,*,k,l] = block1[k,l,*,*]   ; some speedup trick
	
				for k=0,nr_classes-1 do begin
					vtest = center[*,*,k]
					ms[*,*,k] = block3mmtr(la_invert(vtest,status=dummy),inblock) + alog(abs(la_determ(vtest,/check))) 
				endfor
				
; Calculate new class centres
				
				aux = min(ms,classes,dim=3)
				classes -= lindgen(xdim,blocksizes[i])
				classes /= (xdim*blocksizes[i])
				for k=0,nr_classes-1 do begin
		    		aux = where(classes eq k, count)
					if count gt 0 then for x=0,vdim1-1 do for y=0,zdim1-1 do center_new[x,y,k] += total((reform(inblock[*,*,x,y]))[aux])
					nr_per_class_new[k] += count
				endfor
				
; write result			
				
				if iterations eq max_iter then writeu,ddd2,fix(classes)
	
			endfor ; anz_blocks
			free_lun,ddd1
			
; Update class centres
				
			for k=0,nr_classes-1 do center_new[*,*,k] /= nr_per_class_new[k]
			center = center_new

; Test for convergence
			
			change = total(abs(nr_per_class-nr_per_class_new))/2.0/xdim/ydim
			if change lt nb_switch and iterations ne max_iter then iterations = max_iter-1

			nr_per_class = nr_per_class_new
			iter++
		endfor ; iterations
		free_lun,ddd2

		file_copy,outputfile,H_a_class_filename,/overwrite		
		if save_interm eq 1 and round eq 0 then begin
			file_copy,H_a_class_filename,save_interm_filename,/overwrite
			palettes[0,*,*] = palettes[5,*,*]   ; palettes 9 = system 20 classes
			palettes[1,*,*] = palettes[5,*,*]   ; copy to actual and suggestion
			save_rit,filename=save_interm_filename
		endif
	endfor ; round
		
; update file information

	file_move,outputfile,finalfile,/overwrite
		
	file.name = finalfile
	file.type = type_wishart_class
	file.vdim = 1l
	file.zdim = 1l 
	file.xdim = xdim
	file.ydim = ydim
	file.dim  = 2l 
	file.var  = 2l

; set palette
	
	palettes[0,*,*] = palettes[5,*,*]   ; palettes 9 = system 20 classes
	palettes[1,*,*] = palettes[5,*,*]   ; copy to actual and suggestion

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
    	
end
