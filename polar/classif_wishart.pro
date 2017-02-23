;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: classif_wishart
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
pro classif_wishart, CALLED = called, Tfile=inputfile1, InitFile=inputfile2, NR_ITERATIONS=max_iter
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

        if n_elements(inputfile1) eq 0 then inputfile1 = ''
        if n_elements(inputfile2) eq 0 then inputfile2 = ''
        if n_elements(max_iter)   eq 0 then max_iter   = 10
        if n_elements(nr_classes) eq 0 then nr_classes = 8
        if n_elements(nb_switch)  eq 0 then nb_switch  = .05
        random = inputfile2 ne ''

	if ~keyword_set(called) then begin             ; Graphical interface
		inputfile1 = ''
		if file.type eq 220 or file.type eq 221 or file.type eq 222 || (file.type ge 510 && file.type le 513) then inputfile1 = file.name
		inputfile2 = ''
		tryagain:

		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=6,TITLE='Wishart k-means classification',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		line1 = WIDGET_BASE(main,column=3)
		text1 = CW_FIELD(line1,VALUE=inputfile1,/string,XSIZE=60,TITLE='[C] / [T]-matrix     :')
		brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)

		line4 = WIDGET_BASE(main,column=3)
		select = cw_bgroup(line4,[' Yes ',' No '],label_left='Perform random initialisation ',set_value=0,/exclusive,/row)
		text4 = CW_FIELD(line4,VALUE=8,/integer,/string,XSIZE=2,TITLE='      No. of classes :')

		line2 = WIDGET_BASE(main,column=3)
		text2 = CW_FIELD(line2,VALUE=inputfile2,/string,XSIZE=60,TITLE='Initialisation map   :')
		brow2 = WIDGET_BUTTON(line2,VALUE=' browse ',ysize=35)

	   	line3 = widget_base(main,/row)
   		text6 = cw_field(line3,value=5,/integer,xsize=5,  title='% pixels switching class: 	')
	   	text3 = cw_field(line3,value=10,/integer,xsize=10, title='Maximum number of iterations:')

		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin    ; Event loop
			widget_control,select,get_value=random
			widget_control,line2,sensitive=random
			widget_control,line4,sensitive=1-random
			event = widget_event(main)
			if event.id eq but_info then begin $
				infotext = ['Wishart k-means classification',$
				' ',$
				'RAT module originally started in 2003 by Guido Bethke',$
				'',$
				'Speed optimisation 06/2004 by Stephane Guillaso',$
				'Major improvments  01/2007 by Andreas Reigber',$
				'',$
				'further information:',$
				'J.S. Lee et al.: Unsupervised Classification Using Polarimetric',$
				'Decomposition and the Complex Wishart Classifier, IEEE Transactions',$
				'on Geoscience and Remote Sensing, Vol.37, No. 5. pp. 2249-2258, 1999']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main,TITLE='Information')
			end
			if event.id eq brow1 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile1 = DIALOG_PICKFILE(TITLE='Open [C]/[T]-matix file',DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST,PATH=path, GET_PATH=path)
				if strlen(inputfile1) gt 0 then config.workdir = path
				widget_control,text1,set_value=inputfile1
			endif
			if event.id eq brow2 then begin                  ; Info Button clicked
				path = config.workdir
				inputfile2 = DIALOG_PICKFILE(TITLE='Open initialisation file',DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST,PATH=path, GET_PATH=path)
				if strlen(inputfile2) gt 0 then config.workdir = path
				widget_control,text2,set_value=inputfile2
			endif
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,text3,get_value=max_iter
		widget_control,text4,get_value=nr_classes
		widget_control,text6,get_value=nb_switch & nb_switch = float(nb_switch)/100.0
		WIDGET_CONTROL, /DESTROY, main
		if event.id ne but_ok then return     
	endif 

	if inputfile1 eq '' then begin
		error = DIALOG_MESSAGE("Please select a [C] / [T] matrix file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		goto,tryagain
	endif
	if random eq 1 and inputfile2 eq '' then begin
		error = DIALOG_MESSAGE("Please select initialisation file", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		goto,tryagain
	endif
	if max_iter eq '' then begin
		error = DIALOG_MESSAGE("Please set number of iterations", DIALOG_PARENT = wid.base, TITLE='Error',/error)
		goto,tryagain
	endif

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
; first image

	head1 = 1l
	head2 = 1l
	rrat,inputfile1,ddd1,header=head1,info=info1,type=type1		
	if ~(type1 ge 220 && type1 le 222) && ~(type1 ge 510 && type1 le 513) then begin
		error = DIALOG_MESSAGE("[C] / [T] matrix file is of wrong type", DIALOG_PARENT = wid.base,TITLE='Error',/error)
		free_lun,ddd1,ddd2
		goto,tryagain
	endif
	if random eq 1 then begin
		rrat,inputfile2,ddd2,header=head2,info=info2,type=type2
		if type2 lt 400 || type2 ge 500 then begin
			error = DIALOG_MESSAGE("Initialisation file is of wrong type", DIALOG_PARENT = wid.base,TITLE='Error',/error)
			free_lun,ddd1,ddd2
			goto,tryagain
		endif
		if head1[3] ne head2[1] or head1[4] ne head2[2] then begin
			error = DIALOG_MESSAGE("Files sizes not corresponding", DIALOG_PARENT = wid.base,TITLE='Error',/error)
			free_lun,ddd1,ddd2
			goto,tryagain
		endif

		nr_classes = 0
		init_line = make_array(head2[1],type=head2[3])
		for i=0l,head2[2]-1 do begin
			readu,ddd2,init_line
			nr_classes = max([nr_classes,max(init_line)])
		endfor
		free_lun,ddd2
	endif else begin
		inputfile2 = config.tempdir+'wishart_tmp.rat'
		srat,inputfile2,ddd2,header=[2l,head1[3],head1[4],2l]		
		for i=0l,head1[4]-1 do begin
			init_line = fix(randomu(seed,head1[3])*nr_classes)
			writeu,ddd2,init_line
		endfor
		free_lun,ddd2
	endelse
	free_lun,ddd1

; change mousepointer

	WIDGET_CONTROL,/hourglass

; output image

	vdim1 = head1[1]
	zdim1 = head1[2]
	xdim  = head1[3]
	ydim  = head1[4]
	var1  = head1[5]

; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,ydim,bs,anz_blocks,bs_last
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; Initialise class centers
	
	head1 = 1l
	head2 = 1l
	rrat,inputfile1,ddd1,header=head1,info=info1,type=type1		
	rrat,inputfile2,ddd2,header=head2,info=info2,type=type2		
	center = make_array(vdim1,zdim1,nr_classes,type=6)
	nr_per_class = lonarr(nr_classes)
	
	progress,Message='Initialising...',/cancel_button
	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel

		block1 = make_array([vdim1,zdim1,xdim,blocksizes[i]],type=var1)
		block2 = make_array([xdim,blocksizes[i]],type=2l)
		readu,ddd1,block1
		readu,ddd2,block2
		for k=0,nr_classes-1 do begin
	    	aux = where(block2 eq k, count)
			if count gt 0 then for x=0,vdim1-1 do for y=0,zdim1-1 do center[x,y,k] += total((reform(block1[x,y,*,*]))[aux])
			nr_per_class[k] += count
		endfor
	endfor
	free_lun,ddd1,ddd2
	for k=0,nr_classes-1 do center[*,*,k] /= nr_per_class[k]
		
; Here we go....
	
	classnumbers = indgen(nr_classes)
	
	head2 = 1l
	srat,outputfile,ddd2,header=[2l,xdim,ydim,2l],info=info1,type=444l		
	
	iter = 1
	for iterations=1,max_iter do begin    	

		progress,Message='Clustering ('+strcompress(iter,/rem)+')',/cancel_button

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
		rrat,inputfile1,ddd1,header=head1,info=info1,type=type1		
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
	
	progress,/destroy

; update file information

	file_move,outputfile,finalfile,/overwrite
		
	file.name = finalfile
	file.type = 444l
	file.vdim = 1l
	file.zdim = 1l
	file.xdim = xdim
	file.ydim = ydim
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
    	
end
