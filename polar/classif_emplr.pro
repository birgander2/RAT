;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: classif_emplr
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
function block3mmtr,in1,in2   ; trace of a matrix multiplication
	nrx = (size(in2))[1]
	nry = (size(in2))[2]
	out = make_array(nrx,nry)
   pol = (size(in1))[1]
	for vv = 0,pol-1 do $
		for kk = 0,pol-1 do $
			out += reform(in1[kk,vv] * in2[*,*,vv,kk])
	return,out
end
function block3mv,arr1,arr2
	ch = (size(arr2))[3]
	out = arr2-arr2
	for i=0,ch-1 do begin
		for j=0,ch-1 do begin
			out[*,*,i] += arr1[j,i] * arr2[*,*,j]
		endfor
	endfor
	out = reform(out,/overwrite)
	return, out
end


pro classif_emplr, CALLED = called, Tfile=Tfile, InitFile=InitFile, NR_ITERATIONS=NR_ITERATIONS
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

	if not keyword_set(called) then begin             ; Graphical interface
		inputfile1 = ''
                if file.type eq 220 or file.type eq 221 or file.type eq 222 || (file.type ge 510 && file.type le 513) then inputfile1 = file.name
		inputfile2 = ''
		itera = ''

tryagain_plr:
		
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Expectation maximisation classification',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		
		line1 = WIDGET_BASE(main,column=3)
		text1 = CW_FIELD(line1,VALUE=inputfile1,/string,XSIZE=60,TITLE='[C] / [T]-matrix     :')
		brow1 = WIDGET_BUTTON(line1,VALUE=' browse ',ysize=35)

		line4 = WIDGET_BASE(main,column=3)
		select1 = cw_bgroup(line4,[' Yes ',' No '],label_left='Perform random initialisation ',set_value=0,/exclusive,/row)
		text4 = CW_FIELD(line4,VALUE=8,/integer,XSIZE=2,TITLE='      No. of classes :')

		line2 = WIDGET_BASE(main,column=3)
		text2 = CW_FIELD(line2,VALUE=inputfile2,/string,XSIZE=60,TITLE='Initialisation map   :')
		brow2 = WIDGET_BUTTON(line2,VALUE=' browse ',ysize=35)

		line3 = WIDGET_BASE(main,column=2)
		text3 = CW_FIELD(line3,VALUE=10,/integer,XSIZE=3,       TITLE='Number of full iterations :')

		wplr1 = WIDGET_BASE(main,row=10,/frame)
		select2 = cw_bgroup(wplr1,[' Yes ',' No '],label_left='Perform probabilistic label relaxation ',set_value=1,/exclusive,/row)
		text5 = CW_FIELD(wplr1,VALUE=5,/integer,XSIZE=3,       TITLE='No. of internal PLR iterations:')
		text6 = CW_FIELD(wplr1,VALUE=10,/integer,XSIZE=3,      TITLE='Compatibility ratio           :')

	
		buttons  = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
		pos = center_box(toto[0],drawysize=toto[1])
		widget_control, main, xoffset=pos[0], yoffset=pos[1]

		repeat begin    ; Event loop
			widget_control,select1,get_value=flag_ran
			widget_control,select2,get_value=flag_plr
			widget_control,line2,sensitive=flag_ran
			widget_control,text4,sensitive=1-flag_ran
			widget_control,text5,sensitive=1-flag_plr
			widget_control,text6,sensitive=1-flag_plr
			event = widget_event(main)
			if event.id eq but_info then begin $
				infotext = ['Expectation maximisation classification',$
				'including probabilistic label relaxation',$
				' ',$
				'RAT module written 01/2007 by Andreas Reigber',$
				'',$
				'further information:',$
				'A. Reigber, M. Jaeger, M. Neumann and L. Ferro-Famil: Polarimetric',$
				'fuzzy k-means classification with consideration of spatial context',$
				'Proc. POLINSAR07, Frascati, Italy, 2007']
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
		widget_control,text3,get_value=itera
		widget_control,text4,get_value=nr_classes
		widget_control,text5,get_value=plritera
		widget_control,text6,get_value=c_ratio
		WIDGET_CONTROL, /DESTROY, main
		if event.id ne but_ok then return     
	endif else begin

	endelse
	
; undo function
 
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head1 = 1l
	head2 = 1l
	rrat,inputfile1,ddd1,header=head1,info=info1,type=type1		
	if ~(type1 ge 220 && type1 le 222) && ~(type1 ge 510 && type1 le 513) then begin
		error = DIALOG_MESSAGE("[C] / [T] matrix file is of wrong type", DIALOG_PARENT = wid.base,TITLE='Error',/error)
		free_lun,ddd1,ddd2
		goto,tryagain_plr
	endif
	if flag_ran eq 1 then begin
		rrat,inputfile2,ddd2,header=head2,info=info2,type=type2		
		if type2 lt 400 || type2 ge 500 then begin
			error = DIALOG_MESSAGE("Initialisation file is of wrong type", DIALOG_PARENT = wid.base,TITLE='Error',/error)
			free_lun,ddd1,ddd2
			goto,tryagain_plr
		endif
		if head1[3] ne head2[1] or head1[4] ne head2[2] then begin
			error = DIALOG_MESSAGE("Files sizes not corresponding", DIALOG_PARENT = wid.base,TITLE='Error',/error)
			free_lun,ddd1,ddd2
			goto,tryagain_plr
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
	
; ----------------------		
; Initialise class centers
; ----------------------		
	
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

; MISSING: ELIMINATE EMPTY CLASSES
	
	aux = where(nr_per_class ne 0,nr)
	if nr gt 0 then begin
		nr_per_class = nr_per_class[aux]		
		center  = center[*,*,aux]
		nr_classes = nr			
	endif
	
; calculating preview size and number of blocks

  bs = config.blocksize
  overlap = 2
  calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

  byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos
	
; ----------------------		
; HERE WE GO!!!
; ----------------------		
 
; Calculate compatibility matrix

 	p2 = 1.0
	norm = 1.0 / float(c_ratio + nr_classes - 1)
	pxx = fltarr(nr_classes,nr_classes)+p2*norm
	pxx[indgen(nr_classes)*(nr_classes+1)] = c_ratio*norm

; Set Gaussian neighbourhood

	knach3 = [[1,2,1],[2,4,2],[1,2,1]] / 16.0
	knach5 = [[1,4,6,4,1],[4,16,24,16,4],[6,24,36,24,6],[4,16,24,16,4],[1,4,6,4,1]] / 256.0
 	knach = reform(knach5,5,5,1)
 
; ----

	head2 = 1l
	srat,outputfile,ddd2,header=[2l,xdim,ydim,2l],info=info1,type=444l		

	for iterations=1,itera do begin    	
 
		progress,Message='Clustering  ('+strcompress(iterations,/rem)+')',/cancel_button
		
		center_new = make_array(vdim1,zdim1,nr_classes,/complex)			
		sc_new     = make_array(nr_classes,/float)			

		ypos1 = 0                     ; block processing start
		ypos2 = bs - overlap          ; block processing end
		head1 = 1l
		rrat,inputfile1,ddd1,header=head1,info=info1,type=type1		
		for i=0,anz_blocks-1 do begin
			progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		
; Read block
	
			block1  = make_array([vdim1,zdim1,xdim,blocksizes[i]],/complex)
			inblock = make_array([xdim,blocksizes[i],vdim1,zdim1],/complex)
			ms      = make_array([xdim,blocksizes[i],nr_classes],/float)
			readu,ddd1,block1
			for k=0,vdim1-1 do for l=0,zdim1-1 do inblock[*,*,k,l] = block1[k,l,*,*]   ; some speedup trick

; Calculate class memberships
	
			for k=0,nr_classes-1 do begin
				vtest = center[*,*,k]
				ms[*,*,k] = exp(-block3mmtr(la_invert(vtest,status=dummy),inblock)) / abs(la_determ(vtest,/check)) 
			endfor
			tdist = total(ms,3)
			for k=0,nr_classes-1 do ms[*,*,k] /= tdist
			rmnanq,ms
			
; Calculate neighbourhood function

			if not keyword_set(flag_plr) and iterations gt itera/3 then begin
				for l=0,plritera-1 do begin
					q = fltarr(xdim,blocksizes[i],nr_classes)
					q = block3mv(pxx,convol(ms,knach,/center,/edge_truncate))
					ms *= q
					tdist = total(ms,3)
					for k=0,nr_classes-1 do ms[*,*,k] /= tdist
					rmnanq,ms
				endfor
			endif 	

; Calculate new class centres

			if i eq anz_blocks-1 then ypos2 = bs_last ; block processing
			
			sc_new += total(total(ms[*,ypos1:ypos2-1,*],1),1)
			for k=0,nr_classes-1 do $
				for x=0,vdim1-1 do for y=0,zdim1-1 do center_new[x,y,k] += total(inblock[*,ypos1:ypos2-1,x,y] * ms[*,ypos1:ypos2-1,k])

; Write output

			if  iterations eq itera then begin 
				progress,Message='Generating class assignments',/cancel_button
				pmax = max(ms,classes,dim=3)
				classes -= lindgen(xdim,blocksizes[i])
				classes /= (xdim*blocksizes[i])
				writeu,ddd2,fix(classes[*,ypos1:ypos2-1])
			endif
			
			ypos1 = overlap ; block processing

			
			point_lun,-ddd1,file_pos
			point_lun,ddd1,file_pos - 2 * overlap * vdim1 * zdim1 * xdim * byt[var1]
		endfor ; blocks
		free_lun,ddd1

		for k=0,nr_classes-1 do center_new[*,*,k] /= sc_new[k]
		center = center_new

	endfor ; iterations

	free_lun,ddd2

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
	
	palettes[0,*,*] = palettes[5,*,*]   ; palettes 6 = system 20 classes
	palettes[1,*,*] = palettes[5,*,*]   ; to actual and suggestion

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
    	

end
