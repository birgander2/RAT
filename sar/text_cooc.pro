;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: text_cooc
; last revision : Juni 2004
; written by    : Jan-Christoph Unger
; Feature image calculation by means of Co-occurrence measurements
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

Function glcm,arr,dx,dy,maxb,All=all,SYM=sym,dist=dist

siz=size(arr)
CoMat=make_array(maxb,maxb,/byte,value=0)
count=0

if dist lt 0 then dy=-dy 				;transform image system to mathematical system

case 1 of
(Keyword_set(sym)):begin				; symmetrical co-occurrence matrix for valid dicplacements and directions,
	if dx gt 0 then lastX=dx else lastX=0		; for example for horizontal neighbor points with 0° and 180°
	if dy gt 0 then lastY=dy else lastY=0
	if dx lt 0 then firstX=abs(dx) else firstX=0
	if dy lt 0 then firstY=abs(dy) else firstY=0

	for x=firstX, siz[1]-lastX-1 do for y=firstY, siz[2]-lastY-1 do begin
		sp=arr[x,y]
		zp=arr[x+dx,y+dy]
		CoMat[sp,zp]++
		CoMat[zp,sp]++
		count+=2
	endfor
end
(Keyword_set(dist)):begin	;calculating symmetrical co-occurrence matrix for all directions [0°,90°,45°,135°]

	for x=0, siz[1]-1 do for y=0, siz[2]-1 do begin
		sp=arr[x,y]

		if x lt siz[1]-dist-1 then begin
								zp0=arr[x+dist,y]
								CoMat[sp,zp0]++
								CoMat[zp0,sp]++
								count+=2
		endif
		if y lt siz[2]-dist-1 then begin
								zp90=arr[x+0,y+dist]
								CoMat[sp,zp90]++
								CoMat[zp90,sp]++
								count+=2
		endif
		if x lt siz[1]-dist-1 and y gt dist then begin
								zp45=arr[x+dist,y-dist]
								CoMat[sp,zp45]++
								CoMat[zp45,sp]++
								count+=2
		endif
		if x lt siz(1)-dist-1 and y lt siz[2]-dist-1 then begin
								zp135=arr[x+dist,y+dist]
								CoMat[sp,zp135]++
								CoMat[zp135,sp]++
								count+=2
		endif

	endfor
	end
else:	begin	; non symmetrical co-occurrence matrix, only one neighbor point per pixel considered
			if dx gt 0 then lastX=dx else lastX=0
			if dy gt 0 then lastY=dy else lastY=0
			if dx lt 0 then firstX=abs(dx) else firstX=0
			if dy lt 0 then firstY=abs(dy) else firstY=0

			for x=firstX, siz[1]-lastX-1 do for y=firstY, siz[2]-lastY-1 do begin
				sp=arr[x,y]
				zp=arr[x+dx,y+dy]
				CoMat[sp,zp]++
  				count++
			endfor
		end
endcase
	cooc={glcm:bytarr(maxb,maxb),count:0l}
	cooc.glcm=CoMat			;co-occurrence matrix
	cooc.count=count		;counter
	return,cooc			;return structure
end

function mbox,n,anz
	box = intarr(n*n)
	for i=0,n-1 do box[n*i] = (findgen(n)-n/2)+anz*(i-n/2)
	return,box
end

;------------------------------------------------
;Textural feature determination
;------------------------------------------------

FUNCTION p_glcm,cooc, $
			ASM=asm,$	;1
			CONTR=contr,$	;2
			ENTRO=entro,$	;3
			IDM=idm,$	;4
			CORR=corr,$	;5
			DISS=diss,$	;6
			MAXP=maxp,$	;7
			MEAN=mean,$	;8
			VAR=var,$	;9
			CL_SHADE=cl_shade,$	;10
			CL_PROM=cl_prom	;11
vec=fltarr(11)

;Statistikberechnung
;-------------------
glcm=cooc.glcm						;Werte aus Structur gelesen
nglcm=glcm/float(cooc.count)				;normierte GLCM
glcm_dim=size(glcm)
nichtNullwerte_index_vec=where( glcm ne 0,count)
	if count lt 0 then nichtNullwerte_index_vec=indgen(glcm_dim[4]) ;ist aber unwahrscheinlich

spNglcv=nglcm[nichtNullwerte_index_vec]			;spezieller normierter GLC-Vektor (NGLCM)
spGLCV_anz=N_elements(spNglcv)
Zeilen_vec=nichtNullwerte_index_vec/glcm_dim[1]
Spalten_vec=nichtNullwerte_index_vec-Zeilen_vec*glcm_dim[1]

;Haralick-Parameter
;;------------------
;1. Energie bzw. ASM  (Angular second Moment)
		if	Keyword_set(asm) then vec[0]=total(spNglcv^2)

;2. Kontrast bzw. Traegheit (Contrast)
		if 	Keyword_set(contr) then	vec[1]=total(((Zeilen_vec-Spalten_vec)^2)*total(spNglcv))

;3. Entropie (Entropy)
		if 	Keyword_set(entro) then vec[2]=-total(spNglcv*alog10(spNglcv))

;4. IDM(Inverse Difference Moment bzw.lokale Homogenität)
		if	Keyword_set(idm) then vec[3]=total(1.0/(1.0+(Zeilen_vec-Spalten_vec)^2)*spNglcv)

;5.Korrelation (Haralick)
		if	Keyword_set(corr) then	begin
			Zeilenbereich=where(total(glcm,1) ne 0,count)
				if count lt 0 then Zeilenbereich=indgen(glcm_dim[1])
			Spaltenbereich=where(total(glcm,2) ne 0,count)
				if count lt 0 then Spaltenbereich=indgen(glcm_dim[2])
			ZeilenGLCM=nglcm[*,Zeilenbereich]
			kompaktGLCM=reform(ZeilenGLCM[Spaltenbereich,*],(size(Spaltenbereich))[1],(size(zeilenbereich))[1])
			kompaktGLCM_dim=size(kompaktGLCM)
			einsMatrix=fltarr(kompaktGLCM_dim[1])+1.0
			ZeilenSum=total(kompaktGLCM,1)
			Zeilenmittel=total(Zeilenbereich*ZeilenSum)
			Zeilenvarianz=total(((Zeilenbereich-Zeilenmittel)^2)*ZeilenSum)
			SpaltenSum=total(kompaktGLCM,2)
			Spaltenmittel=total(Spaltenbereich*SpaltenSum)
			Spaltenvarianz=total(((Spaltenbereich-Spaltenmittel)^2)*SpaltenSum)
			Zeilenbereichsmatrix=Zeilenbereich##einsMatrix
			Spaltenbereichsmatrix=Spaltenbereich#einsMatrix
			matrix=(Zeilenbereichsmatrix-Zeilenmittel)*(Spaltenbereichsmatrix-Spaltenmittel)*kompaktGLCM
			vec[4]=total(matrix)/(Zeilenvarianz*Spaltenvarianz)
		endif

;5. Dissimilarity
		if	Keyword_set(diss) then vec[5]=total(abs(Zeilen_vec-Spalten_vec)*spNGLCV)

;6.Maximale Wahrscheinlichkeit (Maximum Probability)
		if	Keyword_set(maxp) then vec[6]=max(spNGLCV)

;7. Mittel (Mean)
		if	Keyword_set(mean) then vec[7]=total(Zeilen_vec*spNGLCV)

;8.Varianz (Variance)
		if	Keyword_set(var) then vec[8]=total(((Zeilen_vec-vec[7])^2)*spNGLCV)

;9. Klusterschatten (cluster shade)
		if	Keyword_set(cl_shade) then vec[9]=total(((Zeilen_vec+Spalten_vec-2*vec[7])^3)*spNGLCV)

;9. Klusterhervortreten (cluster prominence)
		if	Keyword_set(cl_prom) then vec[10]=total(((Zeilen_vec+Spalten_vec-2*vec[7])^4)*spNGLCV)

return, vec
end


pro text_cooc

common rat, types, file, wid, config

	all=0

	; Check inputfile

	if file.type ne 100 and file.type ne 103 then begin
		error = DIALOG_MESSAGE("Intensity or amplitude SAR image required",$
		DIALOG_PARENT = wid.base, TITLE='Error',/error)
		return
	endif

	; Complex input data ????

	if file.var eq 6 or file.var eq 9 then begin
		error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], $
		/cancel, DIALOG_PARENT =wid.base, TITLE='Warning')
		if error eq "Cancel" then return
		complex2abs,/called
	endif

	; Graphical interface

	main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Co-Occurrence features',/floating,/tlb_kill_request_events,/tlb_frame_attr)
	main_left = WIDGET_BASE(main,column=1,/base_align_left,xsize=230)

	sub0 = WIDGET_BASE(main_left,column=1,/base_align_left)
	field1   = CW_FIELD(sub0,VALUE=7,/integer,TITLE= " Square boxsize    :",XSIZE=3,ysize=30)
	field2   = CW_FIELD(sub0,VALUE=64,/integer,TITLE=" Grayscale (0-255) :",XSIZE=3,ysize=30)
	bggroup2 = cw_bgroup(sub0,/nonexclusive,' All directions')

	sub1 = WIDGET_BASE(main_left,column=1,/base_align_left)
	bggroup1 = cw_bgroup(sub1,/nonexclusive,' Symmetrical')
	buttons  = WIDGET_BASE(main,column=3,/frame,xsize=230)
	but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
	but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
	but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)

	sub2 = WIDGET_BASE(sub1,column=1,/base_align_left)
	text=widget_label(sub2,value='                    dx      dy',ysize=20)

	sub3 = WIDGET_BASE(sub2,column=3,/base_align_left)
	text=widget_label(sub3,value='Displacements :',ysize=30)
	field3   = CW_FIELD(sub3,VALUE=1,/integer,XSIZE=3,TITLE='')
	field4   = CW_FIELD(sub3,VALUE=0,/integer,XSIZE=3,TITLE='')

	WIDGET_CONTROL, main, /REALIZE, default_button = but_canc
	jump1:

	repeat begin
		event = widget_event(main)

		if event.id eq but_info then begin               ; Info Button clicked
			infotext = ['Texture features (Co-Occurrence)',$
			' ',$
			'Layer:',$
			" 1. Angular Second Moment, 2. Contrast, 3. Entropy, 4. Inverse Difference Moment, 5. Correlation",$
			" 6. Dissimilarity, 7. Maxium Probability, 8. Mean, 9. Variance, 10. Cluster Shade, 11. Cluster Prominence",$
			' ',$
			'RAT module written 06/2004 by Jan-Christoph Unger']
			info2 = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
		endif
		if event.id eq bggroup2 then begin
			widget_control,bggroup2,GET_VALUE=all
			if all eq 1 then begin
				widget_control,sub1,/DESTROY
				sub3 = WIDGET_BASE(sub0,column=3,/base_align_left)
				field5   = CW_FIELD(sub3,VALUE=1,/integer,XSIZE=3,ysize=30,TITLE='Displacement :')
			endif
			if all eq 0 then  begin
				widget_control,sub3,/DESTROY
				sub1 = WIDGET_BASE(main_left,column=1,/base_align_left)
				bggroup1 = cw_bgroup(sub1,/nonexclusive,' Symmetrical')

				sub2 = WIDGET_BASE(sub1,column=1,/base_align_left)
				text=widget_label(sub2,value='                    dx      dy',ysize=20)

				sub3 = WIDGET_BASE(sub2,column=3,/base_align_left)
				text=widget_label(sub3,value='Displacements :',ysize=30)
				field3   = CW_FIELD(sub3,VALUE=1,/integer,XSIZE=3,TITLE='')
				field4   = CW_FIELD(sub3,VALUE=0,/integer,XSIZE=3,TITLE='')
			endif

		endif
	endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

; Error Handling (box size)
	widget_control,field1,GET_VALUE=boxsiz
	if boxsiz le 1 and event.id ne but_canc then begin                                   ; Wrong box size ?
		error = DIALOG_MESSAGE("Boxsizes has to be > 1", DIALOG_PARENT = main, TITLE='Error',/error)
		goto,jump1
	endif

; Error Handling (grayscale quantisation level)
	widget_control,field2,GET_VALUE=gwa
	if gwa gt 255 or gwa eq 0 and event.id ne but_canc then begin
		error = DIALOG_MESSAGE('Grayscale has to be 0 < x <= 255',DIALOG_PARENT = main, TITLE='Error',/error)
		goto,jump1
	endif

	if all eq 0 then begin
		widget_control,bggroup1,GET_VALUE=sym
		widget_control,field3,GET_VALUE=dx
		widget_control,field4,GET_VALUE=dy

		; Error Handling (dx)
		if dx ge (boxsiz/2) and event.id ne but_canc then begin
			error = DIALOG_MESSAGE('dx has to be < '+strcompress(string(boxsiz/2)),$
			DIALOG_PARENT = main, TITLE='Error',/error)
			goto,jump1
		endif

		; Error Handling (dy)
		if dy ge (boxsiz/2) and event.id ne but_canc then begin
			error = DIALOG_MESSAGE('dy has to be < '+strcompress(string(boxsiz/2)),$
			DIALOG_PARENT = main, TITLE='Error',/error)
			goto,jump1
		endif
		dist=0
	endif else begin
		widget_control,field5,get_value=dist
		if dist le 0 or dist ge (boxsiz/2) and event.id ne but_canc then begin
			error = DIALOG_MESSAGE('The displacement has to be < '+strcompress(string(boxsiz/2)),$
			DIALOG_PARENT = main, TITLE='Error',/error)
			goto,jump1
		endif
	endelse

	widget_control,main,/destroy

	if event.id ne but_ok then return		; OK button _not_ clicked

; change mousepointer

WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

;------------------------------------------------------------------------------
	progress,Message='Scanning amplitude...',/cancel_button

	mean_value = 0.0

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type
	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

	for i=0,anz_blocks-1 do begin   ; normal blocks
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
		readu,ddd,block
		mean_value += mean(block) * float(blocksizes[i]) / file.ydim
	endfor
	free_lun,ddd

;------------------------------------------------------------------------------

head = 1l
rrat,file.name,ddd,header=head,info=info,type=type
srat,outputfile,eee,header=[3l,11l,file.xdim,file.ydim,4l],info=infostr,type=404l

; calculating preview size and number of blocks

bs = config.blocksize
overlap = (boxsiz + 1) / 2
calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last
blocksizes = intarr(anz_blocks)+bs
blocksizes[anz_blocks-1] = bs_last

ypos1 = 0                       ; block start
ypos2 = bs - overlap            ; block end

byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos

; pop up progress window

progress,Message='Computing Texture Features ...',/cancel_button


for i=0,anz_blocks-1 do begin
	progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

	block = make_array([file.xdim,blocksizes[i]],type=file.var)
	readu,ddd,block


	; -------- Main Programm----------
	mat = float(bytscl(block,0,config.sar_scale*mean_value)) *(gwa/256.0)

	dimb=size(mat)
	ParameterMatrix=Make_array(11,dimb[1]*dimb[2],/float,value=0.0)	;2D array

	delta = (boxsiz-1)/2
	fbox=mbox(boxsiz,dimb[1])

	for a=delta,dimb[1]-delta-1 do begin
		for b=delta,dimb[2]-delta-1 do begin

			pos  = a+b*dimb[1]
			box  = mat[pos+fbox]

			mat_co=glcm(reform(box,boxsiz,boxsiz),dx,dy,gwa,ALL=all,SYM=sym,DIST=dist)
			fvector=p_glcm(mat_co,/asm,/contr,/entro,/idm,/corr,/diss,/maxp,/mean,/var,/cl_shade,/cl_prom)

			ParameterMatrix[*,pos]=fvector

		endfor
	endfor
	NaN_ind = where(finite(ParameterMatrix) eq 0,anz)   ; find NaN's and replace NaN-values to 0.0
	if anz ne 0 then ParameterMatrix[NaN_ind]=0.0


	featArr=reform(ParameterMatrix,11,dimb[1],dimb[2])
	featArr_sort=fltarr(11,dimb[1],dimb[2])		;3D array

	if i eq anz_blocks-1 then ypos2 = bs_last
	writeu,eee,featArr[*,*,ypos1:ypos2-1]	;save  feature layer

	ypos1 = overlap
	point_lun,-ddd,file_pos
	point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
endfor

	free_lun,ddd,eee

; update file information

	file.name =  finalfile
	file_move,outputfile,finalfile,/overwrite
	file.dim=3l
	file.zdim=11l
	file.vdim=1l
	file.var=4l
	file.type=120l

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif

end
