;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: classif_lee
; written by    : Marc Jäger
; last revision : 13.June.2004
; Classifies polarimetric images using the category preserving Wishart
; method
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


function initHist, fredurBlock, histogram, histLim, entropyLim

histRes = (size(histogram))[1:2]
histStep = histLim / (histRes[0]-1)
blockRes = (size(fredurBlock))[2:3]
outBlock = make_array(blockRes, type=2l)

for i = 0, blockRes[0]-1 do begin
    for j = 0, blockRes[1]-1 do begin
        ignore = max(fredurBlock[*,i,j], maxComponent)
        entropy = fredurBlock[maxComponent,i,j] / total(fredurblock[*,i,j])

        category = maxComponent
        fredurVal = fredurBlock[maxComponent,i,j]
        if (entropy LT entropyLim) then begin
            category = 3 + maxComponent*4
            maxComponent = 3
        endif

        outBlock[i,j] = category

        if (fredurVal GT histLim[maxComponent]) then begin
            growRatio = (fredurVal/histLim[maxComponent]) > 1.25
            newRes = long([CEIL(histRes[0] * growRatio), histRes[1]])
            histLim = ((newRes[0]-1) * histStep)
            newHist = make_array(newRes, type=3l)
            newHist[0:histRes[0]-1, 0:histRes[1]-1] = histogram[*,*]
            histogram = newHist
            histRes = newRes
        endif

        histIndex = FLOOR(fredurVal/histStep[maxComponent])
        histogram[histIndex,maxComponent] = histogram[histIndex,maxComponent]+1l
    endfor
endfor

return, outBlock
end


function initClassify, covarBlock, fredurBlock, catBlock, classLim, classMean, classSize
blockRes = (size(catBlock))[1:2]
classNum = (size(classSize))[1]
outBlock = make_array(blockRes, type=2l)

for i = 0, blockRes[0]-1 do begin
    for j = 0, blockRes[1]-1 do begin
        category = catBlock[i,j] mod 4
        
        if (category EQ 3) then begin
            fredurVal = max(fredurBlock[*,i,j])
        endif else begin
            fredurVal = fredurBlock[category,i,j]
        endelse

        class = classNum-1
        for k = 0,classNum-2 do begin
            if (classLim[k,category] GE fredurVal) then begin
                class = k
                break
            endif
        endfor

        ; RANDOM INIT!!!
        category = 1
        class = round(randomu(seed,1)*(classNum-1))

        alternCateg = 0
        if (category eq 3) then alternCateg = (catBlock[i,j]/4) mod 4

        outBlock[i,j] = category + alternCateg*4 + class*32
        classMean[*,*,class,category] += covarBlock[*,*,i,j]
        classSize[class, category] += 1
    endfor
endfor

return, outBlock
end


function calcClassDist, class1, class2, category, classSize, classMean
mat1 = classMean[*,*,class1,category]/classSize[class1,category]
mat2 = classMean[*,*,class2,category]/classSize[class2,category]

ignore = 2

invMat1 = la_invert(mat1, status=ignore)
invMat2 = la_invert(mat2, status=ignore)
det1 = abs(la_determ(mat1,/check))
det2 = abs(la_determ(mat2,/check))
;la_svd, mat1, sVal1, null1, null2
;la_svd, mat2, sVal2, null1, null2

dist = alog(det1)+alog(det2)+trace(invMat1##mat2+invMat2##mat1)
;dist = alog(sVal1[0])+alog(sVal2[0])+trace(invMat1##mat2+invMat2##mat1)

return, dist
end

function calcMatClassDist, mat1, classIndex, category, classSize, classMean
mat2 = classMean[*,*,classIndex,category]/classSize[classIndex,category]

ignore = 2
invMat2 = la_invert(mat2, status=ignore)
det2 = abs(la_determ(mat2,/check))
dist = alog(det2) + trace(invMat2##mat1)
return, dist
end


pro doWishartIter, classBlock, covarBlock, classMean, classSize, newMean, newSize, FORCEASSIGN=reassign
blockRes = (size(classBlock))[1:2]
covarSize = (size(covarBlock))[1]
covarOffset = covarSize^2
classNum = (size(classSize))[1]


invMean = classMean
for i=0,3 do for j=0,classNum-1 do begin
    invMean[*,*,j,i] /= classSize[j,i]
end
lnClassDet = float(reform(alog(block_det(invMean))))
invMean = block_inv(invMean)

categBlock = classBlock mod 4
altCategBlock = (classBlock / 4) mod 4 - (categBlock ne 3)
classBlock[*,*] = 0
minDistBlock = fltarr(blockRes) + !values.f_infinity

for i=0,3 do begin
    if (i eq 3 and keyword_set(reassign)) then break

    pixInd = where((categBlock eq i)  + (altCategBlock eq i), nr)
    if (nr eq 0) then continue

    for j=0,classNum-1 do begin
        if (classSize[j,i] eq 0) then continue
        
        distBlock = lnClassDet[j,i] + block_trace(real_part(block_mm(reform(invMean[*,*,j,i]),covarBlock)))
        updateInd = where(distBlock[pixInd] lt minDistBlock[pixInd], nr)
        if (nr gt 0) then begin
            updateInd = pixInd[updateInd]
            minDistBlock[updateInd] = distBlock[updateInd]
            classBlock[updateInd] = i + 32*j
            updateInd *= covarOffset
        end
    end
end

for i=0,3 do for j=0,classNum do begin
    classInd = covarOffset * long(where(classBlock eq (j*32 + i), nr))
    if (nr le 0) then continue

    classOffset = j*covarOffset + i*covarOffset*classNum
    for k=0,covarOffset-1 do begin
        newMean[k+classOffset] += total(covarBlock[k+classInd])
    end
    newSize[j,i] += n_elements(classInd)
end    

uncertainInd = where((classBlock mod 4) eq 3, nr)
if (nr gt 0) then begin
    classBlock[uncertainInd] += 4*altCategBlock[uncertainInd]
end

end





pro classMerging, classMean, classSize, classDist, finalClassNum, pixNum, classFd, file, config

classNum = (size(classSize))[1]
mergeList = prepareMerge(classMean, classSize, classDist, finalClassNum, pixNum)
maxClassNum = max(total(((-mergeList < 1) > 0)*(classSize < 1), 1))
newMean = make_array([3,3,maxClassNum, 4], type=6l)
newSize = make_array([maxClassNum, 4], type=3l)
renumList = make_array([classNum,4], type=3l) - 1
categSize = make_array([4], type=3l)

for i = 0,3 do begin
    classCount = 0
    for j = 0, classNum-1 do begin
        if (mergeList[j, i] GE 0) then continue
        if (classSize[j, i] EQ 0) then continue
        
        newMean[*,*,classCount,i] = classMean[*,*,j,i]
        newSize[classCount,i] = classSize[j,i]
        
        renumList[j,i] = classCount
        classCount = classCount+1
    endfor
    categSize[i] = classCount
    
    for j = 0, classNum-1 do begin
        if (mergeList[j,i] LT 0) then begin
            mergeList[j,i] = renumList[j,i]*32 + i
        endif else begin
            mergeList[j,i] = renumList[mergeList[j,i],i]*32 + i
        endelse
    endfor
endfor

classMean = newMean
classSize = newSize

bs = config.blocksize
calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
blocksizes = intarr(anz_blocks)+bs
blocksizes[anz_blocks-1] = bs_last

lastOffset = 0
point_lun, classFd, lastOffset
progress,Message='Merging classes...',/cancel_button
for i = 0, anz_blocks-1 do begin
    progress,percent=(i+1)*100.0/anz_blocks
    
    point_lun, -classFd, lastOffset
    classBlock = make_array([file.xdim,blocksizes[i]], type=2l)
    readu, classFd, classBlock
    mergeClasses, classBlock, mergeList
    point_lun, -classFd, nextOffset
    point_lun, classFd, lastOffset
    writeu, classFd, classBlock
    point_lun, classFd, nextOffset
endfor
end



function prepareMerge, classMean, classSize, classDist, finalClassNum, totalPixNum

classNum = (size(classSize))[1]
classDist = make_array([classNum,classNum, 4], type=4l)
merges = make_array([classNum, 4], type=3l) - 1

for k = 0,3 do begin
    for i = 1,classNum-1 do begin
        for j = 0, i-1 do begin
            classDist[i,j,k] = calcClassDist(i,j,k,classSize,classMean)
        endfor
    endfor
endfor


maxDist = max(classDist)
for i = 0,classNum-1 do begin
    for j = i,classNum-1 do begin
        classDist[i,j,*] = maxDist*2
    endfor
endfor

categSize = total(classSize<1, 1)

classSizeLim = 2. * (totalPixNum/finalClassNum)

while (total(categSize[0:2]) GT finalClassNum) do begin
    mergeClassSize = classSizeLim*2

    while (mergeClassSize GT classSizeLim) do begin
        ignore = min(classDist, mergeIndex)
        
        mergeCoord = [mergeIndex MOD classNum, mergeIndex / classNum, 0l]
        mergeCoord[2] = mergeCoord[1] / classNum
        mergeCoord[1] = mergeCoord[1] MOD classNum
        mergeClassSize = classSize[mergeCoord[0],mergeCoord[2]]+classSize[mergeCoord[1],mergeCoord[2]]

        if (mergeClassSize GT classSizeLim) then begin
            classDist[mergeCoord[0],mergeCoord[1],mergeCoord[2]] = maxDist*2
        endif
    endwhile

    categSize[mergeCoord[2]] = categSize[mergeCoord[2]] - 1l

    if (merges[mergeCoord[0],mergeCoord[2]] GE 0 OR merges[mergeCoord[1],mergeCoord[2]] GE 0) then begin
        ;print, "Error!", classDist[mergeCoord[0],mergeCoord[1],mergeCoord[2]], maxDist*2
    endif

    classMean[*,*,mergeCoord[0],mergeCoord[2]] = classMean[*,*,mergeCoord[1], mergeCoord[2]] + classMean[*,*,mergeCoord[0],mergeCoord[2]]
    classSize[mergeCoord[0],mergeCoord[2]] = classSize[mergeCoord[0],mergeCoord[2]] + classSize[mergeCoord[1],mergeCoord[2]]

    classDist[mergeCoord[1],*,mergeCoord[2]] = maxDist*2
    
    for j = 0,classNum-1 do begin
        if (merges[j,mergeCoord[2]] EQ mergeCoord[1] OR j EQ mergeCoord[1]) then begin
            merges[j,mergeCoord[2]] = mergeCoord[0]
        endif
        
        if (j GT mergeCoord[1]) then begin
            classDist[j, mergeCoord[1], mergeCoord[2]] = maxDist*2
        endif

        if (merges[j,mergeCoord[2]] GE 0) then continue

        if (j GT mergeCoord[0]) then begin
            classDist[j,mergeCoord[0],mergeCoord[2]] = calcClassDist(j,mergeCoord[0],mergeCoord[2],classSize,classMean)
        endif
    endfor

    for j = 0,mergeCoord[0]-1 do begin
        if (merges[j,mergeCoord[2]] LT 0) then begin
            classDist[mergeCoord[0],j,mergeCoord[2]] = calcClassDist(mergeCoord[0],j,mergeCoord[2],classSize,classMean)
        endif
    endfor
endwhile

return, merges
end



pro mergeClasses, classBlock, mergeList

blockRes = (size(classBlock))[1:2]

for i = 0,blockRes[0]-1 do begin
    for j = 0,blockRes[1]-1 do begin
        category = classBlock[i,j] MOD 4
        class = classBlock[i,j] / 32
        
        if (mergeList[class,category] GE 0) then begin
            altCateg = 0
            if (category eq 3) then altCateg = (classBlock[i,j] / 4) mod 4
            classBlock[i,j] = mergeList[class,category] + altCateg*4
        endif
    endfor
endfor
end





pro classif_lee,CALLED=called,SMM=smm
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag, palettes, pnames

; check if array is usable

         if file.type ne 220 then begin
             error_button = DIALOG_MESSAGE(['The input needs to be the covariance matrix.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
             return
         endif


 if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=9,TITLE='Lee Category Preserving Classification',/floating,/tlb_kill_request_events,/tlb_frame_attr)

     decomp = CW_BGROUP(main,[' Use Freeman Durden ',' Use Moriyama '], set_value=0,column=3,/exclusive,/frame)

     line2 = WIDGET_BASE(main,column=3)
     field2 = CW_FIELD(line2,VALUE=0.6,/floating,TITLE='Moriyama threshold:                 ',XSIZE=4)
     WIDGET_CONTROL, field2, sensitive=0

     line3 = WIDGET_BASE(main,column=3)
     field3 = CW_FIELD(line3,VALUE=0.5,/floating,TITLE='Threshold for "uncertain" category: ',XSIZE=4)

     line4 = WIDGET_BASE(main, column=3)
     lineSep = STRING(10B)
     if (config.os eq 'windows') then lineSep = STRING(13B)+lineSep
     field4 = CW_BGROUP(line4, ['Force assignment of'+lineSep+'"uncertain" pixels in last iter.'],set_value=[1], /nonexclusive)

     line4b = WIDGET_BASE(main, column=3)
     field4b = CW_BGROUP(line4b, ['Merge Classes after Iteration (slow but better)'],set_value=[0], /nonexclusive)

     line5 = WIDGET_BASE(main,column=3)
     field5   = CW_FIELD(line5,VALUE=16,/integer,TITLE='Desired number of classes:          ',XSIZE=4)


     line6 = WIDGET_BASE(main,column=3)
     field6   = CW_FIELD(line6,VALUE=15,/integer,TITLE='Internal class numb. per categ.     ',XSIZE=4)

     line7 = WIDGET_BASE(main,column=3)
     field7   = CW_FIELD(line7,VALUE=4,/integer,TITLE='Number of iterations:               ',XSIZE=4)

     buttons  = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
    
    
     repeat begin
         event = widget_event(main)
         if event.id eq but_info then begin ; Info Button clicked
             infotext = ['LEE CATEGORY PRESERVING CLASSIFICATION',$
                         ' ',$
                         'For more information, see J.S. Lee, M. R. Grunes, E. Pottier, and L. Ferro-Famil,',$
                         'Unsupervised Terrain Classification Preserving Polarimetric Scattering Characteristics,',$
                         'IEEE Trans. Geoscience and Remote Sensing, April 2004.',$
                         ' ',$
                         'RAT module written 07/2004 by Marc Jaeger']
             info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
         end

         if event.id eq decomp then begin ; Choice of Decomposition changed
             widget_control, decomp, GET_VALUE=choice
             widget_control, field2, SENSITIVE=choice
         end
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,decomp,GET_VALUE=useMoriyama
     widget_control,field2,GET_VALUE=moriyamaThresh
     widget_control,field3,GET_VALUE=entropyLim
     widget_control,field4,GET_VALUE=forceAssign
     widget_control,field4b,GET_VALUE=mergeAfter
     widget_control,field5,GET_VALUE=finalClassNum
     widget_control,field6,GET_VALUE=classNum
     widget_control,field7,GET_VALUE=maxIter
     widget_control,main,/destroy
     if event.id ne but_ok then return ; OK button _not_ clicked
 endif else begin                ; Routine called with keywords
     useMoriyama = 0
     entropyLim = 0.5
     finalClassNum = 16
     classNum = 15
     maxIter = 4
     forceAssign = 1
 endelse

 ; go

 	WIDGET_CONTROL,/hourglass

 ; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
       
 ; get the Freeman Durden decomposition
         backupFile = file
         fredurFile = outputFile + ".fredur1"
         oldWorkFile = config.workfile1
         config.workfile1 = config.workfile2 + ".fredur1"

         if (useMoriyama) then begin
             decomp_moriyama, CORTHRESH=moriyamaThresh, /CALLED
         endif else begin
             decomp_fredur, /CALLED
         endelse

         config.workfile1 = oldWorkFile
         file = backupFile

 ; read / write header

 	head = 1l
         rrat,fredurFile,fredurFd,header=head
         point_lun, -fredurFd, fredurDataOffset

 	rrat,file.name,ddd,header=head,info=info,type=type
         point_lun, -ddd, covarDataOffset
        
 	srat,outputfile,eee,header=[2l,file.xdim,file.ydim,2l],info=info,type=409l

         classInitFile = outputfile + ".init1"
         get_lun, classFd
         openw, classFd, classInitFile
        

 ; calculating preview size and number of blocks

         bs = config.blocksize
         calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
         blocksizes = intarr(anz_blocks)+bs
         blocksizes[anz_blocks-1] = bs_last

 ; pop up progress window

 	progress,Message='Lee Category Preserving Classification',/cancel_button

 ; classify
        
         for i = 0, anz_blocks-1 do begin
             progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
 		if wid.cancel eq 1 then return


             if (useMoriyama) then begin
                 fredurBlock = make_array([4, file.xdim, blockSizes[i]], type=4l)
             endif else begin
                 fredurBlock = make_array([3, file.xdim, blockSizes[i]], type=4l)
             endelse

             readu, fredurFd, fredurBlock
             fredurBlock = alog(fredurBlock[0:2,*,*] + exp(1.)) - 1. > 0

             if (i EQ 0) then begin
                 histogram = make_array([100,4], type=3l)
                 histLim = [0.,0.,0.,0.]
                 for j = 0,2 do begin
                     histLim[j] = mean(fredurBlock[j, *, *])
                 endfor
                 histLim[3] = mean(histLim)
             endif
            
             cInitBlock = initHist(fredurBlock, histogram, histLim, entropyLim)

             writeu, classFd, cInitBlock
         endfor

         classLim = make_array([classNum-1,4], type=4l)
         classMean = make_array([file.vdim, file.zdim, classNum, 4], type=6l)
         classSize = make_array([classNum,4], type=3l)
         histLen = (size(histogram))[1]
         for i = 0,3 do begin
             pixTotal = total(histogram[*,i])

             if (pixTotal EQ 0) then continue

             classPix = pixTotal/classNum
             histStep = histLim[i]/(histLen-1)

             currentPixNum = 0l
             currentClass = 0l
             currentBin = 0l
             while (currentClass LE classNum-2) do begin
                 if (currentPixNum GT classPix) then begin
                     currentPixNum = currentPixNum-classPix
                     histWeight = 1.-float(currentPixNum)/histogram[currentBin,i]
                     classLim[currentClass,i] = (currentBin+histWeight)*histStep
                     currentClass = currentClass + 1l
                 endif else begin
                     currentPixNum = currentPixNum + histogram[currentBin,i]
                     currentBin = currentBin + 1l
                 endelse
             endwhile
         endfor
         classLim = exp(classLim+1.) - exp(1.)
        
         lastOffset = 0
         point_lun, classFd, lastOffset
         point_lun, fredurFd, fredurDataOffset
         for i = 0, anz_blocks-1 do begin
             progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
 		if wid.cancel eq 1 then return

             point_lun, -classFd, lastOffset
             catBlock = make_array([file.xdim,blocksizes[i]], type=2l)
             readu, classFd, catBlock

             if (useMoriyama) then begin
                 fredurBlock = make_array([4, file.xdim, blockSizes[i]], type=4l)
             endif else begin
                 fredurBlock = make_array([3, file.xdim, blockSizes[i]], type=4l)
             endelse
             readu, fredurFd, fredurBlock
             fredurBlock = fredurBlock[0:2,*,*]
            
             covarBlock = make_array([file.vdim,file.zdim,file.xdim,blockSizes[i]], type=6l)
             readu, ddd, covarBlock

             classBlock = initClassify(covarBlock, fredurBlock, catBlock, classLim, classMean, classSize)

             point_lun, -classFd, nextOffset
             point_lun, classFd, lastOffset
             writeu, classFd, classBlock
             point_lun, classFd, nextOffset

         endfor


         pixNum = (file.xdim*file.ydim)
         if (mergeAfter le 0) then begin
             classMerging, classMean, classSize, classDist, finalClassNum, pixNum, classFd, file, config
         end        

         point_lun, fredurFd, 0
         classNum = (size(classSize))[1]
         for itr=0,maxIter-1 do begin
             progress,Message='Wishart Iteration #' + STRING(itr+1),/cancel_button
             lastOffset = 0
             point_lun, classFd, 0
             point_lun, ddd, covarDataOffset

             newMean = fltarr(file.vdim,file.zdim,classNum, 4)
             newSize = fltarr(classNum, 4)

             for i = 0, anz_blocks-1 do begin
                 progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
 		if wid.cancel eq 1 then return

                 classBlock = make_array([file.xdim, blockSizes[i]], type=2l)
                 readu, classFd, classBlock
                 point_lun, -classFd, nextOffset
                
                 covarBlock = make_array([3,3,file.xdim,blockSizes[i]], type=6l)
                 readu, ddd, covarBlock
                
                 if (itr eq maxIter-1 and forceAssign eq 1) then begin
                     doWishartIter, classBlock, covarBlock, classMean, classSize, newMean, newSize, /FORCEASSIGN
                 endif else begin
                     doWishartIter, classBlock, covarBlock, classMean, classSize, newMean, newSize
                 endelse
                
                 point_lun, classFd, lastOffset
                 writeu, classFd, classBlock
                 lastOffset = nextOffset
             endfor
             
             classMean = newMean
             classSize = newSize

         endfor

         if (mergeAfter ne 0) then begin
             classMerging, classMean, classSize, classDist, finalClassNum, pixNum, classFd, file, config
         end
        
         maxClassNum = (size(classSize))[1]
         renumList = make_array([maxClassNum,4], type=3l)
         for i = 0,3 do begin
             ignore = where(classSize[*,i] eq 0, emptyCount)
             renumList[sort(classSize[*,i]),i] = (indgen(maxClassNum) - emptyCount)>0
         end
        

         histLen = maxClassNum*4 + 3
         h = intarr(histLen)
        

         point_lun, classFd, 0
         progress,Message='Finalising classification',/cancel_button
         for i = 0,anz_blocks-1 do begin
             progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
 		if wid.cancel eq 1 then return

            
             classBlock = make_array([file.xdim, blockSizes[i]], type=2l)
             readu, classFd, classBlock
             categBlock = classBlock MOD 4
             for j = 0,3 do begin
                 categInd = where(categBlock eq j, nr)
                 if (nr eq 0) then continue
                 classBlock[categInd] = renumList[classBlock[categInd]/32,j]*4+j

                 h += histogram(classBlock[categInd], min=0, max=histLen-1)

             end

             writeu, eee, classBlock
         end
        
         free_lun,ddd,eee,fredurFd,classFd
	
 ; update file information

 	file_move,outputfile,finalfile,/overwrite
        file_delete,classInitFile,fredurFile
 	file.name = finalfile
 	file.type = 409l
 	file.dim  = 2l
 	file.zdim = 1l
 	file.vdim = 1l
 	file.var  = 2l

        
; calculate color table
        baseHSV = [[360,100,100],[137,87,60],[40,80,89],[0,0,0]]
        satMin = [40,15,30,100]
        valMin = [70,30,30,0]


        for j=0,2 do begin
            palettes[0,*,j] = 0
            palettes[1,*,j] = 0
        end

        for i=0,3 do begin
            ignore = where(classSize[*,i] ne 0, classNum)
            if (classNum le 0) then continue

            classTab = bytarr(256,3)
            hsv, valMin[i],baseHSV[2,i],baseHSV[1,i],satMin[i],baseHSV[0,i],0,classTab

            palIndex = 4*indgen(classNum)+i

            tabStep = 256.0/((classNum-1)>0)
            for j=0,2 do begin
                colIndex = round(findgen(classNum)*tabStep)
                palettes[0,palIndex,j] = classTab[colIndex,j]
                palettes[1,palIndex,j] = classTab[colIndex,j]
            end
        end

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
