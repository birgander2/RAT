;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_wishart_em
; written by    : Marc Jäger
; last revision : June 2005
; Classifies polarimetric interferometric images using expectation
; maximisation
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


function makeNewDataDir, dataRoot

currentDirNum = 1l
dirFound = 0
while (dirFound eq 0) do begin
    dataDir = dataRoot + strtrim(string(currentDirNum,/print),2) + '/'
    if (file_test(dataDir, /directory) eq 0) then dirFound = 1
    currentDirNum += 1l
end

file_mkdir, dataDir

return, dataDir
end


pro polin_wishart_em, CALLED=called
common rat, types, file, wid, config
common channel, channel_names, channel_selec, color_flag, palettes, pnames

; check if array is usable

allowedTypes = [510,511]
if min(abs(allowedTypes - file.type)) gt 0 then begin
    error_button = DIALOG_MESSAGE(['The input needs to be a coherency/covariance matrix!'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
    return
endif


finalClassNum = 16
incrementPause = 4
lookNum = 83.48
coherPhLooks = 45.0
coherAbsLooks = -1.0
biasIterations = 20
logTabLen = 1024
coherHistLen = 1024 < logTabLen
convShockExp = 4
useFlags = [1,1,1,1]
hypGeomEPS = 0
coherFile = '/home/monk/work/data/eurad/c_poltom01x02_rsf_fe_coh_r49.rat'

dataRoot = '~/work/data-net/polin_seg/'

if not keyword_set(called) then begin ; Graphical interface
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=8,TITLE='Wishart EM Classification',/modal,/tlb_kill_request_events,/tlb_frame_attr)

     useInfo = CW_BGROUP(main,[' Use Intensity ', ' Use Polarimetry ',' Use Abs. Coherence ', 'Use Coherence Phase '], set_value=useFlags,row=4,/nonexclusive,/frame)

    line5 = WIDGET_BASE(main,column=3)
    field5   = CW_FIELD(line5,VALUE=finalClassNum,/integer,TITLE='Final class number:          ',XSIZE=4)

    line7 = WIDGET_BASE(main,column=3)
    field7   = CW_FIELD(line7,VALUE=incrementPause,/integer,TITLE='Number of iterations per class: ',XSIZE=4)
    
    line8 = WIDGET_BASE(main,column=3)
    field8   = CW_FIELD(line8,VALUE=lookNum,/float,TITLE='Number of looks: ',XSIZE=7)

    coherSource = CW_BGROUP(main,[' Coherence from Covariance ',' Coherence from File '], set_value=0,column=3,/exclusive,/frame)
    
    line9 = WIDGET_BASE(main,column=3)
    text9 = CW_FIELD(line9,VALUE=coherFile,/string,XSIZE=60,TITLE='Complex Coherence RAT File :')
    brow9 = WIDGET_BUTTON(line9,VALUE=' Browse ',ysize=35)
    WIDGET_CONTROL, text9, sensitive=0
    WIDGET_CONTROL, brow9, sensitive=0

    line10 = WIDGET_BASE(main,column=3)
    field10   = CW_FIELD(line10,VALUE=coherPhLooks,/float,TITLE='# of samples in coherence Ph comp.: ',XSIZE=7)
    WIDGET_CONTROL, field10, sensitive=0

    line11 = WIDGET_BASE(main,column=3)
    field11   = CW_FIELD(line11,VALUE=coherAbsLooks,/float,TITLE='# of samples in coherence Abs comp.: ',XSIZE=7)
    WIDGET_CONTROL, field11, sensitive=0


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
            infotext = ['POLINSAR EXPECTATION MAXIMISATION',$
                        ' ',$
                        'RAT module written 06/2005 by Marc Jaeger']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end

        if event.id eq brow9 then begin
            path = config.workdir
            coherFile = DIALOG_PICKFILE(TITLE='Open Coherence File',DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST,PATH=path, GET_PATH=path)
            if strlen(coherFile) gt 0 then config.workdir = path
            widget_control,text9,set_value=coherFile
        endif

        if event.id eq coherSource then begin ; Choice of Decomposition changed
            widget_control, coherSource, GET_VALUE=choice
            widget_control, text9, SENSITIVE=choice
            widget_control, brow9, SENSITIVE=choice
            widget_control, field10, SENSITIVE=choice
            widget_control, field11, SENSITIVE=choice
        end
    endrep until (event.id eq but_ok) or (event.id eq but_canc)or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')

    widget_control,field5,GET_VALUE=finalClassNum
    widget_control,field7,GET_VALUE=incrementPause
    widget_control,coherSource,GET_VALUE=loadCoherVal
    widget_control,field8,GET_VALUE=lookNum
    widget_control,text9,GET_VALUE=coherFile
    widget_control,field10,GET_VALUE=coherPhLooks
    widget_control,field11,GET_VALUE=coherAbsLooks
    widget_control,useInfo,GET_VALUE=useFlags
    widget_control,main,/destroy
    if event.id ne  but_ok then return ; OK button _not_ clicked
endif

; go

WIDGET_CONTROL,/hourglass

head = 1l

rrat,file.name,covarFd,header=head,info=info,type=type
point_lun, -covarFd, covarDataOffset

covarSize = file.vdim
polCovarSize = round(covarSize/2)

if (loadCoherVal) then begin
    rrat,coherFile,coherFd,header=coherHead,info=info,type=type
    coherDim = coherHead[0]-2
    coherSize = coherHead[1:coherDim]

    point_lun, -coherFd, coherDataOffset
    
    if (type ne 531) then begin
        error = DIALOG_MESSAGE("The file supplied does not contain complex coherences!", DIALOG_PARENT = wid.base, TITLE='Error',/error)
        free_lun, coherFd, coherFd
        return
    end
    
    if (max(abs(coherHead[coherDim+1:coherDim+2]-[file.xdim,file.ydim])) gt 0) then begin
        error = DIALOG_MESSAGE("The file supplied does not have the same resolution as the covariance matrices!", DIALOG_PARENT = wid.base, TITLE='Error',/error)
        free_lun, covarFd, coherFd
        return
    end

    coherChannels = min(coherSize)

    coherPhLooks *= lookNum
    coherAbsLooks *= lookNum
    if (coherAbsLooks lt 0) then coherAbsLooks = coherPhLooks
end else begin
    coherChannels = polCovarSize
    coherPhLooks = lookNum
    coherAbsLooks = lookNum
end

outputfile  = config.tempdir+config.workfile2
finalfile   = config.tempdir+config.workfile1

                                ; undo function

if not keyword_set(called) then begin
    if file.name eq finalfile then begin   
        file_copy,file.name,config.tempdir+config.workfile3,/overwrite
        config.undofile = config.tempdir+config.workfile3
    endif else config.undofile = file.name
endif

srat,outputfile,outFd,header=[2l,file.xdim,file.ydim,2l],info=info,type=411l

bs = config.blocksize
calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
blocksizes = intarr(anz_blocks)+bs
blocksizes[anz_blocks-1] = bs_last

classCovar = reform(complexarr(polCovarSize*[1,1]), [polCovarSize,polCovarSize,1])
classCovar[*,*,0] = diag_matrix(fltarr(polCovarSize)+1)
classCoher = reform(complexarr(coherChannels),[coherChannels,1]) + 0.5

debugClassif = intarr(file.xdim, file.ydim)
debugLogE = fltarr(file.xdim, file.ydim)
debugRes = 200
debugSize = round([file.xdim,file.ydim]*debugRes/float(max([file.xdim,file.ydim])))

dataDir = makeNewDataDir(dataRoot)

for i=0,finalClassNum-1 do begin
    classStrength = fltarr(i+1)
    maxLogE = fltarr(i+1) - !values.f_infinity
    maxCovar = complexarr(polCovarSize,polCovarSize,i+1)
    maxCoher = complexarr(coherChannels,i+1)

    iterNum = (i eq 0) ? 1 : incrementPause
    shockIter = floor(iterNum/2) + (i eq 0)
    for j=0,iterNum-1 do begin
        point_lun, covarFd, covarDataOffset
        if (loadCoherVal) then point_lun, coherFd, coherDataOffset

        newClassCovar = complexarr(polCovarSize,polCovarSize,i+1)
        newClassCoherAbs = complexarr(coherChannels,i+1)
        newClassCoherPh = complexarr(coherChannels,i+1)
        newClassWeight = fltarr(i+1) 

        progress,Message='Tabulating Hypergeometric Functions',/cancel_button
        classLogTables = makeClassLogTables(classCoher,[coherAbsLooks,coherPhLooks],logTabLen,hypGeomEPS,useFlags)
        
        coherHist = lonarr(coherHistLen,coherChannels,2)

        classHist = fltarr(i+1)
        
        progress,Message='EM  Iteration ' + string(i+1) + string(j+1),/cancel_button

        debugOffset = 0

        for k=0,anz_blocks-1 do begin
            progress,percent=(k+1)*100.0/anz_blocks,/check_cancel
            if wid.cancel eq 1 then return
            
            covarBlock = make_array([file.vdim,file.zdim,file.xdim,blockSizes[k]], type=6l)
            readu, covarFd, covarBlock

            if (loadCoherVal) then begin
                coherBlock = make_array([coherSize,file.xdim,blockSizes[k]], type=6l)
                readu, coherFd, coherBlock

                if (coherDim gt 1) then coherBlock = block_diag(coherBlock)

            end else begin
                coherBlock = block_diag(covarBlock[polCovarSize:covarSize-1,0:polCovarSize-1,*,*])
                for m=0,polCovarSize-1 do begin
                    coherBlock[m,*,*] /= sqrt(float(covarBlock[m,m,*,*])*float(covarBlock[m+polCovarSize,m+polCovarSize,*,*]))
                end

                infInd = where(finite(coherBlock) eq 0, nr)
                if (nr gt 0) then coherBlock[infInd] = 0.0
            end

            covarBlock = reform(covarBlock[0:polCovarSize-1,0:polCovarSize-1,*,*])
            if (useFlags[0] ne 1) then begin
                intenBlock = total(reform(block_diag(covarBlock)),1)
                for i=0,polCovarSize-1 do for j=0,polCovarSize-1 do begin
                    covarBlock[i,j,*,*] /= intenBlock
                end
            end

            logEBlock = eStep(covarBlock, coherBlock, classCovar, classLogTables, lookNum, coherHist, useFlags)

            ignore = max(logEBlock,classification,dimension=1)
            classification = classification mod (i+1)

            if (j eq iterNum-1) then begin
                for m=0,i do begin
                    classInd = where(classification eq m, nr)
                    if (nr eq 0) then continue
                    classEBlock = logEBlock[m,*,*]
                    classStrength[m] += total(classEBlock[classInd])

                    maxE = max(logEBlock[m,*,*], maxInd)
                    if (maxE gt maxLogE[m]) then begin
                        maxLogE[m] = maxE

                        for n=0,polCovarSize^2-1 do begin
                            maxCovar[n+polCovarSize^2*m] = covarBlock[n+polCovarSize^2*long(maxInd)]
                        end
                        
                        for n=0,coherChannels-1 do begin
                            maxCoher[n+coherChannels*m] = coherBlock[n+coherChannels*long(maxInd)]
                        end
                    end
                end
            end

            shockExp = ((j eq shockIter)*convShockExp)
            mStep, covarBlock, coherBlock, logEBlock, newClassCovar, newClassCoherAbs, newClassCoherPh, newClassWeight, CONVSHOCK=shockExp

            debugLogE[*,debugOffset:blockSizes[k]+debugOffset-1] = logEBlock[i,*,*]

            debugClassif[*,debugOffset:blockSizes[k]+debugOffset-1] = classification
            debugOffset += blockSizes[k]

            tek_color
            window, 4, xsize=debugSize[0], ysize=debugSize[1]
            tv, reform(congrid(debugClassif, debugSize[0], debugSize[1], 0))
            loadct, 0
            ; tva, debugLogE , win=6

            classHist += histogram(classification,min=0,max=i,nbins=i+1)
            
        end
        
        maxAbsCoher = abs(maxCoher)

        shiftList = intarr((size(maxCoher))[0])
        shiftList[0] = 1
        maxCoherPhDiff = maxCoher*conj(shift(maxCoher,shiftList))

        maxCoher = maxAbsCoher * maxCoherPhDiff / abs(maxCoherPhDiff)

        plotClassTables, classLogTables, coherHist

;        print, classHist/total(classHist)
        
        emptyInd = where(newClassWeight le 0.0, nr)
        for k=0,nr-1 do begin
            classInd = emptyInd[k]
            newClassCovar[*,*,classInd] = maxCovar[*,*,classInd]
            newClassCoherAbs[*,classInd] = abs(maxCoher[*,classInd])
            newClassCoherPh[*,classInd] = exp(complex(0,atan(maxCoher[*,classInd],/phase)))
            newClassWeight[classInd] = 1.0
            classHist[classInd] = 1.0
            classStrength[classInd] = !values.f_infinity
        end

        for k=0,i do begin
            newClassCovar[*,*,k] /= newClassWeight[k]
            print, 'covar', k
            print, newClassCovar[*,*,k]
        end
        classCovar = reform(newClassCovar, [polCovarSize,polCovarSize,i+1])

        for k=0,i do begin
            newClassCoherAbs[*,k] /= newClassWeight[k]
        end
        newClassCoherPh = exp(complex(0,atan(newClassCoherPh,/phase)))
        classCoher = reform(newClassCoherAbs*newClassCoherPh, [coherChannels,i+1])
        
        print, 'coher'
        print, abs(classCoher)

        ; classCoher = removeCoherAmpBias(classCoher, coherAbsLooks, hypGeomEPS, iteration_num=biasIterations)
        ; print, 'coher'
        ; print, abs(classCoher)
        print, 'weight'
        print, newClassWeight/total(newClassWeight)

    end
    srat, dataDir+'debugClassif'+strtrim(string(i,/print),2)+'.rat', debugClassif

    if (i ne finalClassNum-1) then begin
        classStrength /= classHist
        
        print, 'strength:', classStrength

        ignore = min(classStrength, minInd, /nan)
        newCovar = reform(maxCovar[*,*,minInd])
        newCoher = reform(maxCoher[*,minInd])
        
        newClassCovar = complexarr(polCovarSize,polCovarSize,i+2)
        newClassCovar[*,*,0:i] = classCovar
        newClassCovar[*,*,i+1] = newCovar
        classCovar = newClassCovar

        newClassCoher = complexarr(coherChannels,i+2)
        newClassCoher[*,0:i] = classCoher
        newClassCoher[*,i+1] = newCoher
        classCoher = newClassCoher
    end

end

; load = 1
; if (load ne 0) then rrat, '~/work/data/tmp/classCovar.rat', classCovar
; if (load eq 0) then srat, '~/work/data/tmp/classCovar.rat', classCovar

finalClassNum = (size(classCovar))[3]
classWeight = fltarr(finalClassNum)

progress,Message='Finalising Classification',/cancel_button

classHist = fltarr(finalClassNum)
point_lun, covarFd, covarDataOffset
if (loadCoherVal) then point_lun, coherFd, coherDataOffset
for i=0,anz_blocks-1 do begin
    progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
    if wid.cancel eq 1 then return

    covarBlock = make_array([file.vdim,file.zdim,file.xdim,blockSizes[i]], type=6l)
    readu, covarFd, covarBlock
    
    if (loadCoherVal) then begin
        coherBlock = make_array([coherChannels,file.xdim,blockSizes[i]], type=6l)
        readu, coherFd, coherBlock
    end else begin
        coherBlock = block_diag(covarBlock[polCovarSize:covarSize-1,0:polCovarSize-1,*,*])
        for m=0,polCovarSize-1 do begin
            coherBlock[m,*,*] /= sqrt(covarBlock[m,m,*,*]*covarBlock[m+polCovarSize,m+polCovarSize,*,*])
        end
        infInd = where(finite(coherBlock) eq 0, nr)
        if (nr gt 0) then coherBlock[infInd] = 0.0
    end
    
    covarBlock = reform(covarBlock[0:polCovarSize-1,0:polCovarSize-1,*,*])

    logEBlock = eStep(covarBlock, coherBlock, classCovar, classLogTables, lookNum, coherHist, useFlags)
    ignore = max(logEBlock, classBlock, dimension=1)
    classBlock = classBlock mod finalClassNum

    classHist += histogram(classBlock, min=0, max=finalClassNum-1, nbins=finalClassNum)
    
    writeu, outFd, classBlock
end


; set palettes
palette = optimisePalette(classHist, classCovar)

palettes[0,*,*] = palette
palettes[1,*,*] = palette

free_lun,outFd,covarFd

                                ; update file information

file_move,outputfile,finalfile,/overwrite
file.name = finalfile
file.type = 411l
file.dim  = 2l
file.zdim = 1l
file.vdim = 1l
file.var  = 2l

if not keyword_set(called) then begin
    generate_preview
    update_info_box
endif	

end
