;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: pclass_fredur
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
function optimisePalette, classHist, classCovar

satRange = [0.4,1.0]
valRange = [0.5,1.0]

channelColor = [[1.0,0,0],[0,1.0,0],[0.5,0.4,0.1]]

classNum = n_elements(classHist)
covarSize = (size(classCovar))[1]

classOrder = sort(classHist)
classHist = classHist[classOrder]
classCovar = classCovar[*,*,classOrder]

classColors = reform(classCovar,[covarSize,covarSize,classNum,1])
decomp_fredur, block=classColors, precise=1
classColors = (classColors^4) ## channelColor
classColors *= transpose(255.0/max(classColors,dimension=1)) ## (fltarr(3)+1)

classHist = sqrt(classHist) ;alog(1.0+classHist)
classHist = classHist / total(classHist)
classWeight = fltarr(classNum)
classNorm = sqrt(total(classColors^2,1))
for i=0,classNum-1 do begin
    priorContrast = 0
    for j=0,i-1 do begin
        priorContrast += (classHist[i]-classHist[j]) * total(classColors[*,i]*classColors[*,j],1) / (classNorm[i]*classNorm[j])
    end

    postContrast = 0
    for j=i+1,classNum-1 do begin
        postContrast += (classHist[j]-classHist[i]) * total(classColors[*,i]*classColors[*,j],1) / (classNorm[i]*classNorm[j])
    end

    classWeight[i] = priorContrast / (priorContrast+postContrast)
end
weightRange = [min(classWeight), max(classWeight)]
classWeight = (classWeight - weightRange[0]) / (weightRange[1]-weightRange[0])

color_convert, classColors[0,*], classColors[1,*], classColors[2,*], h, s, v, /rgb_hsv

s[*] = (1.0-classWeight)*satRange[1] + classWeight*satRange[0]
v[*] = (1.0-classWeight)*valRange[1] + classWeight*valRange[0]

color_convert, h, s, v, r, g, b, /hsv_rgb

palette = fltarr(256,3)
palette[classOrder,0] = r
palette[classOrder,1] = g
palette[classOrder,2] = b

return, palette

end




pro pclass_fredur, COVARFILE=covarFile, CALLED=CALLED
common rat, types, file, wid, config
common channel, channel_names, channel_selec, color_flag, palettes, pnames

if file.type lt 400 or file.type ge 499 then begin
    error_button = DIALOG_MESSAGE(['The input needs to be a classification result.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
    return
endif


called = keyword_set(covarFile)
if called eq 0  then begin ; Graphical interface
    covarFile = ''
    
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Freeman Durden Palette',/floating,/tlb_kill_request_events,/tlb_frame_attr)
    
    line1 = WIDGET_BASE(main,column=3)
    text1 = CW_FIELD(line1,VALUE=covarFile,/string,XSIZE=60,TITLE='Covariance Matrix RAT File :')
    brow1 = WIDGET_BUTTON(line1,VALUE=' Browse ',ysize=35)

    buttons  = WIDGET_BASE(main,column=3,/frame)
    but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
    but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
    but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
    WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
    pos = center_box(toto[0],drawysize=toto[1])
    widget_control, main, xoffset=pos[0], yoffset=pos[1]


    repeat begin                ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin $
          infotext = ['Freeman Durden Palette',$
                      ' ',$
                      'RAT module written 2005 by Marc Jaeger']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main,TITLE='Information')
        end

        if event.id eq brow1 then begin
            path = config.workdir
            covarFile = DIALOG_PICKFILE(TITLE='Open [C]-matix File',DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST,PATH=path, GET_PATH=path)
            if strlen(covarFile) gt 0 then config.workdir = path
            widget_control,text1,set_value=covarFile
        endif

    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

    WIDGET_CONTROL, /DESTROY, main
    if event.id ne but_ok then return     
endif 

if covarFile eq '' then begin
    error = DIALOG_MESSAGE("No RAT File for Covariance Matrices given!", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
endif


WIDGET_CONTROL,/hourglass

head = 1l
rrat,file.name,classFd,header=head,info=info,type=type
rrat,covarFile,covarFd,header=head,info=info,type=type

if (type ne 220 and type ne 510) then begin
    error = DIALOG_MESSAGE("The file supplied does not contain covariance matrices!", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    free_lun, covarFd, classFd
    return
end

if (max(abs(head[3:4]-[file.xdim,file.ydim])) gt 0) then begin
    error = DIALOG_MESSAGE("The file supplied does not have the same resolution as the present classification!", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    free_lun, covarFd, classFd
    return
end


covarSize = head[1]
bs = config.blocksize
calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
blocksizes = intarr(anz_blocks)+bs
blocksizes[anz_blocks-1] = bs_last

progress,Message='Computing Class Statistics',/cancel_button

polCovarSize = (covarSize gt 4) ? floor(covarSize/2) : covarSize

classHist = 0l
classCovar = complexarr(polCovarSize,polCovarSize)
classNum = 1

for i = 0, anz_blocks-1 do begin
    progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
    if wid.cancel eq 1 then return
    
    covarBlock = make_array([covarSize,covarSize,file.xdim,blockSizes[i]], type=6l)
    readu, covarFd, covarBlock
    covarBlock = covarBlock[0:polCovarSize-1,0:polCovarSize-1,*,*]
    
    classBlock = make_array([file.vdim,file.zdim,file.xdim,blockSizes[i]], type=file.var)
    readu, classFd, classBlock

    blockHist = histogram(classBlock,min=0,binsize=1,reverse_indices=revInd)
    blockClassNum = n_elements(blockHist)

    if (blockClassNum gt classNum) then begin
        newHist = lonarr(blockClassNum)
        newHist[0:classNum-1] = classHist
        classHist = newHist

        newCovar = complexarr(polCovarSize, polCovarSize, blockClassNum)
        newCovar[*,*,0:classNum-1] = classCovar
        classCovar = newCovar

        classNum = blockClassNum
    end

    classHist[0:blockClassNum-1] += blockHist

    for j=0,blockClassNum-1 do begin
        if (revInd[j] ge revInd[j+1]) then continue
        classInd = polCovarSize^2 * long(revInd[revInd[j]:revInd[j+1]-1])

        for k=0,polCovarSize^2-1 do begin
            classCovar[k+polCovarSize^2*j] += total(covarBlock[k+classInd])
        end
    end
endfor
free_lun, covarFd, classFd
progress,/destroy

classNum -= total(classHist eq 0)
classInd = (reverse(sort(classHist)))[0:classNum-1]

smallHist = fltarr(classNum)
smallCovar = complexarr(polCovarSize,polCovarSize,classNum)
for i=0,classNum-1 do begin
    smallHist[i] = classHist[classInd[i]]
    smallCovar[*,*,i] = classCovar[*,*,classInd[i]] / smallHist[i]
end

palette = optimisePalette(smallHist, smallCovar)


for i=0,classNum-1 do begin
    for j=0,1 do palettes[j,classInd[i],*] = palette[i,*]
end
;palette_write,reform(palettes[0,*,*])

if not called then begin
    generate_preview
    update_info_box
endif

end
