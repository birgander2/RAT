;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: speck_pol_anneal
; written by    : Marc Jäger
; last revision : June 2005 (02/2007, +mb-polinsar, mn)
; Polarimetric speckle filter using simulated annealing
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


pro speck_pol_anneal, CALLED=called,IterNum=iterNum,cooling=cooling,initTemp=initTemp,lookNum=lookNum,singularCovar=singularCovar
common rat, types, file, wid, config
common channel, channel_names, channel_selec, color_flag, palettes, pnames

; check if array is usable

allowedTypes = [220,221,222,510,511,512,513]
if min(abs(file.type- allowedTypes)) ne 0 then begin
    error_button = DIALOG_MESSAGE(['The input needs to be a PolSAR/PolInSAR matrix.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
    return
endif


if n_elements(iterNum) eq 0 then iterNum = 50
if n_elements(cooling) eq 0 then cooling = 0.05
if n_elements(initTemp) eq 0 then initTemp = 1.0
if n_elements(lookNum) eq 0 then lookNum = 1.0
if n_elements(lookWinSize) eq 0 then lookWinSize = 3
if n_elements(singularCovar) eq 0 then singularCovar = 0

if not keyword_set(called) then begin ; Graphical interface
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=7,TITLE='Simulated Annealing',/floating,/tlb_kill_request_events,/tlb_frame_attr)
    
    lineSep = STRING(10B)
    if (config.os eq 'windows') then lineSep = STRING(13B)+lineSep
    note = WIDGET_LABEL(main,VALUE='NOTE: The runtime of this filter is O(age of universe^2).' + lineSep + 'You have been warned.')

    line5 = WIDGET_BASE(main,column=3)
    field5   = CW_FIELD(line5,VALUE=iterNum,/integer,TITLE='Number of iterations:          ',XSIZE=5)

    line7 = WIDGET_BASE(main,column=3)
    field7   = CW_FIELD(line7,VALUE=cooling,/float,  TITLE='Cooling constant (beta):       ',XSIZE=7)

    line8 = WIDGET_BASE(main,column=3)
    field8   = CW_FIELD(line8,VALUE=initTemp,/float, TITLE='Initial temperature:           ',XSIZE=7)

    line9 = WIDGET_BASE(main,column=3)
    field9   = CW_FIELD(line9,VALUE=lookNum,/float,  TITLE='Number of looks:               ',XSIZE=7)

    line10 = WIDGET_BASE(main,column=3)
    field10 = CW_BGROUP(line10, ['Assume singular input covariances'+lineSep+'(number of looks < 3)'],set_value=[singularCovar], /nonexclusive)
; field10   = CW_FIELD(line10,VALUE=lookWinSize,/integer,TITLE='Window size for effective # looks calc.: ',XSIZE=4)

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
            infotext = ['SPECKLE FILTER USING SIMULATED ANNEALING',$
                        ' ',$
			'Based on the filter described in:', $
			'Jesper Schou and Henning Skriever', $
			'Restoration of Polarimetric SAR Images Using Simulated Annealing', $
			'IEEE TGRS, Vol. 39 (9), September 2001', $
			' ', $
                        'RAT module written 06/2005 by Marc Jaeger']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
	
    widget_control,field5,GET_VALUE=iterNum
    widget_control,field7,GET_VALUE=cooling
    widget_control,field8,GET_VALUE=initTemp
    widget_control,field9,GET_VALUE=lookNum
    widget_control,field10,GET_VALUE=singularCovar

    widget_control,main,/destroy
    if event.id ne but_ok then return ; OK button _not_ clicked
endif

WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

head = 1l

rrat,file.name,covarFd,header=head,info=info,type=type
point_lun, -covarFd, covarDataOffset
covarSize = file.vdim

srat,outputfile,outFd,header=[4l,covarSize,covarSize,file.xdim,file.ydim,6l],info=info,type=file.type

energyFile = outputfile + ".energy"
get_lun, energyFd
openw, energyFd, energyFile

estimFile = outputfile + ".estim"
get_lun, estimFd
openw, estimFd, estimFile

bs = config.blocksize
overlap = 1
calc_blocks_overlap,file.ydim,bs,overlap,blockNum,bs_last 
blocksizes = intarr(blockNum)+bs
blocksizes[blockNum-1] = bs_last

byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos
covarOverlap = 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]

cliqueConfig = calcCliqueConfig()
cliqueNum = (size(cliqueConfig))[3]

progress,message='Iteration',submessage='Progress',/cancel_button

for i=0,iterNum do begin
    progress,percent=(100.0*i)/iterNum,message='Iteration'+string(i), submessage='Progress', subpercent=0.0, /check_cancel
    if wid.cancel eq 1 then return

    if (i gt 0) then begin
        temperature = initTemp / alog(1.0 + cooling*(i))

        ; print, 'current Temp:'
        ; print, temperature, initTemp
    end else temperature = 0.0

    point_lun, covarFd, covarDataOffset
    point_lun, estimFd, 0
    point_lun, energyFd, 0

    ypos1 = 0                   ; block start
    ypos2 = bs - overlap        ; block end

    for j=0,blockNum-1 do begin
        ;progress,message='Iteration'+string(i), submessage='Progress',percent=(100.0*i)/iterNum, subpercent=(100.0*j)/(blockNum-1), /check_cancel
        if wid.cancel eq 1 then return

        energyDensity = !values.f_infinity + fltarr(file.xdim,blockSizes[j])

        blockSize = [covarSize,covarSize,file.xdim,blockSizes[j]]
        covarBlock = complexarr(blockSize)
        readu, covarFd, covarBlock
        point_lun,-covarFd, currentCovarOffset
        point_lun,covarFd, currentCovarOffset - covarOverlap
        
        if (i gt 0) then begin
            estimBlock = complexarr(blockSize)

            point_lun, -estimFd, estimOffset
            point_lun, estimFd, estimOffset - ((j<1)*covarOverlap/2)
            readu, estimFd, estimBlock
            point_lun, estimFd, estimOffset 

        end else begin
            estimBlock = covarBlock
        end

        newEstimBlock = estimBlock ;complexarr(blockSize)

        ; lookBuf = estimBlock
        ; estimEffectLooks, block=lookBuf
        lookBuf = calcEffectiveNumLooks(estimBlock,lookWinSize)

        ; print, min(lookBuf), max(lookBuf), mean(lookBuf)
        ; window, 0, xsize=300, ysize=150
        ; plot, histogram(lookBuf, nbins=300,/nan)

        for k=0,cliqueNum-1 do begin
            fractionDone = float(j*cliqueNum+k+1)/(blockNum*cliqueNum)
            progress,subpercent=100.0*fractionDone,/check_cancel
            if wid.cancel eq 1 then return

            clique = reform(cliqueConfig[*,*,k])
            updateEstimate, covarBlock, estimBlock, lookBuf, newEstimBlock, clique, energyDensity, lookNum, singularCovar
        end

        if (i gt 0) then begin
            lastDensity = fltarr(file.xdim, blockSizes[j])

            point_lun, -energyFd, energyOffset
            readu, energyFd, lastDensity

            ; tva, lastDensity, win=3
            ; tva, energyDensity, win=1
            finaliseEstimate, estimBlock, newEstimBlock, energyDensity, lastDensity, temperature, randSeed

;             tva, finite(energyDensity)
;             print, mean(energyDensity,/nan)
;             stop

            point_lun, energyFd, energyOffset
        end else begin
            temperature += blockSizes[j]*mean(energyDensity,/nan)/file.ydim
            estimBlock = covarBlock
        end

        if (finite(temperature) eq 0) then stop

        writeu, energyFd, energyDensity


        if j eq blockNum-1 then ypos2 = bs_last
        writeu, estimFd, estimBlock[*,*,*,ypos1:ypos2-1]
        ypos1 = overlap
    end

    if (i eq 0) then initTemp *= abs(temperature)*alog(1.0+cooling)
end

progress,Message='Finalising',/cancel_button

ypos1 = 0                       ; block start
ypos2 = bs - overlap            ; block end
point_lun, estimFd, 0
for i=0,blockNum-1 do begin
    fractionDone = float(i+1)/blockNum
    progress,percent=100.0*fractionDone,/check_cancel
		if wid.cancel eq 1 then return
    
    blockSize = [covarSize,covarSize,file.xdim,blockSizes[i]]
    estimBlock = complexarr(blockSize)
    readu, estimFd, estimBlock

    if i eq blockNum-1 then ypos2 = bs_last
    writeu,outFd, estimBlock[*,*,*,ypos1:ypos2-1]
    ypos1 = overlap

    point_lun, -estimFd, estimOffset
    point_lun, estimFd, estimOffset - covarOverlap
end

free_lun,covarFd,outFd,estimFd,energyFd

file_move,outputfile,finalfile,/overwrite
file_delete,energyFile
file.name = finalfile

evolute,'Speckle filtering (Simulated Annealing): IterNum='+strcompress(iterNum,/R)+' cooling='+strcompress(cooling,/R)+ $
        ' initTemp='+strcompress(initTemp,/R)+' lookNum='+strcompress(lookNum,/R)+' singularCovar='+strcompress(singularCovar,/R)

if not keyword_set(called) then begin
    generate_preview
    update_info_box
 endif  else progress,/destroy
	
end
