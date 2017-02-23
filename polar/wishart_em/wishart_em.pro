;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: wishart_em
; written by    : Marc Jäger
; last revision : June 2005
; Classifies polarimetric images using expectation maximisation
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


pro wishart_em, CALLED=called
common rat, types, file, wid, config
common channel, channel_names, channel_selec, color_flag, palettes, pnames

; check if array is usable

if file.type ne 220 && ~(file.type ge 510 && file.type le 513) then begin
    error_button = DIALOG_MESSAGE(['The input needs to be the covariance matrix.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
    return
endif


finalClassNum = 16
incrementPause = 4
logHistLen = 4096
lookNum = 11.05
convergenceShock = 2

if not keyword_set(called) then begin ; Graphical interface
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Wishart EM Classification',/floating,/tlb_kill_request_events,/tlb_frame_attr)

    line5 = WIDGET_BASE(main,column=3)
    field5   = CW_FIELD(line5,VALUE=finalClassNum,/integer,TITLE='Final class number:          ',XSIZE=4)

    line7 = WIDGET_BASE(main,column=3)
    field7   = CW_FIELD(line7,VALUE=incrementPause,/integer,TITLE='Number of iterations per class: ',XSIZE=4)


    line8 = WIDGET_BASE(main,column=3)
    field8   = CW_FIELD(line8,VALUE=lookNum,/float,TITLE='Number of looks: ',XSIZE=7)

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
            infotext = ['WISHART EXPECTATION MAXIMISATION',$
                        ' ',$
                        'RAT module written 06/2005 by Marc Jaeger']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

    widget_control,field5,GET_VALUE=finalClassNum
    widget_control,field7,GET_VALUE=incrementPause
    widget_control,field8,GET_VALUE=lookNum
    widget_control,main,/destroy
    if event.id ne but_ok then return ; OK button _not_ clicked
endif

; go

WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

head = 1l

rrat,file.name,covarFd,header=head,info=info,type=type
point_lun, -covarFd, covarDataOffset

srat,outputfile,outFd,header=[2l,file.xdim,file.ydim,2l],info=info,type=411l

bs = config.blocksize
calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
blocksizes = intarr(anz_blocks)+bs
blocksizes[anz_blocks-1] = bs_last

covarSize = file.vdim

classCovar = reform(complexarr(file.vdim,file.zdim), [file.vdim,file.zdim,1])
classCovar[*,*,0] = diag_matrix(fltarr(file.vdim)+1)
classWeight = fltarr(1)+1

for i=0,finalClassNum-1 do begin
    classStrength = fltarr(i+1)-!values.f_infinity
    maxLogE = fltarr(i+1) - !values.f_infinity
    maxCovar = complexarr(covarSize,covarSize,i+1)


    classIterNum = (incrementPause-1)*(i<1)
    for j=0,classIterNum do begin
        point_lun, covarFd, covarDataOffset
        
        newClassCovar = complexarr(covarSize,covarSize,i+1)
        newClassWeight = fltarr(i+1)
        
        classHist = fltarr(i+1)
        
        progress,Message='EM  Iteration ' + string(i+1) + string(j+1),/cancel_button
        for k=0,anz_blocks-1 do begin
            progress,percent=(k+1)*100.0/anz_blocks,/check_cancel
            if wid.cancel eq 1 then return
            
            covarBlock = make_array([file.vdim,file.zdim,file.xdim,blockSizes[k]], type=6l)
            readu, covarFd, covarBlock
            ; reduceDynRange, covarBlock

            logEBlock = polarEStep(covarBlock, classCovar, classWeight, lookNum)
            
            ignore = max(logEBlock,classification,dimension=1)
            classification = classification mod (i+1)
            ; tva, classification, /n, win=0
            classHist += histogram(classification,min=0,max=i,nbins=i+1)            

            if (j eq classIterNum) then begin
                for m=0,i do begin
                    classInd = where(classification eq m, nr)
                    if (nr eq 0) then continue
                    classEBlock = logEBlock[m,*,*]

                    logShift = classStrength[m] > max(classEBlock)
                    classStrength[m] = alog(exp(classStrength[m]-logShift)+total(exp(classEBlock-logShift))) + logShift

                    maxE = max(classEBlock, maxInd)
                    if (maxE gt maxLogE[m]) then begin
                        maxLogE[m] = maxE

                        for n=0,covarSize^2-1 do begin
                            maxCovar[n+covarSize^2*m] = covarBlock[n+covarSize^2*long(maxInd)]
                        end
                    end
                end
            end

            shock = convergenceShock^(j eq floor(0.5*(incrementPause-1)))
            polarMStep, covarBlock, logEBlock, newClassCovar, newClassWeight, shock
        end

;        print, classHist/total(classHist)
        
        emptyInd = where(classHist le 0, nr)
        if (nr gt 0) then begin
            for k=0,nr-1 do begin
                newClassWeight[emptyInd[k]] = 1
                newClassCovar[*,*,emptyInd[k]] = maxCovar[*,*,emptyInd[k]]
            end
        end

        for k=0,i do begin
            newClassCovar[*,*,k] /= newClassWeight[k]
        end
        classCovar = reform(newClassCovar, [file.vdim,file.zdim,i+1])
        classWeight = newClassWeight / total(newClassWeight)
    end

    if (i ne finalClassNum-1) then begin
        classStrength /= classHist
        ignore = min(classStrength, minInd, /nan)
        newCovar = reform(maxCovar[*,*,minInd])
        newWeight = 0.0
        
        newClassCovar = complexarr(file.vdim,file.zdim,i+2)
        newClassCovar[*,*,0:i] = classCovar
        newClassCovar[*,*,i+1] = newCovar
        classCovar = newClassCovar

        newClassWeight = fltarr(i+2) ; + 1.0/(i+2)
        newClassWeight[0:i] = (1.0 - 1.0/(i+2)) * classWeight
        newClassWeight[i+1] = 1.0/(i+2)
        classWeight = newClassWeight
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
for i=0,anz_blocks-1 do begin
    progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
    if wid.cancel eq 1 then return

    covarBlock = make_array([file.vdim,file.zdim,file.xdim,blockSizes[i]], type=6l)
    readu, covarFd, covarBlock
    ; reduceDynRange, covarBlock
    
    logEBlock = polarEStep(covarBlock, classCovar, classWeight, lookNum)
    ignore = max(logEBlock, classBlock, dimension=1)
    classBlock = classBlock mod finalClassNum
    classHist += histogram(classBlock, min=0, max=finalClassNum-1, nbins=finalClassNum)
    writeu, outFd, fix(classBlock)
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
