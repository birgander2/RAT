;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;************************************************************************
;* edge_pol_cfar.pro                              
;*                                                                       
;* Version: 1.0              Revised: September/2004        
;************************************************************************
;* Written by:
;* - Marc Jaeger, TUB                            
;************************************************************************
;* Polarimetric Constant False Alarm Rate Edge Detector
;************************************************************************
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


function calc_filter_mask ,filterIndex, winRad, filterNum, diagDist, imgSize
fltWin = complexarr(2,imgSize[0],imgSize[1])
dist = fltarr(2,imgsize[0],imgsize[1])
center = round(0.5*imgSize)
angle = (!pi * filterIndex)/filterNum
c = cos(angle)
s = sin(angle)

distMat = (fltarr(1,imgSize[1])+1.0)##findgen(imgSize[0])-center[0]
xMat = distMat
yMat = findgen(1,imgSize[1])##(fltarr(imgSize[0])+1.0)-center[1]
xMat = c*xMat+s*yMat
yMat = c*yMat-s*distMat
distMat = sqrt(xMat^2+yMat^2)

fltWin[0,*,*] = ((winRad+0.5-distMat)<1.0)>0.0
fltWin[1,*,*] = fltWin[0,*,*]
fltWin[0,*,*] *= 1.0 - (((0.5*diagDist+0.5-yMat)<1.0)>0.0)
fltWin[1,*,*] *= 1.0 - (((0.5*diagDist+0.5+yMat)<1.0)>0.0)

for i=0,1 do begin
    fltWin[i,*,*] /= total(fltWin[i,*,*])
    fltWin[i,*,*] = fft(shift(reform(fltWin[i,*,*]),-center[0],-center[1]))
end

return, fltWin
end



function pol_cfar, block, winRad, filterNum, diagDist, looks
blockSize = (size(block))[1:4]
fftFactor = float(blockSize[2]*blockSize[3])

for i=0,blockSize[0]-1 do for j=0,blockSize[1]-1 do block[i,j,*,*] = fft(block[i,j,*,*])

lowBlock = fltarr(blockSize)
highBlock = fltarr(blockSize)
lnQ = fltarr(blockSize[2:3])
lnQ[*,*] = !values.f_infinity
vectLen = blockSize[0]

for i=0,filterNum-1 do begin
    fltWin = calc_filter_mask(i, winRad, filterNum, diagDist, blockSize[2:3])

    for j=0,blockSize[0]-1 do begin
        for k=0,blockSize[1]-1 do begin
            lowBlock[j,k,*,*] = fft(fftFactor*block[j,k,*,*]*fltWin[0,*,*],1)
            highBlock[j,k,*,*] = fft(fftFactor*block[j,k,*,*]*fltWin[1,*,*],1)
        end
    end     

    numerator = float(block_det(highBlock))*float(block_det(lowBlock))
    denom = float(block_det(lowBlock + highBlock))

    newLnQ = (2*vectLen*looks)*(alog(2*looks) - alog(looks)) + looks*alog(numerator) - 2*looks*alog(denom)
    infInd = where(finite(newLnQ) eq 0, nr)
    if (nr gt 0) then newLnQ[infInd] = !values.f_infinity
    infInd = 0
    lnQ = newLnQ < lnQ
end

infInd = where(finite(lnQ) eq 0,nr)
if (nr gt 0) then lnQ[infInd] = max(lnQ,/nan)

return, -reform(lnQ)
end



function calc_threshold, pfa, looks, vecSize
lookNum = mean(looks)
ro = 1.0 - (6*vecSize^2 - 3) / (12*lookNum*vecSize)
f = vecSize^2
w = -0.25*f*(1.0-1.0/ro)^2+(7*f*(f-1))/(96*lookNum^2*ro^2)
solnVal = 1.0 - pfa
threshVal = 10.0

; test1 = findgen(30)
; test2 = (1.0-w) * igamma(0.5*f,0.5*test1) + w*igamma(0.5*f+2,0.5*test1)
; window, 2
; plot, test1, test2

; test2 = (1.0-w) * ((exp(-0.5*test1)*threshVal^(0.5*f-1))/(4*gamma(0.5*f))) + w * ((exp(-0.5*test1)*threshVal^(0.5*f+1.0))/(4*gamma(0.5*f+2.0)))
; window, 3
; plot, test1, test2

repeat begin
    newSoln = (1.0-w) * chisqr_pdf(threshVal,f) + w*chisqr_pdf(threshVal,f+4)
    grad = (1.0-w) * ((exp(-0.5*threshVal)*threshVal^(0.5*f-1))/(4*gamma(0.5*f))) + w * ((exp(-0.5*threshVal)*threshVal^(0.5*f+1.0))/(4*gamma(0.5*f+2.0)))
    threshVal += 0.5*(solnVal-newSoln)/grad
end until abs(newSoln-solnVal) lt 0.001

return, threshVal
end



pro edge_pol_cfar,CALLED = called
common rat, types, file, wid, config

allowTypes = [220,511,indgen(11)+200,500,501]

if (min(abs(file.type-allowTypes)) ne 0) then begin
    error_button = DIALOG_MESSAGE(['Data has to be a','polarimetric vector','or a polarimetric matrix'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
    return
endif

if not keyword_set(called) then begin ; Graphical interface
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='MSP-RoA Edge Detector',/floating,/tlb_kill_request_events,/tlb_frame_attr)

    line1 = WIDGET_BASE(main,column=2)
    field1  = CW_FIELD(line1,VALUE=11,/integer,TITLE='Filter radius                     : ',XSIZE=3)

    line2 = WIDGET_BASE(main,column=2)
    field2   = CW_FIELD(line2,VALUE=4,/integer,TITLE='# of filter orientations          : ',XSIZE=3)

    line3 = WIDGET_BASE(main,column=2)
    field3   = CW_FIELD(line3,VALUE=3,/integer,TITLE='Diagonal distance for sub-windows : ',XSIZE=3)

    line4 = WIDGET_BASE(main,column=2)
    lineSep = STRING(10B)
    if (config.os eq 'windows') then lineSep = STRING(13B)+lineSep    
    field4   = CW_FIELD(line4,VALUE=-1.0,/floating,TITLE='Probability of false alarm <= :'+lineSep+'(set negative value to obtain edge strength)',XSIZE=4)

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
            infotext = ['POLARIMETRIC CFAR EDGE DETECTION',$
                        ' ',$
                        'For more information, see J.  Schou and H.  Skriver and A. H.  Nielsen and K.  Conradsen,',$
                        'CFAR Edge Detector for Polarimetric SAR Images,',$
                        'IEEE Trans.Geoscience and Remote Sensing, Vol. 41, pp. 20-32, January 2003',$
                        ' ',$
                        'RAT module written 09/2004 by Marc Jaeger']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
    widget_control,field1,GET_VALUE=winSize
    widget_control,field2,GET_VALUE=filterNum
    widget_control,field3,GET_VALUE=diagDist
    widget_control,field4,GET_VALUE=pfa
    widget_control,main,/destroy
    if event.id ne  but_ok then return ; OK button _not_ clicked
endif else begin                ; Routine called with keywords
    winSize = 11
    filterNum = 4
    diagDist = 3
    pfa = -1.0
endelse

if (file.type lt 500 and file.type ne 220) then begin
    error = DIALOG_MESSAGE(["Data is in scattering vector form.","It will be converted to matrix form first."], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
    if error eq 'Cancel' then return
    k_to_m,/called
endif


if (file.type ge 500 and file.type ne 511) then begin
    error = DIALOG_MESSAGE(["Data is in scattering vector form.","It will be converted to matrix form first."], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
    if error eq 'Cancel' then return
    polin_k2m,/called
endif

; Error Handling

if winSize lt 3 then begin          ; Wrong box size ?
    error = DIALOG_MESSAGE("Boxsize has to be >= 3", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
endif

if diagDist lt 0 then begin          ; Invalid correlation distance ?
    error = DIALOG_MESSAGE("Diagonal distance has to be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
endif

if pfa gt 0 then begin
	fltMask = calc_filter_mask(0, winSize, filterNum, diagDist, [file.xdim,file.ydim])
	calc_looks, /CALLED, LOOK_NUM=looks, FFT_FILTER=reform(fltMask[0,*,*])
	threshVal = calc_threshold(pfa, looks, file.vdim)
endif else looks = 1.0


; change mousepointer

WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

head = 1l
rrat,file.name,ddd,header=head,info=info,type=type
srat,outputfile,eee,header=[2l,file.xdim,file.ydim,4l],info=info,type=type

; calculating preview size and number of blocks

bs = config.blocksize
overlap = winSize
calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last 
blocksizes = intarr(anz_blocks)+bs
blocksizes[anz_blocks-1] = bs_last

ypos1 = 0                       ; block start
ypos2 = bs - overlap            ; block end

byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable types

; pop up progress window

progress,Message='Detecting Edges...',/cancel_button
        
;start block processing
        
for i=0,anz_blocks-1 do begin   
    progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

    block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
    readu,ddd,block
    
    block = pol_cfar(block, winSize, filterNum, diagDist, looks)
    ro = 1.0 - (6*file.vdim^2 - 3) / (12*looks*file.vdim)
    block *= 2.0*ro
    if (pfa gt 0.0) then block = float(block ge threshVal)

    if i eq anz_blocks-1 then ypos2 = bs_last
    writeu,eee,block[*,ypos1:ypos2-1]
    ypos1 = overlap
    point_lun,-ddd,file_pos
    point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
endfor
free_lun,ddd,eee

; update file information

file.name = finalfile
file.type = 110l
file.dim = 2l
file.vdim = 1
file.zdim = 1
file.var = 4l
file_move,outputfile,finalfile,/overwrite

; generate preview

if not keyword_set(called) then begin
    generate_preview
    update_info_box
endif
end
