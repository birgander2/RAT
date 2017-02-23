;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
;************************************************************************
;* edge_msproa.pro                              
;*                                                                       
;* Version: 1.0              Revised: Aug/2004        
;************************************************************************
;* Written by:
;* - Marc Jaeger, TUB                            
;************************************************************************
;* Maximum Strength Edge Pruned Ratio of Averages edge detector (MSP-RoA)            
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

function msproa, ampDat, winSize, corrDist, strengthLim
ampSize = (size(ampDat))[1:2]
fltRes = fltarr(8,ampSize[0],ampSize[1])

winCent = floor(winSize/2)

sqFlt = fltarr(2,winSize,winSize)
sqFlt[0,0:winCent-1,*] = 1.0
sqFlt[1,winCent+1:winSize-1,*] = 1.0

triFlt = fltarr(2,winSize,winSize)
triWin = transpose(fltarr(winSize)+1.)##findgen(winSize) + transpose(findgen(winSize))##(fltarr(winSize)+1.)
triFlt[where(triWin lt winSize-1)*2] = 1.0
triFlt[where(triWin ge winSize)*2+1] = 1.0

for i = 0,1 do begin
    fltRes[i,*,*] = convol(ampDat,reform(sqFlt[i,*,*]),/center,/edge_truncate)
    fltRes[i+2,*,*] = convol(ampDat,transpose(sqFlt[i,*,*]),/center,/edge_truncate)
    fltRes[i+4,*,*] = convol(ampDat,reform(triFlt[i,*,*]),/center,/edge_truncate)
    fltRes[i+6,*,*] = convol(ampDat,reform(reverse(triFlt[i,*,*],2)),/center,/edge_truncate)
end

for i = 0,3 do begin
     zeroInd = where(fltRes[2*i,*,*] eq 0., zeroNum)
     if (zeroNum eq 0) then begin
         zeroInd = where(fltRes[2*i+1,*,*] eq 0, zeroNum)
     endif else begin
         addInd = [where(fltRes[2*i+1,*,*] eq 0., addNum)]
         if (addNum gt 0) then begin
             zeroInd = [zeroInd, addInd]
             zeroNum += addNum
         endif
     endelse
     
     if (zeroNum le 0) then continue

     fltRes[zeroInd*8+2*i] = 1.0
     fltRes[zeroInd*8+2*i+1] = 1.0
end

fltRes = [fltRes[0,*,*]/fltRes[1,*,*],fltRes[2,*,*]/fltRes[3,*,*],fltRes[4,*,*]/fltRes[5,*,*],fltRes[6,*,*]/fltRes[7,*,*]]

gtInd = where(fltRes gt 1., gtNum)
if (gtNum gt 0) then fltRes[gtInd] = 1. / fltRes[gtInd]
fltRes = 1.0 - fltRes

if (strengthLim lt 0) then return, max(fltRes,dimension=1)

ltInd = where(fltRes lt strengthLim, ltNum)
if (ltNum gt 0) then fltRes[ltInd] = -1.0

corrMatSize = 2*corrDist+1
corrWin = fltarr(4,corrMatSize,corrMatSize)
corrWin[0,*,corrDist] = 1.0
corrWin[1,corrDist,*] = 1.0
corrWin[2,*,*] = diag_matrix(fltarr(corrMatSize)+1.0)
if (corrMatSize gt 1) then begin
    corrWin[3,*,*] = reverse(corrWin[2,*,*],2)
endif else corrWin[3,*,*] = corrWin[2,*,*]

result = fltarr(ampSize[0],ampSize[1])
for i = 0,ampSize[0]-1 do begin
    for j = 0,ampSize[1]-1 do begin
        iLim = [(corrDist - i)>0,((i+corrDist+1)-ampSize[0])>0]
        jLim = [(corrDist - j)>0,((j+corrDist+1)-ampSize[1])>0]
        corrArr = fltRes[*,(i+iLim[0])-corrDist:(i-iLim[1])+corrDist,(j+jLim[0])-corrDist:(j-jLim[1])+corrDist] * corrWin[*,iLim[0]:(corrMatSize-1)-iLim[1],jLim[0]:(corrMatSize-1)-jLim[1]]

        for k = 0,3 do begin
            if (fltRes[k,i,j] le 0.0) then continue
            if (max(corrArr[k,*,*]) eq fltRes[k,i,j]) then begin
                result[i,j] = 1.0
                break
            endif
        endfor
    endfor
endfor

return,result
end


pro edge_msproa,CALLED = called, BLOCK=block, WINSIZE=winSize
common rat, types, file, wid, config

strengthLim = 0.45
corrDist = 2

if (keyword_set(block)) then begin
    strengthLim = -1
    called = 1
end

if (not keyword_set(winSize)) then winSize = 11

if not keyword_set(called) then begin ; Graphical interface
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='MSP-RoA Edge Detector',/floating,/tlb_kill_request_events,/tlb_frame_attr)

    line1 = WIDGET_BASE(main,column=2)
    field1   = CW_FIELD(line1,VALUE=winSize,/integer,TITLE='Filter boxsize      : ',XSIZE=3)

    line2 = WIDGET_BASE(main,column=2)
    field2   = CW_FIELD(line2,VALUE=strengthLim,/floating,TITLE='Edge strength >=    : ',XSIZE=4)

    line3 = WIDGET_BASE(main,column=2)
    field3   = CW_FIELD(line3,VALUE=corrDist,/integer,TITLE='Correlation distance: ',XSIZE=3)

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
            infotext = ['MAXIMUM STRENGTH EDGE PRUNED RATIO OF AVERAGE (MSP-ROA) EDGE DETECTION',$
                        ' ',$
                        'For more information, see S.S. Ganugapati and C.R. Moloney, Proc. ICIP 95, Vol II, pp. 165-168.',$
                        ' ',$
                        'RAT module written 08/2004 by Marc Jaeger']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
    widget_control,field1,GET_VALUE=winSize
    widget_control,field2,GET_VALUE=strengthLim
    widget_control,field3,GET_VALUE=corrDist
    widget_control,main,/destroy
    if event.id ne  but_ok then return ; OK button _not_ clicked
endif

; Error Handling

if winSize lt 3 then begin          ; Wrong box size ?
    error = DIALOG_MESSAGE("Boxsizes has to be >= 3", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
endif
if corrDist lt 0 then begin          ; Invalid correlation distance ?
    error = DIALOG_MESSAGE("Correlation distance has to be >= 0", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
endif


; change mousepointer

WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED


; processing a single block (keyword)

if (keyword_set(block)) then begin
    block[*,*] = msproa(block,winSize, corrDist, strengthLim)
    return
end


; handling of complex and amplitude input data

if file.var eq 6 or file.var eq 9 then begin ; Wrong variable type?
    error = DIALOG_MESSAGE(["Image is complex and has to","be converted to float first"], /cancel, DIALOG_PARENT = wid.base, TITLE='Warning')
    if error eq "Cancel" then return else complex2abs,/called
endif

; read / write header

head = 1l
rrat,file.name,ddd,header=head,info=info,type=type		
srat,outputfile,eee,header=head,info=info,type=type		

; calculating preview size and number of blocks

bs = config.blocksize
overlap = winSize/2 
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

; -------- THE FILTER ----------
		for j=0,file.vdim-1 do for k=0,file.zdim-1 do begin
                    block[j,k,*,*] = msproa(reform(block[j,k,*,*]),winSize, corrDist, strengthLim)
                endfor
; -------- THE FILTER ----------

		if i eq anz_blocks-1 then ypos2 = bs_last
		writeu,eee,block[*,*,*,ypos1:ypos2-1]
		ypos1 = overlap
		point_lun,-ddd,file_pos
		point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
	endfor
	free_lun,ddd,eee

; update file information
	
	file.name = finalfile
	file.type = 110l
	file_move,outputfile,finalfile,/overwrite
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif
end
