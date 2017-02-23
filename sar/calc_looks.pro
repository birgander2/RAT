;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calc_looks
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
function calcLookNum, block, winRad, lookHist, histLim, FFT_FILTER=fft_filter
   block = reform(block)

   blockSize = size(block)
   histLen = n_elements(lookHist) ;
   blockDim = blockSize[0]
   blockRes = blockSize[blockDim-1:blockDim]
   blockSize = blockSize[1:blockDim]


   logLim = [0,-alog(histLim[0])]
   histStep = float(logLim[1]-logLim[0])/(histLen-3)
   histUnit = 1.0/histStep

   if (min(blockRes-2*winRad) le 0) then begin
      blockDim = -1
   end

   channels = -1
   case blockDim of
      2: begin
         channels = 1
         intenBlock = fltarr(1,blockRes[0],blockRes[1])
         intenBlock[0,*,*] = abs(block)^2
      end

      3: begin
         channels = blockSize[0]
         intenBlock = fltarr([blockSize[0],blockRes])
         for i=0,blockSize[0]-1 do intenBlock[i,*,*] = abs(block[i,*,*])^2
      end

      4: begin
         if (blockSize[0] eq blockSize[1]) then begin
            channels = blockSize[0]
            intenBlock = fltarr(channels,blockRes[0],blockRes[1])
            for i=0,blockSize[0]-1 do intenBlock[i,*,*] = float(block[i,i,*,*])
         end else begin
            channels = blockSize[0]*blockSize[1]
            intenBlock = fltarr(channels,blockRes[0],blockRes[1])
            for i=0,blockSize[0]-1 do begin
               for j=0,blockSize[1]-1 do begin
                  intenBlock[i+j*blockSize[0],*,*] = abs(block[i,j,*,*])^2
               end
            end
         end
      end
      else:
   end


   winSize = 2*(winRad+1)
   winCenter = floor(winSize/2)
   filter = (fltarr(1,winSize)+1.0)##findgen(winSize)-winCenter
   filter = sqrt(filter^2 + transpose(filter^2))
   filter = ((winRad+0.5-filter)<1.0)>0.0
   filter /= total(filter)

   fftFactor = product(blockRes)
   for i=0,channels-1 do begin
      if (keyword_set(fft_filter)) then begin
         intenBlock[i,*,*] = fftFactor*fft(fft(intenBlock[i,*,*],-1)*fft_filter,1)
      end

      lookBlock = convol(reform(intenBlock[i,*,*])^2,filter,/center,/edge_wrap)
      lookBlock /= convol(reform(intenBlock[i,*,*]),filter,/center,/edge_wrap)^2
      lookBlock = 1.0/(temporary(lookBlock)-1.0)
      lookBlock = -alog(lookBlock/histLim[1]) ;

      edgeBlock = sqrt(reform(intenBlock[i,*,*]))
      edge_msproa, block=edgeBlock, winSize = 2*winRad
      edgeBlock = -alog(edgeBlock)
      infInd = where(finite(edgeBlock) eq 0, nr)
      if (nr gt 0) then edgeBlock[infInd] = max(edgeBlock,/nan)
      edgeBlock = (edgeBlock-mean(edgeBlock)) > 0

      histInd = (round((lookBlock-logLim[0])*histUnit + 1) > 0) < (histLen+1)
      ignore = histogram(histInd,min=0,max=histLen-1,reverse_indices=revInd)
      ignore = 0

      for j=0,histLen-1 do begin
         if (revInd[j] ge revInd[j+1]) then continue
         updateInd = revInd[revInd[j]:revInd[j+1]-1]
         lookHist[j] += total(edgeBlock[updateInd])
      end
   end

   medWidth = round(0.01 * histLen) ;
   medHist = median(lookHist,medWidth) ;
   medHist[[0,histLen-1]] = 0

   histCenter = floor(0.5*histLen)
   diff = complex(0,findgen(histLen) - histCenter)
   diff *= exp(-(imaginary(diff)/(histLen*0.25 < 50))^2)
   diffHist = float(fft(fft(medHist,-1) * shift(diff,-histCenter),1))

   ignore = max(medHist,maxInd)
   ignore = max(diffHist[0:maxInd],maxInd)
   maxInd = min(where(diffHist[0:maxInd] ge 0.8*diffHist[maxInd]))
   lookNum = histLim[1]*exp(-(maxInd*histStep + logLim[0]))

   return, [lookNum,maxInd]
end











pro calc_looks, CALLED = called, LOOK_NUM = lookNum, FFT_FILTER=fftFilter, $
                histLen=histLen, winRad=winRad
common rat, types, file, wid, config

; keywords:
; look_num: is set to the estimated number of looks


allowed = [100,101,indgen(11)+200,220,221,indgen(14)+500]
if (max(allowed-file.type eq 0) ne 1) then begin
    error_button = DIALOG_MESSAGE(['Data has to be SAR complex, polarimetric','vector/matrix or polinsar vector/matrix.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
    return
endif

if n_elements(histLen) eq 0 then histLen = 2048
if n_elements(winRad) eq 0 then winRad=8
histLim = [1e-4,500]

if ~keyword_set(called) then begin ; Graphical interface
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Number of Looks Calculation',/floating,/tlb_kill_request_events,/tlb_frame_attr)

    line1 = WIDGET_BASE(main,column=2)
    field1   = CW_FIELD(line1,VALUE=winRad,/integer,TITLE='Estimation window size  : ',XSIZE=2)

    line2 = WIDGET_BASE(main,column=2)
    field2   = CW_FIELD(line2,VALUE=histLen,/int,TITLE='Histogram length  : ',XSIZE=7)

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
            infotext = ['Effective NUMBER OF LOOKS COMPUTATION',$
                        'Calculates the effective number of looks in a SAR image.',$
                        '',$
                        'RAT module written 04/2005 by Marc Jaeger']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        end
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

    if event.id ne but_ok then begin
        widget_control,main,/destroy
        return
    end

    widget_control,field1,GET_VALUE=winRad
    widget_control,field2,GET_VALUE=histLen
    widget_control,main,/destroy
endif

; Error Handling
if winRad lt 1 then begin          ; Wrong box size ?
    error = DIALOG_MESSAGE("Estimation window size must be >= 1", DIALOG_PARENT = wid.base, TITLE='Error',/error)
    return
endif

; change mousepointer

WIDGET_CONTROL,/hourglass

rrat,file.name,inFD,header=head,info=info,type=type		

; calculating preview size and number of blocks

blockSize = config.blocksize
overlap = 0
calc_blocks_overlap,file.ydim,blockSize,overlap,blockNum,lastSize 

; pop up progress window

progress,Message='Calculating # of looks...',/cancel_button

;start block processing

lookHist = fltarr(histLen)

block = make_array([file.vdim,file.zdim,file.xdim,blockSize],type=file.var)
typeSize = [0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]
typeSize = typeSize[file.var]

for i=0,blockNum-1 do begin   
    progress,percent=(i+1)*100.0/blockNum,/check_cancel
    if wid.cancel eq 1 then return

    first = 1-(i<1)
    last = (i-(blockNum-2))>0
    currentSize = blockSize
    if (i eq blockNum-1) then begin
        block = make_array([file.vdim,file.zdim,file.xdim,lastSize],type=file.var)
    end

    readu,inFD,block

    lookNum = calcLookNum(block, winRad, lookHist, histLim, fft_filter=fftFilter)
endfor

free_lun,inFD

progress, /destroy

if ~keyword_set(called) then begin
    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Estimated number of looks',/floating,/tlb_kill_request_events,/tlb_frame_attr)
    
    sub  = WIDGET_BASE(main,row=2,/frame)
    tx1  = widget_text(sub,XSIZE=64,ysize=2)

    lineSep = STRING(10B)
    if (config.os eq 'windows') then lineSep = STRING(13B)+lineSep
    resultText = 'The estimated number of looks in this image is:'+lineSep+string(lookNum[0])
    widget_control, tx1, set_value = resultText

    plt  = widget_draw(sub,XSIZE=400,ysize=250)

    buttons  = WIDGET_BASE(main,column=1)
    but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
    
    WIDGET_CONTROL, main, /REALIZE, default_button = but_ok

    widget_control, plt, get_value=index
    wset,index
    plot, lookHist

    loadct, 13
    oplot, [lookNum[1],lookNum[1]], [0,max(lookHist)]
    loadct, 0

    
    repeat begin
        event = widget_event(main)
    endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' 
    widget_control,main,/destroy

    widget_control,wid.draw,get_value=index
    wset,index
 endif else $
    print, 'The estimated number of looks in this image is: '+ $
           strcompress(lookNum[0],/R) 

lookNum = lookNum[0]

evolute,'Effective number of looks (ENL) estimation: '+ $
        strcompress(lookNum,/r)+' (Windows size: '+strcompress(winRad,/R)+ $
        ', Histogram size: '+strcompress(histLen,/r) 
end
