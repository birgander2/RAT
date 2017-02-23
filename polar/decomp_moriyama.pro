;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: decomp_moriyama
; written by    : Marc Jäger
; last revision : July 2004
; Calculates the polarimetric Moriyama decomposition
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



pro decomp_moriyama,CORTHRESH=corThresh,CALLED = called
common rat, types, file, wid, config

; check if array is usable

	if file.type ne 220 then begin
	    error_button = DIALOG_MESSAGE(['Data has to be a','covariance matrix'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
	    return
	endif
	
	
	if not keyword_set(called) then begin ; Graphical interface
	    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Moriyama Urban Areas Decomposition',/floating,/tlb_kill_request_events,/tlb_frame_attr)
	    
	    line1 = WIDGET_BASE(main,column=2)
	    field1   = CW_FIELD(line1,VALUE=0.6,/floating,TITLE='HH/HV correlation threshold: ',XSIZE=3)
	    
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
	            infotext = ['MORIYAMA POLARIMETRIC DECOMPOSITION FOR URBAN AREAS',$
	                        ' ',$
	                        'For more information, see T. Moriyama, Proc. EUSAR 2004, vol. 1, pp. 435-438',$
	                        ' ',$
	                        'RAT module written 07/2004 by Marc Jaeger']
	            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
	        end
	    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST' 
	    widget_control,field1,GET_VALUE=correlationLim
	    widget_control,main,/destroy
	    if event.id ne but_ok then return ; OK button _not_ clicked
	endif else begin                ; Routine called with keywords
	    if not keyword_set(corThresh) then begin
	        correlationLim = 0.32
	    endif else begin
	        correlationLim = corThresh
	    endelse
	endelse

	WIDGET_CONTROL,/hourglass
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED


; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[3l,4l,file.xdim,file.ydim,4l],info=info,type=216l
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='Moriyama Decomposition...',/cancel_button

; calculating decomposition

	for i=0,anz_blocks-1 do begin
		progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

		block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
                ps = make_array([file.xdim,blocksizes[i]],type=4l)
                pd = make_array([file.xdim,blocksizes[i]],type=4l)
                pv = make_array([file.xdim,blocksizes[i]],type=4l)
		oblock = make_array([4l,file.xdim,blocksizes[i]],type=4l)
		readu,ddd,block

		shv   = abs(block[2,2,*,*]) * 0.5
		shh   = abs(block[0,0,*,*])
		svv   = abs(block[1,1,*,*])
		shhvv = block[1,0,*,*]
                shhhv = block[2,0,*,*] / sqrt(2.)
                shvvv = block[1,2,*,*] / sqrt(2.)

                coherVal = abs(shhhv) / sqrt(shh * shv) 
                coherVal = coherVal + abs(shvvv) / sqrt(svv * shv)

                urbanIdx = where(coherVal gt 2.*correlationLim, urbanSize)

                if (urbanSize gt 0) then begin
                    evenIdx = where(real_part(shhvv[urbanIdx]) le 0, evenSize)
                    if (evenSize gt 0) then begin
                        c1 = double(shh[urbanIdx[evenIdx]])
                        c2 = double(svv[urbanIdx[evenIdx]])
                        c3r = double(real_part(shhvv[urbanIdx[evenIdx]]))
                        c3i = double(imaginary(shhvv[urbanIdx[evenIdx]]))
                        c4 = double(shv[urbanIdx[evenIdx]])
                        c5r = double(real_part(shhhv[urbanIdx[evenIdx]]))
                        c5i = double(imaginary(shhhv[urbanIdx[evenIdx]]))
                        c6r = double(real_part(shvvv[urbanIdx[evenIdx]]))
                        c6i = double(imaginary(shvvv[urbanIdx[evenIdx]]))

                        fc = (c6r^2 + c6i^2)/c4
                        rr = c6r/fc
                        ri = c6i/fc
                        
                        gDenom = fc * (rr^2 + ri^2)
                        gr = (-rr * c5r + c5i * ri)/gDenom
                        gi = (c5r * ri + rr * c5i)/gDenom

                        fo = (-c1*c2+fc*c2*gi^2+c3r^2+fc*c1-2*gr*fc*c3r+gr^2*fc*c2-2*fc*gi*c3i+c3i^2)/(-c2+fc+fc*gi^2-c1+gr^2*fc+2*c3r-2*gr*fc)
                        fe = -fo - fc + c2
                        ai = (fc * gi  - c3i) / fe
                        ar = (fo + gr*fc - c3r) / fe

                        ps[urbanIdx[evenIdx]] = 2. * fo > 0
                        pd[urbanIdx[evenIdx]] = fe*(1.+ar^2+ai^2) +fc*(1.+gr^2+gi^2) + fc*(rr^2+ri^2) > 0
                        pv[urbanIdx[evenIdx]] = 0.
                    endif

                    oddIdx = where(real_part(shhvv[urbanIdx]) gt 0, oddSize)
                    if (oddSize gt 0) then begin
                        c1 = double(shh[urbanIdx[oddIdx]])
                        c2 = double(svv[urbanIdx[oddIdx]])
                        c3r = double(real_part(shhvv[urbanIdx[oddIdx]]))
                        c3i = double(imaginary(shhvv[urbanIdx[oddIdx]]))
                        c4 = double(shv[urbanIdx[oddIdx]])
                        c5r = double(real_part(shhhv[urbanIdx[oddIdx]]))
                        c5i = double(imaginary(shhhv[urbanIdx[oddIdx]]))
                        c6r = double(real_part(shvvv[urbanIdx[oddIdx]]))
                        c6i = double(imaginary(shvvv[urbanIdx[oddIdx]]))

                        fc = (c6r^2 + c6i^2)/c4
                        rr = c6r/fc
                        ri = c6i/fc

                        gDenom = (rr^2 + ri^2) * fc
                        gr = (-rr*c5r + c5i*ri)/gDenom
                        gi = (rr*c5i + c5r*ri)/gDenom
                        
                        fo = -(-2*fc*c2-2*fc*c3r+fc^2*gi^2-2*fc*gi*c3i+2*gr*fc^2+c3i^2-2*c2*gr*fc-2*gr*fc*c3r+gr^2*fc^2+2*c2*c3r+fc^2+c2^2+c3r^2)/(gr^2*fc+2*gr*fc+fc+fc*gi^2-2*c3r-c2-c1)
                        fe = -fo - fc + c2
                        br = -(-fe + gr*fc - c3r) / fo
                        bi = -(fc*gi - c3i) / fo

                        ps[urbanIdx[oddIdx]] = fo*(1.+br^2+bi^2) > 0
                        pd[urbanIdx[oddIdx]] = 2.*fe + fc*(1.+gr^2+gi^2) + 2.*fc*(rr^2+ri^2) > 0
                        pv[urbanIdx[oddIdx]] = 0.
                    endif

                    
                    pType = make_array([file.xdim,blocksizes[i]],type=4l)
                    pType[urbanIdx] = 1.0
                    oblock[3,*,*] = coherVal / (2. * correlationLim)

                endif

                naturIdx = where(coherVal le 2.*correlationLim, naturSize)
                if (naturSize gt 0) then begin
                    pv[naturIdx] = 8*shv[naturIdx]

                    shh = shh - 3*shv
                    svv = svv - 3*shv
                    shhvv = shhvv - shv
                    sre = real_part(shhvv)
                    sim = imaginary(shhvv)

                    surfIdx = where((sre+shv)[naturIdx] gt 0,nr)
                    if nr gt 0 then begin
                        alpha = -1.0
                        c1 = double(shh[naturIdx[surfIdx]])
                        c2 = double(svv[naturIdx[surfIdx]])
                        c3r = double(sre[naturIdx[surfIdx]])
                        c3i = double(sim[naturIdx[surfIdx]])
                        
                        fDenom = 1. / (c2+2*c3r+c1)
                        bDenom = 1. / (c2^2+2*c3r*c2+c3i^2+c3r^2)

                        fd = (c2*c1-c3r^2-c3i^2) * fDenom > 0
                        fs = fDenom / bDenom > 0
                        br = (-c3i^2+c3r^2+c3r*c2+c1*c3r+c2*c1) * bDenom
                        bi = ((c2+2*c3r+c1)*c3i) * bDenom

                        pd[naturIdx[surfIdx]] = float(2.*fd) > 0
                        ps[naturIdx[surfIdx]] = float(fs*(1.+br^2+bi^2)) > 0
                    endif
                    
                    dbounceIdx = where((sre+shv)[naturIdx] le 0,nr)
                    if nr gt 0 then begin
                        beta  = +1.0
                        c1 = double(shh[naturIdx[dbounceIdx]])
                        c2 = double(svv[naturIdx[dbounceIdx]])
                        c3r = double(sre[naturIdx[dbounceIdx]])
                        c3i = double(sim[naturIdx[dbounceIdx]])

                        fDenom = 1. / (c2-2*c3r+c1)
                        aDenom = 1. / (c3r^2+c2^2+c3i^2-2*c3r*c2)
                        
                        fs = (c2*c1-c3r^2-c3i^2) * fDenom > 0
                        fd = fDenom / aDenom > 0
                        ar = -(-c3i^2+c2*c1-c1*c3r-c3r*c2+c3r^2) * aDenom
                        ai = (c3i*(c2-2*c3r+c1)) * aDenom
                        
                        pd[naturIdx[dbounceIdx]] = float(fd*(1.+ar^2+ai^2)) > 0
                        ps[naturIdx[dbounceIdx]] = float(2.*fs) > 0
                    endif
                endif
                    
                oblock[0,*,*] = pd
                oblock[1,*,*] = pv
                oblock[2,*,*] = ps

                index = where(finite(oblock) eq 0,nr)
                if nr gt 0 then oblock[index] = 0.0
                
                writeu,eee,oblock
            endfor
            free_lun,ddd,eee
            
; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 216l
	file.dim  = 3l
	file.zdim = 4l
	file.vdim = 1l
	file.var  = 4l
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
