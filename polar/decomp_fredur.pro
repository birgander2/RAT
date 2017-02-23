;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: decomp_fredur
; written by    : Marc Jaeger & Andreas Reigber
; last revision : 18. August 2004
; Calculates the polarimetric Freeman-Durden decomposition
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


function fredur_optimised, block
blockSize = (size(block))[1:4]

oblock = make_array([3l,blockSize[2:3]],type=4l)
ps = make_array(blockSize[2:3],type=4l)
pd = make_array(blockSize[2:3],type=4l)

shv   = abs(block[2,2,*,*]) / 2. ; divide this by 2 (in case of 3x3) ?????
shh   = abs(block[0,0,*,*]) - 3*shv > 0
svv   = abs(block[1,1,*,*]) - 3*shv > 0
shhvv = block[0,1,*,*] - shv

oblock[1,*,*] = 8*shv           ; volume contribution

ste = real_part(block[1,0,*,*])
sre = real_part(shhvv)
sim = imaginary(shhvv)

aux = where((sre^2+sim^2) gt (shh*svv),nr) ; preconditioning (shhvv bigger than possible)
if nr gt 0 then begin
    dummy    = sre^2+sim^2
    sre[aux] = sre[aux]*sqrt(shh[aux]*svv[aux]/dummy[aux])
    sim[aux] = sim[aux]*sqrt(shh[aux]*svv[aux]/dummy[aux])
endif
aux = where(ste gt 0,nr)        ; surface scattering dominant
if nr gt 0 then begin
    alpha = -1.0
    beta  = (shh+sre)/(svv+sre)
    fs    = (svv+sre)/(1+beta) > 0
    fd    = svv - fs > 0
    pd[aux] = fd[aux] * 2 > 0
    ps[aux] = fs[aux] * (1 + abs(beta[aux])^2)  > 0
endif
aux = where(ste le 0,nr)        ; double bounce scattering dominant
if nr gt 0 then begin
    beta  = +1.0
    fs    = (svv*shh-sre^2-sim^2)/(shh+svv-2*sre) > 0
    fd    =  svv - fs > 0
    alre  = (sre^2 + svv^2 + sim^2 - 2*sre*svv)
    alim  = sim*(shh+svv-2*sre) / alre
    alre  = (sre*shh + sre*svv - sre^2 - svv*shh + sim^2) / alre
    pd[aux] = fd[aux] * (1 + alre[aux]^2 + alim[aux]^2) > 0
    ps[aux] = fs[aux] * 2 > 0
endif

oblock[0,*,*] = pd
oblock[2,*,*] = ps

index = where(finite(oblock) eq 0,nr) ; eliminate unvalid (nan) pixels
if nr gt 0 then oblock[index] = 0.0

return, oblock
end



function fredur_precise, block
blockSize = (size(block))[1:4]

oblock = make_array([3l,blockSize[2:3]],type=4l)
ps = make_array(blockSize[2:3],type=4l)
pd = make_array(blockSize[2:3],type=4l)

shv   = abs(block[2,2,*,*]) * 0.5 ; divide this by 2 (in case of 3x3) ?????
shh   = abs(block[0,0,*,*]) - 3*shv
svv   = abs(block[1,1,*,*]) - 3*shv
shhvv = block[1,0,*,*] - shv

oblock[1,*,*] = 8*shv           ; volume contribution

ste = real_part(block[1,0,*,*])
sre = real_part(shhvv)
sim = imaginary(shhvv)

aux = where((sre^2+sim^2) gt (shh*svv),nr) ; preconditioning (shhvv bigger than possible)
if nr gt 0 then begin
    dummy    = sre^2+sim^2
    sre[aux] = sre[aux]*sqrt(shh[aux]*svv[aux]/dummy[aux])
    sim[aux] = sim[aux]*sqrt(shh[aux]*svv[aux]/dummy[aux])
endif

aux = where(ste gt 0,nr)        ; surface scattering dominant
if nr gt 0 then begin
    alpha = -1.0
    c1 = double(shh[aux])
    c2 = double(svv[aux])
    c3r = double(sre[aux])
    c3i = double(sim[aux])
    
    fDenom = 1. / (c2+2*c3r+c1)
    bDenom = 1. / (c2^2+2*c3r*c2+c3i^2+c3r^2)
    
    fd = (c2*c1-c3r^2-c3i^2) * fDenom > 0
    fs = fDenom / bDenom > 0
    br = (-c3i^2+c3r^2+c3r*c2+c1*c3r+c2*c1) * bDenom
    bi = ((c2+2*c3r+c1)*c3i) * bDenom
    
    pd[aux] = float(2.*fd) > 0
    ps[aux] = float(fs*(1.+br^2+bi^2)) > 0
endif
aux = where(ste le 0,nr)        ; double bounce scattering dominant
if nr gt 0 then begin
    beta  = +1.0
    c1 = double(shh[aux])
    c2 = double(svv[aux])
    c3r = double(sre[aux])
    c3i = double(sim[aux])
    
    fDenom = 1. / (c2-2*c3r+c1)
    aDenom = 1. / (c3r^2+c2^2+c3i^2-2*c3r*c2)
    
    fs = (c2*c1-c3r^2-c3i^2) * fDenom > 0
    fd = fDenom / aDenom > 0
    ar = -(-c3i^2+c2*c1-c1*c3r-c3r*c2+c3r^2) * aDenom
    ai = (c3i*(c2-2*c3r+c1)) * aDenom
    
    pd[aux] = float(fd*(1.+ar^2+ai^2)) > 0
    ps[aux] = float(2.*fs) > 0
endif
oblock[0,*,*] = pd 
oblock[2,*,*] = ps 

index = where(finite(oblock) eq 0,nr) ; eliminate unvalid (nan) pixels
if nr gt 0 then oblock[index] = 0.0

return, oblock
end




pro decomp_fredur,CALLED = called, PRECISE = precise, BLOCK = block
	common rat, types, file, wid, config


        if (not keyword_set(precise)) then precise = 0

        if (keyword_set(block)) then begin
            if (precise eq 0) then block = fredur_optimised(block)
            if (precise ne 0) then block = fredur_precise(block)
            return
        end


; check if array is usable

	if file.type ne 220 then begin
		error_button = DIALOG_MESSAGE(['Data has to be a','covariance matrix'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
            endif

            if (keyword_set(block)) then called = 1

	if not keyword_set(called) then begin             ; Graphical interface
		main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Freeman-Durden decomposition',/floating,/tlb_kill_request_events,/tlb_frame_attr)
		prec = cw_bgroup(main,[' Optimised calculation ',' Precise calculation '], set_value=0,/exclusive)
		buttons = WIDGET_BASE(main,column=3,/frame)
		but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
		but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
		but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
		widget_control, main, /REALIZE, default_button = but_canc,tlb_get_size=toto

		repeat begin                                        ; Event loop
			event = widget_event(main)
			if event.id eq but_info then begin               ; Info Button clicked
				infotext = ['FREEMAN-DURDEN DECOMPOSITION',$
				' ',$
				'RAT module written 08/2004 by Marc Jaeger & Andreas Reigber',$
				'',$
				'further information:',$
				'A. Freeman, and  S.L. Durden:  A Three-Component Scattering Model',$
				'for Polarimetric SAR Data, IEEE Transactions on Geoscience and',$
				'Remote Sensing, Vol. 36, No. 3, pp. 963-973, 1998']
				info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
			end
		endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
		widget_control,prec,GET_VALUE=precise               ; read widget fields
		widget_control,main,/destroy                        ; remove main widget
		if event.id ne but_ok then return                   ; OK button _not_ clicked
	endif else begin                                       ; Routine called with keywords
		if not keyword_set(precise) then precise = 0
	endelse

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	head = 1l
	rrat,file.name,ddd,header=head,info=info,type=type		
	srat,outputfile,eee,header=[3l,3l,file.xdim,file.ydim,4l],info=info,type=211l	
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

; pop up progress window

	progress,Message='Freeman-Durden Decomposition...',/cancel_button

; calculating decomposition
	
        for i=0,anz_blocks-1 do begin
            progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
            if wid.cancel eq 1 then return
            
            block  = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
            readu,ddd,block
            if (precise eq 0) then oblock = fredur_optimised(block)
            if (precise ne 0) then oblock = fredur_precise(block)

            writeu,eee,oblock
        endfor
	free_lun,ddd,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
	file.type = 211l
	file.dim  = 3l
	file.zdim = 3l
	file.vdim = 1l
	file.var  = 4l
	
; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif	
end
