;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: calib_xtalk
; written by    : Stephane Guillaso
; last revision : 20. September 2004
; Estimate and remove cross-talk parameters
;------------------------------------------------------------------------
; The contents of this file are subject to the Mozilla Public License
; Version 1.1 (the "License"); you may not use this file except in
; compliance with the License. You may obtain a copy of the License at
; http://www.mozilla.org/MPL/
;
; Software distributed under the License is distributed on an "AS IS"
; basis, WITHOUT WARRANTY OF Asmmy KIND, either express or implied. See the
; License for the specific language governing rights and limitations
; under the License.
;
; The Initial Developer of the Original Code is the RAT development team.
; All Rights Reserved.
;------------------------------------------------------------------------
function oap_calxtalk_k4,kvec,cal

   nrx = (size(kvec))[2]
   nry = (size(kvec))[3]

; Alpha calibration
   kvec[0,*,*] /= cal[4,*,*]
   kvec[1,*,*] *= cal[4,*,*]
   kvec[2,*,*] *= cal[4,*,*]
   kvec[3,*,*] /= cal[4,*,*]

; Crosstalk calibration
   norm = 1.0 / ((1.0-cal[0,*,*]*cal[2,*,*])^2 * (1.0-cal[1,*,*]*cal[3,*,*])^2)

   kvec[0,*,*] = norm*(kvec[0,*,*] * 1.0  +  kvec[1,*,*] * (cal[1,*,*]*cal[2,*,*])  -  kvec[2,*,*] * cal[1,*,*]   -  kvec[3,*,*] *  cal[2,*,*] )
   kvec[1,*,*] = norm*(kvec[0,*,*] * (cal[0,*,*]*cal[3,*,*])  +  kvec[1,*,*] * 1.0  -  kvec[2,*,*] * cal[0,*,*]   -  kvec[3,*,*] *  cal[3,*,*] )
   kvec[2,*,*] = norm*(-kvec[0,*,*] * cal[3,*,*]  -  kvec[1,*,*] * cal[2,*,*]  +  kvec[2,*,*] * 1.0   +  kvec[3,*,*] * (cal[2,*,*]*cal[3,*,*]) )
   kvec[3,*,*] = norm*(-kvec[0,*,*] * cal[0,*,*]  -  kvec[1,*,*] * cal[1,*,*]  +  kvec[2,*,*] * (cal[0,*,*]*cal[1,*,*])  +  kvec[3,*,*] *  1.0 )
;

   return,kvec
end

pro xcal_qual,covar
   nrx = (size(covar))[3]
   nry = (size(covar))[4]
   eta = fltarr(nrx,nry)
   snr = fltarr(nrx,nry)

   for i=0,nrx-1 do begin
      for j=0,nry-1 do begin
         e_val = la_eigenql(covar[*,*,i,j], /doub, eigen=e_vec)
         junk  = max(abs([0,0,1,-1]#e_vec), iq)
         snr[i,j]   = 10*alog10(e_val(2)/e_val(iq))
         eta[i,j]   = float((covar(2,2,i,j)+covar(3,3,i,j))-(covar(2,3,i,j)+covar(3,2,i,j))/2)
      endfor
   endfor
end

function oap_xtalk,covar

   nrx = (size(covar))[3]
   nry = (size(covar))[4]

   covar /= mean(abs(covar))

   dcal = dcomplexarr(5,nrx,nry)
   sig1 = complexarr(4,4,nrx,nry)
   sig2 = complexarr(4,4,nrx,nry)
   c1   = complexarr(4,4,nrx,nry)
   c2   = complexarr(4,4,nrx,nry)

   c_fact = 0.2

;------------------------
; Estimate initial alpha
;------------------------

   alpha  = abs(reform(covar[3,3,*,*]/covar[2,2,*,*]))^0.25 * exp(complex(0,atan(covar[2,3,*,*],/p)/2))
   dcal[4,*,*] = alpha

   sig = covar
   for iter=0,9 do begin

;--------------------------
; Calibrate alpha imbalance
;--------------------------

      sig[0,0,*,*] /= (abs(alpha)^2)
      sig[0,1,*,*] *= (alpha / conj(alpha))
      sig[0,2,*,*] *= (alpha / conj(alpha))
      sig[0,3,*,*] /= (abs(alpha)^2)
      sig[1,0,*,*] *= (conj(alpha) / alpha)
      sig[1,1,*,*] *= (abs(alpha)^2)
      sig[1,2,*,*] *= (abs(alpha)^2)
      sig[1,3,*,*] *= (conj(alpha) / alpha)
      sig[2,0,*,*] *= (conj(alpha) / alpha)
      sig[2,1,*,*] *= (abs(alpha)^2)
      sig[2,2,*,*] *= (abs(alpha)^2)
      sig[2,3,*,*] *= (conj(alpha) / alpha)
      sig[3,0,*,*] /= (abs(alpha)^2)
      sig[3,1,*,*] *= (alpha / conj(alpha))
      sig[3,2,*,*] *= (alpha / conj(alpha))
      sig[3,3,*,*] /= (abs(alpha)^2)

;--------------------------
; Estimate A and B parameters
;--------------------------

      A    = reform((sig[0,2,*,*] + sig[0,3,*,*]) / 2)
      B    = reform((sig[1,2,*,*] + sig[1,3,*,*]) / 2)

;--------------------------
; Calculate crosstalk parameters
;--------------------------

      X    = fltarr(8,nrx,nry)
      M    = fltarr(8,8,nrx,nry)

      foo  = sig[0,2,*,*] - A
      X[0,*,*] = real_part(foo)
      X[4,*,*] = imaginary(foo)
      foo  = sig[0,3,*,*] - A
      X[1,*,*] = real_part(foo)
      X[5,*,*] = imaginary(foo)
      foo  = sig[1,2,*,*] - B
      X[2,*,*] = real_part(foo)
      X[6,*,*] = imaginary(foo)
      foo  = sig[1,3,*,*] - B
      X[3,*,*] = real_part(foo)
      X[7,*,*] = imaginary(foo)
                                ;

      M[0,0,*,*] = 0.0
      M[1,0,*,*] = real_part(sig[2,2,*,*])
      M[2,0,*,*] = real_part(sig[0,1,*,*]+sig[3,2,*,*])
      M[3,0,*,*] = real_part(sig[0,0,*,*])
      M[4,0,*,*] = 0.0
      M[5,0,*,*] =-imaginary(-sig[2,2,*,*])
      M[6,0,*,*] =-imaginary(sig[0,1,*,*]-sig[3,2,*,*])
      M[7,0,*,*] =-imaginary(sig[0,0,*,*])

      M[0,1,*,*] = real_part(sig[0,0,*,*])
      M[1,1,*,*] = real_part(sig[0,1,*,*]+sig[2,3,*,*])
      M[2,1,*,*] = real_part(sig[3,3,*,*])
      M[3,1,*,*] = 0.0
      M[4,1,*,*] =-imaginary(sig[0,0,*,*])
      M[5,1,*,*] =-imaginary(sig[0,1,*,*]-sig[2,3,*,*])
      M[6,1,*,*] =-imaginary(-sig[3,3,*,*])
      M[7,1,*,*] = 0.0

      M[0,2,*,*] = real_part(sig[2,2,*,*])
      M[1,2,*,*] = 0.0
      M[2,2,*,*] = real_part(sig[1,1,*,*])
      M[3,2,*,*] = real_part(sig[1,0,*,*]+sig[3,2,*,*])
      M[4,2,*,*] =-imaginary(-sig[2,2,*,*])
      M[5,2,*,*] = 0.0
      M[6,2,*,*] =-imaginary(sig[1,1,*,*])
      M[7,2,*,*] =-imaginary(sig[1,0,*,*]-sig[3,2,*,*])

      M[0,3,*,*] = real_part(sig[1,0,*,*]+sig[2,3,*,*])
      M[1,3,*,*] = real_part(sig[1,1,*,*])
      M[2,3,*,*] = 0.0
      M[3,3,*,*] = real_part(sig[3,3,*,*])
      M[4,3,*,*] =-imaginary(sig[1,0,*,*]-sig[2,3,*,*])
      M[5,3,*,*] =-imaginary(sig[1,1,*,*])
      M[6,3,*,*] = 0.0
      M[7,3,*,*] =-imaginary(-sig[3,3,*,*])

      M[0,4,*,*] = 0.0
      M[1,4,*,*] = imaginary(sig[2,2,*,*])
      M[2,4,*,*] = imaginary(sig[0,1,*,*]+sig[3,2,*,*])
      M[3,4,*,*] = imaginary(sig[0,0,*,*])
      M[4,4,*,*] = 0.0
      M[5,4,*,*] = real_part(-sig[2,2,*,*])
      M[6,4,*,*] = real_part(sig[0,1,*,*]-sig[3,2,*,*])
      M[7,4,*,*] = real_part(sig[0,0,*,*])

      M[0,5,*,*] = imaginary(sig[0,0,*,*])
      M[1,5,*,*] = imaginary(sig[0,1,*,*]+sig[2,3,*,*])
      M[2,5,*,*] = imaginary(sig[3,3,*,*])
      M[3,5,*,*] = 0.0
      M[4,5,*,*] = real_part(sig[0,0,*,*])
      M[5,5,*,*] = real_part(sig[0,1,*,*]-sig[2,3,*,*])
      M[6,5,*,*] = real_part(-sig[3,3,*,*])
      M[7,5,*,*] = 0.0

      M[0,6,*,*] = imaginary(sig[2,2,*,*])
      M[1,6,*,*] = 0.0
      M[2,6,*,*] = imaginary(sig[1,1,*,*])
      M[3,6,*,*] = imaginary(sig[1,0,*,*]+sig[3,2,*,*])
      M[4,6,*,*] = real_part(-sig[2,2,*,*])
      M[5,6,*,*] = 0.0
      M[6,6,*,*] = real_part(sig[1,1,*,*])
      M[7,6,*,*] = real_part(sig[1,0,*,*]-sig[3,2,*,*])

      M[0,7,*,*] = imaginary(sig[1,0,*,*]+sig[2,3,*,*])
      M[1,7,*,*] = imaginary(sig[1,1,*,*])
      M[2,7,*,*] = 0.0
      M[3,7,*,*] = imaginary(sig[3,3,*,*])
      M[4,7,*,*] = real_part(sig[1,0,*,*]-sig[2,3,*,*])
      M[5,7,*,*] = real_part(sig[1,1,*,*])
      M[6,7,*,*] = 0.0
      M[7,7,*,*] = real_part(-sig[3,3,*,*])


      for i=0,nrx-1 do begin
         for j=0,nry-1 do begin
            foo = reform(x[*,i,j])
            bar = reform(m[*,*,i,j])
            if product(finite(foo),/int) eq 0 || product(finite(bar)) eq 0 then continue
            ludc,bar,index,/double
            foo = lusol(bar,index,foo,/double)
            dcal[0:3,i,j] += complex(foo[0:3],foo[4:7])*c_fact
         endfor
      endfor

      c_fact = c_fact^0.2
;--------------------------
; Calibrate crosstalk
;--------------------------

      c1[0,0,*,*] = 1.0
      c1[1,0,*,*] = dcal[1,*,*] * dcal[2,*,*]
      c1[2,0,*,*] = -dcal[1,*,*]
      c1[3,0,*,*] = -dcal[2,*,*]
      c1[0,1,*,*] = dcal[0,*,*] * dcal[3,*,*]
      c1[1,1,*,*] = 1.0
      c1[2,1,*,*] = -dcal[0,*,*]
      c1[3,1,*,*] = -dcal[3,*,*]
      c1[0,2,*,*] = -dcal[3,*,*]
      c1[1,2,*,*] = -dcal[2,*,*]
      c1[2,2,*,*] = 1.0
      c1[3,2,*,*] = dcal[2,*,*] * dcal[3,*,*]
      c1[0,3,*,*] = -dcal[0,*,*]
      c1[1,3,*,*] = -dcal[1,*,*]
      c1[2,3,*,*] = dcal[0,*,*] * dcal[1,*,*]
      c1[3,3,*,*] = 1.0
      for i=0,3 do for j=0,3 do c1[i,j,*,*] /= sqrt(reform((1-dcal[1,*,*] * dcal[3,*,*])*(1-dcal[0,*,*] * dcal[2,*,*])))

      c2[0,0,*,*] = 1.0
      c2[1,0,*,*] = conj(dcal[0,*,*] * dcal[3,*,*])
      c2[2,0,*,*] = -conj(dcal[3,*,*])
      c2[3,0,*,*] = -conj(dcal[0,*,*])
      c2[0,1,*,*] = conj(dcal[1,*,*] * dcal[2,*,*])
      c2[1,1,*,*] = 1.0
      c2[2,1,*,*] = -conj(dcal[2,*,*])
      c2[3,1,*,*] = -conj(dcal[1,*,*])
      c2[0,2,*,*] = -conj(dcal[1,*,*])
      c2[1,2,*,*] = -conj(dcal[0,*,*])
      c2[2,2,*,*] = 1.0
      c2[3,2,*,*] = conj(dcal[0,*,*] * dcal[1,*,*])
      c2[0,3,*,*] = -conj(dcal[2,*,*])
      c2[1,3,*,*] = -conj(dcal[3,*,*])
      c2[2,3,*,*] = conj(dcal[2,*,*] * dcal[3,*,*])
      c2[3,3,*,*] = 1.0
      for i=0,3 do for j=0,3 do c2[i,j,*,*] /= sqrt(reform((1-conj(dcal[1,*,*]*dcal[3,*,*]))*(1-conj(dcal[0,*,*]*dcal[2,*,*]))))

      sig = covar

      acal = dcal[4,*,*]
      sig[0,0,*,*] /= (abs(acal)^2)
      sig[0,1,*,*] *= (acal / conj(acal))
      sig[0,2,*,*] *= (acal / conj(acal))
      sig[0,3,*,*] /= (abs(acal)^2)
      sig[1,0,*,*] *= (conj(acal) / acal)
      sig[1,1,*,*] *= (abs(acal)^2)
      sig[1,2,*,*] *= (abs(acal)^2)
      sig[1,3,*,*] *= (conj(acal) / acal)
      sig[2,0,*,*] *= (conj(acal) / acal)
      sig[2,1,*,*] *= (abs(acal)^2)
      sig[2,2,*,*] *= (abs(acal)^2)
      sig[2,3,*,*] *= (conj(acal) / acal)
      sig[3,0,*,*] /= (abs(acal)^2)
      sig[3,1,*,*] *= (acal / conj(acal))
      sig[3,2,*,*] *= (acal / conj(acal))
      sig[3,3,*,*] /= (abs(acal)^2)


      sig = block_mm(c1,block_mm(sig,c2))

      sig[0,0,*,*] = float(sig[0,0,*,*])
      sig[1,1,*,*] = float(sig[1,1,*,*])
      sig[2,2,*,*] = float(sig[2,2,*,*])
      sig[3,3,*,*] = float(sig[3,3,*,*])

;print,dcal[*,71,29]
;--------------------------
; Update alpha
;--------------------------

      alpha = abs(reform(sig[3,3,*,*]/sig[2,2,*,*]))^0.25 * exp(complex(0,atan(sig[2,3,*,*],/p)/2.0))
      dcal[4,*,*] *= alpha

;--------------------------
; Rescale crosstalk parameters
;--------------------------

      dcal[1,*,*] /= alpha^2
      dcal[3,*,*] *= alpha^2
   endfor

   return,complex(dcal)

end


pro calib_xtalkoap, CALLED=CALLED, SMMX=smmx,SMMY=smmy, EXCLUDEPIX=excludepix, CALFILE=calfile

   common rat, types, file, wid, config, tiling

   if not keyword_set(smmx) then smmx = 16 ; Routine called with keywords
   if not keyword_set(smmy) then smmy = 128 ; Default values
   if not keyword_set(excludepix) then excludepix = 0 ; Default values

   if not keyword_set(flag_chck) then flag_chck = ~keyword_set(CALLED) ; Default values
   if not keyword_set(flag_calf) then flag_calf = 0 ; Default values
   if n_elements(calfile) eq 0 then begin
      calfile = ''
   endif else begin
      flag_calf = 1
   endelse

; ---- CHECK IF ARRAY IS USABLE ----

   if file.type ne 200 then begin
      error_button = DIALOG_MESSAGE(['Data has to be a','lexicographic scattering vector'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif

   if file.zdim ne 4 then begin
      error_button = DIALOG_MESSAGE(['Data has to be','4 parameter vector','HH,VV,HV,VH'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif

   if not keyword_set(called) then begin ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='PolSAR x-talk calibration (OAP)',/floating,/tlb_kill_request_events,/tlb_frame_attr)
      field1   = CW_FIELD(main,VALUE=smmx,/integer,TITLE='Calibration window size x     : ',XSIZE=3)
      field2   = CW_FIELD(main,VALUE=smmy,/integer,TITLE='Calibration window size y     : ',XSIZE=3)
      field3   = CW_FIELD(main,VALUE=excludepix,/integer,TITLE='Exclude % of brightest pixels : ',XSIZE=3)
      chck_field  = CW_BGROUP(main, ['Check calibration a-posteriori'],YPAD=5,/ROW,/NONEXCLUSIVE,SET_VALUE=[flag_chck])
;		symm_field  = CW_BGROUP(main, ['Cross-polar symmetrisation'],YPAD=5,/ROW,/NONEXCLUSIVE,SET_VALUE=[flag_symm])

      cffield1 = CW_BGROUP(main, ['Generate calibration file (*.ps):'],YPAD=5,/ROW,/NONEXCLUSIVE,SET_VALUE=[flag_calf])
      cffield = WIDGET_BASE(main,column=2,sensitive=flag_calf)
      cffield2 = CW_FIELD(cffield,VALUE=calfile,/STRING,XSIZE=60,TITLE='')
      cffield3 = WIDGET_BUTTON(cffield,VALUE='browse',YSIZE=35)


      buttons  = WIDGET_BASE(main,column=3,/frame)
      but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
      but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
      but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
      WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
      pos = center_box(toto[0],drawysize=toto[1])
      widget_control, main, xoffset=pos[0], yoffset=pos[1]

      repeat begin
         event = widget_event(main)
         if event.id eq but_info then begin ; Info Button clicked
            infotext = ['POLSAR XTALK CALIBRATION (OAP)',$
                        ' ',$
                        'RAT module written 02/2008 by Andreas Reigber',$
                        ' ',$
                        'further information:',$
                        'T.L. Ainsworth et.al.: Orientation Angle Preserving A Posteriori',$
                        'Polarimetric SAR Calibration, IEEE Transactions on Geoscience  ',$
                        'and Remote Sensing, Vol.44, No. 4, pp. 994-1003, 2006']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
         end

                                ; get the filename
         if event.id eq cffield3 then begin
            path = config.workdir
            calfile = dialog_pickfile(title='Select output calfile',dialog_parent=wid.base, filter = '*.ps',path=path,get_path=path)
            if strlen(calfile) gt 0 then config.workdir = path
            widget_control,cffield2,set_value=calfile
         endif

                                ; save intermediaire behaviour
         if event.id eq cffield1 then begin
            widget_control,cffield,sensitive=event.select
            flag_calf = event.select
         endif

      endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
      widget_control,field1,GET_VALUE=smmx
      widget_control,field2,GET_VALUE=smmy
      widget_control,field3,GET_VALUE=excludepix
      widget_control,cffield2,GET_VALUE=calfile
      widget_control,chck_field,GET_VALUE=flag_chck
      widget_control,chck_field,GET_VALUE=flag_calf

      widget_control,main,/destroy ; remove main widget
      if event.id ne but_ok then return ; OK button _not_ clicked
   endif

;------------------------------------------------------------
; change mousepointer to hourglass (FIXED)
;------------------------------------------------------------

   WIDGET_CONTROL,/hourglass

; undo function

   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

   head = 1l
   rrat,file.name, ddd,header=head,info=info,type=type
   srat,outputfile,eee,header=head,info=info,type=type

; Clarify blocksizes...

   bs_save = config.blocksize
   if config.blocksize lt smmy*4 then config.blocksize = smmy*4
   while (config.blocksize / smmy) * smmy ne config.blocksize do config.blocksize++
   bs = 0

; Initialise tiling & progess bar

   tiling_init,overlap=smmy
   progress,Message='PolSAR x-talk calibration...',/cancel_button

; calculating new dimensions

   xend = file.xdim / smmx * smmx - 1
   xnew = file.xdim / smmx
   dnew = file.zdim

; initialise parameters

   xrg1 = complexarr(5,file.xdim)
   xrg2 = complexarr(5,file.xdim)
   pxrg = 0

; start block processing

   for i=0,tiling.nr_blocks-1 do begin
      progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
      if wid.cancel eq 1 then return
      tiling_read,ddd,i,block

      bsy = (*tiling.blocksizes)[i]
      if i eq tiling.nr_blocks-1 and (bsy/smmy)*smmy ne bsy then begin
         bs     = bsy
         while (bs / smmy) * smmy ne bs do bs++
         foo = complexarr(1,dnew,file.xdim,bs)
         foo[0,0,0,0] = block
         for j=bsy,bs-1 do foo[*,*,*,j] = foo[*,*,*,(bsy-1-j)>0]
         block = foo
         foo = 0
         bsy = bs
      endif

; remove brightest 5% of pixels
      block = reform(block,/overwrite)

      if excludepix gt 0 then begin
         iblock = block
         foo   = total(abs(block),1)
         hist  = histogram(foo,loc=l,nbins=1000)
         for k=1,999 do hist[k] += hist[k-1]
         bar   = max(hist) * 0.95
         cut = 0 & while hist[cut] lt bar do cut++
         aux = where(foo gt l[cut],nr)
         if nr gt 0 then block[4*aux] = complex(0,0) & block[4*aux+1] = complex(0,0)  & block[4*aux+2] = complex(0,0) & block[4*aux+3] = complex(0,0)
      endif

; generate covariances
      covar = block_xprod(block,conj(block))
      covar = complex(rebin(real_part(covar[*,*,0:xend,*]),dnew,dnew,xnew,bsy/smmy),rebin(imaginary(covar[*,*,0:xend,*]),dnew,dnew,xnew,bsy/smmy))

; interpolate & calibrate scattering vector

      xcal  = complexarr(5,file.xdim,bsy)
;  	xcal[0,0,0] = congrid(oap_xtalk(covar),5,xend+1,bsy,cubic=-0.5)
      xcal[0,0,0] = congrid(oap_xtalk(covar),5,file.xdim,bsy,cubic=-0.5)
  
      if excludepix gt 0 then block = oap_calxtalk_k4(iblock,xcal) else block = oap_calxtalk_k4(block,xcal)

      if flag_chck then begin
         covar = block_xprod(block,conj(block))
         covar = complex(rebin(real_part(covar[*,*,0:xend,*]),dnew,dnew,xnew,bsy/smmy),rebin(imaginary(covar[*,*,0:xend,*]),dnew,dnew,xnew,bsy/smmy))


         xcal2  = complexarr(5,file.xdim,bsy)
;			xcal2[0,0,0]  = congrid(oap_xtalk(covar),5,xend+1,bsy,cubic=-0.5)
         xcal2[0,0,0]  = congrid(oap_xtalk(covar),5,file.xdim,bsy,cubic=-0.5)

;			xcal_qual,covar


      endif


      if flag_chck then begin
         check1 = total(total(abs(xcal[0:3,*,*]),2),1) / 4.0 / file.xdim
         check2 = total(total(abs(xcal2[0:3,*,*]),2),1) / 4.0 / file.xdim
         valid  = where(check1 gt 0.0 and check1 lt 1.0 and check2 gt 0.0 and check2 lt 1.0,nr)
      endif else begin
         check1 = total(total(abs(xcal[0:3,*,*]),2),1) / 4.0 / file.xdim
         valid  = where(check1 gt 0.0 and check1 lt 1.0,nr)
      endelse
      if nr gt 0 then	begin
         xrg1 += total(xcal[*,*,valid],3)
         pxrg += nr
         if flag_chck then xrg2 += total(xcal2[*,*,valid],3)
      endif

;       wins,0
;       !p.multi = [0,2,3]
;       tek_color
;       plot,(abs(xrg1[4,*]/pxrg)),yrange=[0.5,1.5],title='abs(a)'
;       if flag_chck then oplot,(abs(xrg2[4,*]/pxrg)),color=3
;       phase = (atan(xrg1[4,*],/phase))*!radeg
;       if flag_chck then phase = [phase,(atan(xrg2[4,*],/phase))*!radeg]
;       plot,(atan(xrg1[4,*],/phase))*!radeg,title='phase(a)',yrange=[min([0,min(phase)]),max([0,max(phase)])]
;       if flag_chck then oplot,(atan(xrg2[4,*],/phase))*!radeg,color=3
;       plot,10*alog10(abs(xrg1[0,*]/pxrg)),yrang=[-50,0],title='u',ytitle='dB'
;       if flag_chck then oplot,10*alog10(abs(xrg2[0,*]/pxrg)),color=3
;       plot,10*alog10(abs(xrg1[1,*]/pxrg)),yrang=[-50,0],title='v',ytitle='dB'
;       if flag_chck then oplot,10*alog10(abs(xrg2[1,*]/pxrg)),color=3
;       plot,10*alog10(abs(xrg1[2,*]/pxrg)),yrang=[-50,0],title='w',ytitle='dB'
;       if flag_chck then oplot,10*alog10(abs(xrg2[2,*]/pxrg)),color=3
;       plot,10*alog10(abs(xrg1[3,*]/pxrg)),yrang=[-50,0],title='z',ytitle='dB'
;       if flag_chck then oplot,10*alog10(abs(xrg2[3,*]/pxrg)),color=3
;       loadct,0,/silent
;       !p.multi = [0,1,1]

      if bs ne 0 then block=reform(block[*,*,0:(*tiling.blocksizes)[i]-1],1,dnew,file.xdim,(*tiling.blocksizes)[i]) else block = reform(block,1,dnew,file.xdim,bsy)
      tiling_write,eee,i,temporary(block)
      tiling_jumpback,ddd
   endfor
   free_lun,ddd,eee



   if	config.os eq 'windows' then newline = string(13B) + string(10B)
   if	config.os eq 'unix' then newline = string(10B)

   am1 = mean(abs(xrg1[4,*]/pxrg))
   pm1 = atan(mean(exp(complex(0,atan(xrg1[4,*],/phase)))),/phase)*!radeg
   um1 = 10*alog10(mean(abs(xrg1[0,*]/pxrg)))
   vm1 = 10*alog10(mean(abs(xrg1[1,*]/pxrg)))
   wm1 = 10*alog10(mean(abs(xrg1[2,*]/pxrg)))
   zm1 = 10*alog10(mean(abs(xrg1[3,*]/pxrg)))
   if flag_chck then begin
      am2 = mean(abs(xrg2[4,*]/pxrg))
      pm2 = atan(mean(exp(complex(0,atan(xrg2[4,*],/phase)))),/phase)	*!radeg
      um2 = 10*alog10(mean(abs(xrg2[0,*]/pxrg)))
      vm2 = 10*alog10(mean(abs(xrg2[1,*]/pxrg)))
      wm2 = 10*alog10(mean(abs(xrg2[2,*]/pxrg)))
      zm2 = 10*alog10(mean(abs(xrg2[3,*]/pxrg)))
   endif

   caltext = 'Calibration report:'

   caltext = caltext + newline + 'abs(a) ='+strcompress(am1)+'  '
   if flag_chck then caltext = caltext + '  ('+strcompress(am2,/remove)+'   after calibration)'
   caltext = caltext + '       arg(a) ='+strcompress(pm1)+'deg'
   if flag_chck then caltext = caltext + ' ('+strcompress(pm2,/remove)+'deg after calibration)'

   caltext = caltext + newline + 'abs(u) ='+strcompress(um1)+'dB'
   if flag_chck then caltext = caltext + '  ('+strcompress(um2,/remove)+'dB after calibration)'
   caltext = caltext + '       abs(z) ='+strcompress(zm1)+'dB'
   if flag_chck then caltext = caltext + '  ('+strcompress(zm2,/remove)+'dB after calibration)'

   caltext = caltext + newline + 'abs(v) ='+strcompress(vm1)+'dB'
   if flag_chck then caltext = caltext + '  ('+strcompress(vm2,/remove)+'dB after calibration)'
   caltext = caltext + '       abs(w) ='+strcompress(wm1)+'dB'
   if flag_chck then caltext = caltext + '  ('+strcompress(wm2,/remove)+'dB after calibration)'


   if not keyword_set(called) then begin ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=10,TITLE='Calibration result',/floating,/tlb_kill_request_events,/tlb_frame_attr)
      dr1  = widget_draw(main,XSIZE=800,ysize=500)
      cl1  = widget_text(main,XSIZE=110, YSIZE=4, VALUE=caltext)
      but_ok   = WIDGET_BUTTON(main,VALUE=' OK ',/frame)
      WIDGET_CONTROL, main, /REALIZE,tlb_get_size=toto

      !p.multi = [0,2,3]
      tek_color

      plot,(abs(xrg1[4,*]/pxrg)),yrange=[0.5,1.5],title='abs(a)'
      if flag_chck then oplot,(abs(xrg2[4,*]/pxrg)),color=3

      phase = (atan(xrg1[4,*]/pxrg,/phase))*!radeg
      if flag_chck then phase = [phase,(atan(xrg2[4,*]/pxrg,/phase))*!radeg]
      plot,(atan(xrg1[4,*]/pxrg,/phase))*!radeg,title='phase(a)',yrange=[min([0,min(phase)]),max([0,max(phase)])]
      if flag_chck then oplot,(atan(xrg2[4,*]/pxrg,/phase))*!radeg,color=3

      plot,10*alog10(abs(xrg1[0,*]/pxrg)),yrang=[-50,0],title='u',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[0,*]/pxrg)),color=3

      plot,10*alog10(abs(xrg1[1,*]/pxrg)),yrang=[-50,0],title='v',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[1,*]/pxrg)),color=3

      plot,10*alog10(abs(xrg1[2,*]/pxrg)),yrang=[-50,0],title='w',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[2,*]/pxrg)),color=3

      plot,10*alog10(abs(xrg1[3,*]/pxrg)),yrang=[-50,0],title='z',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[3,*]/pxrg)),color=3

      loadct,0,/silent
      !p.multi = [0,1,1]

      repeat begin              ; Event loop
         event = widget_event(main)
      endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
      widget_control,main,/destroy ; remove main widget
      widget_control,wid.draw,get_value=index
      wset,index
   endif

   if flag_calf && (strlen(calfile) gt 0) then begin

      set_plot,'ps'
      device,filename=calfile,/color,/encaps,xsize=21,ysize=29.7

      !p.multi = [0,2,4]
      tek_color

      plot,(abs(xrg1[4,*]/pxrg)),yrange=[0.5,1.5],title='abs(a)'
      if flag_chck then oplot,(abs(xrg2[4,*]/pxrg)),color=3

      phase = (atan(xrg1[4,*]/pxrg,/phase))*!radeg
      if flag_chck then phase = [phase,(atan(xrg2[4,*]/pxrg,/phase))*!radeg]
      plot,(atan(xrg1[4,*]/pxrg,/phase))*!radeg,title='phase(a)',yrange=[min([0,min(phase)]),max([0,max(phase)])]
      if flag_chck then oplot,(atan(xrg2[4,*]/pxrg,/phase))*!radeg,color=3

      plot,10*alog10(abs(xrg1[0,*]/pxrg)),yrang=[-50,0],title='u',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[0,*]/pxrg)),color=3

      plot,10*alog10(abs(xrg1[1,*]/pxrg)),yrang=[-50,0],title='v',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[1,*]/pxrg)),color=3

      plot,10*alog10(abs(xrg1[2,*]/pxrg)),yrang=[-50,0],title='w',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[2,*]/pxrg)),color=3

      plot,10*alog10(abs(xrg1[3,*]/pxrg)),yrang=[-50,0],title='z',ytitle='dB'
      if flag_chck then oplot,10*alog10(abs(xrg2[3,*]/pxrg)),color=3

      loadct,0,/silent
      !p.multi = [0,1,1]

      xyouts,0.1,0.2,'Calibration report:',/normal,charthick=2
      xyouts,0.1,0.18,'abs(a) ='+strcompress(am1),/normal
      xyouts,0.1,0.16,'arg(a) ='+strcompress(pm1)+' deg',/normal
      xyouts,0.1,0.14,'abs(u) ='+strcompress(um1)+' dB',/normal
      xyouts,0.1,0.12,'abs(v) ='+strcompress(vm1)+' dB',/normal
      xyouts,0.1,0.10,'abs(w) ='+strcompress(wm1)+' dB',/normal
      xyouts,0.1,0.08,'abs(z) ='+strcompress(zm1)+' dB',/normal

      if flag_chck then begin
         xyouts,0.4,0.18,'(abs(a) ='+strcompress(am2)+')',/normal
         xyouts,0.4,0.16,'(arg(a) ='+strcompress(pm2)+' deg)',/normal
         xyouts,0.4,0.14,'(abs(u) ='+strcompress(um2)+' dB)',/normal
         xyouts,0.4,0.12,'(abs(v) ='+strcompress(vm2)+' dB)',/normal
         xyouts,0.4,0.10,'(abs(w) ='+strcompress(wm2)+' dB)',/normal
         xyouts,0.4,0.08,'(abs(z) ='+strcompress(zm2)+' dB)',/normal
      endif
      device,/close
      if	config.os eq 'windows' then set_plot,"win"
      if	config.os eq 'unix' then set_plot,"x"
   endif

; update everything
   config.blocksize = bs_save

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute,'PolSAR x-talk calibration (OAP)...'

end








