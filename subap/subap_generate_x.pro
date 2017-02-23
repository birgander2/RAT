;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: subap_generate
; written by    : Maxim Neumann
; last revision : 16.Sept.2005
; Generates Sub-Apertures from given scattering vectors.
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

function subap_subapx, a, res=res, n_ap=n_ap, OUTPRINT=OUTPRINT, DORESIZE=DORESIZE, SIZ_RES=SIZ_RES,ERROR=ERROR, HAMMING=hamming, FLATTEN=flatten
  common rat, types, file, wid, config
  eps = (MACHAR()).EPS
;------------------------------------------------------------------------
; - Calculate the spectrum in X and Y direction
;------------------------------------------------------------------------
  siz   = size(a,/dim)
  n     = siz[n_elements(siz)-2:*]
  n_ch  = (n_elements(siz) le 2? 1 : fix(product(siz[0:n_elements(siz)-3])))
  n_x = n[0]
  n_y = n[1]
 
  if keyword_set(OUTPRINT) then print,'FFT in azimuth direction'
  fa    = fft(a,-1,dim=n_elements(siz)-1) ; rg_dim = az_dim-1

  fa    = reform(fa,[n_ch,n],/overwrite)
  fl_X  = total(abs(fa),1)/n_ch
  fl_X  = total(fl_x,2)/n[1]

	fl_x = ts_smooth(fl_x,100)
	fl_x /= max(fl_x)
	filter = 1/fl_x
	rmnanq,filter
	ind = where(filter gt 10.0,nind)
	if nind gt 0 then filter[ind] = 0.0
	if keyword_set(flatten) then for i=0,n_ch-1 do for j=0,n_y-1 do fa[i,*,j] *= filter

	n_tot   = n_x-nind
	len_sub = n_tot / floor(100.0 / res)
	siz_res = 2^(floor(alog(n_x / floor(100.0 / res))/alog(2))+1)  ; next higher 2^n
	stp_sub = floor(findgen(n_ap)*float(n_x-len_sub)/n_ap-1)+max(ind)
	
	subfilter = fltarr(n_x)
	if keyword_set(hamming) then subfilter[0] = hanning(len_sub,alpha=0.54) else subfilter[0] = hanning(len_sub,alpha=1.00)
;------------------------------------------------------------------------
; - Perform the sub-aperture decomposition
;------------------------------------------------------------------------
;;; - generate the new image output
  if keyword_set(DORESIZE) then newim = complexarr(n_ch,n_ap,siz_res,n[1]) else newim = complexarr(n_ch,n_ap,n[0],n[1])
  for ap=0,n_ap-1 do begin
  	  
	  filter = shift(subfilter,stp_sub[ap])
     if keyword_set(OUTPRINT) then print,format='(A,$)','sub image #'+strcompress(ap+1,/rem)+' '
     for ch=0,n_ch-1 do begin
        progress,percent=(ap*n_ch+ch)*100.0/(n_ap*n_ch),/check_cancel
        if wid.cancel eq 1 then begin
           ERROR = 1
           return, 0
        endif
        if keyword_set(OUTPRINT) then print,format='(A,$)',' ch'+strcompress(ch,/rem)
        
		  tmpch = reform(fa[ch,*,*])
		  for i=0,n_y-1 do tmpch[*,i] *= filter
        
		  if keyword_set(DORESIZE) then begin
           tmpch = shift(tmpch,-stp_sub[ap]+(siz_res-len_sub)/2,0)
           tmpch = tmpch[0:siz_res-1,*]
           tmpch = shift(tmpch,-siz_res/2,0)
        endif else begin
           tmpch = shift(tmpch,-stp_sub[ap]-len_sub/2,0)
		  endelse
        newim[ch,ap,*,*] = fft(tmpch,1,dim=1)
     endfor
     if keyword_set(OUTPRINT) then $
        print,'   done'
  endfor
  return, newim
end


pro subap_generate_x,CALLED = called
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag

; right type? --- calculate new type
  if file.type eq 101L then newtype = 600L $
  else if file.type eq 300L then newtype = 601L $
  else if (file.type ge 200L && file.type le 210L) then newtype = 601L $
  else if (file.type ge 500L && file.type le 501L) then newtype = 601L $
  else begin
     error_button = DIALOG_MESSAGE(['Data has to be in scalar or scattering vector format'], $
                                   DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endelse

  if not keyword_set(called) then begin ; Graphical interface
     main = WIDGET_BASE(GROUP_LEADER=wid.base,row=5,TITLE='Sub-aperture generation in x',/floating,/tlb_kill_request_events,/tlb_frame_attr)

     fld_res = CW_FIELD(main,VALUE=20,/float,TITLE='Resolution (in %)   : ',XSIZE=4)
     fld_nap = CW_FIELD(main,VALUE=8,/integer,TITLE='Number of aperutres : ',XSIZE=4)
     fld_flatten = cw_bgroup(main,set_value=1,"Flatten original spectrum",/nonexclusive)
     fld_hamming = cw_bgroup(main,set_value=1,"Hamming weighting on subapertures",/nonexclusive)
     fld_resize = cw_bgroup(main,set_value=0,"Reduce size proportionaly to resolution loss",/nonexclusive)
	  text = widget_label(main,value='Warning: Memory extensive!!')
     buttons   = WIDGET_BASE(main,column=3,/frame)
     but_ok    = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc  = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info  = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     WIDGET_CONTROL, main,/REALIZE,default_button=but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['SUB APERTURE GENERATION',$
                       ' ',$
                       'RAT module written 11/2005 by Stephane Guillaso, Maxim Neumann and Andreas Reigber']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
        endif
;         if event.id eq fld_resize then begin
;            infotext = ['Not yet implemented!']
;            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
;                                  TITLE='Information')
;            widget_control,fld_resize,set_value=0
;         endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
     widget_control,fld_res,GET_VALUE=res ; read widget fields
     widget_control,fld_nap,GET_VALUE=n_ap
     widget_control,fld_resize,GET_VALUE=resize ; one scattering mechanism
     widget_control,fld_flatten,GET_VALUE=flatten ; 
     widget_control,fld_hamming,GET_VALUE=hamming ; 
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif else begin              ; Routine called with keywords
     res    = 20.
     n_ap   = 5
     resize = 0
  endelse

;;; change mousepointer
  WIDGET_CONTROL,/hourglass
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

;;; read / write header
  head = 1l
  rrat,file.name,ddd,header=head,info=info,type=type
  info += ' subap'+strcompress(n_ap,/r)+'x'+strcompress(fix(res),/R)+'%'
  case head[0] of
     2: newdim=[3L,n_ap,head[1:2]]
     3: newdim=[4L,head[1],n_ap,head[2:3]]
     4: newdim=[4L,product(head[1:2]),n_ap,head[3:4]]
  endcase
  n_ch = (newdim[0] eq 3L?1L:newdim[1]) ; nr of channels
;;;  srat,outputfile... will come later for a change! 
;;;  srat,outputfile,eee,header=[newdim,file.var],info=info,type=newtype

;;; calculating preview size and number of blocks
;;; no block processing ;(   [azimuth-fft]

; pop up progress window
  progress,Message='Generate Sub-apertures...',/cancel_button

  block  = make_array([file.vdim,file.zdim,file.xdim,file.ydim],type=file.var)
  readu,ddd,block
  oblock = subap_subapx(block,res=res,n_ap=n_ap,DORESIZE=resize,SIZ_RES=siz_res,ERROR=ERROR,hamming=hamming,flatten=flatten)
  if keyword_set(ERROR) then return
  if keyword_set(RESIZE) then newdim[n_elements(newdim)-2] = siz_res
  srat,outputfile,eee,header=[newdim,file.var],info=info,type=newtype
  writeu,eee,oblock
  free_lun,ddd,eee
  oblock = 0.0
  	
; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = newdim[0]
  file.vdim = n_ch
  file.zdim = n_ap
  file.xdim = newdim[n_elements(newdim)-2] 
  file.type = newtype
  file.info = info
  
; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy

end

