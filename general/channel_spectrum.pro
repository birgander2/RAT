;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: channel_spectrum
; written by    : Maxim Neumann
; last revision : 10.Nov.2005 (StŽphane Guillaso)
; Presents the spectrum to all channels.
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

pro channel_spectrum
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag

;---- TEST IF A DATASET IS LOADED ----
	if file.type eq 0 then begin
		error_button = DIALOG_MESSAGE(['Please load data'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif
	
;---- DISPLAY CHOSEN METHOD ----
  main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Channel spectrum - Choose method',/floating,/tlb_kill_request_events,/tlb_frame_attr)
  toto = ['1) Calcul all spectra (time consuming, but faster display)',$
          '2) Calcul desired spectrum (use only if you want to display few spectra)']
  butt = cw_bgroup(main,toto,/column,/exclusive,/no_release,set_value=0)
  buttons  = WIDGET_BASE(main,/row,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' Start ',xsize=80,/frame)
  but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=pos[0], yoffset=pos[1]
  repeat begin
     event = widget_event(main)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['CHANNEL SPECTRUM',$
                    ' ',$
                    'RAT module written 09/2005 by Maxim Neumann', $
                    'Process improved in 11/2005 by Stephane Guillaso', $
                    ' ', $
                    ' ', $
                    'This tool becomes more interesting for sub-apertures!']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
     endif
  endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  widget_control,butt,get_value=method
  widget_control,main,/destroy

	if event.id ne but_ok then return
;-> test data


  rrat,file.name,arr
  siz  = size(arr,/dim)
  case n_elements(siz) of
     2: begin 
        vdim=1 & zdim=1 & n=siz
     end
     3: begin
        vdim=1 & zdim=siz[0] & n=siz[1:2]
     end
     4: begin
        vdim=siz[0] & zdim=siz[1] & n=siz[2:3]
     end
  endcase

	; -> define new variable
  arr  = REFORM(arr,[vdim*zdim,n],/OVERWRITE)
  far  = fltarr(n[0],n[1])
  
; -> Considering that image will be displayed in 256x256 define new array variable
  img  = bytarr(vdim*zdim,256,256) ; display image (b/w image, using rat_tv form)
  f    = bytarr(vdim*zdim,256,256) ; display the whole fft image
  f_Y  = bytarr(vdim*zdim,256,256) ; display the Y fft image
  f_X  = bytarr(vdim*zdim,256,256) ; display the X fft image
  f_Yl = fltarr(vdim*zdim,n[1])    ; mean fft over Y direction
  f_Xl = fltarr(vdim*zdim,n[0])    ; mean fft over X direction
  flag = bytarr(vdim*zdim)
	

	if method eq 0 then begin
		t = systime(1)
		progress,Message='Calculating all spectra...',submessage='spectra:'
	
		for ch=0,vdim*zdim-1 do begin
 			progress,percent=(ch+1)*100.0/(vdim*zdim),submessage='spectra: '+STRCOMPRESS(ch+1,/REM),$
 				subpercent=1.0
 			flag[ch] = 1
	
 			;-> set the flag to 1
			a = reform(arr[ch,*,*])
					
			;-> Calculate fft
			fa = scale2d(abs(fft(a,-1)),256,256);,cubic=-0.5)
			mfa = mean(fa)
			f[ch,*,*] = bytscl(fa,0,2.5*mfa)
					
			;-> calculate fft X
			for yy=0,n[1]-1 do begin
				if yy mod 100 eq 0 then progress,subpercent=(yy+1)*100.0/(TOTAL(n))
				far[*,yy] = abs(fft(reform(a[*,yy]),-1))
			endfor
			f_Xl[ch,*] = total(far,2)/n[1]
			fa = scale2d(far,256,256);,cubic=-0.5)
			mfa = mean(fa)
			f_X[ch,*,*] = bytscl(fa,0,2.5*mfa)
					
			;-> calculate fft Y
			for xx=0,n[0]-1 do begin
				if xx mod 100 eq 0 then progress,subpercent=(n[1]+xx+1)*100.0/(TOTAL(n))
				far[xx,*] = abs(fft(reform(a[xx,*]),-1))
			endfor
			f_Yl[ch,*] = total(far,1)/n[0]
			fa = scale2d(far,256,256);,cubic=-0.5)
			mfa = mean(fa)
			f_Y[ch,*,*] = bytscl(fa,0,2.5*mfa)
					
			a = scale2d(abs(a)^0.7,256,256);,cubic=-0.5)
			ma = mean(a)
			img[ch,*,*] = bytscl(a,0,2.5*ma)
		endfor
		progress,/destroy
	endif

  main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Channel spectrum',/floating,/tlb_kill_request_events,/tlb_frame_attr)
  wins = WIDGET_BASE(main,column=3)

  wins1 = widget_base(wins,row=3)
  text1 = widget_label(wins1,value="Span & Whole image spectrum")
  win_span = cw_rat_draw(wins1,256,256)
  win_fft  = cw_rat_draw(wins1,256,256)

  wins2 = widget_base(wins,row=3)
  text2 = widget_label(wins2,value="Y spectrum")
  win_azL = cw_rat_draw(wins2,256,256)
  win_azF = cw_rat_draw(wins2,256,256)

  wins3 = widget_base(wins,row=3)
  text3 = widget_label(wins3,value="X spectrum")
  win_rgL = cw_rat_draw(wins3,256,256)
  win_rgF = cw_rat_draw(wins3,256,256)

  text = widget_label(main,value='Select channel:')
  butt = cw_bgroup(main,channel_names,/exclusive,column=file.zdim,/no_release)
  buttons  = WIDGET_BASE(main,column=3,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' Close ',xsize=80,/frame)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=pos[0], yoffset=pos[1]

  widget_control,win_span,GET_VALUE=winspan
  widget_control,win_fft, GET_VALUE=winfft
  widget_control,win_azL, GET_VALUE=winYl
  widget_control,win_azF, GET_VALUE=winYf
  widget_control,win_rgL, GET_VALUE=winXl
  widget_control,win_rgF, GET_VALUE=winXf

  repeat begin
     event = widget_event(main)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['CHANNEL SPECTRUM',$
                    ' ',$
                    'RAT module written 09/2005 by Maxim Neumann', $
                    'Process improved in 11/2005 by Stephane Guillaso', $
                    ' ', $
                    ' ', $
                    'This tool becomes more interesting for sub-apertures!']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
     endif
     if event.id eq butt then begin
; change mousepointer
        WIDGET_CONTROL,/hourglass
        widget_control,butt,GET_VALUE=ch
        ;print,ch
				
				;-> Test if the image has already be displayed
				if flag[ch] ne 1 then begin
          
          ;-> set the flag to 1
          flag[ch]=1
          progress,message='Calculate spectrum: '+strcompress(ch+1,/rem)
					a = reform(arr[ch,*,*])
					
					;-> Calculate fft
					fa = scale2d(abs(fft(a,-1)),256,256);,cubic=-0.5)
					mfa = mean(fa)
					f[ch,*,*] = bytscl(fa,0,2.5*mfa)
					
					;-> calculate fft X
					for yy=0,n[1]-1 do begin
						if yy mod 100 eq 0 then progress,percent=(yy+1)*100.0/(TOTAL(n))
						far[*,yy] = abs(fft(reform(a[*,yy]),-1))
					endfor
					f_Xl[ch,*] = total(far,2)/n[1]
					fa = scale2d(far,256,256);,cubic=-0.5)
					mfa = mean(fa)
					f_X[ch,*,*] = bytscl(fa,0,2.5*mfa)
					
					;-> calculate fft Y
					for xx=0,n[0]-1 do begin
						if xx mod 100 eq 0 then progress,percent=(n[1]+xx+1)*100.0/(TOTAL(n))
						far[xx,*] = abs(fft(reform(a[xx,*]),-1))
					endfor
					f_Yl[ch,*] = total(far,1)/n[0]
					fa = scale2d(far,256,256);,cubic=-0.5)
					mfa = mean(fa)
					f_Y[ch,*,*] = bytscl(fa,0,2.5*mfa)
					
					a = scale2d(abs(a)^0.7,256,256);,cubic=-0.5)
					ma = mean(a)
					img[ch,*,*] = bytscl(a,0,2.5*ma)
					progress,/destroy
				endif
        wset,winspan
        tv,reform(img[ch,*,*])
				
        wset,winfft
        tv,reform(f[ch,*,*])

        wset,winXL
        plot,reform(f_Xl[ch,*]),xstyle=1,ystyle=1

        wset,winXf
        tv,reform(f_X[ch,*,*])

        wset,winYL
        plot,reform(f_Yl[ch,*]),xstyle=1,ystyle=1

        wset,winYf
        tv,reform(f_Y[ch,*,*])


;
;
;        wset,winazf
;        img = congrid(reform(abs(faz[ch,*,*])),256,256)
;        mm=mean(img)
;        img=bytscl(img,0,2.5*mm)
;        tv,img
;
				;display images

;        img = congrid(reform(abs(arr[ch,*,*])),256,256)
;        mm=mean(img)
;        img=bytscl(img,0,2.5*mm)
;        if flag[ch] ne 1 then begin
;           flag[ch]=1
;           a = reform(arr[ch,*,*])
;
;           frg[ch,*,*]  = fft(a,-1,dim=1) ; range
;           frgL[ch,*,*] = total(abs(reform(frg[ch,*,*])),2)/n[1]
;
;           faz[ch,*,*]  = fft(a,-1,dim=2) ; azimuth
;           fazL[ch,*,*] = total(abs(reform(faz[ch,*,*])),1)/n[0]
;
;           f[ch,*,*] = fft(a,-1)
;        endif
;
;        wset,winazL
;        plot,fazL[ch,*,*],xstyle=1,ystyle=1
;  
;        wset,winrgL
;        plot,frgL[ch,*,*],xstyle=1,ystyle=1
;
;        wset,winfft
;        img = congrid(reform(abs(f[ch,*,*])),256,256)
;        mm=mean(img)
;        img=bytscl(img,0,2.5*mm)
;        tv,img
;
;        wset,winazf
;        img = congrid(reform(abs(faz[ch,*,*])),256,256)
;        mm=mean(img)
;        img=bytscl(img,0,2.5*mm)
;        tv,img
;
;        wset,winrgf
;        img = congrid(reform(abs(frg[ch,*,*])),256,256)
;        mm=mean(img)
;        img=bytscl(img,0,2.5*mm)
;        tv,img
     endif
  endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

  widget_control,main,/destroy

; switch back to main draw widget
  widget_control,wid.draw,get_value=index
  wset,index

end
