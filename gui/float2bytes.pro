; docformat = 'rst'
;+
; Optimized bytscaling for all circumstances
;
; :Params:
;    arr: in, required, type="float"
;       the input array to be bytescaled
;
; :Keywords:
;    type: in, optional, type=integer
;       force this RAT type (default: file.type)
;    overwrite: in, optional, type=flag
;       overwrite input array
;
; :Author: RAT Team
; :Categories: Infrastructure
;
; :Copyright:
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
;-
function float2bytes,arr,TYPE=type, OVERWRITE=OVERWRITE
   common rat, types, file, wid, config
   
   if not keyword_set(type) then type = file.type
   
   if ~keyword_set(OVERWRITE) then _arr=arr

   dim  = size(arr)
   xdim = dim[dim[0]-1]
   ydim = dim[dim[0]]
   if dim[0] ge 3 then zdim = dim[dim[0]-2] else zdim = 1
   if dim[0] eq 4 then vdim = dim[dim[0]-3] else vdim = 1


   scaling = 1                  ; scale at end 
   ch_scl  = 1                  ; scale channels independently (1=mean, 2=max)
   sar_scl = 1                  ; SAR type scaling
   amp_scl = 1                  ; amplitude power trick for better display
   hist_scl = 0                 ; amplitude power trick for better display
   
   n_elements_arr = n_elements(arr) ;; for refl_sym case in T3/T6
   
   case 1 of
      
;-----------------------------------------------------------
; single channel images
;-----------------------------------------------------------

      (type eq 100) or (type eq 51) or (type eq 54) or (type eq 101)  $
        or (type eq 103) $
        or (type eq 250): begin

      end
      
      (type eq 52) or (type eq 55): begin
         arr=bytscl(arr,-!pi,!pi)				  
         scaling = 0
      end
      
      (type eq 120): begin      ; texture image
         oarr = bytarr(file.zdim,xdim,ydim)
         for i=0,file.zdim-1 do begin
            mm = float(total(arr[i,*,*],/double)/n_elements(arr[i,*,*]))	
            oarr[i,*,*] = bytscl(arr[i,*,*],0,2.5*mm)
         endfor
         mval = moment(arr[2,*,*])
         oarr[2,*,*] = bytscl(arr[2,*,*],mval[0]-mval[1],mval[0]+mval[1])
         arr = oarr
         scaling = 0
      end

      (type eq 51) or (type eq 54):  begin 
      end
      
      (type ge 56 && type le 57): begin ; correlation ; coherence
         arr=bytscl(abs(arr),0.0,1.0)
         scaling = 0
      end

      (type eq 58) or (type eq 110) : begin
         hist_scl = 1
         ch_scl  = 0
         sar_scl = 0
         amp_scl = 0
      end
;-----------------------------------------------------------
; Multitemporal
;-----------------------------------------------------------
      
      (type eq 122): begin      ; Ratio
         if max(arr) gt 1.0 then arr=bytscl(arr,0.0,max(arr)) $
         else arr=bytscl(arr,0.0,1.0)
         scaling = 0
      end

      (type eq 123): begin      ; Difference
         if min(arr) ge 0.0 then begin
            arr=bytscl(arr,0.0,max(arr))
         endif else begin
            if (min(arr) ge -1.0) and (max(arr) le 1.0) then begin
               arr=bytscl(arr,-1.0,1.0)
            endif else begin
               arr=bytscl(arr,min(arr),max(arr))
            endelse
         endelse
         scaling = 0
      end

      (type eq 124): begin      ; Propability
         if min(arr) lt 0.0 then arr=bytscl(arr,min(arr),0.0) $
         else arr=bytscl(arr,0.0,1.0)
         scaling = 0
      end

      (type eq 125): begin      ; Propability
         arr=bytscl(arr,0.0,1.0)
         scaling = 0
      end

;-----------------------------------------------------------
; Polarimetric data types
;-----------------------------------------------------------
      
      (type ge 200 and type lt 211):  begin ; scattering vector
      end		
      
      (type ge 220 and type le 222):  begin ; scattering matrices 
         arr = sqrt(arr > 0)
         if arr[0,2,xdim/2,ydim/2] eq 0 && $ ;; reflection symmetric case
           total(arr[0,2,*,*],/double) eq 0. then $
             if vdim mod 3 eq 0 then $
               n_elements_arr *= (5/9.) $
             else if vdim mod 4 eq 0 then $
               n_elements_arr *= (8/16.)
      end
      
      (type eq 214 or type eq 250):  begin ; eigenvalue / span
      end		
      
      (type eq 211): begin
      end

      (type eq 213): begin      ; Krogager
      end
      
      (type eq 233): begin      ; Entopie / Alpha / Anisotropie
         sar_scl = 0
         amp_scl = 0
         ch_scl  = 2
      end

      (type eq 234): begin      ; Alpha / Beta / Gamma / Delta
         sar_scl = 0
         amp_scl = 0 			
         ch_scl  = 2
      end

      (type eq 235): begin      ; ERD
         scaling = 0
         arr = bytscl(temporary(arr),-1,1)
      end

      (type eq 236): begin      ; delta/tau
;         scaling = 0
         arr[0, *, *, *] = atan(arr[0, *, *, *])
         sar_scl = 0
         amp_scl = 0 			
         ch_scl  = 2
      end


      (type eq 230): begin
         arr=bytscl(arr,0.0,1.0)
         scaling = 0
      end
      
      (type eq 231): begin
         arr=bytscl(arr,0.0,!pi/2)
         scaling = 0
      end
      
      (type eq 232): begin
         arr=bytscl(arr,0.0,1.0)
         scaling = 0
      end
      
      (type eq 216): begin      ; Moriyama decomp.
         arr[0:2,*,*] = sqrt(arr[0:2,*,*] > 0)^0.7
         oarr = bytarr(zdim,xdim,ydim)
         for i = 0,2 do begin
            mm = float(total(arr[i,*,*],/double)/n_elements(arr[i,*,*]))
            oarr[i,*,*] = bytscl(arr[i,*,*],0,config.sar_scale*mm)
         endfor
         oarr[3,*,*] = bytscl(arr[3,*,*])
         arr = oarr
         sar_scl = 0
         ch_scl = 0
      end
      
      (type eq 280):  begin     ; ENVISAT APP
      end		

;-----------------------------------------------------------
; Interferometric data types
;-----------------------------------------------------------
      
      (type eq 300): begin      ; interferometric pair
      end

      (type eq 301): begin      ; complex interferogram ?
         arr = sqrt(arr > 0)
         scaling = 1
      end

      (type eq 302): begin
         arr=bytscl(arr,-!pi,!pi)
         scaling = 0
      end
      
      (type eq 303): begin
         arr=bytscl(arr)
         scaling = 0
      end

      (type eq 310): begin
         arr=bytscl(arr,0.0,1.0)
         scaling = 0
      end

;-----------------------------------------------------------
; Classifications
;-----------------------------------------------------------
      
      (type ge 400 and type lt 500 ): begin
         arr=byte(arr)
         scaling = 0
      end
      
;-----------------------------------------------------------
; PolInSAR data types
;-----------------------------------------------------------
      
      ((type ge 500) and (type le 503)): begin ;  PolInSAR vector
      end
      
      ((type ge 510) and (type le 513)): begin ;  PolInSAR matrix
         arr = sqrt(temporary(arr) > 0)
         if arr[0,2,xdim/2,ydim/2] eq 0 && $ ;; reflection symmetric case
           total(arr[0,2,*,*],/double) eq 0. then $
             if vdim mod 3 eq 0 then $
               n_elements_arr *= (5/9.) $
             else if vdim mod 4 eq 0 then $
               n_elements_arr *= (8/16.)
       end
      
      type eq 514: begin        ; normalized matrix
         arr=bytscl(sqrt(abs(temporary(arr))),0.0,1.0)
         scaling = 0
      end
      
;;; obsolete
; 		(type eq 520): begin ; interferogram  
; 		end
;                 (type eq 522) or (type eq 523): begin ; PolInSAR interferometric amplitude
; 		end

      (type ge 530 && type le 539): begin ; coherence
         arr=bytscl(abs(arr),0.0,1.0)
         scaling = 0
      end

;-----------------------------------------------------------
; SubAp data types
;-----------------------------------------------------------
      
      ((type ge 600) and (type le 609)): begin ;  SubAp vector
      end

      ((type ge 610) and (type le 615)): begin ;  SubAp matrix
         arr = sqrt(arr > 0)
      end

;-----------------------------------------------------------
; MBSAR data types
;-----------------------------------------------------------
      
; 		((type ge 800) and (type le 803)): begin ;  scattering vectors
; 		end
      
; 		((type ge 810) and (type le 813)): begin ;  matrices ; some optimization due to high dimensionality and some zero channels (e.g. after azimuth_sym)
;                    arr = sqrt(temporary(arr > 0) )
;                    mmm = 0. & mmn = 0 & xydim = n_elements(arr[0,0,*,*])
;                    for i=0,file.vdim-1 do $
;                       for j=0,file.zdim-1 do $
;                          if total(arr[i,j,*,*]ne 0)ne 0 then begin
;                       ch = arr[i,j,*,*]
;                       mm = total(ch)/xydim
;                       ch /= mm
;                       ch >= 0
;                       ch ^= .7
;                       mmm += total(ch)/xydim & mmn++
;                       arr[i,j,*,*] = ch
;                    endif & undefine,ch
;                    mm = mmm/mmn
;                    arr=bytscl(temporary(arr),0,config.sar_scale*mm)
;                    scaling = 0
; 		end
      
;                 type eq 814: begin ; normalized matrix
;                    arr=bytscl(sqrt(abs(arr)),0.0,1.0)
;                    scaling = 0
;                 end

;                 (type ge 830 && type le 839): begin ; coherence
;                    arr=bytscl(abs(arr),0.0,1.0)
;                    scaling = 0
;                 end

;-----------------------------------------------------------
; Everything else
;-----------------------------------------------------------
      
      else: begin
         arr=bytscl(arr)
         scaling = 0
      endelse
   endcase

   arr = reform(arr,file.vdim,file.zdim,xdim,ydim,/overwrite) 
;  	if file.mult eq 1 then $
;  		arr = reform(arr,file.vdim,file.zdim,xdim,ydim) $
;  	else $
;  		arr = reform(arr,file.mult,file.vdim,file.zdim,xdim,ydim)
   
;         arr_siz = size(arr,/dim) ; make a 4-dimensional array out of arr
;          case n_elements(arr_siz) of
;             3: arr = reform(arr,[1,arr_siz],/OVERWRITE)
;             2: arr = reform(arr,[1,1,arr_siz],/OVERWRITE)
;             1: arr = reform(arr,[1,1,1,arr_siz],/OVERWRITE)
;  			  else:
;          endcase
;-----------------------------------------------------------
; Now do the scaling
;-----------------------------------------------------------

   if scaling eq 1 then begin

; Histogram scaling
      if hist_scl eq 1 then begin
         hist = histogram(arr,nbin=1000,locations=loc)
         mh = total(hist)/20.
         st = 0
         while total(hist[0:st]) lt mh do st++
         en = 999
         while total(hist[en:999]) lt mh do en--
         min_data = loc[st]
         max_data = loc[en]
         arr=bytscl(arr,min_data,max_data)
      endif

; Channel scaling

      if ch_scl eq 1 then begin
         arr /= rebin(rebin(arr,file.vdim,file.zdim,1,1),file.vdim,file.zdim,xdim,ydim)
      endif

      if ch_scl eq 2 then begin
         for i=0,file.vdim-1 do begin               
            for j=0,file.zdim-1 do begin               
               arr[i,j,*,*] /= max(arr[i,j,*,*])	
            endfor
         endfor
      endif
      
; Amplitude trick
;       stop
;       t=systime(1)	
;       if amp_scl eq 1 then arr = (arr > 0)^0.7 
;       print,"B1: ",systime(1)-t
      
      if amp_scl eq 1 then begin
         arr = sqrt(temporary(arr) > 0)
         arr *= sqrt(arr)
      endif
      
; SAR scaling

      if sar_scl eq 1 then begin
         if config.log_scale then $
           arr = bytscl(temporary(10*alog10(arr)),min=0) $
         else begin
           mm = float(total(arr,/double)/n_elements_arr)	
           arr=bytscl(temporary(arr),0,config.sar_scale*mm)
         endelse
      endif else begin
         mm = max(arr)
         arr=bytscl(temporary(arr),0,mm)
      endelse
   endif
   if ~keyword_set(OVERWRITE) then begin
      result=arr
      arr=_arr
      return, reform(result)
   endif
   return,reform(arr)
end
