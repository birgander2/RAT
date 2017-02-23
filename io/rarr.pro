;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Autor: Andreas Reigber
; Datum: 1.4.1998
; read an array written by sarr
; Aufruf: 	rarr,filename,array
; Keywords:	INFO  - here you find the comment (Default = "unknown content")
;        	JOAO  - my old format (2*long,then always floatarray - bad)
;           NOXDR - do not use /XDR
;           HEADER- read only header (if not /JOAO)
;                   in array you get the open LUN.
;           BLOCK - read only this block of the data [X1,Y1,DX,DY]
;           GZIP  - uncompress with gunzip, dont put .gz at the end
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



pro rarr,file,bild,INFO=info,JOAO=joao,NOXDR=noxdr,HEADER=header,BLOCK=block,GZIP=gzip
   catch, error	
	if error ne 0 then begin
		bild = -1
		return
	endif
	
; Unzulaessige KEYWORD-Kombinationen
	if keyword_set(joao) and keyword_set(block) then begin
		print," Kombination /JOAO und /BLOCK nicht implementiert"
		return
	endif 
	if keyword_set(header) and keyword_set(block) then begin
		print," Kombination /HEADER und /BLOCK ist Bloedsinn"
		return
	endif 
	if keyword_set(header) and keyword_set(joao) then begin
		print," Kombination /HEADER und /JOAO nicht implementiert"
		return
	endif 
	if keyword_set(info)  and keyword_set(joao) then begin
		print,"Joao-Format kennt kein Infostring"	
		return
	endif 
	

	if keyword_set(noxdr) then xflag=0 else xflag=1
	
	if not keyword_set(joao) then begin
		if keyword_set(gzip) then begin
			datstr = floor((randomu(seed,1))[0]*1e6)
			datstr = strcompress(datstr,/remove)
			spawn,'gunzip -c '+file+'.gz > /tmp/sarr'+datstr
			file = '/tmp/sarr'+datstr	
		endif
		
	   dim  = 0l
	   var  = 0l
	   info = bytarr(80)  
	   openr,ddd,file,/get_lun,xdr=xflag
	   readu,ddd,dim
	   siz=lonarr(dim)
	   readu,ddd,siz
	   readu,ddd,var
	   readu,ddd,info 
	   info = strtrim(string(info))
		if keyword_set(header) then begin
			bild = ddd
			header = [dim,siz,var]
		endif else begin
			if keyword_set(block) then begin
				case var of
					4:  bits = 4
					6:  bits = 8
					else: begin
						print,'Blocklesen nur bei FLOAT und COMPLEX'
						return						
					end
				endcase				
				point_lun,-ddd,pos
				point_lun,ddd,pos+block[1]*siz[0]*bits
				siz = [siz[0],block[3]]
			endif
			case var of 
				1:  bild=make_array(/byte,dimension=siz)
				2:  bild=make_array(/int,dimension=siz)
				3:  bild=make_array(/long,dimension=siz)
				4:  bild=make_array(/float,dimension=siz)
				5:  bild=make_array(/double,dimension=siz)
				6:  bild=make_array(/complex,dimension=siz)
				9:  bild=make_array(/dcomplex,dimension=siz)
				12: bild=make_array(/uint,dimension=siz)
				13: bild=make_array(/ulong,dimension=siz)
				14: bild=make_array(/l64,dimension=siz)
				14: bild=make_array(/ul64,dimension=siz)
				else: begin
					print,'Arraytyp nicht erkannt (Falsches Format ??)'
					return
				end
			endcase
		   readu,ddd,bild
		   free_lun,ddd		
			if keyword_set(gzip) then spawn,'rm '+file
			if keyword_set(block) then bild = bild[block[0]:block[0]+block[2]-1,*]			
		endelse  ; Header oder alles einlesen

	endif else begin ; Joao-Format

	   sizx = 0l
 	   sizy = 0l
	   openr,ddd,file,/get_lun,xdr=xflag
	   readu,ddd,sizx
	   readu,ddd,sizy
	   bild=fltarr(sizx,sizy)
	   readu,ddd,bild
	   free_lun,ddd

	endelse ; Joao-Format
end
