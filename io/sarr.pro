;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Autor: Andreas Reigber
; Datum: 1.4.1998
; Write a variable to a file. Any dimensions and type.
; Aufruf: 	sarr,filename,array
; Keywords:	INFO  - Comment (default = "unknown content")
;        	JOAO  - my old format (2*long, then always floatarray)
;           NOXDR - dont use /XDR format
;           HEADER- only save header (only if not /JOAO)
;                   in array you get back open LUN
;        	GZIP  - compress with gzip komprimiert, dont put .gz at the end
; This software has been released under the terms of the GNU Public
; license. See http://www.gnu.org/copyleft/gpl.html for details.
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

 
pro sarr,file,bild,INFO=info,JOAO=joao,NOXDR=noxdr,HEADER=header,GZIP=gzip
   on_error,2

	if keyword_set(noxdr) then xflag=0 else xflag=1
   
	if not keyword_set(joao) then begin

		if not keyword_set(info) then info = "unknown content"		
		leng = strlen(info)
		info = byte(info)
		if leng lt 80 then info = [info,bytarr(80-leng)+32b] $
		              else info = info[0:79]
				
	   s=size(bild)
	   s=s[0:s[0]+1]
		if keyword_set(header) then begin
			openw,ddd,file,/get_lun,xdr=xflag
		   writeu,ddd,long(header)
		   writeu,ddd,info
			bild = ddd
		endif else begin
			openw,ddd,file,/get_lun,xdr=xflag
		   writeu,ddd,s
		   writeu,ddd,info
		   writeu,ddd,bild
		   free_lun,ddd
			if keyword_set(gzip) then spawn,'gzip --fast '+file
		endelse
	   
	endif else begin

		if keyword_set(info) then print,"Joao-Format kennt kein Infostring"	
		openw,ddd,file,/get_lun,xdr=xflag
	   writeu,ddd,long((size(bild))[1])
	   writeu,ddd,long((size(bild))[2])
	   writeu,ddd,bild
	   free_lun,ddd

	endelse
	
	
	
end
