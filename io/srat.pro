;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Autor: Andreas Reigber
; Datum: 19.7.2002
; save an array in RAT (Radar Tool) format
; Aufruf: 	sarr,filename,array
; Keywords:	INFO  - Comment (default = "unknown content")
;           NOXDR - dont use /XDR format
;           HEADER- only save header, in array you get back open LUN
;           MULTI - number of files
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


pro srat,file,bild,INFO=info,NOXDR=noxdr,HEADER=header,TYPE=type,PREVIEW=preview,update_info=update_info,MT=mt,MULTI=multi
   ;on_error,2

	if keyword_set(noxdr) then xflag=0 else xflag=1
	if not keyword_set(multi) then multi = 1

	if keyword_set(update_info) then begin
		;--> Read information about original data
      openr,ddd,file,/get_lun,/xdr
      dim  = 0l
      readu,ddd,dim
      free_lun,ddd
      if dim gt 0 and dim lt 9 then xflag=1 else xflag=0 ; if outside, very likely file is not xdr
      if keyword_set(noxdr) then xflag=0                 ; override autodetection..?

      dim  = 0l
		var  = 0l
		type = 0l
		pointeur_preview = long64(0)
		multi= 0l
		dummy= 0l
		info = bytarr(80)
		openr,ddd,file,/get_lun,xdr=xflag
		readu,ddd,dim
		siz=lonarr(dim)
		readu,ddd,siz
		readu,ddd,var
		readu,ddd,type
		readu,ddd,pointeur_preview
		readu,ddd,multi
		readu,ddd,info
		free_lun,ddd
		openw,eee,file,/append,xdr=xflag,/get_lun

		;--> Calculate the pointer to update the info string
		;position = 24 + dim*4
		;point_lun,eee,position
		POINT_LUN, eee, 0
		writeu, eee, dim
		writeu, eee, siz
		writeu, eee, var
		writeu, eee, long(update_info[1])
		writeu, eee, pointeur_preview
		writeu, eee, multi
		leng = strlen(update_info[0])
		new_info = byte(update_info[0])
		if leng lt 80 then new_info = [new_info,bytarr(80-leng)+32b]
		new_info = new_info[0:79]
		writeu,eee,new_info
		free_lun,eee
		return
	endif

	if keyword_set(preview) then begin

;--> Read information about original data
      openr,ddd,file,/get_lun,/xdr
      dim  = 0l
      readu,ddd,dim
      free_lun,ddd
      if dim gt 0 and dim lt 9 then xflag=1 else xflag=0 ; if outside, very likely file is not xdr
      if keyword_set(noxdr) then xflag=0                 ; override autodetection..?

      dim  = 0l
      var  = 0l
		type = 0l
		pointeur_preview = long64(0)
		multi= 0l
      info = bytarr(80)
      openr,ddd,file,/get_lun,xdr=xflag
      readu,ddd,dim
      siz=lonarr(dim)
      readu,ddd,siz
      readu,ddd,var
		readu,ddd,type
		readu,ddd,pointeur_preview
		readu,ddd,multi
      readu,ddd,info
		if multi gt 1 then begin  ; Find end of multifile
			ifile = ""
			for i=1,multi do readu,ddd,ifile
			point_lun,-ddd,pointeur_preview
		endif
		free_lun,ddd

		openw,eee,file,/append,xdr=xflag,/get_lun

		;--> Calculate the pointeur to update the pointeur_preview
		position = 12 + dim*4
		;--> Calculate the pointeur preview position
		if multi le 1 then begin
			byt=[0,1,4,4,4,8,8,0,0,16,0,0,4,4,8,8]	  ; bytelength of the different variable typos
  			pointeur_preview = long64(108) + long64(dim)*long64(4) + long64(product(long64(siz)))*long64(byt[var])
		endif

		;go to position
		point_lun,eee,position
		writeu,eee,long64(pointeur_preview)

		;go to pointeur_preview
		point_lun,eee,pointeur_preview

		; write header
		dim = size(bild)
		xdim = dim[dim[0]-1]
		ydim = dim[dim[0]]
		writeu,eee,long(xdim)
		writeu,eee,long(ydim)
		writeu,eee,0l,0l,0l,0l,0l,0l
		writeu,eee,bild
		free_lun,eee
		return
	endif

	if not keyword_set(info) then info = "unknown content"
	if not keyword_set(type) then type = 0l
	pointeur_preview = long64(0)
	dummy = 0l
	type = long(type)

	leng = strlen(info)
	bytinfo = byte(info)
	if leng lt 80 then bytinfo = [bytinfo,bytarr(80-leng)+32b] $
	              else bytinfo = bytinfo[0:79]
        s=long(size(bild))
        s=s[0:s[0]+1]

        if n_elements(header) ne 0 then begin
		if multi gt 1 and arg_present(mt) then begin
			mt[*].file2 += +'_part'
			mt[*].dim = header[0]
			mt[*].size = header[1:header[0]]
			mt[*].var    = header[header[0]+1]
			for i=0,multi-1 do begin
				openw,ddd,mt[i].file2,/get_lun,xdr=xflag
				writeu,ddd,mt[i].dim
				writeu,ddd,mt[i].size
				writeu,ddd,mt[i].var
				writeu,ddd,mt[i].type
				writeu,ddd,long64(0)
				writeu,ddd,1l
	   		writeu,ddd,mt[i].info
				mt[i].lun2 = ddd
			endfor
			bild = mt.lun2

			openw,ddd,mt[0].mfile,/get_lun,xdr=xflag
			writeu,ddd,long(header)
			writeu,ddd,type
			writeu,ddd,long64(0)
			writeu,ddd,multi
                        writeu,ddd,bytinfo
			for i=0,multi-1 do writeu,ddd,mt[i].file1
			free_lun,ddd
		endif else begin
			openw,ddd,file,/get_lun,xdr=xflag
			writeu,ddd,long(header)
			writeu,ddd,type
			writeu,ddd,pointeur_preview
			writeu,ddd,multi
                        writeu,ddd,bytinfo
			bild = ddd
		endelse
	endif else begin
		openw,ddd,file,/get_lun,xdr=xflag
		writeu,ddd,s
		writeu,ddd,type
		writeu,ddd,pointeur_preview
		writeu,ddd,dummy
		writeu,ddd,bytinfo
		writeu,ddd,bild
		free_lun,ddd
	endelse

end
