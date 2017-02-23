;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Autor: Andreas Reigber
; Datum: 25.9.2003
; read an array in RAT (Radar Tool) format
; Aufruf: 	rrat,filename,array
; Keywords:	INFO  - here you find the comment (Default = "unknown content")
;           NOXDR - do not use /XDR
;           HEADER- read only header, in array you get the open LUN.
;           BLOCK - read only this block of the data [X1,Y1,DX,DY]
;           PREVIEW - read the preview image if existing otherwise return -1
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


pro rrat,file,bild,INFO=info,NOXDR=noxdr,HEADER=header,BLOCK=block,TYPE=type,PREVIEW=preview,FILETYPE=filetype,MT=mt,MULTI=multi, $
         VAR=var,N_DIMENSIONS=dim,DIMENSIONS=siz ;; to get easier some informatin, in accordance to size() (except of VAR) (mn,18.08.06)
;	catch, error
;  	if error ne 0 then begin
;  		bild = -1
;  		return
;  	endif
;
;  	if keyword_set(header) and keyword_set(block) then begin
;  		print," Combination of /HEADER und /BLOCK is stupid"
;  		return
;  	endif
;
                                ;  XDR autodetection

   openr,ddd,file,/get_lun,/xdr
   dim  = 0l
   readu,ddd,dim
   free_lun,ddd
   if dim gt 0 and dim lt 9 then xflag=1 else xflag=0 ; if outside, very likely file is not xdr 
   if keyword_set(noxdr) then xflag=0 ; override autodetection..?
   
   dim  = 0l
   var  = 0l
   type = 0l
   pointeur_preview = long64(0)
   dummy= 0l
   multi= 0l
   info = bytarr(80)
   if file_test(file, /read) then $
     openr,ddd,file,/get_lun,xdr=xflag $
   else if file_test(file+'.rat', /read) then $
     openr,ddd,file+'.rat',/get_lun,xdr=xflag $
   else message, '% RRAT: Error opening file '+file+'. File not available or not readable'
   readu,ddd,dim
   siz=lonarr(dim)
   readu,ddd,siz
   readu,ddd,var
   readu,ddd,type
   readu,ddd,pointeur_preview
   readu,ddd,multi
   readu,ddd,info
   info = strtrim(string(info))
   filetype=type
   multi >= 1

   if keyword_set(preview) then begin
      if pointeur_preview eq 0 then begin
         bild = -1
         free_lun,ddd
      endif else begin
         point_lun,ddd,pointeur_preview
                                ;--> read header
         xdim=0l
         ydim=0l
         dummy=0l
         readu,ddd,xdim,ydim,dummy,dummy,dummy,dummy,dummy,dummy
         case dim of
            2: bild = bytarr(multi,xdim,ydim)
            3: bild = bytarr(multi,siz[0],xdim,ydim)
            4: bild = bytarr(multi,siz[0],siz[1],xdim,ydim)
         endcase
         readu,ddd,bild
         free_lun,ddd
         bild = reform(bild)
      endelse
      return
   endif

   if arg_present(header) then begin
      bild = ddd
      header = [dim,siz,var]
      if multi gt 1 and arg_present(mt) then begin

         mt = replicate({mfile : "", file1 : "", file2 : "", lun1 : 0l, lun2 : 0l, dim : dim, size : lonarr(dim), type : 0l, var : 0l, ptr : long64(0), info : bytarr(80) },multi)
         ifile = ""
         itype = 0l
         iptr  = long64(0)
         for i=0,multi-1 do begin
            readu,ddd,ifile
            mt[i].mfile = file
            mt[i].file1 = ifile
            mt[i].file2 = ifile
            openr,ilun,ifile,/get_lun,/xdr
            readu,ilun,dim
            mt[i].dim = dim
            siz=lonarr(dim)
            readu,ilun,siz
            mt[i].size = siz
            readu,ilun,var
            mt[i].var = var
            readu,ilun,itype
            mt[i].type = itype
            readu,ilun,iptr
            mt[i].ptr  = iptr
            mt[i].lun1 = ilun
            point_lun,ilun,108+dim*4
         endfor
         bild = mt.lun1
      endif else begin
         mt = -1
;			multi = {file1 : "", file2 : "", lun1 : ddd, lun2 : 0l, dim : dim, size : siz, type : type, var : var, ptr : pointeur_preview, info : info }
      endelse

   endif else begin
      if keyword_set(block) then begin
         bits=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]
         ch_fac = 1l
         for i=0,dim-3 do ch_fac = ch_fac * siz[i]
         nrx = long64(siz[dim-2])
         posx = long64(block[0])
         bpp = long64(bits[var])*ch_fac
         point_lun,-ddd,pos
         startlun = pos+long64(block[1])* nrx * bpp
         size = intarr(4) + 1
         size[4-dim] = siz
         size[3] = block[3]
         size[2] = block[2]
         sizl = size
         sizl[3] =  1
         bild = make_array(type=var,dimension=size)
         line = make_array(type=var,dimension=sizl)
         for i=0l,block[3]-1 do begin
            point_lun,ddd,startlun + (i*nrx+posx)*bpp
            readu,ddd,line
            bild[*,*,*,i] = line
         endfor
         bild = reform(bild)
      endif else begin
         if var gt 0 and var le 15 then begin
            bild=make_array(dimension=siz,type=var)
         endif else begin
            print,'Arraytyp nicht erkannt (Falsches Format ??)'
            return
         endelse
         readu,ddd,bild
      endelse
      free_lun,ddd
;           if keyword_set(block) then $
;              bild = reform(bild[*,*,block[0]:block[0]+block[2]-1,*])
   endelse
end
