; docformat = 'rst'
;+
; Calculates the ERD (eigenvalue relative difference) parameters from
; covariance and coherency matrices
;
; :Keywords:
;    called: in, optional, type="flag"
;       call routine without GUI in batchmode
;
; :Author: RAT Team
; :Categories: PolSAR parameters
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
pro decomp_erd,CALLED=called
   common rat, types, file, wid, config, tiling


   if not keyword_set(method) then method = 0 ; Default values

   if file.type ne 220 and file.type ne 221 then begin
      error_button = DIALOG_MESSAGE(['Input data have to be a [C] matrix','[T] matrix or an eigendecomposition.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif

   if file.vdim ne 3 then begin
      error_button = DIALOG_MESSAGE(['3x3 input matrix required !','(maybe calibrate & symmetrize?)'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif

   if file.type eq 221 then begin ; Wrong variable type?
      c_to_t,/called
   endif

   WIDGET_CONTROL,/hourglass

; undo function
 
  undo_prepare,outputfile,finalfile,CALLED=CALLED
   
; read / write header

   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type		
   srat,outputfile,eee,header=[3l,2,file.xdim,file.ydim,4l],info=info,type=235l		
   
; calculating preview size and number of blocks

   tiling_init
   progress,Message='ERD decomposition...',/cancel_button

; start tiling loop

   for i=0,tiling.nr_blocks-1 do begin
      progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
      if wid.cancel eq 1 then return
      tiling_read,ddd,i,block

      lam1 = ( abs(block[0,0,*,*]) + abs(block[1,1,*,*]) + sqrt(abs(block[0,0,*,*] - block[1,1,*,*])^2 + 4*abs(block[1,0,*,*])^2) ) / 2
      lam2 = ( abs(block[0,0,*,*]) + abs(block[1,1,*,*]) - sqrt(abs(block[0,0,*,*] - block[1,1,*,*])^2 + 4*abs(block[1,0,*,*])^2) ) / 2
      lam3 = abs(block[2,2,*,*])

      lams = lam2
      lamd = lam1
      aux = where(real_part(block[1,0,*,*]) gt 0.0,nr)
      if nr gt 0 then begin
         lams[aux] = lam1[aux]
         lamd[aux] = lam2[aux]
      endif
      lam1 = 0
      lam2 = 0
      erd = fltarr(1,2,file.xdim,(*tiling.blocksizes)[i])
      erd[0,0,*,*] = (lams - lam3)/(lams + lam3)
      erd[0,1,*,*] = (lamd - lam3)/(lamd + lam3)
      rmnanq,erd

      tiling_write,eee,i,temporary(erd)
      tiling_jumpback,ddd

   endfor
   free_lun,ddd,eee
   
; update file information

   rat_finalise,outputfile,finalfile,CALLED=called,palette=3
   evolute,'ERD decomposition'
end
