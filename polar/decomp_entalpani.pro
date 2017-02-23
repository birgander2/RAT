; docformat = 'rst'
;+
; Calculates the polarimetric entropy/alpha/anisotropy parameters from
; covariance and coherency matrices or eigenvalue decompositions
;
; :Keywords:
;    method: in, optional, type="boolean"
;       calculate mean or dominant alpha (set keyword to 0 or 1)
;
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
pro decomp_entalpani,CALLED=called,METHOD = method
   compile_opt idl2
   common rat, types, file, wid, config, tiling

   if not keyword_set(method) then method=0

; check if array is usable

   if file.type ne 214 and file.type ne 220 and file.type ne 221 then begin
      error_button = DIALOG_MESSAGE(['Input data have to be a [C] matrix','[T] matrix or an eigendecomposition.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif

   if not keyword_set(called) then begin ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Entropy / Alpha / Anisotropy Calculation',/floating,/tlb_kill_request_events,/tlb_frame_attr)
      field2 = widget_label(main,value='Select type of alpha angle:')
      field1 = CW_BGROUP(main,['Mean alpha','Dominant alpha'],/column,/exclusive,set_value=method)
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
            infotext = ['ENTROPY ALPHA ANISOTROPY CALCULATION V2.0',$
                        ' ',$
                        'RAT module written 02/2005 by Andreas Reigber']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
         end
      endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
      widget_control,field1,GET_VALUE=method ; read widget fields
      widget_control,main,/destroy ; remove main widget
      if event.id ne but_ok then return ; OK button _not_ clicked
   endif 

; ----------------------

   WIDGET_CONTROL,/hourglass

; undo function

   undo_prepare,outputfile,finalfile,CALLED=CALLED
   
   if file.type eq 220 then begin ; Wrong variable type?
;      if not keyword_set(called) then dummy = DIALOG_MESSAGE(["Performing necessary preprocessing step:","Tranformation to Pauli representation"], DIALOG_PARENT = wid.base, /information)
      c_to_t,/called
   endif

   if file.type eq 221 then begin ; Wrong variable type?
 ;     if not keyword_set(called) then dummy = DIALOG_MESSAGE(["Performing necessary preprocessing step:","Eigenvector Decomposition"], DIALOG_PARENT = wid.base, /information)
      decomp_eigen,/called
   endif

; read / write header

   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type		
   srat,outputfile,eee,header=[3l,file.zdim < 3,file.xdim,file.ydim,4l],info=info,type=233l		
   
; calculating preview size and number of blocks

   tiling_init

; pop up progress window

   progress,Message='H/a/A decomposition...',/cancel_button

   index = indgen(file.zdim < 3)
   for i=0,tiling.nr_blocks-1 do begin
      progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
      if wid.cancel eq 1 then return

      oblock = make_array([1,file.zdim < 3,file.xdim,(*tiling.blocksizes)[i]],type=4l)
      tiling_read,ddd,i,block
      
      for k=0,file.xdim-1 do begin
         for l=0,(*tiling.blocksizes)[i]-1 do begin	
            ew = reform(abs(block[file.vdim-1,*,k,l]))
            ev = reform(abs(block[0:file.vdim-2,*,k,l]))
            
            if file.zdim eq 4 then begin
               ew = ew[0:2]
               ev = ev[0:2,0:2]
            endif
            
            pi = ew / total(ew)
            oblock[0,0,k,l] = total(-pi*alog(pi)/alog(file.zdim)) ; entropy
            if method eq 0 then oblock[0,1,k,l] = total(acos(abs(ev[0,index]))*pi[index]) $ ; mean alpha
                           else oblock[0,1,k,l] = acos(abs(ev[0,0]))	
            if file.zdim gt 2 then oblock[0,2,k,l] = (ew[1]-ew[2])/(ew[1]+ew[2]) ; anisotropy	
         endfor
      endfor
      
      aux = where(finite(oblock) eq 0,anz) ; eliminate nan's
      if anz ne 0 then oblock[aux]=0.0
      
      tiling_write,eee,i,temporary(oblock)
      tiling_jumpback,ddd
   endfor
   free_lun,ddd,eee
   
; update everything

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute,'Entropy/Alpha/Anisotroy decomposition. Method: '+strcompress(method,/R)

end
