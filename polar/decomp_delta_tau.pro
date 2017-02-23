;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Delta/Tau decomposition (delta= particle scattering anisotropy
; 									tau= degree of orientation
; written by    : Maxim Neumann
; last revision : March, 2009
;
;  Extraction of physical parameters related to the
;  scattering mechansim type (magnitude of delta), the preferred
;  orientation (phase of delta), and the degree of orientation
;  randomness (tau).
;
;  It is based on a simplified single--layer model, assuming
;  independent scattering (Born approximation). For more infomation, see
;  e.g.: M. Neumann, "Remote sensing of vegetation using
;  multi-baseline polarimetric SAR interferometry: theoretical
;  modeling and physical parameter retrieval", PhD thesis, January
;  2009, University of Rennes 1, France.
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


; docformat = 'rst'
;+
; Delta-Tau Decomposition
;
;  Extraction of physical parameters related to the
;  scattering mechansim type (magnitude of delta), the preferred
;  orientation (phase of delta), and the degree of orientation
;  randomness (tau).
;
;  It is based on a simplified single--layer model, assuming
;  independent scattering (Born approximation). For more infomation, see
;  e.g.: M. Neumann, "Remote sensing of vegetation using
;  multi-baseline polarimetric SAR interferometry: theoretical
;  modeling and physical parameter retrieval", PhD thesis, January
;  2009, University of Rennes 1, France.
;
; :Keywords:
;    called: in, optional, type="flag"
;       call routine without GUI in batchmode
;    
; :Params:
;
; :Author: Maxim Neumann
; 
; :Categories: decomposition
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
pro decomp_delta_tau, CALLED = called
  common rat, types, file, wid, config, tiling
  compile_opt idl2
  
;  if  file.type ne 221 then begin
  if ~((file.type ge 500 && file.type le 513) $
       || file.type eq 221) then begin
;  not yet extended to MB-PolInSAR case...
     error = DIALOG_MESSAGE(["This is wrong data type.", $
                             '', $
                             'This procedure needs a coherency matrix T'], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl
  if file.type ge 500 then begin
     if file.type ge 500 && file.type le 509 then begin
        polin_k2m,/called,/gui,smmx=sx,smmy=sy ;; reform to matrix
        if sx*sy le pol then $
           speck_polmean,/called,/gui
     endif
     if file.type ne 511 && 'Yes' eq $
        dialog_message(["Performing necessary preprocessing step:", $
                        "Tranformation to Pauli representation"],/cancel, $
                       DIALOG_PARENT = wid.base, TITLE='Information') $
     then polin_basis,0,/PAULI
     if file.type ne 511 then $
        return
  endif

  newtype = 236L

   
  WIDGET_CONTROL,/hourglass
  undo_prepare,outputfile,finalfile,CALLED=CALLED

;------------------------------------------------------------
; Read / write file header (FIXED+EXAMPLE)
; -> If the parameters of the output data are not identical
;    to the input data, a different (correct) header has to
;    be written
;------------------------------------------------------------
   
   rrat,file.name, ddd,header=head,info=info,type=type
   head = [3L, 3L, head[3:4], 4L]
   srat,outputfile,eee,header=head,info=info,type=newtype

;------------------------------------------------------------
; Initialise vertical tiling with overlap (FIXED)
; -> omit keyword if no overlap is desired
;------------------------------------------------------------
   tiling_init

;------------------------------------------------------------
; Pop up progress window (FIXED)
;------------------------------------------------------------
   progress,Message='Delta/Tau decomposition...',/cancel_button

;------------------------------------------------------------
; Start block processing (FIXED)
;------------------------------------------------------------
   for i=0,tiling.nr_blocks-1 do begin   
      progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
      if wid.cancel eq 1 then return

;------------------------------------------------------------
; Read tile from input file (FIXED)
; -> after reading the array is 4-dimensional. To get rid of
;    leading empty dimensions use the reform() command
;------------------------------------------------------------
      tiling_read,ddd,i,block

; -------- YOUR FILTER CALL----------
      dmag = abs(sqrt((block[1, 1, *, *]+block[2, 2, *, *])/block[0, 0, *, *]))
      dpha = arg(block[0, 1, *, *])
      tau  = 1 - abs(block[0, 1, *, *])/abs(block[0, 0, *, *])/dmag
      block = [dmag, dpha, tau]
; -------- YOUR FILTER CALL----------	

;------------------------------------------------------------
; Write tile to output file (FIXED)
;------------------------------------------------------------
;      tiling_write,eee,i,temporary(block)
      tiling_write,eee,i,(block)
   endfor

;------------------------------------------------------------
; Free LUNs (FIXED)
;------------------------------------------------------------
   free_lun,ddd,eee

;------------------------------------------------------------
; Update RATs structures and windows information (FIXED)
; -> Set keyword PALETTE to a number of a colour palette
;    if omitted b/w colour is used
;------------------------------------------------------------
   rat_finalise,outputfile,finalfile,CALLED=CALLED

;------------------------------------------------------------
; For the text-history of the changes of the data files.
; -> Content is found in the .rit file an via the RAT menue
;------------------------------------------------------------
   evolute,'Delta / Tau decomposition'
end

