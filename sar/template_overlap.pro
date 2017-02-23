;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; Template function
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



;------------------------------------------------------------
; Your private filtering function
;------------------------------------------------------------

function myfilter,arr,boxsize
   return,smooth(arr,[1,1,boxsize,boxsize])
end

;------------------------------------------------------------
; Main RAT module, to be called by rat.pro
; -> don't forget to:
;    add it to the 'compile.pro' in the respective subdirectory
;    change 'rat.pro' to be able to call the new module
;
; Global variables of RAT (FIXED)
; 
; Important:
;
; file.xdim = horizontal size of image
; file.ydim = vertical size of image
; file.zdim = number of layers, 1st matrix dimension
; file.vdim = used in matrix represenation for 2nd matrix dimension
; file.var  = IDL variable type, following the values returned by the size() command
; file.type = RAT data type, for details have a look in definitions.pro
;------------------------------------------------------------
; Comments for the IDLdoc, which should be provided at least
; for every module (main function) of the file:
;
; Possible values:
;
; :Keywords:
;   for every keyword a line with definition,
;      and a line with comments
;
; :Params:
;   for every parameter a line with definition,
;      and a line with comments
;
; :Author: RAT Team or your name(s)
;
; :Categories: SAR, PolSAR, InSAR, PolInSAR, decomposition, speckle filter,
; General, Infrastructure, spectral tools, image enhancment,
; PolSAR parameters, coherence, experimental
;
; :Copyright:
;   usually dosn't need changes
;------------------------------------------------------------


; docformat = 'rst'
;+
; Template function
;
; :Keywords:
;    called: in, optional, type="flag"
;       call routine without GUI in batchmode
;    
; :Params:
;
; :Author: 
; 
; :Categories: 
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
pro template_overlap, CALLED = called, BOXSIZE = boxsize

   common rat, types, file, wid, config, tiling
   compile_opt idl2

;------------------------------------------------------------
; Error Handling 1 (EXAMPLE)
; -> check if the input file is suitable for the routine
; -> all file type numbers can be found in definitions.pro
;------------------------------------------------------------
   
   if file.type ne 100 then begin
      error_button = DIALOG_MESSAGE(['Data have to be a SAR amplitude data'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif

;------------------------------------------------------------
; Batch mode (FIXED+EXAMPLE)
; -> take parameters from keywords or use default values
;------------------------------------------------------------
   
   if not keyword_set(boxsize) then boxsize = 7l       ; Default values

;------------------------------------------------------------
; Graphical interface mode (FIXED+EXAMPLE)
; -> used only if the keyword /called is not set
;    this is important as possibly other routines want to use
;    your routine as a batch process
;------------------------------------------------------------

   if not keyword_set(called) and not config.batch then begin             ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=3,TITLE='Template Filter',/floating,/tlb_kill_request_events,/tlb_frame_attr)
      field1   = CW_FIELD(main,VALUE=boxsize,/integer,  TITLE='Filter boxsize        : ',XSIZE=3)
      buttons  = WIDGET_BASE(main,column=3,/frame)
      but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
      but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
      but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
      WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
      pos = center_box(toto[0],drawysize=toto[1])
      widget_control, main, xoffset=pos[0], yoffset=pos[1]
	
      repeat begin
         event = widget_event(main)
         if event.id eq but_info then begin               ; Info Button clicked
            infotext = ['TEMPLATE FILTER EXAMPLE',$
            ' ',$
            'RAT module written 10/2007 by Max Musterman',$
            ' ',$
            'further information:',$
            'M.S. Godroth: Advanced template filtering of ergodic signals',$
            'IEEE Transactions on Template Programming, Vol.17, pp. 24-32, 1998']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
         endif
      endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
      widget_control,field1,GET_VALUE=boxsize
      widget_control,main,/destroy                        ; remove main widget
      if event.id ne but_ok then return                   ; OK button _not_ clicked
   endif

;------------------------------------------------------------
; Error Handling 2 (EXAMPLE)
; -> check validity of parameters
;------------------------------------------------------------

   if boxsize lt 3 then begin                                 ; Wrong box size ?
      error = DIALOG_MESSAGE("Boxsize has to be >= 3", DIALOG_PARENT = wid.base, TITLE='Error',/error)
      return
   endif

;------------------------------------------------------------
; change mousepointer to hourglass (FIXED)
;------------------------------------------------------------
   
   WIDGET_CONTROL,/hourglass

;------------------------------------------------------------
; Undo function (FIXED)
; -> save actual data set in temporary directory
;------------------------------------------------------------
   
   undo_prepare,outputfile,finalfile,CALLED=CALLED

;------------------------------------------------------------
; Read / write file header (FIXED+EXAMPLE)
; -> If the parameters of the output data are not identical
;    to the input data, a different (correct) header has to
;    be written
;------------------------------------------------------------
   
   rrat,file.name, ddd,header=head,info=info,type=type		
   srat,outputfile,eee,header=head,info=info,type=type		

;------------------------------------------------------------
; Initialise vertical tiling with overlap (FIXED)
; -> omit keyword if no overlap is desired
;------------------------------------------------------------
   
   tiling_init,overlap=(boxsize+1)/2

;------------------------------------------------------------
; Pop up progress window (FIXED)
;------------------------------------------------------------
   
   progress,Message='Template Filter...',/cancel_button

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
      block = myfilter(block,boxsize)
; -------- YOUR FILTER CALL----------	

;------------------------------------------------------------
; Write tile to output file (FIXED)
;------------------------------------------------------------

      tiling_write,eee,i,temporary(block)

;------------------------------------------------------------
; Jump back in input file to solve overlap problems (FIXED)
;------------------------------------------------------------

      tiling_jumpback,ddd
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
   
   evolute,'Template filtering, boxsize : '+strcompress(boxsize,/R)

end

