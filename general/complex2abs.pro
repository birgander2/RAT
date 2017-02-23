; docformat = 'rst'
;+
; Transforms a complex image to absolute (intensity) values.
;
; :Keywords:
;    called: in, optional, type="flag"
;       call routine without GUI in batchmode
;
; :Author: RAT Team
; :Categories: General
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
pro complex2abs,CALLED=called
   compile_opt idl2
   common rat, types, file, wid, config, tiling

; check if array is complex

   if file.var ne 6 and file.var ne 9 then begin
      error_button = DIALOG_MESSAGE('Data not complex', DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif

; change mousepointer

   WIDGET_CONTROL,/hourglass

; undo function
   
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type		
   head[head[0]+1] = 4l         ; Output is float
   if type eq 101 then type=100
   srat,outputfile,eee,header=head,info=info,type=type		
   
; calculating preview size and number of blocks

   tiling_init
   progress,Message='Transform Complex -> Amplitude...',/cancel_button

;start block processing

   for i=0,tiling.nr_blocks-1 do begin ; normal blocks
      progress,percent=(i+1)*100.0/tiling.nr_blocks,/check_cancel
      if wid.cancel eq 1 then return

      tiling_read,ddd,i,block
      tiling_write,eee,i,abs(block)
   endfor
   free_lun,ddd,eee

; update everything

   rat_finalise,outputfile,finalfile,CALLED=called
   evolute,'Transformation Complex->Amplitude'
   
end
