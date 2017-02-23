;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_remove_topo
; written by    : Maxim Neumann
; last revision : 22.March 2005
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

;;; input: T : coherency or covariance matrix
;;; REF_CHANNEL and BOX are in general well adapted and do not need modification.
function remove_topo_T, input_T, REF_CHANNEL=topo_ch,BOX=box
  T   = input_T
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl
  if n_elements(BOX) eq 0 then box = 15
  topo_box = [1,1,box,box]

  for m=0,n_tr-1 do for s=m+1,n_tr-1 do begin
     topo_ch = [m,s]*pol+1      ; choose VVVV channel (or double-bounce)
     phi = atan(T[topo_ch[0],topo_ch[1],*,*],/phase)
     C   = complex(cos(phi),sin(phi))
     Cs  = smooth(C,topo_box,/E)
     Cs /= abs(Cs)
     for j=m*pol,(m+1)*pol-1 do for i=s*pol,(s+1)*pol-1 do $
        T[i,j,*,*] *= Cs
     for j=s*pol,(s+1)*pol-1 do for i=m*pol,(m+1)*pol-1 do $
        T[i,j,*,*] /= Cs
  endfor
  return,T
end
function remove_topo_k, k, REF_CHANNEL=topo_ch,BOX=box
  polin_get_info,pol=pol,tracks=n_tr,baselines=n_bl
  if n_elements(BOX) eq 0 then box = 20
  topo_box = [1,1,box,box]

  for s=1,n_tr-1 do begin
     phi = atan(k[1, 0, *,*]*conj(k[1, s, *,*]),/phase); choose VV channel
     C   = complex(cos(phi),sin(phi))
     Cs  = smooth(C,topo_box,/E)
     Cs /= abs(Cs)
     for j=0, pol-1 do  $
        k[j,s,*,*] *= Cs
  endfor
  return,k
end

pro polin_remove_topo,CALLED=called, box_given=box_given
  common rat, types, file, wid, config
  compile_opt idl2

; check if array is complex

   if ~((file.type ge 500 && file.type le 513)) then begin
      error_button = DIALOG_MESSAGE('Wrong input data type', DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
      return
   endif
   if n_elements(box_given) ne 0 then box = box_given else $
      box = 20
   show_intrf = 0
   if not keyword_set(called) then begin ; Graphical interface
      main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4, $
                         TITLE='ML Topography Estimation and Removal',/modal,/tlb_kill_request_events,/tlb_frame_attr)
      field1 = CW_FIELD(main,VALUE=box,/integer, $
                        TITLE='Smooth box (default is sufficient) : ',XSIZE=3)
      intrfpha=cw_bgroup(main," generate interferometric phases afterwards",/nonexclusive,set_value=[show_intrf])
      buttons = WIDGET_BASE(main,column=3,/frame)
      but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
      but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
      but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
      WIDGET_CONTROL, main,/REALIZE,default_button=but_canc,tlb_get_size=toto
      pos = center_box(toto[0],drawysize=toto[1])
      widget_control, main, xoffset=pos[0], yoffset=pos[1]
      repeat begin              ; Event loop
         event = widget_event(main)
         if event.id eq but_info then begin ; Info Button clicked
            infotext = ['MAXIMUM LIKELIHOOD TOPOGRAPHY REMOVAL',$
                        ' ',$
                        'RAT module written 2005 by Maxim Neumann']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                  TITLE='Information')
         end
      endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
      widget_control,field1,GET_VALUE=box ; read widget fields
      widget_control,intrfpha,GET_VALUE=show_intrf
      widget_control,main,/destroy ; remove main widget
      if event.id ne but_ok then return ; OK button _not_ clicked
   endif

; change mousepointer
   WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type
   info += ' toporm'
   srat,outputfile,eee,header=head,info=info,type=type

; calculating preview size and number of blocks
   bs = config.blocksize
   overlap = (box + 1) / 2
   calc_blocks_overlap,file.ydim,bs,overlap,anz_blocks,bs_last
;   calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
   blocksizes = intarr(anz_blocks)+bs
   blocksizes[anz_blocks-1] = bs_last

   ypos1 = 0                    ; block start
   ypos2 = bs - overlap         ; block end
   byt=[0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8] ; bytelength of the different variable typos

; pop up progress window
   progress,Message='Removing the topographic phase...',/cancel_button

;start block processing
   for i=0,anz_blocks-1 do begin ; normal blocks
      progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
		if wid.cancel eq 1 then return

      block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]], $
                         type=file.var,/NOZERO)
      readu,ddd,block
; -------- THE FILTER ----------
      if file.type ge 510 then $
         block=remove_topo_T(block,box=box) $
      else $
         block=remove_topo_k(block,box=box)
; -------- THE FILTER ----------
      if i eq anz_blocks-1 then ypos2 = bs_last
      writeu,eee,block[*,*,*,ypos1:ypos2-1]
      ypos1 = overlap
      point_lun,-ddd,file_pos
      point_lun,ddd,file_pos - 2 * overlap * file.vdim * file.zdim * file.xdim * byt[file.var]
   endfor
   free_lun,ddd,eee

; update file information

   file_move,outputfile,finalfile,/overwrite

   file.info = info
   file.name = finalfile

   evolute,'Topographic phase removed. Box: '+strcompress(box,/R)

; generate preview
   if show_intrf then polin_interf_pha $
   else if ~keyword_set(called) then begin
;      generate_preview  ;;; because no visible change in amplitude!
      progress,/DESTROY
      update_info_box
   endif

end
