;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: multi_statistics
; last revision : 12 Jan. 2005
; written by    : Maxim Neumann
; Statistics about multiple channels
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

pro multi_statistics,CALLED = called
  common rat, types, file, wid, config

;---- TEST IF A DATASET IS LOADED ----
	if file.type eq 0 then begin
		error_button = DIALOG_MESSAGE(['Please load data'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
		return
	endif


; change mousepointer
  WIDGET_CONTROL,/hourglass
  if	config.os eq 'windows' then newline = strcompress(13B) + string(10B)
  if	config.os eq 'unix' then newline = string(10B)


; pop up progress window
;  progress,Message='Multiple channel statistics...',/cancel_button

  rrat,file.name,ddd,header=head,info=info,type=type
  block = make_array([file.vdim,file.zdim,file.xdim,file.ydim],type=file.var)
  readu,ddd,block
  free_lun,ddd
;  progress,percent=50,/check_cancel

  main = WIDGET_BASE(GROUP_LEADER=wid.base,row=2,TITLE='Multiple channel statistics',/floating,/tlb_kill_request_events,/tlb_frame_attr)
  tx_base=WIDGET_BASE(main,column=2)
  tx1  = widget_text(tx_base,XSIZE=40,ysize=50,/scroll)
  if file.var eq 6 || file.var eq 9 then $
     tx2  = widget_text(tx_base,XSIZE=40,ysize=50,/scroll)
  buttons  = WIDGET_BASE(main,column=2,/frame)
  but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  WIDGET_CONTROL, main, /REALIZE, default_button = but_ok,tlb_get_size=toto
  pos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, XOFFSET=pos[0], yoffset=pos[1]

  text1 = ''
  text2 = ''
  for v=0,file.vdim-1 do for z=0,file.zdim-1 do begin
     if file.var eq 6 || file.var eq 9 then begin
        ch = abs(block[v,z,*,*])
        m = moment(ch,MDEV=mdev,SDEV=sdev,/NAN)
        mx= max(ch,mxpos,min=mn,sub=mnpos,/NAN)
        md= median(ch)
        text1 += newline + $
                 'Channel ['+strcompress(v,/r)+','+strcompress(z,/r)+'] AMPLITUDE'+newline+ $
                 'Min       :'+strcompress(mn)+' at ['+strcompress((array_indices(block,mnpos))[2],/r)+','+strcompress((array_indices(block,mnpos))[3],/r)+']'+newline+ $
                 'Max       :'+strcompress(mx)+' at ['+strcompress((array_indices(block,mxpos))[2],/r)+','+strcompress((array_indices(block,mxpos))[3],/r)+']'+newline+ $
                 'Mean     $:'+strcompress(m[0])+newline+ $
                 'StdDev   $:'+strcompress(sdev)+newline+ $
                 'Variance  :'+strcompress(m[1])+newline+ $
                 'Skewness  :'+strcompress(m[2])+newline+ $
                 'Kurtosis  :'+strcompress(m[3])+newline+ $
                 'MeanAbsDev:'+strcompress(mdev)+newline
        widget_control, tx1, SET_VALUE = text1
        ch = atan(block[v,z,*,*],/phase)
        m = moment(ch,MDEV=mdev,SDEV=sdev,/NAN)
        mx= max(ch,mxpos,min=mn,sub=mnpos,/NAN)
        md= median(ch)
        text2 += newline + $
                 'Channel ['+strcompress(v,/r)+','+strcompress(z,/r)+'] PHASE'+newline+ $
                 'Min       :'+strcompress(mn)+' at ['+strcompress((array_indices(block,mnpos))[2],/r)+','+strcompress((array_indices(block,mnpos))[3],/r)+']'+newline+ $
                 'Max       :'+strcompress(mx)+' at ['+strcompress((array_indices(block,mxpos))[2],/r)+','+strcompress((array_indices(block,mxpos))[3],/r)+']'+newline+ $
                 'Mean     $:'+strcompress(m[0])+newline+ $
                 'StdDev   $:'+strcompress(sdev)+newline+ $
                 'Variance  :'+strcompress(m[1])+newline+ $
                 'Skewness  :'+strcompress(m[2])+newline+ $
                 'Kurtosis  :'+strcompress(m[3])+newline+ $
                 'MeanAbsDev:'+strcompress(mdev)+newline
     widget_control, tx2, SET_VALUE = text2
     endif else begin
        ch = block[v,z,*,*]
        m = moment(ch,MDEV=mdev,SDEV=sdev,/NAN)
        mx= max(ch,mxpos,min=mn,sub=mnpos,/NAN)
        md= median(ch)
        text1 += newline + $
                 'Channel ['+strcompress(v,/r)+','+strcompress(z,/r)+']'+newline+ $
                 'Min       :'+strcompress(mn)+' at ['+strcompress((array_indices(block,mnpos))[2],/r)+','+strcompress((array_indices(block,mnpos))[3],/r)+']'+newline+ $
                 'Max       :'+strcompress(mx)+' at ['+strcompress((array_indices(block,mxpos))[2],/r)+','+strcompress((array_indices(block,mxpos))[3],/r)+']'+newline+ $
                 'Mean     $:'+strcompress(m[0])+newline+ $
                 'StdDev   $:'+strcompress(sdev)+newline+ $
                 'Variance  :'+strcompress(m[1])+newline+ $
                 'Skewness  :'+strcompress(m[2])+newline+ $
                 'Kurtosis  :'+strcompress(m[3])+newline+ $
                 'MeanAbsDev:'+strcompress(mdev)+newline
     widget_control, tx1, SET_VALUE = text1
     endelse
  endfor

; ---- Event loop
  repeat begin
     event = widget_event(main)
     if event.id eq but_info then begin ; Info Button clicked
        infotext = ['MULTIPLE CHANNEL STATISTICS',$
                    ' ',$
                    'RAT module written 01/2005 by Maxim Neumann']
        info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, TITLE='Information')
     end
  endrep until (event.id eq but_ok) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'
  widget_control,main,/destroy  ; remove main widget
; switch back to main draw widget
  widget_control,wid.draw,get_value=index
  wset,index

;  progress,/destroy
end
