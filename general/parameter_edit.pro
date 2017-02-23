;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: parameter_edit
; last revision : 15.08.2006
; written by    : Maxim Neumann
; Edit information about set parameters.
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

pro parameter_edit,CALLED = called
  common rat, types, file, wid, config
  common rit, pars,evolution    ; pars== parameters structure ==parstruct

  PN       = ptr_new()
  newline  = config.os eq 'unix'? string(10b): string(13b)+string(10b)
  vartypes = ['UNDEFINED','BYTE','INT','LONG','FLOAT','DOUBLE','COMPLEX','STRING','STRUCT','DCOMPLEX','UNDEFINED','UNDEFINED','UINT','ULONG','LONG64','ULONG64']
  undef    = [0L,8,10,11]
  dimstxt  = ['scalar',strcompress(indgen(7)+1,/R)+'-dim']

  line     = {name:'', type:0L, n_dim:0L, dim:lonarr(8), value:'', $
              wid:0L, wid_type:0L, wid_ndim:0L, wid_dim:0L, wid_value:0L}
  n_par    = n_tags(pars)
  par      = replicate(line,n_par)
  par[*].name = tag_names(pars)
  for i=0,n_par-1 do $
     if pars.(i) ne PN then begin
     siz = size(*pars.(i))
     par[i].type  = siz[siz[0]+1]
     par[i].n_dim = siz[0]
     if siz[0] ne 0 then par[i].dim[0:siz[0]-1]=siz[1:siz[0]]
     if par[i].type ne 8 then par[i].value=strjoin(strcompress((*pars.(i))[*])) $
     else par[i].value='STRUCTURES are not editable from this appliction!'
  endif

  main = WIDGET_BASE(GROUP_LEADER=wid.base,/column,TITLE='Edit Parameter Information',/floating,/tlb_kill_request_events,/tlb_frame_attr,/scroll,Y_SCROLL=800,X_SCROLL=1000)
  for i=0,n_par-1 do begin
     par[i].wid 	= widget_base(main,/row)
     tmp        	= widget_label(par[i].wid,value=par[i].name,xsize=150,/align_left)
     par[i].wid_type 	= widget_droplist(par[i].wid,value=vartypes,xsize=130,title="TYPE:")
     par[i].wid_ndim 	= widget_droplist(par[i].wid,value=dimstxt,title="N-DIM:")
     par[i].wid_dim  	= widget_label(par[i].wid,value=par[i].n_dim eq 0?'            ':strjoin(strcompress(par[i].dim[0:par[i].n_dim-1],/R),'x'),xsize=80,/align_left)
     par[i].wid_value	= widget_text(par[i].wid,value=par[i].value,xsize=150,/EDITABLE)
  endfor
  buttons 	= WIDGET_BASE(main,/row,/BASE_ALIGN_CENTER,XSIZE=400)
  but_ok  	= WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
  but_canc	= WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
  but_info	= WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
  but_save  	= WIDGET_BUTTON(buttons,VALUE=' Save now! ',xsize=80)
  WIDGET_CONTROL, main,/REALIZE,default_button=but_ok, tlb_get_size=toto
  guipos = center_box(toto[0],drawysize=toto[1])
  widget_control, main, xoffset=guipos[0], yoffset=guipos[1]
  for i=0,n_par-1 do begin
     widget_control,par[i].wid_type,set_droplist_select=par[i].type
     widget_control,par[i].wid_ndim,set_droplist_select=par[i].n_dim
     if total(par[i].type eq undef) then begin
        widget_control,par[i].wid_ndim,sens=0
        widget_control,par[i].wid_value,sens=0
     endif
  endfor

  repeat begin
     event = widget_event(main)
     if event.id eq but_info then $
        info = DIALOG_MESSAGE(['EDIT PARAMETER INFORMATION', $
                               'RAT module written 08/2006 by Maxim Neumann','', $
                               'Rules:', $
                               'Edit only, if you know exactly what you do!'], $
                              DIALOG_PARENT = main,/INFORMATION)
     for i=0,n_par-1 do $
        case event.id of
        par[i].wid_type: begin
           par[i].type = event.index
           sensitive   = ~total(event.index eq undef)
           widget_control,par[i].wid_ndim,sens=sensitive
           widget_control,par[i].wid_value,sens=sensitive
        end
        par[i].wid_ndim: begin
           par[i].n_dim = event.index
           if event.index eq 0 then dims=0 $
           else begin
              dims = lonarr(event.index)
              mtmp = widget_base(group_leader=main,/col,/floating,title="Please provide dimension sizes!")
              for j=0,event.index-1 do $
                 dims[j] = cw_field(mtmp,/int,title='Dim '+strcompress(j,/R)+': ',value=1,xsize=6)
              dims_ok  	= WIDGET_BUTTON(mtmp,VALUE=' OK ',xsize=80,/frame)
              WIDGET_CONTROL, mtmp,/REALIZE,default_button=dims_ok
              repeat event=widget_event(mtmp) until event.id eq dims_ok
              for j=0,par[i].n_dim-1 do begin
                 widget_control,dims[j],get_value=tmp & dims[j]=tmp
              endfor
              widget_control,mtmp,/destroy ; remove main widget
           endelse
           par[i].dim=dims
           widget_control,par[i].wid_dim,set_value=par[i].n_dim eq 0?'':strjoin(strcompress(par[i].dim[0:par[i].n_dim-1],/R),'x')
        end
        else:
     endcase
  endrep until event.id eq but_ok || event.id eq but_canc || event.id eq but_save || (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
  for i=0,n_par-1 do begin
     widget_control,par[i].wid_value,get_value=tmp & par[i].value=tmp
  endfor
  widget_control,main,/destroy  ; remove main widget
  if event.id ne but_ok && event.id ne but_save then return ; OK button _not_ clicked

  for i=0,n_par-1 do $
     if ~total(par[i].type eq undef) && $
     (pars.(i) eq PN || par[i].value ne strjoin(strcompress((*pars.(i))[*]))) then begin
     if par[i].type ne 6L && par[i].type ne 9L then begin
        val   = strsplit(strcompress(par[i].value),' ',/EXTRACT)
        if par[i].N_DIM eq 0 then begin
           if par[i].type eq 7L then value = par[i].value $
           else value = (make_array(1,TYPE=par[i].type,VALUE=val))[0]
        endif else begin
           value = make_array(par[i].dim[0:par[i].n_dim-1],TYPE=par[i].type,/NOZERO)
           if par[i].type eq 7L then value[*] = strsplit(strjoin(val,' '),'\\0',/REGEX,/EXTRACT) $
           else value[*] = val[*]
        endelse
     endif else begin
        cval = strsplit(par[i].value,' ,()',/EXTRACT)
        if par[i].N_DIM eq 0 then begin
           if par[i].type eq 6L then value = complex(cval[0],cval[1]) $
           else value = dcomplex(cval[0],cval[1])
        endif else begin
           value = make_array([2,par[i].dim[0:par[i].n_dim-1]],TYPE=par[i].type,/NOZERO)
           value[*] = cval
           if par[i].type eq 6L then value = $
              reform(complex(value[0,*,*,*,*,*,*],value[1,*,*,*,*,*,*])) $
           else value = $
              reform(dcomplex(value[0,*,*,*,*,*,*],value[1,*,*,*,*,*,*]))
        endelse
     endelse
     ignore=set_par(par[i].name,value)
  endif

  if event.id eq but_save then $
     save_rit

end
