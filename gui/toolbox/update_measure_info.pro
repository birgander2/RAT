;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: update_measure_info
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
pro update_measure_info,res
	common rat, types, file, wid, config
	common channel, channel_names, channel_selec, color_flag
	if xregistered('tool_box') ne 0 then begin
  		event = LookupManagedWidget('tool_box')
		widget_control, event, get_uvalue=state, /no_copy
	
		names = channel_names
		maxle = max(strlen(names))
		infostr = ''
		xo = 0
		yo = 0
		if	config.os eq 'windows' then newline = string(13B) + string(10B)
		if	config.os eq 'unix' then newline = string(10B)
		for i=0,file.vdim-1 do for j=0,file.zdim-1 do for k=0,maxle-strlen(channel_names[i*file.zdim+j]) do names[i*file.zdim+j] += ' '  
		x   = res.x
		y   = res.y
		xf  = round(float(x) / wid.draw_scale)  < file.xdim-1 > 0
		yf  = round(float(y) / wid.draw_scale)  < file.ydim-1 > 0
		if x ne xo or y ne yo then rrat,file.name,inblock,block=[xf,yf,1,1]
		xo  = x
		yo  = y
		
		infostr  = "Display coordinates : "+strcompress(x)+strcompress(y)
		infostr += newline
		infostr += "Data coordinates    : "+strcompress(xf)+strcompress(yf)
		infostr += newline+newline
		if file.var eq 6 or file.var eq 9 then begin
			infostr += "Data amplitude / phase value(s)"
			infostr += newline + newline
			for i=0,file.vdim-1 do for j=0,file.zdim-1 do infostr += names[i*file.zdim+j] + ' : ' + strcompress(abs(inblock[i*file.zdim+j])) + ' /' + strcompress(atan(inblock[i*file.zdim+j],/phase)*!radeg) + ' [deg]' + newline
		endif else begin
			infostr += "Data values(s)      : "
			infostr += newline + newline
			for i=0,file.vdim-1 do for j=0,file.zdim-1 do infostr += names[i*file.zdim+j] + ' : ' + strcompress(inblock[i*file.zdim+j]) + newline
		endelse
		widget_control,state.measure_text,set_value = infostr
		
		widget_control, event, set_uvalue=state, /no_copy
	endif
	
end
