;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; move_transpose_ratfile.pro
; copies the given file with simultaneous transpose operation
; on given dimensions.
; used for obsolete data type updates.
; 02/2007 by Maxim Neumann
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

pro move_transpose_ratfile, filename, transp_dims=transp_dims, new_type=new_type, new_info=new_info
  common rat, types, file, wid, config
  
  WIDGET_CONTROL,/hourglass

  tmpfile = config.tempdir+'tmp.rat'

; read / write header
  head = 1l
  rrat,filename,ddd,header=head,info=info,type=type

  dim  = head[0]
  vdim = head[0]ge 4?head[head[0]-3]:1
  zdim = head[0]ge 3?head[head[0]-2]:1
  xdim = head[head[0]-1]
  ydim = head[head[0]  ]
  var  = head[head[0]+1]

  srat,tmpfile,eee,header=[head[0],(head[1:head[0]])[transp_dims],head[head[0]+1]], $
       info= n_elements(new_info) gt 0 ? new_info : info, $
       type= n_elements(new_type) gt 0 ? new_type : type

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

;start block processing
  for i=0,anz_blocks-1 do begin ; normal blocks
     if wid.cancel eq 1 then return
     block = make_array([vdim,zdim,xdim,blocksizes[i]],type=var,/nozero)
     readu,ddd,block
     writeu,eee,transpose(block,transp_dims)
  endfor
  free_lun,ddd,eee


; update file information
  file_move,tmpfile,filename,/overwrite
  WIDGET_CONTROL,hourglass=0
end
