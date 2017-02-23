;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; mirror_diag.pro   (RAT module)
; mirrors a data set diagonally (spatial transpose)
; 12/2007 by Maxim Neumann
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

pro mirror_diag,CALLED=called
	common rat, types, file, wid, config

	WIDGET_CONTROL,/hourglass

; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header

	rrat,file.name,ddd,header=head,info=info,type=type
        free_lun,ddd
        newx = head[head[0]]
        newy = head[head[0]-1]
        newhead = [head[0:head[0]-2],newx,newy,head[head[0]+1]]
	srat,outputfile,eee,header=newhead,info=info,type=type
	
; calculating preview size and number of blocks

	bs = config.blocksize
	calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
	blocksizes = intarr(anz_blocks)+bs
	blocksizes[anz_blocks-1] = bs_last

        xbs = config.blocksize
        calc_blocks_normal,file.xdim,Xbs,Xanz_blocks,Xbs_last
        Xblocksizes = intarr(Xanz_blocks)+Xbs
        Xblocksizes[Xanz_blocks-1] = Xbs_last

; pop up progress window

	progress,Message='Mirror diagonally...',/cancel_button

;start block processing
        dims = newhead[1:newhead[0]]

        for j=0,Xanz_blocks-1 do begin
           progress,percent=(j+1)*100.0/Xanz_blocks,/check_cancel
           xblock=make_array([file.vdim,file.zdim,newx,Xblocksizes[j]],type=file.var)
           for i=0,anz_blocks-1 do begin ; normal blocks
              if wid.cancel eq 1 then return
              rrat,file.name,block,block=[Xbs*j,bs*i,Xblocksizes[j],blocksizes[i]]
              dim = size(block,/n_dim)
              xblock[*,*,bs*i:bs*i+blocksizes[i]-1,*] = transpose(block,dim eq 4?[0,1,3,2]:dim eq 3?[0,2,1]:[1,0])
           endfor
           writeu,eee,xblock
        endfor
        free_lun,eee

; update file information

	file_move,outputfile,finalfile,/overwrite
	file.name = finalfile
        file.xdim = newx
        file.ydim = newy

        evolute,'Mirror the image diagonally (spatial transpose).'

; generate preview

	if not keyword_set(called) then begin
		generate_preview
		update_info_box
	endif

end
