;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: tiling
; last revision : 10.Oct.2007
; written by    : Andreas Reigber
;
; Some subroutines for simplifying the handling of image tiles
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

pro tiling_init,OVERLAP = overlap
; Initialise tiling with given block overlap
; Omit keyword for no overlap
   common rat, types, file, wid, config, tiling

   if keyword_set(overlap) then $
     calc_blocks_overlap,file.ydim,config.blocksize,overlap,anz_blocks,bs_last $
   else begin
      calc_blocks_normal, file.ydim,config.blocksize,anz_blocks,bs_last
      overlap = 0
   endelse

   tiling.nr_blocks = anz_blocks
   tiling.overlap   = overlap
   tiling.blocksizes = ptr_new(intarr(anz_blocks))
   *tiling.blocksizes = intarr(anz_blocks)+config.blocksize
   (*tiling.blocksizes)[anz_blocks-1] = bs_last

end

pro tiling_jumpback,lun
; Jump back in inputfile to solve the problem of overlapping blocks
   common rat, types, file, wid, config, tiling
   if tiling.nr_blocks gt 1 then begin
      xdrsize = [0,1,4,8,4,8,8,0,0,16,0,0,4,4,8,8]
      point_lun,-lun,file_pos
      point_lun,lun,long64(file_pos) - long64(2) * tiling.overlap * file.vdim * file.zdim * file.xdim * xdrsize[file.var]
   endif
end

pro tiling_write,lun,nr,block
; Write tile number nr, taking into account possible overlap
   common rat, types, file, wid, config, tiling

   pos = lonarr(2)
   if nr eq 0 then pos[0] = 0 else pos[0] = tiling.overlap
   if nr eq tiling.nr_blocks-1 then pos[1] = (*tiling.blocksizes)[tiling.nr_blocks-1] else pos[1] = config.blocksize - tiling.overlap
   writeu,lun,block[*,*,*,pos[0]:pos[1]-1]
end

pro tiling_read,lun,nr,block
; Read tile number nr, taking into account possible overlap
   common rat, types, file, wid, config, tiling

   block = make_array([file.vdim,file.zdim,file.xdim,(*tiling.blocksizes)[nr]],type=file.var)
   readu,lun,block
end
