;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polar_basis
; written by    : Maxim Neumann
; last revision : 14.Nov.2005
; Performs transforms of the polarimetric basis of PolSAR
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

pro polar_basis_get, ellipt,orient,pauliB, ERROR=ERROR
  common rat, types, file, wid, config
  error = 0
  if (file.type ge 200L && file.type le 209L) || file.type eq 220L || file.type eq 222L || $
     file.type eq 500L || file.type eq 502L || file.type eq 510L || file.type eq 512L then $
     pauliB = 0 $
  else pauliB = 1
  if file.type eq 200L || file.type eq 220L || $
     file.type eq 500L || file.type eq 510L || file.type eq 501L || file.type eq 511L then begin
     ellipt = 0
     orient = 0
  endif else begin
     st1 = get_par('polbasis_ellipticity',ellipt)
     st2 = get_par('polbasis_orientation',orient)
     if st1 ne 0 || st2 ne 0 then begin
        error_button = DIALOG_MESSAGE(['No polarimetric basis angles are given!', $
;                                        'Set ellipticity and orientation angles to zero', $
;                                       'and proceed? Or cancel?', $
                                      ''], DIALOG_PARENT = wid.base, TITLE='Error',/E)
        ERROR=1
     endif
  endelse
end
function polar_basis_UB, rho, DIM=DIM
  if n_elements(DIM) eq 0 then DIM=3
  r  = rho
  rc = conj(r)
  r2 = r^2
  rc2= rc^2
  s2 = sqrt(2.)
  if DIM eq 3 then $
     UB = 1./(1.+r*rc)*[ $
          [ 1,      r2,   s2*r   ], $
          [ rc2,    1,    -s2*rc ], $
          [ -s2*rc, s2*r, 1-r*rc ]]
  if DIM eq 4 then $
     UB = 1./(1.+r*rc)*[ $
          [ 1,  r2,  r,   r    ], $
          [ rc2, 1,-rc,  -rc   ], $
          [ -rc, r,  1,  -r*rc ], $
          [ -rc, r,-r*rc, 1    ]]
  UB = transpose(UB)          ; because of mathematical notation convention
  return, UB
end
function polar_basis_rho,ellipt,orient
  return, (cos(2.*ellipt)*sin(2.*orient)+complex(0,sin(2.*ellipt)))/(1.+cos(2.*ellipt)*cos(2.*orient)+(MACHAR()).EPS)
end



pro polar_basis,CALLED = called,predef,pauliB
  common rat, types, file, wid, config

; check if array is usable
  if file.type ne 200L && file.type ne 209L && file.type ne 220L && file.type ne 222L  then begin
     error_button = DIALOG_MESSAGE(['Data has to be a','PolSAR data type','in lexicographic matrix basis'],  $
                                    DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif
  if n_elements(pauliB) ne 0 && pauliB then begin
     error_button = DIALOG_MESSAGE(['Polarimetric basis change','only from and into','lexicographic matrix basis'],  $
                                    DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif

  polar_basis_get,elliptOrig,orientOrig,pauliBOrig

;---------------------------------
  nr_of_channels = file.zdim
  txt_pre = [' -> HV', $
             ' -> circular',$
             ' -> linear 45deg']
  ellipt_pre = [0., !pi/4.,  0.   ]
  orient_pre = [0.,  0,     !pi/4.]
;  rho_pre = [complex(0,1),complex(0,-1),complex(1,0),complex(-1,0)]
;  strangepol = 0
  
  if n_elements(predef) ne 0 then begin
     if predef ge 0 && predef le 2 then begin
        ellipt = ellipt_pre[predef]
        orient = orient_pre[predef]
     endif else begin
        ellipt = elliptOrig
        orient = orientOrig
     endelse
     if n_elements(pauliB) eq 0 $
     then pauliB = pauliBOrig
  endif else begin
     ellipt = ellipt_pre[0]
     orient = orient_pre[0]
     pauliB = pauliBOrig
     
     main = WIDGET_BASE(GROUP_LEADER=wid.base,column=1,TITLE='Basis transformation',/floating,/tlb_kill_request_events,/tlb_frame_attr)
;     dummy2   = widget_label(main,value='Current matrix basis: '+(pauliBOrig?'Pauli':'Lexicographic'))
     dummy3   = widget_label(main,value='Current Ellipticity: '+strcompress(elliptOrig/!DTOR))
     dummy4   = widget_label(main,value='Current Orientation: '+strcompress(orientOrig/!DTOR))
     dummy    = widget_label(main,value='Give new ellipticity and orientation angles in degrees :',/align_left)
     main2    = widget_base(main,column=2)
     field1   = CW_FIELD(main2,VALUE=ellipt/!DTOR,/floating,TITLE='Ellipticity (-45,+45):',XSIZE=5)
     field2   = CW_FIELD(main2,VALUE=orient/!DTOR,/floating,TITLE='Orientation (-90,+90):',XSIZE=5)
;     but_basis= cw_bgroup(main,set_value=pauliB,['Lexicographic matrix','Pauli matrix'],/exclusive,/row,label_left='New matrix basis: ')
     drop     = WIDGET_droplist(main,value=txt_pre, title="Predefined : ")
     buttons  = WIDGET_BASE(main,column=3,/frame)
     but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     
     WIDGET_CONTROL, main, /REALIZE, default_button = but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]

     
     repeat begin               ; Event loop
        event = widget_event(main)
         if event.id eq but_info then begin ; Info Button clicked
            infotext = ['POLSAR BASIS CHANGE',$
                        ' ',$
                        'RAT module written 11/2005 by Maxim Neumann']
            info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                  TITLE='Information')
         endif
         if event.id eq drop then begin
            dropindex = Widget_Info(drop, /DropList_Select)
            ellipt = ellipt_pre[dropindex]
            orient = orient_pre[dropindex]
            widget_control,field1,set_value=ellipt/!DTOR
            widget_control,field2,set_value=orient/!DTOR
         endif
;          if event.id eq field1 then begin
;             widget_control,field1,GET_VALUE=ellipt ; read widget fields
;             widget_control,field1,set_value=-45>ellipt<45
;          endif
;          if event.id eq field1 then begin
;             widget_control,field2,GET_VALUE=orient ; read widget fields
;             widget_control,field2,set_value=-90>orient<90
;          endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

     if event.id eq but_ok then begin
        widget_control,field1,GET_VALUE=ellipt ; read widget fields
        widget_control,field2,GET_VALUE=orient ; read widget fields
;        widget_control,but_basis,GET_VALUE=pauliB ; matrix basis
        ellipt=-45>ellipt<45
        orient=-90>orient<90
        ellipt *= !DTOR
        orient *= !DTOR
     endif
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return
  endelse

; calculating transformation matrix
;---------------------------------------------------------------
  rho      = polar_basis_rho(ellipt,orient)
  rhoOrig  = polar_basis_rho(elliptOrig,orientOrig)
  if rho eq rhoOrig && pauliB eq pauliBOrig then begin
     error_button = DIALOG_MESSAGE(['You have chosen the same polarization basis!','','Return without changes.'], DIALOG_PARENT = wid.base, TITLE='Error',/ERROR)
     return
  endif
  j      = complex(0,1)
;  pol    = (file.type lt 210L? file.zdim: file.zdim/2)
  pol    = file.zdim
  D3     = 1./sqrt(2.) * [ $
           [ 1,  1,   0     ], $
           [ 1, -1,   0     ], $
           [ 0,  0, sqrt(2.)]]
  D4     = 1./sqrt(2.) * [ $
           [ 1, 1, 0, 0 ], $
           [ 1,-1, 0, 0 ], $
           [ 0, 0, 1, j ], $
           [ 0, 0, 1,-j ]]
  D      = (pol eq 3? D3: D4)
  UB     = polar_basis_UB(rho,DIM=pol)
  UBOrig = polar_basis_UB(rhoOrig,DIM=pol)
  UTM = UB # conj(transpose(UBOrig))
  if pauliBOrig eq 1 then $
     UTM #= conj(transpose(D))
  if pauliB eq 1 then $
     UTM = D # UTM
  UTMc = conj(transpose(UTM))
;  UTM2 = complexarr(pol*2,pol*2)
;  UTM2[0:pol-1,0:pol-1] = UTM
;  UTM2[pol:*,pol:*] = UTM
;  UTM2c = conj(transpose(UTM2))
;---------------------------------------------------------------

  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED

; read / write header
  if file.type eq 200L || file.type eq 209L then newtype = 209L $
  else if file.type eq 220L || file.type eq 222L then newtype = 222L
  if ellipt eq ellipt_pre[0] && orient eq orient_pre[0] then $
     if file.type le 209L then newtype = 200L $
     else newtype = 220L

  head = 1l
  rrat,file.name,ddd,header=head,info=info,type=type
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last 
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

; pop up progress window
  progress,Message='Polarimetric Basis Transformation...',/cancel_button
  
; block processing
  for i=0,anz_blocks-1 do begin
     progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
     if wid.cancel eq 1 then return

     block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]],type=file.var)
     readu,ddd,block

;     for j=0,blocksizes[i]-1 do block[*,*,j] = transpose(matrix_multiply(block[*,*,j],mat,/atranspose))
     if file.type ge 210L then $
        for y=0,blocksizes[i]-1 do for x=0,file.xdim-1 do $
           block[*,*,x,y] = UTM # reform(block[*,*,x,y]) # UTMc $
     else for y=0,blocksizes[i]-1 do for x=0,file.xdim-1 do $
        block[0,*,x,y] = UTM # reform(block[0,*,x,y])

     writeu,eee,block
  endfor
  free_lun,ddd,eee

; update file information
  file.type = newtype

; update parameter information
  st1 = set_par('polbasis_ellipticity',ellipt)
  st2 = set_par('polbasis_orientation',orient)

; evolution
  evolute,'PolSAR basis transformation to ellipticity:'+strcompress(ellipt/!DTOR)+' orientation:'+strcompress(orient/!DTOR)
;          ' new matrix basis: '+(pauliB?'Pauli':'Lexicographic')

  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile

; generate preview
  if not keyword_set(called) then begin
     generate_preview
     update_info_box
  endif
end
