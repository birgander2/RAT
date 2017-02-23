;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module:     polin_classif_wishart
; written by    : Maxim Neumann
; last revision : 17.5.5
; General k-means Wishart Classification of PolInSAR data
; with Initialization, Subclasses and Presegmentation
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; intern routines ;;;;;;;;;;;;;;;;;;;;
FUNCTION argmin, x, minimum=minimum
  m=min(x,pos)
  IF arg_present(minimum) THEN minimum=m
  return, pos
END
pro polin_classif_wins, win, QUADRATIC = quadratic, S5 = s5, CLOSE=close, RANGE=range, POSITION=pos, XPOS=xpos, YPOS=ypos, $
          TITLE=title, LARGE=large, LONG=long, GET=get, WIDE=wide
  if keyword_set(CLOSE) then begin
     DEVICE,window_state = wopen
     if n_elements(win) eq 0 then begin
        for i=0,n_elements(wopen)-1 do $
          if wopen[i] then wdelete, i
     endif else if wopen[win] then wdelete, win
     return
  endif
  if (n_elements(win) eq 0) || (win lt 0) then begin
     DEVICE,window_state=wopen
     win = (where(~wopen))[0]
  endif
  if n_elements(RANGE) eq 2 then begin
     xs = RANGE[0]
     ys = RANGE[1]
  endif else begin
     xs = 500
     ys = 350
  endelse
  if keyword_set(quadratic) then begin
     xs=400
     ys=400
  endif
  if keyword_set(s5) then begin
     xs=512
     ys=512
  endif
  if keyword_set(LARGE) then begin
     xs=1024 & ys=1024
  endif
  if keyword_set(LONG) then begin
     xs=512  & ys=1024
  endif
  if keyword_set(WIDE) then begin
     xs=1024 & ys=512
  endif
  if n_elements(TITLE) eq 0 then title='IDL'+strcompress(win)
  if n_elements(pos) gt 1 then xpos=pos[0]
  if n_elements(pos) eq 2 then ypos=pos[1]
  window,win,xs=xs,ys=ys,TITLE=title,FREE=(win lt 0 || win gt 31),xpos=xpos,ypos=ypos
  if arg_present(GET) ne 0 then GET=!D.WINDOW
  win = !D.WINDOW
  return
end
pro sa_reform, a, orig_size
  orig_size = size(a,/DIMEN)
  N_DIM     = size(a,/N_DIM)
  xy  = orig_size[N_DIM-2] * orig_size[N_DIM-1]
  if N_DIM gt 2 then vz  = product(orig_size[0:N_DIM-3]) $
  else vz = 1
  a = reform(a,vz,xy,/OVERWRITE)
end
pro sa_rereform, a, orig_size
  a = reform(a,orig_size,/OVERWRITE)
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; SEGMENTATION CLUSTERING CLASSIFICATION ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function proto2img,p,segments
;;; generate a full image from prototype and segments data!!!
  t   = systime(1) 
  print,'start to generate the full image from given prototype and segmentation data...'
  h   = histogram(segments,reverse_indices=r)
  orig_size = size(p,/dim)
  sa_reform,reform(p,[[orig_size],1],/OVERWRITE),orig_size_tmp
  siz = size(p,/DIM)

  a = make_array(siz[0],n_elements(segments),TYPE=size(p,/TYPE),/NOZERO)
  for i=0L, n_elements(h)-1 do $ $
     for j=r[i],r[i+1]-1 do $
        a[*,r[j]] = p[*,i]

  sa_rereform,p,orig_size
  if n_elements(orig_size) eq 1 then $
     sa_rereform,a,size(segments,/DIM) $
  else sa_rereform,a,[orig_size[0:n_elements(orig_size)-2],size(segments,/DIM)]
  print,'finished in '+strcompress(systime(1)-t,/R)+' seconds.'
  return,a
end
function segmentize,a,segments,PROTOTYPE=PROTOTYPE,MEDIAN=MEDIAN
;;; segment markers start at 0!!
;;; uses: sa_reform, sa_rereform
  t   = systime(1) 
  print,'start to segmentize (to avarage the input in given segments) and generate the ' $
        + (keyword_set(PROTOTYPE)?'prototype...':'image...')
  sa_reform,a,orig_size
  siz = size(a,/DIM)
  h   = histogram(segments,reverse_indices=r,min=0)

  if keyword_set(PROTOTYPE) then begin
;     seg = scc_rmtiny(segments) ; if used then the segments should also be
;     changed, else proto2img does not work anymore!
     res = make_array(siz[0],n_elements(h),TYPE=size(a,/TYPE))
     for i=0L,n_elements(h)-1 do $
        if r[i] lt r[i+1]-1 $
        then if keyword_set(MEDIAN) $
        then res[*,i]= median(a[*,r[r[i]:r[i+1]-1]],dim=2) $
        else res[*,i]= total(a[*,r[r[i]:r[i+1]-1]],2)/h[i] $
        else if r[i] eq r[i+1]-1 then $
           res[*,i]= a[*,r[r[i]]]
     prot_size = (n_elements(orig_size) eq 2 ? n_elements(h) : [orig_size[0:n_elements(orig_size)-3],n_elements(h)])
     sa_rereform,res,prot_size
  endif else begin
     res = make_array(size(a,/dim),type=size(a,/type),/NOZERO)
     for i=0L,n_elements(h)-1 do $
        if r[i] ne r[i+1] then $
           for j=0,siz[0]-1 do $
              if keyword_set(MEDIAN) $
              then res[j, r[r[i]:r[i+1]-1]] = median(a[j, r[r[i]:r[i+1]-1]]) $
              else res[j, r[r[i]:r[i+1]-1]] = mean(a[j, r[r[i]:r[i+1]-1]])
     sa_rereform,res,orig_size
  endelse
  sa_rereform,a,orig_size
  print,'segmentization finished in '+strcompress(systime(1)-t,/R)+' seconds.'
  return, res
end

function polin_scc_wishart, T, initialization, iteration_Nr, MIN_POINTS=min_points, STATS=STATS,ERROR=ERROR
  common rat

  clusters  = initialization
  orig_size = size(T,/dim)
  Ttype     = size(T,/type)
  p   = orig_size[0]
  n   = (n_elements(orig_size)lt 3?1:product(orig_size[2:n_elements(orig_size)-1]))
  T   = reform(T,[p,p,n],/overwrite)
  if n_elements(min_points)   eq 0 then min_points = 1
  if n_elements(iteration_Nr) eq 0 then itereration_Nr = 1
  tmp_arr = make_array(n,type=Ttype,value=1.)
  if keyword_set(STATS) then h_stats = lonarr(200,iteration_nr+1)

  for it=0,iteration_Nr-1 do begin
     progress,percent=(it+1)*100.0/iteration_nr,/check_cancel,subpercent=0,submessage='Init'
     if wid.cancel eq 1 then begin $
        error = 1
        return,0
     endif

;;; remove zero- (or just very small) classes
     clusters = scc_rmtiny(clusters,min_points)

     hcl = histogram(clusters,r=r,min=0)
     Ncl = n_elements(hcl)

;;; some statistics about the development
     if keyword_set(STATS) then begin
        h_stats[0:(n_elements(hcl)-1) ,it] = hcl
        print,'it '+strcompress(it,/R)+': ',strcompress(hcl)
     endif

;;; generate cluster prototypes
     V   = make_array(p,p,Ncl,/NOZERO,type=Ttype)
     for cl=0L,Ncl-1 do begin
        progress,submessage='Generate cluster prototypes...',subpercent=(cl+1)*100/ncl,/check_cancel
        if wid.cancel eq 1 then begin $
           error = 1
           return,0
        endif
        if hcl[cl] eq 0 then $
           v[*,*,cl] = 0 $
        else v[*,*,cl] = total(T[*,*,r[r[cl]:r[cl+1]-1]],3)/hcl[cl]
     endfor
     Vinv = reform(block_inv(V))
     lnVd = reform(alog(block_det(V)))

;;; calculate the smallest Wishart distances
     d = lnVd ## tmp_arr
     for cl=0L,Ncl-1 do begin
        progress,submessage='Calculate smallest distances...',subpercent=(cl+1)*100/ncl,/check_cancel
        if wid.cancel eq 1 then begin $
           error = 1
           return,0
        endif
        d[*,cl] += reform(block_trace(real_part(block_mm(Vinv[*,*,cl],T))))
     endfor
     for i=0L,n-1 do $
        clusters[i] = argmin(d[i,*])
  endfor

;;; some statistics about the development
  if keyword_set(STATS) then begin
     h_stats[0:(n_elements(hcl)-1),it] = hcl
     print,'it '+strcompress(it,/R)+': ',strcompress(hcl)
     tek_color
     polin_classif_wins,/s
     max_cl = min(where(total(h_stats,2) eq 0))
     plot,indgen(iteration_nr),h_stats[0,*],color=2,yrange=[0,max(h_stats)]
     for i=1,max_cl-1 do $
        oplot,indgen(iteration_nr),h_stats[i,*],color=i+2
  endif

  T = reform(T,orig_size,/overwrite)
  return, clusters
end

function scc_rmtiny, clusters_old, min_points
;;; remove zero- (or just very small, if min_points given) clusters
;;; if min_points gt 1 then the points in too small clusters will be assigned
;;; to another random class!
;;; clusters start at index 0 !!!

  clusters = clusters_old
  if n_elements(min_points) eq 0 then min_points=1
  hcl = histogram(clusters,r=r,min=0)
  Ncl = n_elements(hcl)         ; clusters

  badCl = where(hcl lt min_points,badClNr)
  goodCl = where(hcl ge min_points,goodclNr)
  goodCl = reverse(goodCl)
  for i=0L,badClNr-1 do begin
     maxGoodCL = argmax(histogram(clusters,min=0))
     if i lt goodClNr && badCl[i] lt goodCl[i] then $
        clusters[r[r[goodCl[i]]:r[goodCl[i]+1]-1]] = badCl[i] $
     else if hcl[badCl[i]] ne 0 then $
        clusters[r[r[badCl[i]]:r[badCl[i]+1]-1]] = maxGoodCl
  endfor
  return, clusters
end

function polin_scc_T_classify, T_orig, BASIC_CLASSES=basic_classes, INITIALIZATION=INITIALIZATION, PRESEGMENTATION=presegmentation, nr_init_clusters=nr_init_clusters,$
                         iteration_Nr=iteration_Nr, MIN_POINTS=min_points, STATS=STATS,ERROR=ERROR
;;; T,basic,init and preseg are not segmented prototypes!
;;; if presegmentation is given, then basic and init are expected to be
;;; homogeneous in segments of the presegementation.
;;; subclasses with markers <0 will become values "-1" in the final classification.
  T = T_orig
  siz = size(T)
  vdim= siz[1] & n=siz[3:siz[0]]
  if n_elements(nr_init_clusters) eq 0 then nr_init_clusters=12
  if n_elements(BASIC_CLASSES) ne 0 then bcl = scc_rmtiny(basic_classes) $
  else bcl=intarr(n)
  if n_elements(INITIALIZATION) ne 0 then init = scc_rmtiny(INITIALIZATION) $
  else begin
     ncl = total(nr_init_clusters)
;     init = sqrt(sqrt(abs(block_trace(T)))) ; tmp init over span
;     init = reform(init,1,product(n),/OVERWRITE)
;     init = reform(abs(T),vdim^2,product(n))
     init = scc_rinit(reform(sqrt(abs(T)),vdim^2,product(n)),ncl)
     means = CLUST_WTS(init, N_CLUSTERS = ncl)
     init = CLUSTER(init, means, N_CLUSTERS = ncl)
     init = reform(init,n,/OVERWRITE)
  endelse
  if n_elements(PRESEGMENTATION) ne 0 then begin
     seg  = scc_rmtiny(PRESEGMENTATION)
     T    = segmentize(T,seg,/PROTO)
     bcl  = segmentize(bcl,seg,/PROTO,/MED)
     init = segmentize(init,seg,/PROTO,/MED)
  endif
  for bcli=0,max(bcl) do begin
     curr_ind = where(bcl eq bcli)
     init[curr_ind] = polin_scc_wishart(T[*,*,curr_ind],init[curr_ind],iteration_Nr, MIN_POINTS=min_points, STATS=STATS,ERROR=ERROR)
     if keyword_set(ERROR) then return, 0
  endfor
  mask_out = where(bcl lt 0,mask_out_cnt)
  if mask_out_cnt gt 0 then init[mask_out] = -1
  if n_elements(PRESEGMENTATION) ne 0 then $
     init = proto2img(init,seg)
  return,init
end


pro polin_classif_wishart, CALLED = called,initfile=initfile,NO_GUI=NO_GUI
  common rat, types, file, wid, config
  common channel, channel_names, channel_selec, color_flag, palettes, pnames

  if ~(file.type ge 500L && file.type le 515L) then begin
     error = DIALOG_MESSAGE(["This is wrong data type. Needs PolInSAR ",$
                             "covariance or coherency matrix"], $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

; if necessary, then generate covariance or coherence matrix
  if file.type ge 500 && file.type le 509 then begin
     errormsg = DIALOG_MESSAGE(["The scattering vectors need to be transformed to covariance or coherence matrices!",$
                                "Proceed automatically with vector-->matrix transformation?"],$
                               /question,DIALOG_PARENT = wid.base, TITLE='Proceed with matrix generation?')
     if errormsg eq 'Yes' then polin_k2m,/CALLED,SMMX=1,SMMY=1 $
     else return
  endif


;;; PARAMETERS
  vdim                 = file.vdim
  file_initialization  = (n_elements(initfile)eq 0?'':initfile)
  file_subclasses      = ''
  file_presegmentation = ''
  nr_iter              = 4
  min_clusterpoints    = vdim^2 ; min points in a cluster


;;;;;;;;;; GRAPHICAL INTERFACE ;;;;;;;;;;;;;;;
  if ~keyword_set(called) && ~keyword_set(NO_GUI) then begin
;;;;;;independent of the called keyword;;;;;;;
     main = WIDGET_BASE(GROUP_LEADER=wid.base,/column, $
                        TITLE='PolInSAR Classification based on Wishart Distance of C/T', $
                        /modal,/tlb_kill_request_events,/tlb_frame_attr)
     
     sub_init  = WIDGET_BASE(main,/row)
     text_init = CW_FIELD(sub_init,VALUE=file_initialization,    /STRING,XSIZE=60,TITLE='Initialization (required):  ')
     brow_init = WIDGET_BUTTON(sub_init,VALUE='browse',YSIZE=35)

     sub_subcl = WIDGET_BASE(main,/row)
     text_subcl= CW_FIELD(sub_subcl,VALUE=file_subclasses,       /STRING,XSIZE=60,TITLE='Sub-classes (optional):     ')
     brow_subcl= WIDGET_BUTTON(sub_subcl,VALUE='browse',YSIZE=35)

     sub_preseg = WIDGET_BASE(main,/row)
     text_preseg= CW_FIELD(sub_preseg,VALUE=file_presegmentation,/STRING,XSIZE=60,TITLE='Presegmentation (optional): ')
     brow_preseg= WIDGET_BUTTON(sub_preseg,VALUE='browse',YSIZE=35)

     fld_iter = CW_FIELD(main,VALUE=nr_iter,/integer, $
                         TITLE='Number of iterations    : ',XSIZE=6)
     fld_minp = CW_FIELD(main,VALUE=min_clusterpoints,/integer, $
                         TITLE='Min points in a cluster : ',XSIZE=6)

     buttons   = WIDGET_BASE(main,column=4,/frame)
     but_ok    = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
     but_canc  = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
     but_info  = WIDGET_BUTTON(buttons,VALUE=' Info ',xsize=60)
     but_help  = WIDGET_BUTTON(buttons,VALUE=' Help ',xsize=60)
     WIDGET_CONTROL, main,/REALIZE,default_button=but_canc,tlb_get_size=toto
     pos = center_box(toto[0],drawysize=toto[1])
     widget_control, main, xoffset=pos[0], yoffset=pos[1]
     
     repeat begin               ; Event loop
        event = widget_event(main)
        if event.id eq but_info then begin ; Info Button clicked
           infotext = ['POLINSAR WISHART CLASSIFICATION',$
                       ' ',$
                       'RAT module written 5/2005 by Maxim Neumann']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Information')
        endif
        if event.id eq but_help then begin ; Help Button clicked
           helptext = ['UNDER CONSTRUCTION',$
                       ' ',$
                       'but already under preparation...']
           help = DIALOG_MESSAGE(helptext, DIALOG_PARENT = main, $
                                 TITLE='Help')
        endif
        if event.id eq brow_init then begin ; Browse Button clicked
           file_initialization = DIALOG_PICKFILE(TITLE='Open Initialization file',DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST,PATH=config.workdir, GET_PATH=path)
           if strlen(file_initialization) gt 0 then begin
              config.workdir = path
              rrat,file_initialization,f,head=h
              if ~array_equal(h[1:2],[file.xdim,file.ydim]) $
              then begin
                 info = DIALOG_MESSAGE(['X/Y-dimensions do not pass.','Please choose another file...'],DIALOG_PARENT=main)
                 file_initialization=''
              endif
              free_lun,f
           endif
           widget_control,text_init,set_value=file_initialization
        endif
        if event.id eq brow_subcl then begin ; Browse Button clicked
           file_subclasses = DIALOG_PICKFILE(TITLE='Open Initialization file',DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST,PATH=config.workdir, GET_PATH=path)
           if strlen(file_subclasses) gt 0 then begin
              config.workdir = path
              rrat,file_subclasses,f,head=h
              if ~array_equal(h[1:2],[file.xdim,file.ydim]) $
              then begin
                 info = DIALOG_MESSAGE(['X/Y-dimensions do not pass.','Please choose another file...'],DIALOG_PARENT=main)
                 file_subclasses=''
              endif
              free_lun,f
           endif
           widget_control,text_subcl,set_value=file_subclasses
        endif
        if event.id eq brow_preseg then begin ; Browse Button clicked
           file_presegmentation = DIALOG_PICKFILE(TITLE='Open Initialization file',DIALOG_PARENT=wid.base, FILTER = '*.rat', /MUST_EXIST,PATH=config.workdir, GET_PATH=path)
           if strlen(file_presegmentation) gt 0 then begin
              config.workdir = path
              rrat,file_presegmentation,f,head=h
              if ~array_equal(h[1:2],[file.xdim,file.ydim]) $
              then begin
                 info = DIALOG_MESSAGE(['X/Y-dimensions do not pass.','Please choose another file...'],DIALOG_PARENT=main)
                 file_presegmentation=''
              endif
              free_lun,f
           endif
           widget_control,text_preseg,set_value=file_presegmentation
        endif
        if event.id eq but_ok && strlen(file_initialization) eq 0 then begin
           infotext = ['You have to choose at least the initialization file!',$
                       ' ',$
                       'Random initialization is till now not implemented...',$
                       'Wishart classification is very sensitive to initialization!']
           info = DIALOG_MESSAGE(infotext, DIALOG_PARENT = main, $
                                 TITLE='Please choose an initialization file.')
           event.id = -1
        endif
     endrep until (event.id eq but_ok) or (event.id eq but_canc) or (tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST')
     widget_control,fld_iter,GET_VALUE=nr_iter ; read widget fields
     widget_control,fld_minp,GET_VALUE=min_clusterpoints
     widget_control,main,/destroy ; remove main widget
     if event.id ne but_ok then return ; OK button _not_ clicked
  endif
;;;;;;;;;; GRAPHICAL INTERFACE ;;;;;;;;;;;;;;;
  
  use_subcl  = strlen(file_subclasses)      gt 0
  use_preseg = strlen(file_presegmentation) gt 0

; change mousepointer
  WIDGET_CONTROL,/hourglass

; undo function
  undo_prepare,outputfile,finalfile,CALLED=CALLED


  newtype = 450L
  newvar  = 2L
; read / write header
  rrat,file.name,ddd,header=head,info=info,type=type
  head = [2L,file.xdim,file.ydim,newvar]
  srat,outputfile,eee,header=head,info=info,type=newtype

; calculating preview size and number of blocks
  bs = config.blocksize
  calc_blocks_normal,file.ydim,bs,anz_blocks,bs_last
  blocksizes = intarr(anz_blocks)+bs
  blocksizes[anz_blocks-1] = bs_last

;  progress,/destroy
; pop up progress window
  progress,Message='PolInSAR Wishart Classification...',/cancel_button,Submessage='...'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; no loop...
  free_lun,ddd
  rrat,file.name,block
  rrat,file_initialization,init
  if use_subcl then $
     rrat,file_subclasses,subcl
  if use_preseg then $
     rrat,file_presegmentation,preseg
  oblock=polin_scc_T_classify(block,basic=subcl,init=init,preseg=preseg,iter=nr_iter,min_points=min_clusterpoints,ERROR=ERROR,/stat)
  if keyword_set(ERROR) then return
  writeu,eee,fix(oblock)
  free_lun,eee

;;; no loop...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; start block processing
;   for i=0,anz_blocks-1 do begin ; loop normal blocks
;      progress,percent=(i+1)*100.0/anz_blocks,/check_cancel
;      if wid.cancel eq 1 then return
;       block = make_array([file.vdim,file.zdim,file.xdim,blocksizes[i]] $
;                          ,type=file.var)
;       readu,ddd,block
;       init  = make_array([file.xdim,blocksizes[i]],type=hinit[hinit[0]+1])
; ; -------- THE FILTER ----------
;      oblock=polin_scc_T_classify(block,BASIC_CLASSES=, INITIALIZATION=INITIALIZATION, PRESEGMENTATION=presegmentation, nr_init_clusters=nr_init_clusters,$
;                          iteration_Nr=iteration_Nr, MIN_POINTS=min_points, STATS=STATS)
; ; -------- THE FILTER ----------
;      writeu,eee,FIX(oblock)
;   endfor
;   free_lun,ddd,eee


; update file information
  file_move,outputfile,finalfile,/overwrite
  file.name = finalfile
  file.dim  = 2L
  file.vdim = 1L
  file.zdim = 1L
  file.var  = newvar
  file.type = newtype

; set palette
  palettes[0,*,*] = palettes[5,*,*] ; palettes 5 = 10 classes
  palettes[1,*,*] = palettes[5,*,*] ; to actual and suggestion
  
; generate preview
  if ~keyword_set(called) && ~keyword_set(NO_DISPLAY) then begin
     generate_preview
     update_info_box
  endif else progress,/destroy

end
