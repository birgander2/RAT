;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: spectral_weight
; last revision : 06/05/2004
; written by    : Stéphane Guillaso
; Apply or Remove a weighting function             
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

pro display_plot,fc,dim,val_min,val_max,color=color
   
   tek_color
   case color of 
      "black"   : cl = 0
      "white"   : cl = 1
      "red"     : cl = 2
      "green"   : cl = 3
      "blue"    : cl = 4
      "cyan"    : cl = 5
      "magenta" : cl = 6
      "orange"  : cl = 8
      "yellow"  : cl = 16
   endcase
   
   xx = findgen(dim)
   plots,xx[val_min:val_max],fc[val_min:val_max],color=cl,thick=5
end

pro plot_functions,fc,dim,other=other
   if keyword_set(other) then begin
                                ; max min
      tek_color
      plot,fc,yrange=[min(fc)<min(other),max(fc)>max(other)],color=1
      oplot,other,color=2
      
   endif else plot,fc,yrange=[min(fc),max(fc)]
end

pro plot_hanning,alpha
		plot,hanning(100,alpha=alpha),ticklen=0,xcharsize=0,charthick=0
end

function calcul_fl_cv,fl,dim,fl_mean
   fl_mean  = SMOOTH(fl,7,/edge_truncate)
   fl_mean2 = SMOOTH(fl^2.,7,/edge_truncate)
   fl_cv    = SQRT(fl_mean2 - (fl_mean^2.))/(fl_mean+1e-6)
   rmnanq,fl_cv
   return,fl_cv
end

PRO spectral_weight, CALLED=called, WEIGHT=weight, REMOVE=remove
   COMMON rat, types, file, wid, config
   
;------------------------------------------------------------------------
; Error handling:
;   -> check if file is a complex SAR image
;------------------------------------------------------------------------
   if file.type ne 101 then begin
      error = dialog_message("SAR complex image is required",dialog_parent=wid.base,title='Error',/error)
      return
   endif
   
; undo function
   undo_prepare,outputfile,finalfile,CALLED=CALLED
   
;------------------------------------------------------------------------
; - Transform the arrow mouse pointer into a hourglass
;------------------------------------------------------------------------
   widget_control, /hourglass
   
;------------------------------------------------------------------------
; - Calculate the spectrum in X and Y direction
;------------------------------------------------------------------------
   spectral_watch, /called, spectrum_X=fl_X, spectrum_Y=fl_Y, data=arr

;------------------------------------------------------------------------
; - Calculate the variation coefficient of both spectra
;------------------------------------------------------------------------
   fl_cv_X = calcul_fl_cv(fl_X,file.xdim,fl_mean_X)
   fl_cv_Y = calcul_fl_cv(fl_Y,file.ydim,fl_mean_Y)

;------------------------------------------------------------------------
; - Generate the graphical user interface
;------------------------------------------------------------------------
                                ; define the main widget
   main = widget_base(group_leader=wid.base, /column, title='Apply/Remove Weighting Function',/floating,/tlb_kill_request_events,/tlb_frame_attr)

                                ;--> Choose the direction to apply the procedure
   directionBase = widget_base(main, /row, /frame)
   directionButton = cw_bgroup(directionBase,['X','Y','X & Y'],label_left='Choose a direction: ', /row, /exclusive, set_value=0)
   
                                ;--> Define the main tab
   tabBase = widget_base(main, /row)
   wTab = widget_tab(tabBase)
   
                                ;--> Define the X tab
   XTabBase = widget_base(wTab, /column, title='X direction',map=1)
   
                                ; choose a function
   XChooseFunctionBase = widget_base(XTabBase,/row)
   label = widget_label(XChooseFunctionBase,value='Choose a function: ')
   XChooseFunctionButton = cw_bgroup(XChooseFunctionBase, ['Apply','Remove'],/row, /exclusive,/frame, set_value=0)
   label = widget_label(XChooseFunctionBase,value='a weighting function')
   
   XMainBase = widget_base(XTabBase,/row,/frame)
   
                                ;define the draw widget in the left part
   XleftMainBase = widget_base(XMainBase,/column)
   XleftMainDraw = widget_draw(XleftMainBase, xsize=500, ysize=300)
   Xsub = widget_base(xleftmainbase, /row)
;					label = widget_label(XleftMainBase, value='Select the area where you want to apply a weighting function:',/align_left)
   label = widget_label(Xsub, value='Select the area where you want to apply a weighting function: ',/align_left)
   XInOutChooseButton = cw_bgroup(Xsub, ['In','Out'],/row, /exclusive, set_value=1)
   label = widget_label(Xsub, value='side')
   XleftMainSlider1 = widget_slider(XleftMainBase,title='Select left position:',minimum=0,maximum=file.xdim-2)
   XleftMainSlider2 = widget_slider(XleftMainBase,title='Select right position:',minimum=1,maximum=file.xdim-1,value=file.xdim-1)
   
                                ;define some option in the right part
   XRightMainBase = widget_base(XMainBase,/column)
   
                                ; Define the upper part with value of the slider
   XRightMainUpBase = widget_base(XRightMainBase,/column)
   label = widget_label(XRightMainUpBase,value='Select slider position (press enter)',/align_left)
   XRightMainUpLeftField  = cw_field(XRightMainUpBase, title='Left position:  ', value=0, xsize=5, /integer, /return_event)
   XRightMainUpRightField = cw_field(XRightMainUpBase, title='Right position: ', value=file.xdim-1, xsize=5, /integer, /return_event)
   XRightMainUpVarDispButton = cw_bgroup(XRightMainUpBase,['Display'], label_left='Use Local Spectrum Variance:',/nonexclusive)
   
                                ; Define the downer part for choosing the weighting function
   XRightMainDownBase = widget_base(XRightMainBase,/column,/frame)
   label = widget_label(XRightMainDownBase, value='Choose the weighting function',/align_left)
   XRightMainDownChooseFunctionButton = cw_bgroup(XRightMainDownBase, ['Hamming','Hanning','Other'],/row,/exclusive,set_value=0)
   XRightMainDownChooseFunctionSlider = cw_fslider(XRightMainDownBase,title='Form Value (validate manual edit -> enter)',minimum=0.5,maximum=1.,xsize=253,/edit,value=0.54)
   XRightMainDownChooseFunctionLabel = widget_label(XRightMainDownBase, value='Weighting function: alpha=0.54',/align_left)
   XRightMainDownChooseFunctionDraw = widget_draw(XRightMainDownBase, xsize = 258, ysize=130)
   
                                ;--> Define the Y tab
   YTabBase = widget_base(wTab, /column, title='Y direction')
   
                                ; choose a function
   YChooseFunctionBase = widget_base(YTabBase,/row,map=0)
   label = widget_label(YChooseFunctionBase,value='Choose a function: ')
   YChooseFunctionButton = cw_bgroup(YChooseFunctionBase, ['Apply','Remove'],/row, /exclusive,/frame, set_value=0)
   label = widget_label(YChooseFunctionBase,value='a weighting function')
   
   YMainBase = widget_base(YTabBase,/row,/frame,map=0)
   
                                ;define the draw widget in the left part
   YleftMainBase = widget_base(YMainBase,/column)
   YleftMainDraw = widget_draw(YleftMainBase, xsize=500, ysize=300)
   Ysub = widget_base(Yleftmainbase, /row)
   label = widget_label(Ysub, value='Select the area where you want to apply a weighting function: ',/align_left)
   YInOutChooseButton = cw_bgroup(Ysub, ['In','Out'],/row, /exclusive, set_value=1)
;					label = widget_label(YleftMainBase, value='Select the area where you want to apply a weighting function:',/align_left)
   YLeftMainSlider1 = widget_slider(YleftMainBase,title='Select left position:',minimum=0,maximum=file.ydim-2)
   YLeftMainSlider2 = widget_slider(YleftMainBase,title='Select right position:',minimum=1,maximum=file.ydim-1,value=file.ydim-1)
   
                                ;define some option in the right part
   YRightMainBase = widget_base(YMainBase,/column)
   
                                ; Define the upper part with value of the slider
   YRightMainUpBase = widget_base(YRightMainBase,/column)
   label = widget_label(YRightMainUpBase,value='Select slider position (press enter)',/align_left)
   label = widget_label(Ysub, value='side')
   YRightMainUpLeftField  = cw_field(YRightMainUpBase, title='Left position:  ', value=0, xsize=5, /integer, /return_event)
   YRightMainUpRightField = cw_field(YRightMainUpBase, title='Right position: ', value=file.ydim-1, xsize=5, /integer, /return_event)
   YRightMainUpVarDispButton = cw_bgroup(YRightMainUpBase,['Display'], label_left='Use Local Spectrum Variance:',/nonexclusive)
   
                                ; Define the downer part for choosing the weighting function
   YRightMainDownBase = widget_base(YRightMainBase,/column,/frame)
   label = widget_label(YRightMainDownBase, value='Choose the weighting function',/align_left)
   YRightMainDownChooseFunctionButton = cw_bgroup(YRightMainDownBase, ['Hamming','Hanning','Other'],/row,/exclusive,set_value=0)
   YRightMainDownChooseFunctionSlider = cw_fslider(YRightMainDownBase,title='Form Value (validate manual edit -> enter)',minimum=0.5,maximum=1.,xsize=253,/edit,value=0.54)
   YRightMainDownChooseFunctionLabel = widget_label(YRightMainDownBase, value='Weighting function: alpha=0.54',/align_left)
   YRightMainDownChooseFunctionDraw = widget_draw(YRightMainDownBase, xsize = 258, ysize=130)
   
                                ;--> Generate the three bottom button OK - Cancel - Info
   buttons = widget_base(main, column=3, /frame)
   but_ok   = widget_button(buttons, value=' ok ',     xsize=80, /frame)
   but_canc = widget_button(buttons, value=' cancel ', xsize=60)
   but_info = widget_button(buttons, value=' info ',   xsize=60)
   

                                ; Realise the main widget, define the default button, and center it
   widget_control, main, /realize, tlb_get_size=toto
   pos = center_box(toto[0],drawysize=toto[1])
   widget_control, main, xoffset=pos[0], yoffset=pos[1]

   
;------------------------------------------------------------------------
; - Define flag to diplay
;------------------------------------------------------------------------
   flag_X = 1
   flag_Y = 0

;------------------------------------------------------------------------
; - Plot the fl_X
;------------------------------------------------------------------------
   widget_control,XleftMainDraw,get_value=index
   wset,index
   f_X = fl_X/max(fl_X)
   of_X=0	
   plot_functions,f_X,file.xdim
   
;------------------------------------------------------------------------
; - Plot the fl_Y
;------------------------------------------------------------------------
   widget_control,YleftMainDraw,get_value=index
   wset,index
   f_Y = fl_Y/max(fl_Y)
   of_Y=0	
   plot_functions,f_Y,file.ydim
   
;------------------------------------------------------------------------
; - Plot the hamming function
;------------------------------------------------------------------------
   widget_control,XRightMainDownChooseFunctionDraw,get_value=index
   wset,index
   plot_hanning,0.54

;------------------------------------------------------------------------
; - Plot the hamming function
;------------------------------------------------------------------------
   widget_control,YRightMainDownChooseFunctionDraw,get_value=index
   wset,index
   plot_hanning,0.54

;------------------------------------------------------------------------
; - Loop events to catch the information provided by widget
;------------------------------------------------------------------------
   repeat begin
      
                                ; Get an event from the main widget
      event = widget_event(main)
      
                                ; Select the direction
      if event.id eq directionButton then begin
         widget_control,directionButton,get_value=dummy
         case dummy of 
            0:begin
               widget_control,XChooseFunctionBase, map=1
               widget_control,XMainBase, map=1
               widget_control,YChooseFunctionBase, map=0
               widget_control,YMainBase, map=0
               flag_X = 1
               flag_Y = 0
            end
            1:begin
               widget_control,XChooseFunctionBase, map=0
               widget_control,XMainBase, map=0
               widget_control,YChooseFunctionBase, map=1
               widget_control,YMainBase, map=1
               flag_X = 0
               flag_Y = 1
            end
            2:begin
               widget_control,XChooseFunctionBase, map=1
               widget_control,XMainBase, map=1
               widget_control,YChooseFunctionBase, map=1
               widget_control,YMainBase, map=1
               flag_X = 1
               flag_Y = 1
            end
         endcase
      endif
      
                                ; case of remove is choosen
      if event.id eq XChooseFunctionButton then begin
         widget_control,XChooseFunctionButton,get_value=dummy
         widget_control,XRightMainDownBase,map=1-dummy
      endif
      if event.id eq YChooseFunctionButton then begin
         widget_control,YChooseFunctionButton,get_value=dummy
         widget_control,YRightMainDownBase,map=1-dummy
      endif
      
;------------------------------------------------------------------------
; STUDY OF THE X CASE
;------------------------------------------------------------------------
      
                                ; Study the In/Out side case
      
                                ; Study of the left X slider
      if event.id eq XleftMainSlider1 then begin
                                ;get all available information
         widget_control,XleftMainSlider1,get_value=slX1
         widget_control,XleftMainSlider2,get_value=slX2
         widget_control,XleftMainSlider1,set_value=slX1<(slX2-1)
         widget_control,XRightMainUpLeftField,set_value=slX1<(slX2-1)
      endif
      if event.id eq XRightMainUpLeftField then begin
                                ;get all available information
         widget_control,XRightMainUpLeftField,get_value=slX1
         widget_control,XRightMainUpRightField,get_value=slX2
         widget_control,XleftMainSlider1,set_value=slX1<(slX2-1)
         widget_control,XRightMainUpLeftField,set_value=slX1<(slX2-1)
      endif
      
                                ; Study of the right X slider
      if event.id eq XleftMainSlider2 then begin
                                ;get all available information
         widget_control,XleftMainSlider1,get_value=slX1
         widget_control,XleftMainSlider2,get_value=slX2
         widget_control,XleftMainSlider2,set_value=slX2 > (slX1+1)
         widget_control,XRightMainUpRightField,set_value=slX2 > (slX1+1)
      endif
      if event.id eq XRightMainUpRightField then begin
                                ;get all available information
         widget_control,XRightMainUpLeftField,get_value=slX1
         widget_control,XRightMainUpRightField,get_value=slX2
         widget_control,XleftMainSlider2,set_value=slX2 > (slX1+1)
         widget_control,XRightMainUpRightField,set_value=slX2 > (slX1+1)
      endif
      
                                ; change display
      if event.id eq XRightMainUpVarDispButton then begin
         widget_control,XRightMainUpVarDispButton,get_value=dummy
         case dummy[0] of
            0 : begin
               f_X = fl_X/max(fl_X)
               of_X = 0
            end
            1 : begin
               f_X = fl_cv_X/max(fl_cv_X)
               of_X = fl_X/max(fl_X)
            end
         endcase
      endif
      
                                ;plot function
      widget_control,XRightMainUpLeftField,get_value=slX1
      widget_control,XRightMainUpRightField,get_value=slX2
      widget_control,XleftMainDraw,get_value=index
      wset,index
      plot_functions,f_X,file.xdim,other=of_X
      widget_control,XInOutChooseButton,get_value=XInOut
      case XInOut of
         0: begin
            display_plot,f_X,file.xdim,slX1,slX2,color="orange"
         end
         1: begin
            display_plot,f_X,file.xdim,0,slX1,color="blue"
            display_plot,f_X,file.xdim,slX2,file.xdim-1,color="green"
         end
      endcase
      
                                ; hamming, hannig, other
      if event.id eq XRightMainDownChooseFunctionButton then begin
         widget_control,XRightMainDownChooseFunctionButton,get_value=dummy
         case dummy of 
            0 : begin
               widget_control,XRightMainDownChooseFunctionSlider,set_value=0.54
               widget_control,XRightMainDownChooseFunctionLabel,set_value='Weighting function: alpha=0.54'
               widget_control,XRightMainDownChooseFunctionDraw,get_value=index
               wset,index
               plot_hanning,0.54
            end
            1 : begin
               widget_control,XRightMainDownChooseFunctionSlider,set_value=0.5
               widget_control,XRightMainDownChooseFunctionLabel,set_value='Weighting function: alpha=0.50'
               widget_control,XRightMainDownChooseFunctionDraw,get_value=index
               wset,index
               plot_hanning,0.5
            end
            2 : begin
            end
         endcase
      endif
      
                                ; slider form
      if event.id eq XRightMainDownChooseFunctionSlider then begin
         widget_control,XRightMainDownChooseFunctionSlider, get_value=dummy
         widget_control,XRightMainDownChooseFunctionButton,set_value=2
         if dummy eq 0.54 then widget_control,XRightMainDownChooseFunctionButton,set_value=0
         if dummy eq 0.5 then widget_control,XRightMainDownChooseFunctionButton,set_value=1
         widget_control,XRightMainDownChooseFunctionLabel,set_value='Weighting function: alpha='+strmid(strcompress(dummy,/rem),0,4)
         widget_control,XRightMainDownChooseFunctionDraw,get_value=index
         wset,index
         plot_hanning,dummy
      endif

;------------------------------------------------------------------------
; STUDY OF THE Y CASE
;------------------------------------------------------------------------
                                ; Study of the left Y slider
      if event.id eq YLeftMainSlider1 then begin
                                ;get all available information
         widget_control,YLeftMainSlider1,get_value=slY1
         widget_control,YLeftMainSlider2,get_value=slY2
         widget_control,YLeftMainSlider1,set_value=slY1<(slY2-1)
         widget_control,YRightMainUpLeftField,set_value=slY1<(slY2-1)
      endif
      if event.id eq YRightMainUpLeftField then begin
                                ;get all available information
         widget_control,YRightMainUpLeftField,get_value=slY1
         widget_control,YRightMainUpRightField,get_value=slY2
         widget_control,YLeftMainSlider1,set_value=slY1<(slY2-1)
         widget_control,YRightMainUpLeftField,set_value=slY1<(slY2-1)
      endif
      
                                ; Study of the right Y slider
      if event.id eq YLeftMainSlider2 then begin
                                ;get all available information
         widget_control,YLeftMainSlider1,get_value=slY1
         widget_control,YLeftMainSlider2,get_value=slY2
         widget_control,YLeftMainSlider2,set_value=slY2 > (slY1+1)
         widget_control,YRightMainUpRightField,set_value=slY2 > (slY1+1)
      endif
      if event.id eq YRightMainUpRightField then begin
                                ;get all available information
         widget_control,YRightMainUpLeftField,get_value=slY1
         widget_control,YRightMainUpRightField,get_value=slY2
         widget_control,YLeftMainSlider2,set_value=slY2 > (slY1+1)
         widget_control,YRightMainUpRightField,set_value=slY2 > (slY1+1)
      endif
      
                                ; change display
      if event.id eq YRightMainUpVarDispButton then begin
         widget_control,YRightMainUpVarDispButton,get_value=dummy
         case dummy[0] of
            0 : begin
               f_Y = fl_Y/max(fl_Y)
               of_Y = 0
            end
            1 : begin
               f_Y = fl_cv_Y/max(fl_cv_Y)
               of_Y = fl_Y/max(fl_Y)
            end
         endcase
      endif
      
                                ; plot
      widget_control,YRightMainUpLeftField,get_value=slY1
      widget_control,YRightMainUpRightField,get_value=slY2
      widget_control,YleftMainDraw,get_value=index
      wset,index
      plot_functions,f_Y,file.ydim,other=of_Y
      widget_control,YInOutChooseButton,get_value=YInOut
      case YInOut of
         0: begin
            display_plot,f_Y,file.ydim,slY1,slY2,color="orange"
         end
         1: begin
            display_plot,f_Y,file.ydim,0,slY1,color="blue"
            display_plot,f_Y,file.ydim,slY2,file.ydim-1,color="green"
         end
      endcase
      
                                ; hamming, hannig, other
      if event.id eq YRightMainDownChooseFunctionButton then begin
         widget_control,YRightMainDownChooseFunctionButton,get_value=dummy
         case dummy of 
            0 : begin
               widget_control,YRightMainDownChooseFunctionSlider,set_value=0.54
               widget_control,YRightMainDownChooseFunctionLabel,set_value='Weighting function: alpha=0.54'
               widget_control,YRightMainDownChooseFunctionDraw,get_value=index
               wset,index
               plot_hanning,0.54
            end
            1 : begin
               widget_control,YRightMainDownChooseFunctionSlider,set_value=0.5
               widget_control,YRightMainDownChooseFunctionLabel,set_value='Weighting function: alpha=0.50'
               widget_control,YRightMainDownChooseFunctionDraw,get_value=index
               wset,index
               plot_hanning,0.5
            end
            2 : begin
            end
         endcase
      endif
      
                                ; slider form
      if event.id eq YRightMainDownChooseFunctionSlider then begin
         widget_control,YRightMainDownChooseFunctionSlider, get_value=dummy
         widget_control,YRightMainDownChooseFunctionButton,set_value=2
         if dummy eq 0.54 then widget_control,YRightMainDownChooseFunctionButton,set_value=0
         if dummy eq 0.5 then widget_control,YRightMainDownChooseFunctionButton,set_value=1
         widget_control,YRightMainDownChooseFunctionLabel,set_value='Weighting function: alpha='+strmid(strcompress(dummy,/rem),0,4)
         widget_control,YRightMainDownChooseFunctionDraw,get_value=index
         wset,index
         plot_hanning,dummy
      endif

                                ; If info button is choosen
      if event.id eq but_info then begin
         infotext = ['Apply/Remove Weighting Function',$
                     ' ',$
                     'RAT module written 05/2004 by Stephane Guillaso',$
                     ' ',$
                     'If the Local Spectrum Variance shows two peaks',$
                     'you should include one of each in the left/right selection',$
                     'using slider or manualy entry',$
                     'otherwise, do not use the Local Spectrum Variance',$
                     'results of the procedure may be failed',$
                     ' ']
         info = dialog_message(infotext, dialog_parent = main, title='Information')
      end
      
   endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'


;------------------------------------------------------------------------
; - Get different parameters
;------------------------------------------------------------------------
   widget_control,directionButton,get_value=direction

   widget_control,XRightMainUpLeftField,get_value=min_X
   widget_control,XRightMainUpRightField,get_value=max_X
   widget_control,XChooseFunctionButton,get_value=func_X
   if func_X eq 0 then widget_control,XRightMainDownChooseFunctionSlider,get_value=form_X
   widget_control,XRightMainUpVarDispButton,get_value=varUse_X
   widget_control,XInOutChooseButton,get_value=XInOut

   widget_control,YRightMainUpLeftField,get_value=min_Y
   widget_control,YRightMainUpRightField,get_value=max_Y
   widget_control,YChooseFunctionButton,get_value=func_Y
   if func_Y eq 0 then widget_control,YRightMainDownChooseFunctionSlider,get_value=form_Y
   widget_control,YRightMainUpVarDispButton,get_value=varUse_Y
   widget_control,YInOutChooseButton,get_value=YInOut
   
;------------------------------------------------------------------------
; - Destroy the widget display
;------------------------------------------------------------------------

   widget_control, main, /destroy
   widget_control,wid.draw,get_value=index
   wset,index
   
;------------------------------------------------------------------------
; - Test if we have choose the cancel button. If yes exit the procedure
;------------------------------------------------------------------------

   if event.id ne but_ok then return
   
;------------------------------------------------------------------------
; - Apply following the different option
;------------------------------------------------------------------------
   FlagCatchErr = [0,0]

                                ; apply to X direction
   if direction eq 0 or direction eq 2 then begin
      
      if XInOut eq 1 then begin
                                ; Calcul the shift index
         shift_ind = (max_X + min_X)/2
         
                                ; calculate limite if choosen
         if varUse_X eq 1 then begin
            fl_cv  = shift(fl_cv_X,-shift_ind)
            fl_cv1   = [fl_cv[0:file.xdim/2-1],FLTARR(file.xdim/2)]
            fl_cv2   = [FLTARR(file.xdim/2),fl_cv[file.xdim/2:*]]
            toto = MAX(fl_cv1,lim1)
            toto = MAX(fl_cv2,lim2)
            lim1 = lim1 + 6
            lim2 = lim2 - 6
         endif else begin
            lim1 = max_X - shift_ind + 5
            lim2 = file.xdim - (shift_ind - min_X) -6
         endelse
         
                                ; shift the mean
         fl_mean_X = shift(fl_mean_X,-shift_ind)
         
                                ; calcul filter
         dis_ham = lim2-lim1+1
         if dis_ham gt 10 then begin
            filtre = complexarr(file.xdim)
            if func_X eq 0 then $
              filtre[lim1:lim2] = hanning(dis_ham,alpha=form_X) else $
              filtre[lim1:lim2] = 1

                                ; process over the data
            progress,message='X direction...'
            for lig=0,file.ydim-1 do begin
               progress,percent=(lig+1)*100.0/(file.ydim)
               fl = shift(fft(arr[*,lig],-1),-shift_ind)
               fl[lim1:lim2] = fl[lim1:lim2] / fl_mean_X[lim1:lim2]
               fl = fl*filtre*max(smooth(fl_mean_X[lim1:lim2],31,/edge_truncate))
               arr[*,lig] = fft(shift(fl,shift_ind),1)
            endfor
            progress,/destroy
         endif else begin
            toto = ['You have to choose better value to delimit your spectrum','','Give up the X direction']
            dummy = dialog_message(toto, dialog_parent = wid.base, title='error')
            FlagCatchErr[0] = 1
         endelse
      endif else begin
         
                                ;calculate limite if choosen
         if varUse_X eq 1 then begin
            fl_cv  = fl_cv_X
            fl_cv1   = [fl_cv[0:file.xdim/2-1],FLTARR(file.xdim/2)]
            fl_cv2   = [FLTARR(file.xdim/2),fl_cv[file.xdim/2:*]]
            toto = MAX(fl_cv1,lim1)
            toto = MAX(fl_cv2,lim2)
            lim1 = lim1 + 6
            lim2 = lim2 - 6
         endif else begin
            lim1 = min_X + 6
            lim2 = max_X - 6
         endelse
         
                                ; calcul filter
         dis_ham = lim2-lim1+1
         if dis_ham gt 10 then begin
            filtre = complexarr(file.xdim)
            if func_X eq 0 then $
              filtre[lim1:lim2] = hanning(dis_ham,alpha=form_X) else $
              filtre[lim1:lim2] = 1
            
                                ; process over the data
            progress,message='X direction...'
            for lig=0,file.ydim-1 do begin
               progress,percent=(lig+1)*100.0/(file.ydim)
               fl = fft(arr[*,lig],-1)
               fl[lim1:lim2] = fl[lim1:lim2] / fl_mean_X[lim1:lim2]
               fl = fl*filtre*max(smooth(fl_mean_X[lim1:lim2],31,/edge_truncate))
               arr[*,lig] = fft(fl,1)
            endfor
            progress,/destroy
         endif else begin
            toto = ['You have to choose better value to delimit your spectrum','','Give up the X direction']
            dummy = dialog_message(toto, dialog_parent = wid.base, title='error')
            FlagCatchErr[0] = 1
         endelse
      endelse

   endif else FlagCatchErr[0] = 1


                                ; apply to Y direction
   if direction eq 1 or direction eq 2 then begin
      
      if YInOut eq 1 then begin
                                ; Calcul the shift index
         shift_ind = (max_Y + min_Y)/2
         
                                ; calculate limite if choosen
         if varUse_Y eq 1 then begin
            fl_cv  = shift(fl_cv_Y,-shift_ind)
            fl_cv1   = [fl_cv[0:file.ydim/2-1],FLTARR(file.ydim/2)]
            fl_cv2   = [FLTARR(file.ydim/2),fl_cv[file.ydim/2:*]]
            toto = MAX(fl_cv1,lim1)
            toto = MAX(fl_cv2,lim2)
            lim1 = lim1 + 6
            lim2 = lim2 - 6
         endif else begin
            lim1 = max_Y - shift_ind + 5
            lim2 = file.ydim - (shift_ind - min_Y) -6
         endelse
         
                                ; shift the mean
         fl_mean_Y = shift(fl_mean_Y,-shift_ind)
         
                                ; calcul filter
         dis_ham = lim2-lim1+1
         if dis_ham gt 10 then begin
            filtre = complexarr(file.ydim)
            if func_Y eq 0 then $
              filtre[lim1:lim2] = hanning(dis_ham,alpha=form_Y) else $
              filtre[lim1:lim2] = 1

                                ; process over the data
            progress,message='Y direction...'
            for col=0,file.xdim-1 do begin
               progress,percent=(col+1)*100.0/(file.xdim)
               fl = shift(fft(arr[col,*],-1),-shift_ind)
               fl[lim1:lim2] = fl[lim1:lim2] / fl_mean_Y[lim1:lim2]
               fl = fl*filtre*max(smooth(fl_mean_Y[lim1:lim2],31,/edge_truncate))
               arr[col,*] = fft(shift(fl,shift_ind),1)
            endfor
            progress,/destroy
         endif else begin
            toto = ['You have to choose better value to delimit your spectrum','','Give up the Y direction']
            dummy = dialog_message(toto, dialog_parent = wid.base, title='error')
            FlagCatchErr[1] = 1
         endelse
         
      endif else begin
         
                                ;calculate limite if choosen
         if varUse_Y eq 1 then begin
            fl_cv  = fl_cv_Y
            fl_cv1   = [fl_cv[0:file.ydim/2-1],FLTARR(file.ydim/2)]
            fl_cv2   = [FLTARR(file.ydim/2),fl_cv[file.ydim/2:*]]
            toto = MAX(fl_cv1,lim1)
            toto = MAX(fl_cv2,lim2)
            lim1 = lim1 + 6
            lim2 = lim2 - 6
         endif else begin
            lim1 = min_Y + 6
            lim2 = max_Y - 6
         endelse
         
                                ; calcul filter
         dis_ham = lim2-lim1+1
         if dis_ham gt 10 then begin
            filtre = complexarr(file.ydim)
            if func_Y eq 0 then $
              filtre[lim1:lim2] = hanning(dis_ham,alpha=form_Y) else $
              filtre[lim1:lim2] = 1
            
                                ; process over the data
            progress,message='Y direction...'
            for col=0,file.xdim-1 do begin
               progress,percent=(col+1)*100.0/(file.xdim)
               fl = fft(arr[col,*],-1)
               fl[lim1:lim2] = fl[lim1:lim2] / fl_mean_Y[lim1:lim2]
               fl = fl*filtre*max(smooth(fl_mean_Y[lim1:lim2],31,/edge_truncate))
               arr[col,*] = fft(fl,1)
            endfor
            progress,/destroy
         endif else begin
            toto = ['You have to choose better value to delimit your spectrum','','Give up the Y direction']
            dummy = dialog_message(toto, dialog_parent = wid.base, title='error')
            FlagCatchErr[1] = 1
         endelse
      endelse

   endif else FlagCatchErr[1] = 1

   
;------------------------------------------------------------------------
; - Transform the arrow mouse pointer into a hourglass
;------------------------------------------------------------------------
   widget_control, /hourglass

;------------------------------------------------------------------------
; - save data
;------------------------------------------------------------------------
   head = 1l
   rrat,file.name,ddd,header=head,info=info,type=type
   free_lun,ddd
   srat,outputfile,ddd,header=head,info=info,type=type
   writeu,ddd,arr
   free_lun,ddd
   
   file.name = finalfile
   file_move,outputfile,finalfile,/overwrite

; switch back to main draw widget

   if not keyword_set(called) then begin
      if total(FlagCatchErr) ne 2 then generate_preview
      update_info_box
   endif

end


