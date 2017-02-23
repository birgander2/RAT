function ersdac_palsar_create_polar_stokes,inputfile,CHANNELS=channels,POLAR=ofile_polar,STOKES=ofile_stokes

 common rat, types, file, wid, config 
 common channel, channel_names, channel_selec, color_flag, palettes, pnames 

    main = WIDGET_BASE(GROUP_LEADER=wid.base,row=4,TITLE='Polar and Stokes',/floating,/tlb_kill_request_events,/tlb_frame_attr )
    butt = cw_bgroup(main,['Default','Create Polar and Stokes files '],set_value=0,row=2,/exclusive)    
    buttons  = WIDGET_BASE(main,column=3,/frame)
    but_ok   = WIDGET_BUTTON(buttons,VALUE=' OK ',xsize=80,/frame)
    but_canc = WIDGET_BUTTON(buttons,VALUE=' Cancel ',xsize=60)
    WIDGET_CONTROL, main, /REALIZE, default_button = but_canc, tlb_get_size=toto;tlb size
    pos = center_box(toto[0],drawysize=toto[1])
    widget_control, main, xoffset=pos[0], yoffset=pos[1]
    repeat begin
        event = widget_event(main)
    endrep until (event.id eq but_ok) or (event.id eq but_canc) or tag_names(event,/structure_name) eq 'WIDGET_KILL_REQUEST'

    widget_control,butt,GET_VALUE=channels
    widget_control,main,/destroy
  
    if event.id eq but_canc then return,-1                   ; OK button _not_ clicked
  
    if (channels eq 1) then begin 
  
    ;/* Set File name & Directory
        base_fname = StrSplit(File_BaseName(inputfile), '.', /Extract)
        base_fname = base_fname[0] ;remove extension
        dir = File_DirName(inputfile)

  ;/* Check if directory is not writable
        if (File_Test(Dir, /Write) EQ 0) then begin
            dir = Dialog_Pickfile(/Write,   $
            /Dir,         $
            Title='Select the directory which is writable!')
            if dir EQ '' then Return,-1
        endif

  ;/* Set Output Name and Open
        ofile_polar = dir + Path_Sep() + base_fname+'_polar.rat'
        ofile_stokes = dir + Path_Sep() +  base_fname+'_stokes.rat'

        res = File_Search(ofile_polar, Count=cnt)
        if (cnt GT 0) then begin
            mes = ['The same file already exists!', base_fname+'_polar.rat','overwrite the file?']
            res = Dialog_Message(mes, /Question)
        if res EQ 'Yes' then File_Delete, ofile_polar, /Quiet $
        else ofile_polar = Dialog_pickfile(Path=Dir, /Write, Filter='*.rat')
        endif

    res = File_Search(ofile_stokes, Count=cnt)
        if (cnt GT 0) then begin
            mes = ['The same file already exists!', base_fname+'_polar.rat', 'overwrite the file?']
            res = Dialog_Message(mes, /Question)
            if res EQ 'Yes' then File_Delete, ofile_stokes, /Quiet $
            else ofile_stokes = Dialog_pickfile(Path=Dir, /Write, Filter='*.rat')
        endif  
    endif 
    return,1
    
end