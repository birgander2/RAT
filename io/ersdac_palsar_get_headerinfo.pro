FUNCTION ersdac_palsar_get_headerinfo,  $
			inFile, 				    $
			Header1=header_info1,       $
			DEBUG=debug

on_error, 2 ;Report Errors to Caller
on_ioerror, bad

if (N_params() eq 0) then begin
    inFile = Dialog_Pickfile()
    if inFile eq '' then begin
        void=DIALOG_MESSAGE('no input file is selected')
        return, -1
    endif
endif

    ;Define Header Info
    header_info1 = {      			$
       	rec_seq_num:0L,      		$;1
       	fst_subtype:0B,    			$;2
       	rec_type:0B,          		$;3
       	sec_subtype:0B,      		$;4
       	trd_subtype:0B,      		$;5
       	rec_length:0L,          	$;6
       	ascii_flg:Bytarr(2),		$;7
       	no_use1:Bytarr(2),  		$;8
       	form_doc:Bytarr(12),   		$;9
       	form_doc_ver:Bytarr(2),		$;10
       	file_deg_rev:Bytarr(2),		$;11
       	soft_id:Bytarr(12),  		$;12
       	file_num:Bytarr(4), 		$;13
       	file_name:Bytarr(16), 		$;14
       	rec_seq:Bytarr(4),			$;15
       	seq_num_loc:Bytarr(8),		$;16
       	seq_num_len:Bytarr(4),		$;17
       	rec_code_loc_flg:Bytarr(4),	$;18
       	rec_code_loc:Bytarr(8),   	$;19
       	rec_code_len:Bytarr(4),  	$;20
       	rec_len_flg:Bytarr(4),     	$;21
       	rec_len_loc:Bytarr(8),   	$;22
       	rec_len:Bytarr(4),          $;23
       	blank1:0B,   				$;24
       	blank2:0B,					$;25
       	blank3:0B,					$;26
       	blank4:0B,					$;27
       	Reserv_Seg:Bytarr(64),		$;28
       	sar_data_rec_num:Bytarr(6),	$;29
       	sar_data_rec_len:Bytarr(6),	$;30
       	blank5:Bytarr(24),     		$;31
       	bit_num:Bytarr(4),          $;32
       	sample_num:Bytarr(4),     	$;33
       	byt_num:Bytarr(4),          $;34
       	sample_just:Bytarr(4),   	$;35
       	num_channel:Bytarr(4),		$;36
       	num_line:Bytarr(8),   		$;37
        left_border:Bytarr(4),    	$;38
        num_group:Bytarr(8), 		$;39
        right_border:Bytarr(4),   	$;40
        top_border:Bytarr(4),     	$;41
        bottom_border:Bytarr(4), 	$;42
        interleave:Bytarr(4),    	$;43
        rec_per_line:Bytarr(2),    	$;44
        rec_per_channel:Bytarr(2),  $;45
        prefix:Bytarr(4),           $;46
        byte_count:Bytarr(8),    	$;47
        suffix:Bytarr(4),         	$;48
       	no_use2:Bytarr(4),       	$;49
       	line_num_loc:Bytarr(8),   	$;50
      	chan_num_loc:Bytarr(8),   	$;51
       	time_loc:Bytarr(8),      	$;52
       	left_fill_loc:Bytarr(8),	$;53
       	right_fill_loc:Bytarr(8),	$;54
       	pad_pix:Bytarr(4),			$;55
       	no_use3:Bytarr(28),   		$;56
       	quality_code_loc:Bytarr(8),	$;57
       	calib_info:Bytarr(8),		$;58
       	gain_value:Bytarr(8),		$;59
       	bias_value:Bytarr(8),     	$;60
       	data_type_id:Bytarr(28),   	$;61
       	data_type_code:Bytarr(4),	$;62
       	num_left_fill:Bytarr(4),   	$;63
       	num_right_fill:Bytarr(4),	$;64
       	pixel_range:Bytarr(8), 		$;65
       	no_use4:Bytarr(15804)   } 	;66

OpenR, lun, inFile, /Get_lun, Error=err, /SWAP_IF_LITTLE

ReadU, lun, header_info1
free_lun, lun

return, 1

bad:
free_lun, lun
return, -1

END

