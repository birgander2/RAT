;************************************************
;
; This functions reads common file descriptor
; from ERSDAC CEOS format
;
FUNCTION ersdac_chk_ceos, inFile, Info=info, DEBUG=debug
COMPILE_OPT idl2, hidden

On_error, 2
On_IoError, bad

if n_params() LT 1 then begin
	void=dialog_Message('Invalid input parameter set')
	Return, -1
endif

openR, lun, inFile, /GET_LUN, /SWAP_IF_LITTLE

;Set Common file descriptor

info = {	                        $
	rec_num:0L,						$ 1
	fst_rec_subtype:0B,				$ 2
	rec_type_code:0B,				$ 3
	sec_rec_subtype:0B,				$ 4
	trd_rec_subtype:0B,				$ 5
	rec_length:0L,					$ 6
	ascii_code_char:Bytarr(2),	 	$ 7
	blank1:Bytarr(2),				$ 8
	file_doc_num:Bytarr(12), 		$ 9
	rev_num_man_vol:Bytarr(2),		$ 10
	rev_num_log_vol:Bytarr(2),		$ 11
	log_vol_sys_num:Bytarr(12),		$ 12
	file_num:Bytarr(4),				$ 13
	file_id:Bytarr(16),				$ 14
	rec_comp_flg:Bytarr(4),			$ 15
	rec_num_position:Bytarr(8),		$ 16
	field_leng_rec_data:Bytarr(4),	$ 17
	spec_flg_rec_code:Bytarr(4),	$ 18
	byte_pos_rec_code:Bytarr(8),	$ 19
	filed_leng_rec_code:Bytarr(4),	$ 20
	spec_flg_rec_leng:Bytarr(4),	$ 21
	byte_pos_rec_leng:Bytarr(8), 	$ 22
	num_byte_rec_leng:Bytarr(4),	$ 23
	flg_data_conv_file:Bytarr(1),	$ 24
	flg_data_conf_rec:Bytarr(1),	$ 25
	flg_data_disp_file:Bytarr(1),	$ 26
	flg_data_disp_rec:Bytarr(1),	$ 27
	blank2:Bytarr(64)				$ 28
    }

readU, lun, info

;Check first parameter
if (info.rec_num NE 1) then begin
	void=dialog_message('This file might not be CEOS format.')
	return, -1
endif

;Check Sensor parameter
flg_alos = strmid(string(info.file_id), 0, 4)

if keyword_set(debug) then print, flg_alos
flg_alos = strcompress(flg_alos, /REMOVE_ALL)

if flg_alos NE 'ALOS' then begin
	void=dialog_message('This datasets might not be ERSDAC ALOS format.')
	return, -1
endif

return, 1 ; Success
free_lun, lun

bad:
return, -1 ;Insuccess
free_lun, lun

END
