;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: polin_get_info
; written by       : Maxim Neumann
; last revision    : 02/2007
; Diverse information about MB dataset:
;   - number of polarizations
;   - nr of tracks
;   - nr of baselines
;   - is matrix?
;   - use_kz		: kz_file available?
;   - kz_file		: use_kz? kz_file: ''
; for use in other polin-routines
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


pro polin_get_info, POL_NR=pol, TRACKS_NR=n_tr, BASELINES_NR=n_bl, MATRIX=MATRIX, $
                    USE_KZ=use_kz, KZ_FILE=kz_file, USE_BL=use_bl, BL_FILE=bl_file, $
                    BASIS_POLARIZATION=BASIS_POL
  common rat, types, file, wid, config

;;; polarimetry only!
  if total(file.type eq [200,209,210,220,221,222]) eq 1 then begin
     pol    = file.zdim
     n_tr   = 1
     n_bl   = 0
     matrix = file.type ge 220
     use_kz = 0 & use_bl = 0
     return
  endif


  if ~(file.type ge 500 && file.type le 519) then begin
     error = DIALOG_MESSAGE("This is wrong data type.", $
                            DIALOG_PARENT = wid.base, TITLE='Error',/error)
     return
  endif

;;; nr of polarizations
  if get_par('polarizations',pol) ne 0 then begin
     pol  = file.vdim mod 3 eq 0? 3L: 4L
     err = set_par('polarizations',pol)
  endif

;;; nr of tracks
  if get_par('nr_tracks',n_tr)    ne 0 then begin
     n_tr = file.type le 509? file.zdim: file.zdim/pol
     err  = set_par('nr_tracks',n_tr)
endif

;;; nr of baselines
  n_bl = mb_nr_baselines(n_tr)

;;; is matrix?
  matrix = file.type ge 510 && file.type le 519

;;; kz_file
  if arg_present(use_kz) || arg_present(kz_file) then begin
     tmp = get_par('kz_file',kz_file)
     if tmp eq 0 && file_test(kz_file,/READ) then $
        use_kz = 1 $
     else begin
        use_kz = 0
        kz_file= ''
     endelse
  endif

;;; bl_file (file with baselines)
  if arg_present(use_bl) || arg_present(bl_file) then begin
     tmp = get_par('bl_file',bl_file)
     if tmp eq 0 && file_test(bl_file,/READ) then $
        use_bl = 1 $
     else begin
        use_bl = 0
        bl_file= ''
     endelse
  endif

;;; polarization basis
  if arg_present(basis_pol) then begin
     tmp1 = get_par('polbasis_ellipticity',pol_ellipt)
     tmp2 = get_par('polbasis_orientation',pol_orient)
     if tmp1 eq 0 && tmp2 eq 0 then basis_pol = [pol_ellipt, pol_orient] $
     else basis_pol = -1
  endif

end
