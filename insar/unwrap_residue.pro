;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: unwrap_residue
;
; written by   : Markus Steiof
; last revision: 21. Aug 2007
; reference    : Dennis C. Ghiglia, Mark D. Pritt, Two-Dimensional Phase Unwrapping, 1998
;				 chapter 4.2, page 103 f. (Goldstein's Branch Cut Algorithm)
;------------------------------------------------------------------------
; Identify Residues (part of Goldstein's Branch Cut Algorithm)
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
; The Initial Developer of the Original Code is Markus Steiof.
; All Rights Reserved.
;------------------------------------------------------------------------

;========================================================================
function residue, phase, switch_rm_dipoles, switch_unwrap_bc

	wphase = phase

	size_wphase = size( wphase )
	xsize_wphase = size_wphase[1]
	ysize_wphase = size_wphase[2]

	; Array fuer die Residuen
	residue = fltarr( xsize_wphase, ysize_wphase )

	; '2*pi' wird oft benoetigt
	twopi = 2 * !pi

;	print, "========================================="
;	print, " Identifizierung der Residuen"

;------------------------------------------------------------------------
; Residuen identifizieren
;------------------------------------------------------------------------

;	print, "-----------------------------------------"
;	print, "> Suche Residuen..."
;	t_start = systime( 1 )

	; Summe der 4 Differenzen nicht unbedingt genau '2*pi' oder '-2*pi', deshalb Toleranzbereich durch Schwelle
	threshold = 0.00001

	; zaehlt positive und negative Residuen
	number_plus_resi = 0UL
	number_minus_resi = 0UL

	; Array fuer die 4 Differenzen
	gradients = fltarr( 4 )

	; Bild durchsuchen, Schleife laesst letzte Zeile/Spalte aussen vor
	for j = 0, ysize_wphase - 2 do begin
		for i = 0, xsize_wphase - 2 do begin
			; 4 Phasendifferenzen aufsummieren
			gradients[0] = wphase[i,j + 1] - wphase[i,j]
			gradients[1] = wphase[i + 1,j + 1] - wphase[i,j + 1]
			gradients[2] = wphase[i + 1,j] - wphase[i + 1,j + 1]
			gradients[3] = wphase[i,j] - wphase[i + 1,j]

			; damit im Intervall ]-pi,+pi]
			for n = 0, 3 do begin
				if gradients[n] gt !pi then gradients[n] -= twopi
				if gradients[n] le -!pi then gradients[n] += twopi
			endfor

			; Summe der 4 Differenzen
			sum_gradients = total( gradients )

			; wenn Summe '2*pi' dann Pixel '[i,j]' als positives Residuum markieren
			if abs( sum_gradients - twopi ) le threshold then begin
				residue[i,j] = 1
				number_plus_resi++
			endif

			; wenn Summe '-2*pi' dann Pixel '[i,j]' als negatives Residuum markieren
			if abs( sum_gradients + twopi ) le threshold then begin
				residue[i,j] = -1
				number_minus_resi++
			endif
		endfor
	endfor

;	print, "  |positive Residuen:", number_plus_resi
;	print, "  |negative Residuen:", number_minus_resi
;	print, "  |Residuen gesamt  :", number_plus_resi + number_minus_resi

;------------------------------------------------------------------------
; Dipole entfernen
;------------------------------------------------------------------------

	; wenn der User die Entfernung der Dipole wuenscht
	if switch_rm_dipoles eq 1 then begin
;		print, "> Entferne Dipole..."

		; angrenzende Dipole entfernen

		; zaehlt angrenzende Dipole
		number_adjdipoles = 0UL

		; fuer jedes Residuum
		for j = 0, ysize_wphase - 2 do begin
			for i = 0, xsize_wphase - 2 do begin
				if residue[i,j] ne 0 then begin
					; wenn Pixel in positiver x-Richtung entgegengesetzte Polaritaet hat
					if ( residue[i,j] * residue[i + 1,j] lt 0 ) then begin
						; beide Residuen entfernen
						residue[i,j] = 0
						residue[i + 1,j] = 0
						number_adjdipoles++
					endif
					; wenn Pixel in positiver y-Richtung entgegengesetzte Polaritaet hat
					if ( residue[i,j] * residue[i,j + 1] lt 0 ) then begin
						; beide Residuen entfernen
						residue[i,j] = 0
						residue[i,j + 1] = 0
						number_adjdipoles++
					endif
				endif
			endfor
		endfor
;		print, "  |angrenzende Dipole entfernt:", number_adjdipoles

		; diagonale Dipole entfernen

		; zaehlt diagonale Dipole
		number_diagdipoles = 0UL

		; fuer jedes Residuum
		for j = 0, ysize_wphase - 3 do begin
			for i = 1, xsize_wphase - 2 do begin
				if residue[i,j] ne 0 then begin
					; wenn Pixel in positiver x-/y-Richtung entgegengesetzte Polaritaet hat
					if ( residue[i,j] * residue[i + 1,j + 1] lt 0 ) then begin
						; beide Residuen entfernen
						residue[i,j] = 0
						residue[i + 1,j + 1] = 0
						number_diagdipoles++
					endif $
					else $
						; wenn Pixel in negativer x- und positiver y-Richtung entgegengesetzte Polaritaet hat
						if ( residue[i,j] * residue[i - 1,j + 1] lt 0 ) then begin
							; beide Residuen entfernen
							residue[i,j] = 0
							residue[i - 1,j + 1] = 0
							number_diagdipoles++
						endif
				endif
			endfor
		endfor

		; Sonderfall 1. Spalte (diagonale Dipole)
		for j = 0, ysize_wphase - 3 do begin
			if residue[0,j] ne 0 then begin
				; wenn Pixel in positiver x-/y-Richtung entgegengesetzte Polaritaet hat
				if ( residue[0,j] * residue[1,j + 1] lt 0 ) then begin
					; beide Residuen entfernen
					residue[0,j] = 0
					residue[1,j + 1] = 0
					number_diagdipoles++
				endif
			endif
		endfor

;		print, "  |diagonale Dipole entfernt  :", number_diagdipoles
;		print, "  |restliche pos. Residuen    :", number_plus_resi - number_adjdipoles - number_diagdipoles
;		print, "  |restliche neg. Residuen    :", number_minus_resi - number_adjdipoles - number_diagdipoles
;		print, "  |restliche Residuen gesamt  :", ( number_plus_resi + number_minus_resi ) - 2 * number_adjdipoles - 2 * number_diagdipoles
	endif
;	print, "-----------------------------------------"
;	print, " Identifizierung der Residuen fertig!"
;	t_end = systime( 1 )
;	res_show_time, t_start, t_end, 1
;	print, "-----------------------------------------"

	; fuer die Grafikanzeige:
	; allen negativen Residuen einen neuen Wert zuweisen, damit sie von den positiven optisch zu unterscheiden sind
	residue[where( residue eq -1 )] = 0.4

	; Residuen liefern
	return, residue

end

;========================================================================
pro res_show_time, time_start, time_end, fin
;------------------------------------------------------------------------
; Kalkuliert die Zeitdifferenz zweier Zeitangaben und gibt sie auf der
; Standardausgabe formatiert aus.
;------------------------------------------------------------------------
; PRE:  'time_start', 'time_end' sind Zeiten in Sekunden. Wenn 'fin'
;       gleich '1' ist, wird zusaetzlicher Text ausgegeben.
; POST: Ausgegeben ist die formatierte Zeitdifferenz.
;------------------------------------------------------------------------

	time = time_end - time_start

	; evtl. zusaetzlicher Text bei Zeitausgabe
	if fin eq 1 then d = "Gesamtzeit: " else d = ""

	; < 60 Sekunden
	if time lt 60 then begin
		; < 1 Sekunde
		if time lt 1 then begin
			time = round( time * 100 )
			time = strtrim( ( time / 100.0 ), 1 )
			pos = strpos( time, "." )
			pos = pos + 3
		endif $
		; >= 1 Sekunde
		else begin
			time = round( time * 10 )
			time = strtrim( ( time / 10.0 ), 1 )
			pos = strpos( time, "." )
			pos = pos + 2
		endelse
		print, " ", d, strmid( time, 0, pos ), "s"
	endif $
	; >= 60 Sekunden
	else begin
		; < 1 Stunde
		if time lt 3600 then begin
			m = floor( time / 60 )
			s = floor( time mod 60 )
			m = strtrim( m, 1 )
			s = strtrim( s, 1 )
			print, " ", d, m, "min ", s, "s"
		endif $
		; >= 1 Stunde
		else begin
			h = floor( time / 3600 )
			m = floor( ( time - 3600*h ) / 60 )
			s = floor( time mod 60 )
			h = strtrim( h, 1 )
			m = strtrim( m, 1 )
			s = strtrim( s, 1 )
			print, " ", d, h, "h ", m, "min ", s, "s"
		endelse
	endelse

end

;========================================================================
pro unwrap_residue, CALLED = called

	common rat, types, file, wid, config

	if ( file.type ne 300 ) and ( file.type ne 301 ) and ( file.type ne 302 ) then begin
		error = DIALOG_MESSAGE( "This is not an interferogram", DIALOG_PARENT = wid.base, TITLE = 'Error', /error )
		return
	endif

; read complete image
	rrat, file.name, arr, info = info, type = type	

; Graphical interface
	if not keyword_set( called ) then begin
		main = WIDGET_BASE( GROUP_LEADER = wid.base, TITLE = 'Identify residues', /modal, /align_center, /column )

		sw = 'Dipole entfernen'
		choice = cw_bgroup( main, sw, set_value = 1, /nonexclusive )

		buttons  = WIDGET_BASE( main, /row, /frame, /align_center )
		but_ok   = WIDGET_BUTTON( buttons, VALUE = ' OK ', xsize = 80, /frame )
		but_canc = WIDGET_BUTTON( buttons, VALUE = ' Cancel ', xsize = 60 )
		but_info = WIDGET_BUTTON( buttons, VALUE = ' Info ', xsize = 60 )

		WIDGET_CONTROL, main, /REALIZE, default_button = but_canc, tlb_get_size = toto
		pos = center_box( toto[0], drawysize = toto[1] )
		widget_control, main, xoffset = pos[0], yoffset = pos[1]
		repeat begin											; Event loop
			event = widget_event( main )
			if event.id eq but_info then begin					; Info Button clicked
				infotext = ['IDENTIFY RESIDUES',$
				' ',$
				'RAT module written 08/2007 by Markus Steiof',$
				'',$
				"Identify Residues is a part of Goldstein's Branch Cut Algorithm",$
				'',$
				'Reference:',$
				'Dennis C. Ghiglia, Mark D. Pritt, Two-Dimensional Phase Unwrapping, 1998',$
				"chapter 4.2, page 103 f. (Goldstein's Branch Cut Algorithm)"]
				info = DIALOG_MESSAGE( infotext, DIALOG_PARENT = main, TITLE = 'Information' )
			end
		endrep until ( event.id eq but_ok ) or ( event.id eq but_canc ) 
		widget_control, choice, GET_VALUE = sw					; read widget fields
		widget_control, main, /destroy							; remove main widget
		if event.id ne but_ok then return						; OK button _not_ clicked
	endif

; change mousepointer
	WIDGET_CONTROL, /hourglass

; undo function
	undo_prepare, outputfile, finalfile, CALLED = CALLED

; pop up progress window

	progress, Message = 'Identify residues...'
	progress,percent=100.0

; ADD transforms for complex, pair, etc.
	if file.type eq 300 then arr = reform( atan( arr[0,*,*] * conj( arr[1,*,*] ), /wfaise ) )
	if file.type eq 301 then arr = atan( arr, /wfaise )

; call identify residues
	result = residue( arr, sw )

; save complete image
	srat, outputfile, result, info = info, type = 4l

; update file information
	file_move, outputfile, finalfile, /overwrite

	file.name = finalfile
	file.dim  = 2l
	file.zdim = 1l
	file.var  = 4l
	file.type = 4l

; generate preview
	if not keyword_set( called ) then begin
		generate_preview
		update_info_box
	endif
end
