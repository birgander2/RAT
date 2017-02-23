;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: unwrap_bc
;
; written by   : Markus Steiof
; last revision: 21. Aug 2007
; reference    : Dennis C. Ghiglia, Mark D. Pritt, Two-Dimensional Phase Unwrapping, 1998
;				 chapter 4.2, page 103 f. (Goldstein's Branch Cut Algorithm)
;------------------------------------------------------------------------
; Calculate Branch Cuts (part of Goldstein's Branch Cut Algorithm)
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
function bc, phase, switch_rm_dipoles

	common branch, flag, xsize_wphase, ysize_wphase, twopi

	wphase = phase

	size_wphase = size( wphase )
	xsize_wphase = size_wphase[1]
	ysize_wphase = size_wphase[2]

	; fuer Branch-Cuts und sonst. Notizen
	flag = replicate( {residue: 0, branch: 0.0, balanced: 0, active: 0}, xsize_wphase, ysize_wphase )

	; '2*pi' wird oft benoetigt
	twopi = 2 * !pi

;	print, "========================================="
;	print, " Berechnung der Branch-Cuts"

;------------------------------------------------------------------------
; Residuen identifizieren
;------------------------------------------------------------------------

;	print, "-----------------------------------------"
;	print, "# Residuen identifizieren"
;	print, "> Suche Residuen..."
;	t_start = systime( 1 )
;	t1 = systime( 1 )

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
				flag[i,j].residue = 1
				number_plus_resi++
			endif

			; wenn Summe '-2*pi' dann Pixel '[i,j]' als negatives Residuum markieren
			if abs( sum_gradients + twopi ) le threshold then begin
				flag[i,j].residue = -1
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
				if flag[i,j].residue ne 0 then begin
					; wenn Pixel in positiver x-Richtung entgegengesetzte Polaritaet hat
					if ( flag[i,j].residue * flag[i + 1,j].residue lt 0 ) then begin
						; Branch-Cut zwischen beiden Residuen setzen
						bc_set_branchcut, [i,j], [i + 1,j]
						; beide Residuen entfernen
						flag[i,j].residue = 0
						flag[i + 1,j].residue = 0
						number_adjdipoles++
					endif
					; wenn Pixel in positiver y-Richtung entgegengesetzte Polaritaet hat
					if ( flag[i,j].residue * flag[i,j + 1].residue lt 0 ) then begin
						; Branch-Cut zwischen beiden Residuen setzen
						bc_set_branchcut, [i,j], [i,j + 1]
						; beide Residuen entfernen
						flag[i,j].residue = 0
						flag[i,j + 1].residue = 0
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
				if flag[i,j].residue ne 0 then begin
					; wenn Pixel in positiver x-/y-Richtung entgegengesetzte Polaritaet hat
					if ( flag[i,j].residue * flag[i + 1,j + 1].residue lt 0 ) then begin
						; Branch-Cut zwischen beiden Residuen setzen
						bc_set_branchcut, [i,j], [i + 1,j + 1]
						; beide Residuen entfernen
						flag[i,j].residue = 0
						flag[i + 1,j + 1].residue = 0
						number_diagdipoles++
					endif $
					else $
						; wenn Pixel in negativer x- und positiver y-Richtung entgegengesetzte Polaritaet hat
						if ( flag[i,j].residue * flag[i - 1,j + 1].residue lt 0 ) then begin
							; Branch-Cut zwischen beiden Residuen setzen
							bc_set_branchcut, [i,j], [i - 1,j + 1]
							; beide Residuen entfernen
							flag[i,j].residue = 0
							flag[i - 1,j + 1].residue = 0
							number_diagdipoles++
						endif
				endif
			endfor
		endfor

		; Sonderfall 1. Spalte (diagonale Dipole)
		for j = 0, ysize_wphase - 3 do begin
			if flag[0,j].residue ne 0 then begin
				; wenn Pixel in positiver x-/y-Richtung entgegengesetzte Polaritaet hat
				if ( flag[0,j].residue * flag[1,j + 1].residue lt 0 ) then begin
					; Branch-Cut zwischen beiden Residuen setzen
					bc_set_branchcut, [0,j], [1,j + 1]
					; beide Residuen entfernen
					flag[0,j].residue = 0
					flag[1,j + 1].residue = 0
					number_diagdipoles++
				endif
			endif
		endfor

;		print, "  |diagonale Dipole entfernt  :", number_diagdipoles
;		print, "  |restliche pos. Residuen    :", number_plus_resi - number_adjdipoles - number_diagdipoles
;		print, "  |restliche neg. Residuen    :", number_minus_resi - number_adjdipoles - number_diagdipoles
;		print, "  |restliche Residuen gesamt  :", ( number_plus_resi + number_minus_resi ) - 2 * number_adjdipoles - 2 * number_diagdipoles
	endif

;	t2 = systime( 1 )
;	bc_show_time, t1, t2, 0

;------------------------------------------------------------------------
; Residuen ausbalancieren
;------------------------------------------------------------------------

;	print, "-----------------------------------------"
;	print, "# Residuen ausbalancieren"
;	print, "> Balanciere Residuen aus..."
;	t1 = systime( 1 )

	; fuer jedes unbalancierte Residuum
	; Schleife laesst letzte Zeile/Spalte aussen vor, da dort lt. Definition keine Pixel als Residuen markiert sein koennen
	for j = 0, ysize_wphase - 2 do begin
		for i = 0, xsize_wphase - 2 do begin
			if ( ( flag[i,j].residue eq 1 ) || ( flag[i,j].residue eq -1 ) ) && ( flag[i,j].balanced eq 0 ) then begin
				x = i
				y = j
				; Residuum als aktiv markieren
				flag[i,j].active = 1
				; Aufladung des Residuums notieren
				charge = flag[i,j].residue
				flag[i,j].balanced = 1
				; Residuum als erstes Element der Liste der aktiven Residuen eintragen
				active_pixellist = [[i,j]]
				ysize_active_pixellist = 1
;				print, "  |Starte Turn bei:", [i,j]
				max_boxsize = min( [i,j,xsize_wphase - 1 - i,ysize_wphase - 1 - j] )
				; Kantenlaenge ist '2 * boxsize - 1', deshalb Start mit 'boxsize = 1'
				for boxsize = 1, max_boxsize do begin
					count_active_pixel = 0
					; fuer jedes aktive Pixel
					repeat begin
						; die Box auf das aktive Pixel legen
						x = active_pixellist[0,count_active_pixel]
						y = active_pixellist[1,count_active_pixel]
						; alle inaktiven Boxresiduen in 'boxresilist' ablegen
						boxresilist = bc_get_boxresi( [x,y], boxsize )
						ysize_boxresilist = get_ysize( boxresilist )
						; Zaehler startet bei '1' und nicht '0', da das 1. Element als Informationstraeger dient (Bildrand erreicht?)
						; die Liste durchgehen
						for q = 1, ysize_boxresilist - 1 do begin
							k = boxresilist[0,q]
							l = boxresilist[1,q]
							; wenn das inaktive Residuum unbalanciert ist
							if flag[k,l].balanced eq 0 then begin
								; Aufladung des Residuums zu 'charge' hinzuaddieren
								charge += flag[k,l].residue
								; Residuum als balanciert markieren
								flag[k,l].balanced = 1
							endif
							; Residuum als aktiv markieren
							flag[k,l].active = 1
							; aktives Residuum der 'active_pixellist' hinzufuegen
							active_pixellist = [[active_pixellist],[k,l]]
							; Branch-Cut zwischen aktivem Residuum und Center-Residuum setzen
							bc_set_branchcut, [x,y], [k,l]
							; wenn Aufladung Null ist dann zur Sprungmarke 'ende'
							if charge eq 0 then goto, ende
						endfor
						; wenn Bildrand erreicht ist (1. Element nicht '-1', dann Bildrand erreicht)
						if boxresilist[0,0] ne -1 then begin
							; Aufladung auf Null setzen
							charge = 0
							; Branch-Cut zwischen Center-Residuum und dem Bildrand setzen
							bc_set_branchcut2border, [x,y]
							goto, ende
						endif
						ysize_active_pixellist = get_ysize( active_pixellist )
						count_active_pixel++
					; solange aktive Pixel aus 'active_pixellist' abzuarbeiten sind, Schleife nicht verlassen
					endrep until count_active_pixel ge ysize_active_pixellist
				endfor

				ende:
				; wenn Aufladung nicht Null dann Branch-Cut zwischen Center-Residuum und dem Bildrand setzen
				if charge ne 0 then bc_set_branchcut2border, [x,y]
				ysize_active_pixellist = get_ysize( active_pixellist )
				; alle aktiven Residuen als inaktiv markieren
				for z = 0, ysize_active_pixellist - 1 do flag[active_pixellist[0,z],active_pixellist[1,z]].active = 0
			endif
		endfor
	endfor

;	t2 = systime( 1 )
;	bc_show_time, t1, t2, 0
;	print, "-----------------------------------------"
;	print, " Berechnung der Branch-Cuts fertig!"
;	t_end = systime( 1 )
;	bc_show_time, t_start, t_end, 1
;	print, "-----------------------------------------"

	; Branch-Cuts liefern
	return, flag.branch

end

;========================================================================
function bc_get_boxresi, center_resi, boxsize
;------------------------------------------------------------------------
; Sucht nach inaktiven Residuen in der Box (Kantenlaenge: '2 * boxsize
; - 1') mit Mittelpunkt 'center_resi' und packt sie in eine fortlaufende
; Liste. Die Box wird von innen nach aussen durchsucht, dem entsprechend
; enthaelt die Liste vorne die inneren Residuen. Das 1. Element der Liste
; ist kein Residuum, sondern ein Indikator fuer das Erreichen des Bild-
; randes.
;------------------------------------------------------------------------
; PRE:  'center_resi' befindet sich innerhalb des Bildes. '2 * boxsize -
;       1' ist die Kantenlaenge der Box.
; POST: In 'resilist' sind alle inaktiven Residuen der Box fortlaufend
;       enthalten. Das 1. Element der Liste ist ein Indikator: '[-1,-1]'
;       bedeutet Bildrand nicht erreicht, jeder andere Wert zeigt an,
;       dass der Bildrand erreicht wurde.
;------------------------------------------------------------------------

	common branch

	x = center_resi[0]
	y = center_resi[1]

	; Liste mit 1. Element initialisieren
	resilist = [[-1,-1]]

	; maximale Boxgroesse, bei der der Bildrand erreicht wird
	maximum = min( [x,y,xsize_wphase - 1 - x,ysize_wphase - 1 - y] )

	; wenn aktuelle Boxgroesse den Bildrand erreicht (oder darueber hinausragt)
	if boxsize ge maximum then begin
		; Liste mit 1. Element neu initialisieren; 1. Element nicht '[-1,-1]' bedeutet Bildrand erreicht
		resilist = [[0,0]]
	endif

	; inaktive Residuen in die Liste einfuegen...(1)(2)
	if boxsize le maximum then begin
		; (1)...Box reicht nicht ueber den Bildrand hinaus, deshalb kein Check, ob Pixel innerhalb des Bildes
		for s = 1, boxsize do begin
			y_d = y - s
			y_u = y + s
			for a = -s, s do begin
				xx = x + a
				n = y_d * xsize_wphase + xx
				if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[xx,y_d]]
				n = y_u * xsize_wphase + xx
				if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[xx,y_u]]
			endfor
			x_l = x - s
			x_r = x + s
			for a = -s + 1, s - 1 do begin
				yy = y + a
				n = yy * xsize_wphase + x_l
				if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[x_l,yy]]
				n = yy * xsize_wphase + x_r
				if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[x_r,yy]]
			endfor
		endfor
	endif $
	else begin
		; (2)...Box reicht ueber den Bildrand hinaus, deshalb Check, ob Pixel innerhalb des Bildes (ansonsten wie (1))
		for s = 1, boxsize do begin
			y_d = y - s
			y_u = y + s
			for a = -s, s do begin
				xx = x + a
				if ( xx ge 0 ) && ( xx lt xsize_wphase ) && ( y_d ge 0 ) && ( y_d lt ysize_wphase ) then begin
					n = y_d * xsize_wphase + xx
					if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[xx,y_d]]
				endif
				if ( xx ge 0 ) && ( xx lt xsize_wphase ) && ( y_u ge 0 ) && ( y_u lt ysize_wphase ) then begin
					n = y_u * xsize_wphase + xx
					if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[xx,y_u]]
				endif
			endfor
			x_l = x - s
			x_r = x + s
			for a = -s + 1, s - 1 do begin
				yy = y + a
				if ( x_l ge 0 ) && ( x_l lt xsize_wphase ) && ( yy ge 0 ) && ( yy lt ysize_wphase ) then begin
					n = yy * xsize_wphase + x_l
					if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[x_l,yy]]
				endif
				if ( x_r ge 0 ) && ( x_r lt xsize_wphase ) && ( yy ge 0 ) && ( yy lt ysize_wphase ) then begin
					n = yy * xsize_wphase + x_r
					if ( ( flag[n].residue eq 1 ) || ( flag[n].residue eq -1 ) ) && ( flag[n].active eq 0 ) then resilist = [[resilist],[x_r,yy]]
				endif
			endfor
		endfor
	endelse

	return, resilist

end

;========================================================================
pro bc_set_branchcut, from_resi, to_resi
;------------------------------------------------------------------------
; Setzt einen Branch-Cut zwischen zwei Residuen.
;------------------------------------------------------------------------
; PRE:  'from_resi' und 'to_resi' befinden sich innerhalb des Bildes.
; POST: Ein Branch-Cut liegt zwischen 'from_resi' und 'to_resi', reprae-
;       sentiert durch den Wert 'value' in 'flag.branch'.
;------------------------------------------------------------------------

	common branch

	x_from = from_resi[0]
	y_from = from_resi[1]
    x_to = to_resi[0]
	y_to = to_resi[1]

	; Wert fuer Branch-Cut in 'flag.branch'
	value = 1

	; Branch-Cut wird nur in positiver x-Richtung gesetzt, also evtl. Start/Ziel vertauschen
	if x_from gt x_to then begin
		tmp = x_from
		x_from = x_to
		x_to = tmp
		tmp = y_from
		y_from = y_to
		y_to = tmp
	endif

	x = x_from
	y = y_from

	dx = x_to - x_from
	dy = y_to - y_from

	if dy ge 0 then begin
		; positive y-Richtung
		if dx le dy then begin
			if dx ne 0 then begin
				steps = dy / dx
			endif $
			else begin
				steps = dy
				dx = 1
			endelse

			rest = dy mod dx
			if rest ne 0 then add = dx / rest else add = 0

			count_add = 0
			count_rest = 0

			for i = 1, dx do begin
				count_add++
				for k = 1, steps do begin
					y++
					flag[x + i,y].branch = value
					if ( count_add eq add ) && ( count_rest ne rest ) then begin
						y++
						count_rest++
						flag[x + i,y].branch = value
						count_add = 0
					endif
				endfor
			endfor
		endif $
		else begin
			if dy ne 0 then begin
				steps = dx / dy
			endif $
			else begin
				steps = dx
				dy = 1
			endelse

			rest = dx mod dy
			if rest ne 0 then add = dy / rest else add = 0

			count_add = 0
			count_rest = 0

			for j = 1, dy do begin
				count_add++
				for b = 1, steps do begin
					x++
					flag[x,y + j].branch = value
					if ( count_add eq add ) && ( count_rest ne rest ) then begin
						x++
						count_rest++
						flag[x,y + j].branch = value
						count_add = 0
					endif
				endfor
			endfor
		endelse
	endif $
	else begin
		; negative y-Richtung
		if dx le abs( dy ) then begin
			if dx ne 0 then begin
				steps = abs( dy ) / dx
			endif $
			else begin
				steps = abs( dy )
				dx = 1
			endelse

			rest = abs( dy ) mod dx
			if rest ne 0 then add = dx / rest else add = 0

			count_add = 0
			count_rest = 0

			for i = 1, dx do begin
				count_add++
				for k = 1, steps do begin
					y--
					flag[x + i,y + 1].branch = value
					if ( count_add eq add ) && ( count_rest ne rest ) then begin
						y--
						count_rest++
						flag[x + i,y + 1].branch = value
						count_add = 0
					endif
				endfor
			endfor
		endif $
		else begin
			if dy ne 0 then begin
				steps = dx / abs( dy )
			endif $
			else begin
				steps = dx
				dy = 1
			endelse

			rest = dx mod abs( dy )
			if rest ne 0 then add = abs( dy ) / rest else add = 0

			count_add = 0
			count_rest = 0

			for j = 1, abs( dy ) do begin
				count_add++
				for k = 1, steps do begin
					x++
					flag[x,y - j + 1].branch = value
					if ( count_add eq add ) && ( count_rest ne rest ) then begin
						x++
						count_rest++
						flag[x,y - j + 1].branch = value
						count_add = 0
					endif
				endfor
			endfor
		endelse
	endelse

end

;========================================================================
pro bc_set_branchcut2border, from_resi
;------------------------------------------------------------------------
; Setzt einen Branch-Cut zwischen einem Residuum und dem Bildrand.
;------------------------------------------------------------------------
; PRE:  'from_resi' befindet sich innerhalb des Bildes.
; POST: Ein Branch-Cut liegt zwischen 'from_resi' und dem naechst gelege-
;       nen Bildrand, repraesentiert durch den Wert 'value' in
;       'flag.branch'.
;------------------------------------------------------------------------

	common branch

	x = from_resi[0]
	y = from_resi[1]

	; Wert fuer Branch-Cut in 'flag.branch'
	value = 1

	s = size( flag )
	xsize_flag = s[1]
	ysize_flag = s[2]

	; 4 Richtungen moeglich, kuerzesten Weg zum Bildrand ermitteln
	dir0 = x
	dir1 = y
	dir2 = xsize_flag - 1 - x
	dir3 = ysize_flag - 1 - y
	directions = [dir0,dir1,dir2,dir3]
	min = dir0
	dir = 0
	for i = 1, 3 do begin
		if directions[i] lt min then begin
			min = directions[i]
			dir = i
		endif
	endfor

	; Branch-Cut zum Bildrand in entsprechende Richtung setzen
	case dir of
		0: flag[0:x,y + 1].branch = value
		1: flag[x + 1,0:y].branch = value
		2: flag[x + 1:xsize_flag - 1,y + 1].branch = value
		3: flag[x + 1,y + 1:ysize_flag - 1].branch = value
	endcase

end

;========================================================================
function bc_get_ysize, array
;------------------------------------------------------------------------
; Liefert die Groesse der 2. Dimension eines Arrays. Hat der Array nur
; eine Dimension, wird '1' geliefert. Nuetzlich, um die Anzahl der refe-
; renzierten Pixel zu erfahren, falls Pixelkoordinaten paarweise in einer
; fortlaufenden Liste abgelegt sind.
;------------------------------------------------------------------------
; PRE:  'array' ist ein Array und nicht leer.
; POST: 'y' enthaelt die Groesse der 2. Dimension, ansonsten '1'.
;------------------------------------------------------------------------

	s = size( array )
	if s[0] gt 1 then y = s[2] else y = 1

	return, y

end

;========================================================================
pro bc_show_time, time_start, time_end, fin
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
pro unwrap_bc, CALLED = called

	common rat, types, file, wid, config

	if ( file.type ne 300 ) and ( file.type ne 301 ) and ( file.type ne 302 ) then begin
		error = DIALOG_MESSAGE( "This is not an interferogram", DIALOG_PARENT = wid.base, TITLE = 'Error', /error )
		return
	endif

; read complete image
	rrat, file.name, arr, info = info, type = type	

; Graphical interface
	if not keyword_set( called ) then begin
		main = WIDGET_BASE( GROUP_LEADER = wid.base, TITLE = 'Calculate branch cuts', /modal, /align_center, /column )

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
				infotext = ['CALCULATE BRANCH CUTS',$
				' ',$
				'RAT module written 08/2007 by Markus Steiof',$
				'',$
				"Calculate Branch Cuts is a part of Goldstein's Branch Cut Algorithm",$
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

	progress, Message = 'Calculate branch cuts...'
	progress,percent=100.0

; ADD transforms for complex, pair, etc.
	if file.type eq 300 then arr = reform( atan( arr[0,*,*] * conj( arr[1,*,*] ), /wfaise ) )
	if file.type eq 301 then arr = atan( arr, /wfaise )

; call calculate branch cut
	result = bc( arr, sw )

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
