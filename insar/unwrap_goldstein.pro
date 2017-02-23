;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: unwrap_goldstein
;
; written by   : Markus Steiof
; last revision: 21. Aug 2007
; reference    : Dennis C. Ghiglia, Mark D. Pritt, Two-Dimensional Phase Unwrapping, 1998
;				     chapter 4.2, page 103 f. (Goldstein's Branch Cut Algorithm)
;------------------------------------------------------------------------
; Goldstein's Branch-Cut Phase Unwrapping Algorithm
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
; Hinweis: Kommentare beginnend mit '#' entsprechen dem Pseudocode

forward_function init_adjoinlist

;========================================================================
function goldstein, phase, switch_rm_dipoles, switch_unwrap_bc, xinit, yinit
;------------------------------------------------------------------------
; Diese Funktion unwrappt das uebergebene Interferogramm nach Goldstein's
; Branch-Cut Algorithmus. Der User hat die Moeglichkeit, die Dipole zu
; entfernen, ebenso wie die Branch-Cut-Pixel unzuwrappen. Auch kann er
; den Startpunkt der Pfadintegration festlegen.
;------------------------------------------------------------------------
; PRE:  'phase' ist das zu unwrappende Interferogramm.
;       'switch_rm_dipoles' hat den Wert '1' (Dipole entfernen) oder '0'.
;       'switch_unwrap_bc' hat den Wert '1' (Branch-Cut-Pixel unwrappen)
;       oder '0'. 'xinit', 'yinit' sind die Startkoordinaten fuer die
;       Pfadintegration und liegen innerhalb der Bildgrenzen.
; POST: Das Interferogramm ist ungewrappt.
;------------------------------------------------------------------------

	common gold, flag, xsize_wphase, ysize_wphase, xinit_unwrap, yinit_unwrap, twopi, adjoinlist, wphase, uwphase, border_array, nodark, alldark, onedark1, onedark2, onedark3, onedark4, twodark1, twodark2, twodark3, twodark4, twodark5, twodark6, threedark1, threedark2, threedark3, threedark4, xinit_count, yinit_count

	wphase = phase

	size_wphase = size( wphase )
	xsize_wphase = size_wphase[1]
	ysize_wphase = size_wphase[2]

	; folgende zwei Variablen, um eine Auffindposition in der Funktion 'init_adjoinlist' zu merken
	xinit_count = 1
	yinit_count = 1

	; fuer Notizen...
	flag = replicate( {residue: 0, branch: 0, balanced: 0, active: 0, unwrapped: 0}, xsize_wphase, ysize_wphase )

	; '2pi' wird oft benoetigt
	twopi = 2 * !pi

;	print, "========================================="
;	print, " Goldstein's Branch-Cut Phase Unwrapping"

;------------------------------------------------------------------------
; Residuen identifizieren
;------------------------------------------------------------------------

;	print, "-----------------------------------------"
;	print, "# Residuen identifizieren"
;	print, "> Suche Residuen..."
;	t_start = systime( 1 )
;	t1 = systime( 1 )

	; Summe der 4 Differenzen nicht unbedingt genau '2pi' oder '-2pi', deshalb Toleranzbereich durch Schwelle
	threshold = 0.001

	; zaehlt positive und negative Residuen
	number_plus_resi = 0UL
	number_minus_resi = 0UL

	; Array fuer die 4 Differenzen
	gradients = fltarr( 4 )

	; #fuer (jedes Pixel '[i,j]' in dem Bild)
	for j = 0, ysize_wphase - 2 do begin
		for i = 0, xsize_wphase - 2 do begin
			; #Summiere die gewrappten Phasendifferenzen
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

			; #wenn (die Summe ist '2pi') dann markiere Pixel '[i,j]' als ein positives Residuum
			if abs( sum_gradients - twopi ) le threshold then begin
				flag[i,j].residue = 1
				number_plus_resi++
			endif

			; #wenn (die Summe ist '-2pi') dann markiere Pixel '[i,j]' als ein negatives Residuum
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

		; #fuer (jedes Residuum)
		for j = 0, ysize_wphase - 2 do begin
			for i = 0, xsize_wphase - 2 do begin
				if flag[i,j].residue ne 0 then begin
					; #wenn (es existiert ein angrenzendes Residuum entgegengesetzter Polaritaet) dann
					; wenn Pixel in positiver x-Richtung entgegengesetzte Polaritaet hat
					if ( flag[i,j].residue * flag[i + 1,j].residue lt 0 ) then begin
						; #Setze einen Branch-Cut zwischen den beiden Residuen
						set_branchcut, [i,j], [i + 1,j]
						; #Entferne die beiden Residuen
						flag[i,j].residue = 0
						flag[i + 1,j].residue = 0
						number_adjdipoles++
					endif $
					else $
						; wenn Pixel in positiver y-Richtung entgegengesetzte Polaritaet hat
						if ( flag[i,j].residue * flag[i,j + 1].residue lt 0 ) then begin
							; #Setze einen Branch-Cut zwischen den beiden Residuen
							set_branchcut, [i,j], [i,j + 1]
							; #Entferne die beiden Residuen
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

		; #fuer (jedes Residuum)
		for j = 0, ysize_wphase - 3 do begin
			for i = 1, xsize_wphase - 2 do begin
				if flag[i,j].residue ne 0 then begin
					; #wenn (es existiert ein angrenzendes Residuum entgegengesetzter Polaritaet) dann
					; wenn Pixel in positiver x-/y-Richtung entgegengesetzte Polaritaet hat
					if ( flag[i,j].residue * flag[i + 1,j + 1].residue lt 0 ) then begin
						; #Setze einen Branch-Cut zwischen den beiden Residuen
						set_branchcut, [i,j], [i + 1,j + 1]
						; #Entferne die beiden Residuen
						flag[i,j].residue = 0
						flag[i + 1,j + 1].residue = 0
						number_diagdipoles++
					endif $
					else $
						; wenn Pixel in negativer x- und positiver y-Richtung entgegengesetzte Polaritaet hat
						if ( flag[i,j].residue * flag[i - 1,j + 1].residue lt 0 ) then begin
							; #Setze einen Branch-Cut zwischen den beiden Residuen
							set_branchcut, [i,j], [i - 1,j + 1]
							; #Entferne die beiden Residuen
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
				; #wenn (es existiert ein angrenzendes Residuum entgegengesetzter Polaritaet) dann
				; wenn Pixel in positiver x-/y-Richtung entgegengesetzte Polaritaet hat
				if ( flag[0,j].residue * flag[1,j + 1].residue lt 0 ) then begin
					; #Setze einen Branch-Cut zwischen den beiden Residuen
					set_branchcut, [0,j], [1,j + 1]
					; #Entferne die beiden Residuen
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
;	show_time, t1, t2, 0

;------------------------------------------------------------------------
; Residuen ausbalancieren
;------------------------------------------------------------------------

;	print, "-----------------------------------------"
;	print, "# Residuen ausbalancieren"
;	print, "> Balanciere Residuen aus..."
;	t1 = systime( 1 )

	; #fuer (jedes unbalancierte Residuum)
	; Schleife laesst letzte Zeile/Spalte aussen vor, da dort lt. Definition keine Pixel als Residuen markiert sein koennen
	for j = 0, ysize_wphase - 2 do begin
		for i = 0, xsize_wphase - 2 do begin
			if ( ( flag[i,j].residue eq 1 ) || ( flag[i,j].residue eq -1 ) ) && ( flag[i,j].balanced eq 0 ) then begin
				x = i
				y = j
				; #Markiere das Residuum als aktiv
				flag[i,j].active = 1
				; #Wenn (das Residuum ist positiv) dann 'charge = 1' sonst 'charge = -1'
				charge = flag[i,j].residue
				flag[i,j].balanced = 1
				; Residuum als erstes Element der Liste der aktiven Residuen eintragen
				active_pixellist = [[i,j]]
				ysize_active_pixellist = 1
;				print, "  |Starte Turn bei:", [i,j]
				max_boxsize = min( [i,j,xsize_wphase - 1 - i,ysize_wphase - 1 - j] )
				; #fuer ('n = 3' bis 'max_boxsize' Schrittweite '2')
				; (hier: Kantenlaenge ist '2 * boxsize - 1', deshalb Start mit 'boxsize = 1')
				for boxsize = 1, max_boxsize do begin
					count_active_pixel = 0
					; #fuer (jedes aktive Pixel)
					repeat begin
						; #Zentriere die n*n-Box auf dem aktiven Pixel
						x = active_pixellist[0,count_active_pixel]
						y = active_pixellist[1,count_active_pixel]
						; alle inaktiven Box-Residuen in 'boxresilist' ablegen
						boxresilist = get_boxresi( [x,y], boxsize )
						ysize_boxresilist = get_ysize( boxresilist )
						; #fuer (jedes Box-Pixel in der n*n-Box)
						; (genauer: nur Residuen in der Liste)
						; Zaehler startet bei '1' und nicht '0', da das 1. Element als Informationstraeger dient (Bildrand erreicht?)
						for q = 1, ysize_boxresilist - 1 do begin
							k = boxresilist[0,q]
							l = boxresilist[1,q]
							; #sonst wenn (das Box-Pixel ist ein inaktives Residuum) dann
							; (die Boxsuche liefert nur inaktive Residuen!)
							; #wenn (das Box-Pixel ist unbalanciert) dann
							if flag[k,l].balanced eq 0 then begin
								; #Addiere seine Polaritaet ('1' oder '-1') zu 'charge'
								charge += flag[k,l].residue
								; #Markiere das Box-Pixel als balanciert
								flag[k,l].balanced = 1
							endif
							; #Markiere das Box-Pixel aktiv
							flag[k,l].active = 1
							; aktives Pixel (Residuum) der 'active_pixellist' hinzufuegen
							active_pixellist = [[active_pixellist],[k,l]]
							; #Setze einen Branch-Cut zwischen dem aktiven Pixel und dem Box-Pixel
							set_branchcut, [x,y], [k,l]
							; #wenn ('charge = 0') dann gehe zur Anweisung markiert mit * unten
							if charge eq 0 then goto, ende
						endfor
						; #wenn (das Box-Pixel ist ein Rand-Pixel) dann
						; 1. Element nicht '-1', dann Bildrand erreicht
						if boxresilist[0,0] ne -1 then begin
							; #'charge = 0'
							charge = 0
							; #Setze einen Branch-Cut zwischen dem aktiven Pixel und dem Rand
							set_branchcut2border, [x,y]
							goto, ende
						endif
						ysize_active_pixellist = get_ysize( active_pixellist )
						count_active_pixel++
					; solange aktive Pixel aus 'active_pixellist' abzuarbeiten sind, Schleife nicht verlassen
					endrep until count_active_pixel ge ysize_active_pixellist
				endfor

				ende:
				; #*wenn ('charge' ist nicht Null) dann setze einen Branch-Cut zum Rand
				if charge ne 0 then set_branchcut2border, [x,y]
				; #Markiere alle aktiven Pixel als [balanciert und] inaktiv
				; (alle aktiven Pixel sind schon als balanciert markiert)
				ysize_active_pixellist = get_ysize( active_pixellist )
				for z = 0, ysize_active_pixellist - 1 do flag[active_pixellist[0,z],active_pixellist[1,z]].active = 0
			endif
		endfor
	endfor

;	t2 = systime( 1 )
;	show_time, t1, t2, 0

;------------------------------------------------------------------------
; Pfadintegration
;------------------------------------------------------------------------

;	print, "-----------------------------------------"
;	print, "# Pfad-Integration"
;	print, "> Integriere Nicht-Branch-Cut-Pixel..."
;	t1 = systime( 1 )

	; Initial-Startpunkt fuer Integration
	xinit_unwrap = xinit
	yinit_unwrap = yinit

	uwphase = fltarr( xsize_wphase, ysize_wphase )

	; neuer Array, um einen Rand (1 Pixel breit) mit Einsen um 'flag.branch' zu generieren;
	; neben den Branch-Cut-Pixel werden in diesem Array ebenfalls ungewrappte Pixel notiert;
	; der Grund: viele (zeitaufwendige) Bildrand-Abfragen entfallen dadurch in der Funktion 'update_adjoinlist'
	border_array = fltarr( xsize_wphase + 2, ysize_wphase + 2 )
	border_array[*,*] = 1
	border_array[1,1] = flag.branch

	; diese Arrays werden in der Funktion 'update_adjoinlist' benoetigt und hier einmalig initialisiert
	nodark = [[0,0],[0,0]]
	alldark = [[1,1],[1,1]]
	onedark1 = [[1,0],[0,0]]
	onedark2 = [[0,1],[0,0]]
	onedark3 = [[0,0],[1,0]]
	onedark4 = [[0,0],[0,1]]
	twodark1 = [[1,1],[0,0]]
	twodark2 = [[0,1],[1,0]]
	twodark3 = [[0,0],[1,1]]
	twodark4 = [[1,0],[0,1]]
	twodark5 = [[1,0],[1,0]]
	twodark6 = [[0,1],[0,1]]
	threedark1 = [[1,1],[1,0]]
	threedark2 = [[0,1],[1,1]]
	threedark3 = [[1,0],[1,1]]
	threedark4 = [[1,1],[0,1]]

    ; isolierte Regionen innerhalb des Bildes zaehlen
	number_regions = -1L

	; #wiederhole
	repeat begin
		; #Waehle ein Start-Pixel
		start_adjoinlist = init_adjoinlist()
		x = start_adjoinlist[0]
		y = start_adjoinlist[1]
		; wenn 'x' nicht '-1', dann hat 'init_adjoinlist()' ein Start-Pixel gefunden
		if x ne -1 then begin
			number_regions++
;			print, "  |Startpunkt fuer Adjoin-Liste:", [x - 1,y - 1]
			adjoinlist = [[x,y]]
			count_adjoinlist = 1
			; #Speichere seinen Phasenwert im Ergebnis-Array
			uwphase[x - 1,y - 1] = wphase[x - 1,y - 1]
			; #Markiere das Pixel als ungewrappt
			border_array[x,y] = 1
			; #Aktualisiere die Adjoin-Liste (siehe unten)
			update_adjoinlist, [x,y]
			ysize_adjoinlist = get_ysize( adjoinlist )
			; #waehrend (die Adjoin-Liste ist nicht leer)
			while count_adjoinlist lt ysize_adjoinlist do begin
				; #Hole das naechste Pixel von der Adjoin-Liste
				x = adjoinlist[0,count_adjoinlist]
				y = adjoinlist[1,count_adjoinlist]
				; #Aktualisiere die Adjoin-Liste (siehe unten)
				update_adjoinlist, [x,y]
				ysize_adjoinlist = get_ysize( adjoinlist )
				count_adjoinlist++
				; der Zugriff auf eine lange Liste dauert lange, deshalb wird der nicht benoetigte Teil entfernt
				if count_adjoinlist gt 500 then begin
					adjoinlist = adjoinlist[count_adjoinlist*2:*]
					anz = size( adjoinlist )
					adjoinlist = reform( adjoinlist, 2, anz[1] / 2 )
					count_adjoinlist = 0
				endif
			endwhile
		endif
	; #bis (alle [Nicht-Rand-,] Nicht-Branch-Cut-Pixel sind ungewrappt)
	; (Bildrand-Pixel werden doch ungewrapped!)
	; 'init_adjoinlist()' liefert fuer 'x' und 'y' '-1', falls keine gewrappten Pixel mehr ausser Branch-Cut-Pixel
	endrep until x eq -1

;	print, "  |isolierte Regionen innerhalb des Bildes:", number_regions

	if switch_unwrap_bc eq 1 then begin
		; ein Rand mit Nullen generieren
		border_array[*,0] = 0
		border_array[*,ysize_wphase + 1] = 0
		border_array[0,*] = 0
		border_array[xsize_wphase + 1,*] = 0
		; in 'border_array' die ungewrappten Pixel notieren
		border_array[1,1] = border_array[1,1] - flag.branch
;		print, "> Integriere Branch-Cut-Pixel..."
		; #fuer (jedes Branch-Cut-Pixel)
		for j = 1, ysize_wphase do begin
			for i = 1, xsize_wphase do begin
				if flag[i - 1,j - 1].branch eq 1 then begin
					pixels = [[i + 1,j],[i,j - 1],[i - 1,j],[i,j + 1]]
					for k = 0, 3 do begin
						x = pixels[0,k]
						y = pixels[1,k]
						; #wenn (das Pixel grenzt an ein ungewrapptes Pixel) dann
						if border_array[x,y] eq 1 then begin
							; #Unwrappe das Branch-Cut-Pixel
							gradient = wphase[i - 1,j - 1] - wphase[x - 1,y - 1]
							; damit im Intervall ]-pi,+pi]
							if gradient gt !pi then gradient -= twopi
							if gradient le -!pi then gradient += twopi
							uwphase[i - 1,j - 1] = uwphase[x - 1,y - 1] + gradient
							break
						endif
					endfor
				endif
			endfor
		endfor
	endif

;	t2 = systime( 1 )
;	show_time, t1, t2, 0
;	print, "-----------------------------------------"
;	print, " Phase Unwrapping fertig!"
;	t_end = systime( 1 )
;	show_time, t_start, t_end, 1
;	print, "-----------------------------------------"

	; ungewrappte Phase liefern
	return, uwphase

end

;========================================================================
function init_adjoinlist
;------------------------------------------------------------------------
; Sucht ein Start-Pixel fuer die Adjoin-Liste. Keine Branch-Cut-Pixel und
; nur gewrappte Pixel kommen in Betracht. Das erste gefundene Pixel wird
; zurueckgegeben. Wird kein Pixel fuer die Liste gefunden, liefert die
; Funktion '[-1,-1]'. Beim ersten Aufruf dieser Funktion wird das Start-
; Pixel '[xinit_unwrap,yinit_unwrap]' (siehe Parameter der Funktion
; 'goldstein') zurueckgegeben, falls es nicht auf einem Branch-Cut liegt.
;------------------------------------------------------------------------
; PRE:  Das Start-Pixel '[xinit_unwrap,yinit_unwrap]' muss innerhalb des
;       Bildes liegen oder 'xinit_unwrap'/'yinit_unwrap' gleich '-1'.
; POST: Ein Start-Pixel steht in 'start'. Gibt es nichts mehr zum Unwrap-
;       pen, wird '[-1,-1]' zurueckgegeben.
;------------------------------------------------------------------------

	common gold

	search = 0

	; neue Suche nach Start-Pixel notieren...(1)(2)
	; (1)...wenn 'xinit_unwrap' nicht mehr den Wert der User-Eingabe hat (User-Eingabe ist ungleich '-1')
	if xinit_unwrap eq -1 then search = 1 $
	else $
		; (2)...wenn User-Eingabe des Start-Pixels auf Branch-Cut liegt
		if flag[xinit_unwrap,yinit_unwrap].branch eq 1 then begin
			print, "  |Startpunkt fuer Integration (User-Eingabe) liegt auf einem Branch-Cut!"
			print, "> Suche neuen Startpunkt..."
			search = 1
			; neue Werte, um User-Eingabe des Start-Pixels als "abgearbeitet" zu markieren
			xinit_unwrap = -1
			yinit_unwrap = -1
		endif

	if search eq 1 then begin
		; neues Start-Pixel suchen; wird kein Start-Pixel mehr gefunden, liefert die Funktion '[-1,-1]'
		; wird ein Start-Pixel gefunden, diese Position merken und beim naechsten Suchlauf dort weitersuchen
		start = [-1,-1]
		for j = yinit_count, ysize_wphase do begin
			for i = xinit_count, xsize_wphase do begin
				; wenn kein Branch-Cut-Pixel und kein ungewrapptes Pixel
				if border_array[i,j] eq 0 then begin
					start = [i,j]
					; Auffindposition merken
					xinit_count = i
					yinit_count = j
					goto, ende
				endif
			endfor
			xinit_count = 1
		endfor
	endif $
	else begin
		; hier angekommen, wird die User-Eingabe als Start-Pixel genommen
		start = [xinit_unwrap + 1,yinit_unwrap + 1]
		; neue Werte, um User-Eingabe des Start-Pixels als "abgearbeitet" zu markieren
		xinit_unwrap = -1
		yinit_unwrap = -1
	endelse

	ende:
	return, start

end

;========================================================================
pro update_adjoinlist, pixel
;------------------------------------------------------------------------
; Unwrapped max. 4 Nachbar-Pixel eines uebergebenen Pixels und fuegt sie
; der Adjoin-Liste hinzu. Dies geschieht nur, wenn die Pixel noch nicht
; ungewrapped sind, nicht in der Adjoin-Liste stehen und nicht auf einem
; Branch-Cut liegen. Diese 3 Bedingungen werden in dem Array
; 'border_array' repraesentiert, wobei der Wert '0' das Unwrappen er-
; laubt, '1' hingegen nicht.
;------------------------------------------------------------------------
; PRE:  'pixel' ist ein gewrapptes Pixel aus der Adjoin-Liste.
; POST: Die max. 4 in dieser Funktion ungewrappten Pixel sind der Adjoin-
;       List ('adjoinlist') hinzugefuegt.
;------------------------------------------------------------------------

	common gold

	; Center-Pixel
	x_center = pixel[0]
	y_center = pixel[1]
	x = x_center
	y = y_center

	; folgender Pseudocode laesst sich den Zeilen nicht zuordnen:
	; #fuer (jedes der vier angrenzenden Pixel)
	; #wenn (das Pixel ist nicht auf der Adjoin-Liste und nicht ungewrappt) dann
	; #wenn (das Pixel ist kein Branch-Cut-Pixel [und kein Rand-Pixel]) dann
	; Erlaeuterung: Die voranstehenden Bedingungen werden mit Hilfe von 'border_array' und
	; dem Mustervergleich realisiert (s.u.); das Ergebnis ist jeweils eine Liste von 0-4 Pixel,
    ; fuer die das Unwrappen erlaubt ist (Bildrand-Pixel werden auch ungewrappt!)

	; Werte der 4 Nachbarn des Center-Pixels in einer Liste merken
	adjoin = [[border_array[x,y + 1],border_array[x - 1,y]],[border_array[x,y - 1],border_array[x + 1,y]]]

	; Vergleich der Nachbar-Pixel mit einem Muster ergibt eine Liste ('unwrap') der zu unwrappenden Pixel;
	; alle Pixel in dieser Liste stehen nicht auf der Adjoin-Liste (und sind somit noch gewrapped) und
	; liegen nicht auf einem Branch-Cut; die Reihenfolge der case-Abfragen orientiert sich an der Haeufigkeit
	; der auftretenden Faelle (was oefter eintritt, steht weiter vorne)
	case 1 of
		array_equal( adjoin, threedark1 ) : unwrap = [[x + 1,y]]
		array_equal( adjoin, threedark3 ) : unwrap = [[x - 1,y]]
		array_equal( adjoin, threedark4 ) : unwrap = [[x,y - 1]]
		array_equal( adjoin, twodark1 ) : unwrap = [[x,y - 1],[x + 1,y]]
		array_equal( adjoin, twodark4 ) : unwrap = [[x - 1,y],[x,y - 1]]
		array_equal( adjoin, onedark3 ) : unwrap = [[x,y + 1],[x - 1,y],[x + 1,y]]
		array_equal( adjoin, twodark3 ) : unwrap = [[x,y + 1],[x - 1,y]]
		array_equal( adjoin, twodark2 ) : unwrap = [[x,y + 1],[x + 1,y]]
		array_equal( adjoin, threedark2 ) : unwrap = [[x,y + 1]]
		array_equal( adjoin, onedark4 ) : unwrap = [[x,y + 1],[x - 1,y],[x,y - 1]]
		array_equal( adjoin, onedark1 ) : unwrap = [[x - 1,y],[x,y - 1],[x + 1,y]]
		array_equal( adjoin, twodark5 ) : unwrap = [[x - 1,y],[x + 1,y]]
		array_equal( adjoin, nodark ) : unwrap = [[x,y + 1],[x - 1,y],[x,y - 1],[x + 1,y]]
		array_equal( adjoin, onedark2 ) : unwrap = [[x,y + 1],[x,y - 1],[x + 1,y]]
		array_equal( adjoin, twodark6 ) : unwrap = [[x,y + 1],[x,y - 1]]
		array_equal( adjoin, alldark ) : goto, ende
	endcase

	ysize_unwrap = get_ysize( unwrap )

	for n = 0, ysize_unwrap - 1 do begin
		x = unwrap[ishft( n, 1)]			; 'x = unwrap[0,n]' bzw. 'x = unwrap[2*n]'
		y = unwrap[ishft( n, 1) + 1]		; 'y = unwrap[1,n]' bzw. 'y = unwrap[2*n + 1]'
		; #Unwrappe das Pixel und speichere den Wert im Ergebnis-Array
		gradient = wphase[x - 1,y - 1] - wphase[x_center - 1,y_center - 1]
		; damit im Intervall ]-pi,+pi]
		if gradient gt !pi then gradient -= twopi
		if gradient le -!pi then gradient += twopi
		uwphase[x - 1,y - 1] = uwphase[x_center - 1,y_center - 1] + gradient
		; #Fuege das Pixel in die Adjoin-Liste ein
		adjoinlist = [[adjoinlist],[x,y]]
		; #Markiere das Pixel als ungewrappt
		border_array[x,y] = 1
	endfor

	ende:

end

;========================================================================
function get_boxresi, center_resi, boxsize
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

	common gold

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
pro set_branchcut, from_resi, to_resi
;------------------------------------------------------------------------
; Setzt einen Branch-Cut zwischen zwei Residuen.
;------------------------------------------------------------------------
; PRE:  'from_resi' und 'to_resi' befinden sich innerhalb des Bildes.
; POST: Ein Branch-Cut liegt zwischen 'from_resi' und 'to_resi', reprae-
;       sentiert durch den Wert 'value' in 'flag.branch'.
;------------------------------------------------------------------------

	common gold

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
pro set_branchcut2border, from_resi
;------------------------------------------------------------------------
; Setzt einen Branch-Cut zwischen einem Residuum und dem Bildrand.
;------------------------------------------------------------------------
; PRE:  'from_resi' befindet sich innerhalb des Bildes.
; POST: Ein Branch-Cut liegt zwischen 'from_resi' und dem naechst gelege-
;       nen Bildrand, repraesentiert durch den Wert 'value' in
;       'flag.branch'.
;------------------------------------------------------------------------

	common gold

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
function get_ysize, array
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
pro show_time, time_start, time_end, fin
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
pro unwrap_goldstein, CALLED = called

	common rat, types, file, wid, config

	if ( file.type ne 300 ) and ( file.type ne 301 ) and ( file.type ne 302 ) then begin
		error = DIALOG_MESSAGE( "This is not an interferogram", DIALOG_PARENT = wid.base, TITLE = 'Error', /error )
		return
	endif

; read complete image
	rrat, file.name, arr, info = info, type = type	

; Groesse des Arrays holen, um spaeter die User-Eingabe des Startpunktes fuer die Integration zu checken
	size_arr = size( arr )
	xsize_arr = size_arr[1]
	ysize_arr = size_arr[2]

; Graphical interface
	if not keyword_set( called ) then begin
		main = WIDGET_BASE( GROUP_LEADER = wid.base, TITLE = 'Branch-Cut phase unwrapping', /modal, /align_center, /column )

		sw = ['Dipole entfernen','Integration der Branch-Cut-Pixel']
		choice = cw_bgroup( main, sw, set_value = [1,1], /nonexclusive )

		field1 = cw_field( main, value = xsize_arr / 2 - 1, /integer, title = ' Startpunkt fuer Integration  x:', xsize = 5 )
		field2 = cw_field( main, value = ysize_arr / 2 - 1, /integer, title = ' Startpunkt fuer Integration  y:', xsize = 5 )

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
				infotext = ['BRANCH-CUT PHASE UNWRAPPING',$
				' ',$
				'RAT module written 08/2007 by Markus Steiof',$
				'',$
				'Reference:',$
				'Dennis C. Ghiglia, Mark D. Pritt, Two-Dimensional Phase Unwrapping, 1998',$
				"chapter 4.2, page 103 f. (Goldsteins Branch Cut Algorithm)"]
				info = DIALOG_MESSAGE( infotext, DIALOG_PARENT = main, TITLE = 'Information' )
			end
		endrep until ( event.id eq but_ok ) or ( event.id eq but_canc ) 
		widget_control, choice, GET_VALUE = sw					; read widget fields
		widget_control, field1, GET_VALUE = x_start				; read widget fields
		widget_control, field2, GET_VALUE = y_start				; read widget fields
		widget_control, main, /destroy							; remove main widget
		if event.id ne but_ok then return						; OK button _not_ clicked
	endif

; Error Handling
	if ( x_start lt 0 ) || ( x_start ge xsize_arr ) || ( y_start lt 0 ) || ( y_start ge ysize_arr ) then begin
		error = DIALOG_MESSAGE( "Die Werte muessen innerhalb der Bildgrenzen liegen!", DIALOG_PARENT = wid.base, TITLE = 'Error', /error )
		return
	endif

; change mousepointer

	WIDGET_CONTROL, /hourglass

; undo function

	undo_prepare, outputfile, finalfile, CALLED = CALLED

; pop up progress window
	
	progress, Message = 'Branch-Cut unwrapping...'
	progress,percent=100.0
	
; unwrap image

; ADD transforms for complex, pair, etc.

	if file.type eq 300 then arr = reform( atan( arr[0,*,*] * conj( arr[1,*,*] ), /wfaise ) )
	if file.type eq 301 then arr = atan( arr, /wfaise )

; call branch cut phase unwrapping
	result = goldstein( arr, sw[0], sw[1], x_start, y_start )

; save complete image
	srat, outputfile, result, info = info, type = 303l

; update file information
	file_move, outputfile, finalfile, /overwrite

	file.name = finalfile
	file.dim  = 2l
	file.zdim = 1l
	file.var  = 4l
	file.type = 303l

; generate preview
	if not keyword_set( called ) then begin
		generate_preview
		update_info_box
	endif
end
