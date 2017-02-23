;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: geo_calc
; written by    : Tisham Dhar
; last revision : 08/01/2009
; Perform a few pixel/line to geographic transformations
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

pro geo_calc,x,y,xout,yout,transform,direction
;----Perform forward or reverse transforms
;----using supplied 2-d affine transform
	case direction of
		0: begin
		;----Forward transform
			xout = transform[0] + x*transform[1] + y*transform[2]
			yout = transform[3] + x*transform[4] + y*transform[5]
		end
		1: begin
		;----Reverse transform
			xoff = transform[0]
	    	yoff = transform[3]
	   		xstep = transform[1]
	    	ystep = transform[5]

	    	fwdmatrix = [[transform[1],transform[2]],[transform[4],transform[5]]]
	    	invmatrix = invert(fwdmatrix)

	    	xout = round((x-xoff)*invmatrix[0,0]+(y-yoff)*invmatrix[0,1])
	    	yout = round((x-xoff)*invmatrix[1,0]+(y-yoff)*invmatrix[1,1])

		end
		else: begin
			print, "Which way do you want to go ?"
			xout = x
			yout = y
		end
	endcase
end

function gcps2transform,pasGCPs
;---convert a set of x,y,lat,lon gcp's
;---to an affine transform with 6-components
;---and least-square error
;---GCP's are of the following form by row
;---1:Pixel,2:Line,3:Longitude,4:Latitude
    i = 0
	padfGeoTransform = fltarr(6)
;--------------------------------------------------------------------;
;      Recognise a few special cases.                                ;
;--------------------------------------------------------------------;
    siz = size(pasGCPs)
    print,siz
    nGCPCount = siz[1]
    if nGCPCount lt 2 then begin
        return,-1
    endif

    if nGCPCount eq 2 then begin

    	if (pasGCPs[1,0] eq pasGCPs[0,0]) or (pasGCPs[1,1] eq pasGCPs[0,1]) then return,-1

        padfGeoTransform[1] = (pasGCPs[1,2] - pasGCPs[0,2]) $
            / (pasGCPs[1,0] - pasGCPs[0,0])
        padfGeoTransform[2] = 0.0

        padfGeoTransform[4] = 0.0
        padfGeoTransform[5] = (pasGCPs[1,3] - pasGCPs[0,3]) $
            / (pasGCPs[1,1] - pasGCPs[0,1])

        padfGeoTransform[0] = pasGCPs[0,2] $
            - pasGCPs[0,0] * padfGeoTransform[1] $
            - pasGCPs[0,1] * padfGeoTransform[2]

        padfGeoTransform[3] = pasGCPs[0,3] $
            - pasGCPs[0,0] * padfGeoTransform[4] $
            - pasGCPs[0,1] * padfGeoTransform[5]

        return,padfGeoTransform
    endif

;--------------------------------------------------------------------;
;      Special case of 4 corner coordinates of a non-rotated         ;
;      image.  The points must be in TL-TR-BR-BL order for now.      ;
;      This case helps avoid some imprecision in the general         ;
;      calcuations.                                                  ;
;--------------------------------------------------------------------;
    if  nGCPCount eq 4 $
        and pasGCPs[0,1] eq pasGCPs[1,1] $
        and pasGCPs[2,1] eq pasGCPs[3,1] $
        and pasGCPs[0,0] eq pasGCPs[3,0] $
        and pasGCPs[1,0] eq pasGCPs[2,0] $
        and pasGCPs[0,1] ne pasGCPs[2,1] $
        and pasGCPs[0,0] ne pasGCPs[1,0] $
        and pasGCPs[0,3] eq pasGCPs[1,3] $
        and pasGCPs[2,3] eq pasGCPs[3,3] $
        and pasGCPs[0,2] eq pasGCPs[3,2] $
        and pasGCPs[1,2] eq pasGCPs[2,2] $
        and pasGCPs[0,3] ne pasGCPs[2,3] $
        and pasGCPs[0,2] ne pasGCPs[1,2] then begin

	        padfGeoTransform[1] = (pasGCPs[1,2] - pasGCPs[0,2]) $
	            / (pasGCPs[1,0] - pasGCPs[0,0])
	        padfGeoTransform[2] = 0.0
	        padfGeoTransform[4] = 0.0
	        padfGeoTransform[5] = (pasGCPs[2,3] - pasGCPs[1,3]) $
	            / (pasGCPs[2,1] - pasGCPs[1,1])

	        padfGeoTransform[0] = $
	            pasGCPs[0,2] - pasGCPs[0,0] * padfGeoTransform[1]
	        padfGeoTransform[3] = $
	            pasGCPs[0,3] - pasGCPs[0,1] * padfGeoTransform[5]
	        return,padfGeoTransform
    endif

;--------------------------------------------------------------------;
;In the general case, do a least squares error approximation by      ;
;solving the equation Sum[(A - B*x + C*y - Lon)^2] = minimum		 ;
;--------------------------------------------------------------------;

;--------------------------------------------------------------------;
;Use the IDL Library Regress routine for compactness				 ;
;--------------------------------------------------------------------;
;---independent variables
	x = pasGCPs[*,0]
	y = pasGCPs[*,1]
	indep = [transpose(x),transpose(y)]
	print, indep
	;---dependent variables
	lon = pasGCPs[*,2]
	lat = pasGCPs[*,3]

	print, lon
	fitlon = regress(indep,lon, sigma=sigma, const=constlon, $
	   measure_errors=measure_errors)

	print,constlon
	print,fitlon(*)

	fitlat = regress(indep,lat, sigma=sigma, const=constlat, $
	   measure_errors=measure_errors)

	print,constlat
	print,fitlat(*)

	padfGeoTransform[0] = constlon
	padfGeoTransform[1] = fitlon[0]
	padfGeoTransform[2] = fitlon[1]
	padfGeoTransform[3] = constlat
	padfGeoTransform[4] = fitlat[0]
	padfGeoTransform[5] = fitlat[1]


	return,padfGeoTransform
end