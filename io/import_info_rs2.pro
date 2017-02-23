;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: import_info_terrasarx
; written by    : Tisham Dhar
; last revision : 11/2007
; Import TerraSAR-X Metadata from XML file
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
pro import_info_rs2,INPUTFILE_FIRST=inputfile0
  common rat
  common channel, channel_names, channel_selec, color_flag, palettes, pnames

  if n_elements(inputfile0) ne 0 then xml_file=inputfile0 $
  else begin

;;; GUI for file selection
     path = config.workdir
     xml_file = cw_rat_dialog_pickfile(TITLE='Open RADARSAT-2 system info file', $
                                       DIALOG_PARENT=wid.base, FILTER = 'product.xml', /MUST_EXIST, PATH=path, GET_PATH=path)
  endelse

  if file_test(xml_file,/READ) then config.workdir = path else return


  ;Read relevant XML tags
  ;RS-2 fails at valid schema !!
  oDocument = OBJ_NEW('IDLffXMLDOMDocument', FILENAME=xml_file,MSG_ERROR='msg_method',SCHEMA_CHECKING=0,/EXCLUDE_IGNORABLE_WHITESPACE)

  ;read statevectors
  oNodeList = oDocument->GetElementsByTagName('stateVector')
  veclen = oNodeList->GetLength()

  for i=0,veclen-1 do begin
  	posx = (((oNodeList->Item(i))->GetElementsByTagName('xPosition'))->Item(0))->GetFirstChild()
  	posy = (((oNodeList->Item(i))->GetElementsByTagName('yPosition'))->Item(0))->GetFirstChild()
  	posz = (((oNodeList->Item(i))->GetElementsByTagName('zPosition'))->Item(0))->GetFirstChild()
  	;Test null nodes
  	if posx ne obj_new() and posy ne obj_new() and posz ne obj_new() then begin
  		x = double(posx->GetNodeValue())
  		y = double(posy->GetNodeValue())
  		z = double(posz->GetNodeValue())
  		print,"Radius:"+string(sqrt(x^2+y^2+z^2))
  	endif else begin
  		print,'Null node'
  	endelse
  endfor

  ;rs2 seems to contain to range and azimuth resolutions but a slant
  ;to ground range transform may be we can start there.

  ;For geolocation grid get all imageTiePoint nodes
  oNodeList = oDocument->GetElementsByTagName('imageTiePoint')
  tielen = oNodeList->GetLength()

  gcp_arr = fltarr(tielen,5)

  for i=0,tielen-1 do begin
    row = (((oNodeList->Item(i))->GetElementsByTagName('line'))->Item(0))->GetFirstChild()
    col = (((oNodeList->Item(i))->GetElementsByTagName('pixel'))->Item(0))->GetFirstChild()
    lat = (((oNodeList->Item(i))->GetElementsByTagName('latitude'))->Item(0))->GetFirstChild()
    lon = (((oNodeList->Item(i))->GetElementsByTagName('longitude'))->Item(0))->GetFirstChild()
    alt = (((oNodeList->Item(i))->GetElementsByTagName('height'))->Item(0))->GetFirstChild()
    ;Test null nodes
  	if row ne obj_new() and col ne obj_new() and alt ne obj_new() then begin
  		pixel = double(col->GetNodeValue())
  	    line = double(row->GetNodeValue())
  		latitude = double(lat->GetNodeValue())
  		longitude = double(lon->GetNodeValue())
  		altitude = double(alt->GetNodeValue())
  		print,"Latitude:"+string(latitude)
  		print,"Longitude:"+string(longitude)
  		print,"Altitude:"+string(altitude)

  		gcp_arr[i,0] = pixel
  		gcp_arr[i,1] = line
  		gcp_arr[i,2] = longitude
  		gcp_arr[i,3] = latitude
  		gcp_arr[i,4] = altitude
  	endif else begin
  		print,'Null node'
  	endelse
  endfor

  ;work out geo-transform from GCP's use higher order polynom
  ;if needed.
  ;print,gcp_arr[*,0:3]
  transform = gcps2transform(gcp_arr[*,0:3])
  err=set_par('transform',transform)

end

pro msg_method,filename,line,col,err_msg
	print,filename,line,col,err_msg
end