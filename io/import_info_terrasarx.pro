;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; RAT Module: import_info_terrasarx
; written by    : Tisham Dhar(UofA)
; last revision : 08/01/2009
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
pro import_info_terrasarx,INPUTFILE_FIRST=inputfile0
  common rat
  common channel, channel_names, channel_selec, color_flag, palettes, pnames

  if n_elements(inputfile0) ne 0 then xml_file=inputfile0 $
  else begin

;;; GUI for file selection
     path = config.workdir
     xml_file = cw_rat_dialog_pickfile(TITLE='Open TERRASAR-X system info file', $
                                       DIALOG_PARENT=wid.base, FILTER = '*.xml', /MUST_EXIST, PATH=path, GET_PATH=path)
  endelse

  if file_test(xml_file,/READ) then config.workdir = path else return


  ;Read relevant XML tags
  oDocument = OBJ_NEW('IDLffXMLDOMDocument', FILENAME=xml_file,/EXCLUDE_IGNORABLE_WHITESPACE)

  ;read statevectors
  oNodeList = oDocument->GetElementsByTagName('stateVec')
  veclen = oNodeList->GetLength()

  for i=0,veclen-1 do begin
  	posx = (((oNodeList->Item(i))->GetElementsByTagName('posX'))->Item(0))->GetFirstChild()
  	posy = (((oNodeList->Item(i))->GetElementsByTagName('posY'))->Item(0))->GetFirstChild()
  	posz = (((oNodeList->Item(i))->GetElementsByTagName('posZ'))->Item(0))->GetFirstChild()
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

  ;create scene x-y lat-lon transform based on 5-points
  ;use polynomial if available


  gcp_arr = fltarr(5,5)

  ;read corner coordinates
  oNodeList = oDocument->GetElementsByTagName('sceneCornerCoord')
  cornlen = oNodeList->GetLength()

  for i=0,cornlen-1 do begin
    row = (((oNodeList->Item(i))->GetElementsByTagName('refRow'))->Item(0))->GetFirstChild()
    col = (((oNodeList->Item(i))->GetElementsByTagName('refColumn'))->Item(0))->GetFirstChild()
    lat = (((oNodeList->Item(i))->GetElementsByTagName('lat'))->Item(0))->GetFirstChild()
    lon = (((oNodeList->Item(i))->GetElementsByTagName('lon'))->Item(0))->GetFirstChild()
    ang = (((oNodeList->Item(i))->GetElementsByTagName('incidenceAngle'))->Item(0))->GetFirstChild()
    ;Test null nodes
  	if row ne obj_new() and col ne obj_new() and ang ne obj_new() then begin
  	    pixel = double(col->GetNodeValue())
  	    line = double(row->GetNodeValue())
  		latitude = double(lat->GetNodeValue())
  		longitude = double(lon->GetNodeValue())
  		incidenceAngle = double(ang->GetNodeValue())
  		print,"Latitude:"+string(latitude)
  		print,"Longitude:"+string(longitude)
  		print,"IncidenceAngle:"+string(incidenceAngle)


  		gcp_arr[i,0] = pixel
  		gcp_arr[i,1] = line
  		gcp_arr[i,2] = longitude
  		gcp_arr[i,3] = latitude
  		gcp_arr[i,4] = incidenceAngle
  	endif else begin
  		print,'Null node'
  	endelse
  endfor


  ;read center coordinates
  oNodeList = ((oDocument->GetElementsByTagName("sceneInfo"))->Item(0))->GetElementsByTagName('sceneCenterCoord')
  cenlen = oNodeList->GetLength()

  for i=0,cenlen-1 do begin
    row = (((oNodeList->Item(i))->GetElementsByTagName('refRow'))->Item(0))->GetFirstChild()
    col = (((oNodeList->Item(i))->GetElementsByTagName('refColumn'))->Item(0))->GetFirstChild()
    lat = (((oNodeList->Item(i))->GetElementsByTagName('lat'))->Item(0))->GetFirstChild()
    lon = (((oNodeList->Item(i))->GetElementsByTagName('lon'))->Item(0))->GetFirstChild()
    ang = (((oNodeList->Item(i))->GetElementsByTagName('incidenceAngle'))->Item(0))->GetFirstChild()
    ;Test null nodes
  	if row ne obj_new() and col ne obj_new() and ang ne obj_new() then begin
  		pixel = double(col->GetNodeValue())
  	    line = double(row->GetNodeValue())
  		latitude = double(lat->GetNodeValue())
  		longitude = double(lon->GetNodeValue())
  		incidenceAngle = double(ang->GetNodeValue())
  		print,"Latitude:"+string(latitude)
  		print,"Longitude:"+string(longitude)
  		print,"IncidenceAngle:"+string(incidenceAngle)

  		gcp_arr[4,0] = pixel
  		gcp_arr[4,1] = line
  		gcp_arr[4,2] = longitude
  		gcp_arr[4,3] = latitude
  		gcp_arr[4,4] = incidenceAngle
  	endif else begin
  		print,'Null node'
  	endelse
  endfor


  ;read scene average height over the ellipsoid
  oNodeList = oDocument->GetElementsByTagName('sceneAverageHeight')
  if oNodeList->GetLength() gt 0 then begin
  	averageheight = double(((oNodeList->Item(0))->GetFirstChild())->GetNodeValue())
  	print,"SceneAverageHeight:"+string(averageheight)
  endif

  ;work out geo-transform from GCP's use higher order polynom
  ;if needed.
  ;print,gcp_arr[*,0:3]
  transform = gcps2transform(gcp_arr[*,0:3])
  err=set_par('transform',transform)

  ;read slant and ground range + azimuth resolutions
  oNodeList = oDocument->GetElementsByTagName('slantRangeResolution')
  if oNodeList->GetLength() gt 0 then begin
  	sr_res = double(((oNodeList->Item(0))->GetFirstChild())->GetNodeValue())
  	print,"Slant Range Resolution:"+string(sr_res)
  	err=set_par('res_sr',sr_res)
  endif

  oNodeList = oDocument->GetElementsByTagName('groundRangeResolution')
  if oNodeList->GetLength() gt 0 then begin
  	gr_res = double(((oNodeList->Item(0))->GetFirstChild())->GetNodeValue())
  	print,"Ground Range Resolution:"+string(gr_res)
  	err=set_par('res_gr',gr_res)
  endif

  oNodeList = oDocument->GetElementsByTagName('azimuthResolution')
  if oNodeList->GetLength() gt 0 then begin
  	az_res = double(((oNodeList->Item(0))->GetFirstChild())->GetNodeValue())
  	print,"Azimuth Resolution:"+string(gr_res)
  	err=set_par('res_az',az_res)
  endif

end