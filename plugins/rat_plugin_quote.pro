;------------------------------------------------------------------------
; RAT - Radar Tools
;------------------------------------------------------------------------
; External RAT Module: rat_plugin_quote
; last modified by : Maxim Neumann
; last revision    : 27.09.2007
; Random RAT quote plugin.
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


; Comments about writting a Plugin for RAT (stand: 27.09.07):
; The module MUST contain:
;   PLUGIN_INFO keyword, which should give back a structure with the
;      name of the plugin, as it should apper in the menu, and an
;      array of menu positions, denoting in which sub-menues it should
;      be located (menu_pos can be:
;      {"file","general","sar","polsar","insar","polinsar","help"}).
;   HELP keyword, which displays some kind of info on the plugin, like 
;   authors, functioning, license, ...
;
; furthermore, it is recommended to include for input:
;   CALLED keyword: at least as a phantom (for the case this
;       function is called from another function, i.e. one does not
;       need to generate a preview image at the end)
;   NO_GUI keyword: in case the procedure is called from the bash
;      (perhaps we should name it different?)
;   FUNC_PARAMETERS keyword: a structure, which should contain all
;      necessary parameters for proper module usage, in case no
;      interactions with the user are possible, or desired (see
;      bash-calls). The definition of this structure is module
;      dependent.
;   
; what happens next:
;   - the name of the file should be "rat_plugin_{MYNAME}.sav" or
;     "rat_plugin_{MYNAME}.pro}". The procedure name in this case
;     should be "rat_plugin_{MYNAME}".
;   - the file should be put either in the rat/plugins/
;     directory of the rat sources, or in the $HOME/.rat/plugins
;     install directory
;   - in case the names of the file and the procedure are ok, as well as
;     the location of the file, RAT will be able to find it. Next, it
;     will check for the PLUGIN_INFO structure, and add calls to this
;     procedure to the given submenu positions
;     ('menu_pos->External plugins'). Additionally, independent of
;     'menu_pos', all plugins can be called from the menu
;     'general->All plugins'.



pro rat_plugin_quote, CALLED=called, NO_GUI=no_gui, FUNC_PARAMETERS=pars, $
                      PLUGIN_INFO=plugin_info, HELP=HELP
  common rat, types, file, wid, config

  if arg_present(plugin_info) then begin
     plugin_info={menu_name:"Random RAT Quote",menu_pos:["general"]}
     return
  endif
  if keyword_set(HELP) then begin
     ret=dialog_message("Fun plugin.",dialog_parent=wid.base, $
                        /INFO,TITLE="Help on the 'Randon RAT Quote' plugin")
     return
  endif

  mess=["An old RAT is a brave RAT.", $
        "The trouble with the RAT-race is that even if you win, you're still a RAT.", $
        "High School: the mouse race to prepare you for the RAT race.", $
        "You're a mouse studying to be a RAT."]

  r = fix(randomu(seed)*n_elements(mess)) 

  if keyword_set(NO_GUI) then $
     message,mess[r],/INFO $
  else ret=dialog_message(mess[r],dialog_parent=wid.base,/INFO,TITLE="Random Rat Quote")

end
