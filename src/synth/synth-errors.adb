--  Error handling for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

package body Synth.Errors is
   procedure Error_Msg_Synth (Loc : Location_Type;
                              Msg : String;
                              Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Errorout.Elaboration,
                  Loc, Msg, (1 => Arg1));
   end Error_Msg_Synth;

   procedure Error_Msg_Synth (Loc : Location_Type;
                              Msg : String) is
   begin
      Report_Msg (Msgid_Error, Errorout.Elaboration,
                  Loc, Msg, (1 .. 0 => <>));
   end Error_Msg_Synth;
end Synth.Errors;
