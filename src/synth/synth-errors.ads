--  Error handling for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Errorout; use Errorout;

package Synth.Errors is
   procedure Error_Msg_Synth (Loc : Location_Type;
                              Msg : String;
                              Arg1 : Earg_Type);
   procedure Error_Msg_Synth (Loc : Location_Type;
                              Msg : String;
                              Args : Earg_Arr := No_Eargs);
   procedure Warning_Msg_Synth (Warnid : Msgid_Warnings;
                                Loc : Location_Type;
                                Msg : String;
                                Arg1 : Earg_Type);
   procedure Warning_Msg_Synth (Loc : Location_Type;
                                Msg : String;
                                Args : Earg_Arr := No_Eargs);
   procedure Info_Msg_Synth (Loc : Location_Type;
                             Msg : String;
                             Args : Earg_Arr := No_Eargs);
end Synth.Errors;
