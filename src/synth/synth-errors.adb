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

package body Synth.Errors is
   procedure Error_Msg_Netlist (Loc : Location_Type;
                                Msg : String;
                                Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Elaboration,
                  +Loc, Msg, Args);
   end Error_Msg_Netlist;

   function To_Coord (N : Node) return Source_Coord_Type is
   begin
      return +Get_Location (N);
   end To_Coord;

   procedure Error_Msg_Synth (Inst : Synth_Instance_Acc;
                              Loc : Node;
                              Msg : String;
                              Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Errorout.Elaboration,
                  To_Coord (Loc), Msg, (1 => Arg1));
      if Inst /= null and then Debug_Handler /= null then
         Debug_Handler (Inst, Loc);
      end if;
   end Error_Msg_Synth;

   procedure Error_Msg_Synth (Inst : Synth_Instance_Acc;
                              Loc : Node;
                              Msg : String;
                              Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Elaboration,
                  To_Coord (Loc), Msg, Args);
      if Inst /= null and then Debug_Handler /= null then
         Debug_Handler (Inst, Loc);
      end if;
   end Error_Msg_Synth;

   procedure Warning_Msg_Synth (Warnid : Msgid_Warnings;
                                Loc : Location_Type;
                                Msg : String;
                                Arg1 : Earg_Type) is
   begin
      Report_Msg (Warnid, Errorout.Elaboration, +Loc, Msg, (1 => Arg1));
   end Warning_Msg_Synth;

   procedure Warning_Msg_Synth (Warnid : Msgid_Warnings;
                                Loc : Location_Type;
                                Msg : String;
                                Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Warnid, Errorout.Elaboration, +Loc, Msg, Args);
   end Warning_Msg_Synth;

   procedure Warning_Msg_Synth (Loc : Location_Type;
                                Msg : String;
                                Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Warning, Errorout.Elaboration, +Loc, Msg, Args);
   end Warning_Msg_Synth;

   procedure Info_Msg_Synth (Loc : Location_Type;
                             Msg : String;
                             Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Note, Errorout.Elaboration, +Loc, Msg, Args);
   end Info_Msg_Synth;

end Synth.Errors;
