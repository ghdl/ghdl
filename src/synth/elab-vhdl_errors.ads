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

with Errorout; use Errorout;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Vhdl.Nodes; use Vhdl.Nodes;

package Elab.Vhdl_Errors is
   procedure Error_Msg_Elab (Syn_Inst : Synth_Instance_Acc;
                             Loc : Node;
                             Msg : String;
                             Arg1 : Earg_Type);
   procedure Error_Msg_Elab (Syn_Inst : Synth_Instance_Acc;
                             Loc : Node;
                             Msg : String;
                             Args : Earg_Arr := No_Eargs);

   --  procedure Warning_Msg_Synth (Loc : Location_Type;
   --                               Msg : String;
   --                               Arg1 : Earg_Type);
   --  procedure Warning_Msg_Synth (Loc : Location_Type;
   --                               Msg : String;
   --                               Args : Earg_Arr := No_Eargs);
   --  procedure Info_Msg_Synth (Loc : Location_Type;
   --                            Msg : String;
   --                            Args : Earg_Arr := No_Eargs);

   type Handler_Access is access procedure (Inst : Synth_Instance_Acc;
                                            Loc : Node);
   Debug_Handler : Handler_Access;

   Elaboration_Error : exception;
end Elab.Vhdl_Errors;
