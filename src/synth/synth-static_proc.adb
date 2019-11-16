--  Predefined procedures
--  Copyright (C) 2019 Tristan Gingold
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

with Vhdl.Errors; use Vhdl.Errors;

with Synth.Errors; use Synth.Errors;
with Synth.Files_Operations; use Synth.Files_Operations;

package body Synth.Static_Proc is

   procedure Synth_Static_Procedure (Syn_Inst : Synth_Instance_Acc;
                                     Imp : Node;
                                     Loc : Node) is
   begin
      case Get_Implicit_Definition (Imp) is
         when Iir_Predefined_Foreign_Untruncated_Text_Read =>
            Synth_Untruncated_Text_Read (Syn_Inst, Imp, Loc);
         when others =>
            Error_Msg_Synth
              (+Loc, "call to implicit %n is not supported", +Imp);
      end case;
   end Synth_Static_Procedure;
end Synth.Static_Proc;
