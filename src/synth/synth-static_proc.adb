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

with Synth.Values; use Synth.Values;
with Synth.Errors; use Synth.Errors;
with Synth.Files_Operations; use Synth.Files_Operations;
with Synth.Heap;

package body Synth.Static_Proc is

   procedure Synth_Deallocate (Syn_Inst : Synth_Instance_Acc; Imp : Node)
   is
      Inter : constant Node := Get_Interface_Declaration_Chain (Imp);
      Param : constant Valtyp := Get_Value (Syn_Inst, Inter);
   begin
      Synth.Heap.Synth_Deallocate (Read_Access (Param));
      Write_Access (Param.Val.Mem, Null_Heap_Index);
   end Synth_Deallocate;

   procedure Synth_Static_Procedure (Syn_Inst : Synth_Instance_Acc;
                                     Imp : Node;
                                     Loc : Node) is
   begin
      case Get_Implicit_Definition (Imp) is
         when Iir_Predefined_File_Open =>
            Synth_File_Open (Syn_Inst, Imp, Loc);
         when Iir_Predefined_File_Close =>
            Synth_File_Close (Syn_Inst, Imp, Loc);
         when Iir_Predefined_Foreign_Untruncated_Text_Read =>
            Synth_Untruncated_Text_Read (Syn_Inst, Imp, Loc);
         when Iir_Predefined_Deallocate =>
            Synth_Deallocate (Syn_Inst, Imp);
         when others =>
            Error_Msg_Synth
              (+Loc, "call to implicit %n is not supported", +Imp);
      end case;
   end Synth_Static_Procedure;
end Synth.Static_Proc;
