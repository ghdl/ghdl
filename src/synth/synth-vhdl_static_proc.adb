--  Predefined procedures
--  Copyright (C) 2019 Tristan Gingold
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

with Vhdl.Errors; use Vhdl.Errors;

with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Heap;
with Elab.Vhdl_Files; use Elab.Vhdl_Files;

with Synth.Errors; use Synth.Errors;

package body Synth.Vhdl_Static_Proc is

   procedure Synth_Deallocate (Syn_Inst : Synth_Instance_Acc; Imp : Node)
   is
      Inter : constant Node := Get_Interface_Declaration_Chain (Imp);
      Param : constant Valtyp := Get_Value (Syn_Inst, Inter);
      Val : Heap_Index;
   begin
      Val := Read_Access (Param);
      if Val /= Null_Heap_Index then
         Elab.Vhdl_Heap.Synth_Deallocate (Val);
         Write_Access (Param.Val.Mem, Null_Heap_Index);
      end if;
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
         when Iir_Predefined_Read =>
            Synth_File_Read (Syn_Inst, Imp, Loc);
         when others =>
            Error_Msg_Synth
              (+Loc, "call to implicit %n is not supported", +Imp);
      end case;
   end Synth_Static_Procedure;
end Synth.Vhdl_Static_Proc;
