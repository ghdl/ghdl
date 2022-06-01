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

with Interfaces;

with Types; use Types;

with Vhdl.Errors; use Vhdl.Errors;

with Elab.Memtype;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Heap;
with Elab.Vhdl_Files; use Elab.Vhdl_Files;

with Synth.Errors; use Synth.Errors;

with Grt.Fcvt;

package body Synth.Vhdl_Static_Proc is

   procedure Synth_Deallocate (Syn_Inst : Synth_Instance_Acc; Imp : Node)
   is
      Inter : constant Node := Get_Interface_Declaration_Chain (Imp);
      Param : constant Valtyp := Get_Value (Syn_Inst, Inter);
      Val : Heap_Index;
   begin
      if not Is_Static (Param.Val) then
         --  Certainly an error (and certainly already reported).
         return;
      end if;
      Val := Read_Access (Param);
      if Val /= Null_Heap_Index then
         Elab.Vhdl_Heap.Synth_Deallocate (Val);
         Write_Access (Param.Val.Mem, Null_Heap_Index);
      end if;
   end Synth_Deallocate;

   procedure Synth_Textio_Write_Real (Syn_Inst : Synth_Instance_Acc;
                                      Imp : Node)
   is
      use Elab.Memtype;
      Param1 : constant Node := Get_Interface_Declaration_Chain (Imp);
      Str : constant Valtyp := Get_Value (Syn_Inst, Param1);
      Param2 : constant Node := Get_Chain (Param1);
      Len : constant Valtyp := Get_Value (Syn_Inst, Param2);
      Param3 : constant Node := Get_Chain (Param2);
      Val : constant Valtyp := Get_Value (Syn_Inst, Param3);
      Param4 : constant Node := Get_Chain (Param3);
      Ndigits : constant Valtyp := Get_Value (Syn_Inst, Param4);

      S : String (1 .. Natural (Str.Typ.Abound.Len));
      Last : Natural;
   begin
      Grt.Fcvt.Format_Digits (S, Last,
                              Interfaces.IEEE_Float_64 (Read_Fp64 (Val)),
                              Natural (Read_Discrete (Ndigits)));
      Write_Discrete (Len, Int64 (Last));
      for I in 1 .. Last loop
         Write_U8 (Str.Val.Mem + Size_Type (I - 1), Character'Pos (S (I)));
      end loop;
   end Synth_Textio_Write_Real;

   procedure Synth_Static_Procedure (Syn_Inst : Synth_Instance_Acc;
                                     Imp : Node;
                                     Loc : Node) is
   begin
      case Get_Implicit_Definition (Imp) is
         when Iir_Predefined_File_Open =>
            Synth_File_Open (Syn_Inst, Imp, Loc);
         when Iir_Predefined_File_Open_Status =>
            Synth_File_Open_Status (Syn_Inst, Imp);
         when Iir_Predefined_File_Close =>
            Synth_File_Close (Syn_Inst, Imp, Loc);
         when Iir_Predefined_Foreign_Untruncated_Text_Read =>
            Synth_Untruncated_Text_Read (Syn_Inst, Imp, Loc);
         when Iir_Predefined_Deallocate =>
            Synth_Deallocate (Syn_Inst, Imp);
         when Iir_Predefined_Read =>
            Synth_File_Read (Syn_Inst, Imp, Loc);
         when Iir_Predefined_Write =>
            Synth_File_Write (Syn_Inst, Imp, Loc);
         when Iir_Predefined_Flush =>
            Synth_File_Flush (Syn_Inst, Imp, Loc);
         when Iir_Predefined_Std_Env_Finish_Status =>
            if Hook_Finish /= null then
               Hook_Finish.all (Syn_Inst, Imp);
            else
               raise Internal_Error;
            end if;
         when Iir_Predefined_Foreign_Textio_Write_Real =>
            Synth_Textio_Write_Real (Syn_Inst, Imp);
         when others =>
            Error_Msg_Synth
              (+Loc, "call to implicit %n is not supported", +Imp);
      end case;
   end Synth_Static_Procedure;
end Synth.Vhdl_Static_Proc;
