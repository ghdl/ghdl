--  Simple logic utilities for ieee.std_logic
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

with Synth.Vhdl_Stmts;
with Grt.Severity;

package body Synth.Ieee.Utils is
   procedure Neg_Vec (Src : Memory_Ptr; Dst : Memory_Ptr; Len : Uns32)
   is
      Vb, Carry : Sl_X01;
   begin
      Carry := '1';
      for I in 1 .. Len loop
         Vb := Sl_To_X01 (Read_Std_Logic (Src, Len - I));
         Vb := Not_Table (Vb);
         Write_Std_Logic (Dst, Len - I, Xor_Table (Carry, Vb));
         Carry := And_Table (Carry, Vb);
      end loop;
   end Neg_Vec;

   function Create_Res_Type (Otyp : Type_Acc; Len : Uns32) return Type_Acc is
   begin
      if Otyp.Abound.Len = Len
        and then Otyp.Abound.Right = 0
        and then Otyp.Abound.Dir = Dir_Downto
        and then not Otyp.Is_Global
      then
         --  Try to reuse the same type as the parameter.
         --  But the result type must be allocated on the expr_pool.
         --  FIXME: is this code ever executed ?
         pragma Assert (Otyp.Abound.Left = Int32 (Len) - 1);
         return Otyp;
      end if;
      return Create_Vec_Type_By_Length (Len, Otyp.Arr_El);
   end Create_Res_Type;

   function Null_Res (Arr_Typ : Type_Acc) return Memtyp
   is
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (Arr_Typ, 0);
      Res := Create_Memory (Res.Typ);
      return Res;
   end Null_Res;

   function Has_X (V : Memtyp) return Boolean is
   begin
      for I in 1 .. V.Typ.Abound.Len loop
         if Sl_To_X01 (Read_Std_Logic (V.Mem, I - 1)) = 'X' then
            return True;
         end if;
      end loop;
      return False;
   end Has_X;

   procedure Report_Division_By_Zero (Inst : Synth_Instance_Acc;
                                      Loc : Node;
                                      Msg : String)
   is
      use Synth.Vhdl_Stmts;
      use Grt.Severity;
      Str : String_Acc;
   begin
      Str := new String'(Msg & ": division by 0");
      Report_Assertion_Failure (Inst, Loc, Error_Severity, Str);
   end Report_Division_By_Zero;
end Synth.Ieee.Utils;
