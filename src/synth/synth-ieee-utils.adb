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

   procedure Abs_Vec (Src : Memory_Ptr; Dst : Memory_Ptr; Len : Uns32) is
   begin
      if Len > 0 and then Sl_To_X01 (Read_Std_Logic (Src, 0)) = '1' then
         Neg_Vec (Src, Dst, Len);
      else
         for I in 1 .. Size_Type (Len) loop
            Write_U8 (Dst + (I - 1), Read_U8 (Src + (I - 1)));
         end loop;
      end if;
   end Abs_Vec;

   procedure Fill (Res : Memory_Ptr; Len : Uns32; V : Std_Ulogic) is
   begin
      for I in 1 .. Len loop
         Write_Std_Logic (Res, I - 1, V);
      end loop;
   end Fill;

end Synth.Ieee.Utils;
