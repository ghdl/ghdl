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

   procedure Mul_Vec (L, R : Memory_Ptr;
                      Llen, Rlen : Uns32;
                      L_Sign, R_Sign : Boolean;
                      Res : Memory_Ptr)
   is
      Res_Len : constant Uns32 :=
        Llen + Rlen + Boolean'Pos (L_Sign xor R_Sign);
      Lb, Rb, Vb, Carry : Sl_X01;
   begin
      --  Check for 'X' in L.
      for I in 1 .. Llen loop
         if Read_Std_Logic (L, I - 1) = 'X' then
            Fill (Res, Res_Len, 'X');
            return;
         end if;
      end loop;

      --  Init RES.
      Fill (Res, Res_Len, '0');

      if Rlen = 0 then
         return;
      end if;

      --  Shift and add L.
      for I in 1 .. Rlen - Boolean'Pos (R_Sign) loop
         Rb := Sl_To_X01 (Read_Std_Logic (R, Rlen - I));
         if Rb = '1' then
            --  Compute res := res + shift_left (l, i).
            Carry := '0';
            for J in 1 .. Llen loop
               Lb := Read_Std_Logic (L, Llen - J);
               Vb := Read_Std_Logic (Res, Res_Len - (I + J - 1));
               Write_Std_Logic
                 (Res, Res_Len - (I + J - 1), Compute_Sum (Carry, Vb, Lb));
               Carry := Compute_Carry (Carry, Vb, Lb);
            end loop;
            --  Propagate carry.
            if L_Sign then
               --  Sign extend.
               Lb := Read_Std_Logic (L, 0);
            else
               Lb := '0';
            end if;
            for J in I + Llen .. Res_Len loop
               exit when Lb = '0' and Carry = '0';
               Vb := Read_Std_Logic (Res, Res_Len - J);
               Write_Std_Logic (Res, Res_Len - J, Compute_Sum (Carry, Vb, Lb));
               Carry := Compute_Carry (Carry, Vb, Lb);
            end loop;
         elsif Rb = 'X' then
            Fill (Res, Res_Len, 'X');
            exit;
         end if;
      end loop;
      if R_Sign and then Read_Std_Logic (R, 0) = '1' then
         --  R is a negative number.  It is considered as:
         --   -2**n + (Rn-1 Rn-2 ... R0).
         --  Compute res := res - 2**n * l.
         Carry := '1';
         for I in 1 .. Llen loop
            --  Start at len - (rlen - 1) = llen + 1
            Vb := Read_Std_Logic (Res, Llen - I + 1);
            Lb := Not_Table (Read_Std_Logic (L, Llen - I));
            Write_Std_Logic (Res, Llen - I + 1, Compute_Sum (Carry, Vb, Lb));
            Carry := Compute_Carry (Carry, Vb, Lb);
         end loop;
         --  The last bit.
         Vb := Read_Std_Logic (Res, 0);
         Lb := Not_Table (Read_Std_Logic (L, 0));
         Write_Std_Logic (Res, 0, Compute_Sum (Carry, Vb, Lb));
      end if;
   end Mul_Vec;

   function Compare_Bit (Lb, Rb : Sl_01;
                         L_Sign, R_Sign : Boolean) return Order_Type is
   begin
      if Lb = '1' and Rb = '0' then
         if L_Sign then
            return Less;
         else
            return Greater;
         end if;
      elsif Lb = '0' and Rb = '1' then
         if R_Sign then
            return Greater;
         else
            return Less;
         end if;
      else
         return Equal;
      end if;
   end Compare_Bit;

   function Compare_Vec (L, R : Memory_Ptr;
                         Llen, Rlen : Uns32;
                         L_Sign, R_Sign : Boolean) return Order_Type
   is
      Lb, Rb : Sl_01;
   begin
      --  The sign.
      if L_Sign and Llen > 0 then
         Lb := Sl_To_01 (Read_Std_Logic (L, 0));
      else
         Lb := '0';
      end if;
      if R_Sign and Rlen > 0 then
         Rb := Sl_To_01 (Read_Std_Logic (R, 0));
      else
         Rb := '0';
      end if;
      if Lb /= Rb then
         return Compare_Bit (Lb, Rb, L_Sign, R_Sign);
      end if;

      --  Same sign.
      for I in reverse 1 .. Uns32'Max (Llen, Rlen) loop
         if I <= Llen then
            Lb := Sl_To_01 (Read_Std_Logic (L, Llen - I));
         end if;
         if I <= Rlen then
            Rb := Sl_To_01 (Read_Std_Logic (R, Rlen - I));
         end if;
         if Lb = '0' and Rb = '1' then
            return Less;
         elsif Lb = '1' and Rb = '0' then
            return Greater;
         end if;
      end loop;
      return Equal;
   end Compare_Vec;

end Synth.Ieee.Utils;
