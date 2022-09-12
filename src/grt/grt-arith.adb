--  GHDL Run Time (GRT) -  support for exp
--  Copyright (C) 2022 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

package body Grt.Arith is
   function Hi (V : Ghdl_U64) return Ghdl_U32 is
   begin
      return Ghdl_U32 (Shift_Right (V, 32) and 16#ffff_ffff#);
   end Hi;

   pragma Inline (Hi);

   function Lo (V : Ghdl_U64) return Ghdl_U32 is
   begin
      return Ghdl_U32 (V and 16#ffff_ffff#);
   end Lo;

   pragma Inline (Lo);

   function Hi (V : Ghdl_I64) return Ghdl_U32 is
   begin
      return Hi (To_Ghdl_U64 (V));
   end Hi;

   function Lo (V : Ghdl_I64) return Ghdl_U32 is
   begin
      return Lo (To_Ghdl_U64 (V));
   end Lo;

   procedure Mul_I32_Ovf (L, R : Ghdl_I32;
                          Res : out Ghdl_I32;
                          Ovf : out Boolean)
   is
      T : Ghdl_I64;
   begin
      T := Ghdl_I64 (L) * Ghdl_I64 (R);
      if Hi (T) /= Shift_Right_Arithmetic (Lo (T), 31) then
         Ovf := True;
      else
         Ovf := False;
         Res := Ghdl_I32 (T);
      end if;
   end Mul_I32_Ovf;

   procedure Mul_U64_Ovf (L, R : Ghdl_U64;
                          Res : out Ghdl_U64;
                          Ovf : out Boolean)
   is
      Ll : constant Ghdl_U32 := Lo (L);
      Lh : constant Ghdl_U32 := Hi (L);
      Rl : constant Ghdl_U32 := Lo (R);
      Rh : constant Ghdl_U32 := Hi (R);
      --  Result is:
      --            Ll * Rl
      --       Lh * Rl
      --       Ll * Rh
      --  Lh * Rh
      Vll, Vhl, Vhh : Ghdl_U64;
   begin
      Vhh := Ghdl_U64 (Lh) * Ghdl_U64 (Rh);
      if Vhh /= 0 then
         Ovf := True;
         return;
      end if;

      --  Note: no overflow in the addition because either Rh = 0 or Lh = 0.
      Vhl := Ghdl_U64 (Lh) * Ghdl_U64 (Rl) + Ghdl_U64 (Ll) * Ghdl_U64 (Rh);

      Vll := Ghdl_U64 (Ll) * Ghdl_U64 (Rl);

      Vhl := Vhl + Ghdl_U64 (Hi (Vll));

      if Hi (Vhl) /= 0 then
         Ovf := True;
      else
         Ovf := False;
         Res := Shift_Left (Vhl, 32) or Ghdl_U64 (Lo (Vll));
      end if;
   end Mul_U64_Ovf;

   procedure Exp_I64 (V : Ghdl_I64;
                      E : Std_Integer;
                      Res : out Ghdl_I64;
                      Ovf : out Boolean)
   is
      R : Std_Integer;
      P : Ghdl_U64;
      Ures : Ghdl_U64;
   begin
      if E < 0 then
         Ovf := True;
         return;
      elsif E = 1 then
         Res := V;
         Ovf := False;
         return;
      end if;

      P := To_Ghdl_U64 (V);
      if V < 0 then
         --  Avoid overflow.
         P := (not P) + 1;
      end if;

      Ures := 1;
      R := E;
      loop
         if R mod 2 = 1 then
            Mul_U64_Ovf (Ures, P, Ures, Ovf);
            if Ovf then
               return;
            end if;
         end if;
         R := R / 2;
         exit when R = 0;
         Mul_U64_Ovf (P, P, P, Ovf);
         if Ovf then
            return;
         end if;
      end loop;

      if V < 0 and (E mod 2) = 1 then
         --  Need to negate the result.
         if Shift_Right (Ures, 63) = 1 then
            if Shift_Left (Ures, 1) = 0 then
               Res := To_Ghdl_I64 (Ures);
               Ovf := False;
            else
               Ovf := True;
            end if;
            return;
         end if;
         Res := To_Ghdl_I64 ((not Ures) + 1);
      else
         if Shift_Right (Ures, 63) = 1 then
            Ovf := True;
            return;
         end if;
         Ovf := False;
         Res := To_Ghdl_I64 (Ures);
      end if;
      Ovf := False;
   end Exp_I64;

   procedure Exp_I32 (V : Ghdl_I32;
                      E : Std_Integer;
                      Res : out Ghdl_I32;
                      Ovf : out Boolean)
   is
      R : Std_Integer;
      P : Ghdl_I32;
   begin
      if E < 0 then
         Ovf := True;
         return;
      end if;

      Res := 1;
      P := V;
      R := E;
      loop
         if R mod 2 = 1 then
            Mul_I32_Ovf (Res, P, Res, Ovf);
            if Ovf then
               return;
            end if;
         end if;
         R := R / 2;
         exit when R = 0;
         Mul_I32_Ovf (P, P, P, Ovf);
         if Ovf then
            return;
         end if;
      end loop;
      Ovf := False;
   end Exp_I32;
end Grt.Arith;
