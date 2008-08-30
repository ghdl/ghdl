--  GHDL Run Time (GRT) -  'image subprograms.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with System; use System;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Ada.Unchecked_Conversion;
with Grt.Processes; use Grt.Processes;
with Grt.Vstrings; use Grt.Vstrings;

package body Grt.Images is
   function To_Std_String_Basep is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Std_String_Basep);

   function To_Std_String_Boundp is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Std_String_Boundp);

   procedure Return_String (Res : Std_String_Ptr; Str : String)
   is
   begin
      Res.Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Str'Length));
      Res.Bounds := To_Std_String_Boundp
        (Ghdl_Stack2_Allocate (Std_String_Bound'Size / System.Storage_Unit));
      for I in 0 .. Str'Length - 1 loop
         Res.Base (Ghdl_Index_Type (I)) := Str (Str'First + I);
      end loop;
      Res.Bounds.Dim_1 := (Left => 1,
                           Right => Str'Length,
                           Dir => Dir_To,
                           Length => Str'Length);
   end Return_String;

   procedure Return_Enum
     (Res : Std_String_Ptr; Rti : Ghdl_Rti_Access; Index : Ghdl_Index_Type)
   is
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;
      Str : Ghdl_C_String;
   begin
      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Str := Enum_Rti.Names (Index);
      Return_String (Res, Str (1 .. strlen (Str)));
   end Return_Enum;

   procedure Ghdl_Image_B2
     (Res : Std_String_Ptr; Val : Ghdl_B2; Rti : Ghdl_Rti_Access)
   is
   begin
      Return_Enum (Res, Rti, Ghdl_B2'Pos (Val));
   end Ghdl_Image_B2;

   procedure Ghdl_Image_E8
     (Res : Std_String_Ptr; Val : Ghdl_E8; Rti : Ghdl_Rti_Access)
   is
   begin
      Return_Enum (Res, Rti, Ghdl_E8'Pos (Val));
   end Ghdl_Image_E8;

   procedure Ghdl_Image_E32
     (Res : Std_String_Ptr; Val : Ghdl_E32; Rti : Ghdl_Rti_Access)
   is
   begin
      Return_Enum (Res, Rti, Ghdl_E32'Pos (Val));
   end Ghdl_Image_E32;

   procedure Ghdl_Image_I32 (Res : Std_String_Ptr; Val : Ghdl_I32)
   is
      Str : String (1 .. 11);
      First : Natural;
   begin
      To_String (Str, First, Val);
      Return_String (Res, Str (First .. Str'Last));
   end Ghdl_Image_I32;

   procedure Ghdl_Image_P64
     (Res : Std_String_Ptr; Val : Ghdl_I64; Rti : Ghdl_Rti_Access)
   is
      Str : String (1 .. 21);
      First : Natural;
      Unit : Ghdl_C_String;
      Phys : Ghdl_Rtin_Type_Physical_Acc;
      Unit_Len : Natural;
   begin
      To_String (Str, First, Val);
      Phys := To_Ghdl_Rtin_Type_Physical_Acc (Rti);
      Unit := To_Ghdl_Rtin_Unit_Acc (Phys.Units (0)).Name;
      Unit_Len := strlen (Unit);
      declare
         L : constant Natural := Str'Last + 1 - First;
         Str2 : String (1 .. L + 1 + Unit_Len);
      begin
         Str2 (1 .. L) := Str (First .. Str'Last);
         Str2 (L + 1) := ' ';
         Str2 (L + 2 .. Str2'Last) := Unit (1 .. Unit_Len);
         Return_String (Res, Str2);
      end;
   end Ghdl_Image_P64;

   procedure Ghdl_Image_P32
     (Res : Std_String_Ptr; Val : Ghdl_I32; Rti : Ghdl_Rti_Access)
   is
      Str : String (1 .. 11);
      First : Natural;
      Unit : Ghdl_C_String;
      Phys : Ghdl_Rtin_Type_Physical_Acc;
      Unit_Len : Natural;
   begin
      To_String (Str, First, Val);
      Phys := To_Ghdl_Rtin_Type_Physical_Acc (Rti);
      Unit := To_Ghdl_Rtin_Unit_Acc (Phys.Units (0)).Name;
      Unit_Len := strlen (Unit);
      declare
         L : constant Natural := Str'Last + 1 - First;
         Str2 : String (1 .. L + 1 + Unit_Len);
      begin
         Str2 (1 .. L) := Str (First .. Str'Last);
         Str2 (L + 1) := ' ';
         Str2 (L + 2 .. Str2'Last) := Unit (1 .. Unit_Len);
         Return_String (Res, Str2);
      end;
   end Ghdl_Image_P32;

   procedure Ghdl_Image_F64 (Res : Std_String_Ptr; Val : Ghdl_F64)
   is
      function Trunc (V : Ghdl_F64) return Ghdl_F64;
      pragma Import (C, Trunc);

      --  Sign (1) + digit (1) + dot (1) + digits (15) + exp (1) + sign (1)
      --  + exp_digits (4) -> 24.
      Str : String (1 .. 25);
      P : Natural;
      V : Ghdl_F64;
      Vmax : Ghdl_F64;
      Vd : Ghdl_F64;
      Exp : Integer;
      D : Integer;
      B : Boolean;
   begin
      --  Handle sign.
      if Val < 0.0 then
         Str (1) := '-';
         P := 1;
         V := -Val;
      else
         P := 0;
         V := Val;
      end if;

      --  Compute the mantissa.
      --  and normalize V in [0 .. 10.0[
      --  FIXME: should do a dichotomy.
      if V  = 0.0 then
         Exp := 0;
      elsif V < 1.0 then
         Exp := 0;
         loop
            exit when V >= 1.0;
            Exp := Exp - 1;
            V := V * 10.0;
         end loop;
      else
         Exp := 0;
         loop
            exit when V < 10.0;
            Exp := Exp + 1;
            V := V / 10.0;
         end loop;
      end if;

      Vmax := 10.0 ** (1 - 15);
      for I in 0 .. 15 loop
         --  Vd := Ghdl_F64'Truncation (V);
         Vd := Trunc (V);
         P := P + 1;
         Str (P) := Character'Val (48 + Integer (Vd));
         V := (V - Vd) * 10.0;

         if I = 0 then
            P := P + 1;
            Str (P) := '.';
         end if;
         exit when I > 0 and V < Vmax;
         Vmax := Vmax * 10.0;
      end loop;

      if Exp /= 0 then
         --  LRM93 14.3
         --  if the exponent is present, the `e' is written as a lower case
         --  character.
         P := P + 1;
         Str (P) := 'e';

         if Exp < 0 then
            P := P + 1;
            Str (P) := '-';
            Exp := -Exp;
         end if;
         B := False;
         for I in 0 .. 4 loop
            D := (Exp / 10000) mod 10;
            if D /= 0 or B or I = 4 then
               P := P + 1;
               Str (P) := Character'Val (48 + D);
               B := True;
            end if;
            Exp := (Exp - D * 10000) * 10;
         end loop;
      end if;

      Return_String (Res, Str (1 .. P));
   end Ghdl_Image_F64;

--     procedure Ghdl_Image_F64 (Res : Std_String_Ptr; Val : Ghdl_F64)
--     is
--        --  Sign (1) + digit (1) + dot (1) + digits (15) + exp (1) + sign (1)
--        --  + exp_digits (4) -> 24.
--        Str : String (1 .. 25);

--        procedure Snprintf_G (Str : System.Address;
--                              Size : Integer;
--                              Arg : Ghdl_F64);
--        pragma Import (C, Snprintf_G, "__ghdl_snprintf_g");

--        function strlen (Str : System.Address) return Integer;
--        pragma Import (C, strlen);
--     begin
--        Snprintf_G (Str'Address, Str'Length, Val);
--        Return_String (Res, Str (1 .. strlen (Str'Address)));
--     end Ghdl_Image_F64;

end Grt.Images;
