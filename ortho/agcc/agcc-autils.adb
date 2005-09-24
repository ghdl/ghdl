--  Ada bindings for GCC internals.
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
with Ada.Unchecked_Conversion;
with Agcc.Hconfig; use Agcc.Hconfig;
with Agcc.Machmode; use Agcc.Machmode;

package body Agcc.Autils is
   Arr_Len : constant Natural := Unsigned_64'Size / HOST_WIDE_INT'Size;
   type Arr_Conv is array (Natural range 0 .. Arr_Len - 1) of HOST_WIDE_INT;

   subtype Assert_Type is Boolean range True .. True;
   Assert_Arr_Len_Is_1_Or_2 : constant Assert_Type :=
     Arr_Len = 1 or Arr_Len = 2;
   pragma Unreferenced (Assert_Arr_Len_Is_1_Or_2);

   procedure To_Host_Wide_Int (V : Unsigned_64; L, H : out HOST_WIDE_INT) is
      function Unchecked_Conversion is new Ada.Unchecked_Conversion
        (Source => Unsigned_64, Target => Arr_Conv);
      Res : Arr_Conv;
   begin
      Res := Unchecked_Conversion (V);
      if Arr_Len = 1 then
         H := 0;
         L := Res (0);
      else
         if HOST_WORDS_BIG_ENDIAN then
            L := Res (1);
            H := Res (0);
         else
            L := Res (0);
            H := Res (1);
         end if;
      end if;
   end To_Host_Wide_Int;

   procedure To_Host_Wide_Int (V : Integer_64; L, H : out HOST_WIDE_INT) is
      function Unchecked_Conversion is new Ada.Unchecked_Conversion
        (Source => Integer_64, Target => Arr_Conv);
      Res : Arr_Conv;
   begin
      Res := Unchecked_Conversion (V);
      if Arr_Len = 1 then
         if V < 0 then
            H := -1;
         else
            H := 0;
         end if;
         L := Res (0);
      else
         if HOST_WORDS_BIG_ENDIAN then
            L := Res (1);
            H := Res (0);
         else
            L := Res (0);
            H := Res (1);
         end if;
      end if;
   end To_Host_Wide_Int;

   function To_Real_Value_Type (V : IEEE_Float_64) return REAL_VALUE_TYPE
   is
      Mant_Size : constant Natural := 60;
      Rfract : IEEE_Float_64;
      Fract : Integer_64;
      Exp : Integer;
      L, H : HOST_WIDE_INT;
      Mantisse : REAL_VALUE_TYPE;
   begin
      --  Note: this works only when REAL_ARITHMETIC is defined!!!
      Exp := IEEE_Float_64'Exponent (V);
      Rfract := IEEE_Float_64'Fraction (V);
      Rfract := IEEE_Float_64'Scaling (Rfract, Mant_Size);
      Fract := Integer_64 (Rfract);
      To_Host_Wide_Int (Fract, L, H);
      REAL_VALUE_FROM_INT (Mantisse'Address, L, H, DFmode);
      return REAL_VALUE_LDEXP (Mantisse, Exp - Mant_Size);
   end To_Real_Value_Type;
end Agcc.Autils;
