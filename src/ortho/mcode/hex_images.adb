--  To hexadecimal conversions.
--  Copyright (C) 2006 Tristan Gingold
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
with Ada.Unchecked_Conversion;

package body Hex_Images is
   type Hex_Str_Type is array (0 .. 15) of Character;
   Hexdigits : constant Hex_Str_Type := "0123456789abcdef";

   function Hex_Image (B : Unsigned_8) return String is
      Res : String (1 .. 2);
   begin
      for I in 1 .. 2 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (B, 8 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Conv is new Ada.Unchecked_Conversion
     (Source => Integer_32, Target => Unsigned_32);

   function Hex_Image (W : Unsigned_32) return String is
      Res : String (1 .. 8);
   begin
      for I in 1 .. 8 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (W, 32 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Hex_Image (W : Unsigned_64) return String is
      Res : String (1 .. 16);
   begin
      for I in 1 .. 16 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (W, 64 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Hex_Image (W : Unsigned_16) return String is
      Res : String (1 .. 4);
   begin
      for I in 1 .. 4 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (W, 16 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Hex_Image (W : Integer_32) return String is
   begin
      return Hex_Image (Conv (W));
   end Hex_Image;
end Hex_Images;
