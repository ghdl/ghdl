--  Utils for common types.
--  Copyright (C) 2019 Tristan Gingold
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

package body Types_Utils is
   function Sext (V : Uns64; Sz : Natural) return Uns64
   is
      Sh : constant Natural range 0 .. 64 := 64 - Sz;
   begin
      return Shift_Right_Arithmetic (Shift_Left (V, Sh), Sh);
   end Sext;

   function Sext (V : Uns32; Sz : Natural) return Uns32
   is
      Sh : constant Natural range 0 .. 32 := 32 - Sz;
   begin
      return Shift_Right_Arithmetic (Shift_Left (V, Sh), Sh);
   end Sext;
end Types_Utils;
