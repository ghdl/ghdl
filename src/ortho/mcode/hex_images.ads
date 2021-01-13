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
with Interfaces; use Interfaces;

package Hex_Images is
   function Hex_Image (W : Integer_32) return String;
   function Hex_Image (W : Unsigned_32) return String;
   function Hex_Image (B : Unsigned_8) return String;
   function Hex_Image (W : Unsigned_16) return String;
   function Hex_Image (W : Unsigned_64) return String;
end Hex_Images;
