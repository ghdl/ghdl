--  To hexadecimal conversions.
--  Copyright (C) 2006 Tristan Gingold
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
with Interfaces; use Interfaces;

package Hex_Images is
   function Hex_Image (W : Integer_32) return String;
   function Hex_Image (W : Unsigned_32) return String;
   function Hex_Image (B : Unsigned_8) return String;
   function Hex_Image (W : Unsigned_16) return String;
   function Hex_Image (W : Unsigned_64) return String;
end Hex_Images;
