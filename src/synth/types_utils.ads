--  Utils for common types.
--  Copyright (C) 2019 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Ada.Unchecked_Conversion;

with Types; use Types;

package Types_Utils is
   function To_Int32 is new Ada.Unchecked_Conversion
     (Uns32, Int32);

   function To_Uns32 is new Ada.Unchecked_Conversion
     (Int32, Uns32);

   function To_Uns64 is new Ada.Unchecked_Conversion
     (Int64, Uns64);

   function To_Int64 is new Ada.Unchecked_Conversion
     (Uns64, Int64);

   function To_Uns64 is new Ada.Unchecked_Conversion
     (Fp64, Uns64);
end Types_Utils;
