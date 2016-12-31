--  Nodes recognizer for ieee.numeric_std and ieee.numeric_bit.
--  Copyright (C) 2016 Tristan Gingold
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

package Ieee.Numeric is
   Numeric_Std_Pkg : Iir_Package_Declaration := Null_Iir;
   Numeric_Std_Unsigned_Type : Iir_Array_Type_Definition := Null_Iir;
   Numeric_Std_Signed_Type : Iir_Array_Type_Definition := Null_Iir;

   --  Extract declarations from PKG (ieee.numeric_std).
   procedure Extract_Std_Declarations (Pkg : Iir_Package_Declaration);
end Ieee.Numeric;
