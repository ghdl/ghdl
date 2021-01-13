--  Nodes recognizer for ieee.math_real.
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

package Vhdl.Ieee.Math_Real is
   Math_Real_Pkg : Iir_Package_Declaration := Null_Iir;

   --  Extract declarations from PKG (ieee.math_real).
   procedure Extract_Declarations (Pkg : Iir_Package_Declaration);
end Vhdl.Ieee.Math_Real;
