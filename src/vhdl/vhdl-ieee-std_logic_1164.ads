--  Nodes recognizer for ieee.std_logic_1164.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

package Vhdl.Ieee.Std_Logic_1164 is
   --  Nodes corresponding to declarations in the package.
   Std_Logic_1164_Pkg : Iir_Package_Declaration := Null_Iir;
   Std_Ulogic_Type : Iir_Enumeration_Type_Definition := Null_Iir;
   Std_Ulogic_Vector_Type : Iir_Array_Type_Definition := Null_Iir;
   Std_Ulogic_0 : Iir_Enumeration_Literal := Null_Iir;
   Std_Ulogic_1 : Iir_Enumeration_Literal := Null_Iir;
   Std_Logic_Type : Iir_Enumeration_Subtype_Definition := Null_Iir;
   Std_Logic_Vector_Type : Iir_Array_Type_Definition := Null_Iir;
   Resolved : Iir_Function_Declaration := Null_Iir;

   --  Position of literals (D represents '-' ie dont-care).
   Std_Logic_U_Pos : constant := 0;
   Std_Logic_X_Pos : constant := 1;
   Std_Logic_0_Pos : constant := 2;
   Std_Logic_1_Pos : constant := 3;
   Std_Logic_Z_Pos : constant := 4;
   Std_Logic_W_Pos : constant := 5;
   Std_Logic_L_Pos : constant := 6;
   Std_Logic_H_Pos : constant := 7;
   Std_Logic_D_Pos : constant := 8;

   --  Extract declarations from PKG.
   --  PKG is the package declaration for ieee.std_logic_1164 package.
   --  Fills the node aboves.
   procedure Extract_Declarations (Pkg : Iir_Package_Declaration);
end Vhdl.Ieee.Std_Logic_1164;
