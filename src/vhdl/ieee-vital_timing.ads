--  Nodes recognizer for ieee.vital_timing.
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

package Ieee.Vital_Timing is
   --  Attribute declarations.
   Vital_Level0_Attribute : Iir_Attribute_Declaration := Null_Iir;
   Vital_Level1_Attribute : Iir_Attribute_Declaration := Null_Iir;

   --  Vital delay types.
   VitalDelayType : Iir := Null_Iir;
   VitalDelayType01   : Iir_Array_Type_Definition := Null_Iir;
   VitalDelayType01Z  : Iir_Array_Type_Definition := Null_Iir;
   VitalDelayType01ZX : Iir_Array_Type_Definition := Null_Iir;

   VitalDelayArrayType     : Iir_Array_Type_Definition := Null_Iir;
   VitalDelayArrayType01   : Iir_Array_Type_Definition := Null_Iir;
   VitalDelayArrayType01Z  : Iir_Array_Type_Definition := Null_Iir;
   VitalDelayArrayType01ZX : Iir_Array_Type_Definition := Null_Iir;

   --  Extract declarations from IEEE.VITAL_Timing package.
   procedure Extract_Declarations (Pkg : Iir_Package_Declaration);

   procedure Check_Vital_Level0 (Unit : Iir_Design_Unit);
   procedure Check_Vital_Level1 (Unit : Iir_Design_Unit);
end Ieee.Vital_Timing;
