--  Global checks after semantization pass.
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
with Types; use Types;
with Std_Names; use Std_Names;
with Ieee.Std_Logic_1164;
with Ieee.Vital_Timing;
with Flags; use Flags;

package body Post_Sems is
   procedure Post_Sem_Checks (Unit : Iir_Design_Unit)
   is
      Lib_Unit : Iir;
      Lib : Iir_Library_Declaration;
      Id : Name_Id;

      Value : Iir_Attribute_Value;
      Spec : Iir_Attribute_Specification;
      Attr_Decl : Iir_Attribute_Declaration;
   begin
      Lib_Unit := Get_Library_Unit (Unit);
      Id := Get_Identifier (Lib_Unit);
      Lib := Get_Library (Get_Design_File (Unit));

      if Get_Identifier (Lib) = Name_Ieee then
         --  This is a unit of IEEE.
         if Get_Kind (Lib_Unit) = Iir_Kind_Package_Declaration then
            if Id = Name_Std_Logic_1164 then
               Ieee.Std_Logic_1164.Extract_Declarations (Lib_Unit);
            elsif Id = Name_VITAL_Timing then
               Ieee.Vital_Timing.Extract_Declarations (Lib_Unit);
            end if;
         end if;
      end if;

      --  Look for VITAL attributes.
      if Flag_Vital_Checks then
         Value := Get_Attribute_Value_Chain (Unit);
         while Value /= Null_Iir loop
            Spec := Get_Attribute_Specification (Value);
            Attr_Decl := Get_Attribute_Designator (Spec);
            if Attr_Decl = Ieee.Vital_Timing.Vital_Level0_Attribute then
               Ieee.Vital_Timing.Check_Vital_Level0 (Unit);
            elsif Attr_Decl = Ieee.Vital_Timing.Vital_Level1_Attribute then
               Ieee.Vital_Timing.Check_Vital_Level1 (Unit);
            end if;

            Value := Get_Chain (Value);
         end loop;
      end if;
   end Post_Sem_Checks;
end Post_Sems;

