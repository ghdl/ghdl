--  Global checks after analyze pass.
--  Copyright (C) 2002 - 2016 Tristan Gingold
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
with Types; use Types;
with Std_Names; use Std_Names;
with Vhdl.Sem_Specs;
with Vhdl.Std_Env;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Ieee.Vital_Timing;
with Vhdl.Ieee.Numeric;
with Vhdl.Ieee.Numeric_Std_Unsigned;
with Vhdl.Ieee.Math_Real;
with Vhdl.Ieee.Std_Logic_Unsigned;
with Vhdl.Ieee.Std_Logic_Arith;
with Vhdl.Ieee.Std_Logic_Misc;
with Flags; use Flags;

package body Vhdl.Post_Sems is
   procedure Post_Sem_Checks (Unit : Iir_Design_Unit)
   is
      Lib_Unit : constant Iir := Get_Library_Unit (Unit);
      Lib : Iir_Library_Declaration;
      Id : Name_Id;

      Value : Iir_Attribute_Value;
      Spec : Iir_Attribute_Specification;
      Attr_Decl : Iir_Attribute_Declaration;
   begin
      --  No checks on package bodies or context declaration
      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Package_Body
           | Iir_Kind_Context_Declaration
           | Iir_Kinds_Verification_Unit =>
            return;
         when others =>
            null;
      end case;

      Id := Get_Identifier (Lib_Unit);
      Lib := Get_Library (Get_Design_File (Unit));

      if Get_Identifier (Lib) = Name_Ieee then
         --  This is a unit of IEEE.
         if Get_Kind (Lib_Unit) = Iir_Kind_Package_Declaration then
            case Id is
               when Name_Std_Logic_1164 =>
                  Vhdl.Ieee.Std_Logic_1164.Extract_Declarations (Lib_Unit);
               when Name_VITAL_Timing =>
                  Vhdl.Ieee.Vital_Timing.Extract_Declarations (Lib_Unit);
               when Name_Numeric_Bit =>
                  Vhdl.Ieee.Numeric.Extract_Bit_Declarations
                    (Lib_Unit);
               when Name_Numeric_Std =>
                  Vhdl.Ieee.Numeric.Extract_Std_Declarations
                    (Lib_Unit);
               when Name_Numeric_Std_Unsigned =>
                  Vhdl.Ieee.Numeric_Std_Unsigned.Extract_Declarations
                    (Lib_Unit);
               when Name_Math_Real =>
                  Vhdl.Ieee.Math_Real.Extract_Declarations (Lib_Unit);
               when Name_Std_Logic_Unsigned =>
                  Vhdl.Ieee.Std_Logic_Unsigned.Extract_Declarations
                    (Lib_Unit, Vhdl.Ieee.Std_Logic_Unsigned.Pkg_Unsigned);
               when Name_Std_Logic_Signed =>
                  Vhdl.Ieee.Std_Logic_Unsigned.Extract_Declarations
                    (Lib_Unit, Vhdl.Ieee.Std_Logic_Unsigned.Pkg_Signed);
               when Name_Std_Logic_Arith =>
                  Vhdl.Ieee.Std_Logic_Arith.Extract_Declarations (Lib_Unit);
               when Name_Std_Logic_Misc =>
                  Vhdl.Ieee.Std_Logic_Misc.Extract_Declarations (Lib_Unit);
               when others =>
                  null;
            end case;
         end if;
      elsif Get_Identifier (Lib) = Name_Std then
         --  This is a unit of Std.
         if Get_Kind (Lib_Unit) = Iir_Kind_Package_Declaration
           and then Id = Name_Env
         then
            Vhdl.Std_Env.Extract_Declarations (Lib_Unit);
         end if;
      end if;

      --  Look for VITAL attributes.
      if Flag_Vital_Checks then
         Value := Get_Attribute_Value_Chain
           (Vhdl.Sem_Specs.Get_Attribute_Value_Chain_Parent (Lib_Unit));
         while Value /= Null_Iir loop
            Spec := Get_Attribute_Specification (Value);
            Attr_Decl := Get_Named_Entity (Get_Attribute_Designator (Spec));
            if Attr_Decl = Vhdl.Ieee.Vital_Timing.Vital_Level0_Attribute then
               Vhdl.Ieee.Vital_Timing.Check_Vital_Level0 (Unit);
            elsif Attr_Decl = Vhdl.Ieee.Vital_Timing.Vital_Level1_Attribute
            then
               Vhdl.Ieee.Vital_Timing.Check_Vital_Level1 (Unit);
            end if;

            Value := Get_Value_Chain (Value);
         end loop;
      end if;
   end Post_Sem_Checks;
end Vhdl.Post_Sems;
