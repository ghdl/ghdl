--  std.standard package declarations.
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
with Iirs; use Iirs;

package Std_Package is

   --  This is a special node, not really declared in the STANDARD package,
   --  used to mark a node as erroneous.
   --  Its kind is Iir_Kind_Error.
   Error_Mark : constant Iir;

   -- Some well know values declared in the STANDARD package.
   -- These values (except time_base) *must* not be modified, and are set by
   -- create_std_standard_package.
   -- Time_base is the base unit of time.  It is set during the creation of
   -- all these nodes, and can be modified only *immediatly* after.

   Time_Base: Iir_Unit_Declaration := Null_Iir;

   Std_Standard_File: Iir_Design_File := Null_Iir;
   Std_Standard_Unit : Iir_Design_Unit := Null_Iir;
   Standard_Package : Iir_Package_Declaration := Null_Iir;

   -- Boolean values.
   Boolean_Type: Iir_Type_Declaration := Null_Iir;
   Boolean_Type_Definition: Iir_Enumeration_Type_Definition;
   Boolean_False: Iir_Enumeration_Literal;
   Boolean_True: Iir_Enumeration_Literal;

   -- Bit values.
   Bit_Type: Iir_Type_Declaration := Null_Iir;
   Bit_Type_Definition: Iir_Enumeration_Type_Definition;
   Bit_0: Iir_Enumeration_Literal;
   Bit_1: Iir_Enumeration_Literal;

   -- Predefined character.
   Character_Type: Iir_Type_Declaration;
   Character_Type_Definition : Iir_Enumeration_Type_Definition;

   -- severity level.
   Severity_Level_Type : Iir_Type_Declaration;
   Severity_Level_Type_Definition : Iir_Enumeration_Type_Definition;
   Severity_Level_Note : Iir_Enumeration_Literal;
   Severity_Level_Warning : Iir_Enumeration_Literal;
   Severity_Level_Error : Iir_Enumeration_Literal;
   Severity_Level_Failure : Iir_Enumeration_Literal;

   -- Universal types.
   Universal_Integer_Type : Iir_Anonymous_Type_Declaration;
   Universal_Integer_Type_Definition : constant Iir_Integer_Type_Definition;
   Universal_Integer_Subtype : Iir_Subtype_Declaration;
   Universal_Integer_Subtype_Definition : Iir_Integer_Subtype_Definition;

   Universal_Integer_One : Iir_Integer_Literal;

   Universal_Real_Type : Iir_Anonymous_Type_Declaration;
   Universal_Real_Type_Definition : constant Iir_Floating_Type_Definition;
   Universal_Real_Subtype : Iir_Subtype_Declaration;
   Universal_Real_Subtype_Definition : Iir_Floating_Subtype_Definition;

   -- Predefined integer type.
   Integer_Type: Iir_Anonymous_Type_Declaration;
   Integer_Type_Definition : Iir_Integer_Type_Definition;
   Integer_Subtype : Iir_Subtype_Declaration;
   Integer_Subtype_Definition : Iir_Integer_Subtype_Definition;

   --  Type used when a subtype indication cannot be semantized.
   --  FIXME: To be improved.
   Error_Type : Iir_Integer_Type_Definition renames Integer_Type_Definition;

   -- Predefined real type.
   Real_Type: Iir_Anonymous_Type_Declaration;
   Real_Type_Definition : Iir_Floating_Type_Definition;
   Real_Subtype : Iir_Subtype_Declaration;
   Real_Subtype_Definition : Iir_Floating_Subtype_Definition;

   -- Predefined natural subtype.
   Natural_Subtype: Iir_Subtype_Declaration;
   Natural_Subtype_Definition : Iir_Integer_Subtype_Definition;

   -- Predefined positive subtype.
   Positive_Subtype: Iir_Subtype_Declaration;
   Positive_Subtype_Definition : Iir_Integer_Subtype_Definition;

   -- Predefined positive subtype.
   String_Type: Iir_Type_Declaration;
   String_Type_Definition : Iir_Array_Type_Definition;

   -- Predefined positive subtype.
   Bit_Vector_Type: Iir_Type_Declaration;
   Bit_Vector_Type_Definition : Iir_Array_Type_Definition;

   -- predefined time subtype
   Time_Type: Iir_Anonymous_Type_Declaration;
   Time_Type_Definition: Iir_Physical_Type_Definition;
   Time_Subtype_Definition: Iir_Physical_Subtype_Definition;
   Time_Subtype : Iir_Subtype_Declaration;

   --  For VHDL-93
   Delay_Length_Subtype_Definition : Iir_Physical_Subtype_Definition;
   Delay_Length_Subtype : Iir_Subtype_Declaration;

   --  For VHDL-93:
   --  type File_Open_Kind
   File_Open_Kind_Type : Iir_Type_Declaration;
   File_Open_Kind_Type_Definition : Iir_Enumeration_Type_Definition;
   File_Open_Kind_Read_Mode : Iir_Enumeration_Literal;
   File_Open_Kind_Write_Mode : Iir_Enumeration_Literal;
   File_Open_Kind_Append_Mode : Iir_Enumeration_Literal;

   --  For VHDL-93:
   --  type File_Open_Status
   File_Open_Status_Type : Iir_Type_Declaration;
   File_Open_Status_Type_Definition : Iir_Enumeration_Type_Definition;
   File_Open_Status_Open_Ok : Iir_Enumeration_Literal;
   File_Open_Status_Status_Error : Iir_Enumeration_Literal;
   File_Open_Status_Name_Error : Iir_Enumeration_Literal;
   File_Open_Status_Mode_Error : Iir_Enumeration_Literal;

   --  For VHDL-93:
   --    atribute foreign : string;
   Foreign_Attribute : Iir_Attribute_Declaration;

   --  Internal use only.
   --  These types should be considered like universal types, but
   --  furthermore, they can be converted to any integer/real types while
   --  universal cannot.
   Convertible_Integer_Type_Definition : constant Iir_Integer_Type_Definition;
   Convertible_Real_Type_Definition : constant Iir_Floating_Type_Definition;
   Convertible_Integer_Type : Iir_Anonymous_Type_Declaration;
   Convertible_Real_Type : Iir_Anonymous_Type_Declaration;

   Convertible_Integer_Subtype_Definition : Iir_Integer_Subtype_Definition;
   Convertible_Integer_Subtype : Iir_Subtype_Declaration;

   --  Create the first well-known nodes.
   procedure Create_First_Nodes;

   --  Create the node for the standard package.
   procedure Create_Std_Standard_Package (Parent : Iir_Library_Declaration);

private
   --  For speed reasons, some often used nodes are hard-coded.
   Error_Mark : constant Iir := 2;
   Universal_Integer_Type_Definition : constant Iir_Integer_Type_Definition
     := 3;
   Universal_Real_Type_Definition : constant Iir_Floating_Type_Definition
     := 4;

   Convertible_Integer_Type_Definition : constant Iir_Integer_Type_Definition
     := 5;
   Convertible_Real_Type_Definition : constant Iir_Floating_Type_Definition
     := 6;
end Std_Package;
