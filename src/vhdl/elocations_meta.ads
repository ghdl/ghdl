--  Meta description of elocations
--  Copyright (C) 2017 Tristan Gingold
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
with Iirs; use Iirs;

package Elocations_Meta is
   --  The enumeration of all fields defined in iirs.
   type Fields_Enum is
     (
      Field_Start_Location,
      Field_Right_Paren_Location,
      Field_End_Location,
      Field_Is_Location,
      Field_Begin_Location,
      Field_Then_Location,
      Field_Loop_Location,
      Field_Generate_Location,
      Field_Generic_Location,
      Field_Port_Location,
      Field_Generic_Map_Location,
      Field_Port_Map_Location,
      Field_Arrow_Location,
      Field_Colon_Location,
      Field_Assign_Location
     );
   pragma Discard_Names (Fields_Enum);

   --  Get the name of a field.
   function Get_Field_Image (F : Fields_Enum) return String;


   --  Get/Set a field.
   function Get_Location_Type
      (N : Iir; F : Fields_Enum) return Location_Type;
   procedure Set_Location_Type
      (N : Iir; F : Fields_Enum; V: Location_Type);

   function Has_Start_Location (K : Iir_Kind) return Boolean;
   function Has_Right_Paren_Location (K : Iir_Kind) return Boolean;
   function Has_End_Location (K : Iir_Kind) return Boolean;
   function Has_Is_Location (K : Iir_Kind) return Boolean;
   function Has_Begin_Location (K : Iir_Kind) return Boolean;
   function Has_Then_Location (K : Iir_Kind) return Boolean;
   function Has_Loop_Location (K : Iir_Kind) return Boolean;
   function Has_Generate_Location (K : Iir_Kind) return Boolean;
   function Has_Generic_Location (K : Iir_Kind) return Boolean;
   function Has_Port_Location (K : Iir_Kind) return Boolean;
   function Has_Generic_Map_Location (K : Iir_Kind) return Boolean;
   function Has_Port_Map_Location (K : Iir_Kind) return Boolean;
   function Has_Arrow_Location (K : Iir_Kind) return Boolean;
   function Has_Colon_Location (K : Iir_Kind) return Boolean;
   function Has_Assign_Location (K : Iir_Kind) return Boolean;
end Elocations_Meta;
