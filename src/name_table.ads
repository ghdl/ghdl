--  Name table.
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
with System;
with Types; use Types;

--  A very simple name table. This is an hash table, so that
--  id1 = id2 <=> get_string (id1) = get_string (id2).

--  Note: for VHDL, extended names are represented as they appear in the
--  sources: with a leading and trailing backslash; internal backslashes are
--  doubled.

package Name_Table is
   --  Get an entry in the name table for a character.
   --  (entries for characters are already built).  Characters are put in the
   --  name table, but are always different from identifiers.  They simply
   --  share the same Name_Id type.
   function Get_Identifier (Char: Character) return Name_Id;
   pragma Inline (Get_Identifier);

   --  Get or create an entry in the name table.  Note:
   --  * an identifier is represented in all lower case letter,
   --  * an extended identifier is represented in backslashes, double internal
   --    backslashes are simplified.
   function Get_Identifier (Str: String) return Name_Id;

   --  Likewise, but with a C compatible interface.
   function Get_Identifier_With_Len (Str : Thin_String_Ptr; Len : Natural)
                                    return Name_Id;

   --  Get the string associed to a name.  The first bound is 1.
   --  If the name is a character, then single quote are added.
   function Image (Id: Name_Id) return String;

   --  Get the address of the first character of ID.  The address is valid
   --  until the next call to Get_Identifier (which may reallocate the string
   --  table).
   --  The string is NUL-terminated (this is done by get_identifier).
   function Get_Address (Id: Name_Id) return System.Address;
   function Get_Name_Ptr (Id : Name_Id) return Thin_String_Ptr;

   --  Get the length of ID.
   function Get_Name_Length (Id: Name_Id) return Natural;
   pragma Inline (Get_Name_Length);

   --  Get the character associed to a name.  This is valid only for character
   --  ids.
   function Get_Character (Id: Name_Id) return Character;
   pragma Inline (Get_Character);

   --  Return TRUE iff ID is a character.
   function Is_Character (Id: Name_Id) return Boolean;
   pragma Inline (Is_Character);

   --  Like GET_IDENTIFIER, but return NULL_IDENTIFIER if the identifier
   --  is not found (and do not create an entry for it).
   function Get_Identifier_No_Create (Str : String) return Name_Id;
   function Get_Identifier_No_Create_With_Len
     (Str : Thin_String_Ptr; Len : Natural) return Name_Id;

   --  Get and set the info field associated with each identifier.
   --  Used to store interpretations of the name.
   function Get_Name_Info (Id : Name_Id) return Int32;
   pragma Inline (Get_Name_Info);
   procedure Set_Name_Info (Id : Name_Id; Info: Int32);
   pragma Inline (Set_Name_Info);

   --  Return the latest name_id used.  This is only for debugging or stats.
   function Last_Name_Id return Name_Id;

   --  Initialize this package
   --  This must be called once and only once before any use.
   procedure Initialize;

   --  Free all resources.  The package cannot be used anymore after calling
   --  this procedure.
   procedure Finalize;

   --  Disp statistics.
   --  Used for debugging.
   procedure Disp_Stats;
end Name_Table;
