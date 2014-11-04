--  Name table.
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
with System;
with Types; use Types;

-- A very simple name table.
-- This is an hash table, such as id1=id2 <=> get_string(id1)=get_string(id2).

package Name_Table is
   -- Initialize the package, ie create tables.
   procedure Initialize;

   -- Get an entry in the name table.
   -- (entries for characters are already built).
   function Get_Identifier (Char: Character) return Name_Id;
   pragma Inline (Get_Identifier);

   -- Get or create an entry in the name table.
   -- If an entry is created, its token value is tok_identifier.
   -- Note:
   -- an identifier is represented in all lower case letter,
   -- an extended identifier is represented in backslashes, double internal
   --   backslashes are simplified,
   -- a string is represented by its contents (without the quotation
   --  characters, and simplified),
   -- a bit string is represented by its raw contents (no simplification).
   function Get_Identifier (Str: String) return Name_Id;

   -- Get the string associed to a name.
   -- If the name is a character, then single quote are added.
   function Image (Id: Name_Id) return String;

   -- Get the address of the first character of ID.
   -- The string is NUL-terminated (this is done by get_identifier).
   function Get_Address (Id: Name_Id) return System.Address;

   -- Get the length of ID.
   function Get_Name_Length (Id: Name_Id) return Natural;
   pragma Inline (Get_Name_Length);

   -- Get the character associed to a name.
   function Get_Character (Id: Name_Id) return Character;
   pragma Inline (Get_Character);

   --  Return TRUE iff ID is a character.
   function Is_Character (Id: Name_Id) return Boolean;
   pragma Inline (Is_Character);

   -- Get or create an entry in the name table, use NAME_BUFFER/NAME_LENGTH.
   function Get_Identifier return Name_Id;

   --  Like GET_IDENTIFIER, but return NULL_IDENTIFIER if the identifier
   --  is not found (and do not create an entry for it).
   function Get_Identifier_No_Create return Name_Id;

   --  Set NAME_BUFFER/NAME_LENGTH with the image of ID.
   procedure Image (Id : Name_Id);

   -- Get and set the info field associated with each identifier.
   -- Used to store interpretations of the name.
   function Get_Info (Id: Name_Id) return Int32;
   pragma Inline (Get_Info);
   procedure Set_Info (Id: Name_Id; Info: Int32);
   pragma Inline (Set_Info);

   -- Return the latest name_id used.
   -- kludge, use only for debugging.
   function Last_Name_Id return Name_Id;

   -- Be sure all info fields have their default value.
   procedure Assert_No_Infos;

   -- This buffer is used by get_token to set the name.
   -- This can be seen as a copy buffer but this is necessary for two reasons:
   --  names case must be 'normalized', because VHDL is case insensitive.
   Name_Buffer : String (1 .. 1024);
   -- The length of the name string.
   Name_Length: Natural;

   --  Disp statistics.
   --  Used for debugging.
   procedure Disp_Stats;
end Name_Table;
