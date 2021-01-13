--  String table.
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
with System;
with Types; use Types;

package Str_Table is
   --  String8 are arrays (or strings) of Nat8 elements.  They are used to
   --  store analyzed string or bit string literals.  The elements are the
   --  position of literals, so it is possible to use them for enumerated types
   --  containing at most 256 elements (which is the case of standard.bit and
   --  std_logic_1164.std_ulogic).
   --  It is not possible to free a string8.

   --  Create a new string8; this also close the previous string8.
   --  Initial length is 0.
   function Create_String8 return String8_Id;

   --  Append a new element to the being created string8.
   procedure Append_String8 (El : Nat8);
   procedure Append_String8_Char (El : Character);
   pragma Inline (Append_String8_Char);
   procedure Append_String8_String (S : String);

   --  Resize (reduce or expand) the current string8.  When expanded, new
   --  elements are uninitialized.
   procedure Resize_String8 (Len : Nat32);

   --  Get/Set N-th element of String8 ID.  There is no bound checking.
   function Element_String8 (Id : String8_Id; N : Pos32) return Nat8;
   procedure Set_Element_String8 (Id : String8_Id; N : Pos32; Val : Nat8);

   --  Utility function: get N-th element of ID as a character.  Valid only
   --  if the elements of ID are Latin-1 codes.
   function Char_String8 (Id : String8_Id; N : Pos32) return Character;
   pragma Inline (Char_String8);

   --  Utility function: get the LEN elements as a string.
   function String_String8 (Id : String8_Id; Len : Nat32) return String;

   --  Utility function: get the address of string8 ID.  Note that as soon
   --  as a character is appended (using Append_String8) or a string8 is
   --  resized (using Resize_String8), an address previously returned is not
   --  valid anymore.
   function String8_Address (Id : String8_Id) return System.Address;

   --  Free all the memory
   procedure Finalize;

   --  Initialize the package.
   procedure Initialize;
end Str_Table;
