--  Ada bindings for GCC internals.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Ada.Unchecked_Conversion;
with System;

package C is
   pragma Preelaborate (C);

   --  Representation of a C String: this is an access to a bounded string.
   --  Therefore, with GNAT, such an access is a thin pointer.
   subtype Fat_C_String is String (Positive);
   type C_String is access all Fat_C_String;
   pragma Convention (C, C_String);

   --  Convert an address to a C_STRING.
   function To_C_String is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => C_String);

   --  NULL for a string.
   C_String_Null : constant C_String;

   --  Convert an Ada access string to a C_String.
   --  This simply takes the address of the first character of ACC.  This
   --  is unchecked, so be careful with the life of ACC.
   --  The last element of the string designated by ACC must be the NUL-char.
   --  This is a little bit more restrictive than being only NUL-terminated.
   function To_C_String (Acc : access String) return C_String;

   --  Return the length of a C String (ie, the number of characters before
   --  the Nul).
   function C_String_Len (Str : C_String) return Natural;

   --  An (very large) array of C String.  This is the type of ARGV.
   type C_String_Array is array (Natural) of C_String;
   pragma Convention (C, C_String_Array);

   --  A structure for a string (len and address).
   type C_Str_Len is record
      Len : Natural;
      Str : C_String;
   end record;
   pragma Convention (C_Pass_By_Copy, C_Str_Len);

   type C_Str_Len_Acc is access C_Str_Len;

   function Image (Str : C_Str_Len) return String;
private
   C_String_Null : constant C_String := null;
end C;
