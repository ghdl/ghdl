--  GCC back-end for ortho (identifiers)
--  Copyright (C) 2002-1014 Tristan Gingold
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

package body Ortho_Ident is
   function Get_Identifier_With_Length (Str : Address; Size : Integer)
                                       return O_Ident;
   pragma Import (C, Get_Identifier_With_Length,
                  "get_identifier_with_length_c");

   function Compare_Identifier_String
     (Id : O_Ident; Str : Address; Size : Integer)
     return Boolean;
   pragma Import (C, Compare_Identifier_String);
   pragma Warnings (Off, Compare_Identifier_String);

   function Get_Identifier (Str : String) return O_Ident is
   begin
      return Get_Identifier_With_Length (Str'Address, Str'Length);
   end Get_Identifier;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean is
   begin
      return Compare_Identifier_String (Id, Str'Address, Str'Length);
   end Is_Equal;

   function Get_String (Id : O_Ident) return String
   is
      procedure Get_Identifier_String
        (Id : O_Ident; Str_Ptr : Address; Len_Ptr : Address);
      pragma Import (C, Get_Identifier_String);

      Len : Natural;
      type Str_Acc is access String (Positive);
      Str : Str_Acc;
   begin
      Get_Identifier_String (Id, Str'Address, Len'Address);
      return Str (1 .. Len);
   end Get_String;

end Ortho_Ident;

