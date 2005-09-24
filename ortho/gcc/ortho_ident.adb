--  Ortho implementation for GCC.
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
with Agcc; use Agcc;
with Agcc.Trees;
with C; use C;

package body Ortho_Ident is
   function Get_Identifier (Str : String) return O_Ident
   is
      Str_Nul : String := Str & Nul;
   begin
      return Get_Identifier (Str_Nul'Address);
   end Get_Identifier;

   function Get_String (Id : O_Ident) return String
   is
      use Agcc.Trees;
      Str : C_Str_Len;
   begin
      Str.Len := Get_IDENTIFIER_LENGTH (Id);
      Str.Str := To_C_String (Get_IDENTIFIER_POINTER (Id));
      return Image (Str);
   end Get_String;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean
   is
      S : C_String;
   begin
      if Get_IDENTIFIER_LENGTH (Id) /= Str'Length then
         return False;
      end if;
      S := To_C_String (Get_IDENTIFIER_POINTER (Id));
      return S.all (1 .. Str'Length) = Str;
   end Is_Equal;

end Ortho_Ident;

