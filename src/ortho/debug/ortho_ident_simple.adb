--  Ortho debug identifiers simple implementation.
--  Copyright (C) 2005 Tristan Gingold
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

package body Ortho_Ident_Simple is
   function Get_Identifier (Str : String) return O_Ident
   is
   begin
      return new String'(Str);
   end Get_Identifier;

   function Get_String (Id : O_Ident) return String is
   begin
      if Id = null then
         return "?ANON?";
      else
         return Id.all;
      end if;
   end Get_String;

   function Is_Nul (Id : O_Ident) return Boolean is
   begin
      return Id = null;
   end Is_Nul;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean is
   begin
      return Id.all = Str;
   end Is_Equal;
end Ortho_Ident_Simple;
