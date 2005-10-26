--  Ortho debug hashed identifiers implementation.
--  Copyright (C) 2005 Tristan Gingold
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

package body Ortho_Ident_Hash is
   type O_Ident_Array is array (Hash_Type range <>) of O_Ident;
   Hash_Max : constant Hash_Type := 511;
   Symtable : O_Ident_Array (0 .. Hash_Max - 1) := (others => null);

   function Get_Identifier (Str : String) return O_Ident
   is
      Hash : Hash_Type;
      Ent : Hash_Type;
      Res : O_Ident;
   begin
      --  1.  Compute Hash.
      Hash := 0;
      for I in Str'Range loop
         Hash := Hash * 31 + Character'Pos (Str (I));
      end loop;

      --  2.  Search.
      Ent := Hash mod Hash_Max;
      Res := Symtable (Ent);
      while Res /= null loop
         if Res.Hash = Hash and then Res.Ident.all = Str then
            return Res;
         end if;
         Res := Res.Next;
      end loop;

      --  Not found: add.
      Res := new Ident_Type'(Hash => Hash,
                             Ident => new String'(Str),
                             Next => Symtable (Ent));
      Symtable (Ent) := Res;
      return Res;
   end Get_Identifier;

   function Get_String (Id : O_Ident) return String is
   begin
      if Id = null then
         return "?ANON?";
      else
         return Id.Ident.all;
      end if;
   end Get_String;

   function Is_Nul (Id : O_Ident) return Boolean is
   begin
      return Id = null;
   end Is_Nul;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean is
   begin
      return Id.Ident.all = Str;
   end Is_Equal;
end Ortho_Ident_Hash;
