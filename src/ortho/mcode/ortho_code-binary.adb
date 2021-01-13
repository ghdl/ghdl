--  Interface with binary writer for mcode.
--  Copyright (C) 2006 Tristan Gingold
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
with Ortho_Code.Decls;
with Ortho_Code.Exprs;

package body Ortho_Code.Binary is
   function Get_Decl_Symbol (Decl : O_Dnode) return Symbol
   is
   begin
      return To_Symbol (Decls.Get_Decl_Info (Decl));
   end Get_Decl_Symbol;

   function Get_Label_Symbol (Label : O_Enode) return Symbol is
   begin
      return To_Symbol (Exprs.Get_Label_Info (Label));
   end Get_Label_Symbol;

   procedure Set_Label_Symbol (Label : O_Enode; Sym : Symbol) is
   begin
      Exprs.Set_Label_Info (Label, To_Int32 (Sym));
   end Set_Label_Symbol;
end Ortho_Code.Binary;
