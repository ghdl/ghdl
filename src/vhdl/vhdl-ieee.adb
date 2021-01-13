--  Nodes recognizer for ieee packages - utilities.
--  Copyright (C) 2016 Tristan Gingold
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

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;

package body Vhdl.Ieee is
   function Skip_Copyright_Notice (Decl : Iir) return Iir is
   begin
      if Decl /= Null_Iir
        and then Get_Kind (Decl) = Iir_Kind_Constant_Declaration
        and then (Get_Base_Type (Get_Type (Decl))
                    = Vhdl.Std_Package.String_Type_Definition)
      then
         return Get_Chain (Decl);
      else
         return Decl;
      end if;
   end Skip_Copyright_Notice;

   function Skip_Implicit (Decl : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Decl;
      loop
         exit when Res = Null_Iir;
         exit when not (Get_Kind (Res) = Iir_Kind_Function_Declaration
                          and then Is_Implicit_Subprogram (Res));
         Res := Get_Chain (Res);
      end loop;
      return Res;
   end Skip_Implicit;
end Vhdl.Ieee;
