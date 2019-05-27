--  EDIF utils.
--  Copyright (C) 2019 Tristan Gingold
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

package body Edif.Nutils is
   procedure Init_Constr (Constr : out Constr_Type) is
   begin
      Constr := (Null_Node, Null_Node);
   end Init_Constr;

   procedure Append_Node (Constr : in out Constr_Type; N : Node) is
   begin
      if Constr.First = Null_Node then
         Constr.First := N;
      else
         Set_Chain (Constr.Last, N);
      end if;
      Constr.Last := N;
   end Append_Node;

   function Get_Constr_Chain (Constr : Constr_Type) return Node is
   begin
      return Constr.First;
   end Get_Constr_Chain;
end Edif.Nutils;
