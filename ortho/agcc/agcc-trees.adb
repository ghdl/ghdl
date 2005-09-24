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
package body Agcc.Trees is
   function Build_Int (Low : HOST_WIDE_INT) return Tree is
   begin
      if Low < 0 then
         return Build_Int_2_Wide (Low, -1);
      else
         return Build_Int_2_Wide (Low, 0);
      end if;
   end Build_Int;

   procedure Expand_Start_Bindings (Flags : Integer) is
   begin
      Expand_Start_Bindings_And_Block (Flags, NULL_TREE);
   end Expand_Start_Bindings;

end Agcc.Trees;
