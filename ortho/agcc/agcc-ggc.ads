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
with Agcc.Trees; use Agcc.Trees;

package Agcc.Ggc is
   procedure Ggc_Add_Root (Base : System.Address;
                           Nelt : Natural;
                           Size : Natural;
                           Func : System.Address);

   procedure Ggc_Add_Tree_Root (Base : System.Address; Nelt : Natural);

   procedure Ggc_Mark_Tree (Expr : Tree);
private
   pragma Import (C, Ggc_Add_Root);
   pragma Import (C, Ggc_Mark_Tree);
   pragma Import (C, Ggc_Add_Tree_Root);
end Agcc.Ggc;
