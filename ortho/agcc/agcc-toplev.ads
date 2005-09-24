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
with System;
with Agcc.Hwint; use Agcc.Hwint;

package Agcc.Toplev is
   procedure Rest_Of_Decl_Compilation (Decl : Tree;
                                       Asmspec : System.Address;
                                       Top_Level : C_Bool;
                                       At_End : C_Bool);
   procedure Rest_Of_Type_Compilation (Decl : Tree; Toplevel : C_Bool);
   procedure Rest_Of_Compilation (Decl : Tree);

   function Exact_Log2_Wide (X : HOST_WIDE_INT) return Integer;
   function Floor_Log2_Wide (X : HOST_WIDE_INT) return Integer;

   procedure Error (Msg : System.Address);

   procedure Announce_Function (Func : Tree);

   function Toplev_Main (Argc : Integer; Argv : System.Address)
     return Integer;
private
   pragma Import (C, Rest_Of_Decl_Compilation);
   pragma Import (C, Rest_Of_Type_Compilation);
   pragma Import (C, Rest_Of_Compilation);

   pragma Import (C, Exact_Log2_Wide);
   pragma Import (C, Floor_Log2_Wide);

   pragma Import (C, Error);

   pragma Import (C, Announce_Function);
   pragma Import (C, Toplev_Main);
end Agcc.Toplev;
