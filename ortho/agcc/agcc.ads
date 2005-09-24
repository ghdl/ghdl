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
with System;
with Interfaces.C;

package Agcc is
   pragma Pure (Agcc);

   subtype Chars is System.Address;
   NULL_Chars : Chars renames System.Null_Address;

   Nul : constant Character := Character'Val (0);

   --  Names size_t.
   type Size_T is new Interfaces.C.size_t;

   --  Ada representation of boolean type in C.
   --  Never compare with C_TRUE, since in C any value different from 0 is
   --  considered as true.
   type C_Bool is new Integer;
   pragma Convention (C, C_Bool);

   subtype C_Boolean is C_Bool range 0 .. 1;

   C_False : constant C_Bool := 0;
   C_True : constant C_Bool := 1;

   function "+" (B : C_Bool) return Boolean;
   pragma Inline ("+");
end Agcc;
