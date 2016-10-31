--  LLVM back-end for ortho.
--  Copyright (C) 2014 Tristan Gingold
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

package Ortho_Ident is
   type O_Ident is private;

   function Get_Identifier (Str : String) return O_Ident;
   function Is_Equal (L, R : O_Ident) return Boolean;
   function Is_Equal (Id : O_Ident; Str : String) return Boolean;
   function Is_Nul (Id : O_Ident) return Boolean;
   function Get_String (Id : O_Ident) return String;
   function Get_String_Length (Id : O_Ident) return Natural;

   --  Note: the address is always valid.
   function Get_Cstring (Id : O_Ident) return System.Address;

   O_Ident_Nul : constant O_Ident;

private
   type O_Ident is record
      Addr : System.Address;
   end record;
   O_Ident_Nul : constant O_Ident := (Addr => System.Null_Address);

   pragma Inline (Get_Cstring);
end Ortho_Ident;
