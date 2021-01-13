--  Ortho JIT specifications.
--  Copyright (C) 2009 Tristan Gingold
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

with System; use System;
with Ortho_Nodes; use Ortho_Nodes;

package Ortho_Jit is
   --  Initialize the whole engine.
   procedure Init;

   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address);
   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address;

   --  Do link.
   procedure Link (Status : out Boolean);

   --  Release memory (but the generated code).
   procedure Finish;

   function Decode_Option (Option : String) return Boolean;
   procedure Disp_Help;

   --  Return the name of the code generator, to be displayed by --version.
   function Get_Jit_Name return String;

   --  Symbolizer: convert PC to a filename (a NUL terminated string), line
   --  number and subprogram name (NUL terminated).
   --  Unresolved values are Null_Address and 0.
   procedure Symbolize (Pc : Address;
                        Filename : out Address;
                        Lineno : out Natural;
                        Subprg : out Address);
end Ortho_Jit;
