--  Compile flags for mcode.
--  Copyright (C) 2006 Tristan Gingold
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
package Ortho_Code.Flags is
   type Debug_Type is (Debug_None, Debug_Dwarf);

   --  Debugging information generated.
   Flag_Debug : Debug_Type := Debug_None;

   --  If set, generate a map from type to type declaration.
   --  Set with --be-debug=t
   Flag_Type_Name : Boolean := False;

   --  If set, enable optimiztions.
   Flag_Optimize : Boolean := False;

   --  If set, create basic blocks during tree building.
   Flag_Opt_BB : Boolean := False;

   --  If set, add profiling calls.
   Flag_Profile : Boolean := False;
end Ortho_Code.Flags;
