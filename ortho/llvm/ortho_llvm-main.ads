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

with GNAT.Directory_Operations;
with LLVM.Target; use LLVM.Target;
with LLVM.TargetMachine; use LLVM.TargetMachine;

package Ortho_LLVM.Main is
   use LLVM.Core;

   --  LLVM specific: the module.
   Module : ModuleRef;

   --  Descriptor for the layout.
   Target_Data : TargetDataRef;

   Target_Machine : TargetMachineRef;

   --  Optimization level
   Optimization : CodeGenOptLevel := CodeGenLevelDefault;

   --  Set by -g to generate debug info.
   Flag_Debug : Boolean := False;

   Debug_ID : unsigned;

   --  Some predefined types and functions.
   Char_Type : O_Tnode;
   Char_Ptr_Type : O_Tnode;

   Stacksave_Fun : ValueRef;
   Stacksave_Name : constant String := "llvm.stacksave" & ASCII.NUL;
   Stackrestore_Fun : ValueRef;
   Stackrestore_Name : constant String := "llvm.stackrestore" & ASCII.NUL;

   Current_Directory : constant String :=
     GNAT.Directory_Operations.Get_Current_Dir;

   function To_String (C : Cstring) return String;

   procedure Init;
end Ortho_LLVM.Main;
