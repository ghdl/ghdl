--  LLVM back-end for ortho.
--  Copyright (C) 2014 Tristan Gingold
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

--  with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;

with Ortho_LLVM; use Ortho_LLVM;
with Ortho_LLVM.Jit;

with LLVM.Core; use LLVM.Core;
with LLVM.Target; use LLVM.Target;
--  with LLVM.TargetMachine; use LLVM.TargetMachine;
with LLVM.ExecutionEngine; use LLVM.ExecutionEngine;
with LLVM.Analysis;
--  with Interfaces;
with Interfaces.C; use Interfaces.C;

package body Ortho_Jit is
   --  Snap_Filename : GNAT.OS_Lib.String_Access := null;

   Flag_Dump_Llvm : Boolean := False;

   --  Name of the module.
   Module_Name : String := "ortho" & Ascii.Nul;

   --  procedure DisableLazyCompilation (EE : ExecutionEngineRef;
   --                                    Disable : int);
   --  pragma Import (C, DisableLazyCompilation,
   --        "LLVMDisableLazyCompilation");

   --  Initialize the whole engine.
   procedure Init
   is
      Msg : aliased Cstring;
   begin
      InitializeNativeTarget;
      InitializeNativeAsmPrinter;

      LinkInJIT;

      Module := ModuleCreateWithName (Module_Name'Address);

      -- Now we going to create JIT
      if CreateExecutionEngineForModule
        (Ortho_LLVM.Jit.Engine'Access, Module, Msg'Access) /= 0
      then
         Put_Line (Standard_Error, "cannot create execution engine");
         raise Program_Error;
      end if;

      Target_Data := GetExecutionEngineTargetData (Ortho_LLVM.Jit.Engine);
      SetDataLayout (Module, CopyStringRepOfTargetData (Target_Data));

      Ortho_LLVM.Init;
   end Init;

   procedure Set_Address (Decl : O_Dnode; Addr : Address)
     renames Ortho_LLVM.Jit.Set_Address;

   function Get_Address (Decl : O_Dnode) return Address
     renames Ortho_LLVM.Jit.Get_Address;

   --  procedure InstallLazyFunctionCreator (EE : ExecutionEngineRef;
   --                                        Func : Address);
   --  pragma Import (C, InstallLazyFunctionCreator,
   --                 "LLVMInstallLazyFunctionCreator");

   --  Do link.
   procedure Link (Status : out Boolean)
   is
      use LLVM.Analysis;
      Msg : aliased Cstring;
   begin
      if Flag_Debug then
         Ortho_LLVM.Finish_Debug;
      end if;

      if Flag_Dump_Llvm then
         DumpModule (Module);
      end if;

      --  Verify module.
      if LLVM.Analysis.VerifyModule
        (Module, LLVM.Analysis.PrintMessageAction, Msg'Access) /= 0
      then
         DisposeMessage (Msg);
         Status := False;
         return;
      end if;

      --  FIXME: optim
   end Link;

   procedure Finish
   is
      --  F : ValueRef;
      --  Addr : Address;
      --  pragma Unreferenced (Addr);
   begin
      null;

      --  if No_Lazy then
      --     --  Be sure all functions code has been generated.
      --     F := GetFirstFunction (Module);
      --     while F /= Null_ValueRef loop
      --        if GetFirstBasicBlock (F) /= Null_BasicBlockRef then
      --           --  Only care about defined functions.
      --           Addr := GetPointerToFunction (EE, F);
      --        end if;
      --        F := GetNextFunction (F);
      --     end loop;
      --  end if;
   end Finish;

   function Decode_Option (Option : String) return Boolean
   is
      Opt : constant String (1 .. Option'Length) := Option;
   begin
      if Opt = "--llvm-dump" then
         Flag_Dump_Llvm := True;
         return True;
      end if;
      return False;
   end Decode_Option;

   procedure Disp_Help is
   begin
      null;
   end Disp_Help;

   function Get_Jit_Name return String is
   begin
      return "LLVM";
   end Get_Jit_Name;

end Ortho_Jit;
