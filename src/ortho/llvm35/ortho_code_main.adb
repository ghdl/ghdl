--  LLVM back-end for ortho - Main subprogram.
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;

with Ortho_Front; use Ortho_Front;
with LLVM.BitWriter;
with LLVM.Core; use LLVM.Core;
with LLVM.ExecutionEngine; use LLVM.ExecutionEngine;
with LLVM.Target; use LLVM.Target;
with LLVM.TargetMachine; use LLVM.TargetMachine;
with LLVM.Analysis;
with LLVM.Transforms.Scalar;
with Ortho_LLVM; use Ortho_LLVM;
with Interfaces;
with Interfaces.C; use Interfaces.C;

procedure Ortho_Code_Main is
   --  Name of the output filename (given by option '-o').
   Output : String_Acc := null;

   type Output_Kind_Type is (Output_Llvm, Output_Bytecode,
                             Output_Assembly, Output_Object);
   Output_Kind : Output_Kind_Type := Output_Object;

   --  True if the LLVM output must be displayed (set by '--dump-llvm')
   Flag_Dump_Llvm : Boolean := False;

   --  Index of the first file argument.
   First_File : Natural;

   --  Set by '--exec': function to call and its argument (an integer)
   Exec_Func : String_Acc := null;
   Exec_Val : Integer := 0;

   --  Current option index.
   Optind : Natural;

   --  Number of arguments.
   Argc : constant Natural := Argument_Count;

   --  Name of the module.
   Module_Name : String := "ortho" & Ascii.Nul;

   --  Target triple.
   Triple : Cstring := Empty_Cstring;

   --  Execution engine
   Engine : aliased ExecutionEngineRef;

   Target : aliased TargetRef;

   CPU : constant Cstring := Empty_Cstring;
   Features : constant Cstring := Empty_Cstring;
   Reloc : RelocMode := RelocDefault;

   function To_String (C : Cstring) return String is
      function Strlen (C : Cstring) return Natural;
      pragma Import (C, Strlen);

      subtype Fat_String is String (Positive);
      type Fat_String_Acc is access Fat_String;

      function To_Fat_String_Acc is new
        Ada.Unchecked_Conversion (Cstring, Fat_String_Acc);
   begin
      return To_Fat_String_Acc (C)(1 .. Strlen (C));
   end To_String;

   Codegen : CodeGenFileType := ObjectFile;

   Msg : aliased Cstring;
begin
   Ortho_Front.Init;

   --  Decode options.
   First_File := Natural'Last;
   Optind := 1;
   while Optind <= Argc loop
      declare
         Arg : constant String := Argument (Optind);
      begin
         if Arg (1) = '-' then
            if Arg = "--dump-llvm" then
               Flag_Dump_Llvm := True;
            elsif Arg = "-o" then
               if Optind = Argc then
                  Put_Line (Standard_Error, "error: missing filename to '-o'");
                  return;
               end if;
               Output := new String'(Argument (Optind + 1) & ASCII.Nul);
               Optind := Optind + 1;
            elsif Arg = "-quiet" then
               --  Skip silently.
               null;
            elsif Arg = "-S" then
               Output_Kind := Output_Assembly;
               Codegen := AssemblyFile;
            elsif Arg = "-c" then
               Output_Kind := Output_Object;
               Codegen := ObjectFile;
            elsif Arg = "-O0" then
               Optimization := CodeGenLevelNone;
            elsif Arg = "-O1" or else Arg = "-O" then
               Optimization := CodeGenLevelLess;
            elsif Arg = "-O2" then
               Optimization := CodeGenLevelDefault;
            elsif Arg = "-O3" then
               Optimization := CodeGenLevelAggressive;
            elsif Arg = "-fpic" or Arg = "-fPIC" then
               Reloc := RelocPIC;
            elsif Arg = "-fno-pic" then
               Reloc := RelocStatic;
            elsif Arg = "--emit-llvm" then
               Output_Kind := Output_Llvm;
            elsif Arg = "--emit-bc" then
               Output_Kind := Output_Bytecode;
            elsif Arg = "--exec" then
               if Optind + 1 >= Argc then
                  Put_Line (Standard_Error,
                            "error: missing function name to '--exec'");
                  return;
               end if;
               Exec_Func := new String'(Argument (Optind + 1));
               Exec_Val := Integer'Value (Argument (Optind + 2));
               Optind := Optind + 2;
            elsif Arg = "-glines"
              or else Arg = "-gline-tables-only"
            then
               Flag_Debug_Line := True;
            elsif Arg = "-g" then
               Flag_Debug_Line := True;
               Flag_Debug := True;
            else
               --  This is really an argument.
               declare
                  procedure Unchecked_Deallocation is
                     new Ada.Unchecked_Deallocation
                    (Name => String_Acc, Object => String);

                  Opt : String_Acc := new String'(Arg);
                  Opt_Arg : String_Acc;
                  Res : Natural;
               begin
                  Opt_Arg := null;
                  if Optind < Argument_Count then
                     declare
                        Arg1 : constant String := Argument (Optind + 1);
                     begin
                        if Arg1 (Arg1'First) /= '-' then
                           Opt_Arg := new String'(Arg1);
                        end if;
                     end;
                  end if;

                  Res := Ortho_Front.Decode_Option (Opt, Opt_Arg);
                  case Res is
                     when 0 =>
                        Put_Line (Standard_Error,
                                  "unknown option '" & Arg & "'");
                        return;
                     when 1 =>
                        null;
                     when 2 =>
                        Optind := Optind + 1;
                     when others =>
                        raise Program_Error;
                  end case;
                  Unchecked_Deallocation (Opt);
                  Unchecked_Deallocation (Opt_Arg);
               end;
            end if;
         else
            First_File := Optind;
            exit;
         end if;
      end;
      Optind := Optind + 1;
   end loop;

   --  Link with LLVM libraries.
   InitializeNativeTarget;
   InitializeNativeAsmPrinter;

   LinkInJIT;

   Module := ModuleCreateWithName (Module_Name'Address);

   if Output = null and then Exec_Func /= null then
      -- Now we going to create JIT
      if CreateExecutionEngineForModule
        (Engine'Access, Module, Msg'Access) /= 0
      then
         Put_Line (Standard_Error,
                   "cannot create execute: " & To_String (Msg));
         raise Program_Error;
      end if;

      Target_Data := GetExecutionEngineTargetData (Engine);
   else
      --  Extract target triple
      Triple := GetDefaultTargetTriple;
      SetTarget (Module, Triple);

      --  Get Target
      if GetTargetFromTriple (Triple, Target'Access, Msg'Access) /= 0 then
         raise Program_Error;
      end if;

      --  Create a target machine
      Target_Machine := CreateTargetMachine
        (Target, Triple, CPU, Features, Optimization, Reloc, CodeModelDefault);

      Target_Data := GetTargetMachineData (Target_Machine);
   end if;

   SetDataLayout (Module, CopyStringRepOfTargetData (Target_Data));

   if False then
      declare
         Targ : TargetRef;
      begin
         Put_Line ("Triple: " & To_String (Triple));
         New_Line;
         Put_Line ("Targets:");
         Targ := GetFirstTarget;
         while Targ /= Null_TargetRef loop
            Put_Line (" " & To_String (GetTargetName (Targ))
                        & ": " & To_String (GetTargetDescription (Targ)));
            Targ := GetNextTarget (Targ);
         end loop;
      end;
      -- Target_Data := CreateTargetData (Triple);
   end if;

   Ortho_LLVM.Init;

   Set_Exit_Status (Failure);

   if First_File > Argument_Count then
      begin
         if not Parse (null) then
            return;
         end if;
      exception
         when others =>
            return;
      end;
   else
      for I in First_File .. Argument_Count loop
         declare
            Filename : constant String_Acc :=
              new String'(Argument (First_File));
         begin
            if not Parse (Filename) then
               return;
            end if;
         exception
            when others =>
               return;
         end;
      end loop;
   end if;

   if Flag_Debug_Line then
      Ortho_LLVM.Finish_Debug;
   end if;

   --  Ortho_Mcode.Finish;

   if Flag_Dump_Llvm then
      DumpModule (Module);
   end if;

   --  Verify module.
   if False then
      if LLVM.Analysis.VerifyModule
        (Module, LLVM.Analysis.PrintMessageAction, Msg'Access) /= 0
      then
         DisposeMessage (Msg);
         raise Program_Error;
      end if;
   end if;

   if Optimization > CodeGenLevelNone then
      declare
         use LLVM.Transforms.Scalar;
         Global_Manager : constant Boolean := False;
         Pass_Manager : PassManagerRef;
         Res : Bool;
         pragma Unreferenced (Res);
         A_Func : ValueRef;
      begin
         if Global_Manager then
            Pass_Manager := CreatePassManager;
         else
            Pass_Manager := CreateFunctionPassManagerForModule (Module);
         end if;

         LLVM.Target.AddTargetData (Target_Data, Pass_Manager);
         AddPromoteMemoryToRegisterPass (Pass_Manager);
         AddCFGSimplificationPass (Pass_Manager);

         if Global_Manager then
            Res := RunPassManager (Pass_Manager, Module);
         else
            A_Func := GetFirstFunction (Module);
            while A_Func /= Null_ValueRef loop
               Res := RunFunctionPassManager (Pass_Manager, A_Func);
               A_Func := GetNextFunction (A_Func);
            end loop;
         end if;
      end;
   end if;

   if Output /= null then
      declare
         Error : Boolean;
      begin
         Msg := Empty_Cstring;

         case Output_Kind is
            when Output_Assembly
              | Output_Object =>
               Error := LLVM.TargetMachine.TargetMachineEmitToFile
                 (Target_Machine, Module,
                  Output.all'Address, Codegen, Msg'Access) /= 0;
            when Output_Bytecode =>
               Error := LLVM.BitWriter.WriteBitcodeToFile
                 (Module, Output.all'Address) /= 0;
            when Output_Llvm =>
               Error := PrintModuleToFile
                 (Module, Output.all'Address, Msg'Access) /= 0;
         end case;
         if Error then
            Put_Line (Standard_Error,
                      "error while writing to " & Output.all);
            if Msg /= Empty_Cstring then
               Put_Line (Standard_Error,
                         "message: " & To_String (Msg));
               DisposeMessage (Msg);
            end if;
            Set_Exit_Status (2);
            return;
         end if;
      end;
   elsif Exec_Func /= null then
      declare
         use Interfaces;
         Res : GenericValueRef;
         Vals : GenericValueRefArray (0 .. 0);
         Func : aliased ValueRef;
      begin
         if FindFunction (Engine, Exec_Func.all'Address, Func'Access) /= 0 then
            raise Program_Error;
         end if;

         -- Call the function with argument n:
         Vals (0) := CreateGenericValueOfInt
           (Int32Type, Unsigned_64 (Exec_Val), 0);
         Res := RunFunction (Engine, Func, 1, Vals);

         -- import result of execution
         Put_Line ("Result is "
                     & Unsigned_64'Image (GenericValueToInt (Res, 0)));

      end;
   else
      DumpModule (Module);
   end if;

   Set_Exit_Status (Success);
exception
   when others =>
      Set_Exit_Status (2);
      raise;
end Ortho_Code_Main;
