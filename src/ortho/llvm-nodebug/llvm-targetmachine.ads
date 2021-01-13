--  LLVM binding
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
with System;
with LLVM.Core; use LLVM.Core;
with LLVM.Target; use LLVM.Target;

package LLVM.TargetMachine is

   type TargetMachineRef is new System.Address;
   Null_TargetMachineRef : constant TargetMachineRef :=
     TargetMachineRef (System.Null_Address);

   type TargetRef is new System.Address;
   Null_TargetRef : constant TargetRef := TargetRef (System.Null_Address);

   type CodeGenOptLevel is (CodeGenLevelNone,
                            CodeGenLevelLess,
                            CodeGenLevelDefault,
                            CodeGenLevelAggressive);
   pragma Convention (C, CodeGenOptLevel);

   type RelocMode is (RelocDefault,
                      RelocStatic,
                      RelocPIC,
                      RelocDynamicNoPic);
   pragma Convention (C, RelocMode);

   type CodeModel is (CodeModelDefault,
                      CodeModelJITDefault,
                      CodeModelSmall,
                      CodeModelKernel,
                      CodeModelMedium,
                      CodeModelLarge);
   pragma Convention (C, CodeModel);

   type CodeGenFileType is (AssemblyFile,
                            ObjectFile);
   pragma Convention (C, CodeGenFileType);

   --  Returns the first llvm::Target in the registered targets list.
   function GetFirstTarget return TargetRef;
   pragma Import (C, GetFirstTarget, "LLVMGetFirstTarget");

   --  Returns the next llvm::Target given a previous one (or null if there's
   --  none) */
   function GetNextTarget(T : TargetRef) return TargetRef;
   pragma Import (C, GetNextTarget, "LLVMGetNextTarget");

   --  Target

   --  Finds the target corresponding to the given name and stores it in T.
   --  Returns 0 on success.
   function GetTargetFromName (Name : Cstring) return TargetRef;
   pragma Import (C, GetTargetFromName, "LLVMGetTargetFromName");

   --  Finds the target corresponding to the given triple and stores it in T.
   --  Returns 0 on success. Optionally returns any error in ErrorMessage.
   --  Use LLVMDisposeMessage to dispose the message.
   --  Ada: ErrorMessage is the address of a Cstring.
   function GetTargetFromTriple
     (Triple : Cstring; T : access TargetRef; ErrorMessage : access Cstring)
     return Bool;
   pragma Import (C, GetTargetFromTriple, "LLVMGetTargetFromTriple");

   --  Returns the name of a target. See llvm::Target::getName
   function GetTargetName (T: TargetRef) return Cstring;
   pragma Import (C, GetTargetName, "LLVMGetTargetName");

   --  Returns the description  of a target. See llvm::Target::getDescription
   function GetTargetDescription (T : TargetRef) return Cstring;
   pragma Import (C, GetTargetDescription, "LLVMGetTargetDescription");

   --  Target Machine ----------------------------------------------------

   --  Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine

   function CreateTargetMachine(T : TargetRef;
                                Triple : Cstring;
                                CPU : Cstring;
                                Features : Cstring;
                                Level : CodeGenOptLevel;
                                Reloc : RelocMode;
                                CM : CodeModel)
                               return TargetMachineRef;
   pragma Import (C, CreateTargetMachine, "LLVMCreateTargetMachine");

   --  Create a DataLayout based on the targetMachine.
   function CreateTargetDataLayout (T : TargetMachineRef) return TargetDataRef;
   pragma Import (C, CreateTargetDataLayout, "LLVMCreateTargetDataLayout");

   -- Returns the llvm::DataLayout used for this llvm:TargetMachine.
   function GetTargetMachineData(T : TargetMachineRef) return TargetDataRef;
   pragma Import (C, GetTargetMachineData, "LLVMGetTargetMachineData");

   --  Emits an asm or object file for the given module to the filename. This
   --  wraps several c++ only classes (among them a file stream). Returns any
   --  error in ErrorMessage. Use LLVMDisposeMessage to dispose the message.
   function TargetMachineEmitToFile(T : TargetMachineRef;
                                    M : ModuleRef;
                                    Filename : Cstring;
                                    Codegen : CodeGenFileType;
                                    ErrorMessage : access Cstring)
                                   return Bool;
   pragma Import (C, TargetMachineEmitToFile,
                  "LLVMTargetMachineEmitToFile");

   --  Get a triple for the host machine as a string. The result needs to be
   --  disposed with LLVMDisposeMessage.
   function GetDefaultTargetTriple return Cstring;
   pragma Import (C, GetDefaultTargetTriple, "LLVMGetDefaultTargetTriple");
end LLVM.TargetMachine;
