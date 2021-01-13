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
with System; use System;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with LLVM.Core; use LLVM.Core;
with LLVM.Target; use LLVM.Target;

package LLVM.ExecutionEngine is
   type GenericValueRef is new Address;
   type GenericValueRefArray is array (unsigned range <>) of GenericValueRef;
   pragma Convention (C, GenericValueRefArray);
   type ExecutionEngineRef is new Address;

   procedure LinkInJIT;
   procedure LinkInMCJIT;
   procedure LinkInInterpreter;

   -- Operations on generic values --------------------------------------

   function CreateGenericValueOfInt(Ty : TypeRef;
                                    N : Unsigned_64;
                                    IsSigned : Integer)
                                   return GenericValueRef;

   function CreateGenericValueOfPointer(P : System.Address)
                                           return GenericValueRef;

   function CreateGenericValueOfFloat(Ty : TypeRef; N : double)
                                         return GenericValueRef;

   function GenericValueIntWidth(GenValRef : GenericValueRef)
                                    return unsigned;

   function GenericValueToInt(GenVal : GenericValueRef;
                                  IsSigned : Integer) return Unsigned_64;

   function GenericValueToPointer(GenVal : GenericValueRef)
                                     return System.Address;

   function GenericValueToFloat(TyRef : TypeRef; GenVal : GenericValueRef)
                               return double;

   procedure DisposeGenericValue(GenVal : GenericValueRef);

   -- Operations on execution engines -----------------------------------

   function CreateExecutionEngineForModule
     (EE : access ExecutionEngineRef; M : ModuleRef; Error : access Cstring)
     return Bool;

   function CreateInterpreterForModule (Interp : access ExecutionEngineRef;
                                        M : ModuleRef;
                                        Error : access Cstring)
                                       return Bool;

   function CreateJITCompilerForModule (JIT : access ExecutionEngineRef;
                                        M : ModuleRef;
                                        OptLevel : unsigned;
                                        Error : access Cstring)
     return Bool;


   procedure DisposeExecutionEngine(EE : ExecutionEngineRef);

   procedure RunStaticConstructors(EE : ExecutionEngineRef);

   procedure RunStaticDestructors(EE : ExecutionEngineRef);

   function RunFunctionAsMain(EE : ExecutionEngineRef;
                              F : ValueRef;
                              ArgC : unsigned; Argv : Address; EnvP : Address)
                             return Integer;

   function RunFunction(EE : ExecutionEngineRef;
                        F : ValueRef;
                        NumArgs : unsigned;
                        Args : GenericValueRefArray)
                       return GenericValueRef;

   procedure FreeMachineCodeForFunction(EE : ExecutionEngineRef; F : ValueRef);

   procedure AddModule(EE : ExecutionEngineRef; M : ModuleRef);

   function RemoveModule(EE : ExecutionEngineRef;
                         M : ModuleRef;
                         OutMod : access ModuleRef;
                         OutError : access Cstring) return Bool;

   function FindFunction(EE : ExecutionEngineRef; Name : Cstring;
                                                  OutFn : access ValueRef)
                        return Integer;

   function GetExecutionEngineTargetData(EE : ExecutionEngineRef)
                                        return TargetDataRef;

   procedure AddGlobalMapping(EE : ExecutionEngineRef; Global : ValueRef;
                                                       Addr : Address);

   function GetPointerToGlobal (EE : ExecutionEngineRef; GV : ValueRef)
                               return Address;
   function GetPointerToFunctionOrStub (EE : ExecutionEngineRef;
                                        Func : ValueRef)
                                       return Address;

private
   pragma Import (C, LinkInJIT, "LLVMLinkInJIT");
   pragma Import (C, LinkInMCJIT, "LLVMLinkInMCJIT");
   pragma Import (C, LinkInInterpreter, "LLVMLinkInInterpreter");

   pragma Import (C, CreateGenericValueOfInt, "LLVMCreateGenericValueOfInt");
   pragma Import (C, CreateGenericValueOfPointer,
                  "LLVMCreateGenericValueOfPointer");
   pragma Import (C, CreateGenericValueOfFloat,
                  "LLVMCreateGenericValueOfFloat");
   pragma Import (C, GenericValueIntWidth, "LLVMGenericValueIntWidth");
   pragma Import (C, GenericValueToInt, "LLVMGenericValueToInt");
   pragma Import (C, GenericValueToPointer, "LLVMGenericValueToPointer");
   pragma Import (C, GenericValueToFloat, "LLVMGenericValueToFloat");
   pragma Import (C, DisposeGenericValue, "LLVMDisposeGenericValue");

   -- Operations on execution engines -----------------------------------

   pragma Import (C, CreateExecutionEngineForModule,
                  "LLVMCreateExecutionEngineForModule");
   pragma Import (C, CreateInterpreterForModule,
                  "LLVMCreateInterpreterForModule");
   pragma Import (C, CreateJITCompilerForModule,
                  "LLVMCreateJITCompilerForModule");
   pragma Import (C, DisposeExecutionEngine, "LLVMDisposeExecutionEngine");
   pragma Import (C, RunStaticConstructors, "LLVMRunStaticConstructors");
   pragma Import (C, RunStaticDestructors, "LLVMRunStaticDestructors");
   pragma Import (C, RunFunctionAsMain, "LLVMRunFunctionAsMain");
   pragma Import (C, RunFunction, "LLVMRunFunction");
   pragma Import (C, FreeMachineCodeForFunction,
                  "LLVMFreeMachineCodeForFunction");
   pragma Import (C, AddModule, "LLVMAddModule");
   pragma Import (C, RemoveModule, "LLVMRemoveModule");
   pragma Import (C, FindFunction, "LLVMFindFunction");
   pragma Import (C, GetExecutionEngineTargetData,
                "LLVMGetExecutionEngineTargetData");
   pragma Import (C, AddGlobalMapping, "LLVMAddGlobalMapping");

   pragma Import (C, GetPointerToFunctionOrStub,
                  "LLVMGetPointerToFunctionOrStub");
   pragma Import (C, GetPointerToGlobal,
                  "LLVMGetPointerToGlobal");
end LLVM.ExecutionEngine;
