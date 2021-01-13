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
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with LLVM.Core; use LLVM.Core;

package LLVM.Target is

   type TargetDataRef is new System.Address;

   --  LLVMInitializeNativeTarget - The main program should call this function
   --  to initialize the native target corresponding to the host.  This is
   --  useful for JIT applications to ensure that the target gets linked in
   --  correctly.
   procedure InitializeNativeTarget;
   pragma Import (C, InitializeNativeTarget,
                  "LLVMInitializeNativeTarget_noinline");

   --  LLVMInitializeNativeTargetAsmPrinter - The main program should call this
   --  function to initialize the printer for the native target corresponding
   --  to the host.
   procedure InitializeNativeAsmPrinter;
   pragma Import (C, InitializeNativeAsmPrinter,
                  "LLVMInitializeNativeAsmPrinter_noinline");

   --  Target Data

   --  Obtain the data layout for a module.
   --  see Module::getDataLayout()
   function GetModuleDataLayout (M : ModuleRef) return TargetDataRef;
   pragma Import (C, GetModuleDataLayout, "LLVMGetModuleDataLayout");

   --  Set the data layout for a module.
   --  see Module::setDataLayout()
   procedure SetModuleDataLayout (M : ModuleRef; DL : TargetDataRef);
   pragma Import (C, SetModuleDataLayout, "LLVMSetModuleDataLayout");

   --  Creates target data from a target layout string.
   --  See the constructor llvm::DataLayout::DataLayout.
   function CreateTargetData (StringRep : Cstring) return TargetDataRef;
   pragma Import (C, CreateTargetData, "LLVMCreateTargetData");

   --  Removed in LLVM 3.9 !!
   --  Adds target data information to a pass manager. This does not take
   --  ownership of the target data.
   --  See the method llvm::PassManagerBase::add.
   procedure AddTargetData(TD : TargetDataRef; PM : PassManagerRef);
   pragma Import (C, AddTargetData, "LLVMAddTargetData");

   --  Converts target data to a target layout string. The string must be
   --  disposed with LLVMDisposeMessage.
   --  See the constructor llvm::DataLayout::DataLayout. */
   function CopyStringRepOfTargetData(TD :TargetDataRef) return Cstring;
   pragma Import (C, CopyStringRepOfTargetData,
                  "LLVMCopyStringRepOfTargetData");

   --  Returns the pointer size in bytes for a target.
   --  See the method llvm::DataLayout::getPointerSize.
   function PointerSize(TD : TargetDataRef) return unsigned;
   pragma Import (C, PointerSize, "LLVMPointerSize");

   --  Computes the ABI size of a type in bytes for a target.
   --  See the method llvm::DataLayout::getTypeAllocSize.
   function ABISizeOfType (TD : TargetDataRef; Ty: TypeRef) return Unsigned_64;
   pragma Import (C, ABISizeOfType, "LLVMABISizeOfType");

   --  Computes the ABI alignment of a type in bytes for a target.
   --  See the method llvm::DataLayout::getTypeABISize.
   function ABIAlignmentOfType (TD : TargetDataRef; Ty: TypeRef)
                               return Unsigned_32;
   pragma Import (C, ABIAlignmentOfType, "LLVMABIAlignmentOfType");

   --  Computes the byte offset of the indexed struct element for a target.
   --  See the method llvm::StructLayout::getElementContainingOffset.
   function OffsetOfElement(TD : TargetDataRef;
                            StructTy : TypeRef;
                            Element : Unsigned_32)
                           return Unsigned_64;
   pragma Import (C, OffsetOfElement, "LLVMOffsetOfElement");

end LLVM.Target;
