/*  LLVM binding
  Copyright (C) 2014 Tristan Gingold

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <gnu.org/licenses>.
*/

#include "llvm-c/Target.h"
#include "llvm-c/Core.h"
#include "llvm-c/ExecutionEngine.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"

using namespace llvm;

extern "C" {

void
LLVMInitializeNativeTarget_noinline (void)
{
  LLVMInitializeNativeTarget ();
}

void
LLVMInitializeNativeAsmPrinter_noinline (void)
{
  LLVMInitializeNativeAsmPrinter();
}

LLVMTypeRef LLVMMetadataTypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getMetadataTy(*unwrap(C));
}

LLVMTypeRef LLVMMetadataType_extra(void) {
  return LLVMMetadataTypeInContext(LLVMGetGlobalContext());
}

void
LLVMMDNodeReplaceOperandWith_extra (LLVMValueRef N, unsigned i, LLVMValueRef V) {
  MDNode *MD = cast<MDNode>(unwrap(N));
  MD->replaceOperandWith (i, unwrap(V));
}

void *LLVMGetPointerToFunction(LLVMExecutionEngineRef EE, LLVMValueRef Func)
{
  return unwrap(EE)->getPointerToFunction(unwrap<Function>(Func));
}

}
