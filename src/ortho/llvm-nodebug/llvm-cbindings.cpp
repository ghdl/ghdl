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
#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"

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

}
