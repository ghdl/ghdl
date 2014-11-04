--  LLVM binding
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with LLVM.Core; use LLVM.Core;

package LLVM.Analysis is
   type VerifierFailureAction is
     (
      AbortProcessAction, -- verifier will print to stderr and abort()
      PrintMessageAction, -- verifier will print to stderr and return 1
      ReturnStatusAction  -- verifier will just return 1
     );
   pragma Convention (C, VerifierFailureAction);

   -- Verifies that a module is valid, taking the specified action if not.
   -- Optionally returns a human-readable description of any invalid
   -- constructs.
   -- OutMessage must be disposed with DisposeMessage. */
   function VerifyModule(M : ModuleRef;
                         Action : VerifierFailureAction;
                         OutMessage : access Cstring)
                        return Integer;

   -- Verifies that a single function is valid, taking the specified
   --  action. Useful for debugging.
   function VerifyFunction(Fn : ValueRef; Action : VerifierFailureAction)
     return Integer;

   -- Open up a ghostview window that displays the CFG of the current function.
   -- Useful for debugging.
   procedure ViewFunctionCFG(Fn : ValueRef);
   procedure ViewFunctionCFGOnly(Fn : ValueRef);
private
   pragma Import (C, VerifyModule, "LLVMVerifyModule");
   pragma Import (C, VerifyFunction, "LLVMVerifyFunction");
   pragma Import (C, ViewFunctionCFG, "LLVMViewFunctionCFG");
   pragma Import (C, ViewFunctionCFGOnly, "LLVMViewFunctionCFGOnly");
end LLVM.Analysis;

