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
with LLVM.Core; use LLVM.Core;

package LLVM.Transforms.Scalar is
   --  See llvm::createAggressiveDCEPass function.
   procedure AddAggressiveDCEPass(PM : PassManagerRef);
   pragma Import (C, AddAggressiveDCEPass, "LLVMAddAggressiveDCEPass");

   --  See llvm::createCFGSimplificationPass function.
   procedure AddCFGSimplificationPass(PM : PassManagerRef);
   pragma Import (C, AddCFGSimplificationPass, "LLVMAddCFGSimplificationPass");

   --  See llvm::createDeadStoreEliminationPass function.
   procedure AddDeadStoreEliminationPass(PM : PassManagerRef);
   pragma Import (C, AddDeadStoreEliminationPass,
                  "LLVMAddDeadStoreEliminationPass");

   --  See llvm::createScalarizerPass function.
   procedure AddScalarizerPass(PM : PassManagerRef);
   pragma Import (C, AddScalarizerPass, "LLVMAddScalarizerPass");

   --  See llvm::createGVNPass function.
   procedure AddGVNPass(PM : PassManagerRef);
   pragma Import (C, AddGVNPass, "LLVMAddGVNPass");

   --  See llvm::createIndVarSimplifyPass function.
   procedure AddIndVarSimplifyPass(PM : PassManagerRef);
   pragma Import (C, AddIndVarSimplifyPass, "LLVMAddIndVarSimplifyPass");

   --  See llvm::createInstructionCombiningPass function.
   procedure AddInstructionCombiningPass(PM : PassManagerRef);
   pragma Import (C, AddInstructionCombiningPass,
                  "LLVMAddInstructionCombiningPass");

   --  See llvm::createJumpThreadingPass function.
   procedure AddJumpThreadingPass(PM : PassManagerRef);
   pragma Import (C, AddJumpThreadingPass, "LLVMAddJumpThreadingPass");

   --  See llvm::createLICMPass function.
   procedure AddLICMPass(PM : PassManagerRef);
   pragma Import (C, AddLICMPass, "LLVMAddLICMPass");

   --  See llvm::createLoopDeletionPass function.
   procedure AddLoopDeletionPass(PM : PassManagerRef);
   pragma Import (C, AddLoopDeletionPass, "LLVMAddLoopDeletionPass");

   --  See llvm::createLoopIdiomPass function
   procedure AddLoopIdiomPass(PM : PassManagerRef);
   pragma Import (C, AddLoopIdiomPass, "LLVMAddLoopIdiomPass");

   --  See llvm::createLoopRotatePass function.
   procedure AddLoopRotatePass(PM : PassManagerRef);
   pragma Import (C, AddLoopRotatePass, "LLVMAddLoopRotatePass");

   --  See llvm::createLoopRerollPass function.
   procedure AddLoopRerollPass(PM : PassManagerRef);
   pragma Import (C, AddLoopRerollPass, "LLVMAddLoopRerollPass");

   --  See llvm::createLoopUnrollPass function.
   procedure AddLoopUnrollPass(PM : PassManagerRef);
   pragma Import (C, AddLoopUnrollPass, "LLVMAddLoopUnrollPass");

   --  See llvm::createLoopUnswitchPass function.
   procedure AddLoopUnswitchPass(PM : PassManagerRef);
   pragma Import (C, AddLoopUnswitchPass, "LLVMAddLoopUnswitchPass");

   --  See llvm::createMemCpyOptPass function.
   procedure AddMemCpyOptPass(PM : PassManagerRef);
   pragma Import (C, AddMemCpyOptPass, "LLVMAddMemCpyOptPass");

   --  See llvm::createPartiallyInlineLibCallsPass function.
   procedure AddPartiallyInlineLibCallsPass(PM : PassManagerRef);
   pragma Import (C, AddPartiallyInlineLibCallsPass,
                  "LLVMAddPartiallyInlineLibCallsPass");

   --  See llvm::createPromoteMemoryToRegisterPass function.
   procedure AddPromoteMemoryToRegisterPass(PM : PassManagerRef);
   pragma Import (C, AddPromoteMemoryToRegisterPass,
                  "LLVMAddPromoteMemoryToRegisterPass");

   --  See llvm::createReassociatePass function.
   procedure AddReassociatePass(PM : PassManagerRef);
   pragma Import (C, AddReassociatePass, "LLVMAddReassociatePass");

   --  See llvm::createSCCPPass function.
   procedure AddSCCPPass(PM : PassManagerRef);
   pragma Import (C, AddSCCPPass, "LLVMAddSCCPPass");

   --  See llvm::createScalarReplAggregatesPass function.
   procedure AddScalarReplAggregatesPass(PM : PassManagerRef);
   pragma Import (C, AddScalarReplAggregatesPass,
                  "LLVMAddScalarReplAggregatesPass");

   --  See llvm::createScalarReplAggregatesPass function.
   procedure AddScalarReplAggregatesPassSSA(PM : PassManagerRef);
   pragma Import (C, AddScalarReplAggregatesPassSSA,
                  "LLVMAddScalarReplAggregatesPassSSA");

   --  See llvm::createScalarReplAggregatesPass function.
   procedure AddScalarReplAggregatesPassWithThreshold
     (PM : PassManagerRef; Threshold : Integer);
   pragma Import (C, AddScalarReplAggregatesPassWithThreshold,
                  "LLVMAddScalarReplAggregatesPassWithThreshold");

   --  See llvm::createSimplifyLibCallsPass function.
   procedure AddSimplifyLibCallsPass(PM : PassManagerRef);
   pragma Import (C, AddSimplifyLibCallsPass, "LLVMAddSimplifyLibCallsPass");

   --  See llvm::createTailCallEliminationPass function.
   procedure AddTailCallEliminationPass(PM : PassManagerRef);
   pragma Import (C, AddTailCallEliminationPass,
                  "LLVMAddTailCallEliminationPass");

   --  See llvm::createConstantPropagationPass function.
   procedure AddConstantPropagationPass(PM : PassManagerRef);
   pragma Import (C, AddConstantPropagationPass,
                  "LLVMAddConstantPropagationPass");

   --  See llvm::demotePromoteMemoryToRegisterPass function.
   procedure AddDemoteMemoryToRegisterPass(PM : PassManagerRef);
   pragma Import (C, AddDemoteMemoryToRegisterPass,
                  "LLVMAddDemoteMemoryToRegisterPass");

   --  See llvm::createVerifierPass function.
   procedure AddVerifierPass(PM : PassManagerRef);
   pragma Import (C, AddVerifierPass, "LLVMAddVerifierPass");

   --  See llvm::createCorrelatedValuePropagationPass function
   procedure AddCorrelatedValuePropagationPass(PM : PassManagerRef);
   pragma Import (C, AddCorrelatedValuePropagationPass,
                  "LLVMAddCorrelatedValuePropagationPass");

   --  See llvm::createEarlyCSEPass function
   procedure AddEarlyCSEPass(PM : PassManagerRef);
   pragma Import (C, AddEarlyCSEPass, "LLVMAddEarlyCSEPass");

   --  See llvm::createLowerExpectIntrinsicPass function
   procedure AddLowerExpectIntrinsicPass(PM : PassManagerRef);
   pragma Import (C, AddLowerExpectIntrinsicPass,
                  "LLVMAddLowerExpectIntrinsicPass");

   --  See llvm::createTypeBasedAliasAnalysisPass function
   procedure AddTypeBasedAliasAnalysisPass(PM : PassManagerRef);
   pragma Import (C, AddTypeBasedAliasAnalysisPass,
                  "LLVMAddTypeBasedAliasAnalysisPass");

   --  See llvm::createBasicAliasAnalysisPass function
   procedure AddBasicAliasAnalysisPass(PM : PassManagerRef);
   pragma Import (C, AddBasicAliasAnalysisPass,
                  "LLVMAddBasicAliasAnalysisPass");
end LLVM.Transforms.Scalar;


