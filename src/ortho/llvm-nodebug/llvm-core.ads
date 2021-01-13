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
with Interfaces.C; use Interfaces.C;
use Interfaces;

package LLVM.Core is

   subtype Cstring is System.Address;
   function "=" (L, R : Cstring) return Boolean renames System."=";
   --  Null_Cstring : constant Cstring := Null_Address;
   Nul : constant String := (1 => Character'Val (0));
   Empty_Cstring : constant Cstring := Nul'Address;

   --  The top-level container for all LLVM global data. See the LLVMContext
   --  class.
   type ContextRef is new System.Address;

   --  The top-level container for all other LLVM Intermediate
   --  Representation (IR) objects. See the llvm::Module class.
   type ModuleRef is new System.Address;

   subtype Bool is int;

   --  Each value in the LLVM IR has a type, an LLVMTypeRef. See the llvm::Type
   --  class.
   type TypeRef is new System.Address;
   Null_TypeRef : constant TypeRef := TypeRef (System.Null_Address);
   type TypeRefArray is array (unsigned range <>) of TypeRef;
   pragma Convention (C, TypeRefArray);

   type ValueRef is new System.Address;
   Null_ValueRef : constant ValueRef := ValueRef (System.Null_Address);
   type ValueRefArray is array (unsigned range <>) of ValueRef; -- Ada
   pragma Convention (C, ValueRefArray);

   type BasicBlockRef is new System.Address;
   Null_BasicBlockRef : constant BasicBlockRef :=
     BasicBlockRef (System.Null_Address);
   type BasicBlockRefArray is
     array (unsigned range <>) of BasicBlockRef; -- Ada
   pragma Convention (C, BasicBlockRefArray);

   type BuilderRef is new System.Address;

   --  Used to provide a module to JIT or interpreter.
   --  See the llvm::MemoryBuffer class.
   type MemoryBufferRef is new System.Address;

   --  See the llvm::PassManagerBase class.
   type PassManagerRef is new System.Address;

   type Attribute is new unsigned;
   ZExtAttribute            : constant Attribute := 2**0;
   SExtAttribute            : constant Attribute := 2**1;
   NoReturnAttribute        : constant Attribute := 2**2;
   InRegAttribute           : constant Attribute := 2**3;
   StructRetAttribute       : constant Attribute := 2**4;
   NoUnwindAttribute        : constant Attribute := 2**5;
   NoAliasAttribute         : constant Attribute := 2**6;
   ByValAttribute           : constant Attribute := 2**7;
   NestAttribute            : constant Attribute := 2**8;
   ReadNoneAttribute        : constant Attribute := 2**9;
   ReadOnlyAttribute        : constant Attribute := 2**10;
   NoInlineAttribute        : constant Attribute := 2**11;
   AlwaysInlineAttribute    : constant Attribute := 2**12;
   OptimizeForSizeAttribute : constant Attribute := 2**13;
   StackProtectAttribute    : constant Attribute := 2**14;
   StackProtectReqAttribute : constant Attribute := 2**15;
   Alignment                : constant Attribute := 31 * 2**16;
   NoCaptureAttribute       : constant Attribute := 2**21;
   NoRedZoneAttribute       : constant Attribute := 2**22;
   NoImplicitFloatAttribute : constant Attribute := 2**23;
   NakedAttribute           : constant Attribute := 2**24;
   InlineHintAttribute      : constant Attribute := 2**25;
   StackAlignment           : constant Attribute := 7 * 2**26;
   ReturnsTwice             : constant Attribute := 2**29;
   UWTable                  : constant Attribute := 2**30;
   NonLazyBind              : constant Attribute := 2**31;

   type TypeKind is
     (
      VoidTypeKind,        --  type with no size
      HalfTypeKind,        --  16 bit floating point type
      FloatTypeKind,       --  32 bit floating point type
      DoubleTypeKind,      --  64 bit floating point type
      X86_FP80TypeKind,    --  80 bit floating point type (X87)
      FP128TypeKind,       --  128 bit floating point type (112-bit mantissa)
      PPC_FP128TypeKind,   --  128 bit floating point type (two 64-bits)
      LabelTypeKind,       --  Labels
      IntegerTypeKind,     --  Arbitrary bit width integers
      FunctionTypeKind,    --  Functions
      StructTypeKind,      --  Structures
      ArrayTypeKind,       --  Arrays
      PointerTypeKind,     --  Pointers
      VectorTypeKind,      --  SIMD 'packed' format, or other vector type
      MetadataTypeKind,    --  Metadata
      X86_MMXTypeKind      --  X86 MMX
     );
   pragma Convention (C, TypeKind);

   type Linkage is
     (
      ExternalLinkage,    --  Externally visible function
      AvailableExternallyLinkage,
      LinkOnceAnyLinkage, --  Keep one copy of function when linking (inline)
      LinkOnceODRLinkage, --  Same, but only replaced by someth equivalent.
      LinkOnceODRAutoHideLinkage, --  Obsolete
      WeakAnyLinkage,     --  Keep one copy of function when linking (weak)
      WeakODRLinkage,     --  Same, but only replaced by someth equivalent.
      AppendingLinkage,   --  Special purpose, only applies to global arrays
      InternalLinkage,    --  Rename collisions when linking (static func)
      PrivateLinkage,     --  Like Internal, but omit from symbol table
      DLLImportLinkage,   --  Obsolete
      DLLExportLinkage,   --  Obsolete
      ExternalWeakLinkage,--  ExternalWeak linkage description
      GhostLinkage,       --  Obsolete
      CommonLinkage,      --  Tentative definitions
      LinkerPrivateLinkage, --  Like Private, but linker removes.
      LinkerPrivateWeakLinkage --  Like LinkerPrivate, but is weak.
     );
   pragma Convention (C, Linkage);

   type Visibility is
     (
      DefaultVisibility,  --  The GV is visible
      HiddenVisibility,   --  The GV is hidden
      ProtectedVisibility --  The GV is protected
     );
   pragma Convention (C, Visibility);

   type CallConv is new unsigned;
   CCallConv           : constant CallConv := 0;
   FastCallConv        : constant CallConv := 8;
   ColdCallConv        : constant CallConv := 9;
   X86StdcallCallConv  : constant CallConv := 64;
   X86FastcallCallConv : constant CallConv := 6;

   type IntPredicate is new unsigned;
   IntEQ  : constant IntPredicate := 32; -- equal
   IntNE  : constant IntPredicate := 33; -- not equal
   IntUGT : constant IntPredicate := 34; -- unsigned greater than
   IntUGE : constant IntPredicate := 35; -- unsigned greater or equal
   IntULT : constant IntPredicate := 36; -- unsigned less than
   IntULE : constant IntPredicate := 37; -- unsigned less or equal
   IntSGT : constant IntPredicate := 38; -- signed greater than
   IntSGE : constant IntPredicate := 39; -- signed greater or equal
   IntSLT : constant IntPredicate := 40; -- signed less than
   IntSLE : constant IntPredicate := 41; -- signed less or equal

   type RealPredicate is
     (
      RealPredicateFalse, --  Always false (always folded)
      RealOEQ,            --  True if ordered and equal
      RealOGT,            --  True if ordered and greater than
      RealOGE,            --  True if ordered and greater than or equal
      RealOLT,            --  True if ordered and less than
      RealOLE,            --  True if ordered and less than or equal
      RealONE,            --  True if ordered and operands are unequal
      RealORD,            --  True if ordered (no nans)
      RealUNO,            --  True if unordered: isnan(X) | isnan(Y)
      RealUEQ,            --  True if unordered or equal
      RealUGT,            --  True if unordered or greater than
      RealUGE,            --  True if unordered, greater than, or equal
      RealULT,            --  True if unordered or less than
      RealULE,            --  True if unordered, less than, or equal
      RealUNE,            --  True if unordered or not equal
      RealPredicateTrue   --  Always true (always folded)
     );

   -- Error handling ----------------------------------------------------

   procedure DisposeMessage (Message : Cstring);


   --  Context

   --  Create a new context.
   --  Every call to this function should be paired with a call to
   -- LLVMContextDispose() or the context will leak memory.
   function ContextCreate return ContextRef;

   --  Obtain the global context instance.
   function GetGlobalContext return ContextRef;

   --  Destroy a context instance.
   --  This should be called for every call to LLVMContextCreate() or memory
   --  will be leaked.
   procedure ContextDispose (C : ContextRef);

   -- Modules -----------------------------------------------------------

   -- Create and destroy modules.
   -- See llvm::Module::Module.
   function ModuleCreateWithName (ModuleID : Cstring) return ModuleRef;

   -- See llvm::Module::~Module.
   procedure DisposeModule (M : ModuleRef);

   -- Data layout. See Module::getDataLayout.
   function GetDataLayout(M : ModuleRef) return Cstring;
   procedure SetDataLayout(M : ModuleRef; Triple : Cstring);

   -- Target triple. See Module::getTargetTriple.
   function GetTarget (M : ModuleRef) return Cstring;
   procedure SetTarget (M : ModuleRef; Triple : Cstring);

   -- See Module::dump.
   procedure DumpModule(M : ModuleRef);

   --  Print a representation of a module to a file. The ErrorMessage needs to
   --  be disposed with LLVMDisposeMessage. Returns 0 on success, 1 otherwise.
   --
   --  @see Module::print()
   function PrintModuleToFile(M : ModuleRef;
                              Filename : Cstring;
                              ErrorMessage : access Cstring) return Bool;


   -- Types -------------------------------------------------------------

   -- LLVM types conform to the following hierarchy:
   --
   --   types:
   --     integer type
   --     real type
   --     function type
   --     sequence types:
   --       array type
   --       pointer type
   --       vector type
   --     void type
   --     label type
   --     opaque type

   -- See llvm::LLVMTypeKind::getTypeID.
   function GetTypeKind (Ty : TypeRef) return TypeKind;

   -- Operations on integer types
   function Int1Type return TypeRef;
   function Int8Type return TypeRef;
   function Int16Type return TypeRef;
   function Int32Type return TypeRef;
   function Int64Type return TypeRef;
   function IntType(NumBits : unsigned) return TypeRef;
   function GetIntTypeWidth(IntegerTy : TypeRef) return unsigned;

   function MetadataType return TypeRef;

   -- Operations on real types
   function FloatType return TypeRef;
   function DoubleType return TypeRef;
   function X86FP80Type return TypeRef;
   function FP128Type return TypeRef;
   function PPCFP128Type return TypeRef;

   -- Operations on function types
   function FunctionType(ReturnType : TypeRef;
                         ParamTypes : TypeRefArray;
                         ParamCount : unsigned;
                         IsVarArg : int) return TypeRef;

   function IsFunctionVarArg(FunctionTy : TypeRef) return int;
   function GetReturnType(FunctionTy : TypeRef) return TypeRef;
   function CountParamTypes(FunctionTy : TypeRef) return unsigned;
   procedure GetParamTypes(FunctionTy : TypeRef; Dest : out TypeRefArray);

   -- Operations on struct types
   function StructType(ElementTypes : TypeRefArray;
                       ElementCount : unsigned;
                       Packed : Bool) return TypeRef;
   function StructCreateNamed(C : ContextRef; Name : Cstring) return TypeRef;
   procedure StructSetBody(StructTy : TypeRef;
                           ElementTypes : TypeRefArray;
                           ElementCount : unsigned;
                           Packed : Bool);
   function CountStructElementTypes(StructTy : TypeRef) return unsigned;
   procedure GetStructElementTypes(StructTy : TypeRef;
                                   Dest : out TypeRefArray);
   function IsPackedStruct(StructTy : TypeRef) return Bool;


   -- Operations on array, pointer, and vector types (sequence types)
   function ArrayType(ElementType : TypeRef; ElementCount : unsigned)
                     return TypeRef;
   function PointerType(ElementType : TypeRef; AddressSpace : unsigned := 0)
                       return TypeRef;
   function VectorType(ElementType : TypeRef; ElementCount : unsigned)
                      return TypeRef;

   function GetElementType(Ty : TypeRef) return TypeRef;
   function GetArrayLength(ArrayTy : TypeRef) return unsigned;
   function GetPointerAddressSpace(PointerTy : TypeRef) return unsigned;
   function GetVectorSize(VectorTy : TypeRef) return unsigned;

   -- Operations on other types.
   function VoidType return TypeRef;
   function LabelType return TypeRef;

   -- See Module::dump.
   procedure DumpType(T : TypeRef);

   -- Values ------------------------------------------------------------
   -- The bulk of LLVM's object model consists of values, which comprise a very
   -- rich type hierarchy.
   --
   --   values:
   --     constants:
   --       scalar constants
   --       composite contants
   --       globals:
   --         global variable
   --         function
   --         alias
   --       basic blocks

   -- Operations on all values
   function TypeOf(Val : ValueRef) return TypeRef;
   function GetValueName(Val : ValueRef) return Cstring;
   procedure SetValueName(Val : ValueRef; Name : Cstring);
   procedure DumpValue(Val : ValueRef);

   -- Operations on constants of any type
   function ConstNull(Ty : TypeRef) return ValueRef; --  All zero
   function ConstAllOnes(Ty : TypeRef) return ValueRef; -- Int or Vec
   function GetUndef(Ty : TypeRef) return ValueRef;
   function IsConstant(Val : ValueRef) return int;
   function IsNull(Val : ValueRef) return int;
   function IsUndef(Val : ValueRef) return int;

   --  Convert value instances between types.
   --
   --  Internally, an LLVMValueRef is "pinned" to a specific type. This
   --  series of functions allows you to cast an instance to a specific
   --  type.
   --
   --  If the cast is not valid for the specified type, NULL is returned.
   --
   -- @see llvm::dyn_cast_or_null<>
   function IsAInstruction (Val : ValueRef) return ValueRef;

   -- Operations on scalar constants
   function ConstInt(IntTy : TypeRef; N : Unsigned_64; SignExtend : int)
                    return ValueRef;
   function ConstReal(RealTy : TypeRef; N : double) return ValueRef;
   function ConstRealOfString(RealTy : TypeRef; Text : Cstring)
                             return ValueRef;


   -- Obtain the zero extended value for an integer constant value.
   -- @see llvm::ConstantInt::getZExtValue()
   function ConstIntGetZExtValue (ConstantVal : ValueRef) return Unsigned_64;

   -- Operations on composite constants
   function ConstString(Str : Cstring;
                        Length : unsigned; DontNullTerminate : int)
                       return ValueRef;
   function ConstArray(ElementTy : TypeRef;
                       ConstantVals : ValueRefArray; Length : unsigned)
                      return ValueRef;
   function ConstStruct(ConstantVals : ValueRefArray;
                        Count : unsigned; packed : int) return ValueRef;

   --  Create a non-anonymous ConstantStruct from values.
   --  @see llvm::ConstantStruct::get()
   function ConstNamedStruct(StructTy : TypeRef;
                             ConstantVals : ValueRefArray;
                             Count : unsigned) return ValueRef;

   function ConstVector(ScalarConstantVals : ValueRefArray; Size : unsigned)
                       return ValueRef;

   -- Constant expressions
   function SizeOf(Ty : TypeRef) return ValueRef;
   function AlignOf(Ty : TypeRef) return ValueRef;

   function ConstNeg(ConstantVal : ValueRef) return ValueRef;
   function ConstNot(ConstantVal : ValueRef) return ValueRef;
   function ConstAdd(LHSConstant : ValueRef; RHSConstant : ValueRef)
                    return ValueRef;
   function ConstSub(LHSConstant : ValueRef; RHSConstant : ValueRef)
                    return ValueRef;
   function ConstMul(LHSConstant : ValueRef; RHSConstant : ValueRef)
                    return ValueRef;
   function ConstUDiv(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstSDiv(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstFDiv(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstURem(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstSRem(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstFRem(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstAnd(LHSConstant : ValueRef; RHSConstant : ValueRef)
                    return ValueRef;
   function ConstOr(LHSConstant : ValueRef; RHSConstant : ValueRef)
                   return ValueRef;
   function ConstXor(LHSConstant : ValueRef; RHSConstant : ValueRef)
                    return ValueRef;
   function ConstICmp(Predicate : IntPredicate;
                      LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstFCmp(Predicate : RealPredicate;
                      LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstShl(LHSConstant : ValueRef; RHSConstant : ValueRef)
                    return ValueRef;
   function ConstLShr(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstAShr(LHSConstant : ValueRef; RHSConstant : ValueRef)
                     return ValueRef;
   function ConstGEP(ConstantVal : ValueRef;
                     ConstantIndices : ValueRefArray; NumIndices : unsigned)
                    return ValueRef;
   function ConstTrunc(ConstantVal : ValueRef; ToType : TypeRef)
                      return ValueRef;
   function ConstSExt(ConstantVal : ValueRef; ToType : TypeRef)
                     return ValueRef;
   function ConstZExt(ConstantVal : ValueRef; ToType : TypeRef)
                     return ValueRef;
   function ConstFPTrunc(ConstantVal : ValueRef; ToType : TypeRef)
                        return ValueRef;
   function ConstFPExt(ConstantVal : ValueRef; ToType : TypeRef)
                      return ValueRef;
   function ConstUIToFP(ConstantVal : ValueRef; ToType : TypeRef)
                       return ValueRef;
   function ConstSIToFP(ConstantVal : ValueRef; ToType : TypeRef)
                       return ValueRef;
   function ConstFPToUI(ConstantVal : ValueRef; ToType : TypeRef)
                       return ValueRef;
   function ConstFPToSI(ConstantVal : ValueRef; ToType : TypeRef)
                       return ValueRef;
   function ConstPtrToInt(ConstantVal : ValueRef; ToType : TypeRef)
                         return ValueRef;
   function ConstIntToPtr(ConstantVal : ValueRef; ToType : TypeRef)
                         return ValueRef;
   function ConstBitCast(ConstantVal : ValueRef; ToType : TypeRef)
                        return ValueRef;

   function ConstTruncOrBitCast(ConstantVal : ValueRef; ToType : TypeRef)
                               return ValueRef;

   function ConstSelect(ConstantCondition : ValueRef;
                        ConstantIfTrue : ValueRef;
                        ConstantIfFalse : ValueRef) return ValueRef;
   function ConstExtractElement(VectorConstant : ValueRef;
                                IndexConstant : ValueRef) return ValueRef;
   function ConstInsertElement(VectorConstant : ValueRef;
                               ElementValueConstant : ValueRef;
                               IndexConstant : ValueRef) return ValueRef;
   function ConstShuffleVector(VectorAConstant : ValueRef;
                               VectorBConstant : ValueRef;
                               MaskConstant : ValueRef) return ValueRef;

   -- Operations on global variables, functions, and aliases (globals)
   function GetGlobalParent(Global : ValueRef) return ModuleRef;
   function IsDeclaration(Global : ValueRef) return int;
   function GetLinkage(Global : ValueRef) return Linkage;
   procedure SetLinkage(Global : ValueRef; Link : Linkage);
   function GetSection(Global : ValueRef) return Cstring;
   procedure SetSection(Global : ValueRef; Section : Cstring);
   function GetVisibility(Global : ValueRef) return Visibility;
   procedure SetVisibility(Global : ValueRef; Viz : Visibility);
   function GetAlignment(Global : ValueRef) return unsigned;
   procedure SetAlignment(Global : ValueRef; Bytes : unsigned);

   -- Operations on global variables
   function AddGlobal(M : ModuleRef; Ty : TypeRef; Name : Cstring)
                     return ValueRef;
   function GetNamedGlobal(M : ModuleRef; Name : Cstring) return ValueRef;
   function GetFirstGlobal(M : ModuleRef) return ValueRef;
   function GetLastGlobal(M : ModuleRef) return ValueRef;
   function GetNextGlobal(GlobalVar : ValueRef) return ValueRef;
   function GetPreviousGlobal(GlobalVar : ValueRef) return ValueRef;
   procedure DeleteGlobal(GlobalVar : ValueRef);
   function GetInitializer(GlobalVar : ValueRef) return ValueRef;
   procedure SetInitializer(GlobalVar : ValueRef; ConstantVal : ValueRef);
   function IsThreadLocal(GlobalVar : ValueRef) return int;
   procedure SetThreadLocal(GlobalVar : ValueRef; IsThreadLocal : int);
   function IsGlobalConstant(GlobalVar : ValueRef) return int;
   procedure SetGlobalConstant(GlobalVar : ValueRef; IsConstant : int);

   -- Operations on functions
   function AddFunction(M : ModuleRef; Name : Cstring; FunctionTy : TypeRef)
                       return ValueRef;
   function GetNamedFunction(M : ModuleRef; Name : Cstring) return ValueRef;
   function GetFirstFunction(M : ModuleRef) return ValueRef;
   function GetLastFunction(M : ModuleRef) return ValueRef;
   function GetNextFunction(Fn : ValueRef) return ValueRef;
   function GetPreviousFunction(Fn : ValueRef) return ValueRef;
   procedure DeleteFunction(Fn : ValueRef);
   function GetIntrinsicID(Fn : ValueRef) return unsigned;
   function GetFunctionCallConv(Fn : ValueRef) return CallConv;
   procedure SetFunctionCallConv(Fn : ValueRef; CC : CallConv);
   function GetGC(Fn : ValueRef) return Cstring;
   procedure SetGC(Fn : ValueRef; Name : Cstring);

   --  Add an attribute to a function.
   --  @see llvm::Function::addAttribute()
   procedure AddFunctionAttr (Fn : ValueRef; PA : Attribute);

   --  Add a target-dependent attribute to a fuction
   --  @see llvm::AttrBuilder::addAttribute()
   procedure AddTargetDependentFunctionAttr
     (Fn : ValueRef; A : Cstring; V : Cstring);

   --  Obtain an attribute from a function.
   --  @see llvm::Function::getAttributes()
   function GetFunctionAttr (Fn : ValueRef) return Attribute;

   --  Remove an attribute from a function.
   procedure RemoveFunctionAttr (Fn : ValueRef; PA : Attribute);

   -- Operations on parameters
   function CountParams(Fn : ValueRef) return unsigned;
   procedure GetParams(Fn : ValueRef; Params : ValueRefArray);
   function GetParam(Fn : ValueRef; Index : unsigned) return ValueRef;
   function GetParamParent(Inst : ValueRef) return ValueRef;
   function GetFirstParam(Fn : ValueRef) return ValueRef;
   function GetLastParam(Fn : ValueRef) return ValueRef;
   function GetNextParam(Arg : ValueRef) return ValueRef;
   function GetPreviousParam(Arg : ValueRef) return ValueRef;
   procedure AddAttribute(Arg : ValueRef; PA : Attribute);
   procedure RemoveAttribute(Arg : ValueRef; PA : Attribute);
   procedure SetParamAlignment(Arg : ValueRef; align : unsigned);

   -- Operations on basic blocks
   function BasicBlockAsValue(BB : BasicBlockRef) return ValueRef;
   function ValueIsBasicBlock(Val : ValueRef) return int;
   function ValueAsBasicBlock(Val : ValueRef) return BasicBlockRef;
   function GetBasicBlockParent(BB : BasicBlockRef) return ValueRef;
   function CountBasicBlocks(Fn : ValueRef) return unsigned;
   procedure GetBasicBlocks(Fn : ValueRef; BasicBlocks : BasicBlockRefArray);
   function GetFirstBasicBlock(Fn : ValueRef) return BasicBlockRef;
   function GetLastBasicBlock(Fn : ValueRef) return BasicBlockRef;
   function GetNextBasicBlock(BB : BasicBlockRef) return BasicBlockRef;
   function GetPreviousBasicBlock(BB : BasicBlockRef) return BasicBlockRef;
   function GetEntryBasicBlock(Fn : ValueRef) return BasicBlockRef;
   function AppendBasicBlock(Fn : ValueRef; Name : Cstring)
                            return BasicBlockRef;
   function InsertBasicBlock(InsertBeforeBB : BasicBlockRef;
                             Name : Cstring) return BasicBlockRef;
   procedure DeleteBasicBlock(BB : BasicBlockRef);

   -- Operations on instructions

   --  Determine whether an instruction has any metadata attached.
   function HasMetadata(Val: ValueRef) return Bool;

   --  Return metadata associated with an instruction value.
   function GetMetadata(Val : ValueRef; KindID : unsigned) return ValueRef;

   --  Set metadata associated with an instruction value.
   procedure SetMetadata(Val : ValueRef; KindID : unsigned; Node : ValueRef);

   function GetInstructionParent(Inst : ValueRef) return BasicBlockRef;
   function GetFirstInstruction(BB : BasicBlockRef) return ValueRef;
   function GetLastInstruction(BB : BasicBlockRef) return ValueRef;
   function GetNextInstruction(Inst : ValueRef) return ValueRef;
   function GetPreviousInstruction(Inst : ValueRef) return ValueRef;

   -- Operations on call sites
   procedure SetInstructionCallConv(Instr : ValueRef; CC : unsigned);
   function GetInstructionCallConv(Instr : ValueRef) return unsigned;
   procedure AddInstrAttribute(Instr : ValueRef;
                               index : unsigned; Attr : Attribute);
   procedure RemoveInstrAttribute(Instr : ValueRef;
                                 index : unsigned; Attr : Attribute);
   procedure SetInstrParamAlignment(Instr : ValueRef;
                                    index : unsigned; align : unsigned);

   -- Operations on call instructions (only)
   function IsTailCall(CallInst : ValueRef) return int;
   procedure SetTailCall(CallInst : ValueRef; IsTailCall : int);

   -- Operations on phi nodes
   procedure AddIncoming(PhiNode : ValueRef; IncomingValues : ValueRefArray;
                        IncomingBlocks : BasicBlockRefArray; Count : unsigned);
   function CountIncoming(PhiNode : ValueRef) return unsigned;
   function GetIncomingValue(PhiNode : ValueRef; Index : unsigned)
                            return ValueRef;
   function GetIncomingBlock(PhiNode : ValueRef; Index : unsigned)
                            return BasicBlockRef;

   -- Instruction builders ----------------------------------------------
   --  An instruction builder represents a point within a basic block,
   --  and is the exclusive means of building instructions using the C
   --  interface.

   function CreateBuilder return BuilderRef;
   procedure PositionBuilder(Builder : BuilderRef;
                             Block : BasicBlockRef; Instr : ValueRef);
   procedure PositionBuilderBefore(Builder : BuilderRef; Instr : ValueRef);
   procedure PositionBuilderAtEnd(Builder : BuilderRef; Block : BasicBlockRef);
   function GetInsertBlock(Builder : BuilderRef) return BasicBlockRef;
   procedure DisposeBuilder(Builder : BuilderRef);

   -- Terminators
   function BuildRetVoid(Builder : BuilderRef) return ValueRef;
   function BuildRet(Builder : BuilderRef; V : ValueRef) return ValueRef;
   function BuildBr(Builder : BuilderRef; Dest : BasicBlockRef)
                   return ValueRef;
   function BuildCondBr(Builder : BuilderRef;
                        If_Br : ValueRef;
                        Then_Br : BasicBlockRef; Else_Br : BasicBlockRef)
                       return ValueRef;
   function BuildSwitch(Builder : BuilderRef;
                        V : ValueRef;
                        Else_Br : BasicBlockRef; NumCases : unsigned)
                       return ValueRef;
   function BuildInvoke(Builder : BuilderRef;
                        Fn : ValueRef;
                        Args : ValueRefArray;
                        NumArgs : unsigned;
                        Then_Br : BasicBlockRef;
                        Catch : BasicBlockRef;
                        Name : Cstring) return ValueRef;
   function BuildUnwind(Builder : BuilderRef) return ValueRef;
   function BuildUnreachable(Builder : BuilderRef) return ValueRef;

   -- Add a case to the switch instruction
   procedure AddCase(Switch : ValueRef;
                     OnVal : ValueRef; Dest : BasicBlockRef);

   -- Arithmetic
   function BuildAdd(Builder : BuilderRef;
                     LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildNSWAdd(Builder : BuilderRef;
                        LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                       return ValueRef;
   function BuildNUWAdd(Builder : BuilderRef;
                        LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                       return ValueRef;
   function BuildFAdd(Builder : BuilderRef;
                        LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                       return ValueRef;

   function BuildSub(Builder : BuilderRef;
                     LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildNSWSub(Builder : BuilderRef;
                        LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                       return ValueRef;
   function BuildNUWSub(Builder : BuilderRef;
                        LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                       return ValueRef;
   function BuildFSub(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;

   function BuildMul(Builder : BuilderRef;
                     LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildFMul(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;

   function BuildUDiv(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildSDiv(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildFDiv(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildURem(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildSRem(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildFRem(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildShl(Builder : BuilderRef;
                     LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildLShr(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildAShr(Builder : BuilderRef;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildAnd(Builder : BuilderRef;
                     LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildOr(Builder : BuilderRef;
                    LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                   return ValueRef;
   function BuildXor(Builder : BuilderRef;
                     LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildNeg(Builder : BuilderRef; V : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildFNeg(Builder : BuilderRef; V : ValueRef; Name : Cstring)
                    return ValueRef;
   function BuildNot(Builder : BuilderRef; V : ValueRef; Name : Cstring)
                    return ValueRef;

   -- Memory
   function BuildMalloc(Builder : BuilderRef; Ty : TypeRef; Name : Cstring)
                       return ValueRef;
   function BuildArrayMalloc(Builder : BuilderRef;
                             Ty : TypeRef; Val : ValueRef; Name : Cstring)
                            return ValueRef;
   function BuildAlloca(Builder : BuilderRef; Ty : TypeRef; Name : Cstring)
                       return ValueRef;
   function BuildArrayAlloca(Builder : BuilderRef;
                             Ty : TypeRef; Val : ValueRef; Name : Cstring)
                            return ValueRef;
   function BuildFree(Builder : BuilderRef; PointerVal : ValueRef)
                     return ValueRef;
   function BuildLoad(Builder : BuilderRef; PointerVal : ValueRef;
                           Name : Cstring) return ValueRef;
   function BuildStore(Builder : BuilderRef; Val : ValueRef; Ptr : ValueRef)
                      return ValueRef;
   function BuildGEP(Builder : BuilderRef;
                     Pointer : ValueRef;
                     Indices : ValueRefArray;
                     NumIndices : unsigned; Name : Cstring) return ValueRef;

   -- Casts
   function BuildTrunc(Builder : BuilderRef;
                       Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                      return ValueRef;
   function BuildZExt(Builder : BuilderRef;
                      Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                     return ValueRef;
   function BuildSExt(Builder : BuilderRef;
                      Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                     return ValueRef;
   function BuildFPToUI(Builder : BuilderRef;
                        Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                       return ValueRef;
   function BuildFPToSI(Builder : BuilderRef;
                        Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                       return ValueRef;
   function BuildUIToFP(Builder : BuilderRef;
                        Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                       return ValueRef;
   function BuildSIToFP(Builder : BuilderRef;
                        Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                       return ValueRef;
   function BuildFPTrunc(Builder : BuilderRef;
                         Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                        return ValueRef;
   function BuildFPExt(Builder : BuilderRef;
                       Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                      return ValueRef;
   function BuildPtrToInt(Builder : BuilderRef;
                          Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                         return ValueRef;
   function BuildIntToPtr(Builder : BuilderRef;
                          Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                         return ValueRef;
   function BuildBitCast(Builder : BuilderRef;
                         Val : ValueRef; DestTy : TypeRef; Name : Cstring)
                        return ValueRef;

   -- Comparisons
   function BuildICmp(Builder : BuilderRef;
                      Op : IntPredicate;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;
   function BuildFCmp(Builder : BuilderRef;
                      Op : RealPredicate;
                      LHS : ValueRef; RHS : ValueRef; Name : Cstring)
                     return ValueRef;

   -- Miscellaneous instructions
   function BuildPhi(Builder : BuilderRef; Ty : TypeRef; Name : Cstring)
                    return ValueRef;
   function BuildCall(Builder : BuilderRef;
                      Fn : ValueRef;
                      Args : ValueRefArray; NumArgs : unsigned; Name : Cstring)
                     return ValueRef;
   function BuildSelect(Builder : BuilderRef;
                        If_Sel : ValueRef;
                        Then_Sel : ValueRef;
                        Else_Sel : ValueRef;
                        Name : Cstring) return ValueRef;
   function BuildVAArg(Builder : BuilderRef;
                       List : ValueRef; Ty : TypeRef; Name : Cstring)
                      return ValueRef;
   function BuildExtractElement(Builder : BuilderRef;
                                VecVal : ValueRef;
                                Index : ValueRef;
                                Name : Cstring) return ValueRef;
   function BuildInsertElement(Builder : BuilderRef;
                               VecVal : ValueRef;
                               EltVal : ValueRef;
                               Index : ValueRef;
                               Name : Cstring) return ValueRef;
   function BuildShuffleVector(Builder : BuilderRef;
                               V1 : ValueRef;
                               V2 : ValueRef;
                               Mask : ValueRef;
                               Name : Cstring) return ValueRef;

   -- Memory buffers ----------------------------------------------------

   function CreateMemoryBufferWithContentsOfFile
     (Path : Cstring;
      OutMemBuf : access MemoryBufferRef;
      OutMessage : access Cstring) return int;
   function CreateMemoryBufferWithSTDIN
     (OutMemBuf : access MemoryBufferRef;
      OutMessage : access Cstring) return int;
   procedure DisposeMemoryBuffer(MemBuf : MemoryBufferRef);


   -- Pass Managers -----------------------------------------------------

   -- Constructs a new whole-module pass pipeline. This type of pipeline is
   -- suitable for link-time optimization and whole-module transformations.
   -- See llvm::PassManager::PassManager.
   function CreatePassManager return PassManagerRef;

   -- Constructs a new function-by-function pass pipeline over the module
   -- provider. It does not take ownership of the module provider. This type of
   -- pipeline is suitable for code generation and JIT compilation tasks.
   -- See llvm::FunctionPassManager::FunctionPassManager.
   function CreateFunctionPassManagerForModule(M : ModuleRef)
                                              return PassManagerRef;

   -- Initializes, executes on the provided module, and finalizes all of the
   -- passes scheduled in the pass manager. Returns 1 if any of the passes
   -- modified the module, 0 otherwise. See llvm::PassManager::run(Module&).
   function RunPassManager(PM : PassManagerRef; M : ModuleRef)
                          return int;

   -- Initializes all of the function passes scheduled in the function pass
   -- manager. Returns 1 if any of the passes modified the module, 0 otherwise.
   -- See llvm::FunctionPassManager::doInitialization.
   function InitializeFunctionPassManager(FPM : PassManagerRef)
                                         return int;

   --  Executes all of the function passes scheduled in the function
   --  pass manager on the provided function. Returns 1 if any of the
   --  passes modified the function, false otherwise.
   -- See llvm::FunctionPassManager::run(Function&).
   function RunFunctionPassManager (FPM : PassManagerRef; F : ValueRef)
                                   return int;

   -- Finalizes all of the function passes scheduled in in the function pass
   -- manager. Returns 1 if any of the passes modified the module, 0 otherwise.
   -- See llvm::FunctionPassManager::doFinalization.
   function FinalizeFunctionPassManager(FPM : PassManagerRef)
                                       return int;

   --  Frees the memory of a pass pipeline. For function pipelines,
   --  does not free the module provider.
   -- See llvm::PassManagerBase::~PassManagerBase.
   procedure DisposePassManager(PM : PassManagerRef);

private
   pragma Import (C, ContextCreate, "LLVMContextCreate");
   pragma Import (C, GetGlobalContext, "LLVMGetGlobalContext");
   pragma Import (C, ContextDispose, "LLVMContextDispose");

   pragma Import (C, DisposeMessage, "LLVMDisposeMessage");
   pragma Import (C, ModuleCreateWithName, "LLVMModuleCreateWithName");
   pragma Import (C, DisposeModule, "LLVMDisposeModule");
   pragma Import (C, GetDataLayout, "LLVMGetDataLayout");
   pragma Import (C, SetDataLayout, "LLVMSetDataLayout");
   pragma Import (C, GetTarget, "LLVMGetTarget");
   pragma Import (C, SetTarget, "LLVMSetTarget");
   pragma Import (C, DumpModule, "LLVMDumpModule");
   pragma Import (C, PrintModuleToFile, "LLVMPrintModuleToFile");
   pragma Import (C, GetTypeKind, "LLVMGetTypeKind");
   pragma Import (C, Int1Type, "LLVMInt1Type");
   pragma Import (C, Int8Type, "LLVMInt8Type");
   pragma Import (C, Int16Type, "LLVMInt16Type");
   pragma Import (C, Int32Type, "LLVMInt32Type");
   pragma Import (C, Int64Type, "LLVMInt64Type");
   pragma Import (C, IntType, "LLVMIntType");
   pragma Import (C, GetIntTypeWidth, "LLVMGetIntTypeWidth");
   pragma Import (C, MetadataType, "LLVMMetadataType_extra");

   pragma Import (C, FloatType, "LLVMFloatType");
   pragma Import (C, DoubleType, "LLVMDoubleType");
   pragma Import (C, X86FP80Type, "LLVMX86FP80Type");
   pragma Import (C, FP128Type, "LLVMFP128Type");
   pragma Import (C, PPCFP128Type, "LLVMPPCFP128Type");

   pragma Import (C, FunctionType, "LLVMFunctionType");
   pragma Import (C, IsFunctionVarArg, "LLVMIsFunctionVarArg");
   pragma Import (C, GetReturnType, "LLVMGetReturnType");
   pragma Import (C, CountParamTypes, "LLVMCountParamTypes");
   pragma Import (C, GetParamTypes, "LLVMGetParamTypes");

   pragma Import (C, StructType, "LLVMStructType");
   pragma Import (C, StructCreateNamed, "LLVMStructCreateNamed");
   pragma Import (C, StructSetBody, "LLVMStructSetBody");
   pragma Import (C, CountStructElementTypes, "LLVMCountStructElementTypes");
   pragma Import (C, GetStructElementTypes, "LLVMGetStructElementTypes");
   pragma Import (C, IsPackedStruct, "LLVMIsPackedStruct");

   pragma Import (C, ArrayType, "LLVMArrayType");
   pragma Import (C, PointerType, "LLVMPointerType");
   pragma Import (C, VectorType, "LLVMVectorType");
   pragma Import (C, GetElementType, "LLVMGetElementType");
   pragma Import (C, GetArrayLength, "LLVMGetArrayLength");
   pragma Import (C, GetPointerAddressSpace, "LLVMGetPointerAddressSpace");
   pragma Import (C, GetVectorSize, "LLVMGetVectorSize");

   pragma Import (C, VoidType, "LLVMVoidType");
   pragma Import (C, LabelType, "LLVMLabelType");
   pragma Import (C, DumpType, "LLVMDumpType");

   pragma Import (C, TypeOf, "LLVMTypeOf");
   pragma Import (C, GetValueName, "LLVMGetValueName");
   pragma Import (C, SetValueName, "LLVMSetValueName");
   pragma Import (C, DumpValue, "LLVMDumpValue");

   pragma Import (C, ConstNull, "LLVMConstNull");
   pragma Import (C, ConstAllOnes, "LLVMConstAllOnes");
   pragma Import (C, GetUndef, "LLVMGetUndef");
   pragma Import (C, IsConstant, "LLVMIsConstant");
   pragma Import (C, IsNull, "LLVMIsNull");
   pragma Import (C, IsUndef, "LLVMIsUndef");
   pragma Import (C, IsAInstruction, "LLVMIsAInstruction");

   pragma Import (C, ConstInt, "LLVMConstInt");
   pragma Import (C, ConstReal, "LLVMConstReal");
   pragma Import (C, ConstIntGetZExtValue, "LLVMConstIntGetZExtValue");
   pragma Import (C, ConstRealOfString, "LLVMConstRealOfString");
   pragma Import (C, ConstString, "LLVMConstString");
   pragma Import (C, ConstArray, "LLVMConstArray");
   pragma Import (C, ConstStruct, "LLVMConstStruct");
   pragma Import (C, ConstNamedStruct, "LLVMConstNamedStruct");
   pragma Import (C, ConstVector, "LLVMConstVector");

   pragma Import (C, SizeOf, "LLVMSizeOf");
   pragma Import (C, AlignOf, "LLVMAlignOf");
   pragma Import (C, ConstNeg, "LLVMConstNeg");
   pragma Import (C, ConstNot, "LLVMConstNot");
   pragma Import (C, ConstAdd, "LLVMConstAdd");
   pragma Import (C, ConstSub, "LLVMConstSub");
   pragma Import (C, ConstMul, "LLVMConstMul");
   pragma Import (C, ConstUDiv, "LLVMConstUDiv");
   pragma Import (C, ConstSDiv, "LLVMConstSDiv");
   pragma Import (C, ConstFDiv, "LLVMConstFDiv");
   pragma Import (C, ConstURem, "LLVMConstURem");
   pragma Import (C, ConstSRem, "LLVMConstSRem");
   pragma Import (C, ConstFRem, "LLVMConstFRem");
   pragma Import (C, ConstAnd, "LLVMConstAnd");
   pragma Import (C, ConstOr, "LLVMConstOr");
   pragma Import (C, ConstXor, "LLVMConstXor");
   pragma Import (C, ConstICmp, "LLVMConstICmp");
   pragma Import (C, ConstFCmp, "LLVMConstFCmp");
   pragma Import (C, ConstShl, "LLVMConstShl");
   pragma Import (C, ConstLShr, "LLVMConstLShr");
   pragma Import (C, ConstAShr, "LLVMConstAShr");
   pragma Import (C, ConstGEP, "LLVMConstGEP");
   pragma Import (C, ConstTrunc, "LLVMConstTrunc");
   pragma Import (C, ConstSExt, "LLVMConstSExt");
   pragma Import (C, ConstZExt, "LLVMConstZExt");
   pragma Import (C, ConstFPTrunc, "LLVMConstFPTrunc");
   pragma Import (C, ConstFPExt, "LLVMConstFPExt");
   pragma Import (C, ConstUIToFP, "LLVMConstUIToFP");
   pragma Import (C, ConstSIToFP, "LLVMConstSIToFP");
   pragma Import (C, ConstFPToUI, "LLVMConstFPToUI");
   pragma Import (C, ConstFPToSI, "LLVMConstFPToSI");
   pragma Import (C, ConstPtrToInt, "LLVMConstPtrToInt");
   pragma Import (C, ConstIntToPtr, "LLVMConstIntToPtr");
   pragma Import (C, ConstBitCast, "LLVMConstBitCast");
   pragma Import (C, ConstTruncOrBitCast, "LLVMConstTruncOrBitCast");
   pragma Import (C, ConstSelect, "LLVMConstSelect");
   pragma Import (C, ConstExtractElement, "LLVMConstExtractElement");
   pragma Import (C, ConstInsertElement, "LLVMConstInsertElement");
   pragma Import (C, ConstShuffleVector, "LLVMConstShuffleVector");

   pragma Import (C, GetGlobalParent, "LLVMGetGlobalParent");
   pragma Import (C, IsDeclaration, "LLVMIsDeclaration");
   pragma Import (C, GetLinkage, "LLVMGetLinkage");
   pragma Import (C, SetLinkage, "LLVMSetLinkage");
   pragma Import (C, GetSection, "LLVMGetSection");
   pragma Import (C, SetSection, "LLVMSetSection");
   pragma Import (C, GetVisibility, "LLVMGetVisibility");
   pragma Import (C, SetVisibility, "LLVMSetVisibility");
   pragma Import (C, GetAlignment, "LLVMGetAlignment");
   pragma Import (C, SetAlignment, "LLVMSetAlignment");

   pragma Import (C, AddGlobal, "LLVMAddGlobal");
   pragma Import (C, GetNamedGlobal, "LLVMGetNamedGlobal");
   pragma Import (C, GetFirstGlobal, "LLVMGetFirstGlobal");
   pragma Import (C, GetLastGlobal, "LLVMGetLastGlobal");
   pragma Import (C, GetNextGlobal, "LLVMGetNextGlobal");
   pragma Import (C, GetPreviousGlobal, "LLVMGetPreviousGlobal");
   pragma Import (C, DeleteGlobal, "LLVMDeleteGlobal");
   pragma Import (C, GetInitializer, "LLVMGetInitializer");
   pragma Import (C, SetInitializer, "LLVMSetInitializer");
   pragma Import (C, IsThreadLocal, "LLVMIsThreadLocal");
   pragma Import (C, SetThreadLocal, "LLVMSetThreadLocal");
   pragma Import (C, IsGlobalConstant, "LLVMIsGlobalConstant");
   pragma Import (C, SetGlobalConstant, "LLVMSetGlobalConstant");

   pragma Import (C, AddFunction, "LLVMAddFunction");
   pragma Import (C, GetNamedFunction, "LLVMGetNamedFunction");
   pragma Import (C, GetFirstFunction, "LLVMGetFirstFunction");
   pragma Import (C, GetLastFunction, "LLVMGetLastFunction");
   pragma Import (C, GetNextFunction, "LLVMGetNextFunction");
   pragma Import (C, GetPreviousFunction, "LLVMGetPreviousFunction");
   pragma Import (C, DeleteFunction, "LLVMDeleteFunction");
   pragma Import (C, GetIntrinsicID, "LLVMGetIntrinsicID");
   pragma Import (C, GetFunctionCallConv, "LLVMGetFunctionCallConv");
   pragma Import (C, SetFunctionCallConv, "LLVMSetFunctionCallConv");
   pragma Import (C, GetGC, "LLVMGetGC");
   pragma Import (C, SetGC, "LLVMSetGC");

   pragma Import (C, AddFunctionAttr, "LLVMAddFunctionAttr");
   pragma import (C, AddTargetDependentFunctionAttr,
                  "LLVMAddTargetDependentFunctionAttr");
   pragma Import (C, GetFunctionAttr, "LLVMGetFunctionAttr");
   pragma Import (C, RemoveFunctionAttr, "LLVMRemoveFunctionAttr");

   pragma Import (C, CountParams, "LLVMCountParams");
   pragma Import (C, GetParams, "LLVMGetParams");
   pragma Import (C, GetParam, "LLVMGetParam");
   pragma Import (C, GetParamParent, "LLVMGetParamParent");
   pragma Import (C, GetFirstParam, "LLVMGetFirstParam");
   pragma Import (C, GetLastParam, "LLVMGetLastParam");
   pragma Import (C, GetNextParam, "LLVMGetNextParam");
   pragma Import (C, GetPreviousParam, "LLVMGetPreviousParam");
   pragma Import (C, AddAttribute, "LLVMAddAttribute");
   pragma Import (C, RemoveAttribute, "LLVMRemoveAttribute");
   pragma Import (C, SetParamAlignment, "LLVMSetParamAlignment");

   pragma Import (C, BasicBlockAsValue, "LLVMBasicBlockAsValue");
   pragma Import (C, ValueIsBasicBlock, "LLVMValueIsBasicBlock");
   pragma Import (C, ValueAsBasicBlock, "LLVMValueAsBasicBlock");
   pragma Import (C, GetBasicBlockParent, "LLVMGetBasicBlockParent");
   pragma Import (C, CountBasicBlocks, "LLVMCountBasicBlocks");
   pragma Import (C, GetBasicBlocks, "LLVMGetBasicBlocks");
   pragma Import (C, GetFirstBasicBlock, "LLVMGetFirstBasicBlock");
   pragma Import (C, GetLastBasicBlock, "LLVMGetLastBasicBlock");
   pragma Import (C, GetNextBasicBlock, "LLVMGetNextBasicBlock");
   pragma Import (C, GetPreviousBasicBlock, "LLVMGetPreviousBasicBlock");
   pragma Import (C, GetEntryBasicBlock, "LLVMGetEntryBasicBlock");
   pragma Import (C, AppendBasicBlock, "LLVMAppendBasicBlock");
   pragma Import (C, InsertBasicBlock, "LLVMInsertBasicBlock");
   pragma Import (C, DeleteBasicBlock, "LLVMDeleteBasicBlock");

   pragma Import (C, HasMetadata, "LLVMHasMetadata");
   pragma Import (C, GetMetadata, "LLVMGetMetadata");
   pragma Import (C, SetMetadata, "LLVMSetMetadata");

   pragma Import (C, GetInstructionParent, "LLVMGetInstructionParent");
   pragma Import (C, GetFirstInstruction, "LLVMGetFirstInstruction");
   pragma Import (C, GetLastInstruction, "LLVMGetLastInstruction");
   pragma Import (C, GetNextInstruction, "LLVMGetNextInstruction");
   pragma Import (C, GetPreviousInstruction, "LLVMGetPreviousInstruction");

   pragma Import (C, SetInstructionCallConv, "LLVMSetInstructionCallConv");
   pragma Import (C, GetInstructionCallConv, "LLVMGetInstructionCallConv");
   pragma Import (C, AddInstrAttribute, "LLVMAddInstrAttribute");
   pragma Import (C, RemoveInstrAttribute, "LLVMRemoveInstrAttribute");
   pragma Import (C, SetInstrParamAlignment, "LLVMSetInstrParamAlignment");

   pragma Import (C, IsTailCall, "LLVMIsTailCall");
   pragma Import (C, SetTailCall, "LLVMSetTailCall");

   pragma Import (C, AddIncoming, "LLVMAddIncoming");
   pragma Import (C, CountIncoming, "LLVMCountIncoming");
   pragma Import (C, GetIncomingValue, "LLVMGetIncomingValue");
   pragma Import (C, GetIncomingBlock, "LLVMGetIncomingBlock");

   pragma Import (C, CreateBuilder, "LLVMCreateBuilder");
   pragma Import (C, PositionBuilder, "LLVMPositionBuilder");
   pragma Import (C, PositionBuilderBefore, "LLVMPositionBuilderBefore");
   pragma Import (C, PositionBuilderAtEnd, "LLVMPositionBuilderAtEnd");
   pragma Import (C, GetInsertBlock, "LLVMGetInsertBlock");
   pragma Import (C, DisposeBuilder, "LLVMDisposeBuilder");

   -- Terminators
   pragma Import (C, BuildRetVoid, "LLVMBuildRetVoid");
   pragma Import (C, BuildRet, "LLVMBuildRet");
   pragma Import (C, BuildBr, "LLVMBuildBr");
   pragma Import (C, BuildCondBr, "LLVMBuildCondBr");
   pragma Import (C, BuildSwitch, "LLVMBuildSwitch");
   pragma Import (C, BuildInvoke, "LLVMBuildInvoke");
   pragma Import (C, BuildUnwind, "LLVMBuildUnwind");
   pragma Import (C, BuildUnreachable, "LLVMBuildUnreachable");

   -- Add a case to the switch instruction
   pragma Import (C, AddCase, "LLVMAddCase");

   -- Arithmetic
   pragma Import (C, BuildAdd, "LLVMBuildAdd");
   pragma Import (C, BuildNSWAdd, "LLVMBuildNSWAdd");
   pragma Import (C, BuildNUWAdd, "LLVMBuildNUWAdd");
   pragma Import (C, BuildFAdd, "LLVMBuildFAdd");
   pragma Import (C, BuildSub, "LLVMBuildSub");
   pragma Import (C, BuildNSWSub, "LLVMBuildNSWSub");
   pragma Import (C, BuildNUWSub, "LLVMBuildNUWSub");
   pragma Import (C, BuildFSub, "LLVMBuildFSub");
   pragma Import (C, BuildMul, "LLVMBuildMul");
   pragma Import (C, BuildFMul, "LLVMBuildFMul");
   pragma Import (C, BuildUDiv, "LLVMBuildUDiv");
   pragma Import (C, BuildSDiv, "LLVMBuildSDiv");
   pragma Import (C, BuildFDiv, "LLVMBuildFDiv");
   pragma Import (C, BuildURem, "LLVMBuildURem");
   pragma Import (C, BuildSRem, "LLVMBuildSRem");
   pragma Import (C, BuildFRem, "LLVMBuildFRem");
   pragma Import (C, BuildShl, "LLVMBuildShl");
   pragma Import (C, BuildLShr, "LLVMBuildLShr");
   pragma Import (C, BuildAShr, "LLVMBuildAShr");
   pragma Import (C, BuildAnd, "LLVMBuildAnd");
   pragma Import (C, BuildOr, "LLVMBuildOr");
   pragma Import (C, BuildXor, "LLVMBuildXor");
   pragma Import (C, BuildNeg, "LLVMBuildNeg");
   pragma Import (C, BuildFNeg, "LLVMBuildFNeg");
   pragma Import (C, BuildNot, "LLVMBuildNot");

   -- Memory
   pragma Import (C, BuildMalloc, "LLVMBuildMalloc");
   pragma Import (C, BuildArrayMalloc, "LLVMBuildArrayMalloc");
   pragma Import (C, BuildAlloca, "LLVMBuildAlloca");
   pragma Import (C, BuildArrayAlloca, "LLVMBuildArrayAlloca");
   pragma Import (C, BuildFree, "LLVMBuildFree");
   pragma Import (C, BuildLoad, "LLVMBuildLoad");
   pragma Import (C, BuildStore, "LLVMBuildStore");
   pragma Import (C, BuildGEP, "LLVMBuildGEP");

   -- Casts
   pragma Import (C, BuildTrunc, "LLVMBuildTrunc");
   pragma Import (C, BuildZExt, "LLVMBuildZExt");
   pragma Import (C, BuildSExt, "LLVMBuildSExt");
   pragma Import (C, BuildFPToUI, "LLVMBuildFPToUI");
   pragma Import (C, BuildFPToSI, "LLVMBuildFPToSI");
   pragma Import (C, BuildUIToFP, "LLVMBuildUIToFP");
   pragma Import (C, BuildSIToFP, "LLVMBuildSIToFP");
   pragma Import (C, BuildFPTrunc, "LLVMBuildFPTrunc");
   pragma Import (C, BuildFPExt, "LLVMBuildFPExt");
   pragma Import (C, BuildPtrToInt, "LLVMBuildPtrToInt");
   pragma Import (C, BuildIntToPtr, "LLVMBuildIntToPtr");
   pragma Import (C, BuildBitCast, "LLVMBuildBitCast");

   -- Comparisons
   pragma Import (C, BuildICmp, "LLVMBuildICmp");
   pragma Import (C, BuildFCmp, "LLVMBuildFCmp");

   -- Miscellaneous instructions
   pragma Import (C, BuildPhi, "LLVMBuildPhi");
   pragma Import (C, BuildCall, "LLVMBuildCall");
   pragma Import (C, BuildSelect, "LLVMBuildSelect");
   pragma Import (C, BuildVAArg, "LLVMBuildVAArg");
   pragma Import (C, BuildExtractElement, "LLVMBuildExtractElement");
   pragma Import (C, BuildInsertElement, "LLVMBuildInsertElement");
   pragma Import (C, BuildShuffleVector, "LLVMBuildShuffleVector");

   -- Memory buffers ----------------------------------------------------
   pragma Import (C, CreateMemoryBufferWithContentsOfFile,
                  "LLVMCreateMemoryBufferWithContentsOfFile");
   pragma Import (C, CreateMemoryBufferWithSTDIN,
                  "LLVMCreateMemoryBufferWithSTDIN");
   pragma Import (C, DisposeMemoryBuffer, "LLVMDisposeMemoryBuffer");

   -- Pass Managers -----------------------------------------------------
   pragma Import (C, CreatePassManager, "LLVMCreatePassManager");
   pragma Import (C, CreateFunctionPassManagerForModule,
                  "LLVMCreateFunctionPassManagerForModule");
   pragma Import (C, RunPassManager, "LLVMRunPassManager");
   pragma Import (C, InitializeFunctionPassManager,
                  "LLVMInitializeFunctionPassManager");
   pragma Import (C, RunFunctionPassManager,
                  "LLVMRunFunctionPassManager");
   pragma Import (C, FinalizeFunctionPassManager,
                  "LLVMFinalizeFunctionPassManager");
   pragma Import (C, DisposePassManager, "LLVMDisposePassManager");

end LLVM.Core;
