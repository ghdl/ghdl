/*  LLVM binding
  Copyright (C) 2014 Tristan Gingold

  GHDL is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with GHDL; see the file COPYING.  If not, write to the Free
  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
  02111-1307, USA.  */
#include "llvm-c/Target.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Config/llvm-config.h"
#include "llvm-c/TargetMachine.h"
#include "llvm-c/Core.h"
#include "llvm-c/BitWriter.h"

#include "llvm-c/Analysis.h"
#include "llvm-c/Transforms/Scalar.h"
#if LLVM_VERSION_MAJOR >= 4
#include "llvm-c/Transforms/Utils.h"
#endif

#ifdef USE_DEBUG
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/Support/FileSystem.h"
#include <vector>
#endif

#if LLVM_VERSION_MAJOR >= 4
#define USE_ATTRIBUTES
#endif

using namespace llvm;

//  True if the LLVM output must be displayed (set by '--dump-llvm')
static bool FlagDumpLLVM = false;

//  Verify generated LLVM code.
static bool FlagVerifyLLVM = false;

static bool FlagDebugLines = true;

static LLVMModuleRef TheModule;
static LLVMTargetRef TheTarget;
static LLVMTargetMachineRef TheTargetMachine;
static LLVMTargetDataRef TheTargetData;
static LLVMRelocMode TheReloc = LLVMRelocDefault;
static LLVMCodeGenOptLevel Optimization = LLVMCodeGenLevelDefault;

static LLVMBuilderRef Builder;
static LLVMBuilderRef DeclBuilder;
static LLVMBuilderRef ExtraBuilder;

static LLVMValueRef StackSaveFun;
static LLVMValueRef StackRestoreFun;
static LLVMValueRef CopySignFun;

static LLVMValueRef Fp0_5;

#ifdef USE_ATTRIBUTES
static LLVMAttributeRef NounwindAttr;
static LLVMAttributeRef UwtableAttr;
#endif

static bool Unreach;

#ifdef USE_DEBUG
static unsigned DebugCurrentLine;
static std::string *DebugCurrentFilename;
static std::string *DebugCurrentDirectory;
static DIFile *DebugCurrentFile;

static DIBuilder *DBuilder;
#endif

extern "C" void
set_optimization_level (unsigned level)
{
  switch(level) {
  case 0:
    Optimization = LLVMCodeGenLevelNone;
    break;
  case 1:
    Optimization = LLVMCodeGenLevelLess;
    break;
  case 2:
    Optimization = LLVMCodeGenLevelDefault;
    break;
  default:
    Optimization = LLVMCodeGenLevelAggressive;
    break;
  }
}

extern "C" void
set_dump_llvm (unsigned Flag)
{
  FlagDumpLLVM = Flag != 0;
}

extern "C" void
set_verify_llvm (unsigned Flag)
{
  FlagVerifyLLVM = Flag != 0;
}

extern "C" void
set_pic_flag (unsigned Flag)
{
  TheReloc = Flag ? LLVMRelocPIC : LLVMRelocStatic;
}

static void
generateError(const char *Filename, char *Msg)
{
  fprintf(stderr, "error while writing to %s\n", Filename);
  if (Msg) {
    fprintf(stderr, "message: %s\n", Msg);
    LLVMDisposeMessage(Msg);
  }
  exit(2);
}

static void
generateCommon()
{
  char *Msg;

  if (FlagDumpLLVM)
    LLVMDumpModule(TheModule);

  if (FlagVerifyLLVM) {
    if (LLVMVerifyModule(TheModule, LLVMPrintMessageAction, &Msg)) {
      LLVMDisposeMessage (Msg);
      abort();
    }
  }

  if (Optimization > LLVMCodeGenLevelNone) {
    LLVMPassManagerRef PassManager;
    PassManager = LLVMCreateFunctionPassManagerForModule (TheModule);

    LLVMAddPromoteMemoryToRegisterPass (PassManager);
    LLVMAddCFGSimplificationPass (PassManager);

    for (LLVMValueRef Func = LLVMGetFirstFunction (TheModule);
	 Func != nullptr;
	 Func = LLVMGetNextFunction(Func)) {
      LLVMRunFunctionPassManager (PassManager, Func);
    }
  }
}
extern "C" void
generate_object(char *Filename)
{
  char *Msg;

  generateCommon();

  if (LLVMTargetMachineEmitToFile (TheTargetMachine, TheModule, Filename,
				   LLVMObjectFile, &Msg))
    generateError(Filename, Msg);
}

extern "C" void
generate_assembly(char *Filename)
{
  char *Msg;

  generateCommon();

  if (LLVMTargetMachineEmitToFile (TheTargetMachine, TheModule, Filename,
				   LLVMAssemblyFile, &Msg))
    generateError(Filename, Msg);
}

extern "C" void
generate_bitcode(const char *Filename)
{
  generateCommon();

  if (LLVMWriteBitcodeToFile(TheModule, Filename)) {
    generateError(Filename, nullptr);
  }
}

extern "C" void
generate_llvm(char *Filename)
{
  char *Msg;

  generateCommon();

  if (LLVMPrintModuleToFile(TheModule, Filename, &Msg)) {
    generateError(Filename, Msg);
  }
}

extern "C" void
ortho_llvm_init(const char *Filename, unsigned FilenameLength)
{
  char *Msg;

  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();

  TheModule = LLVMModuleCreateWithName ("ortho");

  //  Get target triple (from how llvm was configured).
  char *Triple = LLVMGetDefaultTargetTriple();

#if LLVM_VERSION_MAJOR >= 7
  {
    char *RawTriple = Triple;
    Triple = LLVMNormalizeTargetTriple(Triple);
    LLVMDisposeMessage(RawTriple);
  }
#endif
  LLVMSetTarget(TheModule, Triple);

  //  Get target - this is a struct that corresponds to the triple.
  if (LLVMGetTargetFromTriple(Triple, &TheTarget, &Msg) != 0) {
    fprintf(stderr, "llvm: cannot find target %s: %s\n", Triple, Msg);
    LLVMDisposeMessage(Msg);
    exit (1);
  }

  //  Create a target machine
  TheTargetMachine = LLVMCreateTargetMachine
    (TheTarget, Triple, "", "", Optimization, TheReloc,
     LLVMCodeModelDefault);

#if LLVM_VERSION_MAJOR < 4
  TheTargetData = LLVMGetTargetMachineData (TheTargetMachine);
  LLVMSetDataLayout (TheModule, LLVMCopyStringRepOfTargetData (TheTargetData));
#else
  TheTargetData = LLVMCreateTargetDataLayout(TheTargetMachine);
  LLVMSetModuleDataLayout(TheModule, TheTargetData);
#endif

  Builder = LLVMCreateBuilder();
  DeclBuilder = LLVMCreateBuilder();
  ExtraBuilder = LLVMCreateBuilder();

  LLVMTypeRef I8Ptr = LLVMPointerType(LLVMInt8Type(), 0);

  StackSaveFun = LLVMAddFunction
    (TheModule, "llvm.stacksave", LLVMFunctionType (I8Ptr, NULL, 0, false));

  LLVMTypeRef ParamTypes[2];

  ParamTypes[0] = I8Ptr;
  StackRestoreFun = LLVMAddFunction
    (TheModule, "llvm.stackrestore",
     LLVMFunctionType(LLVMVoidType(), ParamTypes, 1, false));

  ParamTypes[0] = LLVMDoubleType();
  ParamTypes[1] = LLVMDoubleType();
  CopySignFun = LLVMAddFunction
    (TheModule, "llvm.copysign.f64",
     LLVMFunctionType(LLVMDoubleType(), ParamTypes, 2, false));

  Fp0_5 = LLVMConstReal(LLVMDoubleType(), 0.5);

#ifdef USE_ATTRIBUTES
  unsigned AttrId;

  AttrId = LLVMGetEnumAttributeKindForName("nounwind", 8);
  assert (AttrId != 0);
  NounwindAttr = LLVMCreateEnumAttribute(LLVMGetGlobalContext(), AttrId, 0);

  AttrId = LLVMGetEnumAttributeKindForName("uwtable", 7);
  assert (AttrId != 0);
  UwtableAttr = LLVMCreateEnumAttribute(LLVMGetGlobalContext(), AttrId, 0);
#endif

#ifdef USE_DEBUG
  if (FlagDebugLines) {
    DBuilder = new DIBuilder(*unwrap(TheModule));

    DebugCurrentFilename = new std::string(Filename, FilenameLength);
    SmallString<128> CurrentDir;
    llvm::sys::fs::current_path(CurrentDir);
    DebugCurrentDirectory = new std::string(CurrentDir.data(),
					    CurrentDir.size());

    DebugCurrentFile = DBuilder->createFile(StringRef(*DebugCurrentFilename),
					    StringRef(*DebugCurrentDirectory));
  }
#endif
}

enum OTKind : unsigned char {
  OTKUnsigned, OTKSigned, OTKFloat,
  OTKEnum, OTKBool,
  OTKAccess, OTKIncompleteAccess,
  OTKRecord, OTKIncompleteRecord,
  OTKUnion,
  OTKArray
};

struct OTnodeBase {
  LLVMTypeRef Ref;
  LLVMValueRef Dbg;

  OTKind Kind;
  bool Bounded;
  OTnodeBase (LLVMTypeRef R, OTKind K, bool Bounded) :
    Ref(R), Dbg(nullptr), Kind(K), Bounded(Bounded) {}
};

typedef OTnodeBase *OTnode;

struct OTnodeScal : OTnodeBase {
  //  For scalar: the size
  unsigned ScalSize;

  OTnodeScal (LLVMTypeRef R, OTKind K, unsigned Sz) :
    OTnodeBase(R, K, true), ScalSize(Sz) {}
};

struct OTnodeUnsigned : OTnodeScal {
  OTnodeUnsigned (LLVMTypeRef R, unsigned Sz) :
    OTnodeScal(R, OTKUnsigned, Sz) {}
};

struct OTnodeSigned : OTnodeScal {
  OTnodeSigned (LLVMTypeRef R, unsigned Sz) :
    OTnodeScal(R, OTKSigned, Sz) {}
};

struct OTnodeFloat : OTnodeScal {
  OTnodeFloat (LLVMTypeRef R, unsigned Sz) :
    OTnodeScal(R, OTKFloat, Sz) {}
};

struct OTnodeEnum : OTnodeScal {
  OTnodeEnum (LLVMTypeRef R, unsigned Sz) :
    OTnodeScal(R, OTKEnum, Sz) {}
};

struct OTnodeBool : OTnodeScal {
  OTnodeBool (LLVMTypeRef R) : OTnodeScal(R, OTKBool, 1) {}
};

static LLVMTypeRef
SizeToLLVM (unsigned Sz)
{
  switch (Sz) {
  case 8:
    return LLVMInt8Type();
  case 32:
    return LLVMInt32Type();
  case 64:
    return LLVMInt64Type();
  default:
    abort();
  }
}

extern "C" OTnode
new_unsigned_type(unsigned Sz)
{
  return new OTnodeUnsigned(SizeToLLVM(Sz), Sz);
}

extern "C" OTnode
new_signed_type(unsigned Sz)
{
  return new OTnodeSigned(SizeToLLVM(Sz), Sz);
}

extern "C" OTnode
new_float_type()
{
  return new OTnodeFloat(LLVMDoubleType(), 64);
}

struct OEnumList {
  LLVMTypeRef Ref;
  unsigned Pos;
  OTnodeEnum *Etype;
};

extern "C" void
start_enum_type (OEnumList *List, unsigned Sz)
{
  LLVMTypeRef T = SizeToLLVM(Sz);

  *List = {T, 0, new OTnodeEnum(T, Sz)};
}

struct OCnode {
  LLVMValueRef Ref;
  OTnode Ctype;
};

struct OIdent {
  const char *cstr;
};

extern "C" void
new_enum_literal (OEnumList *List, OIdent Ident, OCnode *Res)
{
  *Res = {LLVMConstInt(List->Ref, List->Pos++, 0),
	  List->Etype};
}

extern "C" void
finish_enum_type (OEnumList *List, OTnode *Res)
{
  *Res = List->Etype;
}

extern "C" void
new_boolean_type(OTnode *Res,
		 OIdent False_Id, OCnode *False_E,
		 OIdent True_Id, OCnode *True_E)
{
  OTnodeBool *T = new OTnodeBool(LLVMInt1Type());
  *Res = T;

  *False_E = {LLVMConstInt(T->Ref, 0, 0), T};
  *True_E = {LLVMConstInt(T->Ref, 1, 0), T};
}

extern "C" OCnode
new_signed_literal (OTnode LType, int64_t Value)
{
  return {LLVMConstInt(LType->Ref, Value, 1), LType};
}

extern "C" OCnode
new_unsigned_literal (OTnode LType, uint64_t Value)
{
  return {LLVMConstInt(LType->Ref, Value, 0), LType};
}

extern "C" OCnode
new_float_literal (OTnode LType, double Value)
{
  return {LLVMConstReal(LType->Ref, Value), LType};
}

struct OTnodeAccBase : OTnodeBase {
  //  For accesses
  OTnode Acc;

  OTnodeAccBase (LLVMTypeRef R, OTKind Kind, OTnode Acc) :
    OTnodeBase(R, Kind, true), Acc(Acc) {}
};

struct OTnodeAcc : OTnodeAccBase {
  OTnodeAcc (LLVMTypeRef R, OTnode Acc) :
    OTnodeAccBase(R, OTKAccess, Acc) {}
};

struct OTnodeIncompleteAcc : OTnodeAccBase {
  OTnodeIncompleteAcc () :
    OTnodeAccBase(nullptr, OTKIncompleteAccess, nullptr) {}
};

extern "C" OTnode
new_access_type(OTnode DType)
{
  if (DType == nullptr) {
    return new OTnodeIncompleteAcc();
  } else {
    return new OTnodeAcc(LLVMPointerType(DType->Ref, 0), DType);
  }
}

extern "C" void
finish_access_type(OTnodeAcc *AccType, OTnode DType)
{
  //  Must be incomplete.
  assert (AccType->Acc == nullptr);

  LLVMTypeRef Types[1] = { DType->Ref };
  LLVMStructSetBody(LLVMGetElementType(AccType->Ref), Types, 1, 0);
  AccType->Acc = DType;
}

extern "C" OCnode
new_null_access (OTnode LType)
{
  return {LLVMConstNull(LType->Ref), LType};
}

enum OFKind { OF_Record, OF_Union};

struct OElement {
  //  Identifier for the element
  OIdent Ident;

  // Type of the element
  OTnode Etype;

  //  Next element (in the linked list)
  OElement *Next;
};

struct OElementList {
  OFKind Kind;

  //  Number of fields.
  unsigned Count;

  //  For record: the access to the incomplete (but named) type.
  OTnode RecType;

  //  For unions: biggest for size and alignment
  unsigned Size;
  unsigned Align;
  //  For unions: type with the biggest alignment.
  LLVMTypeRef AlignType;

  struct OElement *FirstElem;
  struct OElement *LastElem;
};

extern "C" void
start_record_type (OElementList *Elements)
{
  *Elements = {OF_Record,
	       0,
	       nullptr,
	       0, 0, nullptr,
	       nullptr,
	       nullptr};
}

static void
addField(OElementList *Elements, OIdent Ident, OTnode Etype)
{
  Elements->Count++;

  OElement *El = new OElement{Ident, Etype, nullptr};
  if (Elements->FirstElem == nullptr)
    Elements->FirstElem = El;
  else
    Elements->LastElem->Next = El;
  Elements->LastElem = El;
}

struct OFnodeBase {
  OFKind Kind;
  OTnode FType;
  OFnodeBase(OFKind Kind, OTnode FType) : Kind(Kind), FType(FType) {}
};

struct OFnodeRec : OFnodeBase {
  unsigned Index;
  OFnodeRec(OTnode Etype, unsigned Index) :
    OFnodeBase(OF_Record, Etype), Index(Index) {}
};

struct OFnodeUnion : OFnodeBase {
  LLVMTypeRef Utype;
  //  Pointer type - used to do conversion between the union and the field.
  LLVMTypeRef PtrType;
  OFnodeUnion(OTnode Etype, LLVMTypeRef PtrType) :
    OFnodeBase(OF_Union, Etype), Utype(Etype->Ref), PtrType(PtrType) {}
};

extern "C" void
new_record_field(OElementList *Elements,
		 OFnodeRec **El, OIdent Ident, OTnode Etype)
{
  *El = new OFnodeRec(Etype, Elements->Count);
  addField(Elements, Ident, Etype);
}

static void
freeElements(OElementList *Els)
{
  OElement *El, *NEl;

  for (El = Els->FirstElem; El != nullptr; El = NEl) {
    NEl = El->Next;
    delete El;
  }
  Els->FirstElem = nullptr;
  Els->LastElem = nullptr;
}

struct OTnodeRecBase : OTnodeBase {
  OTnodeRecBase (LLVMTypeRef R, OTKind Kind, bool Bounded) :
    OTnodeBase(R, Kind, Bounded) {}
};

struct OTnodeRec : OTnodeRecBase {
  OTnodeRec (LLVMTypeRef R, bool Bounded) :
    OTnodeRecBase(R, OTKRecord, Bounded) {}
};

struct OTnodeIncompleteRec : OTnodeRecBase {
  OTnodeIncompleteRec () :
    OTnodeRecBase(nullptr, OTKIncompleteRecord, false) {}
};

extern "C" void
finish_record_type(OElementList *Els, OTnode *Res)
{
  LLVMTypeRef *Types = new LLVMTypeRef[Els->Count];

  OElement *El;
  int i;
  bool Bounded = true;
  for (i = 0, El = Els->FirstElem; El != nullptr; El = El->Next, i++) {
    Bounded &= El->Etype->Bounded;
    Types[i] = El->Etype->Ref;
  }

  if (Els->RecType != nullptr) {
    //  Completion
    LLVMStructSetBody (Els->RecType->Ref, Types, Els->Count, 0);
    Els->RecType->Bounded = Bounded;
    *Res = Els->RecType;
  } else {
    *Res = new OTnodeRec(LLVMStructType(Types, Els->Count, 0), Bounded);
  }
  freeElements(Els);
}

extern "C" void
new_uncomplete_record_type(OTnode *Res)
{
  *Res = new OTnodeIncompleteRec();
}

extern "C" void
start_uncomplete_record_type(OTnodeRec *Res, OElementList *Els)
{
  //  Must be incomplete.
  assert (Res->Kind == OTKIncompleteRecord);

  *Els = {OF_Record,
	  0,
	  Res,
	  0, 0, nullptr,
	  nullptr,
	  nullptr};
}

extern "C" void
start_union_type(OElementList *Els)
{
  *Els = {OF_Union,
	  0,
	  nullptr,
	  0, 0, nullptr,
	  nullptr,
	  nullptr};
}

extern "C" void
new_union_field(OElementList *Els, OFnodeUnion **El,
		OIdent Ident, OTnode Etype)
{
  unsigned Size = LLVMABISizeOfType(TheTargetData, Etype->Ref);
  unsigned Align = LLVMABIAlignmentOfType(TheTargetData, Etype->Ref);

  *El = new OFnodeUnion(Etype, LLVMPointerType(Etype->Ref, 0));

  if (Size > Els->Size)
    Els->Size = Size;
  if (Els->AlignType == nullptr || Align > Els->Align) {
    Els->Align = Align;
    Els->AlignType = Etype->Ref;
  }
  addField(Els, Ident, Etype);
}

struct OTnodeUnion : OTnodeBase {
  //  For unions
  unsigned Size;
  LLVMTypeRef MainField;

  OTnodeUnion(LLVMTypeRef R, unsigned Sz, LLVMTypeRef Main) :
    OTnodeBase(R, OTKUnion, true), Size(Sz), MainField(Main) {}
};


extern "C" void
finish_union_type(OElementList *Els, OTnode *Res)
{
  unsigned Count;
  LLVMTypeRef Types[2];

  if (Els->AlignType == nullptr) {
    //  An empty union
    Count = 0;
  } else {
    unsigned Pad;

    Types[0] = Els->AlignType;
    Pad = Els->Size - LLVMABISizeOfType(TheTargetData, Els->AlignType);
    if (Pad != 0) {
      Types[1] = LLVMArrayType(LLVMInt8Type(), Pad);
      Count = 2;
    } else {
      Count = 1;
    }
  }

  *Res = new OTnodeUnion(LLVMStructType(Types, Count, 0),
			 Els->Size, Els->AlignType);
  freeElements(Els);
}

struct OTnodeArr : OTnodeBase {
  //  For arrays: type of the element
  OTnode ElType;

  OTnodeArr(LLVMTypeRef R, bool Complete, OTnode E) :
    OTnodeBase(R, OTKArray, Complete), ElType(E) {}
};

extern "C" OTnode
new_array_type(OTnode ElType, OTnode IndexType)
{
  return new OTnodeArr(LLVMArrayType(ElType->Ref, 0), false, ElType);
}

extern "C" OTnode
new_constrained_array_type(OTnodeArr *ArrType, OCnode *Length)
{
  unsigned Len = LLVMConstIntGetZExtValue(Length->Ref);

  return new OTnodeArr(LLVMArrayType(ArrType->ElType->Ref, Len),
		       ArrType->ElType->Bounded,
		       ArrType->ElType);
}

extern "C" void
new_type_decl(OIdent Ident, OTnode Atype)
{
  switch(Atype->Kind) {
  case OTKIncompleteAccess:
    Atype->Ref = LLVMPointerType
      (LLVMStructCreateNamed(LLVMGetGlobalContext(), Ident.cstr), 0);
    break;
  case OTKIncompleteRecord:
    Atype->Ref = LLVMStructCreateNamed(LLVMGetGlobalContext(), Ident.cstr);
    break;
  default:
    break;
  }
}

struct ORecordAggrList {
  unsigned Len;
  LLVMValueRef *Els;
  OTnode Atype;
};

extern "C" void
start_record_aggr(ORecordAggrList *List, OTnode Atype)
{
  unsigned Count = LLVMCountStructElementTypes(Atype->Ref);
  *List = {0, new LLVMValueRef[Count], Atype};
}

extern "C" void
new_record_aggr_el(ORecordAggrList *List, OCnode *Val)
{
  List->Els[List->Len++] = Val->Ref;
}

extern "C" void
finish_record_aggr(ORecordAggrList *List, OCnode *Res)
{
  *Res = {LLVMConstStruct(List->Els, List->Len, 0), List->Atype};
  delete List->Els;
}

struct OArrayAggrList {
  unsigned Len;
  LLVMValueRef *Els;
  LLVMTypeRef ElType;
  OTnode Atype;
};

extern "C" void
start_array_aggr(OArrayAggrList *List, OTnodeArr *Atype, unsigned len)
{
  *List = {0, new LLVMValueRef[len], Atype->ElType->Ref, Atype};
}

extern "C" void
new_array_aggr_el(OArrayAggrList *List, OCnode *Value)
{
  List->Els[List->Len++] = Value->Ref;
}

extern "C" void
finish_array_aggr(OArrayAggrList *List, OCnode *Res)
{
  *Res = {LLVMConstArray(List->ElType, List->Els, List->Len), List->Atype};
  delete List->Els;
}

extern "C" OCnode
new_union_aggr(OTnodeUnion *Atype, OFnodeUnion *Field, OCnode *Value)
{
  unsigned Size = LLVMABISizeOfType(TheTargetData, Field->Utype);
  LLVMValueRef Vals[2];
  unsigned Count;

  Vals[0] = Value->Ref;
  if (Size < Atype->Size) {
    //  Add padding.
    Vals[1] = LLVMGetUndef(LLVMArrayType(LLVMInt8Type(), Atype->Size - Size));
    Count = 2;
  } else {
    Count = 1;
  }

  return {LLVMConstStruct(Vals, Count, false), Atype};
}

extern "C" OCnode
new_default_value(OTnode Ltype)
{
  return {LLVMConstNull(Ltype->Ref), Ltype};
}

static OCnode
constToConst(OTnode Rtype, uint64_t Val)
{
  LLVMValueRef Ref;

  switch (Rtype->Kind) {
  case OTKUnsigned:
  case OTKSigned:
    Ref = LLVMConstInt(Rtype->Ref, Val, 0);
    break;
  case OTKAccess:
    //  It is possible to use an access type for offsetof.
    Ref = LLVMConstInt(LLVMInt64Type(), Val, 0);
    Ref = LLVMConstIntToPtr(Ref, Rtype->Ref);
    break;
  default:
    abort();
  }
  return {Ref, Rtype};
}

extern "C" OCnode
new_sizeof(OTnode Atype, OTnode Rtype)
{
  return constToConst(Rtype, LLVMABISizeOfType(TheTargetData, Atype->Ref));
}

extern "C" OCnode
new_alignof(OTnode Atype, OTnode Rtype)
{
  return constToConst
    (Rtype, LLVMABIAlignmentOfType(TheTargetData, Atype->Ref));
}

extern "C" OCnode
new_offsetof(OTnode Atype, OFnodeRec *Field, OTnode Rtype)
{
  return constToConst
    (Rtype, LLVMOffsetOfElement(TheTargetData, Atype->Ref, Field->Index));
}

struct OEnode {
  LLVMValueRef Ref;
  OTnode Etype;
};

extern "C" OEnode
new_lit(OCnode *Lit)
{
  return {Lit->Ref, Lit->Ctype};
}

enum ODKind : unsigned char {
  ODKConst,
  ODKVar,
  ODKLocal,
  ODKInterface,
  ODKType,
  ODKSubprg
};

struct ODnodeBase {
  LLVMValueRef Ref;
  OTnode Dtype;
  virtual ODKind getKind() const = 0;
  ODnodeBase(LLVMValueRef R, OTnode T) : Ref(R), Dtype(T) {}
  virtual ~ODnodeBase() {}
};

typedef ODnodeBase *ODnode;

struct ODnodeVar : ODnodeBase {
  ODKind getKind() const override { return ODKVar; }
  ODnodeVar(LLVMValueRef R, OTnode T) : ODnodeBase(R, T) {}
};

struct ODnodeLocalVar : ODnodeBase {
  ODKind getKind() const override { return ODKLocal; }
  ODnodeLocalVar(LLVMValueRef R, OTnode T) : ODnodeBase(R, T) {}
};

enum OStorage {
  O_Storage_External,
  O_Storage_Public,
  O_Storage_Private,
  O_Storage_Local
};

extern "C" void
new_var_decl(ODnode *Res, OIdent Ident, OStorage Storage, OTnode Atype)
{
  LLVMValueRef Decl;

  if (Storage == O_Storage_Local) {
    if (Unreach)
      Decl = nullptr;
    else
      Decl = LLVMBuildAlloca (DeclBuilder, Atype->Ref, Ident.cstr);
    *Res = new ODnodeLocalVar(Decl, Atype);
  } else {
    if (Storage == O_Storage_External) {
      Decl = LLVMGetNamedGlobal(TheModule, Ident.cstr);
    } else {
      Decl = nullptr;
    }
    if (Decl == nullptr)
      Decl = LLVMAddGlobal(TheModule, Atype->Ref, Ident.cstr);

    *Res = new ODnodeVar(Decl, Atype);
    if (Storage == O_Storage_Private)
      LLVMSetLinkage(Decl, LLVMInternalLinkage);

    switch(Storage) {
    case O_Storage_Public:
    case O_Storage_Private:
      LLVMSetInitializer(Decl, LLVMConstNull(Atype->Ref));
      break;
    default:
      break;
    }
  }
}

struct ODnodeConst : ODnodeBase {
  OStorage Storage;
  OIdent Ident;
  ODKind getKind() const override { return ODKConst; }
  ODnodeConst(LLVMValueRef R, OTnode T, OStorage S, OIdent I) :
    ODnodeBase(R, T), Storage(S), Ident(I) {}
};

static void
setConstAttributes(LLVMValueRef Ref, OStorage Storage)
{
  LLVMSetGlobalConstant(Ref, true);
  if (Storage == O_Storage_Private)
    LLVMSetLinkage(Ref, LLVMInternalLinkage);
}

extern "C" void
new_const_decl(ODnode *Res, OIdent Ident, OStorage Storage, OTnode Atype)
{
  LLVMValueRef Decl;

  if (Storage == O_Storage_External) {
    //  It is possible to re-declare an external const.
    Decl = LLVMGetNamedGlobal(TheModule, Ident.cstr);
    if (Decl == nullptr)
      Decl = LLVMAddGlobal(TheModule, Atype->Ref, Ident.cstr);
    setConstAttributes(Decl, Storage);
  } else {
    //  If the type of the constant is not yet bounded, delay the creation
    //  of the constant until its initialization.
    if (Atype->Bounded) {
      Decl = LLVMAddGlobal(TheModule, Atype->Ref, Ident.cstr);
      setConstAttributes(Decl, Storage);
    } else {
      Decl = nullptr;
    }
  }

  *Res = new ODnodeConst(Decl, Atype, Storage, Ident);
}

extern "C" void
start_init_value(ODnodeConst **Decl)
{
}

extern "C" void
finish_init_value(ODnodeConst **Decl, OCnode *Val)
{
  LLVMValueRef Ref = (*Decl)->Ref;

  if (Ref == nullptr) {
    Ref = LLVMAddGlobal(TheModule, LLVMTypeOf(Val->Ref), (*Decl)->Ident.cstr);
    setConstAttributes(Ref, (*Decl)->Storage);
    (*Decl)->Ref = Ref;
  }

  LLVMSetInitializer(Ref, Val->Ref);
}

struct ODnodeInter : ODnodeBase {
  ODKind getKind() const override { return ODKInterface; }
  ODnodeInter(LLVMValueRef R, OTnode T) : ODnodeBase(R, T) {}
};

struct OInter {
  ODnodeInter *Decl;
  OIdent Ident;
  OInter *Next;
};

struct OInterList {
  OIdent Ident;
  OStorage Storage;
  OTnode Rtype;

  //  Number of interfaces.
  unsigned Count;
  OInter *FirstInter;
  OInter *LastInter;
};

extern "C" void
start_function_decl(OInterList *Inters, OIdent Ident, OStorage Storage,
		    OTnode Rtype)
{
  *Inters = { Ident, Storage, Rtype, 0, nullptr, nullptr };
}

extern "C" void
start_procedure_decl(OInterList *Inters, OIdent Ident, OStorage Storage)
{
  *Inters = { Ident, Storage, nullptr, 0, nullptr, nullptr };
}

extern "C" void
new_interface_decl(OInterList *Inters,
		   ODnode *Res, OIdent Ident, OTnode Itype)
{
  ODnodeInter *Decl = new ODnodeInter(nullptr, Itype);
  OInter *Inter = new OInter{Decl, Ident, nullptr};

  *Res = Decl;
  Inters->Count++;
  if (Inters->FirstInter == nullptr)
    Inters->FirstInter = Inter;
  else
    Inters->LastInter->Next = Inter;
  Inters->LastInter = Inter;
}

struct ODnodeSubprg : ODnodeBase {
  //  Number of interfaces.
  unsigned Count;
  ODKind getKind() const override { return ODKSubprg; }
  ODnodeSubprg(LLVMValueRef R, OTnode T, unsigned Count) :
    ODnodeBase(R, T), Count(Count) {}
};

extern "C" void
finish_subprogram_decl(OInterList *Inters, ODnode *Res)
{
  LLVMTypeRef *Types = new LLVMTypeRef[Inters->Count];

  //  Build array of interface types.
  int i = 0;
  for (OInter *Inter = Inters->FirstInter; Inter; Inter = Inter->Next, i++)
    Types[i] = Inter->Decl->Dtype->Ref;

  LLVMTypeRef Rtype;
  if (Inters->Rtype == nullptr)
    Rtype = LLVMVoidType();
  else
    Rtype = Inters->Rtype->Ref;

  LLVMTypeRef Ftype = LLVMFunctionType(Rtype, Types, Inters->Count, 0);

  LLVMValueRef Decl;
  if (Inters->Storage == O_Storage_External)
    Decl = LLVMGetNamedFunction(TheModule, Inters->Ident.cstr);
  else
    Decl = nullptr;
  if (Decl == nullptr) {
    Decl = LLVMAddFunction(TheModule, Inters->Ident.cstr, Ftype);
#ifdef USE_ATTRIBUTES
    LLVMAddAttributeAtIndex(Decl, LLVMAttributeFunctionIndex, NounwindAttr);
    LLVMAddAttributeAtIndex(Decl, LLVMAttributeFunctionIndex, UwtableAttr);
#else
    LLVMAddFunctionAttr (Decl, LLVMNoUnwindAttribute);
    LLVMAddFunctionAttr (Decl, LLVMUWTable);
#endif
    LLVMSetFunctionCallConv(Decl, LLVMCCallConv);
  }

  *Res = new ODnodeSubprg(Decl, Inters->Rtype, Inters->Count);

  //  Translate interfaces
  i = 0;
  for (OInter *Inter = Inters->FirstInter, *Next; Inter; Inter = Next, i++) {
    Inter->Decl->Ref = LLVMGetParam(Decl, i);
    LLVMSetValueName(Inter->Decl->Ref, Inter->Ident.cstr);
    Next = Inter->Next;
    delete Inter;
  }
}

//  Data for a declare block.
struct DeclareBlock {
  //  First basic block of the declare.
  LLVMBasicBlockRef StmtBB;

  //  To handle allocb: stack pointer at the entry of the block, that needs
  //  to be restored when leaving the block (either by falling through or
  //  via exit/next).  Set only of New_Alloca is used.
  LLVMValueRef StackValue;

  //  Previous value block.
  DeclareBlock *Prev;
};

static DeclareBlock *CurrentDeclareBlock;
static DeclareBlock *OldDeclareBlock;

static LLVMValueRef CurrentFunc;
static ODnodeSubprg *CurrentFuncDecl;

static void
CreateDeclareBlock()
{
  DeclareBlock *Res;

  //  Allocate a declare block
  if (OldDeclareBlock != nullptr) {
    Res = OldDeclareBlock;
    OldDeclareBlock = Res->Prev;
  } else {
    Res = new DeclareBlock;
  }
  *Res = { nullptr, nullptr, CurrentDeclareBlock };
  CurrentDeclareBlock = Res;

  if (!Unreach) {
    Res->StmtBB = LLVMAppendBasicBlock(CurrentFunc, "");
  }
}

static void
DestroyDeclareBlock()
{
  DeclareBlock *Blk = CurrentDeclareBlock;

  CurrentDeclareBlock = Blk->Prev;

  Blk->Prev = OldDeclareBlock;
  OldDeclareBlock = Blk;
}

extern "C" void
start_subprogram_body(ODnodeSubprg *Func)
{
  LLVMBasicBlockRef DeclBB;

  //  Nested subprograms are not supported.
  assert (CurrentFunc == nullptr);

  CurrentFunc = Func->Ref;
  CurrentFuncDecl = Func;

  assert(!Unreach);

  DeclBB = LLVMAppendBasicBlock(CurrentFunc, "");
  LLVMPositionBuilderAtEnd(DeclBuilder, DeclBB);

  CreateDeclareBlock();
  LLVMPositionBuilderAtEnd(Builder, CurrentDeclareBlock->StmtBB);
}

extern "C" void
finish_subprogram_body()
{
  //  Add a jump from the declare basic block to the first statement BB.
  LLVMBuildBr(DeclBuilder, CurrentDeclareBlock->StmtBB);

  //  Terminate the statement BB
  if (!Unreach) {
    if (CurrentFuncDecl->Dtype == nullptr)
      LLVMBuildRetVoid (Builder);
    else
      LLVMBuildUnreachable (Builder);
  }

  DestroyDeclareBlock();

  CurrentFunc = nullptr;
  Unreach = false;
}

extern "C" void
start_declare_stmt ()
{
  CreateDeclareBlock();

  if (Unreach)
    return;

  //  Add a jump to the new BB.
  LLVMBuildBr(Builder, CurrentDeclareBlock->StmtBB);

  LLVMPositionBuilderAtEnd(Builder, CurrentDeclareBlock->StmtBB);
}

extern "C" void
finish_declare_stmt ()
{
  if (!Unreach) {
    LLVMBasicBlockRef Bb;

    //  Create a basic block for the statements after the dclare
    Bb = LLVMAppendBasicBlock(CurrentFunc, "");

    if (CurrentDeclareBlock->StackValue != nullptr) {
      //  Restore stack pointer
      LLVMBuildCall(Builder, StackRestoreFun,
		    &CurrentDeclareBlock->StackValue, 1, "");
    }
    //  Execution will continue on the next statement
    LLVMBuildBr(Builder, Bb);

    LLVMPositionBuilderAtEnd(Builder, Bb);
  }

  //  Do not reset Unreach.
  DestroyDeclareBlock();
}

struct OSNode {
  //  BB at the entry of the loop.  Will branch to it on next statement and
  //  at the end of the loop.
  LLVMBasicBlockRef BBEntry;
  //  BB after the loop.  Exit statement branches to it.
  LLVMBasicBlockRef BBExit;
};

extern "C" void
start_loop_stmt (OSNode *Label)
{
  if (Unreach) {
    *Label = { nullptr, nullptr };
    return;
  }

  *Label = { LLVMAppendBasicBlock(CurrentFunc, ""), nullptr };
#if 1
  Label->BBExit = LLVMAppendBasicBlock(CurrentFunc, "");
#endif
  LLVMBuildBr(Builder, Label->BBEntry);
  LLVMPositionBuilderAtEnd(Builder, Label->BBEntry);
}

extern "C" void
finish_loop_stmt (OSNode *Label)
{
  if (!Unreach)
    LLVMBuildBr(Builder, Label->BBEntry);

  if (Label->BBExit != nullptr) {
    //  Continue only if the exit was reachable.
    LLVMPositionBuilderAtEnd(Builder, Label->BBExit);
    Unreach = false;
  } else {
    Unreach = true;
  }
}

extern "C" void
new_exit_stmt (OSNode *Label)
{
  if (Unreach)
    return;

#if 0
  //  Currently LABEL is an input (so cannot be modified)
  if (Label->BBExit == nullptr) {
    //  We know the end of the loop is reachable
    Label->BBExit = LLVMAppendBasicBlock(CurrentFunc, "");
  }
#endif

  LLVMBuildBr(Builder, Label->BBExit);
  Unreach = true;
}

extern "C" void
new_next_stmt (OSNode *Label)
{
  if (Unreach)
    return;

  LLVMBuildBr(Builder, Label->BBEntry);
  Unreach = true;
}

struct OIFBlock {
  LLVMBasicBlockRef Bb;
};

extern "C" void
start_if_stmt (OIFBlock *Blk, OEnode Cond)
{
  if (Unreach) {
    *Blk = { nullptr};
    return;
  }

  LLVMBasicBlockRef BBThen;

  //  Create BB for Then and Else.
  BBThen = LLVMAppendBasicBlock(CurrentFunc, "");
  *Blk = { LLVMAppendBasicBlock(CurrentFunc, "") };

  LLVMBuildCondBr(Builder, Cond.Ref, BBThen, Blk->Bb);
  LLVMPositionBuilderAtEnd(Builder, BBThen);
}

extern "C" void
new_else_stmt (OIFBlock *Blk)
{
  LLVMBasicBlockRef BBNext;

  if (!Unreach) {
    //  Create a BB for after the If statement
    BBNext = LLVMAppendBasicBlock(CurrentFunc, "");
    //  And jump to it.
    LLVMBuildBr(Builder, BBNext);
  } else {
    if (Blk->Bb == nullptr) {
      //  The IF statement was unreachable, so is the Else part.
      return;
    }
    //  Do not yet create the BB for after the If statement, as we don't
    //  know if it is reachable.
    BBNext = nullptr;
  }

  //  Use the BB for the Else part.
  LLVMPositionBuilderAtEnd(Builder, Blk->Bb);

  Blk->Bb = BBNext;
  //  The Else part is reachable.
  Unreach = false;
}

extern "C" void
finish_if_stmt (OIFBlock *Blk)
{
  LLVMBasicBlockRef BBNext;

  if (!Unreach) {
    if (Blk->Bb == nullptr)
      BBNext = LLVMAppendBasicBlock(CurrentFunc, "");
    else
      BBNext = Blk->Bb;
    LLVMBuildBr(Builder, BBNext);
    LLVMPositionBuilderAtEnd(Builder, BBNext);
  } else {
    //  The branch doesn't continue.
    if (Blk->Bb != nullptr) {
      //  There is at least one fall-through (either from the Then or from
      //  the Else.
      Unreach = false;
      LLVMPositionBuilderAtEnd(Builder, Blk->Bb);
    }
  }
}

struct OChoice {
  LLVMValueRef Low, High;
  LLVMBasicBlockRef BB;
};

struct OCaseBlock {
  //  BB before the case.
  LLVMBasicBlockRef BBPrev;

  //  Select expression
  LLVMValueRef Value;
  OTnode Vtype;

  //  BB after the case statement
  LLVMBasicBlockRef BBNext;

  //  BB for others
  LLVMBasicBlockRef BBOthers;

  //  BB for the current choice
  LLVMBasicBlockRef BBChoice;

  std::vector<OChoice> *Choices;
};

extern "C" void
start_case_stmt (OCaseBlock *Blk, OEnode Value)
{
  LLVMBasicBlockRef BB;
  std::vector<OChoice> *Choices;

  if (Unreach) {
    //  The case statement is unreachable, discard it completly.
    BB = nullptr;
    Choices = nullptr;
  } else {
    BB = LLVMGetInsertBlock(Builder);
    Choices = new std::vector<OChoice>;
  }

  *Blk = { BB,
	   Value.Ref,
	   Value.Etype,
	   nullptr,
	   nullptr,
	   nullptr,
	   Choices };
}

//  Close previous branch
static void
finishBranch (OCaseBlock *Blk)
{
  if (Unreach) {
    //  No need to close it as this point is not reachable.
    return;
  }

  if (Blk->BBNext == nullptr) {
    //  Create the BB for after the case statement.
    //  It also means the end is reachable.
    Blk->BBNext = LLVMAppendBasicBlock(CurrentFunc, "");
  }
  LLVMBuildBr(Builder, Blk->BBNext);
}

extern "C" void
start_choice (OCaseBlock *Blk)
{
  if (Blk->BBPrev == nullptr) {
    //  The wholse case statement was unreachable
    assert(Unreach);
    return;
  }

  if (Blk->BBChoice != nullptr) {
    //  Close previous branch
    finishBranch(Blk);
  }

  //  This new choice is reachable from the start of the case statement.
  Unreach = false;

  //  Create a new BB.
  Blk->BBChoice = LLVMAppendBasicBlock(CurrentFunc, "");
  LLVMPositionBuilderAtEnd(Builder, Blk->BBChoice);
}

//  Add a choice that will branch to Blk->BBChoice.
static void
newChoice(OCaseBlock *Blk, LLVMValueRef Low, LLVMValueRef High)
{
  if (Unreach)
    return;

  Blk->Choices->push_back({Low, High, Blk->BBChoice});
}

extern "C" void
new_expr_choice (OCaseBlock *Blk, OCnode *Expr)
{
  newChoice(Blk, Expr->Ref, nullptr);
}

extern "C" void
new_range_choice (OCaseBlock *Blk, OCnode *Low, OCnode *High)
{
  newChoice(Blk, Low->Ref, High->Ref);
}

extern "C" void
new_default_choice (OCaseBlock *Blk)
{
  if (Unreach)
    return;

  Blk->BBOthers = Blk->BBChoice;
}

extern "C" void
finish_choice (OCaseBlock *Blk)
{
}

extern "C" void
finish_case_stmt (OCaseBlock *Blk)
{
  LLVMIntPredicate GE, LE;

  if (Blk->BBPrev == nullptr) {
    //  The whole case statement is not reachable.
    return;
  }

  if (Blk->BBChoice != nullptr) {
    //  Close previous branch
    finishBranch(Blk);
  }

  //  Strategy: use a switch instruction for simple choices, put range choices
  //  in the default branch, using if statements.
  //  TODO: could improve the handling of ranges (dichotomy, decision tree...)
  switch (Blk->Vtype->Kind) {
  case OTKUnsigned:
  case OTKEnum:
  case OTKBool:
    GE = LLVMIntUGE;
    LE = LLVMIntULE;
    break;
  case OTKSigned:
    GE = LLVMIntSGE;
    LE = LLVMIntSLE;
    break;
  default:
    llvm_unreachable("bad expr type for case");
  }

  //  BB for the default case.
  LLVMBasicBlockRef BBDefault = LLVMAppendBasicBlock(CurrentFunc, "");
  LLVMPositionBuilderAtEnd(Builder, BBDefault);

  //  Put range choices in the default case.
  unsigned int Count = 0;
  LLVMBasicBlockRef BBLast = BBDefault;
  for(auto &c: *Blk->Choices) {
    if (c.High != nullptr) {
      BBLast = LLVMAppendBasicBlock(CurrentFunc, "");
      LLVMBuildCondBr(Builder,
		      LLVMBuildAnd(Builder,
				   LLVMBuildICmp(Builder, GE,
						 Blk->Value, c.Low, ""),
				   LLVMBuildICmp(Builder, LE,
						 Blk->Value, c.High, ""),
				   ""),
		      c.BB, BBLast);
      LLVMPositionBuilderAtEnd(Builder, BBLast);
    } else {
      Count++;
    }
  }

  //  Insert the switch
  LLVMPositionBuilderAtEnd(Builder, Blk->BBPrev);
  LLVMValueRef Sw = LLVMBuildSwitch(Builder, Blk->Value, BBDefault, Count);
  for(auto &c: *Blk->Choices) {
    if (c.High == nullptr) {
      LLVMAddCase(Sw, c.Low, c.BB);
    }
  }

  //  Insert the others (if there is one).
  LLVMPositionBuilderAtEnd(Builder, BBLast);
  if (Blk->BBOthers != nullptr)
    LLVMBuildBr(Builder, Blk->BBOthers);
  else
    LLVMBuildUnreachable(Builder);

  //  Next BB.
  if (Blk->BBNext != nullptr) {
    Unreach = false;
    LLVMPositionBuilderAtEnd(Builder, Blk->BBNext);
  } else {
    //  No branch falls through
    Unreach = true;
  }
  delete Blk->Choices;
}

struct OAssocList {
  ODnodeSubprg *Subprg;
  unsigned Idx;
  LLVMValueRef *Vals;
};

extern "C" void
start_association (OAssocList *Assocs, ODnodeSubprg *Subprg)
{
  *Assocs = { Subprg, 0, new LLVMValueRef[Subprg->Count] };
}

extern "C" void
new_association (OAssocList *Assocs, OEnode Val)
{
  Assocs->Vals[Assocs->Idx++] = Val.Ref;
}

extern "C" OEnode
new_function_call (OAssocList *Assocs)
{
  LLVMValueRef Res;

  if (!Unreach) {
    Res = LLVMBuildCall(Builder, Assocs->Subprg->Ref,
			Assocs->Vals, Assocs->Subprg->Count, "");
  } else {
    Res = nullptr;
  }
  delete Assocs->Vals;
  return { Res, Assocs->Subprg->Dtype };
}

extern "C" void
new_procedure_call (OAssocList *Assocs)
{
  if (!Unreach) {
    LLVMBuildCall(Builder, Assocs->Subprg->Ref,
		  Assocs->Vals, Assocs->Subprg->Count, "");
  }
  delete Assocs->Vals;
}

extern "C" void
new_func_return_stmt (OEnode Value)
{
  if (Unreach)
    return;
  LLVMBuildRet(Builder, Value.Ref);
  Unreach = true;
}

extern "C" void
new_proc_return_stmt ()
{
  if (Unreach)
    return;
  LLVMBuildRetVoid(Builder);
  Unreach = true;
}

enum ONOpKind {
  /*  Not an operation; invalid.  */
  ON_Nil,

  /*  Dyadic operations.  */
  ON_Add_Ov,
  ON_Sub_Ov,
  ON_Mul_Ov,
  ON_Div_Ov,
  ON_Rem_Ov,
  ON_Mod_Ov,

  /*  Binary operations.  */
  ON_And,
  ON_Or,
  ON_Xor,

  /*  Monadic operations.  */
  ON_Not,
  ON_Neg_Ov,
  ON_Abs_Ov,

  /*  Comparaisons  */
  ON_Eq,
  ON_Neq,
  ON_Le,
  ON_Lt,
  ON_Ge,
  ON_Gt,

  ON_LAST
};

struct ComparePred {
  LLVMIntPredicate SignedPred;
  LLVMIntPredicate UnsignedPred;
  LLVMRealPredicate RealPred;
};

static const ComparePred CompareTable[] = {
  {LLVMIntEQ,  LLVMIntEQ,  LLVMRealOEQ }, // Eq
  {LLVMIntNE,  LLVMIntNE,  LLVMRealONE }, // Ne
  {LLVMIntSLE, LLVMIntULE, LLVMRealOLE }, // Le
  {LLVMIntSLT, LLVMIntULT, LLVMRealOLT }, // Lt
  {LLVMIntSGE, LLVMIntUGE, LLVMRealOGE }, // Ge
  {LLVMIntSGT, LLVMIntUGT, LLVMRealOGT }  // Gt
};

extern "C" OEnode
new_compare_op (ONOpKind Kind, OEnode Left, OEnode Right, OTnode Rtype)
{
  LLVMValueRef Res;

  if (Unreach)
    return {nullptr, Rtype};

  //  Cannot apply C convention to ON_Op_Kind, so we need to truncate it
  //  (as it is represented by a byte from Ada and by int from C)
  Kind = static_cast<ONOpKind>(Kind & 0xff);

  switch(Left.Etype->Kind) {
  case OTKUnsigned:
  case OTKEnum:
  case OTKBool:
  case OTKAccess:
  case OTKIncompleteAccess:
    Res = LLVMBuildICmp(Builder, CompareTable[Kind - ON_Eq].UnsignedPred,
			Left.Ref, Right.Ref, "");
    break;
  case OTKSigned:
    Res = LLVMBuildICmp(Builder, CompareTable[Kind - ON_Eq].SignedPred,
			Left.Ref, Right.Ref, "");
    break;
  case OTKFloat:
    Res = LLVMBuildFCmp(Builder, CompareTable[Kind - ON_Eq].RealPred,
			Left.Ref, Right.Ref, "");
    break;
  default:
    abort();
  }
  return {Res, Rtype};
}

extern "C" OEnode
new_monadic_op (ONOpKind Kind, OEnode Operand)
{
  LLVMValueRef Res;

  if (Unreach)
    return { nullptr, Operand.Etype};

  //  Cannot apply C convention to ON_Op_Kind, so we need to truncate it
  //  (as it is represented by a byte from Ada and by int from C)
  Kind = static_cast<ONOpKind>(Kind & 0xff);

  switch (Operand.Etype->Kind) {
  case OTKUnsigned:
  case OTKSigned:
  case OTKBool:
    switch (Kind) {
    case ON_Not:
      Res = LLVMBuildNot(Builder, Operand.Ref, "");
      break;
    case ON_Neg_Ov:
      Res = LLVMBuildNeg(Builder, Operand.Ref, "");
      break;
    case ON_Abs_Ov:
      Res = LLVMBuildSelect
	(Builder,
	 LLVMBuildICmp (Builder, LLVMIntSLT,
			Operand.Ref,
			LLVMConstInt(Operand.Etype->Ref, 0, 0),
			""),
	 LLVMBuildNeg(Builder, Operand.Ref, ""),
	 Operand.Ref,
	 "");
      break;
    default:
      llvm_unreachable("bad scalar monadic op");
    }
    break;
  case OTKFloat:
    switch (Kind) {
    case ON_Neg_Ov:
      Res = LLVMBuildFNeg(Builder, Operand.Ref, "");
      break;
    case ON_Abs_Ov:
      Res = LLVMBuildSelect
	(Builder,
	 LLVMBuildFCmp (Builder, LLVMRealOLT,
			Operand.Ref,
			LLVMConstReal(Operand.Etype->Ref, 0.0),
			""),
	 LLVMBuildFNeg(Builder, Operand.Ref, ""),
	 Operand.Ref,
	 "");
      break;
    default:
      abort();
    }
    break;
  default:
    abort();
  }
  return {Res, Operand.Etype};
}

static LLVMValueRef
BuildSMod(LLVMBuilderRef Build, LLVMValueRef L, LLVMValueRef R, const char *s)
{
  LLVMTypeRef T = LLVMTypeOf(L);
  LLVMBasicBlockRef NormalBB;
  LLVMBasicBlockRef AdjustBB;
  LLVMBasicBlockRef NextBB;
  LLVMValueRef PhiVals[3];
  LLVMBasicBlockRef PhiBB[3];

  NextBB = LLVMAppendBasicBlock(CurrentFunc, "");
  NormalBB = LLVMAppendBasicBlock(CurrentFunc, "");

  //  Avoid overflow with -1
  //  if R = -1 then
  //    result := 0;
  //  else
  //    ...
  LLVMValueRef Cond;
  Cond = LLVMBuildICmp(Builder, LLVMIntEQ, R, LLVMConstAllOnes(T), "");
  LLVMBuildCondBr(Builder, Cond, NextBB, NormalBB);
  PhiBB[0] = LLVMGetInsertBlock(Builder);
  PhiVals[0] = LLVMConstNull(T);

  //  Rm := Left rem Right
  LLVMPositionBuilderAtEnd(Builder, NormalBB);
  LLVMValueRef Rm = LLVMBuildSRem(Builder, L, R, s);

  //  if Rm = 0 then
  //    result := 0
  //  else
  AdjustBB = LLVMAppendBasicBlock(CurrentFunc, "");
  Cond = LLVMBuildICmp(Builder, LLVMIntEQ, Rm, LLVMConstNull(T), "");
  LLVMBuildCondBr(Builder, Cond, NextBB, AdjustBB);
  PhiBB[1] = NormalBB;
  PhiVals[1] = LLVMConstNull(T);

  //    if (L xor R) < 0 then
  //      result := Rm + R
  //    else
  //      result := Rm
  LLVMPositionBuilderAtEnd(Builder, AdjustBB);
  LLVMValueRef RXor = LLVMBuildXor(Builder, L, R, "");
  Cond = LLVMBuildICmp(Builder, LLVMIntSLT, RXor, LLVMConstNull(T), "");
  LLVMValueRef RmPlusR = LLVMBuildAdd(Builder, Rm, R, "");
  LLVMValueRef Adj = LLVMBuildSelect(Builder, Cond, RmPlusR, Rm, "");
  LLVMBuildBr(Builder, NextBB);
  PhiBB[2] = AdjustBB;
  PhiVals[2] = Adj;

  //  The Phi node
  LLVMPositionBuilderAtEnd(Builder, NextBB);
  LLVMValueRef Phi = LLVMBuildPhi(Builder, T, "");
  LLVMAddIncoming(Phi, PhiVals, PhiBB, 3);

  return Phi;
}

extern "C" OEnode
new_dyadic_op (ONOpKind Kind, OEnode Left, OEnode Right)
{
  LLVMValueRef Res;
  LLVMValueRef (*Build)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *);
  OTKind ArgKind = Left.Etype->Kind;

  if (Unreach)
    return { nullptr, Left.Etype};

  //  Cannot apply C convention to ON_Op_Kind, so we need to truncate it
  //  (as it is represented by a byte from Ada and by int from C)
  Kind = static_cast<ONOpKind>(Kind & 0xff);

  switch (ArgKind) {
  case OTKUnsigned:
  case OTKSigned:
  case OTKBool:
  case OTKEnum:
    switch (Kind) {
    case ON_And:
      Build = &LLVMBuildAnd;
      break;
    case ON_Or:
      Build = &LLVMBuildOr;
      break;
    case ON_Xor:
      Build = &LLVMBuildXor;
      break;

    case ON_Add_Ov:
      Build = &LLVMBuildAdd;
      break;
    case ON_Sub_Ov:
      Build = &LLVMBuildSub;
      break;
    case ON_Mul_Ov:
      Build = &LLVMBuildMul;
      break;
    case ON_Div_Ov:
      if (ArgKind == OTKUnsigned)
	Build = &LLVMBuildUDiv;
      else
	Build = &LLVMBuildSDiv;
      break;
    case ON_Mod_Ov:
      if (ArgKind == OTKUnsigned)
	Build = &LLVMBuildURem;
      else
	Build = &BuildSMod;
      break;
    case ON_Rem_Ov:
      if (ArgKind == OTKUnsigned)
	Build = &LLVMBuildURem;
      else
	Build = &LLVMBuildSRem;
      break;
    default:
      abort();
    }
    break;

  case OTKFloat:
    switch (Kind) {
    case ON_Add_Ov:
      Build = &LLVMBuildFAdd;
      break;
    case ON_Sub_Ov:
      Build = &LLVMBuildFSub;
      break;
    case ON_Mul_Ov:
      Build = &LLVMBuildFMul;
      break;
    case ON_Div_Ov:
      Build = &LLVMBuildFDiv;
      break;
    default:
      llvm_unreachable("bad float dyadic op");
    }
    break;

  default:
    abort();
  }

  Res = Build(Builder, Left.Ref, Right.Ref, "");
  return {Res, Left.Etype};
}

extern "C" OEnode
new_convert (OEnode Val, OTnode Rtype)
{
  if (Unreach) {
    return {nullptr, Rtype};
  }

  if (Rtype == Val.Etype) {
    //  Same type, nothing to do
    return Val;
  }

  if (Rtype->Ref == Val.Etype->Ref) {
    //  Same undelaying LLVM type.  No conversion.
    return {Val.Ref, Rtype};
  }

  LLVMValueRef Res;

  switch(Rtype->Kind) {
  case OTKUnsigned:
  case OTKSigned:
  case OTKEnum:
  case OTKBool:
    switch(Val.Etype->Kind) {
    case OTKUnsigned:
    case OTKSigned:
    case OTKEnum:
    case OTKBool:
      //  Int to Int
      if (static_cast<OTnodeScal*>(Val.Etype)->ScalSize
	  > static_cast<OTnodeScal*>(Rtype)->ScalSize)
	Res = LLVMBuildTrunc(Builder, Val.Ref, Rtype->Ref, "");
      else if (static_cast<OTnodeScal*>(Val.Etype)->ScalSize
	       < static_cast<OTnodeScal*>(Rtype)->ScalSize) {
	if (Val.Etype->Kind == OTKSigned)
	  Res = LLVMBuildSExt(Builder, Val.Ref, Rtype->Ref, "");
	else
	  Res = LLVMBuildZExt(Builder, Val.Ref, Rtype->Ref, "");
      } else {
	Res = LLVMBuildBitCast(Builder, Val.Ref, Rtype->Ref, "");
      }
      break;
    case OTKFloat:
      //  Float to Int
      {
	LLVMValueRef V;
	LLVMValueRef Args[2];
	Args[0] = Fp0_5;
	Args[1] = Val.Ref;
	V = LLVMBuildCall(Builder, CopySignFun, Args, 2, "");
	V = LLVMBuildFAdd(Builder, V, Val.Ref, "");
	Res = LLVMBuildFPToSI(Builder, V, Rtype->Ref, "");
      }
      break;
    default:
      llvm_unreachable("bad convert type");
    }
    break;
  case OTKFloat:
    // x to Float
    switch (Val.Etype->Kind) {
    case OTKSigned:
      Res = LLVMBuildSIToFP(Builder, Val.Ref, Rtype->Ref, "");
      break;
    case OTKUnsigned:
      Res = LLVMBuildUIToFP(Builder, Val.Ref, Rtype->Ref, "");
      break;
    default:
      abort();
    }
    break;
  case OTKAccess:
  case OTKIncompleteAccess:
    assert(LLVMGetTypeKind(LLVMTypeOf(Val.Ref)) == LLVMPointerTypeKind);
    Res = LLVMBuildBitCast(Builder, Val.Ref, Rtype->Ref, "");
    break;
  default:
    abort();
  }
  return {Res, Rtype};
}

extern "C" OEnode
new_convert_ov (OEnode Val, OTnode Rtype)
{
  return new_convert(Val, Rtype);
}

extern "C" OEnode
new_alloca (OTnode Rtype, OEnode Size)
{
  LLVMValueRef Res;

  if (Unreach)
    Res = nullptr;
  else {
    if (CurrentDeclareBlock->StackValue != nullptr
	&& CurrentDeclareBlock->Prev != nullptr) {
      // Save the stack pointer at the entry of the block.
      LLVMValueRef FirstInsn =
	LLVMGetFirstInstruction(CurrentDeclareBlock->StmtBB);
      LLVMBuilderRef Bld;
      if (FirstInsn == nullptr) {
	//  Alloca is the first instruction
	Bld = Builder;
      } else {
	LLVMPositionBuilderBefore(ExtraBuilder, FirstInsn);
	Bld = ExtraBuilder;
      }
      CurrentDeclareBlock->StackValue =
	LLVMBuildCall(Bld, StackSaveFun, nullptr, 0, "");
    }
    Res = LLVMBuildArrayAlloca(Builder, LLVMInt8Type(), Size.Ref, "");
    //  Convert
    Res = LLVMBuildBitCast(Builder, Res, Rtype->Ref, "");
  }
  return {Res, Rtype};
}

extern "C" OCnode
new_subprogram_address (ODnodeSubprg *Subprg, OTnode Atype)
{
  return { LLVMConstBitCast(Subprg->Ref, Atype->Ref), Atype };
}

struct OGnode {
  LLVMValueRef Ref;
  OTnode Gtype;
};

extern "C" OGnode
new_global (ODnode Decl)
{
  return {Decl->Ref, Decl->Dtype };
}

extern "C" OGnode
new_global_selected_element (OGnode Rec, OFnodeBase *El)
{
  LLVMValueRef Res;

  switch(El->Kind) {
  case OF_Record:
    {
      LLVMValueRef Idx[2];
      Idx[0] = LLVMConstInt(LLVMInt32Type(), 0, 0);
      Idx[1] = LLVMConstInt(LLVMInt32Type(),
			    static_cast<OFnodeRec *>(El)->Index, 0);
      Res = LLVMConstGEP(Rec.Ref, Idx, 2);
    }
    break;
  case OF_Union:
    Res = LLVMConstBitCast(Rec.Ref, static_cast<OFnodeUnion *>(El)->PtrType);
    break;
  }
  return {Res, El->FType};
}

extern "C" OCnode
new_global_unchecked_address (OGnode Lvalue, OTnode Atype)
{
  return {LLVMConstBitCast(Lvalue.Ref, Atype->Ref), Atype};
}

extern "C" OCnode
new_global_address (OGnode Lvalue, OTnode Atype)
{
  return new_global_unchecked_address(Lvalue, Atype);
}

struct OLnode {
  bool Direct;
  LLVMValueRef Ref;
  OTnode Ltype;
};

extern "C" OLnode
new_obj (ODnode Obj)
{
  switch(Obj->getKind()) {
  case ODKConst:
  case ODKVar:
  case ODKLocal:
    return { false, Obj->Ref, Obj->Dtype };
  case ODKInterface:
    return { true, Obj->Ref, Obj->Dtype };
  case ODKType:
  case ODKSubprg:
  default:
    llvm_unreachable("bad new_obj obj");
  }
}

extern "C" OEnode
new_value (OLnode *Lvalue)
{
  LLVMValueRef Res;

  if (Unreach)
    Res = nullptr;
  else {
    if (Lvalue->Direct)
      Res = Lvalue->Ref;
    else
      Res = LLVMBuildLoad(Builder, Lvalue->Ref, "");
  }
  return {Res, Lvalue->Ltype };
}

extern "C" OEnode
new_obj_value (ODnode Obj)
{
  OLnode t = new_obj(Obj);
  return new_value (&t);
}

extern "C" OLnode
new_indexed_element (OLnode *Arr, OEnode Index)
{
  LLVMValueRef Idx[2];
  LLVMValueRef Res;

  if (Unreach)
    Res = nullptr;
  else {
    Idx[0] = LLVMConstInt(LLVMInt32Type(), 0, 0);
    Idx[1] = Index.Ref;
    Res = LLVMBuildGEP(Builder, Arr->Ref, Idx, 2, "");
  }
  return { false, Res, static_cast<OTnodeArr *>(Arr->Ltype)->ElType };
}

extern "C" OLnode
new_slice (OLnode *Arr, OTnode Rtype, OEnode Index)
{
  LLVMValueRef Idx[2];
  LLVMValueRef Res;

  if (Unreach)
    Res = nullptr;
  else {
    Idx[0] = LLVMConstInt(LLVMInt32Type(), 0, 0);
    Idx[1] = Index.Ref;
    Res = LLVMBuildGEP(Builder, Arr->Ref, Idx, 2, "");
    Res = LLVMBuildBitCast(Builder, Res, LLVMPointerType(Rtype->Ref, 0), "");
  }
  return { false, Res, Rtype};
}

extern "C" OLnode
new_selected_element (OLnode *Rec, OFnodeBase *El)
{
  LLVMValueRef Res;

  if (Unreach)
    Res = nullptr;
  else {
    switch(El->Kind) {
    case OF_Record:
      {
	LLVMValueRef Idx[2];
	Idx[0] = LLVMConstInt(LLVMInt32Type(), 0, 0);
	Idx[1] = LLVMConstInt(LLVMInt32Type(),
			      static_cast<OFnodeRec *>(El)->Index, 0);
	Res = LLVMBuildGEP(Builder, Rec->Ref, Idx, 2, "");
      }
      break;
    case OF_Union:
      Res = LLVMBuildBitCast(Builder, Rec->Ref,
			     static_cast<OFnodeUnion *>(El)->PtrType, "");
      break;
    }
  }
  return { false, Res, El->FType };
}

extern "C" OLnode
new_access_element (OEnode Acc)
{
  LLVMValueRef Res;

  switch(Acc.Etype->Kind) {
  case OTKAccess:
    Res = Acc.Ref;
    break;
  case OTKIncompleteAccess:
    //  Unwrap the structure
    {
      LLVMValueRef Idx[2];

      Idx[0] = LLVMConstInt(LLVMInt32Type(), 0, 0);
      Idx[1] = LLVMConstInt(LLVMInt32Type(), 0, 0);
      Res = LLVMBuildGEP(Builder, Acc.Ref, Idx, 2, "");
    }
    break;
  default:
    llvm_unreachable("bad new_access_element");
  }
  return {false, Res, static_cast<OTnodeAccBase *>(Acc.Etype)->Acc };
}

extern "C" OEnode
new_unchecked_address (OLnode *Lvalue, OTnode Atype)
{
  LLVMValueRef Res;

  if (Unreach)
    Res = nullptr;
  else
    Res = LLVMBuildBitCast(Builder, Lvalue->Ref, Atype->Ref, "");
  return {Res, Atype};
}

extern "C" OEnode
new_address (OLnode *Lvalue, OTnode Atype)
{
  return new_unchecked_address(Lvalue, Atype);
}

extern "C" void
new_assign_stmt (OLnode *Target, OEnode Value)
{
  assert (!Target->Direct);
  if (!Unreach) {
    LLVMBuildStore(Builder, Value.Ref, Target->Ref);
  }
}

extern "C" void
new_debug_line_decl (unsigned Line)
{
#ifdef USE_DEBUG
  DebugCurrentLine = Line;
#endif
}

extern "C" void
new_debug_line_stmt (unsigned Line)
{
#ifdef USE_DEBUG
  DebugCurrentLine = Line;
#endif
}
