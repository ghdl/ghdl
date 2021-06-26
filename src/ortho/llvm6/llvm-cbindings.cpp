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

//  Style:
//  C bindings for types, instructions
//  C++ API for debug
//
//  Later move to C++ only.

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
#if LLVM_VERSION_MAJOR >= 7
//  Not present in llvm-6, present in llvm-7
#include "llvm-c/Transforms/Utils.h"
#endif

#if LLVM_VERSION_MAJOR >= 6
#define USE_DEBUG
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
static bool FlagDebug = false;

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
static DICompileUnit *DebugCurrentCU;

// Current subprogram.  Used by types, parameters and static consts.
static DISubprogram *DebugCurrentSubprg;

// Current scope.  Used by automatic variables and line locations.
static DIScope *DebugCurrentScope;

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
set_debug_level (unsigned level)
{
  switch(level) {
  case 0:
    FlagDebug = false;
    FlagDebugLines = false;
    break;
  case 1:
    FlagDebug = false;
    FlagDebugLines = true;
    break;
  default:
    FlagDebug = true;
    FlagDebugLines = true;
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

#ifdef USE_DEBUG
  if (FlagDebugLines) {
    DBuilder->finalize();
  }
#endif

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

    LLVMAddCFGSimplificationPass (PassManager);
    LLVMAddPromoteMemoryToRegisterPass (PassManager);

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
    DebugCurrentCU = DBuilder->createCompileUnit
      (llvm::dwarf::DW_LANG_C, DebugCurrentFile, StringRef("ortho-llvm"),
       Optimization > LLVMCodeGenLevelNone, StringRef(), 0);

    DebugCurrentScope = DebugCurrentCU;
  }
#endif
}

//  Set debug location on instruction RES
static void
setDebugLocation(LLVMValueRef Res)
{
#ifdef USE_DEBUG
  if (FlagDebugLines) {
    unwrap(Builder)->SetInstDebugLocation(static_cast<Instruction*>(unwrap(Res)));
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
#ifdef USE_DEBUG
  DIType *Dbg;
#endif

  //  Kind of type.
  OTKind Kind;

  //  If true, the type is bounded: all the elements have a defined size.
  bool Bounded;

  OTnodeBase (LLVMTypeRef R, OTKind K, bool Bounded) :
    Ref(R),
#ifdef USE_DEBUG
    Dbg(nullptr),
#endif
    Kind(K), Bounded(Bounded) {}

  unsigned getAlignment() const {
    return LLVMABIAlignmentOfType(TheTargetData, Ref);
  }
  unsigned long long getSize() const {
    return LLVMABISizeOfType(TheTargetData, Ref);
  }
  unsigned long long getBitSize() const {
    return 8 * getSize();
  }
};

typedef OTnodeBase *OTnode;

struct OTnodeScal : OTnodeBase {
  //  For scalar: the size in bits
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

struct OTnodeEnumBase : OTnodeScal {
#ifdef USE_DEBUG
  DINodeArray *DbgEls;
#endif
  OTnodeEnumBase (LLVMTypeRef R, OTKind K, unsigned Sz) :
    OTnodeScal(R, K, Sz) {}
};

struct OTnodeEnum : OTnodeEnumBase {
  OTnodeEnum (LLVMTypeRef R, unsigned Sz) :
    OTnodeEnumBase(R, OTKEnum, Sz) {}
};

struct OEnumList {
  LLVMTypeRef Ref;
  unsigned Pos;
  OTnodeEnum *Etype;
#ifdef USE_DEBUG
  SmallVector<Metadata *, 8> *Dbg;
#endif
};

extern "C" void
start_enum_type (OEnumList *List, unsigned Sz)
{
  LLVMTypeRef T = SizeToLLVM(Sz);

  *List = {T, 0, new OTnodeEnum(T, Sz)
#ifdef USE_DEBUG
           , nullptr
#endif
  };

#ifdef USE_DEBUG
  if (FlagDebug)
    List->Dbg = new SmallVector<Metadata *, 8>();
#endif
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
  *Res = {LLVMConstInt(List->Ref, List->Pos, 0), List->Etype};

#ifdef USE_DEBUG
  if (FlagDebug) {
    DIEnumerator *D;

    //  Note: IsUnsigned argument is not available in LLVM 6.0
    D = DBuilder->createEnumerator (StringRef(Ident.cstr), List->Pos);

    List->Dbg->push_back(D);
  }
#endif

  List->Pos++;
}

extern "C" void
finish_enum_type (OEnumList *List, OTnodeEnum **Res)
{
  *Res = List->Etype;
#ifdef USE_DEBUG
  if (FlagDebug) {
    List->Etype->DbgEls =
      new DINodeArray(DBuilder->getOrCreateArray(*List->Dbg));
    delete List->Dbg;
  }
#endif
}

struct OTnodeBool : OTnodeEnumBase {
  OTnodeBool (LLVMTypeRef R) : OTnodeEnumBase(R, OTKBool, 1) {}
};

extern "C" void
new_boolean_type(OTnode *Res,
		 OIdent FalseId, OCnode *False_E,
		 OIdent TrueId, OCnode *True_E)
{
  OTnodeBool *T = new OTnodeBool(LLVMInt1Type());
  *Res = T;

  *False_E = {LLVMConstInt(T->Ref, 0, 0), T};
  *True_E = {LLVMConstInt(T->Ref, 1, 0), T};

#ifdef USE_DEBUG
  if (FlagDebug) {
    SmallVector<Metadata *, 2> DbgEls;
    DbgEls.push_back(DBuilder->createEnumerator (StringRef(FalseId.cstr), 0));
    DbgEls.push_back(DBuilder->createEnumerator (StringRef(TrueId.cstr), 1));
    T->DbgEls = new DINodeArray(DBuilder->getOrCreateArray(DbgEls));
  }
#endif
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
#ifdef USE_DEBUG
  if (FlagDebug) {
    //  The '3' is a little bit magic, but correspond to the base type as
    //  defined (e.g.) in DebugInfoMetadata.h for DIDerivedType::getBaseType()
    AccType->Dbg->replaceOperandWith(3, DType->Dbg);
  }
#endif
}

extern "C" OCnode
new_null_access (OTnode LType)
{
  return {LLVMConstNull(LType->Ref), LType};
}

enum OFKind { OF_Record, OF_Union};

struct OFnodeBase {
  OFKind Kind;
  OTnode FType;
  OIdent Ident;
  OFnodeBase(OFKind Kind, OTnode FType, OIdent Ident) :
    Kind(Kind), FType(FType), Ident(Ident) {}
};

struct OElementList {
  OFKind Kind;

  //  Number of fields.
  unsigned BndCount;

  //  For record: the access to the incomplete (but named) type.
  OTnode RecType;

  //  For unions: biggest for size and alignment
  unsigned Size;
  unsigned Align;
  //  For unions: type with the biggest alignment.
  LLVMTypeRef AlignType;

  std::vector<OFnodeBase *> *Els;
};

extern "C" void
start_record_type (OElementList *Elements)
{
  *Elements = {OF_Record,
	       0,
	       nullptr,
	       0, 0, nullptr,
               new std::vector<OFnodeBase *>()};
}

struct OFnodeRec : OFnodeBase {
  unsigned Index;
  OFnodeRec(OTnode Etype, OIdent Ident, unsigned Index) :
    OFnodeBase(OF_Record, Etype, Ident), Index(Index) {}
};

struct OFnodeUnion : OFnodeBase {
  LLVMTypeRef Utype;
  //  Pointer type - used to do conversion between the union and the field.
  LLVMTypeRef PtrType;
  OFnodeUnion(OTnode Etype, OIdent Ident, LLVMTypeRef PtrType) :
    OFnodeBase(OF_Union, Etype, Ident), Utype(Etype->Ref), PtrType(PtrType) {}
};

extern "C" void
new_record_field(OElementList *Elements,
		 OFnodeRec **El, OIdent Ident, OTnode Etype)
{
  *El = new OFnodeRec(Etype, Ident, Etype->Bounded ? Elements->BndCount : ~0U);
  Elements->Els->push_back(*El);
  if (Etype->Bounded)
    Elements->BndCount++;
}

struct OTnodeRecBase : OTnodeBase {
  std::vector<OFnodeBase *> Els;
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

#ifdef USE_DEBUG
static DINodeArray
buildDebugRecordElements(OTnodeRecBase *Atype)
{
  std::vector<Metadata *> els;

  els.reserve(Atype->Els.size());

  unsigned i = 0;
  for (OFnodeBase *e : Atype->Els) {
    if (!e->FType->Bounded)
      break;
    unsigned bitoff = 8 * LLVMOffsetOfElement(TheTargetData, Atype->Ref, i++);
    els.push_back(DBuilder->createMemberType
		  (DebugCurrentSubprg, StringRef(e->Ident.cstr), NULL, 0,
		   e->FType->getBitSize(), /* align */ 0,
		   bitoff, DINode::DIFlags::FlagZero, e->FType->Dbg));
  }

  return DBuilder->getOrCreateArray(els);
}
#endif

extern "C" void
finish_record_type(OElementList *Els, OTnode *Res)
{
  LLVMTypeRef *Types = new LLVMTypeRef[Els->BndCount];

  //  Create types array for elements.
  int i = 0;
  bool Bounded = true;
  for (OFnodeBase *Field : *Els->Els) {
    if (Field->FType->Bounded)
      Types[i++] = Field->FType->Ref;
    else
      Bounded = false;
  }
  assert(i == Els->BndCount);

  OTnodeRecBase *T;

  if (Els->RecType != nullptr) {
    //  Completion
    LLVMStructSetBody (Els->RecType->Ref, Types, Els->BndCount, 0);
    Els->RecType->Bounded = Bounded;
    T = static_cast<OTnodeRecBase *>(Els->RecType);
    T->Els = std::move(*Els->Els);
#ifdef USE_DEBUG
    if (FlagDebug) {
      DICompositeType *Dbg;
      Dbg = DBuilder->createStructType
        (DebugCurrentSubprg, T->Dbg->getName(), DebugCurrentFile,
         DebugCurrentLine, T->getBitSize(), /* Align */ 0,
         DINode::DIFlags::FlagZero, nullptr,
         buildDebugRecordElements(T));
      llvm::TempMDNode fwd_decl(T->Dbg);
      T->Dbg = DBuilder->replaceTemporary(std::move(fwd_decl), Dbg);
    }
#endif
  } else {
    //  Non-completion.
    //  Debug info are created when the type is declared.
    T = new OTnodeRec(LLVMStructType(Types, Els->BndCount, 0), Bounded);
    T->Els = std::move(*Els->Els);
  }
  *Res = T;
}

struct OElementSublist {
  //  Number of fields.
  unsigned Count;
  std::vector<OFnodeBase *> *Base_Els;
  std::vector<OFnodeBase *> *Els;
};

extern "C" void
start_record_subtype (OTnodeRec *Rtype, OElementSublist *Elements)
{
  *Elements = {0,
               &Rtype->Els,
               new std::vector<OFnodeBase *>()};
}

extern "C" void
new_subrecord_field(OElementSublist *Elements,
                    OFnodeRec **El, OTnode Etype)
{
  OFnodeBase *Bel = (*Elements->Base_Els)[Elements->Count];
  *El = new OFnodeRec(Etype, Bel->Ident, Elements->Count);
  Elements->Els->push_back(*El);
  Elements->Count++;
}

extern "C" void
finish_record_subtype(OElementSublist *Els, OTnode *Res)
{
  LLVMTypeRef *Types = new LLVMTypeRef[Els->Count];

  //  Create types array for elements.
  int i = 0;
  for (OFnodeBase *Field : *Els->Els) {
    Types[i++] = Field->FType->Ref;
  }

  OTnodeRecBase *T;
  T = new OTnodeRec(LLVMStructType(Types, Els->Count, 0), true);
  T->Els = std::move(*Els->Els);
  *Res = T;
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
          new std::vector<OFnodeBase *>()};
}

extern "C" void
start_union_type(OElementList *Els)
{
  *Els = {OF_Union,
	  0,
	  nullptr,
	  0, 0, nullptr,
          new std::vector<OFnodeBase *>()};
}

extern "C" void
new_union_field(OElementList *Els, OFnodeUnion **El,
		OIdent Ident, OTnode Etype)
{
  unsigned Size = Etype->getSize();
  unsigned Align = Etype->getAlignment();

  *El = new OFnodeUnion(Etype, Ident, LLVMPointerType(Etype->Ref, 0));

  if (Size > Els->Size)
    Els->Size = Size;
  if (Els->AlignType == nullptr || Align > Els->Align) {
    Els->Align = Align;
    Els->AlignType = Etype->Ref;
  }
  Els->Els->push_back(*El);
}

struct OTnodeUnion : OTnodeBase {
  //  For unions
  std::vector<OFnodeBase *> Els;
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

  OTnodeUnion *T;
  T = new OTnodeUnion(LLVMStructType(Types, Count, 0),
                      Els->Size, Els->AlignType);
  T->Els = std::move(*Els->Els);
  *Res = T;
  delete Els->Els;
}

struct OTnodeArr : OTnodeBase {
  //  For arrays: type of the element
  OTnode ElType;

  OTnodeArr(LLVMTypeRef R, bool Bounded, OTnode E) :
    OTnodeBase(R, OTKArray, Bounded), ElType(E) {}
};

#ifdef USE_DEBUG
static void
addArrayDebug(OTnodeArr *Atype, unsigned Len)
{
  DISubrange *Rng;

  Rng = DBuilder->getOrCreateSubrange(0, Len);
  SmallVector<Metadata *, 1> Subscripts;
  Subscripts.push_back(Rng);

  OTnode ElType = static_cast<OTnodeArr *>(Atype)->ElType;

  Atype->Dbg = DBuilder->createArrayType
    (Atype->getBitSize(), /* align */ 0,
     ElType->Dbg, DBuilder->getOrCreateArray(Subscripts));
}
#endif

extern "C" OTnode
new_array_type(OTnode ElType, OTnode IndexType)
{
  OTnodeArr *Res;
  unsigned Len = 0;

  Res = new OTnodeArr(LLVMArrayType(ElType->Ref, Len), false, ElType);

#ifdef USE_DEBUG
  if (FlagDebug)
    addArrayDebug(Res, Len);
#endif

  return Res;
}

extern "C" OTnode
new_array_subtype(OTnodeArr *ArrType, OTnode ElType, OCnode *Length)
{
  OTnodeArr *Res;
  unsigned Len = LLVMConstIntGetZExtValue(Length->Ref);

  Res = new OTnodeArr(LLVMArrayType(ElType->Ref, Len),
                      ElType->Bounded,
                      ElType);

#ifdef USE_DEBUG
  if (FlagDebug)
    addArrayDebug(Res, Len);
#endif

  return Res;
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

#ifdef USE_DEBUG
  //  Add dwarf type.
  if (FlagDebug) {
    switch(Atype->Kind) {
    case OTKUnsigned:
      Atype->Dbg = DBuilder->createBasicType
        (StringRef(Ident.cstr), static_cast<OTnodeScal*>(Atype)->ScalSize,
         dwarf::DW_ATE_unsigned);
      break;
    case OTKSigned:
      Atype->Dbg = DBuilder->createBasicType
        (StringRef(Ident.cstr), static_cast<OTnodeScal*>(Atype)->ScalSize,
         dwarf::DW_ATE_signed);
      break;
    case OTKFloat:
      Atype->Dbg = DBuilder->createBasicType
        (StringRef(Ident.cstr), static_cast<OTnodeScal*>(Atype)->ScalSize,
         dwarf::DW_ATE_float);
      break;
    case OTKEnum:
    case OTKBool:
      Atype->Dbg = DBuilder->createEnumerationType
        (DebugCurrentSubprg, StringRef(Ident.cstr), DebugCurrentFile,
         DebugCurrentLine, static_cast<OTnodeEnumBase*>(Atype)->ScalSize,
         Atype->getAlignment(),
         *static_cast<OTnodeEnumBase*>(Atype)->DbgEls, nullptr);
      delete static_cast<OTnodeEnumBase*>(Atype)->DbgEls;
      break;

    case OTKIncompleteAccess:
      if (static_cast<OTnodeAccBase*>(Atype)->Acc == nullptr) {
        //  Still incomplete
        Atype->Dbg = DBuilder->createPointerType
          (nullptr, Atype->getBitSize(), 0, None, StringRef(Ident.cstr));
        break;
      }
      // Fallthrough
    case OTKAccess:
      Atype->Dbg = DBuilder->createPointerType
        (static_cast<OTnodeAcc*>(Atype)->Acc->Dbg,
         Atype->getBitSize(), 0, None, StringRef(Ident.cstr));
      break;

    case OTKArray:
      //  The debug info has already been created for arrays, as they can be
      //  anonymous
      Atype->Dbg = DBuilder->createTypedef
        (Atype->Dbg, StringRef(Ident.cstr), DebugCurrentFile,
         DebugCurrentLine, DebugCurrentSubprg);
      break;

    case OTKRecord:
      Atype->Dbg = DBuilder->createStructType
        (DebugCurrentSubprg, StringRef(Ident.cstr), DebugCurrentFile,
         DebugCurrentLine, Atype->getBitSize(), /* align */ 0,
         DINode::DIFlags::FlagPublic, nullptr,
         buildDebugRecordElements(static_cast<OTnodeRecBase *>(Atype)));
      break;

    case OTKUnion:
      {
        unsigned Count = static_cast<OTnodeUnion *>(Atype)->Els.size();
        std::vector<Metadata *> els(Count);

        unsigned i = 0;
        for (OFnodeBase *e : static_cast<OTnodeUnion *>(Atype)->Els) {
          els[i++] = DBuilder->createMemberType
            (DebugCurrentSubprg, StringRef(e->Ident.cstr), DebugCurrentFile,
             DebugCurrentLine, e->FType->getBitSize(),
             e->FType->getAlignment(), 0, DINode::DIFlags::FlagPublic,
             e->FType->Dbg);
        }

        Atype->Dbg = DBuilder->createUnionType
          (DebugCurrentSubprg, StringRef(Ident.cstr), DebugCurrentFile,
           DebugCurrentLine, Atype->getBitSize(), Atype->getAlignment(),
           DINode::DIFlags::FlagPublic, DBuilder->getOrCreateArray(els));
      }
      break;

    case OTKIncompleteRecord:
      Atype->Dbg = DBuilder->createReplaceableCompositeType
        (dwarf::DW_TAG_structure_type, StringRef(Ident.cstr),
         DebugCurrentSubprg, DebugCurrentFile, DebugCurrentLine);
      break;
    }
  }
#endif
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
new_record_sizeof(OTnode Atype, OTnode Rtype)
{
  return new_sizeof(Atype, Rtype);
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
#ifdef USE_DEBUG
    if (FlagDebug && !Unreach) {
      DILocalVariable *D;

      D = DBuilder->createAutoVariable
        (DebugCurrentScope, StringRef(Ident.cstr), DebugCurrentFile,
         DebugCurrentLine, Atype->Dbg, true);
      DBuilder->insertDeclare
        (unwrap(Decl), D, DBuilder->createExpression(),
         DILocation::get(DebugCurrentScope->getContext(), DebugCurrentLine, 0, DebugCurrentScope),
         unwrap(LLVMGetInsertBlock(DeclBuilder)));
    }
#endif
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
    case O_Storage_External:
    case O_Storage_Local:
      break;
    }

#ifdef USE_DEBUG
    if (FlagDebug) {
      DIGlobalVariableExpression *GVE;

      GVE = DBuilder->createGlobalVariableExpression
        (DebugCurrentSubprg, StringRef(Ident.cstr), StringRef(),
         DebugCurrentFile, DebugCurrentLine, Atype->Dbg,
         Storage == O_Storage_Private);
      static_cast<GlobalVariable*>(unwrap(Decl))->addDebugInfo(GVE);
    }
#endif
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

  if (Storage == O_Storage_Local)
    abort();

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
  ODnodeConst *Cst = *Decl;

  LLVMValueRef Ref = Cst->Ref;

  if (Ref == nullptr) {
    Ref = LLVMAddGlobal(TheModule, LLVMTypeOf(Val->Ref), Cst->Ident.cstr);
    setConstAttributes(Ref, Cst->Storage);
    Cst->Ref = Ref;
  }

  LLVMSetInitializer(Ref, Val->Ref);

#ifdef USE_DEBUG
  if (FlagDebug && Cst->Dtype->Dbg != nullptr) {
    DIGlobalVariableExpression *GVE;

    //  Note: the scope of a global expression cannot be a lexical scope.
    GVE = DBuilder->createGlobalVariableExpression
      (DebugCurrentSubprg,
       StringRef(Cst->Ident.cstr), StringRef(),
       DebugCurrentFile, DebugCurrentLine,
       DBuilder->createQualifiedType(dwarf::DW_TAG_const_type, Cst->Dtype->Dbg),
       Cst->Storage == O_Storage_Private);
    static_cast<GlobalVariable*>(unwrap(Ref))->addDebugInfo(GVE);
    }
#endif
}

struct ODnodeInter : ODnodeBase {
  OIdent Ident;
  ODKind getKind() const override { return ODKInterface; }
  ODnodeInter(LLVMValueRef R, OTnode T, OIdent Id) :
    ODnodeBase(R, T), Ident(Id) {}
};

struct OInterList {
  //  Subprogram
  OIdent Ident;
  OStorage Storage;
  OTnode Rtype;

  //  Number of interfaces.
  std::vector<ODnodeInter *> *Inters;
};

extern "C" void
start_function_decl(OInterList *Inters, OIdent Ident, OStorage Storage,
		    OTnode Rtype)
{
  *Inters = { Ident, Storage, Rtype,
              new std::vector<ODnodeInter *>() };
}

extern "C" void
start_procedure_decl(OInterList *Inters, OIdent Ident, OStorage Storage)
{
  *Inters = { Ident, Storage, nullptr,
              new std::vector<ODnodeInter *>() };
}

extern "C" void
new_interface_decl(OInterList *Inters,
		   ODnode *Res, OIdent Ident, OTnode Itype)
{
  ODnodeInter *Decl = new ODnodeInter(nullptr, Itype, Ident);

  *Res = Decl;

  Inters->Inters->push_back(Decl);
}

struct ODnodeSubprg : ODnodeBase {
  // Interfaces
  std::vector<ODnodeInter *> Inters;
  //  Storage
  OStorage Storage;
  OIdent Ident;
  ODKind getKind() const override { return ODKSubprg; }
  ODnodeSubprg(LLVMValueRef R, OTnode T, OStorage S, OIdent Id,
               const std::vector<ODnodeInter *> &Inters) :
    ODnodeBase(R, T), Inters(Inters), Storage(S), Ident(Id) {}
};

extern "C" void
finish_subprogram_decl(OInterList *Inters, ODnodeSubprg **Res)
{
  unsigned ArgsCount = Inters->Inters->size();
  LLVMTypeRef *Types = new LLVMTypeRef[ArgsCount];

  //  Build array of interface types.
  int i = 0;
  for (ODnodeInter *Inter: *Inters->Inters)
    Types[i++] = Inter->Dtype->Ref;

  //  Return type.
  LLVMTypeRef Rtype;
  if (Inters->Rtype == nullptr)
    Rtype = LLVMVoidType();
  else
    Rtype = Inters->Rtype->Ref;

  LLVMTypeRef Ftype = LLVMFunctionType(Rtype, Types, ArgsCount, 0);

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

  //  Translate interfaces
  i = 0;
  for (ODnodeInter *Inter: *Inters->Inters) {
    Inter->Ref = LLVMGetParam(Decl, i);
    LLVMSetValueName(Inter->Ref, Inter->Ident.cstr);
    i++;
  }

  //  Create the result.
  ODnodeSubprg *R;
  R = new ODnodeSubprg(Decl, Inters->Rtype, Inters->Storage, Inters->Ident,
                       std::move(*Inters->Inters));
  *Res = R;
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

#ifdef USE_DEBUG
  DIScope *DebugPrevScope;
#endif
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
  *Res = { nullptr, nullptr, CurrentDeclareBlock
#ifdef USE_DEBUG
           , nullptr
#endif
  };
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
#ifdef USE_DEBUG
  if (FlagDebugLines) {
    DISubroutineType *Ty;

    std::vector<Metadata *> ParamsArr;

    if (FlagDebug) {
      //  First, the return type.
      if (Func->Dtype != nullptr)
        ParamsArr.push_back(Func->Dtype->Dbg);
      else
        ParamsArr.push_back(nullptr);

      //  Then the arguments type.
      for (ODnodeInter *Inter: Func->Inters)
        ParamsArr.push_back(Inter->Dtype->Dbg);
    }

    DITypeRefArray Params = DBuilder->getOrCreateTypeArray(ParamsArr);
    Ty = DBuilder->createSubroutineType(Params);

#if LLVM_VERSION_MAJOR >= 8
    //  For LLVM 8.0
    DebugCurrentSubprg = DBuilder->createFunction
      (DebugCurrentScope, StringRef(Func->Ident.cstr), StringRef(),
       DebugCurrentFile, DebugCurrentLine, Ty, DebugCurrentLine,
       Func->Storage == O_Storage_Private ? DINode::FlagPrivate : DINode::FlagPublic,
       DISubprogram::SPFlagDefinition);
#else
    DebugCurrentSubprg = DBuilder->createFunction
      (DebugCurrentScope, StringRef(Func->Ident.cstr), StringRef(),
       DebugCurrentFile, DebugCurrentLine, Ty,
       Func->Storage == O_Storage_Private, true, DebugCurrentLine);
#endif
    static_cast<Function*>(unwrap(CurrentFunc))->setSubprogram(DebugCurrentSubprg);
    DebugCurrentScope = DebugCurrentSubprg;

    unwrap(Builder)->SetCurrentDebugLocation
      (DILocation::get(DebugCurrentScope->getContext(), DebugCurrentLine, 0, DebugCurrentScope));
  }

  if (FlagDebug) {
    //  Crate local variables for arguments
    unsigned ArgNo = 1;
    for (ODnodeInter *Inter: Func->Inters) {
      LLVMValueRef Var;

      Var = LLVMBuildAlloca(DeclBuilder, Inter->Dtype->Ref, "");
      DILocalVariable *D = DBuilder->createParameterVariable
        (DebugCurrentSubprg, StringRef(Inter->Ident.cstr), ArgNo++,
         DebugCurrentFile, DebugCurrentLine, Inter->Dtype->Dbg, true);
      DBuilder->insertDeclare
        (unwrap(Var), D, DBuilder->createExpression(),
         DILocation::get(DebugCurrentSubprg->getContext(), DebugCurrentLine, 0, DebugCurrentSubprg),
         unwrap(LLVMGetInsertBlock(DeclBuilder)));
      LLVMBuildStore(DeclBuilder, Inter->Ref, Var);
      Inter->Ref = Var;
    }
  }
#endif
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

#ifdef USE_DEBUG
  if (FlagDebugLines) {
    DBuilder->finalizeSubprogram(DebugCurrentSubprg);
    DebugCurrentSubprg = nullptr;
    DebugCurrentScope = DebugCurrentCU;
  }
#endif
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

#ifdef USE_DEBUG
  if (FlagDebug) {
    CurrentDeclareBlock->DebugPrevScope = DebugCurrentScope;
    DebugCurrentScope = DBuilder->createLexicalBlock
      (DebugCurrentScope, DebugCurrentFile, DebugCurrentLine, 0);
  }
#endif
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

#ifdef USE_DEBUG
    if (FlagDebug) {
      DebugCurrentScope = CurrentDeclareBlock->DebugPrevScope;
    }
#endif
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
  *Assocs = { Subprg, 0, new LLVMValueRef[Subprg->Inters.size()] };
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
			Assocs->Vals, Assocs->Subprg->Inters.size(), "");
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
		  Assocs->Vals, Assocs->Subprg->Inters.size(), "");
  }
  delete Assocs->Vals;
}

extern "C" void
new_func_return_stmt (OEnode Value)
{
  if (Unreach)
    return;
  LLVMValueRef Res = LLVMBuildRet(Builder, Value.Ref);
  setDebugLocation(Res);

  Unreach = true;
}

extern "C" void
new_proc_return_stmt ()
{
  if (Unreach)
    return;
  LLVMValueRef Res = LLVMBuildRetVoid(Builder);
  setDebugLocation(Res);
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
#ifdef USE_DEBUG
    if (FlagDebug) {
      //  The argument was allocated on the stack
      return { false, Obj->Ref, Obj->Dtype };
    }
#endif
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
  if (Unreach)
    return;

  LLVMValueRef Res = LLVMBuildStore(Builder, Value.Ref, Target->Ref);
  setDebugLocation(Res);
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
  if (FlagDebugLines && Line != DebugCurrentLine) {
    DebugCurrentLine = Line;
    unwrap(Builder)->SetCurrentDebugLocation
      (DILocation::get(DebugCurrentScope->getContext(), DebugCurrentLine, 0, DebugCurrentScope));
  }
#endif
}
