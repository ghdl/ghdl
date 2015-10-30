--  GHDL Run Time (GRT) - VHPI implementation for Ada.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

--  Ada oriented implementation of VHPI.
--  This doesn't follow exactly what VHPI defined, but:
--  * it should be easy to write a VHPI interface from this implementation.
--  * this implementation is thread-safe (no global storage).
--  * this implementation never allocates memory.
with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;

package Grt.Avhpi is
   --  Object Kinds.
   type VhpiClassKindT is
     (
      VhpiUndefined,
      VhpiAccessTypeDeclK,
      VhpiAggregateK,
      VhpiAliasDeclK,
      VhpiAllLiteralK,
      VhpiAllocatorK,
      VhpiAnyCollectionK,
      VhpiArchBodyK,
      VhpiArgvK,
      VhpiArrayTypeDeclK,
      VhpiAssertStmtK,
      VhpiAssocElemK,
      VhpiAttrDeclK,
      VhpiAttrSpecK,
      VhpiBinaryExprK,
      VhpiBitStringLiteralK,
      VhpiBlockConfigK,
      VhpiBlockStmtK,
      VhpiBranchK,
      VhpiCallbackK,
      VhpiCaseStmtK,
      VhpiCharLiteralK,
      VhpiCompConfigK,
      VhpiCompDeclK,
      VhpiCompInstStmtK,
      VhpiCondSigAssignStmtK,
      VhpiCondWaveformK,
      VhpiConfigDeclK,
      VhpiConstDeclK,
      VhpiConstParamDeclK,
      VhpiConvFuncK,
      VhpiDeRefObjK,
      VhpiDisconnectSpecK,
      VhpiDriverK,
      VhpiDriverCollectionK,
      VhpiElemAssocK,
      VhpiElemDeclK,
      VhpiEntityClassEntryK,
      VhpiEntityDeclK,
      VhpiEnumLiteralK,
      VhpiEnumRangeK,
      VhpiEnumTypeDeclK,
      VhpiExitStmtK,
      VhpiFileDeclK,
      VhpiFileParamDeclK,
      VhpiFileTypeDeclK,
      VhpiFloatRangeK,
      VhpiFloatTypeDeclK,
      VhpiForGenerateK,
      VhpiForLoopK,
      VhpiForeignfK,
      VhpiFuncCallK,
      VhpiFuncDeclK,
      VhpiGenericDeclK,
      VhpiGroupDeclK,
      VhpiGroupTempDeclK,
      VhpiIfGenerateK,
      VhpiIfStmtK,
      VhpiInPortK,
      VhpiIndexedNameK,
      VhpiIntLiteralK,
      VhpiIntRangeK,
      VhpiIntTypeDeclK,
      VhpiIteratorK,
      VhpiLibraryDeclK,
      VhpiLoopStmtK,
      VhpiNextStmtK,
      VhpiNullLiteralK,
      VhpiNullStmtK,
      VhpiOperatorK,
      VhpiOthersLiteralK,
      VhpiOutPortK,
      VhpiPackBodyK,
      VhpiPackDeclK,
      VhpiPackInstK,
      VhpiParamAttrNameK,
      VhpiPhysLiteralK,
      VhpiPhysRangeK,
      VhpiPhysTypeDeclK,
      VhpiPortDeclK,
      VhpiProcCallStmtK,
      VhpiProcDeclK,
      VhpiProcessStmtK,
      VhpiProtectedTypeK,
      VhpiProtectedTypeBodyK,
      VhpiProtectedTypeDeclK,
      VhpiRealLiteralK,
      VhpiRecordTypeDeclK,
      VhpiReportStmtK,
      VhpiReturnStmtK,
      VhpiRootInstK,
      VhpiSelectSigAssignStmtK,
      VhpiSelectWaveformK,
      VhpiSelectedNameK,
      VhpiSigDeclK,
      VhpiSigParamDeclK,
      VhpiSimpAttrNameK,
      VhpiSimpleSigAssignStmtK,
      VhpiSliceNameK,
      VhpiStringLiteralK,
      VhpiSubpBodyK,
      VhpiSubtypeDeclK,
      VhpiSubtypeIndicK,
      VhpiToolK,
      VhpiTransactionK,
      VhpiTypeConvK,
      VhpiUnaryExprK,
      VhpiUnitDeclK,
      VhpiUserAttrNameK,
      VhpiVarAssignStmtK,
      VhpiVarDeclK,
      VhpiVarParamDeclK,
      VhpiWaitStmtK,
      VhpiWaveformElemK,
      VhpiWhileLoopK,

      --  Iterator, but on a name.
      AvhpiNameIteratorK,

      --  Root scope that contains the top entity.  For vpi.
      AvhpiRootScopeK,
      AvhpiRootScopeIteratorK
     );

   type VhpiOneToOneT is
     (
      VhpiAbstractLiteral,
      VhpiActual,
      VhpiAllLiteral,
      VhpiAttrDecl,
      VhpiAttrSpec,
      VhpiBaseType,
      VhpiBaseUnit,
      VhpiBasicSignal,
      VhpiBlockConfig,
      VhpiCaseExpr,
      VhpiCondExpr,
      VhpiConfigDecl,
      VhpiConfigSpec,
      VhpiConstraint,
      VhpiContributor,
      VhpiCurCallback,
      VhpiCurEqProcess,
      VhpiCurStackFrame,
      VhpiDeRefObj,
      VhpiDecl,
      VhpiDesignUnit,
      VhpiDownStack,
      VhpiElemSubtype,
      VhpiEntityAspect,
      VhpiEntityDecl,
      VhpiEqProcessStmt,
      VhpiExpr,
      VhpiFormal,
      VhpiFuncDecl,
      VhpiGroupTempDecl,
      VhpiGuardExpr,
      VhpiGuardSig,
      VhpiImmRegion,
      VhpiInPort,
      VhpiInitExpr,
      VhpiIterScheme,
      VhpiLeftExpr,
      VhpiLexicalScope,
      VhpiLhsExpr,
      VhpiLocal,
      VhpiLogicalExpr,
      VhpiName,
      VhpiOperator,
      VhpiOthersLiteral,
      VhpiOutPort,
      VhpiParamDecl,
      VhpiParamExpr,
      VhpiParent,
      VhpiPhysLiteral,
      VhpiPrefix,
      VhpiPrimaryUnit,
      VhpiProtectedTypeBody,
      VhpiProtectedTypeDecl,
      VhpiRejectTime,
      VhpiReportExpr,
      VhpiResolFunc,
      VhpiReturnExpr,
      VhpiReturnTypeMark,
      VhpiRhsExpr,
      VhpiRightExpr,
      VhpiRootInst,
      VhpiSelectExpr,
      VhpiSeverityExpr,
      VhpiSimpleName,
      VhpiSubpBody,
      VhpiSubpDecl,
      VhpiSubtype,
      VhpiSuffix,
      VhpiTimeExpr,
      VhpiTimeOutExpr,
      VhpiTool,
      VhpiTypeMark,
      VhpiUnitDecl,
      VhpiUpStack,
      VhpiUpperRegion,
      VhpiValExpr,
      VhpiValSubtype
     );

   --  Methods used to traverse 1 to many relationships.
   type VhpiOneToManyT is
     (
      VhpiAliasDecls,
      VhpiArgvs,
      VhpiAttrDecls,
      VhpiAttrSpecs,
      VhpiBasicSignals,
      VhpiBlockStmts,
      VhpiBranchs,
      VhpiCallbacks,
      VhpiChoices,
      VhpiCompInstStmts,
      VhpiCondExprs,
      VhpiCondWaveforms,
      VhpiConfigItems,
      VhpiConfigSpecs,
      VhpiConstDecls,
      VhpiConstraints,
      VhpiContributors,
      VhpiCurRegions,
      VhpiDecls,
      VhpiDepUnits,
      VhpiDesignUnits,
      VhpiDrivenSigs,
      VhpiDrivers,
      VhpiElemAssocs,
      VhpiEntityClassEntrys,
      VhpiEntityDesignators,
      VhpiEnumLiterals,
      VhpiForeignfs,
      VhpiGenericAssocs,
      VhpiGenericDecls,
      VhpiIndexExprs,
      VhpiIndexedNames,
      VhpiInternalRegions,
      VhpiMembers,
      VhpiPackInsts,
      VhpiParamAssocs,
      VhpiParamDecls,
      VhpiPortAssocs,
      VhpiPortDecls,
      VhpiRecordElems,
      VhpiSelectWaveforms,
      VhpiSelectedNames,
      VhpiSensitivitys,
      VhpiSeqStmts,
      VhpiSigAttrs,
      VhpiSigDecls,
      VhpiSigNames,
      VhpiSignals,
      VhpiSpecNames,
      VhpiSpecs,
      VhpiStmts,
      VhpiTransactions,
      VhpiTypeMarks,
      VhpiUnitDecls,
      VhpiUses,
      VhpiVarDecls,
      VhpiWaveformElems,
      VhpiLibraryDecls
     );

   type VhpiIntPropertyT is
     (
      VhpiAccessP,
      VhpiArgcP,
      VhpiAttrKindP,
      VhpiBaseIndexP,
      VhpiBeginLineNoP,
      VhpiEndLineNoP,
      VhpiEntityClassP,
      VhpiForeignKindP,
      VhpiFrameLevelP,
      VhpiGenerateIndexP,
      VhpiIntValP,
      VhpiIsAnonymousP,
      VhpiIsBasicP,
      VhpiIsCompositeP,
      VhpiIsDefaultP,
      VhpiIsDeferredP,
      VhpiIsDiscreteP,
      VhpiIsForcedP,
      VhpiIsForeignP,
      VhpiIsGuardedP,
      VhpiIsImplicitDeclP,
      VhpiIsInvalidP_DEPRECATED,
      VhpiIsLocalP,
      VhpiIsNamedP,
      VhpiIsNullP,
      VhpiIsOpenP,
      VhpiIsPLIP,
      VhpiIsPassiveP,
      VhpiIsPostponedP,
      VhpiIsProtectedTypeP,
      VhpiIsPureP,
      VhpiIsResolvedP,
      VhpiIsScalarP,
      VhpiIsSeqStmtP,
      VhpiIsSharedP,
      VhpiIsTransportP,
      VhpiIsUnaffectedP,
      VhpiIsUnconstrainedP,
      VhpiIsUninstantiatedP,
      VhpiIsUpP,
      VhpiIsVitalP,
      VhpiIteratorTypeP,
      VhpiKindP,
      VhpiLeftBoundP,
      VhpiLevelP_DEPRECATED,
      VhpiLineNoP,
      VhpiLineOffsetP,
      VhpiLoopIndexP,
      VhpiModeP,
      VhpiNumDimensionsP,
      VhpiNumFieldsP_DEPRECATED,
      VhpiNumGensP,
      VhpiNumLiteralsP,
      VhpiNumMembersP,
      VhpiNumParamsP,
      VhpiNumPortsP,
      VhpiOpenModeP,
      VhpiPhaseP,
      VhpiPositionP,
      VhpiPredefAttrP,
      VhpiReasonP,
      VhpiRightBoundP,
      VhpiSigKindP,
      VhpiSizeP,
      VhpiStartLineNoP,
      VhpiStateP,
      VhpiStaticnessP,
      VhpiVHDLversionP,
      VhpiIdP,
      VhpiCapabilitiesP
     );

   --  String properties.
   type VhpiStrPropertyT is
     (
      VhpiCaseNameP,
      VhpiCompNameP,
      VhpiDefNameP,
      VhpiFileNameP,
      VhpiFullCaseNameP,
      VhpiFullNameP,
      VhpiKindStrP,
      VhpiLabelNameP,
      VhpiLibLogicalNameP,
      VhpiLibPhysicalNameP,
      VhpiLogicalNameP,
      VhpiLoopLabelNameP,
      VhpiNameP,
      VhpiOpNameP,
      VhpiStrValP,
      VhpiToolVersionP,
      VhpiUnitNameP
     );

   --  Possible Errors.
   type AvhpiErrorT is
     (
      AvhpiErrorOk,
      AvhpiErrorBadRel,
      AvhpiErrorHandle,
      AvhpiErrorNotImplemented,
      AvhpiErrorIteratorEnd,
      AvhpiErrorBadIndex
     );

   type VhpiHandleT is private;

   --  A null handle.
   Null_Handle : constant VhpiHandleT;

   --  Get the root instance.
   procedure Get_Root_Inst (Res : out VhpiHandleT);

   --  For vpi: the scope that contains the root instance.
   procedure Get_Root_Scope (Res : out VhpiHandleT);

   --  Get the instanciated packages.
   procedure Get_Package_Inst (Res : out VhpiHandleT);

   procedure Vhpi_Handle (Rel : VhpiOneToOneT;
                          Ref : VhpiHandleT;
                          Res : out VhpiHandleT;
                          Error : out AvhpiErrorT);

   procedure Vhpi_Handle_By_Index (Rel : VhpiOneToManyT;
                                   Ref : VhpiHandleT;
                                   Index : Natural;
                                   Res : out VhpiHandleT;
                                   Error : out AvhpiErrorT);

   procedure Vhpi_Iterator (Rel : VhpiOneToManyT;
                            Ref : VhpiHandleT;
                            Res : out VhpiHandleT;
                            Error : out AvhpiErrorT);
   procedure Vhpi_Scan (Iterator : in out VhpiHandleT;
                        Res : out VhpiHandleT;
                        Error : out AvhpiErrorT);

   procedure Vhpi_Get_Str (Property : VhpiStrPropertyT;
                           Obj : VhpiHandleT;
                           Res : out String;
                           Len : out Natural);

   procedure Vhpi_Get_Str (Property : VhpiStrPropertyT;
                           Obj : VhpiHandleT;
                           Res : out Ghdl_C_String);

   subtype VhpiIntT is Ghdl_I32;

   procedure Vhpi_Get (Property : VhpiIntPropertyT;
                       Obj : VhpiHandleT;
                       Res : out VhpiIntT;
                       Error : out AvhpiErrorT);
   procedure Vhpi_Get (Property : VhpiIntPropertyT;
                       Obj : VhpiHandleT;
                       Res : out Boolean;
                       Error : out AvhpiErrorT);

   --  Almost the same as Vhpi_Get_Str (VhpiName, OBJ), but there is not
   --  indexes for generate stmt.
   function Avhpi_Get_Base_Name (Obj : VhpiHandleT) return Ghdl_C_String;

   --  Return TRUE iff HDL1 and HDL2 are equivalent.
   function Vhpi_Compare_Handles (Hdl1, Hdl2 : VhpiHandleT)
                                 return Boolean;

--    procedure Vhpi_Handle_By_Simple_Name (Ref : VhpiHandleT;
--                                          Res : out VhpiHandleT;
--                                          Error : out AvhpiErrorT);

   type VhpiEntityClassT is
     (
      VhpiErrorEC,
      VhpiEntityEC,
      VhpiArchitectureEC,
      VhpiConfigurationEC,
      VhpiProcedureEC,
      VhpiFunctionEC,
      VhpiPackageEC,
      VhpiTypeEC,
      VhpiSubtypeEC,
      VhpiConstantEC,
      VhpiSignalEC,
      VhpiVariableEC,
      VhpiComponentEC,
      VhpiLabelEC,
      VhpiLiteralEC,
      VhpiUnitsEC,
      VhpiFileEC,
      VhpiGroupEC
     );

   function Vhpi_Get_EntityClass (Obj : VhpiHandleT)
                                 return VhpiEntityClassT;

   type VhpiModeT is
     (
      VhpiErrorMode,
      VhpiInMode,
      VhpiOutMode,
      VhpiInoutMode,
      VhpiBufferMode,
      VhpiLinkageMode
     );
   function Vhpi_Get_Mode (Obj : VhpiHandleT) return VhpiModeT;

   function Avhpi_Get_Rti (Obj : VhpiHandleT) return Ghdl_Rti_Access;

   function Avhpi_Get_Address (Obj : VhpiHandleT) return Address;

   function Avhpi_Get_Context (Obj : VhpiHandleT) return Rti_Context;

   function Vhpi_Get_Kind (Obj : VhpiHandleT) return VhpiClassKindT;

   function Vhpi_Put_Value (Obj : VhpiHandleT; Val : Ghdl_I64)
                           return AvhpiErrorT;
private
   type VhpiHandleT (Kind : VhpiClassKindT := VhpiUndefined) is record
      --  Context.
      Ctxt : Rti_Context;

      case Kind is
         when VhpiIteratorK
           | AvhpiRootScopeIteratorK =>
            Rel : VhpiOneToManyT;
            It_Cur : Ghdl_Index_Type;
            It2 : Ghdl_Index_Type;
            Max2 : Ghdl_Index_Type;
         when AvhpiNameIteratorK
           | VhpiIndexedNameK =>
            N_Addr : Address;
            N_Type : Ghdl_Rti_Access;
            N_Idx : Ghdl_Index_Type;
            N_Obj : Ghdl_Rtin_Object_Acc;
         when VhpiSigDeclK
           | VhpiPortDeclK
           | VhpiGenericDeclK
           | VhpiConstDeclK =>
            Obj : Ghdl_Rtin_Object_Acc;
         when VhpiSubtypeIndicK
           | VhpiSubtypeDeclK
           | VhpiArrayTypeDeclK
           | VhpiEnumTypeDeclK
           | VhpiPhysTypeDeclK
           | VhpiIntTypeDeclK =>
            Atype : Ghdl_Rti_Access;
         when VhpiCompInstStmtK =>
            Inst : Ghdl_Rtin_Instance_Acc;
         when VhpiIntRangeK
           | VhpiEnumRangeK
           | VhpiFloatRangeK
           | VhpiPhysRangeK =>
            Rng_Type : Ghdl_Rti_Access;
            Rng_Addr : Ghdl_Range_Ptr;
         when others =>
            null;
      end case;
      --  Current Object.
      --Obj : Ghdl_Rti_Access;
   end record;

   Null_Handle : constant VhpiHandleT := (Kind => VhpiUndefined,
                                          Ctxt => (Base => Null_Address,
                                                   Block => null));
end Grt.Avhpi;
