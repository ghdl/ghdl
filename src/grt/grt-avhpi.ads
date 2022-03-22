--  GHDL Run Time (GRT) - VHPI implementation for Ada.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
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
      VhpiQualifiedExprK,
      VhpiUseClauseK,
      VhpiConcAssertStmtK,
      VhpiConcProcCallStmtK,
      VhpiForeverLoopK,
      VhpiSeqAssertStmtK,
      VhpiSeqProcCallStmtK,
      VhpiSeqSigAssignStmtK,
      VhpiProtectedTypeInstK,

      --  Iterator, but on a name.
      AvhpiNameIteratorK,

      --  Root scope that contains the top entity.  For vpi.
      AvhpiRootScopeK,
      AvhpiRootScopeIteratorK
     );

   for VhpiClassKindT use
   (
      VhpiUndefined => 1000,
      VhpiAccessTypeDeclK => 1001,
      VhpiAggregateK => 1002,
      VhpiAliasDeclK => 1003,
      VhpiAllLiteralK => 1004,
      VhpiAllocatorK => 1005,
      VhpiAnyCollectionK => 1006,
      VhpiArchBodyK => 1007,
      VhpiArgvK => 1008,
      VhpiArrayTypeDeclK => 1009,
      VhpiAssertStmtK => 1010,
      VhpiAssocElemK => 1011,
      VhpiAttrDeclK => 1012,
      VhpiAttrSpecK => 1013,
      VhpiBinaryExprK => 1014,
      VhpiBitStringLiteralK => 1015,
      VhpiBlockConfigK => 1016,
      VhpiBlockStmtK => 1017,
      VhpiBranchK => 1018,
      VhpiCallbackK => 1019,
      VhpiCaseStmtK => 1020,
      VhpiCharLiteralK => 1021,
      VhpiCompConfigK => 1022,
      VhpiCompDeclK => 1023,
      VhpiCompInstStmtK => 1024,
      VhpiCondSigAssignStmtK => 1025,
      VhpiCondWaveformK => 1026,
      VhpiConfigDeclK => 1027,
      VhpiConstDeclK => 1028,
      VhpiConstParamDeclK => 1029,
      VhpiConvFuncK => 1030,
      VhpiDeRefObjK => 1031,
      VhpiDisconnectSpecK => 1032,
      VhpiDriverK => 1033,
      VhpiDriverCollectionK => 1034,
      VhpiElemAssocK => 1035,
      VhpiElemDeclK => 1036,
      VhpiEntityClassEntryK => 1037,
      VhpiEntityDeclK => 1038,
      VhpiEnumLiteralK => 1039,
      VhpiEnumRangeK => 1040,
      VhpiEnumTypeDeclK => 1041,
      VhpiExitStmtK => 1042,
      VhpiFileDeclK => 1043,
      VhpiFileParamDeclK => 1044,
      VhpiFileTypeDeclK => 1045,
      VhpiFloatRangeK => 1046,
      VhpiFloatTypeDeclK => 1047,
      VhpiForGenerateK => 1048,
      VhpiForLoopK => 1049,
      VhpiForeignfK => 1050,
      VhpiFuncCallK => 1051,
      VhpiFuncDeclK => 1052,
      VhpiGenericDeclK => 1053,
      VhpiGroupDeclK => 1054,
      VhpiGroupTempDeclK => 1055,
      VhpiIfGenerateK => 1056,
      VhpiIfStmtK => 1057,
      VhpiInPortK => 1058,
      VhpiIndexedNameK => 1059,
      VhpiIntLiteralK => 1060,
      VhpiIntRangeK => 1061,
      VhpiIntTypeDeclK => 1062,
      VhpiIteratorK => 1063,
      VhpiLibraryDeclK => 1064,
      VhpiLoopStmtK => 1065,
      VhpiNextStmtK => 1066,
      VhpiNullLiteralK => 1067,
      VhpiNullStmtK => 1068,
      VhpiOperatorK => 1069,
      VhpiOthersLiteralK => 1070,
      VhpiOutPortK => 1071,
      VhpiPackBodyK => 1072,
      VhpiPackDeclK => 1073,
      VhpiPackInstK => 1074,
      VhpiParamAttrNameK => 1075,
      VhpiPhysLiteralK => 1076,
      VhpiPhysRangeK => 1077,
      VhpiPhysTypeDeclK => 1078,
      VhpiPortDeclK => 1079,
      VhpiProcCallStmtK => 1080,
      VhpiProcDeclK => 1081,
      VhpiProcessStmtK => 1082,
      VhpiProtectedTypeK => 1083,
      VhpiProtectedTypeBodyK => 1084,
      VhpiProtectedTypeDeclK => 1085,
      VhpiRealLiteralK => 1086,
      VhpiRecordTypeDeclK => 1087,
      VhpiReportStmtK => 1088,
      VhpiReturnStmtK => 1089,
      VhpiRootInstK => 1090,
      VhpiSelectSigAssignStmtK => 1091,
      VhpiSelectWaveformK => 1092,
      VhpiSelectedNameK => 1093,
      VhpiSigDeclK => 1094,
      VhpiSigParamDeclK => 1095,
      VhpiSimpAttrNameK => 1096,
      VhpiSimpleSigAssignStmtK => 1097,
      VhpiSliceNameK => 1098,
      VhpiStringLiteralK => 1099,
      VhpiSubpBodyK => 1100,
      VhpiSubtypeDeclK => 1101,
      VhpiSubtypeIndicK => 1102,
      VhpiToolK => 1103,
      VhpiTransactionK => 1104,
      VhpiTypeConvK => 1105,
      VhpiUnaryExprK => 1106,
      VhpiUnitDeclK => 1107,
      VhpiUserAttrNameK => 1108,
      VhpiVarAssignStmtK => 1109,
      VhpiVarDeclK => 1110,
      VhpiVarParamDeclK => 1111,
      VhpiWaitStmtK => 1112,
      VhpiWaveformElemK => 1113,
      VhpiWhileLoopK => 1114,
      VhpiQualifiedExprK => 1115,
      VhpiUseClauseK => 1116,
      VhpiConcAssertStmtK => 1117,
      VhpiConcProcCallStmtK => 1118,
      VhpiForeverLoopK => 1119,
      VhpiSeqAssertStmtK => 1120,
      VhpiSeqProcCallStmtK => 1121,
      VhpiSeqSigAssignStmtK => 1122,
      VhpiProtectedTypeInstK => 1123,

      --  Iterator, but on a name.
      AvhpiNameIteratorK => 2001,

      --  Root scope that contains the top entity.  For vpi.
      AvhpiRootScopeK => 2002,
      AvhpiRootScopeIteratorK => 2003
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
      VhpiType,
      VhpiTypeMark,
      VhpiUnitDecl,
      VhpiUpStack,
      VhpiUpperRegion,
      VhpiUse,
      VhpiValExpr,
      VhpiValSubtype,
      VhpiElemType,
      VhpiFirstNamedType,
      VhpiReturnType,
      VhpiValType,
      VhpiCurRegion,
      VhpiSignal,
      VhpiLibraryDecl,
      VhpiSimNet,
      VhpiAliasedName,
      VhpiCompDecl,
      VhpiProtectedTypeInst,
      VhpiGenIndex,

      --  From indexedName to to base name.
      VhpiBaseName
    );

   for VhpiOneToOneT use
    (
      VhpiAbstractLiteral => 1301,
      VhpiActual => 1302,
      VhpiAllLiteral => 1303,
      VhpiAttrDecl => 1304,
      VhpiAttrSpec => 1305,
      VhpiBaseType => 1306,
      VhpiBaseUnit => 1307,
      VhpiBasicSignal => 1308,
      VhpiBlockConfig => 1309,
      VhpiCaseExpr => 1310,
      VhpiCondExpr => 1311,
      VhpiConfigDecl => 1312,
      VhpiConfigSpec => 1313,
      VhpiConstraint => 1314,
      VhpiContributor => 1315,
      VhpiCurCallback => 1316,
      VhpiCurEqProcess => 1317,
      VhpiCurStackFrame => 1318,
      VhpiDeRefObj => 1319,
      VhpiDecl => 1320,
      VhpiDesignUnit => 1321,
      VhpiDownStack => 1322,
      VhpiElemSubtype => 1323,
      VhpiEntityAspect => 1324,
      VhpiEntityDecl => 1325,
      VhpiEqProcessStmt => 1326,
      VhpiExpr => 1327,
      VhpiFormal => 1328,
      VhpiFuncDecl => 1329,
      VhpiGroupTempDecl => 1330,
      VhpiGuardExpr => 1331,
      VhpiGuardSig => 1332,
      VhpiImmRegion => 1333,
      VhpiInPort => 1334,
      VhpiInitExpr => 1335,
      VhpiIterScheme => 1336,
      VhpiLeftExpr => 1337,
      VhpiLexicalScope => 1338,
      VhpiLhsExpr => 1339,
      VhpiLocal => 1340,
      VhpiLogicalExpr => 1341,
      VhpiName => 1342,
      VhpiOperator => 1343,
      VhpiOthersLiteral => 1344,
      VhpiOutPort => 1345,
      VhpiParamDecl => 1346,
      VhpiParamExpr => 1347,
      VhpiParent => 1348,
      VhpiPhysLiteral => 1349,
      VhpiPrefix => 1350,
      VhpiPrimaryUnit => 1351,
      VhpiProtectedTypeBody => 1352,
      VhpiProtectedTypeDecl => 1353,
      VhpiRejectTime => 1354,
      VhpiReportExpr => 1355,
      VhpiResolFunc => 1356,
      VhpiReturnExpr => 1357,
      VhpiReturnTypeMark => 1358,
      VhpiRhsExpr => 1359,
      VhpiRightExpr => 1360,
      VhpiRootInst => 1361,
      VhpiSelectExpr => 1362,
      VhpiSeverityExpr => 1363,
      VhpiSimpleName => 1364,
      VhpiSubpBody => 1365,
      VhpiSubpDecl => 1366,
      VhpiSubtype => 1367,
      VhpiSuffix => 1368,
      VhpiTimeExpr => 1369,
      VhpiTimeOutExpr => 1370,
      VhpiTool => 1371,
      VhpiType => 1372,
      VhpiTypeMark => 1373,
      VhpiUnitDecl => 1374,
      VhpiUpStack => 1375,
      VhpiUpperRegion => 1376,
      VhpiUse => 1377,
      VhpiValExpr => 1378,
      VhpiValSubtype => 1379,
      VhpiElemType => 1380,
      VhpiFirstNamedType => 1381,
      VhpiReturnType => 1382,
      VhpiValType => 1383,
      VhpiCurRegion => 1384,
      VhpiSignal => 1385,
      VhpiLibraryDecl => 1386,
      VhpiSimNet => 1387,
      VhpiAliasedName => 1388,
      VhpiCompDecl => 1389,
      VhpiProtectedTypeInst => 1390,
      VhpiGenIndex => 1391,

      VhpiBaseName => 1490
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
      VhpiLibraryDecls,
      vhpiLocalLoads,
      vhpiOptimizedLoads,
      vhpiTypes,
      vhpiUseClauses,
      vhpiGenerateStmts,
      vhpiLocalContributors,
      vhpiOptimizedContributors,
      vhpiParamExprs,
      vhpiEqProcessStmts,
      vhpiEntityClassEntries,
      vhpiSensitivities
     );

   for VhpiOneToManyT use
     (
      VhpiAliasDecls => 1501,
      VhpiArgvs => 1502,
      VhpiAttrDecls => 1503,
      VhpiAttrSpecs => 1504,
      VhpiBasicSignals => 1505,
      VhpiBlockStmts => 1506,
      VhpiBranchs => 1507,
      VhpiCallbacks => 1508,
      VhpiChoices => 1509,
      VhpiCompInstStmts => 1510,
      VhpiCondExprs => 1511,
      VhpiCondWaveforms => 1512,
      VhpiConfigItems => 1513,
      VhpiConfigSpecs => 1514,
      VhpiConstDecls => 1515,
      VhpiConstraints => 1516,
      VhpiContributors => 1517,
      VhpiCurRegions => 1518,
      VhpiDecls => 1519,
      VhpiDepUnits => 1520,
      VhpiDesignUnits => 1521,
      VhpiDrivenSigs => 1522,
      VhpiDrivers => 1523,
      VhpiElemAssocs => 1524,
      VhpiEntityClassEntrys => 1525,
      VhpiEntityDesignators => 1526,
      VhpiEnumLiterals => 1527,
      VhpiForeignfs => 1528,
      VhpiGenericAssocs => 1529,
      VhpiGenericDecls => 1530,
      VhpiIndexExprs => 1531,
      VhpiIndexedNames => 1532,
      VhpiInternalRegions => 1533,
      VhpiMembers => 1534,
      VhpiPackInsts => 1535,
      VhpiParamAssocs => 1536,
      VhpiParamDecls => 1537,
      VhpiPortAssocs => 1538,
      VhpiPortDecls => 1539,
      VhpiRecordElems => 1540,
      VhpiSelectWaveforms => 1541,
      VhpiSelectedNames => 1542,
      VhpiSensitivitys => 1543,
      VhpiSeqStmts => 1544,
      VhpiSigAttrs => 1545,
      VhpiSigDecls => 1546,
      VhpiSigNames => 1547,
      VhpiSignals => 1548,
      VhpiSpecNames => 1549,
      VhpiSpecs => 1550,
      VhpiStmts => 1551,
      VhpiTransactions => 1552,
      VhpiTypeMarks => 1553,
      VhpiUnitDecls => 1554,
      VhpiUses => 1555,
      VhpiVarDecls => 1556,
      VhpiWaveformElems => 1557,
      VhpiLibraryDecls => 1558,
      vhpiLocalLoads => 1559,
      vhpiOptimizedLoads => 1560,
      vhpiTypes => 1561,
      vhpiUseClauses => 1562,
      vhpiGenerateStmts => 1563,
      vhpiLocalContributors => 1564,
      vhpiOptimizedContributors => 1565,
      vhpiParamExprs => 1566,
      vhpiEqProcessStmts => 1567,
      vhpiEntityClassEntries => 1568,
      vhpiSensitivities => 1569
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
      VhpiCapabilitiesP,
      vhpiAutomaticRestoreP,
      vhpiCompInstKindP,
      vhpiIsBuiltInP,
      vhpiIsDynamicP,
      vhpiIsOperatorP,
      vhpiNumFieldsP
     );

   for VhpiIntPropertyT use
     (
      VhpiAccessP => 1001,
      VhpiArgcP => 1002,
      VhpiAttrKindP => 1003,
      VhpiBaseIndexP => 1004,
      VhpiBeginLineNoP => 1005,
      VhpiEndLineNoP => 1006,
      VhpiEntityClassP => 1007,
      VhpiForeignKindP => 1008,
      VhpiFrameLevelP => 1009,
      VhpiGenerateIndexP => 1010,
      VhpiIntValP => 1011,
      VhpiIsAnonymousP => 1012,
      VhpiIsBasicP => 1013,
      VhpiIsCompositeP => 1014,
      VhpiIsDefaultP => 1015,
      VhpiIsDeferredP => 1016,
      VhpiIsDiscreteP => 1017,
      VhpiIsForcedP => 1018,
      VhpiIsForeignP => 1019,
      VhpiIsGuardedP => 1020,
      VhpiIsImplicitDeclP => 1021,
      VhpiIsInvalidP_DEPRECATED => 1022,
      VhpiIsLocalP => 1023,
      VhpiIsNamedP => 1024,
      VhpiIsNullP => 1025,
      VhpiIsOpenP => 1026,
      VhpiIsPLIP => 1027,
      VhpiIsPassiveP => 1028,
      VhpiIsPostponedP => 1029,
      VhpiIsProtectedTypeP => 1030,
      VhpiIsPureP => 1031,
      VhpiIsResolvedP => 1032,
      VhpiIsScalarP => 1033,
      VhpiIsSeqStmtP => 1034,
      VhpiIsSharedP => 1035,
      VhpiIsTransportP => 1036,
      VhpiIsUnaffectedP => 1037,
      VhpiIsUnconstrainedP => 1038,
      VhpiIsUninstantiatedP => 1039,
      VhpiIsUpP => 1040,
      VhpiIsVitalP => 1041,
      VhpiIteratorTypeP => 1042,
      VhpiKindP => 1043,
      VhpiLeftBoundP => 1044,
      VhpiLevelP_DEPRECATED => 1045,
      VhpiLineNoP => 1046,
      VhpiLineOffsetP => 1047,
      VhpiLoopIndexP => 1048,
      VhpiModeP => 1049,
      VhpiNumDimensionsP => 1050,
      VhpiNumFieldsP_DEPRECATED => 1051,
      VhpiNumGensP => 1052,
      VhpiNumLiteralsP => 1053,
      VhpiNumMembersP => 1054,
      VhpiNumParamsP => 1055,
      VhpiNumPortsP => 1056,
      VhpiOpenModeP => 1057,
      VhpiPhaseP => 1058,
      VhpiPositionP => 1059,
      -- 1061 skipped
      VhpiPredefAttrP => 1060,
      VhpiReasonP => 1062,
      VhpiRightBoundP => 1063,
      VhpiSigKindP => 1064,
      VhpiSizeP => 1065,
      VhpiStartLineNoP => 1066,
      VhpiStateP => 1067,
      VhpiStaticnessP => 1068,
      VhpiVHDLversionP => 1069,
      VhpiIdP => 1070,
      VhpiCapabilitiesP => 1071,
      vhpiAutomaticRestoreP => 1072,
      vhpiCompInstKindP => 1073,
      vhpiIsBuiltInP => 1074,
      vhpiIsDynamicP => 1075,
      vhpiIsOperatorP => 1076,
      vhpiNumFieldsP => 1077
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
      VhpiUnitNameP,
      vhpiSaveRestartLocationP,
      vhpiCompInstNameP,
      vhpiInstNamesP,
      vhpiSignatureNameP,
      vhpiSpecNameP
     );

   for VhpiStrPropertyT use
     (
      VhpiCaseNameP => 1301,
      VhpiCompNameP => 1302,
      VhpiDefNameP => 1303,
      VhpiFileNameP => 1304,
      VhpiFullCaseNameP => 1305,
      VhpiFullNameP => 1306,
      VhpiKindStrP => 1307,
      VhpiLabelNameP => 1308,
      VhpiLibLogicalNameP => 1309,
      VhpiLibPhysicalNameP => 1310,
      VhpiLogicalNameP => 1311,
      VhpiLoopLabelNameP => 1312,
      VhpiNameP => 1313,
      VhpiOpNameP => 1314,
      VhpiStrValP => 1315,
      VhpiToolVersionP => 1316,
      VhpiUnitNameP => 1317,
      vhpiSaveRestartLocationP => 1318,
      vhpiCompInstNameP => 1319,
      vhpiInstNamesP => 1320,
      vhpiSignatureNameP => 1321,
      vhpiSpecNameP => 1322
     );

   --  Possible Errors.
   type AvhpiErrorT is
     (
      AvhpiErrorOk,
      AvhpiErrorBadRel,
      AvhpiErrorHandle,
      AvhpiErrorNotImplemented,
      AvhpiErrorIteratorEnd,
      AvhpiErrorBadIndex,
      AvhpiErrorBadEnumVal
     );

   type VhpiHandleT is private;

   subtype VhpiIntT is Ghdl_I32;

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

   --  Get the sub-object using the index within the range.
   --  The implicit relation is VhpiIndexedNames.
   procedure Vhpi_Handle_By_Array_Index (Ref : VhpiHandleT;
                                         Index : VhpiIntT;
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

   for VhpiEntityClassT use
     (
      VhpiErrorEC => 1000,
      VhpiEntityEC => 1001,
      VhpiArchitectureEC => 1002,
      VhpiConfigurationEC => 1003,
      VhpiProcedureEC => 1004,
      VhpiFunctionEC => 1005,
      VhpiPackageEC => 1006,
      VhpiTypeEC => 1007,
      VhpiSubtypeEC => 1008,
      VhpiConstantEC => 1009,
      VhpiSignalEC => 1010,
      VhpiVariableEC => 1011,
      VhpiComponentEC => 1012,
      VhpiLabelEC => 1013,
      VhpiLiteralEC => 1014,
      VhpiUnitsEC => 1015,
      VhpiFileEC => 1016,
      VhpiGroupEC => 1017
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
