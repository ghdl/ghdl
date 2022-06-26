/* --------------------------------------------------------------------
 *
 * Copyright 2019 IEEE P1076 WG Authors
 *
 * See the LICENSE file distributed with this work for copyright and
 * licensing information and the AUTHORS file.
 *
 * This file to you under the Apache License, Version 2.0 (the "License").
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 *
 *
 *   Title     :  vhpi_user.h
 *             :
 *   Developers:  IEEE P1076 Working Group, VHPI Task Force
 *             :
 *   Purpose   :  This header file describes the procedural interface
 *             :  to access VHDL compiled, instantiated and run-time
 *             :  data.It is derived from the UML model. For conformance
 *             :  with the VHPI standard, a VHPI application or program
 *             :  shall reference this header file.
 *             :
 *   Note      :  The contents of this file may be modified in an
 *             :  implementation to provide implementation-defined
 *             :  functionality, as described in B.3.
 *             :
 * --------------------------------------------------------------------
 * modification history :
 * --------------------------------------------------------------------
 * $Revision: 1315 $
 * $Date: 2008-07-13 10:11:53 +0930 (Sun, 13 Jul 2008) $
 * --------------------------------------------------------------------
 */


#ifndef VHPI_USER_H
#define VHPI_USER_H
#include <stddef.h>
#include <stdarg.h>
/* Ensure that size-critical types are defined on all OS platforms. */
#if defined (_MSC_VER)
typedef unsigned __int64 uint64_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int8 uint8_t;
typedef signed __int64 int64_t;
typedef signed __int32 int32_t;
typedef signed __int8 int8_t;
#elif defined(__MINGW32__)
#include <stdint.h>
#elif defined(__linux) || (defined(__APPLE__) && defined(__MACH__))
#include <inttypes.h>
#else
#include <sys/types.h>
#endif

#ifdef  __cplusplus
extern "C" {
#endif

/*--------------------------------------------------------------------*/
/*--------------------------- Portability Help -----------------------*/
/*--------------------------------------------------------------------*/
/* Use to export a symbol */
#if defined (_MSC_VER)
#ifndef PLI_DLLISPEC
#define PLI_DLLISPEC __declspec(dllimport)
#define VHPI_USER_DEFINED_DLLISPEC 1
#endif
#else
#ifndef PLI_DLLISPEC
#define PLI_DLLISPEC
#endif
#endif

/* Use to import a symbol */
#if defined (_MSC_VER)
#ifndef PLI_DLLESPEC
#define PLI_DLLESPEC __declspec(dllexport)
#define VHPI_USER_DEFINED_DLLESPEC 1
#endif
#else
#ifndef PLI_DLLESPEC
#define PLI_DLLESPEC
#endif
#endif

/* Use to mark a function as external */
#ifndef PLI_EXTERN
#define PLI_EXTERN
#endif

/* Use to mark a variable as external */
#ifndef PLI_VEXTERN
#define PLI_VEXTERN extern
#endif

#ifndef PLI_PROTOTYPES
#define PLI_PROTOTYPES
/* object is defined imported by the application */
#define XXTERN PLI_EXTERN PLI_DLLISPEC
/* object is exported by the application */
#define EETERN PLI_EXTERN PLI_DLLESPEC
#endif

/* basic typedefs */
#ifndef VHPI_TYPES
#define VHPI_TYPES
typedef uint32_t *vhpiHandleT;
typedef uint32_t vhpiEnumT;
typedef uint8_t vhpiSmallEnumT;
typedef int32_t vhpiIntT;
typedef int64_t vhpiLongIntT;
typedef unsigned char vhpiCharT;
typedef double vhpiRealT;
typedef int32_t vhpiSmallPhysT;
typedef struct vhpiPhysS
{
  int32_t high;
  uint32_t low;
} vhpiPhysT;

/********************** time structure ****************************/
typedef struct vhpiTimeS
{
  int32_t high;
  uint32_t low;
} vhpiTimeT;

/********************** value structure **************************/

/* value formats */
typedef enum {
  vhpiBinStrVal        = 1, /* do not move */
  vhpiOctStrVal        = 2, /* do not move */
  vhpiDecStrVal        = 3, /* do not move */
  vhpiHexStrVal        = 4, /* do not move */
  vhpiEnumVal          = 5,
  vhpiIntVal           = 6,
  vhpiLogicVal         = 7,
  vhpiRealVal          = 8,
  vhpiStrVal           = 9,
  vhpiCharVal          = 10,
  vhpiTimeVal          = 11,
  vhpiPhysVal          = 12,
  vhpiObjTypeVal       = 13,
  vhpiPtrVal           = 14,
  vhpiEnumVecVal       = 15,
  vhpiIntVecVal        = 16,
  vhpiLogicVecVal      = 17,
  vhpiRealVecVal       = 18,
  vhpiTimeVecVal       = 19,
  vhpiPhysVecVal       = 20,
  vhpiPtrVecVal        = 21,
  vhpiRawDataVal       = 22,
  vhpiSmallEnumVal     = 23,
  vhpiSmallEnumVecVal  = 24,
  vhpiLongIntVal       = 25,
  vhpiLongIntVecVal    = 26,
  vhpiSmallPhysVal     = 27,
  vhpiSmallPhysVecVal  = 28

#ifdef VHPIEXTEND_VAL_FORMATS
  VHPIEXTEND_VAL_FORMATS
#endif

} vhpiFormatT;

/* value structure */
typedef struct vhpiValueS
{
  vhpiFormatT format;  /* vhpi[Char,[Bin,Oct,Dec,Hex]Str,
                               [Small]Enum,Logic,Int,Real,
                               [Small]Phys,Time,Ptr,
                               [Small]EnumVec,LogicVec,IntVect,RealVec,
                               [Small]PhysVec,TimeVec,
                               PtrVec,ObjType,RawData]Val */
  size_t bufSize;  /* the size in bytes of the value buffer;
                      this is set by the user */
  int32_t numElems;
  /* different meanings depending on the format:
     vhpiStrVal, vhpi{Bin...}StrVal: size of string
     array type values: number of array elements
     scalar type values: undefined
  */

  vhpiPhysT unit;
  union
    {
      vhpiEnumT enumv, *enumvs;
      vhpiSmallEnumT smallenumv, *smallenumvs;
      vhpiIntT  intg, *intgs;
      vhpiLongIntT  longintg, *longintgs;
      vhpiRealT real, *reals;
      vhpiSmallPhysT smallphys, *smallphyss;
      vhpiPhysT phys, *physs;
      vhpiTimeT time, *times;
      vhpiCharT ch, *str;
      void *ptr, **ptrs;
    } value;
} vhpiValueT;

#endif

/* Following are the constant definitions. They are divided into
   three major areas:

 1) object types

 2) access methods

 3) properties

*/
#define vhpiUndefined -1

/*************** OBJECT KINDS *******************/
typedef enum {
        vhpiAccessTypeDeclK = 1001,
        vhpiAggregateK = 1002,
        vhpiAliasDeclK = 1003,
        vhpiAllK = 1004,
        vhpiAllocatorK = 1005,
        vhpiAnyCollectionK = 1006,
        vhpiArchBodyK = 1007,
        vhpiArgvK = 1008,
        vhpiArrayTypeDeclK = 1009,
        DEPRECATED_vhpiAssertStmtK = 1010,
        vhpiAssocElemK = 1011,
        vhpiAttrDeclK = 1012,
        vhpiAttrSpecK = 1013,
        DEPRECATED_vhpiBinaryExprK = 1014,
        vhpiBitStringLiteralK = 1015,
        vhpiBlockConfigK = 1016,
        vhpiBlockStmtK = 1017,
        vhpiBranchK = 1018,
        vhpiCallbackK = 1019,
        vhpiCaseStmtK = 1020,
        vhpiCharLiteralK = 1021,
        vhpiCompConfigK = 1022,
        vhpiCompDeclK = 1023,
        vhpiCompInstStmtK = 1024,
        vhpiCondSigAssignStmtK = 1025,
        vhpiCondWaveformK = 1026,
        vhpiConfigDeclK = 1027,
        vhpiConstDeclK = 1028,
        vhpiConstParamDeclK = 1029,
        vhpiConvFuncK = 1030,
        vhpiDerefObjK = 1031,
        vhpiDisconnectSpecK = 1032,
        vhpiDriverK = 1033,
        vhpiDriverCollectionK = 1034,
        vhpiElemAssocK = 1035,
        vhpiElemDeclK = 1036,
        vhpiEntityClassEntryK = 1037,
        vhpiEntityDeclK = 1038,
        vhpiEnumLiteralK = 1039,
        vhpiEnumRangeK = 1040,
        vhpiEnumTypeDeclK = 1041,
        vhpiExitStmtK = 1042,
        vhpiFileDeclK = 1043,
        vhpiFileParamDeclK = 1044,
        vhpiFileTypeDeclK = 1045,
        vhpiFloatRangeK = 1046,
        vhpiFloatTypeDeclK = 1047,
        vhpiForGenerateK = 1048,
        vhpiForLoopK = 1049,
        vhpiForeignfK = 1050,
        vhpiFuncCallK = 1051,
        vhpiFuncDeclK = 1052,
        vhpiGenericDeclK = 1053,
        vhpiGroupDeclK = 1054,
        vhpiGroupTempDeclK = 1055,
        vhpiIfGenerateK = 1056,
        vhpiIfStmtK = 1057,
        vhpiInPortK = 1058,
        vhpiIndexedNameK = 1059,
        vhpiIntLiteralK = 1060,
        vhpiIntRangeK = 1061,
        vhpiIntTypeDeclK = 1062,
        vhpiIteratorK = 1063,
        vhpiLibraryDeclK = 1064,
        DEPRECATED_vhpiLoopStmtK = 1065,
        vhpiNextStmtK = 1066,
        vhpiNullLiteralK = 1067,
        vhpiNullStmtK = 1068,
        DEPRECATED_vhpiOperatorK = 1069,
        vhpiOthersK = 1070,
        vhpiOutPortK = 1071,
        vhpiPackBodyK = 1072,
        vhpiPackDeclK = 1073,
        vhpiPackInstK = 1074,
        vhpiParamAttrNameK = 1075,
        vhpiPhysLiteralK = 1076,
        vhpiPhysRangeK = 1077,
        vhpiPhysTypeDeclK = 1078,
        vhpiPortDeclK = 1079,
        DEPRECATED_vhpiProcCallStmtK = 1080,
        vhpiProcDeclK = 1081,
        vhpiProcessStmtK = 1082,
        DEPRECATED_vhpiProtectedTypeK = 1083,
        vhpiProtectedTypeBodyK = 1084,
        vhpiProtectedTypeDeclK = 1085,
        vhpiRealLiteralK = 1086,
        vhpiRecordTypeDeclK = 1087,
        vhpiReportStmtK = 1088,
        vhpiReturnStmtK = 1089,
        vhpiRootInstK = 1090,
        vhpiSelectSigAssignStmtK = 1091,
        vhpiSelectWaveformK = 1092,
        vhpiSelectedNameK = 1093,
        vhpiSigDeclK = 1094,
        vhpiSigParamDeclK = 1095,
        vhpiSimpAttrNameK = 1096,
        vhpiSimpleSigAssignStmtK = 1097,
        vhpiSliceNameK = 1098,
        vhpiStringLiteralK = 1099,
        vhpiSubpBodyK = 1100,
        vhpiSubtypeDeclK = 1101,
        DEPRECATED_vhpiSubtypeIndicK = 1102,
        vhpiToolK = 1103,
        vhpiTransactionK = 1104,
        vhpiTypeConvK = 1105,
        DEPRECATED_vhpiUnaryExprK = 1106,
        vhpiUnitDeclK = 1107,
        vhpiUserAttrNameK = 1108,
        vhpiVarAssignStmtK = 1109,
        vhpiVarDeclK = 1110,
        vhpiVarParamDeclK = 1111,
        vhpiWaitStmtK = 1112,
        vhpiWaveformElemK = 1113,
        vhpiWhileLoopK = 1114,
        vhpiQualifiedExprK = 1115,
        vhpiUseClauseK = 1116,
        vhpiConcAssertStmtK = 1117,
        vhpiConcProcCallStmtK = 1118,
        vhpiForeverLoopK = 1119,
        vhpiSeqAssertStmtK = 1120,
        vhpiSeqProcCallStmtK = 1121,
        vhpiSeqSigAssignStmtK = 1122,
        vhpiProtectedTypeInstK = 1123
#ifdef VHPIEXTEND_CLASSES
        VHPIEXTEND_CLASSES
#endif
        } vhpiClassKindT;

/********* methods used to traverse 1 to 1 relationships **************/
typedef enum {
        vhpiAbstractLiteral = 1301,
        vhpiActual = 1302,
        vhpiAll = 1303,
        vhpiAttrDecl = 1304,
        vhpiAttrSpec = 1305,
        vhpiBaseType = 1306,
        vhpiBaseUnit = 1307,
        DEPRECATED_vhpiBasicSignal = 1308,
        vhpiBlockConfig = 1309,
        vhpiCaseExpr = 1310,
        vhpiCondExpr = 1311,
        vhpiConfigDecl = 1312,
        vhpiConfigSpec = 1313,
        vhpiConstraint = 1314,
        vhpiContributor = 1315,
        vhpiCurCallback = 1316,
        DEPRECATED_vhpiCurEqProcess = 1317,
        vhpiCurStackFrame = 1318,
        vhpiDerefObj = 1319,
        DEPRECATED_vhpiDecl = 1320,
        vhpiDesignUnit = 1321,
        vhpiDownStack = 1322,
        DEPRECATED_vhpiElemSubtype = 1323,
        vhpiEntityAspect = 1324,
        vhpiEntityDecl = 1325,
        vhpiEqProcessStmt = 1326,
        vhpiExpr = 1327,
        vhpiFormal = 1328,
        vhpiFuncDecl = 1329,
        vhpiGroupTempDecl = 1330,
        vhpiGuardExpr = 1331,
        vhpiGuardSig = 1332,
        vhpiImmRegion = 1333,
        vhpiInPort = 1334,
        vhpiInitExpr = 1335,
        DEPRECATED_vhpiIterScheme = 1336,
        vhpiLeftExpr = 1337,
        vhpiLexicalScope = 1338,
        vhpiLhsExpr = 1339,
        vhpiLocal = 1340,
        vhpiLogicalExpr = 1341,
        DEPRECATED_vhpiName = 1342,
        DEPRECATED_vhpiOperator = 1343,
        vhpiOthers = 1344,
        vhpiOutPort = 1345,
        vhpiParamDecl = 1346,
        DEPRECATED_vhpiParamExpr = 1347,
        vhpiParent = 1348,
        vhpiPhysLiteral = 1349,
        vhpiPrefix = 1350,
        vhpiPrimaryUnit = 1351,
        vhpiProtectedTypeBody = 1352,
        vhpiProtectedTypeDecl = 1353,
        vhpiRejectTime = 1354,
        vhpiReportExpr = 1355,
        vhpiResolFunc = 1356,
        vhpiReturnExpr = 1357,
        DEPRECATED_vhpiReturnTypeMark = 1358,
        vhpiRhsExpr = 1359,
        vhpiRightExpr = 1360,
        vhpiRootInst = 1361,
        vhpiSelectExpr = 1362,
        vhpiSeverityExpr = 1363,
        vhpiSimpleName = 1364,
        vhpiSubpBody = 1365,
        vhpiSubpDecl = 1366,
        DEPRECATED_vhpiSubtype = 1367,
        vhpiSuffix = 1368,
        vhpiTimeExpr = 1369,
        vhpiTimeOutExpr = 1370,
        vhpiTool = 1371,
        vhpiType = 1372,
        DEPRECATED_vhpiTypeMark = 1373,
        vhpiUnitDecl = 1374,
        vhpiUpStack = 1375,
        vhpiUpperRegion = 1376,
        vhpiUse = 1377,
        vhpiValExpr = 1378,
        DEPRECATED_vhpiValSubtype = 1379,
        vhpiElemType = 1380,
        vhpiFirstNamedType = 1381,
        vhpiReturnType = 1382,
        vhpiValType = 1383,
        vhpiCurRegion = 1384,
        vhpiSignal = 1385,
        vhpiLibraryDecl = 1386,
        vhpiSimNet = 1387,
        vhpiAliasedName = 1388,
        vhpiCompDecl = 1389,
        vhpiProtectedTypeInst = 1390,
        vhpiGenIndex = 1391

#ifdef VHPIEXTEND_ONE_METHODS
        VHPIEXTEND_ONE_METHODS

#endif

} vhpiOneToOneT;

/******* methods used to traverse 1 to many relationships ************/
typedef enum {
        vhpiAliasDecls = 1501,
        vhpiArgvs = 1502,
        vhpiAttrDecls = 1503,
        vhpiAttrSpecs = 1504,
        vhpiBasicSignals = 1505,
        vhpiBlockStmts = 1506,
        vhpiBranchs = 1507,
        /* 1508 */
        vhpiChoices = 1509,
        vhpiCompInstStmts = 1510,
        DEPRECATED_vhpiCondExprs = 1511,
        vhpiCondWaveforms = 1512,
        vhpiConfigItems = 1513,
        vhpiConfigSpecs = 1514,
        vhpiConstDecls = 1515,
        vhpiConstraints = 1516,
        DEPRECATED_vhpiContributors = 1517,
        /* 1518 */
        vhpiDecls = 1519,
        vhpiDepUnits = 1520,
        vhpiDesignUnits = 1521,
        vhpiDrivenSigs = 1522,
        vhpiDrivers = 1523,
        vhpiElemAssocs = 1524,
        DEPRECATED_vhpiEntityClassEntrys = 1525,
        vhpiEntityDesignators = 1526,
        vhpiEnumLiterals = 1527,
        vhpiForeignfs = 1528,
        vhpiGenericAssocs = 1529,
        vhpiGenericDecls = 1530,
        vhpiIndexExprs = 1531,
        vhpiIndexedNames = 1532,
        vhpiInternalRegions = 1533,
        vhpiMembers = 1534,
        vhpiPackInsts = 1535,
        vhpiParamAssocs = 1536,
        vhpiParamDecls = 1537,
        vhpiPortAssocs = 1538,
        vhpiPortDecls = 1539,
        vhpiRecordElems = 1540,
        vhpiSelectWaveforms = 1541,
        vhpiSelectedNames = 1542,
        DEPRECATED_vhpiSensitivitys = 1543,
        vhpiSeqStmts = 1544,
        vhpiSigAttrs = 1545,
        vhpiSigDecls = 1546,
        vhpiSigNames = 1547,
        vhpiSignals = 1548,
        DEPRECATED_vhpiSpecNames = 1549,
        vhpiSpecs = 1550,
        vhpiStmts = 1551,
        vhpiTransactions = 1552,
        DEPRECATED_vhpiTypeMarks = 1553,
        vhpiUnitDecls = 1554,
        vhpiUses = 1555,
        vhpiVarDecls = 1556,
        vhpiWaveformElems = 1557,
        vhpiLibraryDecls = 1558,
        vhpiLocalLoads = 1559,
        vhpiOptimizedLoads = 1560,
        vhpiTypes = 1561,
        vhpiUseClauses = 1562,
        vhpiGenerateStmts = 1563,
        vhpiLocalContributors = 1564,
        vhpiOptimizedContributors = 1565,
        vhpiParamExprs = 1566,
        vhpiEqProcessStmts = 1567,
        vhpiEntityClassEntries = 1568,
        vhpiSensitivities = 1569

#ifdef VHPIEXTEND_MANY_METHODS
        VHPIEXTEND_MANY_METHODS
#endif

} vhpiOneToManyT;

/* Note: The following macro is defined for compatibility with
   prototype implementations that use the incorrectly spelled
   enumeration value. The macro is deprecated and will be removed
   in a future revision of the standard.
*/
#define vhpiSensitivitys DEPRECATED_vhpiSensitivitys


/****************** PROPERTIES *******************/
/******* INTEGER or BOOLEAN PROPERTIES **********/
typedef enum {
        vhpiAccessP = 1001,
        vhpiArgcP = 1002,
        vhpiAttrKindP = 1003,
        vhpiBaseIndexP = 1004,
        vhpiBeginLineNoP = 1005,
        vhpiEndLineNoP = 1006,
        vhpiEntityClassP = 1007,
        vhpiForeignKindP = 1008,
        vhpiFrameLevelP = 1009,
        vhpiGenerateIndexP = 1010,
        vhpiIntValP = 1011,
        vhpiIsAnonymousP = 1012,
        vhpiIsBasicP = 1013,
        vhpiIsCompositeP = 1014,
        vhpiIsDefaultP = 1015,
        vhpiIsDeferredP = 1016,
        vhpiIsDiscreteP = 1017,
        vhpiIsForcedP = 1018,
        vhpiIsForeignP = 1019,
        vhpiIsGuardedP = 1020,
        vhpiIsImplicitDeclP = 1021,
        DEPRECATED_vhpiIsInvalidP = 1022,
        vhpiIsLocalP = 1023,
        vhpiIsNamedP = 1024,
        vhpiIsNullP = 1025,
        vhpiIsOpenP = 1026,
        vhpiIsPLIP = 1027,
        vhpiIsPassiveP = 1028,
        vhpiIsPostponedP = 1029,
        vhpiIsProtectedTypeP = 1030,
        vhpiIsPureP = 1031,
        vhpiIsResolvedP = 1032,
        vhpiIsScalarP = 1033,
        vhpiIsSeqStmtP = 1034,
        vhpiIsSharedP = 1035,
        vhpiIsTransportP = 1036,
        vhpiIsUnaffectedP = 1037,
        vhpiIsUnconstrainedP = 1038,
        vhpiIsUninstantiatedP = 1039,
        vhpiIsUpP = 1040,
        vhpiIsVitalP = 1041,
        vhpiIteratorTypeP = 1042,
        vhpiKindP = 1043,
        vhpiLeftBoundP = 1044,
        DEPRECATED_vhpiLevelP = 1045,
        vhpiLineNoP = 1046,
        vhpiLineOffsetP = 1047,
        vhpiLoopIndexP = 1048,
        vhpiModeP = 1049,
        vhpiNumDimensionsP = 1050,
        DEPRECATED_vhpiNumFieldsP = 1051,
        vhpiNumGensP = 1052,
        vhpiNumLiteralsP = 1053,
        vhpiNumMembersP = 1054,
        vhpiNumParamsP = 1055,
        vhpiNumPortsP = 1056,
        vhpiOpenModeP = 1057,
        vhpiPhaseP = 1058,
        vhpiPositionP = 1059,
        vhpiPredefAttrP = 1060,
        /* 1061 */
        vhpiReasonP = 1062,
        vhpiRightBoundP = 1063,
        vhpiSigKindP = 1064,
        vhpiSizeP = 1065,
        vhpiStartLineNoP = 1066,
        vhpiStateP = 1067,
        vhpiStaticnessP = 1068,
        vhpiVHDLversionP = 1069,
        vhpiIdP = 1070,
        vhpiCapabilitiesP = 1071,
        vhpiAutomaticRestoreP = 1072,
        vhpiCompInstKindP = 1073,
        vhpiIsBuiltInP = 1074,
        vhpiIsDynamicP = 1075,
        vhpiIsOperatorP = 1076,
        vhpiNumFieldsP = 1077

#ifdef VHPIEXTEND_INT_PROPERTIES
        VHPIEXTEND_INT_PROPERTIES

#endif

} vhpiIntPropertyT;

/******* STRING PROPERTIES **********/
typedef enum {
        vhpiCaseNameP = 1301,
        vhpiCompNameP = 1302,
        vhpiDefNameP = 1303,
        vhpiFileNameP = 1304,
        vhpiFullCaseNameP = 1305,
        vhpiFullNameP = 1306,
        vhpiKindStrP = 1307,
        vhpiLabelNameP = 1308,
        vhpiLibLogicalNameP = 1309,
        vhpiLibPhysicalNameP = 1310,
        vhpiLogicalNameP = 1311,
        vhpiLoopLabelNameP = 1312,
        vhpiNameP = 1313,
        DEPRECATED_vhpiOpNameP = 1314,
        vhpiStrValP = 1315,
        vhpiToolVersionP = 1316,
        vhpiUnitNameP = 1317,
        vhpiSaveRestartLocationP = 1318,
        vhpiCompInstNameP = 1319,
        vhpiInstNamesP = 1320,
        vhpiSignatureNameP = 1321,
        vhpiSpecNameP = 1322

#ifdef VHPIEXTEND_STR_PROPERTIES
        VHPIEXTEND_STR_PROPERTIES

#endif
} vhpiStrPropertyT;

/******* REAL PROPERTIES **********/
typedef enum {
        vhpiFloatLeftBoundP = 1601,
        vhpiFloatRightBoundP = 1602,
        vhpiRealValP = 1603

#ifdef VHPIEXTEND_REAL_PROPERTIES
        VHPIEXTEND_REAL_PROPERTIES
#endif

} vhpiRealPropertyT;

/******* PHYSICAL PROPERTIES **********/
typedef enum {
        vhpiPhysLeftBoundP = 1651,
        vhpiPhysPositionP = 1652,
        vhpiPhysRightBoundP = 1653,
        vhpiPhysValP = 1654,
        DEPRECATED_vhpiPrecisionP = 1655,
        DEPRECATED_vhpiSimTimeUnitP = 1656,
        vhpiResolutionLimitP = 1657,
        vhpiTimeP = 1658

#ifdef VHPIEXTEND_PHYS_PROPERTIES
        VHPIEXTEND_PHYS_PROPERTIES
#endif

} vhpiPhysPropertyT;

/******************* PROPERTY VALUES ************************/

/* vhpiCapabilitiesP */
typedef enum {
    vhpiProvidesHierarchy             = 1,
    vhpiProvidesStaticAccess          = 2,
    vhpiProvidesConnectivity          = 4,
    vhpiProvidesPostAnalysis          = 8,
    vhpiProvidesForeignModel          = 16,
    vhpiProvidesAdvancedForeignModel  = 32,
    vhpiProvidesSaveRestart           = 64,
    vhpiProvidesReset                 = 128,
    vhpiProvidesDebugRuntime          = 256,
    vhpiProvidesAdvancedDebugRuntime  = 512,
    vhpiProvidesDynamicElab           = 1024

} vhpiCapabibilityT;


/* vhpiOpenModeP */
typedef enum {
        vhpiInOpen         =       1001,
        vhpiOutOpen        =       1002,
        vhpiReadOpen       =       1003,
        vhpiWriteOpen      =       1004,
        vhpiAppendOpen     =       1005
} vhpiOpenModeT;

/* vhpiModeP */
typedef enum {
        vhpiInMode         =       1001,
        vhpiOutMode        =       1002,
        vhpiInoutMode      =       1003,
        vhpiBufferMode     =       1004,
        vhpiLinkageMode    =       1005
} vhpiModeT;

/* vhpiSigKindP */
typedef enum {
        vhpiRegister       =       1001,
        vhpiBus            =       1002,
        vhpiNormal         =       1003
} vhpiSigKindT;

/* vhpiStaticnessP */
typedef enum {
        vhpiLocallyStatic  =       1001,
        vhpiGloballyStatic =       1002,
        vhpiDynamic        =       1003
} vhpiStaticnessT;

/* vhpiPredefAttrP */
typedef enum {
        vhpiActivePA       =       1001,
        vhpiAscendingPA    =       1002,
        vhpiBasePA         =       1003,
        vhpiDelayedPA      =       1004,
        vhpiDrivingPA      =       1005,
        vhpiDriving_valuePA=       1006,
        vhpiEventPA        =       1007,
        vhpiHighPA         =       1008,
        vhpiImagePA        =       1009,
        vhpiInstance_namePA=       1010,
        vhpiLast_activePA  =       1011,
        vhpiLast_eventPA   =       1012,
        vhpiLast_valuePA   =       1013,
        vhpiLeftPA         =       1014,
        vhpiLeftofPA       =       1015,
        vhpiLengthPA       =       1016,
        vhpiLowPA          =       1017,
        vhpiPath_namePA    =       1018,
        vhpiPosPA          =       1019,
        vhpiPredPA         =       1020,
        vhpiQuietPA        =       1021,
        vhpiRangePA        =       1022,
        vhpiReverse_rangePA=       1023,
        vhpiRightPA        =       1024,
        vhpiRightofPA      =       1025,
        vhpiSimple_namePA  =       1026,
        vhpiStablePA       =       1027,
        vhpiSuccPA         =       1028,
        vhpiTransactionPA  =       1029,
        vhpiValPA          =       1030,
        vhpiValuePA        =       1031
} vhpiPredefAttrT;

/* vhpiAttrKindP */
typedef enum {
  vhpiFunctionAK    = 1,
  vhpiRangeAK       = 2,
  vhpiSignalAK      = 3,
  vhpiTypeAK        = 4,
  vhpiValueAK       = 5
#ifdef VHPIEXTEND_ATTR
  VHPIEXTEND_ATTR
#endif
} vhpiAttrKindT;

/* vhpiEntityClassP */
typedef enum {
        vhpiEntityEC         =     1001,
        vhpiArchitectureEC   =     1002,
        vhpiConfigurationEC  =     1003,
        vhpiProcedureEC      =     1004,
        vhpiFunctionEC       =     1005,
        vhpiPackageEC        =     1006,
        vhpiTypeEC           =     1007,
        vhpiSubtypeEC        =     1008,
        vhpiConstantEC       =     1009,
        vhpiSignalEC         =     1010,
        vhpiVariableEC       =     1011,
        vhpiComponentEC      =     1012,
        vhpiLabelEC          =     1013,
        vhpiLiteralEC        =     1014,
        vhpiUnitsEC          =     1015,
        vhpiFileEC           =     1016,
        vhpiGroupEC          =     1017
} vhpiEntityClassT;

/* vhpiAccessP */
typedef enum {
        vhpiRead             =     1,
        vhpiWrite            =     2,
        vhpiConnectivity     =     4,
        vhpiNoAccess         =     8
} vhpiAccessT;

/* value for vhpiStateP property for callbacks */
typedef enum {
        vhpiEnable,
        vhpiDisable,
        vhpiMature /* callback has occurred */
} vhpiStateT;
/* enumeration type for vhpiCompInstKindP property */
typedef enum {
        vhpiDirect,
        vhpiComp,
        vhpiConfig
} vhpiCompInstKindT;


/* the following values are used only for the
   vhpiResolutionLimitP property and for setting the unit field
   of the value structure; they represent the physical position
   of a given VHDL time unit */
/* time unit physical position values {high, low} */
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiFS;
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiPS;
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiNS;
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiUS;
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiMS;
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiS;
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiMN;
PLI_VEXTERN PLI_DLLISPEC const vhpiPhysT  vhpiHR;

/* IEEE std_logic values */
#define vhpiU                  0   /* uninitialized */
#define vhpiX                  1   /* unknown */
#define vhpi0                  2   /* forcing 0 */
#define vhpi1                  3   /* forcing 1 */
#define vhpiZ                  4   /* high impedance */
#define vhpiW                  5   /* weak unknown */
#define vhpiL                  6   /* weak 0 */
#define vhpiH                  7   /* weak 1 */
#define vhpiDontCare           8   /* don't care */

/* IEEE std bit values */
#define vhpibit0               0   /* bit 0 */
#define vhpibit1               1   /* bit 1 */

/* IEEE std boolean values */
#define vhpiFalse              0   /* false */
#define vhpiTrue               1   /* true */

/************** vhpiPhaseP property values *************/
typedef enum {
       vhpiRegistrationPhase   = 1,
       vhpiAnalysisPhase       = 2,
       vhpiElaborationPhase    = 3,
       vhpiInitializationPhase = 4,
       vhpiSimulationPhase     = 5,
       vhpiTerminationPhase    = 6,
       vhpiSavePhase           = 7,
       vhpiRestartPhase        = 8,
       vhpiResetPhase          = 9
} vhpiPhaseT ;

/**************** PLI error information structure ****************/

typedef enum {
        vhpiNote        = 1,
        vhpiWarning     = 2,
        vhpiError       = 3,
        vhpiFailure     = 6,
        vhpiSystem      = 4,
        vhpiInternal    = 5
} vhpiSeverityT;

typedef struct vhpiErrorInfoS
{
  vhpiSeverityT    severity;
  char       *message;
  char       *str;
  char       *file; /* Name of the VHDL file where the VHPI error
                            originated */
  int32_t     line; /* Line number in the VHDL file */
} vhpiErrorInfoT;

/********************* callback structures ************************/
/* callback user data structure */

typedef struct vhpiCbDataS
{
  int32_t reason;            /* callback reason */
  void (*cb_rtn) (const struct vhpiCbDataS *);  /* call routine */
  vhpiHandleT obj;             /* trigger object */
  vhpiTimeT *time;             /* callback time */
  vhpiValueT *value;           /* trigger object value */
  void *user_data;             /* pointer to user data to be passed
                                  to the callback function */
} vhpiCbDataT;

/************************* CALLBACK REASONS *************************/
/*********************** Simulation object related ******************/
/* These are repetitive callbacks */
#define vhpiCbValueChange          1001
#define vhpiCbForce                1002
#define vhpiCbRelease              1003
#define vhpiCbTransaction          1004 /* optional callback reason */

/************************** Statement related ***********************/
/* These are repetitive callbacks */
#define vhpiCbStmt                 1005
#define vhpiCbResume               1006
#define vhpiCbSuspend              1007
#define vhpiCbStartOfSubpCall      1008
#define vhpiCbEndOfSubpCall        1009

/************************** Time related ****************************/
/* the Rep callback reasons are the repeated versions
   of the callbacks */

#define vhpiCbAfterDelay           1010
#define vhpiCbRepAfterDelay        1011

/*********************** Simulation cycle phase related *************/
#define vhpiCbNextTimeStep         1012
#define vhpiCbRepNextTimeStep      1013
#define vhpiCbStartOfNextCycle     1014
#define vhpiCbRepStartOfNextCycle  1015
#define vhpiCbStartOfProcesses     1016
#define vhpiCbRepStartOfProcesses  1017
#define vhpiCbEndOfProcesses       1018
#define vhpiCbRepEndOfProcesses    1019
#define vhpiCbLastKnownDeltaCycle  1020
#define vhpiCbRepLastKnownDeltaCycle 1021
#define vhpiCbStartOfPostponed     1022
#define vhpiCbRepStartOfPostponed  1023
#define vhpiCbEndOfTimeStep        1024
#define vhpiCbRepEndOfTimeStep     1025

/************************** Action related **************************/
/* these are one time callback unless otherwise noted */
#define vhpiCbStartOfTool          1026
#define vhpiCbEndOfTool            1027
#define vhpiCbStartOfAnalysis      1028
#define vhpiCbEndOfAnalysis        1029
#define vhpiCbStartOfElaboration   1030
#define vhpiCbEndOfElaboration     1031
#define vhpiCbStartOfInitialization 1032
#define vhpiCbEndOfInitialization  1033
#define vhpiCbStartOfSimulation    1034
#define vhpiCbEndOfSimulation      1035
#define vhpiCbQuiescense           1036 /* repetitive */
#define vhpiCbPLIError             1037 /* repetitive */
#define vhpiCbStartOfSave          1038
#define vhpiCbEndOfSave            1039
#define vhpiCbStartOfRestart       1040
#define vhpiCbEndOfRestart         1041
#define vhpiCbStartOfReset         1042
#define vhpiCbEndOfReset           1043
#define vhpiCbEnterInteractive     1044 /* repetitive */
#define vhpiCbExitInteractive      1045 /* repetitive */
#define vhpiCbSigInterrupt         1046 /* repetitive */

/* Foreign model callbacks */
#define vhpiCbTimeOut              1047 /* non repetitive */
#define vhpiCbRepTimeOut           1048 /* repetitive */
#define vhpiCbSensitivity          1049 /* repetitive */

/************************* CALLBACK FLAGS ***************************/
#define vhpiReturnCb  0x00000001
#define vhpiDisableCb 0x00000010

/************** vhpiAutomaticRestoreP property values *************/
typedef enum {
       vhpiRestoreAll       = 1,
       vhpiRestoreUserData  = 2,
       vhpiRestoreHandles   = 4,
       vhpiRestoreCallbacks = 8
} vhpiAutomaticRestoreT ;


/******************** FUNCTION DECLARATIONS *********************/

XXTERN int vhpi_assert (vhpiSeverityT severity,
                        char *formatmsg,
                        ...);

/* callback related */

XXTERN vhpiHandleT vhpi_register_cb (vhpiCbDataT *cb_data_p,
                                     int32_t flags);

XXTERN int vhpi_remove_cb (vhpiHandleT cb_obj);

XXTERN int vhpi_disable_cb (vhpiHandleT cb_obj);

XXTERN int vhpi_enable_cb (vhpiHandleT cb_obj);

XXTERN int vhpi_get_cb_info (vhpiHandleT object,
                             vhpiCbDataT *cb_data_p);

/* utilities for sensitivity-set bitmaps */
/* The replacement text for these macros is implementation defined */
/* The behavior is specified in G.1 */
#define VHPI_SENS_ZERO(sens)        vhpi_sens_zero(sens)
#define VHPI_SENS_SET(obj, sens)    vhpi_sens_set(obj, sens)
#define VHPI_SENS_CLR(obj, sens)    vhpi_sens_clr(obj, sens)
#define VHPI_SENS_ISSET(obj, sens)  vhpi_sens_isset(obj, sens)
#define VHPI_SENS_FIRST(sens)       vhpi_sens_first(sens)

/* for obtaining handles */

XXTERN vhpiHandleT vhpi_handle_by_name (const char *name,
                                        vhpiHandleT scope);

XXTERN vhpiHandleT vhpi_handle_by_index (vhpiOneToManyT itRel,
                                         vhpiHandleT parent,
                                         int32_t indx);

/* for traversing relationships */

XXTERN vhpiHandleT vhpi_handle (vhpiOneToOneT type,
                                vhpiHandleT referenceHandle);

XXTERN vhpiHandleT vhpi_iterator (vhpiOneToManyT type,
                                  vhpiHandleT referenceHandle);

XXTERN vhpiHandleT vhpi_scan (vhpiHandleT iterator);

/* for processsing properties */

XXTERN vhpiIntT vhpi_get (vhpiIntPropertyT property,
                          vhpiHandleT object);

XXTERN const vhpiCharT * vhpi_get_str (vhpiStrPropertyT property,
                                       vhpiHandleT object);

XXTERN vhpiRealT vhpi_get_real (vhpiRealPropertyT property,
                                vhpiHandleT object);

XXTERN vhpiPhysT vhpi_get_phys (vhpiPhysPropertyT property,
                                vhpiHandleT object);

/* for access to protected types */

typedef int (*vhpiUserFctT)();

XXTERN int vhpi_protected_call (vhpiHandleT varHdl,
                                vhpiUserFctT userFct,
                                void *userData);

/* value processing */

/* vhpi_put_value modes */
typedef enum {
  vhpiDeposit,
  vhpiDepositPropagate,
  vhpiForce,
  vhpiForcePropagate,
  vhpiRelease,
  vhpiSizeConstraint
} vhpiPutValueModeT;

typedef enum {
  vhpiInertial,
  vhpiTransport
} vhpiDelayModeT;

XXTERN int vhpi_get_value (vhpiHandleT expr,
                           vhpiValueT *value_p);

XXTERN int vhpi_put_value (vhpiHandleT object,
                           vhpiValueT *value_p,
                           vhpiPutValueModeT mode);

XXTERN int vhpi_schedule_transaction (vhpiHandleT drivHdl,
                                      vhpiValueT *value_p,
                                      uint32_t numValues,
                                      vhpiTimeT *delayp,
                                      vhpiDelayModeT delayMode,
                                      vhpiTimeT *pulseRejp);

XXTERN int vhpi_format_value (const vhpiValueT *in_value_p,
                              vhpiValueT *out_value_p);

/* time processing */

XXTERN void vhpi_get_time (vhpiTimeT *time_p,
                           long *cycles);

#define vhpiNoActivity -1

XXTERN int vhpi_get_next_time (vhpiTimeT *time_p);

/* simulation control */

typedef enum {
  vhpiStop     = 0,
  vhpiFinish   = 1,
  vhpiReset    = 2
#ifdef VHPIEXTEND_CONTROL
  VHPIEXTEND_CONTROL
#endif
} vhpiSimControlT;

XXTERN int vhpi_control (vhpiSimControlT command,
                         ...);

/* I/O routine */

XXTERN int vhpi_printf (const char *format,
                                           ...);
XXTERN int vhpi_vprintf (const char *format, va_list args);

/* utilities to print VHDL strings */

XXTERN int vhpi_is_printable( char ch );


/* utility routines */

XXTERN int vhpi_compare_handles (vhpiHandleT handle1,
                                 vhpiHandleT handle2);

XXTERN int vhpi_check_error (vhpiErrorInfoT *error_info_p);

XXTERN int vhpi_release_handle (vhpiHandleT object);

/* creation functions */

XXTERN vhpiHandleT vhpi_create (vhpiClassKindT kind,
                                vhpiHandleT handle1,
                                vhpiHandleT handle2);

/* Foreign model data structures and functions */

typedef enum {
   vhpiArchF   = 1,
   vhpiFuncF   = 2,
   vhpiProcF   = 3,
   vhpiLibF    = 4,
   vhpiAppF    = 5
} vhpiForeignKindT;

typedef struct vhpiForeignDataS {
    vhpiForeignKindT kind;
    char * libraryName;
    char * modelName;
    void (*elabf)(const struct vhpiCbDataS *cb_data_p);
    void (*execf)(const struct vhpiCbDataS *cb_data_p);
} vhpiForeignDataT;

XXTERN vhpiHandleT vhpi_register_foreignf
                (vhpiForeignDataT *foreignDatap);

/* vhpi_get_foreign_info is DEPRECATED and is replaced
   by the function vhpi_get_foreignf_info */
XXTERN int vhpi_get_foreignf_info (vhpiHandleT hdl,
                                   vhpiForeignDataT *foreignDatap);

/* for saving and restoring foreign models data */

XXTERN size_t vhpi_get_data (int32_t id,
                             void *dataLoc,
                             size_t numBytes);

XXTERN size_t vhpi_put_data (int32_t id,
                             void *dataLoc,
                             size_t numBytes);

#ifdef VHPIEXTEND_FUNCTIONS
       VHPIEXTEND_FUNCTIONS
#endif

/**************************** Typedef for VHPI registration functions
 ****************************/

typedef void (*vhpiRegistrationFctT)();


#undef PLI_EXTERN
#undef PLI_VEXTERN

#ifdef VHPI_USER_DEFINED_DLLISPEC
#undef VHPI_USER_DEFINED_DLLISPEC
#undef PLI_DLLISPEC
#endif
#ifdef VHPI_USER_DEFINED_DLLESPEC
#undef VHPI_USER_DEFINED_DLLESPEC
#undef PLI_DLLESPEC
#endif

#ifdef PLI_PROTOTYPES
#undef PLI_PROTOTYPES
#undef XXTERN
#undef EETERN
#endif

#ifdef  __cplusplus
}
#endif

#endif /* VHPI_USER_H */
