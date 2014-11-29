with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;
with Grt.Types; use Grt.Types;

package Grt.Fst_Api is

   subtype fstHandle is unsigned;
   Null_fstHandle : constant fstHandle := 0;

   type fstWriterPackType is
     (FST_WR_PT_ZLIB,
      FST_WR_PT_FASTLZ,
      FST_WR_PT_LZ4);
   pragma Convention (C, fstWriterPackType);

   subtype fstFileType is unsigned;
   FST_FT_MIN          : constant fstFileType := 0;
   FST_FT_VERILOG      : constant fstFileType := 0;
   FST_FT_VHDL         : constant fstFileType := 1;
   FST_FT_VERILOG_VHDL : constant fstFileType := 2;
   FST_FT_MAX          : constant fstFileType := 2;

   subtype fstBlockType is unsigned;
   FST_BL_HDR               : constant fstBlockType := 0;
   FST_BL_VCDATA            : constant fstBlockType := 1;
   FST_BL_BLACKOUT          : constant fstBlockType := 2;
   FST_BL_GEOM              : constant fstBlockType := 3;
   FST_BL_HIER              : constant fstBlockType := 4;
   FST_BL_VCDATA_DYN_ALIAS  : constant fstBlockType := 5;
   FST_BL_HIER_LZ4          : constant fstBlockType := 6;
   FST_BL_HIER_LZ4DUO       : constant fstBlockType := 7;
   FST_BL_VCDATA_DYN_ALIAS2 : constant fstBlockType := 8;
   FST_BL_ZWRAPPER          : constant fstBlockType := 254;
   FST_BL_SKIP              : constant fstBlockType := 255;

   subtype fstScopeType is unsigned;
   FST_ST_MIN               : constant fstScopeType := 0;
   FST_ST_VCD_MODULE        : constant fstScopeType := 0;
   FST_ST_VCD_TASK          : constant fstScopeType := 1;
   FST_ST_VCD_FUNCTION      : constant fstScopeType := 2;
   FST_ST_VCD_BEGIN         : constant fstScopeType := 3;
   FST_ST_VCD_FORK          : constant fstScopeType := 4;
   FST_ST_VCD_GENERATE      : constant fstScopeType := 5;
   FST_ST_VCD_STRUCT        : constant fstScopeType := 6;
   FST_ST_VCD_UNION         : constant fstScopeType := 7;
   FST_ST_VCD_CLASS         : constant fstScopeType := 8;
   FST_ST_VCD_INTERFACE     : constant fstScopeType := 9;
   FST_ST_VCD_PACKAGE       : constant fstScopeType := 10;
   FST_ST_VCD_PROGRAM       : constant fstScopeType := 11;
   FST_ST_VHDL_ARCHITECTURE : constant fstScopeType := 12;
   FST_ST_VHDL_PROCEDURE    : constant fstScopeType := 13;
   FST_ST_VHDL_FUNCTION     : constant fstScopeType := 14;
   FST_ST_VHDL_RECORD       : constant fstScopeType := 15;
   FST_ST_VHDL_PROCESS      : constant fstScopeType := 16;
   FST_ST_VHDL_BLOCK        : constant fstScopeType := 17;
   FST_ST_VHDL_FOR_GENERATE : constant fstScopeType := 18;
   FST_ST_VHDL_IF_GENERATE  : constant fstScopeType := 19;
   FST_ST_VHDL_GENERATE     : constant fstScopeType := 20;
   FST_ST_VHDL_PACKAGE      : constant fstScopeType := 21;
   FST_ST_MAX               : constant fstScopeType := 21;
   FST_ST_GEN_ATTRBEGIN     : constant fstScopeType := 252;
   FST_ST_GEN_ATTREND       : constant fstScopeType := 253;
   FST_ST_VCD_SCOPE         : constant fstScopeType := 254;
   FST_ST_VCD_UPSCOPE       : constant fstScopeType := 255;

   subtype fstVarType is unsigned;
   FST_VT_MIN                : constant fstVarType := 0;
   FST_VT_VCD_EVENT          : constant fstVarType := 0;
   FST_VT_VCD_INTEGER        : constant fstVarType := 1;
   FST_VT_VCD_PARAMETER      : constant fstVarType := 2;
   FST_VT_VCD_REAL           : constant fstVarType := 3;
   FST_VT_VCD_REAL_PARAMETER : constant fstVarType := 4;
   FST_VT_VCD_REG            : constant fstVarType := 5;
   FST_VT_VCD_SUPPLY0        : constant fstVarType := 6;
   FST_VT_VCD_SUPPLY1        : constant fstVarType := 7;
   FST_VT_VCD_TIME           : constant fstVarType := 8;
   FST_VT_VCD_TRI            : constant fstVarType := 9;
   FST_VT_VCD_TRIAND         : constant fstVarType := 10;
   FST_VT_VCD_TRIOR          : constant fstVarType := 11;
   FST_VT_VCD_TRIREG         : constant fstVarType := 12;
   FST_VT_VCD_TRI0           : constant fstVarType := 13;
   FST_VT_VCD_TRI1           : constant fstVarType := 14;
   FST_VT_VCD_WAND           : constant fstVarType := 15;
   FST_VT_VCD_WIRE           : constant fstVarType := 16;
   FST_VT_VCD_WOR            : constant fstVarType := 17;
   FST_VT_VCD_PORT           : constant fstVarType := 18;
   FST_VT_VCD_SPARRAY        : constant fstVarType := 19;
   FST_VT_VCD_REALTIME       : constant fstVarType := 20;
   FST_VT_GEN_STRING         : constant fstVarType := 21;
   FST_VT_SV_BIT             : constant fstVarType := 22;
   FST_VT_SV_LOGIC           : constant fstVarType := 23;
   FST_VT_SV_INT             : constant fstVarType := 24;
   FST_VT_SV_SHORTINT        : constant fstVarType := 25;
   FST_VT_SV_LONGINT         : constant fstVarType := 26;
   FST_VT_SV_BYTE            : constant fstVarType := 27;
   FST_VT_SV_ENUM            : constant fstVarType := 28;
   FST_VT_SV_SHORTREAL       : constant fstVarType := 29;
   FST_VT_MAX                : constant fstVarType := 29;

   subtype fstVarDir is unsigned;
   FST_VD_MIN      : constant fstVarDir := 0;
   FST_VD_IMPLICIT : constant fstVarDir := 0;
   FST_VD_INPUT    : constant fstVarDir := 1;
   FST_VD_OUTPUT   : constant fstVarDir := 2;
   FST_VD_INOUT    : constant fstVarDir := 3;
   FST_VD_BUFFER   : constant fstVarDir := 4;
   FST_VD_LINKAGE  : constant fstVarDir := 5;
   FST_VD_MAX      : constant fstVarDir := 5;

   subtype fstHierType is unsigned;
   FST_HT_MIN       : constant fstHierType := 0;
   FST_HT_SCOPE     : constant fstHierType := 0;
   FST_HT_UPSCOPE   : constant fstHierType := 1;
   FST_HT_VAR       : constant fstHierType := 2;
   FST_HT_ATTRBEGIN : constant fstHierType := 3;
   FST_HT_ATTREND   : constant fstHierType := 4;
   FST_HT_MAX       : constant fstHierType := 4;

   subtype fstAttrType is unsigned;
   FST_AT_MIN   : constant fstAttrType := 0;
   FST_AT_MISC  : constant fstAttrType := 0;
   FST_AT_ARRAY : constant fstAttrType := 1;
   FST_AT_ENUM  : constant fstAttrType := 2;
   FST_AT_PACK  : constant fstAttrType := 3;
   FST_AT_MAX   : constant fstAttrType := 3;

   subtype fstMiscType is unsigned;
   FST_MT_MIN         : constant fstMiscType := 0;
   FST_MT_COMMENT     : constant fstMiscType := 0;
   FST_MT_ENVVAR      : constant fstMiscType := 1;
   FST_MT_SUPVAR      : constant fstMiscType := 2;
   FST_MT_PATHNAME    : constant fstMiscType := 3;
   FST_MT_SOURCESTEM  : constant fstMiscType := 4;
   FST_MT_SOURCEISTEM : constant fstMiscType := 5;
   FST_MT_UNKNOWN     : constant fstMiscType := 6;
   FST_MT_MAX         : constant fstMiscType := 6;

   subtype fstArrayType is unsigned;
   FST_AR_MIN      : constant fstArrayType := 0;
   FST_AR_NONE     : constant fstArrayType := 0;
   FST_AR_UNPACKED : constant fstArrayType := 1;
   FST_AR_PACKED   : constant fstArrayType := 2;
   FST_AR_SPARSE   : constant fstArrayType := 3;
   FST_AR_MAX      : constant fstArrayType := 3;

   subtype fstEnumValueType is unsigned;
   FST_EV_SV_INTEGER           : constant fstEnumValueType := 0;
   FST_EV_SV_BIT               : constant fstEnumValueType := 1;
   FST_EV_SV_LOGIC             : constant fstEnumValueType := 2;
   FST_EV_SV_INT               : constant fstEnumValueType := 3;
   FST_EV_SV_SHORTINT          : constant fstEnumValueType := 4;
   FST_EV_SV_LONGINT           : constant fstEnumValueType := 5;
   FST_EV_SV_BYTE              : constant fstEnumValueType := 6;
   FST_EV_SV_UNSIGNED_INTEGER  : constant fstEnumValueType := 7;
   FST_EV_SV_UNSIGNED_BIT      : constant fstEnumValueType := 8;
   FST_EV_SV_UNSIGNED_LOGIC    : constant fstEnumValueType := 9;
   FST_EV_SV_UNSIGNED_INT      : constant fstEnumValueType := 10;
   FST_EV_SV_UNSIGNED_SHORTINT : constant fstEnumValueType := 11;
   FST_EV_SV_UNSIGNED_LONGINT  : constant fstEnumValueType := 12;
   FST_EV_SV_UNSIGNED_BYTE     : constant fstEnumValueType := 13;
   FST_EV_MAX                  : constant fstEnumValueType := 13;

   subtype fstPackType is unsigned;
   FST_PT_NONE          : constant fstPackType := 0;
   FST_PT_UNPACKED      : constant fstPackType := 1;
   FST_PT_PACKED        : constant fstPackType := 2;
   FST_PT_TAGGED_PACKED : constant fstPackType := 3;
   FST_PT_MAX           : constant fstPackType := 3;

   subtype fstSupplementalVarType is unsigned;
   FST_SVT_MIN           : constant fstSupplementalVarType := 0;
   FST_SVT_NONE          : constant fstSupplementalVarType := 0;
   FST_SVT_VHDL_SIGNAL   : constant fstSupplementalVarType := 1;
   FST_SVT_VHDL_VARIABLE : constant fstSupplementalVarType := 2;
   FST_SVT_VHDL_CONSTANT : constant fstSupplementalVarType := 3;
   FST_SVT_VHDL_FILE     : constant fstSupplementalVarType := 4;
   FST_SVT_VHDL_MEMORY   : constant fstSupplementalVarType := 5;
   FST_SVT_MAX           : constant fstSupplementalVarType := 5;

   subtype fstSupplementalDataType is unsigned;
   FST_SDT_MIN                    : constant fstSupplementalDataType := 0;
   FST_SDT_NONE                   : constant fstSupplementalDataType := 0;
   FST_SDT_VHDL_BOOLEAN           : constant fstSupplementalDataType := 1;
   FST_SDT_VHDL_BIT               : constant fstSupplementalDataType := 2;
   FST_SDT_VHDL_BIT_VECTOR        : constant fstSupplementalDataType := 3;
   FST_SDT_VHDL_STD_ULOGIC        : constant fstSupplementalDataType := 4;
   FST_SDT_VHDL_STD_ULOGIC_VECTOR : constant fstSupplementalDataType := 5;
   FST_SDT_VHDL_STD_LOGIC         : constant fstSupplementalDataType := 6;
   FST_SDT_VHDL_STD_LOGIC_VECTOR  : constant fstSupplementalDataType := 7;
   FST_SDT_VHDL_UNSIGNED          : constant fstSupplementalDataType := 8;
   FST_SDT_VHDL_SIGNED            : constant fstSupplementalDataType := 9;
   FST_SDT_VHDL_INTEGER           : constant fstSupplementalDataType := 10;
   FST_SDT_VHDL_REAL              : constant fstSupplementalDataType := 11;
   FST_SDT_VHDL_NATURAL           : constant fstSupplementalDataType := 12;
   FST_SDT_VHDL_POSITIVE          : constant fstSupplementalDataType := 13;
   FST_SDT_VHDL_TIME              : constant fstSupplementalDataType := 14;
   FST_SDT_VHDL_CHARACTER         : constant fstSupplementalDataType := 15;
   FST_SDT_VHDL_STRING            : constant fstSupplementalDataType := 16;
   FST_SDT_MAX                    : constant fstSupplementalDataType := 16;
   FST_SDT_SVT_SHIFT_COUNT        : constant fstSupplementalDataType := 10;
   FST_SDT_ABS_MAX                : constant fstSupplementalDataType := 1023;

   type fstContext is new System.Address;
   Null_fstContext : constant fstContext := fstContext (System.Null_Address);

   procedure fstWriterClose (ctx : fstContext);
   pragma Import (C, fstWriterClose, "fstWriterClose");

   function fstWriterCreate (nam : Ghdl_C_String; use_compressed_hier : int)
                             return fstContext;
   pragma Import (C, fstWriterCreate, "fstWriterCreate");

   function fstWriterCreateVar
     (ctx : fstContext;
      vt : fstVarType;
      vd : fstVarDir;
      len : unsigned;
      nam : Ghdl_C_String;
      aliasHandle : fstHandle) return fstHandle;
   pragma Import (C, fstWriterCreateVar, "fstWriterCreateVar");

   function fstWriterCreateVar2
     (ctx : fstContext;
      vt : fstVarType;
      vd : fstVarDir;
      len : unsigned;
      nam : Ghdl_C_String;
      aliasHandle : fstHandle;
      c_type : Ghdl_C_String;
      svt : fstSupplementalVarType;
      sdt : fstSupplementalDataType) return fstHandle;
   pragma Import (C, fstWriterCreateVar2, "fstWriterCreateVar2");

   procedure fstWriterEmitValueChange
     (ctx : fstContext;
      handle : fstHandle;
      val : System.Address);
   pragma Import (C, fstWriterEmitValueChange, "fstWriterEmitValueChange");

   procedure fstWriterEmitVariableLengthValueChange
     (ctx : fstContext;
      handle : fstHandle;
      val : System.Address;
      len : unsigned);
   pragma Import (C, fstWriterEmitVariableLengthValueChange,
                  "fstWriterEmitVariableLengthValueChange");

   procedure fstWriterEmitDumpActive (ctx : fstContext; enable : int);
   pragma Import (C, fstWriterEmitDumpActive, "fstWriterEmitDumpActive");

   procedure fstWriterEmitTimeChange
     (ctx : fstContext; tim : Unsigned_64);
   pragma Import (C, fstWriterEmitTimeChange, "fstWriterEmitTimeChange");

   procedure fstWriterFlushContext (ctx : fstContext);
   pragma Import (C, fstWriterFlushContext, "fstWriterFlushContext");

   function fstWriterGetDumpSizeLimitReached (ctx : fstContext) return int;
   pragma Import (C, fstWriterGetDumpSizeLimitReached,
                  "fstWriterGetDumpSizeLimitReached");

   function fstWriterGetFseekFailed (ctx : fstContext) return int;
   pragma Import (C, fstWriterGetFseekFailed, "fstWriterGetFseekFailed");

   procedure fstWriterSetAttrBegin
     (ctx : fstContext;
      attrtype : fstAttrType;
      c_subtype : int;
      attrname : Ghdl_C_String;
      arg : Unsigned_64);
   pragma Import (C, fstWriterSetAttrBegin, "fstWriterSetAttrBegin");

   procedure fstWriterSetAttrEnd (ctx : fstContext);
   pragma Import (C, fstWriterSetAttrEnd, "fstWriterSetAttrEnd");

   procedure fstWriterSetComment (ctx : fstContext; comm : Ghdl_C_String);
   pragma Import (C, fstWriterSetComment, "fstWriterSetComment");

   procedure fstWriterSetDate (ctx : fstContext; dat : Ghdl_C_String);
   pragma Import (C, fstWriterSetDate, "fstWriterSetDate");

   procedure fstWriterSetDumpSizeLimit
     (ctx : Ghdl_C_String; numbytes : Unsigned_64);
   pragma Import (C, fstWriterSetDumpSizeLimit, "fstWriterSetDumpSizeLimit");

   procedure fstWriterSetEnvVar (ctx : fstContext; envvar : Ghdl_C_String);
   pragma Import (C, fstWriterSetEnvVar, "fstWriterSetEnvVar");

   procedure fstWriterSetFileType (ctx : fstContext; filetype : fstFileType);
   pragma Import (C, fstWriterSetFileType, "fstWriterSetFileType");

   procedure fstWriterSetPackType (ctx : fstContext; typ : fstWriterPackType);
   pragma Import (C, fstWriterSetPackType, "fstWriterSetPackType");

   procedure fstWriterSetParallelMode (ctx : fstContext; enable : int);
   pragma Import (C, fstWriterSetParallelMode, "fstWriterSetParallelMode");

   procedure fstWriterSetRepackOnClose (ctx : fstContext; enable : int);
   pragma Import (C, fstWriterSetRepackOnClose, "fstWriterSetRepackOnClose");

   procedure fstWriterSetScope
     (ctx : fstContext;
      scopetype : fstScopeType;
      scopename : Ghdl_C_String;
      scopecomp : Ghdl_C_String);
   pragma Import (C, fstWriterSetScope, "fstWriterSetScope");

   procedure fstWriterSetSourceInstantiationStem
     (ctx : fstContext;
      path : Ghdl_C_String;
      line : unsigned;
      use_realpath : unsigned);
   pragma Import (C, fstWriterSetSourceInstantiationStem,
                  "fstWriterSetSourceInstantiationStem");

   procedure fstWriterSetSourceStem
     (ctx : fstContext;
      path : Ghdl_C_String;
      line : unsigned;
      use_realpath : unsigned);
   pragma Import (C, fstWriterSetSourceStem, "fstWriterSetSourceStem");

   procedure fstWriterSetTimescale (ctx : fstContext; ts : Integer);
   pragma Import (C, fstWriterSetTimescale, "fstWriterSetTimescale");

   procedure fstWriterSetTimescaleFromString
     (ctx : fstContext; s : Ghdl_C_String);
   pragma Import (C, fstWriterSetTimescaleFromString,
                  "fstWriterSetTimescaleFromString");

   procedure fstWriterSetTimezero (ctx : fstContext; tim : Unsigned_64);
   pragma Import (C, fstWriterSetTimezero, "fstWriterSetTimezero");

   procedure fstWriterSetUpscope (ctx : fstContext);
   pragma Import (C, fstWriterSetUpscope, "fstWriterSetUpscope");

   procedure fstWriterSetVersion (ctx : fstContext; vers : Ghdl_C_String);
   pragma Import (C, fstWriterSetVersion, "fstWriterSetVersion");

   function fstUtilityBinToEsc
     (d : access unsigned_char;
      s : access unsigned_char;
      len : int) return int;
   pragma Import (C, fstUtilityBinToEsc, "fstUtilityBinToEsc");

   function fstUtilityEscToBin
     (d : access unsigned_char;
      s : access unsigned_char;
      len : int) return int;
   pragma Import (C, fstUtilityEscToBin, "fstUtilityEscToBin");

end Grt.Fst_Api;
