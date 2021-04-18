--  GHDL Run Time (GRT) - VHPI implementation.
--  Copyright (C) 2021 Marlon James
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

-- VHPI implementation that uses Avhpi.
-- Targeted functionality:
-- Handles allocation for transient strings and structures
-- Handles callback object creation and executing callbacks
-- Provides VHPI error support
-- Provides VHPI tracing support

with System; use System;
with Interfaces; use Interfaces;
with Grt.C; use Grt.C;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Callbacks;
with Grt.Rtis; use Grt.Rtis;
with Grt.Types; use Grt.Types;
with Grt.Vcd;

package Grt.Vhpi is

   -- **********************************************************************
   -- NOTE: Currently this is a minimal implementation that loads VHPI
   --       libraries but does not implement VHPI functionality beyond
   --       vhpi_check_error().
   --       Calling most vhpi_* functions will raise a VHPI error condition.
   -- **********************************************************************

   type Vhpi_Internal_Handle (<>) is private;
   type Vhpi_External_Handle is access Vhpi_Internal_Handle;
   pragma No_Strict_Aliasing (Vhpi_External_Handle);

   --  A null handle.
   Null_External_Handle : constant Vhpi_External_Handle;

   type VhpiFormatT is
     (
      VhpiBinStrVal,
      VhpiOctStrVal,
      VhpiDecStrVal,
      VhpiHexStrVal,
      VhpiEnumVal,
      VhpiIntVal,
      VhpiLogicVal,
      VhpiRealVal,
      VhpiStrVal,
      VhpiCharVal,
      VhpiTimeVal,
      VhpiPhysVal,
      VhpiObjTypeVal,
      VhpiPtrVal,
      VhpiEnumVecVal,
      VhpiIntVecVal,
      VhpiLogicVecVal,
      VhpiRealVecVal,
      VhpiTimeVecVal,
      VhpiPhysVecVal,
      VhpiPtrVecVal,
      VhpiRawDataVal,
      VhpiSmallEnumVal,
      VhpiSmallEnumVecVal,
      VhpiLongIntVal,
      VhpiLongIntVecVal,
      VhpiSmallPhysVal,
      VhpiSmallPhysVecVal
     );
   pragma Convention (C, VhpiFormatT);
   for VhpiFormatT use
     (
      VhpiBinStrVal       => 1,
      VhpiOctStrVal       => 2,
      VhpiDecStrVal       => 3,
      VhpiHexStrVal       => 4,
      VhpiEnumVal         => 5,
      VhpiIntVal          => 6,
      VhpiLogicVal        => 7,
      VhpiRealVal         => 8,
      VhpiStrVal          => 9,
      VhpiCharVal         => 10,
      VhpiTimeVal         => 11,
      VhpiPhysVal         => 12,
      VhpiObjTypeVal      => 13,
      VhpiPtrVal          => 14,
      VhpiEnumVecVal      => 15,
      VhpiIntVecVal       => 16,
      VhpiLogicVecVal     => 17,
      VhpiRealVecVal      => 18,
      VhpiTimeVecVal      => 19,
      VhpiPhysVecVal      => 20,
      VhpiPtrVecVal       => 21,
      VhpiRawDataVal      => 22,
      VhpiSmallEnumVal    => 23,
      VhpiSmallEnumVecVal => 24,
      VhpiLongIntVal      => 25,
      VhpiLongIntVecVal   => 26,
      VhpiSmallPhysVal    => 27,
      VhpiSmallPhysVecVal => 28
     );

   type VhpiFormat_Access is access VhpiFormatT;

   -- typedef struct vhpiPhysS
   -- {
   -- int32_t high;
   -- uint32_t low;
   -- } vhpiPhysT;
   type VhpiPhysT is record
      High : Integer_32;
      Low : Unsigned_32;
   end record;
   pragma Convention (C, VhpiPhysT);

   type VhpiTimeT is new VhpiPhysT;
   pragma Convention (C, VhpiTimeT);

   type VhpiTime_Access is access VhpiTimeT;

   -- typedef struct vhpiValueS
   -- {
   -- vhpiFormatT format;
   -- size_t bufSize;
   -- int32_t numElems;
   -- vhpiPhysT unit;
   -- union
   --    {
   --       vhpiEnumT enumv, *enumvs;
   --       vhpiSmallEnumT smallenumv, *smallenumvs;
   --       vhpiIntT  intg, *intgs;
   --       vhpiLongIntT  longintg, *longintgs;
   --       vhpiRealT real, *reals;
   --       vhpiSmallPhysT smallphys, *smallphyss;
   --       vhpiPhysT phys, *physs;
   --       vhpiTimeT time, *times;
   --       vhpiCharT ch, *str;
   --       void *ptr, **ptrs;
   --    } value;
   -- } vhpiValueT;
   type VhpiValueT (Format : VhpiFormatT) is record
      BufSize : size_t;
      NumElems : Unsigned_32;
      Unit : VhpiPhysT;
      case Format is
         when VhpiBinStrVal
           | VhpiOctStrVal
           | VhpiDecStrVal
           | VhpiHexStrVal
           | VhpiStrVal =>
            Str : Ghdl_C_String;
         when VhpiEnumVal
           | VhpiLogicVal =>
            Enumv : Unsigned_32;
         when VhpiSmallEnumVal =>
            SmallEnumv : Unsigned_8;
         when VhpiIntVal =>
            Intg : Integer_32;
         when VhpiLongIntVal =>
            LongIntg : Integer_64;
         when VhpiRealVal =>
            Realv : Ghdl_Real;
         when VhpiCharVal =>
            Ch : Character;
         when VhpiTimeVal =>
            Time : VhpiTimeT;
         when VhpiPhysVal =>
            Phys : VhpiPhysT;
         when VhpiSmallPhysVal =>
            SmallPhys : Integer_32;
         when VhpiPtrVal
           | VhpiRawDataVal =>
            Ptr : System.Address;
         when VhpiEnumVecVal
           | VhpiLogicVecVal =>
            Enumvs : System.Address;
         when VhpiSmallEnumVecVal =>
            SmallEnumvs : System.Address;
         when VhpiIntVecVal =>
            Intgs : System.Address;
         when VhpiLongIntVecVal =>
            LongIntgs : System.Address;
         when VhpiRealVecVal =>
            Realvs : System.Address;
         when VhpiTimeVecVal =>
            Times : System.Address;
         when VhpiPhysVecVal =>
            Physs : System.Address;
         when VhpiSmallPhysVecVal =>
            SmallPhyss : System.Address;
         when VhpiPtrVecVal =>
            Ptrs : System.Address;
         when others =>
            -- VhpiObjTypeVal
            null;
         end case;
   end record;
   --  No use of convention C, as there is no direct equivalent in the norm.

   type VhpiValue_Access is access VhpiValueT;

   type VhpiSeverityT is
     (
      VhpiNote,
      VhpiWarning,
      VhpiError,
      VhpiSystem,
      VhpiInternal,
      VhpiFailure
     );
   pragma Convention (C, VhpiSeverityT);
   for VhpiSeverityT use
     (
      VhpiNote       => 1,
      VhpiWarning    => 2,
      VhpiError      => 3,
      VhpiSystem     => 4,
      VhpiInternal   => 5,
      VhpiFailure    => 6
     );

   -- typedef struct vhpiErrorInfoS
   -- {
   --   vhpiSeverityT    severity;
   --   char       *message;
   --   char       *str;
   --   char       *file;
   --   int32_t     line;
   -- } vhpiErrorInfoT;
   type VhpiErrorInfoT is record
      Severity : VhpiSeverityT;
      Msg : Ghdl_C_String;
      Str : Ghdl_C_String;
      File : Ghdl_C_String;
      Line : Integer_32;
   end record;
   pragma Convention (C, VhpiErrorInfoT);

   type VhpiErrorInfo_Access is access VhpiErrorInfoT;

   -- typedef struct vhpiCbDataS
   -- {
   -- int32_t reason;
   -- void (*cb_rtn) (const struct vhpiCbDataS *);
   -- vhpiHandleT obj;
   -- vhpiTimeT *time;
   -- vhpiValueT *value;
   -- void *user_data;
   -- } vhpiCbDataT;
   type VhpiCbDataT;
   type VhpiCbFctT is access procedure (Data : access constant VhpiCbDataT);
   pragma Convention (C, VhpiCbFctT);

   type VhpiCbDataT is record
      Reason : Integer_32;
      Cb_Rtn : VhpiCbFctT;
      Obj : Vhpi_External_Handle;
      Time : VhpiTime_Access;
      Value : VhpiValue_Access;
      User_Data : System.Address;
   end record;
   pragma Convention (C, VhpiCbDataT);

   type VhpiCbData_Access is access VhpiCbDataT;

   type Callback_Flags is private;

   ----------------------------------------------------------------------------
   -- VHPI functions
   ----------------------------------------------------------------------------

   -- Callback related

   -- vhpiHandleT vhpi_register_cb (vhpiCbDataT *cb_data_p, int32_t flags)
   function vhpi_register_cb (Data : VhpiCbData_Access; Flags : Callback_Flags)
                             return Vhpi_External_Handle;
   pragma Export (C, vhpi_register_cb, "vhpi_register_cb");

   -- int vhpi_remove_cb (vhpiHandleT cb_obj)
   function vhpi_remove_cb (Cb : Vhpi_External_Handle) return Integer;
   pragma Export (C, vhpi_remove_cb, "vhpi_remove_cb");

   -- int vhpi_disable_cb (vhpiHandleT cb_obj)
   function vhpi_disable_cb (Cb : Vhpi_External_Handle) return Integer;
   pragma Export (C, vhpi_disable_cb, "vhpi_disable_cb");

   -- int vhpi_enable_cb (vhpiHandleT cb_obj)
   function vhpi_enable_cb (Cb : Vhpi_External_Handle) return Integer;
   pragma Export (C, vhpi_enable_cb, "vhpi_enable_cb");

   -- int vhpi_get_cb_info (vhpiHandleT object, vhpiCbDataT *cb_data_p)
   function vhpi_get_cb_info
     (Obj : Vhpi_External_Handle; Data : VhpiCbData_Access) return Integer;
   pragma Export (C, vhpi_get_cb_info, "vhpi_get_cb_info");

   -- For obtaining handles

   -- vhpiHandleT vhpi_handle_by_name (const char *name, vhpiHandleT scope)
   function vhpi_handle_by_name
     (Name : Ghdl_C_String; Scope : Vhpi_External_Handle)
     return Vhpi_External_Handle;
   pragma Export (C, vhpi_handle_by_name, "vhpi_handle_by_name");

   -- vhpiHandleT vhpi_handle_by_index (vhpiOneToManyT itRel,
   --                                   vhpiHandleT parent, int32_t indx)
   function vhpi_handle_by_index
     (Rel : Integer; Parent : Vhpi_External_Handle; Index: Integer)
     return Vhpi_External_Handle;
   pragma Export (C, vhpi_handle_by_index, "vhpi_handle_by_index");

   -- For traversing relationships

   -- vhpiHandleT vhpi_handle (vhpiOneToOneT type,
   --                          vhpiHandleT referenceHandle)
   function vhpi_handle (Rel: Integer; Ref: Vhpi_External_Handle)
                        return Vhpi_External_Handle;
   pragma Export (C, vhpi_handle, "vhpi_handle");

   -- vhpiHandleT vhpi_iterator (vhpiOneToManyT type,
   --                            vhpiHandleT referenceHandle)
   function vhpi_iterator (Rel: Integer; Ref: Vhpi_External_Handle)
                          return Vhpi_External_Handle;
   pragma Export (C, vhpi_iterator, "vhpi_iterator");

   -- vhpiHandleT vhpi_scan (vhpiHandleT iterator)
   function vhpi_scan (Iter : Vhpi_External_Handle)
                      return Vhpi_External_Handle;
   pragma Export (C, vhpi_scan, "vhpi_scan");

   -- For processing properties

   -- vhpiIntT vhpi_get (vhpiIntPropertyT property, vhpiHandleT object)
   function vhpi_get (Property: Integer; Ref: Vhpi_External_Handle)
                     return VhpiIntT;
   pragma Export (C, vhpi_get, "vhpi_get");

   -- const vhpiCharT * vhpi_get_str (vhpiStrPropertyT property,
   --                                 vhpiHandleT object)
   function vhpi_get_str (Property : Integer; Ref : Vhpi_External_Handle)
                         return Ghdl_C_String;
   pragma Export (C, vhpi_get_str, "vhpi_get_str");

   -- vhpiRealT vhpi_get_real (vhpiRealPropertyT property, vhpiHandleT object)
   function vhpi_get_real (Property : Integer; Ref : Vhpi_External_Handle)
                          return Ghdl_Real;
   pragma Export (C, vhpi_get_real, "vhpi_get_real");

   -- vhpiPhysT vhpi_get_phys (vhpiPhysPropertyT property, vhpiHandleT object)
   function vhpi_get_phys (Property : Integer; Ref : Vhpi_External_Handle)
                          return VhpiPhysT;
   pragma Export (C, vhpi_get_phys, "vhpi_get_phys");

   -- For access to protected types

   type VhpiUserFctT is access function return Integer;
   pragma Convention (C, VhpiUserFctT);

   -- int vhpi_protected_call (vhpiHandleT varHdl,
   --                          vhpiUserFctT userFct,
   --                          void *userData)
   function vhpi_protected_call (Var : Vhpi_External_Handle;
                                 User_Fun : VhpiUserFctT;
                                 User_Data : System.Address)
                                return Integer;
   pragma Export (C, vhpi_protected_call, "vhpi_protected_call");

   -- For value processing

   type VhpiPutValueModeT is
     (
      VhpiDeposit,
      VhpiDepositPropagate,
      VhpiForce,
      VhpiForcePropagate,
      VhpiRelease,
      VhpiSizeConstraint
     );
   pragma Convention (C, VhpiPutValueModeT);

   type VhpiDelayModeT is
     (
      VhpiInertial,
      VhpiTransport
     );
   pragma Convention (C, VhpiDelayModeT);

   -- int vhpi_get_value (vhpiHandleT expr, vhpiValueT *value_p)
   function vhpi_get_value
     (Expr : Vhpi_External_Handle; Value : VhpiValue_Access) return Integer;
   pragma Export (C, vhpi_get_value, "vhpi_get_value");

   -- int vhpi_put_value (vhpiHandleT object,
   --                     vhpiValueT *value_p,
   --                     vhpiPutValueModeT mode)
   function vhpi_put_value (Obj : Vhpi_External_Handle;
                            Value : VhpiValue_Access;
                            ModeInt : Integer)
                           return Integer;
   pragma Export (C, vhpi_put_value, "vhpi_put_value");

   -- int vhpi_schedule_transaction (vhpiHandleT drivHdl,
   --                                vhpiValueT *value_p,
   --                                uint32_t numValues,
   --                                vhpiTimeT *delayp,
   --                                vhpiDelayModeT delayMode,
   --                                vhpiTimeT *pulseRejp)
   function vhpi_schedule_transaction (Driver : Vhpi_External_Handle;
                                       Value : VhpiValue_Access;
                                       Num_Values : Unsigned_32;
                                       Delay_Value : VhpiTime_Access;
                                       Delay_ModeInt : Integer;
                                       Pulse_Rejection : VhpiTime_Access)
                                      return Integer;
   pragma Export (C, vhpi_schedule_transaction, "vhpi_schedule_transaction");

   -- int vhpi_format_value (const vhpiValueT *in_value_p,
   --                        vhpiValueT *out_value_p)
   function vhpi_format_value
     (In_Val : VhpiValue_Access; Out_Val : VhpiValue_Access) return Integer;
   pragma Export (C, vhpi_format_value, "vhpi_format_value");

   -- For time processing

   type Long_Access is access Long_Integer;
   pragma Convention (C, Long_Access);

   -- void vhpi_get_time (vhpiTimeT *time_p, long *cycles)
   procedure vhpi_get_time (Time : VhpiTime_Access; Cycles : Long_Access);
   pragma Export (C, vhpi_get_time, "vhpi_get_time");

   vhpiNoActivity : constant Integer := -1;

   -- int vhpi_get_next_time (vhpiTimeT *time_p)
   function vhpi_get_next_time (Time : VhpiTime_Access) return Integer;
   pragma Export (C, vhpi_get_next_time, "vhpi_get_next_time");

   -- Missing, see grt-cvhpi.c
   -- int vhpi_control (vhpiSimControlT command, ...);
   -- int vhpi_printf (const char *format, ...);
   -- int vhpi_vprintf (const char *format, va_list args);

   -- Utilities to print VHDL strings

   -- int vhpi_is_printable ( char ch )
   function vhpi_is_printable (Ch : Character) return Integer;
   pragma Export (C, vhpi_is_printable, "vhpi_is_printable");

   -- Utility routines

   -- int vhpi_compare_handles (vhpiHandleT handle1, vhpiHandleT handle2)
   function vhpi_compare_handles (Hdl1, Hdl2 : Vhpi_External_Handle)
                                 return Integer;
   pragma Export (C, vhpi_compare_handles, "vhpi_compare_handles");

   -- int vhpi_check_error (vhpiErrorInfoT *error_info_p)
   function vhpi_check_error (Info : VhpiErrorInfo_Access) return Integer;
   pragma Export (C, vhpi_check_error, "vhpi_check_error");

   -- int vhpi_release_handle (vhpiHandleT object)
   function vhpi_release_handle (Obj : Vhpi_External_Handle) return Integer;
   pragma Export (C, vhpi_release_handle, "vhpi_release_handle");

   -- Creation functions
   -- vhpiHandleT vhpi_create (vhpiClassKindT kind,
   --                          vhpiHandleT handle1,
   --                          vhpiHandleT handle2)
   function vhpi_create (Kind : Integer; Hdl1, Hdl2 : Vhpi_External_Handle)
                        return Vhpi_External_Handle;
   pragma Export (C, vhpi_create, "vhpi_create");

   -- Foreign model data structures and functions

   type VhpiForeignKindT is
     (
      VhpiArchF,
      VhpiFuncF,
      VhpiProcF,
      VhpiLibF,
      VhpiAppF
     );
   pragma Convention (C, VhpiForeignKindT);
   for VhpiForeignKindT use
     (
      VhpiArchF => 1,
      VhpiFuncF => 2,
      VhpiProcF => 3,
      VhpiLibF  => 4,
      VhpiAppF  => 5
     );

   type VhpiForeignFctT is new VhpiCbFctT;
   pragma Convention (C, VhpiForeignFctT);

   -- typedef struct vhpiForeignDataS {
   --    vhpiForeignKindT kind;
   --    char * libraryName;
   --    char * modelName;
   --    void (*elabf) (const struct vhpiCbDataS *cb_data_p);
   --    void (*execf) (const struct vhpiCbDataS *cb_data_p);
   -- } vhpiForeignDataT;
   type VhpiForeignDataT is record
      Kind : VhpiForeignKindT;
      Lib_Name : Ghdl_C_String;
      Model_Name : Ghdl_C_String;
      Elab_Fun : VhpiForeignFctT;
      Exec_Fun : VhpiForeignFctT;
   end record;
   pragma Convention (C, VhpiForeignDataT);

   type VhpiForeignData_Access is access VhpiForeignDataT;

   -- vhpiHandleT vhpi_register_foreignf (vhpiForeignDataT *foreignDatap)
   function vhpi_register_foreignf (Data : VhpiForeignData_Access)
                                   return Vhpi_External_Handle;
   pragma Export (C, vhpi_register_foreignf, "vhpi_register_foreignf");

   -- int vhpi_get_foreignf_info (vhpiHandleT hdl,
   --                             vhpiForeignDataT *foreignDatap)
   function vhpi_get_foreignf_info
     (Hdl : Vhpi_External_Handle; Data : VhpiForeignData_Access)
     return Integer;
   pragma Export (C, vhpi_get_foreignf_info, "vhpi_get_foreignf_info");

   -- For saving and restoring foreign models data

   -- size_t vhpi_get_data (int32_t id, void *dataLoc, size_t numBytes);
   function vhpi_get_data
     (Id : Integer_32; Data_Loc : System.Address; Num_Bytes : size_t)
     return size_t;
   pragma Export (C, vhpi_get_data, "vhpi_get_data");

   -- size_t vhpi_put_data (int32_t id, void *dataLoc, size_t numBytes);
   function vhpi_put_data
     (Id : Integer_32; Data_Loc : System.Address; Num_Bytes : size_t)
     return size_t;
   pragma Export (C, vhpi_put_data, "vhpi_put_data");

   ----------------------------------------------------------------------------
   -- Internal helper functions
   ----------------------------------------------------------------------------

   type VhpiSimControlT is
     (
      VhpiStop,
      VhpiFinish,
      VhpiReset
     );
   pragma Convention (C, VhpiSimControlT);

   -- int vhpi_control (vhpiSimControlT command, ...)
   -- See grt-cvhpi.c
   function Vhpi_Control_Internal (CommandInt : Integer; Status : Integer)
                                  return Integer;
   pragma Export (C, Vhpi_Control_Internal, "Vhpi_Control_Internal");

   -- int vhpi_assert (vhpiSeverityT severity, char *formatmsg, ...)
   -- See grt-cvhpi.c
   function Vhpi_Assert_Internal (Severity : Integer; Msg : Ghdl_C_String)
                                 return Integer;
   pragma Export (C, Vhpi_Assert_Internal, "Vhpi_Assert_Internal");

   procedure Register;

private
   type Callback_Flags is new Integer_32;
   VhpiReturnCb   : constant Callback_Flags :=  2#0000_0001#;
   VhpiDisableCb  : constant Callback_Flags :=  2#0000_0010#;

   Null_External_Handle : constant Vhpi_External_Handle := null;

   -- Wrap VhpiHandleT
   -- Keep Callback objects out of Avhpi, they are allocated when registered
   type Vhpi_Internal_Handle (Kind : VhpiClassKindT) is record
      case Kind is
         when VhpiCallbackK =>
            Cb : aliased VhpiCbDataT;
            Cb_Prev, Cb_Next : Vhpi_External_Handle;
            Cb_Wire : Grt.Vcd.Verilog_Wire_Info;
            Cb_Handle : Callbacks.Callback_Handle;
            Cb_Refcnt : Natural;
         when others =>
            Ref : VhpiHandleT;
      end case;
   end record;

end Grt.Vhpi;
