--  GHDL Run Time (GRT) - VPI interface.
--  Copyright (C) 2002 - 2014 Tristan Gingold & Felix Bertram
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

-- Description: VPI interface for GRT runtime
--              the main purpose of this code is to interface with the
--              Icarus Verilog Interactive (IVI) simulator GUI

with System; use System;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Grt.Types; use Grt.Types;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Vcd;
with Grt.Callbacks;

package Grt.Vpi is
   --  Properties and objects, see vpi_user.h
   vpiUndefined :     constant Integer := -1;
   vpiType :          constant Integer :=  1;
   vpiName :          constant Integer :=  2;
   vpiFullName :      constant Integer :=  3;
   vpiSize :          constant Integer :=  4;
   vpiFile :          constant Integer :=  5;
   vpiLineNo :        constant Integer :=  6;
   vpiConstant :      constant Integer :=  7;

   vpiDefName :       constant Integer :=  9;
   vpiTimePrecision : constant Integer := 12;
   vpiDefFile :       constant Integer := 15;

   vpiScalar :        constant Integer := 17;
   vpiVector :        constant Integer := 18;

   vpiDirection :     constant Integer := 20;
   vpiInput :         constant Integer :=  1;
   vpiOutput :        constant Integer :=  2;
   vpiInout :         constant Integer :=  3;
   vpiMixedIO :       constant Integer :=  4;
   vpiNoDirection :   constant Integer :=  5;

   vpiIntegerVar :    constant Integer := 25;
   vpiMemory :        constant Integer := 29;
   vpiModPath :       constant Integer := 31;
   vpiModule :        constant Integer := 32;
   vpiNamedEvent :    constant Integer := 34;
   vpiNet :           constant Integer := 36;
   vpiParameter :     constant Integer := 41;
   vpiPort :          constant Integer := 44;
   vpiRealVar :       constant Integer := 47;
   vpiReg :           constant Integer := 48;
   vpiTchk :          constant Integer := 61;

   vpiLeftRange :     constant Integer := 79;
   vpiRightRange :    constant Integer := 83;
   vpiScope :         constant Integer := 84;
   vpiInternalScope : constant Integer := 92;
   vpiProcess :       constant Integer := 92;

   vpiPrimitive :       constant Integer := 103;
   vpiAttribute :       constant Integer := 105;
   vpiPrimitiveArray :  constant Integer := 113;
   vpiNetArray :        constant Integer := 114;
   vpiRange :           constant Integer := 115;
   vpiRegArray :        constant Integer := 116;
   vpiNamedEventArray : constant Integer := 129;

   --  vpi_control constants.
   vpiStop :          constant := 66;
   vpiFinish :        constant := 67;
   vpiReset :         constant := 68;

   --  Additionnal constants.
   vpiCallback :     constant Integer := 200;

   -- codes for the format tag of the vpi_value structure
   vpiBinStrVal:     constant Integer :=  1;
   vpiOctStrVal:     constant Integer :=  2;
   vpiDecStrVal:     constant Integer :=  3;
   vpiHexStrVal:     constant Integer :=  4;
   vpiScalarVal:     constant Integer :=  5;
   vpiIntVal:        constant Integer :=  6;
   vpiRealVal:       constant Integer :=  7;
   vpiStringVal:     constant Integer :=  8;
   vpiVectorVal:     constant Integer :=  9;
   vpiStrengthVal:   constant Integer := 10;
   vpiTimeVal:       constant Integer := 11;
   vpiObjTypeVal:    constant Integer := 12;
   vpiSuppressVal:   constant Integer := 13;

   -- codes for type tag of vpi_time structure
   vpiSimTime:       constant Integer :=  2;

   -- codes for the reason tag of cb_data structure
   cbValueChange       : constant := 1;
   cbReadWriteSynch    : constant := 6;
   cbReadOnlySynch     : constant := 7;
   cbNextSimTime       : constant := 8;
   cbAfterDelay        : constant := 9;
   cbEndOfCompile      : constant := 10;
   cbStartOfSimulation : constant := 11;
   cbEndOfSimulation   : constant := 12;

   --  Error types.
   vpiCompile : constant := 1;
   vpiPLI     : constant := 2;
   vpiRun     : constant := 3;

   --  Error severity levels.
   vpiNotive   : constant := 1;
   vpiWarning  : constant := 2;
   vpiError    : constant := 3;
   vpiSystem   : constant := 4;
   vpiInternal : constant := 5;

   type struct_vpiHandle (<>) is private;
   type vpiHandle is access struct_vpiHandle;
   pragma No_Strict_Aliasing (vpiHandle);

   -- typedef struct t_vpi_time {
   --   int type;
   --   unsigned int high;
   --   unsigned int low;
   --   double real;
   -- } s_vpi_time, *p_vpi_time;
   type s_vpi_time is record
      mType : Integer;
      mHigh : Unsigned_32;
      mLow :  Unsigned_32;
      mReal : Long_Float;
   end record;
   pragma Convention (C, s_vpi_time);
   type p_vpi_time is access s_vpi_time;

   -- typedef struct t_vpi_value
   -- { int format;
   --   union
   --   {       char*str;
   --           int scalar;
   --           int integer;
   --           double real;
   --           struct t_vpi_time *time;
   --           struct t_vpi_vecval *vector;
   --           struct t_vpi_strengthval *strength;
   --           char*misc;
   --   } value;
   -- } s_vpi_value, *p_vpi_value;
   type s_vpi_value (Format : Integer) is record
      case Format is
         when vpiBinStrVal
           | vpiOctStrVal
           | vpiDecStrVal
           | vpiHexStrVal
           | vpiStringVal =>
            Str : Ghdl_C_String;
         when vpiScalarVal =>
            Scalar : Integer;
         when vpiIntVal =>
            Integer_m : Integer;
         when vpiRealVal=>
            Real_M : Ghdl_F64;
            --when vpiTimeVal=>     mTime:     p_vpi_time;
            --when vpiVectorVal=>   mVector:   p_vpi_vecval;
            --when vpiStrengthVal=> mStrength: p_vpi_strengthval;
         when others =>
            null;
         end case;
   end record;
   --  No use of convention C, as there is no direct equivalent in the norm.
   type p_vpi_value is access s_vpi_value;

   --typedef struct t_cb_data {
   --      int reason;
   --      int (*cb_rtn)(struct t_cb_data*cb);
   --      vpiHandle obj;
   --      p_vpi_time time;
   --      p_vpi_value value;
   --      int index;
   --      char *user_data;
   --} s_cb_data, *p_cb_data;
   type s_cb_data;

   type p_cb_data is access all s_cb_data;
   pragma Convention (C, p_cb_data);
   function To_p_cb_data is new Ada.Unchecked_Conversion
     (Source => Address, Target => p_cb_data);

   type cb_rtn_type is access function (Cb : p_cb_data) return Integer;
   pragma Convention (C, cb_rtn_type);

   type s_cb_data is record
      Reason : Integer;
      Cb_Rtn : cb_rtn_type;
      Obj : vpiHandle;
      Time : p_vpi_time;
      Value : p_vpi_value;
      Index : Integer;
      User_Data : Address;
   end record;
   pragma Convention (C, s_cb_data);

   -- vpiHandle  vpi_iterate(int type, vpiHandle ref)
   function vpi_iterate (aType : Integer; Ref : vpiHandle) return vpiHandle;
   pragma Export (C, vpi_iterate, "vpi_iterate");

   -- int vpi_get(int property, vpiHandle ref)
   function vpi_get (Property : Integer; Ref : vpiHandle) return Integer;
   pragma Export (C, vpi_get, "vpi_get");

   -- vpiHandle  vpi_scan(vpiHandle iter)
   function vpi_scan (Iter : vpiHandle) return vpiHandle;
   pragma Export (C, vpi_scan, "vpi_scan");

   -- char *vpi_get_str(int property, vpiHandle ref)
   function vpi_get_str (Property : Integer; Ref : vpiHandle)
                       return Ghdl_C_String;
   pragma Export (C, vpi_get_str, "vpi_get_str");

   -- vpiHandle  vpi_handle(int type, vpiHandle ref)
   function vpi_handle (aType: Integer; Ref: vpiHandle)
                       return vpiHandle;
   pragma Export (C, vpi_handle, "vpi_handle");

   -- void  vpi_get_value(vpiHandle expr, p_vpi_value value);
   procedure vpi_get_value (Expr : vpiHandle; Value : p_vpi_value);
   pragma Export (C, vpi_get_value, "vpi_get_value");

   -- void  vpi_get_time(vpiHandle obj, s_vpi_time*t);
   procedure vpi_get_time (Obj: vpiHandle; Time: p_vpi_time);
   pragma Export (C, vpi_get_time, "vpi_get_time");

   -- vpiHandle vpi_register_cb(p_cb_data data)
   function vpi_register_cb (Data : p_cb_data) return vpiHandle;
   pragma Export (C, vpi_register_cb, "vpi_register_cb");

-------------------------------------------------------------------------------
-- * * *   V P I   d u m m i e s   * * * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   -- int vpi_free_object(vpiHandle ref)
   function vpi_free_object(aRef: vpiHandle) return Integer;
   pragma Export (C, vpi_free_object, "vpi_free_object");

   type s_vpi_vlog_info is record
      Argc : Integer;
      Argv : System.Address;
      Product : Ghdl_C_String;
      Version : Ghdl_C_String;
   end record;
   pragma Convention (C, s_vpi_vlog_info);

   type p_vpi_vlog_info is access all s_vpi_vlog_info;
   pragma Convention (C, p_vpi_vlog_info);

   -- int vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p)
   function vpi_get_vlog_info(info : p_vpi_vlog_info) return Integer;
   pragma Export (C, vpi_get_vlog_info, "vpi_get_vlog_info");


   -- vpiHandle vpi_handle_by_index(vpiHandle ref, int index)
   function vpi_handle_by_index (Ref: vpiHandle; Index: Integer)
                                return vpiHandle;
   pragma Export (C, vpi_handle_by_index, "vpi_handle_by_index");

   function vpi_handle_by_name (Name : Ghdl_C_String; Scope : vpiHandle)
                               return vpiHandle;
   pragma Export (C, vpi_handle_by_name, "vpi_handle_by_name");

   -- unsigned int vpi_mcd_close(unsigned int mcd)
   function vpi_mcd_close (Mcd : Integer) return Integer;
   pragma Export (C, vpi_mcd_close, "vpi_mcd_close");

   -- char *vpi_mcd_name(unsigned int mcd)
   function vpi_mcd_name (Mcd : Integer) return Integer;
   pragma Export (C, vpi_mcd_name, "vpi_mcd_name");

   -- unsigned int vpi_mcd_open(char *name)
   function vpi_mcd_open (Name : Ghdl_C_String) return Integer;
   pragma Export (C, vpi_mcd_open, "vpi_mcd_open");

   -- vpiHandle vpi_put_value(vpiHandle obj, p_vpi_value value,
   --                         p_vpi_time when, int flags)
   function vpi_put_value (aObj : vpiHandle;
                           aValue : p_vpi_value;
                           aWhen : p_vpi_time;
                           aFlags : Integer)
                          return vpiHandle;
   pragma Export (C, vpi_put_value, "vpi_put_value");

   type t_vpi_systf_data is record
      mType: Integer;
      sysfunctype : Integer;
      tfname : Ghdl_C_String;
      calltf : Address;
      compiletf : Address;
      sizetf : Address;
      user_data : Address;
   end record;
   pragma Convention (C, t_vpi_systf_data);

   type p_vpi_systf_data is access all t_vpi_systf_data;
   pragma Convention (C, p_vpi_systf_data);

   -- vpiHandle vpi_register_systf(const struct t_vpi_systf_data*ss)
   function vpi_register_systf (Data : p_vpi_systf_data) return vpiHandle;
   pragma Export (C, vpi_register_systf, "vpi_register_systf");

   -- int vpi_remove_cb(vpiHandle ref)
   function vpi_remove_cb (Ref : vpiHandle) return Integer;
   pragma Export (C, vpi_remove_cb, "vpi_remove_cb");

   --  typedef struct t_vpi_error_info
   --  {
   --      int32_t state;
   --      int32_t level;
   --      char *message;
   --      char *product;
   --      char *code;
   --      char *file;
   --      int32_t line;
   --  } s_vpi_error_info, *p_vpi_error_info;
   type s_vpi_error_info is record
      State : Integer;
      Level : Integer;
      Message : Ghdl_C_String;
      Product : Ghdl_C_String;
      Code : Ghdl_C_String;
      File : Ghdl_C_String;
      Line : Integer;
   end record;
   type p_vpi_error_info is access all s_vpi_error_info;

   function vpi_chk_error (Info : p_vpi_error_info) return Integer;
   pragma Export (C, vpi_chk_error);

   function vpi_control_np (Op : Integer; Status : Integer) return Integer;
   pragma Export (C, vpi_control_np);

-------------------------------------------------------------------------------
-- * * *   G H D L   h o o k s   * * * * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   procedure Register;

private
   type struct_vpiHandle (mType : Integer) is record
      case mType is
         when vpiCallback =>
            Cb : aliased s_cb_data;
            Cb_Prev, Cb_Next : vpiHandle;
            Cb_Wire : Grt.Vcd.Verilog_Wire_Info;
            Cb_Handle : Callbacks.Callback_Handle;
            --  Number of reference to the handler by the simulation kernel.
            Cb_Refcnt : Natural;
         when others =>
            Ref : VhpiHandleT;
      end case;
   end record;
end Grt.Vpi;
