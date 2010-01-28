--  GHDL Run Time (GRT) - VPI interface.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold & Felix Bertram
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

-- Description: VPI interface for GRT runtime
--              the main purpose of this code is to interface with the
--              Icarus Verilog Interactive (IVI) simulator GUI

with System; use System;
with Ada.Unchecked_Conversion;
with Grt.Types; use Grt.Types;
with Grt.Avhpi; use Grt.Avhpi;

package Grt.Vpi is

   -- properties, see vpi_user.h
   vpiUndefined:     constant integer := -1;
   vpiType:          constant integer :=  1;
   vpiName:          constant integer :=  2;
   vpiFullName:      constant integer :=  3;
   vpiTimePrecision: constant integer := 12;

   -- object codes, see vpi_user.h
   vpiModule:        constant integer := 32;
   vpiNet:           constant integer := 36;
   vpiScope:         constant integer := 84;
   vpiInternalScope: constant integer := 92;
   vpiLeftRange:     constant integer := 79;
   vpiRightRange:    constant integer := 83;

   --  Additionnal constants.
   vpiCallback :     constant Integer := 200;

   -- codes for the format tag of the vpi_value structure
   vpiBinStrVal:     constant integer :=  1;
   vpiOctStrVal:     constant integer :=  2;
   vpiDecStrVal:     constant integer :=  3;
   vpiHexStrVal:     constant integer :=  4;
   vpiScalarVal:     constant integer :=  5;
   vpiIntVal:        constant integer :=  6;
   vpiRealVal:       constant integer :=  7;
   vpiStringVal:     constant integer :=  8;
   vpiVectorVal:     constant integer :=  9;
   vpiStrengthVal:   constant integer := 10;
   vpiTimeVal:       constant integer := 11;
   vpiObjTypeVal:    constant integer := 12;
   vpiSuppressVal:   constant integer := 13;

   -- codes for type tag of vpi_time structure
   vpiSimTime:       constant integer :=  2;

   -- codes for the reason tag of cb_data structure
   cbValueChange:    constant integer:= 1;
   cbReadOnlySynch:  constant integer:= 7;
   cbEndOfCompile:   constant integer:= 10;
   cbEndOfSimulation:constant integer:= 12;

   type struct_vpiHandle (mType : Integer := vpiUndefined);
   type vpiHandle is access struct_vpiHandle;

   -- typedef struct t_vpi_time {
   --   int type;
   --   unsigned int high;
   --   unsigned int low;
   --   double real;
   -- } s_vpi_time, *p_vpi_time;
   type s_vpi_time is record
      mType : Integer;
      mHigh : Integer; -- this should be unsigned
      mLow :  Integer; -- this should be unsigned
      mReal : Float;   -- this should be double
   end record;
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
   type s_vpi_value (Format : integer) is record
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
            --when vpiRealVal=>     null; -- what is the equivalent to double?
            --when vpiTimeVal=>     mTime:     p_vpi_time;
            --when vpiVectorVal=>   mVector:   p_vpi_vecval;
            --when vpiStrengthVal=> mStrength: p_vpi_strengthval;
         when others =>
            null;
         end case;
      end record;
   type p_vpi_value is access s_vpi_value;

   --typedef struct t_cb_data {
   --      int reason;
   --      int (*cb_rtn)(struct t_cb_data*cb);
   --      vpiHandle obj;
   --      p_vpi_time time;
   --      p_vpi_value value;
   --      int index;
   --      char*user_data;
   --} s_cb_data, *p_cb_data;
   type s_cb_data;

   type p_cb_data is access all s_cb_data;
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

   type struct_vpiHandle (mType : Integer := vpiUndefined) is record
      case mType is
         when vpiCallback =>
            Cb : p_cb_data;
         when others =>
            Ref   : VhpiHandleT;
      end case;
   end record;

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
   function vpi_handle (aType: integer; Ref: vpiHandle)
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
   function vpi_free_object(aRef: vpiHandle) return integer;
   pragma Export (C, vpi_free_object, "vpi_free_object");

   -- int vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p)
   function vpi_get_vlog_info(aVlog_info_p: System.Address) return integer;
   pragma Export (C, vpi_get_vlog_info, "vpi_get_vlog_info");

   -- vpiHandle vpi_handle_by_index(vpiHandle ref, int index)
   function vpi_handle_by_index(aRef: vpiHandle; aIndex: integer)
                               return vpiHandle;
   pragma Export (C, vpi_handle_by_index, "vpi_handle_by_index");

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
                           aFlags : integer)
                          return vpiHandle;
   pragma Export (C, vpi_put_value, "vpi_put_value");

   -- void vpi_register_systf(const struct t_vpi_systf_data*ss)
   procedure vpi_register_systf (aSs : Address);
   pragma Export (C, vpi_register_systf, "vpi_register_systf");

   -- int vpi_remove_cb(vpiHandle ref)
   function vpi_remove_cb (Ref : vpiHandle) return integer;
   pragma Export (C, vpi_remove_cb, "vpi_remove_cb");

   -- void vpi_vprintf(const char*fmt, va_list ap)
   procedure vpi_vprintf (Fmt: Address; Ap: Address);
   pragma Export (C, vpi_vprintf, "vpi_vprintf");

-------------------------------------------------------------------------------
-- * * *   G H D L   h o o k s   * * * * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   procedure Register;

end Grt.Vpi;

