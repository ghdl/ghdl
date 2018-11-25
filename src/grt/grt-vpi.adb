--  GHDL Run Time (GRT) - VPI interface.
--  Copyright (C) 2002 - 2014 Tristan Gingold & Felix Bertram
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

-------------------------------------------------------------------------------
-- TODO:
-------------------------------------------------------------------------------
-- DONE:
-- * The GHDL VPI implementation doesn't support time
--   callbacks (cbReadOnlySynch). This is needed to support
--   IVI run. Currently, the GHDL simulation runs until
--   complete once a single 'run' is performed...
-- * You are loading '_'-prefixed symbols when you
--   load the vpi plugin. On Linux, there is no leading
--   '_'. I just added code to try both '_'-prefixed and
--   non-'_'-prefixed symbols. I have placed the changed
--   file in the same download dir as the snapshot
-- * I did find out why restart doesn't work for GHDL.
--   You are passing back the leaf name of signals when the
--   FullName is requested.
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Stdio; use Grt.Stdio;
with Grt.C; use Grt.C;
with Grt.Signals; use Grt.Signals;
with Grt.Astdio; use Grt.Astdio;
with Grt.Strings; use Grt.Strings;
with Grt.Hooks; use Grt.Hooks;
with Grt.Options;
with Grt.Vcd; use Grt.Vcd;
with Grt.Errors; use Grt.Errors;
with Grt.Rtis_Types;
with Grt.Std_Logic_1164; use Grt.Std_Logic_1164;
with Grt.Callbacks; use Grt.Callbacks;
with Grt.Vstrings; use Grt.Vstrings;

package body Grt.Vpi is
   --  The VPI interface requires libdl (dlopen, dlsym) to be linked in.
   --  This is now set in Makefile, since this is target dependent.
   --  pragma Linker_Options ("-ldl");

   --errAnyString:     constant String := "grt-vcd.adb: any string" & NUL;
   --errNoString:      constant String := "grt-vcd.adb: no string" & NUL;

   Product : constant String := "GHDL" & NUL;
   Version : constant String := "0.1" & NUL;

   --  If true, emit traces
   Flag_Trace : Boolean := False;
   Trace_File : FILEs;
   Trace_Indent : Natural := 0;

-------------------------------------------------------------------------------
-- * * *   h e l p e r s   * * * * * * * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   ------------------------------------------------------------------------
   -- debugging helpers
   procedure dbgPut (Str : String)
   is
      S : size_t;
      pragma Unreferenced (S);
   begin
      S := fwrite (Str'Address, Str'Length, 1, stderr);
   end dbgPut;

   procedure dbgPut (C : Character)
   is
      R : int;
      pragma Unreferenced (R);
   begin
      R := fputc (Character'Pos (C), stderr);
   end dbgPut;

   procedure dbgNew_Line is
   begin
      dbgPut (Nl);
   end dbgNew_Line;

   procedure dbgPut_Line (Str : String)
   is
   begin
      dbgPut (Str);
      dbgNew_Line;
   end dbgPut_Line;

--    procedure dbgPut_Line (Str : Ghdl_Str_Len_Type)
--    is
--    begin
--       Put_Str_Len(stderr, Str);
--       dbgNew_Line;
--    end dbgPut_Line;

   procedure Free is new Ada.Unchecked_Deallocation
     (Name => vpiHandle, Object => struct_vpiHandle);

   ------------------------------------------------------------------------
   -- NUL-terminate strings.
   -- note: there are several buffers
   -- see IEEE 1364-2001
--   tmpstring1: string(1..1024);
--    function NulTerminate1 (Str : Ghdl_Str_Len_Type) return Ghdl_C_String
--    is
--    begin
--       for i in 1..Str.Len loop
--          tmpstring1(i):= Str.Str(i);
--       end loop;
--       tmpstring1(Str.Len+1):= NUL;
--       return To_Ghdl_C_String (tmpstring1'Address);
--    end NulTerminate1;

   --  Clear error status.
   procedure Reset_Error;

   procedure Trace_Start (Msg : String) is
   begin
      for I in 1 .. Trace_Indent loop
         Put (Trace_File, ' ');
      end loop;
      Put (Trace_File, Msg);
   end Trace_Start;

   procedure Trace (Msg : String) is
   begin
      Put (Trace_File, Msg);
   end Trace;

   procedure Trace (V : Integer) is
   begin
      Put_I32 (Trace_File, Ghdl_I32 (V));
   end Trace;

   procedure Trace_Cb_Reason (V : Integer) is
   begin
      case V is
         when cbValueChange =>
            Trace ("cbValueChange");
         when cbReadWriteSynch =>
            Trace ("cbReadWriteSynch");
         when cbReadOnlySynch =>
            Trace ("cbReadOnlySynch");
         when cbNextSimTime =>
            Trace ("cbNextSimTime");
         when cbAfterDelay =>
            Trace ("cbAfterDelay");
         when cbEndOfCompile =>
            Trace ("cbEndOfCompile");
         when cbStartOfSimulation =>
            Trace ("cbStartOfSimulation");
         when cbEndOfSimulation =>
            Trace ("cbEndOfSimulation");
         when others =>
            Trace (V);
      end case;
   end Trace_Cb_Reason;

   procedure Trace_Property (V : Integer) is
   begin
      case V is
         when vpiUndefined =>
            Trace ("vpiUndefined");
         when vpiType =>
            Trace ("vpiType");
         when vpiName =>
            Trace ("vpiName");
         when vpiFullName =>
            Trace ("vpiFullName");
         when vpiSize =>
            Trace ("vpiSize");
         when vpiTimePrecision =>
            Trace ("vpiTimePrecision");
         when vpiScalar =>
            Trace ("vpiScalar");
         when vpiVector =>
            Trace ("vpiVector");

         when vpiModule =>
            Trace ("vpiModule");
         when vpiNet =>
            Trace ("vpiNet");
         when vpiParameter =>
            Trace ("vpiParameter");
         when vpiScope =>
            Trace ("vpiScope");
         when vpiInternalScope =>
            Trace ("vpiInternalScope");
         when vpiLeftRange =>
            Trace ("vpiLeftRange");
         when vpiRightRange =>
            Trace ("vpiRightRange");

         when vpiStop =>
            Trace ("vpiStop");
         when vpiFinish =>
            Trace ("vpiFinish");
         when vpiReset =>
            Trace ("vpiReset");

         when others =>
            Trace (V);
      end case;
   end Trace_Property;

   procedure Trace_Format (F : Integer) is
   begin
      case F is
         when vpiBinStrVal =>
            Trace ("BinStr");
         when vpiOctStrVal =>
            Trace ("OctStr");
         when vpiDecStrVal =>
            Trace ("DecStr");
         when vpiHexStrVal =>
            Trace ("HexStr");
         when vpiScalarVal =>
            Trace ("Scalar");
         when vpiIntVal =>
            Trace ("Int");
         when vpiRealVal =>
            Trace ("Real");
         when vpiStringVal =>
            Trace ("String");
         when vpiVectorVal =>
            Trace ("Vector");
         when vpiStrengthVal =>
            Trace ("Strength");
         when vpiTimeVal =>
            Trace ("Time");
         when vpiObjTypeVal =>
            Trace ("ObjType");
         when vpiSuppressVal =>
            Trace ("Suppress");

         when others =>
            Trace (F);
      end case;
   end Trace_Format;

   procedure Trace_Time_Tag (V : Integer) is
   begin
      case V is
         when vpiSimTime =>
            Trace ("vpiSimTime");
         when others =>
            Trace (V);
      end case;
   end Trace_Time_Tag;

   procedure Trace (H : vpiHandle)
   is
      function To_Address is
         new Ada.Unchecked_Conversion (vpiHandle, System.Address);
   begin
      Put (Trace_File, To_Address (H));
   end Trace;

   procedure Trace (Str : Ghdl_C_String) is
   begin
      if Str = null then
         Put (Trace_File, "null");
      else
         Put (Trace_File, '"');
         Put (Trace_File, Str);
         Put (Trace_File, '"');
      end if;
   end Trace;

   procedure Trace_Time (V : Std_Time) is
   begin
      Put_Time (Trace_File, V);
   end Trace_Time;

   procedure Trace_Value (V : p_vpi_value) is
   begin
      case V.Format is
         when vpiBinStrVal
           | vpiOctStrVal
           | vpiDecStrVal
           | vpiHexStrVal
           | vpiStringVal =>
            Trace (V.Str);
         when vpiScalarVal =>
            Trace (V.Scalar);
         when vpiIntVal =>
            Trace (V.Integer_m);
            --when vpiRealVal=>     null; -- what is the equivalent to double?
            --when vpiTimeVal=>     mTime:     p_vpi_time;
            --when vpiVectorVal=>   mVector:   p_vpi_vecval;
            --when vpiStrengthVal=> mStrength: p_vpi_strengthval;
         when others =>
            null;
      end case;
   end Trace_Value;

   procedure Trace_Newline is
   begin
      New_Line (Trace_File);
   end Trace_Newline;

   function Vpi_Time_To_Time (V : s_vpi_time) return Std_Time is
      Res : Std_Time;
   begin
      if V.mType /= vpiSimTime then
         raise Program_Error;
      end if;
      Res := Std_Time (Unsigned_64 (V.mHigh) * 2 ** 32 + Unsigned_64 (V.mLow));
      return Res;
   end Vpi_Time_To_Time;

-------------------------------------------------------------------------------
-- * * *   V P I   f u n c t i o n s   * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   ------------------------------------------------------------------------
   -- vpiHandle  vpi_iterate(int type, vpiHandle ref)
   -- Obtain an iterator handle to objects with a one-to-many relationship.
   -- see IEEE 1364-2001, page 685
   function Vpi_Iterate_Internal
     (aType: integer; Ref: vpiHandle) return vpiHandle
   is
      Res : vpiHandle;
      Rel : VhpiOneToManyT;
      Error : AvhpiErrorT;
   begin
      case aType is
         when vpiNet =>
            Rel := VhpiDecls;
         when vpiModule =>
            if Ref = null then
               Res := new struct_vpiHandle (vpiModule);
               Get_Root_Inst (Res.Ref);
               return Res;
            else
               Rel := VhpiInternalRegions;
            end if;
         when vpiInternalScope =>
            Rel := VhpiInternalRegions;
         when others =>
            return null;
      end case;

      -- find the proper start object for our scan
      if Ref = null then
         Res := null;
      else
         Res := new struct_vpiHandle (aType);
         Vhpi_Iterator (Rel, Ref.Ref, Res.Ref, Error);

         if Error /= AvhpiErrorOk then
            Free (Res);
         end if;
      end if;

      return Res;
   end Vpi_Iterate_Internal;

   function vpi_iterate (aType: integer; Ref: vpiHandle) return vpiHandle
   is
      Res : vpiHandle;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_iterate (");
         Trace_Property (aType);
         Trace (", ");
         Trace (Ref);
         Trace (") = ");
      end if;

      Res := Vpi_Iterate_Internal (aType, Ref);

      if Flag_Trace then
         Trace (Res);
         Trace_Newline;
      end if;

      return Res;
   end vpi_iterate;

   ------------------------------------------------------------------------
   -- int vpi_get(int property, vpiHandle ref)
   -- Get the value of an integer or boolean property of an object.
   -- see IEEE 1364-2001, chapter 27.6, page 667
--    function ii_vpi_get_type (aRef: Ghdl_Instance_Name_Acc) return Integer
--    is
--    begin
--       case aRef.Kind is
--          when Ghdl_Name_Entity
--            | Ghdl_Name_Architecture
--            | Ghdl_Name_Block
--            | Ghdl_Name_Generate_Iterative
--            | Ghdl_Name_Generate_Conditional
--            | Ghdl_Name_Instance =>
--             return vpiModule;
--          when Ghdl_Name_Signal =>
--             return vpiNet;
--          when others =>
--             return vpiUndefined;
--       end case;
--    end ii_vpi_get_type;

   function Vpi_Get_Size (Ref : vpiHandle) return Integer
   is
      Info : Verilog_Wire_Info;
   begin
      Get_Verilog_Wire (Ref.Ref, Info);
      case Info.Vtype is
         when Vcd_Var_Vectors =>
            return Natural (Get_Wire_Length (Info));
         when Vcd_Bool
           | Vcd_Bit
           | Vcd_Stdlogic =>
            return 1;
         when Vcd_Integer32 =>
            return 32;
         when Vcd_Enum8 =>
            return 8;
         when Vcd_Float64 =>
            return 0;
         when Vcd_Bad =>
            return 0;
      end case;
   end Vpi_Get_Size;

   function Vpi_Get_Vector (Ref : vpiHandle) return Boolean
   is
      Info : Verilog_Wire_Info;
   begin
      Get_Verilog_Wire (Ref.Ref, Info);
      case Info.Vtype is
         when Vcd_Bool
           | Vcd_Integer32
           | Vcd_Float64
           | Vcd_Bit
           | Vcd_Stdlogic
           | Vcd_Enum8 =>
            return False;
         when Vcd_Bitvector
           | Vcd_Stdlogic_Vector =>
            return True;
         when Vcd_Bad =>
            return False;
      end case;
   end Vpi_Get_Vector;

   function vpi_get (Property: integer; Ref: vpiHandle) return Integer
   is
      Res : Integer;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_get (");
         Trace_Property (Property);
         Trace (", ");
         Trace (Ref);
         Trace (") = ");
      end if;

      case Property is
         when vpiType =>
            Res := Ref.mType;
         when vpiTimePrecision =>
            Res := -3 * Options.Time_Resolution_Scale;
         when vpiSize =>
            Res := Vpi_Get_Size (Ref);
         when vpiVector =>
            Res := Boolean'Pos (Vpi_Get_Vector (Ref));
         when others =>
            dbgPut_Line ("vpi_get: unknown property");
            Res := 0;
      end case;

      if Flag_Trace then
         case Property is
            when vpiType =>
               Trace_Property (Res);
            when others =>
               Trace (Res);
         end case;
         Trace_Newline;
      end if;

      return Res;
   end vpi_get;

   function Vhpi_Handle_To_Vpi_Prop (Res : VhpiHandleT) return Integer is
   begin
      case Vhpi_Get_Kind (Res) is
         when VhpiEntityDeclK
           | VhpiArchBodyK
           | VhpiBlockStmtK
           | VhpiIfGenerateK
           | VhpiForGenerateK
           | VhpiCompInstStmtK =>
            return vpiModule;
         when VhpiPortDeclK
           | VhpiSigDeclK =>
            declare
               Info : Verilog_Wire_Info;
            begin
               Get_Verilog_Wire (Res, Info);
               if Info.Vtype /= Vcd_Bad then
                  return vpiNet;
               end if;
            end;
         when VhpiGenericDeclK =>
            declare
               Info : Verilog_Wire_Info;
            begin
               Get_Verilog_Wire (Res, Info);
               if Info.Vtype /= Vcd_Bad then
                  return vpiParameter;
               end if;
            end;
         when others =>
            null;
      end case;
      return vpiUndefined;
   end Vhpi_Handle_To_Vpi_Prop;

   function Build_vpiHandle (Res : VhpiHandleT; Prop : Integer)
                            return vpiHandle is
   begin
      case Prop is
         when vpiModule =>
            return new struct_vpiHandle'(mType => vpiModule,
                                         Ref => Res);
         when vpiNet =>
            return new struct_vpiHandle'(mType => vpiNet,
                                         Ref => Res);
         when vpiParameter =>
            return new struct_vpiHandle'(mType => vpiParameter,
                                         Ref => Res);
         when others =>
            return null;
      end case;
   end Build_vpiHandle;

   ------------------------------------------------------------------------
   -- vpiHandle  vpi_scan(vpiHandle iter)
   -- Scan the Verilog HDL hierarchy for objects with a one-to-many
   -- relationship.
   -- see IEEE 1364-2001, chapter 27.36, page 709
   function Vpi_Scan_Internal (Iter: vpiHandle) return vpiHandle
   is
      Res : VhpiHandleT;
      Error : AvhpiErrorT;
      R : vpiHandle;
      Kind, Expected_Kind : Integer;
   begin
      if Iter = null then
         return null;
      end if;

      --  There is only one top-level module.
      if Iter.mType = vpiModule then
         case Vhpi_Get_Kind (Iter.Ref) is
            when VhpiRootInstK =>
               R := new struct_vpiHandle (Iter.mType);
               R.Ref := Iter.Ref;
               Iter.Ref := Null_Handle;
               return R;
            when VhpiUndefined =>
               return null;
            when others =>
               --  Fall through.
               null;
         end case;
      end if;

      case Iter.mType is
         when vpiInternalScope
           | vpiModule =>
            Expected_Kind := vpiModule;
         when vpiNet =>
            Expected_Kind := vpiNet;
         when others =>
            Expected_Kind := vpiUndefined;
      end case;

      loop
         Vhpi_Scan (Iter.Ref, Res, Error);
         exit when Error /= AvhpiErrorOk;

         Kind := Vhpi_Handle_To_Vpi_Prop (Res);
         if Kind /= vpiUndefined and then Kind = Expected_Kind then
            return Build_vpiHandle (Res, Kind);
         end if;
      end loop;
      return null;
   end Vpi_Scan_Internal;

   function vpi_scan (Iter: vpiHandle) return vpiHandle
   is
      Res : vpiHandle;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_scan (");
         Trace (Iter);
         Trace (") = ");
      end if;

      Res := Vpi_Scan_Internal (Iter);

      if Flag_Trace then
         Trace (Res);
         Trace_Newline;
      end if;

      return Res;
   end vpi_scan;

   ------------------------------------------------------------------------
   -- char *vpi_get_str(int property, vpiHandle ref)
   -- see IEEE 1364-2001, page xxx
   Tmpstring2 : String (1 .. 1024);
   function Vpi_Get_Str_Internal (Property : Integer; Ref : vpiHandle)
                                 return Ghdl_C_String
   is
      Prop : VhpiStrPropertyT;
      Len : Natural;
      Res : Ghdl_C_String;
   begin
      if Ref = null then
         return null;
      end if;

      case Property is
         when vpiFullName =>
            Prop := VhpiFullNameP;
         when vpiName =>
            Prop := VhpiNameP;
         when vpiType =>
            Tmpstring2 (1 .. 4) := "???" & NUL;
            return To_Ghdl_C_String (Tmpstring2'Address);
         when others =>
            dbgPut_Line ("vpi_get_str: unhandled property");
            return null;
      end case;
      Vhpi_Get_Str (Prop, Ref.Ref, Tmpstring2, Len);
      Tmpstring2 (Len + 1) := NUL;
      if Property = vpiFullName then
         for I in Tmpstring2'First .. Len loop
            if Tmpstring2 (I) = ':' then
               Tmpstring2 (I) := '.';
            end if;
         end loop;
         --  Remove the initial '.'.
         Res := To_Ghdl_C_String (Tmpstring2 (2)'Address);
      else
         Res := To_Ghdl_C_String (Tmpstring2'Address);
      end if;

      return Res;
   end Vpi_Get_Str_Internal;

   function vpi_get_str (Property : Integer; Ref : vpiHandle)
                        return Ghdl_C_String
   is
      Res : Ghdl_C_String;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_get_str (");
         Trace_Property (Property);
         Trace (", ");
         Trace (Ref);
         Trace (") = ");
      end if;

      Res := Vpi_Get_Str_Internal (Property, Ref);

      if Flag_Trace then
         Trace (Res);
         Trace_Newline;
      end if;

      return Res;
   end vpi_get_str;
   ------------------------------------------------------------------------
   -- vpiHandle  vpi_handle(int type, vpiHandle ref)
   -- Obtain a handle to an object with a one-to-one relationship.
   -- see IEEE 1364-2001, chapter 27.16, page 682
   function Vpi_Handle_Internal
     (aType : Integer; Ref : vpiHandle) return vpiHandle
   is
      Res : vpiHandle;
   begin
      if Ref = null then
         return null;
      end if;

      case aType is
         when vpiScope =>
            case Ref.mType is
               when vpiModule =>
                  Res := new struct_vpiHandle (vpiScope);
                  Res.Ref := Ref.Ref;
                  return Res;
               when others =>
                  return null;
            end case;
         when vpiRightRange
           | vpiLeftRange =>
            case Ref.mType is
               when vpiNet =>
                  Res := new struct_vpiHandle (aType);
                  Res.Ref := Ref.Ref;
                  return Res;
               when others =>
                  return null;
            end case;
         when others =>
            return null;
      end case;
   end Vpi_Handle_Internal;

   function vpi_handle (aType : Integer; Ref : vpiHandle) return vpiHandle
   is
      Res : vpiHandle;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_handle (");
         Trace_Property (aType);
         Trace (", ");
         Trace (Ref);
         Trace (") = ");
      end if;

      Res := Vpi_Handle_Internal (aType, Ref);

      if Flag_Trace then
         Trace (Res);
         Trace_Newline;
      end if;

      return Res;
   end vpi_handle;

   ------------------------------------------------------------------------
   -- void  vpi_get_value(vpiHandle expr, p_vpi_value value);
   -- Retrieve the simulation value of an object.
   -- see IEEE 1364-2001, chapter 27.14, page 675
   Buf_Value : Vstring;

   type Map_Type_E8 is array (Ghdl_E8 range 0..8) of character;
   Map_Std_E8: constant Map_Type_E8 := "UX01ZWLH-";

   type Map_Type_B1 is array (Ghdl_B1) of character;
   Map_Std_B1: constant Map_Type_B1 := "01";

   function ii_vpi_get_value_bin_str (Obj : VhpiHandleT)
                                     return Ghdl_C_String
   is
      function E8_To_Char (Val : Ghdl_E8) return Character is
      begin
         if Val not in Map_Type_E8'range then
            return '?';
         else
            return Map_Std_E8 (Val);
         end if;
      end E8_To_Char;

      Info : Verilog_Wire_Info;
      Len : Ghdl_Index_Type;
   begin
      case Vhpi_Get_Kind (Obj) is
         when VhpiPortDeclK
           | VhpiSigDeclK
           | VhpiGenericDeclK =>
            null;
         when others =>
            return null;
      end case;

      --  Get verilog compat info.
      Get_Verilog_Wire (Obj, Info);
      if Info.Vtype = Vcd_Bad then
         return null;
      end if;

      Len := Get_Wire_Length (Info);

      Reset (Buf_Value); -- reset string buffer

      case Info.Vtype is
         when Vcd_Bad
           | Vcd_Enum8
           | Vcd_Float64 =>
            return null;
         when Vcd_Integer32 =>
            declare
               V : Ghdl_U32;
            begin
               V := Verilog_Wire_Val (Info).E32;
               for I in 0 .. 31 loop
                  if (V and 16#8000_0000#) /= 0 then
                     Append (Buf_Value, '1');
                  else
                     Append (Buf_Value, '0');
                  end if;
                  V := Shift_Left (V, 1);
               end loop;
            end;
         when Vcd_Bit
           | Vcd_Bool
           | Vcd_Bitvector =>
            for J in 0 .. Len - 1 loop
               Append (Buf_Value, Map_Std_B1 (Verilog_Wire_Val (Info, J).B1));
            end loop;
         when Vcd_Stdlogic
           | Vcd_Stdlogic_Vector =>
            for J in 0 .. Len - 1 loop
               Append (Buf_Value, E8_To_Char (Verilog_Wire_Val (Info, J).E8));
            end loop;
      end case;
      Append (Buf_Value, NUL);
      return Get_C_String (Buf_Value);
   end ii_vpi_get_value_bin_str;

   procedure vpi_get_value (Expr : vpiHandle; Value : p_vpi_value) is
   begin
      if Flag_Trace then
         Trace_Start ("vpi_get_value (");
         Trace (Expr);
         Trace (", {format=");
         Trace_Format (Value.Format);
         Trace ("}) = ");
      end if;

      case Value.Format is
         when vpiObjTypeVal=>
            -- fill in the object type and value:
            -- For an integer, vpiIntVal
            -- For a real, vpiRealVal
            -- For a scalar, either vpiScalar or vpiStrength
            -- For a time variable, vpiTimeVal with vpiSimTime
            -- For a vector, vpiVectorVal
            dbgPut_Line ("vpi_get_value: vpiObjTypeVal");
         when vpiBinStrVal=>
            Value.Str := ii_vpi_get_value_bin_str (Expr.Ref);
            --aValue.mStr := NulTerminate2(aExpr.mRef.Name.all);
         when vpiOctStrVal=>
            dbgPut_Line("vpi_get_value: vpiNet, vpiOctStrVal");
         when vpiDecStrVal=>
            dbgPut_Line("vpi_get_value: vpiNet, vpiDecStrVal");
         when vpiHexStrVal=>
            dbgPut_Line("vpi_get_value: vpiNet, vpiHexStrVal");
         when vpiScalarVal=>
            dbgPut_Line("vpi_get_value: vpiNet, vpiScalarVal");
         when vpiIntVal=>
            case Expr.mType is
               when vpiLeftRange
                 | vpiRightRange=>
                  declare
                     Info : Verilog_Wire_Info;
                  begin
                     Get_Verilog_Wire (Expr.Ref, Info);
                     if Info.Irange /= null then
                        if Expr.mType = vpiLeftRange then
                           Value.Integer_m := Integer (Info.Irange.I32.Left);
                        else
                           Value.Integer_m := Integer (Info.Irange.I32.Right);
                        end if;
                     else
                        Value.Integer_m  := 0;
                     end if;
                  end;
               when others=>
                  dbgPut_Line ("vpi_get_value: vpiIntVal, unknown mType");
            end case;
         when vpiRealVal=>     dbgPut_Line("vpi_get_value: vpiRealVal");
         when vpiStringVal=>   dbgPut_Line("vpi_get_value: vpiStringVal");
         when vpiTimeVal=>     dbgPut_Line("vpi_get_value: vpiTimeVal");
         when vpiVectorVal=>   dbgPut_Line("vpi_get_value: vpiVectorVal");
         when vpiStrengthVal=> dbgPut_Line("vpi_get_value: vpiStrengthVal");
         when others=>         dbgPut_Line("vpi_get_value: unknown mFormat");
      end case;

      if Flag_Trace then
         Trace_Value (Value);
         Trace_Newline;
      end if;
   end vpi_get_value;

   ------------------------------------------------------------------------
   -- void  vpiHandle vpi_put_value(vpiHandle obj, p_vpi_value value,
   --                               p_vpi_time when, int flags)
   -- Alter the simulation value of an object.
   -- see IEEE 1364-2001, chapter 27.14, page 675
   -- FIXME
   type Std_Ulogic_Array is array (Ghdl_Index_Type range <>) of Std_Ulogic;

   procedure Ii_Vpi_Put_Value (Info : Verilog_Wire_Info;
                               Vec : Std_Ulogic_Array) is
   begin
      case Info.Vtype is
         when Vcd_Bad =>
            return;
         when Vcd_Bit
           | Vcd_Bool
           | Vcd_Bitvector =>
            for J in Vec'Range loop
               declare
                  V : constant Ghdl_B1 :=
                    Ghdl_B1 (Vec (J) = '1' or Vec (J) = 'H');
               begin
                  case Info.Val is
                     when Vcd_Effective | Vcd_Driving =>
                        --  Force_Driving sets both the driving and the
                        --  effective value.
                        Ghdl_Signal_Force_Driving_B1
                          (To_Signal_Arr_Ptr (Info.Ptr)(J), V);
                     when Vcd_Variable =>
                        Verilog_Wire_Val (Info, J).B1 := V;
                  end case;
               end;
            end loop;
         when Vcd_Stdlogic
           | Vcd_Stdlogic_Vector =>
            for J in Vec'Range loop
               declare
                  V : constant Ghdl_E8 := Std_Ulogic'Pos (Vec (J));
               begin
                  case Info.Val is
                     when Vcd_Effective | Vcd_Driving =>
                        --  Force_Driving sets both the driving and the
                        --  effective value.
                        Ghdl_Signal_Force_Driving_E8
                          (To_Signal_Arr_Ptr (Info.Ptr)(J), V);
                     when Vcd_Variable =>
                        Verilog_Wire_Val (Info, J).E8 := V;
                  end case;
               end;
            end loop;
         when Vcd_Enum8 =>
            null;
         when Vcd_Integer32
           | Vcd_Float64 =>
            null;
      end case;
   end Ii_Vpi_Put_Value;

   procedure Ii_Vpi_Put_Value_Int (Info : Verilog_Wire_Info;
                                   Len  : Ghdl_Index_Type;
                                   Val : Unsigned_32)
   is
      V : Unsigned_32;
      Vec : Std_Ulogic_Array (0 .. Len - 1);
   begin
      V := Val;
      for J in reverse 0 .. Len - 1 loop
         if (V mod 2) = 0 then
            Vec (J) := '0';
         else
            Vec (J) := '1';
         end if;
         V := Shift_Right_Arithmetic (V, 1);
      end loop;
      Ii_Vpi_Put_Value (Info, Vec);
   end Ii_Vpi_Put_Value_Int;

   procedure Ii_Vpi_Put_Value_Bin_Str (Info : Verilog_Wire_Info;
                                       Len : Ghdl_Index_Type;
                                       Str : Ghdl_C_String)
   is
      Slen : constant Natural := strlen (Str);
      Soff : Integer;
      Vec : Std_Ulogic_Array (0 .. Len - 1);
      V : Std_Ulogic;
   begin
      Soff := Slen;
      for J in reverse 0 .. Len - 1 loop
         Soff := Soff - 1;
         if Soff >= 0 then
            case Str (Str'First + Soff) is
               when 'u' | 'U' => V := 'U';
               when 'x' | 'X' => V := 'X';
               when '0'       => V := '0';
               when '1'       => V := '1';
               when 'z' | 'Z' => V := 'Z';
               when 'w' | 'W' => V := 'W';
               when 'l' | 'L' => V := 'L';
               when 'h' | 'H' => V := 'H';
               when '-'       => V := '-';
               when others    => V := 'U';
            end case;
         else
            V := '0';
         end if;
         Vec (J) := V;
      end loop;
      Ii_Vpi_Put_Value (Info, Vec);
   end Ii_Vpi_Put_Value_Bin_Str;

   -- vpiHandle vpi_put_value(vpiHandle obj, p_vpi_value value,
   --                         p_vpi_time when, int flags)
   function vpi_put_value (aObj : vpiHandle;
                           aValue : p_vpi_value;
                           aWhen : p_vpi_time;
                           aFlags : integer)
                         return vpiHandle
   is
      pragma Unreferenced (aWhen);
      pragma Unreferenced (aFlags);

      function To_Unsigned_32 is new Ada.Unchecked_Conversion
        (Integer, Unsigned_32);
      Info : Verilog_Wire_Info;
      Len  : Ghdl_Index_Type;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_put_value (");
         Trace (aObj);
         Trace (", ");
         Trace_Value (aValue);
         Trace (")");
         Trace_Newline;
      end if;

      Reset_Error;

      -- A very simple write procedure for VPI.
      -- Basically, it accepts bin_str values and converts to appropriate
      -- types (only std_logic and bit values and vectors).

      -- It'll use Set_Effective_Value procedure to update signals

      -- Ignoring aWhen and aFlags, for now.

      -- Check the Obj type.
      -- * The vpiHandle has a reference (field Ref) to a VhpiHandleT
      --   when it doesnt come from a callback.
      case Vhpi_Get_Kind (aObj.Ref) is
         when VhpiPortDeclK
           | VhpiSigDeclK =>
            null;
         when others =>
            return null;
      end case;

      -- The following code segment was copied from the
      -- ii_vpi_get_value function.
      --  Get verilog compat info.
      Get_Verilog_Wire (aObj.Ref, Info);
      if Info.Vtype = Vcd_Bad then
         return null;
      end if;

      Len := Get_Wire_Length (Info);
      if Len = 0 then
         --  No signal.
         return null;
      end if;

      -- Step 1: convert vpi object to internal format.
      --         p_vpi_handle -> Ghdl_Signal_Ptr
      --         To_Signal_Arr_Ptr (Info.Addr) does part of the magic

      -- Step 2: convert datum to appropriate type.
      --         Ghdl_C_String -> Value_Union

      -- Step 3: assigns value to object using Set_Effective_Value
      --         call (from grt-signals)
      -- Set_Effective_Value(sig_ptr, conv_value);

      -- Checks the format of aValue. Only vpiBinStrVal will be accepted
      --  for now.
      case aValue.Format is
         when vpiObjTypeVal =>
            dbgPut_Line ("vpi_put_value: vpiObjTypeVal");
         when vpiBinStrVal =>
            Ii_Vpi_Put_Value_Bin_Str (Info, Len, aValue.Str);
            -- dbgPut_Line ("vpi_put_value: vpiBinStrVal");
         when vpiOctStrVal =>
            dbgPut_Line ("vpi_put_value: vpiNet, vpiOctStrVal");
         when vpiDecStrVal =>
            dbgPut_Line ("vpi_put_value: vpiNet, vpiDecStrVal");
         when vpiHexStrVal =>
            dbgPut_Line ("vpi_put_value: vpiNet, vpiHexStrVal");
         when vpiScalarVal =>
            dbgPut_Line ("vpi_put_value: vpiNet, vpiScalarVal");
         when vpiIntVal =>
            Ii_Vpi_Put_Value_Int
              (Info, Len, To_Unsigned_32 (aValue.Integer_m));
            -- dbgPut_Line ("vpi_put_value: vpiIntVal");
         when vpiRealVal =>
            dbgPut_Line("vpi_put_value: vpiRealVal");
         when vpiStringVal =>
            dbgPut_Line("vpi_put_value: vpiStringVal");
         when vpiTimeVal =>
            dbgPut_Line("vpi_put_value: vpiTimeVal");
         when vpiVectorVal =>
            dbgPut_Line("vpi_put_value: vpiVectorVal");
         when vpiStrengthVal =>
            dbgPut_Line("vpi_put_value: vpiStrengthVal");
         when others =>
            dbgPut_Line("vpi_put_value: unknown mFormat");
      end case;

      -- Must return a scheduled event caused by vpi_put_value()
      -- Still dont know how to do it.
      return null;
   end vpi_put_value;

   ------------------------------------------------------------------------
   -- void  vpi_get_time(vpiHandle obj, s_vpi_time*t);
   -- see IEEE 1364-2001, page xxx
   procedure vpi_get_time (Obj: vpiHandle; Time: p_vpi_time)
   is
      function To_Unsigned_64 is
         new Ada.Unchecked_Conversion (Std_Time, Unsigned_64);
      Res : Std_Time;
      V : Unsigned_64;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_get_time (");
         Trace (Obj);
         Trace (", {mtype=");
         Trace_Time_Tag (Time.mType);
         Trace ("}) = ");
      end if;

      if Obj /= null
        or else Time.mType /= vpiSimTime
      then
         dbgPut_Line ("vpi_get_time: unhandled");
         return;
      end if;

      Res := Current_Time;

      V := To_Unsigned_64 (Res);
      Time.mHigh := Unsigned_32 (V / 2 ** 32);
      Time.mLow  := Unsigned_32 (V mod 2 ** 32);
      Time.mReal := 0.0;

      if Flag_Trace then
         Trace_Time (Res);
         Trace_Newline;
      end if;
   end vpi_get_time;

   ------------------------------------------------------------------------

   type Callback_List is record
      First, Last : vpiHandle;
   end record;

   procedure Append_Callback (List : in out Callback_List; Hand : vpiHandle) is
   begin
      if List.First = null then
         List.First := Hand;
      else
         List.Last.Cb_Next := Hand;
         Hand.Cb_Prev := List.Last;
      end if;
      List.Last := Hand;
      Hand.Cb_Next := null;
   end Append_Callback;

   procedure Execute_Callback (Hand : vpiHandle)
   is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      if Flag_Trace then
         Trace_Start ("vpi call callback ");
         Trace (Hand);
         Trace (" ");
         Trace_Cb_Reason (Hand.Cb.Reason);
         Trace_Newline;
         Trace_Indent := Trace_Indent + 1;
      end if;
      Res := Hand.Cb.Cb_Rtn (Hand.Cb'Access);
      if Flag_Trace then
         Trace_Indent := Trace_Indent - 1;
         Trace_Start ("vpi end callback ");
         Trace (Hand);
         Trace_Newline;
      end if;
   end Execute_Callback;

   procedure Execute_Callback_List (List : Callback_List)
   is
      H, Next_H : vpiHandle;
   begin
      H := List.First;
      while H /= null loop
         Next_H := H.Cb_Next;
         --  The callback may destroy h.
         Execute_Callback (H);
         H := Next_H;
      end loop;
   end Execute_Callback_List;

   -- vpiHandle vpi_register_cb(p_cb_data data)
   g_cbEndOfCompile      : Callback_List;
   g_cbStartOfSimulation : Callback_List;
   g_cbEndOfSimulation   : Callback_List;

   function To_Address is new Ada.Unchecked_Conversion
     (vpiHandle, System.Address);

   function To_vpiHandle is new Ada.Unchecked_Conversion
     (System.Address, vpiHandle);

   --  Wrapper
   procedure Call_Callback (Arg : System.Address)
   is
      Hand : constant vpiHandle := To_vpiHandle (Arg);
   begin
      Execute_Callback (Hand);
   end Call_Callback;

   procedure Call_Valuechange_Callback (Arg : System.Address)
   is
      Hand : constant vpiHandle := To_vpiHandle (Arg);
   begin
      if Verilog_Wire_Event (Hand.Cb_Wire) then
         --  Note: the call may remove H from the list, or even
         --  destroy it.
         --  However, we assume it doesn't remove the next callback...
         Call_Callback (Arg);
      end if;
   end Call_Valuechange_Callback;

   procedure Resched_Callback (Arg : System.Address)
   is
      Hand : constant vpiHandle := To_vpiHandle (Arg);
   begin
      case Hand.Cb.Reason is
         when cbReadOnlySynch =>
            Register_Callback
              (Cb_End_Of_Time_Step, Hand.Cb_Handle, Oneshot,
               Call_Callback'Access, Arg);
         when cbReadWriteSynch =>
            Register_Callback
              (Cb_Last_Known_Delta, Hand.Cb_Handle, Oneshot,
               Call_Callback'Access, Arg);
         when others =>
            raise Program_Error;
      end case;
   end Resched_Callback;

   function vpi_register_cb (Data : p_cb_data) return vpiHandle
   is
      Res : vpiHandle;
      T : Std_Time;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_register_cb ({reason=");
         Trace_Cb_Reason (Data.Reason);
         Trace (", obj=");
         Trace (Data.Obj);
         case Data.Reason is
            when cbAfterDelay =>
               Trace (", time=");
               Trace_Time (Vpi_Time_To_Time (Data.Time.all));
            when others =>
               null;
         end case;
         Trace ("}) = ");
      end if;

      Res := new struct_vpiHandle (vpiCallback);
      Res.Cb := Data.all;

      case Data.Reason is
         when cbEndOfCompile =>
            Append_Callback (g_cbEndOfCompile, Res);
         when cbStartOfSimulation =>
            Append_Callback (g_cbStartOfSimulation, Res);
         when cbEndOfSimulation =>
            Append_Callback (g_cbEndOfSimulation, Res);
         when cbValueChange =>
            Get_Verilog_Wire (Data.Obj.Ref, Res.Cb_Wire);
            Register_Callback
              (Cb_Signals_Updated, Res.Cb_Handle, Repeat,
               Call_Valuechange_Callback'Access, To_Address (Res));
         when cbReadOnlySynch
           | cbReadWriteSynch =>
            T := Vpi_Time_To_Time (Data.Time.all);
            if T = 0 then
               Resched_Callback (To_Address (Res));
            else
               Register_Callback_At
                 (Cb_After_Delay, Res.Cb_Handle, Current_Time + T,
                  Resched_Callback'Access, To_Address (Res));
            end if;
         when cbAfterDelay =>
            T := Vpi_Time_To_Time (Data.Time.all);
            Register_Callback_At
              (Cb_After_Delay, Res.Cb_Handle, Current_Time + T,
               Call_Callback'Access, To_Address (Res));
         when cbNextSimTime =>
            Register_Callback
              (Cb_Next_Time_Step, Res.Cb_Handle, Oneshot,
               Call_Callback'Access, To_Address (Res));
         when others =>
            dbgPut_Line ("vpi_register_cb: unknown reason");
            Free (Res);
      end case;

      if Flag_Trace then
         Trace (Res);
         Trace_Newline;
      end if;

      return Res;
   end vpi_register_cb;

   -- int vpi_remove_cb(vpiHandle ref)
   function vpi_remove_cb (Ref : vpiHandle) return Integer
   is
      Ref_Copy : vpiHandle;
      Res : Integer;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_remove_cb (");
         Trace (Ref);
         Trace (") = ");
      end if;

      Res := 1;
      Ref_Copy := Ref;
      case Ref.Cb.Reason is
         when cbValueChange =>
            Delete_Callback (Ref.Cb_Handle);
         when cbReadWriteSynch
           | cbReadOnlySynch =>
            Delete_Callback (Ref.Cb_Handle);
         when others =>
            Res := 0;
            Ref_Copy := null;
      end case;

      if Flag_Trace then
         if Ref_Copy = null then
            Trace ("[not free] ");
         else
            Trace ("[free] ");
         end if;
         Trace (Res);
         Trace_Newline;
      end if;

      Free (Ref_Copy);

      return Res;
   end vpi_remove_cb;

   -- int vpi_free_object(vpiHandle ref)
   function vpi_free_object (aRef: vpiHandle) return integer
   is
      Ref_Copy : vpiHandle;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_free_object (");
         Trace (aRef);
         Trace (")");
         Trace_Newline;
      end if;
      Ref_Copy := aRef;
      Free (Ref_Copy);
      return 1;
   end vpi_free_object;

-------------------------------------------------------------------------------
-- * * *   V P I   d u m m i e s   * * * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   -- int vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p)
   function vpi_get_vlog_info (info : p_vpi_vlog_info) return integer is
   begin
      if Flag_Trace then
         Trace_Start ("vpi_get_vlog_info");
         Trace_Newline;
      end if;

      info.all := (Argc => 0,
                   Argv => Null_Address,
                   Product => To_Ghdl_C_String (Product'Address),
                   Version => To_Ghdl_C_String (Version'Address));
      return 0;
   end vpi_get_vlog_info;

   -- vpiHandle vpi_handle_by_index(vpiHandle ref, int index)
   function vpi_handle_by_index (aRef: vpiHandle; aIndex: integer)
                                return vpiHandle
   is
      pragma Unreferenced (aRef);
      pragma Unreferenced (aIndex);
   begin
      if Flag_Trace then
         Trace_Start ("vpi_handle_by_index UNIMPLEMENTED!");
         Trace_Newline;
      end if;

      return null;
   end vpi_handle_by_index;

   --  Return True iff L and R are equal.  L must not have an element set to
   --  NUL.  R must be lower case.
   function Strcasecmp (L : String; R : Ghdl_C_String) return Boolean is
   begin
      if L'Last < L'First - 1 then
         --  Handle null string.
         return R (1) = NUL;
      end if;

      for I in L'Range loop
         if L (I) = NUL then
            --  NUL not allowed in L.
            return False;
         end if;
         if To_Lower (L (I)) /= R (I - L'First + 1) then
            return False;
         end if;
      end loop;

      --  R is NUL terminated.
      return R (L'Length + 1) = NUL;
   end Strcasecmp;

   procedure Find_By_Name (Scope : VhpiHandleT;
                           Rel : VhpiOneToManyT;
                           Name : String;
                           Res : out VhpiHandleT;
                           Err : out AvhpiErrorT)
   is
      It : VhpiHandleT;
      El_Name : Ghdl_C_String;
   begin
      Vhpi_Iterator (Rel, Scope, It, Err);
      if Err /= AvhpiErrorOk then
         return;
      end if;

      loop
         Vhpi_Scan (It, Res, Err);

         --  Either a real error or end of iterator.
         exit when Err /= AvhpiErrorOk;

         El_Name := Avhpi_Get_Base_Name (Res);
         exit when El_Name /= null and then Strcasecmp (Name, El_Name);
      end loop;
   end Find_By_Name;

   function Vpi_Handle_By_Name_Internal
     (Name : Ghdl_C_String; Scope : vpiHandle) return vpiHandle
   is
      B, E : Natural;
      Base, El : VhpiHandleT;
      Err : AvhpiErrorT;
      Prop : Integer;
      Res : vpiHandle;
   begin
      --  Extract the start point.
      if Scope = null then
         Get_Root_Scope (Base);
      else
         Base := Scope.Ref;
      end if;

      B := Name'First;

      --  Iterate on each part of Name.
      loop
         exit when Name (B) = NUL;

         --  Extract the next part of the name.
         declare
            C : Character;
         begin
            E := B;
            loop
               C := Name (E + 1);
               exit when C = NUL or C = '.';
               E := E + 1;
            end loop;
         end;

         --  Find name in Base, first as a decl, then as a sub-region.
         Find_By_Name (Base, VhpiDecls, Name (B .. E), El, Err);
         if Err /= AvhpiErrorOk then
            Find_By_Name (Base, VhpiInternalRegions, Name (B .. E), El, Err);
         end if;

         if Err = AvhpiErrorOk then
            --  Found!
            Base := El;
         else
            --  Not found.
            return null;
         end if;

         --  Next path component.
         B := E + 1;
         exit when Name (B) = NUL;
         pragma Assert (Name (B) = '.');
         B := B + 1;
      end loop;

      Prop := Vhpi_Handle_To_Vpi_Prop (Base);
      if Prop /= vpiUndefined then
         Res := Build_vpiHandle (Base, Prop);
      else
         Res := null;
      end if;

      return Res;
   end Vpi_Handle_By_Name_Internal;

   function vpi_handle_by_name (Name : Ghdl_C_String; Scope : vpiHandle)
                               return vpiHandle
   is
      Res : vpiHandle;
   begin
      if Flag_Trace then
         Trace_Start ("vpi_handle_by_name (");
         Trace (Name);
         Trace (", ");
         Trace (Scope);
         Trace (") = ");
      end if;

      Res := Vpi_Handle_By_Name_Internal (Name, Scope);

      if Flag_Trace then
         Trace (Res);
         Trace_Newline;
      end if;

      return Res;
   end vpi_handle_by_name;

   -- unsigned int vpi_mcd_close(unsigned int mcd)
   function vpi_mcd_close (Mcd: integer) return integer
   is
      pragma Unreferenced (Mcd);
   begin
      return 0;
   end vpi_mcd_close;

   -- char *vpi_mcd_name(unsigned int mcd)
   function vpi_mcd_name (Mcd: integer) return integer
   is
      pragma Unreferenced (Mcd);
   begin
      return 0;
   end vpi_mcd_name;

   -- unsigned int vpi_mcd_open(char *name)
   function vpi_mcd_open (Name : Ghdl_C_String) return Integer
   is
      pragma Unreferenced (Name);
   begin
      return 0;
   end vpi_mcd_open;

   function vpi_register_systf (aSs: System.Address) return vpiHandle
   is
      pragma Unreferenced (aSs);
   begin
      if Flag_Trace then
         Trace_Start ("vpi_register_systf");
         Trace_Newline;
      end if;
      return null;
   end vpi_register_systf;

   -- missing here, see grt-cvpi.c:
   --    vpi_mcd_open_x
   --    vpi_mcd_vprintf
   --    vpi_mcd_fputc
   --    vpi_mcd_fgetc
   --    vpi_sim_vcontrol
   --    vpi_chk_error
   --    vpi_handle_by_name

   Default_Message : constant String := "(no error message)" & NUL;
   Unknown_File : constant String := "(no file)" & NUL;

   Err_Message : Ghdl_C_String := To_Ghdl_C_String (Default_Message'Address);
   Err_Code : Ghdl_C_String := null;
   Err_File : Ghdl_C_String := To_Ghdl_C_String (Unknown_File'Address);
   Err_Line : Integer := 0;
   Err_Status : Integer := 0;

   procedure Reset_Error is
   begin
      Err_Message := To_Ghdl_C_String (Default_Message'Address);
      Err_Code := null;
      Err_File := To_Ghdl_C_String (Unknown_File'Address);
      Err_Line := 0;
      Err_Status := 0;
   end Reset_Error;

   function vpi_chk_error (Info : p_vpi_error_info) return Integer is
   begin
      if Info /= null then
         Info.all := (State => vpiRun,
                      Level => vpiError,
                      Message => Err_Message,
                      Product => To_Ghdl_C_String (Product'Address),
                      Code => Err_Code,
                      File => Err_File,
                      Line => Err_Line);
      end if;
      return Err_Status;
   end vpi_chk_error;

   function vpi_control_np (Op : Integer; Status : Integer) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vpi_control (");
         Trace_Property (Op);
         Trace (", ");
         Trace (Status);
         Trace (")");
         Trace_Newline;
      end if;

      case Op is
         when vpiFinish
           | vpiStop =>
            Options.Break_Simulation := True;
            return 1;
         when others =>
            return 0;
      end case;
   end vpi_control_np;

------------------------------------------------------------------------------
-- * * *   G H D L   h o o k s   * * * * * * * * * * * * * * * * * * * * * * *
------------------------------------------------------------------------------

   --  VCD filename.
   Vpi_Filename : String_Access := null;

   ------------------------------------------------------------------------
   --  Return TRUE if OPT is an option for VPI.
   function Vpi_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
   begin
      if Opt'Length < 5 or else Opt (F .. F + 4) /= "--vpi" then
         return False;
      end if;
      if Opt'Length > 6 and then Opt (F + 5) = '=' then
         --  Add an extra NUL character.
         Vpi_Filename := new String (1 .. Opt'Length - 6 + 1);
         Vpi_Filename (1 .. Opt'Length - 6) := Opt (F + 6 .. Opt'Last);
         Vpi_Filename (Vpi_Filename'Last) := NUL;
         return True;
      elsif Opt'Length >= 11 and then Opt (F + 5 .. F + 10) = "-trace" then
         if Opt'Length > 11 and then Opt (F + 11) = '=' then
            declare
               Filename : String (1 .. Opt'Length - 11);
               Mode : constant String := "wt" & NUL;
            begin
               Filename (1 .. Filename'Last - 1) := Opt (F + 12 .. Opt'Last);
               Filename (Filename'Last) := NUL;
               Trace_File := fopen (Filename'Address, Mode'Address);
               if Trace_File = NULL_Stream then
                  Error_S ("cannot open vpi trace file '");
                  Diag_C (Opt (F + 12 .. Opt'Last));
                  Error_E ("'");
                  return False;
               end if;
            end;
         elsif Opt'Length = 11 then
            Trace_File := stdout;
         else
            Error_S ("incorrect option '");
            Diag_C (Opt);
            Error_E ("'");
            return False;
         end if;
         Flag_Trace := True;
         return True;
      else
         return False;
      end if;
   end Vpi_Option;

   ------------------------------------------------------------------------
   procedure Vpi_Help is
   begin
      Put_Line (" --vpi=FILENAME     load VPI module");
      Put_Line (" --vpi-trace[=FILE] trace vpi calls to FILE");
   end Vpi_Help;

   ------------------------------------------------------------------------
   --  Called before elaboration.

   -- void loadVpiModule(const char* modulename)
   function LoadVpiModule (Filename: Address) return Integer;
   pragma Import (C, LoadVpiModule, "loadVpiModule");

   procedure Vpi_Init
   is
   begin
      if Vpi_Filename /= null then
         if LoadVpiModule (Vpi_Filename.all'Address) /= 0 then
            Error ("cannot load VPI module");
         end if;
      end if;
   end Vpi_Init;

   ------------------------------------------------------------------------
   --  Called after elaboration.
   procedure Vpi_Start
   is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      if Vpi_Filename = null then
         return;
      end if;

      Grt.Rtis_Types.Search_Types_RTI;
      Execute_Callback_List (g_cbEndOfCompile);
      Execute_Callback_List (g_cbStartOfSimulation);
   end Vpi_Start;

   ------------------------------------------------------------------------
   --  Called at the end of the simulation.
   procedure Vpi_End
   is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      Execute_Callback_List (g_cbEndOfSimulation);
      Free (Buf_Value);
   end Vpi_End;

   Vpi_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("vpi: vpi compatible API"),
      Option => Vpi_Option'Access,
      Help => Vpi_Help'Access,
      Init => Vpi_Init'Access,
      Start => Vpi_Start'Access,
      Finish => Vpi_End'Access);

   procedure Register is
   begin
      Register_Hooks (Vpi_Hooks'Access);
   end Register;
end Grt.Vpi;
