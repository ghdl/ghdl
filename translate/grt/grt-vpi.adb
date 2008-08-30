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
with Grt.Table;
with Grt.Astdio; use Grt.Astdio;
with Grt.Hooks; use Grt.Hooks;
with Grt.Vcd; use Grt.Vcd;
with Grt.Errors; use Grt.Errors;
with Grt.Rtis_Types;
pragma Elaborate_All (Grt.Table);

package body Grt.Vpi is
   --  The VPI interface requires libdl (dlopen, dlsym) to be linked in.
   --  This is now set in Makefile, since this is target dependent.
   --  pragma Linker_Options ("-ldl");

   --errAnyString:     constant String := "grt-vcd.adb: any string" & NUL;
   --errNoString:      constant String := "grt-vcd.adb: no string" & NUL;

   type Vpi_Index_Type is new Integer;

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

-------------------------------------------------------------------------------
-- * * *   V P I   f u n c t i o n s   * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   ------------------------------------------------------------------------
   -- vpiHandle  vpi_iterate(int type, vpiHandle ref)
   -- Obtain an iterator handle to objects with a one-to-many relationship.
   -- see IEEE 1364-2001, page 685
   function vpi_iterate (aType: integer; Ref: vpiHandle) return vpiHandle
   is
      Res : vpiHandle;
      Rel : VhpiOneToManyT;
      Error : AvhpiErrorT;
   begin
      --dbgPut_Line ("vpi_iterate");

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
         return null;
      end if;

      Res := new struct_vpiHandle (aType);
      Vhpi_Iterator (Rel, Ref.Ref, Res.Ref, Error);

      if Error /= AvhpiErrorOk then
         Free (Res);
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

   function vpi_get (Property: integer; Ref: vpiHandle) return Integer
   is
   begin
      case Property is
         when vpiType=>
            return Ref.mType;
         when vpiTimePrecision=>
            return -9; -- is this nano-seconds?
         when others=>
            dbgPut_Line ("vpi_get: unknown property");
            return 0;
      end case;
   end vpi_get;

   ------------------------------------------------------------------------
   -- vpiHandle  vpi_scan(vpiHandle iter)
   -- Scan the Verilog HDL hierarchy for objects with a one-to-many
   -- relationship.
   -- see IEEE 1364-2001, chapter 27.36, page 709
   function vpi_scan (Iter: vpiHandle) return vpiHandle
   is
      Res : VhpiHandleT;
      Error : AvhpiErrorT;
      R : vpiHandle;
   begin
      --dbgPut_Line ("vpi_scan");
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

      loop
         Vhpi_Scan (Iter.Ref, Res, Error);
         exit when Error /= AvhpiErrorOk;

         case Vhpi_Get_Kind (Res) is
            when VhpiEntityDeclK
              | VhpiArchBodyK
              | VhpiBlockStmtK
              | VhpiIfGenerateK
              | VhpiForGenerateK
              | VhpiCompInstStmtK =>
               case Iter.mType is
                  when vpiInternalScope
                    | vpiModule =>
                     return new struct_vpiHandle'(mType => vpiModule,
                                                  Ref => Res);
                  when others =>
                     null;
               end case;
            when VhpiPortDeclK
              | VhpiSigDeclK =>
               if Iter.mType = vpiNet then
                  declare
                     Info : Verilog_Wire_Info;
                  begin
                     Get_Verilog_Wire (Res, Info);
                     if Info.Kind /= Vcd_Bad then
                        return new struct_vpiHandle'(mType => vpiNet,
                                                     Ref => Res);
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;
      end loop;
      return null;
   end vpi_scan;

   ------------------------------------------------------------------------
   -- char *vpi_get_str(int property, vpiHandle ref)
   -- see IEEE 1364-2001, page xxx
   Tmpstring2 : String (1 .. 1024);
   function vpi_get_str (Property : Integer; Ref : vpiHandle)
                        return Ghdl_C_String
   is
      Prop : VhpiStrPropertyT;
      Len : Natural;
   begin
      --dbgPut_Line ("vpiGetStr");

      if Ref = null then
         return null;
      end if;

      case Property is
         when vpiFullName=>
            Prop := VhpiFullNameP;
         when vpiName=>
            Prop := VhpiNameP;
         when others=>
            dbgPut_Line ("vpi_get_str: undefined property");
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
         return To_Ghdl_C_String (Tmpstring2 (2)'Address);
      else
         return To_Ghdl_C_String (Tmpstring2'Address);
      end if;
   end vpi_get_str;

   ------------------------------------------------------------------------
   -- vpiHandle  vpi_handle(int type, vpiHandle ref)
   -- Obtain a handle to an object with a one-to-one relationship.
   -- see IEEE 1364-2001, chapter 27.16, page 682
   function vpi_handle (aType : Integer; Ref : vpiHandle) return vpiHandle
   is
      Res : vpiHandle;
   begin
      --dbgPut_Line ("vpi_handle");

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
   end vpi_handle;

   ------------------------------------------------------------------------
   -- void  vpi_get_value(vpiHandle expr, p_vpi_value value);
   -- Retrieve the simulation value of an object.
   -- see IEEE 1364-2001, chapter 27.14, page 675
   Tmpstring3idx : integer;
   Tmpstring3 : String (1 .. 1024);
   procedure ii_vpi_get_value_bin_str_B2 (Val : Ghdl_B2)
   is
   begin
      case Val is
         when True =>
            Tmpstring3 (Tmpstring3idx) := '1';
         when False =>
            Tmpstring3 (Tmpstring3idx) := '0';
      end case;
      Tmpstring3idx := Tmpstring3idx + 1;
   end ii_vpi_get_value_bin_str_B2;

   procedure ii_vpi_get_value_bin_str_E8 (Val : Ghdl_E8)
   is
      type Map_Type_E8 is array (Ghdl_E8 range 0..8) of character;
      Map_Std_E8: constant Map_Type_E8 := "UX01ZWLH-";
   begin
      if Val not in Map_Type_E8'range then
         Tmpstring3 (Tmpstring3idx) := '?';
      else
         Tmpstring3 (Tmpstring3idx) := Map_Std_E8(Val);
      end if;
      Tmpstring3idx := Tmpstring3idx + 1;
   end ii_vpi_get_value_bin_str_E8;

   function ii_vpi_get_value_bin_str (Obj : VhpiHandleT)
                                     return Ghdl_C_String
   is
      Info : Verilog_Wire_Info;
      Len : Ghdl_Index_Type;
   begin
      case Vhpi_Get_Kind (Obj) is
         when VhpiPortDeclK
           | VhpiSigDeclK =>
            null;
         when others =>
            return null;
      end case;

      --  Get verilog compat info.
      Get_Verilog_Wire (Obj, Info);
      if Info.Kind = Vcd_Bad then
         return null;
      end if;

      if Info.Irange = null then
         Len := 1;
      else
         Len := Info.Irange.I32.Len;
      end if;

      Tmpstring3idx := 1; -- reset string buffer

      case Info.Val is
         when Vcd_Effective =>
            case Info.Kind is
               when Vcd_Bad
                 | Vcd_Integer32 =>
                  return null;
               when Vcd_Bit
                 | Vcd_Bool
                 | Vcd_Bitvector =>
                  for J in 0 .. Len - 1 loop
                     ii_vpi_get_value_bin_str_B2
                       (To_Signal_Arr_Ptr (Info.Addr)(J).Value.B2);
                  end loop;
               when Vcd_Stdlogic
                 | Vcd_Stdlogic_Vector =>
                  for J in 0 .. Len - 1 loop
                     ii_vpi_get_value_bin_str_E8
                       (To_Signal_Arr_Ptr (Info.Addr)(J).Value.E8);
                  end loop;
            end case;
         when Vcd_Driving =>
            case Info.Kind is
               when Vcd_Bad
                 | Vcd_Integer32 =>
                  return null;
               when Vcd_Bit
                 | Vcd_Bool
                 | Vcd_Bitvector =>
                  for J in 0 .. Len - 1 loop
                     ii_vpi_get_value_bin_str_B2
                       (To_Signal_Arr_Ptr (Info.Addr)(J).Driving_Value.B2);
                  end loop;
               when Vcd_Stdlogic
                 | Vcd_Stdlogic_Vector =>
                  for J in 0 .. Len - 1 loop
                     ii_vpi_get_value_bin_str_E8
                       (To_Signal_Arr_Ptr (Info.Addr)(J).Driving_Value.E8);
                  end loop;
            end case;
      end case;
      Tmpstring3 (Tmpstring3idx) := NUL;
      return To_Ghdl_C_String (Tmpstring3'Address);
   end ii_vpi_get_value_bin_str;

   procedure vpi_get_value (Expr : vpiHandle; Value : p_vpi_value)
   is
   begin
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
   end vpi_get_value;

   ------------------------------------------------------------------------
   -- void  vpiHandle vpi_put_value(vpiHandle obj, p_vpi_value value,
   --                               p_vpi_time when, int flags)
   -- Alter the simulation value of an object.
   -- see IEEE 1364-2001, chapter 27.14, page 675
   -- FIXME

   procedure ii_vpi_put_value_bin_str_B2 (SigPtr : Ghdl_Signal_Ptr;
                                          Value : Character)
   is
      Tempval : Value_Union;
   begin
      -- use the Set_Effective_Value procedure to update the signal
      case Value is
         when '0' =>
            Tempval.B2 := false;
         when '1' =>
            Tempval.B2 := true;
         when others =>
            dbgPut_Line("ii_vpi_put_value_bin_str_B2: "
                        & "wrong character - signal wont be set");
            return;
      end case;
      SigPtr.Driving_Value := Tempval;
      Set_Effective_Value (SigPtr, Tempval);
   end ii_vpi_put_value_bin_str_B2;

   procedure ii_vpi_put_value_bin_str_E8 (SigPtr : Ghdl_Signal_Ptr;
                                          Value : Character)
   is
      Tempval : Value_Union;
   begin
      case Value is
         when 'U' =>
            Tempval.E8 := 0;
         when 'X' =>
            Tempval.E8 := 1;
         when '0' =>
            Tempval.E8 := 2;
         when '1' =>
            Tempval.E8 := 3;
         when 'Z' =>
            Tempval.E8 := 4;
         when 'W' =>
            Tempval.E8 := 5;
         when 'L' =>
            Tempval.E8 := 6;
         when 'H' =>
            Tempval.E8 := 7;
         when '-' =>
            Tempval.E8 := 8;
         when others =>
            dbgPut_Line("ii_vpi_put_value_bin_str_B8: "
                        & "wrong character - signal wont be set");
            return;
      end case;
      SigPtr.Driving_Value := Tempval;
      Set_Effective_Value (SigPtr, Tempval);
   end ii_vpi_put_value_bin_str_E8;


   procedure ii_vpi_put_value_bin_str(Obj : VhpiHandleT;
                                      ValueStr : Ghdl_C_String)
   is
      Info : Verilog_Wire_Info;
      Len  : Ghdl_Index_Type;
   begin
      -- Check the Obj type.
      -- * The vpiHandle has a reference (field Ref) to a VhpiHandleT
      --   when it doesnt come from a callback.
      case Vhpi_Get_Kind(Obj) is
         when VhpiPortDeclK
           | VhpiSigDeclK =>
            null;
         when others =>
            return;
      end case;

      -- The following code segment was copied from the
      -- ii_vpi_get_value function.
      --  Get verilog compat info.
      Get_Verilog_Wire (Obj, Info);
      if Info.Kind = Vcd_Bad then
         return;
      end if;

      if Info.Irange = null then
         Len := 1;
      else
         Len := Info.Irange.I32.Len;
      end if;

      -- Step 1: convert vpi object to internal format.
      --         p_vpi_handle -> Ghdl_Signal_Ptr
      --         To_Signal_Arr_Ptr (Info.Addr) does part of the magic

      -- Step 2: convert datum to appropriate type.
      --         Ghdl_C_String -> Value_Union

      -- Step 3: assigns value to object using Set_Effective_Value
      --         call (from grt-signals)
      -- Set_Effective_Value(sig_ptr, conv_value);


      -- Took the skeleton from ii_vpi_get_value function
      -- This point of the function must convert the string value to the
      -- native ghdl format.
      case Info.Kind is
         when Vcd_Bad =>
            return;
         when Vcd_Bit
           | Vcd_Bool
           | Vcd_Bitvector =>
            for J in 0 .. Len - 1 loop
               ii_vpi_put_value_bin_str_B2(
                  To_Signal_Arr_Ptr(Info.Addr)(J), ValueStr(Integer(J+1)));
            end loop;
         when Vcd_Stdlogic
           | Vcd_Stdlogic_Vector =>
            for J in 0 .. Len - 1 loop
               ii_vpi_put_value_bin_str_E8(
                  To_Signal_Arr_Ptr(Info.Addr)(J), ValueStr(Integer(J+1)));
            end loop;
         when Vcd_Integer32 =>
            null;
      end case;

      -- Always return null, because this simulation kernel cannot send
      -- a handle to the event back.
      return;
   end ii_vpi_put_value_bin_str;


   -- vpiHandle vpi_put_value(vpiHandle obj, p_vpi_value value,
   --                         p_vpi_time when, int flags)
   function vpi_put_value (aObj: vpiHandle;
                           aValue: p_vpi_value;
                           aWhen: p_vpi_time;
                           aFlags: integer)
                         return vpiHandle
   is
      pragma Unreferenced (aWhen);
      pragma Unreferenced (aFlags);
   begin
      -- A very simple write procedure for VPI.
      -- Basically, it accepts bin_str values and converts to appropriate
      -- types (only std_logic and bit values and vectors).

      -- It'll use Set_Effective_Value procedure to update signals

      -- Ignoring aWhen and aFlags, for now.

      -- Checks the format of aValue. Only vpiBinStrVal will be accepted
      --  for now.
      case aValue.Format is
         when vpiObjTypeVal =>
            dbgPut_Line ("vpi_put_value: vpiObjTypeVal");
         when vpiBinStrVal =>
            ii_vpi_put_value_bin_str(aObj.Ref, aValue.Str);
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
            dbgPut_Line ("vpi_put_value: vpiIntVal");
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
   Sim_Time : Std_Time;
   procedure vpi_get_time (Obj: vpiHandle; Time: p_vpi_time)
   is
      pragma Unreferenced (Obj);
   begin
      --dbgPut_Line ("vpi_get_time");
      Time.mType := vpiSimTime;
      Time.mHigh := 0;
      Time.mLow  := Integer (Sim_Time / 1000000);
      Time.mReal := 0.0;
   end vpi_get_time;

   ------------------------------------------------------------------------
   -- vpiHandle vpi_register_cb(p_cb_data data)
   g_cbEndOfCompile : p_cb_data;
   g_cbEndOfSimulation: p_cb_data;
   --g_cbValueChange:     s_cb_data;
   g_cbReadOnlySync:    p_cb_data;

   type Vpi_Var_Type is record
      Info : Verilog_Wire_Info;
      Cb   : s_cb_data;
   end record;

   package Vpi_Table is new Grt.Table
     (Table_Component_Type => Vpi_Var_Type,
      Table_Index_Type     => Vpi_Index_Type,
      Table_Low_Bound      => 0,
      Table_Initial        => 32);

   function vpi_register_cb (Data : p_cb_data) return vpiHandle
   is
      Res : p_cb_data := null;
   begin
      --dbgPut_Line ("vpi_register_cb");
      case Data.Reason is
         when cbEndOfCompile =>
            Res := new s_cb_data'(Data.all);
            g_cbEndOfCompile := Res;
            Sim_Time:= 0;
         when cbEndOfSimulation =>
            Res := new s_cb_data'(Data.all);
            g_cbEndOfSimulation := Res;
         when cbValueChange =>
            declare
               N : Vpi_Index_Type;
            begin
               --g_cbValueChange:=     aData.all;
               Vpi_Table.Increment_Last;
               N := Vpi_Table.Last;
               Vpi_Table.Table (N).Cb := Data.all;
               Get_Verilog_Wire (Data.Obj.Ref, Vpi_Table.Table (N).Info);
            end;
         when cbReadOnlySynch=>
            Res := new s_cb_data'(Data.all);
            g_cbReadOnlySync := Res;
         when others=>
            dbgPut_Line ("vpi_register_cb: unknwon reason");
      end case;
      if Res /= null then
         return new struct_vpiHandle'(mType => vpiCallback,
                                      Cb => Res);
      else
         return null;
      end if;
   end vpi_register_cb;

-------------------------------------------------------------------------------
-- * * *   V P I   d u m m i e s   * * * * * * * * * * * * * * * * * * * * * *
-------------------------------------------------------------------------------

   -- int vpi_free_object(vpiHandle ref)
   function vpi_free_object (aRef: vpiHandle) return integer
   is
      pragma Unreferenced (aRef);
   begin
      return 0;
   end vpi_free_object;

   -- int vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p)
   function vpi_get_vlog_info (aVlog_info_p: System.Address) return integer
   is
      pragma Unreferenced (aVlog_info_p);
   begin
      return 0;
   end vpi_get_vlog_info;

   -- vpiHandle vpi_handle_by_index(vpiHandle ref, int index)
   function vpi_handle_by_index(aRef: vpiHandle; aIndex: integer)
                               return vpiHandle
   is
      pragma Unreferenced (aRef);
      pragma Unreferenced (aIndex);
   begin
      return null;
   end vpi_handle_by_index;

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

   -- void vpi_register_systf(const struct t_vpi_systf_data*ss)
   procedure vpi_register_systf(aSs: System.Address)
   is
      pragma Unreferenced (aSs);
   begin
      null;
   end vpi_register_systf;

   -- int vpi_remove_cb(vpiHandle ref)
   function vpi_remove_cb (Ref : vpiHandle) return Integer
   is
      pragma Unreferenced (Ref);
   begin
      return 0;
   end vpi_remove_cb;

   -- void vpi_vprintf(const char*fmt, va_list ap)
   procedure vpi_vprintf (Fmt : Address; Ap : Address)
   is
      pragma Unreferenced (Fmt);
      pragma Unreferenced (Ap);
   begin
      null;
   end vpi_vprintf;

   -- missing here, see grt-cvpi.c:
   --    vpi_mcd_open_x
   --    vpi_mcd_vprintf
   --    vpi_mcd_fputc
   --    vpi_mcd_fgetc
   --    vpi_sim_vcontrol
   --    vpi_chk_error
   --    pi_handle_by_name

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
      else
         return False;
      end if;
   end Vpi_Option;

   ------------------------------------------------------------------------
   procedure Vpi_Help is
   begin
      Put_Line (" --vpi=FILENAME     load VPI module");
   end Vpi_Help;

   ------------------------------------------------------------------------
   --  Called before elaboration.

   -- void loadVpiModule(const char* modulename)
   function LoadVpiModule (Filename: Address) return Integer;
   pragma Import (C, LoadVpiModule, "loadVpiModule");


   procedure Vpi_Init
   is
   begin
      Sim_Time:= 0;

      --g_cbEndOfCompile.mCb_rtn:= null;
      --g_cbEndOfSimulation.mCb_rtn:= null;
      --g_cbValueChange.mCb_rtn:= null;

      if Vpi_Filename /= null then
         if LoadVpiModule (Vpi_Filename.all'Address) /= 0 then
            Error ("cannot load VPI module");
         end if;
      end if;
   end Vpi_Init;

   procedure Vpi_Cycle;

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
      Register_Cycle_Hook (Vpi_Cycle'Access);
      if g_cbEndOfCompile /= null then
         Res := g_cbEndOfCompile.Cb_Rtn.all (g_cbEndOfCompile);
      end if;
   end Vpi_Start;

   ------------------------------------------------------------------------
   --  Called before each non delta cycle.
   procedure Vpi_Cycle
   is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      if g_cbReadOnlySync /= null
        and then g_cbReadOnlySync.Time.mLow < Integer (Sim_Time / 1_000_000)
      then
         Res := g_cbReadOnlySync.Cb_Rtn.all (g_cbReadOnlySync);
      end if;

      for I in Vpi_Table.First .. Vpi_Table.Last loop
         if Verilog_Wire_Changed (Vpi_Table.Table (I).Info, Sim_Time) then
            Res := Vpi_Table.Table (I).Cb.Cb_Rtn.all
              (To_p_cb_data (Vpi_Table.Table (I).Cb'Address));
         end if;
      end loop;

      if Current_Time /= Std_Time'last then
         Sim_Time:= Current_Time;
      end if;
   end Vpi_Cycle;

   ------------------------------------------------------------------------
   --  Called at the end of the simulation.
   procedure Vpi_End
   is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      if g_cbEndOfSimulation /= null then
         Res := g_cbEndOfSimulation.Cb_Rtn.all (g_cbEndOfSimulation);
      end if;
   end Vpi_End;

   Vpi_Hooks : aliased constant Hooks_Type :=
     (Option => Vpi_Option'Access,
      Help => Vpi_Help'Access,
      Init => Vpi_Init'Access,
      Start => Vpi_Start'Access,
      Finish => Vpi_End'Access);

   procedure Register is
   begin
      Register_Hooks (Vpi_Hooks'Access);
   end Register;
end Grt.Vpi;
