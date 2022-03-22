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

with Ada.Unchecked_Conversion;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Astdio; use Grt.Astdio;
with Grt.Astdio.Vhdl; use Grt.Astdio.Vhdl;
with Grt.Errors; use Grt.Errors;
with Grt.Hooks; use Grt.Hooks;
with Grt.Stdio; use Grt.Stdio;
with Grt.Vstrings; use Grt.Vstrings;

package body Grt.Vhpi is
   --  The VHPI interface requires libdl (dlopen, dlsym) to be linked in.
   --  This is now set in Makefile, since this is target dependent.
   --  pragma Linker_Options ("-ldl");

   --  If true, emit traces
   Flag_Trace : Boolean := False;
   Trace_File : FILEs;

   VhpiUndefined_External : constant Integer := -1;


   ----------------------------------------------------------------------------
   -- Internal helper functions
   ----------------------------------------------------------------------------

   function To_Address is new Ada.Unchecked_Conversion
     (Vhpi_External_Handle, System.Address);

   -- VHPI errors

   Default_Message : constant String := "(no error message)" & NUL;

   Err_Severity : VhpiSeverityT := VhpiNote;
   Err_Message : Ghdl_C_String := To_Ghdl_C_String (Default_Message'Address);
   Err_Str : Ghdl_C_String := null;
   Err_File : Ghdl_C_String := null;
   Err_Line : Integer_32 := -1;
   Err_Occured : Boolean := False;

   Buf_Err_Message : Vstring;

   procedure Reset_Error is
   begin
      Err_Severity := VhpiNote;
      Err_Message := To_Ghdl_C_String (Default_Message'Address);
      Err_Str := null;
      Err_File := null;
      Err_Line := -1;
      Err_Occured := False;
   end Reset_Error;

   Err_Str_S : constant String := "GHDL Error ";
   -- VHPI function not implemented
   VHPI_0001_Str  : constant String := Err_Str_S & "VHPI_0001";

   procedure Set_Err_Str (S : String) is
   begin
      Err_Str := To_Ghdl_C_String (S'Address);
   end Set_Err_Str;

   procedure Error_Unimplimented (Name : String) is
   begin
      Err_Severity := VhpiError;
      Reset (Buf_Err_Message);
      Append (Buf_Err_Message, Name);
      Append (Buf_Err_Message, " not implemented");
      Append (Buf_Err_Message, NUL);
      Err_Message := Get_C_String (Buf_Err_Message);
      Set_Err_Str (VHPI_0001_Str);
      Err_Occured := True;
   end Error_Unimplimented;

   -- VHPI tracing

   procedure Trace_Start (Msg : String) is
   begin
      -- TODO: Add indent when callbacks are supported
      Put (Trace_File, Msg);
   end Trace_Start;

   procedure Trace_Newline is
   begin
      New_Line (Trace_File);
   end Trace_Newline;

   procedure Trace (Msg : String) is
   begin
      Put (Trace_File, Msg);
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

   procedure Trace (V : Integer_32) is
   begin
      Put_I32 (Trace_File, Ghdl_I32 (V));
   end Trace;

   procedure Trace (V : Integer) is
   begin
      Put_I32 (Trace_File, Ghdl_I32 (V));
   end Trace;

   procedure Trace (V : Unsigned_64) is
   begin
      Put_U64 (Trace_File, Ghdl_U64 (V));
   end Trace;

   procedure Trace (A : System.Address)
   is
   begin
      Put (Trace_File, A);
   end Trace;

   procedure Trace (H : Vhpi_External_Handle)
   is
   begin
      Put (Trace_File, To_Address (H));
   end Trace;

   procedure Trace_Time (V : Std_Time) is
   begin
      Put_Time (Trace_File, V);
   end Trace_Time;

   function Vhpi_Time_To_Time (V : VhpiTimeT) return Std_Time is
      Res : Std_Time;
   begin
      Res := Std_Time (Integer_64 (V.High) * 2 ** 32 + Integer_64 (V.Low));
      return Res;
   end Vhpi_Time_To_Time;


   ----------------------------------------------------------------------------
   -- VHPI functions
   ----------------------------------------------------------------------------

   -- Internal implementations for variadic functions in grt-cvhpi.c

   function Vhpi_Assert_Internal (Severity: Integer; Msg : Ghdl_C_String)
                                 return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_assert (");
         Trace (Severity);
         Trace (", ");
         Trace (Msg);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_assert");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end Vhpi_Assert_Internal;

   function Vhpi_Control_Internal (CommandInt : Integer; Status : Integer)
                                  return Integer
   is
      procedure Trace (C : VhpiSimControlT) is
      begin
         case C is
            when VhpiStop =>
               Trace ("vhpiStop");
            when VhpiFinish =>
               Trace ("vhpiFinish");
            when VhpiReset =>
               Trace ("vhpiReset");
         end case;
      end Trace;

      procedure Get_Command
        (C : Integer; Res : out VhpiSimControlT; Error : out AvhpiErrorT)
      is
         Ef : constant Integer := VhpiSimControlT'Pos (VhpiSimControlT'First);
         El : constant Integer := VhpiSimControlT'Pos (VhpiSimControlT'Last);
      begin
         Error := AvhpiErrorOk;
         if C not in Ef .. El then
            Error := AvhpiErrorBadEnumVal;
            Res := VhpiSimControlT'First;
            return;
         end if;
         Res := VhpiSimControlT'Val(C);
      end Get_Command;

      Command : VhpiSimControlT;
      Err : AvhpiErrorT;
   begin
      Get_Command (CommandInt, Command, Err);
      if Flag_Trace then
         Trace_Start ("vhpi_control (");
         if Err = AvhpiErrorOk then
            Trace (Command);
         else
            Trace (CommandInt);
            Trace (" {invalid command}");
         end if;
         Trace (", ");
         Trace (Status);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_control");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end Vhpi_Control_Internal;

   ----------------------------------------------------------------------------
   -- Callback related

   -- vhpiHandleT vhpi_register_cb (vhpiCbDataT *cb_data_p, int32_t flags)
   function vhpi_register_cb (Data : VhpiCbData_Access; Flags : Callback_Flags)
                             return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_register_cb (");
         if Data = null then
            Trace (System.Null_Address);
         else
            Trace ("{reason=");
            -- TODO: Add callback reason string
            Trace (Data.Reason);
            if Data.Time /= null then
               Trace (", time=");
               Trace_Time (Vhpi_Time_To_Time (Data.Time.all));
            end if;
            Trace ("}");
         end if;
         Trace (", ");
         Trace (Integer_32 (Flags));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_register_cb");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_register_cb;

   -- int vhpi_remove_cb (vhpiHandleT cb_obj)
   function vhpi_remove_cb (Cb : Vhpi_External_Handle) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_remove_cb (");
         Trace (Cb);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_remove_cb");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_remove_cb;

   -- int vhpi_disable_cb (vhpiHandleT cb_obj)
   function vhpi_disable_cb (Cb : Vhpi_External_Handle) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_disable_cb (");
         Trace (Cb);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_disable_cb");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_disable_cb;

   -- int vhpi_enable_cb (vhpiHandleT cb_obj)
   function vhpi_enable_cb (Cb : Vhpi_External_Handle) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_enable_cb (");
         Trace (Cb);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_enable_cb");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_enable_cb;

   -- int vhpi_get_cb_info (vhpiHandleT object, vhpiCbDataT *cb_data_p)
   function vhpi_get_cb_info
     (Obj : Vhpi_External_Handle; Data : VhpiCbData_Access) return Integer
   is
      function To_Address is
         new Ada.Unchecked_Conversion (VhpiCbData_Access, System.Address);
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_cb_info (");
         Trace (Obj);
         Trace (", ");
         Trace (To_Address (Data));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_cb_info");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_get_cb_info;

   ----------------------------------------------------------------------------
   -- For obtaining handles

   -- vhpiHandleT vhpi_handle_by_name (const char *name, vhpiHandleT scope)
   function vhpi_handle_by_name
     (Name : Ghdl_C_String; Scope : Vhpi_External_Handle)
     return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_handle_by_name (");
         Trace (Name);
         Trace (", ");
         Trace (Scope);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_handle_by_name");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_handle_by_name;

   -- vhpiHandleT vhpi_handle_by_index (vhpiOneToManyT itRel,
   --                                   vhpiHandleT parent, int32_t indx)
   function vhpi_handle_by_index
     (Rel : Integer; Parent : Vhpi_External_Handle; Index: Integer)
     return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_handle_by_index (");
         Trace (Integer_32 (Rel));
         Trace (", ");
         Trace (Parent);
         Trace (", ");
         Trace (Integer_32 (Index));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_handle_by_index");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_handle_by_index;

   ----------------------------------------------------------------------------
   -- For traversing relationships

   -- vhpiHandleT vhpi_handle (vhpiOneToOneT type, vhpiHandleT referenceHandle)
   function vhpi_handle (Rel: Integer; Ref: Vhpi_External_Handle)
                        return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_handle (");
         Trace (Integer_32 (Rel));
         Trace (", ");
         Trace (Ref);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_handle");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_handle;

   -- vhpiHandleT vhpi_iterator (vhpiOneToManyT type,
   --                            vhpiHandleT referenceHandle)
   function vhpi_iterator (Rel: Integer; Ref: Vhpi_External_Handle)
                          return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_iterator (");
         Trace (Integer_32 (Rel));
         Trace (", ");
         Trace (Ref);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_iterator");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_iterator;

   -- vhpiHandleT vhpi_scan (vhpiHandleT iterator)
   function vhpi_scan (Iter : Vhpi_External_Handle)
                      return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_scan (");
         Trace (Iter);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_scan");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_scan;

   ----------------------------------------------------------------------------
   -- For processing properties

   -- vhpiIntT vhpi_get (vhpiIntPropertyT property, vhpiHandleT object)
   function vhpi_get (Property: Integer; Ref: Vhpi_External_Handle)
                     return VhpiIntT is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get (");
         Trace (Integer_32 (Property));
         Trace (", ");
         Trace (Ref);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get");

      if Flag_Trace then
         Trace (Integer_32 (VhpiUndefined_External));
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return VhpiIntT (VhpiUndefined_External);
   end vhpi_get;

   -- const vhpiCharT * vhpi_get_str (vhpiStrPropertyT property,
   --                                 vhpiHandleT object)
   function vhpi_get_str (Property : Integer; Ref : Vhpi_External_Handle)
                         return Ghdl_C_String is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_str (");
         Trace (Integer_32 (Property));
         Trace (", ");
         Trace (Ref);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_str");

      if Flag_Trace then
         Trace (Ghdl_C_String'(null));
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return null;
   end vhpi_get_str;

   -- vhpiRealT vhpi_get_real (vhpiRealPropertyT property, vhpiHandleT object)
   function vhpi_get_real (Property : Integer; Ref : Vhpi_External_Handle)
                          return Ghdl_Real is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_real (");
         Trace (Integer_32 (Property));
         Trace (", ");
         Trace (Ref);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_real");

      if Flag_Trace then
         Trace ("0.0 [not implemented]");
         Trace_Newline;
      end if;
      return 0.0;
   end vhpi_get_real;

   -- vhpiPhysT vhpi_get_phys (vhpiPhysPropertyT property, vhpiHandleT object)
   function vhpi_get_phys (Property : Integer; Ref : Vhpi_External_Handle)
                          return VhpiPhysT
   is
      procedure Trace (V : VhpiPhysT) is
      begin
         Put (Trace_File, "{high = ");
         Put_I32 (Trace_File, Ghdl_I32 (V.High));
         Put (Trace_File, ", low = ");
         Put_I32 (Trace_File, Ghdl_I32 (V.Low));
         Put (Trace_File, '}');
      end Trace;

      Res : constant VhpiPhysT := (0, 0);
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_phys (");
         Trace (Integer_32 (Property));
         Trace (", ");
         Trace (Ref);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_phys");

      if Flag_Trace then
         Trace (Res);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Res;
   end vhpi_get_phys;

   ----------------------------------------------------------------------------
   -- For access to protected types

   -- int vhpi_protected_call (vhpiHandleT varHdl,
   --                          vhpiUserFctT userFct,
   --                          void *userData)
   function vhpi_protected_call (Var : Vhpi_External_Handle;
                                 User_Fun : VhpiUserFctT;
                                 User_Data : System.Address)
                                return Integer
   is
      function To_Address is new Ada.Unchecked_Conversion
        (VhpiUserFctT, System.Address);
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_protected_call (");
         Trace (Var);
         Trace (", ");
         Trace (To_Address (User_Fun));
         Trace (", ");
         Trace (User_Data);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_protected_call");

      if Flag_Trace then
         Trace ("-1 [not implemented]");
         Trace_Newline;
      end if;
      return -1;
   end vhpi_protected_call;

   ----------------------------------------------------------------------------
   -- For value processing

   function To_Address is new Ada.Unchecked_Conversion
     (VhpiValue_Access, System.Address);

   function To_Address is new Ada.Unchecked_Conversion
     (VhpiTime_Access, System.Address);

   -- int vhpi_get_value (vhpiHandleT expr, vhpiValueT *value_p)
   function vhpi_get_value
     (Expr : Vhpi_External_Handle; Value : VhpiValue_Access) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_value (");
         Trace (Expr);
         Trace (", ");
         Trace (To_Address (Value));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_value");

      if Flag_Trace then
         Trace ("-1 [not implemented]");
         Trace_Newline;
      end if;
      return -1;
   end vhpi_get_value;

   -- int vhpi_put_value (vhpiHandleT object,
   --                     vhpiValueT *value_p,
   --                     vhpiPutValueModeT mode)
   function vhpi_put_value (Obj : Vhpi_External_Handle;
                            Value : VhpiValue_Access;
                            ModeInt : Integer)
                           return Integer
   is
      procedure Trace (M : VhpiPutValueModeT) is
      begin
         case M is
            when VhpiDeposit =>
               Trace ("vhpiDeposit");
            when VhpiDepositPropagate =>
               Trace ("vhpiDepositPropagate");
            when VhpiForce =>
               Trace ("vhpiForce");
            when VhpiForcePropagate =>
               Trace ("vhpiForcePropagate");
            when VhpiRelease =>
               Trace ("vhpiRelease");
            when VhpiSizeConstraint =>
               Trace ("vhpiSizeConstraint");
         end case;
      end Trace;

      procedure Get_Mode
        (M : Integer; Res : out VhpiPutValueModeT; Error : out AvhpiErrorT)
      is
         Ef : constant Integer :=
           VhpiPutValueModeT'Pos (VhpiPutValueModeT'First);
         El : constant Integer :=
           VhpiPutValueModeT'Pos (VhpiPutValueModeT'Last);
      begin
         Error := AvhpiErrorOk;
         if M not in Ef .. El then
            Error := AvhpiErrorBadEnumVal;
            Res := VhpiPutValueModeT'First;
            return;
         end if;
         Res := VhpiPutValueModeT'Val(M);
      end Get_Mode;

      Mode : VhpiPutValueModeT;
      Err : AvhpiErrorT;
   begin
      Get_Mode (ModeInt, Mode, Err);
      if Flag_Trace then
         Trace_Start ("vhpi_put_value (");
         Trace (Obj);
         Trace (", ");
         -- TODO: Print value
         Trace (To_Address (Value));
         Trace (", ");
         if Err = AvhpiErrorOk then
            Trace (Mode);
         else
            Trace (ModeInt);
            Trace (" {invalid mode}");
         end if;
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_put_value");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_put_value;

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
                                      return Integer
   is
      procedure Trace (V : Unsigned_32) is
      begin
         Put_U32 (Trace_File, Ghdl_U32 (V));
      end Trace;

      procedure Trace (M : VhpiDelayModeT) is
      begin
         case M is
            when VhpiInertial =>
               Trace ("vhpiInertial");
            when VhpiTransport =>
               Trace ("vhpiTransport");
         end case;
      end Trace;

      procedure Get_Mode
        (M : Integer; Res : out VhpiDelayModeT; Error : out AvhpiErrorT)
      is
         Ef : constant Integer := VhpiDelayModeT'Pos (VhpiDelayModeT'First);
         El : constant Integer := VhpiDelayModeT'Pos (VhpiDelayModeT'Last);
      begin
         Error := AvhpiErrorOk;
         if M not in Ef .. El then
            Error := AvhpiErrorBadEnumVal;
            Res := VhpiDelayModeT'First;
            return;
         end if;
         Res := VhpiDelayModeT'Val(M);
      end Get_Mode;

      Delay_Mode : VhpiDelayModeT;
      Err : AvhpiErrorT;
   begin
      Get_Mode (Delay_ModeInt, Delay_Mode, Err);
      if Flag_Trace then
         Trace_Start ("vhpi_schedule_transaction (");
         Trace (Driver);
         Trace (", ");
         -- TODO: Print value
         Trace (To_Address (Value));
         Trace (", ");
         Trace (Num_Values);
         Trace (", ");
         if Delay_Value /= null then
            Trace ("{");
            Trace_Time (Vhpi_Time_To_Time (Delay_Value.all));
            Trace ("}");
         else
            Trace (To_Address (Delay_Value));
         end if;
         Trace (", ");
         if Err = AvhpiErrorOk then
            Trace (Delay_Mode);
         else
            Trace (Delay_ModeInt);
            Trace (" {invalid mode}");
         end if;
         Trace (", ");
         if Pulse_Rejection /= null then
            Trace ("{");
            Trace_Time (Vhpi_Time_To_Time (Pulse_Rejection.all));
            Trace ("}");
         else
            Trace (To_Address (Pulse_Rejection));
         end if;
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_schedule_transaction");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_schedule_transaction;

   -- int vhpi_format_value (const vhpiValueT *in_value_p,
   --                        vhpiValueT *out_value_p)
   function vhpi_format_value
     (In_Val : VhpiValue_Access; Out_Val : VhpiValue_Access) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_format_value (");
         -- TODO: Print value
         Trace (To_Address (In_Val));
         Trace (", ");
         -- TODO: Print output format
         Trace (To_Address (Out_Val));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_format_value");

      if Flag_Trace then
         Trace ("-1 [not implemented]");
         Trace_Newline;
      end if;
      return -1;
   end vhpi_format_value;

   ----------------------------------------------------------------------------
   -- For time processing

   -- void vhpi_get_time (vhpiTimeT *time_p, long *cycles)
   procedure vhpi_get_time (Time : VhpiTime_Access; Cycles : Long_Access)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Long_Access, System.Address);
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_time (");
         Trace (To_Address (Time));
         Trace (", ");
         Trace (To_Address (Cycles));
         Trace (") ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_time");

      if Flag_Trace then
         Trace ("[not implemented]");
         Trace_Newline;
      end if;
   end vhpi_get_time;

   -- int vhpi_get_next_time (vhpiTimeT *time_p)
   function vhpi_get_next_time (Time : VhpiTime_Access) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_next_time (");
         Trace (To_Address (Time));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_next_time");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_get_next_time;

   ----------------------------------------------------------------------------
   -- Utilities to print VHDL strings

   -- int vhpi_is_printable ( char ch )
   function vhpi_is_printable (Ch : Character) return Integer
   is
      procedure Trace (C : Character) is
      begin
         Put (Trace_File, C);
      end Trace;
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_is_printable (");
         Trace (Ch);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_is_printable");

      if Flag_Trace then
         Trace ("0 [not implemented]");
         Trace_Newline;
      end if;
      return 0;
   end vhpi_is_printable;

   ----------------------------------------------------------------------------
   -- Utility routines

   -- int vhpi_compare_handles (vhpiHandleT handle1, vhpiHandleT handle2)
   function vhpi_compare_handles (Hdl1, Hdl2 : Vhpi_External_Handle)
                                 return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_compare_handles (");
         Trace (Hdl1);
         Trace (", ");
         Trace (Hdl2);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_compare_handles");

      if Flag_Trace then
         Trace ("0 [not implemented]");
         Trace_Newline;
      end if;
      return 0;
   end vhpi_compare_handles;

   -- int vhpi_check_error (vhpiErrorInfoT *error_info_p)
   function vhpi_check_error (Info : VhpiErrorInfo_Access) return Integer
   is
      function To_Address is new Ada.Unchecked_Conversion
        (VhpiErrorInfo_Access, System.Address);

      function To_Integer (B : Boolean) return Integer is
      begin
         if B then
            return 1;
         else
            return 0;
         end if;
      end To_Integer;

      Res : Integer;
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_check_error (");
         Trace (To_Address (Info));
         Trace (") return ");
      end if;

      if Info /= null then
         Info.all := (Severity => Err_Severity,
                      Msg => Err_Message,
                      Str => Err_Str,
                      File => Err_File,
                      Line => Err_Line);
      end if;

      Res := To_Integer (Err_Occured);

      if Flag_Trace then
         Trace (Res);
         Trace_Newline;
      end if;
      return Res;
   end vhpi_check_error;

   -- int vhpi_release_handle (vhpiHandleT object)
   function vhpi_release_handle (Obj : Vhpi_External_Handle) return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_release_handle (");
         Trace (To_Address (Obj));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_release_handle");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_release_handle;

   ----------------------------------------------------------------------------
   -- Creation functions
   -- vhpiHandleT vhpi_create (vhpiClassKindT kind,
   --                          vhpiHandleT handle1,
   --                          vhpiHandleT handle2)
   function vhpi_create (Kind : Integer; Hdl1, Hdl2 : Vhpi_External_Handle)
                        return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_create (");
         Trace (Kind);
         Trace (", ");
         Trace (Hdl1);
         Trace (", ");
         Trace (Hdl2);
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_create");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_create;

   ----------------------------------------------------------------------------
   -- Foreign model data structures and functions

   function To_Address is new Ada.Unchecked_Conversion
     (VhpiForeignData_Access, System.Address);

   -- vhpiHandleT vhpi_register_foreignf (vhpiForeignDataT *foreignDatap)
   function vhpi_register_foreignf (Data : VhpiForeignData_Access)
                                   return Vhpi_External_Handle is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_register_foreignf (");
         -- TODO: Print foreign model info
         Trace (To_Address (Data));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_register_foreignf");

      if Flag_Trace then
         Trace (Null_External_Handle);
         Trace (" [not implemented]");
         Trace_Newline;
      end if;
      return Null_External_Handle;
   end vhpi_register_foreignf;

   -- int vhpi_get_foreignf_info (vhpiHandleT hdl,
   --                             vhpiForeignDataT *foreignDatap)
   function vhpi_get_foreignf_info
     (Hdl : Vhpi_External_Handle; Data : VhpiForeignData_Access)
     return Integer is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_foreignf_info (");
         Trace (Hdl);
         Trace (", ");
         Trace (To_Address (Data));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_foreignf_info");

      if Flag_Trace then
         Trace ("1 [not implemented]");
         Trace_Newline;
      end if;
      return 1;
   end vhpi_get_foreignf_info;

   ----------------------------------------------------------------------------
   -- For saving and restoring foreign models data

   -- size_t vhpi_get_data (int32_t id, void *dataLoc, size_t numBytes);
   function vhpi_get_data
     (Id : Integer_32; Data_Loc : System.Address; Num_Bytes : size_t)
     return size_t is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_get_data (");
         Trace (Id);
         Trace (", ");
         Trace (Data_Loc);
         Trace (", ");
         Trace (Unsigned_64 (Num_Bytes));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_get_data");

      if Flag_Trace then
         Trace ("0 [not implemented]");
         Trace_Newline;
      end if;
      return 0;
   end vhpi_get_data;

   -- size_t vhpi_put_data (int32_t id, void *dataLoc, size_t numBytes);
   function vhpi_put_data
     (Id : Integer_32; Data_Loc : System.Address; Num_Bytes : size_t)
     return size_t is
   begin
      if Flag_Trace then
         Trace_Start ("vhpi_put_data (");
         Trace (Id);
         Trace (", ");
         Trace (Data_Loc);
         Trace (", ");
         Trace (Unsigned_64 (Num_Bytes));
         Trace (") return ");
      end if;
      Reset_Error;

      Error_Unimplimented ("vhpi_put_data");

      if Flag_Trace then
         Trace ("0 [not implemented]");
         Trace_Newline;
      end if;
      return 0;
   end vhpi_put_data;


   ----------------------------------------------------------------------------
   -- GHDL hooks
   ----------------------------------------------------------------------------

   type Lib_Cell;
   type Lib_Access is access Lib_Cell;

   type Lib_Cell is record
      File_Name : String_Access;
      Func_Name : String_Access;
      Next : Lib_Access;
   end record;

   Vhpi_Libraries : Lib_Access := null;

   procedure Vhpi_Help is
   begin
      Put_Line
        (" --vhpi=FILENAME[:ENTRYPOINT]  load VHPI library, optionally");
      Put_Line
        ("                               provide entry point name");
      Put_Line
        (" --vhpi-trace[=FILE]  trace vhpi calls to stdout or provided FILE");
   end Vhpi_Help;

   ------------------------------------------------------------------------
   --  Return TRUE if OPT is an option for VHPI.
   function Vhpi_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;

      procedure Bad_Option is
      begin
         Error_S ("incorrect option '");
         Diag_C (Opt);
         Error_E ("'");
      end Bad_Option;
   begin
      if Opt'Length < 6 or else Opt (F .. F + 5) /= "--vhpi" then
         return False;
      end if;
      if Opt'Length = 7 then
         Bad_Option;
         return False;
      end if;
      if Opt'Length > 7 and then Opt (F + 6) = '=' then
         -- Need to support Windows path names and optional entrypoint.
         -- Valid examples:
         -- C:\vhpi_lib.dll:entry_func
         -- .\vhpi_lib.dll:entry_func
         -- ./vhpi_lib.so:entry_func
         -- /path/to/vhpi_lib:entry_func
         -- vhpi_lib:entry_func
         declare
            P : Natural;
            Lf, Ll, L_Len : Natural;
            Ef, El, E_Len : Natural;
            Lib : Lib_Access;
            File : String_Access;
            Func : String_Access;
         begin
            P := F + 7;
            -- Extract library.
            Lf := P;
            while P <= Opt'Length and then Opt (P) /= ':' loop
               P := P + 1;
            end loop;
            -- Skip colon after volume/drive letter on Windows.
            -- This will break if library path is one character.
            if P <= Opt'Length and then Opt (P) = ':' and then P = 2 then
               while P <= Opt'Length and then Opt (P) /= ':' loop
                  P := P + 1;
               end loop;
            end if;
            Ll := P - 1;

            -- Extract entrypoint.
            Ef := P + 1;
            El := Opt'Length;

            -- Store library info.
            Lib := new Lib_Cell;
            --  Add an extra NUL character.
            L_Len := Ll - Lf + 2;
            File := new String (1 .. L_Len);
            File (1 .. L_Len - 1) := Opt (Lf .. Ll);
            File (File'Last) := NUL;
            Lib.File_Name := File;
            if Ef <= El then
               --  Add an extra NUL character.
               E_Len := El - Ef + 2;
               Func := new String (1 .. E_Len);
               Func (1 .. E_Len - 1) := Opt (Ef .. El);
               Func (Func'Last) := NUL;
               Lib.Func_Name := Func;
            end if;

            -- Add new library to the list.
            if Vhpi_Libraries = null then
               Vhpi_Libraries := Lib;
            else
               declare
                  L : Lib_Access := Vhpi_Libraries;
               begin
                  while L.Next /= null loop
                     L := L.Next;
                  end loop;
                  L.Next := Lib;
               end;
            end if;
         end;
         return True;
      elsif Opt'Length >= 12 and then Opt (F + 6 .. F + 11) = "-trace" then
         if Opt'Length > 12 and then Opt (F + 12) = '=' then
            declare
               Filename : String (1 .. Opt'Length - 12);
               Mode : constant String := "wt" & NUL;
            begin
               Filename (1 .. Filename'Last - 1) := Opt (F + 13 .. Opt'Last);
               Filename (Filename'Last) := NUL;
               Trace_File := fopen (Filename'Address, Mode'Address);
               if Trace_File = NULL_Stream then
                  Error_S ("cannot open vhpi trace file '");
                  Diag_C (Opt (F + 13 .. Opt'Last));
                  Error_E ("'");
                  return False;
               end if;
            end;
         elsif Opt'Length = 12 then
            Trace_File := stdout;
         else
            Bad_Option;
            return False;
         end if;
         Flag_Trace := True;
         return True;
      else
         return False;
      end if;
   end Vhpi_Option;

   ------------------------------------------------------------------------
   --  Called before elaboration.

   -- int loadVhpiModule (const char* libname, const char* entrypoint)
   function LoadVhpiModule (Filename, Funcname: Address) return Integer;
   pragma Import (C, LoadVhpiModule, "loadVhpiModule");

   procedure Vhpi_Init
   is
      Lib : Lib_Access := Vhpi_Libraries;
      Res : Integer;
   begin
      if Lib = null then
         return;
      end if;
      while Lib /= null loop
         if Lib.Func_Name = null then
            Res := LoadVhpiModule
              (Lib.File_Name.all'Address, Null_Address);
         else
            Res := LoadVhpiModule
              (Lib.File_Name.all'Address, Lib.Func_Name.all'Address);
         end if;

         if Res /= 0 then
            Error_S ("cannot load VHPI module '");
            Diag_C (Lib.File_Name.all);
            if Lib.Func_Name /= null then
               Diag_C ("' with entry point '");
               Diag_C (Lib.Func_Name.all);
            end if;
            Error_E ("'");
         end if;
         Lib := Lib.Next;
      end loop;
   end Vhpi_Init;

   ------------------------------------------------------------------------
   --  Called after elaboration.
   procedure Vhpi_Start is
   begin
      if Vhpi_Libraries = null then
         return;
      end if;

      -- Grt.Rtis_Types.Search_Types_RTI;
      -- Execute_Callback_List (VhpiCbStartOfSimulation_List);
   end Vhpi_Start;

   ------------------------------------------------------------------------
   --  Called at the end of the simulation.
   procedure Vhpi_End is
   begin
      -- Execute_Callback_List (VhpiCbEndOfSimulation_List);
      Free (Buf_Err_Message);
      null;
   end Vhpi_End;

   Vhpi_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("vhpi: vhpi compatible API"),
      Option => Vhpi_Option'Access,
      Help => Vhpi_Help'Access,
      Init => Vhpi_Init'Access,
      Start => Vhpi_Start'Access,
      Finish => Vhpi_End'Access);

   procedure Register is
   begin
      Register_Hooks (Vhpi_Hooks'Access);
   end Register;

end Grt.Vhpi;
