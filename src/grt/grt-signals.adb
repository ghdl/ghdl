--  GHDL Run Time (GRT) - signals management.
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
with System; use System;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Ada.Unchecked_Deallocation;

with Grt.Errors; use Grt.Errors;
with Grt.Errors_Exec; use Grt.Errors_Exec;
with Grt.Processes; use Grt.Processes;
with Grt.Options; use Grt.Options;
with Grt.Disp_Signals;
with Grt.Astdio;
with Grt.Stdio;
with Grt.Threads; use Grt.Threads;

package body Grt.Signals is
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Transaction, Name => Transaction_Acc);

   procedure Free_In (Trans : Transaction_Acc)
   is
      Ntrans : Transaction_Acc;
   begin
      Ntrans := Trans;
      Free (Ntrans);
   end Free_In;
   pragma Inline (Free_In);

   --  Signal mode (and flags) for the current signal.
   Sig_Mode : Mode_Signal_Type;
   Sig_Has_Active : Boolean;
   Sig_Kind : Kind_Signal_Type;

   --  Last created implicit signal.  This is used to add dependencies on
   --  the prefix.
   Last_Implicit_Signal : Ghdl_Signal_Ptr;

   --  Current signal resolver.
   Current_Resolv : Resolved_Signal_Acc := null;

   function Get_Current_Mode_Signal return Mode_Signal_Type is
   begin
      return Sig_Mode;
   end Get_Current_Mode_Signal;

   procedure Assign
     (Targ : out Value_Union; Val : Ghdl_Value_Ptr; Mode : Mode_Type) is
   begin
      case Mode is
         when Mode_B1 =>
            Targ.B1 := Val.B1;
         when Mode_E8 =>
            Targ.E8 := Val.E8;
         when Mode_E32 =>
            Targ.E32 := Val.E32;
         when Mode_I32 =>
            Targ.I32 := Val.I32;
         when Mode_I64 =>
            Targ.I64 := Val.I64;
         when Mode_F64 =>
            Targ.F64 := Val.F64;
      end case;
   end Assign;

   function Read_Value (Value_Ptr : Ghdl_Value_Ptr; Mode : Mode_Type)
     return Value_Union is
   begin
      case Mode is
         when Mode_B1 =>
            return (Mode => Mode_B1, B1 => Value_Ptr.B1);
         when Mode_E8 =>
            return (Mode => Mode_E8, E8 => Value_Ptr.E8);
         when Mode_E32 =>
            return (Mode => Mode_E32, E32 => Value_Ptr.E32);
         when Mode_I32 =>
            return (Mode => Mode_I32, I32 => Value_Ptr.I32);
         when Mode_I64 =>
            return (Mode => Mode_I64, I64 => Value_Ptr.I64);
         when Mode_F64 =>
            return (Mode => Mode_F64, F64 => Value_Ptr.F64);
      end case;
   end Read_Value;

   --  For direct drivers, only a pointer is available and it may not be
   --  aligned.  Hence this version of Assign.
   procedure Assign
     (Targ : Ghdl_Value_Ptr; Val : Ghdl_Value_Ptr; Mode : Mode_Type) is
   begin
      case Mode is
         when Mode_B1 =>
            Targ.B1 := Val.B1;
         when Mode_E8 =>
            Targ.E8 := Val.E8;
         when Mode_E32 =>
            Targ.E32 := Val.E32;
         when Mode_I32 =>
            Targ.I32 := Val.I32;
         when Mode_I64 =>
            Targ.I64 := Val.I64;
         when Mode_F64 =>
            Targ.F64 := Val.F64;
      end case;
   end Assign;

   procedure Ghdl_Signal_Name_Rti (Sig : Ghdl_Rti_Access;
                                   Ctxt : Ghdl_Rti_Access;
                                   Addr : Address)
   is
      pragma Unreferenced (Ctxt);
      pragma Unreferenced (Addr);

      Sig_Rti : constant Ghdl_Rtin_Object_Acc := To_Ghdl_Rtin_Object_Acc (Sig);
   begin
      Sig_Mode := Mode_Signal_Type'Val
        (Sig.Mode and Ghdl_Rti_Signal_Mode_Mask);
      Sig_Kind := Kind_Signal_Type'Val
        ((Sig.Mode and Ghdl_Rti_Signal_Kind_Mask)
         / Ghdl_Rti_Signal_Kind_Offset);
      Sig_Has_Active :=
        (Sig_Rti.Common.Mode and Ghdl_Rti_Signal_Has_Active) /= 0;
   end Ghdl_Signal_Name_Rti;

   procedure Ghdl_Signal_Set_Mode (Mode : Mode_Signal_Type;
                                   Kind : Kind_Signal_Type;
                                   Has_Active : Boolean) is
   begin
      Sig_Mode := Mode;
      Sig_Kind := Kind;
      Sig_Has_Active := Has_Active;
   end Ghdl_Signal_Set_Mode;

   function Is_Signal_Guarded (Sig : Ghdl_Signal_Ptr) return Boolean is
   begin
      return Sig.Flags.Sig_Kind /= Kind_Signal_No;
   end Is_Signal_Guarded;

   function To_Address is new Ada.Unchecked_Conversion
     (Source => Ghdl_Signal_Ptr, Target => Address);

   function Create_Signal
     (Mode : Mode_Type;
      Value_Ptr : Ghdl_Value_Ptr;
      Mode_Sig : Mode_Signal_Type;
      Resolv_Proc : Resolver_Acc;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
      Resolv : Resolved_Signal_Acc;
      S : Ghdl_Signal_Data (Mode_Sig);
      Init_Val : Value_Union;
   begin
      Sig_Table.Increment_Last;

      if Current_Resolv = null then
         if Resolv_Proc /= null then
            Resolv := new Resolved_Signal_Type'
              (Resolv_Proc => Resolv_Proc,
               Resolv_Inst => Resolv_Inst,
               Resolv_Ptr => Null_Address,
               Sig_Range => (Sig_Table.Last, Sig_Table.Last),
               Disconnect_Time => Bad_Time);
         else
            Resolv := null;
         end if;
      else
         if Resolv_Proc /= null then
            --  Only one resolution function is allowed!
            Internal_Error ("create_signal");
         end if;
         Resolv := Current_Resolv;
         if Current_Resolv.Sig_Range.Last = Sig_Table.Last then
            Current_Resolv := null;
         end if;
      end if;

      case Mode_Sig is
         when Mode_Signal_User =>
            S.Nbr_Drivers := 0;
            S.Drivers := null;
            S.Effective := null;
            S.Resolv := Resolv;
         when Mode_Conv_In
           | Mode_Conv_Out =>
            S.Conv := null;
         when Mode_Stable
           | Mode_Quiet
           | Mode_Delayed =>
            S.Time := 0;
         when Mode_Guard =>
            S.Guard_Func := null;
            S.Guard_Instance := System.Null_Address;
         when Mode_Transaction
           | Mode_End =>
            null;
      end case;

      Init_Val := Read_Value (Value_Ptr, Mode);
      Res := new Ghdl_Signal'(Value_Ptr => Value_Ptr,
                              Driving_Value => Init_Val,
                              Last_Value => Init_Val,
                              --  Note: use -Std_Time'last instead of
                              --  Std_Time'First so that NOW - x'last_event
                              --  returns time'high at initialization!
                              Last_Event => -Std_Time'Last,
                              Last_Active => -Std_Time'Last,
                              Event => False,
                              Active => False,
                              Has_Active => False,

                              Mode => Mode,
                              Flags => (Propag => Propag_None,
                                        Sig_Kind => Sig_Kind,
                                        Is_Direct_Active => False,
                                        Is_Dumped => False,
                                        Is_Drv_Forced => False,
                                        Is_Eff_Forced => False,
                                        Is_Drv_Force_Scheduled => False,
                                        Is_Eff_Force_Scheduled => False,
                                        RO_Event => False,
                                        Implicit_Active_Next => False,
                                        Seen => False),

                              Net => No_Signal_Net,
                              Link => null,
                              Alink => null,
                              Flink => null,

                              Event_List => null,

                              Nbr_Ports => 0,
                              Ports => null,

                              Dump_Table_Idx => 0,

                              S => S);

      if Resolv /= null and then Resolv.Resolv_Ptr = System.Null_Address then
         Resolv.Resolv_Ptr := To_Address (Res);
      end if;

      case Flag_Activity is
         when Activity_All =>
            Res.Has_Active := True;
         when Activity_Minimal =>
            Res.Has_Active := Sig_Has_Active;
         when Activity_None =>
            Res.Has_Active := False;
      end case;

      --  Put the signal in the table.
      Sig_Table.Table (Sig_Table.Last) := Res;

      return Res;
   end Create_Signal;

   procedure Ghdl_Signal_Init (Sig : Ghdl_Signal_Ptr; Val : Value_Union) is
   begin
      Sig.Driving_Value := Val;
      Sig.Last_Value := Val;
   end Ghdl_Signal_Init;

   procedure Ghdl_Signal_Merge_Rti (Sig : Ghdl_Signal_Ptr;
                                    Rti : Ghdl_Rti_Access)
   is
      S_Rti : constant Ghdl_Rtin_Object_Acc := To_Ghdl_Rtin_Object_Acc (Rti);
   begin
      if Flag_Activity = Activity_Minimal then
         if (S_Rti.Common.Mode and Ghdl_Rti_Signal_Has_Active) /= 0 then
            Sig.Has_Active := True;
         end if;
      end if;
   end Ghdl_Signal_Merge_Rti;

   procedure Ghdl_Signal_Create_Resolution (Proc : Resolver_Acc;
                                            Instance : System.Address;
                                            Sig : System.Address;
                                            Nbr_Sig : Ghdl_Index_Type)
   is
   begin
      if Current_Resolv /= null then
         Internal_Error ("Ghdl_Signal_Create_Resolution");
      end if;
      Current_Resolv := new Resolved_Signal_Type'
        (Resolv_Proc => Proc,
         Resolv_Inst => Instance,
         Resolv_Ptr => Sig,
         Sig_Range => (First => Sig_Table.Last + 1,
                       Last => Sig_Table.Last + Sig_Table_Index (Nbr_Sig)),
         Disconnect_Time => Bad_Time);
   end Ghdl_Signal_Create_Resolution;

   procedure Check_New_Source (Sig : Ghdl_Signal_Ptr)
   is
      use Grt.Stdio;
      use Grt.Astdio;
   begin
      if Sig.S.Nbr_Drivers + Sig.Nbr_Ports > 0 then
         if Sig.S.Resolv = null then
            --  LRM 4.3.1.2 Signal Declaration
            --  It is an error if, after the elaboration of a description, a
            --  signal has multiple sources and it is not a resolved signal.
            Put (stderr, "for signal: ");
            Disp_Signals.Put_Signal_Name (stderr, Sig);
            New_Line (stderr);
            Error ("several sources for unresolved signal");
         elsif Sig.S.Mode_Sig = Mode_Buffer and False then
            --  LRM 1.1.1.2  Ports
            --  A BUFFER port may have at most one source.

            --  FIXME: this is not true with VHDL-02.
            --  With VHDL-87/93, should also check that: any actual associated
            --  with a formal buffer port may have at most one source.
            Error ("buffer port which more than one source");
         end if;
      end if;
   end Check_New_Source;

   --  Return TRUE if already present.
   function Ghdl_Signal_Add_Driver (Sign : Ghdl_Signal_Ptr;
                                    Trans : Transaction_Acc)
                                   return Boolean
   is
      Proc : constant Process_Acc := Get_Current_Process;

      type Size_T is mod 2**Standard'Address_Size;

      function Malloc (Size : Size_T) return Driver_Arr_Ptr;
      pragma Import (C, Malloc);

      function Realloc (Ptr : Driver_Arr_Ptr; Size : Size_T)
        return Driver_Arr_Ptr;
      pragma Import (C, Realloc);

      function Size (N : Ghdl_Index_Type) return Size_T is
      begin
         return Size_T (N * Driver_Fat_Array'Component_Size
                        / System.Storage_Unit);
      end Size;
   begin
      if Sign.S.Nbr_Drivers = 0 then
         Check_New_Source (Sign);
         Sign.S.Drivers := Malloc (Size (1));
         Sign.S.Nbr_Drivers := 1;
      else
         -- Do not create a driver twice.
         for I in 0 .. Sign.S.Nbr_Drivers - 1 loop
            if Sign.S.Drivers (I).Proc = Proc then
               return True;
            end if;
         end loop;
         Check_New_Source (Sign);
         Sign.S.Nbr_Drivers := Sign.S.Nbr_Drivers + 1;
         Sign.S.Drivers := Realloc (Sign.S.Drivers, Size (Sign.S.Nbr_Drivers));
      end if;
      Sign.S.Drivers (Sign.S.Nbr_Drivers - 1) :=
        (First_Trans => Trans,
         Last_Trans => Trans,
         Proc => Proc);
      return False;
   end Ghdl_Signal_Add_Driver;

   procedure Ghdl_Process_Add_Driver (Sign : Ghdl_Signal_Ptr)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'(Kind => Trans_Value,
                                Line => 0,
                                Time => 0,
                                Next => null,
                                Val => Read_Value (Sign.Value_Ptr, Sign.Mode));
      if Ghdl_Signal_Add_Driver (Sign, Trans) then
         Free (Trans);
      end if;
   end Ghdl_Process_Add_Driver;

   procedure Ghdl_Process_Add_Port_Driver
     (Sign : Ghdl_Signal_Ptr; Val : Value_Union)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'(Kind => Trans_Value,
                                Line => 0,
                                Time => 0,
                                Next => null,
                                Val => Val);
      if Ghdl_Signal_Add_Driver (Sign, Trans) then
         Free (Trans);
      end if;
   end Ghdl_Process_Add_Port_Driver;

   procedure Ghdl_Signal_Add_Direct_Driver (Sign : Ghdl_Signal_Ptr;
                                            Drv : Ghdl_Value_Ptr)
   is
      Trans : Transaction_Acc;
      Trans1 : Transaction_Acc;
   begin
      --  Create transaction for current driving value.
      Trans := new Transaction'(Kind => Trans_Value,
                                Line => 0,
                                Time => 0,
                                Next => null,
                                Val => Read_Value (Drv, Sign.Mode));
      if Ghdl_Signal_Add_Driver (Sign, Trans) then
         Free (Trans);
         return;
      end if;
      --  Create transaction for the next driving value.
      Trans1 := new Transaction'(Kind => Trans_Direct,
                                 Line => 0,
                                 Time => 0,
                                 Next => null,
                                 Val_Ptr => Drv);
      Sign.S.Drivers (Sign.S.Nbr_Drivers - 1).Last_Trans := Trans1;
      Trans.Next := Trans1;
   end Ghdl_Signal_Add_Direct_Driver;

   procedure Append_Port (Targ : Ghdl_Signal_Ptr; Src : Ghdl_Signal_Ptr)
   is
      type Size_T is new Integer;

      function Malloc (Size : Size_T) return Signal_Arr_Ptr;
      pragma Import (C, Malloc);

      function Realloc (Ptr : Signal_Arr_Ptr; Size : Size_T)
        return Signal_Arr_Ptr;
      pragma Import (C, Realloc);

      function Size (N : Ghdl_Index_Type) return Size_T is
      begin
         return Size_T (N * Ghdl_Signal_Ptr'Size / System.Storage_Unit);
      end Size;
   begin
      if Targ.Nbr_Ports = 0 then
         Targ.Ports := Malloc (Size (1));
         Targ.Nbr_Ports := 1;
      else
         Targ.Nbr_Ports := Targ.Nbr_Ports + 1;
         Targ.Ports := Realloc (Targ.Ports, Size (Targ.Nbr_Ports));
      end if;
      Targ.Ports (Targ.Nbr_Ports - 1) := Src;
   end Append_Port;

   --  Add SRC to port list of TARG, but only if not already in this list.
   procedure Add_Port (Targ : Ghdl_Signal_Ptr; Src : Ghdl_Signal_Ptr)
   is
   begin
      for I in 1 .. Targ.Nbr_Ports loop
         if Targ.Ports (I - 1) = Src then
            return;
         end if;
      end loop;
      Append_Port (Targ, Src);
   end Add_Port;

   procedure Ghdl_Signal_Add_Source (Targ : Ghdl_Signal_Ptr;
                                     Src : Ghdl_Signal_Ptr)
   is
   begin
      Check_New_Source (Targ);
      Append_Port (Targ, Src);
   end Ghdl_Signal_Add_Source;

   procedure Ghdl_Signal_Set_Disconnect (Sign : Ghdl_Signal_Ptr;
                                         Time : Std_Time) is
   begin
      if Sign.S.Resolv = null then
         Internal_Error ("ghdl_signal_set_disconnect: not resolved");
      end if;
      if Sign.S.Resolv.Disconnect_Time /= Bad_Time then
         Error ("disconnection already specified for signal");
      end if;
      if Time < 0 then
         Error ("disconnection time is negative");
      end if;
      Sign.S.Resolv.Disconnect_Time := Time;
   end Ghdl_Signal_Set_Disconnect;

   function Value_Equal (Left, Right : Value_Union; Mode : Mode_Type)
     return Boolean
   is
   begin
      case Mode is
         when Mode_B1 =>
            return Left.B1 = Right.B1;
         when Mode_E8 =>
            return Left.E8 = Right.E8;
         when Mode_E32 =>
            return Left.E32 = Right.E32;
         when Mode_I32 =>
            return Left.I32 = Right.I32;
         when Mode_I64 =>
            return Left.I64 = Right.I64;
         when Mode_F64 =>
            return Left.F64 = Right.F64;
      end case;
   end Value_Equal;

   procedure Error_Trans_Error (Trans : Transaction_Acc);
   pragma No_Return (Error_Trans_Error);

   procedure Error_Trans_Error (Trans : Transaction_Acc) is
   begin
      Error_S ("range check error on signal at ");
      Diag_C (Trans.File);
      Diag_C (':');
      Diag_C (Trans.Line);
      Error_E;
   end Error_Trans_Error;

   function Find_Driver (Sig : Ghdl_Signal_Ptr) return Ghdl_Index_Type
   is
      Proc : Process_Acc;
   begin
      if Sig.S.Drivers = null then
         Error ("assignment to a signal without any driver");
      end if;
      Proc := Get_Current_Process;
      for I in 0 .. Sig.S.Nbr_Drivers - 1 loop
         if Sig.S.Drivers (I).Proc = Proc then
            return I;
         end if;
      end loop;
      Error ("assignment to a signal without a driver for the process");
   end Find_Driver;

   function Get_Driver (Sig : Ghdl_Signal_Ptr) return Driver_Acc
   is
      Proc : Process_Acc;
   begin
      if Sig.S.Drivers = null then
         return null;
      end if;
      Proc := Get_Current_Process;
      for I in 0 .. Sig.S.Nbr_Drivers - 1 loop
         if Sig.S.Drivers (I).Proc = Proc then
            return Sig.S.Drivers (I)'Unrestricted_Access;
         end if;
      end loop;
      return null;
   end Get_Driver;

   --  Return TRUE iff SIG has a future transaction for the current time,
   --  ie iff SIG will be active in the next delta cycle.  This is used to
   --  recompute whether SIG must be in the active chain.  SIG must be a user
   --  signal.
   function Has_Transaction_In_Next_Delta (Sig : Ghdl_Signal_Ptr)
                                          return Boolean  is
   begin
      if Sig.Flags.Is_Direct_Active then
         return True;
      end if;

      for I in 1 .. Sig.S.Nbr_Drivers loop
         declare
            Trans : constant Transaction_Acc :=
              Sig.S.Drivers (I - 1).First_Trans.Next;
         begin
            if Trans.Kind /= Trans_Direct
              and then Trans.Time = Current_Time
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Has_Transaction_In_Next_Delta;

   --  Unique signal used to end lists.  Lists are not null-terminated so that
   --  it is easy to check if a signal is on the list: test if Link is
   --  not null.
   Signal_End_Decl : Ghdl_Signal := (Value_Ptr => null,
                                     Driving_Value => (Mode => Mode_B1,
                                                       B1 => False),
                                     Last_Value => (Mode => Mode_B1,
                                                    B1 => False),
                                     Last_Event => 0,
                                     Last_Active => 0,
                                     Event => False,
                                     Active => False,
                                     Has_Active => False,
                                     Mode => Mode_B1,

                                     Flags => (Propag => Propag_None,
                                               Sig_Kind => Kind_Signal_No,
                                               Is_Direct_Active => False,
                                               Is_Dumped => False,
                                               Is_Drv_Forced => False,
                                               Is_Eff_Forced => False,
                                               Is_Drv_Force_Scheduled => False,
                                               Is_Eff_Force_Scheduled => False,
                                               RO_Event => False,
                                               Implicit_Active_Next => False,
                                               Seen => False),

                                     Net => No_Signal_Net,
                                     Link => null,
                                     Alink => null,
                                     Flink => null,

                                     Event_List => null,

                                     Nbr_Ports => 0,
                                     Ports => null,

                                     Dump_Table_Idx => 0,

                                     S => (Mode_Sig => Mode_End));


   --  Unused but well-known signal which always terminate
   --    signal_active_chain.
   --  As a consequence, every element of the chain has a link field set to
   --  a non-null value (this is of course not true for SIGNAL_END).  This may
   --  be used to quickly check if a signal is in the list.
   --  This signal is not in the signal table.
   Signal_End : constant Ghdl_Signal_Ptr :=
     Signal_End_Decl'Unrestricted_access;

   --  List of signals that will be active in the next delta cycle.  Signals
   --  are inserted on this chain while processes are scheduling a transaction
   --  with a delay of 0.
   Signal_Active_Chain : Ghdl_Signal_Ptr;

   --  List of signals whose 'Active flag must be cleared.  A signal is added
   --  to this list by Mark_Active and removed by Reset_Active_Flag at the
   --  beginning of the next cycle.  This list is null-terminated, as it uses
   --  Alink (instead of Link) to link elements of the list.
   Active_Clear_List : Ghdl_Signal_Ptr := null;

   --  List of signals whose Update flag on the net has to be cleared.
   Update_Clear_Chain : Ghdl_Signal_Ptr;

   --  List of signals which have projected waveforms in the future (beyond
   --  the next delta cycle).
   --  Currently signals are never removed from this list.
   --  TODO: maybe remove signals when they have no transaction in the future?
   Future_List : Ghdl_Signal_Ptr;

   --  Insert SIG on the Future_List, if not already inserted.
   procedure Insert_Future_List (Sig : Ghdl_Signal_Ptr) is
   begin
      if Sig.Flink = null then
         Sig.Flink := Future_List;
         Future_List := Sig;
      end if;
   end Insert_Future_List;

   --  Add SIG in active_chain while the signal is being assigned while
   --  processes are executed.  So SIG has to be considered during the update
   --  phase.
   --
   --  It is also used internally by Run_Propagation to keep the list of nets
   --  whose update flag has to be cleared.
   procedure Insert_Active_Chain (Sig : Ghdl_Signal_Ptr);
   pragma Inline (Insert_Active_Chain);

   procedure Insert_Active_Chain (Sig : Ghdl_Signal_Ptr) is
   begin
      if Sig.Link = null then
         --  Use Grt.Threads.Atomic_Insert ?
         Sig.Link := Signal_Active_Chain;
         Signal_Active_Chain := Sig;
      end if;
   end Insert_Active_Chain;

   procedure Insert_Update_Clear_Chain (Sig : Ghdl_Signal_Ptr) is
   begin
      if Sig.Link = null then
         Sig.Link := Update_Clear_Chain;
         Update_Clear_Chain := Sig;
      end if;
   end Insert_Update_Clear_Chain;

   procedure Ghdl_Signal_Start_Assign (Sign : Ghdl_Signal_Ptr;
                                       Reject : Std_Time;
                                       Trans : Transaction_Acc;
                                       After : Std_Time)
   is
      Assign_Time : Std_Time;
      Drv : constant Ghdl_Index_Type := Find_Driver (Sign);
      Drv_Ptr : constant Driver_Arr_Ptr := Sign.S.Drivers;
      Driver : Driver_Type renames Drv_Ptr (Drv);
   begin
      --  LRM93 8.4.1
      --  It is an error if the time expression in a waveform element
      --  evaluates to a negative value.
      if After < 0 then
         Error ("negative time expression in signal assignment");
      end if;

      if After = 0 then
         --  Put SIGN on the active list if the transaction is scheduled
         --   for the next delta cycle.
         Insert_Active_Chain (Sign);
      else
         --  AFTER > 0.
         --  Put SIGN on the future list.
         Insert_Future_List (Sign);
      end if;

      declare
         --  We don't want an overflow check, it's done manually.
         pragma Suppress (Overflow_Check);
      begin
         Assign_Time := Current_Time + After;
         if Assign_Time < 0 then
            --  Beyond the future
            Free_In (Trans);
            return;
         end if;
      end;

      --  Handle sign as direct driver.
      if Driver.Last_Trans.Kind = Trans_Direct then
         if After /= 0 then
            Internal_Error ("direct assign with non-0 after");
         end if;
         --  FIXME: can be a bound-error too!
         if Trans.Kind = Trans_Value then
            Assign (Driver.Last_Trans.Val_Ptr, Trans.Val'Access, Sign.Mode);
            Free_In (Trans);
         elsif Trans.Kind = Trans_Error then
            Error_Trans_Error (Trans);
         else
            Internal_Error ("direct assign with non-value");
         end if;
         return;
      end if;

      --  LRM93 8.4.1
      --  1. All old transactions that are projected to occur at or after the
      --     time at which the earliest new transaction is projected to occur
      --     are deleted from the projected output waveform.
      if Driver.Last_Trans.Time >= Assign_Time then
         declare
            --  LAST is the last transaction to keep.
            Last : Transaction_Acc;
            Next : Transaction_Acc;
         begin
            Last := Driver.First_Trans;
            --  Find the first transaction to be deleted.
            Next := Last.Next;
            while Next /= null and then Next.Time < Assign_Time loop
               Last := Next;
               Next := Next.Next;
            end loop;
            --  Delete old transactions.
            if Next /= null then
               --  Set the last transaction of the driver.
               Driver.Last_Trans := Last;
               --  Cut the chain.  This is not strickly necessary, since
               --  it will be overriden below, by appending TRANS to the
               --  driver.
               Last.Next := null;
               --  Free removed transactions.
               loop
                  Last := Next.Next;
                  Free (Next);
                  exit when Last = null;
                  Next := Last;
               end loop;
            end if;
         end;
      end if;

      --  2.  The new transaction are then appended to the projected output
      --      waveform in the order of their projected occurence.
      Trans.Time := Assign_Time;
      Driver.Last_Trans.Next := Trans;
      Driver.Last_Trans := Trans;

      --  If the initial delay is inertial delay according to the definitions
      --  of section 8.4, the projected output waveform is further modified
      --  as follows:
      --  1.  All of the new transactions are marked.
      --  2.  An old transaction is marked if the time at which it is projected
      --      to occur is less than the time at which the first new transaction
      --      is projected to occur minus the pulse rejection limit.
      --  3.  For each remaining unmarked, old transaction, the old transaction
      --      is marked if it immediatly precedes a marked transaction and its
      --      value component is the same as that of the marked transaction;
      --  4.  The transaction that determines the current value of the driver
      --      is marked.
      --  5.  All unmarked transactions (all of which are old transactions) are
      --      deleted from the projected output waveform.
      --
      --  GHDL: only transactions that are projected to occur at [T-R, T[
      --  can be deleted (R is the reject time, T is now + after time).
      if Reject > 0 then
         --  LRM93 8.4
         --  It is an error if the pulse rejection limit for any inertially
         --  delayed signal assignment statement is [...] or greater than the
         --  time expression associated with the first waveform element.
         if Reject > After then
            Error ("pulse rejection greater than first waveform delay");
         end if;

         declare
            Prev : Transaction_Acc;
            Next : Transaction_Acc;
         begin
            --  Find the first transaction after the project time less the
            --  rejection time.
            --  PREV will be the last old transaction which is projected to
            --  occur before T - R.
            Prev := Driver.First_Trans;
            loop
               Next := Prev.Next;
               exit when Next.Time >= Assign_Time - Reject;
               Prev := Next;
            end loop;

            --  Scan every transaction until TRANS.  If a transaction value is
            --  different from the TRANS value, then delete all previous
            --  transactions (from T - R to the currently scanned transaction),
            --  since they are not marked.
            while Next /= Trans loop
               if Next.Kind /= Trans.Kind
                 or else
                 (Trans.Kind = Trans_Value
                  and then not Value_Equal (Next.Val, Trans.Val, Sign.Mode))
               then
                  --  NEXT is different from TRANS.
                  --  Delete ]PREV;NEXT].
                  declare
                     D, N : Transaction_Acc;
                  begin
                     D := Prev.Next;
                     Next := Next.Next;
                     Prev.Next := Next;
                     loop
                        N := D.Next;
                        Free (D);
                        exit when N = Next;
                        D := N;
                     end loop;
                  end;
               else
                  Next := Next.Next;
               end if;
            end loop;

            --  A previous assignment (with a 0 after time) may have put this
            --  signal on the active chain.  But maybe this previous
            --  transaction has been removed (due to rejection) and therefore
            --  this signal won't be active at the next delta.  So remove it
            --  from the active chain.  This is a little bit costly (because
            --  the chain is simply linked), but that issue doesn't appear
            --  frequently.
            if Sign.Link /= null
              and then not Has_Transaction_In_Next_Delta (Sign)
            then
               if Signal_Active_Chain = Sign then
                  --  At the head of the chain.
                  --  FIXME: this is not atomic.
                  Signal_Active_Chain := Sign.Link;
               else
                  --  In the middle of the chain.
                  declare
                     Prev : Ghdl_Signal_Ptr := Signal_Active_Chain;
                  begin
                     while Prev.Link /= Sign loop
                        Prev := Prev.Link;
                     end loop;
                     Prev.Link := Sign.Link;
                  end;
               end if;
               Sign.Link := null;
            end if;
         end;
      elsif Reject /= 0 then
         --  LRM93 8.4
         --  It is an error if the pulse rejection limit for any inertially
         --  delayed signal assignment statement is either negative or [...].
         Error ("pulse rejection is negative");
      end if;

      --  Do some checks.
      if Driver.Last_Trans.Next /= null then
         Error ("ghdl_signal_start_assign internal_error");
      end if;
   end Ghdl_Signal_Start_Assign;

   procedure Ghdl_Signal_Next_Assign (Sign : Ghdl_Signal_Ptr;
                                      Val : Value_Union;
                                      After : Std_Time)
   is
      Drv_Ptr : constant Driver_Arr_Ptr := Sign.S.Drivers;
      Driver : Driver_Type renames Drv_Ptr (Find_Driver (Sign));

      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'(Kind => Trans_Value,
                                Line => 0,
                                Time => Current_Time + After,
                                Next => null,
                                Val => Val);
      if Trans.Time <= Driver.Last_Trans.Time then
         Error ("transactions not in ascending order");
      end if;

      pragma Assert (After > 0);

      Insert_Future_List (Sign);

      Driver.Last_Trans.Next := Trans;
      Driver.Last_Trans := Trans;
   end Ghdl_Signal_Next_Assign;

   procedure Ghdl_Signal_Direct_Assign (Sign : Ghdl_Signal_Ptr) is
   begin
      Insert_Active_Chain (Sign);

      --  Must be always set (as Sign.Link may be set by a regular driver).
      Sign.Flags.Is_Direct_Active := True;
   end Ghdl_Signal_Direct_Assign;

   procedure Ghdl_Signal_Simple_Assign_Error (Sign : Ghdl_Signal_Ptr;
                                              File : Ghdl_C_String;
                                              Line : Ghdl_I32)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'(Kind => Trans_Error,
                                Line => Line,
                                Time => 0,
                                Next => null,
                                File => File);
      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_Error;

   procedure Ghdl_Signal_Start_Assign_Error (Sign : Ghdl_Signal_Ptr;
                                             Rej : Std_Time;
                                             After : Std_Time;
                                             File : Ghdl_C_String;
                                             Line : Ghdl_I32)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'(Kind => Trans_Error,
                                Line => Line,
                                Time => 0,
                                Next => null,
                                File => File);
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_Error;

   procedure Ghdl_Signal_Next_Assign_Error (Sign : Ghdl_Signal_Ptr;
                                            After : Std_Time;
                                            File : Ghdl_C_String;
                                            Line : Ghdl_I32)
   is
      Drv_Ptr : constant Driver_Arr_Ptr := Sign.S.Drivers;
      Driver : Driver_Type renames Drv_Ptr (Find_Driver (Sign));

      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'(Kind => Trans_Error,
                                Line => Line,
                                Time => Current_Time + After,
                                Next => null,
                                File => File);
      if Trans.Time <= Driver.Last_Trans.Time then
         Error ("transactions not in ascending order");
      end if;

      pragma Assert (After > 0);
      Insert_Future_List (Sign);

      Driver.Last_Trans.Next := Trans;
      Driver.Last_Trans := Trans;
   end Ghdl_Signal_Next_Assign_Error;

   procedure Ghdl_Signal_Start_Assign_Null (Sign : Ghdl_Signal_Ptr;
                                            Rej : Std_Time;
                                            After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      if not Is_Signal_Guarded (Sign) then
         Error ("null transaction for a non-guarded target");
      end if;
      Trans := new Transaction'(Kind => Trans_Null,
                                Line => 0,
                                Time => 0,
                                Next => null);
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_Null;

   procedure Ghdl_Signal_Disconnect (Sign : Ghdl_Signal_Ptr)
   is
      Trans : Transaction_Acc;
      Time : Std_Time;
   begin
      if not Is_Signal_Guarded (Sign) then
         Error ("null transaction for a non-guarded target");
      end if;
      Trans := new Transaction'(Kind => Trans_Null,
                                Line => 0,
                                Time => 0,
                                Next => null);
      Time := Sign.S.Resolv.Disconnect_Time;
      Ghdl_Signal_Start_Assign (Sign, Time, Trans, Time);
   end Ghdl_Signal_Disconnect;

   function Ghdl_Create_Signal_B1 (Val_Ptr : Ghdl_Value_Ptr;
                                   Resolv_Func : Resolver_Acc;
                                   Resolv_Inst : System.Address)
                                  return Ghdl_Signal_Ptr is
   begin
      return Create_Signal
        (Mode_B1, Val_Ptr, Get_Current_Mode_Signal, Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_B1;

   procedure Ghdl_Signal_Init_B1 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_B1) is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_B1, B1 => Init_Val));
   end Ghdl_Signal_Init_B1;

   procedure Ghdl_Signal_Associate_B1 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_B1) is
   begin
      Sig.Value_Ptr.B1 := Val;
      Sig.Driving_Value.B1 := Val;
   end Ghdl_Signal_Associate_B1;

   procedure Ghdl_Signal_Add_Port_Driver_B1
     (Sig : Ghdl_Signal_Ptr; Val : Ghdl_B1) is
   begin
      Ghdl_Process_Add_Port_Driver
        (Sig, Value_Union'(Mode => Mode_B1, B1 => Val));
   end Ghdl_Signal_Add_Port_Driver_B1;

   procedure Ghdl_Signal_Simple_Assign_B1 (Sign : Ghdl_Signal_Ptr;
                                           Val : Ghdl_B1)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value_Ptr.B1
        and then Sign.S.Drivers (0).First_Trans.Next = null
      then
         return;
      end if;

      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_B1, B1 => Val));

      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_B1;

   procedure Ghdl_Signal_Start_Assign_B1 (Sign : Ghdl_Signal_Ptr;
                                          Rej : Std_Time;
                                          Val : Ghdl_B1;
                                          After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_B1, B1 => Val));
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_B1;

   procedure Ghdl_Signal_Next_Assign_B1 (Sign : Ghdl_Signal_Ptr;
                                         Val : Ghdl_B1;
                                         After : Std_Time)
   is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_B1, B1 => Val), After);
   end Ghdl_Signal_Next_Assign_B1;

   function Ghdl_Create_Signal_E8 (Val_Ptr : Ghdl_Value_Ptr;
                                   Resolv_Func : Resolver_Acc;
                                   Resolv_Inst : System.Address)
                                  return Ghdl_Signal_Ptr is
   begin
      return Create_Signal
        (Mode_E8, Val_Ptr, Get_Current_Mode_Signal, Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_E8;

   procedure Ghdl_Signal_Init_E8 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_E8) is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_E8, E8 => Init_Val));
   end Ghdl_Signal_Init_E8;

   procedure Ghdl_Signal_Associate_E8 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E8) is
   begin
      Sig.Value_Ptr.E8 := Val;
      Sig.Driving_Value.E8 := Val;
   end Ghdl_Signal_Associate_E8;

   procedure Ghdl_Signal_Add_Port_Driver_E8
     (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E8) is
   begin
      Ghdl_Process_Add_Port_Driver
        (Sig, Value_Union'(Mode => Mode_E8, E8 => Val));
   end Ghdl_Signal_Add_Port_Driver_E8;

   procedure Ghdl_Signal_Simple_Assign_E8 (Sign : Ghdl_Signal_Ptr;
                                           Val : Ghdl_E8)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value_Ptr.E8
        and then Sign.S.Drivers (0).First_Trans.Next = null
      then
         return;
      end if;

      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_E8, E8 => Val));

      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_E8;

   procedure Ghdl_Signal_Start_Assign_E8 (Sign : Ghdl_Signal_Ptr;
                                          Rej : Std_Time;
                                          Val : Ghdl_E8;
                                          After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_E8, E8 => Val));
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_E8;

   procedure Ghdl_Signal_Next_Assign_E8 (Sign : Ghdl_Signal_Ptr;
                                         Val : Ghdl_E8;
                                         After : Std_Time) is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_E8, E8 => Val), After);
   end Ghdl_Signal_Next_Assign_E8;

   function Ghdl_Create_Signal_E32 (Val_Ptr : Ghdl_Value_Ptr;
                                    Resolv_Func : Resolver_Acc;
                                    Resolv_Inst : System.Address)
                                   return Ghdl_Signal_Ptr is
   begin
      return Create_Signal
        (Mode_E32, Val_Ptr, Get_Current_Mode_Signal, Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_E32;

   procedure Ghdl_Signal_Init_E32 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_E32)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_E32, E32 => Init_Val));
   end Ghdl_Signal_Init_E32;

   procedure Ghdl_Signal_Associate_E32 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E32)
   is
   begin
      Sig.Value_Ptr.E32 := Val;
      Sig.Driving_Value.E32 := Val;
   end Ghdl_Signal_Associate_E32;

   procedure Ghdl_Signal_Add_Port_Driver_E32
     (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E32) is
   begin
      Ghdl_Process_Add_Port_Driver
        (Sig, Value_Union'(Mode => Mode_E32, E32 => Val));
   end Ghdl_Signal_Add_Port_Driver_E32;

   procedure Ghdl_Signal_Simple_Assign_E32 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_E32)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value_Ptr.E32
        and then Sign.S.Drivers (0).First_Trans.Next = null
      then
         return;
      end if;

      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_E32, E32 => Val));

      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_E32;

   procedure Ghdl_Signal_Start_Assign_E32 (Sign : Ghdl_Signal_Ptr;
                                           Rej : Std_Time;
                                           Val : Ghdl_E32;
                                           After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_E32, E32 => Val));
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_E32;

   procedure Ghdl_Signal_Next_Assign_E32 (Sign : Ghdl_Signal_Ptr;
                                          Val : Ghdl_E32;
                                          After : Std_Time)
   is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_E32, E32 => Val), After);
   end Ghdl_Signal_Next_Assign_E32;

   function Ghdl_Create_Signal_I32
     (Val_Ptr : Ghdl_Value_Ptr;
      Resolv_Func : Resolver_Acc;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_I32, Val_Ptr, Get_Current_Mode_Signal, Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_I32;

   procedure Ghdl_Signal_Init_I32 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_I32)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_I32, I32 => Init_Val));
   end Ghdl_Signal_Init_I32;

   procedure Ghdl_Signal_Associate_I32 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I32)
   is
   begin
      Sig.Value_Ptr.I32 := Val;
      Sig.Driving_Value.I32 := Val;
   end Ghdl_Signal_Associate_I32;

   procedure Ghdl_Signal_Add_Port_Driver_I32
     (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I32) is
   begin
      Ghdl_Process_Add_Port_Driver
        (Sig, Value_Union'(Mode => Mode_I32, I32 => Val));
   end Ghdl_Signal_Add_Port_Driver_I32;

   procedure Ghdl_Signal_Simple_Assign_I32 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I32)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value_Ptr.I32
        and then Sign.S.Drivers (0).First_Trans.Next = null
      then
         return;
      end if;

      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_I32, I32 => Val));

      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_I32;

   procedure Ghdl_Signal_Start_Assign_I32 (Sign : Ghdl_Signal_Ptr;
                                           Rej : Std_Time;
                                           Val : Ghdl_I32;
                                           After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_I32, I32 => Val));
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_I32;

   procedure Ghdl_Signal_Next_Assign_I32 (Sign : Ghdl_Signal_Ptr;
                                          Val : Ghdl_I32;
                                          After : Std_Time)
   is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_I32, I32 => Val), After);
   end Ghdl_Signal_Next_Assign_I32;

   function Ghdl_Create_Signal_I64
     (Val_Ptr : Ghdl_Value_Ptr;
      Resolv_Func : Resolver_Acc;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_I64, Val_Ptr, Get_Current_Mode_Signal, Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_I64;

   procedure Ghdl_Signal_Init_I64 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_I64)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_I64, I64 => Init_Val));
   end Ghdl_Signal_Init_I64;

   procedure Ghdl_Signal_Associate_I64 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I64)
   is
   begin
      Sig.Value_Ptr.I64 := Val;
      Sig.Driving_Value.I64 := Val;
   end Ghdl_Signal_Associate_I64;

   procedure Ghdl_Signal_Add_Port_Driver_I64
     (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I64) is
   begin
      Ghdl_Process_Add_Port_Driver
        (Sig, Value_Union'(Mode => Mode_I64, I64 => Val));
   end Ghdl_Signal_Add_Port_Driver_I64;

   procedure Ghdl_Signal_Simple_Assign_I64 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I64)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value_Ptr.I64
        and then Sign.S.Drivers (0).First_Trans.Next = null
      then
         return;
      end if;

      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_I64, I64 => Val));

      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_I64;

   procedure Ghdl_Signal_Start_Assign_I64 (Sign : Ghdl_Signal_Ptr;
                                           Rej : Std_Time;
                                           Val : Ghdl_I64;
                                           After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_I64, I64 => Val));
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_I64;

   procedure Ghdl_Signal_Next_Assign_I64 (Sign : Ghdl_Signal_Ptr;
                                          Val : Ghdl_I64;
                                          After : Std_Time)
   is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_I64, I64 => Val), After);
   end Ghdl_Signal_Next_Assign_I64;

   function Ghdl_Create_Signal_F64
     (Val_Ptr : Ghdl_Value_Ptr;
      Resolv_Func : Resolver_Acc;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_F64, Val_Ptr, Get_Current_Mode_Signal, Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_F64;

   procedure Ghdl_Signal_Init_F64 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_F64)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_F64, F64 => Init_Val));
   end Ghdl_Signal_Init_F64;

   procedure Ghdl_Signal_Associate_F64 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_F64)
   is
   begin
      Sig.Value_Ptr.F64 := Val;
      Sig.Driving_Value.F64 := Val;
   end Ghdl_Signal_Associate_F64;

   procedure Ghdl_Signal_Add_Port_Driver_F64
     (Sig : Ghdl_Signal_Ptr; Val : Ghdl_F64) is
   begin
      Ghdl_Process_Add_Port_Driver
        (Sig, Value_Union'(Mode => Mode_F64, F64 => Val));
   end Ghdl_Signal_Add_Port_Driver_F64;

   procedure Ghdl_Signal_Simple_Assign_F64 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_F64)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value_Ptr.F64
        and then Sign.S.Drivers (0).First_Trans.Next = null
      then
         return;
      end if;

      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_F64, F64 => Val));

      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_F64;

   procedure Ghdl_Signal_Start_Assign_F64 (Sign : Ghdl_Signal_Ptr;
                                           Rej : Std_Time;
                                           Val : Ghdl_F64;
                                           After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_F64, F64 => Val));
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_F64;

   procedure Ghdl_Signal_Next_Assign_F64 (Sign : Ghdl_Signal_Ptr;
                                          Val : Ghdl_F64;
                                          After : Std_Time)
   is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_F64, F64 => Val), After);
   end Ghdl_Signal_Next_Assign_F64;

   procedure Ghdl_Signal_Internal_Checks
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      for I in Sig_Table.First .. Sig_Table.Last loop
         Sig := Sig_Table.Table (I);

         --  Check drivers.
         case Sig.S.Mode_Sig is
            when Mode_Signal_User =>
               for J in 1 .. Sig.S.Nbr_Drivers loop
                  declare
                     Trans : Transaction_Acc;
                  begin
                     Trans := Sig.S.Drivers (J - 1).First_Trans;
                     while Trans.Next /= null loop
                        if Trans.Next.Time < Trans.Time then
                           Internal_Error ("ghdl_signal_internal_checks: "
                                           & "bad transaction order");
                        end if;
                        Trans := Trans.Next;
                     end loop;
                     if Trans /= Sig.S.Drivers (J - 1).Last_Trans then
                        Internal_Error ("ghdl_signal_internal_checks: "
                                        & "last transaction mismatch");
                     end if;
                  end;
               end loop;
            when others =>
               null;
         end case;
      end loop;
   end Ghdl_Signal_Internal_Checks;

   procedure Ghdl_Signal_Effective_Value (Targ : Ghdl_Signal_Ptr;
                                          Src : Ghdl_Signal_Ptr)
   is
   begin
      if Targ.S.Effective /= null then
         Error ("internal error: already effective value");
      end if;
      Targ.S.Effective := Src;
   end Ghdl_Signal_Effective_Value;

   function Ghdl_Create_Signal_Attribute
     (Val_Ptr : Ghdl_Value_Ptr; Mode : Mode_Signal_Type; Time : Std_Time)
     return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
   begin
      --  Note: bit and boolean are both mode_b1.
      Val_Ptr.B1 := True;
      Res := Create_Signal (Mode_B1, Val_Ptr, Mode, null, Null_Address);
      Last_Implicit_Signal := Res;

      if Mode /= Mode_Transaction then
         Res.S.Time := Time;
         Res.S.Attr_Trans := new Transaction'(Kind => Trans_Value,
                                              Line => 0,
                                              Time => 0,
                                              Next => null,
                                              Val => (Mode => Mode_B1,
                                                      B1 => True));
      end if;

      if Time > 0 then
         --  This signal will have transaction in the future.
         Insert_Future_List (Res);
      end if;

      return Res;
   end Ghdl_Create_Signal_Attribute;

   function Ghdl_Create_Stable_Signal
     (Val_Ptr : Ghdl_Value_Ptr; Val : Std_Time) return Ghdl_Signal_Ptr is
   begin
      return Ghdl_Create_Signal_Attribute (Val_Ptr, Mode_Stable, Val);
   end Ghdl_Create_Stable_Signal;

   function Ghdl_Create_Quiet_Signal
     (Val_Ptr : Ghdl_Value_Ptr; Val : Std_Time) return Ghdl_Signal_Ptr is
   begin
      return Ghdl_Create_Signal_Attribute (Val_Ptr, Mode_Quiet, Val);
   end Ghdl_Create_Quiet_Signal;

   function Ghdl_Create_Transaction_Signal
     (Val_Ptr : Ghdl_Value_Ptr) return Ghdl_Signal_Ptr is
   begin
      return Ghdl_Create_Signal_Attribute (Val_Ptr, Mode_Transaction, 0);
   end Ghdl_Create_Transaction_Signal;

   procedure Ghdl_Signal_Attribute_Register_Prefix (Sig : Ghdl_Signal_Ptr)
   is
   begin
      Add_Port (Last_Implicit_Signal, Sig);
   end Ghdl_Signal_Attribute_Register_Prefix;

   function Ghdl_Signal_Create_Guard
     (Val_Ptr : Ghdl_Value_Ptr; This : System.Address; Proc : Guard_Func_Acc)
     return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
   begin
      Val_Ptr.B1 := Proc.all (This);
      Res := Create_Signal (Mode_B1, Val_Ptr, Mode_Guard, null, Null_Address);
      Res.S.Guard_Func := Proc;
      Res.S.Guard_Instance := This;
      Last_Implicit_Signal := Res;
      return Res;
   end Ghdl_Signal_Create_Guard;

   procedure Ghdl_Signal_Guard_Dependence (Sig : Ghdl_Signal_Ptr)
   is
   begin
      Add_Port (Last_Implicit_Signal, Sig);
      Sig.Has_Active := True;
   end Ghdl_Signal_Guard_Dependence;

   function Ghdl_Create_Delayed_Signal
     (Sig : Ghdl_Signal_Ptr; Val_Ptr : Ghdl_Value_Ptr; Val : Std_Time)
     return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
   begin
      Assign (Val_Ptr, Sig.Value_Ptr, Sig.Mode);
      Res := Create_Signal
        (Sig.Mode, Val_Ptr, Mode_Delayed, null, Null_Address);
      Res.S.Time := Val;
      if Val > 0 then
         Insert_Future_List (Res);
      end if;
      Res.S.Attr_Trans := new Transaction'(Kind => Trans_Value,
                                           Line => 0,
                                           Time => 0,
                                           Next => null,
                                           Val => Read_Value (Val_Ptr,
                                                              Sig.Mode));
      Append_Port (Res, Sig);
      return Res;
   end Ghdl_Create_Delayed_Signal;

   function Signal_Ptr_To_Index (Ptr : Ghdl_Signal_Ptr) return Sig_Table_Index
   is
   begin
      --  Note: we may start from ptr.instance_name.sig_index, but
      --  instance_name is *not* set for conversion signals.
      for I in reverse Sig_Table.First .. Sig_Table.Last loop
         if Sig_Table.Table (I) = Ptr then
            return I;
         end if;
      end loop;
      return -1;
   end Signal_Ptr_To_Index;

   function Ghdl_Signal_Get_Nbr_Ports (Sig : Ghdl_Signal_Ptr)
                                      return Ghdl_Index_Type is
   begin
      return Sig.Nbr_Ports;
   end Ghdl_Signal_Get_Nbr_Ports;

   function Ghdl_Signal_Get_Nbr_Drivers (Sig : Ghdl_Signal_Ptr)
                                        return Ghdl_Index_Type is
   begin
      return Sig.S.Nbr_Drivers;
   end Ghdl_Signal_Get_Nbr_Drivers;

   function Ghdl_Signal_Read_Port
     (Sig : Ghdl_Signal_Ptr; Index : Ghdl_Index_Type)
     return Ghdl_Value_Ptr
   is
   begin
      if Index >= Sig.Nbr_Ports then
         Internal_Error ("ghdl_signal_read_port: bad index");
      end if;
      return To_Ghdl_Value_Ptr (Sig.Ports (Index).Driving_Value'Address);
   end Ghdl_Signal_Read_Port;

   function Ghdl_Signal_Read_Driver
     (Sig : Ghdl_Signal_Ptr; Index : Ghdl_Index_Type)
     return Ghdl_Value_Ptr
   is
      Trans : Transaction_Acc;
   begin
      if Index >= Sig.S.Nbr_Drivers then
         Internal_Error ("ghdl_signal_read_driver: bad index");
      end if;
      Trans := Sig.S.Drivers (Index).First_Trans;
      case Trans.Kind is
         when Trans_Value =>
            return To_Ghdl_Value_Ptr (Trans.Val'Address);
         when Trans_Direct =>
            Internal_Error ("ghdl_signal_read_driver: trans_direct");
         when Trans_Null =>
            return null;
         when Trans_Error =>
            Error_Trans_Error (Trans);
      end case;
   end Ghdl_Signal_Read_Driver;

   procedure Ghdl_Signal_Conversion (Func : System.Address;
                                     Instance : System.Address;
                                     Src : Ghdl_Signal_Ptr;
                                     Src_Len : Ghdl_Index_Type;
                                     Dst : Ghdl_Signal_Ptr;
                                     Dst_Len : Ghdl_Index_Type;
                                     Mode : Mode_Signal_Type)
   is
      Data : Sig_Conversion_Acc;
      Sig : Ghdl_Signal_Ptr;
   begin
      Data := new Sig_Conversion_Type'(Func => Func,
                                       Instance => Instance,
                                       Src => (-1, -1),
                                       Dest => (-1, -1));
      Data.Src.First := Signal_Ptr_To_Index (Src);
      Data.Src.Last := Data.Src.First + Sig_Table_Index (Src_Len) - 1;

      Data.Dest.First := Signal_Ptr_To_Index (Dst);
      Data.Dest.Last := Data.Dest.First + Sig_Table_Index (Dst_Len) - 1;

      --  Convert DEST to new mode.
      for I in Data.Dest.First .. Data.Dest.Last loop
         Sig := Sig_Table.Table (I);
         case Mode is
            when Mode_Conv_In =>
               Sig.S := (Mode_Sig => Mode_Conv_In,
                         Conv => Data);
            when Mode_Conv_Out =>
               Sig.S := (Mode_Sig => Mode_Conv_Out,
                         Conv => Data);
            when others =>
               Internal_Error ("ghdl_signal_conversion");
         end case;
      end loop;
   end Ghdl_Signal_Conversion;

   procedure Ghdl_Signal_In_Conversion (Func : System.Address;
                                        Instance : System.Address;
                                        Src : Ghdl_Signal_Ptr;
                                        Src_Len : Ghdl_Index_Type;
                                        Dst : Ghdl_Signal_Ptr;
                                        Dst_Len : Ghdl_Index_Type)
   is
   begin
      Ghdl_Signal_Conversion
        (Func, Instance, Src, Src_Len, Dst, Dst_Len, Mode_Conv_In);
   end Ghdl_Signal_In_Conversion;

   procedure Ghdl_Signal_Out_Conversion (Func : System.Address;
                                         Instance : System.Address;
                                         Src : Ghdl_Signal_Ptr;
                                         Src_Len : Ghdl_Index_Type;
                                         Dst : Ghdl_Signal_Ptr;
                                         Dst_Len : Ghdl_Index_Type)
   is
   begin
      Ghdl_Signal_Conversion
        (Func, Instance, Src, Src_Len, Dst, Dst_Len, Mode_Conv_Out);
   end Ghdl_Signal_Out_Conversion;

   function Ghdl_Signal_Driving (Sig : Ghdl_Signal_Ptr) return Ghdl_B1
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null then
         --  FIXME: disp signal and process.
         Error ("'driving error: no driver in process for signal");
      end if;
      if Drv.First_Trans.Kind /= Trans_Null then
         return True;
      else
         return False;
      end if;
   end Ghdl_Signal_Driving;

   function Ghdl_Signal_Driving_Value_B1 (Sig : Ghdl_Signal_Ptr) return Ghdl_B1
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null or else Drv.First_Trans.Kind /= Trans_Value then
         Error ("'driving_value: no active driver in process for signal");
      else
         return Drv.First_Trans.Val.B1;
      end if;
   end Ghdl_Signal_Driving_Value_B1;

   function Ghdl_Signal_Driving_Value_E8 (Sig : Ghdl_Signal_Ptr)
                                         return Ghdl_E8
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null or else Drv.First_Trans.Kind /= Trans_Value then
         Error ("'driving_value: no active driver in process for signal");
      else
         return Drv.First_Trans.Val.E8;
      end if;
   end Ghdl_Signal_Driving_Value_E8;

   function Ghdl_Signal_Driving_Value_E32 (Sig : Ghdl_Signal_Ptr)
                                         return Ghdl_E32
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null or else Drv.First_Trans.Kind /= Trans_Value then
         Error ("'driving_value: no active driver in process for signal");
      else
         return Drv.First_Trans.Val.E32;
      end if;
   end Ghdl_Signal_Driving_Value_E32;

   function Ghdl_Signal_Driving_Value_I32 (Sig : Ghdl_Signal_Ptr)
                                          return Ghdl_I32
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null or else Drv.First_Trans.Kind /= Trans_Value then
         Error ("'driving_value: no active driver in process for signal");
      else
         return Drv.First_Trans.Val.I32;
      end if;
   end Ghdl_Signal_Driving_Value_I32;

   function Ghdl_Signal_Driving_Value_I64 (Sig : Ghdl_Signal_Ptr)
                                          return Ghdl_I64
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null or else Drv.First_Trans.Kind /= Trans_Value then
         Error ("'driving_value: no active driver in process for signal");
      else
         return Drv.First_Trans.Val.I64;
      end if;
   end Ghdl_Signal_Driving_Value_I64;

   function Ghdl_Signal_Driving_Value_F64 (Sig : Ghdl_Signal_Ptr)
                                          return Ghdl_F64
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null or else Drv.First_Trans.Kind /= Trans_Value then
         Error ("'driving_value: no active driver in process for signal");
      else
         return Drv.First_Trans.Val.F64;
      end if;
   end Ghdl_Signal_Driving_Value_F64;

   type Force_Kind is (Force, Release);
   type Force_Mode is (Force_Effective, Force_Driving);

   type Force_Value (Kind : Force_Kind);
   type Force_Value_Acc is access Force_Value;

   type Force_Value (Kind : Force_Kind) is record
      Mode : Force_Mode;
      Next : Force_Value_Acc;
      Sig  : Ghdl_Signal_Ptr;
      case Kind is
         when Force =>
            Val : aliased Value_Union;
         when Release =>
            null;
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Force_Value, Force_Value_Acc);

   --  Chain of forced values for the next cycle.
   Force_Value_First : Force_Value_Acc;
   Force_Value_Last : Force_Value_Acc;

   procedure Append_Force_Value (F : Force_Value_Acc) is
   begin
      if Force_Value_First = null then
         Force_Value_First := F;
      else
         Force_Value_Last.Next := F;
      end if;
      Force_Value_Last := F;
   end Append_Force_Value;

   procedure Ghdl_Signal_Release_Eff (Sig : Ghdl_Signal_Ptr) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Release,
                                           Mode => Force_Effective,
                                           Next => null,
                                           Sig => Sig));
   end Ghdl_Signal_Release_Eff;

   procedure Ghdl_Signal_Release_Drv (Sig : Ghdl_Signal_Ptr) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Release,
                                           Mode => Force_Driving,
                                           Next => null,
                                           Sig => Sig));
   end Ghdl_Signal_Release_Drv;

   procedure Ghdl_Signal_Force_Driving_B1 (Sig : Ghdl_Signal_Ptr;
                                           Val : Ghdl_B1) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Driving,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_B1,
                                                   B1 => Val)));
   end Ghdl_Signal_Force_Driving_B1;

   procedure Ghdl_Signal_Force_Effective_B1 (Sig : Ghdl_Signal_Ptr;
                                             Val : Ghdl_B1) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Effective,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_B1,
                                                   B1 => Val)));
   end Ghdl_Signal_Force_Effective_B1;

   procedure Ghdl_Signal_Force_Driving_E8 (Sig : Ghdl_Signal_Ptr;
                                           Val : Ghdl_E8) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Driving,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_E8,
                                                   E8 => Val)));
   end Ghdl_Signal_Force_Driving_E8;

   procedure Ghdl_Signal_Force_Effective_E8 (Sig : Ghdl_Signal_Ptr;
                                             Val : Ghdl_E8) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Effective,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_E8,
                                                   E8 => Val)));
   end Ghdl_Signal_Force_Effective_E8;

   procedure Ghdl_Signal_Force_Driving_E32 (Sig : Ghdl_Signal_Ptr;
                                            Val : Ghdl_E32) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Driving,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_E32,
                                                   E32 => Val)));
   end Ghdl_Signal_Force_Driving_E32;

   procedure Ghdl_Signal_Force_Effective_E32 (Sig : Ghdl_Signal_Ptr;
                                              Val : Ghdl_E32) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Effective,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_E32,
                                                   E32 => Val)));
   end Ghdl_Signal_Force_Effective_E32;

   procedure Ghdl_Signal_Force_Driving_I32 (Sig : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I32) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Driving,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_I32,
                                                   I32 => Val)));
   end Ghdl_Signal_Force_Driving_I32;

   procedure Ghdl_Signal_Force_Effective_I32 (Sig : Ghdl_Signal_Ptr;
                                              Val : Ghdl_I32) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Effective,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_I32,
                                                   I32 => Val)));
   end Ghdl_Signal_Force_Effective_I32;

   procedure Ghdl_Signal_Force_Driving_I64 (Sig : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I64) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Driving,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_I64,
                                                   I64 => Val)));
   end Ghdl_Signal_Force_Driving_I64;

   procedure Ghdl_Signal_Force_Effective_I64 (Sig : Ghdl_Signal_Ptr;
                                              Val : Ghdl_I64) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Effective,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_I64,
                                                   I64 => Val)));
   end Ghdl_Signal_Force_Effective_I64;

   procedure Ghdl_Signal_Force_Driving_F64 (Sig : Ghdl_Signal_Ptr;
                                            Val : Ghdl_F64) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Driving,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_F64,
                                                   F64 => Val)));
   end Ghdl_Signal_Force_Driving_F64;

   procedure Ghdl_Signal_Force_Effective_F64 (Sig : Ghdl_Signal_Ptr;
                                              Val : Ghdl_F64) is
   begin
      Append_Force_Value (new Force_Value'(Kind => Force,
                                           Mode => Force_Effective,
                                           Next => null,
                                           Sig => Sig,
                                           Val => (Mode => Mode_F64,
                                                   F64 => Val)));
   end Ghdl_Signal_Force_Effective_F64;

   --  Remove all (but Signal_End) signals in the next active chain.
   --  Called when a transaction/event will occur before the time for this
   --  chain.
   procedure Flush_Active_Chain
   is
      Sig : Ghdl_Signal_Ptr;
      Next_Sig : Ghdl_Signal_Ptr;
   begin
      --  Free active_chain.
      Sig := Signal_Active_Chain;
      loop
         Next_Sig := Sig.Link;
         exit when Next_Sig = null;
         Sig.Link := null;
         Sig := Next_Sig;
      end loop;
      pragma Assert (Sig = Signal_End);
      Signal_Active_Chain := Sig;
   end Flush_Active_Chain;

   function Find_Next_Time (Tn : Std_Time) return Std_Time
   is
      Res : Std_Time;
      Sig : Ghdl_Signal_Ptr;

      procedure Check_Transaction (Trans : Transaction_Acc) is
      begin
         if Trans = null or else Trans.Kind = Trans_Direct then
            --  Activity of direct drivers is done through link.
            return;
         end if;

         if Trans.Time = Res then
            --  Put to active list.
            Insert_Active_Chain (Sig);
         elsif Trans.Time < Res then
            --  A transaction is scheduled before all the previous one.  Clear
            --  the active chain, and put simply Sig on it.
            Flush_Active_Chain;

            Res := Trans.Time;

            --  Put sig on the list.
            Insert_Active_Chain (Sig);
         end if;
         if Res = Current_Time then
            --  Must have been in the active list.
            Internal_Error ("find_next_time(2)");
         end if;
      end Check_Transaction;
   begin
      pragma Assert (Tn >= Current_Time);
      --  If there is signals in the active list, then next cycle is a delta
      --  cycle, so next time is current_time.
      if Signal_Active_Chain.Link /= null then
         return Current_Time;
      end if;
      if Force_Value_First /= null then
         return Current_Time;
      end if;

      Res := Tn;
      Sig := Future_List;
      while Sig.Flink /= null loop
         case Sig.S.Mode_Sig is
            when Mode_Signal_User =>
               for J in 1 .. Sig.S.Nbr_Drivers loop
                  Check_Transaction (Sig.S.Drivers (J - 1).First_Trans.Next);
               end loop;
            when Mode_Delayed
              | Mode_Stable
              | Mode_Quiet =>
               Check_Transaction (Sig.S.Attr_Trans.Next);
            when others =>
               Internal_Error ("find_next_time(3)");
         end case;
         Sig := Sig.Flink;
      end loop;
      return Res;
   end Find_Next_Time;

--    function Get_Nbr_Non_Null_Source (Sig : Ghdl_Signal_Ptr)
--                                     return Natural
--    is
--       Length : Natural;
--    begin
--       Length := Sig.Nbr_Ports;
--       for I in 0 .. Sig.Nbr_Drivers - 1 loop
--          case Sig.Drivers (I).First_Trans.Kind is
--             when Trans_Value =>
--                Length := Length + 1;
--             when Trans_Null =>
--                null;
--             when Trans_Error =>
--                Error ("range check error");
--          end case;
--       end loop;
--       return Length;
--    end Get_Nbr_Non_Null_Source;

   function To_Resolver_Acc is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Resolver_Acc);

   procedure Compute_Resolved_Signal (Resolv : Resolved_Signal_Acc)
   is
      Sig : constant Ghdl_Signal_Ptr :=
        Sig_Table.Table (Resolv.Sig_Range.First);
      Length : Ghdl_Index_Type;
      type Bool_Array_Type is array (1 .. Sig.S.Nbr_Drivers) of Boolean;
      Vec : Bool_Array_Type;
   begin
      --  Compute number of non-null drivers.
      Length := 0;
      for I in 1 .. Sig.S.Nbr_Drivers loop
         case Sig.S.Drivers (I - 1).First_Trans.Kind is
            when Trans_Value =>
               Length := Length + 1;
               Vec (I) := True;
            when Trans_Null =>
               Vec (I) := False;
            when Trans_Error =>
               Error ("range check error");
            when Trans_Direct =>
               Internal_Error ("compute_resolved_signal: trans_direct");
         end case;
      end loop;

      --  Check driving condition on all signals.
      for J in Resolv.Sig_Range.First + 1.. Resolv.Sig_Range.Last loop
         for I in 1 .. Sig.S.Nbr_Drivers loop
            if (Sig_Table.Table (J).S.Drivers (I - 1).First_Trans.Kind
                /= Trans_Null)
              xor Vec (I)
            then
               Error ("null-transaction required");
            end if;
         end loop;
      end loop;

      --  if no driving sources and register, exit.
      if Length = 0
        and then Sig.Nbr_Ports = 0
        and then Sig.Flags.Sig_Kind = Kind_Signal_Register
      then
         return;
      end if;

      --  Call the procedure.
      Resolv.Resolv_Proc.all (Resolv.Resolv_Inst,
                              Resolv.Resolv_Ptr,
                              Vec'Address,
                              Length,
                              Sig.S.Nbr_Drivers,
                              Sig.Nbr_Ports);
   end Compute_Resolved_Signal;

   procedure Call_Conversion_Function (Conv : Sig_Conversion_Acc)
   is
      F : Conversion_Func_Acc;
   begin
      F := To_Conversion_Func_Acc (Conv.Func);
      F.all (Conv.Instance);
   end Call_Conversion_Function;

   procedure Resume_Process_If_Event
     (Sig : Ghdl_Signal_Ptr; Proc : Process_Acc)
   is
      El : Action_List_Acc;
   begin
      El := new Action_List'(Dynamic => False,
                             Proc => Proc,
                             Next => Sig.Event_List);
      Sig.Event_List := El;
   end Resume_Process_If_Event;

   --  Order of signals:
   --  To be computed: driving value or/and effective value
   --  To be considered: ports, signals, implicit signals, resolution,
   --                    conversion
   --

   procedure Add_Propagation (P : Propagation_Type) is
   begin
      Propagation.Increment_Last;
      Propagation.Table (Propagation.Last) := P;
   end Add_Propagation;

   procedure Add_Forward_Propagation (Sig : Ghdl_Signal_Ptr) is
   begin
      for I in 1 .. Sig.Nbr_Ports loop
         Add_Propagation
           ((Kind => Imp_Forward_Build,
             Forward => new Forward_Build_Type'(Src => Sig.Ports (I - 1),
                                                Targ => Sig)));
      end loop;
   end Add_Forward_Propagation;

   --  Put SIG in PROPAGATION table until ORDER level.
   procedure Order_Signal (Sig : Ghdl_Signal_Ptr; Order : Propag_Order_Flag);

   --  Return TRUE is the effective value of SIG is the driving value of SIG.
   function Is_Eff_Drv (Sig : Ghdl_Signal_Ptr) return Boolean
   is
   begin
      case Sig.S.Mode_Sig is
         when Mode_Signal
           | Mode_Buffer
           | Mode_Out =>
            return True;
         when Mode_Linkage =>
            --  No effective value.
            return False;
         when Mode_Inout
           | Mode_In =>
            if Sig.S.Effective = null then
               if Sig.S.Nbr_Drivers > 0 or Sig.Nbr_Ports > 0 then
                  --  Only for inout.
                  return True;
               else
                  return False;
               end if;
            else
               return False;
            end if;
         when Mode_Conv_In
           | Mode_Conv_Out =>
            return False;
         when Mode_Stable
           | Mode_Guard
           | Mode_Quiet
           | Mode_Transaction
           | Mode_Delayed =>
            return True;
         when Mode_End =>
            return False;
      end case;
   end Is_Eff_Drv;

   procedure Order_Signal_List (Sig : Ghdl_Signal_Ptr;
                                Order : Propag_Order_Flag)
   is
   begin
      for I in 1 .. Sig.Nbr_Ports loop
         Order_Signal (Sig.Ports (I - 1), Order);
      end loop;
   end Order_Signal_List;

   --  Put SIG in PROPAGATION table until ORDER level.
   procedure Order_Signal (Sig : Ghdl_Signal_Ptr; Order : Propag_Order_Flag)
   is
   begin
      if Sig = null then
         return;
      end if;

      --  Catch infinite loops, which must never happen.
      --  Also exit if the signal is already fully ordered.
      case Sig.Flags.Propag is
         when Propag_None =>
            null;
         when Propag_Being_Driving =>
            Internal_Error ("order_signal: being driving");
         when Propag_Being_Effective =>
            Internal_Error ("order_signal: being effective");
         when Propag_Driving =>
            null;
         when Propag_Done =>
            --  If sig was already handled, nothing to do!
            return;
      end case;

      --  First, the driving value.
      if Sig.Flags.Propag = Propag_None then
         case Sig.S.Mode_Sig is
            when Mode_Signal_User =>
               if Sig.S.Nbr_Drivers = 0 and Sig.Nbr_Ports = 0 then
                  --  No source.
                  Sig.Flags.Propag := Propag_Driving;
               elsif Sig.S.Resolv = null then
                  --  Not resolved (so at most one source).
                  if Sig.S.Nbr_Drivers = 1 then
                     --  Not resolved, 1 source : a driver.
                     pragma Assert (Sig.Nbr_Ports = 0);
                     if Is_Eff_Drv (Sig) then
                        Add_Propagation ((Kind => Eff_One_Driver, Sig => Sig));
                        Sig.Flags.Propag := Propag_Done;
                     else
                        Add_Propagation ((Kind => Drv_One_Driver, Sig => Sig));
                        Sig.Flags.Propag := Propag_Driving;
                     end if;
                  else
                     Sig.Flags.Propag := Propag_Being_Driving;
                     --  not resolved, 1 source : Source is a port.
                     pragma Assert (Sig.Nbr_Ports = 1);
                     pragma Assert (Sig.S.Nbr_Drivers = 0);
                     Order_Signal (Sig.Ports (0), Propag_Driving);
                     if Is_Eff_Drv (Sig) then
                        Add_Propagation ((Kind => Eff_One_Port, Sig => Sig));
                        Sig.Flags.Propag := Propag_Done;
                     else
                        Add_Propagation ((Kind => Drv_One_Port, Sig => Sig));
                        Sig.Flags.Propag := Propag_Driving;
                     end if;
                  end if;
               else
                  --  Resolved signal.
                  declare
                     Resolv : Resolved_Signal_Acc;
                     S : Ghdl_Signal_Ptr;
                  begin
                     --  Compute driving value of brothers.
                     Resolv := Sig.S.Resolv;
                     for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last
                     loop
                        S := Sig_Table.Table (I);
                        if S.Flags.Propag /= Propag_None then
                           Internal_Error ("order_signal(1)");
                        end if;
                        S.Flags.Propag := Propag_Being_Driving;
                     end loop;
                     for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last
                     loop
                        S := Sig_Table.Table (I);
                        --  Compute driving value of the sources.
                        for J in 1 .. S.Nbr_Ports loop
                           Order_Signal (S.Ports (J - 1), Propag_Driving);
                        end loop;
                     end loop;
                     for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last
                     loop
                        S := Sig_Table.Table (I);
                        S.Flags.Propag := Propag_Driving;
                     end loop;

                     if Is_Eff_Drv (Sig) then
                        if Resolv.Sig_Range.First = Resolv.Sig_Range.Last then
                           Add_Propagation ((Kind => Eff_One_Resolved,
                                             Sig => Sig));
                        else
                           Add_Propagation ((Kind => Eff_Multiple,
                                             Resolv => Resolv));
                        end if;
                     else
                        if Resolv.Sig_Range.First = Resolv.Sig_Range.Last then
                           Add_Propagation ((Kind => Drv_One_Resolved,
                                             Sig => Sig));
                        else
                           Add_Propagation ((Kind => Drv_Multiple,
                                             Resolv => Resolv));
                        end if;
                     end if;
                  end;
               end if;
            when Mode_Signal_Implicit =>
               Sig.Flags.Propag := Propag_Being_Driving;
               Order_Signal_List (Sig, Propag_Done);
               Sig.Flags.Propag := Propag_Done;
               case Mode_Signal_Implicit (Sig.S.Mode_Sig) is
                  when Mode_Guard =>
                     Add_Propagation ((Kind => Imp_Guard, Sig => Sig));
                  when Mode_Stable =>
                     Add_Forward_Propagation (Sig);
                     Add_Propagation ((Kind => Imp_Stable, Sig => Sig));
                  when Mode_Quiet =>
                     Add_Forward_Propagation (Sig);
                     Add_Propagation ((Kind => Imp_Quiet, Sig => Sig));
                  when Mode_Delayed =>
                     Add_Forward_Propagation (Sig);
                     Add_Propagation ((Kind => Imp_Delayed, Sig => Sig));
                  when Mode_Transaction =>
                     Add_Propagation ((Kind => Imp_Transaction, Sig => Sig));
               end case;
               return;
            when Mode_Conv_In =>
               --  In conversion signals have no driving value
               null;
            when Mode_Conv_Out =>
               declare
                  Conv : Sig_Conversion_Acc;
               begin
                  Conv := Sig.S.Conv;
                  for I in Conv.Dest.First .. Conv.Dest.Last loop
                     Sig_Table.Table (I).Flags.Propag := Propag_Being_Driving;
                  end loop;
                  for I in Conv.Src.First .. Conv.Src.Last loop
                     Order_Signal (Sig_Table.Table (I), Propag_Driving);
                  end loop;
                  Add_Propagation ((Kind => Out_Conversion, Conv => Conv));
                  for I in Conv.Dest.First .. Conv.Dest.Last loop
                     Sig_Table.Table (I).Flags.Propag := Propag_Done;
                  end loop;
               end;
            when Mode_End =>
               Internal_Error ("order_signal: mode_end");
         end case;
      end if;

      -- Effective value.
      if Order = Propag_Driving then
         --  Will be done later.
         return;
      end if;

      case Sig.S.Mode_Sig is
         when Mode_Signal
           | Mode_Buffer
           | Mode_Out =>
            --  Effective value is driving value.
            Sig.Flags.Propag := Propag_Done;
         when Mode_Linkage =>
            --  No effective value.
            Sig.Flags.Propag := Propag_Done;
         when Mode_Inout
           | Mode_In =>
            if Sig.S.Effective = null then
               --  Effective value is driving value or initial value.
               null;
            else
               Sig.Flags.Propag := Propag_Being_Effective;
               Order_Signal (Sig.S.Effective, Propag_Done);
               Add_Propagation ((Kind => Eff_Actual, Sig => Sig));
               Sig.Flags.Propag := Propag_Done;
            end if;
         when Mode_Stable
           | Mode_Guard
           | Mode_Quiet
           | Mode_Transaction
           | Mode_Delayed =>
            --  Sig.Propag is already set to PROPAG_DONE.
            null;
         when Mode_Conv_In =>
            declare
               Conv : Sig_Conversion_Acc;
            begin
               Conv := Sig.S.Conv;
               for I in Conv.Dest.First .. Conv.Dest.Last loop
                  Sig_Table.Table (I).Flags.Propag := Propag_Being_Effective;
               end loop;
               for I in Conv.Src.First .. Conv.Src.Last loop
                  Order_Signal (Sig_Table.Table (I), Propag_Done);
               end loop;
               Add_Propagation ((Kind => In_Conversion, Conv => Conv));
               for I in Conv.Dest.First .. Conv.Dest.Last loop
                  Sig_Table.Table (I).Flags.Propag := Propag_Done;
               end loop;
            end;
         when Mode_Conv_Out =>
            --  No effective value.
            null;
         when Mode_End =>
            Internal_Error ("order_signal: mode_end");
      end case;
   end Order_Signal;

   procedure Set_Net (Sig : Ghdl_Signal_Ptr;
                      Net : Signal_Net_Type;
                      Link : Ghdl_Signal_Ptr)
   is
      use Astdio;
      use Stdio;
   begin
      if Sig = null then
         return;
      end if;

      if Boolean'(False) then
         Put ("set_net ");
         Put_I32 (stdout, Ghdl_I32 (Net));
         Put (" on ");
         Put (stdout, Sig.all'Address);
         Put ("  ");
         Disp_Signals.Disp_Mode_Signal (Sig.S.Mode_Sig);
         New_Line;
      end if;

      if Sig.Net /= No_Signal_Net then
         if Sig.Net /= Net then
            --  Renumber.
            if Boolean'(False) then
               Put ("set_net renumber ");
               Put_I32 (stdout, Ghdl_I32 (Net));
               Put (" on ");
               Put (stdout, Sig.all'Address);
               New_Line;
            end if;

            declare
               S : Ghdl_Signal_Ptr;
               Old : constant Signal_Net_Type := Sig.Net;
            begin
               --  Merge the old net into NET.
               S := Sig;
               loop
                  S.Net := Net;
                  S := S.Link;
                  exit when S = Sig;
               end loop;

               --  Add to the ring.
               S := Sig.Link;
               Sig.Link := Link.Link;
               Link.Link := S;

               --  Check.
               for I in Sig_Table.First .. Sig_Table.Last loop
                  if Sig_Table.Table (I).Net = Old then
--                      Disp_Signals.Disp_Signals_Table;
--                      Disp_Signals.Disp_Signals_Map;

                     Internal_Error ("set_net: link corrupted");
                  end if;
               end loop;
            end;
         end if;
         return;
      end if;

      Sig.Net := Net;

      --  Add SIG in the LINK ring.
      --  Note: this works even if LINK is not a ring (ie, LINK.link = null).
      if Link.Link = null and then Sig /= Link then
         Internal_Error ("set_net: bad link");
      end if;
      Sig.Link := Link.Link;
      Link.Link := Sig;

      --  Dependences.
      case Sig.S.Mode_Sig is
         when Mode_Signal_User =>
            for I in 1 .. Sig.Nbr_Ports loop
               Set_Net (Sig.Ports (I - 1), Net, Link);
            end loop;
            Set_Net (Sig.S.Effective, Net, Link);
            if Sig.S.Resolv /= null then
               for I in Sig.S.Resolv.Sig_Range.First
                 .. Sig.S.Resolv.Sig_Range.Last
               loop
                  Set_Net (Sig_Table.Table (I), Net, Link);
               end loop;
            end if;
         when Mode_Signal_Forward =>
            null;
         when Mode_Transaction
           | Mode_Guard =>
            for I in 1 .. Sig.Nbr_Ports loop
               Set_Net (Sig.Ports (I - 1), Net, Link);
            end loop;
         when Mode_Conv_In
           | Mode_Conv_Out =>
            declare
               S : Ghdl_Signal_Ptr;
               Conv : Sig_Conversion_Acc;
            begin
               Conv := Sig.S.Conv;
               S := Sig_Table.Table (Conv.Src.First);
               if Sig = S or else S.Net /= Net then
                  for J in Conv.Src.First .. Conv.Src.Last loop
                     Set_Net (Sig_Table.Table (J), Net, Link);
                  end loop;
                  for J in Conv.Dest.First .. Conv.Dest.Last loop
                     Set_Net (Sig_Table.Table (J), Net, Link);
                  end loop;
               end if;
            end;
         when Mode_End =>
            Internal_Error ("set_net");
      end case;
   end Set_Net;

   function Get_Propagation_Net (P : Signal_Net_Type) return Signal_Net_Type
   is
   begin
      case Propagation.Table (P).Kind is
         when Drv_Multiple
           | Eff_Multiple =>
            return Sig_Table.Table
              (Propagation.Table (P).Resolv.Sig_Range.First).Net;
         when In_Conversion
           | Out_Conversion =>
            return Sig_Table.Table
              (Propagation.Table (P).Conv.Src.First).Net;
         when Imp_Forward_Build =>
            return Propagation.Table (P).Forward.Src.Net;
         when others =>
            return Propagation.Table (P).Sig.Net;
      end case;
   end Get_Propagation_Net;

   Last_Signal_Net : Signal_Net_Type;

   --  Create a net for SIG, or if one of its dependences has already a net,
   --  merge SIG in this net.
   procedure Merge_Net (Sig : Ghdl_Signal_Ptr)
   is
   begin
      if Sig.S.Mode_Sig in Mode_Signal_User then
         if Sig.S.Resolv = null
           and then Sig.Nbr_Ports = 0
           and then Sig.S.Effective = null
         then
            Internal_Error ("merge_net(1)");
         end if;

         if Sig.S.Effective /= null
           and then Sig.S.Effective.Net /= No_Signal_Net
         then
            --  Avoid to create a net, just merge.
            Set_Net (Sig, Sig.S.Effective.Net, Sig.S.Effective);
            return;
         end if;
      end if;

      if Sig.Nbr_Ports >= 1
        and then Sig.Ports (0).Net /= No_Signal_Net
      then
         --  Avoid to create a net, just merge.
         Set_Net (Sig, Sig.Ports (0).Net, Sig.Ports (0));
      else
         Last_Signal_Net := Last_Signal_Net + 1;
         Set_Net (Sig, Last_Signal_Net, Sig);
      end if;
   end Merge_Net;

   --  Create nets.
   --  For all signals, set the net field.
   procedure Create_Nets
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      Last_Signal_Net := No_Signal_Net;

      for I in reverse Propagation.First .. Propagation.Last loop
         case Propagation.Table (I).Kind is
            when Drv_Error
              | Prop_End =>
               null;
            when Drv_One_Driver
              | Eff_One_Driver =>
               null;
            when Eff_One_Resolved =>
               Sig := Propagation.Table (I).Sig;
               --  Do not create a net if the signal has no dependences.
               if Sig.Net = No_Signal_Net
                 and then (Sig.S.Effective /= null or Sig.Nbr_Ports /= 0)
               then
                  Merge_Net (Sig);
               end if;
            when Drv_One_Port
              | Eff_One_Port
              | Imp_Guard
              | Imp_Transaction
              | Eff_Actual
              | Drv_One_Resolved =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Net = No_Signal_Net then
                  Merge_Net (Sig);
               end if;
            when Imp_Forward =>
               --  Should not yet appear.
               Internal_Error ("create_nets - forward");
            when Imp_Forward_Build =>
               Sig := Propagation.Table (I).Forward.Src;
               if Sig.Net = No_Signal_Net then
                  --  Create a new net with only sig.
                  Last_Signal_Net := Last_Signal_Net + 1;
                  Set_Net (Sig, Last_Signal_Net, Sig);
               end if;
            when Imp_Quiet
              | Imp_Stable
              | Imp_Delayed =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Net = No_Signal_Net then
                  --  Create a new net with only sig.
                  Last_Signal_Net := Last_Signal_Net + 1;
                  Sig.Net := Last_Signal_Net;
                  Sig.Link := Sig;
               end if;
            when Drv_Multiple
              | Eff_Multiple =>
               declare
                  Resolv : Resolved_Signal_Acc;
                  Link : Ghdl_Signal_Ptr;
               begin
                  Last_Signal_Net := Last_Signal_Net + 1;
                  Resolv := Propagation.Table (I).Resolv;
                  Link := Sig_Table.Table (Resolv.Sig_Range.First);
                  for J in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last loop
                     Set_Net (Sig_Table.Table (J), Last_Signal_Net, Link);
                  end loop;
               end;
            when In_Conversion
              | Out_Conversion =>
               declare
                  Conv : Sig_Conversion_Acc;
                  Link : Ghdl_Signal_Ptr;
               begin
                  Conv := Propagation.Table (I).Conv;
                  Link := Sig_Table.Table (Conv.Src.First);
                  if Link.Net = No_Signal_Net then
                     Last_Signal_Net := Last_Signal_Net + 1;
                     Set_Net (Link, Last_Signal_Net, Link);
                  end if;
               end;
         end case;
      end loop;

      --  Reorder propagation table.
      declare
         type Off_Array is array (Signal_Net_Type range <>) of Signal_Net_Type;
         Offs : Off_Array (0 .. Last_Signal_Net) := (others => 0);

         Last_Off : Signal_Net_Type;
         Num : Signal_Net_Type;

--          procedure Disp_Offs
--          is
--             use Grt.Astdio;
--             use Grt.Stdio;
--          begin
--             for I in Offs'Range loop
--                if Offs (I) /= 0 then
--                   Put_I32 (stdout, Ghdl_I32 (I));
--                   Put (": ");
--                   Put_I32 (stdout, Ghdl_I32 (Offs (I)));
--                   New_Line;
--                end if;
--             end loop;
--          end Disp_Offs;

         type Propag_Array is array (Signal_Net_Type range <>)
           of Propagation_Type;

         procedure Deallocate is new Ada.Unchecked_Deallocation
           (Object => Forward_Build_Type, Name => Forward_Build_Acc);

         Net : Signal_Net_Type;
      begin
         --  1) Count number of propagation cell per net.
         for I in Propagation.First .. Propagation.Last loop
            Net := Get_Propagation_Net (I);
            Offs (Net) := Offs (Net) + 1;
         end loop;

         --  2) Convert numbers to offsets.
         Last_Off := 1;
         for I in 1 .. Last_Signal_Net loop
            Num := Offs (I);
            if Num /= 0 then
               --  Reserve one slot for a prepended 'prop_end'.
               Offs (I) := Last_Off + 1;
               Last_Off := Last_Off + 1 + Num;
            end if;
         end loop;
         Offs (0) := Last_Off + 1;

         declare
            Propag : Propag_Array (1 .. Last_Off);  --  := (others => 0);
         begin
            for I in Propagation.First .. Propagation.Last loop
               Net := Get_Propagation_Net (I);
               if Net /= No_Signal_Net then
                  Propag (Offs (Net)) := Propagation.Table (I);
                  Offs (Net) := Offs (Net) + 1;
               end if;
            end loop;
            Propagation.Set_Last (Last_Off);
            Propagation.Release;
            for I in Propagation.First .. Propagation.Last loop
               if Propag (I).Kind = Imp_Forward_Build then
                  Propagation.Table (I) := (Kind => Imp_Forward,
                                         Sig => Propag (I).Forward.Targ);
                  Deallocate (Propag (I).Forward);
               else
                  Propagation.Table (I) := Propag (I);
               end if;
            end loop;
         end;
         for I in 1 .. Last_Signal_Net loop
            --  Ignore holes.
            if Offs (I) /= 0 then
               Propagation.Table (Offs (I)) :=
                 (Kind => Prop_End, Updated => True);
            end if;
         end loop;
         Propagation.Table (1) := (Kind => Prop_End, Updated => True);

         --  4) Convert back from offset to start position (on the prop_end
         --     cell).
         Offs (0) := 1;
         Last_Off := 1;
         for I in 1 .. Last_Signal_Net loop
            if Offs (I) /= 0 then
               Num := Offs (I);
               Offs (I) := Last_Off;
               Last_Off := Num;
            end if;
         end loop;

         --  5) Re-map the nets to cell indexes.
         for I in Sig_Table.First .. Sig_Table.Last loop
            Sig := Sig_Table.Table (I);
            if Sig.Net = No_Signal_Net then
               if Sig.S.Resolv /= null then
                  Sig.Net := Net_One_Resolved;
               elsif Sig.S.Nbr_Drivers = 1 then
                  if Sig.S.Drivers (0).Last_Trans.Kind = Trans_Direct then
                     Sig.Net := Net_One_Direct;
                  else
                     Sig.Net := Net_One_Driver;
                  end if;
               else
                  pragma Assert (Sig.S.Nbr_Drivers = 0);
                  null;
               end if;
            else
               Sig.Net := Offs (Sig.Net);
            end if;
            Sig.Link := null;
         end loop;
      end;
   end Create_Nets;

   function Get_Nbr_Future return Ghdl_I32
   is
      Res : Ghdl_I32;
      Sig : Ghdl_Signal_Ptr;
   begin
      Res := 0;
      Sig := Future_List;
      while Sig.Flink /= null loop
         Res := Res + 1;
         Sig := Sig.Flink;
      end loop;
      return Res;
   end Get_Nbr_Future;

   --  Check every scalar subelement of a resolved signal has a driver
   --  in the same process.
   procedure Check_Resolved_Driver (Resolv : Resolved_Signal_Acc)
   is
      First_Sig : Ghdl_Signal_Ptr;
      Nbr : Ghdl_Index_Type;
   begin
      First_Sig := Sig_Table.Table (Resolv.Sig_Range.First);
      Nbr := First_Sig.S.Nbr_Drivers;
      for I in Resolv.Sig_Range.First + 1 .. Resolv.Sig_Range.Last loop
         if Sig_Table.Table (I).S.Nbr_Drivers /= Nbr then
            --  FIXME: provide more information (signal name, process name).
            Error ("missing drivers for subelement of a resolved signal");
         end if;
      end loop;
   end Check_Resolved_Driver;

   Ieee_Std_Logic_1164_Resolved_Resolv_Ptr : Address;
   pragma Import (C, Ieee_Std_Logic_1164_Resolved_Resolv_Ptr,
                  "ieee__std_logic_1164__resolved_RESOLV_ptr");

   procedure Free is new Ada.Unchecked_Deallocation
     (Name => Resolved_Signal_Acc, Object => Resolved_Signal_Type);

   procedure Order_All_Signals
   is
      Sig : Ghdl_Signal_Ptr;
      Resolv : Resolved_Signal_Acc;
   begin
      --  Do checks and optimization.
      for I in Sig_Table.First .. Sig_Table.Last loop
         Sig := Sig_Table.Table (I);

         --  LRM 5.3
         --  If, by the above rules, no disconnection specification applies to
         --  the drivers of a guarded, scalar signal S whose type mark is T
         --  (including a scalar subelement of a composite signal), then the
         --  following default disconnection specification is implicitly
         --  assumed:
         --    disconnect S : T after 0 ns;
         if Sig.S.Mode_Sig in Mode_Signal_User then
            Resolv := Sig.S.Resolv;
            if Resolv /= null and then Resolv.Disconnect_Time = Bad_Time then
               Resolv.Disconnect_Time := 0;
            end if;

            if Resolv /= null
              and then Resolv.Sig_Range.First = I
              and then Resolv.Sig_Range.Last > I
            then
               --  Check every scalar subelement of a resolved signal
               --  has a driver in the same process.
               Check_Resolved_Driver (Resolv);
            end if;

            if Resolv /= null
              and then Resolv.Sig_Range.First = I
              and then Resolv.Sig_Range.Last = I
              and then
              (Resolv.Resolv_Proc
                 = To_Resolver_Acc (Ieee_Std_Logic_1164_Resolved_Resolv_Ptr))
              and then Sig.S.Nbr_Drivers + Sig.Nbr_Ports <= 1
              and then Sig.Flags.Sig_Kind = Kind_Signal_No
            then
               --  Optimization: remove resolver if there is at most one
               --  source.
               Free (Sig.S.Resolv);
            end if;
         end if;
      end loop;

      --  Really order them.
      for I in Sig_Table.First .. Sig_Table.Last loop
         Order_Signal (Sig_Table.Table (I), Propag_Driving);
      end loop;
      for I in Sig_Table.First .. Sig_Table.Last loop
         Order_Signal (Sig_Table.Table (I), Propag_Done);
      end loop;

      Create_Nets;
   end Order_All_Signals;

   --  Mark SIG as active (set 'Active and 'Last_Active).
   --  This procedure is called while signals are updated.
   --  Put SIG on Active_Clear_List (if not already) so that 'Active will be
   --  cleared at the end of the delta cycle.
   procedure Mark_Active (Sig : Ghdl_Signal_Ptr);
   pragma Inline (Mark_Active);

   procedure Mark_Active (Sig : Ghdl_Signal_Ptr) is
   begin
      if not Sig.Active then
         --  Sig is active...
         Sig.Active := True;
         Sig.Last_Active := Current_Time;
         --  ... but only for one cycle.  It has to be cleared before the next
         --  cycle.
         Sig.Alink := Active_Clear_List;
         Active_Clear_List := Sig;
      end if;
   end Mark_Active;

   procedure Set_Stable_Quiet_Activity
     (Mode : Propagation_Kind_Type; Sig : Ghdl_Signal_Ptr) is
   begin
      case Mode is
         when Imp_Stable =>
            for I in 0 .. Sig.Nbr_Ports - 1 loop
               if Sig.Ports (I).Event then
                  Mark_Active (Sig);
                  return;
               end if;
            end loop;
         when Imp_Quiet
           | Imp_Transaction
           | Imp_Guard =>
            for I in 1 .. Sig.Nbr_Ports loop
               if Sig.Ports (I - 1).Active then
                  Mark_Active (Sig);
                  return;
               end if;
            end loop;
         when others =>
            Internal_Error ("set_stable_quiet_activity");
      end case;
   end Set_Stable_Quiet_Activity;

   function Get_Resolved_Activity (Sig : Ghdl_Signal_Ptr) return Boolean
   is
      Trans : Transaction_Acc;
      Res : Boolean := False;
   begin
      for J in 1 .. Sig.S.Nbr_Drivers loop
         Trans := Sig.S.Drivers (J - 1).First_Trans.Next;
         if Trans /= null then
            if Trans.Kind = Trans_Direct then
               Assign (Sig.S.Drivers (J - 1).First_Trans.Val,
                       Trans.Val_Ptr, Sig.Mode);
               --  In fact we knew the signal was active!
               Res := True;
            elsif Trans.Time = Current_Time then
               Free (Sig.S.Drivers (J - 1).First_Trans);
               Sig.S.Drivers (J - 1).First_Trans := Trans;
               Res := True;
            end if;
         end if;
      end loop;
      if Res then
         return True;
      end if;
      for J in 1 .. Sig.Nbr_Ports loop
         if Sig.Ports (J - 1).Active then
            return True;
         end if;
      end loop;
      return False;
   end Get_Resolved_Activity;

   procedure Set_Conversion_Activity (Conv : Sig_Conversion_Acc)
   is
      Active : Boolean := False;
   begin
      for I in Conv.Src.First .. Conv.Src.Last loop
         Active := Active or Sig_Table.Table (I).Active;
      end loop;
      if Active then
         Call_Conversion_Function (Conv);
      end if;
      for I in Conv.Dest.First .. Conv.Dest.Last loop
         Sig_Table.Table (I).Active := Active;
      end loop;
   end Set_Conversion_Activity;

   procedure Delayed_Implicit_Process (Sig : Ghdl_Signal_Ptr)
   is
      Pfx : constant Ghdl_Signal_Ptr := Sig.Ports (0);
      Ntime : Std_Time;
      Trans : Transaction_Acc;
      Last : Transaction_Acc;
      Val : Value_Union;
   begin
      if Pfx.Event then
         --  LRM 14.1
         --  P: process (S)
         --  begin
         --     R <= transport S after T;
         --  end process;

         Ntime := Current_Time + Sig.S.Time;

         --  Find the last transaction.
         Last := Sig.S.Attr_Trans;
         while Last.Next /= null loop
            Last := Last.Next;
         end loop;

         --  The transaction are scheduled after the last one.
         pragma Assert (Last.Time <= Ntime);

         Val := Read_Value (Pfx.Value_Ptr, Pfx.Mode);

         if Last.Time = Ntime then
            --  Change the projected value.
            Last.Val := Val;
         else
            --  Create the transaction.
            Trans := new Transaction'(Kind => Trans_Value,
                                      Line => 0,
                                      Time => Ntime,
                                      Next => null,
                                      Val => Val);

            --  Append the transaction.
            Last.Next := Trans;
         end if;

         if Sig.S.Time = 0 then
            --  The signal will be active on the next cycle.
            Sig.Flags.Implicit_Active_Next := True;
         end if;
      end if;
   end Delayed_Implicit_Process;

   --  Set the effective value of signal SIG to VAL.
   --  If the value is different from the previous one, resume processes.
   procedure Set_Effective_Value
     (Sig : Ghdl_Signal_Ptr; Val : Ghdl_Value_Ptr)
   is
      El : Action_List_Acc;
   begin
      case Sig.Mode is
         when Mode_B1 =>
            if Sig.Value_Ptr.B1 = Val.B1 then
               return;
            end if;
            Sig.Last_Value.B1 := Sig.Value_Ptr.B1;
            Sig.Value_Ptr.B1 := Val.B1;
         when Mode_E8 =>
            if Sig.Value_Ptr.E8 = Val.E8 then
               return;
            end if;
            Sig.Last_Value.E8 := Sig.Value_Ptr.E8;
            Sig.Value_Ptr.E8 := Val.E8;
         when Mode_E32 =>
            if Sig.Value_Ptr.E32 = Val.E32 then
               return;
            end if;
            Sig.Last_Value.E32 := Sig.Value_Ptr.E32;
            Sig.Value_Ptr.E32 := Val.E32;
         when Mode_I32 =>
            if Sig.Value_Ptr.I32 = Val.I32 then
               return;
            end if;
            Sig.Last_Value.I32 := Sig.Value_Ptr.I32;
            Sig.Value_Ptr.I32 := Val.I32;
         when Mode_I64 =>
            if Sig.Value_Ptr.I64 = Val.I64 then
               return;
            end if;
            Sig.Last_Value.I64 := Sig.Value_Ptr.I64;
            Sig.Value_Ptr.I64 := Val.I64;
         when Mode_F64 =>
            if Sig.Value_Ptr.F64 = Val.F64 then
               return;
            end if;
            Sig.Last_Value.F64 := Sig.Value_Ptr.F64;
            Sig.Value_Ptr.F64 := Val.F64;
      end case;

      Sig.Event := True;
      Sig.Last_Event := Current_Time;
      if not Sig.Flags.RO_Event then
         Sig.Flags.RO_Event := True;
         if Sig.Dump_Table_Idx /= 0 then
            Changed_Sig_Table.Append(Sig);
         end if;
      end if;

      El := Sig.Event_List;
      while El /= null loop
         Resume_Process (El.Proc);
         El := El.Next;
      end loop;
   end Set_Effective_Value;

   procedure Run_Propagation (Sig_Net : Ghdl_Signal_Ptr)
   is
      Net : constant Signal_Net_Type := Sig_Net.Net;
      Propagation_Kind : Propagation_Kind_Type;
      I : Signal_Net_Type;
      Sig : Ghdl_Signal_Ptr;
      Trans : Transaction_Acc;
      First_Trans : Transaction_Acc;
   begin
      pragma Assert (Net in Signal_Net_Defined);

      if Propagation.Table (Net).Updated then
         --  Propagation was already run for this net.
         return;
      end if;

      --  Set the updated flag, so that propagation is run only once.
      Propagation.Table (Net).Updated := True;
      --  And put it on the Update_Clear_Chain to clear the update flag.
      Insert_Update_Clear_Chain (Sig_Net);

      I := Net + 1;
      loop
         Propagation_Kind := Propagation.Table (I).Kind;

         --  First: the driving value.
         case Propagation_Kind is
            when Drv_One_Driver
              | Eff_One_Driver =>
               Sig := Propagation.Table (I).Sig;
               First_Trans := Sig.S.Drivers (0).First_Trans;
               Trans := First_Trans.Next;
               if Trans /= null then
                  if Trans.Kind = Trans_Direct then
                     --  Note: already or will be marked as active in
                     --    update_signals.
                     Mark_Active (Sig);
                     Assign (First_Trans.Val, Trans.Val_Ptr, Sig.Mode);
                     if not Sig.Flags.Is_Drv_Forced then
                        Sig.Driving_Value := First_Trans.Val;
                     end if;
                  elsif Trans.Time = Current_Time then
                     Mark_Active (Sig);
                     Free (First_Trans);
                     Sig.S.Drivers (0).First_Trans := Trans;
                     if not Sig.Flags.Is_Drv_Forced then
                        case Trans.Kind is
                           when Trans_Value =>
                              Sig.Driving_Value := Trans.Val;
                           when Trans_Direct =>
                              Internal_Error ("run_propagation: trans_direct");
                           when Trans_Null =>
                              Error ("null transaction");
                           when Trans_Error =>
                              Error_Trans_Error (Trans);
                        end case;
                     end if;
                  end if;
               end if;
            when Drv_One_Resolved
              | Eff_One_Resolved =>
               Sig := Propagation.Table (I).Sig;
               if Get_Resolved_Activity (Sig) then
                  Mark_Active (Sig);
                  if not Sig.Flags.Is_Drv_Forced then
                     Compute_Resolved_Signal
                       (Propagation.Table (I).Sig.S.Resolv);
                  end if;
               end if;
            when Drv_One_Port
              | Eff_One_Port =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Ports (0).Active then
                  Mark_Active (Sig);
                  if not Sig.Flags.Is_Drv_Forced then
                     Sig.Driving_Value := Sig.Ports (0).Driving_Value;
                  end if;
               end if;
            when Eff_Actual =>
               Sig := Propagation.Table (I).Sig;
               --  Note: the signal may have drivers (inout ports).
               if Sig.S.Effective.Active and not Sig.Active then
                  Mark_Active (Sig);
               end if;
            when Drv_Multiple
              | Eff_Multiple =>
               declare
                  Active : Boolean := False;
                  Resolv : Resolved_Signal_Acc;
               begin
                  Resolv := Propagation.Table (I).Resolv;
                  for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last loop
                     Sig := Sig_Table.Table (I);
                     Active := Active or Get_Resolved_Activity (Sig);
                  end loop;
                  if Active then
                     --  Mark the first signal as active (since only this one
                     --  will be checked to set effective value).
                     for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last
                     loop
                        Mark_Active (Sig_Table.Table (I));
                     end loop;
                     if not Sig.Flags.Is_Drv_Forced then
                        Compute_Resolved_Signal (Resolv);
                     end if;
                  end if;
               end;
            when Imp_Guard
              | Imp_Stable
              | Imp_Quiet
              | Imp_Transaction
              | Imp_Forward_Build =>
               null;
            when Imp_Forward =>
               Sig := Propagation.Table (I).Sig;
               Insert_Active_Chain (Sig);
            when Imp_Delayed =>
               Sig := Propagation.Table (I).Sig;
               Trans := Sig.S.Attr_Trans.Next;
               if Trans /= null and then Trans.Time = Current_Time then
                  Mark_Active (Sig);
                  Free (Sig.S.Attr_Trans);
                  Sig.S.Attr_Trans := Trans;
                  if not Sig.Flags.Is_Drv_Forced then
                     Sig.Driving_Value := Trans.Val;
                  end if;
               end if;
            when In_Conversion =>
               null;
            when Out_Conversion =>
               --  FIXME: do not overwrite the drv_forced signals.
               Set_Conversion_Activity (Propagation.Table (I).Conv);
            when Prop_End =>
               return;
            when Drv_Error =>
               Internal_Error ("update signals");
         end case;

         --  Second: the effective value.
         case Propagation_Kind is
            when Drv_One_Driver
              | Drv_One_Port
              | Drv_One_Resolved
              | Drv_Multiple =>
               null;
            when Eff_One_Driver
              | Eff_One_Port
              | Eff_One_Resolved =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Active then
                  if not Sig.Flags.Is_Eff_Forced then
                     Set_Effective_Value
                       (Sig, Sig.Driving_Value'Unrestricted_Access);
                  end if;
               end if;
            when Eff_Multiple =>
               declare
                  Resolv : Resolved_Signal_Acc;
               begin
                  Resolv := Propagation.Table (I).Resolv;
                  if Sig_Table.Table (Resolv.Sig_Range.First).Active then
                     --  If one signal is active, all are active.
                     for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last
                     loop
                        Sig := Sig_Table.Table (I);
                        if not Sig.Flags.Is_Eff_Forced then
                           Set_Effective_Value
                             (Sig, Sig.Driving_Value'Unrestricted_Access);
                        end if;
                     end loop;
                  end if;
               end;
            when Eff_Actual =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Active then
                  if not Sig.Flags.Is_Eff_Forced then
                     Set_Effective_Value (Sig, Sig.S.Effective.Value_Ptr);
                  end if;
               end if;
            when Imp_Forward
              | Imp_Forward_Build =>
               null;
            when Imp_Guard =>
               --  Guard signal is active iff one of its dependence is active.
               Sig := Propagation.Table (I).Sig;
               Set_Stable_Quiet_Activity (Imp_Guard, Sig);
               if Sig.Active then
                  if not Sig.Flags.Is_Drv_Forced then
                     Sig.Driving_Value.B1 :=
                       Sig.S.Guard_Func.all (Sig.S.Guard_Instance);
                  end if;

                  if not Sig.Flags.Is_Eff_Forced then
                     Set_Effective_Value
                       (Sig, Sig.Driving_Value'Unrestricted_Access);
                  end if;
               end if;
            when Imp_Stable
              | Imp_Quiet =>
               Sig := Propagation.Table (I).Sig;
               --  Mark Sig active if one of its source is active (Imp_Quiet)
               --  or has an event (Imp_Stable).
               Set_Stable_Quiet_Activity (Propagation_Kind, Sig);
               if Sig.Active then
                  --  Set driver.
                  --  LRM02 12.6.3
                  --  If an event has occurred on signal S, then S'Stable(T) is
                  --  updated by assigning the value FALSE to the variable
                  --  representing the current value of S'Table(T), ...
                  if not Sig.Flags.Is_Drv_Forced then
                     Sig.Driving_Value :=
                       Value_Union'(Mode => Mode_B1, B1 => False);
                  end if;
                  --  LRM02 12.6.3
                  --  ... and the driver of S'Stable(T) is a assigned the
                  --  waveform TRUE after T.
                  Trans := new Transaction'
                    (Kind => Trans_Value,
                     Line => 0,
                     Time => Current_Time + Sig.S.Time,
                     Next => null,
                     Val => Value_Union'(Mode => Mode_B1, B1 => True));
                  --  Remove previous transaction.
                  if Sig.S.Attr_Trans.Next /= null then
                     Free (Sig.S.Attr_Trans.Next);
                  end if;
                  Sig.S.Attr_Trans.Next := Trans;
                  if not Sig.Flags.Is_Eff_Forced then
                     Set_Effective_Value
                       (Sig, Sig.Driving_Value'Unrestricted_Access);
                  end if;
                  if Sig.S.Time = 0 then
                     --  Signal is active in the next cycle.  If Time > 0, it
                     --  has been put in Future_List during creation.
                     Sig.Flags.Implicit_Active_Next := True;
                  end if;
               else
                  --  LRM02 12.6.3
                  --  Otherwise, if the driver of S'Stable(T) is active, then
                  --  S'Stable(T) is updated by assigning the current value of
                  --  the driver to the variable representing the current value
                  --  of S'Stable(T).
                  Trans := Sig.S.Attr_Trans.Next;
                  if Trans /= null and then Trans.Time = Current_Time then
                     Mark_Active (Sig);
                     Free (Sig.S.Attr_Trans);
                     Sig.S.Attr_Trans := Trans;
                     if not Sig.Flags.Is_Drv_Forced then
                        Sig.Driving_Value := Trans.Val;
                     end if;
                     if not Sig.Flags.Is_Eff_Forced then
                        Set_Effective_Value
                          (Sig, Sig.Driving_Value'Unrestricted_Access);
                     end if;
                  end if;
               end if;
            when Imp_Transaction =>
               --  LRM 12.6.3 Updating Implicit Signals
               --  Finally, for any implicit signal S'Transaction, the current
               --  value of the signal is modified if and only if S is active.
               --  If signal S is active, then S'Transaction is updated by
               --  assigning the value of the expression (not S'Transaction)
               --  to the variable representing the current value of
               --  S'Transaction.
               declare
                  Val : aliased Value_Union;
               begin
                  Sig := Propagation.Table (I).Sig;
                  Val := (Mode => Mode_B1,
                          B1 => not Sig.Value_Ptr.B1);
                  for I in 0 .. Sig.Nbr_Ports - 1 loop
                     if Sig.Ports (I).Active then
                        Mark_Active (Sig);
                        if not Sig.Flags.Is_Eff_Forced then
                           Set_Effective_Value (Sig, Val'Unrestricted_Access);
                        end if;
                        exit;
                     end if;
                  end loop;
               end;
            when Imp_Delayed =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Active then
                  if not Sig.Flags.Is_Eff_Forced then
                     Set_Effective_Value
                       (Sig, Sig.Driving_Value'Unrestricted_Access);
                  end if;
               end if;
               Delayed_Implicit_Process (Sig);
            when In_Conversion =>
               --  TODO: handle eff_forced signals.
               Set_Conversion_Activity (Propagation.Table (I).Conv);
            when Out_Conversion =>
               null;
            when Prop_End =>
               null;
            when Drv_Error =>
               Internal_Error ("run_propagation(2)");
         end case;
         I := I + 1;
      end loop;
   end Run_Propagation;

   procedure Reset_Active_Flag
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      --  1) Reset active flag.
      Sig := Active_Clear_List;
      Active_Clear_List := null;
      while Sig /= null loop
         Sig.Active := False;
         Sig.Event := False;

         if Options.Flag_Stats then
            if Sig.Active then
               Nbr_Active := Nbr_Active + 1;
            end if;
            if Sig.Event then
               Nbr_Events := Nbr_Events + 1;
            end if;
         end if;
         Sig := Sig.Alink;
      end loop;

--       for I in Sig_Table.First .. Sig_Table.Last loop
--          Sig := Sig_Table.Table (I);
--          if Sig.Active or Sig.Event then
--             Internal_Error ("reset_active_flag");
--          end if;
--       end loop;
   end Reset_Active_Flag;

   procedure Update_A_Signal (Sig : Ghdl_Signal_Ptr)
   is
      Trans : Transaction_Acc;
   begin
      --  14.7.3.2 Driving values
      --  a) If a driving-value release is scheduled for S or for a signal
      --     of which S is a subelement, S becomes driving-value released,
      --     that is, no longer driving-value forced.  Proceed to step b).
      --  b) If a driving force is scheduled for S or for a signal of which
      --     S is a subelement, S becomes driving-value forced and the
      --     driving value of S is the driving force value of S or the
      --     element of the driving force value for the signal of which S
      --     is a subelement, as appropriate; no further steps are
      --     required.  Otherwise, proceed to step c).
      --  c) If S is driving-value foced, the driving value of S is unchanged
      --     from its previous value; no further steps are required.
      --     Otherwise, proceed to step d).
      --  d) If a driving-value deposit is scheduled for S or for a signal of
      --     which S is a subelement, the driving value of S is the driving
      --     deposite value for S or the element of the driving deposit for
      --     the signal of which S is a subelement, as appropriate; no further
      --     steps are requited.  Otherwise, proceed to step e) or f), as
      --     appropriate;
      --  GHDL: not yet implemented.
      null;

      case Sig.Net is
         when Net_One_Driver =>
            --  This signal is active.
            Mark_Active (Sig);

            --  Update driver
            Trans := Sig.S.Drivers (0).First_Trans.Next;
            if Trans /= null then
               Free (Sig.S.Drivers (0).First_Trans);
               Sig.S.Drivers (0).First_Trans := Trans;
            end if;

            --  Update driving value (unless forced)
            if not Sig.Flags.Is_Drv_Forced then
               case Trans.Kind is
                  when Trans_Value =>
                     Sig.Driving_Value := Trans.Val;
                  when Trans_Direct =>
                     Internal_Error ("update_signals: trans_direct");
                  when Trans_Null =>
                     Error ("null transaction");
                  when Trans_Error =>
                     Error_Trans_Error (Trans);
               end case;
            end if;

            if not Sig.Flags.Is_Eff_Forced then
               Set_Effective_Value
                 (Sig, Sig.Driving_Value'Unrestricted_Access);
            end if;

         when Net_One_Direct =>
            Mark_Active (Sig);
            Sig.Flags.Is_Direct_Active := False;

            Trans := Sig.S.Drivers (0).Last_Trans;
            Assign (Sig.S.Drivers (0).First_Trans.Val,
                    Trans.Val_Ptr, Sig.Mode);
            if not Sig.Flags.Is_Drv_Forced then
               Sig.Driving_Value := Sig.S.Drivers (0).First_Trans.Val;
            end if;
            if not Sig.Flags.Is_Eff_Forced then
               Set_Effective_Value
                 (Sig, Sig.Driving_Value'Unrestricted_Access);
            end if;

         when Net_One_Resolved =>
            --  This signal is active.
            Mark_Active (Sig);
            Sig.Flags.Is_Direct_Active := False;

            for J in 1 .. Sig.S.Nbr_Drivers loop
               Trans := Sig.S.Drivers (J - 1).First_Trans.Next;
               if Trans /= null then
                  if Trans.Kind = Trans_Direct then
                     Assign (Sig.S.Drivers (J - 1).First_Trans.Val,
                             Trans.Val_Ptr, Sig.Mode);
                  elsif Trans.Time = Current_Time then
                     Free (Sig.S.Drivers (J - 1).First_Trans);
                     Sig.S.Drivers (J - 1).First_Trans := Trans;
                  end if;
               end if;
            end loop;
            if not Sig.Flags.Is_Drv_Forced then
               Compute_Resolved_Signal (Sig.S.Resolv);
            end if;
            if not Sig.Flags.Is_Eff_Forced then
               Set_Effective_Value
                 (Sig, Sig.Driving_Value'Unrestricted_Access);
            end if;

         when No_Signal_Net =>
            --  Can happen with force/release.
            --  This signal is active.
            Mark_Active (Sig);

            --  Driving value is not modified (there is not driver).

            if not Sig.Flags.Is_Eff_Forced then
               Set_Effective_Value
                 (Sig, Sig.Driving_Value'Unrestricted_Access);
            end if;

         when Signal_Net_Defined =>
            Sig.Flags.Is_Direct_Active := False;
            Run_Propagation (Sig);
      end case;
   end Update_A_Signal;

   procedure Update_Signals
   is
      Sig : Ghdl_Signal_Ptr;
      Next_Sig : Ghdl_Signal_Ptr;
   begin
      --  LRM93 12.6.2
      --  1) Reset active flag: all signals active in the previous cycle are
      --     not anymore active.
      Reset_Active_Flag;

      --  Forced signals.
      --  LRM08 14.7.3 Propagation of signal values
      --  A signal is said to be active during a given simulation cycle if
      --  ...
      --  - A force, a deposite, or a release is scheduled for the signal.
      if Force_Value_First /= null then
         declare
            Fv : Force_Value_Acc;
            Next_Fv : Force_Value_Acc;
         begin
            Fv := Force_Value_First;
            while Fv /= null loop
               Sig := Fv.Sig;

               case Fv.Kind is
                  when Force =>
                     --  TODO: warn if forced many times in the same cycle ?
                     case Fv.Mode is
                        when Force_Driving =>
                           Sig.Flags.Is_Drv_Forced := True;
                           Sig.Flags.Is_Drv_Force_Scheduled := True;
                           Sig.Driving_Value := Fv.Val;
                        when Force_Effective =>
                           Sig.Flags.Is_Eff_Forced := True;
                           Sig.Flags.Is_Eff_Force_Scheduled := True;
                           Set_Effective_Value (Sig, Fv.Val'Access);
                     end case;
                  when Release =>
                     case Fv.Mode is
                        when Force_Driving =>
                           if not Sig.Flags.Is_Drv_Force_Scheduled then
                              Sig.Flags.Is_Drv_Forced := False;
                           end if;
                        when Force_Effective =>
                           if not Sig.Flags.Is_Eff_Force_Scheduled then
                              Sig.Flags.Is_Eff_Forced := False;
                           end if;
                     end case;
               end case;

               --  If alredy in the active chain, it means that a driver is
               --  also active.  Do not do anything particular.
               if Sig.Net in Signal_Net_Defined then
                  --  Mark SIG as active so that propagation will execute
                  --  just below.
                  Mark_Active (Sig);
               end if;
               Insert_Active_Chain (Sig);

               Fv := Fv.Next;
            end loop;

            --  Free force/release.  This is done after to clear the
            --  schedule flags.
            --  Not highly efficient, but there shouldn't be a lot of force /
            --  release, and it allows to detect force+release 'conflicts'.
            Fv := Force_Value_First;
            while Fv /= null loop
               Fv.Sig.Flags.Is_Drv_Force_Scheduled := False;
               Fv.Sig.Flags.Is_Eff_Force_Scheduled := False;

               Next_Fv := Fv.Next;
               Free (Fv);
               Fv := Next_Fv;
            end loop;
            Force_Value_First := null;
            Force_Value_Last := null;
         end;
      end if;

      --  For each active signal.
      loop
         --  Extract an active signal.
         Sig := Signal_Active_Chain;
         Next_Sig := Sig.Link;

         --  Leave Signal_End.
         exit when Next_Sig = null;

         --  (Remove it from the chain).
         Signal_Active_Chain := Next_Sig;
         Sig.Link := null;

         Update_A_Signal (Sig);

         Sig := Next_Sig;
      end loop;

      --  Un-mark updated nets.
      Sig := Update_Clear_Chain;
      Update_Clear_Chain := Signal_End;
      while Sig.Link /= null loop
         Propagation.Table (Sig.Net).Updated := False;
         Next_Sig := Sig.Link;
         Sig.Link := null;

         --  Maybe put SIG in the active list, if it will be active during
         --  the next cycle.
         --  This can happen only for 'quiet, 'stable or 'delayed.
         if Sig.Flags.Implicit_Active_Next then
            pragma Assert (Sig.S.Mode_Sig in Mode_Signal_Forward);
            Sig.Flags.Implicit_Active_Next := False;
            Insert_Active_Chain (Sig);
         end if;

         Sig := Next_Sig;
      end loop;
   end Update_Signals;

   procedure Run_Propagation_Init (Start : Signal_Net_Type)
   is
      I : Signal_Net_Type;
      Sig : Ghdl_Signal_Ptr;
   begin
      I := Start;
      loop
         --  First: the driving value.
         case Propagation.Table (I).Kind is
            when Drv_One_Driver
              | Eff_One_Driver =>
               --  Nothing to do: drivers were already created.
               null;
            when Drv_One_Resolved
              | Eff_One_Resolved =>
               --  Execute the resolution function.
               Sig := Propagation.Table (I).Sig;
               if Sig.Nbr_Ports > 0 then
                  Compute_Resolved_Signal (Sig.S.Resolv);
               end if;
            when Drv_One_Port
              | Eff_One_Port =>
               --  Copy value.
               Sig := Propagation.Table (I).Sig;
               Sig.Driving_Value := Sig.Ports (0).Driving_Value;
            when Eff_Actual =>
               null;
            when Drv_Multiple
              | Eff_Multiple =>
               Compute_Resolved_Signal (Propagation.Table (I).Resolv);
            when Imp_Guard
              | Imp_Stable
              | Imp_Quiet
              | Imp_Transaction
              | Imp_Forward
              | Imp_Forward_Build =>
               null;
            when Imp_Delayed =>
               --  LRM 14.1
               --  Assuming that the initial value of R is the same as the
               --  initial value of S, [...]
               Sig := Propagation.Table (I).Sig;
               Sig.Driving_Value := Sig.Ports (0).Driving_Value;
            when In_Conversion =>
               null;
            when Out_Conversion =>
               Call_Conversion_Function (Propagation.Table (I).Conv);
            when Prop_End =>
               return;
            when Drv_Error =>
               Internal_Error ("init_signals");
         end case;

         --  Second: the effective value.
         case Propagation.Table (I).Kind is
            when Drv_One_Driver
              | Drv_One_Port
              | Drv_One_Resolved
              | Drv_Multiple =>
               null;
            when Eff_One_Driver
              | Eff_One_Port
              | Eff_One_Resolved
              | Imp_Delayed =>
               Sig := Propagation.Table (I).Sig;
               Assign (Sig.Value_Ptr, Sig.Driving_Value'Access, Sig.Mode);
            when Eff_Multiple =>
               declare
                  Resolv : Resolved_Signal_Acc;
               begin
                  Resolv := Propagation.Table (I).Resolv;
                  for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last loop
                     Sig := Sig_Table.Table (I);
                     Assign (Sig.Value_Ptr,
                             Sig.Driving_Value'Access, Sig.Mode);
                  end loop;
               end;
            when Eff_Actual =>
               Sig := Propagation.Table (I).Sig;
               Assign (Sig.Value_Ptr, Sig.S.Effective.Value_Ptr, Sig.Mode);
            when Imp_Guard =>
               --  Guard signal is active iff one of its dependence is active.
               Sig := Propagation.Table (I).Sig;
               Sig.Driving_Value.B1 :=
                 Sig.S.Guard_Func.all (Sig.S.Guard_Instance);
               Assign (Sig.Value_Ptr, Sig.Driving_Value'Access, Sig.Mode);
            when Imp_Stable
              | Imp_Quiet
              | Imp_Transaction
              | Imp_Forward
              | Imp_Forward_Build =>
               --  Already initialized during creation.
               null;
            when In_Conversion =>
               Call_Conversion_Function (Propagation.Table (I).Conv);
            when Out_Conversion =>
               null;
            when Prop_End =>
               null;
            when Drv_Error =>
               Internal_Error ("init_signals(2)");
         end case;

         I := I + 1;
      end loop;
   end Run_Propagation_Init;

   --  LRM93 12.6.4 The simulation cycle
   --  The initialization phase consists of the following steps:
   --  - The driving value and the effective value of each explicitly
   --    declared signal are computed, and the current value of the signal
   --    is set to the effective value.  This value is assumed to have been
   --    the value of the signal for an infinite length of time prior to
   --    the start of the simulation.
   procedure Init_Signals
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      for I in Sig_Table.First .. Sig_Table.Last loop
         Sig := Sig_Table.Table (I);

         case Sig.Net is
            when Net_One_Driver
              | Net_One_Direct =>
               --  Use the current value of the transaction for the current
               --  value of the signal.
               Assign (Sig.Driving_Value'Access,
                       Sig.S.Drivers (0).First_Trans.Val'Access, Sig.Mode);
               Assign (Sig.Value_Ptr, Sig.Driving_Value'Access, Sig.Mode);

            when Net_One_Resolved =>
               Sig.Has_Active := True;
               if Sig.S.Nbr_Drivers + Sig.Nbr_Ports > 0 then
                  Compute_Resolved_Signal (Sig.S.Resolv);
                  Assign (Sig.Value_Ptr, Sig.Driving_Value'Access, Sig.Mode);
               end if;

            when No_Signal_Net =>
               Assign (Sig.Value_Ptr, Sig.Driving_Value'Access, Sig.Mode);

            when others =>
               if Propagation.Table (Sig.Net).Updated then
                  Propagation.Table (Sig.Net).Updated := False;
                  Run_Propagation_Init (Sig.Net + 1);
               end if;
         end case;
      end loop;

   end Init_Signals;

   procedure Init is
   begin
      Signal_Active_Chain := Signal_End;
      Future_List := Signal_End;
      Update_Clear_Chain := Signal_End;
   end Init;

end Grt.Signals;
