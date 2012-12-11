--  GHDL Run Time (GRT) - signals management.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with System; use System;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Ada.Unchecked_Deallocation;
with Grt.Errors; use Grt.Errors;
with Grt.Processes; use Grt.Processes;
with Grt.Options; use Grt.Options;
with Grt.Rtis_Types; use Grt.Rtis_Types;
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

   function Is_Signal_Guarded (Sig : Ghdl_Signal_Ptr) return Boolean
   is
   begin
      return (Sig.Rti.Common.Mode and Ghdl_Rti_Signal_Kind_Mask)
        /= Ghdl_Rti_Signal_Kind_No;
   end Is_Signal_Guarded;

   Sig_Rti : Ghdl_Rtin_Object_Acc;
   Last_Implicit_Signal : Ghdl_Signal_Ptr;
   Current_Resolv : Resolved_Signal_Acc := null;

   function Get_Current_Mode_Signal return Mode_Signal_Type
   is
   begin
      return Mode_Signal_Type'Val
        (Sig_Rti.Common.Mode and Ghdl_Rti_Signal_Mode_Mask);
   end Get_Current_Mode_Signal;


   procedure Ghdl_Signal_Name_Rti (Sig : Ghdl_Rti_Access;
                                   Ctxt : Ghdl_Rti_Access;
                                   Addr : Address)
   is
      pragma Unreferenced (Ctxt);
      pragma Unreferenced (Addr);
   begin
      Sig_Rti := To_Ghdl_Rtin_Object_Acc (Sig);
   end Ghdl_Signal_Name_Rti;

   function To_Address is new Ada.Unchecked_Conversion
     (Source => Ghdl_Signal_Ptr, Target => Address);

   function Create_Signal
     (Mode : Mode_Type;
      Init_Val : Value_Union;
      Mode_Sig : Mode_Signal_Type;
      Resolv_Proc : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
      Resolv : Resolved_Signal_Acc;
      S : Ghdl_Signal_Data (Mode_Sig);
   begin
      Sig_Table.Increment_Last;

      if Current_Resolv = null then
         if Resolv_Proc /= Null_Address then
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
         if Resolv_Proc /= Null_Address then
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

      Res := new Ghdl_Signal'(Value => Init_Val,
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
                                        Is_Dumped => False,
                                        Cyc_Event => False,
                                        Seen => False),

                              Net => No_Signal_Net,
                              Link => null,
                              Alink => null,
                              Flink => null,

                              Event_List => null,
                              Rti => Sig_Rti,

                              Nbr_Ports => 0,
                              Ports => null,

                              S => S);

      if Resolv /= null and then Resolv.Resolv_Ptr = System.Null_Address then
         Resolv.Resolv_Ptr := To_Address (Res);
      end if;

      case Flag_Activity is
         when Activity_All =>
            Res.Has_Active := True;
         when Activity_Minimal =>
            if (Sig_Rti.Common.Mode and Ghdl_Rti_Signal_Has_Active) /= 0 then
               Res.Has_Active := True;
            end if;
         when Activity_None =>
            Res.Has_Active := False;
      end case;

      --  Put the signal in the table.
      Sig_Table.Table (Sig_Table.Last) := Res;

      return Res;
   end Create_Signal;

   procedure Ghdl_Signal_Init (Sig : Ghdl_Signal_Ptr; Val : Value_Union) is
   begin
      Sig.Value := Val;
      Sig.Driving_Value := Val;
      Sig.Last_Value := Val;
   end Ghdl_Signal_Init;

   procedure Ghdl_Signal_Merge_Rti (Sig : Ghdl_Signal_Ptr;
                                    Rti : Ghdl_Rti_Access)
   is
      S_Rti : Ghdl_Rtin_Object_Acc;
   begin
      S_Rti := To_Ghdl_Rtin_Object_Acc (Rti);
      if Flag_Activity = Activity_Minimal then
         if (S_Rti.Common.Mode and Ghdl_Rti_Signal_Has_Active) /= 0 then
            Sig.Has_Active := True;
         end if;
      end if;
   end Ghdl_Signal_Merge_Rti;

   procedure Ghdl_Signal_Create_Resolution (Proc : System.Address;
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
            Put ("for signal: ");
            Disp_Signals.Put_Signal_Name (stderr, Sig);
            New_Line (stderr);
            Error ("several sources for unresolved signal");
            --  FIXME: display signal name.
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

      Proc : Process_Acc;
   begin
      Proc := Get_Current_Process;
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
                                Val => Sign.Value);
      if Ghdl_Signal_Add_Driver (Sign, Trans) then
         Free (Trans);
      end if;
   end Ghdl_Process_Add_Driver;

   procedure Ghdl_Signal_Direct_Driver (Sign : Ghdl_Signal_Ptr;
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
                                Val => Sign.Value);
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
   end Ghdl_Signal_Direct_Driver;

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

   procedure Direct_Assign
     (Targ : out Value_Union; Val : Ghdl_Value_Ptr; Mode : Mode_Type)
   is
   begin
      case Mode is
         when Mode_B2 =>
            Targ.B2 := Val.B2;
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
   end Direct_Assign;

   function Value_Equal (Left, Right : Value_Union; Mode : Mode_Type)
     return Boolean
   is
   begin
      case Mode is
         when Mode_B2 =>
            return Left.B2 = Right.B2;
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

   procedure Error_Trans_Error (Trans : Transaction_Acc) is
   begin
      Error_C ("range check error on signal at ");
      Error_C (Trans.File);
      Error_C (":");
      Error_C (Natural (Trans.Line));
      Error_E ("");
   end Error_Trans_Error;
   pragma No_Return (Error_Trans_Error);

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
            return Sig.S.Drivers (I)'Access;
         end if;
      end loop;
      return null;
   end Get_Driver;

   --  Unused but well-known signal which always terminate
   --    ghdl_signal_active_chain.
   --  As a consequence, every element of the chain has a link field set to
   --  a non-null value (this is of course not true for SIGNAL_END).  This may
   --  be used to quickly check if a signal is in the list.
   --  This signal is not in the signal table.
   Signal_End : Ghdl_Signal_Ptr;

   --  List of signals which have projected waveforms in the future (beyond
   --  the next delta cycle).
   Future_List : aliased Ghdl_Signal_Ptr;

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
         if Sign.Link = null then
            Sign.Link := Grt.Threads.Atomic_Insert
              (Ghdl_Signal_Active_Chain'access, Sign);
         end if;
      else
         --  AFTER > 0.
         --  Put SIGN on the future list.
         if Sign.Flink = null then
            Sign.Flink := Grt.Threads.Atomic_Insert (Future_List'access, Sign);
         end if;
      end if;

      Assign_Time := Current_Time + After;
      if Assign_Time < 0 then
         --  Beyond the future
         Free_In (Trans);
         return;
      end if;

      --  Handle sign as direct driver.
      if Driver.Last_Trans.Kind = Trans_Direct then
         if After /= 0 then
            Internal_Error ("direct assign with non-0 after");
         end if;
         --  FIXME: can be a bound-error too!
         if Trans.Kind = Trans_Value then
            case Sign.Mode is
               when Mode_B2 =>
                  Driver.Last_Trans.Val_Ptr.B2 := Trans.Val.B2;
               when Mode_E8 =>
                  Driver.Last_Trans.Val_Ptr.E8 := Trans.Val.E8;
               when Mode_E32 =>
                  Driver.Last_Trans.Val_Ptr.E32 := Trans.Val.E32;
               when Mode_I32 =>
                  Driver.Last_Trans.Val_Ptr.I32 := Trans.Val.I32;
               when Mode_I64 =>
                  Driver.Last_Trans.Val_Ptr.I64 := Trans.Val.I64;
               when Mode_F64 =>
                  Driver.Last_Trans.Val_Ptr.F64 := Trans.Val.F64;
            end case;
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
      if After > 0 and then Sign.Flink = null then
         --  Put SIGN on the future list.
         Sign.Flink := Future_List;
         Future_List := Sign;
      end if;

      Trans := new Transaction'(Kind => Trans_Value,
                                Line => 0,
                                Time => Current_Time + After,
                                Next => null,
                                Val => Val);
      if Trans.Time <= Driver.Last_Trans.Time then
         Error ("transactions not in ascending order");
      end if;
      Driver.Last_Trans.Next := Trans;
      Driver.Last_Trans := Trans;
   end Ghdl_Signal_Next_Assign;

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
      if After > 0 and then Sign.Flink = null then
         --  Put SIGN on the future list.
         Sign.Flink := Future_List;
         Future_List := Sign;
      end if;

      Trans := new Transaction'(Kind => Trans_Error,
                                Line => Line,
                                Time => Current_Time + After,
                                Next => null,
                                File => File);
      if Trans.Time <= Driver.Last_Trans.Time then
         Error ("transactions not in ascending order");
      end if;
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

   procedure Ghdl_Signal_Associate (Sig : Ghdl_Signal_Ptr; Val : Value_Union)
   is
   begin
      Sig.Value := Val;
      Sig.Driving_Value := Val;
   end Ghdl_Signal_Associate;

   function Ghdl_Create_Signal_B2
     (Init_Val : Ghdl_B2;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_B2, Value_Union'(Mode => Mode_B2, B2 => Init_Val),
         Get_Current_Mode_Signal,
         Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_B2;

   procedure Ghdl_Signal_Init_B2 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_B2) is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_B2, B2 => Init_Val));
   end Ghdl_Signal_Init_B2;

   procedure Ghdl_Signal_Associate_B2 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_B2) is
   begin
      Ghdl_Signal_Associate (Sig, Value_Union'(Mode => Mode_B2, B2 => Val));
   end Ghdl_Signal_Associate_B2;

   procedure Ghdl_Signal_Simple_Assign_B2 (Sign : Ghdl_Signal_Ptr;
                                           Val : Ghdl_B2)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value.B2
        and then Sign.S.Drivers (0).First_Trans.Next = null
      then
         return;
      end if;

      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_B2, B2 => Val));

      Ghdl_Signal_Start_Assign (Sign, 0, Trans, 0);
   end Ghdl_Signal_Simple_Assign_B2;

   procedure Ghdl_Signal_Start_Assign_B2 (Sign : Ghdl_Signal_Ptr;
                                          Rej : Std_Time;
                                          Val : Ghdl_B2;
                                          After : Std_Time)
   is
      Trans : Transaction_Acc;
   begin
      Trans := new Transaction'
        (Kind => Trans_Value,
         Line => 0,
         Time => 0,
         Next => null,
         Val => Value_Union'(Mode => Mode_B2, B2 => Val));
      Ghdl_Signal_Start_Assign (Sign, Rej, Trans, After);
   end Ghdl_Signal_Start_Assign_B2;

   procedure Ghdl_Signal_Next_Assign_B2 (Sign : Ghdl_Signal_Ptr;
                                         Val : Ghdl_B2;
                                         After : Std_Time)
   is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_B2, B2 => Val), After);
   end Ghdl_Signal_Next_Assign_B2;

   function Ghdl_Create_Signal_E8
     (Init_Val : Ghdl_E8;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_E8, Value_Union'(Mode => Mode_E8, E8 => Init_Val),
         Get_Current_Mode_Signal,
         Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_E8;

   procedure Ghdl_Signal_Init_E8 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_E8) is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_E8, E8 => Init_Val));
   end Ghdl_Signal_Init_E8;

   procedure Ghdl_Signal_Associate_E8 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E8) is
   begin
      Ghdl_Signal_Associate (Sig, Value_Union'(Mode => Mode_E8, E8 => Val));
   end Ghdl_Signal_Associate_E8;

   procedure Ghdl_Signal_Simple_Assign_E8 (Sign : Ghdl_Signal_Ptr;
                                           Val : Ghdl_E8)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value.E8
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
                                         After : Std_Time)
   is
   begin
      Ghdl_Signal_Next_Assign
        (Sign, Value_Union'(Mode => Mode_E8, E8 => Val), After);
   end Ghdl_Signal_Next_Assign_E8;

   function Ghdl_Create_Signal_E32
     (Init_Val : Ghdl_E32;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_E32, Value_Union'(Mode => Mode_E32, E32 => Init_Val),
         Get_Current_Mode_Signal,
         Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_E32;

   procedure Ghdl_Signal_Init_E32 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_E32)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_E32, E32 => Init_Val));
   end Ghdl_Signal_Init_E32;

   procedure Ghdl_Signal_Associate_E32 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E32)
   is
   begin
      Ghdl_Signal_Associate (Sig, Value_Union'(Mode => Mode_E32, E32 => Val));
   end Ghdl_Signal_Associate_E32;

   procedure Ghdl_Signal_Simple_Assign_E32 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_E32)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value.E32
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
     (Init_Val : Ghdl_I32;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_I32, Value_Union'(Mode => Mode_I32, I32 => Init_Val),
         Get_Current_Mode_Signal,
         Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_I32;

   procedure Ghdl_Signal_Init_I32 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_I32)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_I32, I32 => Init_Val));
   end Ghdl_Signal_Init_I32;

   procedure Ghdl_Signal_Associate_I32 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I32)
   is
   begin
      Ghdl_Signal_Associate (Sig, Value_Union'(Mode => Mode_I32, I32 => Val));
   end Ghdl_Signal_Associate_I32;

   procedure Ghdl_Signal_Simple_Assign_I32 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I32)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value.I32
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
     (Init_Val : Ghdl_I64;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_I64, Value_Union'(Mode => Mode_I64, I64 => Init_Val),
         Get_Current_Mode_Signal,
         Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_I64;

   procedure Ghdl_Signal_Init_I64 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_I64)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_I64, I64 => Init_Val));
   end Ghdl_Signal_Init_I64;

   procedure Ghdl_Signal_Associate_I64 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I64)
   is
   begin
      Ghdl_Signal_Associate (Sig, Value_Union'(Mode => Mode_I64, I64 => Val));
   end Ghdl_Signal_Associate_I64;

   procedure Ghdl_Signal_Simple_Assign_I64 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I64)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value.I64
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
     (Init_Val : Ghdl_F64;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr
   is
   begin
      return Create_Signal
        (Mode_F64, Value_Union'(Mode => Mode_F64, F64 => Init_Val),
         Get_Current_Mode_Signal,
         Resolv_Func, Resolv_Inst);
   end Ghdl_Create_Signal_F64;

   procedure Ghdl_Signal_Init_F64 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_F64)
   is
   begin
      Ghdl_Signal_Init (Sig, Value_Union'(Mode => Mode_F64, F64 => Init_Val));
   end Ghdl_Signal_Init_F64;

   procedure Ghdl_Signal_Associate_F64 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_F64)
   is
   begin
      Ghdl_Signal_Associate (Sig, Value_Union'(Mode => Mode_F64, F64 => Val));
   end Ghdl_Signal_Associate_F64;

   procedure Ghdl_Signal_Simple_Assign_F64 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_F64)
   is
      Trans : Transaction_Acc;
   begin
      if not Sign.Has_Active
        and then Sign.Net = Net_One_Driver
        and then Val = Sign.Value.F64
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

   Bit_Signal_Rti : aliased Ghdl_Rtin_Object :=
     (Common => (Kind => Ghdl_Rtik_Signal,
                 Depth => 0,
                 Mode => Ghdl_Rti_Signal_Mode_None,
                 Max_Depth => 0),
      Name => null,
      Loc => (Rel => True, Off => 0),
      Obj_Type => null);

   Boolean_Signal_Rti : aliased Ghdl_Rtin_Object :=
     (Common => (Kind => Ghdl_Rtik_Signal,
                 Depth => 0,
                 Mode => Ghdl_Rti_Signal_Mode_None,
                 Max_Depth => 0),
      Name => null,
      Loc => (Rel => True, Off => 0),
      Obj_Type => null);

   function Ghdl_Create_Signal_Attribute
     (Mode : Mode_Signal_Type; Time : Std_Time)
     return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
--      Sig_Type : Ghdl_Desc_Ptr;
   begin
      case Mode is
         when Mode_Transaction =>
            Sig_Rti := To_Ghdl_Rtin_Object_Acc
              (To_Ghdl_Rti_Access (Bit_Signal_Rti'Address));
         when Mode_Quiet
           | Mode_Stable =>
            Sig_Rti := To_Ghdl_Rtin_Object_Acc
              (To_Ghdl_Rti_Access (Boolean_Signal_Rti'Address));
         when others =>
            Internal_Error ("ghdl_create_signal_attribute");
      end case;
      --  Note: bit and boolean are both mode_b2.
      Res := Create_Signal
        (Mode_B2, Value_Union'(Mode => Mode_B2, B2 => True),
         Mode, Null_Address, Null_Address);

      Last_Implicit_Signal := Res;

      if Mode /= Mode_Transaction then
         Res.S.Time := Time;
         Res.S.Attr_Trans := new Transaction'(Kind => Trans_Value,
                                              Line => 0,
                                              Time => 0,
                                              Next => null,
                                              Val => Res.Value);
      end if;

      if Time > 0 then
         Res.Flink := Future_List;
         Future_List := Res;
      end if;

      return Res;
   end Ghdl_Create_Signal_Attribute;

   function Ghdl_Create_Stable_Signal (Val : Std_Time) return Ghdl_Signal_Ptr
   is
   begin
      return Ghdl_Create_Signal_Attribute (Mode_Stable, Val);
   end Ghdl_Create_Stable_Signal;

   function Ghdl_Create_Quiet_Signal (Val : Std_Time) return Ghdl_Signal_Ptr
   is
   begin
      return Ghdl_Create_Signal_Attribute (Mode_Quiet, Val);
   end Ghdl_Create_Quiet_Signal;

   function Ghdl_Create_Transaction_Signal return Ghdl_Signal_Ptr
   is
   begin
      return Ghdl_Create_Signal_Attribute (Mode_Transaction, 0);
   end Ghdl_Create_Transaction_Signal;

   procedure Ghdl_Signal_Attribute_Register_Prefix (Sig : Ghdl_Signal_Ptr)
   is
   begin
      Add_Port (Last_Implicit_Signal, Sig);
   end Ghdl_Signal_Attribute_Register_Prefix;

   --Guard_String : constant String := "guard";
   --Guard_Name : constant Ghdl_Str_Len_Address_Type :=
   --  (Len => 5, Str => Guard_String'Address);
   --function To_Ghdl_Str_Len_Ptr is new Ada.Unchecked_Conversion
   --  (Source => System.Address, Target => Ghdl_Str_Len_Ptr);

   Guard_Rti : aliased constant Ghdl_Rtin_Object :=
     (Common => (Kind => Ghdl_Rtik_Signal,
                 Depth => 0,
                 Mode => Ghdl_Rti_Signal_Mode_None,
                 Max_Depth => 0),
      Name => null,
      Loc => (Rel => True, Off => 0),
      Obj_Type => Std_Standard_Boolean_RTI_Ptr);

   function Ghdl_Signal_Create_Guard (This : System.Address;
                                      Proc : Guard_Func_Acc)
     return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
   begin
      Sig_Rti := To_Ghdl_Rtin_Object_Acc
        (To_Ghdl_Rti_Access (Guard_Rti'Address));
      Res := Create_Signal
        (Mode_B2, Value_Union'(Mode => Mode_B2, B2 => Proc.all (This)),
         Mode_Guard, Null_Address, Null_Address);
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

   function Ghdl_Create_Delayed_Signal (Sig : Ghdl_Signal_Ptr; Val : Std_Time)
                                       return Ghdl_Signal_Ptr
   is
      Res : Ghdl_Signal_Ptr;
   begin
      Res := Create_Signal (Sig.Mode, Sig.Value,
                            Mode_Delayed, Null_Address, Null_Address);
      Res.S.Time := Val;
      if Val > 0 then
         Res.Flink := Future_List;
         Future_List := Res;
      end if;
      Res.S.Attr_Trans := new Transaction'(Kind => Trans_Value,
                                           Line => 0,
                                           Time => 0,
                                           Next => null,
                                           Val => Res.Value);
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

   function Ghdl_Signal_Driving (Sig : Ghdl_Signal_Ptr) return Ghdl_B2
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

   function Ghdl_Signal_Driving_Value_B2 (Sig : Ghdl_Signal_Ptr) return Ghdl_B2
   is
      Drv : Driver_Acc;
   begin
      Drv := Get_Driver (Sig);
      if Drv = null or else Drv.First_Trans.Kind /= Trans_Value then
         Error ("'driving_value: no active driver in process for signal");
      else
         return Drv.First_Trans.Val.B2;
      end if;
   end Ghdl_Signal_Driving_Value_B2;

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

   Ghdl_Implicit_Signal_Active_Chain : Ghdl_Signal_Ptr;

   procedure Flush_Active_List
   is
      Sig : Ghdl_Signal_Ptr;
      Next_Sig : Ghdl_Signal_Ptr;
   begin
      --  Free active_chain.
      Sig := Ghdl_Signal_Active_Chain;
      loop
         Next_Sig := Sig.Link;
         exit when Next_Sig = null;
         Sig.Link := null;
         Sig := Next_Sig;
      end loop;
      Ghdl_Signal_Active_Chain := Sig;
   end Flush_Active_List;

   function Find_Next_Time return Std_Time
   is
      Res : Std_Time;
      Sig : Ghdl_Signal_Ptr;

      procedure Check_Transaction (Trans : Transaction_Acc)
      is
      begin
         if Trans = null or else Trans.Kind = Trans_Direct then
            --  Activity of direct drivers is done through link.
            return;
         end if;

         if Trans.Time = Res and Sig.Link = null then
            Sig.Link := Ghdl_Signal_Active_Chain;
            Ghdl_Signal_Active_Chain := Sig;
         elsif Trans.Time < Res then
            Flush_Active_List;

            --  Put sig on the list.
            Sig.Link := Ghdl_Signal_Active_Chain;
            Ghdl_Signal_Active_Chain := Sig;

            Res := Trans.Time;
         end if;
         if Res = Current_Time then
            --  Must have been in the active list.
            Internal_Error ("find_next_time(2)");
         end if;
      end Check_Transaction;
   begin
      --  If there is signals in the active list, then next cycle is a delta
      --  cycle, so next time is current_time.
      if Ghdl_Signal_Active_Chain.Link /= null then
         return Current_Time;
      end if;
      if Ghdl_Implicit_Signal_Active_Chain.Link /= null then
         return Current_Time;
      end if;
      Res := Std_Time'Last;

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


   type Resolver_Acc is access procedure
     (Instance : System.Address;
      Val : System.Address;
      Bool_Vec : System.Address;
      Vec_Len : Ghdl_Index_Type;
      Nbr_Drv : Ghdl_Index_Type;
      Nbr_Ports : Ghdl_Index_Type);

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
      if Length = 0 and then Sig.Nbr_Ports = 0
        and then ((Sig.Rti.Common.Mode and Ghdl_Rti_Signal_Kind_Mask)
                  = Ghdl_Rti_Signal_Kind_Register)
      then
         return;
      end if;

      --  Call the procedure.
      To_Resolver_Acc (Resolv.Resolv_Proc).all
        (Resolv.Resolv_Inst,
         Resolv.Resolv_Ptr,
         Vec'Address,
         Length,
         Sig.S.Nbr_Drivers,
         Sig.Nbr_Ports);
   end Compute_Resolved_Signal;

   type Conversion_Func_Acc is access procedure (Instance : System.Address);
   pragma Convention (C, Conversion_Func_Acc);
   function To_Conversion_Func_Acc is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Conversion_Func_Acc);

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

   procedure Add_Forward_Propagation (Sig : Ghdl_Signal_Ptr)
   is
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
           | Mode_Buffer =>
            return True;
         when Mode_Linkage
           | Mode_Out =>
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
               if Sig.S.Mode_Sig in Mode_Signal_Forward then
                  Add_Forward_Propagation (Sig);
               end if;
               case Mode_Signal_Implicit (Sig.S.Mode_Sig) is
                  when Mode_Guard =>
                     Add_Propagation ((Kind => Imp_Guard, Sig => Sig));
                  when Mode_Stable =>
                     Add_Propagation ((Kind => Imp_Stable, Sig => Sig));
                  when Mode_Quiet =>
                     Add_Propagation ((Kind => Imp_Quiet, Sig => Sig));
                  when Mode_Transaction =>
                     Add_Propagation ((Kind => Imp_Transaction, Sig => Sig));
                  when Mode_Delayed =>
                     Add_Propagation ((Kind => Imp_Delayed, Sig => Sig));
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
           | Mode_Buffer =>
            --  Effective value is driving value.
            Sig.Flags.Propag := Propag_Done;
         when Mode_Linkage
           | Mode_Out =>
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
         type Off_Array_Acc is access Off_Array;
         Offs : Off_Array_Acc;
         procedure Free is new Ada.Unchecked_Deallocation
           (Name => Off_Array_Acc, Object => Off_Array);

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
         type Propag_Array_Acc is access Propag_Array;
         Propag : Propag_Array_Acc;
         procedure Free is new Ada.Unchecked_Deallocation
           (Name => Propag_Array_Acc, Object => Propag_Array);

         procedure Deallocate is new Ada.Unchecked_Deallocation
           (Object => Forward_Build_Type, Name => Forward_Build_Acc);

         Net : Signal_Net_Type;
      begin
         --  1) Count number of propagation cell per net.
         Offs := new Off_Array (0 .. Last_Signal_Net);
         Offs.all := (others => 0);
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

         --  3) Gather entries by net (copy)
         Propag := new Propag_Array (1 .. Last_Off);
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
         Free (Propag);
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
               end if;
            else
               Sig.Net := Offs (Sig.Net);
            end if;
            Sig.Link := null;
         end loop;
         Free (Offs);
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
              and then (Resolv.Resolv_Proc
                        = Ieee_Std_Logic_1164_Resolved_Resolv_Ptr)
              and then Sig.S.Nbr_Drivers + Sig.Nbr_Ports <= 1
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

   --  Add SIG in active_chain.
   procedure Add_Active_Chain (Sig : Ghdl_Signal_Ptr);
   pragma Inline (Add_Active_Chain);

   procedure Add_Active_Chain (Sig : Ghdl_Signal_Ptr)
   is
   begin
      if Sig.Link = null then
         Sig.Link := Ghdl_Signal_Active_Chain;
         Ghdl_Signal_Active_Chain := Sig;
      end if;
   end Add_Active_Chain;

   Clear_List : Ghdl_Signal_Ptr := null;

   procedure Mark_Active (Sig : Ghdl_Signal_Ptr);
   pragma Inline (Mark_Active);

   procedure Mark_Active (Sig : Ghdl_Signal_Ptr)
   is
   begin
      if not Sig.Active then
         Sig.Active := True;
         Sig.Last_Active := Current_Time;
         Sig.Alink := Clear_List;
         Clear_List := Sig;
      end if;
   end Mark_Active;

   procedure Set_Guard_Activity (Sig : Ghdl_Signal_Ptr) is
   begin
      for I in 1 .. Sig.Nbr_Ports loop
         if Sig.Ports (I - 1).Active then
            Mark_Active (Sig);
            return;
         end if;
      end loop;
   end Set_Guard_Activity;

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
           | Imp_Transaction =>
            for I in 0 .. Sig.Nbr_Ports - 1 loop
               if Sig.Ports (I).Active then
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
               Direct_Assign (Sig.S.Drivers (J - 1).First_Trans.Val,
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
      Pfx : Ghdl_Signal_Ptr;
      Trans : Transaction_Acc;
      Last : Transaction_Acc;
      Prev : Transaction_Acc;
   begin
      Pfx := Sig.Ports (0);
      if Pfx.Event then
         --  LRM 14.1
         --  P: process (S)
         --  begin
         --     R <= transport S after T;
         --  end process;
         Trans := new Transaction'(Kind => Trans_Value,
                                   Line => 0,
                                   Time => Current_Time + Sig.S.Time,
                                   Next => null,
                                   Val => Pfx.Value);
         --  Find the last transaction.
         Last := Sig.S.Attr_Trans;
         Prev := Last;
         while Last.Next /= null loop
            Prev := Last;
            Last := Last.Next;
         end loop;
         --  Maybe, remove it.
         if Last.Time > Trans.Time then
            Internal_Error ("delayed time");
         elsif Last.Time = Trans.Time then
            if Prev /= Last then
               Free (Last);
            else
               --  No transaction.
               if Last.Time /= 0 then
                  --  This can happen only at time = 0.
                  Internal_Error ("delayed");
               end if;
            end if;
         else
            Prev := Last;
         end if;
         --  Append the transaction.
         Prev.Next := Trans;
         if Sig.S.Time = 0 then
            Add_Active_Chain (Sig);
         end if;
      end if;
   end Delayed_Implicit_Process;

   --  Set the effective value of signal SIG to VAL.
   --  If the value is different from the previous one, resume processes.
   procedure Set_Effective_Value (Sig : Ghdl_Signal_Ptr; Val : Value_Union)
   is
      El : Action_List_Acc;
   begin
      if not Value_Equal (Sig.Value, Val, Sig.Mode) then
         Sig.Last_Value := Sig.Value;
         Sig.Value := Val;
         Sig.Event := True;
         Sig.Last_Event := Current_Time;
         Sig.Flags.Cyc_Event := True;

         El := Sig.Event_List;
         while El /= null loop
            Resume_Process (El.Proc);
            El := El.Next;
         end loop;
      end if;
   end Set_Effective_Value;

   procedure Run_Propagation (Start : Signal_Net_Type)
   is
      I : Signal_Net_Type;
      Sig : Ghdl_Signal_Ptr;
      Trans : Transaction_Acc;
      First_Trans : Transaction_Acc;
   begin
      I := Start;
      loop
         --  First: the driving value.
         case Propagation.Table (I).Kind is
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
                     Direct_Assign (First_Trans.Val,
                                    Trans.Val_Ptr, Sig.Mode);
                     Sig.Driving_Value := First_Trans.Val;
                  elsif Trans.Time = Current_Time then
                     Mark_Active (Sig);
                     Free (First_Trans);
                     Sig.S.Drivers (0).First_Trans := Trans;
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
            when Drv_One_Resolved
              | Eff_One_Resolved =>
               Sig := Propagation.Table (I).Sig;
               if Get_Resolved_Activity (Sig) then
                  Mark_Active (Sig);
                  Compute_Resolved_Signal (Propagation.Table (I).Sig.S.Resolv);
               end if;
            when Drv_One_Port
              | Eff_One_Port =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Ports (0).Active then
                  Mark_Active (Sig);
                  Sig.Driving_Value := Sig.Ports (0).Driving_Value;
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
                     Compute_Resolved_Signal (Resolv);
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
               if Sig.Link = null then
                  Sig.Link := Ghdl_Implicit_Signal_Active_Chain;
                  Ghdl_Implicit_Signal_Active_Chain := Sig;
               end if;
            when Imp_Delayed =>
               Sig := Propagation.Table (I).Sig;
               Trans := Sig.S.Attr_Trans.Next;
               if Trans /= null and then Trans.Time = Current_Time then
                  Mark_Active (Sig);
                  Free (Sig.S.Attr_Trans);
                  Sig.S.Attr_Trans := Trans;
                  Sig.Driving_Value := Trans.Val;
               end if;
            when In_Conversion =>
               null;
            when Out_Conversion =>
               Set_Conversion_Activity (Propagation.Table (I).Conv);
            when Prop_End =>
               return;
            when Drv_Error =>
               Internal_Error ("update signals");
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
              | Eff_One_Resolved =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Active then
                  Set_Effective_Value (Sig, Sig.Driving_Value);
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
                        Set_Effective_Value (Sig, Sig.Driving_Value);
                     end loop;
                  end if;
               end;
            when Eff_Actual =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Active then
                  Set_Effective_Value (Sig, Sig.S.Effective.Value);
               end if;
            when Imp_Forward
              | Imp_Forward_Build =>
               null;
            when Imp_Guard =>
               --  Guard signal is active iff one of its dependence is active.
               Sig := Propagation.Table (I).Sig;
               Set_Guard_Activity (Sig);
               if Sig.Active then
                  Sig.Driving_Value.B2 :=
                    Sig.S.Guard_Func.all (Sig.S.Guard_Instance);
                  Set_Effective_Value (Sig, Sig.Driving_Value);
               end if;
            when Imp_Stable
              | Imp_Quiet =>
               Sig := Propagation.Table (I).Sig;
               Set_Stable_Quiet_Activity (Propagation.Table (I).Kind, Sig);
               if Sig.Active then
                  Sig.Driving_Value :=
                    Value_Union'(Mode => Mode_B2, B2 => False);
                  --  Set driver.
                  Trans := new Transaction'
                    (Kind => Trans_Value,
                     Line => 0,
                     Time => Current_Time + Sig.S.Time,
                     Next => null,
                     Val => Value_Union'(Mode => Mode_B2, B2 => True));
                  if Sig.S.Attr_Trans.Next /= null then
                     Free (Sig.S.Attr_Trans.Next);
                  end if;
                  Sig.S.Attr_Trans.Next := Trans;
                  Set_Effective_Value (Sig, Sig.Driving_Value);
                  if Sig.S.Time = 0 then
                     Add_Active_Chain (Sig);
                  end if;
               else
                  Trans := Sig.S.Attr_Trans.Next;
                  if Trans /= null and then Trans.Time = Current_Time then
                     Mark_Active (Sig);
                     Free (Sig.S.Attr_Trans);
                     Sig.S.Attr_Trans := Trans;
                     Sig.Driving_Value := Trans.Val;
                     Set_Effective_Value (Sig, Sig.Driving_Value);
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
               Sig := Propagation.Table (I).Sig;
               for I in 0 .. Sig.Nbr_Ports - 1 loop
                  if Sig.Ports (I).Active then
                     Mark_Active (Sig);
                     Set_Effective_Value
                       (Sig, Value_Union'(Mode => Mode_B2,
                                          B2 => not Sig.Value.B2));
                     exit;
                  end if;
               end loop;
            when Imp_Delayed =>
               Sig := Propagation.Table (I).Sig;
               if Sig.Active then
                  Set_Effective_Value (Sig, Sig.Driving_Value);
               end if;
               Delayed_Implicit_Process (Sig);
            when In_Conversion =>
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
      Sig := Clear_List;
      Clear_List := null;
      while Sig /= null loop
         if Options.Flag_Stats then
            if Sig.Active then
               Nbr_Active := Nbr_Active + 1;
            end if;
            if Sig.Event then
               Nbr_Events := Nbr_Events + 1;
            end if;
         end if;
         Sig.Active := False;
         Sig.Event := False;

         Sig := Sig.Alink;
      end loop;

--       for I in Sig_Table.First .. Sig_Table.Last loop
--          Sig := Sig_Table.Table (I);
--          if Sig.Active or Sig.Event then
--             Internal_Error ("reset_active_flag");
--          end if;
--       end loop;
   end Reset_Active_Flag;

   procedure Update_Signals
   is
      Sig : Ghdl_Signal_Ptr;
      Next_Sig : Ghdl_Signal_Ptr;
      Trans : Transaction_Acc;
   begin
      --  LRM93 12.6.2
      --  1) Reset active flag.
      Reset_Active_Flag;

      Sig := Ghdl_Signal_Active_Chain;
      Ghdl_Signal_Active_Chain := Signal_End;
      while Sig.S.Mode_Sig /= Mode_End loop
         Next_Sig := Sig.Link;
         Sig.Link := null;

         case Sig.Net is
            when Net_One_Driver =>
               --  This signal is active.
               Mark_Active (Sig);

               Trans := Sig.S.Drivers (0).First_Trans.Next;
               Free (Sig.S.Drivers (0).First_Trans);
               Sig.S.Drivers (0).First_Trans := Trans;
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
               Set_Effective_Value (Sig, Sig.Driving_Value);

            when Net_One_Direct =>
               Mark_Active (Sig);

               Trans := Sig.S.Drivers (0).Last_Trans;
               Direct_Assign (Sig.Driving_Value, Trans.Val_Ptr, Sig.Mode);
               Sig.S.Drivers (0).First_Trans.Val := Sig.Driving_Value;
               Set_Effective_Value (Sig, Sig.Driving_Value);

            when Net_One_Resolved =>
               --  This signal is active.
               Mark_Active (Sig);

               for J in 1 .. Sig.S.Nbr_Drivers loop
                  Trans := Sig.S.Drivers (J - 1).First_Trans.Next;
                  if Trans /= null then
                     if Trans.Kind = Trans_Direct then
                        Direct_Assign (Sig.S.Drivers (J - 1).First_Trans.Val,
                                       Trans.Val_Ptr, Sig.Mode);
                     elsif Trans.Time = Current_Time then
                        Free (Sig.S.Drivers (J - 1).First_Trans);
                        Sig.S.Drivers (J - 1).First_Trans := Trans;
                     end if;
                  end if;
               end loop;
               Compute_Resolved_Signal (Sig.S.Resolv);
               Set_Effective_Value (Sig, Sig.Driving_Value);

            when No_Signal_Net =>
               Internal_Error ("update_signals: no_signal_net");

            when others =>
               if not Propagation.Table (Sig.Net).Updated then
                  Propagation.Table (Sig.Net).Updated := True;
                  Run_Propagation (Sig.Net + 1);

                  --  Put it on the list, so that updated flag will be cleared.
                  Add_Active_Chain (Sig);
               end if;
         end case;

         Sig := Next_Sig;
      end loop;

      --  Implicit signals (forwarded).
      loop
         Sig := Ghdl_Implicit_Signal_Active_Chain;
         exit when Sig.Link = null;
         Ghdl_Implicit_Signal_Active_Chain := Sig.Link;
         Sig.Link := null;

         if not Propagation.Table (Sig.Net).Updated then
            Propagation.Table (Sig.Net).Updated := True;
            Run_Propagation (Sig.Net + 1);

            --  Put it on the list, so that updated flag will be cleared.
            Add_Active_Chain (Sig);
         end if;
      end loop;

      --  Un-mark updated.
      Sig := Ghdl_Signal_Active_Chain;
      Ghdl_Signal_Active_Chain := Signal_End;
      while Sig.Link /= null loop
         Propagation.Table (Sig.Net).Updated := False;
         Next_Sig := Sig.Link;
         Sig.Link := null;

         --  Maybe put SIG in the active list, if it will be active during
         --  the next cycle.
         --  This can happen only for 'quiet, 'stable or 'delayed.
         case Sig.S.Mode_Sig is
            when Mode_Stable
              | Mode_Quiet
              | Mode_Delayed =>
               declare
                  Trans : Transaction_Acc;
               begin
                  Trans := Sig.S.Attr_Trans.Next;
                  if Trans /= null and then Trans.Time = Current_Time then
                     Sig.Link := Ghdl_Implicit_Signal_Active_Chain;
                     Ghdl_Implicit_Signal_Active_Chain := Sig;
                  end if;
               end;
            when others =>
               null;
         end case;

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
               Sig.Value := Sig.Driving_Value;
            when Eff_Multiple =>
               declare
                  Resolv : Resolved_Signal_Acc;
               begin
                  Resolv := Propagation.Table (I).Resolv;
                  for I in Resolv.Sig_Range.First .. Resolv.Sig_Range.Last loop
                     Sig := Sig_Table.Table (I);
                     Sig.Value := Sig.Driving_Value;
                  end loop;
               end;
            when Eff_Actual =>
               Sig := Propagation.Table (I).Sig;
               Sig.Value := Sig.S.Effective.Value;
            when Imp_Guard =>
               --  Guard signal is active iff one of its dependence is active.
               Sig := Propagation.Table (I).Sig;
               Sig.Driving_Value.B2 :=
                 Sig.S.Guard_Func.all (Sig.S.Guard_Instance);
               Sig.Value := Sig.Driving_Value;
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

   procedure Init_Signals
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      for I in Sig_Table.First .. Sig_Table.Last loop
         Sig := Sig_Table.Table (I);

         case Sig.Net is
            when Net_One_Driver
              | Net_One_Direct =>
               --  Nothing to do: drivers were already created.
               null;

            when Net_One_Resolved =>
               Sig.Has_Active := True;
               if Sig.Nbr_Ports > 0 then
                  Compute_Resolved_Signal (Sig.S.Resolv);
                  Sig.Value := Sig.Driving_Value;
               end if;

            when No_Signal_Net =>
               null;

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
      Signal_End := new Ghdl_Signal'(Value => (Mode => Mode_B2,
                                               B2 => False),
                                     Driving_Value => (Mode => Mode_B2,
                                                       B2 => False),
                                     Last_Value => (Mode => Mode_B2,
                                                    B2 => False),
                                     Last_Event => 0,
                                     Last_Active => 0,
                                     Event => False,
                                     Active => False,
                                     Has_Active => False,
                                     Mode => Mode_B2,

                                     Flags => (Propag => Propag_None,
                                               Is_Dumped => False,
                                               Cyc_Event => False,
                                               Seen => False),

                                     Net => No_Signal_Net,
                                     Link => null,
                                     Alink => null,
                                     Flink => null,

                                     Event_List => null,
                                     Rti => null,

                                     Nbr_Ports => 0,
                                     Ports => null,

                                     S => (Mode_Sig => Mode_End));

      Ghdl_Signal_Active_Chain := Signal_End;
      Ghdl_Implicit_Signal_Active_Chain := Signal_End;
      Future_List := Signal_End;

      Boolean_Signal_Rti.Obj_Type := Std_Standard_Boolean_RTI_Ptr;
      Bit_Signal_Rti.Obj_Type := Std_Standard_Bit_RTI_Ptr;
   end Init;

end Grt.Signals;
