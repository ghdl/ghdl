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
with System;
with Ada.Unchecked_Conversion;
with Grt.Table;
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;
limited with Grt.Processes;
pragma Elaborate_All (Grt.Table);

package Grt.Signals is
   pragma Suppress (All_Checks);

   --  Kind of transaction.
   type Transaction_Kind is
     (
      --  Normal transaction, with a value.
      Trans_Value,
      --  Normal transaction, with a pointer to a value (direct assignment).
      Trans_Direct,
      --  Null transaction.
      Trans_Null,
      --  Like a normal transaction, but without a value due to check error.
      Trans_Error
     );

   type Transaction;
   type Transaction_Acc is access Transaction;
   type Transaction (Kind : Transaction_Kind) is record
      --  Line for error.  Put here to compact the record.
      Line : Ghdl_I32;

      Next : Transaction_Acc;
      Time : Std_Time;
      case Kind is
         when Trans_Value =>
            Val : Value_Union;
         when Trans_Direct =>
            Val_Ptr : Ghdl_Value_Ptr;
         when Trans_Null =>
            null;
         when Trans_Error =>
            --  Filename for error.
            File : Ghdl_C_String;
      end case;
   end record;

   type Process_Acc is access Grt.Processes.Process_Type;

   --  A driver is bound to a process (PROC) and contains a list of
   --  transactions.
   type Driver_Type is record
      First_Trans : Transaction_Acc;
      Last_Trans : Transaction_Acc;
      Proc : Process_Acc;
   end record;

   type Driver_Acc is access all Driver_Type;
   type Driver_Fat_Array is array (Ghdl_Index_Type) of aliased Driver_Type;
   type Driver_Arr_Ptr is access Driver_Fat_Array;

   --  Function access type used to evaluate the guard expression.
   type Guard_Func_Acc is access function (This : System.Address)
                                          return Ghdl_B2;
   pragma Convention (C, Guard_Func_Acc);

   --  Simply linked list of processes to be resumed in case of events.

   type Ghdl_Signal;
   type Ghdl_Signal_Ptr is access Ghdl_Signal;

   function To_Ghdl_Signal_Ptr is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Ghdl_Signal_Ptr);

   type Signal_Fat_Array is array (Ghdl_Index_Type) of Ghdl_Signal_Ptr;
   type Signal_Arr_Ptr is access Signal_Fat_Array;

   function To_Signal_Arr_Ptr is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Signal_Arr_Ptr);

   --  List of processes to wake-up in case of event on the signal.
   type Action_List;
   type Action_List_Acc is access Action_List;

   type Action_List (Dynamic : Boolean) is record
      --  Next action for the current signal.
      Next : Action_List_Acc;

      --  Process to wake-up.
      Proc : Process_Acc;

      case Dynamic is
         when True =>
            --  For a non-sensitized process.
            --  Previous action (to speed-up remove from the chain).
            Prev : Action_List_Acc;

            Sig : Ghdl_Signal_Ptr;

            --  Chain of signals for the process.
            Chain : Action_List_Acc;
         when False =>
            null;
      end case;
   end record;


   --  How to compute resolved signal.
   type Resolved_Signal_Type is record
      Resolv_Proc : System.Address;
      Resolv_Inst : System.Address;
      Resolv_Ptr : System.Address;
      Sig_Range : Sig_Table_Range;
      Disconnect_Time : Std_Time;
   end record;

   type Resolved_Signal_Acc is access Resolved_Signal_Type;

   --  Signal conversion data.
   type Sig_Conversion_Type is record
      --  Function which performs the conversion.
      Func : System.Address;
      Instance : System.Address;

      Src : Sig_Table_Range;
      Dest : Sig_Table_Range;
   end record;
   type Sig_Conversion_Acc is access Sig_Conversion_Type;

   type Forward_Build_Type is record
      Src : Ghdl_Signal_Ptr;
      Targ : Ghdl_Signal_Ptr;
   end record;
   type Forward_Build_Acc is access Forward_Build_Type;

   --  Used to order the signals for the propagation of signals values.
   type Propag_Order_Flag is
     (
      --  The signal was not yet ordered.
      Propag_None,
      --  The signal is being ordered for driving value.
      --  This stage is used to catch loop (which can not occur).
      Propag_Being_Driving,
      --  The signal has been ordered for driving value.
      Propag_Driving,
      --  The signal is being ordered for effective value.
      Propag_Being_Effective,
      --  The signal has completly been ordered.
      Propag_Done);

   --  Each signal belongs to a signal_net.
   --  Signals on the same net must be updated in order.
   --  Signals on different nets have no direct relation-ship, and thus may
   --  be updated without order.
   --  Net NO_SIGNAL_NET is special: it groups all lonely signals.
   type Signal_Net_Type is new Integer;
   No_Signal_Net : constant Signal_Net_Type := 0;
   Net_One_Driver : constant Signal_Net_Type := -1;
   Net_One_Direct : constant Signal_Net_Type := -2;
   Net_One_Resolved : constant Signal_Net_Type := -3;

   --  Flush the list of active signals.
   procedure Flush_Active_List;

   type Ghdl_Signal_Data (Mode_Sig : Mode_Signal_Type := Mode_Signal)
   is record
      case Mode_Sig is
         when Mode_Signal_User =>
            Nbr_Drivers : Ghdl_Index_Type;
            Drivers : Driver_Arr_Ptr;

            --  Signal which defines the effective value of this signal,
            --  if any.
            Effective : Ghdl_Signal_Ptr;

            --  Null if not resolved.
            Resolv : Resolved_Signal_Acc;

         when Mode_Conv_In
           | Mode_Conv_Out =>
            --  Conversion paramaters for conv_in, conv_out.
            Conv : Sig_Conversion_Acc;

         when Mode_Stable
           | Mode_Quiet
           | Mode_Delayed =>
            --  Time parameter for 'stable, 'quiet or 'delayed
            Time : Std_Time;
            Attr_Trans : Transaction_Acc;

         when Mode_Guard =>
            --  Guard function and instance used to compute the
            --  guard expression.
            Guard_Func : Guard_Func_Acc;
            Guard_Instance : System.Address;

         when Mode_Transaction
           | Mode_End =>
            null;
      end case;
   end record;
   pragma Suppress (Discriminant_Check, On => Ghdl_Signal_Data);

   type Ghdl_Signal_Flags is record
      --  Status of the ordering.
      Propag : Propag_Order_Flag;

      --  If set, the signal is dumped in a GHW file.
      Is_Dumped : Boolean;

      --  Set when an event occured.
      --  Only reset by GHW file dumper.
      Cyc_Event : Boolean;

      --  Set if the signal has already been visited.  When outside of the
      --  algorithm that use it, it must be cleared.
      Seen : Boolean;
   end record;
   pragma Pack (Ghdl_Signal_Flags);

   type Ghdl_Signal is record
      --  Fields known by ghdl.
      Value : Value_Union;
      Driving_Value : Value_Union;
      Last_Value : Value_Union;
      Last_Event : Std_Time;
      Last_Active : Std_Time;

      --  Chain of signals.
      --  Used to build nets.
      --  This is also the simply linked list of future active signals.
      Link : Ghdl_Signal_Ptr;

      Event : Boolean;
      Active : Boolean;
      --  If set, the activity of the signal is required by the user.
      Has_Active : Boolean;

      --  Internal fields.
      --  Values mode of this signal.
      Mode : Mode_Type;

      --  Misc flags.
      Flags : Ghdl_Signal_Flags;

      --  Net of the signal.
      Net : Signal_Net_Type;

      --  Chain of signals whose active flag was set.  Used to clear it.
      Alink : Ghdl_Signal_Ptr;

      --  Chain of signals that have a projected waveform in the real future.
      Flink : Ghdl_Signal_Ptr;

      --  List of processes to resume when there is an event on
      --  this signal.
      Event_List : Action_List_Acc;

      --  Path of the signal (with its name) in the design hierarchy.
      --  Used to get the type of the signal.
      Rti : Ghdl_Rtin_Object_Acc;

      --  For user signals: the sources of a signals are drivers
      --  and connected ports.
      --  For implicit signals: PORTS is used as dependence list.
      Nbr_Ports : Ghdl_Index_Type;
      Ports : Signal_Arr_Ptr;

      --  Mode of the signal (in, out ...)
      --Mode_Signal : Mode_Signal_Type;
      S : Ghdl_Signal_Data;
   end record;

   --  Each simple signal declared can be accessed by SIG_TABLE.
   package Sig_Table is new Grt.Table
     (Table_Component_Type => Ghdl_Signal_Ptr,
      Table_Index_Type => Sig_Table_Index,
      Table_Low_Bound => 0,
      Table_Initial => 128);

   --  Return the next time at which a driver becomes active.
   function Find_Next_Time return Std_Time;

   --  Elementary propagation computation.
   --  See LRM 12.6.2 and 12.6.3
   type Propagation_Kind_Type is
     (
      --  How to compute driving value:
      --  Default value.
      Drv_Error,

      --  One source, a driver and not resolved:
      --  the driving value is the driver.
      Drv_One_Driver,

      --  Same as previous, and the effective value is the driving value.
      Eff_One_Driver,

      --  One source, a port and not resolved:
      --  the driving value is the driving value of the port.
      --  Dependence.
      Drv_One_Port,

      --  Same as previous, and the effective value is the driving value.
      Eff_One_Port,

      --  Several sources or resolved:
      --  signal is not composite.
      Drv_One_Resolved,
      Eff_One_Resolved,

      --  Use the resolution function, signal is composite.
      Drv_Multiple,

      --  Same as previous, but the effective value is the previous value.
      Eff_Multiple,

      --  The effective value is the actual associated.
      Eff_Actual,

      --  Sig must be updated but does not belong to the same net.
      Imp_Forward,
      Imp_Forward_Build,

      --  Implicit guard signal.
      --  Its value must be evaluated after the effective value of its
      --  dependences.
      Imp_Guard,

      --  Implicit stable.
      --  Its value must be evaluated after the effective value of its
      --  dependences.
      Imp_Stable,

      --  Implicit quiet.
      --  Its value must be evaluated after the driving value of its
      --  dependences.
      Imp_Quiet,

      --  Implicit transaction.
      --  Its value must be evaluated after the driving value of its
      --  dependences.
      Imp_Transaction,

      --  Implicit delayed
      --  Its value must be evaluated after the driving value of its
      --  dependences.
      Imp_Delayed,

      --  in_conversion.
      --  Pseudo-signal which is set by conversion function.
      In_Conversion,
      Out_Conversion,

      --  End of propagation.
      Prop_End
      );

   type Propagation_Type (Kind : Propagation_Kind_Type := Drv_Error) is record
      case Kind is
         when Drv_Error =>
            null;
         when Drv_One_Driver
           | Eff_One_Driver
           | Drv_One_Port
           | Eff_One_Port
           | Imp_Forward
           | Imp_Guard
           | Imp_Quiet
           | Imp_Transaction
           | Imp_Stable
           | Imp_Delayed
           | Eff_Actual
           | Eff_One_Resolved
           | Drv_One_Resolved =>
            Sig : Ghdl_Signal_Ptr;
         when Drv_Multiple
           | Eff_Multiple =>
            Resolv : Resolved_Signal_Acc;
         when In_Conversion
           | Out_Conversion =>
            Conv : Sig_Conversion_Acc;
         when Imp_Forward_Build =>
            Forward : Forward_Build_Acc;
         when Prop_End =>
            Updated : Boolean;
      end case;
   end record;

   package Propagation is new Grt.Table
     (Table_Component_Type => Propagation_Type,
      Table_Index_Type => Signal_Net_Type,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   --  Get the signal index of PTR.
   function Signal_Ptr_To_Index (Ptr : Ghdl_Signal_Ptr) return Sig_Table_Index;

   --  Compute propagation order of signals.
   procedure Order_All_Signals;

   --  Initialize the package (mainly the lists).
   procedure Init;

   --  Initialize all signals.
   procedure Init_Signals;

   --  Update signals.
   procedure Update_Signals;

   --  Set the effective value of signal SIG to VAL.
   --  If the value is different from the previous one, resume processes.
   procedure Set_Effective_Value (Sig : Ghdl_Signal_Ptr; Val : Value_Union);

   --  Add PROC in the list of processes to be resumed in case of event on
   --  SIG.
   procedure Resume_Process_If_Event
     (Sig : Ghdl_Signal_Ptr; Proc : Process_Acc);

   procedure Ghdl_Signal_Name_Rti (Sig : Ghdl_Rti_Access;
                                   Ctxt : Ghdl_Rti_Access;
                                   Addr : System.Address);

   procedure Ghdl_Signal_Merge_Rti (Sig : Ghdl_Signal_Ptr;
                                    Rti : Ghdl_Rti_Access);

   --  Performs some internal checks on signals (transaction order).
   --  Internal_error is called in case of error.
   procedure Ghdl_Signal_Internal_Checks;

   --  Subprograms to be called by generated code.
   procedure Ghdl_Signal_Simple_Assign_Error (Sign : Ghdl_Signal_Ptr;
                                              File : Ghdl_C_String;
                                              Line : Ghdl_I32);
   procedure Ghdl_Signal_Start_Assign_Error (Sign : Ghdl_Signal_Ptr;
                                             Rej : Std_Time;
                                             After : Std_Time;
                                             File : Ghdl_C_String;
                                             Line : Ghdl_I32);
   procedure Ghdl_Signal_Next_Assign_Error (Sign : Ghdl_Signal_Ptr;
                                            After : Std_Time;
                                            File : Ghdl_C_String;
                                            Line : Ghdl_I32);

   procedure Ghdl_Signal_Set_Disconnect (Sign : Ghdl_Signal_Ptr;
                                         Time : Std_Time);

   procedure Ghdl_Signal_Disconnect (Sign : Ghdl_Signal_Ptr);

   procedure Ghdl_Signal_Start_Assign_Null (Sign : Ghdl_Signal_Ptr;
                                            Rej : Std_Time;
                                            After : Std_Time);

   function Ghdl_Signal_Driving (Sig : Ghdl_Signal_Ptr) return Ghdl_B2;

   function Ghdl_Create_Signal_B2
     (Init_Val : Ghdl_B2;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr;
   procedure Ghdl_Signal_Init_B2 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_B2);
   procedure Ghdl_Signal_Associate_B2 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_B2);
   procedure Ghdl_Signal_Simple_Assign_B2 (Sign : Ghdl_Signal_Ptr;
                                           Val : Ghdl_B2);
   procedure Ghdl_Signal_Start_Assign_B2 (Sign : Ghdl_Signal_Ptr;
                                          Rej : Std_Time;
                                          Val : Ghdl_B2;
                                          After : Std_Time);
   procedure Ghdl_Signal_Next_Assign_B2 (Sign : Ghdl_Signal_Ptr;
                                         Val : Ghdl_B2;
                                         After : Std_Time);
   function Ghdl_Signal_Driving_Value_B2 (Sig : Ghdl_Signal_Ptr)
                                         return Ghdl_B2;

   function Ghdl_Create_Signal_E8
     (Init_Val : Ghdl_E8;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr;
   procedure Ghdl_Signal_Init_E8 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_E8);
   procedure Ghdl_Signal_Associate_E8 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E8);
   procedure Ghdl_Signal_Simple_Assign_E8 (Sign : Ghdl_Signal_Ptr;
                                           Val : Ghdl_E8);
   procedure Ghdl_Signal_Start_Assign_E8 (Sign : Ghdl_Signal_Ptr;
                                          Rej : Std_Time;
                                          Val : Ghdl_E8;
                                          After : Std_Time);
   procedure Ghdl_Signal_Next_Assign_E8 (Sign : Ghdl_Signal_Ptr;
                                         Val : Ghdl_E8;
                                         After : Std_Time);
   function Ghdl_Signal_Driving_Value_E8 (Sig : Ghdl_Signal_Ptr)
                                         return Ghdl_E8;

   function Ghdl_Create_Signal_E32
     (Init_Val : Ghdl_E32;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr;
   procedure Ghdl_Signal_Init_E32 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_E32);
   procedure Ghdl_Signal_Associate_E32 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_E32);
   procedure Ghdl_Signal_Simple_Assign_E32 (Sign : Ghdl_Signal_Ptr;
                                           Val : Ghdl_E32);
   procedure Ghdl_Signal_Start_Assign_E32 (Sign : Ghdl_Signal_Ptr;
                                          Rej : Std_Time;
                                          Val : Ghdl_E32;
                                          After : Std_Time);
   procedure Ghdl_Signal_Next_Assign_E32 (Sign : Ghdl_Signal_Ptr;
                                         Val : Ghdl_E32;
                                         After : Std_Time);
   function Ghdl_Signal_Driving_Value_E32 (Sig : Ghdl_Signal_Ptr)
                                         return Ghdl_E32;

   function Ghdl_Create_Signal_I32
     (Init_Val : Ghdl_I32;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr;
   procedure Ghdl_Signal_Init_I32 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_I32);
   procedure Ghdl_Signal_Associate_I32 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I32);
   procedure Ghdl_Signal_Simple_Assign_I32 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I32);
   procedure Ghdl_Signal_Start_Assign_I32 (Sign : Ghdl_Signal_Ptr;
                                           Rej : Std_Time;
                                           Val : Ghdl_I32;
                                           After : Std_Time);
   procedure Ghdl_Signal_Next_Assign_I32 (Sign : Ghdl_Signal_Ptr;
                                          Val : Ghdl_I32;
                                          After : Std_Time);
   function Ghdl_Signal_Driving_Value_I32 (Sig : Ghdl_Signal_Ptr)
                                         return Ghdl_I32;

   function Ghdl_Create_Signal_I64
     (Init_Val : Ghdl_I64;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr;
   procedure Ghdl_Signal_Init_I64 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_I64);
   procedure Ghdl_Signal_Associate_I64 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_I64);
   procedure Ghdl_Signal_Simple_Assign_I64 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_I64);
   procedure Ghdl_Signal_Start_Assign_I64 (Sign : Ghdl_Signal_Ptr;
                                           Rej : Std_Time;
                                           Val : Ghdl_I64;
                                           After : Std_Time);
   procedure Ghdl_Signal_Next_Assign_I64 (Sign : Ghdl_Signal_Ptr;
                                          Val : Ghdl_I64;
                                          After : Std_Time);
   function Ghdl_Signal_Driving_Value_I64 (Sig : Ghdl_Signal_Ptr)
                                          return Ghdl_I64;

   function Ghdl_Create_Signal_F64
     (Init_Val : Ghdl_F64;
      Resolv_Func : System.Address;
      Resolv_Inst : System.Address)
     return Ghdl_Signal_Ptr;
   procedure Ghdl_Signal_Init_F64 (Sig : Ghdl_Signal_Ptr; Init_Val : Ghdl_F64);
   procedure Ghdl_Signal_Associate_F64 (Sig : Ghdl_Signal_Ptr; Val : Ghdl_F64);
   procedure Ghdl_Signal_Simple_Assign_F64 (Sign : Ghdl_Signal_Ptr;
                                            Val : Ghdl_F64);
   procedure Ghdl_Signal_Start_Assign_F64 (Sign : Ghdl_Signal_Ptr;
                                           Rej : Std_Time;
                                           Val : Ghdl_F64;
                                           After : Std_Time);
   procedure Ghdl_Signal_Next_Assign_F64 (Sign : Ghdl_Signal_Ptr;
                                          Val : Ghdl_F64;
                                          After : Std_Time);
   function Ghdl_Signal_Driving_Value_F64 (Sig : Ghdl_Signal_Ptr)
                                         return Ghdl_F64;

   --  Add a driver to SIGN for the current process.
   procedure Ghdl_Process_Add_Driver (Sign : Ghdl_Signal_Ptr);

   --  Add a direct driver for the current process.
   procedure Ghdl_Signal_Direct_Driver (Sign : Ghdl_Signal_Ptr;
                                        Drv : Ghdl_Value_Ptr);

   --  Used for connexions:
   --  SRC is a source for TARG.
   procedure Ghdl_Signal_Add_Source (Targ : Ghdl_Signal_Ptr;
                                     Src : Ghdl_Signal_Ptr);

   --  Add an in conversion (from SRC to DEST using function FUNC).
   procedure Ghdl_Signal_In_Conversion (Func : System.Address;
                                        Instance : System.Address;
                                        Src : Ghdl_Signal_Ptr;
                                        Src_Len : Ghdl_Index_Type;
                                        Dst : Ghdl_Signal_Ptr;
                                        Dst_Len : Ghdl_Index_Type);
   procedure Ghdl_Signal_Out_Conversion (Func : System.Address;
                                         Instance : System.Address;
                                         Src : Ghdl_Signal_Ptr;
                                         Src_Len : Ghdl_Index_Type;
                                         Dst : Ghdl_Signal_Ptr;
                                         Dst_Len : Ghdl_Index_Type);

   --  Mark the next signals as resolved.
   procedure Ghdl_Signal_Create_Resolution (Proc : System.Address;
                                            Instance : System.Address;
                                            Sig : System.Address;
                                            Nbr_Sig : Ghdl_Index_Type);

   --  The effective value of TARG is the effective value of SRC.
   procedure Ghdl_Signal_Effective_Value (Targ : Ghdl_Signal_Ptr;
                                          Src : Ghdl_Signal_Ptr);

   --  Create a new 'stable (VAL) signal.
   function Ghdl_Create_Stable_Signal (Val : Std_Time) return Ghdl_Signal_Ptr;
   --  Create a new 'quiet (VAL) signal.
   function Ghdl_Create_Quiet_Signal (Val : Std_Time) return Ghdl_Signal_Ptr;

   --  Create a new 'transaction signal.
   function Ghdl_Create_Transaction_Signal return Ghdl_Signal_Ptr;

   --  Create a new SIG'delayed (VAL) signal.
   function Ghdl_Create_Delayed_Signal (Sig : Ghdl_Signal_Ptr; Val : Std_Time)
                                       return Ghdl_Signal_Ptr;

   --  Add SIG in the set of prefix for the last created signal.
   procedure Ghdl_Signal_Attribute_Register_Prefix (Sig : Ghdl_Signal_Ptr);

   --  Create a new implicitly defined GUARD signal.
   function Ghdl_Signal_Create_Guard (This : System.Address;
                                      Proc : Guard_Func_Acc)
     return Ghdl_Signal_Ptr;
   --  Add SIG to the list of referenced signals that appear in the guard
   --  expression.
   procedure Ghdl_Signal_Guard_Dependence (Sig : Ghdl_Signal_Ptr);

   --  Return number of ports/drivers.
   function Ghdl_Signal_Get_Nbr_Ports (Sig : Ghdl_Signal_Ptr)
                                      return Ghdl_Index_Type;
   function Ghdl_Signal_Get_Nbr_Drivers (Sig : Ghdl_Signal_Ptr)
                                        return Ghdl_Index_Type;

   function Ghdl_Signal_Read_Port
     (Sig : Ghdl_Signal_Ptr; Index : Ghdl_Index_Type)
     return Ghdl_Value_Ptr;
   function Ghdl_Signal_Read_Driver
     (Sig : Ghdl_Signal_Ptr; Index : Ghdl_Index_Type)
     return Ghdl_Value_Ptr;

   Ghdl_Signal_Active_Chain : aliased Ghdl_Signal_Ptr;

   --  Statistics.
   Nbr_Active : Ghdl_I32;
   Nbr_Events: Ghdl_I32;
   function Get_Nbr_Future return Ghdl_I32;
private
   pragma Export (C, Ghdl_Signal_Name_Rti,
                  "__ghdl_signal_name_rti");
   pragma Export (C, Ghdl_Signal_Merge_Rti,
                  "__ghdl_signal_merge_rti");

   pragma Export (C, Ghdl_Signal_Simple_Assign_Error,
                  "__ghdl_signal_simple_assign_error");
   pragma Export (C, Ghdl_Signal_Start_Assign_Error,
                  "__ghdl_signal_start_assign_error");
   pragma Export (C, Ghdl_Signal_Next_Assign_Error,
                  "__ghdl_signal_next_assign_error");

   pragma Export (C, Ghdl_Signal_Start_Assign_Null,
                  "__ghdl_signal_start_assign_null");

   pragma Export (C, Ghdl_Signal_Set_Disconnect,
                  "__ghdl_signal_set_disconnect");
   pragma Export (C, Ghdl_Signal_Disconnect,
                  "__ghdl_signal_disconnect");

   pragma Export (Ada, Ghdl_Signal_Driving,
                  "__ghdl_signal_driving");

   pragma Export (Ada, Ghdl_Create_Signal_B2,
                  "__ghdl_create_signal_b2");
   pragma Export (Ada, Ghdl_Signal_Init_B2,
                  "__ghdl_signal_init_b2");
   pragma Export (Ada, Ghdl_Signal_Associate_B2,
                  "__ghdl_signal_associate_b2");
   pragma Export (Ada, Ghdl_Signal_Simple_Assign_B2,
                  "__ghdl_signal_simple_assign_b2");
   pragma Export (Ada, Ghdl_Signal_Start_Assign_B2,
                  "__ghdl_signal_start_assign_b2");
   pragma Export (Ada, Ghdl_Signal_Next_Assign_B2,
                  "__ghdl_signal_next_assign_b2");
   pragma Export (Ada, Ghdl_Signal_Driving_Value_B2,
                  "__ghdl_signal_driving_value_b2");

   pragma Export (C, Ghdl_Create_Signal_E8,
                  "__ghdl_create_signal_e8");
   pragma Export (C, Ghdl_Signal_Init_E8,
                  "__ghdl_signal_init_e8");
   pragma Export (C, Ghdl_Signal_Associate_E8,
                  "__ghdl_signal_associate_e8");
   pragma Export (C, Ghdl_Signal_Simple_Assign_E8,
                  "__ghdl_signal_simple_assign_e8");
   pragma Export (C, Ghdl_Signal_Start_Assign_E8,
                  "__ghdl_signal_start_assign_e8");
   pragma Export (C, Ghdl_Signal_Next_Assign_E8,
                  "__ghdl_signal_next_assign_e8");
   pragma Export (C, Ghdl_Signal_Driving_Value_E8,
                  "__ghdl_signal_driving_value_e8");

   pragma Export (C, Ghdl_Create_Signal_E32,
                  "__ghdl_create_signal_e32");
   pragma Export (C, Ghdl_Signal_Init_E32,
                  "__ghdl_signal_init_e32");
   pragma Export (C, Ghdl_Signal_Associate_E32,
                  "__ghdl_signal_associate_e32");
   pragma Export (C, Ghdl_Signal_Simple_Assign_E32,
                  "__ghdl_signal_simple_assign_e32");
   pragma Export (C, Ghdl_Signal_Start_Assign_E32,
                  "__ghdl_signal_start_assign_e32");
   pragma Export (C, Ghdl_Signal_Next_Assign_E32,
                  "__ghdl_signal_next_assign_e32");
   pragma Export (C, Ghdl_Signal_Driving_Value_E32,
                  "__ghdl_signal_driving_value_e32");

   pragma Export (C, Ghdl_Create_Signal_I32,
                  "__ghdl_create_signal_i32");
   pragma Export (C, Ghdl_Signal_Init_I32,
                  "__ghdl_signal_init_i32");
   pragma Export (C, Ghdl_Signal_Associate_I32,
                  "__ghdl_signal_associate_i32");
   pragma Export (C, Ghdl_Signal_Simple_Assign_I32,
                  "__ghdl_signal_simple_assign_i32");
   pragma Export (C, Ghdl_Signal_Start_Assign_I32,
                  "__ghdl_signal_start_assign_i32");
   pragma Export (C, Ghdl_Signal_Next_Assign_I32,
                  "__ghdl_signal_next_assign_i32");
   pragma Export (C, Ghdl_Signal_Driving_Value_I32,
                  "__ghdl_signal_driving_value_i32");

   pragma Export (C, Ghdl_Create_Signal_I64,
                  "__ghdl_create_signal_i64");
   pragma Export (C, Ghdl_Signal_Init_I64,
                  "__ghdl_signal_init_i64");
   pragma Export (C, Ghdl_Signal_Associate_I64,
                  "__ghdl_signal_associate_i64");
   pragma Export (C, Ghdl_Signal_Simple_Assign_I64,
                  "__ghdl_signal_simple_assign_i64");
   pragma Export (C, Ghdl_Signal_Start_Assign_I64,
                  "__ghdl_signal_start_assign_i64");
   pragma Export (C, Ghdl_Signal_Next_Assign_I64,
                  "__ghdl_signal_next_assign_i64");
   pragma Export (C, Ghdl_Signal_Driving_Value_I64,
                  "__ghdl_signal_driving_value_i64");

   pragma Export (C, Ghdl_Create_Signal_F64,
                  "__ghdl_create_signal_f64");
   pragma Export (C, Ghdl_Signal_Init_F64,
                  "__ghdl_signal_init_f64");
   pragma Export (C, Ghdl_Signal_Associate_F64,
                  "__ghdl_signal_associate_f64");
   pragma Export (C, Ghdl_Signal_Simple_Assign_F64,
                  "__ghdl_signal_simple_assign_f64");
   pragma Export (C, Ghdl_Signal_Start_Assign_F64,
                  "__ghdl_signal_start_assign_f64");
   pragma Export (C, Ghdl_Signal_Next_Assign_F64,
                  "__ghdl_signal_next_assign_f64");
   pragma Export (C, Ghdl_Signal_Driving_Value_F64,
                  "__ghdl_signal_driving_value_f64");

   pragma Export (C, Ghdl_Process_Add_Driver,
                  "__ghdl_process_add_driver");
   pragma Export (C, Ghdl_Signal_Direct_Driver,
                  "__ghdl_signal_direct_driver");

   pragma Export (C, Ghdl_Signal_Add_Source,
                  "__ghdl_signal_add_source");
   pragma Export (C, Ghdl_Signal_Effective_Value,
                  "__ghdl_signal_effective_value");
   pragma Export (C, Ghdl_Signal_In_Conversion,
                  "__ghdl_signal_in_conversion");
   pragma Export (C, Ghdl_Signal_Out_Conversion,
                  "__ghdl_signal_out_conversion");

   pragma Export (C, Ghdl_Signal_Create_Resolution,
                  "__ghdl_signal_create_resolution");

   pragma Export (C, Ghdl_Create_Stable_Signal,
                  "__ghdl_create_stable_signal");
   pragma Export (C, Ghdl_Create_Quiet_Signal,
                  "__ghdl_create_quiet_signal");
   pragma Export (C, Ghdl_Create_Transaction_Signal,
                  "__ghdl_create_transaction_signal");
   pragma Export (C, Ghdl_Signal_Attribute_Register_Prefix,
                  "__ghdl_signal_attribute_register_prefix");
   pragma Export (C, Ghdl_Create_Delayed_Signal,
                  "__ghdl_create_delayed_signal");

   pragma Export (Ada, Ghdl_Signal_Create_Guard,
                  "__ghdl_signal_create_guard");
   pragma Export (C, Ghdl_Signal_Guard_Dependence,
                  "__ghdl_signal_guard_dependence");

   pragma Export (C, Ghdl_Signal_Get_Nbr_Ports,
                  "__ghdl_signal_get_nbr_ports");
   pragma Export (C, Ghdl_Signal_Get_Nbr_Drivers,
                  "__ghdl_signal_get_nbr_drivers");
   pragma Export (C, Ghdl_Signal_Read_Port,
                  "__ghdl_signal_read_port");
   pragma Export (C, Ghdl_Signal_Read_Driver,
                  "__ghdl_signal_read_driver");

   pragma Export (C, Ghdl_Signal_Active_Chain,
                  "__ghdl_signal_active_chain");

end Grt.Signals;
