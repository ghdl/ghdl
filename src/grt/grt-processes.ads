--  GHDL Run Time (GRT) -  processes.
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
with System;
with Ada.Unchecked_Conversion;
with Grt.Stack2; use Grt.Stack2;
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Signals; use Grt.Signals;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr;

package Grt.Processes is
   pragma Suppress (All_Checks);

   --  Internal initialisations.
   procedure Init;

   --  Do the VHDL simulation.
   --  Return simulation status:
   --  >= 0 in case of success (end of time reached).
   --  < 0 in case of failure or stop request.
   function Simulation return Integer;

   --  Broken down version of Simulation.
   function Simulation_Init return Integer;
   pragma Export (Ada, Simulation_Init, "__ghdl_simulation_init");
   function Simulation_Cycle return Integer;
   procedure Simulation_Finish;

   function Simulation_Step return Integer;
   pragma Export (Ada, Simulation_Step, "__ghdl_simulation_step");
   --  Return value:
   --  0: delta cycle
   --  1: non-delta cycle
   --  2: stop
   --  3: finished
   --  4: stop-time reached
   --  5: stop-delta reached

   --  True if simulation has reached a user timeout (--stop-time or
   --  --stop-delta).  Emit an info message as a side effect.
   function Has_Simulation_Timeout return Boolean;

   --  Updated by Initialization_Phase and Simulation_Cycle to the time of the
   --  next cycle.  Unchanged in case of delta-cycle.
   Next_Time : Std_Time;

   --  Number of delta cycles.
   Nbr_Delta_Cycles : Ghdl_I64;
   --  Number of non-delta cycles.
   Nbr_Cycles : Ghdl_I64;

   --  If True, do AMS simulation
   Flag_AMS : Boolean := False;

   --  The break flag of AMS simulation
   --
   --  LRM 1076.1-2017 14.7.5.2 Initialization
   --  At the beginning of initialization, [...] and the break flag is assumed
   --  to be cleared.
   Break_Flag : Boolean := False;

   type Process_Type is private;
   --  type Process_Acc is access all Process_Type;

   --  Return the identifier of the current process.
   --  During the elaboration, this is the identifier of the last process
   --  being elaborated.  So, this function can be used to create signal
   --  drivers.

   --  Return the total number of processes and number of sensitized processes.
   --  Used for statistics.
   function Get_Nbr_Processes return Natural;
   function Get_Nbr_Sensitized_Processes return Natural;

   --  Total number of resumed processes.
   function Get_Nbr_Resumed_Processes return Long_Long_Integer;


   --  Instance is the parameter of the process procedure.
   --  This is in fact a fully opaque type whose content is private to the
   --  process.
   type Instance is limited private;
   type Instance_Acc is access all Instance;
   pragma Convention (C, Instance_Acc);

   --  A process is identified by a procedure having a single private
   --  parameter (its instance).
   type Proc_Acc is access procedure (Self : Instance_Acc);
   pragma Convention (C, Proc_Acc);

   function To_Address is new Ada.Unchecked_Conversion
     (Instance_Acc, System.Address);

   --  Register a process during elaboration.
   --  This procedure is called by vhdl elaboration code.
   procedure Ghdl_Process_Register (Instance : Instance_Acc;
                                    Proc : Proc_Acc;
                                    Ctxt : Ghdl_Rti_Access;
                                    Addr : System.Address);
   procedure Ghdl_Sensitized_Process_Register (Instance : Instance_Acc;
                                               Proc : Proc_Acc;
                                               Ctxt : Ghdl_Rti_Access;
                                               Addr : System.Address);
   procedure Ghdl_Postponed_Process_Register (Instance : Instance_Acc;
                                              Proc : Proc_Acc;
                                              Ctxt : Ghdl_Rti_Access;
                                              Addr : System.Address);
   procedure Ghdl_Postponed_Sensitized_Process_Register
     (Instance : Instance_Acc;
      Proc : Proc_Acc;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address);

   --  For verilog processes.
   procedure Ghdl_Finalize_Register (Instance : Instance_Acc;
                                     Proc : Proc_Acc);

   procedure Ghdl_Initial_Register (Instance : Instance_Acc;
                                    Proc : Proc_Acc);
   procedure Ghdl_Always_Register (Instance : Instance_Acc;
                                   Proc : Proc_Acc);

   function Ghdl_Register_Foreign_Process
     (Instance : Instance_Acc; Proc : Proc_Acc) return Process_Acc;

   --  Add a simple signal in the sensitivity of the last registered
   --  (sensitized) process.
   procedure Ghdl_Process_Add_Sensitivity (Sig : Ghdl_Signal_Ptr);

   --  Resume a process.
   procedure Resume_Process (Proc : Process_Acc);

   --  Incomplete wait statement:

   --  Wait without timeout or sensitivity: wait;
   procedure Ghdl_Process_Wait_Exit;

   --  Wait for a timeout (without sensitivity): wait for X;
   procedure Ghdl_Process_Wait_Timeout (Time : Std_Time;
                                        Filename : Ghdl_C_String;
                                        Line : Ghdl_I32);

   --  Full wait statement:
   --  1. Call Ghdl_Process_Wait_Set_Timeout (if there is a timeout)
   --  2. Call Ghdl_Process_Wait_Add_Sensitivity (for each signal)
   --  3. Call Ghdl_Process_Wait_Suspend, go to 4 if it returns true (timeout)
   --     Evaluate the condition and go to 4 if true
   --     Else, restart 3
   --  4. Call Ghdl_Process_Wait_Close

   --  Add a timeout for a wait.
   procedure Ghdl_Process_Wait_Set_Timeout (Time : Std_Time;
                                            Filename : Ghdl_C_String;
                                            Line : Ghdl_I32);
   --  Add a sensitivity for a wait.
   procedure Ghdl_Process_Wait_Add_Sensitivity (Sig : Ghdl_Signal_Ptr);
   --  Wait until timeout or sensitivity.
   procedure Ghdl_Process_Wait_Suspend;
   --  Return TRUE if woken up by a timeout.
   function Ghdl_Process_Wait_Timed_Out return Boolean;
   --  Finish a wait statement.
   procedure Ghdl_Process_Wait_Close;

   --  Verilog.
   procedure Ghdl_Process_Delay (Del : Ghdl_U32);

   --  Secondary stack.
   function Ghdl_Stack2_Allocate (Size : Ghdl_Index_Type)
     return System.Address;
   function Ghdl_Stack2_Mark return Mark_Id;
   procedure Ghdl_Stack2_Release (Mark : Mark_Id);

   --  Protected variables.
   procedure Ghdl_Protected_Enter (Obj : System.Address);
   procedure Ghdl_Protected_Leave (Obj : System.Address);
   procedure Ghdl_Protected_Init (Obj : System.Address);
   procedure Ghdl_Protected_Fini (Obj : System.Address);

   function Get_Rti_Context (Proc : Process_Acc) return Rtis_Addr.Rti_Context;
private
   type Instance is null record;

   --  State of a process.
   type Process_State is
     (
      --  Sensitized process.  Its state cannot change.
      State_Sensitized,

      --  Non-sensitized process, ready to run.
      State_Ready,

      --  Non-sensitized process being suspended on a timeout (without
      --  sensitivity).
      State_Delayed,

      --  Non-sensitized process being suspended, with sensitivity.
      State_Wait,

      --  Non-sensitized process being awaked by a wait timeout.  This state
      --  is transcient.
      --  This is necessary so that the process will exit immediately from the
      --  wait statements without checking if the wait condition is true.
      State_Timeout,

      --  Non-sensitized process waiting until end.
      State_Dead);

   type Process_Type is record
      --  Subprogram containing process code.
      Subprg : Proc_Acc;

      --  Instance (THIS parameter) for the subprogram.
      This : Instance_Acc;

      --  True if the process is resumed and will be run at next cycle.
      Resumed : Boolean;

      --  True if the process is postponed.
      Postponed : Boolean;

      --  State of the process.
      State : Process_State;

      --  Secondary stack for this process.
      Stack2 : Stack2_Ptr;

      --  Sensitivity list while the (non-sensitized) process is waiting.
      Sensitivity : Action_List_Acc;

      --  Name of the process.
      Rti : Rtis_Addr.Rti_Context;

      --  Timeout value for wait.
      Timeout : Std_Time;

      Timeout_Chain_Next : Process_Acc;
      Timeout_Chain_Prev : Process_Acc;
   end record;

   pragma Export (C, Ghdl_Process_Register,
                  "__ghdl_process_register");
   pragma Export (C, Ghdl_Sensitized_Process_Register,
                  "__ghdl_sensitized_process_register");
   pragma Export (C, Ghdl_Postponed_Process_Register,
                  "__ghdl_postponed_process_register");
   pragma Export (C, Ghdl_Postponed_Sensitized_Process_Register,
                  "__ghdl_postponed_sensitized_process_register");

   pragma Export (C, Ghdl_Finalize_Register, "__ghdl_finalize_register");

   pragma Export (C, Ghdl_Always_Register, "__ghdl_always_register");
   pragma Export (C, Ghdl_Initial_Register, "__ghdl_initial_register");
   pragma Export (C, Ghdl_Register_Foreign_Process,
                  "__ghdl_register_foreign_process");

   pragma Export (C, Ghdl_Process_Add_Sensitivity,
                  "__ghdl_process_add_sensitivity");

   pragma Export (C, Ghdl_Process_Wait_Exit,
                  "__ghdl_process_wait_exit");
   pragma Export (C, Ghdl_Process_Wait_Timeout,
                  "__ghdl_process_wait_timeout");
   pragma Export (C, Ghdl_Process_Wait_Add_Sensitivity,
                  "__ghdl_process_wait_add_sensitivity");
   pragma Export (C, Ghdl_Process_Wait_Set_Timeout,
                  "__ghdl_process_wait_set_timeout");
   pragma Export (Ada, Ghdl_Process_Wait_Suspend,
                  "__ghdl_process_wait_suspend");
   pragma Export (Ada, Ghdl_Process_Wait_Timed_Out,
                  "__ghdl_process_wait_timed_out");
   pragma Export (C, Ghdl_Process_Wait_Close,
                  "__ghdl_process_wait_close");

   pragma Export (C, Ghdl_Process_Delay, "__ghdl_process_delay");

   pragma Export (C, Ghdl_Stack2_Allocate, "__ghdl_stack2_allocate");
   pragma Export (C, Ghdl_Stack2_Mark, "__ghdl_stack2_mark");
   pragma Export (C, Ghdl_Stack2_Release, "__ghdl_stack2_release");

   pragma Export (C, Ghdl_Protected_Enter, "__ghdl_protected_enter");
   pragma Export (C, Ghdl_Protected_Leave, "__ghdl_protected_leave");
   pragma Export (C, Ghdl_Protected_Init, "__ghdl_protected_init");
   pragma Export (C, Ghdl_Protected_Fini, "__ghdl_protected_fini");
end Grt.Processes;
