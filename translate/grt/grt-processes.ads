--  GHDL Run Time (GRT) -  processes.
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
with Grt.Stack2; use Grt.Stack2;
with Grt.Types; use Grt.Types;
with Grt.Signals; use Grt.Signals;
with Grt.Stacks; use Grt.Stacks;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr;
with Grt.Stdio;

package Grt.Processes is
   pragma Suppress (All_Checks);

   --  Internal initialisations.
   procedure Init;

   --  Do the VHDL simulation.
   --  Return 0 in case of success (end of time reached).
   function Simulation return Integer;

   --  Number of delta cycles.
   Nbr_Delta_Cycles : Integer;
   --  Number of non-delta cycles.
   Nbr_Cycles : Integer;

   --  If true, the simulation should be stopped.
   Break_Simulation : Boolean;

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
   function Get_Nbr_Resumed_Processes return Natural;

   --  Disp the name of process PROC.
   procedure Disp_Process_Name (Stream : Grt.Stdio.FILEs; Proc : Process_Acc);

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

   --  Add a simple signal in the sensitivity of the last registered
   --  (sensitized) process.
   procedure Ghdl_Process_Add_Sensitivity (Sig : Ghdl_Signal_Ptr);

   --  Resume a process.
   procedure Resume_Process (Proc : Process_Acc);

   --  Wait without timeout or sensitivity.
   procedure Ghdl_Process_Wait_Exit;
   --  Wait for a timeout.
   procedure Ghdl_Process_Wait_Timeout (Time : Std_Time);
   --  Add a sensitivity for a wait.
   procedure Ghdl_Process_Wait_Add_Sensitivity (Sig : Ghdl_Signal_Ptr);
   --  Add a timeout for a wait.
   procedure Ghdl_Process_Wait_Set_Timeout (Time : Std_Time);
   --  Wait until timeout or sensitivity.
   --  Return TRUE in case of timeout.
   function Ghdl_Process_Wait_Suspend return Boolean;
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

private
   --  State of a process.
   type Process_State is
     (
      --  Sensitized process.  Its state cannot change.
      State_Sensitized,

      --  Non-sensitized process, ready to run.
      State_Ready,

      --  Verilog process, being suspended.
      State_Delayed,

      --  Non-sensitized process being suspended.
      State_Wait,

      --  Non-sensitized process being awaked by a wait timeout.  This state
      --  is transcient.
      --  This is necessary so that the process will exit immediately from the
      --  wait statements without checking if the wait condition is true.
      State_Timeout,

      --  Non-sensitized process waiting until end.
      State_Dead);

   type Process_Type is record
      --  Stack for the process.
      --  This must be the first field of the record (and this is the only
      --  part visible).
      --  Must be NULL_STACK for sensitized processes.
      Stack : Stacks.Stack_Type;

      --  Subprogram containing process code.
      Subprg : Proc_Acc;

      --  Instance (THIS parameter) for the subprogram.
      This : Instance_Acc;

      --  Name of the process.
      Rti : Rtis_Addr.Rti_Context;

      --  True if the process is resumed and will be run at next cycle.
      Resumed : Boolean;

      --  True if the process is postponed.
      Postponed : Boolean;

      State : Process_State;

      --  Timeout value for wait.
      Timeout : Std_Time;

      --  Sensitivity list while the (non-sensitized) process is waiting.
      Sensitivity : Action_List_Acc;

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
