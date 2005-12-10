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
with GNAT.Table;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements; --  Work around GNAT bug.
with Grt.Stack2; use Grt.Stack2;
with Grt.Disp;
with Grt.Astdio;
with Grt.Signals; use Grt.Signals;
with Grt.Errors; use Grt.Errors;
with Grt.Stacks; use Grt.Stacks;
with Grt.Options;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Rtis_Utils;
with Grt.Hooks;
with Grt.Disp_Signals;
with Grt.Stdio;
with Grt.Stats;
with Grt.Threads; use Grt.Threads;

package body Grt.Processes is
   Last_Time : constant Std_Time := Std_Time'Last;

   --  Table of processes.
   package Process_Table is new GNAT.Table
     (Table_Component_Type => Process_Type,
      Table_Index_Type => Process_Id,
      Table_Low_Bound => 1,
      Table_Initial => 16,
      Table_Increment => 100);

   --  List of non_sensitized processes.
   package Non_Sensitized_Process_Table is new GNAT.Table
     (Table_Component_Type => Process_Id,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 2,
      Table_Increment => 100);

   --  List of processes to be resume at next cycle.
   type Process_Id_Array is array (Natural range <>) of Process_Id;
   type Process_Id_Array_Acc is access Process_Id_Array;

   Resume_Process_Table : Process_Id_Array_Acc;
   Last_Resume_Process : Natural := 0;
   Postponed_Resume_Process_Table : Process_Id_Array_Acc;
   Last_Postponed_Resume_Process : Natural := 0;

   --  Number of postponed processes.
   Nbr_Postponed_Processes : Natural := 0;
   Nbr_Non_Postponed_Processes : Natural := 0;

   --  Number of resumed processes.
   Nbr_Resumed_Processes : Natural := 0;

   procedure Free is new Ada.Unchecked_Deallocation
     (Name => Sensitivity_Acc, Object => Sensitivity_El);

   procedure Init is
   begin
      Process_Table.Init;
   end Init;

   function Get_Nbr_Processes return Natural is
   begin
      return Natural (Process_Table.Last);
   end Get_Nbr_Processes;

   function Get_Nbr_Sensitized_Processes return Natural
   is
      Res : Natural := 0;
   begin
      for I in Process_Table.First .. Process_Table.Last loop
         if Process_Table.Table (I).State = State_Sensitized then
            Res := Res + 1;
         end if;
      end loop;
      return Res;
   end Get_Nbr_Sensitized_Processes;

   function Get_Nbr_Resumed_Processes return Natural is
   begin
      return Nbr_Resumed_Processes;
   end Get_Nbr_Resumed_Processes;

   procedure Process_Register (This : System.Address;
                               Proc : System.Address;
                               Ctxt : Rti_Context;
                               State : Process_State;
                               Postponed : Boolean)
   is
      function To_Proc_Acc is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Proc_Acc);
      Stack : Stack_Type;
   begin
      if State /= State_Sensitized then
         Stack := Stack_Create (Proc, This);
      else
         Stack := Null_Stack;
      end if;
      Process_Table.Increment_Last;
      Process_Table.Table (Process_Table.Last) :=
        (Subprg => To_Proc_Acc (Proc),
         This => This,
         Rti => Ctxt,
         Sensitivity => null,
         Resumed => False,
         Postponed => Postponed,
         State => State,
         Timeout => Bad_Time,
         Stack => Stack);
      --  Used to create drivers.
      Set_Current_Process (Process_Table.Last, null);

      if State /= State_Sensitized then
         Non_Sensitized_Process_Table.Append (Process_Table.Last);
      end if;
      if Postponed then
         Nbr_Postponed_Processes := Nbr_Postponed_Processes + 1;
      else
         Nbr_Non_Postponed_Processes := Nbr_Non_Postponed_Processes + 1;
      end if;
   end Process_Register;

   procedure Ghdl_Process_Register
     (Instance : System.Address;
      Proc : System.Address;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Timeout, False);
   end Ghdl_Process_Register;

   procedure Ghdl_Sensitized_Process_Register
     (Instance : System.Address;
      Proc : System.Address;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Sensitized, False);
   end Ghdl_Sensitized_Process_Register;

   procedure Ghdl_Postponed_Process_Register
     (Instance : System.Address;
      Proc : System.Address;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Timeout, True);
   end Ghdl_Postponed_Process_Register;

   procedure Ghdl_Postponed_Sensitized_Process_Register
     (Instance : System.Address;
      Proc : System.Address;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Sensitized, True);
   end Ghdl_Postponed_Sensitized_Process_Register;

   procedure Verilog_Process_Register (This : System.Address;
                                       Proc : System.Address;
                                       Ctxt : Rti_Context)
   is
      function To_Proc_Acc is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Proc_Acc);
   begin
      Process_Table.Increment_Last;
      Process_Table.Table (Process_Table.Last) :=
        (Rti => Ctxt,
         Sensitivity => null,
         Resumed => False,
         Postponed => False,
         State => State_Sensitized,
         Timeout => Bad_Time,
         Subprg => To_Proc_Acc (Proc),
         This => This,
         Stack => Null_Stack);
      --  Used to create drivers.
      Set_Current_Process (Process_Table.Last, null);
   end Verilog_Process_Register;

   procedure Ghdl_Initial_Register (Instance : System.Address;
                                    Proc : System.Address)
   is
   begin
      Verilog_Process_Register (Instance, Proc, Null_Context);
   end Ghdl_Initial_Register;

   procedure Ghdl_Always_Register (Instance : System.Address;
                                   Proc : System.Address)
   is
   begin
      Verilog_Process_Register (Instance, Proc, Null_Context);
   end Ghdl_Always_Register;

   procedure Ghdl_Process_Add_Sensitivity (Sig : Ghdl_Signal_Ptr)
   is
   begin
      Resume_Process_If_Event (Sig, Process_Table.Last);
   end Ghdl_Process_Add_Sensitivity;

   procedure Resume_Process (Proc : Process_Id)
   is
      P : Process_Type renames Process_Table.Table (Proc);
   begin
      if not P.Resumed then
         P.Resumed := True;
         if P.Postponed then
            Last_Postponed_Resume_Process := Last_Postponed_Resume_Process + 1;
            Postponed_Resume_Process_Table (Last_Postponed_Resume_Process)
              := Proc;
         else
            Last_Resume_Process := Last_Resume_Process + 1;
            Resume_Process_Table (Last_Resume_Process) := Proc;
         end if;
      end if;
   end Resume_Process;

   function Ghdl_Stack2_Allocate (Size : Ghdl_Index_Type)
     return System.Address
   is
   begin
      return Grt.Stack2.Allocate (Get_Stack2, Size);
   end Ghdl_Stack2_Allocate;

   function Ghdl_Stack2_Mark return Mark_Id
   is
      St2 : Stack2_Ptr := Get_Stack2;
   begin
      if St2 = Null_Stack2_Ptr then
         St2 := Grt.Stack2.Create;
         Set_Stack2 (St2);
      end if;
      return Grt.Stack2.Mark (St2);
   end Ghdl_Stack2_Mark;

   procedure Ghdl_Stack2_Release (Mark : Mark_Id) is
   begin
      Grt.Stack2.Release (Get_Stack2, Mark);
   end Ghdl_Stack2_Release;

   function To_Acc is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Process_Acc);

   procedure Ghdl_Process_Wait_Add_Sensitivity (Sig : Ghdl_Signal_Ptr)
   is
      El : Sensitivity_Acc;
   begin
      El := new Sensitivity_El'(Sig => Sig,
                                Next => Get_Current_Process.Sensitivity);
      Get_Current_Process.Sensitivity := El;
   end Ghdl_Process_Wait_Add_Sensitivity;

   procedure Ghdl_Process_Wait_Set_Timeout (Time : Std_Time)
   is
   begin
      if Time < 0 then
         --  LRM93 8.1
         Error ("negative timeout clause");
      end if;
      Get_Current_Process.Timeout := Current_Time + Time;
   end Ghdl_Process_Wait_Set_Timeout;

   function Ghdl_Process_Wait_Suspend return Boolean
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      if Proc.State = State_Sensitized then
         Error ("wait statement in a sensitized process");
      end if;
      --  Suspend this process.
      Proc.State := State_Wait;
--       if Cur_Proc.Timeout = Bad_Time then
--          Cur_Proc.Timeout := Std_Time'Last;
--       end if;
      Stack_Switch (Get_Main_Stack, Proc.Stack);
      return Proc.State = State_Timeout;
   end Ghdl_Process_Wait_Suspend;

   procedure Ghdl_Process_Wait_Close
   is
      Proc : constant Process_Acc := Get_Current_Process;
      El : Sensitivity_Acc;
      N_El : Sensitivity_Acc;
   begin
      El := Proc.Sensitivity;
      Proc.Sensitivity := null;
      while El /= null loop
         N_El := El.Next;
         Free (El);
         El := N_El;
      end loop;
   end Ghdl_Process_Wait_Close;

   procedure Ghdl_Process_Wait_Exit
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      if Proc.State = State_Sensitized then
         Error ("wait statement in a sensitized process");
      end if;
      --  Mark this process as dead, in order to kill it.
      --  It cannot be killed now, since this code is still in the process.
      Proc.State := State_Dead;
      --  Suspend this process.
      Stack_Switch (Get_Main_Stack, Proc.Stack);
   end Ghdl_Process_Wait_Exit;

   procedure Ghdl_Process_Wait_Timeout (Time : Std_Time)
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      if Proc.State = State_Sensitized then
         Error ("wait statement in a sensitized process");
      end if;
      if Time < 0 then
         --  LRM93 8.1
         Error ("negative timeout clause");
      end if;
      Proc.Timeout := Current_Time + Time;
      Proc.State := State_Wait;
      --  Suspend this process.
      Stack_Switch (Get_Main_Stack, Proc.Stack);
   end Ghdl_Process_Wait_Timeout;

   --  Verilog.
   procedure Ghdl_Process_Delay (Del : Ghdl_U32)
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      Proc.Timeout := Current_Time + Std_Time (Del);
      Proc.State := State_Delayed;
   end Ghdl_Process_Delay;

   --  Protected object lock.
   --  Note: there is no real locks, since the kernel is single threading.
   --  Multi lock is allowed, and rules are just checked.
   type Object_Lock is record
      --  The owner of the lock.
      --  Nul_Process_Id means the lock is free.
      Process : Process_Id;
      --  Number of times the lock has been acquired.
      Count : Natural;
   end record;

   type Object_Lock_Acc is access Object_Lock;
   type Object_Lock_Acc_Acc is access Object_Lock_Acc;

   function To_Lock_Acc_Acc is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Object_Lock_Acc_Acc);

   procedure Ghdl_Protected_Enter (Obj : System.Address)
   is
      Lock : Object_Lock_Acc := To_Lock_Acc_Acc (Obj).all;
   begin
      if Lock.Process = Nul_Process_Id then
         if Lock.Count /= 0 then
            Internal_Error ("protected_enter");
         end if;
         Lock.Process := Get_Current_Process_Id;
         Lock.Count := 1;
      else
         if Lock.Process /= Get_Current_Process_Id then
            Internal_Error ("protected_enter(2)");
         end if;
         Lock.Count := Lock.Count + 1;
      end if;
   end Ghdl_Protected_Enter;

   procedure Ghdl_Protected_Leave (Obj : System.Address)
   is
      Lock : Object_Lock_Acc := To_Lock_Acc_Acc (Obj).all;
   begin
      if Lock.Process /= Get_Current_Process_Id then
         Internal_Error ("protected_leave(1)");
      end if;

      if Lock.Count <= 0 then
         Internal_Error ("protected_leave(2)");
      end if;
      Lock.Count := Lock.Count - 1;
      if Lock.Count = 0 then
         Lock.Process := Nul_Process_Id;
      end if;
   end Ghdl_Protected_Leave;

   procedure Ghdl_Protected_Init (Obj : System.Address)
   is
      Lock : Object_Lock_Acc_Acc := To_Lock_Acc_Acc (Obj);
   begin
      Lock.all := new Object_Lock'(Process => Nul_Process_Id,
                                   Count => 0);
   end Ghdl_Protected_Init;

   procedure Ghdl_Protected_Fini (Obj : System.Address)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Object => Object_Lock, Name => Object_Lock_Acc);

      Lock : Object_Lock_Acc_Acc := To_Lock_Acc_Acc (Obj);
   begin
      if Lock.all.Count /= 0 or Lock.all.Process /= Nul_Process_Id then
         Internal_Error ("protected_fini");
      end if;
      Deallocate (Lock.all);
   end Ghdl_Protected_Fini;

   function Compute_Next_Time return Std_Time
   is
      Res : Std_Time;
   begin
      --  f) The time of the next simulation cycle, Tn, is determined by
      --     setting it to the earliest of
      --     1) TIME'HIGH
      Res := Std_Time'Last;

      --     2) The next time at which a driver becomes active, or
      Res := Std_Time'Min (Res, Grt.Signals.Find_Next_Time);

      if Res = Current_Time then
         return Res;
      end if;

      --     3) The next time at which a process resumes.
      for I in Non_Sensitized_Process_Table.First ..
        Non_Sensitized_Process_Table.Last
      loop
         declare
            Pid : Process_Id := Non_Sensitized_Process_Table.Table (I);
            Proc : Process_Type renames Process_Table.Table (Pid);
         begin
            if Proc.State = State_Wait
              and then Proc.Timeout < Res
              and then Proc.Timeout >= 0
            then
               --  No signals to be updated.
               Grt.Signals.Flush_Active_List;

               if Proc.Timeout = Current_Time then
                  --  Can't be better.
                  return Current_Time;
               else
                  Res := Proc.Timeout;
               end if;
            end if;
         end;
      end loop;

      return Res;
   end Compute_Next_Time;

   procedure Disp_Process_Name (Stream : Grt.Stdio.FILEs; Proc : Process_Id)
   is
   begin
      Grt.Rtis_Utils.Put (Stream, Process_Table.Table (Proc).Rti);
   end Disp_Process_Name;

   type Run_Handler is access function return Integer;
   --  pragma Convention (C, Run_Handler);

   function Run_Through_Longjump (Hand : Run_Handler) return Integer;
   pragma Import (C, Run_Through_Longjump, "__ghdl_run_through_longjump");

   --  Run resumed processes.
   --  If POSTPONED is true, resume postponed processes, else resume
   --  non-posponed processes.
   --  Returns one of these values:
   --  No process has been run.
   Run_None : constant Integer := 1;
   --  At least one process was run.
   Run_Resumed : constant Integer := 2;
   --  Simulation is finished.
   Run_Finished : constant Integer := 3;
   --  Failure, simulation should stop.
   Run_Failure : constant Integer := -1;

   Mt_Last : Natural;
   Mt_Table : Process_Id_Array_Acc;
   Mt_Index : aliased Natural;

   procedure Run_Processes_Threads
   is
      Pid : Process_Id;
      Idx : Natural;
   begin
      loop
         --  Atomically get a process to be executed
         Idx := Grt.Threads.Atomic_Inc (Mt_Index'Access);
         if Idx > Mt_Last then
            return;
         end if;
         Pid := Mt_Table (Idx);

         declare
            Proc : Process_Type renames Process_Table.Table (Pid);
         begin
            if Grt.Options.Trace_Processes then
               Grt.Astdio.Put ("run process ");
               Disp_Process_Name (Stdio.stdout, Pid);
               Grt.Astdio.Put (" [");
               Grt.Astdio.Put (Stdio.stdout, Proc.This);
               Grt.Astdio.Put ("]");
               Grt.Astdio.New_Line;
            end if;
            if not Proc.Resumed then
               Internal_Error ("run non-resumed process");
            end if;
            Proc.Resumed := False;
            Set_Current_Process
              (Pid, To_Acc (Process_Table.Table (Pid)'Address));
            if Proc.State = State_Sensitized then
               Proc.Subprg.all (Proc.This);
            else
               Stack_Switch (Proc.Stack, Get_Main_Stack);
            end if;
            if Grt.Options.Checks then
               Ghdl_Signal_Internal_Checks;
               Grt.Stack2.Check_Empty (Get_Stack2);
            end if;
         end;
      end loop;
   end Run_Processes_Threads;

   function Run_Processes (Postponed : Boolean) return Integer
   is
      Table : Process_Id_Array_Acc;
      Last : Natural;
   begin
      if Options.Flag_Stats then
         Stats.Start_Processes;
      end if;

      if Postponed then
         Table := Postponed_Resume_Process_Table;
         Last := Last_Postponed_Resume_Process;
         Last_Postponed_Resume_Process := 0;
      else
         Table := Resume_Process_Table;
         Last := Last_Resume_Process;
         Last_Resume_Process := 0;
      end if;
      Nbr_Resumed_Processes := Nbr_Resumed_Processes + Last;

      if Options.Nbr_Threads = 1 then
         for I in 1 .. Last loop
            declare
               Pid : constant Process_Id := Table (I);
               Proc : Process_Type renames Process_Table.Table (Pid);
            begin
               if not Proc.Resumed then
                  Internal_Error ("run non-resumed process");
               end if;
               if Grt.Options.Trace_Processes then
                  Grt.Astdio.Put ("run process ");
                  Disp_Process_Name (Stdio.stdout, Pid);
                  Grt.Astdio.Put (" [");
                  Grt.Astdio.Put (Stdio.stdout, Proc.This);
                  Grt.Astdio.Put ("]");
                  Grt.Astdio.New_Line;
               end if;

               Proc.Resumed := False;
               Set_Current_Process
                 (Pid, To_Acc (Process_Table.Table (Pid)'Address));
               if Proc.State = State_Sensitized then
                  Proc.Subprg.all (Proc.This);
               else
                  Stack_Switch (Proc.Stack, Get_Main_Stack);
               end if;
               if Grt.Options.Checks then
                  Ghdl_Signal_Internal_Checks;
                  Grt.Stack2.Check_Empty (Get_Stack2);
               end if;
            end;
         end loop;
      else
         Mt_Last := Last;
         Mt_Table := Table;
         Mt_Index := 1;
         Threads.Run_Parallel (Run_Processes_Threads'Access);
      end if;

      if Last >= 1 then
         return Run_Resumed;
      else
         return Run_None;
      end if;
   end Run_Processes;

   function Initialization_Phase return Integer
   is
      Status : Integer;
   begin
      --  LRM93 12.6.4
      --  At the beginning of initialization, the current time, Tc, is assumed
      --  to be 0 ns.
      Current_Time := 0;

      --  The initialization phase consists of the following steps:
      --  - The driving value and the effective value of each explicitly
      --    declared signal are computed, and the current value of the signal
      --    is set to the effective value.  This value is assumed to have been
      --    the value of the signal for an infinite length of time prior to
      --    the start of the simulation.
      Init_Signals;

      --  - The value of each implicit signal of the form S'Stable(T) or
      --    S'Quiet(T) is set to true.  The value of each implicit signal of
      --    the form S'Delayed is set to the initial value of its prefix, S.
      --  GHDL: already done when the signals are created.
      null;

      --  - The value of each implicit GUARD signal is set to the result of
      --    evaluating the corresponding guard expression.
      null;

      for I in Process_Table.First .. Process_Table.Last loop
         Resume_Process (I);
      end loop;

      --  - Each nonpostponed process in the model is executed until it
      --    suspends.
      Status := Run_Processes (Postponed => False);
      if Status = Run_Failure then
         return Run_Failure;
      end if;

      --  - Each postponed process in the model is executed until it suspends.
      Status := Run_Processes (Postponed => True);
      if Status = Run_Failure then
         return Run_Failure;
      end if;

      --  - The time of the next simulation cycle (which in this case is the
      --    first simulation cycle), Tn, is calculated according to the rules
      --    of step f of the simulation cycle, below.
      Current_Time := Compute_Next_Time;

      return Run_Resumed;
   end Initialization_Phase;

   --  Launch a simulation cycle.
   --  Set FINISHED to true if this is the last cycle.
   function Simulation_Cycle return Integer
   is
      Tn : Std_Time;
      Status : Integer;
   begin
      --  LRM93 12.6.4
      --  A simulation cycle consists of the following steps:
      --
      --  a) The current time, Tc is set equal to Tn.  Simulation is complete
      --     when Tn = TIME'HIGH and there are no active drivers or process
      --     resumptions at Tn.
      --  GHDL: this is done at the last step of the cycle.
      null;

      --  b) Each active explicit signal in the model is updated.  (Events
      --     may occur on signals as a result).
      --  c) Each implicit signal in the model is updated.  (Events may occur
      --     on signals as a result.)
      if Options.Flag_Stats then
         Stats.Start_Update;
      end if;
      Update_Signals;
      if Options.Flag_Stats then
         Stats.Start_Resume;
      end if;

      --  d) For each process P, if P is currently sensitive to a signal S and
      --     if an event has occured on S in this simulation cycle, then P
      --     resumes.
      for I in Non_Sensitized_Process_Table.First ..
        Non_Sensitized_Process_Table.Last
      loop
         declare
            Pid : Process_Id := Non_Sensitized_Process_Table.Table (I);
            Proc : Process_Type renames Process_Table.Table (Pid);
            El : Sensitivity_Acc;
         begin
            case Proc.State is
               when State_Sensitized =>
                  null;
               when State_Delayed =>
                  if Proc.Timeout = Current_Time then
                     Proc.Timeout := Bad_Time;
                     Resume_Process (Pid);
                     Proc.State := State_Sensitized;
                  end if;
               when State_Wait =>
                  if Proc.Timeout = Current_Time then
                     Proc.Timeout := Bad_Time;
                     Resume_Process (Pid);
                     Proc.State := State_Timeout;
                  else
                     El := Proc.Sensitivity;
                     while El /= null loop
                        if El.Sig.Event then
                           Resume_Process (Pid);
                           exit;
                        else
                           El := El.Next;
                        end if;
                     end loop;
                  end if;
               when State_Timeout =>
                  Internal_Error ("process in timeout");
               when State_Dead =>
                  null;
            end case;
         end;
      end loop;

      --  e) Each nonpostponed that has resumed in the current simulation cycle
      --     is executed until it suspends.
      Status := Run_Processes (Postponed => False);
      if Status = Run_Failure then
         return Run_Failure;
      end if;

      --  f) The time of the next simulation cycle, Tn, is determined by
      --     setting it to the earliest of
      --     1) TIME'HIGH
      --     2) The next time at which a driver becomes active, or
      --     3) The next time at which a process resumes.
      --     If Tn = Tc, then the next simulation cycle (if any) will be a
      --     delta cycle.
      if Options.Flag_Stats then
         Stats.Start_Next_Time;
      end if;
      Tn := Compute_Next_Time;

      --  g) If the next simulation cycle will be a delta cycle, the remainder
      --     of the step is skipped.
      --     Otherwise, each postponed process that has resumed but has not
      --     been executed since its last resumption is executed until it
      --     suspends.  Then Tn is recalculated according to the rules of
      --     step f.  It is an error if the execution of any postponed
      --     process causes a delta cycle to occur immediatly after the
      --     current simulation cycle.
      if Tn = Current_Time then
         if Current_Time = Last_Time and then Status = Run_None then
            return Run_Finished;
         else
            Current_Delta := Current_Delta + 1;
            return Run_Resumed;
         end if;
      else
         Current_Delta := 0;
         if Nbr_Postponed_Processes /= 0 then
            Status := Run_Processes (Postponed => True);
         end if;
         if Status = Run_Resumed then
            Flush_Active_List;
            if Options.Flag_Stats then
               Stats.Start_Next_Time;
            end if;
            Tn := Compute_Next_Time;
            if Tn = Current_Time then
               Error ("postponed process causes a delta cycle");
            end if;
         elsif Status = Run_Failure then
            return Run_Failure;
         end if;
         Current_Time := Tn;
         return Run_Resumed;
      end if;
   end Simulation_Cycle;

   function Simulation return Integer
   is
      use Options;
      Status : Integer;
   begin
      if Nbr_Threads /= 1 then
         Threads.Init;
      end if;

--       if Disp_Sig_Types then
--          Grt.Disp.Disp_Signals_Type;
--       end if;

      --  Allocate processes arrays.
      Resume_Process_Table :=
        new Process_Id_Array (1 .. Nbr_Non_Postponed_Processes);
      Postponed_Resume_Process_Table :=
        new Process_Id_Array (1 .. Nbr_Postponed_Processes);
      Grt.Hooks.Call_Start_Hooks;

      Status := Run_Through_Longjump (Initialization_Phase'Access);
      if Status /= Run_Resumed then
         return -1;
      end if;

      Current_Delta := 0;
      Nbr_Delta_Cycles := 0;
      Nbr_Cycles := 0;
      if Trace_Signals then
         Grt.Disp_Signals.Disp_All_Signals;
      end if;

      if Current_Time /= 0 then
         --  This is the end of a cycle.
         Cycle_Time := 0;
         Grt.Hooks.Call_Cycle_Hooks;
      end if;

      loop
         Cycle_Time := Current_Time;
         if Disp_Time then
            Grt.Disp.Disp_Now;
         end if;
         Status := Run_Through_Longjump (Simulation_Cycle'Access);
         exit when Status = Run_Failure;
         if Trace_Signals then
            Grt.Disp_Signals.Disp_All_Signals;
         end if;

         --  Statistics.
         if Current_Delta = 0 then
            Nbr_Cycles := Nbr_Cycles + 1;
         else
            Nbr_Delta_Cycles := Nbr_Delta_Cycles + 1;
         end if;

         exit when Status = Run_Finished;
         if Current_Delta = 0 then
            Grt.Hooks.Call_Cycle_Hooks;
         end if;

         if Current_Delta >= Stop_Delta then
            Error ("simulation stopped by --stop-delta");
            exit;
         end if;
         if Current_Time > Stop_Time then
            Info ("simulation stopped by --stop-time");
            exit;
         end if;
      end loop;

      if Nbr_Threads /= 1 then
         Threads.Finish;
      end if;

      Grt.Hooks.Call_Finish_Hooks;

      if Status = Run_Failure then
         return -1;
      else
         return 0;
      end if;
   end Simulation;

end Grt.Processes;
