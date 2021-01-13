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
with Grt.Table;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Disp;
with Grt.Astdio;
with Grt.Astdio.Vhdl; use Grt.Astdio.Vhdl;
with Grt.Errors; use Grt.Errors;
with Grt.Errors_Exec; use Grt.Errors_Exec;
with Grt.Options;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Rtis_Utils;
with Grt.Hooks;
with Grt.Callbacks; use Grt.Callbacks;
with Grt.Disp_Signals;
with Grt.Stats;
with Grt.Threads; use Grt.Threads;
pragma Elaborate_All (Grt.Table);
with Grt.Stdio;

package body Grt.Processes is
   Last_Time : constant Std_Time := Std_Time'Last;

   --  Identifier for a process.
   type Process_Id is new Integer;

   --  Table of processes.
   package Process_Table is new Grt.Table
     (Table_Component_Type => Process_Acc,
      Table_Index_Type => Process_Id,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   type Finalizer_Type is record
      --  Subprogram containing process code.
      Subprg : Proc_Acc;

      --  Instance (THIS parameter) for the subprogram.
      This : Instance_Acc;
   end record;

   --  List of finalizer.
   package Finalizer_Table is new Grt.Table
     (Table_Component_Type => Finalizer_Type,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 2);

   --  List of processes to be resume at next cycle.
   type Process_Acc_Array is array (Natural range <>) of Process_Acc;
   type Process_Acc_Array_Acc is access Process_Acc_Array;

   Resume_Process_Table : Process_Acc_Array_Acc;
   Last_Resume_Process : Natural := 0;
   Postponed_Resume_Process_Table : Process_Acc_Array_Acc;
   Last_Postponed_Resume_Process : Natural := 0;

   --  Number of processes.
   Nbr_Postponed_Processes : Natural := 0;
   Nbr_Non_Postponed_Processes : Natural := 0;

   --  Number of resumed processes.
   Nbr_Resumed_Processes : Long_Long_Integer := 0;

   --  Earliest time out within non-sensitized processes.
   Process_First_Timeout : Std_Time := Last_Time;
   Process_Timeout_Chain : Process_Acc := null;

   Elab_Process : Process_Acc;

   procedure Init is
   begin
      --  Create a dummy process so that elaboration has a context.
      Elab_Process := new Process_Type'(Subprg => null,
                                        This => null,
                                        Rti => Null_Context,
                                        Sensitivity => null,
                                        Stack2 => Null_Stack2_Ptr,
                                        Resumed => False,
                                        Postponed => False,
                                        State => State_Sensitized,
                                        Timeout => Bad_Time,
                                        Timeout_Chain_Next => null,
                                        Timeout_Chain_Prev => null);
      Set_Current_Process (Elab_Process);

      --  LRM93 12.3 Elaboration of a declarative part
      --  During static elaboration, the function STD.STANDARD.NOW (see 14.2)
      --  returns the vallue 0 ns.
      Current_Time := 0;
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

   function Get_Nbr_Resumed_Processes return Long_Long_Integer is
   begin
      return Nbr_Resumed_Processes;
   end Get_Nbr_Resumed_Processes;

   function Get_Rti_Context (Proc : Process_Acc) return Rti_Context is
   begin
      return Proc.Rti;
   end Get_Rti_Context;

   procedure Process_Register (This : Instance_Acc;
                               Proc : Proc_Acc;
                               Ctxt : Rti_Context;
                               State : Process_State;
                               Postponed : Boolean)
   is
      P : Process_Acc;
   begin
      P := new Process_Type'(Subprg => Proc,
                             This => This,
                             Rti => Ctxt,
                             Sensitivity => null,
                             Stack2 => Null_Stack2_Ptr,
                             Resumed => False,
                             Postponed => Postponed,
                             State => State,
                             Timeout => Bad_Time,
                             Timeout_Chain_Next => null,
                             Timeout_Chain_Prev => null);
      Process_Table.Append (P);
      --  Used to create drivers.
      Set_Current_Process (P);
      if Postponed then
         Nbr_Postponed_Processes := Nbr_Postponed_Processes + 1;
      else
         Nbr_Non_Postponed_Processes := Nbr_Non_Postponed_Processes + 1;
      end if;
   end Process_Register;

   procedure Ghdl_Process_Register
     (Instance : Instance_Acc;
      Proc : Proc_Acc;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Ready, False);
   end Ghdl_Process_Register;

   procedure Ghdl_Sensitized_Process_Register
     (Instance : Instance_Acc;
      Proc : Proc_Acc;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Sensitized, False);
   end Ghdl_Sensitized_Process_Register;

   procedure Ghdl_Postponed_Process_Register
     (Instance : Instance_Acc;
      Proc : Proc_Acc;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Ready, True);
   end Ghdl_Postponed_Process_Register;

   procedure Ghdl_Postponed_Sensitized_Process_Register
     (Instance : Instance_Acc;
      Proc : Proc_Acc;
      Ctxt : Ghdl_Rti_Access;
      Addr : System.Address)
   is
   begin
      Process_Register (Instance, Proc, (Addr, Ctxt), State_Sensitized, True);
   end Ghdl_Postponed_Sensitized_Process_Register;

   procedure Verilog_Process_Register (This : Instance_Acc;
                                       Proc : Proc_Acc;
                                       Ctxt : Rti_Context)
   is
      P : Process_Acc;
   begin
      P := new Process_Type'(Rti => Ctxt,
                             Sensitivity => null,
                             Resumed => False,
                             Postponed => False,
                             State => State_Sensitized,
                             Stack2 => Null_Stack2_Ptr,
                             Timeout => Bad_Time,
                             Timeout_Chain_Next => null,
                             Timeout_Chain_Prev => null,
                             Subprg => Proc,
                             This => This);
      Process_Table.Append (P);
      Nbr_Non_Postponed_Processes := Nbr_Non_Postponed_Processes + 1;
      --  Used to create drivers.
      Set_Current_Process (P);
   end Verilog_Process_Register;

   procedure Ghdl_Initial_Register (Instance : Instance_Acc;
                                    Proc : Proc_Acc)
   is
   begin
      Verilog_Process_Register (Instance, Proc, Null_Context);
   end Ghdl_Initial_Register;

   procedure Ghdl_Always_Register (Instance : Instance_Acc;
                                   Proc : Proc_Acc)
   is
   begin
      Verilog_Process_Register (Instance, Proc, Null_Context);
   end Ghdl_Always_Register;

   function Ghdl_Register_Foreign_Process
     (Instance : Instance_Acc; Proc : Proc_Acc) return Process_Acc is
   begin
      Verilog_Process_Register (Instance, Proc, Null_Context);
      return Get_Current_Process;
   end Ghdl_Register_Foreign_Process;

   procedure Ghdl_Process_Add_Sensitivity (Sig : Ghdl_Signal_Ptr)
   is
   begin
      Resume_Process_If_Event
        (Sig, Process_Table.Table (Process_Table.Last));
   end Ghdl_Process_Add_Sensitivity;

   procedure Ghdl_Finalize_Register (Instance : Instance_Acc;
                                     Proc : Proc_Acc)
   is
   begin
      Finalizer_Table.Append (Finalizer_Type'(Proc, Instance));
   end Ghdl_Finalize_Register;

   procedure Call_Finalizers is
      El : Finalizer_Type;
   begin
      for I in Finalizer_Table.First .. Finalizer_Table.Last loop
         El := Finalizer_Table.Table (I);
         El.Subprg.all (El.This);
      end loop;
   end Call_Finalizers;

   procedure Resume_Process (Proc : Process_Acc)
   is
   begin
      if not Proc.Resumed then
         Proc.Resumed := True;
         if Proc.Postponed then
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
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      return Grt.Stack2.Allocate (Proc.Stack2, Size);
   end Ghdl_Stack2_Allocate;

   function Ghdl_Stack2_Mark return Mark_Id
   is
      Proc : constant Process_Acc := Get_Current_Process;
      St2 : Stack2_Ptr;
   begin
      St2 := Proc.Stack2;

      --  Check that stack2 has been created.  This check is done only here,
      --  because Mark is called before Release (obviously) but also before
      --  Allocate.
      if St2 = Null_Stack2_Ptr then
         if Proc.State = State_Sensitized then
            --  Sensitized processes share the stack2, as the stack2 is empty
            --  when sensitized processes suspend.
            St2 := Get_Common_Stack2;
         else
            St2 := Grt.Stack2.Create;
         end if;
         Proc.Stack2 := St2;
      end if;

      return Grt.Stack2.Mark (St2);
   end Ghdl_Stack2_Mark;

   procedure Ghdl_Stack2_Release (Mark : Mark_Id)
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      Grt.Stack2.Release (Proc.Stack2, Mark);
   end Ghdl_Stack2_Release;

   procedure Free is new Ada.Unchecked_Deallocation
     (Action_List, Action_List_Acc);

   --  List of unused action_list to be recycled.
   Old_Action_List : Action_List_Acc;

   procedure Ghdl_Process_Wait_Add_Sensitivity (Sig : Ghdl_Signal_Ptr)
   is
      Proc : constant Process_Acc := Get_Current_Process;
      El : Action_List_Acc;
   begin
      --  Allocate a structure.
      if Old_Action_List = null then
         El := new Action_List (Dynamic => True);
      else
         El := Old_Action_List;
         Old_Action_List := El.Next;
         pragma Assert (El.Dynamic);
      end if;

      El.all := Action_List'(Dynamic => True,
                             Next => Sig.Event_List,
                             Proc => Proc,
                             Prev => null,
                             Sig => Sig,
                             Chain => Proc.Sensitivity);

      --  Put EL on SIG event list.
      if Sig.Event_List /= null and then Sig.Event_List.Dynamic then
         Sig.Event_List.Prev := El;
      end if;
      Sig.Event_List := El;

      --  Put EL on PROC sensitivity list.
      Proc.Sensitivity := El;
   end Ghdl_Process_Wait_Add_Sensitivity;

   procedure Update_Process_First_Timeout (Proc : Process_Acc) is
   begin
      --  Update Process_First_Timeout
      if Proc.Timeout < Process_First_Timeout then
         Process_First_Timeout := Proc.Timeout;
      end if;

      --  Append PROC on Process_Timeout_Chain.
      Proc.Timeout_Chain_Next := Process_Timeout_Chain;
      Proc.Timeout_Chain_Prev := null;
      if Process_Timeout_Chain /= null then
         Process_Timeout_Chain.Timeout_Chain_Prev := Proc;
      end if;
      Process_Timeout_Chain := Proc;
   end Update_Process_First_Timeout;

   procedure Remove_Process_From_Timeout_Chain (Proc : Process_Acc) is
   begin
      --  Remove Proc from the timeout list.
      if Proc.Timeout_Chain_Prev /= null then
         Proc.Timeout_Chain_Prev.Timeout_Chain_Next :=
           Proc.Timeout_Chain_Next;
      elsif Process_Timeout_Chain = Proc then
         --  Only if Proc is in the chain.
         Process_Timeout_Chain := Proc.Timeout_Chain_Next;
      end if;
      if Proc.Timeout_Chain_Next /= null then
         Proc.Timeout_Chain_Next.Timeout_Chain_Prev :=
           Proc.Timeout_Chain_Prev;
         Proc.Timeout_Chain_Next := null;
      end if;
      --  Be sure a second call won't corrupt the chain.
      Proc.Timeout_Chain_Prev := null;
   end Remove_Process_From_Timeout_Chain;

   procedure Ghdl_Process_Wait_Set_Timeout (Time : Std_Time;
                                            Filename : Ghdl_C_String;
                                            Line : Ghdl_I32)
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      if Time < 0 then
         --  LRM93 8.1
         Error ("negative timeout clause", Filename, Line);
      end if;
      Proc.Timeout := Current_Time + Time;
      Update_Process_First_Timeout (Proc);
   end Ghdl_Process_Wait_Set_Timeout;

   function Ghdl_Process_Wait_Timed_Out return Boolean
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      -- Note: in case of timeout, the timeout is removed when process is
      -- woken up.
      return Proc.State = State_Timeout;
   end Ghdl_Process_Wait_Timed_Out;

   procedure Ghdl_Process_Wait_Suspend
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      if Proc.State = State_Sensitized then
         Error ("wait statement in a sensitized process");
      end if;
      --  Suspend this process.
      Proc.State := State_Wait;
   end Ghdl_Process_Wait_Suspend;

   procedure Ghdl_Process_Wait_Close
   is
      Proc : constant Process_Acc := Get_Current_Process;
      El : Action_List_Acc;
      N_El : Action_List_Acc;
   begin
      --  Remove the action_list for sensitivity.
      El := Proc.Sensitivity;
      Proc.Sensitivity := null;
      while El /= null loop
         pragma Assert (El.Proc = Proc);
         pragma Assert (El.Dynamic);

         --  Remove EL from signal Event_List.
         if El.Prev = null then
            --  First element of the list; set list head.
            El.Sig.Event_List := El.Next;
         else
            --  Previous elements must be dynamic ones.
            pragma Assert (El.Prev.Dynamic);
            El.Prev.Next := El.Next;
         end if;
         if El.Next /= null and then El.Next.Dynamic then
            --  No Prev link in non-dynamic element.
            El.Next.Prev := El.Prev;
         end if;

         N_El := El.Chain;

         --  Free element...
         if Boolean'(True) then
            --  ... by moving it to the recycle list.
            El.Next := Old_Action_List;
            Old_Action_List := El;
         else
            --  ... by releasing memory.
            Free (El);
         end if;

         El := N_El;
      end loop;

      --  Remove Proc from the timeout list.
      Remove_Process_From_Timeout_Chain (Proc);

      --  This is necessary when the process has been woken-up by an event
      --  before the timeout triggers.
      if Process_First_Timeout = Proc.Timeout then
         --  Remove the timeout.
         Proc.Timeout := Bad_Time;

         declare
            Next_Timeout : Std_Time;
            P : Process_Acc;
         begin
            Next_Timeout := Last_Time;
            P := Process_Timeout_Chain;
            while P /= null loop
               case P.State is
                  when State_Delayed
                    | State_Wait =>
                     if P.Timeout > 0
                       and then P.Timeout < Next_Timeout
                     then
                        Next_Timeout := P.Timeout;
                     end if;
                  when others =>
                     null;
               end case;
               P := P.Timeout_Chain_Next;
            end loop;
            Process_First_Timeout := Next_Timeout;
         end;
      else
         --  Remove the timeout.
         Proc.Timeout := Bad_Time;
      end if;
      Proc.State := State_Ready;
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
   end Ghdl_Process_Wait_Exit;

   procedure Ghdl_Process_Wait_Timeout (Time : Std_Time;
                                        Filename : Ghdl_C_String;
                                        Line : Ghdl_I32)
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      if Proc.State = State_Sensitized then
         Error ("wait statement in a sensitized process");
      end if;
      if Time < 0 then
         --  LRM93 8.1
         Error ("negative timeout clause", Filename, Line);
      end if;
      Proc.State := State_Delayed;
      if Time <= Std_Time'Last - Current_Time then
         Proc.Timeout := Current_Time + Time;
         Update_Process_First_Timeout (Proc);
      else
         --  Delay past the end of the times.
         Proc.Timeout := Std_Time'Last;
      end if;
   end Ghdl_Process_Wait_Timeout;

   --  Verilog.
   procedure Ghdl_Process_Delay (Del : Ghdl_U32)
   is
      Proc : constant Process_Acc := Get_Current_Process;
   begin
      Proc.Timeout := Current_Time + Std_Time (Del);
      Proc.State := State_Delayed;
      Update_Process_First_Timeout (Proc);
   end Ghdl_Process_Delay;

   --  Protected object lock.
   --  Note: there is no real locks, since the kernel is single threading.
   --  Multi lock is allowed, and rules are just checked.
   type Object_Lock is record
      --  The owner of the lock.
      --  Nul_Process_Id means the lock is free.
      Process : Process_Acc;
      --  Number of times the lock has been acquired.
      Count : Natural;
   end record;

   type Object_Lock_Acc is access Object_Lock;
   type Object_Lock_Acc_Acc is access Object_Lock_Acc;

   function To_Lock_Acc_Acc is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Object_Lock_Acc_Acc);

   procedure Ghdl_Protected_Enter (Obj : System.Address)
   is
      Lock : constant Object_Lock_Acc := To_Lock_Acc_Acc (Obj).all;
   begin
      if Lock.Count = 0 then
         --  Protected object not locked.
         if Lock.Process /= null then
            --  Sanity check failed: count must be 0.
            Internal_Error ("protected_enter");
         end if;

         --  Note: during elaboration, there is no current process.
         Lock.Process := Get_Current_Process;
         Lock.Count := 1;
      else
         --  Protected object already locked.
         if Lock.Process /= Get_Current_Process then
            --  Should be locked by the current process.
            Internal_Error ("protected_enter(2)");
         end if;
         Lock.Count := Lock.Count + 1;
      end if;
   end Ghdl_Protected_Enter;

   procedure Ghdl_Protected_Leave (Obj : System.Address)
   is
      Lock : constant Object_Lock_Acc := To_Lock_Acc_Acc (Obj).all;
   begin
      if Lock.Process /= Get_Current_Process then
         Internal_Error ("protected_leave(1)");
      end if;

      if Lock.Count = 0 then
         Internal_Error ("protected_leave(2)");
      end if;
      Lock.Count := Lock.Count - 1;
      if Lock.Count = 0 then
         Lock.Process := null;
      end if;
   end Ghdl_Protected_Leave;

   procedure Ghdl_Protected_Init (Obj : System.Address)
   is
      Lock : constant Object_Lock_Acc_Acc := To_Lock_Acc_Acc (Obj);
   begin
      Lock.all := new Object_Lock'(Process => null, Count => 0);
   end Ghdl_Protected_Init;

   procedure Ghdl_Protected_Fini (Obj : System.Address)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Object => Object_Lock, Name => Object_Lock_Acc);

      Lock : constant Object_Lock_Acc_Acc := To_Lock_Acc_Acc (Obj);
   begin
      if Lock.all.Count /= 0 or Lock.all.Process /= null then
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

      --     3) The next time at which a process resumes.
      Res := Std_Time'Min (Res, Process_First_Timeout);

      --  LRM08 14.7.5.1 Model execution
      --    d) The next time at which a registered and enabled vhpiCbAfterDelay
      --    [...] callback is to occur.
      Res := Std_Time'Min (Res, Get_First_Time (Hooks.Cb_After_Delay));

      if Res = Current_Time then
         return Res;
      end if;

      --     2) The next time at which a driver becomes active, or [...]
      Res := Grt.Signals.Find_Next_Time (Res);

      --  Note that Find_Next_Time has a side effect: it updates the
      --  active_chain.  That's the reason why it is the last.
      return Res;
   end Compute_Next_Time;

   procedure Disp_Process_Name (Stream : Grt.Stdio.FILEs; Proc : Process_Acc)
   is
   begin
      Grt.Rtis_Utils.Put (Stream, Proc.Rti);
   end Disp_Process_Name;

   procedure Disp_All_Processes
   is
      use Grt.Stdio;
      use Grt.Astdio;
   begin
      for I in Process_Table.First .. Process_Table.Last loop
         declare
            Proc : constant Process_Acc := Process_Table.Table (I);
         begin
            Disp_Process_Name (stdout, Proc);
            New_Line (stdout);
            Put (stdout, "  State: ");
            case Proc.State is
               when State_Sensitized =>
                  Put (stdout, "sensitized");
               when State_Wait =>
                  Put (stdout, "wait");
                  if Proc.Timeout /= Bad_Time then
                     Put (stdout, " until ");
                     Put_Time (stdout, Proc.Timeout);
                  end if;
               when State_Ready =>
                  Put (stdout, "ready");
               when State_Timeout =>
                  Put (stdout, "timeout");
               when State_Delayed =>
                  Put (stdout, "delayed");
               when State_Dead =>
                  Put (stdout, "dead");
            end case;
--              Put (stdout, ": time: ");
--              Put_U64 (stdout, Proc.Stats_Time);
--              Put (stdout, ", runs: ");
--              Put_U32 (stdout, Proc.Stats_Run);
            New_Line (stdout);
         end;
      end loop;
   end Disp_All_Processes;

   pragma Unreferenced (Disp_All_Processes);

   --  Run resumed processes.
   --  If POSTPONED is true, resume postponed processes, else resume
   --  non-posponed processes.

   Mt_Last : Natural;
   Mt_Table : Process_Acc_Array_Acc;
   Mt_Index : aliased Natural;

   procedure Run_Processes_Threads
   is
      Proc : Process_Acc;
      Idx : Natural;
   begin
      loop
         --  Atomically get a process to be executed
         Idx := Grt.Threads.Atomic_Inc (Mt_Index'Access);
         if Idx > Mt_Last then
            return;
         end if;
         Proc := Mt_Table (Idx);

         if Grt.Options.Trace_Processes then
            Grt.Astdio.Put ("run process ");
            Disp_Process_Name (Stdio.stdout, Proc);
            Grt.Astdio.Put (" [");
            Grt.Astdio.Put (Stdio.stdout, To_Address (Proc.This));
            Grt.Astdio.Put ("]");
            Grt.Astdio.New_Line;
         end if;
         if not Proc.Resumed then
            Internal_Error ("run non-resumed process");
         end if;
         Proc.Resumed := False;
         Set_Current_Process (Proc);
         Proc.Subprg.all (Proc.This);
         if Grt.Options.Checks then
            Ghdl_Signal_Internal_Checks;
         end if;
      end loop;
   end Run_Processes_Threads;

   function Run_Processes (Postponed : Boolean) return Integer
   is
      Table : Process_Acc_Array_Acc;
      Last : Natural;
   begin
      if Postponed then
         null;
      else
         Call_Callbacks (Hooks.Cb_Start_Of_Processes);
      end if;

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
      Nbr_Resumed_Processes :=
        Nbr_Resumed_Processes + Long_Long_Integer (Last);

      if Options.Nbr_Threads = 1 then
         for I in 1 .. Last loop
            declare
               Proc : constant Process_Acc := Table (I);
            begin
               if not Proc.Resumed then
                  Internal_Error ("run non-resumed process");
               end if;
               if Grt.Options.Trace_Processes then
                  Grt.Astdio.Put ("run process ");
                  Disp_Process_Name (Stdio.stdout, Proc);
                  Grt.Astdio.Put (" [");
                  Grt.Astdio.Put (Stdio.stdout, To_Address (Proc.This));
                  Grt.Astdio.Put ("]");
                  Grt.Astdio.New_Line;
               end if;

               Proc.Resumed := False;
               Set_Current_Process (Proc);
               Proc.Subprg.all (Proc.This);
               if Grt.Options.Checks then
                  if Proc.State = State_Sensitized
                    and then not Is_Empty (Proc.Stack2)
                  then
                     --  A non-sensitized process may store its state
                     --  on stack2.
                     Internal_Error ("non-empty stack2");
                  end if;
                  Ghdl_Signal_Internal_Checks;
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

   procedure Initialization_Phase
   is
      Status : Integer;
      pragma Unreferenced (Status);
   begin
      --  Allocate processes arrays.
      Resume_Process_Table :=
        new Process_Acc_Array (1 .. Nbr_Non_Postponed_Processes);
      Postponed_Resume_Process_Table :=
        new Process_Acc_Array (1 .. Nbr_Postponed_Processes);

      --  LRM93 12.6.4
      --  At the beginning of initialization, the current time, Tc, is assumed
      --  to be 0 ns.
      --
      --  GHDL: already initialized before elaboration.
      pragma Assert (Current_Time = 0);

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
         Resume_Process (Process_Table.Table (I));
      end loop;

      --  - Each nonpostponed process in the model is executed until it
      --    suspends.
      Status := Run_Processes (Postponed => False);

      --  - Each postponed process in the model is executed until it suspends.
      Status := Run_Processes (Postponed => True);

      --  - The time of the next simulation cycle (which in this case is the
      --    first simulation cycle), Tn, is calculated according to the rules
      --    of step f of the simulation cycle, below.
      Next_Time := Compute_Next_Time;
      if Next_Time /= 0 then
         if Has_Callbacks (Hooks.Cb_Last_Known_Delta) then
            Call_Callbacks (Hooks.Cb_Last_Known_Delta);
            Flush_Active_Chain;
            Next_Time := Compute_Next_Time;
         end if;
      end if;

      --  Clear current_delta, will be set by Simulation_Cycle.
      Current_Delta := 0;
   end Initialization_Phase;

   --  Launch a simulation cycle.
   function Simulation_Cycle return Integer
   is
      Tn : Std_Time;
      Status : Integer;
   begin
      --  LRM08 14.7.5.3 Simulation cycle (ex LRM93 12.6.4)
      --  A simulation cycle consists of the following steps:
      --
      --  a) The current time, Tc is set equal to Tn.  Simulation is complete
      --     when Tn = TIME'HIGH and there are no active drivers or process
      --     resumptions at Tn.
      --  GHDL: the check is done at the last step of the cycle.
      Current_Time := Next_Time;
      if Grt.Options.Disp_Time then
         Grt.Disp.Disp_Now;
      end if;

      --  b) The following actions occur in the indicated order:
      --     1) If the current simulation cycle is not a delta cycle, each
      --        registered and enabled vhpiCbNextTimeStep and
      --        vhpiCbRepNextTimeStep callback is executed [TODO]
      if Current_Delta = 0 then
         Call_Callbacks (Hooks.Cb_Next_Time_Step);
      end if;

      --     2) Each registered and enabled vhpiCbStartOfNextCycle and
      --        vhpiCbRepStartOfNextCycle callback is executed [TODO]
      --     3) Each registered and enabled vhpiCbAfterDelay and
      --        vhpiCbRepAfterDelay callback is executed.
      if Current_Time = Get_First_Time (Hooks.Cb_After_Delay) then
         Call_Time_Callbacks (Hooks.Cb_After_Delay);
         if Options.Break_Simulation then
            return Run_Stop;
         end if;
      end if;

      --  c) Each active driver in the model is updated.  If a force or deposit
      --     was scheduled for any driver, the force or deposit is no longer
      --     scheduler for the driver [TODO]
      --  d) Each signal on each net in the model that includes active drivers
      --     is updated in an order that is consistent with the dependency
      --     relaction between signals (see 14.7.4).  (Events may occur on
      --     signals as a results.) If a force, deposit, or release was
      --     scheduled for any signal, the force, deposit, or release is no
      --     longer scheduled for the signal.
      if Options.Flag_Stats then
         Stats.Start_Update;
      end if;
      Update_Signals;
      Call_Callbacks (Hooks.Cb_Signals_Updated);
      if Options.Flag_Stats then
         Stats.Start_Resume;
      end if;

      --  e) Any action required to give effect to a PSL directive is performed
      --     [TODO]
      null;

      --  f) The following actions occur in the indicated order:
      --     2) For each process P, if P is currently sensitive to a signal S
      --        and if an event has occurred on S in this simulation cycle,
      --        then P resumes.
      if Current_Time = Process_First_Timeout then
         --  There are processes to awake.
         Tn := Last_Time;
         declare
            Proc : Process_Acc;
            Next_Proc : Process_Acc;
         begin
            Proc := Process_Timeout_Chain;
            while Proc /= null loop
               Next_Proc := Proc.Timeout_Chain_Next;
               case Proc.State is
                  when State_Sensitized =>
                     null;
                  when State_Delayed =>
                     if Proc.Timeout = Current_Time then
                        Proc.Timeout := Bad_Time;
                        Remove_Process_From_Timeout_Chain (Proc);
                        Resume_Process (Proc);
                        Proc.State := State_Ready;
                     elsif Proc.Timeout > 0 and then Proc.Timeout < Tn then
                        Tn := Proc.Timeout;
                     end if;
                  when State_Wait =>
                     if Proc.Timeout = Current_Time then
                        Proc.Timeout := Bad_Time;
                        Resume_Process (Proc);
                        Proc.State := State_Timeout;
                     elsif Proc.Timeout > 0 and then Proc.Timeout < Tn then
                        Tn := Proc.Timeout;
                     end if;
                  when State_Timeout
                    | State_Ready =>
                     Internal_Error ("process in timeout");
                  when State_Dead =>
                     null;
               end case;
               Proc := Next_Proc;
            end loop;
         end;
         Process_First_Timeout := Tn;
      end if;

      --     3) For each nonpostponed that has resumed in the current
      --        simulation cycle, the following actions occur in the indicated
      --        order:
      --        - Each registered and enabled vhpiCbResume callback associated
      --          with P is executed [TODO]
      --        - The processes executes until it suspends.
      --        - Each registered and enabled vhpiCbSyspend callback associated
      --          with P is executed [TODO]
      Status := Run_Processes (Postponed => False);

      --  g) The time of the next simulation cycle, Tn, is calculated according
      --    to the rules of 14.7.5.1
      if Options.Flag_Stats then
         Stats.Start_Next_Time;
      end if;
      Tn := Compute_Next_Time;

      --  h) If the next simulation cycle will be a delta cycle, the remainder
      --     of the step is skipped. Otherwise the following actions occur
      --     in the indicated order:
      --     1) Each registered and enabled vhpiLastKnownDeltaCycle and
      --        vhpiCbRepLastKnownDeltaCycle callback is executed. Tn is
      --        recalculated according to the rules of 14.7.5.1
      --     [...]
      --     4) For each postponed process P, if P has resumed but has not been
      --        executed since its last resumption, the following actions occur
      --        in the indicated order:
      --        - Each registered and enabled vhpiCbResume callback associated
      --          with P is executed [TODO]
      --        - The process executes until it suspends.
      --        - Each registered and enabled vhpiCbSuspend callback associated
      --          with P is executed [TODO]
      --     5) Tn is recalculated according to the rules of 14.7.5.1
      --     6) [TODO]
      --     7) If Tn = TIME'HIGH and there are no active drivers, process
      --        resumptions, or registered and enabled vhpiCbAfterDelay,
      --        vhpiCbRepAfterDelay, vhpiCbTimeOut, or VhpiCbRepTimeOut
      --        callbacks to occur at Tn, then each registered and enabled
      --        vhpiCbQuiescence is executed. [TODO]
      --        Tn is recalculated according to the rules of 14.7.5.1
      --     It is an error if the execution of any postponed process or any
      --     callback executed in substeps 3) through 7) of step h) causes a
      --     delta cycle to occur immediatly after the current simulation
      --     cycle.
      if Tn /= Current_Time then
         if Has_Callbacks (Hooks.Cb_Last_Known_Delta) then
            Call_Callbacks (Hooks.Cb_Last_Known_Delta);
            Flush_Active_Chain;
            Tn := Compute_Next_Time;
         end if;
      end if;
      if Tn /= Current_Time then
         if Last_Postponed_Resume_Process /= 0 then
            Flush_Active_Chain;
            Status := Run_Processes (Postponed => True);
            if Options.Flag_Stats then
               Stats.Start_Next_Time;
            end if;
            Tn := Compute_Next_Time;
            if Tn = Current_Time then
               Error ("postponed process causes a delta cycle");
            end if;
         end if;

         if Has_Callbacks (Hooks.Cb_End_Of_Time_Step) then
            Call_Callbacks (Hooks.Cb_End_Of_Time_Step);
            Tn := Compute_Next_Time;
         end if;

         Next_Time := Tn;
         Current_Delta := 0;

         --  Statistics.
         Nbr_Cycles := Nbr_Cycles + 1;

         --  For wave dumpers.
         Grt.Hooks.Call_Cycle_Hooks;

         return Run_Resumed;
      end if;

      if Current_Time = Last_Time and then Status = Run_None then
         --  End of time and no process to run.
         return Run_Finished;
      else
         Current_Delta := Current_Delta + 1;

         --  Statistics.
         Nbr_Delta_Cycles := Nbr_Delta_Cycles + 1;

         return Run_Resumed;
      end if;
   end Simulation_Cycle;

   function Simulation_Init return Integer
   is
      use Options;
   begin
      if Flag_Stats then
         Stats.Start_Order;
      end if;

      Grt.Hooks.Call_Start_Hooks;

      Grt.Signals.Order_All_Signals;

      if Grt.Options.Disp_Signals_Map then
         Grt.Disp_Signals.Disp_Signals_Map;
      end if;
      if Grt.Options.Disp_Signals_Table then
         Grt.Disp_Signals.Disp_Signals_Table;
      end if;
      if Disp_Signals_Order then
         Grt.Disp.Disp_Signals_Order;
      end if;
      if Disp_Sensitivity then
         Grt.Disp_Signals.Disp_All_Sensitivity;
      end if;

      if Nbr_Threads /= 1 then
         Threads.Init;
      end if;

--       if Disp_Sig_Types then
--          Grt.Disp.Disp_Signals_Type;
--       end if;

      Initialization_Phase;

      Nbr_Delta_Cycles := 0;
      Nbr_Cycles := 0;
      if Trace_Signals then
         Grt.Disp_Signals.Disp_All_Signals;
      end if;

      if Next_Time /= 0 then
         --  This is the end of a cycle.  This can happen when the time is not
         --  zero after initialization.
         Grt.Hooks.Call_Cycle_Hooks;
      end if;

      return 0;
   end Simulation_Init;

   function Has_Simulation_Timeout return Boolean
   is
      use Options;
   begin
      if Next_Time > Stop_Time
        and then Next_Time /= Std_Time'Last
      then
         --  FIXME: Implement with a callback instead ?  This could be done
         --  in 2 steps: an after_delay for the time and then a read_only
         --  to finish the current cycle.  Note that no message should be
         --  printed if the simulation is already finished at the stop time.
         Info_S ("simulation stopped by --stop-time @");
         Diag_C_Now;
         Info_E;
         return True;
      elsif Current_Delta >= Stop_Delta then
         Info_S ("simulation stopped @");
         Diag_C_Now;
         Diag_C (" by --stop-delta=");
         Diag_C (Stop_Delta);
         Info_E;
         return True;
      else
         return False;
      end if;
   end Has_Simulation_Timeout;

   function Simulation_Step return Integer
   is
      use Options;
      Status : Integer;
   begin
      Status := Simulation_Cycle;

      --  Simulation has been stopped/finished by vpi.
      if Status = Run_Stop then
         return 2;
      end if;

      if Trace_Signals then
         Grt.Disp_Signals.Disp_All_Signals;
      end if;

      --  Simulation is finished.
      if Status = Run_Finished then
         return 3;
      end if;

      --  Simulation is stopped by user timeout.
      if Has_Simulation_Timeout then
         return 4;
      end if;

      if Current_Delta = 0 then
         Grt.Hooks.Call_Cycle_Hooks;
         return 1;
      else
         if Current_Delta >= Stop_Delta then
            return 5;
         else
            return 0;
         end if;
      end if;
   end Simulation_Step;

   function Simulation_Main_Loop return Integer
   is
      use Options;
      Status : Integer;
   begin
      loop
         Status := Simulation_Cycle;

         --  Simulation has been stopped/finished by vpi.
         exit when Status = Run_Stop;

         if Trace_Signals then
            Grt.Disp_Signals.Disp_All_Signals;
         end if;

         --  Simulation is finished.
         exit when Status = Run_Finished;

         --  Simulation is stopped by user timeout.
         if Has_Simulation_Timeout then
            Status := Run_Limit;
            exit;
         end if;
      end loop;

      return Status;
   end Simulation_Main_Loop;

   procedure Simulation_Finish
   is
      use Options;
   begin
      if Nbr_Threads /= 1 then
         Threads.Finish;
      end if;

      Call_Finalizers;
   end Simulation_Finish;

   function Simulation return Integer
   is
      Status : Integer;
   begin
      Status := Simulation_Init;
      pragma Assert (Status = 0);

      Status := Simulation_Main_Loop;

      --  Note: the caller must call Simulation_Finish.

      return Status;
   end Simulation;
end Grt.Processes;
