--  GHDL Run Time (GRT) -  Hooks.
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
with Grt.Callbacks;

package Grt.Hooks is
   pragma Preelaborate (Grt.Hooks);

   type Option_Hook_Type is access function (Opt : String) return Boolean;
   type Proc_Hook_Type is access procedure;

   type Cst_String_Acc is access constant String;

   type Hooks_Type is record
      --  A one-line description of the hook.  The format is:
      --  "NAME: description".  NAME should be uniq and is tested by the
      --  switch --has-feature=NAME.
      --  DESC can be null if there is no interesting feature added.
      Desc : Cst_String_Acc;

      --  Called for every unknown command line argument.
      --  Return TRUE if handled.
      Option : Option_Hook_Type;

      --  Display command line help.
      Help : Proc_Hook_Type;

      --  Called at initialization (after decoding options).
      Init : Proc_Hook_Type;

      --  Called just after elaboration.
      Start : Proc_Hook_Type;

      --  Called at the end of execution.
      Finish : Proc_Hook_Type;
   end record;

   type Hooks_Acc is access constant Hooks_Type;

   --  Registers hook.
   procedure Register_Hooks (Hooks : Hooks_Acc);

   --  Register an hook which will call PROC after every non-delta cycles.
   procedure Register_Cycle_Hook (Proc : Proc_Hook_Type);

   --  Display the description of the hooks.
   procedure Display_Hooks_Desc;

   --  Return True if NAME is present in the list of modules.
   function Has_Feature (Name : String) return Boolean;

   --  Call hooks.
   function Call_Option_Hooks (Opt : String) return Boolean;
   procedure Call_Help_Hooks;
   procedure Call_Init_Hooks;
   procedure Call_Start_Hooks;
   procedure Call_Finish_Hooks;

   --  Call non-delta cycles hooks.
   procedure Call_Cycle_Hooks;
   pragma Inline_Always (Call_Cycle_Hooks);

   --  Nil procedure.
   procedure Proc_Hook_Nil;

   --  Callbacks.

   --  Called at the beginning of a non-delta simulation cycle.
   Cb_Next_Time_Step : Callbacks.Callback_List;

   --  Called at the beginning of the cycle at time T.  Can create a new
   --  simulation cycle, and called after Cb_Next_Time_Step.
   Cb_After_Delay : Callbacks.Callback_Time_List;

   --  Called before running processes.
   Cb_Start_Of_Processes : Callbacks.Callback_List;

   --  Called after updating the signals.  For value change detection.
   Cb_Signals_Updated : Callbacks.Callback_List;

   --  Called at the last known delta cycle of a timestep, before execution
   --  of postponed processes.
   --  The callback may change signals and therefore generating new delta
   --  cycle.
   Cb_Last_Known_Delta : Callbacks.Callback_List;

   --  Called after postponed processes, may change the next time.
   Cb_End_Of_Time_Step : Callbacks.Callback_List;

end Grt.Hooks;
