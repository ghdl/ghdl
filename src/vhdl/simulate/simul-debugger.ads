--  Debugger for interpreter
--  Copyright (C) 2014 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;
with Simul.Environments; use Simul.Environments;
with Grt.Types;

package Simul.Debugger is
   Flag_Debugger : Boolean := False;
   Flag_Interractive : Boolean := False;

   Flag_Need_Debug : Boolean := False;

   -- Disp a message for a constraint error.
   -- And raise the exception execution_constraint_error.
   procedure Error_Msg_Constraint (Expr: Iir);
   pragma No_Return (Error_Msg_Constraint);

   -- Disp a message during execution.
   procedure Error_Msg_Exec (Msg: String; Loc: Iir);
   pragma No_Return (Error_Msg_Exec);

   procedure Warning_Msg_Exec (Msg: String; Loc: Iir);

   --  Disp a block instance, in a human readable way.
   --  Used to debug.
   procedure Disp_Block_Instance (Instance: Block_Instance_Acc);

   -- Disp the instance tree.
   procedure Disp_Instances_Tree;

   --  Disp the name of an instance, without newline.  The name of
   --  architectures is displayed unless Short is True.
   procedure Disp_Instance_Name (Instance: Block_Instance_Acc;
                                 Short : Boolean := False);

   -- Disp the resulting processes of elaboration.
   -- procedure Disp_Processes;

   --  Disp the label of PROCESS, or <unlabeled> if PROCESS has no label.
   procedure Disp_Label (Process : Iir);

   --  Disp all signals name and values.
   procedure Disp_Signals_Value;

   --  Disp stats about the design (number of process, number of signals...)
   procedure Disp_Design_Stats;

   --  The reason why the debugger is invoked.
   type Debug_Reason is
     (--  Called from an external debugger while debugging ghdl.
      Reason_Internal_Debug,

      --  Interractive session, elaboration not done
      Reason_Start,

      --  At end of elaboration, for an interractive session
      Reason_Elab,

      --  Simulation time limit reached.
      Reason_Time,

      --  Before execution of a statement.
      Reason_Break,

      --  Assertion failure
      Reason_Assert,

      --  Non recoverable error occurred (such as index error, overflow...)
      Reason_Error
     );

   Debugger_Quit : exception;

   --  Time at which simulation must stop and return to user interraction.
   Break_Time : Grt.Types.Std_Time;

   --  Interractive debugger.
   procedure Debug (Reason: Debug_Reason);

   --  Call the debugger in case of error.
   procedure Debug_Error;
end Simul.Debugger;
