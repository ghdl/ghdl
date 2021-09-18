--  Iir to ortho translator.
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

package Trans.Chap8 is
   --  If TRUE, generate extra-code to catch at run-time incoherent state
   --  issues.
   State_Debug : constant Boolean := True;

   --  The initial state.  Used in process to loop.
   State_Init : constant State_Type := 0;

   --  The state for 'return' in a subprogram.
   State_Return : constant State_Type := 1;

   --  Called at the entry of the generated procedure to setup the state
   --  machinery: set the local state variable, create the state machine
   --  (loop, case, first choice).  The current position in the graph is
   --  vertex 0 (initial state): there is an implicit State_Allocate and a
   --  State_Start.  This is not reentrant (does not nest).
   procedure State_Entry (Info : Ortho_Info_Acc);

   --  Last action of the generated procedure: close the case and the loop.
   --  Destroy the state machinery.
   procedure State_Leave (Parent : Iir);

   --  True if the current process or subprogram is state based.
   function State_Enabled return Boolean;

   --  Create a new state.
   function State_Allocate return State_Type;

   --  Start statements for STATE.
   procedure State_Start (State : State_Type);

   --  Jump to state NEXT_STATE.  Note: this doesn't modify the control flow,
   --  so there must be no statements after State_Jump until the next
   --  State_Start.
   procedure State_Jump (Next_State : State_Type);

   --  Suspend the current process or subprogram.  It will resume to
   --  NEXT_STATE.
   procedure State_Suspend (Next_State : State_Type);

   procedure Translate_Statements_Chain (First : Iir);

   --  Return true if there is a return statement in the chain.
   function Translate_Statements_Chain_Has_Return (First : Iir) return Boolean;

   --  Create a case branch for CHOICE.
   --  Used by case statement and aggregates.
   procedure Translate_Case_Choice
     (Choice : Iir; Choice_Type : Iir; Blk : in out O_Case_Block);

   --  Procedures with a paramater to be called by Translate_Case.
   type Case_Handler is tagged null record;
   --  Called to translate the associated node of a choice.
   procedure Case_Association_Cb (Assoc : Iir;
                                  Handler : in out Case_Handler) is null;

   --  Translate a case statement or a selected signal assignment.
   procedure Translate_Case (N : Iir; Handler : in out Case_Handler'Class);

   --  Create declarations for a for-loop statement.
   procedure Translate_For_Loop_Statement_Declaration (Stmt : Iir);

   procedure Translate_Report (Stmt : Iir; Subprg : O_Dnode; Level : Iir);

   --  Create the state record for the CALL procedure call.
   procedure Translate_Procedure_Call_State (Call : Iir);

   function Translate_Subprogram_Call
     (Call : Iir; Assoc_Chain : Iir; Obj : Iir) return O_Enode;

   --  Signal assignment for an inertial association.
   procedure Translate_Inertial_Assignment
     (Targ : Mnode; Targ_Type : Iir; Val : Mnode; Assoc : Iir);
end Trans.Chap8;
