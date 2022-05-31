--  Statements synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

with Netlists; use Netlists;

with Synth.Vhdl_Environment; use Synth.Vhdl_Environment.Env;

package Synth.Vhdl_Stmts is
   --  Create a new Synth_Instance for calling subprogram IMP/BOD.
   function Synth_Subprogram_Call_Instance (Inst : Synth_Instance_Acc;
                                            Imp : Node;
                                            Bod : Node)
                                           return Synth_Instance_Acc;

   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node);

   --  Dynamic index for Synth_Assignment_Prefix.
   --  As dynamic is about dynamic (!) index, the index is a net.
   type Dyn_Name is record
      --  Start and type of the indexed part, which can be a part of the
      --  base name.
      Pfx_Off : Value_Offsets;
      Pfx_Typ : Type_Acc;

      --  Variable offset.
      Voff : Net;
   end record;

   No_Dyn_Name : constant Dyn_Name := (Pfx_Off => No_Value_Offsets,
                                       Pfx_Typ => null,
                                       Voff => No_Net);

   --  Transform PFX into DEST_*.
   --  DEST_BASE is the base object (with its own typ).  Can be the result,
   --   a net or an object larger than the result.
   --  DEST_TYP is the type of the result.
   --  DEST_OFF is the offset, within DEST_DYN.
   --  DEST_DYN is set (Voff field set) when there is a non-static index.
   procedure Synth_Assignment_Prefix (Syn_Inst : Synth_Instance_Acc;
                                      Pfx : Node;
                                      Dest_Base : out Valtyp;
                                      Dest_Typ : out Type_Acc;
                                      Dest_Off : out Value_Offsets;
                                      Dest_Dyn : out Dyn_Name);

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Node;
                               Val : Valtyp;
                               Loc : Node);

   function Synth_Read_Memory (Syn_Inst : Synth_Instance_Acc;
                               Obj : Valtyp;
                               Res_Typ : Type_Acc;
                               Off : Uns32;
                               Dyn : Dyn_Name;
                               Loc : Node) return Valtyp;

   function Synth_User_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp;

   --  Operation implemented by a user function.
   function Synth_User_Operator (Syn_Inst : Synth_Instance_Acc;
                                 Left_Expr : Node;
                                 Right_Expr : Node;
                                 Expr : Node) return Valtyp;

   --  Generate netlists for concurrent statements STMTS.
   procedure Synth_Concurrent_Statements
     (Syn_Inst : Synth_Instance_Acc; Stmts : Node);

   --  Apply attributes of UNIT.
   procedure Synth_Attribute_Values
     (Syn_Inst : Synth_Instance_Acc; Unit : Node);

   procedure Synth_Verification_Unit (Syn_Inst : Synth_Instance_Acc;
                                      Unit : Node;
                                      Parent_Inst : Synth_Instance_Acc);

   procedure Execute_Assertion_Statement (Inst : Synth_Instance_Acc;
                                          Stmt : Node);
   procedure Execute_Report_Statement (Inst : Synth_Instance_Acc;
                                       Stmt : Node);
   procedure Init_For_Loop_Statement (Inst : Synth_Instance_Acc;
                                      Stmt : Node;
                                      Val : out Valtyp);
   procedure Finish_For_Loop_Statement (Inst : Synth_Instance_Acc;
                                        Stmt : Node);
   procedure Synth_Variable_Assignment (Inst : Synth_Instance_Acc;
                                        Stmt : Node);
   procedure Synth_Conditional_Variable_Assignment
     (Inst : Synth_Instance_Acc; Stmt : Node);

   procedure Synth_Procedure_Call (Syn_Inst : Synth_Instance_Acc; Stmt : Node);
   procedure Synth_Subprogram_Back_Association
     (Subprg_Inst : Synth_Instance_Acc;
      Caller_Inst : Synth_Instance_Acc;
      Inter_Chain : Node;
      Assoc_Chain : Node);

   --  Return the statements chain to be executed.
   function Execute_Static_Case_Statement
     (Inst : Synth_Instance_Acc; Stmt : Node; Sel : Valtyp) return Node;

   type Target_Kind is
     (
      --  The target is an object or a static part of it.
      Target_Simple,

      --  The target is an aggregate.
      Target_Aggregate,

      --  The assignment is dynamically indexed.
      Target_Memory
     );

   type Target_Info (Kind : Target_Kind := Target_Simple) is record
      --  In all cases, the type of the target is known or computed.
      Targ_Type : Type_Acc;

      case Kind is
         when Target_Simple =>
            --  For a simple target, the destination is known.
            Obj : Valtyp;
            Off : Value_Offsets;
         when Target_Aggregate =>
            --  For an aggregate: the type is computed and the details will
            --  be handled at the assignment.
            Aggr : Node;
         when Target_Memory =>
            --  For a memory: the destination is known.
            Mem_Obj : Valtyp;
            --  The dynamic offset.
            Mem_Dyn : Dyn_Name;
            --  Offset of the data to be accessed from the memory.
            Mem_Doff : Uns32;
      end case;
   end record;

   function Synth_Target (Syn_Inst : Synth_Instance_Acc;
                          Target : Node) return Target_Info;

   --  Split aggregate assignment into smaller parts.
   generic
      with procedure Assign (Inst : Synth_Instance_Acc;
                             Targ_Info : Target_Info;
                             Val : Valtyp;
                             Loc : Node);
   procedure Assign_Aggregate (Inst : Synth_Instance_Acc;
                               Target : Node;
                               Target_Typ : Type_Acc;
                               Val : Valtyp;
                               Loc : Node);


private
   --  There are 2 execution mode:
   --  * static: it is like simulation, all the inputs are known, neither
   --    gates nor signals are generated.  This mode is used during
   --    elaboration and when all inputs of a subprogram are known.
   --  * dynamic: inputs can be wires so gates are generated.  But many types
   --    (like file or access) cannot be handled.
   type Mode_Type is (Mode_Static, Mode_Dynamic);

   type Loop_Context (Mode : Mode_Type);
   type Loop_Context_Acc is access all Loop_Context;

   type Loop_Context (Mode : Mode_Type) is record
      Prev_Loop : Loop_Context_Acc;
      Loop_Stmt : Node;

      case Mode is
         when Mode_Dynamic =>
            --  Set when this loop has next/exit statements for itself.
            --  Set to true so that inner loops have to declare W_Quit.
            Need_Quit : Boolean;

            --  Value of W_En at the entry of the loop.
            Saved_En : Net;

            --  Set to 0 in case of exit for the loop.
            --  Set to 0 in case of exit/next for outer loop.
            --  Initialized to 1.
            W_Exit : Wire_Id;

            --  Set to 0 if this loop has to be quited because of an
            --  exit/next for an outer loop.  Initialized to 1.
            W_Quit : Wire_Id;

            --  Mark to release wires.
            Wire_Mark : Wire_Id;
         when Mode_Static =>
            S_Exit : Boolean;
            S_Quit : Boolean;
      end case;
   end record;

   --  Context for sequential statements.
   type Seq_Context (Mode : Mode_Type) is record
      Inst : Synth_Instance_Acc;

      Cur_Loop : Loop_Context_Acc;

      Ret_Value : Valtyp;
      Ret_Typ : Type_Acc;
      Nbr_Ret : Int32;

      case Mode is
         when Mode_Dynamic =>
            --  Enable execution.  For loop controls.
            W_En : Wire_Id;

            W_Ret : Wire_Id;

            --  Return value.
            W_Val : Wire_Id;

            Ret_Init : Net;

         when Mode_Static =>
            S_En : Boolean;
      end case;
   end record;
end Synth.Vhdl_Stmts;
