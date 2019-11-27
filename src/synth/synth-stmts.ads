--  Statements synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;

with Netlists; use Netlists;

with Synth.Values; use Synth.Values;
with Synth.Context; use Synth.Context;
with Synth.Environment; use Synth.Environment;

package Synth.Stmts is
   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node);

   procedure Synth_Assignment_Prefix (Syn_Inst : Synth_Instance_Acc;
                                      Pfx : Node;
                                      Dest_Obj : out Value_Acc;
                                      Dest_Off : out Uns32;
                                      Dest_Voff : out Net;
                                      Dest_Rdwd : out Width;
                                      Dest_Type : out Type_Acc);

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Node;
                               Val : Value_Acc;
                               Loc : Node);

   function Synth_Read_Memory (Syn_Inst : Synth_Instance_Acc;
                               Obj : Value_Acc;
                               Off : Uns32;
                               Voff : Net;
                               Typ : Type_Acc;
                               Loc : Node) return Value_Acc;

   function Synth_User_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc;

   --  Generate netlists for concurrent statements STMTS.
   procedure Synth_Concurrent_Statements
     (Syn_Inst : Synth_Instance_Acc; Stmts : Node);

   procedure Synth_Verification_Unit
     (Syn_Inst : Synth_Instance_Acc; Unit : Node);

   --  For iterators.
   function In_Range (Rng : Discrete_Range_Type; V : Int64) return Boolean;
   procedure Update_Index (Rng : Discrete_Range_Type; Idx : in out Int64);

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

      Ret_Value : Value_Acc;
      Ret_Typ : Type_Acc;
      Nbr_Ret : Int32;

      case Mode is
         when Mode_Dynamic =>
            --  Enable execution.
            W_En : Wire_Id;

            W_Ret : Wire_Id;

            --  Return value.
            W_Val : Wire_Id;

            Ret_Init : Net;

         when Mode_Static =>
            S_En : Boolean;
      end case;
   end record;
end Synth.Stmts;
