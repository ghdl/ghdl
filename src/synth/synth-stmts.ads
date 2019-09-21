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
                                      Dest_Wid : out Wire_Id;
                                      Dest_Off : out Uns32;
                                      Dest_Type : out Type_Acc);

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Node;
                               Val : Value_Acc;
                               Loc : Node);

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
   --  Context for sequential statements.
   type Seq_Context is record
      Inst : Synth_Instance_Acc;

      --  Enable execution.
      W_En : Wire_Id;

      --  Return value.
      W_Val : Wire_Id;

      Ret_Init : Net;

      Ret_Value : Value_Acc;
      Ret_Typ : Type_Acc;
      Nbr_Ret : Int32;
   end record;
end Synth.Stmts;
