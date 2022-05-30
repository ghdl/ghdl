--  Operations synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

package Synth.Vhdl_Oper is
   function Synth_Predefined_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp;
   function Synth_Operator_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp;

   function Synth_Dyadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                    Imp : Node;
                                    Left_Expr : Node;
                                    Right_Expr : Node;
                                    Expr : Node) return Valtyp;

   function Synth_Monadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                     Imp : Node;
                                     Operand_Expr : Node;
                                     Loc : Node) return Valtyp;

   function Create_Bounds_From_Length
     (Syn_Inst : Synth_Instance_Acc; Atype : Iir; Len : Iir_Index32)
     return Bound_Type;


   type Eval_Predefined_Acc is access
     function (Param : Valtyp; Res_Typ : Type_Acc) return Memtyp;

   Hook_Bit_Rising_Edge : Eval_Predefined_Acc;
   Hook_Bit_Falling_Edge : Eval_Predefined_Acc;

   Hook_Std_Rising_Edge : Eval_Predefined_Acc;
   Hook_Std_Falling_Edge : Eval_Predefined_Acc;
end Synth.Vhdl_Oper;
