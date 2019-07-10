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

with Ada.Unchecked_Deallocation;

with Types; use Types;
with Grt.Algos;
with Areapools;
with Vhdl.Errors; use Vhdl.Errors;

with Vhdl.Types;
with Vhdl.Sem_Expr;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Evaluation;

with PSL.Types;
with PSL.Nodes;
with PSL.NFAs;
with PSL.Errors;

with Synth.Types; use Synth.Types;
with Synth.Errors; use Synth.Errors;
with Synth.Decls; use Synth.Decls;
with Synth.Expr; use Synth.Expr;
with Synth.Environment; use Synth.Environment;
with Synth.Insts; use Synth.Insts;

with Vhdl.Annotations; use Vhdl.Annotations;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Gates;

package body Synth.Stmts is
   function Synth_Waveform (Syn_Inst : Synth_Instance_Acc;
                            Wf : Node;
                            Targ_Type : Node) return Value_Acc is
   begin
      if Get_Kind (Wf) = Iir_Kind_Unaffected_Waveform then
         --  TODO
         raise Internal_Error;
      end if;
      if Get_Chain (Wf) /= Null_Node then
         --  Warning.
         null;
      end if;
      if Get_Time (Wf) /= Null_Node then
         --  Warning
         null;
      end if;
      return Synth_Expression_With_Type
        (Syn_Inst, Get_We_Value (Wf), Targ_Type);
   end Synth_Waveform;

   procedure Synth_Assign (Dest : Value_Acc; Val : Value_Acc; Vtype : Node) is
   begin
      case Dest.Kind is
         when Value_Wire =>
            Phi_Assign (Dest.W, Get_Net (Val, Vtype));
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Assign;

   procedure Synth_Assignment_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                         Target : Node;
                                         Val : Value_Acc)
   is
      Targ_Type : constant Node := Get_Type (Target);
      Choice : Node;
      Assoc : Node;
      Pos : Uns32;
   begin
      if Is_Vector_Type (Targ_Type) then
         Choice := Get_Association_Choices_Chain (Target);
         Pos := Get_Width (Syn_Inst, Targ_Type);
         while Is_Valid (Choice) loop
            Assoc := Get_Associated_Expr (Choice);
            case Get_Kind (Choice) is
               when Iir_Kind_Choice_By_None =>
                  Pos := Pos - 1;
                  Synth_Assignment (Syn_Inst, Assoc, Bit_Extract (Val, Pos));
               when others =>
                  Error_Kind ("synth_assignment_aggregate", Choice);
            end case;
            Choice := Get_Chain (Choice);
         end loop;
      else
         raise Internal_Error;
      end if;
   end Synth_Assignment_Aggregate;

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Node;
                               Val : Value_Acc) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Simple_Name =>
            Synth_Assignment (Syn_Inst, Get_Named_Entity (Target), Val);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Anonymous_Signal_Declaration =>
            Synth_Assign (Get_Value (Syn_Inst, Target),
                          Val, Get_Type (Target));
         when Iir_Kind_Aggregate =>
            Synth_Assignment_Aggregate (Syn_Inst, Target, Val);
         when Iir_Kind_Indexed_Name =>
            declare
               Pfx : constant Node := Get_Prefix (Target);
               Targ : constant Value_Acc :=
                 Get_Value (Syn_Inst, Get_Base_Name (Pfx));
               Indexes : constant Node_Flist := Get_Index_List (Target);
               N_Idx : Node;
               Idx : Value_Acc;
               V : Net;
            begin
               if Get_Nbr_Elements (Indexes) /= 1
                 or else Targ.Kind /= Value_Wire
               then
                  --  Only support assignment of vector.
                  raise Internal_Error;
               end if;
               N_Idx := Get_Nth_Element (Indexes, 0);
               Idx := Synth_Expression_With_Type
                 (Syn_Inst, N_Idx, Get_Type (N_Idx));
               if Is_Const (Idx) then
                  --  FIXME: check index.
                  V := Build_Insert (Build_Context,
                                     Get_Net (Targ, Get_Type (Pfx)),
                                     Get_Net (Val, Get_Type (Target)),
                                     Index_To_Offset (Targ, Idx.Scal, Target));
               else
                  raise Internal_Error;
               end if;
               Synth_Assign (Targ, Create_Value_Net (V, null), Get_Type (Pfx));
            end;
         when Iir_Kind_Slice_Name =>
            declare
               Pfx : constant Node := Get_Prefix (Target);
               Targ : constant Value_Acc :=
                 Get_Value (Syn_Inst, Get_Base_Name (Pfx));
               Res_Bnd : Value_Bound_Acc;
               Inp : Net;
               Step : Uns32;
               Off : Int32;
               Wd : Uns32;
               I : Net;
               V : Net;
               Res : Net;
            begin
               if Targ.Kind /= Value_Wire then
                  --  Only support assignment of vector.
                  raise Internal_Error;
               end if;
               Synth_Slice_Suffix (Syn_Inst, Target, Extract_Bound (Targ),
                                   Res_Bnd, Inp, Step, Off, Wd);
               I := Get_Net (Targ, Get_Type (Pfx));
               V := Get_Net (Val, Get_Type (Target));
               if Inp /= No_Net then
                  Res := Build_Dyn_Insert
                    (Build_Context, I, V, Inp, Step, Off);
               else
                  Res := Build_Insert (Build_Context, I, V, Uns32 (Off));
               end if;
               Synth_Assign
                 (Targ, Create_Value_Net (Res, Res_Bnd), Get_Type (Pfx));
            end;
         when others =>
            Error_Kind ("synth_assignment", Target);
      end case;
   end Synth_Assignment;

   --  Concurrent or sequential simple signal assignment
   procedure Synth_Simple_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Target : constant Node := Get_Target (Stmt);
      Val : Value_Acc;
   begin
      Val := Synth_Waveform
        (Syn_Inst, Get_Waveform_Chain (Stmt), Get_Type (Target));
      Synth_Assignment (Syn_Inst, Target, Val);
   end Synth_Simple_Signal_Assignment;

   procedure Synth_Conditional_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Target : constant Node := Get_Target (Stmt);
      Targ_Type : constant Node := Get_Type (Target);
      Cond : Node;
      Cwf : Node;
      Val, Cond_Val : Value_Acc;
      First, Last : Value_Acc;
   begin
      Last := null;
      Cwf := Get_Conditional_Waveform_Chain (Stmt);
      while Cwf /= Null_Node loop
         Val := Synth_Waveform (Syn_Inst, Get_Waveform_Chain (Cwf), Targ_Type);
         Cond := Get_Condition (Cwf);
         if Cond /= Null_Node then
            Cond_Val := Synth_Expression (Syn_Inst, Cond);
            Val := Create_Value_Mux2 (Cond_Val, Val, null);
         end if;

         if Last /= null then
            Last.M_F := Val;
         else
            First := Val;
         end if;
         Last := Val;
         Cwf := Get_Chain (Cwf);
      end loop;
      Synth_Assignment (Syn_Inst, Target, First);
   end Synth_Conditional_Signal_Assignment;

   procedure Synth_Variable_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Target : constant Node := Get_Target (Stmt);
      Val : Value_Acc;
   begin
      Val := Synth_Expression_With_Type
        (Syn_Inst, Get_Expression (Stmt), Get_Type (Target));
      Synth_Assignment (Syn_Inst, Target, Val);
   end Synth_Variable_Assignment;

   procedure Synth_If_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Cond : constant Node := Get_Condition (Stmt);
      Els : constant Node := Get_Else_Clause (Stmt);
      Cond_Val : Value_Acc;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      Cond_Val := Synth_Expression (Syn_Inst, Cond);
      if Is_Const (Cond_Val) then
         if Cond_Val.Scal = 1 then
            --  True.
            Synth_Sequential_Statements
              (Syn_Inst, Get_Sequential_Statement_Chain (Stmt));
         else
            pragma Assert (Cond_Val.Scal = 0);
            if Is_Valid (Els) then
               --  Else part
               if Is_Null (Get_Condition (Els)) then
                  --  Final else part.
                  Synth_Sequential_Statements
                    (Syn_Inst, Get_Sequential_Statement_Chain (Els));
               else
                  --  Elsif.  Handled as a nested if.
                  Synth_If_Statement (Syn_Inst, Els);
               end if;
            end if;
         end if;
      else
         Push_Phi;
         Synth_Sequential_Statements
           (Syn_Inst, Get_Sequential_Statement_Chain (Stmt));
         Pop_Phi (Phi_True);

         Push_Phi;
         if Is_Valid (Els) then
            if Is_Null (Get_Condition (Els)) then
               --  Final else part.
               Synth_Sequential_Statements
                 (Syn_Inst, Get_Sequential_Statement_Chain (Els));
            else
               --  Elsif.  Handled as a nested if.
               Synth_If_Statement (Syn_Inst, Els);
            end if;
         end if;
         Pop_Phi (Phi_False);

         Merge_Phis (Build_Context, Get_Net (Cond_Val, Get_Type (Cond)),
                     Phi_True, Phi_False);
      end if;
   end Synth_If_Statement;

   procedure Convert_Bv_To_Uns64 (Expr : Node; Val : out Uns64; Dc : out Uns64)
   is
      El_Type : constant Node :=
        Get_Base_Type (Get_Element_Subtype (Get_Type (Expr)));
   begin
      if El_Type = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type then
         declare
            use Vhdl.Evaluation.String_Utils;

            Info : constant Str_Info := Get_Str_Info (Expr);
         begin
            if Info.Len > 64 then
               raise Internal_Error;
            end if;
            Val := 0;
            Dc := 0;
            for I in 0 .. Info.Len - 1 loop
               Val := Shift_Left (Val, 1);
               Dc := Shift_Left (Dc, 1);
               case Get_Pos (Info, I) is
                  when Vhdl.Ieee.Std_Logic_1164.Std_Logic_0_Pos =>
                     Val := Val or 0;
                  when Vhdl.Ieee.Std_Logic_1164.Std_Logic_1_Pos =>
                     Val := Val or 1;
                  when Vhdl.Ieee.Std_Logic_1164.Std_Logic_U_Pos
                    |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_X_Pos
                    |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_Z_Pos
                    |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_W_Pos
                    |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_D_Pos
                    |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_L_Pos
                    |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_H_Pos =>
                     Dc := Dc or 1;
                  when others =>
                     raise Internal_Error;
               end case;
            end loop;
         end;
      else
         raise Internal_Error;
      end if;
   end Convert_Bv_To_Uns64;

   --  EXPR is a choice, so a locally static literal.
   procedure Convert_To_Uns64 (Expr : Node; Val : out Uns64; Dc : out Uns64)
   is
      Expr_Type : constant Node := Get_Type (Expr);
   begin
      case Get_Kind (Expr_Type) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            Convert_Bv_To_Uns64 (Expr, Val, Dc);
         when Iir_Kind_Enumeration_Type_Definition =>
            Dc := 0;
            Val := Uns64 (Get_Enum_Pos (Strip_Denoting_Name (Expr)));
         when others =>
            Error_Kind ("convert_to_uns64", Expr_Type);
      end case;
   end Convert_To_Uns64;

   type Alternative_Index is new Int32;

   type Choice_Data_Type is record
      --  Value of the choice
      Val : Uns64;

      --  Corresponding alternative
      Alt : Alternative_Index;
   end record;

   type Choice_Data_Array is array (Natural range <>) of Choice_Data_Type;
   type Choice_Data_Array_Acc is access Choice_Data_Array;
   procedure Free_Choice_Data_Array is new Ada.Unchecked_Deallocation
     (Choice_Data_Array, Choice_Data_Array_Acc);

   type Alternative_Data_Type is record
      Asgns : Assign;
      Val : Net;
   end record;
   type Alternative_Data_Array is
     array (Alternative_Index range <>) of Alternative_Data_Type;
   type Alternative_Data_Acc is access Alternative_Data_Array;
   procedure Free_Alternative_Data_Array is new Ada.Unchecked_Deallocation
     (Alternative_Data_Array, Alternative_Data_Acc);

   type Wire_Id_Array is array (Natural range <>) of Wire_Id;
   type Wire_Id_Array_Acc is access Wire_Id_Array;
   procedure Free_Wire_Id_Array is new Ada.Unchecked_Deallocation
     (Wire_Id_Array, Wire_Id_Array_Acc);

   procedure Sort_Wire_Id_Array (Arr : in out Wire_Id_Array)
   is
      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Arr (Op1) < Arr (Op2);
      end Lt;

      procedure Swap (From : Natural; To : Natural)
      is
         T : Wire_Id;
      begin
         T := Arr (From);
         Arr (From) := Arr (To);
         Arr (To) := T;
      end Swap;

      procedure Wid_Heap_Sort is
         new Grt.Algos.Heap_Sort (Lt => Lt, Swap => Swap);
   begin
      Wid_Heap_Sort (Arr'Length);
   end Sort_Wire_Id_Array;

   function Count_Wires_In_Alternatives (Alts : Alternative_Data_Array)
                                        return Natural
   is
      Res : Natural;
      Asgn : Assign;
      W : Wire_Id;
   begin
      Res := 0;
      for I in Alts'Range loop
         Asgn := Alts (I).Asgns;
         while Asgn /= No_Assign loop
            W := Get_Wire_Id (Asgn);
            if not Wire_Id_Table.Table (W).Mark_Flag then
               Res := Res + 1;
               Wire_Id_Table.Table (W).Mark_Flag := True;
            end if;
            Asgn := Get_Assign_Chain (Asgn);
         end loop;
      end loop;
      return Res;
   end Count_Wires_In_Alternatives;

   procedure Fill_Wire_Id_Array (Arr : out Wire_Id_Array;
                                 Alts : Alternative_Data_Array)
   is
      Idx : Natural;
      Asgn : Assign;
      W : Wire_Id;
   begin
      Idx := Arr'First;
      for I in Alts'Range loop
         Asgn := Alts (I).Asgns;
         while Asgn /= No_Assign loop
            W := Get_Wire_Id (Asgn);
            if Wire_Id_Table.Table (W).Mark_Flag then
               Arr (Idx) := W;
               Idx := Idx + 1;
               Wire_Id_Table.Table (W).Mark_Flag := False;
            end if;
            Asgn := Get_Assign_Chain (Asgn);
         end loop;
      end loop;
      pragma Assert (Idx = Arr'Last + 1);
   end Fill_Wire_Id_Array;

   type Case_Element is record
      Sel : Uns64;
      Val : Net;
   end record;

   type Case_Element_Array is array (Natural range <>) of Case_Element;
   type Case_Element_Array_Acc is access Case_Element_Array;
   procedure Free_Case_Element_Array is new Ada.Unchecked_Deallocation
     (Case_Element_Array, Case_Element_Array_Acc);

   --  Generate a netlist for a 'big' mux selected by SEL.  The inputs are
   --  described by ELS: E.Val must be selected when SEL = E.Sel; if there
   --  is no E in Els for a value, DEFAULT is selected.
   --  The result of the netlist is stored in RES.
   --
   --  A tree of MUX4 is built.
   --
   --  ELS must be sorted by SEL values.
   --  ELS is overwritten/modified so after the call it contains garbage.  The
   --  reason is that ELS might be large, so temporary arrays are not allocated
   --  on the stack, and ELS is expected to be built only for this subprogram.
   procedure Synth_Case (Sel : Net;
                         Els : in out Case_Element_Array;
                         Default : Net;
                         Res : out Net)
   is
      Wd : constant Width := Get_Width (Sel);
      Mask : Uns64;
      Sub_Sel : Net;
      Lels : Natural;
      Iels : Natural;
      Oels : Natural;
   begin
      Lels := Els'Last;
      Iels := Els'First;

      if Lels < Iels then
         --  No choices
         Res := Default;
         return;
      end if;

      --  Handle SEL bits by 2, so group case_element by 4.
      for I in 1 .. Natural (Wd / 2) loop
         Sub_Sel := Build_Extract (Build_Context,
                                   Sel, Width (2 * (I - 1)), 2);
         Mask := Shift_Left (not 0, Natural (2 * I));
         Iels := Els'First;
         Oels := Els'First;
         while Iels <= Lels loop
            declare
               type Net4 is array (0 .. 3) of Net;
               G : Net4;
               S_Group : constant Uns64 := Els (Iels).Sel and Mask;
               S_El : Uns64;
               El_Idx : Natural;
               Rsel : Net;
            begin
               G := (others => Default);
               for K in 0 .. 3 loop
                  exit when Iels > Lels;
                  S_El := Els (Iels).Sel;
                  exit when (S_El and Mask) /= S_Group;
                  El_Idx := Natural
                    (Shift_Right (S_El, Natural (2 * (I - 1))) and 3);
                  G (El_Idx) := Els (Iels).Val;
                  Iels := Iels + 1;
               end loop;
               if G (3) /= No_Net then
                  Rsel := Build_Mux4 (Build_Context,
                                      Sub_Sel, G (0), G (1), G (2), G (3));
               elsif G (2) /= No_Net then
                  Rsel := Build_Mux2
                    (Build_Context,
                     Build_Extract_Bit (Build_Context,
                                        Sel, Width (2 * (I - 1)) + 1),
                     Build_Mux2 (Build_Context,
                                 Build_Extract_Bit (Build_Context,
                                                    Sel, Width (2 * (I - 1))),
                                 G (0), G (1)),
                     G (2));
               elsif G (1) /= No_Net then
                  Rsel := Build_Mux2
                    (Build_Context,
                     Build_Extract_Bit (Build_Context,
                                        Sel, Width (2 * (I - 1))),
                     G (0), G (1));
               else
                  Rsel := G (0);
               end if;
               Els (Oels) := (Sel => S_Group, Val => Rsel);
               Oels := Oels + 1;
            end;
         end loop;
         Lels := Oels - 1;
      end loop;

      --  If the width is not a multiple of 2, handle the last level.
      if Wd mod 2 = 1 then
         if Wd = 1 then
            Sub_Sel := Sel;
         else
            Sub_Sel := Build_Extract_Bit (Build_Context, Sel, Wd - 1);
         end if;
         Iels := Els'First;
         Oels := Els'First;
         while Iels <= Lels loop
            declare
               type Net2 is array (0 .. 1) of Net;
               G : Net2;
               S_Group : constant Uns64 := Els (Iels).Sel and Mask;
               S_El : Uns64;
               El_Idx : Natural;
            begin
               G := (others => Default);
               for K in 0 .. 1 loop
                  exit when Iels > Lels;
                  S_El := Els (Iels).Sel;
                  El_Idx := Natural
                    (Shift_Right (S_El, Natural (Wd - 1)) and 1);
                  G (El_Idx) := Els (Iels).Val;
                  Iels := Iels + 1;
               end loop;
               Els (Oels) :=
                 (Sel => S_Group,
                  Val => Build_Mux2 (Build_Context, Sub_Sel, G (0), G (1)));
               Oels := Oels + 1;
            end;
         end loop;
         Lels := Oels - 1;
      end if;
      pragma Assert (Lels = Els'First);
      Res := Els (Els'First).Val;
   end Synth_Case;

   procedure Synth_Case_Statement (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      use Vhdl.Sem_Expr;

      Expr : constant Node := Get_Expression (Stmt);
      Choices : constant Node := Get_Case_Statement_Alternative_Chain (Stmt);
      Choice : Node;

      Case_Info : Choice_Info_Type;
      Annex_Arr : Annex_Array_Acc;

      --  Array of alternatives
      Alts : Alternative_Data_Acc;
      Alt_Idx : Alternative_Index;
      Others_Alt_Idx : Alternative_Index;

      --  Array of choices.  Contains tuple of (Value, Alternative).
      Choice_Data : Choice_Data_Array_Acc;
      Choice_Idx : Natural;

      Case_El : Case_Element_Array_Acc;

      Nbr_Wires : Natural;
      Wires : Wire_Id_Array_Acc;

      Sel : Value_Acc;
      Sel_Net : Net;
   begin
      --  Strategies to synthesize a case statement.  Assume the selector is
      --  a net of W bits
      --  - a large mux, with 2**W inputs
      --    - if the number of choices is dense
      --    - if W is small
      --  - a onehot mux.  Each choice is converted to an single bit condition
      --    by adding a comparison operator (equal for single choice,
      --    inequalities for ranges, or for multiple choices). Only one of
      --    these conditions is true (plus 'others').
      --    - if the number of choices is sparse
      --    - large range choices
      --  - a tree of mux/mux2
      --    - large number of choices, densily grouped but sparsed compared
      --       to 2**W (eg: a partially filled memory)
      --    - divide and conquier

      --  Create a net for the expression.
      Sel := Synth_Expression (Syn_Inst, Expr);

      --  Count choices and alternatives.
      Count_Choices (Case_Info, Choices);
      Fill_Choices_Array (Case_Info, Choices);

      --  Allocate structures.
      --  Because there is no 1-1 link between choices and alternatives,
      --  create an array for the choices and an array for the alternatives.
      Alts := new Alternative_Data_Array
        (1 .. Alternative_Index (Case_Info.Nbr_Alternatives));
      Choice_Data := new Choice_Data_Array (1 .. Case_Info.Nbr_Choices);
      Annex_Arr := new Annex_Array (1 .. Case_Info.Nbr_Choices);
      Case_Info.Annex_Arr := Annex_Arr;

      --  Synth statements, extract choice value.
      Alt_Idx := 0;
      Others_Alt_Idx := 0;
      Choice_Idx := 0;
      Choice := Choices;
      while Is_Valid (Choice) loop
         if not Get_Same_Alternative_Flag (Choice) then
            Alt_Idx := Alt_Idx + 1;

            declare
               Phi : Phi_Type;
            begin
               Push_Phi;
               Synth_Sequential_Statements
                 (Syn_Inst, Get_Associated_Chain (Choice));
               Pop_Phi (Phi);
               Alts (Alt_Idx).Asgns := Sort_Phi (Phi);
            end;
         end if;

         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_Expression =>
               Choice_Idx := Choice_Idx + 1;
               Annex_Arr (Choice_Idx) := Int32 (Alt_Idx);
               declare
                  Choice_Expr : constant Node :=
                    Get_Choice_Expression (Choice);
                  Val, Dc : Uns64;
               begin
                  Convert_To_Uns64 (Choice_Expr, Val, Dc);
                  if Dc = 0 then
                     Choice_Data (Choice_Idx) := (Val => Val,
                                                  Alt => Alt_Idx);
                  else
                     Error_Msg_Synth (+Choice_Expr, "meta-values never match");
                     Choice_Data (Choice_Idx) := (Val => 0,
                                                  Alt => 0);
                  end if;
               end;
            when Iir_Kind_Choice_By_Others =>
               Others_Alt_Idx := Alt_Idx;
            when others =>
               raise Internal_Error;
         end case;
         Choice := Get_Chain (Choice);
      end loop;
      pragma Assert (Choice_Idx = Choice_Data'Last);

      --  Sort by order.
      if Get_Kind (Get_Type (Expr)) in Iir_Kinds_Discrete_Type_Definition then
         Sort_Discrete_Choices (Case_Info);
      else
         Sort_String_Choices (Case_Info);
      end if;

      --  Create list of wire_id, sort it.
      Nbr_Wires := Count_Wires_In_Alternatives (Alts.all);
      Wires := new Wire_Id_Array (1 .. Nbr_Wires);
      Fill_Wire_Id_Array (Wires.all, Alts.all);

      --  Sort Wires.
      Sort_Wire_Id_Array (Wires.all);

      --  Associate each choice with the assign node
      --  For each wire_id:
      --    Build mux2/mux4 tree (group by 4)
      Case_El := new Case_Element_Array (1 .. Case_Info.Nbr_Choices);

      Sel_Net := Get_Net (Sel, Get_Type (Expr));

      --  For each wire, compute the result.
      for I in Wires'Range loop
         declare
            Wi : constant Wire_Id := Wires (I);
            Last_Val : constant Net := Get_Last_Assigned_Value (Wi);
            Res : Net;
            Default : Net;
            C : Natural;
         begin
            --  Extract the value for each alternative.
            for Alt of Alts.all loop
               --  If there is an assignment to Wi in Alt, it will define the
               --  value.  Otherwise, use Last_Val, ie the last assignment
               --  before the case.
               if Get_Wire_Id (Alt.Asgns) = Wi then
                  Alt.Val := Get_Assign_Value (Alt.Asgns);
                  Alt.Asgns := Get_Assign_Chain (Alt.Asgns);
               else
                  Alt.Val := Last_Val;
               end if;
            end loop;

            --  Build the map between choices and values.
            for J in Annex_Arr'Range loop
               C := Natural (Annex_Arr (J));
               Case_El (J) := (Sel => Choice_Data (C).Val,
                               Val => Alts (Choice_Data (C).Alt).Val);
            end loop;

            --  Extract default value (for missing alternative).
            if Others_Alt_Idx /= 0 then
               Default := Alts (Others_Alt_Idx).Val;
            else
               Default := No_Net;
            end if;

            --  Generate the muxes tree.
            Synth_Case (Sel_Net, Case_El.all, Default, Res);
            Phi_Assign (Wi, Res);
         end;
      end loop;

      --  free.
      Free_Case_Element_Array (Case_El);
      Free_Wire_Id_Array (Wires);
      Free_Choice_Data_Array (Choice_Data);
      Free_Annex_Array (Annex_Arr);
      Free_Alternative_Data_Array (Alts);
   end Synth_Case_Statement;

   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node)
   is
      Inter : Node;
      Assoc : Node;
      Assoc_Inter : Node;
      Actual : Node;
      Val : Value_Acc;
   begin
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);

         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               Actual := Get_Default_Value (Inter);
            when Iir_Kind_Association_Element_By_Expression =>
               Actual := Get_Actual (Assoc);
            when others =>
               raise Internal_Error;
         end case;

         case Iir_Parameter_Modes (Get_Mode (Inter)) is
            when Iir_In_Mode =>
               Val := Synth_Expression_With_Type
                 (Caller_Inst, Actual, Get_Type (Inter));
            when Iir_Out_Mode | Iir_Inout_Mode =>
               --  FIXME: todo
               raise Internal_Error;
         end case;

         case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_Variable_Declaration =>
               --  FIXME: Arguments are passed by copy.
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_Signal_Declaration =>
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_File_Declaration =>
               raise Internal_Error;
         end case;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Subprogram_Association;

   procedure Synth_Subprogram_Back_Association
     (Subprg_Inst : Synth_Instance_Acc;
      Caller_Inst : Synth_Instance_Acc;
      Inter_Chain : Node;
      Assoc_Chain : Node)
   is
      Inter : Node;
      Assoc : Node;
      Assoc_Inter : Node;
      Val : Value_Acc;
   begin
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);

         if Get_Mode (Inter) = Iir_Out_Mode then
            Val := Synth_Expression_With_Type
              (Subprg_Inst, Inter, Get_Type (Inter));
            Synth_Assignment (Caller_Inst, Get_Actual (Assoc), Val);

         end if;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Subprogram_Back_Association;

   procedure Synth_Procedure_Call
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Call : constant Node := Get_Procedure_Call (Stmt);
      Imp  : constant Node := Get_Implementation (Call);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Subprg_Body : constant Node := Get_Subprogram_Body (Imp);
      Decls_Chain : constant Node := Get_Declaration_Chain (Subprg_Body);
      Sub_Syn_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
   begin
      if Get_Implicit_Definition (Imp) in Iir_Predefined_Implicit then
         Error_Msg_Synth (+Stmt, "call to implicit %n is not supported", +Imp);
         return;
      elsif Get_Foreign_Flag (Imp) then
         Error_Msg_Synth (+Stmt, "call to foreign %n is not supported", +Imp);
         return;
      end if;

      Areapools.Mark (M, Instance_Pool.all);
      Sub_Syn_Inst := Make_Instance (Syn_Inst, Get_Info (Imp));

      Synth_Subprogram_Association
        (Sub_Syn_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      Synth_Declarations (Sub_Syn_Inst, Decls_Chain);

      if Is_Valid (Decls_Chain) then
         Sub_Syn_Inst.Name := New_Sname (Syn_Inst.Name, Get_Identifier (Imp));
         Synth_Declarations (Sub_Syn_Inst, Decls_Chain);
      end if;

      Synth_Sequential_Statements
        (Sub_Syn_Inst, Get_Sequential_Statement_Chain (Subprg_Body));

      Synth_Subprogram_Back_Association
        (Sub_Syn_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      Free_Instance (Sub_Syn_Inst);
      Areapools.Release (M, Instance_Pool.all);
   end Synth_Procedure_Call;

   function In_Range (Rng : Value_Acc; V : Int64) return Boolean is
   begin
      case Rng.Rng.Dir is
         when Iir_To =>
            return V >= Rng.Rng.Left and then V <= Rng.Rng.Right;
         when Iir_Downto =>
            return V <= Rng.Rng.Left and then V >= Rng.Rng.Right;
      end case;
   end In_Range;

   procedure Synth_For_Loop_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      It_Rng : Value_Acc;
      It_Type : Node;
      Val : Value_Acc;
   begin
      It_Type := Get_Declaration_Type (Iterator);
      if It_Type /= Null_Node then
         Synth_Subtype_Indication (Syn_Inst, It_Type);
      end if;

      --  Initial value.
      It_Rng := Get_Value (Syn_Inst, Get_Type (Iterator));
      Val := Create_Value_Discrete (It_Rng.Rng.Left);
      Create_Object (Syn_Inst, Iterator, Val);

      while In_Range (It_Rng, Val.Scal) loop
         Synth_Sequential_Statements (Syn_Inst, Stmts);
         case It_Rng.Rng.Dir is
            when Iir_To =>
               Val.Scal := Val.Scal + 1;
            when Iir_Downto =>
               Val.Scal := Val.Scal - 1;
         end case;
      end loop;
      Destroy_Object (Syn_Inst, Iterator);
      if It_Type /= Null_Node then
         Destroy_Object (Syn_Inst, It_Type);
      end if;
   end Synth_For_Loop_Statement;

   procedure Synth_Return_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Val : Value_Acc;
      Expr : constant Node := Get_Expression (Stmt);
   begin
      if Expr = Null_Node then
         --  TODO: return in procedure.
         raise Internal_Error;
      end if;
      Val := Synth_Expression (Syn_Inst, Expr);
      if Syn_Inst.Return_Value /= null then
         --  TODO: multiple return
         raise Internal_Error;
      end if;
      Syn_Inst.Return_Value := Val;
   end Synth_Return_Statement;

   procedure Synth_Sequential_Statements
     (Syn_Inst : Synth_Instance_Acc; Stmts : Node)
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Is_Valid (Stmt) loop
         case Get_Kind (Stmt) is
            when Iir_Kind_If_Statement =>
               Synth_If_Statement (Syn_Inst, Stmt);
            when Iir_Kind_Simple_Signal_Assignment_Statement =>
               Synth_Simple_Signal_Assignment (Syn_Inst, Stmt);
            when Iir_Kind_Variable_Assignment_Statement =>
               Synth_Variable_Assignment (Syn_Inst, Stmt);
            when Iir_Kind_Case_Statement =>
               Synth_Case_Statement (Syn_Inst, Stmt);
            when Iir_Kind_For_Loop_Statement =>
               Synth_For_Loop_Statement (Syn_Inst, Stmt);
            when Iir_Kind_Null_Statement =>
               --  Easy
               null;
            when Iir_Kind_Return_Statement =>
               Synth_Return_Statement (Syn_Inst, Stmt);
               exit;
            when Iir_Kind_Procedure_Call_Statement =>
               Synth_Procedure_Call (Syn_Inst, Stmt);
            when others =>
               Error_Kind ("synth_sequential_statements", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Synth_Sequential_Statements;

   Proc_Pool : aliased Areapools.Areapool;

   --  Synthesis of statements of a non-sensitized process.
   procedure Synth_Process_Sequential_Statements
     (Syn_Inst : Synth_Instance_Acc; Proc : Node)
   is
      Stmt : Node;
      Cond : Node;
      Cond_Val : Value_Acc;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      Stmt := Get_Sequential_Statement_Chain (Proc);

      --  The first statement must be a wait statement.
      if Get_Kind (Stmt) /= Iir_Kind_Wait_Statement then
         Error_Msg_Synth (+Stmt, "expect wait as the first statement");
         return;
      end if;

      --  Handle the condition as an if.
      Cond := Get_Condition_Clause (Stmt);
      Cond_Val := Synth_Expression (Syn_Inst, Cond);

      Push_Phi;
      Synth_Sequential_Statements (Syn_Inst, Get_Chain (Stmt));
      Pop_Phi (Phi_True);
      Push_Phi;
      Pop_Phi (Phi_False);

      Merge_Phis (Build_Context, Get_Net (Cond_Val, Get_Type (Cond)),
                  Phi_True, Phi_False);
   end Synth_Process_Sequential_Statements;

   procedure Synth_Process_Statement
     (Syn_Inst : Synth_Instance_Acc; Proc : Node)
   is
      use Areapools;
      Info : constant Sim_Info_Acc := Get_Info (Proc);
      Decls_Chain : constant Node := Get_Declaration_Chain (Proc);
      Proc_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
   begin
      Proc_Inst := Make_Instance (Syn_Inst, Info);
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      if Is_Valid (Decls_Chain) then
         Proc_Inst.Name := New_Sname (Syn_Inst.Name, Get_Identifier (Proc));
         Synth_Declarations (Proc_Inst, Decls_Chain);
      end if;

      case Iir_Kinds_Process_Statement (Get_Kind (Proc)) is
         when Iir_Kind_Sensitized_Process_Statement =>
            Synth_Sequential_Statements
              (Proc_Inst, Get_Sequential_Statement_Chain (Proc));
            --  FIXME: check sensitivity list.
         when Iir_Kind_Process_Statement =>
            Synth_Process_Sequential_Statements (Proc_Inst, Proc);
      end case;

      Free_Instance (Proc_Inst);
      Release (M, Proc_Pool);
      Instance_Pool := null;
   end Synth_Process_Statement;

   procedure Synth_Generate_Statement_Body
     (Syn_Inst : Synth_Instance_Acc; Bod : Node)
   is
      use Areapools;
      Info : constant Sim_Info_Acc := Get_Info (Bod);
      Decls_Chain : constant Node := Get_Declaration_Chain (Bod);
      Bod_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
   begin
      Bod_Inst := Make_Instance (Syn_Inst, Info);
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      if Is_Valid (Decls_Chain) then
         Bod_Inst.Name := New_Sname (Syn_Inst.Name, Get_Identifier (Bod));
         Synth_Declarations (Bod_Inst, Decls_Chain);
      end if;

      Synth_Concurrent_Statements
        (Bod_Inst, Get_Concurrent_Statement_Chain (Bod));

      Free_Instance (Bod_Inst);
      Release (M, Proc_Pool);
      Instance_Pool := null;
   end Synth_Generate_Statement_Body;

   procedure Synth_Concurrent_Assertion_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Cond : constant Node := Get_Assertion_Condition (Stmt);
      Val : Value_Acc;
   begin
      Val := Synth_Expression (Syn_Inst, Cond);
      if Is_Const (Val) then
         if Val.Scal /= 1 then
            raise Internal_Error;
         end if;
         return;
      end if;
      Build_Assert (Build_Context, Get_Net (Val, Get_Type (Cond)));
   end Synth_Concurrent_Assertion_Statement;

   function Synth_PSL_Expression
     (Syn_Inst : Synth_Instance_Acc; Expr : PSL.Types.PSL_Node) return Net
   is
      use PSL.Nodes;
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr =>
            declare
               E : constant Vhdl.Types.Vhdl_Node := Get_HDL_Node (Expr);
            begin
               return Get_Net (Synth_Expression (Syn_Inst, E), Get_Type (E));
            end;
         when N_Not_Bool =>
            return Build_Monadic
              (Build_Context, Netlists.Gates.Id_Not,
               Synth_PSL_Expression (Syn_Inst, Get_Boolean (Expr)));
         when others =>
            PSL.Errors.Error_Kind ("translate_psl_expr", Expr);
      end case;
   end Synth_PSL_Expression;

   function Synth_Psl_NFA (Syn_Inst : Synth_Instance_Acc;
                           NFA : PSL.Types.PSL_NFA;
                           Nbr_States : Int32;
                           States : Net) return Net
   is
      use PSL.NFAs;
      S : NFA_State;
      S_Num : Int32;
      D_Num : Int32;
      I : Net;
      Cond : Net;
      E : NFA_Edge;
      D_Arr : Net_Array_Acc;
      Res : Net;
   begin
      D_Arr := new Net_Array'(0 .. Nbr_States - 1 => No_Net);
      S := Get_First_State (NFA);
      while S /= No_State loop
         S_Num := Get_State_Label (S);
         I := Build_Extract_Bit (Build_Context, States, Uns32 (S_Num));

         E := Get_First_Src_Edge (S);
         while E /= No_Edge loop
            Cond := Build_Dyadic
              (Build_Context, Netlists.Gates.Id_And,
               I, Synth_PSL_Expression (Syn_Inst, Get_Edge_Expr (E)));

            D_Num := Nbr_States - 1 - Get_State_Label (Get_Edge_Dest (E));
            if D_Arr (D_Num) = No_Net then
               D_Arr (D_Num) := Cond;
            else
               D_Arr (D_Num) := Build_Dyadic
                 (Build_Context, Netlists.Gates.Id_Or, D_Arr (D_Num), Cond);
            end if;

            E := Get_Next_Src_Edge (E);
         end loop;

         S := Get_Next_State (S);
      end loop;

      if D_Arr (Nbr_States - 1) = No_Net then
         D_Arr (Nbr_States - 1) := Build_Const_UB32 (Build_Context, 0, 1);
      end if;

      Res := Concat_Array (D_Arr);
      Free_Net_Array (D_Arr);

      return Res;
   end Synth_Psl_NFA;

   procedure Synth_Psl_Restrict_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Nbr_States : constant Int32 := Get_PSL_Nbr_States (Stmt);
      Init : Net;
      Clk : Net;
      States : Net;
      Next_States : Net;
   begin
      --  create init net, clock net
      pragma Assert (Nbr_States <= 32);
      Init := Build_Const_UB32 (Build_Context, 1, Uns32 (Nbr_States));
      Clk := Synth_PSL_Expression (Syn_Inst, Get_PSL_Clock (Stmt));

      --  build idff
      States := Build_Idff (Build_Context, Clk, No_Net, Init);

      --  create update nets
      --  For each state: if set, evaluate all outgoing edges.
      Next_States :=
        Synth_Psl_NFA (Syn_Inst, Get_PSL_NFA (Stmt), Nbr_States, States);
      Connect (Get_Input (Get_Parent (States), 1), Next_States);

      --  Build assume gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Build_Assume (Build_Context,
                    Build_Reduce (Build_Context,
                                  Netlists.Gates.Id_Red_Or, Next_States));
   end Synth_Psl_Restrict_Directive;

   procedure Synth_Concurrent_Statements
     (Syn_Inst : Synth_Instance_Acc; Stmts : Node)
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Is_Valid (Stmt) loop
         Push_Phi;
         case Get_Kind (Stmt) is
            when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
               Synth_Simple_Signal_Assignment (Syn_Inst, Stmt);
            when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
               Synth_Conditional_Signal_Assignment (Syn_Inst, Stmt);
            when Iir_Kinds_Process_Statement =>
               Synth_Process_Statement (Syn_Inst, Stmt);
            when Iir_Kind_If_Generate_Statement =>
               declare
                  Gen : Node;
                  Bod : Node;
                  Cond : Value_Acc;
               begin
                  Gen := Stmt;
                  loop
                     Cond := Synth_Expression (Syn_Inst, Get_Condition (Gen));
                     pragma Assert (Cond.Kind = Value_Discrete);
                     if Cond.Scal = 1 then
                        Bod := Get_Generate_Statement_Body (Gen);
                        Synth_Generate_Statement_Body (Syn_Inst, Bod);
                        exit;
                     end if;
                     Gen := Get_Generate_Else_Clause (Gen);
                     exit when Gen = Null_Node;
                  end loop;
               end;
            when Iir_Kind_Concurrent_Assertion_Statement =>
               Synth_Concurrent_Assertion_Statement (Syn_Inst, Stmt);
            when Iir_Kind_Component_Instantiation_Statement =>
               if Is_Component_Instantiation (Stmt) then
                  Synth_Component_Instantiation_Statement (Syn_Inst, Stmt);
               else
                  Synth_Design_Instantiation_Statement (Syn_Inst, Stmt);
               end if;
            when Iir_Kind_Psl_Default_Clock =>
               null;
            when Iir_Kind_Psl_Restrict_Directive =>
               Synth_Psl_Restrict_Directive (Syn_Inst, Stmt);
            when others =>
               Error_Kind ("synth_statements", Stmt);
         end case;
         Pop_And_Merge_Phi (Build_Context);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Synth_Concurrent_Statements;
end Synth.Stmts;
