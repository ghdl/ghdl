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

with Grt.Algos;
with Areapools;
with Std_Names;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Types;
with Vhdl.Sem_Expr;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Evaluation;

with PSL.Types;
with PSL.Nodes;
with PSL.NFAs;
with PSL.Errors;

with Synth.Errors; use Synth.Errors;
with Synth.Decls; use Synth.Decls;
with Synth.Expr; use Synth.Expr;
with Synth.Insts; use Synth.Insts;
with Synth.Source;

with Netlists.Builders; use Netlists.Builders;
with Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;

package body Synth.Stmts is
   procedure Synth_Sequential_Statements
     (C : in out Seq_Context; Stmts : Node);

   procedure Set_Location (N : Net; Loc : Node)
     renames Synth.Source.Set_Location;

   function Synth_Waveform (Syn_Inst : Synth_Instance_Acc;
                            Wf : Node;
                            Targ_Type : Type_Acc) return Value_Acc is
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
      if Targ_Type = null then
         return Synth_Expression (Syn_Inst, Get_We_Value (Wf));
      else
         return Synth_Expression_With_Type
           (Syn_Inst, Get_We_Value (Wf), Targ_Type);
      end if;
   end Synth_Waveform;

   procedure Synth_Assign (Wid : Wire_Id;
                           Typ : Type_Acc;
                           Val : Value_Acc;
                           Offset : Uns32;
                           Loc : Source.Syn_Src) is
   begin
      Phi_Assign (Build_Context, Wid,
                  Get_Net (Synth_Subtype_Conversion (Val, Typ, False, Loc)),
                  Offset);
   end Synth_Assign;

   procedure Synth_Assignment_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                         Target : Node;
                                         Target_Type : Type_Acc;
                                         Val : Value_Acc;
                                         Loc : Node)
   is
      Choice : Node;
      Assoc : Node;
      Pos : Uns32;
   begin
      if Target_Type.Kind = Type_Vector then
         Choice := Get_Association_Choices_Chain (Target);
         Pos := Target_Type.W;
         while Is_Valid (Choice) loop
            Assoc := Get_Associated_Expr (Choice);
            case Get_Kind (Choice) is
               when Iir_Kind_Choice_By_None =>
                  Pos := Pos - 1;
                  Synth_Assignment
                    (Syn_Inst, Assoc, Bit_Extract (Val, Pos, Target), Loc);
               when others =>
                  Error_Kind ("synth_assignment_aggregate", Choice);
            end case;
            Choice := Get_Chain (Choice);
         end loop;
      else
         raise Internal_Error;
      end if;
   end Synth_Assignment_Aggregate;

   procedure Synth_Assignment_Prefix (Syn_Inst : Synth_Instance_Acc;
                                      Pfx : Node;
                                      Dest_Obj : out Value_Acc;
                                      Dest_Off : out Uns32;
                                      Dest_Voff : out Net;
                                      Dest_Rdwd : out Width;
                                      Dest_Type : out Type_Acc) is
   begin
      case Get_Kind (Pfx) is
         when Iir_Kind_Simple_Name =>
            Synth_Assignment_Prefix (Syn_Inst, Get_Named_Entity (Pfx),
                                     Dest_Obj, Dest_Off,
                                     Dest_Voff, Dest_Rdwd, Dest_Type);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Anonymous_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            declare
               Targ : constant Value_Acc := Get_Value (Syn_Inst, Pfx);
            begin
               Dest_Voff := No_Net;
               Dest_Rdwd := 0;
               Dest_Type := Targ.Typ;

               if Targ.Kind = Value_Alias then
                  Dest_Obj := Targ.A_Obj;
                  Dest_Off := Targ.A_Off;
               else
                  Dest_Obj := Targ;
                  Dest_Off := 0;
               end if;
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Voff : Net;
               Off : Uns32;
               W : Width;
               Dest_W : Width;
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx),
                  Dest_Obj, Dest_Off, Dest_Voff, Dest_Rdwd, Dest_Type);
               Dest_W := Dest_Type.W;
               Synth_Indexed_Name (Syn_Inst, Pfx, Dest_Type, Voff, Off, W);

               Dest_Off := Dest_Off + Off;
               Dest_Type := Get_Array_Element (Dest_Type);

               if Voff /= No_Net then
                  if Dest_Voff = No_Net then
                     Dest_Voff := Voff;
                     Dest_Rdwd := Dest_W;
                  else
                     Dest_Voff := Build_Addidx
                       (Get_Build (Syn_Inst), Dest_Voff, Voff);
                  end if;
               end if;
            end;

         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Pfx));
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx),
                  Dest_Obj, Dest_Off, Dest_Voff, Dest_Rdwd, Dest_Type);
               Dest_Off := Dest_Off + Dest_Type.Rec.E (Idx + 1).Off;
               Dest_Type := Dest_Type.Rec.E (Idx + 1).Typ;
            end;

         when Iir_Kind_Slice_Name =>
            declare
               Pfx_Bnd : Bound_Type;
               El_Typ : Type_Acc;
               Res_Bnd : Bound_Type;
               Sl_Voff : Net;
               Sl_Off : Uns32;
               Wd : Uns32;
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx),
                  Dest_Obj, Dest_Off, Dest_Voff, Dest_Rdwd, Dest_Type);

               Get_Onedimensional_Array_Bounds (Dest_Type, Pfx_Bnd, El_Typ);
               Synth_Slice_Suffix (Syn_Inst, Pfx, Pfx_Bnd, El_Typ.W,
                                   Res_Bnd, Sl_Voff, Sl_Off, Wd);

               Dest_Off := Dest_Off + Sl_Off;

               if Sl_Voff /= No_Net then
                  if Dest_Voff /= No_Net then
                     Dest_Voff := Build_Addidx
                       (Get_Build (Syn_Inst), Dest_Voff, Sl_Voff);
                  else
                     Dest_Rdwd := Dest_Type.W;
                     Dest_Voff := Sl_Voff;
                  end if;
                  Dest_Type := Create_Slice_Type (Wd, El_Typ);
               else
                  Dest_Type :=
                    Create_Onedimensional_Array_Subtype (Dest_Type, Res_Bnd);
               end if;
            end;

         when others =>
            Error_Kind ("synth_assignment_prefix", Pfx);
      end case;
   end Synth_Assignment_Prefix;

   type Target_Kind is
     (Target_Simple, Target_Aggregate, Target_Memory);

   type Target_Info (Kind : Target_Kind := Target_Simple) is record
      --  In all cases, the type of the target is known or computed.
      Targ_Type : Type_Acc;

      case Kind is
         when Target_Simple =>
            --  For a simple target, the destination is known.
            Obj : Value_Acc;
            Off : Uns32;
         when Target_Aggregate =>
            --  For an aggregate: the type is computed and the details will
            --  be handled at the assignment.
            Aggr : Node;
         when Target_Memory =>
            --  For a memory: the destination is known.
            Mem_Obj : Value_Acc;
            --  The width of the whole mrmory.
            Mem_Width : Width;
            --  The dynamic offset.
            Mem_Voff : Net;
            Mem_Off : Uns32;
      end case;
   end record;

   type Target_Info_Array is array (Natural range <>) of Target_Info;

   function Synth_Target (Syn_Inst : Synth_Instance_Acc;
                          Target : Node) return Target_Info is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate =>
            declare
               Targ_Type : constant Node := Get_Type (Target);
               Base_Typ : Type_Acc;
               Bnd : Bound_Type;
            begin
               Base_Typ :=
                 Get_Value_Type (Syn_Inst, Get_Base_Type (Targ_Type));
               case Base_Typ.Kind is
                  when Type_Unbounded_Vector =>
                     Bnd := Expr.Synth_Array_Bounds (Syn_Inst, Targ_Type, 0);
                     return Target_Info' (Kind => Target_Aggregate,
                                          Targ_Type => Create_Vector_Type
                                            (Bnd, Base_Typ.Uvec_El),
                                          Aggr => Target);
                  when others =>
                     raise Internal_Error;
               end case;
            end;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Anonymous_Signal_Declaration
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            declare
               Obj : Value_Acc;
               Off : Uns32;
               Typ : Type_Acc;

               Voff : Net;
               Rdwd : Width;
            begin
               Synth_Assignment_Prefix (Syn_Inst, Target,
                                        Obj, Off, Voff, Rdwd, Typ);
               if Voff = No_Net then
                  --  FIXME: check index.
                  return Target_Info'(Kind => Target_Simple,
                                      Targ_Type => Typ,
                                      Obj => Obj,
                                      Off => Off);
               else
                  return Target_Info'(Kind => Target_Memory,
                                      Targ_Type => Typ,
                                      Mem_Obj => Obj,
                                      Mem_Width => Rdwd,
                                      Mem_Voff => Voff,
                                      Mem_Off => Off);
               end if;
            end;
         when others =>
            Error_Kind ("synth_target", Target);
      end case;
   end Synth_Target;

   procedure Assign_Value (Targ : Value_Acc; Val : Value_Acc; Loc : Node) is
   begin
      case Targ.Kind is
         when Value_Discrete =>
            Targ.Scal := Val.Scal;
         when Value_Array =>
            declare
               Len : constant Iir_Index32 := Val.Arr.Len;
            begin
               for I in 1 .. Len loop
                  Assign_Value (Targ.Arr.V (Targ.Arr.Len - Len + I),
                                Val.Arr.V (I), Loc);
               end loop;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Assign_Value;

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Target_Info;
                               Val : Value_Acc;
                               Loc : Node) is
   begin
      case Target.Kind is
         when Target_Aggregate =>
            Synth_Assignment_Aggregate
              (Syn_Inst, Target.Aggr, Target.Targ_Type, Val, Loc);
         when Target_Simple =>
            if Target.Obj.Kind = Value_Wire then
               Synth_Assign (Target.Obj.W, Target.Targ_Type,
                             Val, Target.Off, Loc);
            else
               pragma Assert (Target.Off = 0);
               Assign_Value (Target.Obj, Val, Loc);
            end if;
         when Target_Memory =>
            declare
               V : Net;
            begin
               V := Get_Current_Assign_Value
                 (Get_Build (Syn_Inst), Target.Mem_Obj.W, Target.Mem_Off,
                  Target.Mem_Width);
               V := Build_Dyn_Insert (Get_Build (Syn_Inst), V, Get_Net (Val),
                  Target.Mem_Voff, Target.Mem_Off);
               Set_Location (V, Loc);
               Synth_Assign
                 (Target.Mem_Obj.W, Target.Targ_Type,
                  Create_Value_Net (V, Target.Targ_Type), Target.Mem_Off, Loc);
            end;
      end case;
   end Synth_Assignment;

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Node;
                               Val : Value_Acc;
                               Loc : Node)
   is
      Info : Target_Info;
   begin
      Info := Synth_Target (Syn_Inst, Target);
      Synth_Assignment (Syn_Inst, Info, Val, Loc);
   end Synth_Assignment;

   function Synth_Read_Memory (Syn_Inst : Synth_Instance_Acc;
                               Obj : Value_Acc;
                               Off : Uns32;
                               Voff : Net;
                               Typ : Type_Acc;
                               Loc : Node) return Value_Acc
   is
      N : Net;
   begin
      if Voff /= No_Net then
         N := Build_Dyn_Extract
           (Get_Build (Syn_Inst), Get_Net (Obj), Voff, Off, Typ.W);
      else
         if Off = 0 and then Typ.W = Obj.Typ.W then
            --  Nothing to do if extracting the whole object.
            return Obj;
         end if;
         N := Build_Extract (Get_Build (Syn_Inst), Get_Net (Obj), Off, Typ.W);
      end if;
      Set_Location (N, Loc);
      return Create_Value_Net (N, Typ);
   end Synth_Read_Memory;

   --  Concurrent or sequential simple signal assignment
   procedure Synth_Simple_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Targ : Target_Info;
      Val : Value_Acc;
   begin
      Targ := Synth_Target (Syn_Inst, Get_Target (Stmt));
      Val := Synth_Waveform
        (Syn_Inst, Get_Waveform_Chain (Stmt), Targ.Targ_Type);
      Synth_Assignment (Syn_Inst, Targ, Val, Stmt);
   end Synth_Simple_Signal_Assignment;

   procedure Synth_Conditional_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Targ : Target_Info;
      Cond : Node;
      Cwf : Node;
      Val, Cond_Val : Value_Acc;
      First, Last : Net;
      V : Net;
   begin
      Targ := Synth_Target (Syn_Inst, Get_Target (Stmt));
      Last := No_Net;
      Cwf := Get_Conditional_Waveform_Chain (Stmt);
      while Cwf /= Null_Node loop
         Val := Synth_Waveform
           (Syn_Inst, Get_Waveform_Chain (Cwf), Targ.Targ_Type);
         V := Get_Net (Val);
         Cond := Get_Condition (Cwf);
         if Cond /= Null_Node then
            Cond_Val := Synth_Expression (Syn_Inst, Cond);
            V := Build_Mux2 (Build_Context,
                             Get_Net (Cond_Val),
                             No_Net, V);
            Set_Location (V, Cwf);
         end if;

         if Last /= No_Net then
            Connect (Get_Input (Get_Net_Parent (Last), 1), V);
         else
            First := V;
         end if;
         Last := V;
         Cwf := Get_Chain (Cwf);
      end loop;
      Val := Create_Value_Net (First, Targ.Targ_Type);
      Synth_Assignment (Syn_Inst, Targ, Val, Stmt);
   end Synth_Conditional_Signal_Assignment;

   procedure Synth_Variable_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Targ : Target_Info;
      Val : Value_Acc;
   begin
      Targ := Synth_Target (Syn_Inst, Get_Target (Stmt));
      Val := Synth_Expression_With_Type
        (Syn_Inst, Get_Expression (Stmt), Targ.Targ_Type);
      Synth_Assignment (Syn_Inst, Targ, Val, Stmt);
   end Synth_Variable_Assignment;

   procedure Synth_Conditional_Variable_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Target : constant Node := Get_Target (Stmt);
      Targ_Type : Type_Acc;
      Cond : Node;
      Ce : Node;
      Val, Cond_Val : Value_Acc;
      V : Net;
      First, Last : Net;
   begin
      Targ_Type := Get_Value_Type (Syn_Inst, Get_Type (Target));
      Last := No_Net;
      Ce := Get_Conditional_Expression_Chain (Stmt);
      while Ce /= Null_Node loop
         Val := Synth_Expression_With_Type
           (Syn_Inst, Get_Expression (Ce), Targ_Type);
         V := Get_Net (Val);
         Cond := Get_Condition (Ce);
         if Cond /= Null_Node then
            Cond_Val := Synth_Expression (Syn_Inst, Cond);
            V := Build_Mux2 (Build_Context, Get_Net (Cond_Val), No_Net, V);
            Set_Location (V, Ce);
         end if;

         if Last /= No_Net then
            Connect (Get_Input (Get_Net_Parent (Last), 1), V);
         else
            First := V;
         end if;
         Last := V;
         Ce := Get_Chain (Ce);
      end loop;
      Val := Create_Value_Net (First, Targ_Type);
      Synth_Assignment (Syn_Inst, Target, Val, Stmt);
   end Synth_Conditional_Variable_Assignment;

   procedure Synth_If_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Cond : constant Node := Get_Condition (Stmt);
      Els : constant Node := Get_Else_Clause (Stmt);
      Cond_Val : Value_Acc;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      Cond_Val := Synth_Expression (C.Inst, Cond);
      if Is_Const (Cond_Val) then
         if Cond_Val.Scal = 1 then
            --  True.
            Synth_Sequential_Statements
              (C, Get_Sequential_Statement_Chain (Stmt));
         else
            pragma Assert (Cond_Val.Scal = 0);
            if Is_Valid (Els) then
               --  Else part
               if Is_Null (Get_Condition (Els)) then
                  --  Final else part.
                  Synth_Sequential_Statements
                    (C, Get_Sequential_Statement_Chain (Els));
               else
                  --  Elsif.  Handled as a nested if.
                  Synth_If_Statement (C, Els);
               end if;
            end if;
         end if;
      else
         Push_Phi;
         Synth_Sequential_Statements
           (C, Get_Sequential_Statement_Chain (Stmt));
         Pop_Phi (Phi_True);

         Push_Phi;
         if Is_Valid (Els) then
            if Is_Null (Get_Condition (Els)) then
               --  Final else part.
               Synth_Sequential_Statements
                 (C, Get_Sequential_Statement_Chain (Els));
            else
               --  Elsif.  Handled as a nested if.
               Synth_If_Statement (C, Els);
            end if;
         end if;
         Pop_Phi (Phi_False);

         Merge_Phis (Build_Context,
                     Get_Net (Cond_Val), Phi_True, Phi_False, Stmt);
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
      elsif El_Type = Vhdl.Std_Package.Bit_Type_Definition then
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
               case Get_Pos (Info, I) is
                  when 0 =>
                     Val := Val or 0;
                  when 1 =>
                     Val := Val or 1;
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
         when Iir_Kind_Integer_Type_Definition =>
            --  TODO: signed values.
            Dc := 0;
            Val := Uns64 (Get_Value (Expr));
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
      Asgns : Seq_Assign;
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
         return Is_Lt (Arr (Op1), Arr (Op2));
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
      Asgn : Seq_Assign;
      W : Wire_Id;
   begin
      Res := 0;
      for I in Alts'Range loop
         Asgn := Alts (I).Asgns;
         while Asgn /= No_Seq_Assign loop
            W := Get_Wire_Id (Asgn);
            if not Get_Wire_Mark (W) then
               Res := Res + 1;
               Set_Wire_Mark (W, True);
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
      Asgn : Seq_Assign;
      W : Wire_Id;
   begin
      Idx := Arr'First;
      for I in Alts'Range loop
         Asgn := Alts (I).Asgns;
         while Asgn /= No_Seq_Assign loop
            W := Get_Wire_Id (Asgn);
            if Get_Wire_Mark (W) then
               Arr (Idx) := W;
               Idx := Idx + 1;
               Set_Wire_Mark (W, False);
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
         --  Extract 2 bits from the selector.
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

   type Partial_Assign_Array_Acc is access Partial_Assign_Array;
   procedure Free_Partial_Assign_Array is new Ada.Unchecked_Deallocation
     (Partial_Assign_Array, Partial_Assign_Array_Acc);

   procedure Synth_Case_Statement (C : in out Seq_Context; Stmt : Node)
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
      Pasgns : Partial_Assign_Array_Acc;
      Nets : Net_Array_Acc;

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
      Sel := Synth_Expression_With_Basetype (C.Inst, Expr);

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
               Synth_Sequential_Statements (C, Get_Associated_Chain (Choice));
               Pop_Phi (Phi);
               Alts (Alt_Idx).Asgns := Sort_Phi (Phi);
            end;
         end if;

         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_Expression =>
               Choice_Idx := Choice_Idx + 1;
               Annex_Arr (Choice_Idx) := Int32 (Choice_Idx);
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
      Sort_Wire_Id_Array (Wires.all);

      --  Associate each choice with the assign node
      --  For each wire_id:
      --    Build mux2/mux4 tree (group by 4)
      Case_El := new Case_Element_Array (1 .. Case_Info.Nbr_Choices);

      Pasgns := new Partial_Assign_Array (1 .. Int32 (Alts'Last));
      Nets := new Net_Array (1 .. Int32 (Alts'Last));

      Sel_Net := Get_Net (Sel);

      --  For each wire, compute the result.
      for I in Wires'Range loop
         declare
            Wi : constant Wire_Id := Wires (I);
            Last_Val : Net;
            Res : Net;
            Default : Net;
            C : Natural;
            Min_Off, Off : Uns32;
            Wd : Width;
            List : Partial_Assign_List;
         begin
            --  Extract the value for each branch.
            for I in Alts'Range loop
               --  If there is an assignment to Wi in Alt, it will define the
               --  value.
               if Get_Wire_Id (Alts (I).Asgns) = Wi then
                  Pasgns (Int32 (I)) := Get_Assign_Partial (Alts (I).Asgns);
                  Alts (I).Asgns := Get_Assign_Chain (Alts (I).Asgns);
               else
                  Pasgns (Int32 (I)) := No_Partial_Assign;
               end if;
            end loop;

            Partial_Assign_Init (List);
            Min_Off := 0;
            loop
               Off := Min_Off;

               -- Extract value of partial assignments to NETS.
               Extract_Merge_Partial_Assigns
                 (Build_Context, Pasgns.all, Nets.all, Off, Wd);
               exit when Off = Uns32'Last and Wd = Width'Last;

               --  If a branch has no value, use the value before the case.
               Last_Val := No_Net;
               for I in Nets'Range loop
                  if Nets (I) = No_Net then
                     if Last_Val = No_Net then
                        Last_Val := Get_Current_Assign_Value
                          (Build_Context, Wi, Off, Wd);
                     end if;
                     Nets (I) := Last_Val;
                  end if;
               end loop;

               --  Build the map between choices and values.
               for J in Annex_Arr'Range loop
                  C := Natural (Annex_Arr (J));
                  Case_El (J) := (Sel => Choice_Data (C).Val,
                                  Val => Nets (Int32 (Choice_Data (C).Alt)));
               end loop;

               --  Extract default value (for missing alternative).
               if Others_Alt_Idx /= 0 then
                  Default := Nets (Int32 (Others_Alt_Idx));
               else
                  Default := No_Net;
               end if;

               --  Generate the muxes tree.
               Synth_Case (Sel_Net, Case_El.all, Default, Res);

               Partial_Assign_Append (List, New_Partial_Assign (Res, Off));
               Min_Off := Off + Wd;
            end loop;

            Merge_Partial_Assigns (Build_Context, Wi, List);
         end;
      end loop;

      --  free.
      Free_Case_Element_Array (Case_El);
      Free_Wire_Id_Array (Wires);
      Free_Choice_Data_Array (Choice_Data);
      Free_Annex_Array (Annex_Arr);
      Free_Alternative_Data_Array (Alts);
      Free_Partial_Assign_Array (Pasgns);
      Free_Net_Array (Nets);
   end Synth_Case_Statement;

   procedure Synth_Selected_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      use Vhdl.Sem_Expr;

      Targ : constant Node := Get_Target (Stmt);

      Expr : constant Node := Get_Expression (Stmt);
      Choices : constant Node := Get_Selected_Waveform_Chain (Stmt);
      Choice : Node;

      Targ_Type : Type_Acc;

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

      Sel : Value_Acc;
      Sel_Net : Net;
   begin
      Targ_Type := Get_Value_Type (Syn_Inst, Get_Type (Targ));
      --  Create a net for the expression.
      Sel := Synth_Expression_With_Basetype (Syn_Inst, Expr);

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

            Alts (Alt_Idx).Val := Get_Net
              (Synth_Waveform
                 (Syn_Inst, Get_Associated_Chain (Choice), Targ_Type));
         end if;

         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_Expression =>
               Choice_Idx := Choice_Idx + 1;
               Annex_Arr (Choice_Idx) := Int32 (Choice_Idx);
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

      --  Associate each choice with the assign node
      --  For each wire_id:
      --    Build mux2/mux4 tree (group by 4)
      Case_El := new Case_Element_Array (1 .. Case_Info.Nbr_Choices);

      Sel_Net := Get_Net (Sel);

      declare
         Res : Net;
         Default : Net;
         C : Natural;
      begin
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
         Synth_Assignment (Syn_Inst, Get_Target (Stmt),
                           Create_Value_Net (Res, Targ_Type),
                           Stmt);
      end;

      --  free.
      Free_Case_Element_Array (Case_El);
      Free_Choice_Data_Array (Choice_Data);
      Free_Annex_Array (Annex_Arr);
      Free_Alternative_Data_Array (Alts);
   end Synth_Selected_Signal_Assignment;

   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node;
                                           Infos : out Target_Info_Array)
   is
      pragma Assert (Infos'First = 1);
      Inter : Node;
      Inter_Type : Type_Acc;
      Assoc : Node;
      Assoc_Inter : Node;
      Actual : Node;
      Val : Value_Acc;
      Nbr_Inout : Natural;
   begin
      Set_Instance_Const (Subprg_Inst, True);

      Nbr_Inout := 0;
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         Inter_Type := Get_Value_Type (Caller_Inst, Get_Type (Inter));

         case Iir_Parameter_Modes (Get_Mode (Inter)) is
            when Iir_In_Mode =>
               case Get_Kind (Assoc) is
                  when Iir_Kind_Association_Element_Open =>
                     Actual := Get_Default_Value (Inter);
                     Val := Synth_Expression_With_Type
                       (Subprg_Inst, Actual, Inter_Type);
                  when Iir_Kind_Association_Element_By_Expression =>
                     Actual := Get_Actual (Assoc);
                     Val := Synth_Expression_With_Type
                       (Caller_Inst, Actual, Inter_Type);
                  when others =>
                     raise Internal_Error;
               end case;
            when Iir_Out_Mode | Iir_Inout_Mode =>
               Nbr_Inout := Nbr_Inout + 1;
               Actual := Get_Actual (Assoc);
               Infos (Nbr_Inout) := Synth_Target (Caller_Inst, Actual);
               declare
                  Info : Target_Info renames Infos (Nbr_Inout);
               begin
                  if Info.Kind /= Target_Simple then
                     raise Internal_Error;
                  end if;
                  case Iir_Kinds_Interface_Object_Declaration
                    (Get_Kind (Inter))
                  is
                     when Iir_Kind_Interface_Constant_Declaration =>
                        raise Internal_Error;
                     when Iir_Kind_Interface_Variable_Declaration =>
                        --  Always pass by value.
                        Val := Synth_Read_Memory
                          (Caller_Inst, Info.Obj, Info.Off, No_Net,
                           Info.Targ_Type, Assoc);
                     when Iir_Kind_Interface_Signal_Declaration =>
                        --  Always pass by reference (use an alias).
                        Val := Create_Value_Alias
                          (Info.Obj, Info.Off, Info.Targ_Type);
                     when Iir_Kind_Interface_File_Declaration =>
                        raise Internal_Error;
                  end case;
               end;
         end case;

         --  FIXME: conversion only for constants, reshape for all.
         Val := Synth_Subtype_Conversion (Val, Inter_Type, True, Assoc);

         if Get_Instance_Const (Subprg_Inst) and then not Is_Const (Val) then
            Set_Instance_Const (Subprg_Inst, False);
         end if;

         case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration =>
               --  Pass by reference.
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_Variable_Declaration =>
               --  FIXME: Arguments are passed by copy.
               Create_Object (Subprg_Inst, Inter, Val);
               raise Internal_Error;
            when Iir_Kind_Interface_Signal_Declaration =>
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_File_Declaration =>
               raise Internal_Error;
         end case;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Subprogram_Association;

   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node)
   is
      Infos : Target_Info_Array (1 .. 0);
      pragma Unreferenced (Infos);
   begin
      Synth_Subprogram_Association (Subprg_Inst, Caller_Inst,
                                    Inter_Chain, Assoc_Chain, Infos);
   end Synth_Subprogram_Association;

   procedure Synth_Subprogram_Back_Association
     (Subprg_Inst : Synth_Instance_Acc;
      Caller_Inst : Synth_Instance_Acc;
      Inter_Chain : Node;
      Assoc_Chain : Node;
      Infos : Target_Info_Array)
   is
      pragma Assert (Infos'First = 1);
      Inter : Node;
      Assoc : Node;
      Assoc_Inter : Node;
      Val : Value_Acc;
      Nbr_Inout : Natural;
   begin
      Nbr_Inout := 0;
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);

         case Iir_Parameter_Modes (Get_Mode (Inter)) is
            when Iir_In_Mode =>
               null;
            when Iir_Out_Mode | Iir_Inout_Mode =>
               Nbr_Inout := Nbr_Inout + 1;
               if False then
                  Val := Synth_Expression (Subprg_Inst, Inter);
                  Synth_Assignment
                    (Caller_Inst, Infos (Nbr_Inout), Val, Assoc);
               end if;
         end case;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
      pragma Assert (Nbr_Inout = Infos'Last);
   end Synth_Subprogram_Back_Association;

   function Synth_Label (Stmt : Node) return Sname
   is
      Label : constant Name_Id := Get_Label (Stmt);
   begin
      if Label = Null_Identifier then
         return No_Sname;
      else
         return New_Sname_User (Label);
      end if;
   end Synth_Label;

   procedure Count_Associations
     (Inter_Chain : Node; Assoc_Chain : Node; Nbr_Inout : out Natural)
   is
      Assoc : Node;
      Assoc_Inter : Node;
      Inter : Node;
   begin
      Nbr_Inout := 0;

      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);

         case Iir_Parameter_Modes (Get_Mode (Inter)) is
            when Iir_In_Mode =>
               null;
            when Iir_Out_Mode | Iir_Inout_Mode =>
               Nbr_Inout := Nbr_Inout + 1;
         end case;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Count_Associations;

   function Synth_Subprogram_Call
     (Syn_Inst : Synth_Instance_Acc; Call : Node) return Value_Acc
   is
      Imp  : constant Node := Get_Implementation (Call);
      Is_Func : constant Boolean := Is_Function_Declaration (Imp);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Bod : constant Node := Get_Subprogram_Body (Imp);
      Area_Mark : Areapools.Mark_Type;
      Res : Value_Acc;
      C : Seq_Context;
      Wire_Mark : Wire_Id;
      Subprg_Phi : Phi_Type;
      Nbr_Inout : Natural;
   begin
      Mark (Wire_Mark);
      Areapools.Mark (Area_Mark, Instance_Pool.all);
      C := (Inst => Make_Instance (Syn_Inst, Bod,
                                   New_Internal_Name (Build_Context)),
            Cur_Loop => null,
            W_En => Alloc_Wire (Wire_Variable, Imp),
            W_Ret => Alloc_Wire (Wire_Variable, Imp),
            W_Val => No_Wire_Id,
            Ret_Init => No_Net,
            Ret_Value => null,
            Ret_Typ => null,
            Nbr_Ret => 0);

      if Is_Func then
         C.W_Val := Alloc_Wire (Wire_Variable, Imp);
      end if;

      Count_Associations (Inter_Chain, Assoc_Chain, Nbr_Inout);

      declare
         Infos : Target_Info_Array (1 .. Nbr_Inout);
      begin
         Synth_Subprogram_Association
           (C.Inst, Syn_Inst, Inter_Chain, Assoc_Chain, Infos);

         Push_Phi;

         if Is_Func then
            --  Set a default value for the return.
            C.Ret_Typ := Get_Value_Type (Syn_Inst, Get_Return_Type (Imp));
            Set_Wire_Gate (C.W_Val,
                           Build_Signal (Build_Context,
                                         New_Internal_Name (Build_Context),
                                         C.Ret_Typ.W));
            C.Ret_Init := Build_Const_X (Build_Context, C.Ret_Typ.W);
            Phi_Assign (Build_Context, C.W_Val, C.Ret_Init, 0);
         end if;

         Set_Wire_Gate
           (C.W_En, Build_Signal (Build_Context,
                                  New_Internal_Name (Build_Context), 1));
         Phi_Assign (Build_Context, C.W_En, Get_Inst_Bit1 (Syn_Inst), 0);

         Set_Wire_Gate
           (C.W_Ret, Build_Signal (Build_Context,
                                   New_Internal_Name (Build_Context), 1));
         Phi_Assign (Build_Context, C.W_Ret, Get_Inst_Bit1 (Syn_Inst), 0);

         Decls.Synth_Declarations (C.Inst, Get_Declaration_Chain (Bod), True);

         Synth_Sequential_Statements
           (C, Get_Sequential_Statement_Chain (Bod));

         if Is_Func then
            if C.Nbr_Ret = 0 then
               raise Internal_Error;
            elsif C.Nbr_Ret = 1 and then Is_Const (C.Ret_Value) then
               Res := C.Ret_Value;
            else
               Res := Create_Value_Net
                 (Get_Current_Value (Build_Context, C.W_Val), C.Ret_Value.Typ);
            end if;
         else
            Res := null;
            Synth_Subprogram_Back_Association
              (C.Inst, Syn_Inst, Inter_Chain, Assoc_Chain, Infos);
         end if;

         Pop_Phi (Subprg_Phi);

         Decls.Finalize_Declarations
           (C.Inst, Get_Declaration_Chain (Bod), True);
         pragma Unreferenced (Infos);

         --  Propagate assignments.
         Propagate_Phi_Until_Mark (Get_Build (C.Inst), Subprg_Phi, Wire_Mark);
      end;

      --  Free wires.
      Free_Wire (C.W_En);
      Free_Wire (C.W_Ret);
      if Is_Func then
         Free_Wire (C.W_Val);
      end if;

      Free_Instance (C.Inst);
      Areapools.Release (Area_Mark, Instance_Pool.all);

      Release (Wire_Mark);

      return Res;
   end Synth_Subprogram_Call;

   procedure Synth_Procedure_Call (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Call : constant Node := Get_Procedure_Call (Stmt);
      Imp  : constant Node := Get_Implementation (Call);
      Res : Value_Acc;
   begin
      if Get_Implicit_Definition (Imp) in Iir_Predefined_Implicit then
         Error_Msg_Synth (+Stmt, "call to implicit %n is not supported", +Imp);
         return;
      elsif Get_Foreign_Flag (Imp) then
         Error_Msg_Synth (+Stmt, "call to foreign %n is not supported", +Imp);
         return;
      end if;

      Res := Synth_Subprogram_Call (Syn_Inst, Call);
      pragma Assert (Res = null);
   end Synth_Procedure_Call;

   function In_Range (Rng : Discrete_Range_Type; V : Int64) return Boolean is
   begin
      case Rng.Dir is
         when Iir_To =>
            return V >= Rng.Left and then V <= Rng.Right;
         when Iir_Downto =>
            return V <= Rng.Left and then V >= Rng.Right;
      end case;
   end In_Range;

   procedure Update_Index (Rng : Discrete_Range_Type; Idx : in out Int64) is
   begin
      case Rng.Dir is
         when Iir_To =>
            Idx := Idx + 1;
         when Iir_Downto =>
            Idx := Idx - 1;
      end case;
   end Update_Index;

   procedure Loop_Control_Init (C : Seq_Context; Stmt : Node)
   is
      Lc : constant Loop_Context_Acc := C.Cur_Loop;

   begin
      Mark (C.Cur_Loop.Wire_Mark);

      if (Lc.Prev_Loop /= null and then Lc.Prev_Loop.Need_Quit) then
         Lc.W_Quit := Alloc_Wire (Wire_Variable, Lc.Loop_Stmt);
         Set_Wire_Gate
           (Lc.W_Quit, Build_Signal (Get_Build (C.Inst),
                                     New_Internal_Name (Build_Context), 1));
         Phi_Assign (Get_Build (C.Inst), Lc.W_Quit, Get_Inst_Bit1 (C.Inst), 0);
      end if;

      if Get_Exit_Flag (Stmt) or else Get_Next_Flag (Stmt) then
         Lc.Saved_En := Get_Current_Value (null, C.W_En);
         Lc.Need_Quit := True;
      end if;

      if Get_Exit_Flag (Stmt) then
         --  Exit statement for this loop.
         Lc.W_Exit := Alloc_Wire (Wire_Variable, Lc.Loop_Stmt);
         Set_Wire_Gate
           (Lc.W_Exit, Build_Signal (Get_Build (C.Inst),
                                     New_Internal_Name (Build_Context), 1));
         Phi_Assign (Get_Build (C.Inst), Lc.W_Exit, Get_Inst_Bit1 (C.Inst), 0);
      end if;
   end Loop_Control_Init;

   function Loop_Control_And (C : Seq_Context; L, R : Net) return Net
   is
      B1 : constant Net := Get_Inst_Bit1 (C.Inst);
   begin
      --  Optimize common cases.
      if L = B1 then
         return R;
      elsif R = B1 then
         return L;
      else
         return Build_Dyadic (Get_Build (C.Inst), Netlists.Gates.Id_And, L, R);
      end if;
   end Loop_Control_And;

   procedure Loop_Control_Update (C : Seq_Context)
   is
      Lc : constant Loop_Context_Acc := C.Cur_Loop;
      Res : Net;
   begin
      --  Execution continue iff:
      --  1. Loop was enabled (Lc.Saved_En)
      Res := Lc.Saved_En;
      if Res = No_Net then
         --  No loop control.
         return;
      end if;

      --  2. No return (C.W_Ret)
      if C.W_Ret /= No_Wire_Id then
         Res := Loop_Control_And (C, Res, Get_Current_Value (null, C.W_Ret));
      end if;

      --  3. No exit.
      if Lc.W_Exit /= No_Wire_Id then
         Res := Loop_Control_And (C, Res, Get_Current_Value (null, Lc.W_Exit));
      end if;

      --  4. No quit.
      if Lc.W_Quit /= No_Wire_Id then
         Res := Loop_Control_And (C, Res, Get_Current_Value (null, Lc.W_Quit));
      end if;

      Phi_Assign (Get_Build (C.Inst), C.W_En, Res, 0);
   end Loop_Control_Update;

   procedure Loop_Control_Finish (C : Seq_Context)
   is
      Lc : constant Loop_Context_Acc := C.Cur_Loop;
      Res : Net;
   begin
      --  Execute continue iff:
      --  1. Loop was enabled (Lc.Saved_En)
      Res := Lc.Saved_En;
      if Res = No_Net then
         --  No loop control.
         return;
      end if;

      --  2. No return (C.W_Ret)
      if C.W_Ret /= No_Wire_Id then
         Res := Loop_Control_And (C, Res, Get_Current_Value (null, C.W_Ret));
      end if;

      --  3. No quit (C.W_Quit)
      if Lc.W_Quit /= No_Wire_Id then
         Res := Loop_Control_And (C, Res, Get_Current_Value (null, Lc.W_Quit));
      end if;

      Phi_Discard_Wires (Lc.W_Quit, Lc.W_Exit);

      if Lc.W_Quit /= No_Wire_Id then
         Free_Wire (Lc.W_Quit);
      end if;

      if Lc.W_Exit /= No_Wire_Id then
         Free_Wire (Lc.W_Exit);
      end if;

      Release (C.Cur_Loop.Wire_Mark);

      Phi_Assign (Get_Build (C.Inst), C.W_En, Res, 0);
   end Loop_Control_Finish;

   procedure Synth_Exit_Next_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Cond : constant Node := Get_Condition (Stmt);
      Is_Exit : constant Boolean := Get_Kind (Stmt) = Iir_Kind_Exit_Statement;
      Loop_Label : Node;
      Lc : Loop_Context_Acc;
      Cond_Val : Value_Acc;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin

      if Cond /= Null_Node then
         Cond_Val := Synth_Expression (C.Inst, Cond);
         Push_Phi;
      end if;

      --  Execution is suspended.
      Phi_Assign (Get_Build (C.Inst), C.W_En, Get_Inst_Bit0 (C.Inst), 0);

      Lc := C.Cur_Loop;

      Loop_Label := Get_Loop_Label (Stmt);
      if Loop_Label = Null_Node then
         Loop_Label := Lc.Loop_Stmt;
      else
         Loop_Label := Get_Named_Entity (Loop_Label);
      end if;

      loop
         if Lc.Loop_Stmt = Loop_Label then
            if Is_Exit then
               Phi_Assign (Get_Build (C.Inst), Lc.W_Exit,
                           Get_Inst_Bit0 (C.Inst), 0);
            end if;
            exit;
         else
            Phi_Assign (Get_Build (C.Inst), Lc.W_Quit,
                        Get_Inst_Bit0 (C.Inst), 0);
         end if;
         Lc := Lc.Prev_Loop;
      end loop;

      if Cond /= Null_Node then
         Pop_Phi (Phi_True);
         Push_Phi;
         Pop_Phi (Phi_False);
         Merge_Phis (Build_Context,
                     Get_Net (Cond_Val), Phi_True, Phi_False, Stmt);
      end if;
   end Synth_Exit_Next_Statement;

   procedure Synth_For_Loop_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
      It_Rng : Type_Acc;
      Val : Value_Acc;
      Lc : aliased Loop_Context;
   begin
      Lc := (Prev_Loop => C.Cur_Loop,
             Loop_Stmt => Stmt,
             Need_Quit => False,
             Saved_En => No_Net,
             W_Exit => No_Wire_Id,
             W_Quit => No_Wire_Id,
             Wire_Mark => No_Wire_Id);
      C.Cur_Loop := Lc'Unrestricted_Access;

      Loop_Control_Init (C, Stmt);

      if It_Type /= Null_Node then
         Synth_Subtype_Indication (C.Inst, It_Type);
      end if;

      --  Initial value.
      It_Rng := Get_Value_Type (C.Inst, Get_Type (Iterator));
      Val := Create_Value_Discrete (It_Rng.Drange.Left, It_Rng);
      Create_Object (C.Inst, Iterator, Val);

      while In_Range (It_Rng.Drange, Val.Scal) loop
         Synth_Sequential_Statements (C, Stmts);
         Update_Index (It_Rng.Drange, Val.Scal);
         Loop_Control_Update (C);
      end loop;
      Loop_Control_Finish (C);

      Destroy_Object (C.Inst, Iterator);
      if It_Type /= Null_Node then
         Destroy_Object (C.Inst, It_Type);
      end if;

      C.Cur_Loop := Lc.Prev_Loop;
   end Synth_For_Loop_Statement;

   procedure Synth_While_Loop_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      Cond : constant Node := Get_Condition (Stmt);
      Val : Value_Acc;
      Lc : aliased Loop_Context;
   begin
      Lc := (Prev_Loop => C.Cur_Loop,
             Loop_Stmt => Stmt,
             Need_Quit => False,
             Saved_En => No_Net,
             W_Exit => No_Wire_Id,
             W_Quit => No_Wire_Id,
             Wire_Mark => No_Wire_Id);
      C.Cur_Loop := Lc'Unrestricted_Access;

      Loop_Control_Init (C, Stmt);

      loop
         if Cond /= Null_Node then
            Val := Synth_Expression_With_Type (C.Inst, Cond, Boolean_Type);
            if not Is_Const (Val) then
               Error_Msg_Synth (+Cond, "loop condition must be static");
               exit;
            end if;
            exit when Val.Scal = 0;
         end if;

         Synth_Sequential_Statements (C, Stmts);

         Loop_Control_Update (C);
      end loop;
      Loop_Control_Finish (C);

      C.Cur_Loop := Lc.Prev_Loop;
   end Synth_While_Loop_Statement;

   procedure Synth_Return_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Val : Value_Acc;
      Expr : constant Node := Get_Expression (Stmt);
   begin
      if Expr /= Null_Node then
         --  Return in function.
         Val := Synth_Expression_With_Type (C.Inst, Expr, C.Ret_Typ);
         Val := Synth_Subtype_Conversion (Val, C.Ret_Typ, False, Stmt);

         if C.Nbr_Ret = 0 then
            C.Ret_Value := Val;
            if not Is_Bounded_Type (C.Ret_Typ) then
               --  The function was declared with an unconstrained return type.
               --  Now that a value has been returned, we know the subtype of
               --  the returned values.  So adjust it.
               --  All the returned values must have the same length.
               C.Ret_Typ := Val.Typ;
               Set_Width (Get_Wire_Gate (C.W_Val), C.Ret_Typ.W);
               Set_Width (C.Ret_Init, C.Ret_Typ.W);
            end if;
         end if;
         Phi_Assign (Get_Build (C.Inst), C.W_Val, Get_Net (Val), 0);
      end if;

      --  The subprogram has returned.  Do not execute further statements.
      Phi_Assign (Get_Build (C.Inst), C.W_En, Get_Inst_Bit0 (C.Inst), 0);

      if C.W_Ret /= No_Wire_Id then
         Phi_Assign (Get_Build (C.Inst), C.W_Ret, Get_Inst_Bit0 (C.Inst), 0);
      end if;

      C.Nbr_Ret := C.Nbr_Ret + 1;
   end Synth_Return_Statement;

   procedure Synth_Sequential_Statements
     (C : in out Seq_Context; Stmts : Node)
   is
      Stmt : Node;
      Phi_T, Phi_F : Phi_Type;
      Has_Phi : Boolean;
      En : Net;
   begin
      Stmt := Stmts;
      while Is_Valid (Stmt) loop
         En := Get_Current_Value (null, C.W_En);
         pragma Assert (En /= Get_Inst_Bit0 (C.Inst));
         Has_Phi := En /= Get_Inst_Bit1 (C.Inst);
         if Has_Phi then
            Push_Phi;
         end if;
         case Get_Kind (Stmt) is
            when Iir_Kind_If_Statement =>
               Synth_If_Statement (C, Stmt);
            when Iir_Kind_Simple_Signal_Assignment_Statement =>
               Synth_Simple_Signal_Assignment (C.Inst, Stmt);
            when Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Synth_Conditional_Signal_Assignment (C.Inst, Stmt);
            when Iir_Kind_Variable_Assignment_Statement =>
               Synth_Variable_Assignment (C.Inst, Stmt);
            when Iir_Kind_Conditional_Variable_Assignment_Statement =>
               Synth_Conditional_Variable_Assignment (C.Inst, Stmt);
            when Iir_Kind_Case_Statement =>
               Synth_Case_Statement (C, Stmt);
            when Iir_Kind_For_Loop_Statement =>
               Synth_For_Loop_Statement (C, Stmt);
            when Iir_Kind_While_Loop_Statement =>
               Synth_While_Loop_Statement (C, Stmt);
            when Iir_Kind_Null_Statement =>
               --  Easy
               null;
            when Iir_Kind_Return_Statement =>
               Synth_Return_Statement (C, Stmt);
            when Iir_Kind_Procedure_Call_Statement =>
               Synth_Procedure_Call (C.Inst, Stmt);
            when Iir_Kind_Report_Statement
              | Iir_Kind_Assertion_Statement =>
               --  TODO ?
               null;
            when Iir_Kind_Exit_Statement
              | Iir_Kind_Next_Statement =>
               Synth_Exit_Next_Statement (C, Stmt);
            when others =>
               Error_Kind ("synth_sequential_statements", Stmt);
         end case;
         if Has_Phi then
            Pop_Phi (Phi_T);
            Push_Phi;
            Pop_Phi (Phi_F);
            Merge_Phis (Build_Context,
                        Get_Current_Value (Build_Context, C.W_En),
                        Phi_T, Phi_F, Stmt);
         end if;
         if Get_Current_Value (null, C.W_En) = Get_Inst_Bit0 (C.Inst) then
            return;
         end if;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Synth_Sequential_Statements;

   Proc_Pool : aliased Areapools.Areapool;

   --  Synthesis of statements of a non-sensitized process.
   procedure Synth_Process_Sequential_Statements
     (C : in out Seq_Context; Proc : Node)
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
      if Cond = Null_Node then
         Error_Msg_Synth (+Stmt, "expect wait condition");
         return;
      end if;
      Cond_Val := Synth_Expression (C.Inst, Cond);

      Push_Phi;
      Synth_Sequential_Statements (C, Get_Chain (Stmt));
      Pop_Phi (Phi_True);
      Push_Phi;
      Pop_Phi (Phi_False);

      Merge_Phis (Build_Context, Get_Net (Cond_Val),
                  Phi_True, Phi_False, Stmt);
   end Synth_Process_Sequential_Statements;

   procedure Synth_Process_Statement
     (Syn_Inst : Synth_Instance_Acc; Proc : Node)
   is
      use Areapools;
      Label : constant Name_Id := Get_Identifier (Proc);
      Decls_Chain : constant Node := Get_Declaration_Chain (Proc);
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      M : Areapools.Mark_Type;
      C_Sname : Sname;
      C : Seq_Context;
   begin
      if Label = Null_Identifier then
         C_Sname := New_Internal_Name (Build_Context, Get_Sname (Syn_Inst));
      else
         C_Sname := New_Sname (Get_Sname (Syn_Inst), Label);
      end if;
      C := (Inst => Make_Instance (Syn_Inst, Proc, C_Sname),
            Cur_Loop => null,
            W_En => Alloc_Wire (Wire_Variable, Proc),
            W_Ret => No_Wire_Id,
            W_Val => No_Wire_Id,
            Ret_Init => No_Net,
            Ret_Value => null,
            Ret_Typ => null,
            Nbr_Ret => 0);


      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      if Is_Valid (Decls_Chain) then
         Synth_Declarations (C.Inst, Decls_Chain);
      end if;

      Set_Wire_Gate (C.W_En, Build_Signal (Build_Context,
                                           New_Internal_Name (Build_Context),
                                           1));
      Phi_Assign (Build_Context, C.W_En, Get_Inst_Bit1 (Syn_Inst), 0);

      case Iir_Kinds_Process_Statement (Get_Kind (Proc)) is
         when Iir_Kind_Sensitized_Process_Statement =>
            Synth_Sequential_Statements
              (C, Get_Sequential_Statement_Chain (Proc));
            --  FIXME: check sensitivity list.
         when Iir_Kind_Process_Statement =>
            Synth_Process_Sequential_Statements (C, Proc);
      end case;

      --  FIXME: free W_En ?

      Free_Instance (C.Inst);
      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;
   end Synth_Process_Statement;

   function Synth_User_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc is
   begin
      --  Is it a call to an ieee function ?
      declare
         Imp  : constant Node := Get_Implementation (Expr);
         Pkg : constant Node := Get_Parent (Imp);
         Unit : Node;
         Lib : Node;
      begin
         if Get_Kind (Pkg) = Iir_Kind_Package_Declaration then
            Unit := Get_Parent (Pkg);
            if Get_Kind (Unit) = Iir_Kind_Design_Unit then
               Lib := Get_Library (Get_Design_File (Unit));
               if Get_Identifier (Lib) = Std_Names.Name_Ieee then
                  Error_Msg_Synth
                    (+Expr, "unhandled call to an ieee function");
                  raise Internal_Error;
               end if;
            end if;
         end if;
      end;

      return Synth_Subprogram_Call (Syn_Inst, Expr);
   end Synth_User_Function_Call;

   procedure Synth_Concurrent_Assertion_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Cond : constant Node := Get_Assertion_Condition (Stmt);
      Val : Value_Acc;
      Inst : Instance;
   begin
      Val := Synth_Expression (Syn_Inst, Cond);
      if Is_Const (Val) then
         if Val.Scal /= 1 then
            raise Internal_Error;
         end if;
         return;
      end if;
      Inst := Build_Assert (Build_Context, Synth_Label (Stmt), Get_Net (Val));
      Set_Location (Inst, Get_Location (Stmt));
   end Synth_Concurrent_Assertion_Statement;

   procedure Synth_Block_Statement (Syn_Inst : Synth_Instance_Acc; Blk : Node)
   is
      use Areapools;
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      Blk_Inst : Synth_Instance_Acc;
      Blk_Sname : Sname;
      M : Areapools.Mark_Type;
   begin
      --  No support for guard or header.
      if Get_Block_Header (Blk) /= Null_Node
        or else Get_Guard_Decl (Blk) /= Null_Node
      then
         raise Internal_Error;
      end if;

      Blk_Sname := New_Sname (Get_Sname (Syn_Inst), Get_Identifier (Blk));
      Blk_Inst := Make_Instance (Syn_Inst, Blk, Blk_Sname);
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      Synth_Declarations (Blk_Inst, Get_Declaration_Chain (Blk));
      Synth_Concurrent_Statements
        (Blk_Inst, Get_Concurrent_Statement_Chain (Blk));

      Free_Instance (Blk_Inst);
      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;
   end Synth_Block_Statement;

   function Synth_PSL_Expression
     (Syn_Inst : Synth_Instance_Acc; Expr : PSL.Types.PSL_Node) return Net
   is
      use PSL.Types;
      use PSL.Nodes;
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr =>
            declare
               E : constant Vhdl.Types.Vhdl_Node := Get_HDL_Node (Expr);
            begin
               return Get_Net (Synth_Expression (Syn_Inst, E));
            end;
         when N_Not_Bool =>
            return Build_Monadic
              (Build_Context, Netlists.Gates.Id_Not,
               Synth_PSL_Expression (Syn_Inst, Get_Boolean (Expr)));
         when N_And_Bool =>
            declare
               L : constant PSL_Node := Get_Left (Expr);
               R : constant PSL_Node := Get_Right (Expr);
               Edge : Net;
            begin
               --  Handle edge (as it can be in default clock).
               if Get_Kind (L) = N_HDL_Expr and then Get_Kind (R) = N_HDL_Expr
               then
                  Edge := Synth_Clock_Edge
                    (Syn_Inst, Get_HDL_Node (L), Get_HDL_Node (R));
                  if Edge /= No_Net then
                     return Edge;
                  end if;
               end if;
               return Build_Dyadic
                 (Build_Context, Netlists.Gates.Id_And,
                  Synth_PSL_Expression (Syn_Inst, L),
                  Synth_PSL_Expression (Syn_Inst, R));
            end;
         when N_Or_Bool =>
            return Build_Dyadic
              (Build_Context, Netlists.Gates.Id_Or,
               Synth_PSL_Expression (Syn_Inst, Get_Left (Expr)),
               Synth_PSL_Expression (Syn_Inst, Get_Right (Expr)));
         when N_True =>
            return Build_Const_UB32 (Build_Context, 1, 1);
         when N_False =>
            return Build_Const_UB32 (Build_Context, 0, 1);
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

   function Synth_Psl_Sequence_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node) return Net
   is
      use Netlists.Gates;
      Nbr_States : constant Int32 := Get_PSL_Nbr_States (Stmt);
      Init : Net;
      Clk : Net;
      Clk_Inst : Instance;
      States : Net;
      Next_States : Net;
   begin
      --  create init net, clock net
      pragma Assert (Nbr_States <= 32);
      Init := Build_Const_UB32 (Build_Context, 1, Uns32 (Nbr_States));
      Clk := Synth_PSL_Expression (Syn_Inst, Get_PSL_Clock (Stmt));

      --  Check the clock is an edge and extract it.
      Clk_Inst := Get_Net_Parent (Clk);
      if Get_Id (Clk_Inst) /= Id_Edge then
         Error_Msg_Synth (+Stmt, "clock is not an edge");
         return No_Net;
      end if;

      Clk := Get_Input_Net (Clk_Inst, 0);

      --  build idff
      States := Build_Idff (Build_Context, Clk, No_Net, Init);

      --  create update nets
      --  For each state: if set, evaluate all outgoing edges.
      Next_States :=
        Synth_Psl_NFA (Syn_Inst, Get_PSL_NFA (Stmt), Nbr_States, States);
      Connect (Get_Input (Get_Net_Parent (States), 1), Next_States);

      --  The NFA state is correct as long as there is a 1.
      return Build_Reduce (Build_Context,
                           Netlists.Gates.Id_Red_Or, Next_States);
   end Synth_Psl_Sequence_Directive;

   procedure Synth_Psl_Restrict_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Res : Net;
      Inst : Instance;
   begin
      --  Build assume gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Res := Synth_Psl_Sequence_Directive (Syn_Inst, Stmt);
      if Res /= No_Net then
         Inst := Build_Assume (Build_Context, Synth_Label (Stmt), Res);
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Restrict_Directive;

   procedure Synth_Psl_Cover_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Res : Net;
      Inst : Instance;
   begin
      --  Build cover gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Res := Synth_Psl_Sequence_Directive (Syn_Inst, Stmt);
      if Res /= No_Net then
         Inst := Build_Cover (Build_Context, Synth_Label (Stmt), Res);
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Cover_Directive;

   function Synth_Psl_Property_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node) return Net
   is
      use PSL.Types;
      use PSL.NFAs;
      use Netlists.Gates;
      NFA : constant PSL_NFA := Get_PSL_NFA (Stmt);
      Nbr_States : constant Int32 := Get_PSL_Nbr_States (Stmt);
      Init : Net;
      Clk : Net;
      Clk_Inst : Instance;
      States : Net;
      Next_States : Net;
   begin
      --  create init net, clock net
      pragma Assert (Nbr_States <= 32);
      Init := Build_Const_UB32 (Build_Context, 1, Uns32 (Nbr_States));
      Clk := Synth_PSL_Expression (Syn_Inst, Get_PSL_Clock (Stmt));

      --  Check the clock is an edge and extract it.
      Clk_Inst := Get_Net_Parent (Clk);
      if Get_Id (Clk_Inst) /= Id_Edge then
         Error_Msg_Synth (+Stmt, "clock is not an edge");
         return No_Net;
      end if;

      Clk := Get_Input_Net (Clk_Inst, 0);

      --  build idff
      States := Build_Idff (Build_Context, Clk, No_Net, Init);

      --  create update nets
      --  For each state: if set, evaluate all outgoing edges.
      Next_States := Synth_Psl_NFA (Syn_Inst, NFA, Nbr_States, States);
      Connect (Get_Input (Get_Net_Parent (States), 1), Next_States);

      return Build_Monadic
        (Build_Context, Netlists.Gates.Id_Not,
         Build_Extract_Bit
           (Build_Context, Next_States,
            Uns32 (Get_State_Label (Get_Final_State (NFA)))));
   end Synth_Psl_Property_Directive;

   procedure Synth_Psl_Assume_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Res : Net;
      Inst : Instance;
   begin
      --  Build assume gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Res := Synth_Psl_Property_Directive (Syn_Inst, Stmt);
      if Res /= No_Net then
         Inst := Build_Assume (Build_Context, Synth_Label (Stmt), Res);
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Assume_Directive;

   procedure Synth_Psl_Assert_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Res : Net;
      Inst : Instance;
   begin
      --  Build assert gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assert on States, then the first cycle is ignored).
      Res := Synth_Psl_Property_Directive (Syn_Inst, Stmt);
      if Res /= No_Net then
         Inst := Build_Assert (Build_Context, Synth_Label (Stmt), Res);
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Assert_Directive;

   procedure Synth_Generate_Statement_Body (Syn_Inst : Synth_Instance_Acc;
                                            Bod : Node;
                                            Name : Sname;
                                            Iterator : Node := Null_Node;
                                            Iterator_Val : Value_Acc := null)
   is
      use Areapools;
      Decls_Chain : constant Node := Get_Declaration_Chain (Bod);
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      Bod_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
   begin
      Bod_Inst := Make_Instance (Syn_Inst, Bod, Name);
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      if Iterator /= Null_Node then
         --  Add the iterator (for for-generate).
         Create_Object (Bod_Inst, Iterator, Iterator_Val);
      end if;

      Synth_Declarations (Bod_Inst, Decls_Chain);

      Synth_Concurrent_Statements
        (Bod_Inst, Get_Concurrent_Statement_Chain (Bod));

      Free_Instance (Bod_Inst);
      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;
   end Synth_Generate_Statement_Body;

   procedure Synth_For_Generate_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      Bod : constant Node := Get_Generate_Statement_Body (Stmt);
      Configs : constant Node := Get_Generate_Block_Configuration (Bod);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
      Config : Node;
      It_Rng : Type_Acc;
      Val : Value_Acc;
      Name : Sname;
      Lname : Sname;
   begin
      if It_Type /= Null_Node then
         Synth_Subtype_Indication (Syn_Inst, It_Type);
      end if;

      --  Initial value.
      It_Rng := Get_Value_Type (Syn_Inst, Get_Type (Iterator));
      Val := Create_Value_Discrete (It_Rng.Drange.Left, It_Rng);

      Name := New_Sname (Get_Sname (Syn_Inst), Get_Identifier (Stmt));

      while In_Range (It_Rng.Drange, Val.Scal) loop
         --  Find and apply the config block.
         declare
            Spec : Node;
         begin
            Config := Configs;
            while Config /= Null_Node loop
               Spec := Get_Block_Specification (Config);
               case Get_Kind (Spec) is
                  when Iir_Kind_Simple_Name =>
                     exit;
                  when others =>
                     Error_Kind ("synth_for_generate_statement", Spec);
               end case;
               Config := Get_Prev_Block_Configuration (Config);
            end loop;
            if Config = Null_Node then
               raise Internal_Error;
            end if;
            Apply_Block_Configuration (Config, Bod);
         end;

         --  FIXME: get position ?
         Lname := New_Sname_Version (Name, Uns32 (Val.Scal));

         Synth_Generate_Statement_Body (Syn_Inst, Bod, Lname, Iterator, Val);
         Update_Index (It_Rng.Drange, Val.Scal);
      end loop;
   end Synth_For_Generate_Statement;

   procedure Synth_Concurrent_Statements
     (Syn_Inst : Synth_Instance_Acc; Stmts : Node)
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Is_Valid (Stmt) loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
               Push_Phi;
               Synth_Simple_Signal_Assignment (Syn_Inst, Stmt);
               Pop_And_Merge_Phi (Build_Context, Stmt);
            when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
               Push_Phi;
               Synth_Conditional_Signal_Assignment (Syn_Inst, Stmt);
               Pop_And_Merge_Phi (Build_Context, Stmt);
            when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
               Push_Phi;
               Synth_Selected_Signal_Assignment (Syn_Inst, Stmt);
               Pop_And_Merge_Phi (Build_Context, Stmt);
            when Iir_Kind_Concurrent_Procedure_Call_Statement =>
               Push_Phi;
               Synth_Procedure_Call (Syn_Inst, Stmt);
               Pop_And_Merge_Phi (Build_Context, Stmt);
            when Iir_Kinds_Process_Statement =>
               Push_Phi;
               Synth_Process_Statement (Syn_Inst, Stmt);
               Pop_And_Merge_Phi (Build_Context, Stmt);
            when Iir_Kind_If_Generate_Statement =>
               declare
                  Gen : Node;
                  Bod : Node;
                  Cond : Value_Acc;
                  Name : Sname;
               begin
                  Gen := Stmt;
                  Name := New_Sname (Get_Sname (Syn_Inst),
                                     Get_Identifier (Stmt));
                  loop
                     Cond := Synth_Expression (Syn_Inst, Get_Condition (Gen));
                     pragma Assert (Cond.Kind = Value_Discrete);
                     if Cond.Scal = 1 then
                        Bod := Get_Generate_Statement_Body (Gen);
                        Synth_Generate_Statement_Body (Syn_Inst, Bod, Name);
                        exit;
                     end if;
                     Gen := Get_Generate_Else_Clause (Gen);
                     exit when Gen = Null_Node;
                  end loop;
               end;
            when Iir_Kind_For_Generate_Statement =>
               Synth_For_Generate_Statement (Syn_Inst, Stmt);
            when Iir_Kind_Component_Instantiation_Statement =>
               Push_Phi;
               if Is_Component_Instantiation (Stmt) then
                  declare
                     Comp_Config : constant Node :=
                       Get_Component_Configuration (Stmt);
                  begin
                     if Get_Binding_Indication (Comp_Config) = Null_Node then
                        --  Not bound.
                        Synth_Blackbox_Instantiation_Statement
                          (Syn_Inst, Stmt);
                     else
                        Synth_Component_Instantiation_Statement
                          (Syn_Inst, Stmt);
                     end if;
                  end;
               else
                  Synth_Design_Instantiation_Statement (Syn_Inst, Stmt);
               end if;
               Pop_And_Merge_Phi (Build_Context, Stmt);
            when Iir_Kind_Block_Statement =>
               Synth_Block_Statement (Syn_Inst, Stmt);
            when Iir_Kind_Psl_Default_Clock =>
               null;
            when Iir_Kind_Psl_Restrict_Directive =>
               Synth_Psl_Restrict_Directive (Syn_Inst, Stmt);
            when Iir_Kind_Psl_Assume_Directive =>
               Synth_Psl_Assume_Directive (Syn_Inst, Stmt);
            when Iir_Kind_Psl_Cover_Directive =>
               Synth_Psl_Cover_Directive (Syn_Inst, Stmt);
            when Iir_Kind_Psl_Assert_Directive =>
               Synth_Psl_Assert_Directive (Syn_Inst, Stmt);
            when Iir_Kind_Concurrent_Assertion_Statement =>
               --  Passive statement.
               Synth_Concurrent_Assertion_Statement (Syn_Inst, Stmt);
            when others =>
               Error_Kind ("synth_statements", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Synth_Concurrent_Statements;

   procedure Synth_Verification_Unit
     (Syn_Inst : Synth_Instance_Acc; Unit : Node)
   is
      Item : Node;
   begin
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock =>
               null;
            when Iir_Kind_Psl_Assert_Directive =>
               Synth_Psl_Assert_Directive (Syn_Inst, Item);
            when Iir_Kind_Psl_Assume_Directive =>
               Synth_Psl_Assume_Directive (Syn_Inst, Item);
            when Iir_Kind_Psl_Cover_Directive =>
               Synth_Psl_Cover_Directive (Syn_Inst, Item);
            when others =>
               Error_Kind ("synth_verification_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Synth_Verification_Unit;
end Synth.Stmts;
