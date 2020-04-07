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
with Name_Table;
with Std_Names;
with Errorout; use Errorout;
with Files_Map;
with Simple_IO;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Types;
with Vhdl.Sem_Expr;
with Vhdl.Sem_Inst;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
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
with Synth.Static_Proc;
with Synth.Heap;
with Synth.Flags;
with Synth.Debugger;

with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Butils; use Netlists.Butils;

package body Synth.Stmts is
   procedure Synth_Sequential_Statements
     (C : in out Seq_Context; Stmts : Node);

   procedure Set_Location (N : Net; Loc : Node)
     renames Synth.Source.Set_Location;

   function Synth_Waveform (Syn_Inst : Synth_Instance_Acc;
                            Wf : Node;
                            Targ_Type : Type_Acc) return Valtyp is
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
                           Val : Valtyp;
                           Offset : Uns32;
                           Loc : Source.Syn_Src)
   is
      Cval : Valtyp;
      N : Net;
   begin
      Cval := Synth_Subtype_Conversion (Val, Typ, False, Loc);
      if Cval = No_Valtyp then
         --  In case of error.
         return;
      end if;
      N := Get_Net (Cval);
      Phi_Assign (Build_Context, Wid, N, Offset);
   end Synth_Assign;

   procedure Synth_Assignment_Prefix (Syn_Inst : Synth_Instance_Acc;
                                      Pfx : Node;
                                      Dest_Base : out Valtyp;
                                      Dest_Typ : out Type_Acc;
                                      Dest_Off : out Value_Offsets;
                                      Dest_Voff : out Net;
                                      Dest_Rdwd : out Width) is
   begin
      case Get_Kind (Pfx) is
         when Iir_Kind_Simple_Name =>
            Synth_Assignment_Prefix (Syn_Inst, Get_Named_Entity (Pfx),
                                     Dest_Base, Dest_Typ,
                                     Dest_Off, Dest_Voff, Dest_Rdwd);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Anonymous_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            declare
               Targ : constant Valtyp := Get_Value (Syn_Inst, Pfx);
            begin
               Dest_Voff := No_Net;
               Dest_Rdwd := 0;
               Dest_Typ := Targ.Typ;

               if Targ.Val.Kind = Value_Alias then
                  --  Replace alias by the aliased name.
                  Dest_Base := (Targ.Typ, Targ.Val.A_Obj);
                  Dest_Off := Targ.Val.A_Off;
               else
                  Dest_Base := Targ;
                  Dest_Off := (0, 0);
               end if;
            end;
         when Iir_Kind_Function_Call =>
            Dest_Base := Synth_Expression (Syn_Inst, Pfx);
            Dest_Typ := Dest_Base.Typ;
            Dest_Off := (0, 0);
            Dest_Voff := No_Net;
            Dest_Rdwd := 0;
         when Iir_Kind_Indexed_Name =>
            declare
               Voff : Net;
               Off : Value_Offsets;
               Dest_W : Width;
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx),
                  Dest_Base, Dest_Typ, Dest_Off, Dest_Voff, Dest_Rdwd);
               Strip_Const (Dest_Base);
               Dest_W := Dest_Base.Typ.W;
               Synth_Indexed_Name (Syn_Inst, Pfx, Dest_Typ, Voff, Off);

               Dest_Typ := Get_Array_Element (Dest_Typ);

               Dest_Off.Net_Off := Dest_Off.Net_Off + Off.Net_Off;
               Dest_Off.Mem_Off := Dest_Off.Mem_Off + Off.Mem_Off;

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
                  Dest_Base, Dest_Typ, Dest_Off, Dest_Voff, Dest_Rdwd);
               Dest_Off.Net_Off :=
                 Dest_Off.Net_Off + Dest_Typ.Rec.E (Idx + 1).Boff;
               Dest_Off.Mem_Off :=
                 Dest_Off.Mem_Off + Dest_Typ.Rec.E (Idx + 1).Moff;

               Dest_Typ := Dest_Typ.Rec.E (Idx + 1).Typ;
            end;

         when Iir_Kind_Slice_Name =>
            declare
               Pfx_Bnd : Bound_Type;
               El_Typ : Type_Acc;
               Res_Bnd : Bound_Type;
               Sl_Voff : Net;
               Sl_Off : Value_Offsets;
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx),
                  Dest_Base, Dest_Typ, Dest_Off, Dest_Voff, Dest_Rdwd);
               Strip_Const (Dest_Base);

               Get_Onedimensional_Array_Bounds (Dest_Typ, Pfx_Bnd, El_Typ);
               Synth_Slice_Suffix (Syn_Inst, Pfx, Pfx_Bnd, El_Typ,
                                   Res_Bnd, Sl_Voff, Sl_Off);

               Dest_Off.Net_Off := Dest_Off.Net_Off + Sl_Off.Net_Off;
               Dest_Off.Mem_Off := Dest_Off.Mem_Off + Sl_Off.Mem_Off;

               if Sl_Voff /= No_Net then
                  --  Variable slice.
                  if Dest_Voff /= No_Net then
                     Dest_Voff := Build_Addidx
                       (Get_Build (Syn_Inst), Dest_Voff, Sl_Voff);
                  else
                     Dest_Rdwd := Dest_Base.Typ.W;
                     Dest_Voff := Sl_Voff;
                  end if;
                  Dest_Typ := Create_Slice_Type (Res_Bnd.Len, El_Typ);
               else
                  --  Fixed slice.
                  Dest_Typ := Create_Onedimensional_Array_Subtype
                    (Dest_Typ, Res_Bnd);
               end if;
            end;

         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            Synth_Assignment_Prefix
              (Syn_Inst, Get_Prefix (Pfx),
               Dest_Base, Dest_Typ, Dest_Off, Dest_Voff, Dest_Rdwd);
            if Dest_Off /= (0, 0) and then Dest_Voff /= No_Net then
               raise Internal_Error;
            end if;
            Dest_Base := Heap.Synth_Dereference (Read_Access (Dest_Base));
            Dest_Typ := Dest_Base.Typ;

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
            Mem_Voff : Net;
            --  Offset of the memory in the wire (usually 0).
            Mem_Moff : Uns32;
            --  Width of the whole memory
            Mem_Mwidth : Width;
            --  Offset of the data to be accessed from the memory.
            Mem_Doff : Uns32;
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
                 Get_Subtype_Object (Syn_Inst, Get_Base_Type (Targ_Type));
               case Base_Typ.Kind is
                  when Type_Unbounded_Vector =>
                     Bnd := Expr.Synth_Array_Bounds (Syn_Inst, Targ_Type, 1);
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
           | Iir_Kind_Slice_Name
           | Iir_Kind_Dereference =>
            declare
               Base : Valtyp;
               Typ : Type_Acc;
               Off : Value_Offsets;

               Voff : Net;
               Rdwd : Width;
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Target, Base, Typ, Off, Voff, Rdwd);
               if Voff = No_Net then
                  --  FIXME: check index.
                  return Target_Info'(Kind => Target_Simple,
                                      Targ_Type => Typ,
                                      Obj => Base,
                                      Off => Off);
               else
                  return Target_Info'(Kind => Target_Memory,
                                      Targ_Type => Typ,
                                      Mem_Obj => Base,
                                      Mem_Mwidth => Rdwd,
                                      Mem_Moff => 0, -- Uns32 (Off.Mem_Off),
                                      Mem_Voff => Voff,
                                      Mem_Doff => Off.Net_Off);
               end if;
            end;
         when others =>
            Error_Kind ("synth_target", Target);
      end case;
   end Synth_Target;

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Target_Info;
                               Val : Valtyp;
                               Loc : Node);

   --  Extract a part of VAL from a target aggregate at offset OFF (offset
   --  in the array).
   function Aggregate_Extract
     (Val : Valtyp; Off : Uns32; Typ : Type_Acc; Loc : Node)
     return Valtyp
   is
      El_Typ : constant Type_Acc := Get_Array_Element (Val.Typ);
   begin
      case Val.Val.Kind is
         when Value_Net
           | Value_Wire =>
            declare
               N : Net;
            begin
               N := Build2_Extract
                 (Build_Context, Get_Net (Val), Off * El_Typ.W, Typ.W);
               Set_Location (N, Loc);
               return Create_Value_Net (N, Typ);
            end;
         when Value_Memory =>
            declare
               Res : Valtyp;
            begin
               Res := Create_Value_Memory (Typ);
               --  Need to reverse offsets.
               Copy_Memory
                 (Res.Val.Mem,
                  Val.Val.Mem + (Val.Typ.Sz - Size_Type (Off + 1) * El_Typ.Sz),
                  Typ.Sz);
               return Res;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Aggregate_Extract;

   procedure Synth_Assignment_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                         Target : Node;
                                         Target_Typ : Type_Acc;
                                         Val : Valtyp;
                                         Loc : Node)
   is
      Targ_Bnd : constant Bound_Type := Get_Array_Bound (Target_Typ, 1);
      Choice : Node;
      Assoc : Node;
      Pos : Uns32;
      Targ_Info : Target_Info;
   begin
      Choice := Get_Association_Choices_Chain (Target);
      Pos := Targ_Bnd.Len;
      while Is_Valid (Choice) loop
         Assoc := Get_Associated_Expr (Choice);
         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_None =>
               Targ_Info := Synth_Target (Syn_Inst, Assoc);
               if Get_Element_Type_Flag (Choice) then
                  Pos := Pos - 1;
               else
                  Pos := Pos - Get_Array_Bound (Targ_Info.Targ_Type, 1).Len;
               end if;
               Synth_Assignment
                 (Syn_Inst, Targ_Info,
                  Aggregate_Extract (Val, Pos, Targ_Info.Targ_Type, Assoc),
                  Loc);
            when others =>
               Error_Kind ("synth_assignment_aggregate", Choice);
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Synth_Assignment_Aggregate;

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Target_Info;
                               Val : Valtyp;
                               Loc : Node) is
   begin
      case Target.Kind is
         when Target_Aggregate =>
            Synth_Assignment_Aggregate
              (Syn_Inst, Target.Aggr, Target.Targ_Type, Val, Loc);
         when Target_Simple =>
            if Target.Obj.Val.Kind = Value_Wire then
               Synth_Assign (Target.Obj.Val.W, Target.Targ_Type,
                             Val, Target.Off.Net_Off, Loc);
            else
               if not Is_Static (Val.Val) then
                  --  Maybe the error message is too cryptic ?
                  Error_Msg_Synth
                    (+Loc, "cannot assign a net to a static value");
               else
                  declare
                     V : Valtyp;
                  begin
                     V := Val;
                     Strip_Const (V);
                     Copy_Memory (Target.Obj.Val.Mem + Target.Off.Mem_Off,
                                  V.Val.Mem, V.Typ.Sz);
                  end;
               end if;
            end if;
         when Target_Memory =>
            declare
               V : Net;
            begin
               V := Get_Current_Assign_Value
                 (Get_Build (Syn_Inst), Target.Mem_Obj.Val.W,
                  Target.Mem_Moff, Target.Mem_Mwidth);
               V := Build_Dyn_Insert (Get_Build (Syn_Inst), V, Get_Net (Val),
                  Target.Mem_Voff, Target.Mem_Doff);
               Set_Location (V, Loc);
               Synth_Assign
                 (Target.Mem_Obj.Val.W, Target.Targ_Type,
                  Create_Value_Net (V, Target.Targ_Type),
                  Target.Mem_Moff, Loc);
            end;
      end case;
   end Synth_Assignment;

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Node;
                               Val : Valtyp;
                               Loc : Node)
   is
      Info : Target_Info;
   begin
      Info := Synth_Target (Syn_Inst, Target);
      Synth_Assignment (Syn_Inst, Info, Val, Loc);
   end Synth_Assignment;

   function Synth_Read_Memory (Syn_Inst : Synth_Instance_Acc;
                               Obj : Valtyp;
                               Res_Typ : Type_Acc;
                               Off : Uns32;
                               Voff : Net;
                               Loc : Node) return Valtyp
   is
      N : Net;
   begin
      N := Get_Net (Obj);
      if Voff /= No_Net then
         Synth.Source.Set_Location_Maybe (N, Loc);
         N := Build_Dyn_Extract
           (Get_Build (Syn_Inst), N, Voff, Off, Res_Typ.W);
      else
         pragma Assert (not Is_Static (Obj.Val));
         N := Build2_Extract
           (Get_Build (Syn_Inst), N, Off, Res_Typ.W);
      end if;
      Set_Location (N, Loc);
      return Create_Value_Net (N, Res_Typ);
   end Synth_Read_Memory;

   function Synth_Read (Syn_Inst : Synth_Instance_Acc;
                        Targ : Target_Info;
                        Loc : Node) return Valtyp
   is
      N : Net;
   begin
      case Targ.Kind is
         when Target_Simple =>
            N := Build2_Extract (Get_Build (Syn_Inst), Get_Net (Targ.Obj),
                                 Targ.Off.Net_Off, Targ.Targ_Type.W);
            return Create_Value_Net (N, Targ.Targ_Type);
         when Target_Aggregate =>
            raise Internal_Error;
         when Target_Memory =>
            return Synth_Read_Memory (Syn_Inst, Targ.Mem_Obj, Targ.Targ_Type,
                                      Targ.Mem_Moff, Targ.Mem_Voff, Loc);
      end case;
   end Synth_Read;

   --  Concurrent or sequential simple signal assignment
   procedure Synth_Simple_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Targ : Target_Info;
      Val : Valtyp;
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
      Inp : Input;
      Val, Cond_Val : Valtyp;
      Cond_Net : Net;
      First, Last : Net;
      V : Net;
   begin
      Targ := Synth_Target (Syn_Inst, Get_Target (Stmt));
      Last := No_Net;
      Cwf := Get_Conditional_Waveform_Chain (Stmt);
      Cond := Null_Node;
      while Cwf /= Null_Node loop
         Val := Synth_Waveform
           (Syn_Inst, Get_Waveform_Chain (Cwf), Targ.Targ_Type);
         V := Get_Net (Val);
         Cond := Get_Condition (Cwf);
         if Cond /= Null_Node then
            Cond_Val := Synth_Expression (Syn_Inst, Cond);
            if Cond_Val = No_Valtyp then
               Cond_Net := Build_Const_UB32 (Build_Context, 0, 1);
            else
               Cond_Net := Get_Net (Cond_Val);
            end if;

            V := Build_Mux2 (Build_Context, Cond_Net, No_Net, V);
            Set_Location (V, Cwf);
         end if;

         if Last /= No_Net then
            Inp := Get_Input (Get_Net_Parent (Last), 1);
            Connect (Inp, V);
         else
            First := V;
         end if;
         Last := V;
         Cwf := Get_Chain (Cwf);
      end loop;
      if Cond /= Null_Node then
         pragma Assert (Last /= No_Net);
         Inp := Get_Input (Get_Net_Parent (Last), 1);
         if Get_Driver (Inp) = No_Net then
            --  No else.
            Val := Synth_Read (Syn_Inst, Targ, Stmt);
            Connect (Inp, Get_Net (Val));
         end if;
      end if;
      Val := Create_Value_Net (First, Targ.Targ_Type);
      Synth_Assignment (Syn_Inst, Targ, Val, Stmt);
   end Synth_Conditional_Signal_Assignment;

   procedure Synth_Variable_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Targ : Target_Info;
      Val : Valtyp;
   begin
      Targ := Synth_Target (Syn_Inst, Get_Target (Stmt));
      Val := Synth_Expression_With_Type
        (Syn_Inst, Get_Expression (Stmt), Targ.Targ_Type);
      if Val = No_Valtyp then
         Set_Error (Syn_Inst);
         return;
      end if;
      Synth_Assignment (Syn_Inst, Targ, Val, Stmt);
   end Synth_Variable_Assignment;

   procedure Synth_Conditional_Variable_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Target : constant Node := Get_Target (Stmt);
      Targ_Type : Type_Acc;
      Cond : Node;
      Ce : Node;
      Val, Cond_Val : Valtyp;
      V : Net;
      First, Last : Net;
   begin
      Targ_Type := Get_Subtype_Object (Syn_Inst, Get_Type (Target));
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
      Cond_Val : Valtyp;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      Cond_Val := Synth_Expression (C.Inst, Cond);
      if Cond_Val = No_Valtyp then
         Set_Error (C.Inst);
         return;
      end if;
      if Is_Static (Cond_Val.Val) then
         Strip_Const (Cond_Val);
         if Read_Discrete (Cond_Val) = 1 then
            --  True.
            Synth_Sequential_Statements
              (C, Get_Sequential_Statement_Chain (Stmt));
         else
            pragma Assert (Read_Discrete (Cond_Val) = 0);
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

         Merge_Phis (Get_Build (C.Inst),
                     Get_Net (Cond_Val), Phi_True, Phi_False, Stmt);
      end if;
   end Synth_If_Statement;

   --  EXPR is a choice, so a locally static literal.
   function Convert_To_Uns64 (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Uns64
   is
      Expr_Val : Valtyp;
      Vec : Logvec_Array (0 .. 1);
      Off : Uns32;
      Has_Zx : Boolean;
   begin
      Expr_Val := Synth_Expression_With_Basetype (Syn_Inst, Expr);
      Off := 0;
      Has_Zx := False;
      Vec := (others => (0, 0));
      Value2logvec (Expr_Val, Vec, Off, Has_Zx);
      if Has_Zx then
         Error_Msg_Synth (+Expr, "meta-values never match");
      end if;
      return Uns64 (Vec (0).Val) or Shift_Left (Uns64 (Vec (1).Val), 32);
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

   type Partial_Assign_Array_Acc is access Partial_Assign_Array;
   procedure Free_Partial_Assign_Array is new Ada.Unchecked_Deallocation
     (Partial_Assign_Array, Partial_Assign_Array_Acc);

   procedure Synth_Case_Statement_Dynamic
     (C : in out Seq_Context; Stmt : Node; Sel : Valtyp)
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
               Choice_Data (Choice_Idx) :=
                 (Val => Convert_To_Uns64 (C.Inst,
                                           Get_Choice_Expression (Choice)),
                  Alt => Alt_Idx);
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
            Ch : Natural;
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
                  Ch := Natural (Annex_Arr (J));
                  Case_El (J) := (Sel => Choice_Data (Ch).Val,
                                  Val => Nets (Int32 (Choice_Data (Ch).Alt)));
               end loop;

               --  Extract default value (for missing alternative).
               if Others_Alt_Idx /= 0 then
                  Default := Nets (Int32 (Others_Alt_Idx));
               else
                  Default := No_Net;
               end if;

               --  Generate the muxes tree.
               Synth_Case (Get_Build (C.Inst),
                           Sel_Net, Case_El.all, Default, Res,
                           Get_Location (Expr));

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
   end Synth_Case_Statement_Dynamic;

   procedure Synth_Case_Statement_Static_Array
     (C : in out Seq_Context; Stmt : Node; Sel : Valtyp)
   is
      Choices : constant Node := Get_Case_Statement_Alternative_Chain (Stmt);
      Choice : Node;
      Stmts : Node;
      Sel_Expr : Node;
      Sel_Val : Valtyp;
   begin
      --  Synth statements, extract choice value.
      Stmts := Null_Node;
      Choice := Choices;
      loop
         pragma Assert (Is_Valid (Choice));
         if not Get_Same_Alternative_Flag (Choice) then
            Stmts := Get_Associated_Chain (Choice);
         end if;

         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_Expression =>
               Sel_Expr := Get_Choice_Expression (Choice);
               Sel_Val := Synth_Expression_With_Basetype (C.Inst, Sel_Expr);
               if Is_Equal (Sel_Val, Sel) then
                  Synth_Sequential_Statements (C, Stmts);
                  exit;
               end if;
            when Iir_Kind_Choice_By_Others =>
               Synth_Sequential_Statements (C, Stmts);
               exit;
            when others =>
               raise Internal_Error;
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Synth_Case_Statement_Static_Array;

   procedure Synth_Case_Statement_Static_Scalar
     (C : in out Seq_Context; Stmt : Node; Sel : Int64)
   is
      Choices : constant Node := Get_Case_Statement_Alternative_Chain (Stmt);
      Choice : Node;
      Stmts : Node;
      Sel_Expr : Node;
   begin
      --  Synth statements, extract choice value.
      Stmts := Null_Node;
      Choice := Choices;
      loop
         pragma Assert (Is_Valid (Choice));
         if not Get_Same_Alternative_Flag (Choice) then
            Stmts := Get_Associated_Chain (Choice);
         end if;

         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_Expression =>
               Sel_Expr := Get_Choice_Expression (Choice);
               if Vhdl.Evaluation.Eval_Pos (Sel_Expr) = Sel then
                  Synth_Sequential_Statements (C, Stmts);
                  exit;
               end if;
            when Iir_Kind_Choice_By_Others =>
               Synth_Sequential_Statements (C, Stmts);
               exit;
            when Iir_Kind_Choice_By_Range =>
               declare
                  Bnd : Discrete_Range_Type;
                  Is_In : Boolean;
               begin
                  Synth_Discrete_Range
                    (C.Inst, Get_Choice_Range (Choice), Bnd);
                  case Bnd.Dir is
                     when Iir_To =>
                        Is_In := Sel >= Bnd.Left and Sel <= Bnd.Right;
                     when Iir_Downto =>
                        Is_In := Sel <= Bnd.Left and Sel >= Bnd.Right;
                  end case;
                  if Is_In then
                     Synth_Sequential_Statements (C, Stmts);
                     exit;
                  end if;
               end;
            when others =>
               raise Internal_Error;
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Synth_Case_Statement_Static_Scalar;

   procedure Synth_Case_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Expr : constant Node := Get_Expression (Stmt);
      Sel : Valtyp;
   begin
      Sel := Synth_Expression_With_Basetype (C.Inst, Expr);
      Strip_Const (Sel);
      if Is_Static (Sel.Val) then
         case Sel.Typ.Kind is
            when Type_Bit
              | Type_Logic
              | Type_Discrete =>
               Synth_Case_Statement_Static_Scalar (C, Stmt,
                                                   Read_Discrete (Sel));
            when Type_Vector
              | Type_Array =>
               Synth_Case_Statement_Static_Array (C, Stmt, Sel);
            when others =>
               raise Internal_Error;
         end case;
      else
         Synth_Case_Statement_Dynamic (C, Stmt, Sel);
      end if;
   end Synth_Case_Statement;

   procedure Synth_Selected_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      use Vhdl.Sem_Expr;

      Expr : constant Node := Get_Expression (Stmt);
      Choices : constant Node := Get_Selected_Waveform_Chain (Stmt);
      Choice : Node;

      Targ : Target_Info;
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

      Sel : Valtyp;
      Sel_Net : Net;
   begin
      Targ := Synth_Target (Syn_Inst, Get_Target (Stmt));
      Targ_Type := Targ.Targ_Type;

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
               Choice_Data (Choice_Idx) :=
                 (Val => Convert_To_Uns64 (Syn_Inst,
                                           Get_Choice_Expression (Choice)),
                  Alt => Alt_Idx);
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
         Synth_Case (Get_Build (Syn_Inst),
                     Sel_Net, Case_El.all, Default, Res,
                     Get_Location (Expr));
         Synth_Assignment
           (Syn_Inst, Targ, Create_Value_Net (Res, Targ_Type), Stmt);
      end;

      --  free.
      Free_Case_Element_Array (Case_El);
      Free_Choice_Data_Array (Choice_Data);
      Free_Annex_Array (Annex_Arr);
      Free_Alternative_Data_Array (Alts);
   end Synth_Selected_Signal_Assignment;

   function Synth_Label (Stmt : Node) return Sname
   is
      Label : constant Name_Id := Get_Label (Stmt);
   begin
      if Label = Null_Identifier then
         return No_Sname;
      else
         return New_Sname_User (Label, No_Sname);
      end if;
   end Synth_Label;

   function Is_Copyback_Interface (Inter : Node) return Boolean is
   begin
      case Iir_Parameter_Modes (Get_Mode (Inter)) is
         when Iir_In_Mode =>
            return False;
         when Iir_Out_Mode | Iir_Inout_Mode =>
            return Get_Kind (Inter) = Iir_Kind_Interface_Variable_Declaration;
      end case;
   end Is_Copyback_Interface;

   type Association_Iterator_Kind is
     (Association_Function,
      Association_Operator);

   type Association_Iterator_Init
     (Kind : Association_Iterator_Kind := Association_Function) is
   record
      Inter_Chain : Node;
      case Kind is
         when Association_Function =>
            Assoc_Chain : Node;
         when Association_Operator =>
            Left : Node;
            Right : Node;
      end case;
   end record;

   function Association_Iterator_Build (Inter_Chain : Node; Assoc_Chain : Node)
                                       return Association_Iterator_Init is
   begin
      return Association_Iterator_Init'(Kind => Association_Function,
                                        Inter_Chain => Inter_Chain,
                                        Assoc_Chain => Assoc_Chain);
   end Association_Iterator_Build;

   function Association_Iterator_Build
     (Inter_Chain : Node; Left : Node; Right : Node)
     return Association_Iterator_Init is
   begin
      return Association_Iterator_Init'(Kind => Association_Operator,
                                        Inter_Chain => Inter_Chain,
                                        Left => Left,
                                        Right => Right);
   end Association_Iterator_Build;

   function Count_Associations (Init : Association_Iterator_Init)
                               return Natural
   is
      Assoc : Node;
      Assoc_Inter : Node;
      Inter : Node;
      Nbr_Inout : Natural;
   begin
      case Init.Kind is
         when Association_Function =>
            Nbr_Inout := 0;

            Assoc := Init.Assoc_Chain;
            Assoc_Inter := Init.Inter_Chain;
            while Is_Valid (Assoc) loop
               Inter := Get_Association_Interface (Assoc, Assoc_Inter);

               if Is_Copyback_Interface (Inter) then
                  Nbr_Inout := Nbr_Inout + 1;
               end if;

               Next_Association_Interface (Assoc, Assoc_Inter);
            end loop;

            return Nbr_Inout;
         when Association_Operator =>
            return 0;
      end case;
   end Count_Associations;

   type Association_Iterator
     (Kind : Association_Iterator_Kind := Association_Function) is
   record
      Inter : Node;
      case Kind is
         when Association_Function =>
            First_Named_Assoc : Node;
            Next_Assoc : Node;
         when Association_Operator =>
            Op1 : Node;
            Op2 : Node;
      end case;
   end record;

   procedure Association_Iterate_Init (Iterator : out Association_Iterator;
                                       Init : Association_Iterator_Init) is
   begin
      case Init.Kind is
         when Association_Function =>
            Iterator := (Kind => Association_Function,
                         Inter => Init.Inter_Chain,
                         First_Named_Assoc => Null_Node,
                         Next_Assoc => Init.Assoc_Chain);
         when Association_Operator =>
            Iterator := (Kind => Association_Operator,
                         Inter => Init.Inter_Chain,
                         Op1 => Init.Left,
                         Op2 => Init.Right);
      end case;
   end Association_Iterate_Init;

   --  Return the next association.
   --  ASSOC can be:
   --  * an Iir_Kind_Association_By_XXX node (normal case)
   --  * Null_Iir if INTER is not associated (and has a default value).
   --  * an expression (for operator association).
   procedure Association_Iterate_Next (Iterator : in out Association_Iterator;
                                       Inter : out Node;
                                       Assoc : out Node)
   is
      Formal : Node;
   begin
      Inter := Iterator.Inter;
      if Inter = Null_Node then
         --  End of iterator.
         Assoc := Null_Node;
         return;
      else
         --  Advance to the next interface for the next call.
         Iterator.Inter := Get_Chain (Iterator.Inter);
      end if;

      case Iterator.Kind is
         when Association_Function =>
            if Iterator.First_Named_Assoc = Null_Node then
               Assoc := Iterator.Next_Assoc;
               if Assoc = Null_Node then
                  --  No more association: open association.
                  return;
               end if;
               Formal := Get_Formal (Assoc);
               if Formal = Null_Node then
                  --  Association by position.
                  --  Update for the next call.
                  Iterator.Next_Assoc := Get_Chain (Assoc);
                  return;
               end if;
               Iterator.First_Named_Assoc := Assoc;
            end if;

            --  Search by name.
            Assoc := Iterator.First_Named_Assoc;
            while Assoc /= Null_Node loop
               Formal := Get_Formal (Assoc);
               pragma Assert (Formal /= Null_Node);
               Formal := Get_Interface_Of_Formal (Formal);
               if Formal = Inter then
                  --  Found.
                  --  Optimize in case assocs are in order.
                  if Assoc = Iterator.First_Named_Assoc then
                     Iterator.First_Named_Assoc := Get_Chain (Assoc);
                  end if;
                  return;
               end if;
               Assoc := Get_Chain (Assoc);
            end loop;

            --  Not found: open association.
            return;

         when Association_Operator =>
            Assoc := Iterator.Op1;
            Iterator.Op1 := Iterator.Op2;
            Iterator.Op2 := Null_Node;
      end case;
   end Association_Iterate_Next;

   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Init : Association_Iterator_Init;
                                           Infos : out Target_Info_Array)
   is
      pragma Assert (Infos'First = 1);
      Inter : Node;
      Inter_Type : Type_Acc;
      Assoc : Node;
      Actual : Node;
      Val : Valtyp;
      Nbr_Inout : Natural;
      Iterator : Association_Iterator;
      Info : Target_Info;
   begin
      Set_Instance_Const (Subprg_Inst, True);

      Nbr_Inout := 0;

      --  Process in INTER order.
      Association_Iterate_Init (Iterator, Init);
      loop
         Association_Iterate_Next (Iterator, Inter, Assoc);
         exit when Inter = Null_Node;

         Inter_Type := Get_Subtype_Object (Caller_Inst, Get_Type (Inter));

         case Iir_Parameter_Modes (Get_Mode (Inter)) is
            when Iir_In_Mode =>
               if Assoc = Null_Node
                 or else Get_Kind (Assoc) = Iir_Kind_Association_Element_Open
               then
                  Actual := Get_Default_Value (Inter);
                  Val := Synth_Expression_With_Type
                    (Subprg_Inst, Actual, Inter_Type);
               else
                  if Get_Kind (Assoc) =
                    Iir_Kind_Association_Element_By_Expression
                  then
                     Actual := Get_Actual (Assoc);
                  else
                     Actual := Assoc;
                  end if;
                  Val := Synth_Expression_With_Type
                    (Caller_Inst, Actual, Inter_Type);
               end if;
            when Iir_Out_Mode | Iir_Inout_Mode =>
               Actual := Get_Actual (Assoc);
               Info := Synth_Target (Caller_Inst, Actual);

               case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter))
                  is
                  when Iir_Kind_Interface_Constant_Declaration =>
                     raise Internal_Error;
                  when Iir_Kind_Interface_Variable_Declaration =>
                     --  Always pass by value.
                     Nbr_Inout := Nbr_Inout + 1;
                     Infos (Nbr_Inout) := Info;
                     if Info.Kind = Target_Simple
                       and then Is_Static (Info.Obj.Val)
                     then
                        Val := Create_Value_Memory (Info.Targ_Type);
                        Copy_Memory (Val.Val.Mem,
                                     Info.Obj.Val.Mem + Info.Off.Mem_Off,
                                     Info.Targ_Type.Sz);
                     else
                        Val := Synth_Read (Caller_Inst, Info, Assoc);
                     end if;
                  when Iir_Kind_Interface_Signal_Declaration =>
                     --  Always pass by reference (use an alias).
                     if Info.Kind /= Target_Simple then
                        raise Internal_Error;
                     end if;
                     Val := Create_Value_Alias
                       (Info.Obj.Val, Info.Off, Info.Targ_Type);
                  when Iir_Kind_Interface_File_Declaration =>
                     Val := Info.Obj;
                  when Iir_Kind_Interface_Quantity_Declaration =>
                     raise Internal_Error;
               end case;
         end case;

         if Val = No_Valtyp then
            Set_Error (Subprg_Inst);
            return;
         end if;

         --  FIXME: conversion only for constants, reshape for all.
         Val := Synth_Subtype_Conversion (Val, Inter_Type, True, Assoc);

         if Get_Instance_Const (Subprg_Inst) and then not Is_Static (Val.Val)
         then
            Set_Instance_Const (Subprg_Inst, False);
         end if;

         case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration =>
               --  Pass by reference.
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_Variable_Declaration =>
               --  Arguments are passed by copy.
               if Is_Static (Val.Val) or else Get_Mode (Inter) = Iir_In_Mode
               then
                  Val := Unshare (Val, Current_Pool);
               else
                  --  Will be changed to a wire.
                  null;
               end if;
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_Signal_Declaration =>
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_File_Declaration =>
               Create_Object (Subprg_Inst, Inter, Val);
            when Iir_Kind_Interface_Quantity_Declaration =>
               raise Internal_Error;
         end case;
      end loop;
   end Synth_Subprogram_Association;

   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node)
   is
      Infos : Target_Info_Array (1 .. 0);
      pragma Unreferenced (Infos);
      Init : Association_Iterator_Init;
   begin
      Init := Association_Iterator_Build (Inter_Chain, Assoc_Chain);
      Synth_Subprogram_Association (Subprg_Inst, Caller_Inst, Init, Infos);
   end Synth_Subprogram_Association;

   --  Create wires for out and inout interface variables.
   procedure Synth_Subprogram_Association_Wires
     (Subprg_Inst : Synth_Instance_Acc; Init : Association_Iterator_Init)
   is
      Inter : Node;
      Assoc : Node;
      Val : Valtyp;
      Iterator : Association_Iterator;
      Wire : Wire_Id;
   begin
      --  Process in INTER order.
      Association_Iterate_Init (Iterator, Init);
      loop
         Association_Iterate_Next (Iterator, Inter, Assoc);
         exit when Inter = Null_Node;

         if Get_Mode (Inter) in Iir_Out_Modes
           and then Get_Kind (Inter) = Iir_Kind_Interface_Variable_Declaration
         then
            Val := Get_Value (Subprg_Inst, Inter);
            --  Arguments are passed by copy.
            Wire := Alloc_Wire (Wire_Variable, Inter);
            Set_Wire_Gate (Wire, Get_Net (Val));

            Val := Create_Value_Wire (Wire, Val.Typ);
            Create_Object_Force (Subprg_Inst, Inter, No_Valtyp);
            Create_Object_Force (Subprg_Inst, Inter, Val);
         end if;
      end loop;
   end Synth_Subprogram_Association_Wires;

   procedure Synth_Subprogram_Back_Association
     (Subprg_Inst : Synth_Instance_Acc;
      Caller_Inst : Synth_Instance_Acc;
      Init : Association_Iterator_Init;
      Infos : Target_Info_Array)
   is
      pragma Assert (Infos'First = 1);
      Inter : Node;
      Assoc : Node;
      Assoc_Inter : Node;
      Val : Valtyp;
      Nbr_Inout : Natural;
   begin
      Nbr_Inout := 0;
      pragma Assert (Init.Kind = Association_Function);
      Assoc := Init.Assoc_Chain;
      Assoc_Inter := Init.Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);

         if Is_Copyback_Interface (Inter) then
            if not Get_Whole_Association_Flag (Assoc) then
               raise Internal_Error;
            end if;
            Nbr_Inout := Nbr_Inout + 1;
            Val := Get_Value (Subprg_Inst, Inter);
            Synth_Assignment (Caller_Inst, Infos (Nbr_Inout), Val, Assoc);

            --  Free wire used for out/inout interface variables.
            if Val.Val.Kind = Value_Wire then
               Phi_Discard_Wires (Val.Val.W, No_Wire_Id);
               Free_Wire (Val.Val.W);
            end if;
         end if;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
      pragma Assert (Nbr_Inout = Infos'Last);
   end Synth_Subprogram_Back_Association;

   function Synth_Dynamic_Subprogram_Call (Syn_Inst : Synth_Instance_Acc;
                                           Sub_Inst : Synth_Instance_Acc;
                                           Call : Node;
                                           Init : Association_Iterator_Init;
                                           Infos : Target_Info_Array)
                                          return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Call);
      Is_Func : constant Boolean := Is_Function_Declaration (Imp);
      Bod : constant Node := Vhdl.Sem_Inst.Get_Subprogram_Body_Origin (Imp);
      Res : Valtyp;
      C : Seq_Context (Mode_Dynamic);
      Wire_Mark : Wire_Id;
      Subprg_Phi : Phi_Type;
   begin
      Mark (Wire_Mark);
      C := (Mode => Mode_Dynamic,
            Inst => Sub_Inst,
            Cur_Loop => null,
            W_En => No_Wire_Id,
            W_Ret => No_Wire_Id,
            W_Val => No_Wire_Id,
            Ret_Init => No_Net,
            Ret_Value => No_Valtyp,
            Ret_Typ => null,
            Nbr_Ret => 0);

      C.W_En := Alloc_Wire (Wire_Variable, Imp);
      C.W_Ret := Alloc_Wire (Wire_Variable, Imp);

      if Is_Func then
         C.W_Val := Alloc_Wire (Wire_Variable, Imp);
      end if;

      Push_Phi;

      Synth_Subprogram_Association_Wires (Sub_Inst, Init);

      if Is_Func then
         --  Set a default value for the return.
         C.Ret_Typ := Get_Subtype_Object (Syn_Inst, Get_Return_Type (Imp));

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
      if not Is_Error (C.Inst) then
         Synth_Sequential_Statements (C, Get_Sequential_Statement_Chain (Bod));
      end if;

      if Is_Error (C.Inst) then
         Res := No_Valtyp;
      else
         if Is_Func then
            if C.Nbr_Ret = 0 then
               raise Internal_Error;
            elsif C.Nbr_Ret = 1 and then Is_Static (C.Ret_Value.Val) then
               Res := C.Ret_Value;
            else
               Res := Create_Value_Net
                 (Get_Current_Value (Build_Context, C.W_Val), C.Ret_Value.Typ);
            end if;
         else
            Res := No_Valtyp;
            Synth_Subprogram_Back_Association (C.Inst, Syn_Inst, Init, Infos);
         end if;
      end if;

      Pop_Phi (Subprg_Phi);

      Decls.Finalize_Declarations (C.Inst, Get_Declaration_Chain (Bod), True);
      pragma Unreferenced (Infos);

      --  Propagate assignments.
      Propagate_Phi_Until_Mark (Get_Build (C.Inst), Subprg_Phi, Wire_Mark);

      --  Free wires.
      Free_Wire (C.W_En);
      Free_Wire (C.W_Ret);
      if Is_Func then
         Free_Wire (C.W_Val);
      end if;

      Release (Wire_Mark);

      return Res;
   end Synth_Dynamic_Subprogram_Call;

   function Synth_Static_Subprogram_Call (Syn_Inst : Synth_Instance_Acc;
                                          Sub_Inst : Synth_Instance_Acc;
                                          Call : Node;
                                          Init : Association_Iterator_Init;
                                          Infos : Target_Info_Array)
                                         return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Call);
      Is_Func : constant Boolean := Is_Function_Declaration (Imp);
      Bod : constant Node := Get_Subprogram_Body (Imp);
      Res : Valtyp;
      C : Seq_Context (Mode_Static);
   begin
      C := (Mode_Static,
            Inst => Sub_Inst,
            Cur_Loop => null,
            S_En => True,
            Ret_Value => No_Valtyp,
            Ret_Typ => null,
            Nbr_Ret => 0);

      if Is_Func then
         --  Set a default value for the return.
         C.Ret_Typ := Get_Subtype_Object (Syn_Inst, Get_Return_Type (Imp));
      end if;

      Decls.Synth_Declarations (C.Inst, Get_Declaration_Chain (Bod), True);

      if not Is_Error (C.Inst) then
         Synth_Sequential_Statements (C, Get_Sequential_Statement_Chain (Bod));
      end if;

      if Is_Error (C.Inst) then
         Res := No_Valtyp;
      else
         if Is_Func then
            if C.Nbr_Ret = 0 then
               Error_Msg_Synth
                 (+Call, "function call completed without a return statement");
               Res := No_Valtyp;
            else
               pragma Assert (C.Nbr_Ret = 1);
               pragma Assert (Is_Static (C.Ret_Value.Val));
               Res := C.Ret_Value;
            end if;
         else
            Res := No_Valtyp;
            Synth_Subprogram_Back_Association (C.Inst, Syn_Inst, Init, Infos);
         end if;
      end if;

      Decls.Finalize_Declarations (C.Inst, Get_Declaration_Chain (Bod), True);
      pragma Unreferenced (Infos);

      return Res;
   end Synth_Static_Subprogram_Call;

   function Synth_Subprogram_Call (Syn_Inst : Synth_Instance_Acc;
                                   Call : Node;
                                   Init : Association_Iterator_Init)
                                  return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Call);
      Is_Func : constant Boolean := Is_Function_Declaration (Imp);
      Bod : constant Node := Vhdl.Sem_Inst.Get_Subprogram_Body_Origin (Imp);
      Nbr_Inout : constant Natural := Count_Associations (Init);
      Infos : Target_Info_Array (1 .. Nbr_Inout);
      Area_Mark : Areapools.Mark_Type;
      Res : Valtyp;
      Sub_Inst : Synth_Instance_Acc;
      Up_Inst : Synth_Instance_Acc;
   begin
      Areapools.Mark (Area_Mark, Instance_Pool.all);

      Up_Inst := Get_Instance_By_Scope (Syn_Inst, Get_Parent_Scope (Imp));
      Sub_Inst := Make_Instance (Up_Inst, Bod,
                                 New_Internal_Name (Build_Context));
      Set_Instance_Base (Sub_Inst, Syn_Inst);

      Synth_Subprogram_Association (Sub_Inst, Syn_Inst, Init, Infos);

      if Is_Error (Sub_Inst) then
         Res := No_Valtyp;
      else
         if not Is_Func then
            if Get_Purity_State (Imp) /= Pure then
               Set_Instance_Const (Sub_Inst, False);
            end if;
         end if;

         if Get_Instance_Const (Sub_Inst) then
            Res := Synth_Static_Subprogram_Call
              (Syn_Inst, Sub_Inst, Call, Init, Infos);
         else
            Res := Synth_Dynamic_Subprogram_Call
              (Syn_Inst, Sub_Inst, Call, Init, Infos);
         end if;
      end if;

      --  Propagate error.
      if Is_Error (Sub_Inst) then
         Set_Error (Syn_Inst);
      end if;

      if Debugger.Flag_Need_Debug then
         Debugger.Debug_Leave (Sub_Inst);
      end if;

      Free_Instance (Sub_Inst);
      Areapools.Release (Area_Mark, Instance_Pool.all);

      return Res;
   end Synth_Subprogram_Call;

   function Synth_Subprogram_Call
     (Syn_Inst : Synth_Instance_Acc; Call : Node) return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Call);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Init : Association_Iterator_Init;
   begin
      Init := Association_Iterator_Build (Inter_Chain, Assoc_Chain);
      return Synth_Subprogram_Call (Syn_Inst, Call, Init);
   end Synth_Subprogram_Call;

   function Synth_User_Operator (Syn_Inst : Synth_Instance_Acc;
                                 Left_Expr : Node;
                                 Right_Expr : Node;
                                 Expr : Node) return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Init : Association_Iterator_Init;
   begin
      Init := Association_Iterator_Build (Inter_Chain, Left_Expr, Right_Expr);
      return Synth_Subprogram_Call (Syn_Inst, Expr, Init);
   end Synth_User_Operator;

   procedure Synth_Implicit_Procedure_Call
     (Syn_Inst : Synth_Instance_Acc; Call : Node)
   is
      Imp  : constant Node := Get_Implementation (Call);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Init : constant Association_Iterator_Init :=
        Association_Iterator_Build (Inter_Chain, Assoc_Chain);
      Nbr_Inout : constant Natural := Count_Associations (Init);
      Infos : Target_Info_Array (1 .. Nbr_Inout);
      Area_Mark : Areapools.Mark_Type;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Areapools.Mark (Area_Mark, Instance_Pool.all);
      Sub_Inst := Make_Instance (Syn_Inst, Imp,
                                 New_Internal_Name (Build_Context));

      Synth_Subprogram_Association (Sub_Inst, Syn_Inst, Init, Infos);

      Synth.Static_Proc.Synth_Static_Procedure (Sub_Inst, Imp, Call);

      Synth_Subprogram_Back_Association (Sub_Inst, Syn_Inst, Init, Infos);

      Free_Instance (Sub_Inst);
      Areapools.Release (Area_Mark, Instance_Pool.all);
   end Synth_Implicit_Procedure_Call;

   procedure Synth_Procedure_Call (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Call : constant Node := Get_Procedure_Call (Stmt);
      Imp  : constant Node := Get_Implementation (Call);
      Res : Valtyp;
   begin
      case Get_Implicit_Definition (Imp) is
         when Iir_Predefined_None =>
            if Get_Foreign_Flag (Imp) then
               Error_Msg_Synth
                 (+Stmt, "call to foreign %n is not supported", +Imp);
            else
               Res := Synth_Subprogram_Call (Syn_Inst, Call);
               pragma Assert (Res = No_Valtyp);
            end if;
         when others =>
            Synth_Implicit_Procedure_Call (Syn_Inst, Call);
      end case;
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

   procedure Update_Index (Rng : Discrete_Range_Type; V : in out Valtyp)
   is
      T : Int64;
   begin
      T := Read_Discrete (V);
      case Rng.Dir is
         when Iir_To =>
            T := T + 1;
         when Iir_Downto =>
            T := T - 1;
      end case;
      Write_Discrete (V, T);
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

   procedure Synth_Dynamic_Exit_Next_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Cond : constant Node := Get_Condition (Stmt);
      Is_Exit : constant Boolean := Get_Kind (Stmt) = Iir_Kind_Exit_Statement;
      Static_Cond : Boolean;
      Loop_Label : Node;
      Lc : Loop_Context_Acc;
      Cond_Val : Valtyp;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      if Cond /= Null_Node then
         Cond_Val := Synth_Expression (C.Inst, Cond);
         Static_Cond := Is_Static_Val (Cond_Val.Val);
         if Static_Cond then
            if Get_Static_Discrete (Cond_Val) = 0 then
               --  Not executed.
               return;
            end if;
         else
            Push_Phi;
         end if;
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

      if Cond /= Null_Node and not Static_Cond then
         Pop_Phi (Phi_True);
         Push_Phi;
         Pop_Phi (Phi_False);
         Merge_Phis (Build_Context,
                     Get_Net (Cond_Val), Phi_True, Phi_False, Stmt);
      end if;
   end Synth_Dynamic_Exit_Next_Statement;

   procedure Synth_Static_Exit_Next_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Cond : constant Node := Get_Condition (Stmt);
      Is_Exit : constant Boolean := Get_Kind (Stmt) = Iir_Kind_Exit_Statement;
      Loop_Label : Node;
      Lc : Loop_Context_Acc;
      Cond_Val : Valtyp;
   begin
      if Cond /= Null_Node then
         Cond_Val := Synth_Expression (C.Inst, Cond);
         pragma Assert (Is_Static_Val (Cond_Val.Val));
         if Get_Static_Discrete (Cond_Val) = 0 then
            --  Not executed.
            return;
         end if;
      end if;

      --  Execution is suspended.
      C.S_En := False;

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
               Lc.S_Exit := True;
            end if;
            exit;
         else
            Lc.S_Quit := True;
         end if;
         Lc := Lc.Prev_Loop;
      end loop;
   end Synth_Static_Exit_Next_Statement;

   procedure Init_For_Loop_Statement (C : in out Seq_Context;
                                      Stmt : Node;
                                      Val : out Valtyp)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
      It_Rng : Type_Acc;
   begin
      if It_Type /= Null_Node then
         Synth_Subtype_Indication (C.Inst, It_Type);
      end if;

      --  Initial value.
      It_Rng := Get_Subtype_Object (C.Inst, Get_Type (Iterator));
      Val := Create_Value_Discrete (It_Rng.Drange.Left, It_Rng);
      Create_Object (C.Inst, Iterator, Val);
   end Init_For_Loop_Statement;

   procedure Finish_For_Loop_Statement (C : in out Seq_Context;
                                        Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
   begin
      Destroy_Object (C.Inst, Iterator);
      if It_Type /= Null_Node then
         Destroy_Object (C.Inst, It_Type);
      end if;
   end Finish_For_Loop_Statement;

   procedure Synth_Dynamic_For_Loop_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      Val : Valtyp;
      Lc : aliased Loop_Context (Mode_Dynamic);
   begin
      Lc := (Mode => Mode_Dynamic,
             Prev_Loop => C.Cur_Loop,
             Loop_Stmt => Stmt,
             Need_Quit => False,
             Saved_En => No_Net,
             W_Exit => No_Wire_Id,
             W_Quit => No_Wire_Id,
             Wire_Mark => No_Wire_Id);
      C.Cur_Loop := Lc'Unrestricted_Access;

      Loop_Control_Init (C, Stmt);

      Init_For_Loop_Statement (C, Stmt, Val);

      while In_Range (Val.Typ.Drange, Read_Discrete (Val)) loop
         Synth_Sequential_Statements (C, Stmts);

         Update_Index (Val.Typ.Drange, Val);
         Loop_Control_Update (C);

         --  Constant exit.
         exit when (Get_Current_Value (null, C.W_En) = Get_Inst_Bit0 (C.Inst));

         --  FIXME: dynamic exits.
      end loop;
      Loop_Control_Finish (C);

      Finish_For_Loop_Statement (C, Stmt);

      C.Cur_Loop := Lc.Prev_Loop;
   end Synth_Dynamic_For_Loop_Statement;

   procedure Synth_Static_For_Loop_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      Val : Valtyp;
      Lc : aliased Loop_Context (Mode_Static);
   begin
      Lc := (Mode_Static,
             Prev_Loop => C.Cur_Loop,
             Loop_Stmt => Stmt,
             S_Exit => False,
             S_Quit => False);
      C.Cur_Loop := Lc'Unrestricted_Access;

      Init_For_Loop_Statement (C, Stmt, Val);

      while In_Range (Val.Typ.Drange, Read_Discrete (Val)) loop
         Synth_Sequential_Statements (C, Stmts);
         C.S_En := True;

         Update_Index (Val.Typ.Drange, Val);

         exit when Lc.S_Exit or Lc.S_Quit or C.Nbr_Ret > 0;
      end loop;

      Finish_For_Loop_Statement (C, Stmt);

      C.Cur_Loop := Lc.Prev_Loop;
   end Synth_Static_For_Loop_Statement;

   procedure Synth_Dynamic_While_Loop_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Bit0 : constant Net := Get_Inst_Bit0 (C.Inst);
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      Cond : constant Node := Get_Condition (Stmt);
      Val : Valtyp;
      Lc : aliased Loop_Context (Mode_Dynamic);
   begin
      Lc := (Mode => Mode_Dynamic,
             Prev_Loop => C.Cur_Loop,
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
            if not Is_Static (Val.Val) then
               Error_Msg_Synth (+Cond, "loop condition must be static");
               exit;
            end if;
            exit when Read_Discrete (Val) = 0;
         end if;

         Synth_Sequential_Statements (C, Stmts);

         Loop_Control_Update (C);

         --  Exit from the loop if W_Exit/W_Ret/W_Quit = 0
         if Lc.W_Exit /= No_Wire_Id
           and then Get_Current_Value (null, Lc.W_Exit) = Bit0
         then
            exit;
         end if;
         if C.W_Ret /= No_Wire_Id
           and then Get_Current_Value (null, C.W_Ret) = Bit0
         then
            exit;
         end if;
         if Lc.W_Quit /= No_Wire_Id
           and then Get_Current_Value (null, Lc.W_Quit) = Bit0
         then
            exit;
         end if;
      end loop;
      Loop_Control_Finish (C);

      C.Cur_Loop := Lc.Prev_Loop;
   end Synth_Dynamic_While_Loop_Statement;

   procedure Synth_Static_While_Loop_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      Cond : constant Node := Get_Condition (Stmt);
      Val : Valtyp;
      Lc : aliased Loop_Context (Mode_Static);
   begin
      Lc := (Mode => Mode_Static,
             Prev_Loop => C.Cur_Loop,
             Loop_Stmt => Stmt,
             S_Exit => False,
             S_Quit => False);
      C.Cur_Loop := Lc'Unrestricted_Access;

      loop
         if Cond /= Null_Node then
            Val := Synth_Expression_With_Type (C.Inst, Cond, Boolean_Type);
            pragma Assert (Is_Static (Val.Val));
            exit when Read_Discrete (Val) = 0;
         end if;

         Synth_Sequential_Statements (C, Stmts);
         C.S_En := True;

         --  Exit from the loop if S_Exit/S_Quit
         exit when Lc.S_Exit or Lc.S_Quit or C.Nbr_Ret > 0;
      end loop;

      C.Cur_Loop := Lc.Prev_Loop;
   end Synth_Static_While_Loop_Statement;

   procedure Synth_Return_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Is_Dyn : constant Boolean := not Get_Instance_Const (C.Inst);
      Val : Valtyp;
      Expr : constant Node := Get_Expression (Stmt);
   begin
      if Expr /= Null_Node then
         --  Return in function.
         Val := Synth_Expression_With_Type (C.Inst, Expr, C.Ret_Typ);
         if Val = No_Valtyp then
            Set_Error (C.Inst);
            return;
         end if;

         Val := Synth_Subtype_Conversion (Val, C.Ret_Typ, True, Stmt);

         if C.Nbr_Ret = 0 then
            C.Ret_Value := Val;
            if not Is_Bounded_Type (C.Ret_Typ) then
               --  The function was declared with an unconstrained return type.
               --  Now that a value has been returned, we know the subtype of
               --  the returned values.  So adjust it.
               --  All the returned values must have the same length.
               C.Ret_Typ := Val.Typ;
               if Is_Dyn then
                  Set_Width (Get_Wire_Gate (C.W_Val), C.Ret_Typ.W);
                  Set_Width (C.Ret_Init, C.Ret_Typ.W);
               end if;
            end if;
         end if;
         if Is_Dyn then
            Phi_Assign (Get_Build (C.Inst), C.W_Val, Get_Net (Val), 0);
         end if;
      end if;

      if Is_Dyn then
         --  The subprogram has returned.  Do not execute further statements.
         Phi_Assign (Get_Build (C.Inst), C.W_En, Get_Inst_Bit0 (C.Inst), 0);

         if C.W_Ret /= No_Wire_Id then
            Phi_Assign (Get_Build (C.Inst), C.W_Ret,
                        Get_Inst_Bit0 (C.Inst), 0);
         end if;
      end if;

      C.Nbr_Ret := C.Nbr_Ret + 1;
   end Synth_Return_Statement;

   procedure Synth_Static_Report
     (C : in out Seq_Context; Stmt : Node)
   is
      use Simple_IO;

      Is_Report : constant Boolean :=
        Get_Kind (Stmt) = Iir_Kind_Report_Statement;
      Rep_Expr : constant Node := Get_Report_Expression (Stmt);
      Sev_Expr : constant Node := Get_Severity_Expression (Stmt);
      Rep : Valtyp;
      Sev : Valtyp;
      Sev_V : Natural;
   begin
      if Rep_Expr /= Null_Node then
         Rep := Synth_Expression_With_Basetype (C.Inst, Rep_Expr);
         if Rep = No_Valtyp then
            Set_Error (C.Inst);
            return;
         end if;
         Strip_Const (Rep);
      end if;
      if Sev_Expr /= Null_Node then
         Sev := Synth_Expression (C.Inst, Sev_Expr);
         if Sev = No_Valtyp then
            Set_Error (C.Inst);
            return;
         end if;
         Strip_Const (Sev);
      end if;

      Put_Err (Disp_Location (Stmt));
      Put_Err (":(");
      if Is_Report then
         Put_Err ("report");
      else
         Put_Err ("assertion");
      end if;
      Put_Err (' ');
      if Sev = No_Valtyp then
         if Is_Report then
            Sev_V := 0;
         else
            Sev_V := 2;
         end if;
      else
         Sev_V := Natural (Read_Discrete (Sev));
      end if;
      case Sev_V is
         when 0 =>
            Put_Err ("note");
         when 1 =>
            Put_Err ("warning");
         when 2 =>
            Put_Err ("error");
         when 3 =>
            Put_Err ("failure");
         when others =>
            Put_Err ("??");
      end case;
      Put_Err ("): ");

      Put_Line_Err (Value_To_String (Rep));
   end Synth_Static_Report;

   procedure Synth_Static_Report_Statement
     (C : in out Seq_Context; Stmt : Node) is
   begin
      Synth_Static_Report (C, Stmt);
   end Synth_Static_Report_Statement;

   procedure Synth_Static_Assertion_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Cond : Valtyp;
   begin
      Cond := Synth_Expression (C.Inst, Get_Assertion_Condition (Stmt));
      if Cond = No_Valtyp then
         Set_Error (C.Inst);
         return;
      end if;
      pragma Assert (Is_Static (Cond.Val));
      Strip_Const (Cond);
      if Read_Discrete (Cond) = 1 then
         return;
      end if;
      Synth_Static_Report (C, Stmt);
   end Synth_Static_Assertion_Statement;

   procedure Synth_Sequential_Statements
     (C : in out Seq_Context; Stmts : Node)
   is
      Is_Dyn : constant Boolean := not Get_Instance_Const (C.Inst);
      Stmt : Node;
      Phi_T, Phi_F : Phi_Type;
      Has_Phi : Boolean;
      En : Net;
   begin
      Stmt := Stmts;
      while Is_Valid (Stmt) loop
         if Is_Dyn then
            En := Get_Current_Value (null, C.W_En);
            pragma Assert (En /= Get_Inst_Bit0 (C.Inst));
            Has_Phi := En /= Get_Inst_Bit1 (C.Inst);
            if Has_Phi then
               Push_Phi;
            end if;
         end if;

         if Flags.Flag_Trace_Statements then
            declare
               Name : Name_Id;
               Line : Natural;
               Col : Natural;
            begin
               Files_Map.Location_To_Position
                 (Get_Location (Stmt), Name, Line, Col);
               Simple_IO.Put_Line ("Execute statement at "
                                     & Name_Table.Image (Name)
                                     & Natural'Image (Line));
            end;
         end if;
         if Synth.Debugger.Flag_Need_Debug then
            Synth.Debugger.Debug_Break (C.Inst, Stmt);
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
               if Is_Dyn then
                  Synth_Dynamic_For_Loop_Statement (C, Stmt);
               else
                  Synth_Static_For_Loop_Statement (C, Stmt);
               end if;
            when Iir_Kind_While_Loop_Statement =>
               if Is_Dyn then
                  Synth_Dynamic_While_Loop_Statement (C, Stmt);
               else
                  Synth_Static_While_Loop_Statement (C, Stmt);
               end if;
            when Iir_Kind_Null_Statement =>
               --  Easy
               null;
            when Iir_Kind_Return_Statement =>
               Synth_Return_Statement (C, Stmt);
            when Iir_Kind_Procedure_Call_Statement =>
               Synth_Procedure_Call (C.Inst, Stmt);
            when Iir_Kind_Report_Statement =>
               if not Is_Dyn then
                  Synth_Static_Report_Statement (C, Stmt);
               end if;
            when Iir_Kind_Assertion_Statement =>
               if not Is_Dyn then
                  Synth_Static_Assertion_Statement (C, Stmt);
               end if;
            when Iir_Kind_Exit_Statement
              | Iir_Kind_Next_Statement =>
               if Is_Dyn then
                  Synth_Dynamic_Exit_Next_Statement (C, Stmt);
               else
                  Synth_Static_Exit_Next_Statement (C, Stmt);
               end if;
            when others =>
               Error_Kind ("synth_sequential_statements", Stmt);
         end case;
         if Is_Dyn then
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
         else
            if not C.S_En or C.Nbr_Ret /= 0 then
               return;
            end if;
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
      Cond_Val : Valtyp;
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
      C : Seq_Context (Mode_Dynamic);
   begin
      if Label = Null_Identifier then
         C_Sname := New_Internal_Name (Build_Context, Get_Sname (Syn_Inst));
      else
         C_Sname := New_Sname_User (Label, Get_Sname (Syn_Inst));
      end if;
      C := (Mode => Mode_Dynamic,
            Inst => Make_Instance (Syn_Inst, Proc, C_Sname),
            Cur_Loop => null,
            W_En => Alloc_Wire (Wire_Variable, Proc),
            W_Ret => No_Wire_Id,
            W_Val => No_Wire_Id,
            Ret_Init => No_Net,
            Ret_Value => No_Valtyp,
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
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp is
   begin
      --  Is it a call to an ieee function ?
      declare
         Imp  : constant Node := Get_Implementation (Expr);
         Pkg : constant Node := Get_Parent (Imp);
         Unit : Node;
         Lib : Node;
      begin
         if Get_Kind (Pkg) = Iir_Kind_Package_Declaration
           and then not Is_Uninstantiated_Package (Pkg)
         then
            Unit := Get_Parent (Pkg);
            if Get_Kind (Unit) = Iir_Kind_Design_Unit then
               Lib := Get_Library (Get_Design_File (Unit));
               if Get_Identifier (Lib) = Std_Names.Name_Ieee then
                  Error_Msg_Synth
                    (+Expr, "unhandled call to ieee function %i", +Imp);
                  Set_Error (Syn_Inst);
                  return No_Valtyp;
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
      Val : Valtyp;
      Inst : Instance;
   begin
      Val := Synth_Expression (Syn_Inst, Cond);
      if Val = No_Valtyp then
         Set_Error (Syn_Inst);
         return;
      end if;
      if Is_Static (Val.Val) then
         if Read_Discrete (Val) /= 1 then
            Error_Msg_Synth (+Stmt, "assertion failure");
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

      Apply_Block_Configuration
        (Get_Block_Block_Configuration (Blk), Blk);

      Blk_Sname := New_Sname_User (Get_Identifier (Blk), Get_Sname (Syn_Inst));
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
         when N_HDL_Bool =>
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
               if Get_Kind (L) in N_HDLs and then Get_Kind (R) in N_HDLs then
                  Edge := Synth_Clock_Edge
                    (Syn_Inst, Get_HDL_Node (L), Get_HDL_Node (R));
                  if Edge /= No_Net then
                     return Edge;
                  end if;
               end if;
               if Get_Kind (R) = N_EOS then
                  --  It is never EOS!
                  return Build_Const_UB32 (Build_Context, 0, 1);
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
         when N_False
           | N_EOS =>
            return Build_Const_UB32 (Build_Context, 0, 1);
         when others =>
            PSL.Errors.Error_Kind ("synth_psl_expr", Expr);
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

      --  For each state:
      S := Get_First_State (NFA);
      while S /= No_State loop
         S_Num := Get_State_Label (S);
         I := Build_Extract_Bit (Build_Context, States, Uns32 (S_Num));

         --  For each edge:
         E := Get_First_Src_Edge (S);
         while E /= No_Edge loop
            --  Edge condition.
            Cond := Build_Dyadic
              (Build_Context, Netlists.Gates.Id_And,
               I, Synth_PSL_Expression (Syn_Inst, Get_Edge_Expr (E)));

            --  TODO: if EOS is present, then this is a live state.

            --  Reverse order for final concatenation.
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

      Concat_Array (D_Arr.all, Res);
      Free_Net_Array (D_Arr);

      return Res;
   end Synth_Psl_NFA;

   procedure Synth_Psl_Dff (Syn_Inst : Synth_Instance_Acc;
                            Stmt : Node;
                            Next_States : out Net)
   is
      use Netlists.Gates;
      Nbr_States : constant Int32 := Get_PSL_Nbr_States (Stmt);
      States : Net;
      Init : Net;
      Clk : Net;
      Clk_Inst : Instance;
   begin
      --  create init net, clock net
      Init := Build_Const_UB32 (Build_Context, 1, Uns32 (Nbr_States));
      Clk := Synth_PSL_Expression (Syn_Inst, Get_PSL_Clock (Stmt));

      --  Check the clock is an edge and extract it.
      Clk_Inst := Get_Net_Parent (Clk);
      if Get_Id (Clk_Inst) /= Id_Edge then
         Error_Msg_Synth (+Stmt, "clock is not an edge");
         Next_States := No_Net;
         return;
      end if;

      Clk := Get_Input_Net (Clk_Inst, 0);

      --  build idff
      States := Build_Idff (Build_Context, Clk, No_Net, Init);

      --  create update nets
      --  For each state: if set, evaluate all outgoing edges.
      Next_States :=
        Synth_Psl_NFA (Syn_Inst, Get_PSL_NFA (Stmt), Nbr_States, States);
      Connect (Get_Input (Get_Net_Parent (States), 1), Next_States);
   end Synth_Psl_Dff;

   function Synth_Psl_Final
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node; Next_States : Net) return Net
   is
      use PSL.Types;
      use PSL.NFAs;
      NFA : constant PSL_NFA := Get_PSL_NFA (Stmt);
   begin
      return Build_Extract_Bit
        (Get_Build (Syn_Inst), Next_States,
         Uns32 (Get_State_Label (Get_Final_State (NFA))));
   end Synth_Psl_Final;

   function Synth_Psl_Not_Final
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node; Next_States : Net)
     return Net is
   begin
      return Build_Monadic
        (Get_Build (Syn_Inst), Netlists.Gates.Id_Not,
         Synth_Psl_Final (Syn_Inst, Stmt, Next_States));
   end Synth_Psl_Not_Final;

   procedure Synth_Psl_Restrict_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Next_States : Net;
      Res : Net;
      Inst : Instance;
   begin
      --  Build assume gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States /= No_Net then
         --  The restriction holds as long as there is a 1 in the NFA state.
         Res := Build_Reduce (Build_Context,
                              Netlists.Gates.Id_Red_Or, Next_States);
         Inst := Build_Assume (Build_Context, Synth_Label (Stmt), Res);
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Restrict_Directive;

   procedure Synth_Psl_Cover_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Next_States : Net;
      Res : Net;
      Inst : Instance;
   begin
      --  Build cover gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States /= No_Net then
         --  The sequence is covered as soon as the final state is reached.
         Res := Synth_Psl_Final (Syn_Inst, Stmt, Next_States);
         Inst := Build_Cover (Build_Context, Synth_Label (Stmt), Res);
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Cover_Directive;

   procedure Synth_Psl_Assume_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Next_States : Net;
      Inst : Instance;
   begin
      --  Build assume gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States /= No_Net then
         Inst := Build_Assume
           (Build_Context, Synth_Label (Stmt),
            Synth_Psl_Not_Final (Syn_Inst, Stmt, Next_States));
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Assume_Directive;

   procedure Synth_Psl_Assert_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      use PSL.Types;
      use PSL.NFAs;
      NFA : constant PSL_NFA := Get_PSL_NFA (Stmt);
      Active : NFA_State;
      Next_States : Net;
      Inst : Instance;
      Lab : Sname;
   begin
      --  Build assert gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assert on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States /= No_Net then
         Lab := Synth_Label (Stmt);

         Inst := Build_Assert
           (Build_Context, Lab,
            Synth_Psl_Not_Final (Syn_Inst, Stmt, Next_States));
         Set_Location (Inst, Get_Location (Stmt));

         Active := Get_Active_State (NFA);
         if Active /= No_State then
            if Lab /= No_Sname then
               Lab := New_Sname_User (Std_Names.Name_Cover, Lab);
            end if;
            Inst := Build_Assert_Cover
              (Get_Build (Syn_Inst), Lab,
               Build_Extract_Bit (Get_Build (Syn_Inst), Next_States,
                                  Uns32 (Get_State_Label (Active))));
            Set_Location (Inst, Get_Location (Stmt));
         end if;
      end if;
   end Synth_Psl_Assert_Directive;

   procedure Synth_Generate_Statement_Body
     (Syn_Inst : Synth_Instance_Acc;
      Bod : Node;
      Name : Sname;
      Iterator : Node := Null_Node;
      Iterator_Val : Valtyp := No_Valtyp)
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

   procedure Synth_If_Generate_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Gen : Node;
      Bod : Node;
      Icond : Node;
      Cond : Valtyp;
      Name : Sname;
   begin
      Gen := Stmt;
      Name := New_Sname_User (Get_Identifier (Stmt), Get_Sname (Syn_Inst));
      loop
         Icond := Get_Condition (Gen);
         if Icond /= Null_Node then
            Cond := Synth_Expression (Syn_Inst, Icond);
            Strip_Const (Cond);
         else
            --  It is the else generate.
            Cond := No_Valtyp;
         end if;
         if Cond = No_Valtyp or else Read_Discrete (Cond) = 1 then
            Bod := Get_Generate_Statement_Body (Gen);
            Apply_Block_Configuration
              (Get_Generate_Block_Configuration (Bod), Bod);
            Synth_Generate_Statement_Body (Syn_Inst, Bod, Name);
            exit;
         end if;
         Gen := Get_Generate_Else_Clause (Gen);
         exit when Gen = Null_Node;
      end loop;
   end Synth_If_Generate_Statement;

   procedure Synth_For_Generate_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      Bod : constant Node := Get_Generate_Statement_Body (Stmt);
      Configs : constant Node := Get_Generate_Block_Configuration (Bod);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
      Config : Node;
      It_Rng : Type_Acc;
      Val : Valtyp;
      Name : Sname;
      Lname : Sname;
   begin
      if It_Type /= Null_Node then
         Synth_Subtype_Indication (Syn_Inst, It_Type);
      end if;

      --  Initial value.
      It_Rng := Get_Subtype_Object (Syn_Inst, Get_Type (Iterator));
      Val := Create_Value_Discrete (It_Rng.Drange.Left, It_Rng);

      Name := New_Sname_User (Get_Identifier (Stmt), Get_Sname (Syn_Inst));

      while In_Range (It_Rng.Drange, Read_Discrete (Val)) loop
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
         Lname := New_Sname_Version (Uns32 (Read_Discrete (Val)), Name);

         Synth_Generate_Statement_Body (Syn_Inst, Bod, Lname, Iterator, Val);
         Update_Index (It_Rng.Drange, Val);
      end loop;
   end Synth_For_Generate_Statement;

   procedure Synth_Concurrent_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node) is
   begin
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
            Synth_If_Generate_Statement (Syn_Inst, Stmt);
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
                     Synth_Blackbox_Instantiation_Statement (Syn_Inst, Stmt);
                  else
                     Synth_Component_Instantiation_Statement (Syn_Inst, Stmt);
                  end if;
               end;
               --  Un-apply configuration.
               Set_Component_Configuration (Stmt, Null_Node);
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
            Error_Kind ("synth_concurrent_statement", Stmt);
      end case;
   end Synth_Concurrent_Statement;

   procedure Synth_Concurrent_Statements
     (Syn_Inst : Synth_Instance_Acc; Stmts : Node)
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Is_Valid (Stmt) loop
         Synth_Concurrent_Statement (Syn_Inst, Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Synth_Concurrent_Statements;

   procedure Synth_Attribute_Formal (Syn_Inst : Synth_Instance_Acc;
                                     Val : Node;
                                     Id : Netlists.Gates.Formal_Module_Id)
   is
      Spec : constant Node := Get_Attribute_Specification (Val);
      Sig : constant Node := Get_Designated_Entity (Val);
      V : Valtyp;
   begin
      --  The type must be boolean
      if (Get_Base_Type (Get_Type (Val)) /=
            Vhdl.Std_Package.Boolean_Type_Definition)
      then
         Error_Msg_Synth (+Val, "type of attribute %i must be boolean",
                          (1 => +Get_Attribute_Designator (Spec)));
         return;
      end if;

      --  The designated entity must be a signal.
      if Get_Kind (Sig) /= Iir_Kind_Signal_Declaration then
         Error_Msg_Synth (+Val, "attribute %i only applies to signals",
                          (1 => +Get_Attribute_Designator (Spec)));
         return;
      end if;

      --  The value must be true
      V := Synth_Expression_With_Type
        (Syn_Inst, Get_Expression (Spec), Boolean_Type);
      if Read_Discrete (V) /= 1 then
         return;
      end if;

      declare
         Off : Value_Offsets;
         Voff : Net;
         Wd : Width;
         N : Net;
         Base : Valtyp;
         Typ : Type_Acc;
      begin
         Synth_Assignment_Prefix (Syn_Inst, Sig, Base, Typ, Off, Voff, Wd);
         pragma Assert (Off = (0, 0));
         pragma Assert (Voff = No_Net);
         pragma Assert (Base.Val.Kind = Value_Wire);
         pragma Assert (Base.Typ = Typ);

         N := Build_Formal_Input (Get_Build (Syn_Inst), Id, Typ.W);
         Add_Conc_Assign (Base.Val.W, N, 0, Val);
      end;

   end Synth_Attribute_Formal;

   procedure Synth_Attribute_Values
     (Syn_Inst : Synth_Instance_Acc; Unit : Node)
   is
      use Std_Names;
      use Netlists.Gates;

      Val : Node;
      Spec : Node;
      Id : Name_Id;
   begin
      Val := Get_Attribute_Value_Chain (Unit);
      while Val /= Null_Node loop
         Spec := Get_Attribute_Specification (Val);
         Id := Get_Identifier (Get_Attribute_Designator (Spec));
         case Id is
            when Name_Allconst =>
               Synth_Attribute_Formal (Syn_Inst, Val, Id_Allconst);
            when Name_Allseq =>
               Synth_Attribute_Formal (Syn_Inst, Val, Id_Allseq);
            when Name_Anyconst =>
               Synth_Attribute_Formal (Syn_Inst, Val, Id_Anyconst);
            when Name_Anyseq =>
               Synth_Attribute_Formal (Syn_Inst, Val, Id_Anyseq);
            when others =>
               Warning_Msg_Synth (+Spec, "unhandled attribute %i", (1 => +Id));
         end case;
         Val := Get_Value_Chain (Val);
      end loop;
   end Synth_Attribute_Values;

   procedure Synth_Verification_Unit
     (Syn_Inst : Synth_Instance_Acc; Unit : Node)
   is
      use Areapools;
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      Unit_Inst : Synth_Instance_Acc;
      Unit_Sname : Sname;
      M : Areapools.Mark_Type;
      Item : Node;
      Last_Type : Node;
   begin
      Unit_Sname := New_Sname_User (Get_Identifier (Unit),
                                    Get_Sname (Syn_Inst));
      Unit_Inst := Make_Instance (Syn_Inst, Unit, Unit_Sname);
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      Last_Type := Null_Node;
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock =>
               null;
            when Iir_Kind_Psl_Assert_Directive =>
               Synth_Psl_Assert_Directive (Unit_Inst, Item);
            when Iir_Kind_Psl_Assume_Directive =>
               Synth_Psl_Assume_Directive (Unit_Inst, Item);
            when Iir_Kind_Psl_Restrict_Directive =>
               Synth_Psl_Restrict_Directive (Unit_Inst, Item);
            when Iir_Kind_Psl_Cover_Directive =>
               Synth_Psl_Cover_Directive (Unit_Inst, Item);
            when Iir_Kind_Signal_Declaration
              | Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration
              | Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body
              | Iir_Kind_Attribute_Declaration
              | Iir_Kind_Attribute_Specification =>
               Synth_Declaration (Unit_Inst, Item, False, Last_Type);
            when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
               Synth_Concurrent_Statement (Unit_Inst, Item);
            when others =>
               Error_Kind ("synth_verification_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      Synth_Attribute_Values (Unit_Inst, Unit);

      Free_Instance (Unit_Inst);
      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;
   end Synth_Verification_Unit;
end Synth.Stmts;
