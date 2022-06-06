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

with Ada.Unchecked_Deallocation;

with Grt.Types; use Grt.Types;
with Grt.Algos;
with Grt.Severity; use Grt.Severity;
with Areapools;
with Name_Table;
with Std_Names;
with Errorout; use Errorout;
with Files_Map;
with Simple_IO;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Sem_Expr;
with Vhdl.Sem_Inst;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Evaluation;
with Vhdl.Ieee.Std_Logic_1164;

with PSL.Types;
with PSL.Nodes;
with PSL.Subsets;
with PSL.NFAs;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Heap;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;
with Elab.Debugger;

with Synth.Errors; use Synth.Errors;
with Synth.Vhdl_Decls; use Synth.Vhdl_Decls;
with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Vhdl_Insts; use Synth.Vhdl_Insts;
with Synth.Source;
with Synth.Vhdl_Static_Proc;
with Synth.Flags;
with Synth.Vhdl_Context; use Synth.Vhdl_Context;

with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;

package body Synth.Vhdl_Stmts is
   procedure Synth_Sequential_Statements
     (C : in out Seq_Context; Stmts : Node);

   procedure Set_Location (N : Net; Loc : Node)
     renames Synth.Source.Set_Location;

   function Synth_Waveform (Syn_Inst : Synth_Instance_Acc;
                            Wf : Node;
                            Targ_Type : Type_Acc) return Valtyp
   is
      Res : Valtyp;
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
         Res := Synth_Expression_With_Type
           (Syn_Inst, Get_We_Value (Wf), Targ_Type);
         Res := Synth_Subtype_Conversion
           (Get_Build (Syn_Inst), Res, Targ_Type, False, Wf);
         return Res;
      end if;
   end Synth_Waveform;

   procedure Synth_Assignment_Prefix (Syn_Inst : Synth_Instance_Acc;
                                      Pfx : Node;
                                      Dest_Base : out Valtyp;
                                      Dest_Typ : out Type_Acc;
                                      Dest_Off : out Value_Offsets;
                                      Dest_Dyn : out Dyn_Name) is
   begin
      case Get_Kind (Pfx) is
         when Iir_Kind_Simple_Name =>
            Synth_Assignment_Prefix (Syn_Inst, Get_Named_Entity (Pfx),
                                     Dest_Base, Dest_Typ, Dest_Off, Dest_Dyn);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            declare
               Targ : constant Valtyp := Get_Value (Syn_Inst, Pfx);
            begin
               Dest_Dyn := No_Dyn_Name;
               Dest_Typ := Targ.Typ;

               if Targ.Val.Kind = Value_Alias then
                  --  Replace alias by the aliased name.
                  Dest_Base := (Targ.Val.A_Typ, Targ.Val.A_Obj);
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
            Dest_Dyn := No_Dyn_Name;

         when Iir_Kind_Indexed_Name =>
            declare
               El_Typ : Type_Acc;
               Voff : Net;
               Off : Value_Offsets;
               Err : Boolean;
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx),
                  Dest_Base, Dest_Typ, Dest_Off, Dest_Dyn);
               Strip_Const (Dest_Base);
               Synth_Indexed_Name (Syn_Inst, Pfx, Dest_Typ,
                                   El_Typ, Voff, Off, Err);

               if Err then
                  Dest_Base := No_Valtyp;
               elsif Voff = No_Net then
                  --  Static index.
                  Dest_Off := Dest_Off + Off;
               else
                  --  Dynamic index.
                  if Dest_Dyn.Voff = No_Net then
                     --  The first one.
                     Dest_Dyn := (Pfx_Off => Dest_Off,
                                  Pfx_Typ => Dest_Typ,
                                  Voff => Voff);
                     Dest_Off := Off;
                  else
                     --  Nested one.
                     --  FIXME
                     Dest_Off := Dest_Off + Off;
                  --  if Dest_Off /= (0, 0) then
                  --     Error_Msg_Synth (+Pfx, "nested memory not supported");
                  --  end if;

                     Dest_Dyn.Voff := Build_Addidx
                       (Get_Build (Syn_Inst), Dest_Dyn.Voff, Voff);
                     Set_Location (Dest_Dyn.Voff, Pfx);
                  end if;
               end if;

               Dest_Typ := El_Typ;
            end;

         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Pfx));
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx),
                  Dest_Base, Dest_Typ, Dest_Off, Dest_Dyn);
               Dest_Off := Dest_Off + Dest_Typ.Rec.E (Idx + 1).Offs;

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
                  Dest_Base, Dest_Typ, Dest_Off, Dest_Dyn);
               Strip_Const (Dest_Base);

               Get_Onedimensional_Array_Bounds (Dest_Typ, Pfx_Bnd, El_Typ);
               Synth_Slice_Suffix (Syn_Inst, Pfx, Pfx_Bnd, El_Typ,
                                   Res_Bnd, Sl_Voff, Sl_Off);


               if Sl_Voff = No_Net then
                  --  Fixed slice.
                  Dest_Typ := Create_Onedimensional_Array_Subtype
                    (Dest_Typ, Res_Bnd, El_Typ);
                  Dest_Off.Net_Off := Dest_Off.Net_Off + Sl_Off.Net_Off;
                  Dest_Off.Mem_Off := Dest_Off.Mem_Off + Sl_Off.Mem_Off;
               else
                  --  Variable slice.
                  if Dest_Dyn.Voff = No_Net then
                     --  First one.
                     Dest_Dyn := (Pfx_Off => Dest_Off,
                                  Pfx_Typ => Dest_Typ,
                                  Voff => Sl_Voff);
                     Dest_Off := Sl_Off;
                  else
                     --  Nested.
                     if Dest_Off /= (0, 0) then
                        Error_Msg_Synth (+Pfx, "nested memory not supported");
                     end if;

                     Dest_Dyn.Voff := Build_Addidx
                       (Get_Build (Syn_Inst), Dest_Dyn.Voff, Sl_Voff);
                     Set_Location (Dest_Dyn.Voff, Pfx);
                  end if;
                  Dest_Typ := Create_Slice_Type (Res_Bnd.Len, El_Typ);
               end if;
            end;

         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            Synth_Assignment_Prefix
              (Syn_Inst, Get_Prefix (Pfx),
               Dest_Base, Dest_Typ, Dest_Off, Dest_Dyn);
            if Dest_Off /= (0, 0) and then Dest_Dyn.Voff /= No_Net then
               raise Internal_Error;
            end if;
            Dest_Base := Elab.Vhdl_Heap.Synth_Dereference
              (Read_Access (Dest_Base));
            Dest_Typ := Dest_Base.Typ;

         when others =>
            Error_Kind ("synth_assignment_prefix", Pfx);
      end case;
   end Synth_Assignment_Prefix;

   function Synth_Aggregate_Target_Type (Syn_Inst : Synth_Instance_Acc;
                                         Target : Node) return Type_Acc
   is
      Targ_Type : constant Node := Get_Type (Target);
      Base_Type : constant Node := Get_Base_Type (Targ_Type);
      Base_Typ : Type_Acc;
      Bnd : Bound_Type;
      Len : Uns32;
      Res : Type_Acc;
   begin
      Base_Typ := Get_Subtype_Object (Syn_Inst, Base_Type);
      --  It's a basetype, so not bounded.
      pragma Assert (Base_Typ.Kind = Type_Unbounded_Vector);

      if Is_Fully_Constrained_Type (Targ_Type) then
         --  If the aggregate subtype is known, just use it.
         Bnd := Vhdl_Expr.Synth_Array_Bounds (Syn_Inst, Targ_Type, 1);
      else
         --  Ok, so the subtype of the aggregate is not known, in general
         --  because the length of an element is not known.  That's with
         --  vhdl-2008.
         Len := 0;
         declare
            Choice : Node;
            El : Node;
            El_Typ : Type_Acc;
         begin
            Choice := Get_Association_Choices_Chain (Target);
            while Choice /= Null_Node loop
               pragma Assert (Get_Kind (Choice) = Iir_Kind_Choice_By_None);
               El := Get_Associated_Expr (Choice);
               El_Typ := Elab.Vhdl_Expr.Exec_Type_Of_Object (Syn_Inst, El);
               Bnd := Get_Array_Bound (El_Typ);
               Len := Len + Bnd.Len;
               Choice := Get_Chain (Choice);
            end loop;
         end;

         --  Compute the range.
         declare
            Idx_Type : constant Node := Get_Index_Type (Base_Type, 0);
            Idx_Typ : Type_Acc;
         begin
            Idx_Typ := Get_Subtype_Object (Syn_Inst, Idx_Type);
            Bnd := (Dir => Idx_Typ.Drange.Dir,
                    Left => Int32 (Idx_Typ.Drange.Left),
                    Right => 0,
                    Len => Len);
            case Bnd.Dir is
               when Dir_To =>
                  Bnd.Right := Bnd.Left + Int32 (Len);
               when Dir_Downto =>
                  Bnd.Right := Bnd.Left - Int32 (Len);
            end case;
         end;
      end if;

      --  Compute the type.
      case Base_Typ.Kind is
         when Type_Unbounded_Vector =>
            Res := Create_Vector_Type (Bnd, Base_Typ.Uarr_El);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Synth_Aggregate_Target_Type;

   function Synth_Target (Syn_Inst : Synth_Instance_Acc;
                          Target : Node) return Target_Info is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate =>
            return Target_Info'(Kind => Target_Aggregate,
                                Targ_Type => Synth_Aggregate_Target_Type
                                  (Syn_Inst, Target),
                                Aggr => Target);
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Dereference =>
            declare
               Base : Valtyp;
               Typ : Type_Acc;
               Off : Value_Offsets;

               Dyn : Dyn_Name;
            begin
               Synth_Assignment_Prefix (Syn_Inst, Target, Base, Typ, Off, Dyn);
               if Dyn.Voff = No_Net then
                  --  FIXME: check index.
                  return Target_Info'(Kind => Target_Simple,
                                      Targ_Type => Typ,
                                      Obj => Base,
                                      Off => Off);
               else
                  return Target_Info'(Kind => Target_Memory,
                                      Targ_Type => Typ,
                                      Mem_Obj => Base,
                                      Mem_Dyn => Dyn,
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
   function Aggregate_Extract (Ctxt : Context_Acc;
                               Val : Valtyp;
                               Off : Uns32;
                               Typ : Type_Acc;
                               Loc : Node) return Valtyp
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
                 (Ctxt, Get_Net (Ctxt, Val), Off * El_Typ.W, Typ.W);
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

   procedure Assign_Aggregate (Inst : Synth_Instance_Acc;
                               Target : Node;
                               Target_Typ : Type_Acc;
                               Val : Valtyp;
                               Loc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Targ_Bnd : constant Bound_Type := Get_Array_Bound (Target_Typ);
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
               Targ_Info := Synth_Target (Inst, Assoc);
               if Get_Element_Type_Flag (Choice) then
                  Pos := Pos - 1;
               else
                  Pos := Pos - Get_Array_Bound (Targ_Info.Targ_Type).Len;
               end if;
               Assign (Inst, Targ_Info,
                       Aggregate_Extract (Ctxt, Val, Pos,
                                          Targ_Info.Targ_Type, Assoc),
                       Loc);
            when others =>
               Error_Kind ("assign_aggregate", Choice);
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Assign_Aggregate;

   procedure Synth_Assignment_Aggregate is
      new Assign_Aggregate (Assign => Synth_Assignment);

   procedure Synth_Assignment_Simple (Syn_Inst : Synth_Instance_Acc;
                                      Targ : Valtyp;
                                      Off : Value_Offsets;
                                      Val : Valtyp;
                                      Loc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      W : Wire_Id;
      V : Valtyp;
   begin
      if Targ = No_Valtyp then
         --  There was an error.
         return;
      end if;

      if Targ.Val.Kind = Value_Alias then
         Synth_Assignment_Simple (Syn_Inst, (Targ.Val.A_Typ, Targ.Val.A_Obj),
                                  Off + Targ.Val.A_Off, Val, Loc);
         return;
      end if;

      V := Val;

      if Targ.Val.Kind = Value_Wire then
         W := Get_Value_Wire (Targ.Val);
         if Is_Static (V.Val)
           and then V.Typ.Sz = Targ.Typ.Sz
         then
            pragma Assert (Off = No_Value_Offsets);
            Phi_Assign_Static (W, Unshare (Get_Memtyp (V)));
         else
            if V.Typ.W = 0 then
               --  Forget about null wires.
               return;
            end if;
            Phi_Assign_Net (Ctxt, W, Get_Net (Ctxt, V), Off.Net_Off);
         end if;
      else
         if not Is_Static (V.Val) then
            --  Maybe the error message is too cryptic ?
            Error_Msg_Synth
              (+Loc, "cannot assign a net to a static value");
         else
            Copy_Memory (Targ.Val.Mem + Off.Mem_Off, Get_Memory (V), V.Typ.Sz);
         end if;
      end if;
   end Synth_Assignment_Simple;

   procedure Synth_Assignment_Memory (Syn_Inst : Synth_Instance_Acc;
                                      Targ_Base : Value_Acc;
                                      Targ_Poff : Uns32;
                                      Targ_Ptyp : Type_Acc;
                                      Targ_Voff : Net;
                                      Targ_Eoff : Uns32;
                                      Val : Valtyp;
                                      Loc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      W : constant Wire_Id := Get_Value_Wire (Targ_Base);
      N : Net;
   begin
      --  Get the whole memory.
      N := Get_Current_Assign_Value (Ctxt, W, Targ_Poff, Targ_Ptyp.W);
      --  Insert the new value.
      N := Build_Dyn_Insert
        (Ctxt, N, Get_Net (Ctxt, Val), Targ_Voff, Targ_Eoff);
      Set_Location (N, Loc);
      --  Write.
      Phi_Assign_Net (Ctxt, W, N, Targ_Poff);
   end Synth_Assignment_Memory;

   procedure Synth_Assignment (Syn_Inst : Synth_Instance_Acc;
                               Target : Target_Info;
                               Val : Valtyp;
                               Loc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      V : Valtyp;
   begin
      V := Synth_Subtype_Conversion (Ctxt, Val, Target.Targ_Type, False, Loc);
      pragma Unreferenced (Val);
      if V = No_Valtyp then
         --  In case of error.
         return;
      end if;

      case Target.Kind is
         when Target_Aggregate =>
            Synth_Assignment_Aggregate
              (Syn_Inst, Target.Aggr, Target.Targ_Type, V, Loc);
         when Target_Simple =>
            Synth_Assignment_Simple (Syn_Inst, Target.Obj, Target.Off, V, Loc);
         when Target_Memory =>
            Synth_Assignment_Memory
              (Syn_Inst, Target.Mem_Obj.Val,
               Target.Mem_Dyn.Pfx_Off.Net_Off, Target.Mem_Dyn.Pfx_Typ,
               Target.Mem_Dyn.Voff, Target.Mem_Doff,
               V, Loc);
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
                               Dyn : Dyn_Name;
                               Loc : Node) return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      N : Net;
   begin
      N := Get_Net (Ctxt, Obj);
      if Dyn.Voff /= No_Net then
         Synth.Source.Set_Location_Maybe (N, Loc);
         if Res_Typ.W /= 0 then
            --  Do not try to extract if the net is null.
            N := Build_Dyn_Extract (Ctxt, N, Dyn.Voff,
                                    Off + Dyn.Pfx_Off.Net_Off, Res_Typ.W);
         end if;
      else
         pragma Assert (not Is_Static (Obj.Val));
         N := Build2_Extract (Ctxt, N, Off, Res_Typ.W);
      end if;
      Set_Location (N, Loc);
      return Create_Value_Net (N, Res_Typ);
   end Synth_Read_Memory;

   function Synth_Read (Syn_Inst : Synth_Instance_Acc;
                        Targ : Target_Info;
                        Loc : Node) return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      N : Net;
   begin
      case Targ.Kind is
         when Target_Simple =>
            N := Build2_Extract (Ctxt, Get_Net (Ctxt, Targ.Obj),
                                 Targ.Off.Net_Off, Targ.Targ_Type.W);
            return Create_Value_Net (N, Targ.Targ_Type);
         when Target_Aggregate =>
            raise Internal_Error;
         when Target_Memory =>
            return Synth_Read_Memory (Syn_Inst, Targ.Mem_Obj, Targ.Targ_Type,
                                      0, Targ.Mem_Dyn, Loc);
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
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
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
         if Val = No_Valtyp then
            --  Mark the error, but try to continue.
            Set_Error (Syn_Inst);
         else
            V := Get_Net (Ctxt, Val);
            Cond := Get_Condition (Cwf);
            if Cond /= Null_Node then
               Cond_Val := Synth_Expression (Syn_Inst, Cond);
               if Cond_Val = No_Valtyp then
                  Cond_Net := Build_Const_UB32 (Ctxt, 0, 1);
               else
                  Cond_Net := Get_Net (Ctxt, Cond_Val);
               end if;

               V := Build_Mux2 (Ctxt, Cond_Net, No_Net, V);
               Set_Location (V, Cwf);
            end if;

            if Last /= No_Net then
               Inp := Get_Input (Get_Net_Parent (Last), 1);
               Connect (Inp, V);
            else
               First := V;
            end if;
            Last := V;
         end if;
         Cwf := Get_Chain (Cwf);
      end loop;
      if Cond /= Null_Node then
         pragma Assert (Last /= No_Net);
         Inp := Get_Input (Get_Net_Parent (Last), 1);
         if Get_Driver (Inp) = No_Net then
            --  No else.
            Val := Synth_Read (Syn_Inst, Targ, Stmt);
            Connect (Inp, Get_Net (Ctxt, Val));
         end if;
      end if;
      Val := Create_Value_Net (First, Targ.Targ_Type);
      Synth_Assignment (Syn_Inst, Targ, Val, Stmt);
   end Synth_Conditional_Signal_Assignment;

   procedure Synth_Variable_Assignment (Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Targ : Target_Info;
      Val : Valtyp;
   begin
      Targ := Synth_Target (Inst, Get_Target (Stmt));
      Val := Synth_Expression_With_Type
        (Inst, Get_Expression (Stmt), Targ.Targ_Type);
      if Val = No_Valtyp then
         Set_Error (Inst);
         return;
      end if;
      Synth_Assignment (Inst, Targ, Val, Stmt);
   end Synth_Variable_Assignment;

   procedure Synth_Conditional_Variable_Assignment
     (Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Target : constant Node := Get_Target (Stmt);
      Targ_Type : Type_Acc;
      Cond : Node;
      Ce : Node;
      Val, Cond_Val : Valtyp;
      V, Last : Net;
      First : Valtyp;
      Cond_Tri : Tri_State_Type;
   begin
      Targ_Type := Get_Subtype_Object (Inst, Get_Type (Target));
      First := No_Valtyp;
      Last := No_Net;
      Ce := Get_Conditional_Expression_Chain (Stmt);
      while Ce /= Null_Node loop
         --  First, evaluate the condition.
         Cond := Get_Condition (Ce);
         if Cond /= Null_Node then
            Cond_Val := Synth_Expression (Inst, Cond);
            if Is_Static_Val (Cond_Val.Val) then
               Strip_Const (Cond_Val);
               if Read_Discrete (Get_Value_Memtyp (Cond_Val)) = 1 then
                  Cond_Tri := True;
               else
                  Cond_Tri := False;
               end if;
            else
               Cond_Tri := Unknown;
            end if;
         else
            Cond_Tri := True;
         end if;

         if Cond_Tri /= False then
            Val := Synth_Expression_With_Type
              (Inst, Get_Expression (Ce), Targ_Type);
            --  Convert to the target subtype so that all the conditional
            --  expressions have the same width.
            Val := Synth_Subtype_Conversion (Ctxt, Val, Targ_Type, False, Ce);

            if Cond_Tri = True and then First = No_Valtyp then
               --  This is the first and only value.
               --  If the value is static, it stays static.
               First := Val;
               exit;
            else
               V := Get_Net (Ctxt, Val);
               if Cond_Tri = Unknown then
                  --  Note: as one input of the mux2 is not connected, there
                  --  is no check on inputs width.
                  V := Build_Mux2 (Ctxt, Get_Net (Ctxt, Cond_Val), No_Net, V);
                  Set_Location (V, Ce);
               end if;

               if First = No_Valtyp then
                  First := Create_Value_Net (V, Targ_Type);
               else
                  Connect (Get_Input (Get_Net_Parent (Last), 1), V);
               end if;
               Last := V;
            end if;
         else
            --  The condition is true, so do not evaluate the value.
            pragma Assert (Cond /= Null_Node);
            null;
         end if;

         --  No need to go farther if the condition is true.
         exit when Cond_Tri = True;

         Ce := Get_Chain (Ce);
      end loop;
      Synth_Assignment (Inst, Target, First, Stmt);
   end Synth_Conditional_Variable_Assignment;

   procedure Synth_If_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Cond : constant Node := Get_Condition (Stmt);
      Els : constant Node := Get_Else_Clause (Stmt);
      Ctxt : constant Context_Acc := Get_Build (C.Inst);
      Cond_Val : Valtyp;
      Cond_Net : Net;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      Cond_Val := Synth_Expression (C.Inst, Cond);
      if Cond_Val = No_Valtyp then
         Set_Error (C.Inst);
         return;
      end if;
      if Is_Static_Val (Cond_Val.Val) then
         Strip_Const (Cond_Val);
         if Read_Discrete (Get_Value_Memtyp (Cond_Val)) = 1 then
            --  True.
            Synth_Sequential_Statements
              (C, Get_Sequential_Statement_Chain (Stmt));
         else
            pragma Assert (Read_Discrete (Get_Value_Memtyp (Cond_Val)) = 0);
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
         --  The statements for the 'then' part.
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

         Cond_Net := Get_Net (Ctxt, Cond_Val);
         Merge_Phis (Ctxt, Cond_Net, Phi_True, Phi_False, Get_Location (Stmt));
      end if;
   end Synth_If_Statement;

   type Alternative_Index is new Int32;

   --  Only keep '0' and '1' in choices for std_logic.
   function Ignore_Choice_Logic (V : Ghdl_U8; Loc : Node) return Boolean is
   begin
      case V is
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_0_Pos
            | Vhdl.Ieee.Std_Logic_1164.Std_Logic_1_Pos =>
            return False;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_L_Pos
            | Vhdl.Ieee.Std_Logic_1164.Std_Logic_H_Pos =>
            Warning_Msg_Synth
              (+Loc, "choice with 'L' or 'H' value is ignored");
            return True;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_U_Pos
            | Vhdl.Ieee.Std_Logic_1164.Std_Logic_X_Pos
            | Vhdl.Ieee.Std_Logic_1164.Std_Logic_D_Pos
            | Vhdl.Ieee.Std_Logic_1164.Std_Logic_Z_Pos
            | Vhdl.Ieee.Std_Logic_1164.Std_Logic_W_Pos =>
            Warning_Msg_Synth (+Loc, "choice with meta-value is ignored");
            return True;
         when others =>
            --  Only 9 values.
            raise Internal_Error;
      end case;
   end Ignore_Choice_Logic;

   function Ignore_Choice_Expression (V : Valtyp; Loc : Node) return Boolean is
   begin
      case V.Typ.Kind is
         when Type_Bit =>
            return False;
         when Type_Logic =>
            if V.Typ = Logic_Type then
               return Ignore_Choice_Logic (Read_U8 (V.Val.Mem), Loc);
            else
               return False;
            end if;
         when Type_Discrete =>
            return False;
         when Type_Vector =>
            if V.Typ.Arr_El = Logic_Type then
               for I in 1 .. Size_Type (V.Typ.Abound.Len) loop
                  if Ignore_Choice_Logic (Read_U8 (V.Val.Mem + (I - 1)), Loc)
                  then
                     return True;
                  end if;
               end loop;
               return False;
            else
               return False;
            end if;
         when Type_Array =>
            return False;
         when others =>
            raise Internal_Error;
      end case;
   end Ignore_Choice_Expression;

   --  Create the condition for choices of CHOICE chain belonging to the same
   --  alternative.  Update CHOICE to the next alternative.
   procedure Synth_Choice (Syn_Inst : Synth_Instance_Acc;
                           Sel : Net;
                           Choice_Typ : Type_Acc;
                           Nets : in out Net_Array;
                           Other_Choice : in out Nat32;
                           Choice_Idx : in out Nat32;
                           Choice : in out Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Cond : Net;
      Res : Net;
   begin
      Res := No_Net;
      loop
         case Iir_Kinds_Case_Choice (Get_Kind (Choice)) is
            when Iir_Kind_Choice_By_Expression =>
               declare
                  V : Valtyp;
               begin
                  V := Synth_Expression_With_Basetype
                    (Syn_Inst, Get_Choice_Expression (Choice));
                  V := Synth_Subtype_Conversion
                    (Ctxt, V, Choice_Typ, False, Choice);
                  if Ignore_Choice_Expression (V, Choice) then
                     Cond := No_Net;
                  else
                     Cond := Build_Compare
                       (Ctxt, Id_Eq, Sel, Get_Net (Ctxt, V));
                     Set_Location (Cond, Choice);
                  end if;
               end;

            when Iir_Kind_Choice_By_Range =>
               declare
                  Rng : Discrete_Range_Type;
                  Cmp_L, Cmp_R : Module_Id;
                  L, R : Net;
               begin
                  Synth_Discrete_Range
                    (Syn_Inst, Get_Choice_Range (Choice), Rng);

                  if Rng.Is_Signed then
                     case Rng.Dir is
                        when Dir_To =>
                           Cmp_L := Id_Sge;
                           Cmp_R := Id_Sle;
                        when Dir_Downto =>
                           Cmp_L := Id_Sle;
                           Cmp_R := Id_Sge;
                     end case;
                     L := Build2_Const_Int (Ctxt, Rng.Left, Choice_Typ.W);
                     R := Build2_Const_Int (Ctxt, Rng.Right, Choice_Typ.W);
                  else
                     case Rng.Dir is
                        when Dir_To =>
                           Cmp_L := Id_Uge;
                           Cmp_R := Id_Ule;
                        when Dir_Downto =>
                           Cmp_L := Id_Ule;
                           Cmp_R := Id_Uge;
                     end case;
                     L := Build2_Const_Uns
                       (Ctxt, Uns64 (Rng.Left), Choice_Typ.W);
                     R := Build2_Const_Uns
                       (Ctxt, Uns64 (Rng.Right), Choice_Typ.W);
                  end if;

                  L := Build_Compare (Ctxt, Cmp_L, Sel, L);
                  Set_Location (L, Choice);

                  R := Build_Compare (Ctxt, Cmp_R, Sel, R);
                  Set_Location (R, Choice);

                  Cond := Build_Dyadic (Ctxt, Id_And, L, R);
                  Set_Location (Cond, Choice);
               end;

            when Iir_Kind_Choice_By_Others =>
               --  Last and only one.
               pragma Assert (Res = No_Net);
               Other_Choice := Choice_Idx + 1;
               pragma Assert (Get_Chain (Choice) = Null_Node);
               Choice := Null_Node;
               return;
         end case;

         if not Get_Same_Alternative_Flag (Choice) then
            --  First choice.
            Choice_Idx := Choice_Idx + 1;
            Res := Cond;
         else
            if Cond = No_Net then
               --  No new condition.
               null;
            else
               if Res /= No_Net then
                  Res := Build_Dyadic (Ctxt, Id_Or, Res, Cond);
                  Set_Location (Res, Choice);
               else
                  Res := Cond;
               end if;
            end if;
         end if;

         Choice := Get_Chain (Choice);
         exit when Choice = Null_Node
           or else not Get_Same_Alternative_Flag (Choice);
      end loop;
      if Res = No_Net then
         Res := Build_Const_UB32 (Ctxt, 0, 1);
      end if;
      Nets (Choice_Idx) := Res;
   end Synth_Choice;

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

   --  Count the number of wires used in all the alternatives.
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

   --  Fill ARR from wire_id of ALTS.
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

   type Seq_Assign_Value_Array_Acc is access Seq_Assign_Value_Array;
   procedure Free_Seq_Assign_Value_Array is new Ada.Unchecked_Deallocation
     (Seq_Assign_Value_Array, Seq_Assign_Value_Array_Acc);

   function Is_Assign_Value_Array_Static
     (Wid : Wire_Id; Arr : Seq_Assign_Value_Array) return Memtyp
   is
      Res : Memtyp;
      Prev_Val : Memtyp;
   begin
      Prev_Val := Null_Memtyp;
      for I in Arr'Range loop
         case Arr (I).Is_Static is
            when False =>
               --  A value is not static.
               return Null_Memtyp;
            when Unknown =>
               if Prev_Val = Null_Memtyp then
                  --  First use of previous value.
                  if Get_Kind (Wid) /= Wire_Variable
                    or else not Is_Static_Wire (Wid)
                  then
                     --  The previous value is not static.
                     return Null_Memtyp;
                  end if;
                  Prev_Val := Get_Static_Wire (Wid);
                  if Res /= Null_Memtyp then
                     --  There is already a result.
                     if not Is_Equal (Res, Prev_Val) then
                        --  The previous value is different from the result.
                        return Null_Memtyp;
                     end if;
                  else
                     Res := Prev_Val;
                  end if;
               end if;
            when True =>
               if Res = Null_Memtyp then
                  --  First value.  Keep it.
                  Res := Arr (I).Val;
               else
                  if not Is_Equal (Res, Arr (I).Val) then
                     --  Value is different.
                     return  Null_Memtyp;
                  end if;
               end if;
         end case;
      end loop;
      return Res;
   end Is_Assign_Value_Array_Static;

   procedure Synth_Case_Statement_Dynamic
     (C : in out Seq_Context; Stmt : Node; Sel : Valtyp)
   is
      use Vhdl.Sem_Expr;
      Ctxt : constant Context_Acc := Get_Build (C.Inst);

      Choices : constant Node := Get_Case_Statement_Alternative_Chain (Stmt);

      Case_Info : Choice_Info_Type;

      --  Array of alternatives
      Alts : Alternative_Data_Acc;
      Alt_Idx : Alternative_Index;
      Others_Alt_Idx : Alternative_Index;

      Nbr_Choices : Nat32;

      Pasgns : Seq_Assign_Value_Array_Acc;
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
      --Fill_Choices_Array (Case_Info, Choices);

      --  Allocate structures.
      --  Because there is no 1-1 link between choices and alternatives,
      --  create an array for the choices and an array for the alternatives.
      Alts := new Alternative_Data_Array
        (1 .. Alternative_Index (Case_Info.Nbr_Alternatives));

      --  Compute number of non-default alternatives.
      Nbr_Choices := Nat32 (Case_Info.Nbr_Alternatives);
      if Case_Info.Others_Choice /= Null_Node then
         Nbr_Choices := Nbr_Choices - 1;
      end if;

      Nets := new Net_Array (1 .. Int32 (Alts'Last));

      Sel_Net := Get_Net (Ctxt, Sel);

      --  Synth statements and keep list of assignments.
      --  Also synth choices.
      declare
         Choice : Node;
         Choice_Idx, Other_Choice : Nat32;
         Phi : Phi_Type;
      begin
         Alt_Idx := 0;
         Choice_Idx := 0;
         Other_Choice := 0;

         Choice := Choices;
         while Is_Valid (Choice) loop
            --  Must be a choice for a new alternative.
            pragma Assert (not Get_Same_Alternative_Flag (Choice));

            --  A new sequence of statements.
            Alt_Idx := Alt_Idx + 1;

            Push_Phi;
            Synth_Sequential_Statements (C, Get_Associated_Chain (Choice));
            Pop_Phi (Phi);
            Alts (Alt_Idx).Asgns := Sort_Phi (Phi);

            Synth_Choice (C.Inst, Sel_Net, Sel.Typ,
                          Nets.all, Other_Choice, Choice_Idx, Choice);
         end loop;
         pragma Assert (Choice_Idx = Nbr_Choices);
         Others_Alt_Idx := Alternative_Index (Other_Choice);
      end;

      --  Create the one-hot vector.
      if Nbr_Choices = 0 then
         Sel_Net := No_Net;
      else
         Sel_Net := Build2_Concat (Ctxt, Nets (1 .. Nbr_Choices));
      end if;

      --  Create list of wire_id, sort it.
      Nbr_Wires := Count_Wires_In_Alternatives (Alts.all);
      Wires := new Wire_Id_Array (1 .. Nbr_Wires);
      Fill_Wire_Id_Array (Wires.all, Alts.all);
      Sort_Wire_Id_Array (Wires.all);

      --  Associate each choice with the assign node
      --  For each wire_id:
      --    Build mux2/mux4 tree (group by 4)
      Pasgns := new Seq_Assign_Value_Array (1 .. Int32 (Alts'Last));

      --  For each wire, compute the result.
      for I in Wires'Range loop
         declare
            Wi : constant Wire_Id := Wires (I);
            Last_Val : Net;
            Res_Inst : Instance;
            Res : Net;
            Default : Net;
            Min_Off, Off : Uns32;
            Wd : Width;
            List : Partial_Assign_List;
            Sval : Memtyp;
         begin
            --  Extract the value for each branch.
            for I in Alts'Range loop
               --  If there is an assignment to Wi in Alt, it will define the
               --  value.
               if Get_Wire_Id (Alts (I).Asgns) = Wi then
                  Pasgns (Int32 (I)) :=
                    Get_Seq_Assign_Value (Alts (I).Asgns);
                  Alts (I).Asgns := Get_Assign_Chain (Alts (I).Asgns);
               else
                  Pasgns (Int32 (I)) := (Is_Static => Unknown);
               end if;
            end loop;

            --  If:
            --  1) All present values in PASGNS are static
            --  2) There is no missing values *or* the previous value is
            --     static.
            --  3) The default value is unused *or* it is static
            --  4) All the values are equal.
            --  then assign directly.
            Sval := Is_Assign_Value_Array_Static (Wi, Pasgns.all);
            if Sval /= Null_Memtyp then
               --  Use static assignment.
               Phi_Assign_Static (Wi, Sval);
            else
               --  Compute the final value for each partial part of the wire.
               Partial_Assign_Init (List);
               Min_Off := 0;
               loop
                  Off := Min_Off;

                  --  Extract value of partial assignments to NETS.
                  Extract_Merge_Partial_Assigns
                    (Ctxt, Pasgns.all, Nets.all, Off, Wd);
                  exit when Off = Uns32'Last and Wd = Width'Last;

                  --  If a branch has no value, use the value before the case.
                  --  Also do it for the default value!
                  Last_Val := No_Net;
                  for I in Nets'Range loop
                     if Nets (I) = No_Net then
                        if Last_Val = No_Net then
                           Last_Val := Get_Current_Assign_Value
                             (Ctxt, Wi, Off, Wd);
                        end if;
                        Nets (I) := Last_Val;
                     end if;
                  end loop;

                  --  Extract default value (for missing alternative).
                  if Others_Alt_Idx /= 0 then
                     Default := Nets (Int32 (Others_Alt_Idx));
                  else
                     Default := Build_Const_X (Ctxt, Wd);
                  end if;

                  if Nbr_Choices = 0 then
                     Res := Default;
                  else
                     Res := Build_Pmux (Ctxt, Sel_Net, Default);
                     Res_Inst := Get_Net_Parent (Res);
                     Set_Location (Res_Inst, Get_Location (Stmt));

                     for I in 1 .. Nbr_Choices loop
                        Connect
                          (Get_Input (Res_Inst, Port_Nbr (2 + I - Nets'First)),
                           Nets (I));
                     end loop;
                  end if;

                  Partial_Assign_Append (List, New_Partial_Assign (Res, Off));
                  Min_Off := Off + Wd;
               end loop;

               Merge_Partial_Assigns (Ctxt, Wi, List);
            end if;
         end;
      end loop;

      --  free.
      Free_Wire_Id_Array (Wires);
      Free_Alternative_Data_Array (Alts);
      Free_Seq_Assign_Value_Array (Pasgns);
      Free_Net_Array (Nets);
   end Synth_Case_Statement_Dynamic;

   function Execute_Static_Case_Statement_Array
     (Inst : Synth_Instance_Acc; Stmt : Node; Sel : Valtyp) return Node
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
               Sel_Val := Synth_Expression_With_Basetype (Inst, Sel_Expr);
               if Is_Equal (Sel_Val, Sel) then
                  return Stmts;
               end if;
            when Iir_Kind_Choice_By_Others =>
               return Stmts;
            when others =>
               raise Internal_Error;
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Execute_Static_Case_Statement_Array;

   function Execute_Static_Case_Statement_Scalar
     (Inst : Synth_Instance_Acc; Stmt : Node; Sel : Int64) return Node
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
                  return Stmts;
               end if;
            when Iir_Kind_Choice_By_Others =>
               return Stmts;
            when Iir_Kind_Choice_By_Range =>
               declare
                  Bnd : Discrete_Range_Type;
                  Is_In : Boolean;
               begin
                  Synth_Discrete_Range (Inst, Get_Choice_Range (Choice), Bnd);
                  case Bnd.Dir is
                     when Dir_To =>
                        Is_In := Sel >= Bnd.Left and Sel <= Bnd.Right;
                     when Dir_Downto =>
                        Is_In := Sel <= Bnd.Left and Sel >= Bnd.Right;
                  end case;
                  if Is_In then
                     return Stmts;
                  end if;
               end;
            when others =>
               raise Internal_Error;
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Execute_Static_Case_Statement_Scalar;

   function Execute_Static_Case_Statement
     (Inst : Synth_Instance_Acc; Stmt : Node; Sel : Valtyp) return Node is
   begin
      case Sel.Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete =>
            return Execute_Static_Case_Statement_Scalar
              (Inst, Stmt, Read_Discrete (Sel));
         when Type_Vector
           | Type_Array =>
            return Execute_Static_Case_Statement_Array (Inst, Stmt, Sel);
         when others =>
            raise Internal_Error;
      end case;
   end Execute_Static_Case_Statement;

   procedure Synth_Case_Statement (C : in out Seq_Context; Stmt : Node)
   is
      Expr : constant Node := Get_Expression (Stmt);
      Sel : Valtyp;
      Stmts : Node;
   begin
      Sel := Synth_Expression_With_Basetype (C.Inst, Expr);
      Strip_Const (Sel);
      if Is_Static (Sel.Val) then
         Stmts := Execute_Static_Case_Statement (C.Inst, Stmt, Sel);
         Synth_Sequential_Statements (C, Stmts);
      else
         Synth_Case_Statement_Dynamic (C, Stmt, Sel);
      end if;
   end Synth_Case_Statement;

   procedure Synth_Selected_Signal_Assignment
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      use Vhdl.Sem_Expr;
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);

      Expr : constant Node := Get_Expression (Stmt);
      Choices : constant Node := Get_Selected_Waveform_Chain (Stmt);

      Targ : Target_Info;
      Targ_Type : Type_Acc;

      Case_Info : Choice_Info_Type;

      --  Array of alternatives
      Alts : Alternative_Data_Acc;
      Alt_Idx : Alternative_Index;
      Others_Alt_Idx : Alternative_Index;

      --  Array of choices.  Contains tuple of (Value, Alternative).
      Nbr_Choices : Nat32;

      Nets : Net_Array_Acc;


      Sel : Valtyp;
      Sel_Net : Net;
   begin
      Targ := Synth_Target (Syn_Inst, Get_Target (Stmt));
      Targ_Type := Targ.Targ_Type;

      --  Create a net for the expression.
      Sel := Synth_Expression_With_Basetype (Syn_Inst, Expr);
      Sel_Net := Get_Net (Ctxt, Sel);

      --  Count choices and alternatives.
      Count_Choices (Case_Info, Choices);
      --  Fill_Choices_Array (Case_Info, Choices);

      --  Allocate structures.
      --  Because there is no 1-1 link between choices and alternatives,
      --  create an array for the choices and an array for the alternatives.
      Alts := new Alternative_Data_Array
        (1 .. Alternative_Index (Case_Info.Nbr_Alternatives));

      --  Compute number of non-default alternatives.
      Nbr_Choices := Nat32 (Case_Info.Nbr_Alternatives);
      if Case_Info.Others_Choice /= Null_Node then
         Nbr_Choices := Nbr_Choices - 1;
      end if;

      Nets := new Net_Array (1 .. Nbr_Choices);

      --  Synth statements, extract choice value.
      declare
         Choice, Wf : Node;
         Val : Valtyp;
         Choice_Idx, Other_Choice : Nat32;
      begin
         Alt_Idx := 0;
         Choice_Idx := 0;
         Other_Choice := 0;

         Choice := Choices;
         while Is_Valid (Choice) loop
            pragma Assert (not Get_Same_Alternative_Flag (Choice));

            Wf := Get_Associated_Chain (Choice);
            Val := Synth_Waveform (Syn_Inst, Wf, Targ_Type);

            Alt_Idx := Alt_Idx + 1;
            Alts (Alt_Idx).Val := Get_Net (Ctxt, Val);

            Synth_Choice (Syn_Inst, Sel_Net, Sel.Typ,
                          Nets.all, Other_Choice, Choice_Idx, Choice);
         end loop;
         pragma Assert (Choice_Idx = Nbr_Choices);
         Others_Alt_Idx := Alternative_Index (Other_Choice);
      end;

      --  Create the one-hot vector.
      if Nbr_Choices = 0 then
         Sel_Net := No_Net;
      else
         Sel_Net := Build2_Concat (Ctxt, Nets (1 .. Nbr_Choices));
      end if;

      declare
         Res : Net;
         Res_Inst : Instance;
         Default : Net;
      begin
         --  Extract default value (for missing alternative).
         if Others_Alt_Idx /= 0 then
            Default := Alts (Others_Alt_Idx).Val;
         else
            Default := Build_Const_X (Ctxt, Targ_Type.W);
         end if;

         if Nbr_Choices = 0 then
            Res := Default;
         else
            Res := Build_Pmux (Ctxt, Sel_Net, Default);
            Res_Inst := Get_Net_Parent (Res);
            Set_Location (Res_Inst, Get_Location (Stmt));

            for I in 1 .. Nbr_Choices loop
               Connect
                 (Get_Input (Res_Inst, Port_Nbr (2 + I - Nets'First)),
                  Alts (Alternative_Index (I)).Val);
            end loop;
         end if;

         Synth_Assignment
           (Syn_Inst, Targ, Create_Value_Net (Res, Targ_Type), Stmt);
      end;

      --  free.
      Free_Alternative_Data_Array (Alts);
      Free_Net_Array (Nets);
   end Synth_Selected_Signal_Assignment;

   function Synth_Label (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
                         return Sname
   is
      Label : constant Name_Id := Get_Label (Stmt);
   begin
      if Label = Null_Identifier then
         return No_Sname;
      else
         return New_Sname_User (Label, Get_Sname (Syn_Inst));
      end if;
   end Synth_Label;

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
               --  Compare by identifier, as INTER can be the generic
               --  interface, while FORMAL is the instantiated one.
               if Get_Identifier (Formal) = Get_Identifier (Inter) then
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

   function Info_To_Valtyp (Info : Target_Info) return Valtyp is
   begin
      case Info.Kind is
         when Target_Simple =>
            if Info.Off = No_Value_Offsets then
               return Info.Obj;
            else
               return Create_Value_Alias (Info.Obj, Info.Off, Info.Targ_Type);
            end if;
         when Target_Aggregate =>
            raise Internal_Error;
         when Target_Memory =>
            return Create_Value_Dyn_Alias (Info.Mem_Obj.Val,
                                           Info.Mem_Dyn.Pfx_Off.Net_Off,
                                           Info.Mem_Dyn.Pfx_Typ,
                                           Info.Mem_Dyn.Voff,
                                           Info.Mem_Doff,
                                           Info.Targ_Type);
      end case;
   end Info_To_Valtyp;

   procedure Synth_Subprogram_Associations (Subprg_Inst : Synth_Instance_Acc;
                                            Caller_Inst : Synth_Instance_Acc;
                                            Init : Association_Iterator_Init)
   is
      Ctxt : constant Context_Acc := Get_Build (Caller_Inst);
      Inter : Node;
      Inter_Type : Type_Acc;
      Assoc : Node;
      Actual : Node;
      Val : Valtyp;
      Iterator : Association_Iterator;
      Info : Target_Info;
   begin
      Set_Instance_Const (Subprg_Inst, True);

      --  Process in INTER order.
      Association_Iterate_Init (Iterator, Init);
      loop
         Association_Iterate_Next (Iterator, Inter, Assoc);
         exit when Inter = Null_Node;

         Inter_Type := Get_Subtype_Object (Subprg_Inst, Get_Type (Inter));

         case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration =>
               pragma Assert (Get_Mode (Inter) = Iir_In_Mode);
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
            when Iir_Kind_Interface_Variable_Declaration =>
               --  Always pass by value.
               Actual := Get_Actual (Assoc);
               Info := Synth_Target (Caller_Inst, Actual);
               if Is_Copyback_Parameter (Inter) then
                  Create_Object (Caller_Inst, Assoc, Info_To_Valtyp (Info));
               end if;
               if Info.Kind /= Target_Memory
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
               Actual := Get_Actual (Assoc);
               Info := Synth_Target (Caller_Inst, Actual);
               if Info.Kind = Target_Memory then
                  raise Internal_Error;
               end if;
               Val := Create_Value_Alias
                 (Info.Obj, Info.Off, Info.Targ_Type);
            when Iir_Kind_Interface_File_Declaration =>
               Actual := Get_Actual (Assoc);
               Info := Synth_Target (Caller_Inst, Actual);
               Val := Info.Obj;
            when Iir_Kind_Interface_Quantity_Declaration =>
               raise Internal_Error;
         end case;

         if Val = No_Valtyp then
            Set_Error (Subprg_Inst);
            return;
         end if;

         --  FIXME: conversion only for constants, reshape for all.
         case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_Variable_Declaration =>
               if Get_Mode (Inter) /= Iir_Out_Mode then
                  --  Always passed by value
                  Val := Synth_Subtype_Conversion
                    (Ctxt, Val, Inter_Type, True, Assoc);
               else
                  --  Use default value ?
                  null;
               end if;
            when Iir_Kind_Interface_Signal_Declaration =>
               --  LRM08 4.2.2.3 Signal parameters
               --  If an actual signal is associated with a signal parameter
               --  of mode IN or INOUT, and if the type of the formal is a
               --  scalar type, then it is an error if the subtype of the
               --  actual is not compatible with the subtype of the formal.
               --  Similarly, if an actual signal is associated with a signal
               --  parameter of mode OUT or INOUT, and if the type of the
               --  actual is a scalar type, then it is an error if the subtype
               --  of the formal is not compatible with the subtype of the
               --  actual.
               if Get_Kind (Get_Type (Inter)) in
                 Iir_Kinds_Scalar_Type_And_Subtype_Definition
               then
                  if Get_Mode (Inter) in Iir_In_Modes then
                     if not Is_Scalar_Subtype_Compatible (Val.Typ, Inter_Type)
                     then
                        Error_Msg_Synth
                          (+Actual,
                           "scalar subtype of actual is not compatible with "
                             & "signal formal interface");
                        Val := No_Valtyp;
                     end if;
                  end if;
                  if Get_Mode (Inter) in Iir_Out_Modes then
                     if not Is_Scalar_Subtype_Compatible (Inter_Type, Val.Typ)
                     then
                        Error_Msg_Synth
                          (+Actual,
                           "signal formal interface scalar subtype is not "
                             & "compatible with of actual subtype");
                        Val := No_Valtyp;
                     end if;
                  end if;
               else
                  --  Check matching.
                  --  This is equivalent to subtype conversion for non-scalar
                  --  types.
                  Val := Synth_Subtype_Conversion
                    (Ctxt, Val, Inter_Type, True, Assoc);
               end if;
            when Iir_Kind_Interface_File_Declaration =>
               null;
            when Iir_Kind_Interface_Quantity_Declaration =>
               raise Internal_Error;
         end case;

         if Val = No_Valtyp then
            --  Error after conversion.
            Set_Error (Subprg_Inst);
            return;
         end if;

         if Get_Instance_Const (Subprg_Inst) and then not Is_Static (Val.Val)
         then
            Set_Instance_Const (Subprg_Inst, False);
         end if;

         case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration =>
               --  Pass by copy.
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
   end Synth_Subprogram_Associations;

   procedure Synth_Subprogram_Association (Subprg_Inst : Synth_Instance_Acc;
                                           Caller_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node)
   is
      Init : Association_Iterator_Init;
   begin
      Init := Association_Iterator_Build (Inter_Chain, Assoc_Chain);
      Synth_Subprogram_Associations (Subprg_Inst, Caller_Inst, Init);
   end Synth_Subprogram_Association;

   --  Create wires for out and inout interface variables.
   procedure Synth_Subprogram_Association_Wires
     (Subprg_Inst : Synth_Instance_Acc; Init : Association_Iterator_Init)
   is
      Ctxt : constant Context_Acc := Get_Build (Subprg_Inst);
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
            Wire := Alloc_Wire (Wire_Variable, (Inter, Val.Typ));
            Set_Wire_Gate (Wire, Get_Net (Ctxt, Val));

            Val := Create_Value_Wire (Wire, Val.Typ);
            Create_Object_Force (Subprg_Inst, Inter, No_Valtyp);
            Create_Object_Force (Subprg_Inst, Inter, Val);
         end if;
      end loop;
   end Synth_Subprogram_Association_Wires;

   procedure Synth_Subprogram_Back_Association
     (Subprg_Inst : Synth_Instance_Acc;
      Caller_Inst : Synth_Instance_Acc;
      Inter_Chain : Node;
      Assoc_Chain : Node)
   is
      Inter : Node;
      Assoc : Node;
      Assoc_Inter : Node;
      Val : Valtyp;
      Targ : Valtyp;
      W : Wire_Id;
      D : Destroy_Type;
   begin
      Destroy_Init (D, Caller_Inst);
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);

         if Is_Copyback_Parameter (Inter) then
            if not Get_Whole_Association_Flag (Assoc) then
               raise Internal_Error;
            end if;
            Targ := Get_Value (Caller_Inst, Assoc);
            Val := Get_Value (Subprg_Inst, Inter);
            if Targ.Val.Kind = Value_Dyn_Alias then
               Synth_Assignment_Memory
                 (Caller_Inst, Targ.Val.D_Obj,
                  Targ.Val.D_Poff, Targ.Val.D_Ptyp,
                  Get_Value_Dyn_Alias_Voff (Targ.Val), Targ.Val.D_Eoff,
                  Val, Assoc);
            else
               Synth_Assignment_Simple
                 (Caller_Inst, Targ, No_Value_Offsets, Val, Assoc);
            end if;

            --  Free wire used for out/inout interface variables.
            if Val.Val.Kind = Value_Wire then
               W := Get_Value_Wire (Val.Val);
               Phi_Discard_Wires (W, No_Wire_Id);
               Free_Wire (W);
            end if;

            Destroy_Object (D, Assoc);
         end if;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
      Destroy_Finish (D);
   end Synth_Subprogram_Back_Association;

   function Build_Control_Signal (Syn_Inst : Synth_Instance_Acc;
                                  W : Width;
                                  Loc : Source.Syn_Src) return Net
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Res : Net;
   begin
      Res := Build_Signal (Ctxt, New_Internal_Name (Ctxt), W);
      Set_Location (Res, Loc);
      return Res;
   end Build_Control_Signal;

   function Synth_Dynamic_Subprogram_Call (Syn_Inst : Synth_Instance_Acc;
                                           Sub_Inst : Synth_Instance_Acc;
                                           Call : Node;
                                           Init : Association_Iterator_Init)
                                          return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Call);
      Is_Func : constant Boolean := Is_Function_Declaration (Imp);
      Bod : constant Node := Vhdl.Sem_Inst.Get_Subprogram_Body_Origin (Imp);
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
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

      C.W_En := Alloc_Wire (Wire_Variable, (Imp, Bit_Type));
      C.W_Ret := Alloc_Wire (Wire_Variable, (Imp, Bit_Type));

      if Is_Func then
         C.W_Val := Alloc_Wire (Wire_Variable, (Imp, null));
      end if;

      --  Create a phi so that all assignments are gathered.
      Push_Phi;

      Synth_Subprogram_Association_Wires (Sub_Inst, Init);

      if Is_Func then
         --  Set a default value for the return.
         C.Ret_Typ := Get_Subtype_Object (Syn_Inst, Get_Return_Type (Imp));

         Set_Wire_Gate (C.W_Val,
                        Build_Control_Signal (Sub_Inst, C.Ret_Typ.W, Imp));
         C.Ret_Init := Build_Const_X (Ctxt, C.Ret_Typ.W);
         Phi_Assign_Net (Ctxt, C.W_Val, C.Ret_Init, 0);
      end if;

      Set_Wire_Gate
        (C.W_En, Build_Control_Signal (Sub_Inst, 1, Imp));
      Phi_Assign_Static (C.W_En, Bit1);

      Set_Wire_Gate
        (C.W_Ret, Build_Control_Signal (Sub_Inst, 1, Imp));
      Phi_Assign_Static (C.W_Ret, Bit1);

      Vhdl_Decls.Synth_Declarations
        (C.Inst, Get_Declaration_Chain (Bod), True);
      if not Is_Error (C.Inst) then
         Synth_Sequential_Statements (C, Get_Sequential_Statement_Chain (Bod));
      end if;

      if Is_Error (C.Inst) then
         Res := No_Valtyp;
      else
         if Is_Func then
            if C.Nbr_Ret = 0 then
               Error_Msg_Synth
                 (+Bod, "missing return statement at end of function");
               Res := No_Valtyp;
            elsif C.Nbr_Ret = 1 and then Is_Static (C.Ret_Value.Val) then
               Res := C.Ret_Value;
            else
               Res := Create_Value_Net
                 (Get_Current_Value (Ctxt, C.W_Val), C.Ret_Value.Typ);
            end if;
         else
            Res := No_Valtyp;
            Synth_Subprogram_Back_Association
              (C.Inst, Syn_Inst, Init.Inter_Chain, Init.Assoc_Chain);
         end if;
      end if;

      Pop_Phi (Subprg_Phi);

      Vhdl_Decls.Finalize_Declarations
        (C.Inst, Get_Declaration_Chain (Bod), True);

      --  Propagate assignments.
      --  Wires that have been created for this subprogram will be destroyed.
      --  But assignment for outer wires (passed through parameters) have
      --  to be kept.  We cannot merge phi because this won't be allowed for
      --  local wires.
      Propagate_Phi_Until_Mark (Ctxt, Subprg_Phi, Wire_Mark);

      --  Free wires.
      --  These wires are currently unassigned because they were created
      --  within the Phi.
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
                                          Call     : Node;
                                          Bod      : Node;
                                          Init : Association_Iterator_Init)
                                         return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Call);
      Is_Func : constant Boolean := Is_Function_Declaration (Imp);
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

      Synth_Declarations (C.Inst, Get_Declaration_Chain (Bod), True);

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
            Synth_Subprogram_Back_Association
              (C.Inst, Syn_Inst, Init.Inter_Chain, Init.Assoc_Chain);
         end if;
      end if;

      Vhdl_Decls.Finalize_Declarations
        (C.Inst, Get_Declaration_Chain (Bod), True);

      return Res;
   end Synth_Static_Subprogram_Call;

   function Synth_Subprogram_Call_Instance (Inst : Synth_Instance_Acc;
                                            Imp : Node;
                                            Bod : Node)
                                           return Synth_Instance_Acc
   is
      Res : Synth_Instance_Acc;
      Up_Inst : Synth_Instance_Acc;
   begin
      Up_Inst := Get_Instance_By_Scope (Inst, Get_Parent_Scope (Imp));
      Res := Make_Elab_Instance (Up_Inst, Bod, Config => Null_Node);
      Set_Caller_Instance (Res, Inst);
      return Res;
   end Synth_Subprogram_Call_Instance;

   function Synth_Subprogram_Call (Syn_Inst : Synth_Instance_Acc;
                                   Call : Node;
                                   Init : Association_Iterator_Init)
                                  return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Imp  : constant Node := Get_Implementation (Call);
      Is_Func : constant Boolean := Is_Function_Declaration (Imp);
      Bod : constant Node := Vhdl.Sem_Inst.Get_Subprogram_Body_Origin (Imp);
      Area_Mark : Areapools.Mark_Type;
      Res : Valtyp;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Areapools.Mark (Area_Mark, Instance_Pool.all);

      Sub_Inst := Synth_Subprogram_Call_Instance (Syn_Inst, Imp, Bod);
      if Ctxt /= null then
         Set_Extra (Sub_Inst, Syn_Inst, New_Internal_Name (Ctxt));
      end if;

      Synth_Subprogram_Associations (Sub_Inst, Syn_Inst, Init);

      if Is_Error (Sub_Inst) then
         Res := No_Valtyp;
      else
         if not Is_Func then
            if Ctxt /= null and then Get_Purity_State (Imp) /= Pure then
               Set_Instance_Const (Sub_Inst, False);
            end if;
         end if;

         if Get_Instance_Const (Sub_Inst) then
            Res := Synth_Static_Subprogram_Call
              (Syn_Inst, Sub_Inst, Call, Bod, Init);
         else
            Res := Synth_Dynamic_Subprogram_Call
              (Syn_Inst, Sub_Inst, Call, Init);
         end if;
      end if;

      --  Propagate error.
      if Is_Error (Sub_Inst) then
         Set_Error (Syn_Inst);
      end if;

      if Elab.Debugger.Flag_Need_Debug then
         Elab.Debugger.Debug_Leave (Sub_Inst);
      end if;

      Free_Instance (Sub_Inst);
      Areapools.Release (Area_Mark, Instance_Pool.all);

      return Res;
   end Synth_Subprogram_Call;

   function Synth_Subprogram_Call
     (Syn_Inst : Synth_Instance_Acc; Call : Node) return Valtyp
   is
      Imp : constant Node := Get_Implementation (Call);

      --  The corresponding body (for a package instantiation, this could be
      --  the shared body of the uninstantiated package).
      Bod : constant Node := Vhdl.Sem_Inst.Get_Subprogram_Body_Origin (Imp);

      --  Get the subprogram declaration of the subprogram body.
      --  Usually, IMP = IMP2, unless of shared generic packages.
      Imp2 : constant Node := Get_Subprogram_Specification (Bod);

      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);

      --  Use the interfaces corresponding to the body.
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp2);
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
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Imp  : constant Node := Get_Implementation (Call);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Init : constant Association_Iterator_Init :=
        Association_Iterator_Build (Inter_Chain, Assoc_Chain);
      Area_Mark : Areapools.Mark_Type;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Areapools.Mark (Area_Mark, Instance_Pool.all);
      Sub_Inst := Make_Elab_Instance (Syn_Inst, Imp, Null_Node);

      if Ctxt /= null then
         Set_Extra (Sub_Inst, Syn_Inst, New_Internal_Name (Ctxt));
      end if;

      Synth_Subprogram_Associations (Sub_Inst, Syn_Inst, Init);

      Synth.Vhdl_Static_Proc.Synth_Static_Procedure (Sub_Inst, Imp, Call);

      Synth_Subprogram_Back_Association
        (Sub_Inst, Syn_Inst, Init.Inter_Chain, Init.Assoc_Chain);

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

   --  Return True iff WID is a static wire and its value is V.
   function Is_Static_Bit (Wid : Wire_Id; V : Ghdl_U8) return Boolean
   is
      M : Memtyp;
   begin
      pragma Assert (Get_Kind (Wid) = Wire_Variable);
      if not Is_Static_Wire (Wid) then
         return False;
      end if;
      M := Get_Static_Wire (Wid);
      return Read_U8 (M) = V;
   end Is_Static_Bit;

   function Is_Static_Bit0 (Wid : Wire_Id) return Boolean is
   begin
      return Is_Static_Bit (Wid, 0);
   end Is_Static_Bit0;

   function Is_Static_Bit1 (Wid : Wire_Id) return Boolean is
   begin
      return Is_Static_Bit (Wid, 1);
   end Is_Static_Bit1;

   pragma Inline (Is_Static_Bit0);
   pragma Inline (Is_Static_Bit1);

   procedure Loop_Control_Init (C : Seq_Context; Stmt : Node)
   is
      Lc : constant Loop_Context_Acc := C.Cur_Loop;
   begin
      --  We might create new wires that will be destroy at the end of the
      --  loop.  Use mark and sweep to control their lifetime.
      Mark (C.Cur_Loop.Wire_Mark);

      if Lc.Prev_Loop /= null and then Lc.Prev_Loop.Need_Quit then
         --  An exit or next statement that targets an outer loop may suspend
         --  the execution of this loop.
         Lc.W_Quit := Alloc_Wire (Wire_Variable, (Lc.Loop_Stmt, Bit_Type));
         Set_Wire_Gate (Lc.W_Quit, Build_Control_Signal (C.Inst, 1, Stmt));
         Phi_Assign_Static (Lc.W_Quit, Bit1);
      end if;

      if Get_Exit_Flag (Stmt) or else Get_Next_Flag (Stmt) then
         --  There is an exit or next statement that target this loop.
         --  We need to save W_En, as if the execution is suspended due to
         --  exit or next, it will resume at the end of the loop.
         if Is_Static_Wire (C.W_En) then
            pragma Assert (Is_Static_Bit1 (C.W_En));
            Lc.Saved_En := No_Net;
         else
            Lc.Saved_En := Get_Current_Value (null, C.W_En);
         end if;
         --  Subloops may be suspended if there is an exit or a next statement
         --  for this loop within subloops.
         Lc.Need_Quit := True;
      end if;

      if Get_Exit_Flag (Stmt) then
         --  There is an exit statement for this loop.  Create the wire.
         Lc.W_Exit := Alloc_Wire (Wire_Variable, (Lc.Loop_Stmt, Bit_Type));
         Set_Wire_Gate (Lc.W_Exit, Build_Control_Signal (C.Inst, 1, Stmt));
         Phi_Assign_Static (Lc.W_Exit, Bit1);
      end if;
   end Loop_Control_Init;

   procedure Loop_Control_And_Start (Is_Net : out Boolean;
                                 S      : out Boolean;
                                 N      : out Net;
                                 En     : Net) is
   begin
      if En = No_Net then
         Is_Net := False;
         N := No_Net;
         S := True;
      else
         Is_Net := True;
         N := En;
         S := True;
      end if;
   end Loop_Control_And_Start;

   procedure Loop_Control_And (C : Seq_Context;
                               Is_Net : in out Boolean;
                               S      : in out Boolean;
                               N      : in out Net;
                               R : Wire_Id)
   is
      Res : Net;
   begin
      if R = No_Wire_Id or else Is_Static_Bit1 (R) then
         --  No change.
         return;
      end if;

      if Is_Static_Bit0 (R) then
         --  Stays 0.
         Is_Net := False;
         S := False;
         N := No_Net;
         return;
      end if;

      if not Is_Net and then not S then
         --  Was 0, remains 0.
         return;
      end if;

      pragma Assert (Is_Net or else S);

      --  Optimize common cases.
      Res := Get_Current_Value (null, R);

      if Is_Net then
         N := Build_Dyadic (Get_Build (C.Inst), Id_And, N, Res);
         Set_Location (N, C.Cur_Loop.Loop_Stmt);
      else
         N := Res;
      end if;

      Is_Net := True;
   end Loop_Control_And;

   procedure Loop_Control_And_Assign (C : Seq_Context;
                                      Is_Net : Boolean;
                                      S      : Boolean;
                                      N      : Net;
                                      W      : Wire_Id) is
   begin
      if Is_Net then
         Phi_Assign_Net (Get_Build (C.Inst), W, N, 0);
      else
         if S then
            Phi_Assign_Static (W, Bit1);
         else
            Phi_Assign_Static (W, Bit0);
         end if;
      end if;
   end Loop_Control_And_Assign;

   procedure Loop_Control_Update (C : Seq_Context)
   is
      Lc : constant Loop_Context_Acc := C.Cur_Loop;
      N  : Net;
      S  : Boolean;
      Is_Net : Boolean;
   begin
      if not Lc.Need_Quit then
         --  No next/exit statement for this loop.  So no control.
         return;
      end if;

      --  Execution continue iff:
      --  1. Loop was enabled (Lc.Saved_En)
      Loop_Control_And_Start (Is_Net, S, N, Lc.Saved_En);

      --  2. No return (C.W_Ret)
      Loop_Control_And (C, Is_Net, S, N, C.W_Ret);

      --  3. No exit.
      Loop_Control_And (C, Is_Net, S, N, Lc.W_Exit);

      --  4. No quit.
      Loop_Control_And (C, Is_Net, S, N, Lc.W_Quit);

      Loop_Control_And_Assign (C, Is_Net, S, N, C.W_En);
   end Loop_Control_Update;

   procedure Loop_Control_Finish (C : Seq_Context)
   is
      Lc : constant Loop_Context_Acc := C.Cur_Loop;
      N   : Net;
      S  : Boolean;
      Is_Net : Boolean;
   begin
      --  Execution continue after this loop iff:
      --  1. Loop was enabled (Lc.Saved_En)
      Loop_Control_And_Start (Is_Net, S, N, Lc.Saved_En);

      --  2. No return (C.W_Ret)
      Loop_Control_And (C, Is_Net, S, N, C.W_Ret);

      --  3. No quit (C.W_Quit)
      Loop_Control_And (C, Is_Net, S, N, Lc.W_Quit);

      Phi_Discard_Wires (Lc.W_Quit, Lc.W_Exit);

      if Lc.W_Quit /= No_Wire_Id then
         Free_Wire (Lc.W_Quit);
      end if;

      if Lc.W_Exit /= No_Wire_Id then
         Free_Wire (Lc.W_Exit);
      end if;

      Release (C.Cur_Loop.Wire_Mark);

      Loop_Control_And_Assign (C, Is_Net, S, N, C.W_En);
   end Loop_Control_Finish;

   procedure Synth_Dynamic_Exit_Next_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (C.Inst);
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
            --  Create a branch for the True case.
            Push_Phi;
         end if;
      end if;

      --  Execution is suspended for the current sequence of statements.
      Phi_Assign_Static (C.W_En, Bit0);

      Lc := C.Cur_Loop;

      --  Compute the loop statement indicated by the exit/next statement.
      Loop_Label := Get_Loop_Label (Stmt);
      if Loop_Label = Null_Node then
         Loop_Label := Lc.Loop_Stmt;
      else
         Loop_Label := Get_Named_Entity (Loop_Label);
      end if;

      --  Update the W_Exit and W_Quit flags for the loops.  All the loops
      --  until the label are canceled.
      loop
         if Lc.Loop_Stmt = Loop_Label then
            --  Final loop.
            if Is_Exit then
               Phi_Assign_Static (Lc.W_Exit, Bit0);
            end if;
            exit;
         else
            Phi_Assign_Static (Lc.W_Quit, Bit0);
         end if;
         Lc := Lc.Prev_Loop;
      end loop;

      if Cond /= Null_Node and not Static_Cond then
         Pop_Phi (Phi_True);

         --  If the condition is false, do nothing.
         Push_Phi;
         Pop_Phi (Phi_False);

         Merge_Phis (Ctxt, Get_Net (Ctxt, Cond_Val), Phi_True, Phi_False,
                     Get_Location (Stmt));
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
         if Cond_Val = No_Valtyp then
            Set_Error (C.Inst);
            return;
         end if;
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

   procedure Init_For_Loop_Statement (Inst : Synth_Instance_Acc;
                                      Stmt : Node;
                                      Val : out Valtyp)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
      It_Rng : Type_Acc;
   begin
      if It_Type /= Null_Node then
         Synth_Subtype_Indication (Inst, It_Type);
      end if;

      --  Initial value.
      It_Rng := Get_Subtype_Object (Inst, Get_Type (Iterator));
      Val := Create_Value_Discrete (It_Rng.Drange.Left, It_Rng);
      Create_Object (Inst, Iterator, Val);
   end Init_For_Loop_Statement;

   procedure Finish_For_Loop_Statement (Inst : Synth_Instance_Acc;
                                        Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
      D : Destroy_Type;
   begin
      Destroy_Init (D, Inst);
      Destroy_Object (D, Iterator);
      if It_Type /= Null_Node then
         Destroy_Object (D, It_Type);
      end if;
      Destroy_Finish (D);
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

      Init_For_Loop_Statement (C.Inst, Stmt, Val);

      while In_Range (Val.Typ.Drange, Read_Discrete (Val)) loop
         Synth_Sequential_Statements (C, Stmts);

         Update_Index (Val.Typ.Drange, Val);
         Loop_Control_Update (C);

         --  Constant exit.
         exit when Is_Static_Bit0 (C.W_En);

         --  FIXME: dynamic exits.
      end loop;
      Loop_Control_Finish (C);

      Finish_For_Loop_Statement (C.Inst, Stmt);

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

      Init_For_Loop_Statement (C.Inst, Stmt, Val);

      while In_Range (Val.Typ.Drange, Read_Discrete (Val)) loop
         Synth_Sequential_Statements (C, Stmts);
         C.S_En := True;

         Update_Index (Val.Typ.Drange, Val);

         exit when Lc.S_Exit or Lc.S_Quit or C.Nbr_Ret > 0;
      end loop;

      Finish_For_Loop_Statement (C.Inst, Stmt);

      C.Cur_Loop := Lc.Prev_Loop;
   end Synth_Static_For_Loop_Statement;

   procedure Synth_Dynamic_While_Loop_Statement
     (C : in out Seq_Context; Stmt : Node)
   is
      Stmts : constant Node := Get_Sequential_Statement_Chain (Stmt);
      Cond : constant Node := Get_Condition (Stmt);
      Val : Valtyp;
      Lc : aliased Loop_Context (Mode_Dynamic);
      Iter_Nbr : Natural;
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

      Iter_Nbr := 0;

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
         exit when Lc.W_Exit /= No_Wire_Id and then Is_Static_Bit0 (Lc.W_Exit);
         exit when C.W_Ret /= No_Wire_Id and then Is_Static_Bit0 (C.W_Ret);
         exit when Lc.W_Quit /= No_Wire_Id and then Is_Static_Bit0 (Lc.W_Quit);

         Iter_Nbr := Iter_Nbr + 1;
         if Iter_Nbr > Flags.Flag_Max_Loop and Flags.Flag_Max_Loop /= 0 then
            Error_Msg_Synth
              (+Stmt, "maximum number of iterations (%v) reached",
               +Uns32 (Flags.Flag_Max_Loop));
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
      Ctxt : constant Context_Acc := Get_Build (C.Inst);
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

         Val := Synth_Subtype_Conversion (Ctxt, Val, C.Ret_Typ, True, Stmt);

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
            Phi_Assign_Net (Ctxt, C.W_Val, Get_Net (Ctxt, Val), 0);
         end if;
      end if;

      if Is_Dyn then
         --  The subprogram has returned.  Do not execute further statements.
         Phi_Assign_Static (C.W_En, Bit0);

         if C.W_Ret /= No_Wire_Id then
            Phi_Assign_Static (C.W_Ret, Bit0);
         end if;
      end if;

      C.Nbr_Ret := C.Nbr_Ret + 1;
   end Synth_Return_Statement;

   procedure Synth_Static_Report (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
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
         Rep := Synth_Expression_With_Basetype (Syn_Inst, Rep_Expr);
         if Rep = No_Valtyp then
            Set_Error (Syn_Inst);
            return;
         end if;
         Strip_Const (Rep);
      end if;
      if Sev_Expr /= Null_Node then
         Sev := Synth_Expression (Syn_Inst, Sev_Expr);
         if Sev = No_Valtyp then
            Set_Error (Syn_Inst);
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
         when Note_Severity =>
            Put_Err ("note");
         when Warning_Severity =>
            Put_Err ("warning");
         when Error_Severity =>
            Put_Err ("error");
         when Failure_Severity =>
            Put_Err ("failure");
         when others =>
            Put_Err ("??");
      end case;
      Put_Err ("): ");

      if Rep = No_Valtyp then
         Put_Line_Err ("Assertion violation");
      else
         Put_Line_Err (Value_To_String (Rep));
      end if;

      if Sev_V >= Flags.Severity_Level then
         Error_Msg_Synth (+Stmt, "error due to assertion failure");
         Elab.Debugger.Debug_Error (Syn_Inst, Stmt);
      end if;
   end Synth_Static_Report;

   procedure Execute_Report_Statement (Inst : Synth_Instance_Acc;
                                       Stmt : Node) is
   begin
      Synth_Static_Report (Inst, Stmt);
   end Execute_Report_Statement;

   --  Return True if EXPR can be evaluated with static values.
   --  Does not need to be fully accurate, used for report/assert messages.
   function Is_Static_Expr (Inst : Synth_Instance_Acc;
                            Expr : Node) return Boolean is
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            return Is_Static_Expr (Inst, Get_Left (Expr))
              and then Is_Static_Expr (Inst, Get_Right (Expr));
         when Iir_Kind_Image_Attribute =>
            return Is_Static_Expr (Inst, Get_Parameter (Expr));
         when Iir_Kind_Instance_Name_Attribute
            | Iir_Kinds_Literal
            | Iir_Kind_Enumeration_Literal =>
            return True;
         when Iir_Kind_Length_Array_Attribute =>
            --  Attributes on types can be evaluated.
            return True;
         when Iir_Kind_Simple_Name =>
            return Is_Static_Expr (Inst, Get_Named_Entity (Expr));
         when others =>
            Error_Kind ("is_static_expr", Expr);
            return False;
      end case;
   end Is_Static_Expr;

   procedure Synth_Dynamic_Report_Statement (Inst : Synth_Instance_Acc;
                                             Stmt : Node;
                                             Is_Cond : Boolean)
   is
      Rep_Expr : constant Node := Get_Report_Expression (Stmt);
      Sev_Expr : constant Node := Get_Severity_Expression (Stmt);
   begin
      if not Is_Cond
        and then Is_Static_Expr (Inst, Rep_Expr)
        and then (Sev_Expr = Null_Node
                    or else Is_Static_Expr (Inst, Sev_Expr))
      then
         Synth_Static_Report (Inst, Stmt);
      end if;
   end Synth_Dynamic_Report_Statement;

   procedure Execute_Assertion_Statement (Inst : Synth_Instance_Acc;
                                          Stmt : Node)
   is
      Cond : Valtyp;
   begin
      Cond := Synth_Expression (Inst, Get_Assertion_Condition (Stmt));
      if Cond = No_Valtyp then
         Set_Error (Inst);
         return;
      end if;
      pragma Assert (Is_Static (Cond.Val));
      Strip_Const (Cond);
      if Read_Discrete (Cond) = 1 then
         return;
      end if;
      Synth_Static_Report (Inst, Stmt);
   end Execute_Assertion_Statement;

   procedure Synth_Dynamic_Assertion_Statement (C : Seq_Context; Stmt : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (C.Inst);
      Loc : constant Location_Type := Get_Location (Stmt);
      Cond : Valtyp;
      N : Net;
      En : Net;
      Inst : Instance;
   begin
      if not Flags.Flag_Formal then
         return;
      end if;

      Cond := Synth_Expression (C.Inst, Get_Assertion_Condition (Stmt));
      if Cond = No_Valtyp then
         Set_Error (C.Inst);
         return;
      end if;
      N := Get_Net (Ctxt, Cond);
      En := Phi_Enable (Ctxt, (Stmt, Bit_Type), Bit0, Bit1,
                        Get_Location (Stmt));
      if En /= No_Net then
         --  Build: En -> Cond
         N := Build2_Imp (Ctxt, En, N, Loc);
      end if;
      Inst := Build_Assert (Ctxt, Synth_Label (C.Inst, Stmt), N);
      Set_Location (Inst, Loc);
   end Synth_Dynamic_Assertion_Statement;

   procedure Synth_Sequential_Statements
     (C : in out Seq_Context; Stmts : Node)
   is
      Is_Dyn : constant Boolean := not Get_Instance_Const (C.Inst);
      Ctxt : constant Context_Acc := Get_Build (C.Inst);
      Stmt : Node;
      Phi_T, Phi_F : Phi_Type;
      Has_Phi : Boolean;
   begin
      Stmt := Stmts;
      while Is_Valid (Stmt) loop
         if Is_Dyn then
            pragma Assert (not Is_Static_Bit0 (C.W_En));
            Has_Phi := not Is_Static_Bit1 (C.W_En);
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
         if Elab.Debugger.Flag_Need_Debug then
            Elab.Debugger.Debug_Break (C.Inst, Stmt);
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
                  Execute_Report_Statement (C.Inst, Stmt);
               else
                  --  Not executed.
                  --  Depends on the execution path: the report statement may
                  --  be conditionally executed.
                  Synth_Dynamic_Report_Statement (C.Inst, Stmt, True);
               end if;
            when Iir_Kind_Assertion_Statement =>
               if not Is_Dyn then
                  Execute_Assertion_Statement (C.Inst, Stmt);
               else
                  Synth_Dynamic_Assertion_Statement (C, Stmt);
               end if;
            when Iir_Kind_Exit_Statement
              | Iir_Kind_Next_Statement =>
               if Is_Dyn then
                  Synth_Dynamic_Exit_Next_Statement (C, Stmt);
               else
                  Synth_Static_Exit_Next_Statement (C, Stmt);
               end if;
            when Iir_Kind_Wait_Statement =>
               Error_Msg_Synth
                 (+Stmt, "wait statement not allowed for synthesis");
            when others =>
               Error_Kind ("synth_sequential_statements", Stmt);
         end case;
         if Is_Dyn then
            if Has_Phi then
               Pop_Phi (Phi_T);
               Push_Phi;
               Pop_Phi (Phi_F);
               Merge_Phis (Ctxt, Get_Current_Value (Ctxt, C.W_En),
                           Phi_T, Phi_F, Get_Location (Stmt));
            end if;
            if Is_Static_Bit0 (C.W_En) then
               --  Not more execution.
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
      Ctxt : constant Context_Acc := Get_Build (C.Inst);
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

      Merge_Phis (Ctxt, Get_Net (Ctxt, Cond_Val), Phi_True, Phi_False,
                  Get_Location (Stmt));
   end Synth_Process_Sequential_Statements;

   procedure Synth_Process_Statement
     (Syn_Inst : Synth_Instance_Acc; Proc : Node)
   is
      use Areapools;
      Label : constant Name_Id := Get_Identifier (Proc);
      Decls_Chain : constant Node := Get_Declaration_Chain (Proc);
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      M : Areapools.Mark_Type;
      C_Sname : Sname;
      C : Seq_Context (Mode_Dynamic);
   begin
      if Label = Null_Identifier then
         C_Sname := New_Internal_Name (Ctxt, Get_Sname (Syn_Inst));
      else
         C_Sname := New_Sname_User (Label, Get_Sname (Syn_Inst));
      end if;
      C := (Mode => Mode_Dynamic,
            Inst => Make_Instance (Syn_Inst, Proc, C_Sname),
            Cur_Loop => null,
            W_En => Alloc_Wire (Wire_Variable, (Proc, Bit_Type)),
            W_Ret => No_Wire_Id,
            W_Val => No_Wire_Id,
            Ret_Init => No_Net,
            Ret_Value => No_Valtyp,
            Ret_Typ => null,
            Nbr_Ret => 0);

      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      Push_Phi;

      Synth_Declarations (C.Inst, Decls_Chain);

      Set_Wire_Gate (C.W_En, Build_Control_Signal (Syn_Inst, 1, Proc));
      Phi_Assign_Static (C.W_En, Bit1);

      if not Is_Error (C.Inst) then
         case Iir_Kinds_Process_Statement (Get_Kind (Proc)) is
            when Iir_Kind_Sensitized_Process_Statement =>
               Synth_Sequential_Statements
                 (C, Get_Sequential_Statement_Chain (Proc));
               --  FIXME: check sensitivity list.
            when Iir_Kind_Process_Statement =>
               Synth_Process_Sequential_Statements (C, Proc);
         end case;
      end if;

      Pop_And_Merge_Phi (Ctxt, Get_Location (Proc));

      Finalize_Declarations (C.Inst, Decls_Chain);

      Free_Instance (C.Inst);
      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;

      Finalize_Assignment (Ctxt, C.W_En);
      Free_Wire (C.W_En);
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
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
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
            Synth_Static_Report (Syn_Inst, Stmt);
         end if;
         return;
      end if;

      if not Flags.Flag_Formal then
         --  Ignore the net.
         return;
      end if;

      Inst := Build_Assert
        (Ctxt, Synth_Label (Syn_Inst, Stmt), Get_Net (Ctxt, Val));
      Set_Location (Inst, Get_Location (Stmt));
   end Synth_Concurrent_Assertion_Statement;

   procedure Synth_Block_Statement (Syn_Inst : Synth_Instance_Acc; Blk : Node)
   is
      use Areapools;
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      Blk_Inst : constant Synth_Instance_Acc :=
        Get_Sub_Instance (Syn_Inst, Blk);
      Decls_Chain : constant Node := Get_Declaration_Chain (Blk);
      Blk_Sname : Sname;
      M : Areapools.Mark_Type;
   begin
      Blk_Sname := New_Sname_User (Get_Identifier (Blk), Get_Sname (Syn_Inst));
      Set_Extra (Blk_Inst, Syn_Inst, Blk_Sname);
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      Synth_Concurrent_Declarations (Blk_Inst, Decls_Chain);
      Synth_Concurrent_Statements
        (Blk_Inst, Get_Concurrent_Statement_Chain (Blk));

      Synth_Attribute_Values (Blk_Inst, Blk);

      Finalize_Declarations (Blk_Inst, Decls_Chain);

      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;
   end Synth_Block_Statement;

   function Synth_Psl_NFA (Syn_Inst : Synth_Instance_Acc;
                           NFA : PSL.Types.PSL_NFA;
                           Nbr_States : Int32;
                           States : Net;
                           Loc : Source.Syn_Src) return Net
   is
      use PSL.NFAs;
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
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
         I := Build_Extract_Bit (Ctxt, States, Uns32 (S_Num));
         Set_Location (I, Loc);

         --  For each edge:
         E := Get_First_Src_Edge (S);
         while E /= No_Edge loop
            --  Edge condition.
            Cond := Build_Dyadic
              (Ctxt, Id_And,
               I, Synth_PSL_Expression (Syn_Inst, Get_Edge_Expr (E)));
            Set_Location (Cond, Loc);

            --  TODO: if EOS is present, then this is a live state.

            --  Reverse order for final concatenation.
            D_Num := Nbr_States - 1 - Get_State_Label (Get_Edge_Dest (E));
            if D_Arr (D_Num) /= No_Net then
               Cond := Build_Dyadic (Ctxt, Id_Or, D_Arr (D_Num), Cond);
               Set_Location (Cond, Loc);
            end if;
            D_Arr (D_Num) := Cond;

            E := Get_Next_Src_Edge (E);
         end loop;

         S := Get_Next_State (S);
      end loop;

      --  Maybe there is no edge to the first state (common for restrict).
      if D_Arr (Nbr_States - 1) = No_Net then
         D_Arr (Nbr_States - 1) := Build_Const_UB32 (Ctxt, 0, 1);
      end if;

      --  Maybe there is no edge to the final state.
      if D_Arr (0) = No_Net then
         D_Arr (0) := Build_Const_UB32 (Ctxt, 0, 1);
      end if;

      Concat_Array (Ctxt, D_Arr.all, Res);
      Free_Net_Array (D_Arr);

      return Res;
   end Synth_Psl_NFA;

   procedure Synth_Psl_Dff (Syn_Inst : Synth_Instance_Acc;
                            Stmt : Node;
                            Next_States : out Net)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Nbr_States : constant Int32 := Get_PSL_Nbr_States (Stmt);
      Has_Async_Abort : Boolean;
      States : Net;
      Init : Net;
      Rst  : Net;
      Clk : Net;
      Clk_Inst : Instance;
   begin
      --  create init net, clock net
      Init := Build_Const_UB32 (Ctxt, 1, Uns32 (Nbr_States));
      Set_Location (Init, Stmt);
      Clk := Synth_PSL_Expression (Syn_Inst, Get_PSL_Clock (Stmt));

      --  Check the clock is an edge and extract it.
      Clk_Inst := Get_Net_Parent (Clk);
      if Get_Id (Clk_Inst) not in Edge_Module_Id then
         Error_Msg_Synth (+Stmt, "clock is not an edge");
         Next_States := No_Net;
         return;
      end if;

      Rst := No_Net;
      Has_Async_Abort := False;
      if Get_Kind (Stmt) in Iir_Kinds_Psl_Property_Directive
        and then Get_PSL_Abort_Flag (Stmt)
      then
         declare
            use PSL.Types;
            use PSL.Subsets;
            use PSL.Nodes;
            Abort_Prop : constant PSL_Node := Get_Psl_Property (Stmt);
         begin
            Rst := Synth_PSL_Expression (Syn_Inst, Get_Boolean (Abort_Prop));
            Has_Async_Abort := Is_Async_Abort (Abort_Prop);
         end;
      end if;

      --  build idff
      if Rst /= No_Net and then Has_Async_Abort then
         --  In case of async_abort.
         States := Build_Iadff (Ctxt, Clk, No_Net, Rst, Init, Init);
      else
         States := Build_Idff (Ctxt, Clk, No_Net, Init);
      end if;
      Set_Location (States, Stmt);

      --  create update nets
      --  For each state: if set, evaluate all outgoing edges.
      Next_States :=
        Synth_Psl_NFA (Syn_Inst, Get_PSL_NFA (Stmt), Nbr_States, States, Stmt);

      --  Handle sync_abort.
      if Rst /= No_Net and then not Has_Async_Abort then
         Next_States := Build_Mux2 (Ctxt, Rst, Next_States, Init);
         Set_Location (Next_States, Stmt);
      end if;

      Connect (Get_Input (Get_Net_Parent (States), 1), Next_States);
   end Synth_Psl_Dff;

   function Synth_Psl_Final
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node; Next_States : Net) return Net
   is
      use PSL.Types;
      use PSL.NFAs;
      NFA : constant PSL_NFA := Get_PSL_NFA (Stmt);
      Res : Net;
   begin
      Res := Build_Extract_Bit
        (Get_Build (Syn_Inst), Next_States,
         Uns32 (Get_State_Label (Get_Final_State (NFA))));
      Set_Location (Res, Stmt);
      return Res;
   end Synth_Psl_Final;

   function Synth_Psl_Not_Final
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node; Next_States : Net)
     return Net
   is
      Res : Net;
   begin
      Res := Build_Monadic (Get_Build (Syn_Inst), Id_Not,
                            Synth_Psl_Final (Syn_Inst, Stmt, Next_States));
      Set_Location (Res, Stmt);
      return Res;
   end Synth_Psl_Not_Final;

   procedure Synth_Psl_Restrict_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Next_States : Net;
      Res : Net;
      Inst : Instance;
   begin
      if not Flags.Flag_Formal then
         return;
      end if;

      --  Build assume gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States /= No_Net then
         --  The restriction holds as long as there is a 1 in the NFA state.
         Res := Build_Reduce (Ctxt, Id_Red_Or, Next_States);
         Set_Location (Res, Stmt);
         Inst := Build_Assume (Ctxt, Synth_Label (Syn_Inst, Stmt), Res);
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
      if not Flags.Flag_Formal then
         return;
      end if;

      --  Build cover gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States /= No_Net then
         --  The sequence is covered as soon as the final state is reached.
         Res := Synth_Psl_Final (Syn_Inst, Stmt, Next_States);
         Inst := Build_Cover
           (Get_Build (Syn_Inst), Synth_Label (Syn_Inst, Stmt), Res);
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Cover_Directive;

   procedure Synth_Psl_Assume_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Next_States : Net;
      Inst : Instance;
   begin
      if not Flags.Flag_Formal then
         return;
      end if;

      --  Build assume gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assume on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States /= No_Net then
         Inst := Build_Assume
           (Ctxt, Synth_Label (Syn_Inst, Stmt),
            Synth_Psl_Not_Final (Syn_Inst, Stmt, Next_States));
         Set_Location (Inst, Get_Location (Stmt));
      end if;
   end Synth_Psl_Assume_Directive;

   procedure Synth_Psl_Assert_Directive
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      use PSL.Types;
      use PSL.NFAs;
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      NFA : constant PSL_NFA := Get_PSL_NFA (Stmt);
      Active : NFA_State;
      Next_States : Net;
      Inst : Instance;
      Lab : Sname;
   begin
      if not Flags.Flag_Formal then
         return;
      end if;

      --  Build assert gate.
      --  Note: for synthesis, we assume the next state will be correct.
      --  (If we assert on States, then the first cycle is ignored).
      Synth_Psl_Dff (Syn_Inst, Stmt, Next_States);
      if Next_States = No_Net then
         return;
      end if;
      Lab := Synth_Label (Syn_Inst, Stmt);

      Inst := Build_Assert
        (Ctxt, Lab, Synth_Psl_Not_Final (Syn_Inst, Stmt, Next_States));
      Set_Location (Inst, Get_Location (Stmt));

      --  Also add a cover gate to cover assertion activation.
      if Flags.Flag_Assert_Cover then
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
     (Syn_Inst : Synth_Instance_Acc; Bod : Node)
   is
      use Areapools;
      Decls_Chain : constant Node := Get_Declaration_Chain (Bod);
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      M : Areapools.Mark_Type;
   begin
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      Synth_Concurrent_Declarations (Syn_Inst, Decls_Chain);

      Synth_Concurrent_Statements
        (Syn_Inst, Get_Concurrent_Statement_Chain (Bod));

      Synth_Attribute_Values (Syn_Inst, Bod);

      Finalize_Declarations (Syn_Inst, Decls_Chain);

      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;
   end Synth_Generate_Statement_Body;

   procedure Synth_If_Generate_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Sub_Inst : Synth_Instance_Acc;
      Name : Sname;
   begin
      Sub_Inst := Get_Sub_Instance (Syn_Inst, Stmt);
      if Sub_Inst = null then
         return;
      end if;

      Name := New_Sname_User (Get_Identifier (Stmt), Get_Sname (Syn_Inst));
      Set_Extra (Sub_Inst, Syn_Inst, Name);
      Synth_Generate_Statement_Body (Sub_Inst, Get_Source_Scope (Sub_Inst));
   end Synth_If_Generate_Statement;

   procedure Synth_For_Generate_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      Bod : constant Node := Get_Generate_Statement_Body (Stmt);
      It_Rng : Type_Acc;
      Sub_Inst : Synth_Instance_Acc;
      Gen_Inst : Synth_Instance_Acc;
      Name : Sname;
      Lname : Sname;
   begin
      It_Rng := Get_Subtype_Object (Syn_Inst, Get_Type (Iterator));
      Gen_Inst := Get_Sub_Instance (Syn_Inst, Stmt);

      Name := New_Sname_User (Get_Identifier (Stmt), Get_Sname (Syn_Inst));
      Set_Extra (Gen_Inst, Syn_Inst, Name);

      for I in 1 .. Get_Range_Length (It_Rng.Drange) loop
         --  FIXME: get position ?
         Lname := New_Sname_Version (Uns32 (I), Name);

         Sub_Inst := Get_Generate_Sub_Instance (Gen_Inst, Positive (I));
         Set_Extra (Sub_Inst, Gen_Inst, Lname);

         Synth_Generate_Statement_Body (Sub_Inst, Bod);
      end loop;
   end Synth_For_Generate_Statement;

   procedure Synth_Concurrent_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            Push_Phi;
            Synth_Simple_Signal_Assignment (Syn_Inst, Stmt);
            Pop_And_Merge_Phi (Ctxt, Get_Location (Stmt));
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Push_Phi;
            Synth_Conditional_Signal_Assignment (Syn_Inst, Stmt);
            Pop_And_Merge_Phi (Ctxt, Get_Location (Stmt));
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Push_Phi;
            Synth_Selected_Signal_Assignment (Syn_Inst, Stmt);
            Pop_And_Merge_Phi (Ctxt, Get_Location (Stmt));
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Push_Phi;
            Synth_Procedure_Call (Syn_Inst, Stmt);
            Pop_And_Merge_Phi (Ctxt, Get_Location (Stmt));
         when Iir_Kinds_Process_Statement =>
            Synth_Process_Statement (Syn_Inst, Stmt);
         when Iir_Kind_If_Generate_Statement =>
            Synth_If_Generate_Statement (Syn_Inst, Stmt);
         when Iir_Kind_For_Generate_Statement =>
            Synth_For_Generate_Statement (Syn_Inst, Stmt);
         when Iir_Kind_Component_Instantiation_Statement =>
            if Is_Component_Instantiation (Stmt) then
               declare
                  Comp_Inst : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Syn_Inst, Stmt);
                  Comp_Config : constant Node :=
                    Get_Instance_Config (Comp_Inst);
               begin
                  if Comp_Config = Null_Node
                    or else Get_Binding_Indication (Comp_Config) = Null_Node
                  then
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
         when Iir_Kind_Block_Statement =>
            Synth_Block_Statement (Syn_Inst, Stmt);
         when Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Declaration =>
            null;
         when Iir_Kind_Psl_Restrict_Directive =>
            Synth_Psl_Restrict_Directive (Syn_Inst, Stmt);
         when Iir_Kind_Psl_Assume_Directive =>
            if Flags.Flag_Assume_As_Assert then
               Synth_Psl_Assert_Directive (Syn_Inst, Stmt);
            else
               Synth_Psl_Assume_Directive (Syn_Inst, Stmt);
            end if;
         when Iir_Kind_Psl_Cover_Directive =>
            Synth_Psl_Cover_Directive (Syn_Inst, Stmt);
         when Iir_Kind_Psl_Assert_Directive =>
            if Flags.Flag_Assert_As_Assume then
               Synth_Psl_Assume_Directive (Syn_Inst, Stmt);
            else
               Synth_Psl_Assert_Directive (Syn_Inst, Stmt);
            end if;
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

   --  For allconst/allseq/...
   procedure Synth_Attribute_Formal (Syn_Inst : Synth_Instance_Acc;
                                     Val : Node;
                                     Id : Formal_Module_Id)
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
         Dyn : Dyn_Name;
         N : Net;
         Base : Valtyp;
         Typ : Type_Acc;
      begin
         Synth_Assignment_Prefix (Syn_Inst, Sig, Base, Typ, Off, Dyn);
         pragma Assert (Off = (0, 0));
         pragma Assert (Dyn.Voff = No_Net);
         pragma Assert (Base.Val.Kind = Value_Wire);
         pragma Assert (Base.Typ = Typ);

         N := Build_Formal_Input (Get_Build (Syn_Inst), Id, Typ.W);
         Set_Location (N, Val);
         Add_Conc_Assign (Get_Value_Wire (Base.Val), N, 0);
      end;
   end Synth_Attribute_Formal;

   procedure Synth_Attribute_Values
     (Syn_Inst : Synth_Instance_Acc; Unit : Node)
   is
      use Std_Names;

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
            when Name_Loc
               | Name_Keep
               | Name_Gclk =>
               --  Applies to nets/ports.
               null;
            when others =>
               Warning_Msg_Synth (+Spec, "unhandled attribute %i", (1 => +Id));
         end case;
         Val := Get_Value_Chain (Val);
      end loop;
   end Synth_Attribute_Values;

   procedure Synth_Verification_Unit (Syn_Inst : Synth_Instance_Acc;
                                      Unit : Node;
                                      Parent_Inst : Synth_Instance_Acc)
   is
      use Areapools;
      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
      Unit_Sname : Sname;
      M : Areapools.Mark_Type;
      Item : Node;
   begin
      Unit_Sname := New_Sname_User (Get_Identifier (Unit),
                                    Get_Sname (Syn_Inst));
      Set_Extra (Syn_Inst, Parent_Inst, Unit_Sname);
      Mark (M, Proc_Pool);
      Instance_Pool := Proc_Pool'Access;

      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Declaration
               | Iir_Kind_PSL_Inherit_Spec =>
               null;
            when Iir_Kind_Psl_Assert_Directive =>
               Synth_Psl_Assert_Directive (Syn_Inst, Item);
            when Iir_Kind_Psl_Assume_Directive =>
               Synth_Psl_Assume_Directive (Syn_Inst, Item);
            when Iir_Kind_Psl_Restrict_Directive =>
               Synth_Psl_Restrict_Directive (Syn_Inst, Item);
            when Iir_Kind_Psl_Cover_Directive =>
               Synth_Psl_Cover_Directive (Syn_Inst, Item);
            when Iir_Kind_Signal_Declaration
               | Iir_Kind_Constant_Declaration
               | Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration
               | Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body
               | Iir_Kind_Attribute_Declaration
               | Iir_Kind_Attribute_Specification
               | Iir_Kind_Object_Alias_Declaration
               | Iir_Kind_Non_Object_Alias_Declaration
               | Iir_Kind_Subtype_Declaration
               | Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Synth_Concurrent_Declaration (Syn_Inst, Item);
            when Iir_Kinds_Concurrent_Signal_Assignment
               | Iir_Kinds_Process_Statement
               | Iir_Kinds_Generate_Statement
               | Iir_Kind_Block_Statement
               | Iir_Kind_Concurrent_Procedure_Call_Statement
               | Iir_Kind_Component_Instantiation_Statement =>
               Synth_Concurrent_Statement (Syn_Inst, Item);
            when others =>
               Error_Kind ("synth_verification_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      Synth_Attribute_Values (Syn_Inst, Unit);

      --  Finalize
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Assert_Directive
               | Iir_Kind_Psl_Assume_Directive
               | Iir_Kind_Psl_Restrict_Directive
               | Iir_Kind_Psl_Cover_Directive
               | Iir_Kind_Psl_Declaration
               | Iir_Kind_PSL_Inherit_Spec =>
               null;
            when Iir_Kinds_Concurrent_Signal_Assignment
               | Iir_Kinds_Process_Statement
               | Iir_Kinds_Generate_Statement
               | Iir_Kind_Block_Statement
               | Iir_Kind_Concurrent_Procedure_Call_Statement
               | Iir_Kind_Component_Instantiation_Statement =>
               null;
            when Iir_Kind_Signal_Declaration
               | Iir_Kind_Constant_Declaration
               | Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration
               | Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body
               | Iir_Kind_Attribute_Declaration
               | Iir_Kind_Attribute_Specification
               | Iir_Kind_Object_Alias_Declaration
               | Iir_Kind_Non_Object_Alias_Declaration
               | Iir_Kind_Subtype_Declaration
               | Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Finalize_Declaration (Syn_Inst, Item, False);
            when others =>
               Error_Kind ("synth_verification_unit(2)", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      Release (M, Proc_Pool);
      Instance_Pool := Prev_Instance_Pool;
   end Synth_Verification_Unit;
end Synth.Vhdl_Stmts;
