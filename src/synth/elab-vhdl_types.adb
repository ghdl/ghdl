--  Create declarations for synthesis.
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
with Mutils; use Mutils;
with Errorout;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Evaluation;
with Vhdl.Errors; use Vhdl.Errors;

with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;
with Elab.Vhdl_Decls;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;

with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Errors;
with Synth.Vhdl_Context;

package body Elab.Vhdl_Types is
   function Synth_Subtype_Indication_With_Parent
     (Syn_Inst : Synth_Instance_Acc;
      Parent_Typ : Type_Acc;
      Atype : Node) return Type_Acc;

   function Elab_Subtype_Indication (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Type_Acc;

   --  Return true if a subtype indication needs to be elaborated (because
   --  it defines its own subtype definition).
   function Need_Elab_Subtype_Indication (Atype : Node) return Boolean is
   begin
      return Get_Kind (Atype) in Iir_Kinds_Subtype_Definition;
   end Need_Elab_Subtype_Indication;

   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type
   is
      use Synth.Vhdl_Context;
      L, R : Valtyp;
      Lval, Rval : Int64;
   begin
      --  Static values.
      L := Synth_Expression_With_Basetype (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression_With_Basetype (Syn_Inst, Get_Right_Limit (Rng));
      if L = No_Valtyp or else R = No_Valtyp then
         Set_Error (Syn_Inst);
         return (Dir => Get_Direction (Rng),
                 Left => 0,
                 Right => 0,
                 Is_Signed => False);
      end if;

      Strip_Const (L);
      Strip_Const (R);

      if not (Is_Static_Val (L.Val) and Is_Static_Val (R.Val)) then
         Error_Msg_Elab (Syn_Inst, Rng, "limits of range are not constant");
         Set_Error (Syn_Inst);
         return (Dir => Get_Direction (Rng),
                 Left => 0,
                 Right => 0,
                 Is_Signed => False);
      end if;

      Lval := Get_Static_Discrete (L);
      Rval := Get_Static_Discrete (R);
      return Build_Discrete_Range_Type (Lval, Rval, Get_Direction (Rng));
   end Synth_Discrete_Range_Expression;

   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type
   is
      Dir : constant Direction_Type := Get_Direction (Rng);
      L, R : Valtyp;
      Lv, Rv : Fp64;
   begin
      --  Static values (so no enable).
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      if L = No_Valtyp then
         case Dir is
            when Dir_To =>
               Lv := Fp64'First;
            when Dir_Downto =>
               Lv := Fp64'Last;
         end case;
      else
         Lv := Read_Fp64 (L);
      end if;

      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));
      if R = No_Valtyp then
         case Dir is
            when Dir_To =>
               Rv := Fp64'Last;
            when Dir_Downto =>
               Rv := Fp64'First;
         end case;
      else
         Rv := Read_Fp64 (R);
      end if;
      return (Dir, Lv, Rv);
   end Synth_Float_Range_Expression;

   --  Return the type of the prefix for an array attribute.
   function Synth_Array_Attribute_Prefix
     (Syn_Inst : Synth_Instance_Acc; Attr : Node) return Type_Acc
   is
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix : constant Iir := Strip_Denoting_Name (Prefix_Name);
   begin
      --  Prefix is an array object or an array subtype.
      if Get_Kind (Prefix) = Iir_Kind_Subtype_Declaration then
         --  TODO: does this cover all the cases ?
         return Get_Subtype_Object (Syn_Inst, Get_Subtype_Indication (Prefix));
      else
         --  The expression cannot be fully executed as it can be a signal
         --  (whose evaluation is not allowed during elaboration).
         return Exec_Name_Subtype (Syn_Inst, Prefix);
      end if;
   end Synth_Array_Attribute_Prefix;

   function Synth_Array_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Bound_Type
   is
      Dim    : constant Natural :=
        Vhdl.Evaluation.Eval_Attribute_Parameter_Or_1 (Attr);
      Typ    : Type_Acc;
   begin
      Typ := Synth_Array_Attribute_Prefix (Syn_Inst, Attr);

      for I in 2 .. Dim loop
         Typ := Typ.Arr_El;
      end loop;
      return Get_Array_Bound (Typ);
   end Synth_Array_Attribute;

   procedure Synth_Discrete_Range (Syn_Inst : Synth_Instance_Acc;
                                   Bound : Node;
                                   Rng : out Discrete_Range_Type) is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            Rng := Synth_Discrete_Range_Expression (Syn_Inst, Bound);
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            if Get_Type_Declarator (Bound) /= Null_Node then
               declare
                  Typ : Type_Acc;
               begin
                  --  This is a named subtype, so it has been evaluated.
                  Typ := Get_Subtype_Object (Syn_Inst, Bound);
                  Rng := Typ.Drange;
               end;
            else
               Synth_Discrete_Range
                 (Syn_Inst, Get_Range_Constraint (Bound), Rng);
            end if;
         when Iir_Kind_Range_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Bound);
               Rng := Build_Discrete_Range_Type
                 (Int64 (B.Left), Int64 (B.Right), B.Dir);
            end;
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            declare
               B : Bound_Type;
               T : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Bound);
               --  Reverse
               case B.Dir is
                  when Dir_To =>
                     B.Dir := Dir_Downto;
                  when Dir_Downto =>
                     B.Dir := Dir_To;
               end case;
               T := B.Right;
               B.Right := B.Left;
               B.Left := T;

               Rng := Build_Discrete_Range_Type
                 (Int64 (B.Left), Int64 (B.Right), B.Dir);
            end;
         when Iir_Kinds_Denoting_Name =>
            --  A discrete subtype name.
            Synth_Discrete_Range
              (Syn_Inst, Get_Subtype_Indication (Get_Named_Entity (Bound)),
               Rng);
         when others =>
            Error_Kind ("synth_discrete_range", Bound);
      end case;
   end Synth_Discrete_Range;

   function Synth_Bounds_From_Range (Rng : Discrete_Range_Type)
                                    return Bound_Type is
   begin
      return (Dir => Rng.Dir,
              Left => Int32 (Rng.Left), Right => Int32 (Rng.Right),
              Len => Get_Range_Length (Rng));
   end Synth_Bounds_From_Range;

   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Bound_Type
   is
      Rng : Discrete_Range_Type;
   begin
      Synth_Discrete_Range (Syn_Inst, Atype, Rng);
      return (Dir => Rng.Dir,
              Left => Int32 (Rng.Left), Right => Int32 (Rng.Right),
              Len => Get_Range_Length (Rng));
   end Synth_Bounds_From_Range;

   function Create_Bounds_From_Length
     (Bounds : Discrete_Range_Type; Len : Iir_Index32) return Bound_Type
   is
      Res : Bound_Type;
   begin
      Res := (Left => Int32 (Bounds.Left),
              Right => 0,
              Dir => Bounds.Dir,
              Len => Uns32 (Len));

      if Len = 0 then
         --  Special case.
         Res.Right := Res.Left;
         case Bounds.Dir is
            when Dir_To =>
               Res.Left := Res.Right + 1;
            when Dir_Downto =>
               Res.Left := Res.Right - 1;
         end case;
      else
         case Bounds.Dir is
            when Dir_To =>
               Res.Right := Res.Left + Int32 (Len - 1);
            when Dir_Downto =>
               Res.Right := Res.Left - Int32 (Len - 1);
         end case;
      end if;
      return Res;
   end Create_Bounds_From_Length;

   function Synth_Subtype_Indication_If_Anonymous
     (Syn_Inst : Synth_Instance_Acc;
      Atype : Node;
      Parent : Type_Acc) return Type_Acc is
   begin
      if Get_Type_Declarator (Atype) = Null_Node then
         if Parent /= null then
            return Synth_Subtype_Indication_With_Parent
              (Syn_Inst, Parent, Atype);
         else
            return Synth_Subtype_Indication (Syn_Inst, Atype);
         end if;
      else
         return Get_Subtype_Object (Syn_Inst, Atype);
      end if;
   end Synth_Subtype_Indication_If_Anonymous;

   function Synth_Array_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node) return Type_Acc
   is
      El_St : constant Node := Get_Element_Subtype_Indication (Def);
      Ndims : constant Natural := Get_Nbr_Dimensions (Def);
      Idx : Node;
      El_Typ : Type_Acc;
      Idx_Typ : Type_Acc;
      Typ : Type_Acc;
   begin
      if Need_Elab_Subtype_Indication (El_St) then
         El_Typ := Elab_Subtype_Indication (Syn_Inst, El_St);
      else
         El_Typ := Get_Elaborated_Subtype_Indication (Syn_Inst, El_St);
      end if;

      if El_Typ.Kind in Type_Nets and then Ndims = 1 then
         --  An array of nets is a vector.
         Idx := Get_Index_Type (Def, 0);
         Idx_Typ := Get_Subtype_Object (Syn_Inst, Idx);
         Typ := Create_Unbounded_Vector (El_Typ, Idx_Typ);
      else
         Typ := El_Typ;
         for I in reverse 1 .. Ndims loop
            Idx := Get_Index_Type (Def, Flist_First + (I - 1));
            Idx_Typ := Get_Subtype_Object (Syn_Inst, Idx);
            Typ := Create_Unbounded_Array (Idx_Typ, I = Ndims, Typ);
         end loop;
      end if;
      return Typ;
   end Synth_Array_Type_Definition;

   function Synth_Record_Type_Definition (Syn_Inst : Synth_Instance_Acc;
                                          Parent_Typ : Type_Acc;
                                          Def : Node) return Type_Acc
   is
      El_List : constant Node_Flist := Get_Elements_Declaration_List (Def);
      Rec_Els : Rec_El_Array_Acc;
      El      : Node;
      El_Type : Node;
      El_Typ  : Type_Acc;
      Bounded : Boolean;

      Parent_Els : Rec_El_Array_Acc;
   begin
      Rec_Els := Create_Rec_El_Array
        (Iir_Index32 (Get_Nbr_Elements (El_List)));

      if Parent_Typ /= null then
         Parent_Els := Parent_Typ.Rec;
      end if;

      Bounded := True;
      for I in Flist_First .. Flist_Last (El_List) loop
         El := Get_Nth_Element (El_List, I);
         El_Type := Get_Type (El);
         if Parent_Typ /= null then
            --  Get parent subtype for the element.
            El_Typ := Parent_Els.E (Iir_Index32 (I + 1)).Typ;

            if Get_Kind (El) = Iir_Kind_Record_Element_Constraint then
               --  There is an additional constraint.
               El_Typ := Synth_Subtype_Indication_If_Anonymous
                 (Syn_Inst, El_Type, El_Typ);
            end if;
         else
            El_Typ := Synth_Subtype_Indication_If_Anonymous
              (Syn_Inst, El_Type, null);
         end if;
         if Bounded and then not Is_Bounded_Type (El_Typ) then
            Bounded := False;
         end if;
         Rec_Els.E (Iir_Index32 (I + 1)).Typ := El_Typ;
      end loop;

      if Bounded then
         return Create_Record_Type (Parent_Typ, Rec_Els);
      else
         return Create_Unbounded_Record (Parent_Typ, Rec_Els);
      end if;
   end Synth_Record_Type_Definition;

   function Synth_Access_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node) return Type_Acc
   is
      Des_Type : constant Node := Get_Designated_Type (Def);
      Des_Ind : constant Node := Get_Designated_Subtype_Indication (Def);
      T : Node;
      Des_Typ : Type_Acc;
      Typ : Type_Acc;
   begin
      --  Need to handle incomplete access type.
      if Get_Kind (Des_Ind) in Iir_Kinds_Denoting_Name then
         T := Get_Named_Entity (Des_Ind);
         if Get_Kind (T) = Iir_Kind_Type_Declaration
           and then
           Get_Kind (Get_Type (T)) = Iir_Kind_Incomplete_Type_Definition
         then
            --  Access to incomplete type.
            Des_Typ := null;
         else
            Des_Typ := Get_Subtype_Object (Syn_Inst, Des_Type);
         end if;
      else
         Des_Typ := Synth_Subtype_Indication_If_Anonymous
           (Syn_Inst, Des_Type, null);
      end if;

      Typ := Create_Access_Type
        (null, Des_Typ, (Get_Signal_Type_Flag (Des_Type)
                         and then Get_Has_Signal_Flag (Des_Type)));
      return Typ;
   end Synth_Access_Type_Definition;

   procedure Elab_Incomplete_Type_Finish (Syn_Inst : Synth_Instance_Acc;
                                          Incomp : Node;
                                          Des_Def : Node)
   is
      Has_Signal : constant Boolean :=
        Get_Signal_Type_Flag (Des_Def) and then Get_Has_Signal_Flag (Des_Def);
      Des_Typ : Type_Acc;
      Acc : Node;
      Acc_Typ : Type_Acc;
   begin
      Des_Typ := Get_Subtype_Object (Syn_Inst, Des_Def);

      --  Complete all the access types in the chain.
      Acc := Get_Incomplete_Type_Ref_Chain (Incomp);
      while Acc /= Null_Node loop
         Acc_Typ := Get_Subtype_Object (Syn_Inst, Acc);
         Complete_Access_Type (Acc_Typ, Des_Typ, Has_Signal);
         Acc := Get_Incomplete_Type_Ref_Chain (Acc);
      end loop;
   end Elab_Incomplete_Type_Finish;

   function Synth_File_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node) return Type_Acc
   is
      File_Type : constant Node := Get_Type (Get_File_Type_Mark (Def));
      File_Typ  : Type_Acc;
      Typ       : Type_Acc;
      Sig       : String_Acc;
   begin
      File_Typ := Get_Subtype_Object (Syn_Inst, File_Type);

      if Get_Text_File_Flag (Def)
        or else
          Get_Kind (File_Type) in Iir_Kinds_Scalar_Type_And_Subtype_Definition
      then
         Sig := null;
      else
         declare
            Sig_Str : String (1 .. Get_File_Signature_Length (File_Type) + 2);
            Off : Natural := Sig_Str'First;
         begin
            Get_File_Signature (File_Type, Sig_Str, Off);
            Sig_Str (Off + 0) := '.';
            Sig_Str (Off + 1) := ASCII.NUL;
            Sig := new String'(Sig_Str);
         end;
      end if;

      Typ := Create_File_Type (File_Typ);
      Typ.File_Signature := Sig;

      return Typ;
   end Synth_File_Type_Definition;

   function Scalar_Size_To_Size (Def : Node) return Size_Type is
   begin
      case Get_Scalar_Size (Def) is
         when Scalar_8 =>
            return 1;
         when Scalar_16 =>
            return 2;
         when Scalar_32 =>
            return 4;
         when Scalar_64 =>
            return 8;
      end case;
   end Scalar_Size_To_Size;

   function Elab_Enumeration_Type_Definition (Def : Node) return Type_Acc is
   begin
      if Def = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type
        or else Def = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Type
      then
         return Logic_Type;
      elsif Def = Vhdl.Std_Package.Boolean_Type_Definition then
         return Boolean_Type;
      elsif Def = Vhdl.Std_Package.Bit_Type_Definition then
         return Bit_Type;
      else
         declare
            Nbr_El : constant Natural :=
              Get_Nbr_Elements (Get_Enumeration_Literal_List (Def));
            Rng : Discrete_Range_Type;
            W : Uns32;
         begin
            W := Uns32 (Clog2 (Uns64 (Nbr_El)));
            Rng := (Dir => Dir_To,
                    Is_Signed => False,
                    Left => 0,
                    Right => Int64 (Nbr_El - 1));
            return Create_Discrete_Type (Rng, Scalar_Size_To_Size (Def), W);
         end;
      end if;
   end Elab_Enumeration_Type_Definition;

   procedure Elab_Type_Definition (Syn_Inst : Synth_Instance_Acc; Def : Node)
   is
      Marker : Mark_Type;
      Typ : Type_Acc;
   begin
      Mark_Expr_Pool (Marker);
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Typ := Elab_Enumeration_Type_Definition (Def);
         when Iir_Kind_Array_Type_Definition =>
            Typ := Synth_Array_Type_Definition (Syn_Inst, Def);
         when Iir_Kind_Access_Type_Definition =>
            Typ := Synth_Access_Type_Definition (Syn_Inst, Def);
         when Iir_Kind_File_Type_Definition =>
            Typ := Synth_File_Type_Definition (Syn_Inst, Def);
         when Iir_Kind_Record_Type_Definition =>
            Typ := Synth_Record_Type_Definition (Syn_Inst, null, Def);
         when Iir_Kind_Protected_Type_Declaration =>
            Typ := Protected_Type;
            Create_Subtype_Object (Syn_Inst, Def, Typ);
            Elab.Vhdl_Decls.Elab_Declarations
              (Syn_Inst, Get_Declaration_Chain (Def));

            --  Do not call create_subtype_object twice.
            Typ := null;
         when Iir_Kind_Incomplete_Type_Definition =>
            return;
         when others =>
            Vhdl.Errors.Error_Kind ("synth_type_definition", Def);
      end case;
      if Typ /= null then
         Typ := Unshare (Typ, Instance_Pool);
         Create_Subtype_Object (Syn_Inst, Def, Typ);
      end if;
      Release_Expr_Pool (Marker);
   end Elab_Type_Definition;

   function Elab_Scalar_Type_Definition (Def : Node; St : Node) return Type_Acc
   is
      Cst : constant Node := Get_Range_Constraint (St);
      L, R : Int64;
      Rng : Discrete_Range_Type;
      W : Uns32;
   begin
      L := Get_Value (Get_Left_Limit (Cst));
      R := Get_Value (Get_Right_Limit (Cst));
      Rng := Build_Discrete_Range_Type (L, R, Get_Direction (Cst));
      W := Discrete_Range_Width (Rng);
      return Create_Discrete_Type (Rng, Scalar_Size_To_Size (Def), W);
   end Elab_Scalar_Type_Definition;

   function Elab_Floating_Type_Definition (Def : Node; St : Node)
                                          return Type_Acc
   is
      pragma Unreferenced (Def);
      Cst : constant Node := Get_Range_Constraint (St);
      L, R : Fp64;
      Rng : Float_Range_Type;
   begin
      L := Get_Fp_Value (Get_Left_Limit (Cst));
      R := Get_Fp_Value (Get_Right_Limit (Cst));
      Rng := (Get_Direction (Cst), L, R);
      return Create_Float_Type (Rng);
   end Elab_Floating_Type_Definition;

   procedure Elab_Anonymous_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node; St : Node)
   is
      Marker : Mark_Type;
      Typ : Type_Acc;
   begin
      Mark_Expr_Pool (Marker);
      case Get_Kind (Def) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            Typ := Elab_Scalar_Type_Definition (Def, St);
         when Iir_Kind_Floating_Type_Definition =>
            Typ := Elab_Floating_Type_Definition (Def, St);
         when Iir_Kind_Array_Type_Definition =>
            Typ := Synth_Array_Type_Definition (Syn_Inst, Def);
         when others =>
            Vhdl.Errors.Error_Kind ("synth_anonymous_type_definition", Def);
      end case;
      Typ := Unshare (Typ, Instance_Pool);
      Create_Subtype_Object (Syn_Inst, Def, Typ);
      Release_Expr_Pool (Marker);
   end Elab_Anonymous_Type_Definition;

   function Synth_Discrete_Range_Constraint
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type
   is
      Res : Discrete_Range_Type;
   begin
      Synth_Discrete_Range (Syn_Inst, Rng, Res);
      return Res;
   end Synth_Discrete_Range_Constraint;

   function Synth_Float_Range_Constraint
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type is
   begin
      case Get_Kind (Rng) is
         when Iir_Kind_Range_Expression =>
            --  FIXME: check range.
            return Synth_Float_Range_Expression (Syn_Inst, Rng);
         when others =>
            Vhdl.Errors.Error_Kind ("synth_float_range_constraint", Rng);
      end case;
   end Synth_Float_Range_Constraint;

   function Has_Element_Subtype_Indication (Atype : Node) return Boolean is
   begin
      return Get_Array_Element_Constraint (Atype) /= Null_Node
        or else
        (Get_Resolution_Indication (Atype) /= Null_Node
           and then
           (Get_Kind (Get_Resolution_Indication (Atype))
              = Iir_Kind_Array_Element_Resolution));
   end Has_Element_Subtype_Indication;

   procedure Check_Bound_Compatibility (Syn_Inst : Synth_Instance_Acc;
                                        Loc : Node;
                                        Bnd : Bound_Type;
                                        Typ : Type_Acc)
   is
      use Synth.Errors;
      use Errorout;
   begin
      --  A null range is always compatible (see LRM08 5.2.1).
      if Bnd.Len = 0 then
         return;
      end if;

      if not In_Range (Typ.Drange, Int64 (Bnd.Left)) then
         Error_Msg_Synth (Syn_Inst, Loc,
                          "left bound (%v) not in range (%v to %v)",
                          (+Bnd.Left, +Typ.Drange.Left, +Typ.Drange.Right));
      elsif not In_Range (Typ.Drange, Int64 (Bnd.Right)) then
         Error_Msg_Synth (Syn_Inst, Loc,
                          "right bound (%v) not in range (%v to %v)",
                          (+Bnd.Right, +Typ.Drange.Left, +Typ.Drange.Right));
      end if;
   end Check_Bound_Compatibility;

   function Synth_Array_Subtype_Indication (Syn_Inst : Synth_Instance_Acc;
                                            Parent_Typ : Type_Acc;
                                            Atype : Node) return Type_Acc
   is
      Parent_Type : constant Node := Get_Parent_Type (Atype);
      El_Type : constant Node := Get_Element_Subtype (Atype);
      St_Indexes : constant Node_Flist := Get_Index_Subtype_List (Atype);
      El_Typ : Type_Acc;
   begin
      --  Get parent real array element.
      El_Typ := Parent_Typ;
      while not Is_Last_Dimension (El_Typ) loop
         El_Typ := Get_Array_Element (El_Typ);
      end loop;
      El_Typ := Get_Array_Element (El_Typ);

      --  VHDL08
      if Has_Element_Subtype_Indication (Atype) then
         --  This subtype has created a new anonymous subtype for the
         --  element.
         El_Typ := Synth_Subtype_Indication_With_Parent
           (Syn_Inst, El_Typ, El_Type);
      end if;

      if not Get_Index_Constraint_Flag (Atype) then
         if Get_Element_Subtype (Parent_Type) = Get_Element_Subtype (Atype)
         then
            --  That's an alias.
            --  FIXME: maybe a resolution function was added?
            --  FIXME: also handle resolution added in element subtype.
            return Parent_Typ;
         end if;
      end if;

      case Parent_Typ.Kind is
         when Type_Unbounded_Vector =>
            if Get_Index_Constraint_Flag (Atype) then
               declare
                  St_El : Node;
                  Bnd : Bound_Type;
                  Bnd_Static : Boolean;
               begin
                  St_El := Get_Index_Type (St_Indexes, 0);
                  Bnd := Synth_Bounds_From_Range (Syn_Inst, St_El);
                  Bnd_Static := Get_Type_Staticness (St_El) = Locally;
                  Check_Bound_Compatibility
                    (Syn_Inst, St_El, Bnd, Parent_Typ.Uarr_Idx);
                  return Create_Vector_Type (Bnd, Bnd_Static, El_Typ);
               end;
            else
               --  An alias.
               --  Handle vhdl08 definition of std_logic_vector from
               --  std_ulogic_vector.
               return Parent_Typ;
            end if;
         when Type_Unbounded_Array =>
            --  FIXME: partially constrained arrays, subtype in indexes...
            if Get_Index_Constraint_Flag (Atype) then
               declare
                  El_Bounded : constant Boolean := Is_Bounded_Type (El_Typ);
                  St_El : Node;
                  Res_Typ : Type_Acc;
                  Bnd : Bound_Type;
                  P : Type_Acc;
                  Bnd_Static : Boolean;
               begin
                  Res_Typ := El_Typ;
                  for I in reverse Flist_First .. Flist_Last (St_Indexes) loop
                     St_El := Get_Index_Type (St_Indexes, I);
                     Bnd := Synth_Bounds_From_Range (Syn_Inst, St_El);
                     Bnd_Static := Get_Type_Staticness (St_El) = Locally;
                     --  Get parent index.
                     P := Parent_Typ;
                     for J in Flist_First + 1 .. I loop
                        P := P.Uarr_El;
                     end loop;
                     Check_Bound_Compatibility
                       (Syn_Inst, St_El, Bnd, P.Uarr_Idx);
                     if El_Bounded then
                        Res_Typ := Create_Array_Type
                          (Bnd, Bnd_Static, Res_Typ = El_Typ, Res_Typ);
                     else
                        Res_Typ := Create_Array_Unbounded_Type
                          (Bnd, Bnd_Static, Res_Typ = El_Typ, Res_Typ);
                     end if;
                  end loop;
                  return Res_Typ;
               end;
            else
               return Create_Unbounded_Array
                 (Parent_Typ.Uarr_Idx, Parent_Typ.Ulast, El_Typ);
            end if;
         when Type_Array_Unbounded =>
            if Is_Bounded_Type (El_Typ) then
               return Create_Array_From_Array_Unbounded
                 (Parent_Typ, El_Typ);
            else
               raise Internal_Error;
            end if;
         when Type_Vector
           | Type_Array =>
            --  An alias with just a different resolver ?
            return Parent_Typ;
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Array_Subtype_Indication;

   function Synth_Subtype_Indication_With_Parent
     (Syn_Inst : Synth_Instance_Acc;
      Parent_Typ : Type_Acc;
      Atype : Node) return Type_Acc is
   begin
      if Get_Type_Declarator (Atype) = Null_Node then
         case Get_Kind (Atype) is
            when Iir_Kind_Array_Subtype_Definition =>
               return Synth_Array_Subtype_Indication
                 (Syn_Inst, Parent_Typ, Atype);
            when Iir_Kind_Record_Subtype_Definition =>
               return Synth_Record_Type_Definition
                 (Syn_Inst, Parent_Typ, Atype);
            when others =>
               return Synth_Subtype_Indication (Syn_Inst, Atype);
         end case;
      else
         return Get_Subtype_Object (Syn_Inst, Atype);
      end if;
   end Synth_Subtype_Indication_With_Parent;

   function Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc is
   begin
      --  TODO: handle aliases directly.
      case Get_Kind (Atype) is
         when Iir_Kinds_Denoting_Name =>
            return Get_Subtype_Object (Syn_Inst, Get_Type (Atype));
         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Parent_Type : constant Node := Get_Parent_Type (Atype);
               Parent_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Parent_Type);
            begin
               if Parent_Typ = null then
                  Elab.Vhdl_Errors.Error_Msg_Elab
                    (Syn_Inst, Atype, "base type is not yet elaborated");
                  raise Elab.Vhdl_Errors.Elaboration_Error;
               end if;
               return Synth_Array_Subtype_Indication
                 (Syn_Inst, Parent_Typ, Atype);
            end;
         when Iir_Kind_Record_Subtype_Definition =>
            declare
               Parent_Type : constant Node := Get_Parent_Type (Atype);
               Parent_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Parent_Type);
            begin
               if Parent_Typ = null then
                  Elab.Vhdl_Errors.Error_Msg_Elab
                    (Syn_Inst, Atype, "base type is not yet elaborated");
                  raise Elab.Vhdl_Errors.Elaboration_Error;
               end if;
               return Synth_Record_Type_Definition
                 (Syn_Inst, Parent_Typ, Atype);
            end;
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            declare
               Btype : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Base_Type (Atype));
               Rng : Discrete_Range_Type;
               W : Uns32;
            begin
               Rng := Synth_Discrete_Range_Constraint
                 (Syn_Inst, Get_Range_Constraint (Atype));
               if Rng = Btype.Drange then
                  return Btype;
               end if;
               case Type_All_Discrete (Btype.Kind) is
                  when Type_Discrete =>
                     W := Discrete_Range_Width (Rng);
                     return Create_Discrete_Type (Rng, Btype.Sz, W);
                  when Type_Bit =>
                     return Create_Bit_Subtype (Rng);
                  when Type_Logic =>
                     return Create_Logic_Subtype (Rng);
               end case;
            end;
         when Iir_Kind_Floating_Subtype_Definition =>
            declare
               Rng : Float_Range_Type;
            begin
               Rng := Synth_Float_Range_Constraint
                 (Syn_Inst, Get_Range_Constraint (Atype));
               return Create_Float_Type (Rng);
            end;
         when Iir_Kind_Access_Subtype_Definition =>
            declare
               Parent_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Parent_Type (Atype));
               Acc_Typ : Type_Acc;
            begin
               Acc_Typ := Synth_Subtype_Indication
                 (Syn_Inst, Get_Designated_Type (Atype));
               return Create_Access_Type (Parent_Typ, Acc_Typ, False);
            end;
         when Iir_Kind_File_Subtype_Definition =>
            --  Same as parent.
            declare
               Parent_Type : constant Node := Get_Parent_Type (Atype);
            begin
               return Get_Subtype_Object (Syn_Inst, Parent_Type);
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            return Get_Subtype_Object (Syn_Inst, Atype);
         when others =>
            Vhdl.Errors.Error_Kind ("synth_subtype_indication", Atype);
      end case;
   end Synth_Subtype_Indication;

   procedure Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node)
   is
      Typ : Type_Acc;
      Marker : Mark_Type;
   begin
      Mark_Expr_Pool (Marker);
      Typ := Synth_Subtype_Indication (Syn_Inst, Atype);
      Create_Subtype_Object (Syn_Inst, Atype, Unshare (Typ, Instance_Pool));
      Release_Expr_Pool (Marker);
   end Synth_Subtype_Indication;

   function Get_Declaration_Type (Decl : Node) return Node
   is
      Ind : constant Node := Get_Subtype_Indication (Decl);
      Atype : Node;
   begin
      if Get_Is_Ref (Decl) or else Ind = Null_Iir then
         --  A secondary declaration in a list.
         return Null_Node;
      end if;
      Atype := Ind;
      loop
         case Get_Kind (Atype) is
            when Iir_Kinds_Denoting_Name =>
               Atype := Get_Named_Entity (Atype);
            when Iir_Kind_Subtype_Declaration
              | Iir_Kind_Type_Declaration
              | Iir_Kind_Subtype_Attribute
              | Iir_Kind_Interface_Type_Declaration =>
               --  Type already declared, so already handled.
               return Null_Node;
            when Iir_Kind_Array_Subtype_Definition
              | Iir_Kind_Record_Subtype_Definition
              | Iir_Kind_Integer_Subtype_Definition
              | Iir_Kind_Floating_Subtype_Definition
              | Iir_Kind_Physical_Subtype_Definition
              | Iir_Kind_Enumeration_Subtype_Definition =>
               return Atype;
            when others =>
               Vhdl.Errors.Error_Kind ("get_declaration_type", Atype);
         end case;
      end loop;
   end Get_Declaration_Type;

   function Get_Elaborated_Subtype_Indication (Syn_Inst : Synth_Instance_Acc;
                                               Atype : Node) return Type_Acc
   is
      Marker : Mark_Type;
      Res_Type : Node;
   begin
      case Get_Kind (Atype) is
         when Iir_Kinds_Subtype_Definition =>
            Res_Type := Atype;
         when Iir_Kinds_Denoting_Name =>
            --  Already elaborated.
            --  We cannot use the object type as it can be a subtype
            --  deduced from the default value (for constants).
            Res_Type := Get_Type (Get_Named_Entity (Atype));
         when Iir_Kind_Subtype_Attribute =>
            declare
               Pfx : constant Node := Get_Prefix (Atype);
               T : Type_Acc;
            begin
               Mark_Expr_Pool (Marker);
               T := Exec_Name_Subtype (Syn_Inst, Pfx);
               Release_Expr_Pool (Marker);
               pragma Assert (T.Is_Global);
               return T;
            end;
         when Iir_Kind_Element_Attribute =>
            declare
               T : Type_Acc;
            begin
               T := Synth_Array_Attribute_Prefix (Syn_Inst, Atype);
               pragma Assert (T.Is_Global);
               --  Always a bounded array/vector.
               return T.Arr_El;
            end;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_File_Type_Definition =>
            --  For interface types of implicit operators.
            Res_Type := Atype;
         when Iir_Kind_Interface_Type_Definition =>
            Res_Type := Atype;
         when others =>
            Error_Kind ("elab_subtype_indication", Atype);
      end case;

      return Get_Subtype_Object (Syn_Inst, Res_Type);
   end Get_Elaborated_Subtype_Indication;

   function Elab_Subtype_Indication (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Type_Acc
   is
      Marker : Mark_Type;
      Typ : Type_Acc;
   begin
      --  That's a new type.
      Mark_Expr_Pool (Marker);
      Typ := Synth_Subtype_Indication (Syn_Inst, Atype);
      if Typ /= null then
         Typ := Unshare (Typ, Instance_Pool);
         Create_Subtype_Object (Syn_Inst, Atype, Typ);
      end if;
      Release_Expr_Pool (Marker);
      return Typ;
   end Elab_Subtype_Indication;

   function Elab_Declaration_Type
     (Syn_Inst : Synth_Instance_Acc; Decl : Node) return Type_Acc
   is
      Atype : Node;
      Is_Ref : Boolean;
   begin
      Atype := Get_Subtype_Indication (Decl);
      if Atype = Null_Node then
         Atype := Get_Type (Decl);
         Is_Ref := True;
      else
         Is_Ref := Get_Is_Ref (Decl);
      end if;
      if Is_Ref
        or else not Need_Elab_Subtype_Indication (Atype)
      then
         return Get_Elaborated_Subtype_Indication (Syn_Inst, Atype);
      else
         return Elab_Subtype_Indication (Syn_Inst, Atype);
      end if;
   end Elab_Declaration_Type;

   procedure Elab_Declaration_Type
     (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      Atype : constant Node := Get_Subtype_Indication (Decl);
      Res : Type_Acc;
      pragma Unreferenced (Res);
   begin
      if Atype = Null_Node then
         return;
      end if;
      if Need_Elab_Subtype_Indication (Atype) then
         Res := Elab_Subtype_Indication (Syn_Inst, Atype);
      end if;
   end Elab_Declaration_Type;

end Elab.Vhdl_Types;
