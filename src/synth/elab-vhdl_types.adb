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

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Evaluation;
with Vhdl.Errors; use Vhdl.Errors;

with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;
with Elab.Vhdl_Decls;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;

package body Elab.Vhdl_Types is
   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type
   is
      L, R : Valtyp;
      Lval, Rval : Int64;
   begin
      --  Static values.
      L := Exec_Expression_With_Basetype (Syn_Inst, Get_Left_Limit (Rng));
      R := Exec_Expression_With_Basetype (Syn_Inst, Get_Right_Limit (Rng));
      Strip_Const (L);
      Strip_Const (R);

      if not (Is_Static (L.Val) and Is_Static (R.Val)) then
         Error_Msg_Elab (+Rng, "limits of range are not constant");
         Set_Error (Syn_Inst);
         return (Dir => Get_Direction (Rng),
                 Left => 0,
                 Right => 0,
                 Is_Signed => False);
      end if;

      Lval := Read_Discrete (L);
      Rval := Read_Discrete (R);
      return Build_Discrete_Range_Type (Lval, Rval, Get_Direction (Rng));
   end Synth_Discrete_Range_Expression;

   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type
   is
      L, R : Valtyp;
   begin
      --  Static values (so no enable).
      L := Exec_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Exec_Expression (Syn_Inst, Get_Right_Limit (Rng));
      return (Get_Direction (Rng), Read_Fp64 (L), Read_Fp64 (R));
   end Synth_Float_Range_Expression;

   function Synth_Array_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Bound_Type
   is
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix : constant Iir := Strip_Denoting_Name (Prefix_Name);
      Dim    : constant Natural :=
        Vhdl.Evaluation.Eval_Attribute_Parameter_Or_1 (Attr);
      Typ    : Type_Acc;
      Val    : Valtyp;
   begin
      --  Prefix is an array object or an array subtype.
      if Get_Kind (Prefix) = Iir_Kind_Subtype_Declaration then
         --  TODO: does this cover all the cases ?
         Typ := Get_Subtype_Object (Syn_Inst, Get_Subtype_Indication (Prefix));
      else
         Val := Exec_Name (Syn_Inst, Prefix_Name);
         Typ := Val.Typ;
      end if;

      return Get_Array_Bound (Typ, Dim_Type (Dim));
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

   procedure Synth_Subtype_Indication_If_Anonymous
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) is
   begin
      if Get_Type_Declarator (Atype) = Null_Node then
         Synth_Subtype_Indication (Syn_Inst, Atype);
      end if;
   end Synth_Subtype_Indication_If_Anonymous;

   function Synth_Subtype_Indication_If_Anonymous
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc is
   begin
      if Get_Type_Declarator (Atype) = Null_Node then
         return Synth_Subtype_Indication (Syn_Inst, Atype);
      else
         return Get_Subtype_Object (Syn_Inst, Atype);
      end if;
   end Synth_Subtype_Indication_If_Anonymous;

   function Synth_Array_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node) return Type_Acc
   is
      El_Type : constant Node := Get_Element_Subtype (Def);
      Ndims : constant Natural := Get_Nbr_Dimensions (Def);
      El_Typ : Type_Acc;
      Typ : Type_Acc;
   begin
      Synth_Subtype_Indication_If_Anonymous (Syn_Inst, El_Type);
      El_Typ := Get_Subtype_Object (Syn_Inst, El_Type);

      if El_Typ.Kind in Type_Nets and then Ndims = 1 then
         Typ := Create_Unbounded_Vector (El_Typ);
      else
         Typ := Create_Unbounded_Array (Dim_Type (Ndims), El_Typ);
      end if;
      return Typ;
   end Synth_Array_Type_Definition;

   function Synth_Record_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node) return Type_Acc
   is
      El_List : constant Node_Flist := Get_Elements_Declaration_List (Def);
      Rec_Els : Rec_El_Array_Acc;
      El      : Node;
      El_Type : Node;
      El_Typ  : Type_Acc;
   begin
      Rec_Els := Create_Rec_El_Array
        (Iir_Index32 (Get_Nbr_Elements (El_List)));

      for I in Flist_First .. Flist_Last (El_List) loop
         El := Get_Nth_Element (El_List, I);
         El_Type := Get_Type (El);
         El_Typ := Synth_Subtype_Indication_If_Anonymous (Syn_Inst, El_Type);
         Rec_Els.E (Iir_Index32 (I + 1)).Typ := El_Typ;
      end loop;

      if not Is_Fully_Constrained_Type (Def) then
         return Create_Unbounded_Record (Rec_Els);
      else
         return Create_Record_Type (Rec_Els);
      end if;
   end Synth_Record_Type_Definition;

   function Synth_Access_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node) return Type_Acc
   is
      Des_Type : constant Node := Get_Designated_Type (Def);
      Des_Typ : Type_Acc;
      Typ : Type_Acc;
   begin
      Synth_Subtype_Indication_If_Anonymous (Syn_Inst, Des_Type);
      Des_Typ := Get_Subtype_Object (Syn_Inst, Des_Type);

      Typ := Create_Access_Type (Des_Typ);
      return Typ;
   end Synth_Access_Type_Definition;

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

   procedure Elab_Type_Definition (Syn_Inst : Synth_Instance_Acc; Def : Node)
   is
      Typ : Type_Acc;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            if Def = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type
              or else Def = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Type
            then
               Typ := Logic_Type;
            elsif Def = Vhdl.Std_Package.Boolean_Type_Definition then
               Typ := Boolean_Type;
            elsif Def = Vhdl.Std_Package.Bit_Type_Definition then
               Typ := Bit_Type;
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
                  Typ := Create_Discrete_Type
                    (Rng, Scalar_Size_To_Size (Def), W);
               end;
            end if;
         when Iir_Kind_Array_Type_Definition =>
            Typ := Synth_Array_Type_Definition (Syn_Inst, Def);
         when Iir_Kind_Access_Type_Definition =>
            Typ := Synth_Access_Type_Definition (Syn_Inst, Def);
         when Iir_Kind_File_Type_Definition =>
            Typ := Synth_File_Type_Definition (Syn_Inst, Def);
         when Iir_Kind_Record_Type_Definition =>
            Typ := Synth_Record_Type_Definition (Syn_Inst, Def);
         when Iir_Kind_Protected_Type_Declaration =>
            --  TODO...
            Elab.Vhdl_Decls.Elab_Declarations
              (Syn_Inst, Get_Declaration_Chain (Def));
         when others =>
            Vhdl.Errors.Error_Kind ("synth_type_definition", Def);
      end case;
      if Typ /= null then
         Create_Subtype_Object (Syn_Inst, Def, Typ);
      end if;
   end Elab_Type_Definition;

   procedure Elab_Anonymous_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node; St : Node)
   is
      Typ : Type_Acc;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            declare
               Cst : constant Node := Get_Range_Constraint (St);
               L, R : Int64;
               Rng : Discrete_Range_Type;
               W : Uns32;
            begin
               L := Get_Value (Get_Left_Limit (Cst));
               R := Get_Value (Get_Right_Limit (Cst));
               Rng := Build_Discrete_Range_Type (L, R, Get_Direction (Cst));
               W := Discrete_Range_Width (Rng);
               Typ := Create_Discrete_Type
                 (Rng, Scalar_Size_To_Size (Def), W);
            end;
         when Iir_Kind_Floating_Type_Definition =>
            declare
               Cst : constant Node := Get_Range_Constraint (St);
               L, R : Fp64;
               Rng : Float_Range_Type;
            begin
               L := Get_Fp_Value (Get_Left_Limit (Cst));
               R := Get_Fp_Value (Get_Right_Limit (Cst));
               Rng := (Get_Direction (Cst), L, R);
               Typ := Create_Float_Type (Rng);
            end;
         when Iir_Kind_Array_Type_Definition =>
            Typ := Synth_Array_Type_Definition (Syn_Inst, Def);
         when others =>
            Vhdl.Errors.Error_Kind ("synth_anonymous_type_definition", Def);
      end case;
      Create_Subtype_Object (Syn_Inst, Def, Typ);
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

   function Synth_Array_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc
   is
      El_Type : constant Node := Get_Element_Subtype (Atype);
      St_Indexes : constant Node_Flist := Get_Index_Subtype_List (Atype);
      Ptype : Node;
      St_El : Node;
      Btyp : Type_Acc;
      Etyp : Type_Acc;
      Bnds : Bound_Array_Acc;
   begin
      --  VHDL08
      if Has_Element_Subtype_Indication (Atype) then
         --  This subtype has created a new anonymous subtype for the
         --  element.
         Synth_Subtype_Indication (Syn_Inst, El_Type);
      end if;

      if not Get_Index_Constraint_Flag (Atype) then
         Ptype := Get_Type (Get_Subtype_Type_Mark (Atype));
         if Get_Element_Subtype (Ptype) = Get_Element_Subtype (Atype) then
            --  That's an alias.
            --  FIXME: maybe a resolution function was added?
            --  FIXME: also handle resolution added in element subtype.
            return Get_Subtype_Object (Syn_Inst, Ptype);
         end if;
      end if;

      Btyp := Get_Subtype_Object (Syn_Inst, Get_Base_Type (Atype));
      case Btyp.Kind is
         when Type_Unbounded_Vector =>
            if Get_Index_Constraint_Flag (Atype) then
               St_El := Get_Index_Type (St_Indexes, 0);
               return Create_Vector_Type
                 (Synth_Bounds_From_Range (Syn_Inst, St_El), Btyp.Uvec_El);
            else
               --  An alias.
               --  Handle vhdl08 definition of std_logic_vector from
               --  std_ulogic_vector.
               return Btyp;
            end if;
         when Type_Unbounded_Array =>
            --  FIXME: partially constrained arrays, subtype in indexes...
            Etyp := Get_Subtype_Object (Syn_Inst, El_Type);
            if Get_Index_Constraint_Flag (Atype) then
               Bnds := Create_Bound_Array
                 (Dim_Type (Get_Nbr_Elements (St_Indexes)));
               for I in Flist_First .. Flist_Last (St_Indexes) loop
                  St_El := Get_Index_Type (St_Indexes, I);
                  Bnds.D (Dim_Type (I + 1)) :=
                    Synth_Bounds_From_Range (Syn_Inst, St_El);
               end loop;
               return Create_Array_Type (Bnds, Etyp);
            else
               raise Internal_Error;
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Array_Subtype_Indication;

   function Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc is
   begin
      --  TODO: handle aliases directly.
      case Get_Kind (Atype) is
         when Iir_Kind_Array_Subtype_Definition =>
            return Synth_Array_Subtype_Indication (Syn_Inst, Atype);
         when Iir_Kind_Record_Subtype_Definition =>
            return Synth_Record_Type_Definition (Syn_Inst, Atype);
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            declare
               Btype : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Base_Type (Atype));
               Rng : Discrete_Range_Type;
               W : Uns32;
            begin
               if Btype.Kind in Type_Nets then
                  --  A subtype of a bit/logic type is still a bit/logic.
                  --  FIXME: bounds.
                  return Btype;
               else
                  Rng := Synth_Discrete_Range_Constraint
                    (Syn_Inst, Get_Range_Constraint (Atype));
                  W := Discrete_Range_Width (Rng);
                  return Create_Discrete_Type (Rng, Btype.Sz, W);
               end if;
            end;
         when Iir_Kind_Floating_Subtype_Definition =>
            declare
               Rng : Float_Range_Type;
            begin
               Rng := Synth_Float_Range_Constraint
                 (Syn_Inst, Get_Range_Constraint (Atype));
               return Create_Float_Type (Rng);
            end;
         when others =>
            Vhdl.Errors.Error_Kind ("synth_subtype_indication", Atype);
      end case;
   end Synth_Subtype_Indication;

   procedure Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node)
   is
      Typ : Type_Acc;
   begin
      Typ := Synth_Subtype_Indication (Syn_Inst, Atype);
      Create_Subtype_Object (Syn_Inst, Atype, Typ);
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
              | Iir_Kind_Type_Declaration =>
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

   procedure Elab_Declaration_Type
     (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      Atype : constant Node := Get_Declaration_Type (Decl);
   begin
      if Atype = Null_Node then
         --  Already elaborated.
         return;
      end if;
      Synth_Subtype_Indication (Syn_Inst, Atype);
   end Elab_Declaration_Type;
end Elab.Vhdl_Types;
