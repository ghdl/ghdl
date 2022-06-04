--  Expressions synthesis.
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
with Name_Table;
with Std_Names;
with Str_Table;
with Errorout; use Errorout;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Heap; use Elab.Vhdl_Heap;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;
with Elab.Debugger;

with Synth.Vhdl_Stmts; use Synth.Vhdl_Stmts;
with Synth.Vhdl_Oper; use Synth.Vhdl_Oper;
with Synth.Vhdl_Aggr;
with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Vhdl_Eval; use Synth.Vhdl_Eval;

with Grt.Types;
with Grt.To_Strings;

package body Elab.Vhdl_Expr is
   function Synth_Bounds_From_Length (Atype : Node; Len : Int32)
                                     return Bound_Type
   is
      Rng : constant Node := Get_Range_Constraint (Atype);
      Limit : Int32;
   begin
      Limit := Int32 (Eval_Pos (Get_Left_Limit (Rng)));
      case Get_Direction (Rng) is
         when Dir_To =>
            return (Dir => Dir_To,
                    Left => Limit,
                    Right => Limit + Len - 1,
                    Len => Uns32 (Len));
         when Dir_Downto =>
            return (Dir => Dir_Downto,
                    Left => Limit,
                    Right => Limit - Len + 1,
                    Len => Uns32 (Len));
      end case;
   end Synth_Bounds_From_Length;

   function Exec_Simple_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node) return Valtyp
   is
      Aggr_Type : constant Node := Get_Type (Aggr);
      pragma Assert (Get_Nbr_Dimensions (Aggr_Type) = 1);
      El_Type : constant Node := Get_Element_Subtype (Aggr_Type);
      El_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, El_Type);
      Els : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
      Last : constant Natural := Flist_Last (Els);
      Bnd : Bound_Type;
      Res_Type : Type_Acc;
      Val : Valtyp;
      Res : Valtyp;
   begin
      --  Allocate the result.
      Bnd := Synth_Array_Bounds (Syn_Inst, Aggr_Type, 1);
      pragma Assert (Bnd.Len = Uns32 (Last + 1));

      if El_Typ.Kind in Type_Nets then
         Res_Type := Create_Vector_Type (Bnd, El_Typ);
      else
         Res_Type := Create_Array_Type (Bnd, True, El_Typ);
      end if;

      Res := Create_Value_Memory (Res_Type);

      for I in Flist_First .. Last loop
         --  Elements are supposed to be static, so no need for enable.
         Val := Exec_Expression_With_Type
           (Syn_Inst, Get_Nth_Element (Els, I), El_Typ);
         pragma Assert (Is_Static (Val.Val));
         Write_Value (Res.Val.Mem + Size_Type (I) * El_Typ.Sz, Val);
      end loop;

      return Res;
   end Exec_Simple_Aggregate;

   --  Change the bounds of VAL.
   function Reshape_Value (Val : Valtyp; Ntype : Type_Acc) return Valtyp is
   begin
      case Val.Val.Kind is
         when Value_Alias =>
            return Create_Value_Alias
              ((Val.Val.A_Typ, Val.Val.A_Obj), Val.Val.A_Off, Ntype);
         when Value_Const =>
            return Reshape_Value ((Val.Typ, Val.Val.C_Val), Ntype);
         when Value_Memory =>
            return (Ntype, Val.Val);
         when others =>
            raise Internal_Error;
      end case;
   end Reshape_Value;

   function Exec_Subtype_Conversion (Vt : Valtyp;
                                     Dtype : Type_Acc;
                                     Bounds : Boolean;
                                     Loc : Node)
                                    return Valtyp
   is
      Vtype : constant Type_Acc := Vt.Typ;
   begin
      if Vt = No_Valtyp then
         --  Propagate error.
         return No_Valtyp;
      end if;
      if Dtype = Vtype then
         return Vt;
      end if;

      case Dtype.Kind is
         when Type_Bit =>
            pragma Assert (Vtype.Kind = Type_Bit);
            return Vt;
         when Type_Logic =>
            pragma Assert (Vtype.Kind = Type_Logic);
            return Vt;
         when Type_Discrete =>
            pragma Assert (Vtype.Kind in Type_All_Discrete);
            case Vt.Val.Kind is
               when Value_Net
                  | Value_Wire
                  | Value_Alias =>
                  raise Internal_Error;
               when Value_Const =>
                  return Exec_Subtype_Conversion
                    ((Vt.Typ, Vt.Val.C_Val), Dtype, Bounds, Loc);
               when Value_Memory =>
                  --  Check for overflow.
                  declare
                     Val : constant Int64 := Read_Discrete (Vt);
                  begin
                     if not In_Range (Dtype.Drange, Val) then
                        Error_Msg_Elab (+Loc, "value out of range");
                        return No_Valtyp;
                     end if;
                     return Create_Value_Discrete (Val, Dtype);
                  end;
               when others =>
                  raise Internal_Error;
            end case;
         when Type_Float =>
            pragma Assert (Vtype.Kind = Type_Float);
            --  TODO: check range
            return Vt;
         when Type_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or Vtype.Kind = Type_Slice);
            if Dtype.W /= Vtype.W then
               Error_Msg_Elab
                 (+Loc, "mismatching vector length; got %v, expect %v",
                  (+Vtype.W, +Dtype.W));
               return No_Valtyp;
            end if;
            if Bounds then
               return Reshape_Value (Vt, Dtype);
            else
               return Vt;
            end if;
         when Type_Slice =>
            --  TODO: check width
            return Vt;
         when Type_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            --  Check bounds.
            declare
               Src_Typ, Dst_Typ : Type_Acc;
            begin
               Src_Typ := Vtype;
               Dst_Typ := Dtype;
               loop
                  pragma Assert (Src_Typ.Alast = Dst_Typ.Alast);
                  if Src_Typ.Abound.Len /= Dst_Typ.Abound.Len then
                     Error_Msg_Elab (+Loc, "mismatching array bounds");
                     return No_Valtyp;
                  end if;
                  exit when Src_Typ.Alast;
                  Src_Typ := Src_Typ.Arr_El;
                  Dst_Typ := Dst_Typ.Arr_El;
               end loop;
               --  TODO: check element.
               if Bounds then
                  return Reshape_Value (Vt, Dtype);
               else
                  return Vt;
               end if;
            end;
         when Type_Unbounded_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            return Vt;
         when Type_Unbounded_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or else Vtype.Kind = Type_Slice);
            return Vt;
         when Type_Record =>
            pragma Assert (Vtype.Kind = Type_Record);
            --  TODO: handle elements.
            return Vt;
         when Type_Unbounded_Record =>
            pragma Assert (Vtype.Kind = Type_Record);
            return Vt;
         when Type_Access =>
            return Vt;
         when Type_File
            | Type_Protected =>
            --  No conversion expected.
            --  As the subtype is identical, it is already handled by the
            --  above check.
            raise Internal_Error;
      end case;
   end Exec_Subtype_Conversion;

   function Exec_Value_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                 return Valtyp
   is
      Param : constant Node := Get_Parameter (Attr);
      Etype : constant Node := Get_Type (Attr);
      Btype : constant Node := Get_Base_Type (Etype);
      V : Valtyp;
      Dtype : Type_Acc;
   begin
      --  The value is supposed to be static.
      V := Exec_Expression (Syn_Inst, Param);
      if V = No_Valtyp then
         return No_Valtyp;
      end if;

      Dtype := Get_Subtype_Object (Syn_Inst, Etype);
      if not Is_Static (V.Val) then
         Error_Msg_Elab (+Attr, "parameter of 'value must be static");
         return No_Valtyp;
      end if;

      declare
         Str : constant String := Value_To_String (V);
         Res_N : Node;
         Val : Int64;
      begin
         case Get_Kind (Btype) is
            when Iir_Kind_Enumeration_Type_Definition =>
               Res_N := Eval_Value_Attribute (Str, Etype, Attr);
               Val := Int64 (Get_Enum_Pos (Res_N));
               Free_Iir (Res_N);
            when Iir_Kind_Integer_Type_Definition =>
               Val := Int64'Value (Str);
            when others =>
               Error_Msg_Elab (+Attr, "unhandled type for 'value");
               return No_Valtyp;
         end case;
         return Create_Value_Discrete (Val, Dtype);
      end;
   end Exec_Value_Attribute;

   function Synth_Image_Attribute_Str (Val : Valtyp; Expr_Type : Iir)
                                      return String
   is
      use Grt.Types;
   begin
      case Get_Kind (Expr_Type) is
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            declare
               Str : String (1 .. 24);
               Last : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, Last, Ghdl_F64 (Read_Fp64 (Val)));
               return Str (Str'First .. Last);
            end;
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            declare
               Str : String (1 .. 21);
               First : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, First, Ghdl_I64 (Read_Discrete (Val)));
               return Str (First .. Str'Last);
            end;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            declare
               Lits : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Get_Base_Type (Expr_Type));
            begin
               return Name_Table.Image
                 (Get_Identifier
                    (Get_Nth_Element (Lits, Natural (Read_Discrete (Val)))));
            end;
         when Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            declare
               Str : String (1 .. 21);
               First : Natural;
               Id : constant Name_Id :=
                 Get_Identifier (Get_Primary_Unit (Get_Base_Type (Expr_Type)));
            begin
               Grt.To_Strings.To_String
                 (Str, First, Ghdl_I64 (Read_Discrete (Val)));
               return Str (First .. Str'Last) & ' ' & Name_Table.Image (Id);
            end;
         when others =>
            Error_Kind ("synth_image_attribute_str", Expr_Type);
      end case;
   end Synth_Image_Attribute_Str;

   function Exec_Image_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                 return Valtyp
   is
      Param : constant Node := Get_Parameter (Attr);
      Etype : constant Node := Get_Type (Attr);
      V : Valtyp;
      Dtype : Type_Acc;
      Res : Memtyp;
   begin
      --  The parameter is expected to be static.
      V := Exec_Expression (Syn_Inst, Param);
      if V = No_Valtyp then
         return No_Valtyp;
      end if;
      Dtype := Get_Subtype_Object (Syn_Inst, Etype);
      if not Is_Static (V.Val) then
         Error_Msg_Elab (+Attr, "parameter of 'image must be static");
         return No_Valtyp;
      end if;

      Strip_Const (V);
      Res := String_To_Memtyp
        (Synth_Image_Attribute_Str (V, Get_Type (Param)), Dtype);
      return Create_Value_Memtyp (Res);
   end Exec_Image_Attribute;

   function Exec_Instance_Name_Attribute
     (Syn_Inst : Synth_Instance_Acc; Attr : Node) return Valtyp
   is
      Atype : constant Node := Get_Type (Attr);
      Atyp  : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Atype);
      Name  : constant Path_Instance_Name_Type :=
        Get_Path_Instance_Name_Suffix (Attr);
      Res : Memtyp;
   begin
      --  Return a truncated name, as the prefix is not completly known.
      Res := String_To_Memtyp (Name.Suffix, Atyp);
      return Create_Value_Memtyp (Res);
   end Exec_Instance_Name_Attribute;

   --  Convert index IDX in PFX to an offset.
   --  SYN_INST and LOC are used in case of error.
   function Index_To_Offset
     (Syn_Inst : Synth_Instance_Acc; Bnd : Bound_Type; Idx : Int64; Loc : Node)
     return Value_Offsets
   is
      Res : Value_Offsets;
   begin
      if not In_Bounds (Bnd, Int32 (Idx)) then
         Error_Msg_Elab (+Loc, "index not within bounds");
         Elab.Debugger.Debug_Error (Syn_Inst, Loc);
         return (0, 0);
      end if;

      --  The offset is from the LSB (bit 0).  Bit 0 is the rightmost one.
      case Bnd.Dir is
         when Dir_To =>
            Res.Net_Off := Uns32 (Bnd.Right - Int32 (Idx));
            Res.Mem_Off := Size_Type (Int32 (Idx) - Bnd.Left);
         when Dir_Downto =>
            Res.Net_Off := Uns32 (Int32 (Idx) - Bnd.Right);
            Res.Mem_Off := Size_Type (Bnd.Left - Int32 (Idx));
      end case;

      return Res;
   end Index_To_Offset;

   procedure Check_Matching_Bounds (L, R : Type_Acc; Loc : Node) is
   begin
      if not Are_Types_Equal (L, R) then
         Error_Msg_Elab (+Loc, "non matching bounds");
      end if;
   end Check_Matching_Bounds;

   --  Return the bounds of a one dimensional array/vector type and the
   --  width of the element.
   procedure Get_Onedimensional_Array_Bounds
     (Typ : Type_Acc; Bnd : out Bound_Type; El_Typ : out Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Array
           | Type_Vector =>
            pragma Assert (Typ.Alast);
            El_Typ := Typ.Arr_El;
            Bnd := Typ.Abound;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Onedimensional_Array_Bounds;

   function Create_Onedimensional_Array_Subtype
     (Btyp : Type_Acc; Bnd : Bound_Type; El_Typ : Type_Acc) return Type_Acc
   is
      Res : Type_Acc;
   begin
      case Btyp.Kind is
         when Type_Vector =>
            pragma Assert (El_Typ.Kind in Type_Nets);
            Res := Create_Vector_Type (Bnd, Btyp.Arr_El);
         when Type_Unbounded_Vector =>
            pragma Assert (El_Typ.Kind in Type_Nets);
            Res := Create_Vector_Type (Bnd, Btyp.Uarr_El);
         when Type_Array =>
            pragma Assert (Btyp.Alast);
            pragma Assert (Is_Bounded_Type (Btyp.Arr_El));
            Res := Create_Array_Type (Bnd, True, Btyp.Arr_El);
         when Type_Unbounded_Array =>
            pragma Assert (Btyp.Ulast);
            pragma Assert (Is_Bounded_Type (El_Typ));
            Res := Create_Array_Type (Bnd, True, El_Typ);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Onedimensional_Array_Subtype;

   procedure Exec_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                Name : Node;
                                Pfx_Type : Type_Acc;
                                Off : out Value_Offsets)
   is
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      El_Typ : constant Type_Acc := Get_Array_Element (Pfx_Type);
      Idx_Expr : Node;
      Idx_Val : Valtyp;
      Bnd : Bound_Type;
      Stride : Uns32;
      Idx_Off : Value_Offsets;
   begin
      Off := (0, 0);

      Stride := 1;
      for I in reverse Flist_First .. Flist_Last (Indexes) loop
         Idx_Expr := Get_Nth_Element (Indexes, I);

         --  Use the base type as the subtype of the index is not synth-ed.
         Idx_Val := Exec_Expression_With_Basetype (Syn_Inst, Idx_Expr);
         if Idx_Val = No_Valtyp then
            --  Propagate errorc
            Off := (0, 0);
            return;
         end if;

         Strip_Const (Idx_Val);

         Bnd := Get_Array_Bound (Pfx_Type);

         pragma Assert (Is_Static (Idx_Val.Val));

         Idx_Off := Index_To_Offset (Syn_Inst, Bnd,
                                     Get_Static_Discrete (Idx_Val), Name);
         Off.Net_Off := Off.Net_Off + Idx_Off.Net_Off * Stride * El_Typ.W;
         Off.Mem_Off := Off.Mem_Off
           + Idx_Off.Mem_Off * Size_Type (Stride) * El_Typ.Sz;

         Stride := Stride * Bnd.Len;
      end loop;
   end Exec_Indexed_Name;

   procedure Exec_Slice_Const_Suffix (Syn_Inst: Synth_Instance_Acc;
                                      Expr : Node;
                                      Name : Node;
                                      Pfx_Bnd : Bound_Type;
                                      L, R : Int64;
                                      Dir : Direction_Type;
                                      El_Typ : Type_Acc;
                                      Res_Bnd : out Bound_Type;
                                      Off : out Value_Offsets)
   is
      Is_Null : Boolean;
      Len : Uns32;
   begin
      if Pfx_Bnd.Dir /= Dir then
         Error_Msg_Elab (+Name, "direction mismatch in slice");
         Off := (0, 0);
         if Dir = Dir_To then
            Res_Bnd := (Dir => Dir_To, Left => 1, Right => 0, Len => 0);
         else
            Res_Bnd := (Dir => Dir_Downto, Left => 0, Right => 1, Len => 0);
         end if;
         return;
      end if;

      --  Might be a null slice.
      case Pfx_Bnd.Dir is
         when Dir_To =>
            Is_Null := L > R;
         when Dir_Downto =>
            Is_Null := L < R;
      end case;
      if Is_Null then
         Len := 0;
         Off := (0, 0);
      else
         if not In_Bounds (Pfx_Bnd, Int32 (L))
           or else not In_Bounds (Pfx_Bnd, Int32 (R))
         then
            Error_Msg_Elab (+Name, "index not within bounds");
            Elab.Debugger.Debug_Error (Syn_Inst, Expr);
            Off := (0, 0);
            return;
         end if;

         case Pfx_Bnd.Dir is
            when Dir_To =>
               Len := Uns32 (R - L + 1);
               Off.Net_Off := Uns32 (Pfx_Bnd.Right - Int32 (R)) * El_Typ.W;
               Off.Mem_Off := Size_Type (Int32 (L) - Pfx_Bnd.Left) * El_Typ.Sz;
            when Dir_Downto =>
               Len := Uns32 (L - R + 1);
               Off.Net_Off := Uns32 (Int32 (R) - Pfx_Bnd.Right) * El_Typ.W;
               Off.Mem_Off := Size_Type (Pfx_Bnd.Left - Int32 (L)) * El_Typ.Sz;
         end case;
      end if;
      Res_Bnd := (Dir => Pfx_Bnd.Dir,
                  Len => Len,
                  Left => Int32 (L),
                  Right => Int32 (R));
   end Exec_Slice_Const_Suffix;

   procedure Exec_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                Name : Node;
                                Pfx_Bnd : Bound_Type;
                                El_Typ : Type_Acc;
                                Res_Bnd : out Bound_Type;
                                Off : out Value_Offsets)
   is
      Expr : constant Node := Get_Suffix (Name);
      Left, Right : Valtyp;
      Dir : Direction_Type;
   begin
      Off := (0, 0);

      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            --  As the range may be dynamic, cannot use synth_discrete_range.
            Left := Exec_Expression_With_Basetype
              (Syn_Inst, Get_Left_Limit (Expr));
            Right := Exec_Expression_With_Basetype
              (Syn_Inst, Get_Right_Limit (Expr));
            Dir := Get_Direction (Expr);

         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kinds_Denoting_Name =>
            declare
               Rng : Discrete_Range_Type;
            begin
               Synth_Discrete_Range (Syn_Inst, Expr, Rng);
               Exec_Slice_Const_Suffix (Syn_Inst, Expr,
                                        Name, Pfx_Bnd,
                                        Rng.Left, Rng.Right, Rng.Dir,
                                        El_Typ, Res_Bnd, Off);
               return;
            end;
         when others =>
            Error_Msg_Elab
              (+Expr, "only range expression supported for slices");
            Res_Bnd := (Dir => Dir_To, Left => 1, Right => 0, Len => 0);
            return;
      end case;

      pragma Assert (Is_Static (Left.Val));
      pragma Assert (Is_Static (Right.Val));
      Exec_Slice_Const_Suffix (Syn_Inst, Expr,
                               Name, Pfx_Bnd,
                               Get_Static_Discrete (Left),
                               Get_Static_Discrete (Right),
                               Dir,
                               El_Typ, Res_Bnd, Off);
   end Exec_Slice_Suffix;

   function Exec_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Valtyp is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Exec_Name (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            return Get_Value (Syn_Inst, Name);
         when Iir_Kind_Enumeration_Literal =>
            declare
               Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Type (Name));
               Res : Valtyp;
            begin
               Res := Create_Value_Memory (Typ);
               Write_Discrete (Res, Int64 (Get_Enum_Pos (Name)));
               return Res;
            end;
         when Iir_Kind_Unit_Declaration =>
            declare
               Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Type (Name));
            begin
               return Create_Value_Discrete
                 (Vhdl.Evaluation.Get_Physical_Value (Name), Typ);
            end;
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            declare
               Val : Valtyp;
            begin
               Val := Exec_Expression (Syn_Inst, Get_Prefix (Name));
               return Elab.Vhdl_Heap.Synth_Dereference (Read_Access (Val));
            end;
         when others =>
            Error_Kind ("exec_name", Name);
      end case;
   end Exec_Name;

   function Exec_Name_Subtype (Syn_Inst : Synth_Instance_Acc; Name : Node)
                              return Type_Acc is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Exec_Name_Subtype (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            return Get_Value (Syn_Inst, Name).Typ;
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Name));
               Pfx : constant Node := Get_Prefix (Name);
               Res : Type_Acc;
            begin
               Res := Exec_Name_Subtype (Syn_Inst, Pfx);
               Res := Res.Rec.E (Idx + 1).Typ;
               return Res;
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Pfx : constant Node := Get_Prefix (Name);
               Res : Type_Acc;
            begin
               Res := Exec_Name_Subtype (Syn_Inst, Pfx);
               return Res.Arr_El;
            end;
         when Iir_Kind_Enumeration_Literal
            | Iir_Kind_Unit_Declaration =>
            return Get_Subtype_Object (Syn_Inst, Get_Type (Name));
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            declare
               Val : Valtyp;
            begin
               Val := Exec_Expression (Syn_Inst, Get_Prefix (Name));
               Val := Elab.Vhdl_Heap.Synth_Dereference (Read_Access (Val));
               return Val.Typ;
            end;
         when Iir_Kind_Function_Call =>
            declare
               Val : Valtyp;
            begin
               Val := Synth.Vhdl_Expr.Synth_Expression (Syn_Inst, Name);
               return Val.Typ;
            end;
         when others =>
            Error_Kind ("exec_name_subtype", Name);
      end case;
   end Exec_Name_Subtype;

   procedure Exec_Assignment_Prefix (Syn_Inst : Synth_Instance_Acc;
                                     Pfx : Node;
                                     Dest_Base : out Valtyp;
                                     Dest_Typ : out Type_Acc;
                                     Dest_Off : out Value_Offsets) is
   begin
      case Get_Kind (Pfx) is
         when Iir_Kind_Simple_Name =>
            Exec_Assignment_Prefix (Syn_Inst, Get_Named_Entity (Pfx),
                                    Dest_Base, Dest_Typ, Dest_Off);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            declare
               Targ : constant Valtyp := Get_Value (Syn_Inst, Pfx);
            begin
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

         when Iir_Kind_Indexed_Name =>
            declare
               Off : Value_Offsets;
            begin
               Exec_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx), Dest_Base, Dest_Typ, Dest_Off);
               Strip_Const (Dest_Base);
               Exec_Indexed_Name (Syn_Inst, Pfx, Dest_Typ, Off);

               Dest_Off := Dest_Off + Off;
               Dest_Typ := Get_Array_Element (Dest_Typ);
            end;

         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Pfx));
            begin
               Exec_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx), Dest_Base, Dest_Typ, Dest_Off);
               Dest_Off := Dest_Off + Dest_Typ.Rec.E (Idx + 1).Offs;

               Dest_Typ := Dest_Typ.Rec.E (Idx + 1).Typ;
            end;

         when Iir_Kind_Slice_Name =>
            declare
               Pfx_Bnd : Bound_Type;
               El_Typ : Type_Acc;
               Res_Bnd : Bound_Type;
               Sl_Off : Value_Offsets;
            begin
               Exec_Assignment_Prefix
                 (Syn_Inst, Get_Prefix (Pfx), Dest_Base, Dest_Typ, Dest_Off);
               Strip_Const (Dest_Base);

               Get_Onedimensional_Array_Bounds (Dest_Typ, Pfx_Bnd, El_Typ);
               Exec_Slice_Suffix (Syn_Inst, Pfx, Pfx_Bnd, El_Typ,
                                  Res_Bnd, Sl_Off);

               --  Fixed slice.
               Dest_Typ := Create_Onedimensional_Array_Subtype
                 (Dest_Typ, Res_Bnd, El_Typ);
               Dest_Off.Net_Off := Dest_Off.Net_Off + Sl_Off.Net_Off;
               Dest_Off.Mem_Off := Dest_Off.Mem_Off + Sl_Off.Mem_Off;
            end;

         when others =>
            Error_Kind ("exec_assignment_prefix", Pfx);
      end case;
   end Exec_Assignment_Prefix;

   --  Return the type of EXPR without evaluating it.
   function Exec_Type_Of_Object (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                                return Type_Acc is
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Object_Declaration =>
            declare
               Val : constant Valtyp := Get_Value (Syn_Inst, Expr);
            begin
               return Val.Typ;
            end;
         when Iir_Kind_Simple_Name =>
            return Exec_Type_Of_Object (Syn_Inst, Get_Named_Entity (Expr));
         when Iir_Kind_Slice_Name =>
            declare
               Pfx_Typ : Type_Acc;
               Pfx_Bnd : Bound_Type;
               El_Typ : Type_Acc;
               Res_Bnd : Bound_Type;
               Sl_Off : Value_Offsets;
            begin
               Pfx_Typ := Exec_Type_Of_Object (Syn_Inst, Get_Prefix (Expr));
               Get_Onedimensional_Array_Bounds (Pfx_Typ, Pfx_Bnd, El_Typ);
               Exec_Slice_Suffix (Syn_Inst, Expr, Pfx_Bnd, El_Typ,
                                   Res_Bnd, Sl_Off);
               return Create_Onedimensional_Array_Subtype
                 (Pfx_Typ, Res_Bnd, El_Typ);
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Pfx_Typ : Type_Acc;
            begin
               Pfx_Typ := Exec_Type_Of_Object (Syn_Inst, Get_Prefix (Expr));
               return Get_Array_Element (Pfx_Typ);
            end;
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Expr));
               Pfx_Typ : Type_Acc;
            begin
               Pfx_Typ := Exec_Type_Of_Object (Syn_Inst, Get_Prefix (Expr));
               return Pfx_Typ.Rec.E (Idx + 1).Typ;
            end;

         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            declare
               Val : Valtyp;
               Res : Valtyp;
            begin
               --  Maybe do not dereference it if its type is known ?
               Val := Exec_Expression (Syn_Inst, Get_Prefix (Expr));
               Res := Elab.Vhdl_Heap.Synth_Dereference (Read_Access (Val));
               return Res.Typ;
            end;

         when Iir_Kind_String_Literal8 =>
            --  TODO: the value should be computed (once) and its type
            --  returned.
            return Synth_Subtype_Indication (Syn_Inst, Get_Type (Expr));

         when others =>
            Vhdl.Errors.Error_Kind ("synth_type_of_object", Expr);
      end case;
      return null;
   end Exec_Type_Of_Object;

   function Exec_Type_Conversion
     (Syn_Inst : Synth_Instance_Acc; Conv : Node) return Valtyp
   is
      Expr : constant Node := Get_Expression (Conv);
      Conv_Type : constant Node := Get_Type (Conv);
      Conv_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Conv_Type);
      Val : Valtyp;
   begin
      Val := Exec_Expression_With_Basetype (Syn_Inst, Expr);
      if Val = No_Valtyp then
         return No_Valtyp;
      end if;
      Strip_Const (Val);
      case Get_Kind (Conv_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Val.Typ.Kind = Type_Discrete then
               --  Int to int.
               return Val;
            elsif Val.Typ.Kind = Type_Float then
               return Create_Value_Discrete
                 (Int64 (Read_Fp64 (Val)), Conv_Typ);
            else
               Error_Msg_Elab (+Conv, "unhandled type conversion (to int)");
               return No_Valtyp;
            end if;
         when Iir_Kind_Floating_Subtype_Definition =>
            if Is_Static (Val.Val) then
               return Create_Value_Float
                 (Fp64 (Read_Discrete (Val)), Conv_Typ);
            else
               Error_Msg_Elab (+Conv, "unhandled type conversion (to float)");
               return No_Valtyp;
            end if;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            case Conv_Typ.Kind is
               when Type_Vector
                 | Type_Unbounded_Vector
                 | Type_Array
                 | Type_Unbounded_Array =>
                  return Val;
               when others =>
                  Error_Msg_Elab
                    (+Conv, "unhandled type conversion (to array)");
                  return No_Valtyp;
            end case;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            pragma Assert (Get_Base_Type (Get_Type (Expr))
                             = Get_Base_Type (Conv_Type));
            return Val;
         when others =>
            Error_Msg_Elab (+Conv, "unhandled type conversion");
            return No_Valtyp;
      end case;
   end Exec_Type_Conversion;

   function Error_Ieee_Operator (Imp : Node; Loc : Node) return Boolean
   is
      use Std_Names;
      Parent : constant Iir := Get_Parent (Imp);
   begin
      if Get_Kind (Parent) = Iir_Kind_Package_Declaration
        and then (Get_Identifier
                    (Get_Library (Get_Design_File (Get_Design_Unit (Parent))))
                    = Name_Ieee)
      then
         case Get_Identifier (Parent) is
            when Name_Std_Logic_1164
               | Name_Std_Logic_Arith
               | Name_Std_Logic_Signed
               | Name_Std_Logic_Unsigned
               | Name_Std_Logic_Misc
               | Name_Numeric_Std
               | Name_Numeric_Bit
               | Name_Math_Real =>
               Error_Msg_Elab
                 (+Loc, "unhandled predefined IEEE operator %i", +Imp);
               Error_Msg_Elab
                 (+Imp, " declared here");
               return True;
            when others =>
               --  ieee 2008 packages are handled like regular packages.
               null;
         end case;
      end if;

      return False;
   end Error_Ieee_Operator;

   function Exec_String_Literal (Syn_Inst : Synth_Instance_Acc;
                                 Str : Node;
                                 Str_Typ : Type_Acc) return Valtyp
   is
      pragma Unreferenced (Syn_Inst);
      pragma Assert (Get_Kind (Str) = Iir_Kind_String_Literal8);
      Id : constant String8_Id := Get_String8_Id (Str);

      Str_Type : constant Node := Get_Type (Str);
      El_Type : Type_Acc;
      Bounds : Bound_Type;
      Res_Type : Type_Acc;
      Res : Valtyp;
      Pos : Nat8;
   begin
      case Str_Typ.Kind is
         when Type_Vector
           | Type_Array =>
            Bounds := Str_Typ.Abound;
         when Type_Unbounded_Vector
            | Type_Unbounded_Array =>
            Bounds := Synth_Bounds_From_Length
              (Get_Index_Type (Str_Type, 0), Get_String_Length (Str));
         when others =>
            raise Internal_Error;
      end case;

      El_Type := Get_Array_Element (Str_Typ);
      if El_Type.Kind in Type_Nets then
         Res_Type := Create_Vector_Type (Bounds, El_Type);
      else
         Res_Type := Create_Array_Type (Bounds, True, El_Type);
      end if;
      Res := Create_Value_Memory (Res_Type);

      --  Only U8 are handled.
      pragma Assert (El_Type.Sz = 1);

      --  From left to right.
      for I in 1 .. Bounds.Len loop
         -- FIXME: use literal from type ??
         Pos := Str_Table.Element_String8 (Id, Pos32 (I));
         Write_U8 (Res.Val.Mem + Size_Type (I - 1), Nat8'Pos (Pos));
      end loop;

      return Res;
   end Exec_String_Literal;

   --  Return the left bound if the direction of the range is LEFT_DIR.
   function Synth_Low_High_Type_Attribute
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Left_Dir : Direction_Type)
     return Valtyp
   is
      Typ : Type_Acc;
      R : Int64;
   begin
      Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Get_Prefix (Expr)));
      pragma Assert (Typ.Kind = Type_Discrete);
      if Typ.Drange.Dir = Left_Dir then
         R := Typ.Drange.Left;
      else
         R := Typ.Drange.Right;
      end if;
      return Create_Value_Discrete (R, Typ);
   end Synth_Low_High_Type_Attribute;

   function Exec_Short_Circuit (Syn_Inst : Synth_Instance_Acc;
                                 Val : Int64;
                                 Left_Expr : Node;
                                 Right_Expr : Node;
                                 Typ : Type_Acc) return Valtyp
   is
      Left : Valtyp;
      Right : Valtyp;
   begin
      Left := Exec_Expression_With_Type (Syn_Inst, Left_Expr, Typ);
      if Left = No_Valtyp then
         --  Propagate error.
         return No_Valtyp;
      end if;
      pragma Assert (Is_Static (Left.Val));
      if Get_Static_Discrete (Left) = Val then
         --  Short-circuit when the left operand determines the result.
         return Create_Value_Discrete (Val, Typ);
      end if;

      Strip_Const (Left);
      Right := Exec_Expression_With_Type (Syn_Inst, Right_Expr, Typ);
      if Right = No_Valtyp then
         --  Propagate error.
         return No_Valtyp;
      end if;
      Strip_Const (Right);

      pragma Assert (Is_Static (Right.Val));
      if Get_Static_Discrete (Right) = Val then
         --  If the right operand can determine the result, return it.
         return Create_Value_Discrete (Val, Typ);
      end if;

      --  Return a static value if both operands are static.
      --  Note: we know the value of left if it is not constant.
      return Create_Value_Discrete (Get_Static_Discrete (Right), Typ);
   end Exec_Short_Circuit;

   function Exec_Expression_With_Type (Syn_Inst : Synth_Instance_Acc;
                                       Expr : Node;
                                       Expr_Type : Type_Acc) return Valtyp is
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
            begin
               --  Specially handle short-circuit operators.
               case Def is
                  when Iir_Predefined_Boolean_And =>
                     return Exec_Short_Circuit
                       (Syn_Inst, 0, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type);
                  when Iir_Predefined_Boolean_Or =>
                     return Exec_Short_Circuit
                       (Syn_Inst, 1, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type);
                  when Iir_Predefined_Bit_And =>
                     return Exec_Short_Circuit
                       (Syn_Inst, 0, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type);
                  when Iir_Predefined_Bit_Or =>
                     return Exec_Short_Circuit
                       (Syn_Inst, 1, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type);
                  when Iir_Predefined_None =>
                     if Error_Ieee_Operator (Imp, Expr) then
                        return No_Valtyp;
                     else
                        return Synth_User_Operator
                          (Syn_Inst, Get_Left (Expr), Get_Right (Expr), Expr);
                     end if;
                  when others =>
                     return Synth_Dyadic_Operation
                       (Syn_Inst, Imp,
                        Get_Left (Expr), Get_Right (Expr), Expr);
               end case;
            end;
         when Iir_Kinds_Monadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
            begin
               if Def = Iir_Predefined_None then
                  if Error_Ieee_Operator (Imp, Expr) then
                     return No_Valtyp;
                  else
                     return Synth_User_Operator
                       (Syn_Inst, Get_Operand (Expr), Null_Node, Expr);
                  end if;
               else
                  return Synth_Monadic_Operation
                    (Syn_Inst, Imp, Get_Operand (Expr), Expr);
               end if;
            end;
         when Iir_Kind_Simple_Name
            | Iir_Kind_Selected_Name
            | Iir_Kind_Interface_Signal_Declaration --  For PSL.
            | Iir_Kind_Signal_Declaration   -- For PSL.
            | Iir_Kind_Implicit_Dereference
            | Iir_Kind_Dereference =>
            declare
               Res : Valtyp;
            begin
               Res := Exec_Name (Syn_Inst, Expr);
               if Res.Val.Kind = Value_Signal then
                  Vhdl_Errors.Error_Msg_Elab
                    (+Expr, "cannot use signal value during elaboration");
                  return No_Valtyp;
               end if;
               if Res.Typ /= null
                 and then Res.Typ.W = 0 and then Res.Val.Kind /= Value_Memory
               then
                  --  This is a null object.  As nothing can be done about it,
                  --  returns 0.
                  return Create_Value_Memtyp (Create_Memory_Zero (Res.Typ));
               end if;
               return Res;
            end;
         when Iir_Kind_Reference_Name =>
            --  Only used for anonymous signals in internal association.
            return Exec_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            declare
               Base : Valtyp;
               Typ : Type_Acc;
               Off : Value_Offsets;
               Res : Valtyp;
            begin
               Exec_Assignment_Prefix (Syn_Inst, Expr, Base, Typ, Off);
               Res := Create_Value_Memory (Typ);
               Copy_Memory (Res.Val.Mem, Base.Val.Mem + Off.Mem_Off, Typ.Sz);
               return Res;
            end;
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Expr));
               Pfx : constant Node := Get_Prefix (Expr);
               Res_Typ : Type_Acc;
               Val : Valtyp;
               Res : Valtyp;
            begin
               Val := Exec_Expression (Syn_Inst, Pfx);
               Strip_Const (Val);
               Res_Typ := Val.Typ.Rec.E (Idx + 1).Typ;
               if Res_Typ.W = 0 and then Val.Val.Kind /= Value_Memory then
                  --  This is a null object.  As nothing can be done about it,
                  --  returns 0.
                  return Create_Value_Memtyp (Create_Memory_Zero (Res_Typ));
               end if;
               pragma Assert (Is_Static (Val.Val));
               Res := Create_Value_Memory (Res_Typ);
               Copy_Memory
                 (Res.Val.Mem,
                  Val.Val.Mem + Val.Typ.Rec.E (Idx + 1).Offs.Mem_Off,
                  Res_Typ.Sz);
               return Res;
            end;
         when Iir_Kind_Character_Literal =>
            return Exec_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Integer_Literal =>
            declare
               Res : Valtyp;
            begin
               Res := Create_Value_Memory (Expr_Type);
               Write_Discrete (Res, Get_Value (Expr));
               return Res;
            end;
         when Iir_Kind_Floating_Point_Literal =>
            return Create_Value_Float (Get_Fp_Value (Expr), Expr_Type);
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return Create_Value_Discrete
              (Get_Physical_Value (Expr), Expr_Type);
         when Iir_Kind_String_Literal8 =>
            return Exec_String_Literal (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Enumeration_Literal =>
            return Exec_Name (Syn_Inst, Expr);
         when Iir_Kind_Type_Conversion =>
            return Exec_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Exec_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr),
               Get_Subtype_Object (Syn_Inst, Get_Type (Get_Type_Mark (Expr))));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
            begin
               case Get_Implicit_Definition (Imp) is
                  when Iir_Predefined_Operators
                     | Iir_Predefined_Ieee_Numeric_Std_Binary_Operators =>
                     return Synth_Operator_Function_Call (Syn_Inst, Expr);
                  when Iir_Predefined_None =>
                     return Synth_User_Function_Call (Syn_Inst, Expr);
                  when others =>
                     return Synth_Predefined_Function_Call (Syn_Inst, Expr);
               end case;
            end;
         when Iir_Kind_Aggregate =>
            return Synth.Vhdl_Aggr.Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Simple_Aggregate =>
            return Exec_Simple_Aggregate (Syn_Inst, Expr);
         when Iir_Kind_Parenthesis_Expression =>
            return Exec_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr), Expr_Type);
         when Iir_Kind_Left_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Left), Expr_Type);
            end;
         when Iir_Kind_Right_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Right), Expr_Type);
            end;
         when Iir_Kind_High_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Dir_To =>
                     V := B.Right;
                  when Dir_Downto =>
                     V := B.Left;
               end case;
               return Create_Value_Discrete (Int64 (V), Expr_Type);
            end;
         when Iir_Kind_Low_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Dir_To =>
                     V := B.Left;
                  when Dir_Downto =>
                     V := B.Right;
               end case;
               return Create_Value_Discrete (Int64 (V), Expr_Type);
            end;
         when Iir_Kind_Length_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Len), Expr_Type);
            end;
         when Iir_Kind_Ascending_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int64;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Dir_To =>
                     V := 1;
                  when Dir_Downto =>
                     V := 0;
               end case;
               return Create_Value_Discrete (V, Expr_Type);
            end;

         when Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute =>
            declare
               Param : constant Node := Get_Parameter (Expr);
               V : Valtyp;
               Dtype : Type_Acc;
            begin
               V := Exec_Expression (Syn_Inst, Param);
               Dtype := Get_Subtype_Object (Syn_Inst, Get_Type (Expr));
               --  FIXME: to be generalized.  Not always as simple as a
               --  subtype conversion.
               return Exec_Subtype_Conversion (V, Dtype, False, Expr);
            end;
         when Iir_Kind_Low_Type_Attribute =>
            return Synth_Low_High_Type_Attribute (Syn_Inst, Expr, Dir_To);
         when Iir_Kind_High_Type_Attribute =>
            return Synth_Low_High_Type_Attribute (Syn_Inst, Expr, Dir_Downto);
         when Iir_Kind_Value_Attribute =>
            return Exec_Value_Attribute (Syn_Inst, Expr);
         when Iir_Kind_Image_Attribute =>
            return Exec_Image_Attribute (Syn_Inst, Expr);
         when Iir_Kind_Instance_Name_Attribute =>
            return Exec_Instance_Name_Attribute (Syn_Inst, Expr);
         when Iir_Kind_Null_Literal =>
            return Create_Value_Access (Null_Heap_Index, Expr_Type);
         when Iir_Kind_Allocator_By_Subtype =>
            declare
               T : Type_Acc;
               Acc : Heap_Index;
            begin
               T := Synth_Subtype_Indication
                 (Syn_Inst, Get_Subtype_Indication (Expr));
               Acc := Allocate_By_Type (T);
               return Create_Value_Access (Acc, Expr_Type);
            end;
         when Iir_Kind_Allocator_By_Expression =>
            declare
               V : Valtyp;
               Acc : Heap_Index;
            begin
               V := Exec_Expression_With_Type
                 (Syn_Inst, Get_Expression (Expr), Expr_Type.Acc_Acc);
               Acc := Allocate_By_Value (V);
               return Create_Value_Access (Acc, Expr_Type);
            end;
         when Iir_Kind_Stable_Attribute =>
            Error_Msg_Elab (+Expr, "signal attribute not supported");
            return No_Valtyp;
         when Iir_Kind_Overflow_Literal =>
            Error_Msg_Elab (+Expr, "out of bound expression");
            return No_Valtyp;
         when others =>
            Error_Kind ("exec_expression_with_type", Expr);
      end case;
   end Exec_Expression_With_Type;

   function Exec_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Valtyp
   is
      Etype : Node;
   begin
      Etype := Get_Type (Expr);

      case Get_Kind (Expr) is
         when Iir_Kind_High_Array_Attribute
           |  Iir_Kind_Low_Array_Attribute
           |  Iir_Kind_Integer_Literal =>
            --  The type of this attribute is the type of the index, which is
            --  not synthesized as atype (only as an index).
            --  For integer_literal, the type is not really needed, and it
            --  may be created by static evaluation of an array attribute.
            Etype := Get_Base_Type (Etype);
         when others =>
            null;
      end case;

      return Exec_Expression_With_Type
        (Syn_Inst, Expr, Get_Subtype_Object (Syn_Inst, Etype));
   end Exec_Expression;

   function Exec_Expression_With_Basetype
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp
   is
      Basetype : Type_Acc;
   begin
      Basetype := Get_Subtype_Object
        (Syn_Inst, Get_Base_Type (Get_Type (Expr)));
      return Exec_Expression_With_Type (Syn_Inst, Expr, Basetype);
   end Exec_Expression_With_Basetype;
end Elab.Vhdl_Expr;
