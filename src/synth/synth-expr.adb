--  Expressions synthesis.
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

with Types_Utils; use Types_Utils;
with Name_Table;
with Std_Names;
with Str_Table;
with Mutils; use Mutils;

with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Annotations; use Vhdl.Annotations;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Utils; use Netlists.Utils;

with Synth.Errors; use Synth.Errors;
with Synth.Environment;
with Synth.Decls;
with Synth.Stmts; use Synth.Stmts;
with Synth.Oper; use Synth.Oper;
with Synth.Heap; use Synth.Heap;
with Synth.Debugger;
with Synth.Aggr;

with Grt.Types;
with Grt.To_Strings;

package body Synth.Expr is
   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Valtyp;

   procedure Set_Location (N : Net; Loc : Node)
     renames Synth.Source.Set_Location;

   function Get_Value_Memtyp (V : Valtyp) return Memtyp is
   begin
      case V.Val.Kind is
         when Value_Memory =>
            return (V.Typ, V.Val.Mem);
         when Value_Const =>
            return Get_Memtyp (V);
         when Value_Wire =>
            return Synth.Environment.Get_Static_Wire (V.Val.W);
         when Value_Alias =>
            declare
               Res : Memtyp;
            begin
               Res := Get_Value_Memtyp ((V.Val.A_Typ, V.Val.A_Obj));
               return (V.Typ, Res.Mem + V.Val.A_Off.Mem_Off);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Value_Memtyp;

   function Get_Static_Discrete (V : Valtyp) return Int64 is
   begin
      case V.Val.Kind is
         when Value_Memory =>
            return Read_Discrete (V);
         when Value_Const =>
            return Read_Discrete (Get_Memtyp (V));
         when Value_Wire =>
            return Read_Discrete (Synth.Environment.Get_Static_Wire (V.Val.W));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Static_Discrete;

   function Is_Positive (V : Valtyp) return Boolean
   is
      N : Net;
      Inst : Instance;
   begin
      pragma Assert (V.Typ.Kind = Type_Discrete);
      case V.Val.Kind is
         when Value_Const
           | Value_Memory =>
            return Read_Discrete (Get_Memtyp (V)) >= 0;
         when Value_Net =>
            N := V.Val.N;
         when Value_Wire =>
            return Read_Discrete
              (Synth.Environment.Get_Static_Wire (V.Val.W)) >= 0;
         when others =>
            raise Internal_Error;
      end case;
      Inst := Get_Net_Parent (N);
      case Get_Id (Inst) is
         when Id_Uextend
           | Id_Const_UB32 =>
            return True;
         when others =>
            --  Be conservative.
            return False;
      end case;
   end Is_Positive;

   procedure From_Std_Logic (Enum : Int64; Val : out Uns32; Zx : out Uns32) is
   begin
      case Enum is
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_0_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_L_Pos =>
            Val := 0;
            Zx := 0;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_1_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_H_Pos =>
            Val := 1;
            Zx := 0;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_U_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_X_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_D_Pos =>
            Val := 1;
            Zx := 1;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_Z_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_W_Pos =>
            Val := 0;
            Zx := 1;
         when others =>
            --  Only 9 values.
            raise Internal_Error;
      end case;
   end From_Std_Logic;

   procedure From_Bit (Enum : Int64; Val : out Uns32) is
   begin
      if Enum = 0 then
         Val := 0;
      elsif Enum = 1 then
         Val := 1;
      else
         raise Internal_Error;
      end if;
   end From_Bit;

   procedure To_Logic
     (Enum : Int64; Etype : Type_Acc; Val : out Uns32; Zx : out Uns32) is
   begin
      if Etype = Logic_Type then
         pragma Assert (Etype.Kind = Type_Logic);
         From_Std_Logic (Enum, Val, Zx);
      elsif Etype = Boolean_Type or Etype = Bit_Type then
         pragma Assert (Etype.Kind = Type_Bit);
         From_Bit (Enum, Val);
         Zx := 0;
      else
         raise Internal_Error;
      end if;
   end To_Logic;

   procedure Uns2logvec (Val : Uns64;
                         W : Width;
                         Vec : in out Logvec_Array;
                         Off : in out Uns32) is
   begin
      if W = 0 then
         return;
      end if;

      for I in 0 .. W - 1 loop
         declare
            B : constant Uns32 := Uns32 (Shift_Right (Val, Natural (I)) and 1);
            Idx : constant Digit_Index := Digit_Index (Off / 32);
            Pos : constant Natural := Natural (Off mod 32);
         begin
            Vec (Idx).Val := Vec (Idx).Val or Shift_Left (B, Pos);
         end;
         Off := Off + 1;
      end loop;
   end Uns2logvec;

   procedure Bit2logvec (Val : Uns32;
                         Vec : in out Logvec_Array;
                         Off : in out Uns32)
   is
      pragma Assert (Val <= 1);
      Idx : constant Digit_Index := Digit_Index (Off / 32);
      Pos : constant Natural := Natural (Off mod 32);
      Va : Uns32;
   begin
      Va := Shift_Left (Val, Pos);
      Vec (Idx).Val := Vec (Idx).Val or Va;
      Vec (Idx).Zx := 0;
      Off := Off + 1;
   end Bit2logvec;

   procedure Logic2logvec (Val : Int64;
                           Vec : in out Logvec_Array;
                           Off : in out Uns32;
                           Has_Zx : in out Boolean)
   is
      pragma Assert (Val <= 8);
      Idx : constant Digit_Index := Digit_Index (Off / 32);
      Pos : constant Natural := Natural (Off mod 32);
      Va : Uns32;
      Zx : Uns32;
   begin
      From_Std_Logic (Val, Va, Zx);
      Has_Zx := Has_Zx or Zx /= 0;
      Va := Shift_Left (Va, Pos);
      Zx := Shift_Left (Zx, Pos);
      Vec (Idx).Val := Vec (Idx).Val or Va;
      Vec (Idx).Zx := Vec (Idx).Zx or Zx;
      Off := Off + 1;
   end Logic2logvec;

   procedure Value2logvec (Mem : Memory_Ptr;
                           Typ : Type_Acc;
                           Off : in out Uns32;
                           W : in out Width;
                           Vec : in out Logvec_Array;
                           Vec_Off : in out Uns32;
                           Has_Zx : in out Boolean) is
   begin
      if Off >= Typ.W then
         --  Offset not yet reached.
         Off := Off - Typ.W;
         return;
      end if;
      if W = 0 then
         return;
      end if;

      case Typ.Kind is
         when Type_Bit =>
            --  Scalar bits cannot be cut.
            pragma Assert (Off = 0 and W >= Typ.W);
            Bit2logvec (Uns32 (Read_U8 (Mem)), Vec, Vec_Off);
            W := W - Typ.W;
         when Type_Logic =>
            --  Scalar bits cannot be cut.
            pragma Assert (Off = 0 and W >= Typ.W);
            Logic2logvec (Int64 (Read_U8 (Mem)), Vec, Vec_Off, Has_Zx);
            W := W - Typ.W;
         when Type_Discrete =>
            --  Scalar bits cannot be cut.
            pragma Assert (Off = 0 and W >= Typ.W);
            Uns2logvec (To_Uns64 (Read_Discrete (Memtyp'(Typ, Mem))),
                        Typ.W, Vec, Vec_Off);
            W := W - Typ.W;
         when Type_Float =>
            --  Fp64 is for sure 64 bits.  Assume the endianness of floats is
            --  the same as integers endianness.
            --  Scalar bits cannot be cut.
            pragma Assert (Off = 0 and W >= Typ.W);
            Uns2logvec (To_Uns64 (Read_Fp64 (Mem)), 64, Vec, Vec_Off);
            W := W - Typ.W;
         when Type_Vector =>
            declare
               Vlen : Uns32;
            begin
               Vlen := Uns32 (Vec_Length (Typ));
               pragma Assert (Off < Vlen);
               pragma Assert (Vlen > 0);

               if Vlen > Off + W then
                  Vlen := Off + W;
               end if;
               case Typ.Vec_El.Kind is
                  when Type_Bit =>
                     --  TODO: optimize off mod 32 = 0.
                     for I in reverse Off + 1 .. Vlen loop
                        Bit2logvec (Uns32 (Read_U8 (Mem + Size_Type (I - 1))),
                                    Vec, Vec_Off);
                     end loop;
                  when Type_Logic =>
                     for I in reverse Off + 1 .. Vlen loop
                        Logic2logvec
                          (Int64 (Read_U8 (Mem + Size_Type (I - 1))),
                           Vec, Vec_Off, Has_Zx);
                     end loop;
                  when others =>
                     raise Internal_Error;
               end case;
               W := W - (Vlen - Off);
               Off := 0;
            end;
         when Type_Array =>
            declare
               Alen : constant Iir_Index32 := Get_Array_Flat_Length (Typ);
               El_Typ : constant Type_Acc := Typ.Arr_El;
            begin
               for I in reverse 1 .. Alen loop
                  Value2logvec (Mem + Size_Type (I - 1) * El_Typ.Sz, El_Typ,
                                Off, W, Vec, Vec_Off, Has_Zx);
                  exit when W = 0;
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Value2logvec (Mem + Typ.Rec.E (I).Moff, Typ.Rec.E (I).Typ,
                             Off, W, Vec, Vec_Off, Has_Zx);
               exit when W = 0;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Value2logvec;

   procedure Value2logvec (Val : Memtyp;
                           Off : Uns32;
                           W : Width;
                           Vec : in out Logvec_Array;
                           Vec_Off : in out Uns32;
                           Has_Zx : in out Boolean)
   is
      Off1 : Uns32;
      W1 : Width;
   begin
      Off1 := Off;
      W1 := W;
      Value2logvec (Val.Mem, Val.Typ, Off1, W1, Vec, Vec_Off, Has_Zx);
      pragma Assert (Off1 = 0);
      pragma Assert (W1 = 0);
   end Value2logvec;

   --  Resize for a discrete value.
   function Synth_Resize (Val : Valtyp; W : Width; Loc : Node) return Net
   is
      Wn : constant Width := Val.Typ.W;
      N : Net;
      Res : Net;
      V : Int64;
   begin
      if Is_Static (Val.Val)
        and then Wn /= W
      then
         --  Optimization: resize directly.
         V := Read_Discrete (Val);
         if Val.Typ.Drange.Is_Signed then
            Res := Build2_Const_Int (Build_Context, V, W);
         else
            Res := Build2_Const_Uns (Build_Context, To_Uns64 (V), W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      end if;

      N := Get_Net (Val);
      if Wn > W then
         return Build2_Trunc (Build_Context, Id_Utrunc, N, W,
                              Get_Location (Loc));
      elsif Wn < W then
         if Val.Typ.Drange.Is_Signed then
            Res := Build_Extend (Build_Context, Id_Sextend, N, W);
         else
            Res := Build_Extend (Build_Context, Id_Uextend, N, W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      else
         return N;
      end if;
   end Synth_Resize;

   procedure Concat_Array (Arr : in out Net_Array)
   is
      Last : Int32;
      Idx, New_Idx : Int32;
   begin
      Last := Arr'Last;
      while Last > Arr'First loop
         Idx := Arr'First;
         New_Idx := Arr'First - 1;
         while Idx <= Last loop
            --  Gather at most 4 nets.
            New_Idx := New_Idx + 1;

            if Idx = Last then
               Arr (New_Idx) := Arr (Idx);
               Idx := Idx + 1;
            elsif Idx + 1 = Last then
               Arr (New_Idx) := Build_Concat2
                 (Build_Context, Arr (Idx), Arr (Idx + 1));
               Idx := Idx + 2;
            elsif Idx + 2 = Last then
               Arr (New_Idx) := Build_Concat3
                 (Build_Context, Arr (Idx), Arr (Idx + 1), Arr (Idx + 2));
               Idx := Idx + 3;
            else
               Arr (New_Idx) := Build_Concat4
                 (Build_Context,
                  Arr (Idx), Arr (Idx + 1), Arr (Idx + 2), Arr (Idx + 3));
               Idx := Idx + 4;
            end if;
         end loop;
         Last := New_Idx;
      end loop;
   end Concat_Array;

   procedure Concat_Array (Arr : in out Net_Array; N : out Net) is
   begin
      Concat_Array (Arr);
      N := Arr (Arr'First);
   end Concat_Array;

   function Synth_Discrete_Range_Expression
     (L : Int64; R : Int64; Dir : Iir_Direction) return Discrete_Range_Type is
   begin
      return (Dir => Dir,
              Left => L,
              Right => R,
              Is_Signed => L < 0 or R < 0);
   end Synth_Discrete_Range_Expression;

   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type
   is
      L, R : Valtyp;
      Lval, Rval : Int64;
   begin
      L := Synth_Expression_With_Basetype (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression_With_Basetype (Syn_Inst, Get_Right_Limit (Rng));
      Strip_Const (L);
      Strip_Const (R);

      if not (Is_Static (L.Val) and Is_Static (R.Val)) then
         Error_Msg_Synth (+Rng, "limits of range are not constant");
         raise Internal_Error;
      end if;

      Lval := Read_Discrete (L);
      Rval := Read_Discrete (R);
      return (Dir => Get_Direction (Rng),
              Left => Lval,
              Right => Rval,
              Is_Signed => Lval < 0 or Rval < 0);
   end Synth_Discrete_Range_Expression;

   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type
   is
      L, R : Valtyp;
   begin
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));
      return (Get_Direction (Rng), Read_Fp64 (L), Read_Fp64 (R));
   end Synth_Float_Range_Expression;

   --  Return the type of EXPR without evaluating it.
   function Synth_Type_Of_Object (Syn_Inst : Synth_Instance_Acc; Expr : Node)
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
            return Synth_Type_Of_Object (Syn_Inst, Get_Named_Entity (Expr));
         when Iir_Kind_Slice_Name =>
            declare
               Pfx_Typ : Type_Acc;
               Pfx_Bnd : Bound_Type;
               El_Typ : Type_Acc;
               Res_Bnd : Bound_Type;
               Sl_Voff : Net;
               Sl_Off : Value_Offsets;
            begin
               Pfx_Typ := Synth_Type_Of_Object (Syn_Inst, Get_Prefix (Expr));
               Get_Onedimensional_Array_Bounds (Pfx_Typ, Pfx_Bnd, El_Typ);
               Synth_Slice_Suffix (Syn_Inst, Expr, Pfx_Bnd, El_Typ,
                                   Res_Bnd, Sl_Voff, Sl_Off);

               if Sl_Voff /= No_Net then
                  raise Internal_Error;
               end if;
               return Create_Onedimensional_Array_Subtype (Pfx_Typ, Res_Bnd);
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Pfx_Typ : Type_Acc;
            begin
               Pfx_Typ := Synth_Type_Of_Object (Syn_Inst, Get_Prefix (Expr));
               return Get_Array_Element (Pfx_Typ);
            end;
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Expr));
               Pfx_Typ : Type_Acc;
            begin
               Pfx_Typ := Synth_Type_Of_Object (Syn_Inst, Get_Prefix (Expr));
               return Pfx_Typ.Rec.E (Idx + 1).Typ;
            end;

         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            declare
               Val : Valtyp;
               Res : Valtyp;
            begin
               --  Maybe do not dereference it if its type is known ?
               Val := Synth_Expression (Syn_Inst, Get_Prefix (Expr));
               Res := Heap.Synth_Dereference (Read_Access (Val));
               return Res.Typ;
            end;

         when others =>
            Vhdl.Errors.Error_Kind ("synth_type_of_object", Expr);
      end case;
      return null;
   end Synth_Type_Of_Object;

   function Synth_Array_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Bound_Type
   is
      Prefix : constant Iir := Strip_Denoting_Name (Get_Prefix (Attr));
      Dim : constant Natural :=
        Vhdl.Evaluation.Eval_Attribute_Parameter_Or_1 (Attr);
      Typ : Type_Acc;
   begin
      --  Prefix is an array object or an array subtype.
      if Get_Kind (Prefix) = Iir_Kind_Subtype_Declaration then
         --  TODO: does this cover all the cases ?
         Typ := Get_Subtype_Object (Syn_Inst, Get_Subtype_Indication (Prefix));
      else
         Typ := Synth_Type_Of_Object (Syn_Inst, Prefix);
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
               Rng := Discrete_Range_Type'(Dir => B.Dir,
                                           Is_Signed => True,
                                           Left => Int64 (B.Left),
                                           Right => Int64 (B.Right));
            end;
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            declare
               B : Bound_Type;
               T : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Bound);
               --  Reverse
               case B.Dir is
                  when Iir_To =>
                     B.Dir := Iir_Downto;
                  when Iir_Downto =>
                     B.Dir := Iir_To;
               end case;
               T := B.Right;
               B.Right := B.Left;
               B.Left := T;

               Rng := Discrete_Range_Type'(Dir => B.Dir,
                                           Is_Signed => True,
                                           Left => Int64 (B.Left),
                                           Right => Int64 (B.Right));
            end;
         when others =>
            Error_Kind ("synth_discrete_range", Bound);
      end case;
   end Synth_Discrete_Range;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Dim_Type) return Bound_Type
   is
      Info : constant Sim_Info_Acc := Get_Info (Atype);
   begin
      if Info = null then
         pragma Assert (Get_Type_Declarator (Atype) = Null_Node);
         declare
            Index_Type : constant Node :=
              Get_Index_Type (Atype, Natural (Dim - 1));
         begin
            return Synth_Bounds_From_Range (Syn_Inst, Index_Type);
         end;
      else
         declare
            Bnds : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Atype);
         begin
            case Bnds.Kind is
               when Type_Vector =>
                  pragma Assert (Dim = 1);
                  return Bnds.Vbound;
               when Type_Array =>
                  return Bnds.Abounds.D (Dim);
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end if;
   end Synth_Array_Bounds;

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

   function Synth_Simple_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node) return Valtyp
   is
      Aggr_Type : constant Node := Get_Type (Aggr);
      pragma Assert (Get_Nbr_Dimensions (Aggr_Type) = 1);
      El_Type : constant Node := Get_Element_Subtype (Aggr_Type);
      El_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, El_Type);
      Els : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
      Last : constant Natural := Flist_Last (Els);
      Bnd : Bound_Type;
      Bnds : Bound_Array_Acc;
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
         Bnds := Create_Bound_Array (1);
         Bnds.D (1) := Bnd;
         Res_Type := Create_Array_Type (Bnds, El_Typ);
      end if;

      Res := Create_Value_Memory (Res_Type);

      for I in Flist_First .. Last loop
         Val := Synth_Expression_With_Type
           (Syn_Inst, Get_Nth_Element (Els, I), El_Typ);
         pragma Assert (Is_Static (Val.Val));
         Write_Value (Res.Val.Mem + Size_Type (I) * El_Typ.Sz, Val);
      end loop;

      return Res;
   end Synth_Simple_Aggregate;

   --  Change the bounds of VAL.
   function Reshape_Value (Val : Valtyp; Ntype : Type_Acc) return Valtyp is
   begin
      case Val.Val.Kind is
         when Value_Wire =>
            return Create_Value_Wire (Val.Val.W, Ntype);
         when Value_Net =>
            return Create_Value_Net (Val.Val.N, Ntype);
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

   function Synth_Subtype_Conversion (Vt : Valtyp;
                                      Dtype : Type_Acc;
                                      Bounds : Boolean;
                                      Loc : Source.Syn_Src)
                                     return Valtyp
   is
      Vtype : constant Type_Acc := Vt.Typ;
   begin
      if Vt = No_Valtyp then
         --  Propagate error.
         return No_Valtyp;
      end if;

      case Dtype.Kind is
         when Type_Bit =>
            pragma Assert (Vtype.Kind = Type_Bit);
            return Vt;
         when Type_Logic =>
            pragma Assert (Vtype.Kind = Type_Logic);
            return Vt;
         when Type_Discrete =>
            pragma Assert (Vtype.Kind = Type_Discrete);
            declare
               N : Net;
            begin
               if Vtype.W /= Dtype.W then
                  --  Truncate.
                  --  TODO: check overflow.
                  case Vt.Val.Kind is
                     when Value_Net
                       | Value_Wire
                       | Value_Alias =>
                        if Is_Static_Val (Vt.Val) then
                           return Create_Value_Discrete
                             (Get_Static_Discrete (Vt), Dtype);
                        end if;

                        N := Get_Net (Vt);
                        if Vtype.Drange.Is_Signed then
                           N := Build2_Sresize
                             (Build_Context, N, Dtype.W, Get_Location (Loc));
                        else
                           N := Build2_Uresize
                             (Build_Context, N, Dtype.W, Get_Location (Loc));
                        end if;
                        return Create_Value_Net (N, Dtype);
                     when Value_Const =>
                        return Synth_Subtype_Conversion
                          ((Vt.Typ, Vt.Val.C_Val), Dtype, Bounds, Loc);
                     when Value_Memory =>
                        return Create_Value_Discrete
                          (Read_Discrete (Vt), Dtype);
                     when others =>
                        raise Internal_Error;
                  end case;
               else
                  --  TODO: check overflow if sign differ.
                  return Vt;
               end if;
            end;
         when Type_Float =>
            pragma Assert (Vtype.Kind = Type_Float);
            --  TODO: check range
            return Vt;
         when Type_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or Vtype.Kind = Type_Slice);
            if Dtype.W /= Vtype.W then
               Error_Msg_Synth (+Loc, "mismatching vector length");
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
            --  TODO: check bounds, handle elements
            if Bounds then
               return Reshape_Value (Vt, Dtype);
            else
               return Vt;
            end if;
         when Type_Unbounded_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            return Vt;
         when Type_Unbounded_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or else Vtype.Kind = Type_Slice);
            return Vt;
         when Type_Record =>
            --  TODO: handle elements.
            return Vt;
         when Type_Access =>
            return Vt;
         when Type_File =>
            pragma Assert (Vtype = Dtype);
            return Vt;
      end case;
   end Synth_Subtype_Conversion;

   function Synth_Value_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Valtyp
   is
      Param : constant Node := Get_Parameter (Attr);
      Etype : constant Node := Get_Type (Attr);
      Btype : constant Node := Get_Base_Type (Etype);
      V : Valtyp;
      Dtype : Type_Acc;
   begin
      V := Synth_Expression (Syn_Inst, Param);
      if V = No_Valtyp then
         return No_Valtyp;
      end if;

      Dtype := Get_Subtype_Object (Syn_Inst, Etype);
      if not Is_Static (V.Val) then
         Error_Msg_Synth (+Attr, "parameter of 'value must be static");
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
               Error_Msg_Synth (+Attr, "unhandled type for 'value");
               return No_Valtyp;
         end case;
         return Create_Value_Discrete (Val, Dtype);
      end;
   end Synth_Value_Attribute;

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
            Error_Kind ("execute_image_attribute", Expr_Type);
      end case;
   end Synth_Image_Attribute_Str;

   function String_To_Valtyp (Str : String; Styp : Type_Acc) return Valtyp
   is
      Len : constant Natural := Str'Length;
      Bnd : Bound_Array_Acc;
      Typ : Type_Acc;
      Res : Valtyp;
   begin
      Bnd := Create_Bound_Array (1);
      Bnd.D (1) := (Dir => Iir_To, Left => 1, Right => Int32 (Len),
                    Len => Width (Len));
      Typ := Create_Array_Type (Bnd, Styp.Uarr_El);

      Res := Create_Value_Memory (Typ);
      for I in Str'Range loop
         Write_U8 (Res.Val.Mem + Size_Type (I - Str'First),
                   Character'Pos (Str (I)));
      end loop;
      return Res;
   end String_To_Valtyp;

   function Synth_Image_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Valtyp
   is
      Param : constant Node := Get_Parameter (Attr);
      Etype : constant Node := Get_Type (Attr);
      V : Valtyp;
      Dtype : Type_Acc;
   begin
      V := Synth_Expression (Syn_Inst, Param);
      if V = No_Valtyp then
         return No_Valtyp;
      end if;
      Dtype := Get_Subtype_Object (Syn_Inst, Etype);
      if not Is_Static (V.Val) then
         Error_Msg_Synth (+Attr, "parameter of 'image must be static");
         return No_Valtyp;
      end if;

      Strip_Const (V);
      return String_To_Valtyp
        (Synth_Image_Attribute_Str (V, Get_Type (Param)), Dtype);
   end Synth_Image_Attribute;

   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Valtyp is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Name));
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
               Val := Synth_Expression (Syn_Inst, Get_Prefix (Name));
               return Heap.Synth_Dereference (Read_Access (Val));
            end;
         when others =>
            Error_Kind ("synth_name", Name);
      end case;
   end Synth_Name;

   function In_Bounds (Bnd : Bound_Type; V : Int32) return Boolean is
   begin
      case Bnd.Dir is
         when Iir_To =>
            return V >= Bnd.Left and then V <= Bnd.Right;
         when Iir_Downto =>
            return V <= Bnd.Left and then V >= Bnd.Right;
      end case;
   end In_Bounds;

   --  Convert index IDX in PFX to an offset.
   --  SYN_INST and LOC are used in case of error.
   function Index_To_Offset
     (Syn_Inst : Synth_Instance_Acc; Bnd : Bound_Type; Idx : Int64; Loc : Node)
     return Value_Offsets
   is
      Res : Value_Offsets;
   begin
      if not In_Bounds (Bnd, Int32 (Idx)) then
         Error_Msg_Synth (+Loc, "index not within bounds");
         Synth.Debugger.Debug_Error (Syn_Inst, Loc);
         return (0, 0);
      end if;

      --  The offset is from the LSB (bit 0).  Bit 0 is the rightmost one.
      case Bnd.Dir is
         when Iir_To =>
            Res.Net_Off := Uns32 (Bnd.Right - Int32 (Idx));
            Res.Mem_Off := Size_Type (Int32 (Idx) - Bnd.Left);
         when Iir_Downto =>
            Res.Net_Off := Uns32 (Int32 (Idx) - Bnd.Right);
            Res.Mem_Off := Size_Type (Bnd.Left - Int32 (Idx));
      end case;

      return Res;
   end Index_To_Offset;

   function Dyn_Index_To_Offset
     (Bnd : Bound_Type; Idx_Val : Valtyp; Loc : Node) return Net
   is
      Idx2 : Net;
      Off : Net;
      Right : Net;
      Wbounds : Width;
   begin
      Wbounds := Clog2 (Bnd.Len);
      Idx2 := Synth_Resize (Idx_Val, Wbounds, Loc);

      if Bnd.Right = 0 and then Bnd.Dir = Iir_Downto then
         --  Simple case without adjustments.
         return Idx2;
      end if;

      Right := Build_Const_UB32 (Build_Context, To_Uns32 (Bnd.Right),
                                 Wbounds);
      Set_Location (Right, Loc);

      case Bnd.Dir is
         when Iir_To =>
            --  L <= I <= R    -->   off = R - I
            Off := Build_Dyadic (Build_Context, Id_Sub, Right, Idx2);
         when Iir_Downto =>
            --  L >= I >= R    -->   off = I - R
            Off := Build_Dyadic (Build_Context, Id_Sub, Idx2, Right);
      end case;
      Set_Location (Off, Loc);
      return Off;
   end Dyn_Index_To_Offset;

   --  Return the bounds of a one dimensional array/vector type and the
   --  width of the element.
   procedure Get_Onedimensional_Array_Bounds
     (Typ : Type_Acc; Bnd : out Bound_Type; El_Typ : out Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Vector =>
            El_Typ := Typ.Vec_El;
            Bnd := Typ.Vbound;
         when Type_Array =>
            El_Typ := Typ.Arr_El;
            Bnd := Typ.Abounds.D (1);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Onedimensional_Array_Bounds;

   function Create_Onedimensional_Array_Subtype
     (Btyp : Type_Acc; Bnd : Bound_Type) return Type_Acc
   is
      Res : Type_Acc;
      Bnds : Bound_Array_Acc;
   begin
      case Btyp.Kind is
         when Type_Vector =>
            Res := Create_Vector_Type (Bnd, Btyp.Vec_El);
         when Type_Unbounded_Vector =>
            Res := Create_Vector_Type (Bnd, Btyp.Uvec_El);
         when Type_Array =>
            pragma Assert (Btyp.Abounds.Ndim = 1);
            Bnds := Create_Bound_Array (1);
            Bnds.D (1) := Bnd;
            Res := Create_Array_Type (Bnds, Btyp.Arr_El);
         when Type_Unbounded_Array =>
            pragma Assert (Btyp.Uarr_Ndim = 1);
            Bnds := Create_Bound_Array (1);
            Bnds.D (1) := Bnd;
            Res := Create_Array_Type (Bnds, Btyp.Uarr_El);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Onedimensional_Array_Subtype;

   procedure Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Type : Type_Acc;
                                 Voff : out Net;
                                 Off : out Value_Offsets)
   is
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      El_Typ : constant Type_Acc := Get_Array_Element (Pfx_Type);
      Idx_Expr : Node;
      Idx_Val : Valtyp;
      Bnd : Bound_Type;
      Stride : Uns32;
      Ivoff : Net;
      Idx_Off : Value_Offsets;
   begin
      Voff := No_Net;
      Off := (0, 0);

      for I in Flist_First .. Flist_Last (Indexes) loop
         Idx_Expr := Get_Nth_Element (Indexes, I);

         --  Compute stride.  This is O(n**2), but for small n.
         Stride := 1;
         for J in I + 1 .. Flist_Last (Indexes) loop
            Bnd := Get_Array_Bound (Pfx_Type, Dim_Type (J + 1));
            Stride := Stride * Bnd.Len;
         end loop;

         --  Use the base type as the subtype of the index is not synth-ed.
         Idx_Val := Synth_Expression_With_Basetype (Syn_Inst, Idx_Expr);
         Strip_Const (Idx_Val);

         Bnd := Get_Array_Bound (Pfx_Type, Dim_Type (I + 1));

         if Is_Static (Idx_Val.Val) then
            Idx_Off := Index_To_Offset (Syn_Inst, Bnd,
                                        Read_Discrete (Idx_Val), Name);
            Off.Net_Off := Off.Net_Off + Idx_Off.Net_Off * Stride * El_Typ.W;
            Off.Mem_Off := Off.Mem_Off
              + Idx_Off.Mem_Off * Size_Type (Stride) * El_Typ.Sz;
         else
            Ivoff := Dyn_Index_To_Offset (Bnd, Idx_Val, Name);
            Ivoff := Build_Memidx (Get_Build (Syn_Inst), Ivoff, El_Typ.W,
                                   Bnd.Len - 1,
                                   Width (Clog2 (Uns64 (Stride * Bnd.Len))));
            Set_Location (Ivoff, Idx_Expr);

            if Voff = No_Net then
               Voff := Ivoff;
            else
               Voff := Build_Addidx (Get_Build (Syn_Inst), Ivoff, Voff);
               Set_Location (Voff, Idx_Expr);
            end if;
         end if;
      end loop;
   end Synth_Indexed_Name;

   function Is_Static (N : Net) return Boolean is
   begin
      case Get_Id (Get_Module (Get_Net_Parent (N))) is
         when Id_Const_UB32 =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Static;

   function Get_Const (N : Net) return Int32
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      case Get_Id (Get_Module (Inst)) is
         when Id_Const_UB32 =>
            return To_Int32 (Get_Param_Uns32 (Inst, 0));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Const;

   --  Decompose VAL as FACTOR * INP + ADDEND (where only INP is non-static).
   procedure Decompose_Mul_Add (Val : Net;
                                Inp : out Net;
                                Factor : out Int32;
                                Addend : out Int32)
   is
      Inst : Instance;
      Val_I0, Val_I1 : Net;
   begin
      Factor := 1;
      Addend := 0;
      Inp := Val;

      loop
         Inst := Get_Net_Parent (Inp);
         case Get_Id (Get_Module (Inst)) is
            when Id_Add =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I0) then
                  Addend := Addend + Get_Const (Val_I0) * Factor;
                  Inp := Val_I1;
               elsif Is_Static (Val_I1) then
                  Addend := Addend + Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               else
                  --  It's an addition, but without any constant value.
                  return;
               end if;
            when Id_Sub =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I1) then
                  Addend := Addend - Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               elsif Is_Static (Val_I0) then
                  Addend := Addend + Get_Const (Val_I0) * Factor;
                  Factor := -Factor;
                  Inp := Val_I1;
               else
                  --  It's a substraction, but without any constant value.
                  return;
               end if;
            when Id_Smul =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I0) then
                  Factor := Factor * Get_Const (Val_I0);
                  Inp := Val_I1;
               elsif Is_Static (Val_I1) then
                  Factor := Factor * Get_Const (Val_I1);
                  Inp := Val_I0;
               else
                  --  A mul but without any constant value.
                  return;
               end if;
            when Id_Utrunc
              | Id_Uextend =>
               Inp := Get_Input_Net (Inst, 0);
            when others =>
               --  Cannot decompose it.
               return;
         end case;
      end loop;
   end Decompose_Mul_Add;

   --  Identify LEFT to/downto RIGHT as:
   --  INP * STEP + WIDTH - 1 + OFF to/downto INP * STEP + OFF
   procedure Synth_Extract_Dyn_Suffix (Ctxt : Context_Acc;
                                       Loc : Node;
                                       Pfx_Bnd : Bound_Type;
                                       Left : Net;
                                       Right : Net;
                                       Inp : out Net;
                                       Step : out Uns32;
                                       Off : out Uns32;
                                       Width : out Uns32)
   is
      L_Inp, R_Inp : Net;
      L_Fac, R_Fac : Int32;
      L_Add, R_Add : Int32;
   begin
      Inp := No_Net;
      Step := 0;
      Off := 0;
      Width := 0;

      if Left = Right then
         L_Inp := Left;
         R_Inp := Right;
         L_Fac := 1;
         R_Fac := 1;
         L_Add := 0;
         R_Add := 0;
      else
         Decompose_Mul_Add (Left, L_Inp, L_Fac, L_Add);
         Decompose_Mul_Add (Right, R_Inp, R_Fac, R_Add);
      end if;

      if not Same_Net (L_Inp, R_Inp) then
         Error_Msg_Synth
           (+Loc, "cannot extract same variable part for dynamic slice");
         return;
      end if;
      Inp := L_Inp;

      if L_Fac /= R_Fac then
         Error_Msg_Synth
           (+Loc, "cannot extract same constant factor for dynamic slice");
         return;
      end if;
      if L_Fac < 0 then
         Step := Uns32 (-L_Fac);
         Inp := Build_Monadic (Ctxt, Id_Neg, Inp);
         Set_Location (Inp, Loc);
      else
         Step := Uns32 (L_Fac);
      end if;

      case Pfx_Bnd.Dir is
         when Iir_To =>
            Off := Uns32 (L_Add - Pfx_Bnd.Left);
            Width := Uns32 (R_Add - L_Add + 1);
         when Iir_Downto =>
            Off := Uns32 (R_Add - Pfx_Bnd.Right);
            Width := Uns32 (L_Add - R_Add + 1);
      end case;
   end Synth_Extract_Dyn_Suffix;

   procedure Synth_Slice_Const_Suffix (Syn_Inst: Synth_Instance_Acc;
                                       Expr : Node;
                                       Name : Node;
                                       Pfx_Bnd : Bound_Type;
                                       L, R : Int64;
                                       Dir : Iir_Direction;
                                       El_Typ : Type_Acc;
                                       Res_Bnd : out Bound_Type;
                                       Off : out Value_Offsets)
   is
      Is_Null : Boolean;
      Len : Uns32;
   begin
      if Pfx_Bnd.Dir /= Dir then
         Error_Msg_Synth (+Name, "direction mismatch in slice");
         Off := (0, 0);
         if Dir = Iir_To then
            Res_Bnd := (Dir => Iir_To, Left => 1, Right => 0, Len => 0);
         else
            Res_Bnd := (Dir => Iir_Downto, Left => 0, Right => 1, Len => 0);
         end if;
         return;
      end if;

      --  Might be a null slice.
      case Pfx_Bnd.Dir is
         when Iir_To =>
            Is_Null := L > R;
         when Iir_Downto =>
            Is_Null := L < R;
      end case;
      if Is_Null then
         Len := 0;
         Off := (0, 0);
      else
         if not In_Bounds (Pfx_Bnd, Int32 (L))
           or else not In_Bounds (Pfx_Bnd, Int32 (R))
         then
            Error_Msg_Synth (+Name, "index not within bounds");
            Synth.Debugger.Debug_Error (Syn_Inst, Expr);
            Off := (0, 0);
            return;
         end if;

         case Pfx_Bnd.Dir is
            when Iir_To =>
               Len := Uns32 (R - L + 1);
               Off.Net_Off := Uns32 (Pfx_Bnd.Right - Int32 (R)) * El_Typ.W;
               Off.Mem_Off := Size_Type (Int32 (L) - Pfx_Bnd.Left) * El_Typ.Sz;
            when Iir_Downto =>
               Len := Uns32 (L - R + 1);
               Off.Net_Off := Uns32 (Int32 (R) - Pfx_Bnd.Right) * El_Typ.W;
               Off.Mem_Off := Size_Type (Pfx_Bnd.Left - Int32 (L)) * El_Typ.Sz;
         end case;
      end if;
      Res_Bnd := (Dir => Pfx_Bnd.Dir,
                  Len => Len,
                  Left => Int32 (L),
                  Right => Int32 (R));
   end Synth_Slice_Const_Suffix;

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Bound_Type;
                                 El_Typ : Type_Acc;
                                 Res_Bnd : out Bound_Type;
                                 Inp : out Net;
                                 Off : out Value_Offsets)
   is
      Expr : constant Node := Get_Suffix (Name);
      Left, Right : Valtyp;
      Dir : Iir_Direction;
      Step : Uns32;
      Max : Uns32;
      Inp_W : Width;
   begin
      Off := (0, 0);

      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            Left := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Left_Limit (Expr));
            Right := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Right_Limit (Expr));
            Dir := Get_Direction (Expr);
         when Iir_Kind_Range_Array_Attribute =>
            declare
               Rng : Discrete_Range_Type;
            begin
               Synth_Discrete_Range (Syn_Inst, Expr, Rng);
               Inp := No_Net;
               Synth_Slice_Const_Suffix (Syn_Inst, Expr,
                                         Name, Pfx_Bnd,
                                         Rng.Left, Rng.Right, Rng.Dir,
                                         El_Typ, Res_Bnd, Off);
               return;
            end;
         when others =>
            Error_Msg_Synth
              (+Expr, "only range expression supported for slices");
      end case;

      if Is_Static_Val (Left.Val) and then Is_Static_Val (Right.Val) then
         Inp := No_Net;
         Synth_Slice_Const_Suffix (Syn_Inst, Expr,
                                   Name, Pfx_Bnd,
                                   Get_Static_Discrete (Left),
                                   Get_Static_Discrete (Right),
                                   Dir,
                                   El_Typ, Res_Bnd, Off);
      else
         if Pfx_Bnd.Dir /= Dir then
            Error_Msg_Synth (+Name, "direction mismatch in slice");
            Inp := No_Net;
            Off := (0, 0);
            if Dir = Iir_To then
               Res_Bnd := (Dir => Iir_To, Left => 1, Right => 0, Len => 0);
            else
               Res_Bnd := (Dir => Iir_Downto, Left => 0, Right => 1, Len => 0);
            end if;
            return;
         end if;

         if Is_Static (Left.Val) or else Is_Static (Right.Val) then
            Error_Msg_Synth
              (+Name, "left and right bounds of a slice must be "
                 & "either constant or dynamic");
            return;
         end if;
         Synth_Extract_Dyn_Suffix
           (Get_Build (Syn_Inst), Name,
            Pfx_Bnd, Get_Net (Left), Get_Net (Right), Inp, Step, Off.Net_Off,
            Res_Bnd.Len);
         Inp_W := Get_Width (Inp);
         --  FIXME: convert range to offset.
         --  Extract max from the range.
         --  example: len=128  wd=8  step=8  => max=16
         --           len=8    wd=4  step=1  => max=4
         --  max so that max*step+wd <= len - off
         --              max <= (len - off - wd) / step
         Max := (Pfx_Bnd.Len - Off.Net_Off - Res_Bnd.Len) / Step;
         if Clog2 (Uns64 (Max)) > Natural (Inp_W) then
            --  The width of Inp limits the max.
            Max := 2**Natural (Inp_W) - 1;
         end if;
         Inp := Build_Memidx
           (Get_Build (Syn_Inst),
            Inp, Step * El_Typ.W, Max,
            Inp_W + Width (Clog2 (Uns64 (Step * El_Typ.W))));
      end if;
   end Synth_Slice_Suffix;

   --  Match: clk_signal_name'event
   --  and return clk_signal_name.
   function Extract_Event_Expr_Prefix (Expr : Node) return Node is
   begin
      if Get_Kind (Expr) = Iir_Kind_Event_Attribute then
         return Get_Prefix (Expr);
      else
         return Null_Node;
      end if;
   end Extract_Event_Expr_Prefix;

   function Is_Same_Node (Left, Right : Node) return Boolean is
   begin
      if Get_Kind (Left) /= Get_Kind (Right) then
         return False;
      end if;
      case Get_Kind (Left) is
         when Iir_Kind_Simple_Name =>
            return Get_Named_Entity (Left) = Get_Named_Entity (Right);
         when others =>
            Error_Kind ("is_same_node", Left);
      end case;
   end Is_Same_Node;

   --  Match: clk_signal_name = '1' | clk_signal_name = '0'
   function Extract_Clock_Level
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Prefix : Node) return Net
   is
      Clk : Net;
      Imp : Node;
      Left, Right : Node;
      Lit : Node;
      Posedge : Boolean;
   begin
      Clk := Get_Net (Synth_Name (Syn_Inst, Prefix));
      if Get_Kind (Expr) /= Iir_Kind_Equality_Operator then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Imp := Get_Implementation (Expr);
      if Get_Implicit_Definition (Imp) /= Iir_Predefined_Enum_Equality then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Left := Get_Left (Expr);
      Right := Get_Right (Expr);
      if Get_Kind (Right) /= Iir_Kind_Character_Literal then
         Error_Msg_Synth
           (+Expr, "ill-formed clock-level, '0' or '1' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Lit := Get_Named_Entity (Right);
      if Lit = Vhdl.Std_Package.Bit_0
        or else Lit = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_0
      then
         Posedge := False;
      elsif Lit = Vhdl.Std_Package.Bit_1
        or else Lit = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_1
      then
         Posedge := True;
      else
         Error_Msg_Synth
           (+Lit, "ill-formed clock-level, '0' or '1' expected");
         Posedge := True;
      end if;
      if not Is_Same_Node (Prefix, Left) then
         Error_Msg_Synth
           (+Left, "clock signal name doesn't match");
      end if;
      if not Posedge then
         Clk := Build_Monadic (Build_Context, Id_Not, Clk);
      end if;
      return Build_Edge (Build_Context, Clk);
   end Extract_Clock_Level;

   --  Try to match: clk'event and clk = X
   --            or: clk = X and clk'event
   --  where X is '0' or '1'.
   function Synth_Clock_Edge
     (Syn_Inst : Synth_Instance_Acc; Left, Right : Node) return Net
   is
      Prefix : Node;
   begin
      --  Try with left.
      Prefix := Extract_Event_Expr_Prefix (Left);
      if Is_Valid (Prefix) then
         return Extract_Clock_Level (Syn_Inst, Right, Prefix);
      end if;

      --  Try with right.
      Prefix := Extract_Event_Expr_Prefix (Right);
      if Is_Valid (Prefix) then
         return Extract_Clock_Level (Syn_Inst, Left, Prefix);
      end if;

      return No_Net;
   end Synth_Clock_Edge;

   function Synth_Type_Conversion (Syn_Inst : Synth_Instance_Acc; Conv : Node)
                                  return Valtyp
   is
      Expr : constant Node := Get_Expression (Conv);
      Conv_Type : constant Node := Get_Type (Conv);
      Conv_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Conv_Type);
      Val : Valtyp;
   begin
      Val := Synth_Expression_With_Basetype (Syn_Inst, Expr);
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
               Error_Msg_Synth (+Conv, "unhandled type conversion (to int)");
               return No_Valtyp;
            end if;
         when Iir_Kind_Floating_Subtype_Definition =>
            if Is_Static (Val.Val) then
               return Create_Value_Float
                 (Fp64 (Read_Discrete (Val)), Conv_Typ);
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to float)");
               return No_Valtyp;
            end if;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            case Conv_Typ.Kind is
               when Type_Vector
                 | Type_Unbounded_Vector =>
                  return Val;
               when others =>
                  Error_Msg_Synth
                    (+Conv, "unhandled type conversion (to array)");
                  return No_Valtyp;
            end case;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            pragma Assert (Get_Base_Type (Get_Type (Expr))
                             = Get_Base_Type (Conv_Type));
            return Val;
         when others =>
            Error_Msg_Synth (+Conv, "unhandled type conversion");
            return No_Valtyp;
      end case;
   end Synth_Type_Conversion;

   procedure Error_Ieee_Operator (Imp : Node; Loc : Node) is
   begin
      if Get_Kind (Get_Parent (Imp)) = Iir_Kind_Package_Declaration
        and then (Get_Identifier
                    (Get_Library
                       (Get_Design_File (Get_Design_Unit (Get_Parent (Imp)))))
                    = Std_Names.Name_Ieee)
      then
         Error_Msg_Synth (+Loc, "unhandled predefined IEEE operator %i", +Imp);
         Error_Msg_Synth (+Imp, " declared here");
      end if;
   end Error_Ieee_Operator;

   function Synth_String_Literal
     (Syn_Inst : Synth_Instance_Acc; Str : Node; Str_Typ : Type_Acc)
     return Valtyp
   is
      pragma Assert (Get_Kind (Str) = Iir_Kind_String_Literal8);
      Id : constant String8_Id := Get_String8_Id (Str);

      Str_Type : constant Node := Get_Type (Str);
      El_Type : Type_Acc;
      Bounds : Bound_Type;
      Bnds : Bound_Array_Acc;
      Res_Type : Type_Acc;
      Res : Valtyp;
      Pos : Nat8;
   begin
      case Str_Typ.Kind is
         when Type_Vector =>
            Bounds := Str_Typ.Vbound;
         when Type_Array =>
            Bounds := Str_Typ.Abounds.D (1);
         when Type_Unbounded_Vector
           | Type_Unbounded_Array =>
            Bounds := Synth_Array_Bounds (Syn_Inst, Str_Type, 1);
         when others =>
            raise Internal_Error;
      end case;

      El_Type := Get_Subtype_Object (Syn_Inst, Get_Element_Subtype (Str_Type));
      if El_Type.Kind in Type_Nets then
         Res_Type := Create_Vector_Type (Bounds, El_Type);
      else
         Bnds := Create_Bound_Array (1);
         Bnds.D (1) := Bounds;
         Res_Type := Create_Array_Type (Bnds, El_Type);
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
   end Synth_String_Literal;

   --  Return the left bound if the direction of the range is LEFT_DIR.
   function Synth_Low_High_Type_Attribute
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Left_Dir : Iir_Direction)
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

   subtype And_Or_Module_Id is Module_Id range Id_And .. Id_Or;

   function Synth_Short_Circuit (Syn_Inst : Synth_Instance_Acc;
                                 Id : And_Or_Module_Id;
                                 Left_Expr : Node;
                                 Right_Expr : Node;
                                 Typ : Type_Acc;
                                 Expr : Node) return Valtyp
   is
      Left : Valtyp;
      Right : Valtyp;
      Val : Int64;
      N : Net;
   begin
      --  The short-circuit value.
      case Id is
         when Id_And =>
            Val := 0;
         when Id_Or =>
            Val := 1;
      end case;

      Left := Synth_Expression_With_Type (Syn_Inst, Left_Expr, Typ);
      if Left = No_Valtyp then
         return No_Valtyp;
      end if;
      if Is_Static_Val (Left.Val)
        and then Get_Static_Discrete (Left) = Val
      then
         return Create_Value_Discrete (Val, Boolean_Type);
      end if;

      Strip_Const (Left);
      Right := Synth_Expression_With_Type (Syn_Inst, Right_Expr, Typ);
      if Right = No_Valtyp then
         return No_Valtyp;
      end if;
      Strip_Const (Right);

      --  Return a static value if both operands are static.
      --  Note: we know the value of left if it is not constant.
      if Is_Static_Val (Left.Val) and then Is_Static_Val (Right.Val) then
         Val := Get_Static_Discrete (Right);
         return Create_Value_Discrete (Val, Boolean_Type);
      end if;

      N := Build_Dyadic (Build_Context, Id, Get_Net (Left), Get_Net (Right));
      Set_Location (N, Expr);
      return Create_Value_Net (N, Boolean_Type);
   end Synth_Short_Circuit;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Type_Acc)
     return Valtyp is
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Edge : Net;
            begin
               --  Match clock-edge
               if Def = Iir_Predefined_Boolean_And then
                  Edge := Synth_Clock_Edge (Syn_Inst,
                                            Get_Left (Expr), Get_Right (Expr));
                  if Edge /= No_Net then
                     return Create_Value_Net (Edge, Boolean_Type);
                  end if;
               end if;

               --  Specially handle short-circuit operators.
               case Def is
                  when Iir_Predefined_Boolean_And =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_And, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type, Expr);
                  when Iir_Predefined_Boolean_Or =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_Or, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type, Expr);
                  when Iir_Predefined_Bit_And =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_And, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type, Expr);
                  when Iir_Predefined_Bit_Or =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_Or, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type, Expr);
                  when Iir_Predefined_None =>
                     Error_Ieee_Operator (Imp, Expr);
                     return Synth_User_Operator
                       (Syn_Inst, Get_Left (Expr), Get_Right (Expr), Expr);
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
                  Error_Ieee_Operator (Imp, Expr);
                  return Synth_User_Operator
                    (Syn_Inst, Get_Operand (Expr), Null_Node, Expr);
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
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Reference_Name =>
            --  Only used for anonymous signals in internal association.
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Anonymous_Signal_Declaration =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr), Expr_Type);
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            declare
               Base : Valtyp;
               Typ : Type_Acc;
               Off : Value_Offsets;
               Res : Valtyp;

               Voff : Net;
               Rdwd : Width;
            begin
               Synth_Assignment_Prefix
                 (Syn_Inst, Expr, Base, Typ, Off, Voff, Rdwd);
               if Voff = No_Net and then Is_Static (Base.Val) then
                  Res := Create_Value_Memory (Typ);
                  Copy_Memory
                    (Res.Val.Mem, Base.Val.Mem + Off.Mem_Off, Typ.Sz);
                  return Res;
               end if;
               return Synth_Read_Memory
                 (Syn_Inst, Base, Typ, Off.Net_Off, Voff, Expr);
            end;
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Expr));
               Pfx : constant Node := Get_Prefix (Expr);
               Res_Typ : Type_Acc;
               N : Net;
               Val : Valtyp;
               Res : Valtyp;
            begin
               Val := Synth_Expression (Syn_Inst, Pfx);
               Strip_Const (Val);
               Res_Typ := Val.Typ.Rec.E (Idx + 1).Typ;
               if Is_Static (Val.Val) then
                  Res := Create_Value_Memory (Res_Typ);
                  Copy_Memory
                    (Res.Val.Mem, Val.Val.Mem + Val.Typ.Rec.E (Idx + 1).Moff,
                     Res_Typ.Sz);
                  return Res;
               else
                  N := Build_Extract
                    (Build_Context, Get_Net (Val),
                     Val.Typ.Rec.E (Idx + 1).Boff, Get_Type_Width (Res_Typ));
                  Set_Location (N, Expr);
                  return Create_Value_Net (N, Res_Typ);
               end if;
            end;
         when Iir_Kind_Character_Literal =>
            return Synth_Expression_With_Type
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
            return Synth_String_Literal (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Enumeration_Literal =>
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Type_Conversion =>
            return Synth_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr),
               Get_Subtype_Object (Syn_Inst, Get_Type (Get_Type_Mark (Expr))));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
            begin
               case Get_Implicit_Definition (Imp) is
                  when Iir_Predefined_Pure_Functions =>
                     return Synth_Operator_Function_Call (Syn_Inst, Expr);
                  when Iir_Predefined_None =>
                     return Synth_User_Function_Call (Syn_Inst, Expr);
                  when others =>
                     return Synth_Predefined_Function_Call (Syn_Inst, Expr);
               end case;
            end;
         when Iir_Kind_Aggregate =>
            return Synth.Aggr.Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Simple_Aggregate =>
            return Synth_Simple_Aggregate (Syn_Inst, Expr);
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
                  when Iir_To =>
                     V := B.Right;
                  when Iir_Downto =>
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
                  when Iir_To =>
                     V := B.Left;
                  when Iir_Downto =>
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
         when Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute =>
            declare
               Param : constant Node := Get_Parameter (Expr);
               V : Valtyp;
               Dtype : Type_Acc;
            begin
               V := Synth_Expression (Syn_Inst, Param);
               Dtype := Get_Subtype_Object (Syn_Inst, Get_Type (Expr));
               --  FIXME: to be generalized.  Not always as simple as a
               --  subtype conversion.
               return Synth_Subtype_Conversion (V, Dtype, False, Expr);
            end;
         when Iir_Kind_Low_Type_Attribute =>
            return Synth_Low_High_Type_Attribute (Syn_Inst, Expr, Iir_To);
         when Iir_Kind_High_Type_Attribute =>
            return Synth_Low_High_Type_Attribute (Syn_Inst, Expr, Iir_Downto);
         when Iir_Kind_Value_Attribute =>
            return Synth_Value_Attribute (Syn_Inst, Expr);
         when Iir_Kind_Image_Attribute =>
            return Synth_Image_Attribute (Syn_Inst, Expr);
         when Iir_Kind_Null_Literal =>
            return Create_Value_Access (Null_Heap_Index, Expr_Type);
         when Iir_Kind_Allocator_By_Subtype =>
            declare
               T : Type_Acc;
               Acc : Heap_Index;
            begin
               T := Synth.Decls.Synth_Subtype_Indication
                 (Syn_Inst, Get_Subtype_Indication (Expr));
               Acc := Allocate_By_Type (T);
               return Create_Value_Access (Acc, Expr_Type);
            end;
         when Iir_Kind_Allocator_By_Expression =>
            declare
               V : Valtyp;
               Acc : Heap_Index;
            begin
               V := Synth_Expression_With_Type
                 (Syn_Inst, Get_Expression (Expr), Expr_Type.Acc_Acc);
               Acc := Allocate_By_Value (V);
               return Create_Value_Access (Acc, Expr_Type);
            end;
         when Iir_Kind_Overflow_Literal =>
            declare
               N : Net;
            begin
               Error_Msg_Synth
                 (+Expr, "error detected during analysis injected");
               N := Build_Const_X (Get_Build (Syn_Inst), Expr_Type.W);
               return Create_Value_Net (N, Expr_Type);
            end;
         when others =>
            Error_Kind ("synth_expression_with_type", Expr);
      end case;
   end Synth_Expression_With_Type;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Valtyp is
   begin
      return Synth_Expression_With_Type
        (Syn_Inst, Expr, Get_Subtype_Object (Syn_Inst, Get_Type (Expr)));
   end Synth_Expression;

   function Synth_Expression_With_Basetype
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp
   is
      Basetype : Type_Acc;
   begin
      Basetype := Get_Subtype_Object
        (Syn_Inst, Get_Base_Type (Get_Type (Expr)));
      return Synth_Expression_With_Type (Syn_Inst, Expr, Basetype);
   end Synth_Expression_With_Basetype;
end Synth.Expr;
