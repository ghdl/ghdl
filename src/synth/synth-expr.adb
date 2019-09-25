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

with Ada.Unchecked_Conversion;
with Types_Utils; use Types_Utils;
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
with Netlists.Locations; use Netlists.Locations;

with Synth.Types; use Synth.Types;
with Synth.Errors; use Synth.Errors;
with Synth.Environment;
with Synth.Decls;
with Synth.Stmts; use Synth.Stmts;
with Synth.Oper; use Synth.Oper;

package body Synth.Expr is
   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Value_Acc;

   procedure Set_Location2 (N : Net; Loc : Node) is
   begin
      Set_Location (Get_Net_Parent (N), Get_Location (Loc));
   end Set_Location2;

   procedure Set_Location (N : Net; Loc : Node) is
   begin
      --  Short and compact code as it is inlined.
      if Flag_Locations then
         Set_Location2 (N, Loc);
      end if;
   end Set_Location;

   function Get_Const_Discrete (V : Value_Acc) return Int64
   is
      N : Net;
      Inst : Instance;
   begin
      case V.Kind is
         when Value_Discrete =>
            return V.Scal;
         when Value_Net =>
            N := V.N;
         when Value_Wire =>
            N := Synth.Environment.Get_Const_Wire (V.W);
         when others =>
            raise Internal_Error;
      end case;
      Inst := Get_Net_Parent (N);
      case Get_Id (Inst) is
         when Id_Const_UB32 =>
            declare
               Va : constant Uns32 := Get_Param_Uns32 (Inst, 0);
               Wd : constant Natural := Natural (Get_Width (N));
               T : Uns64;
            begin
               T := Shift_Left (Uns64 (Va), 64 - Wd);
               return To_Int64 (Shift_Right_Arithmetic (T, 64 - Wd));
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Const_Discrete;

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
         From_Std_Logic (Enum, Val, Zx);
      elsif Etype = Boolean_Type or Etype = Bit_Type then
         From_Bit (Enum, Val);
         Zx := 0;
      else
         raise Internal_Error;
      end if;
   end To_Logic;

   function Bit_Extract (Val : Value_Acc; Off : Uns32; Loc : Node)
                        return Value_Acc
   is
      N : Net;
   begin
      case Val.Kind is
         when Value_Array
           | Value_Const_Array =>
            pragma Assert (Val.Typ.Vbound.Len >= Off);
            return Val.Arr.V (Iir_Index32 (Val.Typ.Vbound.Len - Off));
         when Value_Net
           | Value_Wire =>
            N := Build_Extract_Bit (Build_Context, Get_Net (Val), Off);
            Set_Location (N, Loc);
            return Create_Value_Net (N, Val.Typ.Vec_El);
         when others =>
            raise Internal_Error;
      end case;
   end Bit_Extract;

   --  Resize for a discrete value.
   function Synth_Resize (Val : Value_Acc; W : Width; Loc : Node) return Net
   is
      Wn : constant Width := Val.Typ.W;
      N : Net;
      Res : Net;
   begin
      if Is_Const (Val) then
         if Wn /= W then
            pragma Assert (Val.Kind = Value_Discrete);
            if Val.Typ.Drange.Is_Signed then
               Res := Build2_Const_Int (Build_Context, Val.Scal, W);
            else
               Res := Build2_Const_Uns (Build_Context, To_Uns64 (Val.Scal), W);
            end if;
            Set_Location (Res, Loc);
            return Res;
         end if;
      end if;

      N := Get_Net (Val);
      if Wn > W then
         Res := Build_Trunc (Build_Context, Id_Utrunc, N, W);
         Set_Location (Res, Loc);
         return Res;
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

   function Get_Index_Offset
     (Index : Value_Acc; Bounds : Bound_Type; Expr : Iir) return Uns32 is
   begin
      if Index.Kind = Value_Discrete then
         declare
            Left : constant Int64 := Int64 (Bounds.Left);
            Right : constant Int64 := Int64 (Bounds.Right);
         begin
            case Bounds.Dir is
               when Iir_To =>
                  if Index.Scal >= Left and then Index.Scal <= Right then
                     -- to
                     return Uns32 (Index.Scal - Left);
                  end if;
               when Iir_Downto =>
                  if Index.Scal <= Left and then Index.Scal >= Right then
                     -- downto
                     return Uns32 (Left - Index.Scal);
                  end if;
            end case;
         end;
      else
         raise Internal_Error;
      end if;
      Error_Msg_Synth (+Expr, "index out of bounds");
      return 0;
   end Get_Index_Offset;

   function Get_Array_Bound (Typ : Type_Acc; Dim : Natural)
                            return Bound_Type is
   begin
      case Typ.Kind is
         when Type_Vector =>
            pragma Assert (Dim = 0);
            return Typ.Vbound;
         when Type_Array =>
            return Typ.Abounds.D (Iir_Index32 (Dim + 1));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Bound;

   procedure Fill_Array_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Res : Value_Array_Acc;
                                   Typ : Type_Acc;
                                   Dim : Natural;
                                   Const_P : out Boolean)
   is
      Bound : constant Bound_Type := Get_Array_Bound (Typ, Dim);
      Aggr_Type : constant Node := Get_Type (Aggr);
      El_Typ : constant Type_Acc := Get_Array_Element (Typ);
      Nbr_Dims : constant Natural := Get_Nbr_Dimensions (Aggr_Type);
      type Boolean_Array is array (Uns32 range <>) of Boolean;
      pragma Pack (Boolean_Array);
      --  FIXME: test Res.V (I) instead.
      Is_Set : Boolean_Array (0 .. Bound.Len - 1);
      Value : Node;
      Assoc : Node;
      Pos : Uns32;

      procedure Set_Elem (Pos : Uns32)
      is
         Val : Value_Acc;
      begin
         if Dim = Nbr_Dims - 1 then
            Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Typ);
            Res.V (Iir_Index32 (Pos + 1)) := Val;
            pragma Assert (not Is_Set (Pos));
            Is_Set (Pos) := True;
            if Const_P and then not Is_Const (Val) then
               Const_P := False;
            end if;
         else
            Error_Msg_Synth (+Assoc, "multi-dim aggregate not handled");
         end if;
      end Set_Elem;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := 0;
      Is_Set := (others => False);
      Const_P := True;
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  if Pos >= Bound.Len then
                     Error_Msg_Synth (+Assoc, "element out of array bound");
                  else
                     Set_Elem (Pos);
                  end if;
                  Pos := Pos + 1;
               when Iir_Kind_Choice_By_Others =>
                  while Pos < Bound.Len loop
                     if not Is_Set (Pos) then
                        Set_Elem (Pos);
                     end if;
                     Pos := Pos + 1;
                  end loop;
               when Iir_Kind_Choice_By_Expression =>
                  declare
                     Ch : constant Node := Get_Choice_Expression (Assoc);
                     Idx : Value_Acc;
                  begin
                     Idx := Synth_Expression (Syn_Inst, Ch);
                     if not Is_Const (Idx) then
                        Error_Msg_Synth (+Ch, "choice is not static");
                     else
                        Set_Elem (Get_Index_Offset (Idx, Bound, Ch));
                     end if;
                  end;
               when Iir_Kind_Choice_By_Range =>
                  declare
                     Ch : constant Node := Get_Choice_Range (Assoc);
                     Rng : Discrete_Range_Type;
                     Val : Value_Acc;
                  begin
                     Rng := Synth_Discrete_Range_Expression (Syn_Inst, Ch);
                     Val := Create_Value_Discrete
                       (Rng.Left,
                        Get_Value_Type (Syn_Inst, Get_Type (Ch)));
                     while In_Range (Rng, Val.Scal) loop
                        Set_Elem (Get_Index_Offset (Val, Bound, Ch));
                        Update_Index (Rng, Val.Scal);
                     end loop;
                  end;
               when others =>
                  Error_Msg_Synth
                    (+Assoc, "unhandled association form");
            end case;
            Assoc := Get_Chain (Assoc);
            exit when Is_Null (Assoc);
            exit when not Get_Same_Alternative_Flag (Assoc);
         end loop;
      end loop;
   end Fill_Array_Aggregate;

   procedure Fill_Record_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node;
                                    Rec : Value_Array_Acc;
                                    Const_P : out Boolean)
   is
      El_List : constant Node_Flist :=
        Get_Elements_Declaration_List (Get_Type (Aggr));
      Value : Node;
      Assoc : Node;
      Pos : Natural;

      procedure Set_Elem (Pos : Natural)
      is
         Val : Value_Acc;
         El_Type : Type_Acc;
      begin
         El_Type := Get_Value_Type
           (Syn_Inst, Get_Type (Get_Nth_Element (El_List, Pos)));
         Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Type);
         Rec.V (Iir_Index32 (Pos + 1)) := Val;
         if Const_P and not Is_Const (Val) then
            Const_P := False;
         end if;
      end Set_Elem;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := 0;
      Const_P := True;
      Rec.V := (others => null);
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  Set_Elem (Pos);
                  Pos := Pos + 1;
               when Iir_Kind_Choice_By_Others =>
                  for I in Rec.V'Range loop
                     if Rec.V (I) = null then
                        Set_Elem (Natural (I - 1));
                     end if;
                  end loop;
               when Iir_Kind_Choice_By_Name =>
                  Pos := Natural (Get_Element_Position
                                    (Get_Named_Entity
                                       (Get_Choice_Name (Assoc))));
                  Set_Elem (Pos);
               when others =>
                  Error_Msg_Synth
                    (+Assoc, "unhandled association form");
            end case;
            Assoc := Get_Chain (Assoc);
            exit when Is_Null (Assoc);
            exit when not Get_Same_Alternative_Flag (Assoc);
         end loop;
      end loop;
   end Fill_Record_Aggregate;

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

   function Concat_Array (Arr : Net_Array_Acc) return Net is
   begin
      Concat_Array (Arr.all);
      return Arr (Arr'First);
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
      L, R : Value_Acc;
   begin
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));

      if not (Is_Const (L) and Is_Const (R)) then
         Error_Msg_Synth (+Rng, "limits of range are not constant");
         raise Internal_Error;
      end if;

      return (Dir => Get_Direction (Rng),
              Left => L.Scal,
              Right => R.Scal,
              Is_Signed => L.Scal < 0 or R.Scal < 0);
   end Synth_Discrete_Range_Expression;

   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type
   is
      L, R : Value_Acc;
   begin
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));
      return ((Get_Direction (Rng), L.Fp, R.Fp));
   end Synth_Float_Range_Expression;

   function Synth_Array_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Bound_Type
   is
      Prefix : constant Iir := Strip_Denoting_Name (Get_Prefix (Attr));
      Dim : constant Natural :=
        Vhdl.Evaluation.Eval_Attribute_Parameter_Or_1 (Attr);
      Res : Value_Acc;
   begin
      --  Prefix is an array object or an array subtype.
      Res := Synth_Name (Syn_Inst, Prefix);
      if Res.Typ.Kind = Type_Vector then
         if Dim /= 1 then
            raise Internal_Error;
         end if;
         return Res.Typ.Vbound;
      else
         return Res.Typ.Abounds.D (Iir_Index32 (Dim));
      end if;
   end Synth_Array_Attribute;

   procedure Synth_Discrete_Range (Syn_Inst : Synth_Instance_Acc;
                                   Bound : Node;
                                   Rng : out Discrete_Range_Type;
                                   W : out Width) is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            Rng := Synth_Discrete_Range_Expression (Syn_Inst, Bound);
            W := Discrete_Range_Width (Rng);
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            if Get_Type_Declarator (Bound) /= Null_Node then
               declare
                  Typ : Type_Acc;
               begin
                  --  This is a named subtype, so it has been evaluated.
                  Typ := Get_Value_Type (Syn_Inst, Bound);
                  Rng := Typ.Drange;
                  W := Typ.W;
               end;
            else
               Synth_Discrete_Range
                 (Syn_Inst, Get_Range_Constraint (Bound), Rng, W);
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
               W := B.Wbounds;
            end;
         when others =>
            Error_Kind ("synth_discrete_range", Bound);
      end case;
   end Synth_Discrete_Range;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Natural) return Bound_Type
   is
      Info : constant Sim_Info_Acc := Get_Info (Atype);
   begin
      if Info = null then
         pragma Assert (Get_Type_Declarator (Atype) = Null_Node);
         declare
            Index_Type : constant Node := Get_Index_Type (Atype, Dim);
         begin
            return Synth_Bounds_From_Range (Syn_Inst, Index_Type);
         end;
      else
         declare
            Bnds : constant Value_Acc := Get_Value (Syn_Inst, Atype);
         begin
            case Bnds.Typ.Kind is
               when Type_Vector =>
                  pragma Assert (Dim = 0);
                  return Bnds.Typ.Vbound;
               when Type_Array =>
                  return Bnds.Typ.Abounds.D (Iir_Index32 (Dim + 1));
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
      W : Width;
      Len : Int64;
   begin
      Synth_Discrete_Range (Syn_Inst, Atype, Rng, W);
      case Rng.Dir is
         when Iir_To =>
            Len := Rng.Right - Rng.Left + 1;
         when Iir_Downto =>
            Len := Rng.Left - Rng.Right + 1;
      end case;
      if Len < 0 then
         Len := 0;
      end if;
      return (Dir => Rng.Dir,
              Wlen => Width (Clog2 (Uns64 (Len))),
              Wbounds => W,
              Left => Int32 (Rng.Left), Right => Int32 (Rng.Right),
              Len => Uns32 (Len));
   end Synth_Bounds_From_Range;

   function Synth_Aggregate_Array (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Aggr_Type : Type_Acc) return Value_Acc
   is
      Arr : Value_Array_Acc;
      Res : Value_Acc;
      Const_P : Boolean;
   begin
      Arr := Create_Value_Array
        (Iir_Index32 (Get_Array_Flat_Length (Aggr_Type)));

      Fill_Array_Aggregate (Syn_Inst, Aggr, Arr, Aggr_Type, 0, Const_P);

      if Const_P then
         Res := Create_Value_Const_Array (Aggr_Type, Arr);
      else
         Res := Create_Value_Array (Aggr_Type, Arr);
      end if;

      return Res;
   end Synth_Aggregate_Array;

   function Synth_Aggregate_Record (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node;
                                    Aggr_Type : Type_Acc) return Value_Acc
   is
      Arr : Value_Array_Acc;
      Res : Value_Acc;
      Const_P : Boolean;
   begin
      --  Allocate the result.
      Arr := Create_Value_Array (Aggr_Type.Rec.Len);

      Fill_Record_Aggregate (Syn_Inst, Aggr, Arr, Const_P);

      if Const_P then
         Res := Create_Value_Const_Record (Aggr_Type, Arr);
      else
         Res := Create_Value_Record (Aggr_Type, Arr);
      end if;

      return Res;
   end Synth_Aggregate_Record;

   --  Aggr_Type is the type from the context.
   function Synth_Aggregate (Syn_Inst : Synth_Instance_Acc;
                             Aggr : Node;
                             Aggr_Type : Type_Acc) return Value_Acc is
   begin
      case Aggr_Type.Kind is
         when Type_Unbounded_Array | Type_Unbounded_Vector =>
            declare
               Res_Type : Type_Acc;
            begin
               Res_Type := Decls.Synth_Array_Subtype_Indication
                 (Syn_Inst, Get_Type (Aggr));
               return Synth_Aggregate_Array (Syn_Inst, Aggr, Res_Type);
            end;
         when Type_Vector | Type_Array =>
            return Synth_Aggregate_Array (Syn_Inst, Aggr, Aggr_Type);
         when Type_Record =>
            return Synth_Aggregate_Record (Syn_Inst, Aggr, Aggr_Type);
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Aggregate;

   function Synth_Simple_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node) return Value_Acc
   is
      Aggr_Type : constant Node := Get_Type (Aggr);
      pragma Assert (Get_Nbr_Dimensions (Aggr_Type) = 1);
      El_Type : constant Node := Get_Element_Subtype (Aggr_Type);
      El_Typ : constant Type_Acc := Get_Value_Type (Syn_Inst, El_Type);
      Els : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
      Last : constant Natural := Flist_Last (Els);
      Bnd : Bound_Type;
      Bnds : Bound_Array_Acc;
      Res_Type : Type_Acc;
      Arr : Value_Array_Acc;
      Val : Value_Acc;
   begin
      --  Allocate the result.
      Bnd := Synth_Array_Bounds (Syn_Inst, Aggr_Type, 0);
      pragma Assert (Bnd.Len = Uns32 (Last + 1));

      if El_Typ.Kind = Type_Bit then
         Res_Type := Create_Vector_Type (Bnd, El_Typ);
      else
         Bnds := Create_Bound_Array (1);
         Bnds.D (1) := Bnd;
         Res_Type := Create_Array_Type (Bnds, El_Typ);
      end if;

      Arr := Create_Value_Array (Iir_Index32 (Last + 1));

      for I in Flist_First .. Last loop
         Val := Synth_Expression_With_Type
           (Syn_Inst, Get_Nth_Element (Els, I), El_Typ);
         pragma Assert (Is_Const (Val));
         Arr.V (Iir_Index32 (Last - I + 1)) := Val;
      end loop;

      return Create_Value_Const_Array (Res_Type, Arr);
   end Synth_Simple_Aggregate;

   --  Change the bounds of VAL.
   function Reshape_Value (Val : Value_Acc; Ntype : Type_Acc)
                          return Value_Acc is
   begin
      case Val.Kind is
         when Value_Array =>
            return Create_Value_Array (Ntype, Val.Arr);
         when Value_Const_Array =>
            return Create_Value_Const_Array (Ntype, Val.Arr);
         when Value_Wire =>
            return Create_Value_Wire (Val.W, Ntype);
         when Value_Net =>
            return Create_Value_Net (Val.N, Ntype);
         when others =>
            raise Internal_Error;
      end case;
   end Reshape_Value;

   function Synth_Subtype_Conversion (Val : Value_Acc;
                                      Dtype : Type_Acc;
                                      Bounds : Boolean;
                                      Loc : Source.Syn_Src)
                                     return Value_Acc
   is
      Vtype : constant Type_Acc := Val.Typ;
   begin
      case Dtype.Kind is
         when Type_Bit =>
            pragma Assert (Vtype.Kind = Type_Bit);
            return Val;
         when Type_Discrete =>
            pragma Assert (Vtype.Kind = Type_Discrete);
            declare
               N : Net;
            begin
               if Vtype.W > Dtype.W then
                  --  Truncate.
                  --  TODO: check overflow.
                  case Val.Kind is
                     when Value_Net
                       | Value_Wire =>
                        N := Get_Net (Val);
                        N := Build_Trunc
                          (Build_Context, Id_Utrunc, N, Dtype.W);
                        Set_Location (N, Loc);
                        return Create_Value_Net (N, Dtype);
                     when Value_Discrete =>
                        return Create_Value_Discrete (Val.Scal, Dtype);
                     when others =>
                        raise Internal_Error;
                  end case;
               elsif Vtype.W < Dtype.W then
                  --  Extend.
                  case Val.Kind is
                     when Value_Discrete =>
                        return Create_Value_Discrete (Val.Scal, Dtype);
                     when Value_Net
                       | Value_Wire =>
                        N := Get_Net (Val);
                        if Vtype.Drange.Is_Signed then
                           N := Build_Extend
                             (Build_Context, Id_Sextend, N, Dtype.W);
                        else
                           N := Build_Extend
                             (Build_Context, Id_Uextend, N, Dtype.W);
                        end if;
                        Set_Location (N, Loc);
                        return Create_Value_Net (N, Dtype);
                     when others =>
                        raise Internal_Error;
                  end case;
               else
                  --  TODO: check overflow if sign differ.
                  return Val;
               end if;
            end;
         when Type_Float =>
            pragma Assert (Vtype.Kind = Type_Float);
            --  TODO: check range
            return Val;
         when Type_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or Vtype.Kind = Type_Slice);
            if False and then Dtype.W /= Vtype.W then
               --  TODO: bad width.
               raise Internal_Error;
            end if;
            if Bounds then
               return Reshape_Value (Val, Dtype);
            else
               return Val;
            end if;
         when Type_Slice =>
            --  TODO: check width
            return Val;
         when Type_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            --  TODO: check bounds, handle elements
            return Val;
         when Type_Unbounded_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            return Val;
         when Type_Unbounded_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector);
            return Val;
         when Type_Record =>
            --  TODO: handle elements.
            return Val;
      end case;
   end Synth_Subtype_Conversion;

   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Value_Acc is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Anonymous_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            return Get_Value (Syn_Inst, Name);
         when Iir_Kind_Enumeration_Literal =>
            return Create_Value_Discrete
              (Int64 (Get_Enum_Pos (Name)),
               Get_Value_Type (Syn_Inst, Get_Type (Name)));
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

   function Index_To_Offset (Bnd : Bound_Type; Idx : Int64; Loc : Node)
                            return Uns32 is
   begin
      if not In_Bounds (Bnd, Int32 (Idx)) then
         Error_Msg_Synth (+Loc, "index not within bounds");
         return 0;
      end if;

      --  The offset is from the LSB (bit 0).  Bit 0 is the rightmost one.
      case Bnd.Dir is
         when Iir_To =>
            return Uns32 (Bnd.Right - Int32 (Idx));
         when Iir_Downto =>
            return Uns32 (Int32 (Idx) - Bnd.Right);
      end case;
   end Index_To_Offset;

   function Dyn_Index_To_Offset
     (Bnd : Bound_Type; Idx_Val : Value_Acc; Loc : Node) return Net
   is
      Idx2 : Net;
      Off : Net;
      Right : Net;
   begin
      Idx2 := Synth_Resize (Idx_Val, Bnd.Wbounds, Loc);

      if Bnd.Right = 0 and then Bnd.Dir = Iir_Downto then
         --  Simple case without adjustments.
         return Idx2;
      end if;

      Right := Build_Const_UB32 (Build_Context, To_Uns32 (Bnd.Right),
                                 Bnd.Wbounds);
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

   procedure Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Type : Type_Acc;
                                 Voff : out Net;
                                 Mul : out Uns32;
                                 Off : out Uns32;
                                 W : out Width)
   is
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      Idx_Expr : constant Node := Get_Nth_Element (Indexes, 0);
      Idx_Val : Value_Acc;
      Idx_Type : Type_Acc;
   begin
      if Get_Nbr_Elements (Indexes) /= 1 then
         Error_Msg_Synth (+Name, "multi-dim arrays not yet supported");
         raise Internal_Error;
      end if;

      --  Use the base type as the subtype of the index is not synth-ed.
      Idx_Type := Get_Value_Type
        (Syn_Inst, Get_Base_Type (Get_Type (Idx_Expr)));
      Idx_Val := Synth_Expression_With_Type (Syn_Inst, Idx_Expr, Idx_Type);

      if Pfx_Type.Kind = Type_Vector then
         W := 1;
         Mul := 0;
         if Idx_Val.Kind = Value_Discrete then
            Voff := No_Net;
            Off := Index_To_Offset (Pfx_Type.Vbound, Idx_Val.Scal, Name);
         else
            Voff := Dyn_Index_To_Offset (Pfx_Type.Vbound, Idx_Val, Name);
            Off := 0;
         end if;
      elsif Pfx_Type.Kind = Type_Array then
         W := Get_Type_Width (Pfx_Type.Arr_El);
         if Idx_Val.Kind = Value_Discrete then
            Voff := No_Net;
            Off := Index_To_Offset
              (Pfx_Type.Abounds.D (1), Idx_Val.Scal, Name);
            Mul := 0;
         else
            Voff := Dyn_Index_To_Offset
              (Pfx_Type.Abounds.D (1), Idx_Val, Name);
            Mul := W;
            Off := 0;
         end if;
      else
         raise Internal_Error;
      end if;
   end Synth_Indexed_Name;

   function Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                               return Value_Acc
   is
      Pfx_Val : Value_Acc;
      Voff : Net;
      Mul : Uns32;
      Off : Uns32;
      W : Width;
      Res : Net;
   begin
      Pfx_Val := Synth_Expression (Syn_Inst, Get_Prefix (Name));

      Synth_Indexed_Name (Syn_Inst, Name, Pfx_Val.Typ, Voff, Mul, Off, W);

      if Voff = No_Net then
         pragma Assert (Mul = 0);
         if W = 1 and then Pfx_Val.Kind = Value_Array then
            return Bit_Extract (Pfx_Val, Off, Name);
         else
            Res := Build_Extract (Build_Context, Get_Net (Pfx_Val), Off, W);
            Set_Location (Res, Name);
            return Create_Value_Net (Res, Get_Array_Element (Pfx_Val.Typ));
         end if;
      else
         Res := Build_Dyn_Extract
           (Build_Context, Get_Net (Pfx_Val), Voff, Mul, Int32 (Off), W);
         Set_Location (Res, Name);
         return Create_Value_Net (Res, Get_Array_Element (Pfx_Val.Typ));
      end if;
   end Synth_Indexed_Name;

   function Is_Const (N : Net) return Boolean is
   begin
      case Get_Id (Get_Module (Get_Parent (N))) is
         when Id_Const_UB32 =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Const;

   function Get_Const (N : Net) return Int32
   is
      Inst : constant Instance := Get_Parent (N);
   begin
      case Get_Id (Get_Module (Inst)) is
         when Id_Const_UB32 =>
            return To_Int32 (Get_Param_Uns32 (Inst, 0));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Const;

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
         Inst := Get_Parent (Inp);
         case Get_Id (Get_Module (Inst)) is
            when Id_Add =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Const (Val_I0) then
                  Addend := Addend + Get_Const (Val_I0) * Factor;
                  Inp := Val_I1;
               elsif Is_Const (Val_I1) then
                  Addend := Addend + Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               else
                  --  It's an addition, but without any constant value.
                  return;
               end if;
            when Id_Sub =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Const (Val_I1) then
                  Addend := Addend - Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               else
                  --  It's a substraction, but without any constant value.
                  return;
               end if;
            when Id_Smul =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Const (Val_I0) then
                  Factor := Factor * Get_Const (Val_I0);
                  Inp := Val_I1;
               elsif Is_Const (Val_I1) then
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

   function Is_Same (L, R : Net) return Boolean is
   begin
      if L = R then
         return True;
      end if;

      if Get_Width (L) /= Get_Width (R) then
         return False;
      end if;

      declare
         Linst : constant Instance := Get_Parent (L);
         Rinst : constant Instance := Get_Parent (R);
      begin
         if Get_Id (Linst) /= Get_Id (Rinst) then
            return False;
         end if;
         if Get_Id (Linst) = Id_Uextend then
            return Is_Same (Get_Input_Net (Linst, 0),
                            Get_Input_Net (Rinst, 0));
         end if;
      end;
      return False;
   end Is_Same;

   --  Identify LEFT to/downto RIGHT as:
   --  INP * STEP + WIDTH - 1 + OFF to/downto INP * STEP + OFF
   procedure Synth_Extract_Dyn_Suffix (Loc : Node;
                                       Pfx_Bnd : Bound_Type;
                                       Left : Net;
                                       Right : Net;
                                       Inp : out Net;
                                       Step : out Uns32;
                                       Off : out Int32;
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

      if not Is_Same (L_Inp, R_Inp) then
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
      --  FIXME: what to do with negative values.
      Step := Uns32 (L_Fac);

      case Pfx_Bnd.Dir is
         when Iir_To =>
            Off := L_Add - Pfx_Bnd.Left;
            Width := Uns32 (R_Add - L_Add + 1);
         when Iir_Downto =>
            Off := R_Add - Pfx_Bnd.Right;
            Width := Uns32 (L_Add - R_Add + 1);
      end case;
   end Synth_Extract_Dyn_Suffix;

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Bound_Type;
                                 Res_Bnd : out Bound_Type;
                                 Inp : out Net;
                                 Step : out Uns32;
                                 Off : out Int32;
                                 Wd : out Uns32)
   is
      Expr : constant Node := Get_Suffix (Name);
      Left, Right : Value_Acc;
      Dir : Iir_Direction;
   begin
      Off := 0;

      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            Left := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Left_Limit (Expr));
            Right := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Right_Limit (Expr));
            Dir := Get_Direction (Expr);
         when others =>
            Error_Msg_Synth (+Expr, "only range supported for slices");
      end case;

      if Pfx_Bnd.Dir /= Dir then
         Error_Msg_Synth (+Name, "direction mismatch in slice");
         Step := 0;
         Wd := 0;
         return;
      end if;

      if Is_Const_Val (Left) and then Is_Const_Val (Right) then
         declare
            L : constant Int64 := Get_Const_Discrete (Left);
            R : constant Int64 := Get_Const_Discrete (Right);
         begin
            Inp := No_Net;
            Step := 0;

            if not In_Bounds (Pfx_Bnd, Int32 (L))
              or else not In_Bounds (Pfx_Bnd, Int32 (R))
            then
               Error_Msg_Synth (+Name, "index not within bounds");
               Wd := 0;
               Off := 0;
               return;
            end if;

            case Pfx_Bnd.Dir is
               when Iir_To =>
                  Wd := Width (R - L + 1);
                  Res_Bnd := (Dir => Iir_To,
                              Wlen => Wd,
                              Wbounds => Wd,
                              Len => Wd,
                              Left => Int32 (L),
                              Right => Int32 (R));
                  Off := Pfx_Bnd.Right - Res_Bnd.Right;
               when Iir_Downto =>
                  Wd := Width (L - R + 1);
                  Res_Bnd := (Dir => Iir_Downto,
                              Wlen => Wd,
                              Wbounds => Wd,
                              Len => Wd,
                              Left => Int32 (L),
                              Right => Int32 (R));
                  Off := Res_Bnd.Right - Pfx_Bnd.Right;
            end case;
         end;
      else
         if Is_Const (Left) or else Is_Const (Right) then
            Error_Msg_Synth
              (+Name, "left and right bounds of a slice must be "
                 & "either constant or dynamic");
            return;
         end if;
         Synth_Extract_Dyn_Suffix
           (Name, Pfx_Bnd, Get_Net (Left), Get_Net (Right),
            Inp, Step, Off, Wd);
      end if;
   end Synth_Slice_Suffix;

   function Synth_Slice_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                              return Value_Acc
   is
      Pfx_Node : constant Node := Get_Prefix (Name);
      Pfx : constant Value_Acc := Synth_Expression (Syn_Inst, Pfx_Node);
      Res_Bnd : Bound_Type;
      Res_Type : Type_Acc;
      Inp : Net;
      Step : Uns32;
      Off : Int32;
      Wd : Uns32;
      N : Net;
   begin
      Synth_Slice_Suffix
        (Syn_Inst, Name, Pfx.Typ.Vbound, Res_Bnd, Inp, Step, Off, Wd);
      if Inp /= No_Net then
         N := Build_Dyn_Extract (Build_Context, Get_Net (Pfx),
                                 Inp, Step, Off, Wd);
         Set_Location (N, Name);
         --  TODO: the bounds cannot be created as they are not known.
         Res_Type := Create_Slice_Type (Wd, Pfx.Typ.Vec_El);
         return Create_Value_Net (N, Res_Type);
      else
         N := Build_Extract (Build_Context, Get_Net (Pfx), Uns32 (Off), Wd);
         Set_Location (N, Name);
         Res_Type := Create_Vector_Type (Res_Bnd, Pfx.Typ.Vec_El);
         return Create_Value_Net (N, Res_Type);
      end if;
   end Synth_Slice_Name;

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
                                  return Value_Acc
   is
      Expr : constant Node := Get_Expression (Conv);
      Conv_Type : constant Node := Get_Type (Conv);
      Val : Value_Acc;
   begin
      Val := Synth_Expression_With_Basetype (Syn_Inst, Expr);
      case Get_Kind (Conv_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Val.Typ.Kind = Type_Float then
               return Create_Value_Discrete
                 (Int64 (Val.Fp), Get_Value_Type (Syn_Inst, Conv_Type));
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to int)");
               return null;
            end if;
         when Iir_Kind_Floating_Subtype_Definition =>
            if Is_Const (Val) then
               return Create_Value_Float
                 (Fp64 (Val.Scal), Get_Value_Type (Syn_Inst, Conv_Type));
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to float)");
               return null;
            end if;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            if Is_Vector_Type (Conv_Type) then
               return Val;
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to array)");
               return Val;
            end if;
         when others =>
            Error_Msg_Synth (+Conv, "unhandled type conversion");
            return null;
      end case;
   end Synth_Type_Conversion;

   function Synth_Assoc_In (Syn_Inst : Synth_Instance_Acc;
                            Assoc : Node) return Value_Acc is
   begin
      if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression then
         return Synth_Expression (Syn_Inst, Get_Actual (Assoc));
      else
         Error_Kind ("synth_assoc_in", Assoc);
      end if;
   end Synth_Assoc_In;

   procedure Error_Unknown_Operator (Imp : Node; Loc : Node) is
   begin
      if Get_Kind (Get_Parent (Imp)) = Iir_Kind_Package_Declaration
        and then (Get_Identifier
                    (Get_Library
                       (Get_Design_File (Get_Design_Unit (Get_Parent (Imp)))))
                    = Std_Names.Name_Ieee)
      then
         Error_Msg_Synth (+Loc, "unhandled predefined IEEE operator %i", +Imp);
         Error_Msg_Synth (+Imp, " declared here");
      else
         Error_Msg_Synth (+Loc, "user defined operator %i not handled", +Imp);
      end if;
   end Error_Unknown_Operator;

   function Synth_String_Literal (Syn_Inst : Synth_Instance_Acc; Str : Node)
                                 return Value_Acc
   is
      pragma Assert (Get_Kind (Str) = Iir_Kind_String_Literal8);
      Id : constant String8_Id := Get_String8_Id (Str);

      Str_Type : constant Node := Get_Type (Str);
      El_Type : Type_Acc;
      Bounds : Bound_Type;
      Res_Type : Type_Acc;
      Res : Value_Acc;
      Arr : Value_Array_Acc;
      Pos : Nat8;
   begin
      Bounds := Synth_Array_Bounds (Syn_Inst, Str_Type, 0);
      El_Type := Get_Value_Type (Syn_Inst, Get_Element_Subtype (Str_Type));
      Res_Type := Create_Vector_Type (Bounds, El_Type);
      Arr := Create_Value_Array (Iir_Index32 (Bounds.Len));

      for I in Arr.V'Range loop
         -- FIXME: use literal from type ??
         Pos := Str_Table.Element_String8 (Id, Pos32 (I));
         Arr.V (I) := Create_Value_Discrete (Int64 (Pos), El_Type);
      end loop;

      Res := Create_Value_Const_Array (Res_Type, Arr);
      return Res;
   end Synth_String_Literal;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Type_Acc)
     return Value_Acc
   is
      Res : Value_Acc;
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

               --  FIXME: short-circuit operators ?
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Dyadic_Operation
                    (Syn_Inst, Imp, Get_Left (Expr), Get_Right (Expr), Expr);
               else
                  Error_Unknown_Operator (Imp, Expr);
                  raise Internal_Error;
               end if;
            end;
         when Iir_Kinds_Monadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
            begin
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Monadic_Operation
                    (Syn_Inst, Def, Get_Operand (Expr), Expr);
               else
                  Error_Unknown_Operator (Imp, Expr);
                  raise Internal_Error;
               end if;
            end;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Interface_Signal_Declaration --  For PSL.
           | Iir_Kind_Signal_Declaration =>  -- For PSL.
            Res := Synth_Name (Syn_Inst, Expr);
            return Synth_Subtype_Conversion (Res, Expr_Type, False, Expr);
         when Iir_Kind_Reference_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Expr));
         when Iir_Kind_Indexed_Name =>
            return Synth_Indexed_Name (Syn_Inst, Expr);
         when Iir_Kind_Slice_Name =>
            return Synth_Slice_Name (Syn_Inst, Expr);
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Expr));
               Pfx : constant Node := Get_Prefix (Expr);
               Res_Typ : Type_Acc;
               N : Net;
            begin
               Res := Synth_Expression (Syn_Inst, Pfx);
               Res_Typ := Res.Typ.Rec.E (Idx + 1).Typ;
               --  FIXME: handle const.
               N := Build_Extract
                 (Build_Context, Get_Net (Res),
                  Res.Typ.Rec.E (Idx + 1).Off, Get_Type_Width (Res_Typ));
               return Create_Value_Net (N, Res_Typ);
            end;
         when Iir_Kind_Character_Literal =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Integer_Literal =>
            return Create_Value_Discrete (Get_Value (Expr), Expr_Type);
         when Iir_Kind_Floating_Point_Literal =>
            return Create_Value_Float (Get_Fp_Value (Expr), Expr_Type);
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return Create_Value_Discrete
              (Get_Physical_Value (Expr), Expr_Type);
         when Iir_Kind_String_Literal8 =>
            return Synth_String_Literal (Syn_Inst, Expr);
         when Iir_Kind_Enumeration_Literal =>
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Type_Conversion =>
            return Synth_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr),
               Get_Value_Type (Syn_Inst, Get_Type (Get_Type_Mark (Expr))));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Clk : Net;
               Edge : Net;
            begin
               if Imp = Vhdl.Ieee.Std_Logic_1164.Rising_Edge then
                  Clk := Get_Net
                    (Synth_Assoc_In
                       (Syn_Inst, Get_Parameter_Association_Chain (Expr)));
                  Edge := Build_Edge (Build_Context, Clk);
                  return Create_Value_Net (Edge, Boolean_Type);
               elsif Imp = Vhdl.Ieee.Std_Logic_1164.Falling_Edge then
                  Clk := Get_Net
                    (Synth_Assoc_In
                       (Syn_Inst, Get_Parameter_Association_Chain (Expr)));
                  Clk := Build_Monadic (Build_Context, Id_Not, Clk);
                  Edge := Build_Edge (Build_Context, Clk);
                  return Create_Value_Net (Edge, Boolean_Type);
               elsif Get_Implicit_Definition (Imp) /= Iir_Predefined_None then
                  return Synth_Predefined_Function_Call (Syn_Inst, Expr);
               else
                  return Synth_User_Function_Call (Syn_Inst, Expr);
               end if;
            end;
         when Iir_Kind_Aggregate =>
            return Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
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
         when others =>
            Error_Kind ("synth_expression_with_type", Expr);
      end case;
   end Synth_Expression_With_Type;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Value_Acc is
   begin
      return Synth_Expression_With_Type
        (Syn_Inst, Expr, Get_Value_Type (Syn_Inst, Get_Type (Expr)));
   end Synth_Expression;

   function Synth_Expression_With_Basetype
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc
   is
      Basetype : Type_Acc;
   begin
      Basetype := Get_Value_Type (Syn_Inst, Get_Base_Type (Get_Type (Expr)));
      return Synth_Expression_With_Type (Syn_Inst, Expr, Basetype);
   end Synth_Expression_With_Basetype;

end Synth.Expr;
