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
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;

with Areapools;
with Vhdl.Annotations; use Vhdl.Annotations;

with Synth.Errors; use Synth.Errors;
with Synth.Types; use Synth.Types;
with Synth.Stmts; use Synth.Stmts;
with Synth.Decls;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Utils; use Netlists.Utils;

package body Synth.Expr is
   function Is_Const (Val : Value_Acc) return Boolean is
   begin
      case Val.Kind is
         when Value_Discrete =>
            return True;
         when Value_Net
           | Value_Wire
           | Value_Mux2 =>
            return False;
         when others =>
            --  TODO.
            raise Internal_Error;
      end case;
   end Is_Const;

   function Is_Float (Val : Value_Acc) return Boolean is
   begin
      return Val.Kind = Value_Float;
   end Is_Float;

   function Get_Width (Val : Value_Acc) return Uns32 is
   begin
      case Val.Kind is
         when Value_Wire
           | Value_Net =>
            return Get_Width (Get_Net (Val, Null_Node));
         when others =>
            raise Internal_Error; --  TODO
      end case;
   end Get_Width;

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
     (Enum : Int64; Etype : Node; Val : out Uns32; Zx  : out Uns32)
   is
      Btype : constant Node := Get_Base_Type (Etype);
   begin
      if Btype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type then
         From_Std_Logic (Enum, Val, Zx);
      elsif Btype = Vhdl.Std_Package.Boolean_Type_Definition
        or else Btype = Vhdl.Std_Package.Bit_Type_Definition
      then
         From_Bit (Enum, Val);
         Zx := 0;
      else
         raise Internal_Error;
      end if;
   end To_Logic;

   function Bit_Extract (Val : Value_Acc; Off : Uns32) return Value_Acc is
   begin
      case Val.Kind is
         when Value_Array =>
            pragma Assert (Val.Bounds.D (1).Len >= Off);
            return Val.Arr.V (Iir_Index32 (Val.Bounds.D (1).Len - Off));
         when Value_Net
           | Value_Wire =>
            return Create_Value_Net
              (Build_Extract_Bit
                 (Build_Context, Get_Net (Val, Null_Node), Off),
               No_Bound);
         when others =>
            raise Internal_Error;
      end case;
   end Bit_Extract;

   function Synth_Uresize (N : Net; W : Width) return Net
   is
      Wn : constant Width := Get_Width (N);
   begin
      if Wn > W then
         return Build_Trunc (Build_Context, Id_Utrunc, N, W);
      elsif Wn < W then
         return Build_Extend (Build_Context, Id_Uextend, N, W);
      else
         return N;
      end if;
   end Synth_Uresize;

   function Synth_Uresize (Val : Value_Acc; Vtype : Node; W : Width)
                          return Net is
   begin
      if Is_Const (Val)
        and then (Get_Kind (Get_Base_Type (Vtype))
                    = Iir_Kind_Integer_Type_Definition)
      then
         return Build_Const_UB32
           (Build_Context, Uns32 (Val.Scal), W);
      end if;
      return Synth_Uresize (Get_Net (Val, Vtype), W);
   end Synth_Uresize;

   function Get_Index_Offset (Index: Value_Acc;
                              Bounds: Value_Bound_Acc;
                              Expr: Iir)
                             return Uns32 is
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

   procedure Fill_Array_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Res : Value_Acc;
                                   Dim : Natural)
   is
      Bound : constant Value_Bound_Acc := Res.Bounds.D (1);
      Aggr_Type : constant Node := Get_Type (Aggr);
      El_Type : constant Node := Get_Element_Subtype (Aggr_Type);
      Nbr_Dims : constant Natural := Get_Nbr_Dimensions (Aggr_Type);
      Idx_Type : constant Node := Get_Index_Type (Aggr_Type, Dim);
      type Boolean_Array is array (Uns32 range <>) of Boolean;
      pragma Pack (Boolean_Array);
      Is_Set : Boolean_Array (0 .. Bound.Len - 1);
      Value : Node;
      Assoc : Node;
      Pos : Uns32;

      procedure Set_Elem (Pos : Uns32)
      is
         Val : Value_Acc;
      begin
         if Dim = Nbr_Dims - 1 then
            Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Type);
            Res.Arr.V (Iir_Index32 (Pos + 1)) := Val;
            pragma Assert (not Is_Set (Pos));
            Is_Set (Pos) := True;
         else
            Error_Msg_Synth (+Assoc, "multi-dim aggregate not handled");
         end if;
      end Set_Elem;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := 0;
      Is_Set := (others => False);
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
                     Idx := Synth_Expression_With_Type
                       (Syn_Inst, Ch, Idx_Type);
                     if not Is_Const (Idx) then
                        Error_Msg_Synth (+Ch, "choice is not static");
                     else
                        Set_Elem (Get_Index_Offset (Idx, Bound, Ch));
                     end if;
                  end;
               when Iir_Kind_Choice_By_Range =>
                  declare
                     Ch : constant Node := Get_Choice_Range (Assoc);
                     Rng : Value_Acc;
                     Val : Value_Acc;
                  begin
                     Rng := Synth_Range_Expression (Syn_Inst, Ch);
                     Val := Create_Value_Discrete (Rng.Rng.Left);
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

   --  Convert the one-dimension VAL to a net by concatenating.
   function Vectorize_Array (Val : Value_Acc; Etype : Node) return Value_Acc
   is
      Arr : Net_Array_Acc;
      Len : Int32;
      Idx : Iir_Index32;
      Res : Value_Acc;
   begin
      --  Dynamically allocate ARR to handle large arrays.
      Arr := new Net_Array (1 .. Int32 (Val.Arr.Len));

      --  Gather consecutive constant values.
      Idx := 1;
      Len := 0;
      while Idx <= Val.Arr.Len loop
         declare
            W_Zx, B_Zx : Uns32;
            W_Va, B_Va : Uns32;
            Off : Natural;
            E : Net;
         begin
            W_Zx := 0;
            W_Va := 0;
            Off := 0;
            while Idx <= Val.Arr.Len
              and then Off < 32
              and then Is_Const (Val.Arr.V (Idx))
              and then Is_Bit_Type (Etype)
            loop
               To_Logic (Val.Arr.V (Idx).Scal, Etype, B_Va, B_Zx);
               W_Zx := W_Zx or Shift_Left (B_Zx, Off);
               W_Va := W_Va or Shift_Left (B_Va, Off);
               Off := Off + 1;
               Idx := Idx + 1;
            end loop;
            if Off = 0 then
               E := Get_Net (Val.Arr.V (Idx), Etype);
               Idx := Idx + 1;
            else
               if W_Zx = 0 then
                  E := Build_Const_UB32
                    (Build_Context, W_Va, Uns32 (Off));
               else
                  E := Build_Const_UL32
                    (Build_Context, W_Va, W_Zx, Uns32 (Off));
               end if;
            end if;
            Len := Len + 1;
            Arr (Len) := E;
         end;
      end loop;

      Concat_Array (Arr (1 .. Len));
      Res := Create_Value_Net (Arr (1), Val.Bounds.D (1));

      Free_Net_Array (Arr);

      return Res;
   end Vectorize_Array;

   function Synth_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Value_Acc
   is
      L, R : Value_Acc;
      Res : Value_Acc;
   begin
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));
      case Get_Kind (Get_Type (Rng)) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            if not (Is_Const (L) and Is_Const (R)) then
               Error_Msg_Synth (+Rng, "limits of range are not constant");
               return null;
            end if;
            Res := Create_Value_Range ((Get_Direction (Rng), L.Scal, R.Scal));
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            Res := Create_Value_Fp_Range ((Get_Direction (Rng), L.Fp, R.Fp));
         when others =>
            Error_Kind ("synth_range_expression", Get_Type (Rng));
      end case;
      return Res;
   end Synth_Range_Expression;

   function Synth_Range (Syn_Inst : Synth_Instance_Acc; Bound : Node)
                         return Value_Acc is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            return Synth_Range_Expression (Syn_Inst, Bound);
         when Iir_Kind_Integer_Subtype_Definition =>
            return Synth_Range (Syn_Inst, Get_Range_Constraint (Bound));
         when others =>
            Error_Kind ("synth_range", Bound);
      end case;
   end Synth_Range;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Natural) return Value_Bound_Acc
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
            return Bnds.Bnds.D (Iir_Index32 (Dim) + 1);
         end;
      end if;
   end Synth_Array_Bounds;

   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Value_Bound_Acc
   is
      Rng : Value_Acc;
      Len : Int64;
   begin
      Rng := Synth_Range (Syn_Inst, Atype);
      case Rng.Rng.Dir is
         when Iir_To =>
            Len := Rng.Rng.Right - Rng.Rng.Left + 1;
         when Iir_Downto =>
            Len := Rng.Rng.Left - Rng.Rng.Right + 1;
      end case;
      if Len < 0 then
         Len := 0;
      end if;
      return Create_Value_Bound
        ((Rng.Rng.Dir, Int32 (Rng.Rng.Left), Int32 (Rng.Rng.Right),
          Uns32 (Len)));
   end Synth_Bounds_From_Range;

   function Synth_Aggregate (Syn_Inst : Synth_Instance_Acc;
                             Aggr : Node;
                             Aggr_Type : Node) return Value_Acc is
   begin
      case Get_Kind (Aggr_Type) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            if not Is_Vector_Type (Aggr_Type) then
               --  TODO: generalize, in particular multi-dim arrays.
               raise Internal_Error;
            end if;
            declare
               Bnd : Value_Bound_Acc;
               Bnds : Value_Bound_Array_Acc;
               Res : Value_Acc;
            begin
               --  Create bounds.
               Bnd := Synth_Array_Bounds (Syn_Inst, Aggr_Type, 0);
               --  Allocate result
               Bnds := Create_Value_Bound_Array (1);
               Bnds.D (1) := Bnd;
               Res := Create_Value_Array (Bnds);
               Create_Array_Data (Res);
               Fill_Array_Aggregate (Syn_Inst, Aggr, Res, 0);
               if Is_Vector_Type (Aggr_Type) then
                  --  Vectorize
                  Res := Vectorize_Array
                    (Res, Get_Element_Subtype (Aggr_Type));
               end if;
               return Res;
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            raise Internal_Error;
         when others =>
            Error_Kind ("synth_aggregate", Aggr_Type);
      end case;
   end Synth_Aggregate;

   function Synth_Bit_Eq_Const
     (Cst : Value_Acc; Expr : Value_Acc; Etype : Node; Loc : Node)
     return Value_Acc
   is
      pragma Unreferenced (Loc);
      Val : Uns32;
      Zx : Uns32;
   begin
      To_Logic (Cst.Scal, Etype, Val, Zx);
      if Zx /= 0 then
         return Create_Value_Net
           (Build_Const_UL32 (Build_Context, 0, 1, 1), No_Bound);
      elsif Val = 1 then
         return Expr;
      else
         pragma Assert (Val = 0);
         return Create_Value_Net
           (Build_Monadic (Build_Context, Id_Not, Get_Net (Expr, Etype)),
            No_Bound);
      end if;
   end Synth_Bit_Eq_Const;

   --  Create the result range of an operator.  According to the ieee standard,
   --  the range is LEN-1 downto 0.
   function Create_Res_Bound (Prev : Value_Acc; N : Net) return Value_Bound_Acc
   is
      Res : Value_Bound_Acc;
      Wd : Width;
   begin
      case Prev.Kind is
         when Value_Net
           | Value_Wire =>
            Res := Extract_Bound (Prev);
         when others =>
            raise Internal_Error;
      end case;

      if Res /= No_Bound
        and then Res.Dir = Iir_Downto
        and then Res.Right = 0
      then
         --  Normalized range
         return Res;
      end if;

      Wd := Get_Width (N);
      return Create_Value_Bound ((Dir => Iir_Downto,
                                  Left => Int32 (Wd - 1),
                                  Right => 0,
                                  Len => Wd));
   end Create_Res_Bound;

   function Create_Bounds_From_Length
     (Syn_Inst : Synth_Instance_Acc; Atype : Iir; Len : Iir_Index32)
     return Value_Bound_Acc
   is
      Res : Value_Bound_Acc;
      Index_Bounds : Value_Acc;
   begin
      Index_Bounds := Synth_Range (Syn_Inst, Atype);

      Res := Create_Value_Bound ((Left => Int32 (Index_Bounds.Rng.Left),
                                  Right => 0,
                                  Dir => Index_Bounds.Rng.Dir,
                                  Len => Uns32 (Len)));

      if Len = 0 then
         --  Special case.
         Res.Right := Res.Left;
         case Index_Bounds.Rng.Dir is
            when Iir_To =>
               Res.Left := Res.Right + 1;
            when Iir_Downto =>
               Res.Left := Res.Right - 1;
         end case;
      else
         case Index_Bounds.Rng.Dir is
            when Iir_To =>
               Res.Right := Res.Left + Int32 (Len - 1);
            when Iir_Downto =>
               Res.Right := Res.Left - Int32 (Len - 1);
         end case;
      end if;
      return Res;
   end Create_Bounds_From_Length;

   function Synth_Dyadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                    Def : Iir_Predefined_Functions;
                                    Left_Expr : Node;
                                    Right_Expr : Node;
                                    Expr : Node) return Value_Acc
   is
      Ltype : constant Node := Get_Type (Left_Expr);
      Rtype : constant Node := Get_Type (Right_Expr);
      Left : Value_Acc;
      Right : Value_Acc;

      function Synth_Bit_Dyadic (Id : Dyadic_Module_Id) return Value_Acc is
      begin
         return Create_Value_Net
           (Build_Dyadic (Build_Context, Id,
                          Get_Net (Left, Ltype), Get_Net (Right, Rtype)),
            No_Bound);
      end Synth_Bit_Dyadic;

      function Synth_Compare (Id : Compare_Module_Id) return Value_Acc is
      begin
         return Create_Value_Net
           (Build_Compare (Build_Context, Id,
                           Get_Net (Left, Ltype), Get_Net (Right, Rtype)),
            No_Bound);
      end Synth_Compare;

      function Synth_Compare_Uns_Nat (Id : Compare_Module_Id)
                                     return Value_Acc is
      begin
         return Create_Value_Net
           (Build_Compare (Build_Context, Id,
                           Get_Net (Left, Ltype),
                           Synth_Uresize (Right, Rtype, Get_Width (Left))),
            No_Bound);
      end Synth_Compare_Uns_Nat;

      function Synth_Vec_Dyadic (Id : Dyadic_Module_Id) return Value_Acc
      is
         L : constant Net := Get_Net (Left, Ltype);
      begin
         return Create_Value_Net
           (Build_Dyadic (Build_Context, Id, L, Get_Net (Right, Rtype)),
            Create_Res_Bound (Left, L));
      end Synth_Vec_Dyadic;

      function Synth_Dyadic_Uns (Id : Dyadic_Module_Id; Is_Res_Vec : Boolean)
                                return Value_Acc
      is
         L : constant Net := Get_Net (Left, Ltype);
         R : constant Net := Get_Net (Right, Rtype);
         W : constant Width := Width'Max (Get_Width (L), Get_Width (R));
         Rtype : Value_Bound_Acc;
      begin
         if Is_Res_Vec then
            Rtype := Create_Value_Bound ((Iir_Downto, Int32 (W - 1), 0, W));
         else
            Rtype := No_Bound;
         end if;
         return Create_Value_Net
           (Build_Dyadic
              (Build_Context, Id, Synth_Uresize (L, W), Synth_Uresize (R, W)),
            Rtype);
      end Synth_Dyadic_Uns;

      function Synth_Compare_Uns_Uns (Id : Compare_Module_Id)
                                     return Value_Acc
      is
         L : constant Net := Get_Net (Left, Ltype);
         R : constant Net := Get_Net (Right, Rtype);
         W : constant Width := Width'Max (Get_Width (L), Get_Width (R));
      begin
         return Create_Value_Net
           (Build_Compare (Build_Context, Id,
                           Synth_Uresize (L, W),
                           Synth_Uresize (R, W)),
            No_Bound);
      end Synth_Compare_Uns_Uns;

      function Synth_Dyadic_Uns_Nat (Id : Dyadic_Module_Id) return Value_Acc
      is
         L : constant Net := Get_Net (Left, Ltype);
      begin
         return Create_Value_Net
           (Build_Dyadic (Build_Context, Id,
                          L, Synth_Uresize (Right, Rtype, Get_Width (Left))),
            Create_Res_Bound (Left, L));
      end Synth_Dyadic_Uns_Nat;
   begin
      Left := Synth_Expression (Syn_Inst, Left_Expr);
      Right := Synth_Expression (Syn_Inst, Right_Expr);

      case Def is
         when Iir_Predefined_Error =>
            return null;

         when Iir_Predefined_Bit_And
           | Iir_Predefined_Boolean_And
           | Iir_Predefined_Ieee_1164_Scalar_And =>
            return Synth_Bit_Dyadic (Id_And);
         when Iir_Predefined_Bit_Xor
           | Iir_Predefined_Ieee_1164_Scalar_Xor =>
            return Synth_Bit_Dyadic (Id_Xor);
         when Iir_Predefined_Bit_Or
           | Iir_Predefined_Boolean_Or
           | Iir_Predefined_Ieee_1164_Scalar_Or =>
            return Synth_Bit_Dyadic (Id_Or);
         when Iir_Predefined_Ieee_1164_Vector_And =>
            return Synth_Vec_Dyadic (Id_And);
         when Iir_Predefined_Ieee_1164_Vector_Or =>
            return Synth_Vec_Dyadic (Id_Or);
         when Iir_Predefined_Ieee_1164_Vector_Xor =>
            return Synth_Vec_Dyadic (Id_Xor);

         when Iir_Predefined_Bit_Nor
           | Iir_Predefined_Ieee_1164_Scalar_Nor =>
            return Synth_Bit_Dyadic (Id_Nor);
         when Iir_Predefined_Bit_Nand
           | Iir_Predefined_Ieee_1164_Scalar_Nand =>
            return Synth_Bit_Dyadic (Id_Nand);
         when Iir_Predefined_Bit_Xnor
           | Iir_Predefined_Ieee_1164_Scalar_Xnor =>
            return Synth_Bit_Dyadic (Id_Xnor);

         when Iir_Predefined_Enum_Equality =>
            if Is_Bit_Type (Ltype) then
               pragma Assert (Is_Bit_Type (Rtype));
               if Is_Const (Left) then
                  return Synth_Bit_Eq_Const (Left, Right, Ltype, Expr);
               elsif Is_Const (Right) then
                  return Synth_Bit_Eq_Const (Right, Left, Ltype, Expr);
               end if;
            end if;
            return Synth_Compare (Id_Eq);
         when Iir_Predefined_Enum_Inequality =>
            --  TODO: Optimize ?
            return Synth_Compare (Id_Ne);
         when Iir_Predefined_Enum_Less_Equal =>
            return Synth_Compare (Id_Ult);

         when Iir_Predefined_Array_Equality =>
            --  TODO: check size, handle non-vector.
            return Synth_Compare (Id_Eq);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat =>
            --  "+" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Id_Add);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Sl =>
            --  "+" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns (Id_Add, True);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat =>
            --  "-" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Id_Sub);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns =>
            --  "-" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns (Id_Sub, True);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat =>
            --  "=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Eq);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Slv =>
            --  "=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Eq);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Nat =>
            --  "/=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ne);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat =>
            --  "<" (Unsigned, Natural)
            if Is_Const (Right) and then Right.Scal = 0 then
               --  Always false.
               return Create_Value_Discrete (0);
            end if;
            return Synth_Compare_Uns_Nat (Id_Ult);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Slv =>
            --  "<" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ult);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Slv =>
            --  "<=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ule);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat =>
            --  ">" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ugt);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Slv =>
            --  ">" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ugt);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Slv =>
            --  ">=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Uge);
         when Iir_Predefined_Array_Element_Concat =>
            declare
               L : constant Net := Get_Net (Left, Ltype);
            begin
               return Create_Value_Net
                 (Build_Concat2 (Build_Context, L,
                                 Get_Net (Right, Rtype)),
                  Create_Bounds_From_Length
                    (Syn_Inst,
                     Get_Index_Type (Get_Type (Expr), 0),
                     Iir_Index32 (Get_Width (L) + 1)));
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               R : constant Net := Get_Net (Right, Rtype);
            begin
               return Create_Value_Net
                 (Build_Concat2 (Build_Context, Get_Net (Left, Ltype), R),
                  Create_Bounds_From_Length
                    (Syn_Inst,
                     Get_Index_Type (Get_Type (Expr), 0),
                     Iir_Index32 (Get_Width (R) + 1)));
            end;
         when Iir_Predefined_Element_Element_Concat =>
            return Create_Value_Net
              (Build_Concat2 (Build_Context,
                              Get_Net (Left, Ltype),
                              Get_Net (Right, Rtype)),
               Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), 2));
         when Iir_Predefined_Array_Array_Concat =>
            declare
               L : constant Net := Get_Net (Left, Ltype);
               R : constant Net := Get_Net (Right, Ltype);
            begin
               return Create_Value_Net
                 (Build_Concat2 (Build_Context, L, R),
                  Create_Bounds_From_Length
                    (Syn_Inst,
                     Get_Index_Type (Get_Type (Expr), 0),
                     Iir_Index32 (Get_Width (L) + Get_Width (R))));
            end;
         when Iir_Predefined_Integer_Plus =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete (Left.Scal + Right.Scal);
            else
               return Synth_Vec_Dyadic (Id_Add);
            end if;
         when Iir_Predefined_Integer_Minus =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete (Left.Scal - Right.Scal);
            else
               return Synth_Vec_Dyadic (Id_Sub);
            end if;
         when Iir_Predefined_Integer_Mul =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete (Left.Scal * Right.Scal);
            else
               return Synth_Vec_Dyadic (Id_Mul);
            end if;
         when Iir_Predefined_Integer_Div =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete (Left.Scal / Right.Scal);
            else
               Error_Msg_Synth (+Expr, "non-constant division not supported");
               return null;
            end if;

         when others =>
            Error_Msg_Synth (+Expr, "synth_dyadic_operation: unhandled "
                               & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Dyadic_Operation;

   function Synth_Monadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                     Def : Iir_Predefined_Functions;
                                     Operand_Expr : Node;
                                     Loc : Node) return Value_Acc
   is
      Operand : Value_Acc;

      function Synth_Bit_Monadic (Id : Monadic_Module_Id) return Value_Acc is
      begin
         return Create_Value_Net
           (Build_Monadic (Build_Context, Id,
                           Get_Net (Operand, Get_Type (Operand_Expr))),
            No_Bound);
      end Synth_Bit_Monadic;

      function Synth_Vec_Monadic (Id : Monadic_Module_Id) return Value_Acc
      is
         Op: constant Net := Get_Net (Operand, Get_Type (Operand_Expr));
      begin
         return Create_Value_Net
           (Build_Monadic (Build_Context, Id, Op),
            Create_Res_Bound (Operand, Op));
      end Synth_Vec_Monadic;
   begin
      Operand := Synth_Expression (Syn_Inst, Operand_Expr);
      case Def is
         when Iir_Predefined_Error =>
            return null;
         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Synth_Bit_Monadic (Id_Not);
         when Iir_Predefined_Ieee_1164_Vector_Not =>
            return Synth_Vec_Monadic (Id_Not);
         when Iir_Predefined_Ieee_Numeric_Std_Neg_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn =>
            return Synth_Vec_Monadic (Id_Neg);
         when others =>
            Error_Msg_Synth
              (+Loc,
               "unhandled monadic: " & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Monadic_Operation;

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
           | Iir_Kind_Iterator_Declaration =>
            return Get_Value (Syn_Inst, Name);
         when Iir_Kind_Enumeration_Literal =>
            return Create_Value_Discrete (Int64 (Get_Enum_Pos (Name)));
         when others =>
            Error_Kind ("synth_name", Name);
      end case;
   end Synth_Name;

   function In_Bounds (Bnd : Value_Bound_Acc; V : Int32) return Boolean is
   begin
      case Bnd.Dir is
         when Iir_To =>
            return V >= Bnd.Left and then V <= Bnd.Right;
         when Iir_Downto =>
            return V <= Bnd.Left and then V >= Bnd.Right;
      end case;
   end In_Bounds;

   function Index_To_Offset (Pfx : Value_Acc; Idx : Int64; Loc : Node)
                            return Uns32
   is
      Rng : Value_Bound_Acc;
   begin
      Rng := Extract_Bound (Pfx);
      if not In_Bounds (Rng, Int32 (Idx)) then
         Error_Msg_Synth (+Loc, "index not within bounds");
         return 0;
      end if;

      --  The offset is from the LSB (bit 0).  Bit 0 is the rightmost one.
      case Rng.Dir is
         when Iir_To =>
            return Uns32 (Rng.Right - Int32 (Idx));
         when Iir_Downto =>
            return Uns32 (Int32 (Idx) - Rng.Right);
      end case;
   end Index_To_Offset;

   function Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                               return Value_Acc
   is
      Pfx : constant Value_Acc :=
        Synth_Expression (Syn_Inst, Get_Prefix (Name));
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      Idx_Val : constant Value_Acc :=
        Synth_Expression (Syn_Inst, Get_Nth_Element (Indexes, 0));
      Off : Uns32;
   begin
      if Get_Nbr_Elements (Indexes) /= 1 then
         Error_Msg_Synth (+Name, "multi-dim arrays not supported");
         return null;
      end if;

      if Idx_Val.Kind /= Value_Discrete then
         Error_Msg_Synth (+Name, "non constant integer index not supported");
         return null;
      end if;

      Off := Index_To_Offset (Pfx, Idx_Val.Scal, Name);
      return Bit_Extract (Pfx, Off);
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
         if Get_Id (Get_Module (Inst)) = Id_Add then
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
         elsif Get_Id (Get_Module (Inst)) = Id_Sub then
            Val_I0 := Get_Input_Net (Inst, 0);
            Val_I1 := Get_Input_Net (Inst, 1);
            if Is_Const (Val_I1) then
               Addend := Addend - Get_Const (Val_I1) * Factor;
               Inp := Val_I0;
            else
               --  It's a substraction, but without any constant value.
               return;
            end if;
         elsif Get_Id (Get_Module (Inst)) = Id_Mul then
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
         else
            --  Cannot decompose it.
            return;
         end if;
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

   procedure Synth_Extract_Dyn_Suffix (Loc : Node;
                                       Pfx_Bnd : Value_Bound_Acc;
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
                                 Pfx_Bnd : Value_Bound_Acc;
                                 Res_Bnd : out Value_Bound_Acc;
                                 Inp : out Net;
                                 Step : out Uns32;
                                 Off : out Int32;
                                 Wd : out Uns32)
   is
      Expr : constant Node := Get_Suffix (Name);
      Left, Right : Value_Acc;
      Dir : Iir_Direction;
   begin
      Res_Bnd := null;
      Off := 0;

      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            Left := Synth_Expression (Syn_Inst, Get_Left_Limit (Expr));
            Right := Synth_Expression (Syn_Inst, Get_Right_Limit (Expr));
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

      if not Is_Const (Left) or else not Is_Const (Right) then
         if Left.Kind /= Value_Net and Right.Kind /= Value_Net then
            Error_Msg_Synth
              (+Name, "left and right bounds of a slice must be "
                 & "either constant or dynamic");
            return;
         else
            Synth_Extract_Dyn_Suffix (Name, Pfx_Bnd, Left.N, Right.N,
                                      Inp, Step, Off, Wd);
         end if;
      else
         Inp := No_Net;
         Step := 0;

         if not In_Bounds (Pfx_Bnd, Int32 (Left.Scal))
           or else not In_Bounds (Pfx_Bnd, Int32 (Right.Scal))
         then
            Error_Msg_Synth (+Name, "index not within bounds");
            Wd := 0;
            Off := 0;
            return;
         end if;

         case Pfx_Bnd.Dir is
            when Iir_To =>
               Wd := Width (Right.Scal - Left.Scal + 1);
               Res_Bnd := Create_Value_Bound
                 (Value_Bound_Type'(Dir => Iir_To,
                                    Len => Wd,
                                    Left => Int32 (Left.Scal),
                                    Right => Int32 (Right.Scal)));
               Off := Pfx_Bnd.Right - Res_Bnd.Right;
            when Iir_Downto =>
               Wd := Width (Left.Scal - Right.Scal + 1);
               Res_Bnd := Create_Value_Bound
                 (Value_Bound_Type'(Dir => Iir_Downto,
                                    Len => Wd,
                                    Left => Int32 (Left.Scal),
                                    Right => Int32 (Right.Scal)));
               Off := Res_Bnd.Right - Pfx_Bnd.Right;
         end case;
      end if;
   end Synth_Slice_Suffix;

   function Synth_Slice_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                              return Value_Acc
   is
      Pfx_Node : constant Node := Get_Prefix (Name);
      Pfx : constant Value_Acc := Synth_Expression (Syn_Inst, Pfx_Node);
      Bnd : Value_Bound_Acc;
      Res_Bnd : Value_Bound_Acc;
      Inp : Net;
      Step : Uns32;
      Off : Int32;
      Wd : Uns32;
   begin
      Bnd := Extract_Bound (Pfx);
      Synth_Slice_Suffix (Syn_Inst, Name, Bnd, Res_Bnd, Inp, Step, Off, Wd);
      if Inp /= No_Net then
         return Create_Value_Net
           (Build_Dyn_Extract (Build_Context,
                               Get_Net (Pfx, Get_Type (Pfx_Node)),
                               Inp, Step, Off, Wd),
            null);
      else
         return Create_Value_Net
           (Build_Extract (Build_Context,
                           Get_Net (Pfx, Get_Type (Pfx_Node)),
                           Uns32 (Off), Wd),
            Res_Bnd);
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
      Clk := Get_Net (Synth_Name (Syn_Inst, Prefix), Get_Type (Prefix));
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
   function Synth_Clock_Edge (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Value_Acc
   is
      pragma Assert (Get_Kind (Expr) = Iir_Kind_And_Operator);
      Left : constant Node := Get_Left (Expr);
      Right : constant Node := Get_Right (Expr);
      Prefix : Node;
   begin
      --  Try with left.
      Prefix := Extract_Event_Expr_Prefix (Left);
      if Is_Valid (Prefix) then
         return Create_Value_Net
           (Extract_Clock_Level (Syn_Inst, Right, Prefix), No_Bound);
      end if;

      --  Try with right.
      Prefix := Extract_Event_Expr_Prefix (Right);
      if Is_Valid (Prefix) then
         return Create_Value_Net
           (Extract_Clock_Level (Syn_Inst, Left, Prefix), No_Bound);
      end if;

      return null;
   end Synth_Clock_Edge;

   function Synth_Type_Conversion (Syn_Inst : Synth_Instance_Acc; Conv : Node)
                                  return Value_Acc
   is
      Expr : constant Node := Get_Expression (Conv);
      Conv_Type : constant Node := Get_Type (Conv);
      Val : Value_Acc;
   begin
      Val := Synth_Expression (Syn_Inst, Expr);
      case Get_Kind (Conv_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Is_Float (Val) then
               return Create_Value_Discrete (Int64 (Val.Fp));
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to int)");
               return null;
            end if;
         when Iir_Kind_Floating_Subtype_Definition =>
            if Is_Const (Val) then
               return Create_Value_Float (Fp64 (Val.Scal));
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
      Bounds : Value_Bound_Acc;
      Barr : Value_Bound_Array_Acc;
      Res : Value_Acc;
      Pos : Nat8;
   begin
      Bounds := Synth_Array_Bounds (Syn_Inst, Str_Type, 0);
      Barr := Create_Value_Bound_Array (1);
      Barr.D (1) := Bounds;
      Res := Create_Value_Array (Barr);

      for I in Res.Arr.V'Range loop
         -- FIXME: use literal from type ??
         Pos := Str_Table.Element_String8 (Id, Pos32 (I));
         Res.Arr.V (I) := Create_Value_Discrete (Int64 (Pos));
      end loop;

      return Res;
   end Synth_String_Literal;

   function Eval_To_Unsigned (Arg : Int64; Sz : Int64) return Value_Acc
   is
      Len : constant Iir_Index32 := Iir_Index32 (Sz);
      Arr : Value_Array_Acc;
      Bnds : Value_Bound_Array_Acc;
   begin
      Arr := Create_Value_Array (Len);
      for I in 1 .. Len loop
         Arr.V (Len - I + 1) := Create_Value_Discrete
           ((Arg / 2 ** Natural (I - 1)) mod 2);
      end loop;
      Bnds := Create_Value_Bound_Array (1);
      Bnds.D (1) := Create_Value_Bound
        ((Dir => Iir_Downto, Left => Int32 (Len - 1), Right => 0,
          Len => Uns32 (Len)));
      return Create_Value_Array (Bnds, Arr);
   end Eval_To_Unsigned;

   function Synth_User_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Expr);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Bod : constant Node := Get_Subprogram_Body (Imp);
      Subprg_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
      Res : Value_Acc;
   begin
      Areapools.Mark (M, Instance_Pool.all);
      Subprg_Inst := Make_Instance (Syn_Inst, Get_Info (Bod));

      Subprg_Inst.Name := New_Internal_Name (Build_Context);

      Synth_Subprogram_Association
        (Subprg_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      Decls.Synth_Declarations (Subprg_Inst, Get_Declaration_Chain (Bod));

      Synth_Sequential_Statements
        (Subprg_Inst, Get_Sequential_Statement_Chain (Bod));

      Res := Subprg_Inst.Return_Value;

      Free_Instance (Subprg_Inst);
      Areapools.Release (M, Instance_Pool.all);

      return Res;
   end Synth_User_Function_Call;

   function Synth_Predefined_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Expr);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Subprg_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
   begin
      Areapools.Mark (M, Instance_Pool.all);
      Subprg_Inst := Make_Instance (Syn_Inst, Get_Info (Imp));

      Synth_Subprogram_Association
        (Subprg_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      case Def is
         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns =>
            declare
               Arg : constant Value_Acc := Subprg_Inst.Objects (1);
               Size : constant Value_Acc := Subprg_Inst.Objects (2);
               Arg_Net : Net;
            begin
               if not Is_Const (Size) then
                  Error_Msg_Synth (+Expr, "to_unsigned size must be constant");
                  return Arg;
               else
                  --  FIXME: what if the arg is constant too ?
                  if Is_Const (Arg) then
                     return Eval_To_Unsigned (Arg.Scal, Size.Scal);
                  else
                     Arg_Net := Get_Net (Arg, Get_Type (Inter_Chain));
                     return Create_Value_Net
                       (Synth_Uresize (Arg_Net, Uns32 (Size.Scal)),
                        Create_Res_Bound (Arg, Arg_Net));
                  end if;
               end if;
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat =>
            --  UNSIGNED to Natural.
            return Create_Value_Net
              (Synth_Uresize (Get_Net (Subprg_Inst.Objects (1),
                                       Get_Type (Inter_Chain)), 32),
               null);
         when Iir_Predefined_Ieee_Math_Real_Log2 =>
            declare
               V : constant Value_Acc := Subprg_Inst.Objects (1);

               function Log2 (Arg : Fp64) return Fp64;
               pragma Import (C, Log2);
            begin
               if not Is_Float (V) then
                  Error_Msg_Synth
                    (+Expr, "argument must be a float value");
                  return null;
               end if;
               return Create_Value_Float (Log2 (V.Fp));
            end;
         when Iir_Predefined_Ieee_Math_Real_Ceil =>
            declare
               V : constant Value_Acc := Subprg_Inst.Objects (1);

               function Ceil (Arg : Fp64) return Fp64;
               pragma Import (C, Ceil);
            begin
               if not Is_Float (V) then
                  Error_Msg_Synth
                    (+Expr, "argument must be a float value");
                  return null;
               end if;
               return Create_Value_Float (Ceil (V.Fp));
            end;
         when others =>
            Error_Msg_Synth
              (+Expr,
               "unhandled function: " & Iir_Predefined_Functions'Image (Def));
      end case;

      Free_Instance (Subprg_Inst);
      Areapools.Release (M, Instance_Pool.all);

      return null;
   end Synth_Predefined_Function_Call;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Node)
     return Value_Acc is
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Edge : Value_Acc;
            begin
               --  Match clock-edge
               if Def = Iir_Predefined_Boolean_And then
                  Edge := Synth_Clock_Edge (Syn_Inst, Expr);
                  if Edge /= null then
                     return Edge;
                  end if;
               end if;

               --  FIXME: short-circuit operators ?
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Dyadic_Operation
                    (Syn_Inst, Def, Get_Left (Expr), Get_Right (Expr), Expr);
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
           | Iir_Kind_Interface_Signal_Declaration =>  -- For PSL...
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Reference_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Expr));
         when Iir_Kind_Indexed_Name =>
            return Synth_Indexed_Name (Syn_Inst, Expr);
         when Iir_Kind_Slice_Name =>
            return Synth_Slice_Name (Syn_Inst, Expr);
         when Iir_Kind_Character_Literal =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Integer_Literal =>
            return Create_Value_Discrete (Get_Value (Expr));
         when Iir_Kind_Floating_Point_Literal =>
            return Create_Value_Float (Get_Fp_Value (Expr));
         when Iir_Kind_Physical_Int_Literal =>
            return Create_Value_Discrete (Get_Physical_Value (Expr));
         when Iir_Kind_String_Literal8 =>
            return Synth_String_Literal (Syn_Inst, Expr);
         when Iir_Kind_Enumeration_Literal =>
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Type_Conversion =>
            return Synth_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr), Get_Type (Expr));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Clk : Net;
               Edge : Net;
            begin
               if Imp = Vhdl.Ieee.Std_Logic_1164.Rising_Edge then
                  Clk := Get_Net
                    (Synth_Assoc_In
                       (Syn_Inst, Get_Parameter_Association_Chain (Expr)),
                     Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type);
                  Edge := Build_Edge (Build_Context, Clk);
                  return Create_Value_Net (Edge, No_Bound);
               elsif Imp = Vhdl.Ieee.Std_Logic_1164.Falling_Edge then
                  Clk := Get_Net
                    (Synth_Assoc_In
                       (Syn_Inst, Get_Parameter_Association_Chain (Expr)),
                     Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type);
                  Clk := Build_Monadic (Build_Context, Id_Not, Clk);
                  Edge := Build_Edge (Build_Context, Clk);
                  return Create_Value_Net (Edge, No_Bound);
               elsif Get_Implicit_Definition (Imp) /= Iir_Predefined_None then
                  return Synth_Predefined_Function_Call (Syn_Inst, Expr);
               else
                  return Synth_User_Function_Call (Syn_Inst, Expr);
               end if;
            end;
         when Iir_Kind_Aggregate =>
            return Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
         when others =>
            Error_Kind ("synth_expression", Expr);
      end case;
      raise Fatal_Error;
      return null;
   end Synth_Expression_With_Type;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Value_Acc is
   begin
      return Synth_Expression_With_Type (Syn_Inst, Expr, Get_Type (Expr));
   end Synth_Expression;

end Synth.Expr;
