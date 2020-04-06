--  Operations synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Types; use Types;
with Types_Utils; use Types_Utils;
with Mutils;

with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;

with Areapools;

with Netlists; use Netlists;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Utils;

with Synth.Errors; use Synth.Errors;
with Synth.Stmts; use Synth.Stmts;
with Synth.Expr; use Synth.Expr;
with Synth.Source;
with Synth.Static_Oper; use Synth.Static_Oper;

package body Synth.Oper is
   procedure Set_Location (N : Net; Loc : Node)
     renames Synth.Source.Set_Location;

   function Synth_Uresize (N : Net; W : Width; Loc : Node) return Net is
   begin
      return Build2_Uresize (Build_Context, N, W, Get_Location (Loc));
   end Synth_Uresize;

   function Synth_Uresize (Val : Valtyp; W : Width; Loc : Node) return Net
   is
      Res : Net;
   begin
      if Is_Static (Val.Val) and then Val.Typ.Kind = Type_Discrete then
         if Val.Typ.Drange.Is_Signed and then Read_Discrete (Val) < 0 then
            --  TODO.
            raise Internal_Error;
         else
            Res := Build2_Const_Uns
              (Build_Context, To_Uns64 (Read_Discrete (Val)), W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      end if;
      return Synth_Uresize (Get_Net (Val), W, Loc);
   end Synth_Uresize;

   function Synth_Sresize (Val : Valtyp; W : Width; Loc : Node) return Net
   is
      Res : Net;
   begin
      if Is_Static (Val.Val) and then Val.Typ.Kind = Type_Discrete then
         if Val.Typ.Drange.Is_Signed then
            Res := Build2_Const_Int (Build_Context, Read_Discrete (Val), W);
         else
            --  TODO.
            raise Internal_Error;
         end if;
         Set_Location (Res, Loc);
         return Res;
      end if;
      return Build2_Sresize (Build_Context, Get_Net (Val), W,
                             Get_Location (Loc));
   end Synth_Sresize;

   function Synth_Bit_Eq_Const (Cst : Valtyp; Expr : Valtyp; Loc : Node)
                               return Valtyp
   is
      Val : Uns32;
      Zx : Uns32;
      N : Net;
   begin
      if Is_Static (Expr.Val) then
         return Create_Value_Discrete
           (Boolean'Pos (Read_Discrete (Cst) = Read_Discrete (Expr)),
            Boolean_Type);
      end if;

      To_Logic (Read_Discrete (Cst), Cst.Typ, Val, Zx);
      if Zx /= 0 then
         --  Equal unknown -> return X
         N := Build_Const_UL32 (Build_Context, 0, 1, 1);
         Set_Location (N, Loc);
         return Create_Value_Net (N, Boolean_Type);
      elsif Val = 1 then
         --  The result type is a boolean; convert if needed.
         if Expr.Typ.Kind = Type_Logic then
            return Create_Value_Net (Get_Net (Expr), Boolean_Type);
         else
            pragma Assert (Expr.Typ.Kind = Type_Bit);
            return Expr;
         end if;
      else
         pragma Assert (Val = 0);
         N := Build_Monadic (Build_Context, Id_Not, Get_Net (Expr));
         Set_Location (N, Loc);
         return Create_Value_Net (N, Boolean_Type);
      end if;
   end Synth_Bit_Eq_Const;

   --  Create the result range of an operator.  According to the ieee standard,
   --  the range is LEN-1 downto 0.
   function Create_Res_Bound (Prev : Valtyp) return Type_Acc
   is
      Res : Type_Acc;
   begin
      Res := Prev.Typ;

      if Res.Vbound.Dir = Iir_Downto
        and then Res.Vbound.Right = 0
      then
         --  Normalized range
         return Res;
      end if;

      return Create_Vec_Type_By_Length (Res.W, Res.Vec_El);
   end Create_Res_Bound;

   function Create_Bounds_From_Length
     (Syn_Inst : Synth_Instance_Acc; Atype : Iir; Len : Iir_Index32)
     return Bound_Type
   is
      Res : Bound_Type;
      Index_Bounds : Discrete_Range_Type;
   begin
      Synth_Discrete_Range (Syn_Inst, Atype, Index_Bounds);

      Res := (Left => Int32 (Index_Bounds.Left),
              Right => 0,
              Dir => Index_Bounds.Dir,
              Len => Uns32 (Len));

      if Len = 0 then
         --  Special case.
         Res.Right := Res.Left;
         case Index_Bounds.Dir is
            when Iir_To =>
               Res.Left := Res.Right + 1;
            when Iir_Downto =>
               Res.Left := Res.Right - 1;
         end case;
      else
         case Index_Bounds.Dir is
            when Iir_To =>
               Res.Right := Res.Left + Int32 (Len - 1);
            when Iir_Downto =>
               Res.Right := Res.Left - Int32 (Len - 1);
         end case;
      end if;
      return Res;
   end Create_Bounds_From_Length;

   --  Do a match comparison between CST and OPER.
   --  Return No_Net if CST has incorrect value.
   function Synth_Match (Cst : Valtyp;
                         Oper : Valtyp;
                         Expr : Node;
                         Op : Compare_Module_Id := Id_Eq) return Net
   is
      Wd : constant Width := Cst.Typ.W;
      pragma Assert (Wd > 0);
      Nwords : constant Natural := Natural ((Wd + 31) / 32);
      Mask : Uns32_Arr_Acc;
      Vals : Uns32_Arr_Acc;
      Boff : Natural;
      Woff : Natural;
      B : Uns32;
      M : Uns32;
      Nv : Net;
      Nm : Net;
      Res : Net;
   begin
      --  Flatten 0/1 DC.
      Mask := new Uns32_Arr'(0 .. Nwords - 1 => 0);
      Vals := new Uns32_Arr'(0 .. Nwords - 1 => 0);

      Boff := 0;
      Woff := 0;
      for I in reverse 1 .. Vec_Length (Cst.Typ) loop
         case Read_U8 (Cst.Val.Mem + Size_Type (I - 1)) is
            when Std_Logic_0_Pos
              |  Std_Logic_L_Pos =>
               B := 0;
               M := 1;
            when Std_Logic_1_Pos
              |  Std_Logic_H_Pos =>
               B := 1;
               M := 1;
            when Std_Logic_U_Pos
              |  Std_Logic_X_Pos
              |  Std_Logic_Z_Pos
              |  Std_Logic_W_Pos =>
               --  Never match
               --  FIXME: warning ?
               Unchecked_Deallocate (Mask);
               Unchecked_Deallocate (Vals);
               return No_Net;
            when Std_Logic_D_Pos =>
               B := 0;
               M := 0;
            when others =>
               raise Internal_Error;
         end case;
         Mask (Woff) := Mask (Woff) or Shift_Left (M, Boff);
         Vals (Woff) := Vals (Woff) or Shift_Left (B, Boff);
         Boff := Boff + 1;
         if Boff = 32 then
            Boff := 0;
            Woff := Woff + 1;
         end if;
      end loop;

      --  Generate and + eq
      Nv := Build2_Const_Vec (Build_Context, Wd, Vals.all);
      Set_Location (Nv, Expr);
      Unchecked_Deallocate (Vals);
      Nm := Build2_Const_Vec (Build_Context, Wd, Mask.all);
      Set_Location (Nm, Expr);
      Unchecked_Deallocate (Mask);
      Res := Build_Dyadic (Build_Context, Id_And, Get_Net (Oper), Nm);
      Set_Location (Res, Expr);
      Res := Build_Compare (Build_Context, Op, Res, Nv);
      Set_Location (Res, Expr);

      return Res;
   end Synth_Match;

   --  Note: LEFT or RIGHT can be a single bit.
   function Synth_Dyadic_Uns_Uns
     (Id : Dyadic_Module_Id; Left, Right : Valtyp; Expr : Node) return Valtyp
   is
      W : constant Width := Width'Max (Left.Typ.W, Right.Typ.W);
      El_Typ : Type_Acc;
      Rtype : Type_Acc;
      L1, R1 : Net;
      N : Net;
   begin
      if Left.Typ.Kind = Type_Vector then
         El_Typ := Left.Typ.Vec_El;
      elsif Right.Typ.Kind = Type_Vector then
         El_Typ := Right.Typ.Vec_El;
      else
         raise Internal_Error;
      end if;
      Rtype := Create_Vec_Type_By_Length (W, El_Typ);
      L1 := Synth_Uresize (Left, W, Expr);
      R1 := Synth_Uresize (Right, W, Expr);
      N := Build_Dyadic (Build_Context, Id, L1, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Rtype);
   end Synth_Dyadic_Uns_Uns;

   function Synth_Dyadic_Uns_Nat
     (Id : Dyadic_Module_Id; Left, Right : Valtyp; Expr : Node) return Valtyp
   is
      L : constant Net := Get_Net (Left);
      R1 : Net;
      N : Net;
   begin
      R1 := Synth_Uresize (Right, Left.Typ.W, Expr);
      N := Build_Dyadic (Build_Context, Id, L, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Left));
   end Synth_Dyadic_Uns_Nat;

   function Synth_Dyadic_Nat_Uns
     (Id : Dyadic_Module_Id; Left, Right : Valtyp; Expr : Node) return Valtyp
   is
      R : constant Net := Get_Net (Right);
      L1 : Net;
      N : Net;
   begin
      L1 := Synth_Uresize (Left, Right.Typ.W, Expr);
      N := Build_Dyadic (Build_Context, Id, L1, R);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Right));
   end Synth_Dyadic_Nat_Uns;

   --  Note: LEFT or RIGHT can be a single bit.
   function Synth_Dyadic_Sgn_Sgn
     (Id : Dyadic_Module_Id; Left, Right : Valtyp; Expr : Node) return Valtyp
   is
      W : constant Width := Width'Max (Left.Typ.W, Right.Typ.W);
      El_Typ : Type_Acc;
      Rtype : Type_Acc;
      L1, R1 : Net;
      N : Net;
   begin
      if Left.Typ.Kind = Type_Vector then
         El_Typ := Left.Typ.Vec_El;
      elsif Right.Typ.Kind = Type_Vector then
         El_Typ := Right.Typ.Vec_El;
      else
         raise Internal_Error;
      end if;
      Rtype := Create_Vec_Type_By_Length (W, El_Typ);
      L1 := Synth_Sresize (Left, W, Expr);
      R1 := Synth_Sresize (Right, W, Expr);
      N := Build_Dyadic (Build_Context, Id, L1, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Rtype);
   end Synth_Dyadic_Sgn_Sgn;

   function Synth_Dyadic_Sgn_Int
     (Id : Dyadic_Module_Id; Left, Right : Valtyp; Expr : Node) return Valtyp
   is
      L : constant Net := Get_Net (Left);
      R1 : Net;
      N : Net;
   begin
      R1 := Synth_Sresize (Right, Left.Typ.W, Expr);
      N := Build_Dyadic (Build_Context, Id, L, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Left));
   end Synth_Dyadic_Sgn_Int;

   function Synth_Dyadic_Int_Sgn
     (Id : Dyadic_Module_Id; Left, Right : Valtyp; Expr : Node) return Valtyp
   is
      R : constant Net := Get_Net (Right);
      L1 : Net;
      N : Net;
   begin
      L1 := Synth_Sresize (Left, Right.Typ.W, Expr);
      N := Build_Dyadic (Build_Context, Id, R, L1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Right));
   end Synth_Dyadic_Int_Sgn;

   function Synth_Dyadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                    Imp : Node;
                                    Left_Expr : Node;
                                    Right_Expr : Node;
                                    Expr : Node) return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Inter_Chain : constant Node :=
        Get_Interface_Declaration_Chain (Imp);
      Expr_Type : constant Node := Get_Type (Expr);
      Left_Type : constant Node := Get_Type (Inter_Chain);
      Right_Type : constant Node := Get_Type (Get_Chain (Inter_Chain));
      Left_Typ : constant Type_Acc :=
        Get_Subtype_Object (Syn_Inst, Left_Type);
      Right_Typ : constant Type_Acc :=
        Get_Subtype_Object (Syn_Inst, Right_Type);
      Expr_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Expr_Type);
      Left : Valtyp;
      Right : Valtyp;

      function Synth_Bit_Dyadic (Id : Dyadic_Module_Id) return Valtyp
      is
         N : Net;
      begin
         N := Build_Dyadic (Build_Context, Id,
                            Get_Net (Left), Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Left.Typ);
      end Synth_Bit_Dyadic;

      function Synth_Compare (Id : Compare_Module_Id; Res_Type : Type_Acc)
                             return Valtyp
      is
         N : Net;
      begin
         pragma Assert (Left_Type = Right_Type);
         pragma Assert (Res_Type = Expr_Typ);
         N := Build_Compare
           (Build_Context, Id, Get_Net (Left), Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Type);
      end Synth_Compare;

      function Synth_Minmax (Id : Compare_Module_Id) return Valtyp
      is
         L : constant Net := Get_Net (Left);
         R : constant Net := Get_Net (Right);
         Sel, N : Net;
      begin
         pragma Assert (Left_Type = Right_Type);
         Sel := Build_Compare (Build_Context, Id, L, R);
         Set_Location (Sel, Expr);
         N := Build_Mux2 (Build_Context, Sel, R, L);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Expr_Typ);
      end Synth_Minmax;

      function Synth_Compare_Array (Id, Id_Eq : Compare_Module_Id;
                                    Res_Type : Type_Acc) return Valtyp
      is
         pragma Unreferenced (Id_Eq);
         N : Net;
      begin
         if Left.Typ.Kind = Type_Vector then
            Warning_Msg_Synth
              (+Expr, "comparing non-numeric vector is unexpected");
            if Left.Typ.W = Right.Typ.W then
               N := Build_Compare
                 (Get_Build (Syn_Inst), Id, Get_Net (Left), Get_Net (Right));
               Set_Location (N, Expr);
               return Create_Value_Net (N, Res_Type);
            elsif Left.Typ.W < Right.Typ.W then
               --  TODO: truncate right, compare using id_eq.
               raise Internal_Error;
            else
               --  TODO: truncate left, compare using id.
               raise Internal_Error;
            end if;
         else
            raise Internal_Error;
         end if;
      end Synth_Compare_Array;

      function Synth_Compare_Uns_Nat
        (Id : Compare_Module_Id; Res_Type : Type_Acc) return Valtyp
      is
         N : Net;
      begin
         N := Synth_Uresize (Right, Left.Typ.W, Expr);
         N := Build_Compare (Build_Context, Id, Get_Net (Left), N);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Type);
      end Synth_Compare_Uns_Nat;

      function Synth_Compare_Nat_Uns
        (Id : Compare_Module_Id; Res_Type : Type_Acc) return Valtyp
      is
         N : Net;
      begin
         N := Synth_Uresize (Left, Right.Typ.W, Expr);
         N := Build_Compare (Build_Context, Id, Get_Net (Right), N);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Type);
      end Synth_Compare_Nat_Uns;

      function Synth_Compare_Sgn_Int
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp
      is
         N : Net;
      begin
         N := Synth_Sresize (Right, Left.Typ.W, Expr);
         N := Build_Compare (Build_Context, Id, Get_Net (Left), N);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Typ);
      end Synth_Compare_Sgn_Int;

      function Synth_Compare_Int_Sgn
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp
      is
         N : Net;
      begin
         N := Synth_Sresize (Left, Right.Typ.W, Expr);
         N := Build_Compare (Build_Context, Id, N, Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Typ);
      end Synth_Compare_Int_Sgn;

      function Synth_Vec_Dyadic (Id : Dyadic_Module_Id) return Valtyp
      is
         N : Net;
      begin
         --  FIXME: check same length.
         N := Build_Dyadic (Build_Context, Id,
                            Get_Net (Left), Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Create_Res_Bound (Left));
      end Synth_Vec_Dyadic;

      function Synth_Int_Dyadic (Id : Dyadic_Module_Id) return Valtyp
      is
         Etype : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Expr_Type);
         N : Net;
      begin
         N := Build_Dyadic
           (Build_Context, Id, Get_Net (Left), Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Etype);
      end Synth_Int_Dyadic;

      function Synth_Compare_Uns_Uns
        (Id : Compare_Module_Id; Res_Type : Type_Acc) return Valtyp
      is
         W : constant Width := Width'Max (Left.Typ.W, Right.Typ.W);
         L1, R1 : Net;
         N : Net;
      begin
         L1 := Synth_Uresize (Left, W, Expr);
         R1 := Synth_Uresize (Right, W, Expr);
         N := Build_Compare (Build_Context, Id, L1, R1);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Type);
      end Synth_Compare_Uns_Uns;

      function Synth_Compare_Sgn_Sgn
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp
      is
         W : constant Width := Width'Max (Left.Typ.W, Right.Typ.W);
         L1, R1 : Net;
         N : Net;
      begin
         L1 := Synth_Sresize (Left, W, Expr);
         R1 := Synth_Sresize (Right, W, Expr);
         N := Build_Compare (Build_Context, Id, L1, R1);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Typ);
      end Synth_Compare_Sgn_Sgn;

      type Oper_Kind is (Oper_Left, Oper_Right);

      function Synth_Udivmod (Id : Dyadic_Module_Id; Vec : Oper_Kind)
                            return Valtyp
      is
         W : constant Width := Width'Max (Left.Typ.W, Right.Typ.W);
         L1, R1 : Net;
         Res_Typ : Type_Acc;
         N : Net;
      begin
         L1 := Synth_Uresize (Left, W, Expr);
         R1 := Synth_Uresize (Right, W, Expr);
         case Vec is
            when Oper_Left =>
               Res_Typ := Left.Typ;
            when Oper_Right =>
               Res_Typ := Right.Typ;
         end case;
         Res_Typ := Create_Vec_Type_By_Length (Res_Typ.W, Res_Typ.Vec_El);
         N := Build_Dyadic (Ctxt, Id, L1, R1);
         Set_Location (N, Expr);
         N := Build2_Uresize (Ctxt, N, Res_Typ.W, Get_Location (Expr));
         return Create_Value_Net (N, Res_Typ);
      end Synth_Udivmod;

      function Synth_Sdivmod (Id : Dyadic_Module_Id; Vec : Oper_Kind)
                            return Valtyp
      is
         W : constant Width := Width'Max (Left.Typ.W, Right.Typ.W);
         L1, R1 : Net;
         Res_Typ : Type_Acc;
         N : Net;
      begin
         L1 := Synth_Sresize (Left, W, Expr);
         R1 := Synth_Sresize (Right, W, Expr);
         case Vec is
            when Oper_Left =>
               Res_Typ := Left.Typ;
            when Oper_Right =>
               Res_Typ := Right.Typ;
         end case;
         Res_Typ := Create_Vec_Type_By_Length (Res_Typ.W, Res_Typ.Vec_El);
         N := Build_Dyadic (Ctxt, Id, L1, R1);
         Set_Location (N, Expr);
         N := Build2_Sresize (Ctxt, N, Res_Typ.W, Get_Location (Expr));
         return Create_Value_Net (N, Res_Typ);
      end Synth_Sdivmod;

      function Synth_Shift (Id_Pos : Module_Id; Id_Neg : Module_Id)
                           return Valtyp
      is
         pragma Unreferenced (Id_Neg);
         L1, R1 : Net;
         N : Net;
         Is_Pos : Boolean;
      begin
         Is_Pos := Is_Positive (Right);

         L1 := Get_Net (Left);
         R1 := Get_Net (Right);
         if Is_Pos then
            N := Build_Shift_Rotate (Ctxt, Id_Pos, L1, R1);
         else
            raise Internal_Error;
         end if;
         Set_Location (N, Expr);
         return Create_Value_Net (N, Create_Res_Bound (Left));
      end Synth_Shift;

      function Synth_Rotation (Id : Module_Id) return Valtyp
      is
         Amt : Int64;
         Ww : Width;
         L1, R1 : Net;
         N : Net;
      begin
         if Is_Static_Val (Right.Val) then
            Amt := Get_Static_Discrete (Right);
            if Amt < 0 then
               raise Internal_Error;
            end if;
            Amt := Amt mod Int64 (Left.Typ.W);
            R1 := Build_Const_UB32 (Ctxt, Uns32 (Amt), Right.Typ.W);
            Set_Location (R1, Right_Expr);
         elsif not Is_Positive (Right) then
            Error_Msg_Synth (+Expr, "rotation quantity must be unsigned");
            return Left;
         else
            R1 := Get_Net (Right);
            Ww := Netlists.Utils.Clog2 (Left.Typ.W);
            if Right.Typ.W >= Ww then
               if Mutils.Is_Power2 (Uns64 (Left.Typ.W)) then
                  R1 := Build2_Trunc (Ctxt, Id_Utrunc, R1, Ww, +Expr);
               else
                  Error_Msg_Synth
                    (+Expr, "vector length of rotation must be a power of 2");
                  return Left;
               end if;
            end if;
         end if;
         L1 := Get_Net (Left);
         N := Build_Shift_Rotate (Ctxt, Id, L1, R1);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Create_Res_Bound (Left));
      end Synth_Rotation;
   begin
      Left := Synth_Expression_With_Type (Syn_Inst, Left_Expr, Left_Typ);
      if Left = No_Valtyp then
         return No_Valtyp;
      end if;
      Left := Synth_Subtype_Conversion (Left, Left_Typ, False, Expr);
      Strip_Const (Left);
      Right := Synth_Expression_With_Type (Syn_Inst, Right_Expr, Right_Typ);
      if Right = No_Valtyp then
         return No_Valtyp;
      end if;
      Right := Synth_Subtype_Conversion (Right, Right_Typ, False, Expr);
      Strip_Const (Right);

      if Is_Static_Val (Left.Val) and Is_Static_Val (Right.Val) then
         return Synth_Static_Dyadic_Predefined
           (Syn_Inst, Imp, Left, Right, Expr);
      end if;

      case Def is
         when Iir_Predefined_Error =>
            return No_Valtyp;

         when Iir_Predefined_Bit_And
           | Iir_Predefined_Boolean_And
           | Iir_Predefined_Ieee_1164_Scalar_And =>
            return Synth_Bit_Dyadic (Id_And);
         when Iir_Predefined_Bit_Xor
           | Iir_Predefined_Boolean_Xor
           | Iir_Predefined_Ieee_1164_Scalar_Xor =>
            return Synth_Bit_Dyadic (Id_Xor);
         when Iir_Predefined_Bit_Or
           | Iir_Predefined_Boolean_Or
           | Iir_Predefined_Ieee_1164_Scalar_Or =>
            return Synth_Bit_Dyadic (Id_Or);
         when Iir_Predefined_Bit_Nor
           | Iir_Predefined_Ieee_1164_Scalar_Nor =>
            return Synth_Bit_Dyadic (Id_Nor);
         when Iir_Predefined_Bit_Nand
           | Iir_Predefined_Boolean_Nand
           | Iir_Predefined_Ieee_1164_Scalar_Nand =>
            return Synth_Bit_Dyadic (Id_Nand);
         when Iir_Predefined_Bit_Xnor
           | Iir_Predefined_Boolean_Xnor
           | Iir_Predefined_Ieee_1164_Scalar_Xnor =>
            return Synth_Bit_Dyadic (Id_Xnor);

         when Iir_Predefined_Ieee_1164_Vector_And
            | Iir_Predefined_Ieee_Numeric_Std_And_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_And);
         when Iir_Predefined_Ieee_1164_Vector_Or
            | Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Or);
         when Iir_Predefined_Ieee_1164_Vector_Nand
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Nand);
         when Iir_Predefined_Ieee_1164_Vector_Nor
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Nor);
         when Iir_Predefined_TF_Array_Xor
           | Iir_Predefined_Ieee_1164_Vector_Xor
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Xor);
         when Iir_Predefined_Ieee_1164_Vector_Xnor
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Xnor);

         when Iir_Predefined_Enum_Equality =>
            if Left_Typ = Bit_Type
              or else Left_Typ = Logic_Type
            then
               if Is_Static (Left.Val) then
                  return Synth_Bit_Eq_Const (Left, Right, Expr);
               elsif Is_Static (Right.Val) then
                  return Synth_Bit_Eq_Const (Right, Left, Expr);
               end if;
            end if;
            return Synth_Compare (Id_Eq, Boolean_Type);
         when Iir_Predefined_Enum_Inequality =>
            --  TODO: Optimize ?
            return Synth_Compare (Id_Ne, Boolean_Type);
         when Iir_Predefined_Enum_Less_Equal =>
            return Synth_Compare (Id_Ult, Boolean_Type);

         when Iir_Predefined_Std_Ulogic_Match_Equality =>
            return Synth_Compare (Id_Eq, Logic_Type);
         when Iir_Predefined_Std_Ulogic_Match_Inequality =>
            return Synth_Compare (Id_Ne, Logic_Type);
         when Iir_Predefined_Std_Ulogic_Match_Less =>
            return Synth_Compare (Id_Ult, Logic_Type);
         when Iir_Predefined_Std_Ulogic_Match_Less_Equal =>
            return Synth_Compare (Id_Ule, Logic_Type);
         when Iir_Predefined_Std_Ulogic_Match_Greater =>
            return Synth_Compare (Id_Ugt, Logic_Type);
         when Iir_Predefined_Std_Ulogic_Match_Greater_Equal =>
            return Synth_Compare (Id_Uge, Logic_Type);

         when Iir_Predefined_Array_Equality
           | Iir_Predefined_Record_Equality =>
            if not Is_Matching_Bounds (Left.Typ, Right.Typ) then
               Warning_Msg_Synth
                 (+Expr,
                  "length of '=' operands doesn't match, result is false");
               return Create_Value_Discrete (0, Boolean_Type);
            end if;
            return Synth_Compare (Id_Eq, Boolean_Type);
         when Iir_Predefined_Std_Ulogic_Array_Match_Equality =>
            declare
               Cst, Oper : Valtyp;
               Res : Net;
            begin
               if Left.Typ.W /= Right.Typ.W then
                  Error_Msg_Synth
                    (+Expr, "operands of ?= don't have the same size");
                  return Create_Value_Discrete (0, Bit_Type);
               end if;

               if Is_Static (Left.Val) then
                  Cst := Left;
                  Oper := Right;
               elsif Is_Static (Right.Val) then
                  Cst := Right;
                  Oper := Left;
               else
                  Warning_Msg_Synth
                    (+Expr, "no operand of ?= is constant, handled like =");
                  return Synth_Compare (Id_Eq, Logic_Type);
               end if;
               Res := Synth_Match (Cst, Oper, Expr);
               if Res = No_Net then
                  return Create_Value_Discrete (Std_Logic_X_Pos, Expr_Typ);
               else
                  return Create_Value_Net (Res, Logic_Type);
               end if;
            end;
         when Iir_Predefined_Std_Ulogic_Array_Match_Inequality =>
            declare
               Cst, Oper : Valtyp;
               Res : Net;
            begin
               if Left.Typ.W /= Right.Typ.W then
                  Error_Msg_Synth
                    (+Expr, "operands of ?/= don't have the same size");
                  return Create_Value_Discrete (1, Bit_Type);
               end if;

               if Is_Static (Left.Val) then
                  Cst := Left;
                  Oper := Right;
               elsif Is_Static (Right.Val) then
                  Cst := Right;
                  Oper := Left;
               else
                  Warning_Msg_Synth
                    (+Expr, "no operand of ?/= is constant, handled like /=");
                  return Synth_Compare (Id_Ne, Logic_Type);
               end if;
               Res := Synth_Match (Cst, Oper, Expr, Id_Ne);
               if Res = No_Net then
                  return Create_Value_Discrete (Std_Logic_X_Pos, Expr_Typ);
               else
                  return Create_Value_Net (Res, Logic_Type);
               end if;
            end;
         when Iir_Predefined_Array_Inequality
            | Iir_Predefined_Record_Inequality =>
            if not Is_Matching_Bounds (Left.Typ, Right.Typ) then
               Warning_Msg_Synth
                 (+Expr,
                  "length of '/=' operands doesn't match, result is true");
               return Create_Value_Discrete (1, Boolean_Type);
            end if;
            return Synth_Compare (Id_Ne, Boolean_Type);
         when Iir_Predefined_Array_Greater =>
            return Synth_Compare_Array (Id_Ugt, Id_Uge, Boolean_Type);
         when Iir_Predefined_Array_Less =>
            return Synth_Compare_Array (Id_Ult, Id_Ule, Boolean_Type);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Int =>
            --  "+" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Nat_Uns =>
            --  "+" (Natural, Unsigned)
            return Synth_Dyadic_Nat_Uns (Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Log
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Log
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Slv
           | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv =>
            --  "+" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns_Uns (Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int
           | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Int =>
            --  "+" (Signed, Integer)
            return Synth_Dyadic_Sgn_Int (Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Int_Sgn =>
            --  "+" (Integer, Signed)
            return Synth_Dyadic_Int_Sgn (Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Log
           | Iir_Predefined_Ieee_Numeric_Std_Add_Log_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv
           | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Slv =>
            --  "+" (Signed, Signed)
            return Synth_Dyadic_Sgn_Sgn (Id_Add, Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Int =>
            --  "-" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Slv
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Log_Slv
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Log =>
            --  "-" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns_Uns (Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Int
           | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Int =>
            --  "-" (Signed, Integer)
            return Synth_Dyadic_Sgn_Int (Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Int_Sgn =>
            --  "-" (Integer, Signed)
            return Synth_Dyadic_Int_Sgn (Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Log
           | Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Sgn =>
            --  "-" (Signed, Signed)
            return Synth_Dyadic_Sgn_Sgn (Id_Sub, Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv
           | Iir_Predefined_Ieee_Std_Logic_Signed_Mul_Slv_Slv =>
            --  "*" (Signed, Signed)
            declare
               W : constant Width := Left.Typ.W + Right.Typ.W;
               L, R : Net;
               N : Net;
            begin
               L := Synth_Sresize (Left, W, Left_Expr);
               R := Synth_Sresize (Right, W, Right_Expr);
               N := Build_Dyadic (Build_Context, Id_Smul, L, R);
               Set_Location (N, Expr);
               return Create_Value_Net
                 (N, Create_Vec_Type_By_Length (W, Left.Typ.Vec_El));
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Int =>
            declare
               Lw : constant Width := Left.Typ.W;
               W : constant Width := 2 * Lw;
               Rtype : Type_Acc;
               L, R : Net;
               N : Net;
            begin
               L := Synth_Sresize (Left, W, Left_Expr);
               R := Synth_Sresize (Right, W, Right_Expr);
               Rtype := Create_Vec_Type_By_Length (W, Left.Typ.Vec_El);
               N := Build_Dyadic (Build_Context, Id_Smul, L, R);
               Set_Location (N, Expr);
               return Create_Value_Net (N, Rtype);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Int_Sgn =>
            declare
               Rw : constant Width := Right.Typ.W;
               W : constant Width := 2 * Rw;
               Rtype : Type_Acc;
               L, R : Net;
               N : Net;
            begin
               L := Synth_Sresize (Left, W, Left_Expr);
               R := Synth_Sresize (Right, W, Right_Expr);
               Rtype := Create_Vec_Type_By_Length (W, Right.Typ.Vec_El);
               N := Build_Dyadic (Build_Context, Id_Smul, L, R);
               Set_Location (N, Expr);
               return Create_Value_Net (N, Rtype);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Mul_Slv_Slv =>
            --  "*" (unsigned, unsigned)
            declare
               W : constant Width := Left.Typ.W + Right.Typ.W;
               Rtype : Type_Acc;
               L, R : Net;
               N : Net;
            begin
               L := Synth_Uresize (Left, W, Left_Expr);
               R := Synth_Uresize (Right, W, Right_Expr);
               Rtype := Create_Vec_Type_By_Length (W, Left.Typ.Vec_El);
               N := Build_Dyadic (Build_Context, Id_Umul, L, R);
               Set_Location (N, Expr);
               return Create_Value_Net (N, Rtype);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Nat =>
            declare
               Lw : constant Width := Left.Typ.W;
               W : constant Width := 2 * Lw;
               L1, R1 : Net;
               Rtype : Type_Acc;
               N : Net;
            begin
               L1 := Synth_Uresize (Left, W, Expr);
               R1 := Synth_Uresize (Right, W, Expr);
               Rtype := Create_Vec_Type_By_Length (W, Left.Typ.Vec_El);
               N := Build_Dyadic (Ctxt, Id_Umul, L1, R1);
               Set_Location (N, Expr);
               return Create_Value_Net (N, Rtype);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Nat_Uns =>
            declare
               Rw : constant Width := Right.Typ.W;
               W : constant Width := 2 * Rw;
               L1, R1 : Net;
               Rtype : Type_Acc;
               N : Net;
            begin
               L1 := Synth_Uresize (Left, W, Expr);
               R1 := Synth_Uresize (Right, W, Expr);
               Rtype := Create_Vec_Type_By_Length (W, Right.Typ.Vec_El);
               N := Build_Dyadic (Ctxt, Id_Umul, L1, R1);
               Set_Location (N, Expr);
               return Create_Value_Net (N, Rtype);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Nat =>
            return Synth_Udivmod (Id_Udiv, Oper_Left);
         when Iir_Predefined_Ieee_Numeric_Std_Div_Nat_Uns =>
            return Synth_Udivmod (Id_Udiv, Oper_Right);

         when Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Int
           | Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Sgn =>
            return Synth_Sdivmod (Id_Sdiv, Oper_Left);
         when Iir_Predefined_Ieee_Numeric_Std_Div_Int_Sgn =>
            return Synth_Sdivmod (Id_Sdiv, Oper_Right);

         when Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Nat =>
            return Synth_Udivmod (Id_Umod, Oper_Left);
         when Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Rem_Nat_Uns =>
            return Synth_Udivmod (Id_Umod, Oper_Right);

         when Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Int =>
            return Synth_Sdivmod (Id_Srem, Oper_Left);
         when Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Rem_Int_Sgn =>
            return Synth_Sdivmod (Id_Srem, Oper_Right);

         when Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Nat =>
            return Synth_Udivmod (Id_Umod, Oper_Left);
         when Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Mod_Nat_Uns =>
            return Synth_Udivmod (Id_Umod, Oper_Right);

         when Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Int =>
            return Synth_Sdivmod (Id_Smod, Oper_Left);
         when Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Mod_Int_Sgn =>
            return Synth_Sdivmod (Id_Smod, Oper_Right);

         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Slv
           | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Uns =>
            --  "=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat
           | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Nat =>
            --  "=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Nat_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Nat_Uns =>
            --  "=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Int
           | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Int =>
            --  "=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Sgn =>
            --  "=" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Int_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Int_Sgn =>
            --  "=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Eq, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Slv
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Uns =>
            --  "/=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Nat
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Nat =>
            --  "/=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Nat_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Nat_Uns =>
            --  "/=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Sgn =>
            --  "/=" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Int
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Int =>
            --  "/=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Int_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Int_Sgn =>
            --  "/=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Ne, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat
           | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Nat =>
            --  "<" (Unsigned, Natural)
            if Is_Static (Right.Val) and then Read_Discrete (Right) = 0 then
               --  Always false.
               return Create_Value_Discrete (0, Expr_Typ);
            end if;
            return Synth_Compare_Uns_Nat (Id_Ult, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Slv
           | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Uns =>
            --  "<" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ult, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Nat_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Nat_Uns =>
            --  "<" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ult, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Sgn =>
            --  "<" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Int
           | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Int =>
            --  "<" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Int_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Int_Sgn =>
            --  "<" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Slt, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Slv
           | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Uns =>
            --  "<=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ule, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Nat
           | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Nat =>
            --  "<=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ule, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Nat_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Nat_Uns =>
            --  "<=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ule, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Sgn =>
            --  "<=" (Signed, Signed)
            return Synth_Compare_Sgn_Sgn (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Int
           | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Int =>
            --  "<=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Int_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Int_Sgn =>
            --  "<=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Sle, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Slv
           | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Uns =>
            --  ">" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ugt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Int
           | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Nat =>
            --  ">" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ugt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Nat_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Nat_Uns =>
            --  ">" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ugt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Sgn =>
            --  ">" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Int
           | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Int =>
            --  ">" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Int_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Int_Sgn =>
            --  ">" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Sgt, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Slv
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Uns =>
            --  ">=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Uge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Nat_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Nat_Uns =>
            --  ">=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Uge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Nat
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Int =>
            --  ">=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Uge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Sgn =>
            --  ">=" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Int
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Int =>
            --  ">=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Int_Sgn
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Int_Sgn =>
            --  ">=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Sge, Expr_Typ);

         when Iir_Predefined_Array_Element_Concat =>
            declare
               L : constant Net := Get_Net (Left);
               Bnd : Bound_Type;
               N : Net;
            begin
               N := Build_Concat2 (Build_Context, L, Get_Net (Right));
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Width (L) + 1));

               return Create_Value_Net
                 (N, Create_Onedimensional_Array_Subtype (Left_Typ, Bnd));
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               R : constant Net := Get_Net (Right);
               Bnd : Bound_Type;
               N : Net;
            begin
               N := Build_Concat2 (Build_Context, Get_Net (Left), R);
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Width (R) + 1));

               return Create_Value_Net
                 (N, Create_Onedimensional_Array_Subtype (Right_Typ, Bnd));
            end;
         when Iir_Predefined_Element_Element_Concat =>
            declare
               N : Net;
               Bnd : Bound_Type;
            begin
               N := Build_Concat2
                 (Build_Context, Get_Net (Left), Get_Net (Right));
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), 2);
               return Create_Value_Net
                 (N, Create_Onedimensional_Array_Subtype (Expr_Typ, Bnd));
            end;
         when Iir_Predefined_Array_Array_Concat =>
            declare
               L : constant Net := Get_Net (Left);
               R : constant Net := Get_Net (Right);
               Bnd : Bound_Type;
               N : Net;
            begin
               N := Build_Concat2 (Build_Context, L, R);
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Width (L) + Get_Width (R)));

               return Create_Value_Net
                 (N, Create_Onedimensional_Array_Subtype (Expr_Typ, Bnd));
            end;
         when Iir_Predefined_Integer_Plus =>
            return Synth_Int_Dyadic (Id_Add);
         when Iir_Predefined_Integer_Minus =>
            return Synth_Int_Dyadic (Id_Sub);
         when Iir_Predefined_Integer_Mul =>
            return Synth_Int_Dyadic (Id_Smul);
         when Iir_Predefined_Integer_Div =>
            return Synth_Int_Dyadic (Id_Sdiv);
         when Iir_Predefined_Integer_Mod =>
            if Is_Static_Val (Right.Val) then
               --  Optimize when the divisor is a power of 2.
               declare
                  use Mutils;
                  Etype : constant Type_Acc :=
                    Get_Subtype_Object (Syn_Inst, Expr_Type);
                  R : constant Int64 := Get_Static_Discrete (Right);
                  Log_R : Natural;
                  N : Net;
               begin
                  if R > 0 and then Is_Power2 (Uns64 (R)) then
                     Log_R := Clog2 (Uns64 (R));
                     pragma Assert (Log_R <= Natural (Left.Typ.W));
                     N := Get_Net (Left);
                     N := Build2_Extract (Ctxt, N, 0, Width (Log_R));
                     N := Build2_Uresize (Ctxt, N, Left.Typ.W,
                                          Get_Location (Expr));
                     return Create_Value_Net (N, Etype);
                  end if;
               end;
            end if;
            return Synth_Int_Dyadic (Id_Smod);
         when Iir_Predefined_Integer_Rem =>
            return Synth_Int_Dyadic (Id_Srem);
         when Iir_Predefined_Integer_Exp =>
            Error_Msg_Synth
              (+Expr, "non-constant exponentiation not supported");
            return No_Valtyp;
         when Iir_Predefined_Integer_Less_Equal =>
            return Synth_Compare (Id_Sle, Boolean_Type);
         when Iir_Predefined_Integer_Less =>
            return Synth_Compare (Id_Slt, Boolean_Type);
         when Iir_Predefined_Integer_Greater_Equal =>
            return Synth_Compare (Id_Sge, Boolean_Type);
         when Iir_Predefined_Integer_Greater =>
            return Synth_Compare (Id_Sgt, Boolean_Type);
         when Iir_Predefined_Integer_Equality =>
            return Synth_Compare (Id_Eq, Boolean_Type);
         when Iir_Predefined_Integer_Inequality =>
            return Synth_Compare (Id_Ne, Boolean_Type);
         when Iir_Predefined_Integer_Minimum =>
            return Synth_Minmax (Id_Slt);
         when Iir_Predefined_Integer_Maximum =>
            return Synth_Minmax (Id_Sgt);
         when Iir_Predefined_Physical_Physical_Div =>
            Error_Msg_Synth (+Expr, "non-constant division not supported");
            return No_Valtyp;

         when Iir_Predefined_Floating_Div =>
            Error_Msg_Synth (+Expr, "non-constant division not supported");
            return No_Valtyp;

         when Iir_Predefined_Ieee_Numeric_Std_Sra_Sgn_Int =>
            return Synth_Shift (Id_Asr, Id_None);

         when Iir_Predefined_Ieee_Numeric_Std_Sll_Uns_Int =>
            return Synth_Shift (Id_Lsl, Id_None);

         when Iir_Predefined_Ieee_1164_Vector_Ror =>
            return Synth_Rotation (Id_Ror);

         when others =>
            Error_Msg_Synth (+Expr, "synth_dyadic_operation: unhandled "
                               & Iir_Predefined_Functions'Image (Def));
            return No_Valtyp;
      end case;
   end Synth_Dyadic_Operation;

   function Synth_Monadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                     Imp : Node;
                                     Operand_Expr : Node;
                                     Loc : Node) return Valtyp
   is
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Inter_Chain : constant Node :=
        Get_Interface_Declaration_Chain (Imp);
      Oper_Type : constant Node := Get_Type (Inter_Chain);
      Oper_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Oper_Type);
      Operand : Valtyp;

      function Synth_Bit_Monadic (Id : Monadic_Module_Id) return Valtyp
      is
         N : Net;
      begin
         N := Build_Monadic (Build_Context, Id, Get_Net (Operand));
         Set_Location (N, Loc);
         return Create_Value_Net (N, Operand.Typ);
      end Synth_Bit_Monadic;

      function Synth_Vec_Monadic (Id : Monadic_Module_Id) return Valtyp
      is
         Op: constant Net := Get_Net (Operand);
         N : Net;
      begin
         N := Build_Monadic (Build_Context, Id, Op);
         Set_Location (N, Loc);
         return Create_Value_Net (N, Create_Res_Bound (Operand));
      end Synth_Vec_Monadic;

      function Synth_Vec_Reduce_Monadic (Id : Reduce_Module_Id) return Valtyp
      is
         Op: constant Net := Get_Net (Operand);
         N : Net;
      begin
         N := Build_Reduce (Build_Context, Id, Op);
         Set_Location (N, Loc);
         return Create_Value_Net (N, Operand.Typ.Vec_El);
      end Synth_Vec_Reduce_Monadic;
   begin
      Operand := Synth_Expression_With_Type (Syn_Inst, Operand_Expr, Oper_Typ);
      if Operand = No_Valtyp then
         return No_Valtyp;
      end if;
      Operand := Synth_Subtype_Conversion (Operand, Oper_Typ, False, Loc);
      Strip_Const (Operand);

      if Is_Static_Val (Operand.Val) then
         return Synth_Static_Monadic_Predefined
           (Syn_Inst, Imp, Operand, Loc);
      end if;

      case Def is
         when Iir_Predefined_Error =>
            return No_Valtyp;
         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Synth_Bit_Monadic (Id_Not);
         when Iir_Predefined_Boolean_Not
           | Iir_Predefined_Bit_Not =>
            return Synth_Bit_Monadic (Id_Not);
         when Iir_Predefined_Ieee_1164_Vector_Not
            | Iir_Predefined_Ieee_Numeric_Std_Not_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Not_Sgn =>
            return Synth_Vec_Monadic (Id_Not);
         when Iir_Predefined_Ieee_Numeric_Std_Neg_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn =>
            return Synth_Vec_Monadic (Id_Neg);
         when Iir_Predefined_Ieee_Numeric_Std_Abs_Sgn =>
            return Synth_Vec_Monadic (Id_Abs);
         when Iir_Predefined_Ieee_1164_Vector_And_Reduce =>
            return Synth_Vec_Reduce_Monadic(Id_Red_And);
         when Iir_Predefined_Ieee_1164_Vector_Or_Reduce =>
            return Synth_Vec_Reduce_Monadic(Id_Red_Or);
         when Iir_Predefined_Ieee_1164_Condition_Operator =>
            return Create_Value_Net
              (Get_Net (Operand),
               Get_Subtype_Object (Syn_Inst, Get_Type (Imp)));
         when Iir_Predefined_Integer_Negation =>
            declare
               N : Net;
            begin
               N := Build_Monadic (Build_Context, Id_Neg, Get_Net (Operand));
               Set_Location (N, Loc);
               return Create_Value_Net (N, Operand.Typ);
            end;
         when others =>
            Error_Msg_Synth
              (+Loc,
               "unhandled monadic: " & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Monadic_Operation;

   function Synth_Shift_Rotate (Id : Shift_Rotate_Module_Id;
                                Left, Right : Valtyp;
                                Expr : Node) return Valtyp
   is
      L : constant Net := Get_Net (Left);
      N : Net;
   begin
      N := Build_Shift_Rotate (Build_Context, Id, L, Get_Net (Right));
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Left));
   end Synth_Shift_Rotate;

   function Synth_Dynamic_Predefined_Function_Call
     (Subprg_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Subprg_Inst);
      Imp  : constant Node := Get_Implementation (Expr);
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Param1 : Node;
      Param2 : Node;
      Res_Typ : constant Type_Acc :=
        Get_Subtype_Object (Subprg_Inst, Get_Type (Imp));

      --  Resize PARAM1 to PARAM2 bit according to IS_SIGNED.
      function Synth_Conv_Vector (Is_Signed : Boolean) return Valtyp
      is
         Arg : constant Valtyp := Get_Value (Subprg_Inst, Param1);
         Size_Vt : Valtyp;
         Size : Width;
         Arg_Net : Net;
      begin
         Size_Vt := Get_Value (Subprg_Inst, Param2);
         Strip_Const (Size_Vt);
         if not Is_Static (Size_Vt.Val) then
            Error_Msg_Synth (+Expr, "size parameter must be constant");
            return No_Valtyp;
         end if;
         Size := Uns32 (Read_Discrete (Size_Vt));
         Arg_Net := Get_Net (Arg);
         Arg_Net := Build2_Resize (Ctxt, Arg_Net, Size, Is_Signed,
                                   Get_Location (Expr));
         return Create_Value_Net
           (Arg_Net, Create_Vec_Type_By_Length (Size, Logic_Type));
      end Synth_Conv_Vector;

      L : Valtyp;
      R : Valtyp;
   begin
      Param1 := Inter_Chain;
      if Param1 /= Null_Node then
         L := Get_Value (Subprg_Inst, Param1);
         Param2 := Get_Chain (Inter_Chain);
         if Param2 /= Null_Node then
            R := Get_Value (Subprg_Inst, Param2);
         else
            R := No_Valtyp;
         end if;
      else
         L := No_Valtyp;
         R := No_Valtyp;
         Param2 := Null_Node;
      end if;

      case Def is
         when Iir_Predefined_Ieee_1164_Rising_Edge =>
            declare
               Clk : Net;
               Edge : Net;
            begin
               Clk := Get_Net (L);
               Edge := Build_Edge (Ctxt, Clk);
               return Create_Value_Net (Edge, Boolean_Type);
            end;
         when Iir_Predefined_Ieee_1164_Falling_Edge =>
            declare
               Clk : Net;
               Edge : Net;
            begin
               Clk := Get_Net (L);
               Clk := Build_Monadic (Ctxt, Id_Not, Clk);
               Edge := Build_Edge (Ctxt, Clk);
               return Create_Value_Net (Edge, Boolean_Type);
            end;
         when Iir_Predefined_Ieee_1164_Scalar_Is_X
           | Iir_Predefined_Ieee_1164_Vector_Is_X =>
            --  Always false.
            return Create_Value_Discrete (0, Boolean_Type);
         when Iir_Predefined_Ieee_1164_To_Bitvector
           | Iir_Predefined_Ieee_1164_To_Stdlogicvector_Suv
           | Iir_Predefined_Ieee_1164_To_Stdlogicvector_Bv
           | Iir_Predefined_Ieee_Numeric_Std_To_01_Uns
           | Iir_Predefined_Ieee_Numeric_Std_To_01_Sgn =>
            if Is_Static (L.Val) then
               raise Internal_Error;
            end if;
            return Create_Value_Net (Get_Net (L), Create_Res_Bound (L));
         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns
           | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Int =>
            return Synth_Conv_Vector (False);
         when Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Int =>
            return Synth_Conv_Vector (True);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Conv_Integer =>
            --  UNSIGNED to Natural.
            return Create_Value_Net
              (Synth_Uresize (Get_Net (L), Res_Typ.W, Expr), Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Sgn_Int =>
            --  SIGNED to Integer.
            return Create_Value_Net
              (Synth_Sresize (L, Res_Typ.W, Expr), Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Nat =>
            declare
               W : Width;
            begin
               if not Is_Static (R.Val) then
                  Error_Msg_Synth (+Expr, "size must be constant");
                  return No_Valtyp;
               end if;
               W := Uns32 (Read_Discrete (R));
               return Create_Value_Net
                 (Synth_Uresize (Get_Net (L), W, Expr),
                  Create_Vec_Type_By_Length (W, Logic_Type));
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Nat =>
            declare
               W : Width;
            begin
               if not Is_Static (R.Val) then
                  Error_Msg_Synth (+Expr, "size must be constant");
                  return No_Valtyp;
               end if;
               W := Uns32 (Read_Discrete (R));
               return Create_Value_Net
                 (Build2_Sresize (Ctxt, Get_Net (L), W, Get_Location (Expr)),
                  Create_Vec_Type_By_Length (W, Logic_Type));
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Shl_Uns_Nat
           | Iir_Predefined_Ieee_Numeric_Std_Shl_Sgn_Nat =>
            return Synth_Shift_Rotate (Id_Lsl, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Shr_Uns_Nat =>
            return Synth_Shift_Rotate (Id_Lsr, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Shr_Sgn_Nat =>
            return Synth_Shift_Rotate (Id_Asr, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Rol_Uns_Nat =>
            return Synth_Shift_Rotate (Id_Rol, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Ror_Uns_Nat =>
            return Synth_Shift_Rotate (Id_Ror, L, R, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Uns =>
            return Synth_Dyadic_Uns_Uns (Id_Umin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Nat =>
            return Synth_Dyadic_Uns_Nat (Id_Umin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Nat_Uns =>
            return Synth_Dyadic_Nat_Uns (Id_Umin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Sgn =>
            return Synth_Dyadic_Sgn_Sgn (Id_Smin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Int =>
            return Synth_Dyadic_Sgn_Int (Id_Smin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Int_Sgn =>
            return Synth_Dyadic_Int_Sgn (Id_Smin, L, R, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Uns =>
            return Synth_Dyadic_Uns_Uns (Id_Umax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Nat =>
            return Synth_Dyadic_Uns_Nat (Id_Umax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Nat_Uns =>
            return Synth_Dyadic_Nat_Uns (Id_Umax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Sgn =>
            return Synth_Dyadic_Sgn_Sgn (Id_Smax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Int =>
            return Synth_Dyadic_Sgn_Int (Id_Smax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Int_Sgn =>
            return Synth_Dyadic_Int_Sgn (Id_Smax, L, R, Expr);

         when Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Slv
           | Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Suv =>
            return Create_Value_Net
              (Build_Reduce (Ctxt, Id_Red_Or, Get_Net (L)), Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Match_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Match_Slv =>
            declare
               Cst, Oper : Valtyp;
               Res : Net;
            begin
               if Is_Static (L.Val) then
                  Cst := L;
                  Oper := R;
               elsif Is_Static (R.Val) then
                  Cst := R;
                  Oper := L;
               else
                  Error_Msg_Synth
                    (+Expr, "one operand of std_match must be constant");
                  return No_Valtyp;
               end if;
               if Oper.Typ.W /= Cst.Typ.W then
                  Error_Msg_Synth
                    (+Expr, "operands of std_match don't have the same size");
                  return Create_Value_Discrete (0, Boolean_Type);
               end if;
               Res := Synth_Match (Cst, Oper, Expr);
               if Res = No_Net then
                  return Create_Value_Discrete (0, Boolean_Type);
               else
                  return Create_Value_Net (Res, Boolean_Type);
               end if;
            end;
         when others =>
            Error_Msg_Synth
              (+Expr,
               "unhandled function: " & Iir_Predefined_Functions'Image (Def));
            return No_Valtyp;
      end case;
   end Synth_Dynamic_Predefined_Function_Call;

   function Synth_Predefined_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Expr);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Inter : Node;
      Subprg_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
      Static : Boolean;
      Res : Valtyp;
   begin
      Areapools.Mark (M, Instance_Pool.all);
      Subprg_Inst := Make_Instance (Syn_Inst, Imp);

      Synth_Subprogram_Association
        (Subprg_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      --  If all operands are static, handle the call differently.
      Static := True;
      Inter := Inter_Chain;
      while Inter /= Null_Node loop
         if not Is_Static (Get_Value (Subprg_Inst, Inter).Val) then
            Static := False;
            exit;
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      if Static then
         Res := Synth_Static_Predefined_Function_Call (Subprg_Inst, Expr);
      else
         Res := Synth_Dynamic_Predefined_Function_Call (Subprg_Inst, Expr);
      end if;

      Free_Instance (Subprg_Inst);
      Areapools.Release (M, Instance_Pool.all);

      return Res;
   end Synth_Predefined_Function_Call;

   function Synth_Operator_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Assoc : Node;
      Inter : Node;
      Op1, Op2 : Node;
   begin
      Assoc := Get_Parameter_Association_Chain (Expr);
      Inter := Get_Interface_Declaration_Chain (Imp);

      Op1 := Get_Actual (Assoc);
      if Get_Chain (Inter) = Null_Node then
         return Synth_Monadic_Operation (Syn_Inst, Imp, Op1, Expr);
      else
         Op2 := Get_Actual (Get_Chain (Assoc));
         return Synth_Dyadic_Operation (Syn_Inst, Imp, Op1, Op2, Expr);
      end if;
   end Synth_Operator_Function_Call;
end Synth.Oper;
