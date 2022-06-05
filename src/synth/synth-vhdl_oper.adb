--  Operations synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;

with Synth.Errors; use Synth.Errors;
with Synth.Vhdl_Stmts; use Synth.Vhdl_Stmts;
with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Source;
with Synth.Vhdl_Eval; use Synth.Vhdl_Eval;
with Synth.Vhdl_Context; use Synth.Vhdl_Context;

package body Synth.Vhdl_Oper is
   procedure Set_Location (N : Net; Loc : Node)
     renames Standard.Synth.Source.Set_Location;

   function Synth_Uresize
     (Ctxt : Context_Acc; Val : Valtyp; W : Width; Loc : Node) return Net
   is
      Res : Net;
   begin
      if Is_Static (Val.Val) and then Val.Typ.Kind = Type_Discrete then
         if Val.Typ.Drange.Is_Signed and then Read_Discrete (Val) < 0 then
            --  TODO.
            raise Internal_Error;
         else
            Res := Build2_Const_Uns
              (Ctxt, To_Uns64 (Read_Discrete (Val)), W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      end if;
      return Build2_Uresize (Ctxt, Get_Net (Ctxt, Val), W, Get_Location (Loc));
   end Synth_Uresize;

   function Synth_Sresize
     (Ctxt : Context_Acc; Val : Valtyp; W : Width; Loc : Node) return Net
   is
      Res : Net;
   begin
      if Is_Static (Val.Val) and then Val.Typ.Kind = Type_Discrete then
         if Val.Typ.Drange.Is_Signed then
            Res := Build2_Const_Int (Ctxt, Read_Discrete (Val), W);
         else
            --  TODO.
            raise Internal_Error;
         end if;
         Set_Location (Res, Loc);
         return Res;
      end if;
      return Build2_Sresize (Ctxt, Get_Net (Ctxt, Val), W, Get_Location (Loc));
   end Synth_Sresize;

   function Synth_Resize (Ctxt : Context_Acc;
                          Val : Valtyp;
                          W : Width;
                          Sgn : Boolean;
                          Loc : Node) return Net is
   begin
      if Sgn then
         return Synth_Sresize (Ctxt, Val, W, Loc);
      else
         return Synth_Uresize (Ctxt, Val, W, Loc);
      end if;
   end Synth_Resize;

   function Synth_Bit_Eq_Const
     (Ctxt : Context_Acc; Cst : Valtyp; Expr : Valtyp; Loc : Node)
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
         N := Build_Const_UL32 (Ctxt, 0, 1, 1);
         Set_Location (N, Loc);
         return Create_Value_Net (N, Boolean_Type);
      elsif Val = 1 then
         --  The result type is a boolean; convert if needed.
         if Expr.Typ.Kind = Type_Logic then
            return Create_Value_Net (Get_Net (Ctxt, Expr), Boolean_Type);
         else
            pragma Assert (Expr.Typ.Kind = Type_Bit);
            return Expr;
         end if;
      else
         pragma Assert (Val = 0);
         N := Build_Monadic (Ctxt, Id_Not, Get_Net (Ctxt, Expr));
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

      case Res.Kind is
         when Type_Vector =>
            if Res.Abound.Dir = Dir_Downto
              and then Res.Abound.Right = 0
            then
               --  Normalized range
               return Res;
            end if;
            return Create_Vec_Type_By_Length (Res.W, Res.Arr_El);

         when Type_Slice =>
            return Create_Vec_Type_By_Length (Res.W, Res.Slice_El);

         when Type_Unbounded_Vector =>
            raise Internal_Error;

         when others =>
            raise Internal_Error;
      end case;
   end Create_Res_Bound;

   function Create_Bounds_From_Length
     (Syn_Inst : Synth_Instance_Acc; Atype : Iir; Len : Iir_Index32)
     return Bound_Type
   is
      Index_Bounds : Discrete_Range_Type;
   begin
      Synth_Discrete_Range (Syn_Inst, Atype, Index_Bounds);
      return Create_Bounds_From_Length (Index_Bounds, Len);
   end Create_Bounds_From_Length;

   --  Do a match comparison between CST and OPER.
   --  Return No_Net if CST has incorrect value.
   function Synth_Match (Ctxt : Context_Acc;
                         Cst : Valtyp;
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
      Nv := Build2_Const_Vec (Ctxt, Wd, Vals.all);
      Set_Location (Nv, Expr);
      Unchecked_Deallocate (Vals);
      Nm := Build2_Const_Vec (Ctxt, Wd, Mask.all);
      Set_Location (Nm, Expr);
      Unchecked_Deallocate (Mask);
      Res := Build_Dyadic (Ctxt, Id_And, Get_Net (Ctxt, Oper), Nm);
      Set_Location (Res, Expr);
      Res := Build_Compare (Ctxt, Op, Res, Nv);
      Set_Location (Res, Expr);

      return Res;
   end Synth_Match;

   --  Note: LEFT or RIGHT can be a single bit.
   function Synth_Dyadic_Xxx_Xxx (Ctxt : Context_Acc;
                                  Id : Dyadic_Module_Id;
                                  W : Width;
                                  Left, Right : Valtyp;
                                  Lsgn, Rsgn : Boolean;
                                  Expr : Node) return Valtyp
   is
      El_Typ : Type_Acc;
      Rtype : Type_Acc;
      L1, R1 : Net;
      N : Net;
   begin
      --  Note: LEFT or RIGHT can be a single bit.
      if Left.Typ.Kind = Type_Vector then
         El_Typ := Left.Typ.Arr_El;
      elsif Right.Typ.Kind = Type_Vector then
         El_Typ := Right.Typ.Arr_El;
      else
         raise Internal_Error;
      end if;
      Rtype := Create_Vec_Type_By_Length (W, El_Typ);

      L1 := Synth_Resize (Ctxt, Left, W, Lsgn, Expr);
      R1 := Synth_Resize (Ctxt, Right, W, Rsgn, Expr);
      N := Build_Dyadic (Ctxt, Id, L1, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Rtype);
   end Synth_Dyadic_Xxx_Xxx;

   function Synth_Dyadic_Uns_Uns (Ctxt : Context_Acc;
                                  Id : Dyadic_Module_Id;
                                  Left, Right : Valtyp;
                                  Expr : Node) return Valtyp is
   begin
      return Synth_Dyadic_Xxx_Xxx
        (Ctxt, Id, Width'Max (Left.Typ.W, Right.Typ.W),
         Left, Right, False, False, Expr);
   end Synth_Dyadic_Uns_Uns;

   function Synth_Dyadic_Sgn_Sgn (Ctxt : Context_Acc;
                                  Id : Dyadic_Module_Id;
                                  Left, Right : Valtyp;
                                  Expr : Node) return Valtyp is
   begin
      return Synth_Dyadic_Xxx_Xxx
        (Ctxt, Id, Width'Max (Left.Typ.W, Right.Typ.W),
         Left, Right, True, True, Expr);
   end Synth_Dyadic_Sgn_Sgn;

   --  For std_logic_arith
   function Synth_Dyadic_Uns_Sgn_Sgn (Ctxt : Context_Acc;
                                      Id : Dyadic_Module_Id;
                                      Left, Right : Valtyp;
                                      Expr : Node) return Valtyp is
   begin
      return Synth_Dyadic_Xxx_Xxx
        (Ctxt, Id, Width'Max (Left.Typ.W + 1, Right.Typ.W),
         Left, Right, False, True, Expr);
   end Synth_Dyadic_Uns_Sgn_Sgn;

   --  For std_logic_arith
   function Synth_Dyadic_Sgn_Uns_Sgn (Ctxt : Context_Acc;
                                      Id : Dyadic_Module_Id;
                                      Left, Right : Valtyp;
                                      Expr : Node) return Valtyp is
   begin
      return Synth_Dyadic_Xxx_Xxx
        (Ctxt, Id, Width'Max (Left.Typ.W, Right.Typ.W + 1),
         Left, Right, True, False, Expr);
   end Synth_Dyadic_Sgn_Uns_Sgn;

   function Synth_Dyadic_Uns_Nat (Ctxt : Context_Acc;
                                  Id : Dyadic_Module_Id;
                                  Left, Right : Valtyp;
                                  Expr : Node) return Valtyp
   is
      L : constant Net := Get_Net (Ctxt, Left);
      R1 : Net;
      N : Net;
   begin
      R1 := Synth_Uresize (Ctxt, Right, Left.Typ.W, Expr);
      N := Build_Dyadic (Ctxt, Id, L, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Left));
   end Synth_Dyadic_Uns_Nat;

   function Synth_Dyadic_Nat_Uns (Ctxt : Context_Acc;
                                  Id : Dyadic_Module_Id;
                                  Left, Right : Valtyp;
                                  Expr : Node) return Valtyp
   is
      R : constant Net := Get_Net (Ctxt, Right);
      L1 : Net;
      N : Net;
   begin
      L1 := Synth_Uresize (Ctxt, Left, Right.Typ.W, Expr);
      N := Build_Dyadic (Ctxt, Id, L1, R);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Right));
   end Synth_Dyadic_Nat_Uns;

   function Synth_Dyadic_Sgn_Int (Ctxt : Context_Acc;
                                  Id : Dyadic_Module_Id;
                                  Left, Right : Valtyp;
                                  Expr : Node) return Valtyp
   is
      L : constant Net := Get_Net (Ctxt, Left);
      R1 : Net;
      N : Net;
   begin
      R1 := Synth_Sresize (Ctxt, Right, Left.Typ.W, Expr);
      N := Build_Dyadic (Ctxt, Id, L, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Left));
   end Synth_Dyadic_Sgn_Int;

   function Synth_Dyadic_Int_Sgn (Ctxt : Context_Acc;
                                  Id : Dyadic_Module_Id;
                                  Left, Right : Valtyp;
                                  Expr : Node) return Valtyp
   is
      R : constant Net := Get_Net (Ctxt, Right);
      L1 : Net;
      N : Net;
   begin
      L1 := Synth_Sresize (Ctxt, Left, Right.Typ.W, Expr);
      N := Build_Dyadic (Ctxt, Id, R, L1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Right));
   end Synth_Dyadic_Int_Sgn;

   function Synth_Dyadic_Vec_Log (Ctxt : Context_Acc;
                                  Id   : Dyadic_Module_Id;
                                  Vec, Log : Valtyp;
                                  Expr     : Node) return Valtyp
   is
      V : constant Net := Get_Net (Ctxt, Vec);
      L : constant Net := Get_Net (Ctxt, Log);
      Wd : constant Width := Get_Width (V);
      Lv : Net;
      Res : Net;
   begin
      Lv := Build2_Sresize (Ctxt, L, Wd, Get_Location (Expr));
      Res := Build_Dyadic (Ctxt, Id, V, Lv);
      Set_Location (Res, Expr);
      return Create_Value_Net (Res, Create_Res_Bound (Vec));
   end Synth_Dyadic_Vec_Log;

   function Synth_Compare_Xxx_Xxx (Ctxt : Context_Acc;
                                   Id : Compare_Module_Id;
                                   W : Width;
                                   Left, Right : Valtyp;
                                   Lsgn, Rsgn : Boolean;
                                   Res_Typ : Type_Acc;
                                   Expr : Node) return Valtyp
   is
      L1, R1 : Net;
      N : Net;
   begin
      L1 := Synth_Resize (Ctxt, Left, W, Lsgn, Expr);
      R1 := Synth_Resize (Ctxt, Right, W, Rsgn, Expr);
      N := Build2_Compare (Ctxt, Id, L1, R1);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Res_Typ);
   end Synth_Compare_Xxx_Xxx;

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
      Srec : Memtyp;
      Left : Valtyp;
      Right : Valtyp;

      function Synth_Bit_Dyadic (Id : Dyadic_Module_Id) return Valtyp
      is
         N : Net;
      begin
         N := Build_Dyadic
           (Ctxt, Id, Get_Net (Ctxt, Left), Get_Net (Ctxt, Right));
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
         N := Build2_Compare
           (Ctxt, Id, Get_Net (Ctxt, Left), Get_Net (Ctxt, Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Res_Type);
      end Synth_Compare;

      function Synth_Compare_Array (Id : Compare_Module_Id;
                                    Res_Type : Type_Acc) return Valtyp
      is
         Ln, Rn, N : Net;
      begin
         if Left.Typ.Kind = Type_Vector then
            Ln := Get_Net (Ctxt, Left);
            Rn := Get_Net (Ctxt, Right);
            Warning_Msg_Synth
              (+Expr, "comparing non-numeric vector is unexpected");
            if Left.Typ.W = Right.Typ.W then
               N := Build2_Compare (Ctxt, Id, Ln, Rn);
            elsif Left.Typ.W < Right.Typ.W then
               --  Truncate right.
               Rn := Build_Extract
                 (Ctxt, Rn, Right.Typ.W - Left.Typ.W, Left.Typ.W);
               --  Because it has been truncated, it cannot be equal.
               if Id = Id_Ule then
                  N := Build2_Compare (Ctxt, Id_Ult, Ln, Rn);
               else
                  raise Internal_Error;
               end if;
            else
               --  TODO: truncate left, compare using id.
               raise Internal_Error;
            end if;
            Set_Location (N, Expr);
            return Create_Value_Net (N, Res_Type);
         else
            raise Internal_Error;
         end if;
      end Synth_Compare_Array;

      function Synth_Compare_Uns_Uns
        (Id : Compare_Module_Id; Res_Type : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Width'Max (Left.Typ.W, Right.Typ.W),
            Left, Right, False, False, Res_Type, Expr);
      end Synth_Compare_Uns_Uns;

      function Synth_Compare_Sgn_Sgn
        (Id : Compare_Module_Id; Res_Type : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Width'Max (Left.Typ.W, Right.Typ.W),
            Left, Right, True, True, Res_Type, Expr);
      end Synth_Compare_Sgn_Sgn;

      --  For std_logic_arith
      function Synth_Compare_Uns_Sgn
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Width'Max (Left.Typ.W + 1, Right.Typ.W),
            Left, Right, False, True, Res_Typ, Expr);
      end Synth_Compare_Uns_Sgn;

      --  For std_logic_arith
      function Synth_Compare_Sgn_Uns
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Width'Max (Left.Typ.W, Right.Typ.W + 1),
            Left, Right, True, False, Res_Typ, Expr);
      end Synth_Compare_Sgn_Uns;

      function Synth_Compare_Uns_Nat
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Left.Typ.W,
            Left, Right, False, False, Res_Typ, Expr);
      end Synth_Compare_Uns_Nat;

      function Synth_Compare_Nat_Uns
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Right.Typ.W,
            Left, Right, False, False, Res_Typ, Expr);
      end Synth_Compare_Nat_Uns;

      function Synth_Compare_Sgn_Int
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Left.Typ.W,
            Left, Right, True, True, Res_Typ, Expr);
      end Synth_Compare_Sgn_Int;

      function Synth_Compare_Int_Sgn
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Right.Typ.W,
            Left, Right, True, True, Res_Typ, Expr);
      end Synth_Compare_Int_Sgn;

      --  For std_logic_arith
      function Synth_Compare_Uns_Int
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Left.Typ.W + 1,
            Left, Right, False, True, Res_Typ, Expr);
      end Synth_Compare_Uns_Int;

      --  For std_logic_arith
      function Synth_Compare_Int_Uns
        (Id : Compare_Module_Id; Res_Typ : Type_Acc) return Valtyp is
      begin
         return Synth_Compare_Xxx_Xxx
           (Ctxt, Id, Right.Typ.W + 1,
            Left, Right, True, False, Res_Typ, Expr);
      end Synth_Compare_Int_Uns;

      function Synth_Vec_Dyadic (Id : Dyadic_Module_Id) return Valtyp
      is
         N : Net;
      begin
         if Left.Typ.W /= Right.Typ.W then
            Error_Msg_Synth (+Expr, "operands don't have the same length");
            return No_Valtyp;
         end if;
         N := Build_Dyadic (Ctxt, Id,
                            Get_Net (Ctxt, Left), Get_Net (Ctxt, Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Create_Res_Bound (Left));
      end Synth_Vec_Dyadic;

      function Synth_Int_Dyadic (Id : Dyadic_Module_Id) return Valtyp
      is
         Etype : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Expr_Type);
         N : Net;
      begin
         N := Build_Dyadic
           (Ctxt, Id, Get_Net (Ctxt, Left), Get_Net (Ctxt, Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Etype);
      end Synth_Int_Dyadic;

      type Oper_Kind is (Oper_Left, Oper_Right);

      function Synth_Udivmod (Id : Dyadic_Module_Id; Vec : Oper_Kind)
                            return Valtyp
      is
         W : constant Width := Width'Max (Left.Typ.W, Right.Typ.W);
         L1, R1 : Net;
         Res_Typ : Type_Acc;
         N : Net;
      begin
         L1 := Synth_Uresize (Ctxt, Left, W, Expr);
         R1 := Synth_Uresize (Ctxt, Right, W, Expr);
         case Vec is
            when Oper_Left =>
               Res_Typ := Left.Typ;
            when Oper_Right =>
               Res_Typ := Right.Typ;
         end case;
         Res_Typ := Create_Vec_Type_By_Length (Res_Typ.W, Res_Typ.Arr_El);
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
         L1 := Synth_Sresize (Ctxt, Left, W, Expr);
         R1 := Synth_Sresize (Ctxt, Right, W, Expr);
         case Vec is
            when Oper_Left =>
               Res_Typ := Left.Typ;
            when Oper_Right =>
               Res_Typ := Right.Typ;
         end case;
         Res_Typ := Create_Vec_Type_By_Length (Res_Typ.W, Res_Typ.Arr_El);
         N := Build_Dyadic (Ctxt, Id, L1, R1);
         Set_Location (N, Expr);
         N := Build2_Sresize (Ctxt, N, Res_Typ.W, Get_Location (Expr));
         return Create_Value_Net (N, Res_Typ);
      end Synth_Sdivmod;

      function Synth_Shift (Sh_Pos : Module_Id; Sh_Neg : Module_Id)
                           return Valtyp
      is
         L1, R1 : Net;
         N, Nn, Nr1, Cond : Net;
      begin
         L1 := Get_Net (Ctxt, Left);
         R1 := Get_Net (Ctxt, Right);

         --  Handle the case when the RHS is positive.
         N := Build_Shift_Rotate (Ctxt, Sh_Pos, L1, R1);
         Set_Location (N, Expr);

         if not Is_Positive (Right) then
            --  If we cannot trivially prove that the RHS is positive, also
            --  handle the case when it could be negative.
            --  At worst, the optimizer will remove that part.
            Nr1 := Build_Monadic (Ctxt, Id_Neg, R1);
            Set_Location (Nr1, Expr);
            Nn := Build_Shift_Rotate (Ctxt, Sh_Neg, L1, Nr1);
            Set_Location (Nn, Expr);

            --  Extract the sign bit.
            Cond := Build_Extract (Ctxt, R1, Get_Width (R1) - 1, 1);
            Set_Location (Cond, Expr);

            N := Build_Mux2 (Ctxt, Cond, N, Nn);
            Set_Location (N, Expr);
         end if;

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
            R1 := Get_Net (Ctxt, Right);
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
         L1 := Get_Net (Ctxt, Left);
         N := Build_Shift_Rotate (Ctxt, Id, L1, R1);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Create_Res_Bound (Left));
      end Synth_Rotation;
   begin
      Left := Synth_Expression_With_Type (Syn_Inst, Left_Expr, Left_Typ);
      if Left = No_Valtyp then
         return No_Valtyp;
      end if;
      Left := Synth_Subtype_Conversion (Ctxt, Left, Left_Typ, False, Expr);
      Right := Synth_Expression_With_Type (Syn_Inst, Right_Expr, Right_Typ);
      if Right = No_Valtyp then
         return No_Valtyp;
      end if;
      Right := Synth_Subtype_Conversion (Ctxt, Right, Right_Typ, False, Expr);

      if Is_Static_Val (Left.Val) and Is_Static_Val (Right.Val) then
         Srec := Eval_Static_Dyadic_Predefined
           (Imp, Expr_Typ,
            Get_Value_Memtyp (Left), Get_Value_Memtyp (Right), Expr);
         if Srec = Null_Memtyp then
            return No_Valtyp;
         end if;
         return Create_Value_Memtyp (Srec);
      end if;

      Strip_Const (Left);
      Strip_Const (Right);

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
           | Iir_Predefined_Boolean_Nor
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

         when Iir_Predefined_TF_Array_And
            | Iir_Predefined_Ieee_1164_Vector_And
            | Iir_Predefined_Ieee_Numeric_Std_And_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_And);
         when Iir_Predefined_TF_Array_Or
            | Iir_Predefined_Ieee_1164_Vector_Or
            | Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Or);
         when Iir_Predefined_TF_Array_Nand
            | Iir_Predefined_Ieee_1164_Vector_Nand
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Nand);
         when Iir_Predefined_TF_Array_Nor
            | Iir_Predefined_Ieee_1164_Vector_Nor
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Nor);
         when Iir_Predefined_TF_Array_Xor
            | Iir_Predefined_Ieee_1164_Vector_Xor
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Xor);
         when Iir_Predefined_TF_Array_Xnor
            | Iir_Predefined_Ieee_1164_Vector_Xnor
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Xnor);

         when Iir_Predefined_Ieee_1164_And_Suv_Log =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_And, Left, Right, Expr);
         when Iir_Predefined_Ieee_1164_And_Log_Suv =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_And, Right, Left, Expr);
         when Iir_Predefined_Ieee_1164_Nand_Suv_Log =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Nand, Left, Right, Expr);
         when Iir_Predefined_Ieee_1164_Nand_Log_Suv =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Nand, Right, Left, Expr);
         when Iir_Predefined_Ieee_1164_Or_Suv_Log =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Or, Left, Right, Expr);
         when Iir_Predefined_Ieee_1164_Or_Log_Suv =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Or, Right, Left, Expr);
         when Iir_Predefined_Ieee_1164_Nor_Suv_Log =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Nor, Left, Right, Expr);
         when Iir_Predefined_Ieee_1164_Nor_Log_Suv =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Nor, Right, Left, Expr);
         when Iir_Predefined_Ieee_1164_Xor_Suv_Log =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Xor, Left, Right, Expr);
         when Iir_Predefined_Ieee_1164_Xor_Log_Suv =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Xor, Right, Left, Expr);
         when Iir_Predefined_Ieee_1164_Xnor_Suv_Log =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Xnor, Left, Right, Expr);
         when Iir_Predefined_Ieee_1164_Xnor_Log_Suv =>
            return Synth_Dyadic_Vec_Log (Ctxt, Id_Xnor, Right, Left, Expr);

         when Iir_Predefined_Enum_Equality =>
            if Left_Typ = Bit_Type
              or else Left_Typ = Logic_Type
            then
               if Is_Static (Left.Val) then
                  return Synth_Bit_Eq_Const (Ctxt, Left, Right, Expr);
               elsif Is_Static (Right.Val) then
                  return Synth_Bit_Eq_Const (Ctxt, Right, Left, Expr);
               end if;
            end if;
            return Synth_Compare (Id_Eq, Boolean_Type);
         when Iir_Predefined_Enum_Inequality =>
            --  TODO: Optimize ?
            return Synth_Compare (Id_Ne, Boolean_Type);
         when Iir_Predefined_Enum_Less_Equal =>
            return Synth_Compare (Id_Ule, Boolean_Type);
         when Iir_Predefined_Enum_Less =>
            return Synth_Compare (Id_Ult, Boolean_Type);
         when Iir_Predefined_Enum_Greater_Equal =>
            return Synth_Compare (Id_Uge, Boolean_Type);
         when Iir_Predefined_Enum_Greater =>
            return Synth_Compare (Id_Ugt, Boolean_Type);

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
               Res := Synth_Match (Ctxt, Cst, Oper, Expr);
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
               Res := Synth_Match (Ctxt, Cst, Oper, Expr, Id_Ne);
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
            return Synth_Compare_Array (Id_Ugt, Boolean_Type);
         when Iir_Predefined_Array_Greater_Equal =>
            return Synth_Compare_Array (Id_Uge, Boolean_Type);
         when Iir_Predefined_Array_Less =>
            return Synth_Compare_Array (Id_Ult, Boolean_Type);
         when Iir_Predefined_Array_Less_Equal =>
            return Synth_Compare_Array (Id_Ule, Boolean_Type);

         when Iir_Predefined_Array_Element_Concat =>
            declare
               L : constant Net := Get_Net (Ctxt, Left);
               Le_Typ : constant Type_Acc := Get_Array_Element (Left.Typ);
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               N : Net;
            begin
               Check_Matching_Bounds (Le_Typ, Right.Typ, Expr);
               N := Build2_Concat2 (Ctxt, L, Get_Net (Ctxt, Right));
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Bound_Length (Left.Typ) + 1));

               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Left_Typ, Bnd, Le_Typ);
               return Create_Value_Net (N, Res_Typ);
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               R : constant Net := Get_Net (Ctxt, Right);
               Re_Typ : constant Type_Acc := Get_Array_Element (Right.Typ);
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               N : Net;
            begin
               Check_Matching_Bounds (Left.Typ, Re_Typ, Expr);
               N := Build2_Concat2 (Ctxt, Get_Net (Ctxt, Left), R);
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Bound_Length (Right.Typ) + 1));

               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Right_Typ, Bnd, Re_Typ);
               return Create_Value_Net (N, Res_Typ);
            end;
         when Iir_Predefined_Element_Element_Concat =>
            declare
               N : Net;
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
            begin
               Check_Matching_Bounds (Left.Typ, Right.Typ, Expr);
               N := Build2_Concat2
                 (Ctxt, Get_Net (Ctxt, Left), Get_Net (Ctxt, Right));
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), 2);
               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Expr_Typ, Bnd, Left.Typ);
               return Create_Value_Net (N, Res_Typ);
            end;
         when Iir_Predefined_Array_Array_Concat =>
            declare
               Le_Typ : constant Type_Acc := Get_Array_Element (Left.Typ);
               Re_Typ : constant Type_Acc := Get_Array_Element (Right.Typ);
               L : constant Net := Get_Net (Ctxt, Left);
               R : constant Net := Get_Net (Ctxt, Right);
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               N : Net;
            begin
               Check_Matching_Bounds (Le_Typ, Re_Typ, Expr);
               N := Build2_Concat2 (Ctxt, L, R);
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Bound_Length (Left.Typ)
                                 + Get_Bound_Length (Right.Typ)));

               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Expr_Typ, Bnd, Le_Typ);
               return Create_Value_Net (N, Res_Typ);
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
                     N := Get_Net (Ctxt, Left);
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
         when Iir_Predefined_Physical_Physical_Div =>
            Error_Msg_Synth (+Expr, "non-constant division not supported");
            return No_Valtyp;

         when Iir_Predefined_Floating_Div =>
            Error_Msg_Synth (+Expr, "non-constant division not supported");
            return No_Valtyp;

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Log
            | Iir_Predefined_Ieee_Numeric_Std_Add_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Log
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Log
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv =>
            --  "+" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns_Uns (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat =>
            --  "+" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Int =>
            --  "+" (Unsigned, Integer)
            return Synth_Dyadic_Sgn_Int (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Int_Slv =>
            --  "+" (Natural, Unsigned)
            return Synth_Dyadic_Nat_Uns (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv =>
            --  "+" (Signed, Integer)
            return Synth_Dyadic_Sgn_Int (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Int_Slv =>
            --  "+" (Integer, Signed)
            return Synth_Dyadic_Int_Sgn (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Slv =>
            --  "+" (Signed, Signed)
            return Synth_Dyadic_Sgn_Sgn (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv =>
            --  "+" (Unsigned, Signed)
            return Synth_Dyadic_Uns_Sgn_Sgn (Ctxt, Id_Add, Left, Right, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv =>
            --  "+" (Signed, Unsigned)
            return Synth_Dyadic_Sgn_Uns_Sgn (Ctxt, Id_Add, Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Log
            | Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Log
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Log
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv =>
            --  "-" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns_Uns (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Slv =>
            --  "-" (Signed, Signed)
            return Synth_Dyadic_Sgn_Sgn (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat =>
            --  "-" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Int =>
            --  "-" (Unsigned, Integer)
            return Synth_Dyadic_Sgn_Int (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Int_Slv =>
            --  "-" (Natural, Unsigned)
            return Synth_Dyadic_Nat_Uns (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Int =>
            --  "-" (Signed, Integer)
            return Synth_Dyadic_Sgn_Int (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Int_Slv =>
            --  "-" (Integer, Signed)
            return Synth_Dyadic_Int_Sgn (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv =>
            --  "-" (Unsigned, Signed)
            return Synth_Dyadic_Uns_Sgn_Sgn (Ctxt, Id_Sub, Left, Right, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv =>
            --  "-" (Signed, Unsigned)
            return Synth_Dyadic_Sgn_Uns_Sgn (Ctxt, Id_Sub, Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv
           | Iir_Predefined_Ieee_Std_Logic_Signed_Mul_Slv_Slv =>
            --  "*" (Signed, Signed)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, Left.Typ.W + Right.Typ.W,
               Left, Right, True, True, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Int =>
            --  "*" (Signed, Integer)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, 2 * Left.Typ.W,
               Left, Right, True, True, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Int_Sgn =>
            --  "*" (Integer, Signed)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, 2 * Right.Typ.W,
               Left, Right, True, True, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Mul_Slv_Slv =>
            --  "*" (unsigned, unsigned)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, Left.Typ.W + Right.Typ.W,
               Left, Right, False, False, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Nat =>
            --  "*" (unsigned, natural)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, 2 * Left.Typ.W,
               Left, Right, False, False, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Nat_Uns =>
            --  "*" (natural, unsigned)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, 2 * Right.Typ.W,
               Left, Right, False, False, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv =>
            --  "*" (unsigned, signed)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, Left.Typ.W + 1 + Right.Typ.W,
               Left, Right, False, True, Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv =>
            --  "*" (signed, unsigned)
            return Synth_Dyadic_Xxx_Xxx
              (Ctxt, Id_Smul, Left.Typ.W + Right.Typ.W + 1,
               Left, Right, True, False, Expr);

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
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Uns =>
            --  "=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Int =>
            --  "=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Int_Slv =>
            --  "=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Int
            | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Slv_Int =>
            --  "=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Slv_Slv =>
            --  "=" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Int_Slv =>
            --  "=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Uns =>
            --  "=" (Integer, Unsigned)
            return Synth_Compare_Int_Uns (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Int =>
            --  "=" (Unsigned, Integer)
            return Synth_Compare_Uns_Int (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Sgn =>
            --  "=" (Unsigned, Signed)
            return Synth_Compare_Uns_Sgn (Id_Eq, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Uns =>
            --  "=" (Signed, Unsigned)
            return Synth_Compare_Sgn_Uns (Id_Eq, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Uns =>
            --  "/=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Nat
           | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Nat =>
            --  "/=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Int_Slv =>
            --  "/=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Slv_Slv =>
            --  "/=" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Int
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Slv_Int =>
            --  "/=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Int_Slv =>
            --  "/=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Uns =>
            --  "/=" (Integer, Unsigned)
            return Synth_Compare_Int_Uns (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Int
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Int =>
            --  "/=" (Unsigned, Integer)
            return Synth_Compare_Uns_Int (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Sgn =>
            --  "/=" (Unsigned, Signed)
            return Synth_Compare_Uns_Sgn (Id_Ne, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Uns =>
            --  "/=" (Signed, Unsigned)
            return Synth_Compare_Sgn_Uns (Id_Ne, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Uns =>
            --  "<" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ult, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Int =>
            --  "<" (Unsigned, Natural)
            if Is_Static (Right.Val) and then Read_Discrete (Right) = 0 then
               --  Always false.
               return Create_Value_Discrete (0, Expr_Typ);
            end if;
            return Synth_Compare_Uns_Nat (Id_Ult, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Int_Slv =>
            --  "<" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ult, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Slv_Slv =>
            --  "<" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Int
            | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Slv_Int =>
            --  "<" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Int_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Int_Slv =>
            --  "<" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Uns =>
            --  "<" (Integer, Unsigned)
            return Synth_Compare_Int_Uns (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Int =>
            --  "<" (Unsigned, Integer)
            return Synth_Compare_Uns_Int (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Sgn =>
            --  "<" (Unsigned, Signed)
            return Synth_Compare_Uns_Sgn (Id_Slt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Uns =>
            --  "<" (Signed, Unsigned)
            return Synth_Compare_Sgn_Uns (Id_Slt, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Slv
            | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Uns =>
            --  "<=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ule, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Le_Slv_Slv =>
            --  "<=" (Signed, Signed)
            return Synth_Compare_Sgn_Sgn (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Int =>
            --  "<=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ule, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Int_Slv =>
            --  "<=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ule, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Int
            | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Le_Slv_Int =>
            --  "<=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Le_Int_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Le_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Le_Int_Slv =>
            --  "<=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Uns =>
            --  "<=" (Integer, Unsigned)
            return Synth_Compare_Int_Uns (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Int =>
            --  "<=" (Unsigned, Integer)
            return Synth_Compare_Uns_Int (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Sgn =>
            --  "<=" (Unsigned, Signed)
            return Synth_Compare_Uns_Sgn (Id_Sle, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Uns =>
            --  "<=" (Signed, Unsigned)
            return Synth_Compare_Sgn_Uns (Id_Sle, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Uns =>
            --  ">" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ugt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Int =>
            --  ">" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ugt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Int_Slv =>
            --  ">" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Ugt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Slv_Slv =>
            --  ">" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Int
            | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Slv_Int =>
            --  ">" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Int_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Int_Slv =>
            --  ">" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Uns =>
            --  ">" (Integer, Unsigned)
            return Synth_Compare_Int_Uns (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Int =>
            --  ">" (Unsigned, Integer)
            return Synth_Compare_Uns_Int (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Sgn =>
            --  ">" (Unsigned, Signed)
            return Synth_Compare_Uns_Sgn (Id_Sgt, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Uns =>
            --  ">" (Signed, Unsigned)
            return Synth_Compare_Sgn_Uns (Id_Sgt, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Uns =>
            --  ">=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Uge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Int_Slv =>
            --  ">=" (Natural, Unsigned) [resize]
            return Synth_Compare_Nat_Uns (Id_Uge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Int =>
            --  ">=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Uge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Slv_Slv =>
            --  ">=" (Signed, Signed) [resize]
            return Synth_Compare_Sgn_Sgn (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Int
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Slv_Int =>
            --  ">=" (Signed, Integer)
            return Synth_Compare_Sgn_Int (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Int_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Int_Slv =>
            --  ">=" (Integer, Signed)
            return Synth_Compare_Int_Sgn (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Uns =>
            --  ">=" (Integer, Unsigned)
            return Synth_Compare_Int_Uns (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Int =>
            --  ">=" (Unsigned, Integer)
            return Synth_Compare_Uns_Int (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Sgn =>
            --  ">=" (Unsigned, Signed)
            return Synth_Compare_Uns_Sgn (Id_Sge, Expr_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Uns =>
            --  ">=" (Signed, Unsigned)
            return Synth_Compare_Sgn_Uns (Id_Sge, Expr_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Sra_Sgn_Int =>
            return Synth_Shift (Id_Asr, Id_Lsl);

         when Iir_Predefined_Ieee_Numeric_Std_Sll_Uns_Int
            | Iir_Predefined_Ieee_Numeric_Std_Sll_Sgn_Int
            | Iir_Predefined_Ieee_1164_Vector_Sll =>
            return Synth_Shift (Id_Lsl, Id_Lsr);

         when Iir_Predefined_Ieee_Numeric_Std_Srl_Uns_Int
            | Iir_Predefined_Ieee_Numeric_Std_Srl_Sgn_Int
            | Iir_Predefined_Ieee_1164_Vector_Srl =>
            return Synth_Shift (Id_Lsr, Id_Lsl);

         when Iir_Predefined_Ieee_Numeric_Std_Ror_Uns_Int
            | Iir_Predefined_Ieee_Numeric_Std_Ror_Sgn_Int
            | Iir_Predefined_Ieee_1164_Vector_Ror =>
            return Synth_Rotation (Id_Ror);

         when Iir_Predefined_Ieee_Numeric_Std_Rol_Uns_Int
            | Iir_Predefined_Ieee_Numeric_Std_Rol_Sgn_Int
            | Iir_Predefined_Ieee_1164_Vector_Rol =>
            return Synth_Rotation (Id_Rol);

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
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
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
         N := Build_Monadic (Ctxt, Id, Get_Net (Ctxt, Operand));
         Set_Location (N, Loc);
         return Create_Value_Net (N, Operand.Typ);
      end Synth_Bit_Monadic;

      function Synth_Vec_Monadic (Id : Monadic_Module_Id) return Valtyp
      is
         Op: constant Net := Get_Net (Ctxt, Operand);
         N : Net;
      begin
         N := Build_Monadic (Ctxt, Id, Op);
         Set_Location (N, Loc);
         return Create_Value_Net (N, Create_Res_Bound (Operand));
      end Synth_Vec_Monadic;

      function Synth_Vec_Reduce_Monadic
        (Id : Reduce_Module_Id; Neg : Boolean := False) return Valtyp
      is
         Op: constant Net := Get_Net (Ctxt, Operand);
         N : Net;
      begin
         N := Build_Reduce (Ctxt, Id, Op);
         Set_Location (N, Loc);
         if Neg then
            N := Build_Monadic (Ctxt, Id_Not, N);
            Set_Location (N, Loc);
         end if;
         return Create_Value_Net (N, Operand.Typ.Arr_El);
      end Synth_Vec_Reduce_Monadic;
   begin
      Operand := Synth_Expression_With_Type (Syn_Inst, Operand_Expr, Oper_Typ);
      if Operand = No_Valtyp then
         return No_Valtyp;
      end if;
      Operand := Synth_Subtype_Conversion
        (Ctxt, Operand, Oper_Typ, False, Loc);
      Strip_Const (Operand);

      if Is_Static_Val (Operand.Val) then
         return Create_Value_Memtyp
           (Eval_Static_Monadic_Predefined
              (Imp, Get_Value_Memtyp (Operand), Loc));
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
            | Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Neg_Slv =>
            return Synth_Vec_Monadic (Id_Neg);
         when Iir_Predefined_Ieee_Numeric_Std_Abs_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Abs_Slv =>
            return Synth_Vec_Monadic (Id_Abs);

         when Iir_Predefined_Ieee_1164_And_Suv
            | Iir_Predefined_Ieee_Numeric_Std_And_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_And_Uns =>
            return Synth_Vec_Reduce_Monadic (Id_Red_And);
         when Iir_Predefined_Ieee_1164_Nand_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Uns =>
            return Synth_Vec_Reduce_Monadic (Id_Red_And, True);
         when Iir_Predefined_Ieee_1164_Or_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Or_Uns =>
            return Synth_Vec_Reduce_Monadic (Id_Red_Or);
         when Iir_Predefined_Ieee_1164_Nor_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Uns =>
            return Synth_Vec_Reduce_Monadic (Id_Red_Or, True);
         when Iir_Predefined_Ieee_1164_Xor_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns =>
            return Synth_Vec_Reduce_Monadic (Id_Red_Xor);
         when Iir_Predefined_Ieee_1164_Xnor_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns =>
            return Synth_Vec_Reduce_Monadic (Id_Red_Xor, True);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Id_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Id_Slv =>
            --  Unary "+": nop
            return Create_Value_Net (Get_Net (Ctxt, Operand),
                                     Create_Res_Bound (Operand));

         when Iir_Predefined_Ieee_1164_Condition_Operator
            | Iir_Predefined_Bit_Condition =>
            return Create_Value_Net
              (Get_Net (Ctxt, Operand),
               Get_Subtype_Object (Syn_Inst, Get_Type (Imp)));
         when Iir_Predefined_Integer_Negation =>
            declare
               N : Net;
            begin
               N := Build_Monadic (Ctxt, Id_Neg, Get_Net (Ctxt, Operand));
               Set_Location (N, Loc);
               return Create_Value_Net (N, Operand.Typ);
            end;

         when others =>
            Error_Msg_Synth
              (+Loc,
               "unhandled monadic: " & Iir_Predefined_Functions'Image (Def));
            return No_Valtyp;
      end case;
   end Synth_Monadic_Operation;

   function Synth_Shift_Rotate (Ctxt : Context_Acc;
                                Id : Shift_Rotate_Module_Id;
                                Left, Right : Valtyp;
                                Expr : Node) return Valtyp
   is
      L : constant Net := Get_Net (Ctxt, Left);
      N : Net;
   begin
      N := Build_Shift_Rotate (Ctxt, Id, L, Get_Net (Ctxt, Right));
      Set_Location (N, Expr);
      return Create_Value_Net (N, Create_Res_Bound (Left));
   end Synth_Shift_Rotate;

   function Synth_Find_Bit (Ctxt : Context_Acc;
                            Left, Right : Valtyp;
                            Res_Typ     : Type_Acc;
                            Leftmost    : Boolean;
                            Expr        : Node) return Valtyp
   is
      pragma Assert (Left.Typ.Kind = Type_Vector);
      Len : constant Uns32 := Left.Typ.Abound.Len;
      Max : Int32;
      Rng : Discrete_Range_Type;
      W   : Uns32;
      Typ : Type_Acc;
      R_Net : Net;
      L_Net : Net;
      Res : Net;
   begin
      if Len = 0 then
         return Create_Value_Int (-1, Res_Typ);
      end if;

      --  The intermediate result is computed using the least number of bits,
      --  which must represent all positive values in the bounds using a
      --  signed word (so that -1 is also represented).
      Max := Int32'Max (Left.Typ.Abound.Left, Left.Typ.Abound.Right);
      W := Netlists.Utils.Clog2 (Uns32 (Max)) + 1;
      Rng := (Dir => Dir_To,
              Is_Signed => True,
              Left => -1,
              Right => Int64 (Max));
      Typ := Create_Discrete_Type (Rng, Res_Typ.Sz, W);

      R_Net := Get_Net (Ctxt, Right);
      L_Net := Get_Net (Ctxt, Left);
      Res := Build2_Const_Int (Ctxt, -1, W);
      for I in 0 .. Len - 1 loop
         declare
            Pos : Uns32;
            V   : Int64;
            Sel : Net;
         begin
            if Leftmost then
               --  Iterate from the right to the left.
               Pos := I;
               if Left.Typ.Abound.Dir = Dir_To then
                  V := Int64 (Left.Typ.Abound.Right) - Int64 (I);
               else
                  V := Int64 (Left.Typ.Abound.Right) + Int64 (I);
               end if;
            else
               Pos := Len - I - 1;
               if Left.Typ.Abound.Dir = Dir_To then
                  V := Int64 (Left.Typ.Abound.Left) + Int64 (I);
               else
                  V := Int64 (Left.Typ.Abound.Left) - Int64 (I);
               end if;
            end if;
            Sel := Build2_Compare (Ctxt, Id_Eq,
                                   Build2_Extract (Ctxt, L_Net, Pos, 1),
                                   R_Net);
            Set_Location (Sel, Expr);
            Res := Build_Mux2 (Ctxt, Sel, Res, Build2_Const_Int (Ctxt, V, W));
            Set_Location (Res, Expr);
         end;
      end loop;

      return Synth_Subtype_Conversion (Ctxt, Create_Value_Net (Res, Typ),
                                       Res_Typ, False, Expr);
   end Synth_Find_Bit;

   --  Resize ARG to SIZE bits according to IS_SIGNED.
   function Synth_Resize (Ctxt : Context_Acc;
                          Arg : Valtyp;
                          Size : Width;
                          Is_Signed : Boolean;
                          Loc : Node) return Valtyp
   is
      N : Net;
   begin
      N := Get_Net (Ctxt, Arg);
      N := Build2_Resize (Ctxt, N, Size, Is_Signed, Get_Location (Loc));
      return Create_Value_Net
        (N, Create_Vec_Type_By_Length (Size, Logic_Type));
   end Synth_Resize;

   function Synth_Minmax (Ctxt : Context_Acc;
                          Left, Right : Valtyp;
                          Res_Typ : Type_Acc;
                          Id : Compare_Module_Id;
                          Expr : Node) return Valtyp
   is
      L : constant Net := Get_Net (Ctxt, Left);
      R : constant Net := Get_Net (Ctxt, Right);
      Sel, N : Net;
   begin
      Sel := Build2_Compare (Ctxt, Id, L, R);
      Set_Location (Sel, Expr);
      N := Build_Mux2 (Ctxt, Sel, R, L);
      Set_Location (N, Expr);
      return Create_Value_Net (N, Res_Typ);
   end Synth_Minmax;

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
      begin
         Size_Vt := Get_Value (Subprg_Inst, Param2);
         Strip_Const (Size_Vt);
         if not Is_Static (Size_Vt.Val) then
            Error_Msg_Synth (+Expr, "size parameter must be constant");
            return No_Valtyp;
         end if;
         Size := Uns32 (Read_Discrete (Size_Vt));
         return Synth_Resize (Ctxt, Arg, Size, Is_Signed, Expr);
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
         when Iir_Predefined_Integer_Minimum =>
            return Synth_Minmax (Ctxt, L, R, Res_Typ, Id_Slt, Expr);
         when Iir_Predefined_Integer_Maximum =>
            return Synth_Minmax (Ctxt, L, R, Res_Typ, Id_Sgt, Expr);
         when Iir_Predefined_Bit_Rising_Edge =>
            if Hook_Bit_Rising_Edge /= null then
               return Create_Value_Memtyp
                 (Hook_Bit_Rising_Edge.all (L, Res_Typ));
            end if;
            raise Internal_Error;
         when Iir_Predefined_Bit_Falling_Edge =>
            if Hook_Bit_Falling_Edge /= null then
               return Create_Value_Memtyp
                 (Hook_Bit_Falling_Edge.all (L, Res_Typ));
            end if;
            raise Internal_Error;
         when Iir_Predefined_Ieee_1164_Rising_Edge =>
            if Hook_Std_Rising_Edge /= null then
               return Create_Value_Memtyp
                 (Hook_Std_Rising_Edge.all (L, Res_Typ));
            end if;
            declare
               Edge : Net;
            begin
               Edge := Build_Posedge (Ctxt, Get_Net (Ctxt, L));
               Set_Location (Edge, Expr);
               return Create_Value_Net (Edge, Res_Typ);
            end;
         when Iir_Predefined_Ieee_1164_Falling_Edge =>
            if Hook_Std_Falling_Edge /= null then
               return Create_Value_Memtyp
                 (Hook_Std_Falling_Edge.all (L, Res_Typ));
            end if;
            declare
               Edge : Net;
            begin
               Edge := Build_Negedge (Ctxt, Get_Net (Ctxt, L));
               Set_Location (Edge, Expr);
               return Create_Value_Net (Edge, Res_Typ);
            end;
         when Iir_Predefined_Ieee_1164_Scalar_Is_X
            | Iir_Predefined_Ieee_1164_Vector_Is_X =>
            --  Always false.
            return Create_Value_Discrete (0, Boolean_Type);
         when Iir_Predefined_Ieee_1164_To_Bitvector
            | Iir_Predefined_Ieee_1164_To_Stdlogicvector_Suv
            | Iir_Predefined_Ieee_1164_To_Stdlogicvector_Bv
            | Iir_Predefined_Ieee_1164_To_Stdulogicvector_Slv
            | Iir_Predefined_Ieee_1164_To_Stdulogicvector_Bv
            | Iir_Predefined_Ieee_Numeric_Std_To_01_Uns
            | Iir_Predefined_Ieee_Numeric_Std_To_01_Sgn
            | Iir_Predefined_Ieee_1164_To_X01_Slv =>
            if Is_Static (L.Val) then
               raise Internal_Error;
            end if;
            --  A no-op (with change of bounds).
            return Create_Value_Net (Get_Net (Ctxt, L), Create_Res_Bound (L));
         when Iir_Predefined_Ieee_1164_To_Bit
            | Iir_Predefined_Ieee_1164_To_X01_Log
            | Iir_Predefined_Ieee_1164_To_Stdulogic =>
            --  A no-op.
            return Create_Value_Net (Get_Net (Ctxt, L), Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Int =>
            return Synth_Conv_Vector (False);
         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Uns_Uns =>
            declare
               B : constant Bound_Type := Get_Array_Bound (R.Typ);
            begin
               return Synth_Resize (Ctxt, L, B.Len, False, Expr);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Int =>
            return Synth_Conv_Vector (True);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Integer_Slv_Nat
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Log
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Conv_Integer =>
            --  UNSIGNED to Natural.
            return Create_Value_Net
              (Synth_Uresize (Ctxt, L, Res_Typ.W, Expr), Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Conv_Integer =>
            --  SIGNED to Integer.
            return Create_Value_Net
              (Synth_Sresize (Ctxt, L, Res_Typ.W, Expr), Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Log
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ext =>
            declare
               W : Width;
            begin
               if not Is_Static (R.Val) then
                  Error_Msg_Synth (+Expr, "size must be constant");
                  return No_Valtyp;
               end if;
               W := Uns32 (Read_Discrete (R));
               return Create_Value_Net
                 (Synth_Uresize (Ctxt, L, W, Expr),
                  Create_Vec_Type_By_Length (W, Logic_Type));
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Uns =>
            declare
               B : Bound_Type;
               W : Width;
            begin
               B := Get_Array_Bound (R.Typ);
               W := B.Len;
               return Create_Value_Net
                 (Build2_Uresize (Ctxt, Get_Net (Ctxt, L),
                                  W, Get_Location (Expr)),
                  Create_Vec_Type_By_Length (W, Logic_Type));
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Nat
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sxt =>
            if not Is_Static (R.Val) then
               Error_Msg_Synth (+Expr, "size must be constant");
               return No_Valtyp;
            end if;
            return Synth_Resize
              (Ctxt, L, Uns32 (Read_Discrete (R)), True, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Sgn =>
            declare
               B : constant Bound_Type := Get_Array_Bound (R.Typ);
            begin
               return Synth_Resize (Ctxt, L, B.Len, True, Expr);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Sgn_Nat
            | Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Shl
            | Iir_Predefined_Ieee_Std_Logic_Signed_Shl =>
            return Synth_Shift_Rotate (Ctxt, Id_Lsl, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Shr =>
            return Synth_Shift_Rotate (Ctxt, Id_Lsr, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Sgn_Nat
            | Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Shr =>
            return Synth_Shift_Rotate (Ctxt, Id_Asr, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Rot_Left_Uns_Nat =>
            return Synth_Shift_Rotate (Ctxt, Id_Rol, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Rot_Right_Uns_Nat =>
            return Synth_Shift_Rotate (Ctxt, Id_Ror, L, R, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Uns =>
            return Synth_Dyadic_Uns_Uns (Ctxt, Id_Umin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Nat =>
            return Synth_Dyadic_Uns_Nat (Ctxt, Id_Umin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Nat_Uns =>
            return Synth_Dyadic_Nat_Uns (Ctxt, Id_Umin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Sgn =>
            return Synth_Dyadic_Sgn_Sgn (Ctxt, Id_Smin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Int =>
            return Synth_Dyadic_Sgn_Int (Ctxt, Id_Smin, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Int_Sgn =>
            return Synth_Dyadic_Int_Sgn (Ctxt, Id_Smin, L, R, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Uns =>
            return Synth_Dyadic_Uns_Uns (Ctxt, Id_Umax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Nat =>
            return Synth_Dyadic_Uns_Nat (Ctxt, Id_Umax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Nat_Uns =>
            return Synth_Dyadic_Nat_Uns (Ctxt, Id_Umax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Sgn =>
            return Synth_Dyadic_Sgn_Sgn (Ctxt, Id_Smax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Int =>
            return Synth_Dyadic_Sgn_Int (Ctxt, Id_Smax, L, R, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Int_Sgn =>
            return Synth_Dyadic_Int_Sgn (Ctxt, Id_Smax, L, R, Expr);

         when Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Slv
           | Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Suv =>
            declare
               N : Net;
            begin
               N := Build_Reduce (Ctxt, Id_Red_Or, Get_Net (Ctxt, L));
               Set_Location (N, Expr);
               return Create_Value_Net (N, Res_Typ);
            end;

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
               Strip_Const (Cst);
               Res := Synth_Match (Ctxt, Cst, Oper, Expr);
               if Res = No_Net then
                  return Create_Value_Discrete (0, Boolean_Type);
               else
                  return Create_Value_Net (Res, Boolean_Type);
               end if;
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Uns =>
            return Synth_Find_Bit (Ctxt, L, R, Res_Typ, True, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Uns =>
            return Synth_Find_Bit (Ctxt, L, R, Res_Typ, False, Expr);

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
      Param : Valtyp;
      Res : Valtyp;
   begin
      Areapools.Mark (M, Instance_Pool.all);
      Subprg_Inst := Make_Instance (Syn_Inst, Imp);

      Synth_Subprogram_Association
        (Subprg_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      if Is_Error (Subprg_Inst) then
         Res := No_Valtyp;
      else
         --  If all operands are static, handle the call differently.
         Static := True;
         Inter := Inter_Chain;
         while Inter /= Null_Node loop
            Param := Get_Value (Subprg_Inst, Inter);
            if not Is_Static (Param.Val) then
               Static := False;
               exit;
            end if;
            Inter := Get_Chain (Inter);
         end loop;

         if Static then
            declare
               Param1 : Valtyp;
               Param2 : Valtyp;
               Res_Typ : Type_Acc;
               Mt : Memtyp;
            begin
               Inter := Inter_Chain;
               if Inter /= Null_Node then
                  Param1 := Get_Value (Subprg_Inst, Inter);
                  Strip_Const (Param1);
                  Inter := Get_Chain (Inter);
               else
                  Param1 := No_Valtyp;
               end if;
               if Inter /= Null_Node then
                  Param2 := Get_Value (Subprg_Inst, Inter);
                  Strip_Const (Param2);
                  Inter := Get_Chain (Inter);
               else
                  Param2 := No_Valtyp;
               end if;

               Res_Typ := Get_Subtype_Object (Subprg_Inst, Get_Type (Imp));

               Mt := Eval_Static_Predefined_Function_Call
                 (Param1, Param2, Res_Typ, Expr);
               if Mt /= Null_Memtyp then
                  Res := Create_Value_Memtyp (Mt);
               else
                  Res := No_Valtyp;
               end if;
            end;
         else
            Res := Synth_Dynamic_Predefined_Function_Call (Subprg_Inst, Expr);
         end if;
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
end Synth.Vhdl_Oper;
