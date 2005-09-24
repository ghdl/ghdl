--  Ortho implementation for GCC.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--with Ada.Unchecked_Conversion;
with Agcc; use Agcc;
with Agcc.Hwint; use Agcc.Hwint;
with Agcc.Toplev; use Agcc.Toplev;
with Agcc.Tm; use Agcc.Tm;
with Agcc.Stor_Layout; use Agcc.Stor_Layout;
with Agcc.Machmode;
with System;
with Agcc.Autils; use Agcc.Autils;
with Agcc.Real; use Agcc.Real;
with Agcc.Fe; use Agcc.Fe;
with Agcc.Rtl; use Agcc.Rtl;
with Agcc.Input; use Agcc.Input;
with Agcc.Machmode;

package body Ortho_Gcc is
   type ON_Op_To_Tree_Code_Type is array (ON_Op_Kind) of Tree_Code;
   ON_Op_To_Tree_Code : constant ON_Op_To_Tree_Code_Type :=
     (
      --  Dyadic operations.
      ON_Add_Ov => PLUS_EXPR,
      ON_Sub_Ov => MINUS_EXPR,
      ON_Mul_Ov => MULT_EXPR,
      ON_Div_Ov => ERROR_MARK,
      ON_Rem_Ov => TRUNC_MOD_EXPR,
      ON_Mod_Ov => FLOOR_MOD_EXPR,

      --  Binary operations.
      ON_And => TRUTH_AND_EXPR,
      ON_Or => TRUTH_OR_EXPR,
      ON_Xor => TRUTH_XOR_EXPR,
      ON_And_Then => TRUTH_ANDIF_EXPR,
      ON_Or_Else => TRUTH_ORIF_EXPR,

      --  Monadic operations.
      ON_Not => TRUTH_NOT_EXPR,
      ON_Neg_Ov => NEGATE_EXPR,
      ON_Abs_Ov => ABS_EXPR,

      --  Comparaisons
      ON_Eq => EQ_EXPR,
      ON_Neq => NE_EXPR,
      ON_Le => LE_EXPR,
      ON_Lt => LT_EXPR,
      ON_Ge => GE_EXPR,
      ON_Gt => GT_EXPR,

      ON_Nil => ERROR_MARK
      );

   --  Constants used for FP rounding.
   Fp_Const_P5 : REAL_VALUE_TYPE;   -- 0.5
   Fp_Const_M_P5 : REAL_VALUE_TYPE; -- -0.5
   Fp_Const_Zero : REAL_VALUE_TYPE; -- 0.0

   procedure Init
   is
      use Agcc.Machmode;

      L, H : HOST_WIDE_INT;
      V : REAL_VALUE_TYPE;
   begin
      To_Host_Wide_Int (Integer_64'(1), L, H);
      REAL_VALUE_FROM_INT (V'Address, L, H, DFmode);
      Fp_Const_P5 := REAL_VALUE_LDEXP (V, -1);

      To_Host_Wide_Int (Integer_64'(-1), L, H);
      REAL_VALUE_FROM_INT (V'Address, L, H, DFmode);
      Fp_Const_M_P5 := REAL_VALUE_LDEXP (V, -1);

      To_Host_Wide_Int (Integer_64'(0), L, H);
      REAL_VALUE_FROM_INT (Fp_Const_Zero'Address, L, H, DFmode);
   end Init;

   procedure Chain_Init (Constr : out Chain_Constr_Type) is
   begin
      Constr.First := NULL_TREE;
      Constr.Last := NULL_TREE;
   end Chain_Init;

   procedure Chain_Append (Constr : in out Chain_Constr_Type; El : Tree)
   is
   begin
      if Constr.First = NULL_TREE then
         if Constr.Last /= NULL_TREE then
            raise Program_Error;
         end if;
         Constr.First := El;
      else
         Set_TREE_CHAIN (Constr.Last, El);
      end if;
      Constr.Last := El;
   end Chain_Append;


   procedure List_Init (Constr : out List_Constr_Type) is
   begin
      Constr := (First => NULL_TREE, Last => NULL_TREE);
   end List_Init;

   procedure List_Append (Constr : in out List_Constr_Type; El : Tree)
   is
      Res : Tree;
   begin
      Res := Tree_Cons (NULL_TREE, El, NULL_TREE);
      if Constr.First = NULL_TREE then
         Constr.First := Res;
      else
         Set_TREE_CHAIN (Constr.Last, Res);
      end if;
      Constr.Last := Res;
   end List_Append;


   function New_Dyadic_Op (Kind : ON_Dyadic_Op_Kind; Left, Right : O_Enode)
     return O_Enode
   is
      Left_Type : Tree;
      Code : Tree_Code;
   begin
      Left_Type := Get_TREE_TYPE (Tree (Left));
      if Left_Type /= Get_TREE_TYPE (Tree (Right)) then
         raise Type_Error;
      end if;
      case Kind is
         when ON_Div_Ov =>
            if Get_TREE_CODE (Left_Type) = REAL_TYPE then
               Code := RDIV_EXPR;
            else
               Code := TRUNC_DIV_EXPR;
            end if;
         when others =>
            Code := ON_Op_To_Tree_Code (Kind);
      end case;
      return O_Enode (Build (Code, Left_Type, Tree (Left), Tree (Right)));
   end New_Dyadic_Op;

   function New_Monadic_Op (Kind : ON_Monadic_Op_Kind; Operand : O_Enode)
     return O_Enode
   is
   begin
      return Build1 (ON_Op_To_Tree_Code (Kind),
                     Get_TREE_TYPE (Operand), Operand);
   end New_Monadic_Op;

   function New_Compare_Op
     (Kind : ON_Compare_Op_Kind; Left, Right : O_Enode; Ntype : O_Tnode)
     return O_Enode
   is
   begin
      if Get_TREE_CODE (Ntype) /= BOOLEAN_TYPE then
         raise Type_Error;
      end if;
      if Get_TREE_TYPE (Left) /= Get_TREE_TYPE (Right) then
         raise Type_Error;
      end if;
      return O_Enode (Build (ON_Op_To_Tree_Code (Kind),
                             Tree (Ntype), Tree (Left), Tree (Right)));
   end New_Compare_Op;

--   function Unchecked_Conversion is new
--   Ada.Unchecked_Conversion (Source => Unsigned_32, Target => HOST_WIDE_INT);

--    function High_Part (V : Unsigned_64) return HOST_WIDE_INT
--    is
--    begin
--       return Unchecked_Conversion (Unsigned_32 (Shift_Left (V, 32)));
--    end High_Part;

--    function Low_Part (V : Unsigned_64) return HOST_WIDE_INT
--    is
--    begin
--       return Unchecked_Conversion
--         (Unsigned_32 (V and (Unsigned_32'Modulus - 1)));
--    end Low_Part;

   function New_Signed_Literal (Ltype : O_Tnode; Value : Integer_64)
     return O_Cnode
   is
      L, H : HOST_WIDE_INT;
      Res : Tree;
   begin
      To_Host_Wide_Int (Value, L, H);
      Res := Build_Int_2 (L, H);
      Set_TREE_TYPE (Res, Tree (Ltype));
      return O_Cnode (Res);
   end New_Signed_Literal;

   function New_Unsigned_Literal (Ltype : O_Tnode; Value : Unsigned_64)
     return O_Cnode
   is
      Res : Tree;
      L, H : HOST_WIDE_INT;
   begin
      To_Host_Wide_Int (Value, L, H);
      Res := Build_Int_2 (L, H);
      Set_TREE_TYPE (Res, Tree (Ltype));
      return O_Cnode (Res);
   end New_Unsigned_Literal;

   function New_Null_Access (Ltype : O_Tnode) return O_Cnode
   is
      Res : Tree;
   begin
      Res := Build_Int_2 (0, 0);
      Set_TREE_TYPE (Res, Tree (Ltype));
      return O_Cnode (Res);
   end New_Null_Access;

   function New_Float_Literal (Ltype : O_Tnode; Value : IEEE_Float_64)
                              return O_Cnode
   is
      Res : REAL_VALUE_TYPE;
   begin
      Res := To_Real_Value_Type (Value);
      return O_Cnode (Build_Real (Tree (Ltype), Res));
   end New_Float_Literal;

   procedure Check_Constrained_Type (Atype : O_Tnode)
   is
      pragma Unreferenced (Atype);
   begin
      null;
   end Check_Constrained_Type;

   procedure Finish_Type_Def (Atype : O_Tnode) is
   begin
      Layout_Type (Atype);
      --Rest_Of_Type_Compilation (Tree (Atype), True);
   end Finish_Type_Def;

   procedure New_Uncomplete_Record_Type (Res : out O_Tnode) is
   begin
      Res := Make_Node (RECORD_TYPE);
   end New_Uncomplete_Record_Type;

   procedure Start_Record_Type (Elements : out O_Element_List) is
   begin
      Elements.Res := Make_Node (RECORD_TYPE);
      Chain_Init (Elements.Chain);
   end Start_Record_Type;

   procedure Start_Uncomplete_Record_Type (Res : O_Tnode;
                                           Elements : out O_Element_List) is
   begin
      Elements.Res := Tree (Res);
      Chain_Init (Elements.Chain);
   end Start_Uncomplete_Record_Type;

   procedure New_Record_Union_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode)
   is
      Res : Tree;
   begin
      Check_Constrained_Type (Etype);
      Res := Build_Decl (FIELD_DECL, Ident, Tree (Etype));
      Set_DECL_CONTEXT (Res, Elements.Res);
      Chain_Append (Elements.Chain, Res);
      El := O_Fnode (Res);
   end New_Record_Union_Field;

   procedure New_Record_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode)
     renames New_Record_Union_Field;

   procedure Finish_Record_Type
     (Elements : in out O_Element_List; Res : out O_Tnode) is
   begin
      Set_TYPE_FIELDS (Elements.Res, Elements.Chain.First);
      Finish_Type_Def (O_Tnode (Elements.Res));
      Res := O_Tnode (Elements.Res);
      if Get_TYPE_NAME (Elements.Res) /= NULL_TREE then
         --  The type was completed.
         Rest_Of_Type_Compilation (Elements.Res, C_True);
      end if;
   end Finish_Record_Type;

   procedure Start_Union_Type (Elements : out O_Element_List) is
   begin
      Elements.Res := Make_Node (UNION_TYPE);
      Chain_Init (Elements.Chain);
   end Start_Union_Type;

   procedure New_Union_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode)
     renames New_Record_Union_Field;

   procedure Finish_Union_Type
     (Elements : in out O_Element_List; Res : out O_Tnode) is
   begin
      Set_TYPE_FIELDS (Elements.Res, Elements.Chain.First);
      Finish_Type_Def (O_Tnode (Elements.Res));
      Res := O_Tnode (Elements.Res);
   end Finish_Union_Type;

   function New_Access_Type (Dtype : O_Tnode) return O_Tnode
   is
      Res : Tree;
   begin
      if Dtype = O_Tnode_Null then
         Res := Make_Node (POINTER_TYPE);
         Set_TREE_TYPE (Res, NULL_TREE);
         Set_TYPE_MODE (Res, Machmode.Ptr_Mode);
         Layout_Type (Res);
         return O_Tnode (Res);
      else
         return Build_Pointer_Type (Dtype);
      end if;
   end New_Access_Type;

   procedure Finish_Access_Type (Atype : O_Tnode; Dtype : O_Tnode) is
   begin
      if Get_TREE_CODE (Atype) /= POINTER_TYPE
        or else Get_TREE_TYPE (Atype) /= O_Tnode_Null
      then
         raise Syntax_Error;
      end if;
      Set_TREE_TYPE (Atype, Dtype);
   end Finish_Access_Type;

   function New_Array_Type (El_Type : O_Tnode; Index_Type : O_Tnode)
     return O_Tnode
   is
   begin
      Check_Constrained_Type (El_Type);
      return Build_Array_Type (El_Type, Index_Type);
   end New_Array_Type;

   function New_Constrained_Array_Type (Atype : O_Tnode; Length : O_Cnode)
     return O_Tnode
   is
      Range_Type : Tree;
      Index_Type : Tree;
      Len : Tree;
      One : Tree;
   begin
      --if Atype.Kind /= ON_Array_Type then
      --   raise Type_Error;
      --end if;
      Index_Type := Get_TYPE_DOMAIN (Tree (Atype));
      if +Integer_Zerop (Tree (Length)) then
         --  Handle null array, by creating a one-length array...
         Len := Size_Zero_Node;
      else
         One := Build_Int_2 (1, 0);
         Set_TREE_TYPE (One, Index_Type);
         Len := Build (MINUS_EXPR, Index_Type, Tree (Length), One);
         Len := Fold (Len);
      end if;
      Range_Type := Build_Range_Type (Index_Type, Size_Zero_Node, Len);
      return O_Tnode (Build_Array_Type (Get_TREE_TYPE (Tree (Atype)),
                                        Range_Type));
   end New_Constrained_Array_Type;

   function New_Unsigned_Type (Size : Natural) return O_Tnode
   is
   begin
      return Make_Unsigned_Type (Size);
   end New_Unsigned_Type;

   function New_Signed_Type (Size : Natural) return O_Tnode
   is
   begin
      return Make_Signed_Type (Size);
   end New_Signed_Type;

   function New_Float_Type return O_Tnode is
      Res : O_Tnode;
   begin
      Res := Make_Node (REAL_TYPE);
      Set_TYPE_PRECISION (Res, DOUBLE_TYPE_SIZE);
      Layout_Type (Res);
      return Res;
   end New_Float_Type;

   procedure New_Boolean_Type (Res : out O_Tnode;
                               False_Id : O_Ident;
                               False_E : out O_Cnode;
                               True_Id : O_Ident;
                               True_E : out O_Cnode)
   is
      pragma Unreferenced (False_Id);
      pragma Unreferenced (True_Id);
   begin
      --  see java/decl.c
      Res := O_Tnode'(Make_Node (BOOLEAN_TYPE));
      Set_TYPE_PRECISION (Tree (Res), 1);
      Fixup_Unsigned_Type (Tree (Res));
      False_E := O_Cnode (Get_TYPE_MIN_VALUE (Tree (Res)));
      True_E := O_Cnode (Get_TYPE_MAX_VALUE (Tree (Res)));
   end New_Boolean_Type;

   procedure Start_Enum_Type (List : out O_Enum_List; Size : Natural)
   is
   begin
      List.Res := Make_Node (ENUMERAL_TYPE);
      Chain_Init (List.Chain);
      List.Num := 0;
      List.Size := Size;
   end Start_Enum_Type;

   procedure New_Enum_Literal
     (List : in out O_Enum_List; Ident : O_Ident; Res : out O_Cnode)
   is
   begin
      Res := Build_Int_2 (HOST_WIDE_INT (List.Num), 0);
      Set_TREE_TYPE (Tree (Res), List.Res);
      Chain_Append (List.Chain, Tree_Cons (Ident, Tree (Res), NULL_TREE));
      List.Num := List.Num + 1;
   end New_Enum_Literal;

   procedure Finish_Enum_Type (List : in out O_Enum_List; Res : out O_Tnode) is
   begin
      Res := O_Tnode (List.Res);
      Set_TYPE_VALUES (List.Res, List.Chain.First);
      Set_TYPE_MIN_VALUE (List.Res, Get_TREE_VALUE (List.Chain.First));
      Set_TYPE_MAX_VALUE (List.Res, Get_TREE_VALUE (List.Chain.Last));
      Set_TREE_UNSIGNED (List.Res, C_True);
      Set_TYPE_PRECISION (List.Res, Integer (List.Size));
      Finish_Type_Def (Res);
   end Finish_Enum_Type;

   procedure Start_Record_Aggr (List : out O_Record_Aggr_List; Atype : O_Tnode)
   is
   begin
      List.Atype := Tree (Atype);
      Chain_Init (List.Chain);
   end Start_Record_Aggr;

   procedure New_Record_Aggr_El
     (List : in out O_Record_Aggr_List; Value : O_Cnode)
   is
   begin
      --  FIXME: should check type of value.
      Chain_Append (List.Chain,
                    Build_Tree_List (NULL_TREE, Tree (Value)));
   end New_Record_Aggr_El;

   procedure Finish_Record_Aggr
     (List : in out O_Record_Aggr_List; Res : out O_Cnode)
   is
   begin
      Res := O_Cnode (Build_Constructor (List.Atype, List.Chain.First));
   end Finish_Record_Aggr;

   procedure Start_Array_Aggr (List : out O_Array_Aggr_List; Atype : O_Tnode)
   is
   begin
      List.Atype := Tree (Atype);
      Chain_Init (List.Chain);
   end Start_Array_Aggr;

   procedure New_Array_Aggr_El
     (List : in out O_Array_Aggr_List; Value : O_Cnode)
   is
   begin
      --  FIXME: should check type of value.
      Chain_Append (List.Chain,
                    Build_Tree_List (NULL_TREE, Tree (Value)));
   end New_Array_Aggr_El;

   procedure Finish_Array_Aggr
     (List : in out O_Array_Aggr_List; Res : out O_Cnode)
   is
   begin
      Res := O_Cnode (Build_Constructor (List.Atype, List.Chain.First));
   end Finish_Array_Aggr;

   function New_Union_Aggr (Atype : O_Tnode; Field : O_Fnode; Value : O_Cnode)
                           return O_Cnode
   is
      El : Tree;
      Res : Tree;
   begin
      El := Build_Tree_List (Tree (Field), Tree (Value));
      Res := Build_Constructor (Tree (Atype), El);
      Set_TREE_CONSTANT (Res, C_True);
      return O_Cnode (Res);
   end New_Union_Aggr;

   function New_Indexed_Element (Arr : O_Lnode; Index : O_Enode)
     return O_Lnode
   is
      Res : Tree;
      V : C_Bool;
   begin
      V := Mark_Addressable (Tree (Arr));
      Res := Build (ARRAY_REF, Get_TREE_TYPE (Get_TREE_TYPE (Tree (Arr))),
                    Tree (Arr), Tree (Index));
      return O_Lnode (Res);
   end New_Indexed_Element;

   function New_Slice (Arr : O_Lnode; Res_Type : O_Tnode; Index : O_Enode)
     return O_Lnode
   is
      Res : Tree;
      Ptr_Type : Tree;
      V : C_Bool;
   begin
      --  *((RES_TYPE *)(&ARR[INDEX]))
      --  convert ARR to a pointer, add index, and reconvert to array ?
      if Get_TREE_CODE (Res_Type) /= ARRAY_TYPE then
         raise Type_Error;
      end if;
      V := Mark_Addressable (Tree (Arr));
      Ptr_Type := Build_Pointer_Type (Tree (Res_Type));
      Res := Build (ARRAY_REF, Get_TREE_TYPE (Get_TREE_TYPE (Tree (Arr))),
                    Tree (Arr), Tree (Index));
      Res := Build1 (ADDR_EXPR, Ptr_Type, Res);
      Res := Build1 (INDIRECT_REF, Tree (Res_Type), Res);
      return O_Lnode (Res);
   end New_Slice;

   function New_Selected_Element (Rec : O_Lnode; El : O_Fnode)
     return O_Lnode
   is
   begin
      if Get_TREE_CODE (Get_TREE_TYPE (Rec)) /= RECORD_TYPE then
         raise Type_Error;
      end if;
      return O_Lnode (Build (COMPONENT_REF, Get_TREE_TYPE (Tree (El)),
                             Tree (Rec), Tree (El)));
   end New_Selected_Element;

   function New_Access_Element (Acc : O_Enode) return O_Lnode
   is
      Acc_Type : Tree;
   begin
      Acc_Type := Get_TREE_TYPE (Tree (Acc));
      if Get_TREE_CODE (Acc_Type) /= POINTER_TYPE then
         raise Type_Error;
      end if;
      return O_Lnode (Build1 (INDIRECT_REF, Get_TREE_TYPE (Acc_Type),
                              Tree (Acc)));
   end New_Access_Element;

   function New_Convert_Ov (Val : O_Enode; Rtype : O_Tnode) return O_Enode
   is
      Val_Type : Tree;
      Val_Code : Tree_Code;
      Rtype_Code : Tree_Code;
      Code : Tree_Code;
   begin
      Val_Type := Get_TREE_TYPE (Tree (Val));
      if Val_Type = Tree (Rtype) then
         return Val;
      end if;
      --  FIXME: check conversions.
      Val_Code := Get_TREE_CODE (Val_Type);
      Rtype_Code := Get_TREE_CODE (Rtype);
      if Val_Code = POINTER_TYPE and then Rtype_Code = POINTER_TYPE then
         Code := NOP_EXPR;
      elsif Val_Code = INTEGER_TYPE and then Rtype_Code = INTEGER_TYPE then
         Code := CONVERT_EXPR;
      elsif Val_Code = REAL_TYPE and then Rtype_Code = INTEGER_TYPE then
         --  REAL to INTEGER
         --  Gcc only handles FIX_TRUNC_EXPR, but we need rounding.
         declare
            M_P5 : Tree;
            P5 : Tree;
            Zero : Tree;
            Saved : Tree;
            Comp : Tree;
            Adj : Tree;
            Res : Tree;
         begin
            M_P5 := Build_Real (Val_Type, Fp_Const_M_P5);
            P5 := Build_Real (Val_Type, Fp_Const_P5);
            Zero := Build_Real (Val_Type, Fp_Const_Zero);
            Saved := Build_Save_Expr (Tree (Val));
            Comp := Build (GE_EXPR, Integer_Type_Node, Saved, Zero);
            --  FIXME: instead of res = res + (comp ? .5 : -.5)
            --                 do: res = res (comp ? + : -) .5
            Adj := Build (COND_EXPR, Val_Type, Comp, P5, M_P5);
            Res := Build (PLUS_EXPR, Val_Type, Saved, Adj);
            Res := Build1 (FIX_TRUNC_EXPR, Tree (Rtype), Res);
            return O_Enode (Res);
         end;
      elsif Val_Code = INTEGER_TYPE and then Rtype_Code = ENUMERAL_TYPE then
         Code := CONVERT_EXPR;
      elsif Val_Code = ENUMERAL_TYPE and then Rtype_Code = INTEGER_TYPE then
         Code := CONVERT_EXPR;
      elsif Val_Code = INTEGER_TYPE and then Rtype_Code = REAL_TYPE then
         Code := FLOAT_EXPR;
      elsif Val_Code = BOOLEAN_TYPE and then Rtype_Code = BOOLEAN_TYPE then
         Code := NOP_EXPR;
      elsif Val_Code = BOOLEAN_TYPE and then Rtype_Code = INTEGER_TYPE then
         Code := CONVERT_EXPR;
      elsif Val_Code = INTEGER_TYPE and then Rtype_Code = BOOLEAN_TYPE then
         --  From integer to boolean.
         Code := NOP_EXPR;
      elsif Val_Code = REAL_TYPE and then Rtype_Code = REAL_TYPE then
         Code := CONVERT_EXPR;
      else
         raise Program_Error;
      end if;
      return O_Enode (Build1 (Code, Tree (Rtype), Tree (Val)));
   end New_Convert_Ov;

   function Build_Addr (Operand : Tree; Atype : Tree) return Tree
   is
      use Agcc.Machmode;
      Result : Tree;
   begin
      case Get_TREE_CODE (Operand) is
         when INDIRECT_REF =>
            --  This may be an unchecked conversion.
            Result := Get_TREE_OPERAND (Operand, 0);
            if Get_TREE_CODE (Get_TREE_TYPE (Result)) /= POINTER_TYPE then
               raise Program_Error;
            end if;
            return Result;

         when ARRAY_REF
           | COMPONENT_REF =>
            --  Find the address of the prefix and add the offset.
            declare
               Op_Type : Tree;
               Bitsize, Bitpos : HOST_WIDE_INT;
               Inner, Offset : Tree;
               Mode : Machine_Mode;
               Unsignedp, Volatilep : Integer;
            begin
               Op_Type := Get_TREE_TYPE (Get_TREE_OPERAND (Operand, 0));
               Inner := Get_Inner_Reference
                 (Operand, Bitsize'Address, Bitpos'Address, Offset'Address,
                  Mode'Address, Unsignedp'Address, Volatilep'Address);

               -- Compute the offset as a byte offset from INNER.  */
               if Offset = NULL_TREE then
                  Offset := Size_Zero_Node;
               end if;

               Offset := Size_Binop
                 (PLUS_EXPR, Offset,
                  Size_Int (Bitpos / HOST_WIDE_INT (BITS_PER_UNIT)));

               -- Take the address of INNER, convert the offset to void *, and
               -- add then.  It will later be converted to the desired result
               -- type, if any.
               Inner := Build_Addr (Inner, Ptr_Type_Node);
               Inner := Convert (Ptr_Type_Node, Inner);
               Offset := Convert (Ptr_Type_Node, Offset);
               Result := Fold
                 (Build (PLUS_EXPR, Ptr_Type_Node, Inner, Offset));
               return Result;
            end;

         when VAR_DECL =>
            --  This is like Mark_Addressable.
            Put_Var_Into_Stack (Operand, C_True);
            Set_TREE_ADDRESSABLE (Operand, C_True);
            return Build1 (ADDR_EXPR, Atype, Operand);

         when others =>
            raise Program_Error;
      end case;
   end Build_Addr;

   function New_Addr (Lvalue : Tree; Atype : O_Tnode)
                     return Tree
   is
      Result : Tree;
   begin
      Result := Build_Addr (Lvalue, Tree (Atype));
      if Get_TREE_TYPE (Result) /= Tree (Atype) then
         if Get_TREE_CODE (Get_TREE_TYPE (Result)) /= POINTER_TYPE then
            raise Program_Error;
         end if;
         Result := Build1 (NOP_EXPR, Tree (Atype), Result);
      end if;
      return Result;
      --return O_Enode (Build1 (ADDR_EXPR, Tree (Atype), Tree (Lvalue)));
   end New_Addr;

   function New_Unchecked_Address (Lvalue : O_Lnode; Atype : O_Tnode)
                                  return O_Enode
   is
   begin
      return O_Enode (New_Addr (Tree (Lvalue), Atype));
   end New_Unchecked_Address;

   function New_Address (Lvalue : O_Lnode; Atype : O_Tnode) return O_Enode
   is
   begin
      --if Get_TREE_TYPE (Lvalue) /= Get_TREE_TYPE (Atype) then
      --   raise Type_Error;
      --end if;
      return O_Enode (New_Addr (Tree (Lvalue), Atype));
   end New_Address;

   function New_Global_Unchecked_Address (Decl : O_Dnode; Atype : O_Tnode)
                                         return O_Cnode
   is
   begin
      return O_Cnode (New_Addr (Tree (Decl), Atype));
   end New_Global_Unchecked_Address;

   function New_Global_Address (Decl : O_Dnode; Atype : O_Tnode) return O_Cnode
   is
   begin
      --if Get_TREE_TYPE (Lvalue) /= Get_TREE_TYPE (Atype) then
      --   raise Type_Error;
      --end if;
      return O_Cnode (New_Addr (Tree (Decl), Atype));
   end New_Global_Address;

   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
     return O_Cnode
   is
   begin
      return O_Cnode (Build1 (ADDR_EXPR, Tree (Atype), Tree (Subprg)));
   end New_Subprogram_Address;

   function New_Value (Lvalue : O_Lnode) return O_Enode is
   begin
      return O_Enode (Lvalue);
   end New_Value;

   function New_Obj_Value (Obj : O_Dnode) return O_Enode is
   begin
      return O_Enode (Obj);
   end New_Obj_Value;

   function New_Obj (Obj : O_Dnode) return O_Lnode is
   begin
      return O_Lnode (Obj);
   end New_Obj;

   function New_Lit (Lit : O_Cnode) return O_Enode is
   begin
      return O_Enode (Lit);
   end New_Lit;

   function New_Offsetof (Field : O_Fnode; Rtype : O_Tnode) return O_Cnode
   is
      Off : Tree;
      Bit_Off : Tree;
      Pos : HOST_WIDE_INT;
      Res : Tree;
   begin
      Off := Get_DECL_FIELD_OFFSET (Tree (Field));
      if Host_Integerp (Off, 1) = 0 then
         --  The offset must be a constant.
         raise Program_Error;
      end if;
      Bit_Off := Get_DECL_FIELD_BIT_OFFSET (Tree (Field));
      if Host_Integerp (Bit_Off, 1) = 0 then
         --  The offset must be a constant.
         raise Program_Error;
      end if;
      Pos := Get_TREE_INT_CST_LOW (Off)
        + (Get_TREE_INT_CST_LOW (Bit_Off) / HOST_WIDE_INT (BITS_PER_UNIT));
      Res := Build_Int_2 (Pos, 0);
      Set_TREE_TYPE (Res, Tree (Rtype));
      return O_Cnode (Res);
   end New_Offsetof;

   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode
   is
      Size : Tree;
   begin
      Size := Get_TYPE_SIZE_UNIT (Tree (Atype));
      --Size := Size_Binop (CEIL_DIV_EXPR, Size,
      --                    Size_Int (HOST_WIDE_INT (BITS_PER_UNIT)));

      return O_Cnode (Fold (Build1 (NOP_EXPR, Tree (Rtype), Size)));
   end New_Sizeof;

   function New_Alloca (Rtype : O_Tnode; Size : O_Enode) return O_Enode
   is
      Var : Tree;
      Var_Type : Tree;
      Res : Tree;
   begin
      --  Create a dummy variable of the correct size.
      --  This way, the storage will be deallocated at the end of the scope.
      Var_Type := Build_Array_Type (Char_Type_Node,
                                    Build_Index_Type (Tree (Size)));
      Var := Build_Decl (VAR_DECL, NULL_TREE, Var_Type);
      Set_TREE_STATIC (Var, C_False);
      Set_DECL_EXTERNAL (Var, C_False);
      Set_TREE_PUBLIC (Var, C_False);
      Pushdecl (Var);
      Expand_Decl (Var);
      Res := Build1 (ADDR_EXPR, Tree (Rtype), Var);
      return O_Enode (Res);

      -- Old code that use alloca.  This is not space efficient, since the
      -- storage will be freed only at the exit of the function.
      --Arg := Build1 (CONVERT_EXPR, Sizetype, Tree (Size));
      --Res := Build (CALL_EXPR, Ptr_Type_Node, Alloca_Function_Ptr,
      --              Tree_Cons (NULL_TREE, Arg, NULL_TREE));
      --return O_Enode (Build1 (NOP_EXPR, Tree (Rtype), Res));
   end New_Alloca;

   ---------------------
   --  Declarations.  --
   ---------------------

   type String_Acc is access String;
   Current_Filename : String_Acc := null;

   procedure New_Debug_Filename_Decl (Filename : String)
   is
      Len : Natural;
   begin
      Len := Filename'Length;
      if Current_Filename = null
        or else Current_Filename.all'Length /= Len + 1
        or else Current_Filename (1 .. Len) /= Filename
      then
         Current_Filename := new String (1 .. Len + 1);
         Current_Filename (1 .. Len) := Filename;
         Current_Filename (Len + 1) := Nul;
         Input_Location.File := Current_Filename (1)'Address;
      end if;
   end New_Debug_Filename_Decl;

   procedure New_Debug_Line_Decl (Line : Natural)
   is
   begin
      Input_Location.Line := Line;
   end New_Debug_Line_Decl;

   procedure New_Debug_Comment_Decl (Comment : String)
   is
      pragma Unreferenced (Comment);
   begin
      null;
   end New_Debug_Comment_Decl;

   procedure New_Type_Decl (Ident : O_Ident; Atype : O_Tnode)
   is
      Decl : Tree;
      Ttype : Tree := Tree (Atype);
   begin
--       if Atype.Decl /= null then
--          raise Type_Error;
--       end if;
      Set_TYPE_NAME (Ttype, Ident);
      Decl := Build_Decl (TYPE_DECL, Ident, Ttype);
      Set_TYPE_STUB_DECL (Ttype, Decl);
      Pushdecl (Decl);
      if Get_TYPE_SIZE (Ttype) /= NULL_TREE then
         --  Do not generate debug info for uncompleted types.
         Rest_Of_Type_Compilation (Ttype, C_True);
      end if;
   end New_Type_Decl;

   procedure Set_Storage (Node : Tree; Storage : O_Storage)
   is
   begin
      case Storage is
         when O_Storage_External =>
            Set_DECL_EXTERNAL (Node, C_True);
            Set_TREE_PUBLIC (Node, C_True);
            Set_TREE_STATIC (Node, C_False);
         when O_Storage_Public =>
            Set_DECL_EXTERNAL (Node, C_False);
            Set_TREE_PUBLIC (Node, C_True);
            Set_TREE_STATIC (Node, C_True);
         when O_Storage_Private =>
            Set_DECL_EXTERNAL (Node, C_False);
            Set_TREE_PUBLIC (Node, C_False);
            Set_TREE_STATIC (Node, C_True);
         when O_Storage_Local =>
            Set_DECL_EXTERNAL (Node, C_False);
            Set_TREE_PUBLIC (Node, C_False);
            Set_TREE_STATIC (Node, C_False);
      end case;
   end Set_Storage;

   procedure New_Const_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode)
   is
      Cst : Tree;
   begin
      Cst := Build_Decl (VAR_DECL, Ident, Tree (Atype));
      Set_Storage (Cst, Storage);
      Set_TREE_READONLY (Cst, C_True);
      Pushdecl (Cst);
      case Storage is
         when O_Storage_Local =>
            raise Syntax_Error;
         when O_Storage_External =>
            --  We are at top level if Current_Function_Decl is null.
            Rest_Of_Decl_Compilation
              (Cst, NULL_Chars,
               Boolean'Pos (Current_Function_Decl = NULL_TREE), C_False);
         when O_Storage_Public
           | O_Storage_Private =>
            null;
      end case;
      Res := O_Dnode (Cst);
   end New_Const_Decl;

   procedure Start_Const_Value (Const : in out O_Dnode)
   is
      pragma Unreferenced (Const);
   begin
      null;
   end Start_Const_Value;

   procedure Finish_Const_Value (Const : in out O_Dnode; Val : O_Cnode)
   is
   begin
      Set_DECL_INITIAL (Tree (Const), Tree (Val));
      Set_TREE_CONSTANT (Val, C_True);
      Rest_Of_Decl_Compilation
        (Tree (Const), NULL_Chars,
         Boolean'Pos (Current_Function_Decl = NULL_TREE), C_False);
   end Finish_Const_Value;

   procedure New_Var_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode)
   is
      Var : Tree;
   begin
      Var := Build_Decl (VAR_DECL, Ident, Tree (Atype));
      if Current_Function_Decl /= NULL_TREE  then
         --  Local variable.
         Set_TREE_STATIC (Var, C_False);
         Set_DECL_EXTERNAL (Var, C_False);
         Set_TREE_PUBLIC (Var, C_False);
      else
         Set_Storage (Var, Storage);
      end if;
      Pushdecl (Var);
      if Current_Function_Decl /= NULL_TREE  then
         Expand_Decl (Var);
      else
         Rest_Of_Decl_Compilation (Var, NULL_Chars, C_True, C_False);
      end if;
      Res := O_Dnode (Var);
   end New_Var_Decl;

   procedure Start_Function_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage;
      Rtype : O_Tnode)
   is
   begin
      Interfaces.Ident := Ident;
      Interfaces.Storage := Storage;
      Interfaces.Rtype := Rtype;
      Chain_Init (Interfaces.Param_Chain);
      List_Init (Interfaces.Param_List);
   end Start_Function_Decl;

   procedure Start_Procedure_Decl
     (Interfaces : out O_Inter_List; Ident : O_Ident; Storage : O_Storage) is
   begin
      Start_Function_Decl (Interfaces, Ident, Storage,
                           O_Tnode (Void_Type_Node));
   end Start_Procedure_Decl;

   procedure New_Interface_Decl
     (Interfaces : in out O_Inter_List;
      Res : out O_Dnode;
      Ident : O_Ident;
      Atype : O_Tnode)
   is
      R : Tree;
   begin
      R := Build_Decl (PARM_DECL, Ident, Tree (Atype));
      --Set_DECL_CONTEXT (Res, Xxx);

      --  Do type conversion: convert boolean and enums to int
      if +PROMOTE_PROTOTYPES then
         case Get_TREE_CODE (Tree (Atype)) is
            when ENUMERAL_TYPE
              | BOOLEAN_TYPE =>
               Set_DECL_ARG_TYPE (R, Integer_Type_Node);
            when others =>
               Set_DECL_ARG_TYPE (R, Tree (Atype));
         end case;
      else
         Set_DECL_ARG_TYPE (R, Tree (Atype));
      end if;

      Chain_Append (Interfaces.Param_Chain, R);
      List_Append (Interfaces.Param_List, Tree (Atype));
      Res := O_Dnode (R);
   end New_Interface_Decl;

   --  Current function nest level, or the number of parents.
   Function_Nest_Level : Natural := 0;

   procedure Finish_Subprogram_Decl
     (Interfaces : in out O_Inter_List; Res : out O_Dnode)
   is
      Decl : Tree;
      Result : Tree;
      Parm : Tree;
      Is_Global : Boolean;
   begin
      Decl := Build_Decl (FUNCTION_DECL, Interfaces.Ident,
                          Build_Function_Type (Tree (Interfaces.Rtype),
                                               Interfaces.Param_List.First));
      Is_Global := Function_Nest_Level = 0
        or Interfaces.Storage = O_Storage_External;
      if Is_Global then
         Set_Storage (Decl, Interfaces.Storage);
      else
         --  A nested subprogram.
         Set_DECL_EXTERNAL (Decl, C_False);
         Set_TREE_PUBLIC (Decl, C_False);
      end if;
      --  The function exist in static storage.
      Set_TREE_STATIC (Decl, C_True);
      Set_DECL_INITIAL (Decl, Error_Mark_Node);
      Set_TREE_ADDRESSABLE (Decl, C_True);

      --  Declare the result.
      --  FIXME: should be moved in start_function_body.
      Result := Build_Decl (RESULT_DECL, NULL_TREE, Tree (Interfaces.Rtype));
      Set_DECL_RESULT (Decl, Result);
      Set_DECL_CONTEXT (Result, Decl);

      Set_DECL_ARGUMENTS (Decl, Interfaces.Param_Chain.First);
      --  Set DECL_CONTEXT of parameters.
      Parm := Interfaces.Param_Chain.First;
      while Parm /= NULL_TREE loop
         Set_DECL_CONTEXT (Parm, Decl);
         Parm := Get_TREE_CHAIN (Parm);
      end loop;

      Pushdecl (Decl);

      if Is_Global then
         Rest_Of_Decl_Compilation (Decl, NULL_Chars, C_True, C_False);
      else
         Expand_Decl (Decl);
      end if;

      --Make_Function_Rtl (Decl);

      Res := O_Dnode (Decl);
   end Finish_Subprogram_Decl;

   procedure Start_Subprogram_Body (Func : O_Dnode)
   is
   begin
      if Function_Nest_Level /= 0 then
         --  For a nested subprogram:
         Push_Function_Context;
         --start_function (c-decl.c)
         --  announce_function
         --  current_function_decl = pushdecl (x)
         --  ??
         --XXX
         --finish_function(1) (c-decl.c)
         --  poplevel
         --pop_function_context
         --add_decl_stmt
      end if;
      Function_Nest_Level := Function_Nest_Level + 1;

      Current_Function_Decl := Tree (Func);
      Announce_Function (Tree (Func));

      --  Create a binding for the parameters.
      Pushlevel (C_False);
      --  FIXME: should push parameters.
      --Make_Function_Rtl (Current_Function_Decl);
      Init_Function_Start (Func, NULL_Chars, 0);
      Expand_Function_Start (Func, C_False);
      --  Create a binding for the function.
      --  This is necessary for compatibility.
      Pushlevel (C_False);
      Expand_Start_Bindings (0);
   end Start_Subprogram_Body;

   procedure Finish_Subprogram_Body
   is
   begin
      Expand_End_Bindings (Getdecls, C_True, C_False);
      Poplevel (C_True, C_False, C_False);
      Expand_Function_End (NULL_Chars, 0, C_False);
      Poplevel (C_True, C_False, C_True);
      --  The subprogram is not external anymore (extern inline is not
      --  supported).  As a result, code will be generated.
      Set_DECL_EXTERNAL (Current_Function_Decl, C_False);

      --  FIXME: protect against ggc.  See c-decl.c:c_expand_body
      Rest_Of_Compilation (Current_Function_Decl);
      Function_Nest_Level := Function_Nest_Level - 1;
      if Function_Nest_Level > 0 then
         Pop_Function_Context;
      else
         Current_Function_Decl := NULL_TREE;
      end if;
   end Finish_Subprogram_Body;

   -------------------
   --  Statements.  --
   -------------------

   procedure New_Debug_Line_Stmt (Line : Natural) is
   begin
      Input_Location.Line := Line;
      Emit_Line_Note (Input_Location);
   end New_Debug_Line_Stmt;

   procedure New_Debug_Comment_Stmt (Comment : String)
   is
      pragma Unreferenced (Comment);
   begin
      null;
   end New_Debug_Comment_Stmt;

   procedure Start_Declare_Stmt
   is
   begin
      Pushlevel (C_False);
      Expand_Start_Bindings (0);
   end Start_Declare_Stmt;

   procedure Finish_Declare_Stmt
   is
   begin
      Expand_End_Bindings (Getdecls, C_True, C_True);
      Poplevel (C_True, C_False, C_False);
   end Finish_Declare_Stmt;

   procedure Start_Association (Assocs : out O_Assoc_List; Subprg : O_Dnode)
   is
   begin
      Assocs.Subprg := Tree (Subprg);
      List_Init (Assocs.List);
   end Start_Association;

--    function Get_Base_Type (Atype : O_Tnode) return O_Tnode
--    is
--    begin
--       case Atype.Kind is
--          when ON_Array_Sub_Type =>
--             return Atype.Base_Type;
--          when others =>
--             return Atype;
--       end case;
--    end Get_Base_Type;

   procedure New_Association (Assocs : in out O_Assoc_List; Val : O_Enode)
   is
   begin
      List_Append (Assocs.List, Tree (Val));
   end New_Association;

   --  Return a pointer to function FUNC.
   function Build_Function_Ptr (Func : Tree) return Tree is
   begin
      return Build1 (ADDR_EXPR,
                     Build_Pointer_Type (Get_TREE_TYPE (Func)), Func);
   end Build_Function_Ptr;

   function New_Function_Call (Assocs : O_Assoc_List) return O_Enode
   is
   begin
      return O_Enode (Build (CALL_EXPR,
                             Get_TREE_TYPE (Get_TREE_TYPE (Assocs.Subprg)),
                             Build_Function_Ptr (Assocs.Subprg),
                             Assocs.List.First, NULL_TREE));
   end New_Function_Call;

   procedure New_Procedure_Call (Assocs : in out O_Assoc_List)
   is
      Res : Tree;
   begin
      Res := Build (CALL_EXPR,
                    Get_TREE_TYPE (Get_TREE_TYPE (Assocs.Subprg)),
                    Build_Function_Ptr (Assocs.Subprg),
                    Assocs.List.First, NULL_TREE);
      Set_TREE_SIDE_EFFECTS (Res, C_True);
      Expand_Expr_Stmt (Res);
   end New_Procedure_Call;


   procedure New_Assign_Stmt (Target : O_Lnode; Value : O_Enode)
   is
      N : Tree;
   begin
      N := Build (MODIFY_EXPR, Get_TREE_TYPE (Tree (Target)),
                  Tree (Target), Tree (Value));
      Set_TREE_SIDE_EFFECTS (N, C_True);
      Expand_Expr_Stmt (N);
   end New_Assign_Stmt;

   procedure New_Return_Stmt (Value : O_Enode)
   is
      Assign : Tree;
   begin
      Assign := Build (MODIFY_EXPR, Get_TREE_TYPE (Tree (Value)),
                       Get_DECL_RESULT (Current_Function_Decl),
                       Tree (Value));
      Set_TREE_SIDE_EFFECTS (Assign, C_True);
      --Set_TREE_USED (Assign, True);
      Expand_Expr_Stmt (Assign);
      Expand_Return (Value);
   end New_Return_Stmt;

   procedure New_Return_Stmt
   is
   begin
      Expand_Null_Return;
   end New_Return_Stmt;

   procedure Start_If_Stmt (Block : in out O_If_Block; Cond : O_Enode)
   is
      pragma Unreferenced (Block);
   begin
      Expand_Start_Cond (Cond, C_False);
   end Start_If_Stmt;

   procedure New_Elsif_Stmt (Block : in out O_If_Block; Cond : O_Enode)
   is
      pragma Unreferenced (Block);
   begin
      Expand_Start_Elseif (Cond);
   end New_Elsif_Stmt;

   procedure New_Else_Stmt (Block : in out O_If_Block)
   is
      pragma Unreferenced (Block);
   begin
      Expand_Start_Else;
   end New_Else_Stmt;

   procedure Finish_If_Stmt (Block : in out O_If_Block)
   is
      pragma Unreferenced (Block);
   begin
      Expand_End_Cond;
   end Finish_If_Stmt;

   procedure Start_Loop_Stmt (Label : out O_Snode)
   is
   begin
      Label := Expand_Start_Loop (C_True);
      --  This is required to avoid crash with goto fixup.
      Expand_Start_Bindings (0);
   end Start_Loop_Stmt;

   procedure Finish_Loop_Stmt (Label : in out O_Snode)
   is
      pragma Unreferenced (Label);
   begin
      Expand_End_Bindings (NULL_TREE, C_False, C_False);
      Expand_End_Loop;
   end Finish_Loop_Stmt;

   procedure New_Exit_Stmt (L : O_Snode) is
   begin
      Expand_Exit_Loop (L);
   end New_Exit_Stmt;

   procedure New_Next_Stmt (L : O_Snode) is
   begin
      Expand_Continue_Loop (L);
   end New_Next_Stmt;

   procedure Start_Case_Stmt (Block : in out O_Case_Block; Value : O_Enode)
   is
   begin
      Expand_Start_Case (C_True, Value, Get_TREE_TYPE (Value), NULL_Chars);
      Block := O_Case_Block'(Expr => Tree (Value),
                           First => True, Label => NULL_TREE);
   end Start_Case_Stmt;

   procedure Start_Choice (Block : in out O_Case_Block)
   is
   begin
      if Block.First then
         Block.First := False;
      else
         --  Add a "break" statement.
         if Expand_Exit_Something = 0 then
            raise Type_Error;
         end if;
      end if;
      if Block.Label /= NULL_TREE then
         raise Syntax_Error;
      end if;
      Block.Label := Build_Decl (LABEL_DECL, NULL_TREE, NULL_TREE);
      --Pushdecl (Choices.Label);
   end Start_Choice;

   procedure New_Expr_Choice (Block : in out O_Case_Block; Expr : O_Cnode)
   is
      Duplicate : Tree;
   begin
      if Pushcase (Tree (Expr), Agcc.Trees.Convert'Address,
                   Block.Label, Duplicate'Address) /= 0 then
         raise Syntax_Error;
      end if;
   end New_Expr_Choice;

   procedure New_Range_Choice (Block : in out O_Case_Block;
                               Low, High : O_Cnode)
   is
      Duplicate : Tree;
   begin
      if Pushcase_Range (Tree (Low), Tree (High), Agcc.Trees.Convert'Address,
                         Tree (Block.Label), Duplicate'Address) /= 0
      then
         raise Syntax_Error;
      end if;
   end New_Range_Choice;

   procedure New_Default_Choice (Block : in out O_Case_Block)
   is
      Duplicate : Tree;
   begin
      if Pushcase (NULL_TREE, System.Null_Address,
                   Block.Label, Duplicate'Address) /= 0
      then
         raise Syntax_Error;
      end if;
   end New_Default_Choice;

   procedure Finish_Choice (Block : in out O_Case_Block)
   is
   begin
      Block.Label := NULL_TREE;
   end Finish_Choice;

   procedure Finish_Case_Stmt (Block: in out O_Case_Block) is
   begin
      Expand_End_Case_Type (Block.Expr, NULL_TREE);
   end Finish_Case_Stmt;
end Ortho_Gcc;
