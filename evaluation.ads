--  Evaluation of static expressions.
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Types; use Types;
with Iirs; use Iirs;

package Evaluation is

   --  Get the value of a physical integer literal or unit.
   function Get_Physical_Value (Expr : Iir) return Iir_Int64;

   --  Evaluate (ie compute) expression EXPR.
   --  EXPR is required to be a locally static expression, otherwise an error
   --  message is generated.
   --  The result is a literal.
   function Eval_Expr (Expr: Iir) return Iir;

   --  Same as Eval_Expr, but do not check that EXPR is locally static.
   --  May be used instead of Eval_Expr if you know than EXPR is locally
   --  static, or for literals of type std.time.
   function Eval_Static_Expr (Expr: Iir) return Iir;

   --  Same as Eval_Expr, but if EXPR is not locally static, the result is
   --  EXPR.  Also, if EXPR is null_iir, then null_iir is returned.
   --  The purpose of this function is to evaluate an expression only if it
   --  is locally static.
   function Eval_Expr_If_Static (Expr : Iir) return Iir;

   --  Return TRUE if literal EXPR is in SUB_TYPE bounds.
   function Eval_Is_In_Bound (Expr : Iir; Sub_Type : Iir) return Boolean;

   --  Emit an error if EXPR violates SUB_TYPE bounds.
   procedure Eval_Check_Bound (Expr : Iir; Sub_Type : Iir);

   --  Return TRUE if range expression A_RANGE is not included in SUB_TYPE.
   function Eval_Is_Range_In_Bound
     (A_Range : Iir; Sub_Type : Iir; Any_Dir : Boolean)
     return Boolean;

   --  Emit an error if A_RANGE is not included in SUB_TYPE.
   procedure Eval_Check_Range (A_Range : Iir; Sub_Type : Iir;
                               Any_Dir : Boolean);

   --  Same as Eval_Expr, but a range check with SUB_TYPE is performed after
   --  computation.
   function Eval_Expr_Check (Expr : Iir; Sub_Type : Iir) return Iir;

   --  Call Eval_Expr_Check only if EXPR is static.
   function Eval_Expr_Check_If_Static (Expr : Iir; Atype : Iir) return Iir;

   --  Return TRUE iff VAL belongs to BOUND.
   function Eval_Int_In_Range (Val : Iir_Int64; Bound : Iir) return Boolean;

   --  Return the length of the discrete range CONSTRAINT.
   function Eval_Discrete_Range_Length (Constraint : Iir) return Iir_Int64;

   --  Return the length of SUB_TYPE.
   function Eval_Discrete_Type_Length (Sub_Type : Iir) return Iir_Int64;

   --  Get the left bound of a range constraint.
   --  Note: the range constraint may be an attribute or a subtype.
   function Eval_Discrete_Range_Left (Constraint : Iir) return Iir;

   --  Return the range_expression of RNG, which is a range or a subtype.
   --  Return NULL_IIR if the range constraint is not a range_expression.
   function Eval_Range (Rng : Iir) return Iir;

   --  Return the position of EXPR, ie the result of sub_type'pos (EXPR), where
   --  sub_type is the type of expr.
   --  EXPR must be of a discrete subtype.
   function Eval_Pos (Expr : Iir) return Iir_Int64;

   --  Create an array subtype from LEN and BASE_TYPE, according to rules
   --  of LRM93 7.3.2.2. (which are the same as LRM93 7.2.4).
   function Create_Unidim_Array_By_Length
     (Base_Type : Iir; Len : Iir_Int64; Loc : Iir)
     return Iir_Array_Subtype_Definition;

   --  Create a subtype of A_TYPE whose length is LEN.
   --  This is used to create subtypes for strings or aggregates.
   function Create_Range_Subtype_By_Length
     (A_Type : Iir; Len : Iir_Int64; Loc : Location_Type)
     return Iir;

   --  Store into NAME_BUFFER,NAME_LENGTH the simple name, character literal
   --  or operator sumbol of ID, using the same format as SIMPLE_NAME
   --  attribute.
   procedure Eval_Simple_Name (Id : Name_Id);

   --  Compare two string literals (of same length).
   type Compare_Type is (Compare_Lt, Compare_Eq, Compare_Gt);
   function Compare_String_Literals (L, R : Iir) return Compare_Type;
end Evaluation;
