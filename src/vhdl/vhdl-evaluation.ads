--  Evaluation of static expressions.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Evaluation is

   --  Evaluation is about compile-time computation of expressions, such as
   --  2 + 1 --> 3.  This is (of course) possible only with locally (and some
   --  globally) static expressions.  Evaluation is required during semantic
   --  analysis at many places (in fact those where locally static expression
   --  are required by the language).  For example, the type of O'Range (N)
   --  depends on N, so we need to evaluate N.
   --
   --  The result of evaluation is a literal (integer, enumeration, real,
   --  physical), a string or a simple aggregate.  For scalar types, the
   --  result is therefore normalized (there is only one kind of result), but
   --  for array types, the result isn't: in general it will be a string, but
   --  it may be a simple aggregate.  Strings are preferred (because they are
   --  more compact), but aren't possible in some cases.  For example, the
   --  evaluation of "Text" & NUL cannot be a string.
   --
   --  Some functions (like Eval_Static_Expr) simply returns a result (which
   --  may be a node of the expression), others returns a result and set the
   --  origin (Literal_Origin or Range_Origin) to remember the original
   --  expression that was evaluation.  The original expression is kept so that
   --  it is possible to print the original tree.

   --  Get the value of a physical integer literal or unit.  May propagate
   --  Constraint_Error.
   function Get_Physical_Value (Expr : Iir) return Int64;

   --  Get the parameter of an attribute, or 1 if doesn't exist.
   function Eval_Attribute_Parameter_Or_1 (Attr : Iir) return Natural;

   --  Evaluate the locally static expression EXPR (without checking that EXPR
   --  is locally static).  Return a literal or an aggregate, without setting
   --  the origin, and do not modify EXPR.  This can be used only to get the
   --  value of an expression, without replacing it.
   function Eval_Static_Expr (Expr: Iir) return Iir;

   --  Evaluate (ie compute) expression EXPR.
   --  EXPR is required to be a locally static expression, otherwise an error
   --  message is generated.
   --  The result is a literal with the origin set.
   function Eval_Expr (Expr: Iir) return Iir;

   --  Same as Eval_Expr, but if EXPR is not locally static, the result is
   --  EXPR.  Also, if EXPR is null_iir, then null_iir is returned.
   --  The purpose of this function is to evaluate an expression only if it
   --  is locally static.
   function Eval_Expr_If_Static (Expr : Iir) return Iir;

   --  Concatenate all the elements of OPERANDS.
   --  The first element of OPERANDS is the rightest one, the last the
   --  leftest one.  All the elements are concatenation operators.
   --  All the elements are static.
   function Eval_Concatenation (Operands : Iir_Array) return Iir;

   --  Evaluate a physical literal and return a normalized literal (using
   --  the primary unit as unit).
   function Eval_Physical_Literal (Expr : Iir) return Iir;

   --  Return TRUE if literal EXPR is in SUB_TYPE bounds.
   --  OVERFLOW is the value returned for overflow_literal.  The default is
   --   False because an overflow is never within the bounds (by definition).
   --   But if you use this function to report an error, you prefer to
   --   get True as you don't want to report a second error.
   function Eval_Is_In_Bound
     (Expr : Iir; Sub_Type : Iir; Overflow : Boolean := False) return Boolean;

   --  Emit an error if EXPR violates SUB_TYPE bounds.
   procedure Eval_Check_Bound (Expr : Iir; Sub_Type : Iir);

   --  Same as Eval_Expr, but a range check with SUB_TYPE is performed after
   --  computation.
   function Eval_Expr_Check (Expr : Iir; Sub_Type : Iir) return Iir;

   --  Call Eval_Expr_Check only if EXPR is static.
   function Eval_Expr_Check_If_Static (Expr : Iir; Atype : Iir) return Iir;

   --  For a locally static range RNG (a range expression, a range attribute
   --  or a name that denotes a type or a subtype) returns its corresponding
   --  locally static range_expression.  The bounds of the results are also
   --  literals.
   --  Return a range_expression or NULL_IIR for a non locally static range.
   function Eval_Static_Range (Rng : Iir) return Iir;

   --  Return a locally static range expression with the origin set for ARANGE.
   function Eval_Range (Arange : Iir) return Iir;

   --  If ARANGE is a locally static range, return locally static range
   --  expression (with the origin set), else return ARANGE.
   function Eval_Range_If_Static (Arange : Iir) return Iir;

   --  Emit an error if A_RANGE is not included in SUB_TYPE.  A_RANGE can be
   --  a range expression, a range attribute or a name that denotes a discrete
   --  type or subtype.  A_RANGE must be a locally static range.
   procedure Eval_Check_Range (A_Range : Iir; Sub_Type : Iir;
                               Any_Dir : Boolean);

   --  Return TRUE if A_RANGE is compatible with SUB_TYPE.  Compatibility is
   --  defined in LRM:
   --
   --  LRM08 5.2 Scalar types
   --  A range constraint is /compatible/ with a subtype if each bound of the
   --  range belongs to the subtype or if the range constraint defines a null
   --  range.
   function Eval_Is_Range_In_Bound
     (A_Range : Iir; Sub_Type : Iir; Any_Dir : Boolean)
     return Boolean;

   --  Return TRUE iff VAL belongs to BOUND.
   function Eval_Int_In_Range (Val : Int64; Bound : Iir) return Boolean;

   --  Return the length of the discrete range CONSTRAINT.
   function Eval_Discrete_Range_Length (Constraint : Iir) return Int64;

   --  Return the length of SUB_TYPE.
   function Eval_Discrete_Type_Length (Sub_Type : Iir) return Int64;

   --  Get the left bound of a range constraint.
   --  Note: the range constraint may be an attribute or a subtype.
   function Eval_Discrete_Range_Left (Constraint : Iir) return Iir;

   --  Return true iff RNG is a null range.
   function Eval_Is_Null_Discrete_Range (Rng : Iir) return Boolean;

   --  Return the position of EXPR, ie the result of sub_type'pos (EXPR), where
   --  sub_type is the type of expr.
   --  EXPR must be of a discrete subtype.
   function Eval_Pos (Expr : Iir) return Int64;

   --  Return True iff L and R (scalar literals) are equal.
   function Eval_Is_Eq (L, R : Iir) return Boolean;

   --  Replace ORIGIN (an overflow literal) with extreme positive value (if
   --  IS_POS is true) or extreme negative value.
   function Build_Extreme_Value (Is_Pos : Boolean; Origin : Iir) return Iir;

   --  Create a Iir_Kind_Overflow node of type EXPR_TYPE for ORIGIN.
   function Build_Overflow (Origin : Iir; Expr_Type : Iir) return Iir;

   --  Fill VECT with choices from CHOICES_CHAIN: each position of CHOICE_RANGE
   --  is associated with its corresponding choice from CHOICES_CHAIN.
   --  VECT bounds must be 0 .. Len - 1, where Len is the length of
   --  CHOICE_RANGE.
   procedure Build_Array_Choices_Vector
     (Vect : out Iir_Array; Choice_Range : Iir; Choices_Chain : Iir);

   --  Create an array subtype from LEN and BASE_TYPE, according to rules
   --  of LRM93 7.3.2.2. (which are the same as LRM93 7.2.4).
   function Create_Unidim_Array_By_Length
     (Base_Type : Iir; Len : Int64; Loc : Iir)
     return Iir_Array_Subtype_Definition;

   --  Create a subtype of A_TYPE whose length is LEN.
   --  This is used to create subtypes for strings or aggregates.
   function Create_Range_Subtype_By_Length
     (A_Type : Iir; Len : Int64; Loc : Location_Type)
     return Iir;

   --  Compute ATYPE'value (VALUE) using origin ORIG, but without checking
   --  bounds.
   function Eval_Value_Attribute
     (Value : String; Atype : Iir; Orig : Iir) return Iir;

   --  From one-dimensional array expression PREFIX extract element at
   --  offset OFF (from 0 to length - 1).  Note that the element is directly
   --  returned, not a copy of it (so it should be referenced if stored in
   --  the tree).
   function Eval_Indexed_Name_By_Offset (Prefix : Iir; Off : Iir_Index32)
                                        return Iir;

   --  Return the simple name, character literal or operator sumbol of ID,
   --  using the same format as SIMPLE_NAME attribute.
   function Eval_Simple_Name (Id : Name_Id) return String;

   --  Convert aggregate or string literal to a simple agggregate.
   function Eval_String_Literal (Str : Iir) return Iir;

   --  Compare two string literals (of same length).
   type Compare_Type is (Compare_Lt, Compare_Eq, Compare_Gt);
   function Compare_String_Literals (L, R : Iir) return Compare_Type;

   package String_Utils is
      type Str_Info (Is_String : Boolean := True) is record
         Len : Nat32;

         case Is_String is
            when True =>
               Id : String8_Id;
            when False =>
               --  A simple aggregate.  List of elements.
               List : Iir_Flist;
         end case;
      end record;

      --  Fill Res from EL.  This is used to speed up Lt and Eq operations.
      function Get_Str_Info (Expr : Iir) return Str_Info;

      --  Return the position of element IDX of STR.
      function Get_Pos (Str : Str_Info; Idx : Nat32) return Iir_Int32;
   end String_Utils;

   --  Return the local part of 'Instance_Name or 'Path_Name.
   type Path_Instance_Name_Type (Len : Natural) is record
      --  The node before suffix (entity, architecture or generate iterator).
      Path_Instance : Iir;

      --  The suffix
      Suffix : String (1 .. Len);
   end record;

   function Get_Path_Instance_Name_Suffix (Attr : Iir)
                                          return Path_Instance_Name_Type;

   --  Create a copy of VAL.
   function Copy_Constant (Val : Iir) return Iir;
end Vhdl.Evaluation;
