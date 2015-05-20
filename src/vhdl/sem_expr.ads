--  Semantic analysis.
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

package Sem_Expr is
   -- Set semantic to EXPR.
   --  Replace simple_name with the referenced node,
   --  Set type to nodes,
   --  Resolve overloading

   Deferred_Constant_Allowed : Boolean := False;

   -- Semantize an expression (other than a range) with a possible overloading.
   -- Sem_expression_ov (and therefore sem_expression) must be called *once*
   -- for each expression node with A_TYPE1 not null and at most *once* with
   -- A_TYPE1 null.
   --
   -- When A_TYPE1 is null, sem_expression_ov find all possible types
   -- of the expression.  If there is only one possible type (ie, overloading
   -- is non-existant or solved), then the type of the expression is set,
   -- and the node is completly semantized.  Sem_expression_ov must not
   -- be called for such a node.
   -- If there is several possible types (ie overloaded), then the type is
   -- set with a list of overload.  To finishes the semantisation,
   -- sem_expression_ov must be called again with A_TYPE1 set to the
   -- expected type.
   --
   -- If A_TYPE1 is set, sem_expression_ov must finishes the semantisation
   -- of the expression, and set its type, which is not necessary a base type.
   -- A_TYPE1 must be a base type.
   --
   -- In case of error, it displays a message and return null.
   -- In case of success, it returns the semantized expression, which can
   -- be different from EXPR (eg, a character literal is transformed into an
   -- enumeration literal).
   function Sem_Expression_Ov (Expr: Iir; A_Type1: Iir) return Iir;

   -- If A_TYPE is not null, then EXPR must be of type A_TYPE.
   -- Return null in case of error.
   function Sem_Expression (Expr: Iir; A_Type: Iir) return Iir;

   --  Same as Sem_Expression, but also implicitly choose an universal type
   --  if overloaded.
   function Sem_Expression_Universal (Expr : Iir) return Iir;

   --  Same as Sem_Expression but specialized for a case expression.
   --  (Handle specific overloading rules).
   function Sem_Case_Expression (Expr : Iir) return Iir;

   --  Sem COND as a condition.
   --  In VHDL08, this follows 9.2.9 Condition operator.
   --  In VHDL87 and 93, type of COND must be a boolean.
   --  A check is made that COND can be read.
   function Sem_Condition (Cond : Iir) return Iir;

   --  Same as Sem_Expression but knowing that the type of EXPR must be a
   --  composite type.  Used for expressions in assignment statement when the
   --  target is an aggregate.
   function Sem_Composite_Expression (Expr : Iir) return Iir;

   --  Return True iif INTER is allowed to be read.  Follow rules of
   --  LRM08 6.5.2 Interface object declarations.
   function Can_Interface_Be_Read (Inter : Iir) return Boolean;

   --  Return True iif INTER is allowed to be updated.  Follow rules of
   --  LRM08 6.5.2 Interface object declarations.
   function Can_Interface_Be_Updated (Inter : Iir) return Boolean;

   --  Check EXPR can be read.
   procedure Check_Read (Expr : Iir);

   --  Check EXPR can be updated.
   procedure Check_Update (Expr : Iir);

   --  Check the type of EXPR can be implicitly converted to TARG_TYPE, ie
   --  if TARG_TYPE is a constrained array subtype, number of elements matches.
   --  Return FALSE in case of error.
   --  If TARG_TYPE or EXPR is NULL_IIR, silently returns TRUE.
   function Check_Implicit_Conversion (Targ_Type : Iir; Expr : Iir)
                                      return Boolean;

   -- For a procedure call, A_TYPE must be null.
   function Sem_Subprogram_Call (Expr: Iir; A_Type: Iir) return Iir;

   --  If EXPR is a node for an expression, then return EXPR.
   --  Otherwise, emit an error message using LOC as location
   --   and return NULL_IIR.
   --  If EXPR is NULL_IIR, NULL_IIR is silently returned.
   function Check_Is_Expression (Expr : Iir; Loc : Iir) return Iir;

   --  Semantize a procedure_call or a concurrent_procedure_call_statement.
   --  A procedure call is not an expression but because most of the code
   --  for procedure call is common with function call, procedure calls are
   --  handled in this package.
   procedure Sem_Procedure_Call (Call : Iir_Procedure_Call; Stmt : Iir);

   --  Analyze a range (ie a range attribute or a range expression).  If
   --  ANY_DIR is true, the range can't be a null range (slice vs subtype,
   --  used in static evaluation). A_TYPE may be Null_Iir.
   --  Return Null_Iir in case of error, or EXPR analyzed (and evaluated if
   --  possible).
   function Sem_Range_Expression (Expr: Iir; A_Type: Iir; Any_Dir : Boolean)
     return Iir;

   --  Analyze a discrete range.  If ANY_DIR is true, the range can't be a
   --  null range (slice vs subtype -- used in static evaluation). A_TYPE may
   --  be Null_Iir. Return Null_Iir in case of error.
   function Sem_Discrete_Range_Expression
     (Expr: Iir; A_Type: Iir; Any_Dir: Boolean) return Iir;

   --  Semantize a discrete range and convert to integer if both bounds are
   --  universal integer types, according to rules of LRM 3.2.1.1
   function Sem_Discrete_Range_Integer (Expr: Iir) return Iir;

   --  Transform LIT into a physical_literal.
   --  LIT can be either a not semantized physical literal or
   --  a simple name that is a physical unit.  In the later case, a physical
   --  literal is created.
   function Sem_Physical_Literal (Lit: Iir) return Iir;

   --  CHOICES_LIST is a list of choices (none, expression, range, list or
   --    others).
   --  If IS_SUB_RANGE is true, then SUB_TYPE may not be fully convered,
   --    otherwise, SUB_TYPE must be fully covered.
   --    This is used when the subtype of an aggregate must be determined.
   --  SUB_TYPE is the discrete subtype.
   --  Emit a message if:
   --  * the SUB_TYPE is not fully covered by the choices
   --  * the choices are not mutually exclusif (an element is present twice)
   --  * OTHERS is not the last choice, or is present several times.
   --
   --  If there is at least one named choice, LOW and HIGH are set with the
   --  lowest and highest index.
   --  If LOW and HIGH are set, they are locally static.
   --
   --  Unidimensional strings are not handled here but by
   --  sem_string_choices_range.
   --
   --  TODO:
   --  * be smarter if only positional choices (do not create the list).
   --  * smarter messages.
   procedure Sem_Choices_Range
     (Choice_Chain : in out Iir;
      Sub_Type : Iir;
      Is_Sub_Range : Boolean;
      Is_Case_Stmt : Boolean;
      Loc : Location_Type;
      Low : out Iir;
      High : out Iir);

   --  Semantize CHOICE_LIST when the choice expression SEL is of a
   --  one-dimensional character array type.
   procedure Sem_String_Choices_Range (Choice_Chain : Iir; Sel : Iir);

   type Compatibility_Level is
     (Not_Compatible, Via_Conversion, Fully_Compatible);

   -- LEFT are RIGHT must be really a type (not a subtype).
   function Are_Basetypes_Compatible (Left: Iir; Right: Iir)
     return Compatibility_Level;

   --  Return TRUE iif types of LEFT and RIGHT are compatible.
   function Are_Nodes_Compatible (Left: Iir; Right: Iir)
     return Compatibility_Level;

   --  Return TRUE iff the type of EXPR is compatible with A_TYPE
   function Is_Expr_Compatible (A_Type : Iir; Expr : Iir)
                               return Compatibility_Level;

   --  LIST1, LIST2 are either a type node or an overload list of types.
   --  Return THE type which is compatible with LIST1 are LIST2.
   --  Return null_iir if there is no such type or if there are several types.
   function Search_Compatible_Type (List1, List2 : Iir) return Iir;
end Sem_Expr;
