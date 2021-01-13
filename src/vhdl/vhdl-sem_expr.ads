--  Semantic analysis.
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

with Ada.Unchecked_Deallocation;
with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Sem_Expr is
   -- Set semantic to EXPR.
   --  Replace simple_name with the referenced node,
   --  Set type to nodes,
   --  Resolve overloading

   Deferred_Constant_Allowed : Boolean := False;

   --  Analyze an expression (other than a range) with a possible overloading.
   --  Sem_expression_ov (and therefore sem_expression) must be called *once*
   --  for each expression node with A_TYPE1 not null and at most *once* with
   --  A_TYPE1 null.
   --
   --  When A_TYPE1 is null, sem_expression_ov find all possible types
   --  of the expression.  If there is only one possible type (ie, overloading
   --  is non-existant or solved), then the type of the expression is set,
   --  and the node is completly analyzed.  Sem_expression_ov must not
   --  be called for such a node.
   --  If there is several possible types (ie overloaded), then the type is
   --  set with a list of overload.  To finishes the semantisation,
   --  sem_expression_ov must be called again with A_TYPE1 set to the
   --  expected type.
   --
   --  If A_TYPE1 is set, sem_expression_ov must finishes the analyze of the
   --  expression, and set its type, which is not necessary a base type.
   --  A_TYPE1 must be a base type.
   --
   --  In case of error, it displays a message and return null_iir.
   --  In case of success, it returns the analyzed expression, which can
   --  be different from EXPR (eg, a character literal is transformed into an
   --  enumeration literal).
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

   --  Sem COND as a condition.  COND must have not been analyzed.
   --  In VHDL08, this follows 9.2.9 Condition operator.
   --  In VHDL87 and 93, type of COND must be a boolean.
   --  A check is made that COND can be read.
   function Sem_Condition (Cond : Iir) return Iir;
   function Sem_Condition_Pass2 (Cond : Iir) return Iir;

   --  Insert a call to condition operator.
   function Insert_Condition_Operator (Cond : Iir) return Iir;

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

   -- For a procedure call, A_TYPE must be null.
   function Sem_Subprogram_Call (Expr: Iir; A_Type: Iir) return Iir;

   --  If EXPR is a node for an expression, then return EXPR.
   --  Otherwise, emit an error message using LOC as location
   --   and return NULL_IIR.
   --  If EXPR is NULL_IIR, NULL_IIR is silently returned.
   function Check_Is_Expression (Expr : Iir; Loc : Iir) return Iir;

   --  Analyze a procedure_call or a concurrent_procedure_call_statement.
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
   function Sem_Discrete_Range (Expr: Iir; A_Type: Iir; Any_Dir: Boolean)
                               return Iir;

   --  Analyze a discrete range and convert to integer if both bounds are
   --  universal integer types, according to rules of LRM 3.2.1.1
   function Sem_Discrete_Range_Integer (Expr: Iir) return Iir;

   --  Transform LIT into a physical_literal.
   --  LIT can be either a not analyzed physical literal or
   --  a simple name that is a physical unit.  In the later case, a physical
   --  literal is created.
   function Sem_Physical_Literal (Lit: Iir) return Iir;

   type Annex_Array is array (Natural range <>) of Int32;
   type Annex_Array_Acc is access Annex_Array;
   procedure Free_Annex_Array is new Ada.Unchecked_Deallocation
     (Annex_Array, Annex_Array_Acc);

   --  Various info and sorted array for choices.
   type Choice_Info_Type is record
      --  Number of choices by expression or by range.
      Nbr_Choices : Natural;

      --  Number of alternatives
      Nbr_Alternatives : Natural;

      --  Set to the others choice is present.
      Others_Choice : Iir;

      --  Array of sorted choices.
      Arr : Iir_Array_Acc;

      --  Allocated and deallocated by the user.  If not null, it will be
      --  reordered when ARR is sorted.
      Annex_Arr : Annex_Array_Acc;
   end record;

   --  Compute the number of locally static choices (excluding others) and
   --  set Has_Others.
   procedure Count_Choices (Info : out Choice_Info_Type; Choice_Chain : Iir);

   --  Allocate and fill INFO.ARR.
   procedure Fill_Choices_Array (Info : in out Choice_Info_Type;
                                 Choice_Chain : Iir);

   --  Sort INFO.ARR.  Only for one-dimensional strings.
   procedure Sort_String_Choices (Info : in out Choice_Info_Type);

   --  Likewise for discrete choices.
   procedure Sort_Discrete_Choices (Info : in out Choice_Info_Type);

   --  CHOICES_CHAIN is a chain of choices (none, expression, range or
   --    others).  It is an in-out as it may be mutated (from expression to
   --    range).
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
   procedure Sem_Choices_Range (Choice_Chain : in out Iir;
                                Choice_Type : Iir;
                                Low : out Iir;
                                High : out Iir;
                                Loc : Location_Type;
                                Is_Sub_Range : Boolean;
                                Is_Case_Stmt : Boolean);

   --  Check that the values of CHOICE_CHAIN are a continuous range, and
   --  extract the lower LOW and upper HIGH bound (useful to create the
   --  corresponding subtype).  The values must be of type SUB_TYPE, and if
   --  IS_SUB_RANGE True, they must be within SUB_TYPE.
   --  The choices must be locally static.
   --  If REORDER_CHOICES is true, CHOICE_CHAIN is ordered.
   procedure Sem_Check_Continuous_Choices (Choice_Chain : Iir;
                                           Choice_Type : Iir;
                                           Low : out Iir;
                                           High : out Iir;
                                           Loc : Location_Type;
                                           Is_Sub_Range : Boolean);

   --  Analyze CHOICE_LIST when the choice expression SEL is of a
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

   --  Return the intersection of LIST1 and LIST2.
   --  This function accept wildcard types.
   function Compatible_Types_Intersect (List1, List2 : Iir) return Iir;

   --  Return True if an expression is not analyzed (its type is not set).
   --  All expressions from the parser are not analyzed.
   function Is_Expr_Not_Analyzed (Expr : Iir) return Boolean;
   pragma Inline (Is_Expr_Not_Analyzed);

   --  Return True if an expression is fully analyzed: its type is set to
   --  either a type definition, or to an error type.
   --  Some expressions can be partially analyzed: either set to an overload
   --  list or to a wildcard type.
   function Is_Expr_Fully_Analyzed (Expr : Iir) return Boolean;
   pragma Inline (Is_Expr_Fully_Analyzed);

   --  Analyze EXPR using ATYPE.
   --  If EXPR is not analyzed, EXPR is analyzed using type constraints from
   --   ATYPE.
   --  If ATYPE is a defined type (neither an overload list nor a wildcard
   --  type), EXPR will be fully analyzed (possibly with an error).
   --  If EXPR is partially or fully analyzed, ATYPE must not be null_iir and
   --  it is checked with the types of EXPR.  EXPR may become fully analyzed.
   function Sem_Expression_Wildcard
     (Expr : Iir; Atype : Iir; Constrained : Boolean := False) return Iir;

   --  To be used after Sem_Expression_Wildcard to update list ATYPE of
   --  possible types.
   procedure Merge_Wildcard_Type (Expr : Iir; Atype : in out Iir);
end Vhdl.Sem_Expr;
