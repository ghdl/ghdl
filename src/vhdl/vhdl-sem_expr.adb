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

with Grt.Algos;
with Errorout; use Errorout;
with Name_Table;
with Std_Names;
with Str_Table;
with Flags; use Flags;

with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Vhdl.Sem_Scopes; use Vhdl.Sem_Scopes;
with Vhdl.Sem_Names; use Vhdl.Sem_Names;
with Vhdl.Sem;
with Vhdl.Sem_Types;
with Vhdl.Sem_Stmts; use Vhdl.Sem_Stmts;
with Vhdl.Sem_Assocs; use Vhdl.Sem_Assocs;
with Vhdl.Sem_Decls;
with Vhdl.Sem_Psl;
with Vhdl.Xrefs; use Vhdl.Xrefs;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Ieee.Numeric;

package body Vhdl.Sem_Expr is

   -- Replace type of TARGET by A_TYPE.
   -- If TARGET has already a type, it must be an overload list, and in this
   -- case, this list is freed, or it must be A_TYPE.
   -- A_TYPE can't be an overload list.
   --
   -- This procedure can be called in the second pass, when the type is known.
   procedure Replace_Type (Target: Iir; A_Type: Iir)
   is
      Old_Type: Iir;
   begin
      pragma Assert (not Is_Overload_List (A_Type));

      Old_Type := Get_Type (Target);
      if Old_Type /= Null_Iir then
         if Is_Overload_List (Old_Type) then
            Free_Iir (Old_Type);
         elsif Old_Type = A_Type then
            return;
         else
            -- Cannot replace an existing type by another one.
            raise Internal_Error;
         end if;
      end if;
      if A_Type = Null_Iir then
         return;
      end if;
      Set_Type (Target, A_Type);
   end Replace_Type;

   --  Return true if EXPR is overloaded, ie has several meanings.
   function Is_Overloaded (Expr : Iir) return Boolean
   is
      Expr_Type : constant Iir := Get_Type (Expr);
   begin
      return Expr_Type = Null_Iir or else Is_Overload_List (Expr_Type);
   end Is_Overloaded;

   --  Return the common type of base types LEFT and RIGHT.
   --  LEFT are RIGHT must be really base types (not subtypes).
   --  Roughly speaking, it returns LEFT (= RIGHT) if LEFT = RIGHT (ie, same
   --  type), null otherwise.
   --  However, it handles implicite conversions of universal types.
   function Get_Common_Basetype (Left: Iir; Right: Iir)
     return Iir is
   begin
      if Left = Right then
         return Left;
      end if;
      case Get_Kind (Left) is
         when Iir_Kind_Integer_Type_Definition =>
            if Right = Convertible_Integer_Type_Definition then
               return Left;
            elsif Left = Convertible_Integer_Type_Definition
              and then Get_Kind (Right) = Iir_Kind_Integer_Type_Definition
            then
               return Right;
            end if;
         when Iir_Kind_Floating_Type_Definition =>
            if Right = Convertible_Real_Type_Definition then
               return Left;
            elsif Left = Convertible_Real_Type_Definition
              and then Get_Kind (Right) = Iir_Kind_Floating_Type_Definition
            then
               return Right;
            end if;
         when others =>
            null;
      end case;
      return Null_Iir;
   end Get_Common_Basetype;

   -- LEFT are RIGHT must be really a type (not a subtype).
   function Are_Basetypes_Compatible (Left: Iir; Right: Iir)
     return Compatibility_Level is
   begin
      if Left = Right then
         return Fully_Compatible;
      end if;
      case Get_Kind (Left) is
         when Iir_Kind_Integer_Type_Definition =>
            if Right = Convertible_Integer_Type_Definition then
               if Left = Universal_Integer_Type_Definition then
                  return Fully_Compatible;
               else
                  return Via_Conversion;
               end if;
            elsif Left = Convertible_Integer_Type_Definition
              and then Get_Kind (Right) = Iir_Kind_Integer_Type_Definition
            then
               if Right = Universal_Integer_Type_Definition then
                  return Fully_Compatible;
               else
                  return Via_Conversion;
               end if;
            end if;
         when Iir_Kind_Floating_Type_Definition =>
            if Right = Convertible_Real_Type_Definition then
               if Left = Universal_Real_Type_Definition then
                  return Fully_Compatible;
               else
                  return Via_Conversion;
               end if;
            elsif Left = Convertible_Real_Type_Definition
              and then Get_Kind (Right) = Iir_Kind_Floating_Type_Definition
            then
               if Right = Universal_Real_Type_Definition then
                  return Fully_Compatible;
               else
                  return Via_Conversion;
               end if;
            end if;
         when Iir_Kind_Foreign_Vector_Type_Definition =>
            declare
               use Vhdl.Ieee.Std_Logic_1164;
               El_Type : Iir;
            begin
               if Right = Bit_Type_Definition
                 or else Right = Boolean_Type_Definition
                 or else Right = Bit_Vector_Type_Definition
                 or else Right = Std_Logic_Type
                 or else Right = Std_Ulogic_Type
               then
                  return Fully_Compatible;
               end if;
               if Get_Kind (Right) = Iir_Kind_Array_Type_Definition then
                  El_Type := Get_Base_Type (Get_Element_Subtype (Right));
                  if El_Type = Std_Logic_Type
                    or else El_Type = Std_Ulogic_Type
                    or else El_Type = Bit_Type_Definition
                  then
                     return Fully_Compatible;
                  end if;
               end if;
            end;
         when others =>
            null;
      end case;
      return Not_Compatible;
   end Are_Basetypes_Compatible;

   function Are_Types_Compatible (Left: Iir; Right: Iir)
                                 return Compatibility_Level is
   begin
      return Are_Basetypes_Compatible (Get_Base_Type (Left),
                                       Get_Base_Type (Right));
   end Are_Types_Compatible;

   function Are_Nodes_Compatible (Left: Iir; Right: Iir)
                                 return Compatibility_Level is
   begin
      return Are_Types_Compatible (Get_Type (Left), Get_Type (Right));
   end Are_Nodes_Compatible;

   --  Return TRUE iif LEFT_TYPE and RIGHT_TYPES are compatible. RIGHT_TYPES
   --  may be an overload list.
   function Compatibility_Types1 (Left_Type : Iir; Right_Types : Iir)
                                 return Compatibility_Level
   is
      El : Iir;
      Right_List : Iir_List;
      It : List_Iterator;
      Level : Compatibility_Level;
   begin
      pragma Assert (not Is_Overload_List (Left_Type));

      if Is_Overload_List (Right_Types) then
         Right_List := Get_Overload_List (Right_Types);
         Level := Not_Compatible;
         It := List_Iterate (Right_List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            Level := Compatibility_Level'Max
              (Level, Are_Types_Compatible (Left_Type, El));
            if Level = Fully_Compatible then
               return Fully_Compatible;
            end if;
            Next (It);
         end loop;
         return Level;
      else
         return Are_Types_Compatible (Left_Type, Right_Types);
      end if;
   end Compatibility_Types1;

   --  Return compatibility for nodes LEFT and RIGHT.
   --  LEFT is expected to be an interface of a function definition.
   --  Type of RIGHT can be an overload_list
   --  RIGHT might be implicitly converted to LEFT.
   function Compatibility_Nodes (Left : Iir; Right : Iir)
     return Compatibility_Level
   is
      Left_Type : constant Iir := Get_Base_Type (Get_Type (Left));
      Right_Type : constant Iir := Get_Type (Right);
   begin
      --  Check.
      case Get_Kind (Left_Type) is
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Array_Type_Definition =>
            null;
         when others =>
            Error_Kind ("compatibility_nodes", Left_Type);
      end case;

      return Compatibility_Types1 (Left_Type, Right_Type);
   end Compatibility_Nodes;

   function Is_String_Type (A_Type : Iir) return Boolean
   is
      Base_Type : constant Iir := Get_Base_Type (A_Type);
      El_Bt : Iir;
   begin
      --  LRM 7.3.1
      --  [...] the type of the literal must be a one-dimensional array ...
      if not Is_One_Dimensional_Array_Type (Base_Type) then
         return False;
      end if;
      --  LRM 7.3.1
      --  ... of a character type ...
      El_Bt := Get_Base_Type (Get_Element_Subtype (Base_Type));
      if Get_Kind (El_Bt) /= Iir_Kind_Enumeration_Type_Definition then
         return False;
      end if;
      --  FIXME: character type
      return True;
   end Is_String_Type;

   --  Return TRUE iff A_TYPE can be the type of string or bit string literal
   --  EXPR.  EXPR is needed to distinguish between string and bit string
   --  for VHDL87 rule about the type of a bit string.
   function Is_String_Literal_Type (A_Type : Iir; Expr : Iir) return Boolean
   is
      El_Bt : Iir;
   begin
      if not Is_String_Type (A_Type) then
         return False;
      end if;
      El_Bt := Get_Base_Type (Get_Element_Subtype (A_Type));
      --  LRM87 7.3.1
      --  ... (for string literals) or of type BIT (for bit string literals).
      if Flags.Vhdl_Std = Vhdl_87
        and then Get_Bit_String_Base (Expr) /= Base_None
        and then El_Bt /= Bit_Type_Definition
      then
         return False;
      end if;
      return True;
   end Is_String_Literal_Type;

   --  Return TRUE iff A_TYPE can be the type of an aggregate.
   function Is_Aggregate_Type (A_Type : Iir) return Boolean is
   begin
      --  LRM 7.3.2 Aggregates
      --  [...]  the type of the aggregate must be a composite type.
      case Get_Kind (Get_Base_Type (A_Type)) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Record_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Aggregate_Type;

   --  Return TRUE iff A_TYPE can be the type of a null literal.
   function Is_Null_Literal_Type (A_Type : Iir) return Boolean is
   begin
      --  LRM 7.3.1 Literals
      --  The literal NULL represents the null access value for any access
      --  type.
      return
        Get_Kind (Get_Base_Type (A_Type)) = Iir_Kind_Access_Type_Definition;
   end Is_Null_Literal_Type;

   --  Return TRUE iff A_TYPE can be the type of allocator EXPR.  Note that
   --  the allocator must have been analyzed.
   function Is_Allocator_Type (A_Type : Iir; Expr : Iir) return Boolean
   is
      Base_Type : constant Iir := Get_Base_Type (A_Type);
      Designated_Type : Iir;
   begin
      --  LRM 7.3.6 Allocators
      --  [...] the value returned is of an access type having the named
      --  designated type.

      if Get_Kind (Base_Type) /= Iir_Kind_Access_Type_Definition then
         return False;
      end if;
      Designated_Type := Get_Allocator_Designated_Type (Expr);
      pragma Assert (Designated_Type /= Null_Iir);
      --  Cheat: there is no allocators on universal types.
      return Get_Base_Type (Get_Designated_Type (Base_Type))
        = Get_Base_Type (Designated_Type);
   end Is_Allocator_Type;

   --  Return TRUE iff the type of EXPR is compatible with A_TYPE
   function Is_Expr_Compatible (A_Type : Iir; Expr : Iir)
                               return Compatibility_Level
   is
      Expr_Type : constant Iir := Get_Type (Expr);
      Is_Compatible : Boolean;
   begin
      if Expr_Type /= Null_Iir then
         return Compatibility_Types1 (A_Type, Expr_Type);
      end if;

      case Get_Kind (Expr) is
         when Iir_Kind_Aggregate =>
            Is_Compatible := Is_Aggregate_Type (A_Type);
         when Iir_Kind_String_Literal8 =>
            Is_Compatible := Is_String_Literal_Type (A_Type, Expr);
         when Iir_Kind_Null_Literal =>
            Is_Compatible := Is_Null_Literal_Type (A_Type);
         when Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype =>
            Is_Compatible := Is_Allocator_Type (A_Type, Expr);
         when Iir_Kind_Parenthesis_Expression =>
            return Is_Expr_Compatible (A_Type, Get_Expression (Expr));
         when others =>
            --  Error while EXPR was typed.  FIXME: should create an ERROR
            --  node?
            Is_Compatible := False;
      end case;
      if Is_Compatible then
         return Fully_Compatible;
      else
         return Not_Compatible;
      end if;
   end Is_Expr_Compatible;

   function Check_Is_Expression (Expr : Iir; Loc : Iir) return Iir
   is
   begin
      if Expr = Null_Iir then
         return Null_Iir;
      end if;
      case Get_Kind (Expr) is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kinds_Subtype_Definition
           | Iir_Kind_Design_Unit
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Library_Clause
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Signature
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Terminal_Declaration =>
            Error_Msg_Sem (+Loc, "%n not allowed in an expression", +Expr);
            return Null_Iir;
         when Iir_Kind_Function_Declaration =>
            return Expr;
         when Iir_Kind_Overload_List =>
            return Expr;
         when Iir_Kinds_Literal
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Enumeration_Literal =>
            return Expr;
         when Iir_Kinds_External_Name =>
            return Expr;
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Aggregate
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Overflow_Literal =>
            return Expr;
         when Iir_Kinds_Dyadic_Operator
           | Iir_Kinds_Monadic_Operator =>
            return Expr;
         when Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kinds_Expression_Attribute
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Parenthesis_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Function_Call =>
            return Expr;
         when Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell
           | Iir_Kind_Psl_Onehot
           | Iir_Kind_Psl_Onehot0 =>
            return Expr;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name =>
            return Expr;
         when Iir_Kind_Error =>
            return Expr;
         when others =>
            Error_Kind ("check_is_expression", Expr);
            --N := Get_Type (Expr);
            --return Expr;
      end case;
   end Check_Is_Expression;

   -- Find a type compatible with A_TYPE in TYPE_LIST (which can be an
   -- overload list or a simple type) and return it.
   -- In case of failure, return null.
   function Search_Overloaded_Type (Type_List: Iir; A_Type: Iir)
     return Iir
   is
      Type_List_List : Iir_List;
      It : List_Iterator;
      El: Iir;
      Com : Iir;
      Res : Iir;
   begin
      if not Is_Overload_List (Type_List) then
         return Get_Common_Basetype (Get_Base_Type (Type_List),
                                     Get_Base_Type (A_Type));
      else
         Type_List_List := Get_Overload_List (Type_List);
         Res := Null_Iir;
         It := List_Iterate (Type_List_List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            Com := Get_Common_Basetype (Get_Base_Type (El),
                                        Get_Base_Type (A_Type));
            if Com /= Null_Iir then
               if Res = Null_Iir then
                  Res := Com;
               else
                  --  Several compatible types.
                  return Null_Iir;
               end if;
            end if;
            Next (It);
         end loop;
         return Res;
      end if;
   end Search_Overloaded_Type;

   --  LIST1, LIST2 are either a type node or an overload list of types.
   --  Return THE type which is compatible with LIST1 are LIST2.
   --  Return null_iir if there is no such type or if there are several types.
   function Search_Compatible_Type (List1, List2 : Iir) return Iir
   is
      List1_List : Iir_List;
      It : List_Iterator;
      Res : Iir;
      El : Iir;
      Tmp : Iir;
   begin
      if Is_Overload_List (List1) then
         List1_List := Get_Overload_List (List1);
         Res := Null_Iir;
         It := List_Iterate (List1_List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            Tmp := Search_Overloaded_Type (List2, El);
            if Tmp /= Null_Iir then
               if Res = Null_Iir then
                  Res := Tmp;
               else
                  --  Several types match.
                  return Null_Iir;
               end if;
            end if;
            Next (It);
         end loop;
         return Res;
      else
         return Search_Overloaded_Type (List2, List1);
      end if;
   end Search_Compatible_Type;

   -- Analyze the range expression EXPR.
   -- If A_TYPE is not null_iir, EXPR is expected to be of type A_TYPE.
   -- LRM93 3.2.1.1
   -- FIXME: avoid to run it on an already analyzed node, be careful
   --  with range_type_expr.
   function Sem_Simple_Range_Expression
     (Expr: Iir_Range_Expression; A_Type: Iir; Any_Dir : Boolean)
      return Iir_Range_Expression
   is
      Base_Type: Iir;
      Left, Right: Iir;
      Left_Type, Right_Type : Iir;
      Expr_Type : Iir;
   begin
      Expr_Type := Get_Type (Expr);
      Left := Get_Left_Limit_Expr (Expr);
      Right := Get_Right_Limit_Expr (Expr);

      if Expr_Type = Null_Iir then
         --  Pass 1.

         if A_Type = Null_Iir then
            Base_Type := Null_Iir;
         else
            Base_Type := Get_Base_Type (A_Type);
         end if;

         --  Analyze left and right bounds.
         Right := Sem_Expression_Ov (Right, Base_Type);
         Left := Sem_Expression_Ov (Left, Base_Type);

         if Left = Null_Iir or else Right = Null_Iir then
            if A_Type /= Null_Iir then
               --  Can continue with the error.
               if Left = Null_Iir then
                  Left := Create_Error_Expr
                    (Get_Left_Limit_Expr (Expr), A_Type);
               end if;
               if Right = Null_Iir then
                  Right := Create_Error_Expr
                    (Get_Right_Limit_Expr (Expr), A_Type);
               end if;
            else
               --  Error.
               return Null_Iir;
            end if;
         end if;

         Left_Type := Get_Type (Left);
         Right_Type := Get_Type (Right);
         --  Check for string or aggregate literals
         --  FIXME: improve error message
         if Left_Type = Null_Iir then
            Error_Msg_Sem (+Left, "bad expression for a scalar");
            return Null_Iir;
         end if;
         if Right_Type = Null_Iir then
            Error_Msg_Sem (+Right, "bad expression for a scalar");
            return Null_Iir;
         end if;

         if Is_Overload_List (Left_Type)
           or else Is_Overload_List (Right_Type)
         then
            if Base_Type /= Null_Iir then
               --  Cannot happen, since sem_expression_ov should resolve
               --  ambiguties if a type is given.
               raise Internal_Error;
            end if;

            --  Try to find a common type.
            Expr_Type := Search_Compatible_Type (Left_Type, Right_Type);
            if Expr_Type = Null_Iir then
               if Compatibility_Types1 (Universal_Integer_Type_Definition,
                                        Left_Type) /= Not_Compatible
                 and then
                 Compatibility_Types1 (Universal_Integer_Type_Definition,
                                       Right_Type) /= Not_Compatible
               then
                  Expr_Type := Universal_Integer_Type_Definition;
               elsif Compatibility_Types1 (Universal_Real_Type_Definition,
                                           Left_Type) /= Not_Compatible
                 and then
                 Compatibility_Types1 (Universal_Real_Type_Definition,
                                       Right_Type) /= Not_Compatible
               then
                  Expr_Type := Universal_Real_Type_Definition;
               else
                  --  FIXME: handle overload
                  Error_Msg_Sem
                    (+Expr,
                     "left and right expressions of range are not compatible");
                  return Null_Iir;
               end if;
            end if;
            Left := Sem_Expression (Left, Expr_Type);
            Right := Sem_Expression (Right, Expr_Type);
            if Left = Null_Iir or else Right = Null_Iir then
               return Null_Iir;
            end if;
         else
            Expr_Type := Get_Common_Basetype (Get_Base_Type (Left_Type),
                                              Get_Base_Type (Right_Type));
            if Expr_Type = Null_Iir then
               Error_Msg_Sem
                 (+Expr,
                  "left and right expressions of range are not compatible");
               return Null_Iir;
            end if;
         end if;

         --  The type of the range is known, finish analysis.
      else
         --  Second call.

         pragma Assert (A_Type /= Null_Iir);

         if Is_Overload_List (Expr_Type) then
            --  FIXME: resolve overload
            raise Internal_Error;
         else
            if Are_Types_Compatible (Expr_Type, A_Type) = Not_Compatible then
               Error_Msg_Sem
                 (+Expr, "type of range doesn't match expected type");
               return Null_Iir;
            end if;

            return Expr;
         end if;
      end if;

      Check_Read (Left);
      Check_Read (Right);

      Left := Eval_Expr_If_Static (Left);
      Right := Eval_Expr_If_Static (Right);

      Set_Left_Limit_Expr (Expr, Left);
      Set_Right_Limit_Expr (Expr, Right);

      Set_Left_Limit (Expr, Left);
      Set_Right_Limit (Expr, Right);

      Set_Expr_Staticness (Expr, Min (Get_Expr_Staticness (Left),
                                      Get_Expr_Staticness (Right)));

      if A_Type /= Null_Iir then
         if Are_Types_Compatible (Expr_Type, A_Type) = Not_Compatible then
            Error_Msg_Sem (+Expr, "type of range doesn't match expected type");
            return Null_Iir;
         end if;

         --  Use A_TYPE for the type of the expression.
         Expr_Type := A_Type;
      end if;

      Set_Type (Expr, Expr_Type);
      if Get_Kind (Expr_Type)
        not in Iir_Kinds_Scalar_Type_And_Subtype_Definition
      then
         Error_Msg_Sem (+Expr, "type of range is not a scalar type");
         return Null_Iir;
      end if;

      if Get_Expr_Staticness (Expr) = Locally
        and then Get_Type_Staticness (Expr_Type) = Locally
        and then Get_Kind (Expr_Type) in Iir_Kinds_Subtype_Definition
      then
         Eval_Check_Range (Expr, Expr_Type, Any_Dir);
      end if;

      return Expr;
   end Sem_Simple_Range_Expression;

   -- The result can be:
   --  a subtype definition
   --  a range attribute
   --  a range type definition
   -- LRM93 3.2.1.1
   -- FIXME: avoid to run it on an already analyzed node, be careful
   --  with range_type_expr.
   function Sem_Range_Expression (Expr: Iir; A_Type: Iir; Any_Dir : Boolean)
                                 return Iir
   is
      Res : Iir;
      Res_Type : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            Res := Sem_Simple_Range_Expression (Expr, A_Type, Any_Dir);
            return Res;

         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Parenthesis_Name =>
            if Get_Named_Entity (Expr) = Null_Iir then
               Sem_Name (Expr);
            end if;
            Res := Name_To_Range (Expr);
            if Is_Error (Res) then
               return Null_Iir;
            end if;

            case Get_Kind (Res) is
               when Iir_Kind_Simple_Name
                 | Iir_Kind_Selected_Name =>
                  pragma Assert (Get_Kind (Get_Named_Entity (Res))
                                   in Iir_Kinds_Type_Declaration);
                  Res_Type := Get_Type (Get_Named_Entity (Res));
               when Iir_Kind_Range_Array_Attribute
                 | Iir_Kind_Reverse_Range_Array_Attribute =>
                  Res_Type := Get_Type (Res);
               when others =>
                  Error_Msg_Sem (+Expr, "name must denote a range");
                  return Null_Iir;
            end case;
            if A_Type /= Null_Iir
              and then Get_Base_Type (Res_Type) /= Get_Base_Type (A_Type)
            then
               Error_Not_Match (Expr, A_Type);
               return Null_Iir;
            end if;

         when others =>
            Error_Msg_Sem (+Expr, "range expression required");
            return Null_Iir;
      end case;

      if Get_Kind (Res_Type)
        not in Iir_Kinds_Scalar_Type_And_Subtype_Definition
      then
         Error_Msg_Sem (+Expr, "%n is not a range type", +Res);
         return Null_Iir;
      end if;

      Res := Eval_Range_If_Static (Res);

      if A_Type /= Null_Iir
        and then Get_Type_Staticness (A_Type) = Locally
        and then Get_Kind (A_Type) in Iir_Kinds_Subtype_Definition
      then
         if Get_Expr_Staticness (Res) = Locally then
            Eval_Check_Range (Res, A_Type, Any_Dir);
         end if;
      end if;
      return Res;
   end Sem_Range_Expression;

   function Sem_Discrete_Range (Expr: Iir; A_Type: Iir; Any_Dir : Boolean)
                               return Iir
   is
      Res : Iir;
      Res_Type : Iir;
   begin
      if Get_Kind (Expr) = Iir_Kind_Subtype_Definition then
         Res := Sem_Types.Sem_Subtype_Indication (Expr);
         if Res = Null_Iir then
            return Null_Iir;
         end if;

         Res_Type := Res;
         if A_Type /= Null_Iir
           and then (Are_Types_Compatible
                       (A_Type, Get_Type_Of_Subtype_Indication (Res))
                       = Not_Compatible)
         then
            --  A_TYPE is known when analyzing an index_constraint within
            --  a subtype indication.
            Error_Msg_Sem (+Expr, "subtype %n doesn't match expected type %n",
                           (+Res, +A_Type));
            --  FIXME: override type of RES ?
         end if;
      else
         Res := Sem_Range_Expression (Expr, A_Type, Any_Dir);

         if Res = Null_Iir then
            return Null_Iir;
         end if;

         Res_Type := Get_Type (Res);
      end if;

      --  Check the type is discrete.
      if Get_Kind (Res_Type) not in Iir_Kinds_Discrete_Type_Definition then
         if Get_Kind (Res_Type) /= Iir_Kind_Error then
            --  FIXME: avoid that test with error.
            if Get_Kind (Res) not in Iir_Kinds_Denoting_Name then
               Error_Msg_Sem (+Res, "range is not discrete");
            else
               Error_Msg_Sem
                 (+Expr, "%n is not a discrete range type", +Res);
            end if;
         end if;
         return Null_Iir;
      end if;

      return Res;
   end Sem_Discrete_Range;

   function Sem_Discrete_Range_Integer (Expr: Iir) return Iir
   is
      Res : Iir;
      Range_Type : Iir;
   begin
      Res := Sem_Discrete_Range (Expr, Null_Iir, True);
      if Res = Null_Iir then
         return Null_Iir;
      end if;
      if Get_Kind (Expr) /= Iir_Kind_Range_Expression then
         return Res;
      end if;

      Range_Type := Get_Type (Res);
      if Range_Type = Convertible_Integer_Type_Definition then
         --  LRM 3.2.1.1  Index constraints and discrete ranges
         --  For a discrete range used in a constrained array
         --  definition and defined by a range, an implicit
         --  conversion to the predefined type INTEGER is assumed
         --  if each bound is either a numeric literal or an
         --  attribute, and the type of both bounds (prior to the
         --  implicit conversion) is the type universal_integer.

         --  FIXME: catch phys/phys.
         Set_Type (Res, Integer_Type_Definition);
         if Get_Expr_Staticness (Res) = Locally then
            Eval_Check_Range (Res, Integer_Subtype_Definition, True);
         end if;
      elsif Range_Type = Universal_Integer_Type_Definition then
         if Vhdl_Std >= Vhdl_08 then
            --  LRM08 5.3.2.2
            --  For a discrete range used in a constrained array definition
            --  and defined by a range, an implicit conversion to the
            --  predefined type INTEGER is assumed if the type of both bounds
            --  (prior the implicit conversion) is the type universal_integer.
            null;
         elsif Flag_Relaxed_Rules then
            null;
         elsif Vhdl_Std /= Vhdl_93 then
            --  GHDL: this is not allowed, however often used:
            --  eg: for i in 0 to v'length + 1 loop
            --  eg: for i in -1 to 1 loop

            --  Be tolerant.
            Warning_Msg_Sem (Warnid_Universal, +Res,
                             "universal integer bound must be numeric literal "
                               & "or attribute");
         else
            Error_Msg_Sem (+Res, "universal integer bound must be numeric "
                             & "literal or attribute");
         end if;
         Set_Type (Res, Integer_Type_Definition);
      end if;
      return Res;
   end Sem_Discrete_Range_Integer;

   function Is_Ieee_Operation (Imp : Iir) return Boolean
   is
      use Std_Names;
      Parent : Iir;
   begin
      pragma Assert (Get_Kind (Imp) = Iir_Kind_Function_Declaration);

      --  TODO: remove this code so that all operations are allowed (and not
      --   only operators).
      case Get_Identifier (Imp) is
         when Name_Id_Operators
           | Name_Word_Operators
           | Name_Logical_Operators =>
            null;
         when others =>
            --  Not an operator.
            return False;
      end case;

      --  TODO: numeric_bit, numeric_bit_unsigned, numeric_std_unsigned.
      Parent := Get_Parent (Imp);
      return Parent = Vhdl.Ieee.Numeric.Numeric_Std_Pkg
        or Parent = Vhdl.Ieee.Std_Logic_1164.Std_Logic_1164_Pkg;
   end Is_Ieee_Operation;

   procedure Set_Function_Call_Staticness (Expr : Iir; Imp : Iir)
   is
      Staticness : Iir_Staticness;
   begin
      --  LRM93 7.4.1 (Locally Static Primaries)
      --  4. a function call whose function name denotes an implicitly
      --     defined operator, and whose actual parameters are each
      --     locally static expressions;
      --
      --  LRM93 7.4.2 (Globally Static Primaries)
      --  9. a function call whose function name denotes a pure function,
      --     and whose actual parameters are each globally static
      --     expressions.
      --
      --  LRM08 9.4.2 Locally statuc primaries
      --  [...] if every operator in the expression denotes [...] an operator
      --  defined in one of the packages STD_LOGIC_1164, NUMERIC_BIT,
      --  NUMERIC_STD, NUMERIC_BIT_UNSIGNED or NUMERIC_STD_UNSIGNED in library
      --  IEEE, and if every primary in the expression is a locally static
      --  primary, where a locally static primary is defined to be one of the
      --  following:
      --  [...]
      --  e) A function call whose function name denotes an implicitely
      --    defined operation or an operation defined in one of the packages
      --    STD_LOGIC_1164, NUMERIC_BIT, NUMERIC_STD, NUMERIC_BIT_UNSIGNED,
      --    or NUMERIC_STD_UNSIGNED in library IEEE and whose actual
      --    parameters are each locally static expressions.
      --
      --  GHDL note: operation is defined in:
      --  LRM08 5 Types
      --  The set of operations of a type includes the explicitly declared
      --  subprograms that have a parameter of result of the type.  The
      --  remaining operations of a type are the basic operations and the
      --  predefined operations.
      case Get_Kind (Expr) is
         when Iir_Kinds_Monadic_Operator =>
            Staticness := Get_Expr_Staticness (Get_Operand (Expr));
         when Iir_Kinds_Dyadic_Operator =>
            Staticness := Min (Get_Expr_Staticness (Get_Left (Expr)),
                               Get_Expr_Staticness (Get_Right (Expr)));
         when Iir_Kind_Function_Call =>
            Staticness := Locally;
            declare
               Assoc : Iir;
            begin
               Assoc := Get_Parameter_Association_Chain (Expr);
               while Assoc /= Null_Iir loop
                  if Get_Kind (Assoc)
                    = Iir_Kind_Association_Element_By_Expression
                  then
                     Staticness := Min
                       (Get_Expr_Staticness (Get_Actual (Assoc)),
                        Staticness);
                  end if;
                  Assoc := Get_Chain (Assoc);
               end loop;
            end;
         when Iir_Kind_Procedure_Call =>
            return;
         when others =>
            Error_Kind ("set_function_call_staticness (1)", Expr);
      end case;

      --  Staticness.
      case Get_Kind (Imp) is
         when Iir_Kind_Function_Declaration =>
            case Get_Implicit_Definition (Imp) is
               when Iir_Predefined_Error =>
                  raise Internal_Error;
               when Iir_Predefined_Pure_Functions =>
                  null;
               when Iir_Predefined_Impure_Functions =>
                  --  Predefined functions such as Now, Endfile are not static.
                  Staticness := None;
               when Iir_Predefined_Explicit =>
                  if Vhdl_Std >= Vhdl_08
                    and then Is_Ieee_Operation (Imp)
                  then
                     null;
                  elsif Get_Pure_Flag (Imp) then
                     Staticness := Min (Staticness, Globally);
                  else
                     Staticness := None;
                  end if;
            end case;
         when Iir_Kind_Interface_Function_Declaration =>
            Staticness := None;
         when others =>
            Error_Kind ("set_function_call_staticness", Imp);
      end case;
      Set_Expr_Staticness (Expr, Staticness);
   end Set_Function_Call_Staticness;

   --  Add CALLEE in the callees list of SUBPRG (which must be a subprg decl).
   procedure Add_In_Callees_List (Subprg : Iir; Callee : Iir)
   is
      Holder : constant Iir := Get_Callees_List_Holder (Subprg);
      List : Iir_List;
   begin
      List := Get_Callees_List (Holder);
      if List = Null_Iir_List then
         List := Create_Iir_List;
         Set_Callees_List (Holder, List);
      end if;
      --  FIXME: May use a flag in IMP to speed up the
      --  add operation.
      Add_Element (List, Callee);
   end Add_In_Callees_List;

   --  Check purity rules when SUBPRG calls CALLEE.
   --  Both SUBPRG and CALLEE are subprogram declarations.
   --  Update purity_state/impure_depth of SUBPRG if it is a procedure.
   procedure Sem_Call_Purity_Check (Subprg : Iir; Callee : Iir; Loc : Iir) is
   begin
      if Callee = Subprg then
         return;
      end if;

      --  Handle easy cases.
      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration =>
            if not Get_Pure_Flag (Subprg) then
               return;
            end if;
         when Iir_Kind_Procedure_Declaration =>
            if Get_Purity_State (Subprg) = Impure then
               return;
            end if;
         when Iir_Kinds_Process_Statement =>
            return;
         when others =>
            Error_Kind ("sem_call_purity_check(0)", Subprg);
      end case;

      case Get_Kind (Callee) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            if Get_Pure_Flag (Callee) then
               --  Pure functions may be called anywhere.
               return;
            end if;
            --  CALLEE is impure.
            case Get_Kind (Subprg) is
               when Iir_Kind_Function_Declaration =>
                  Error_Pure (Semantic, Subprg, Callee, Loc);
               when Iir_Kind_Procedure_Declaration =>
                  Set_Purity_State (Subprg, Impure);
               when others =>
                  Error_Kind ("sem_call_purity_check(1)", Subprg);
            end case;
         when Iir_Kind_Procedure_Declaration =>
            declare
               Depth : Iir_Int32;
               Callee_Body : constant Iir := Get_Subprogram_Body (Callee);
               Subprg_Body : constant Iir := Get_Subprogram_Body (Subprg);
            begin
               --  Get purity depth of callee, if possible.
               case Get_Purity_State (Callee) is
                  when Pure =>
                     return;
                  when Impure =>
                     Depth := Iir_Depth_Impure;
                  when Maybe_Impure =>
                     if Callee_Body = Null_Iir then
                        --  Cannot be 'maybe_impure' if no body!
                        raise Internal_Error;
                     end if;
                     Depth := Get_Impure_Depth (Callee_Body);
                  when Unknown =>
                     --  Add in list.
                     Add_In_Callees_List (Subprg, Callee);

                     if Callee_Body /= Null_Iir then
                        Depth := Get_Impure_Depth (Callee_Body);
                     else
                        return;
                     end if;
               end case;
               case Get_Kind (Subprg) is
                  when Iir_Kind_Function_Declaration =>
                     if Depth = Iir_Depth_Impure then
                        Error_Pure (Semantic, Subprg, Callee, Loc);
                     else
                        if Depth < Get_Subprogram_Depth (Subprg) then
                           Error_Pure (Semantic, Subprg, Callee, Loc);
                        end if;
                     end if;
                  when Iir_Kind_Procedure_Declaration =>
                     if Depth = Iir_Depth_Impure then
                        Set_Purity_State (Subprg, Impure);
                        --  FIXME: free callee list ? (wait state).
                     else
                        --  Set depth to the worst.
                        if Depth < Get_Impure_Depth (Subprg_Body) then
                           Set_Impure_Depth (Subprg_Body, Depth);
                        end if;
                     end if;
                  when others =>
                     Error_Kind ("sem_call_purity_check(2)", Subprg);
               end case;
            end;
         when Iir_Kind_Interface_Procedure_Declaration =>
            --  We have no idea about this procedure.
            null;
         when others =>
            Error_Kind ("sem_call_purity_check", Callee);
      end case;
   end Sem_Call_Purity_Check;

   procedure Sem_Call_Wait_Check (Subprg : Iir; Callee : Iir; Loc : Iir)
   is
      procedure Error_Wait is
      begin
         Report_Start_Group;
         Error_Msg_Sem
           (+Loc, "%n must not contain wait statement, but calls",
            (1 => +Subprg));
         Error_Msg_Sem
           (+Callee, "%n which has (indirectly) a wait statement", +Callee);
         Report_End_Group;
      end Error_Wait;
   begin
      pragma Assert (Get_Kind (Callee) = Iir_Kind_Procedure_Declaration);

      case Get_Wait_State (Callee) is
         when False =>
            return;
         when True =>
            null;
         when Unknown =>
            Add_In_Callees_List (Subprg, Callee);
            return;
      end case;

      --  LRM 8.1
      --  It is an error if a wait statement appears [...] in a procedure that
      --  has a parent that is a function subprogram.
      --
      --  Furthermore, it is an error if a wait statement appears [...] in a
      --  procedure that has a parent that is such a process statement.
      case Get_Kind (Subprg) is
         when Iir_Kind_Sensitized_Process_Statement =>
            Error_Wait;
            return;
         when Iir_Kind_Process_Statement =>
            return;
         when Iir_Kind_Function_Declaration =>
            Error_Wait;
            return;
         when Iir_Kind_Procedure_Declaration =>
            if Is_Subprogram_Method (Subprg) then
               Error_Wait;
            else
               Set_Wait_State (Subprg, True);
            end if;
         when others =>
            Error_Kind ("sem_call_wait_check", Subprg);
      end case;
   end Sem_Call_Wait_Check;

   procedure Sem_Call_All_Sensitized_Check
     (Subprg : Iir; Callee : Iir; Loc : Iir)
   is
   begin
      --  No need to deal with 'process (all)' if standard predates it.
      if Vhdl_Std < Vhdl_08 then
         return;
      end if;

      --  If subprogram called is pure, then there is no signals reference.
      case Get_Kind (Callee) is
         when Iir_Kind_Function_Declaration =>
            if Get_Pure_Flag (Callee) then
               return;
            end if;
         when Iir_Kind_Procedure_Declaration =>
            if Get_Purity_State (Callee) = Pure then
               return;
            end if;
         when Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            --  FIXME: how to compute sensitivity ?  Recurse ?
            return;
         when others =>
            Error_Kind ("sem_call_all_sensitized_check", Callee);
      end case;

      case Get_All_Sensitized_State (Callee) is
         when Invalid_Signal =>
            case Get_Kind (Subprg) is
               when Iir_Kind_Sensitized_Process_Statement =>
                  if Get_Sensitivity_List (Subprg) = Iir_List_All then
                     --  LRM08 11.3
                     --
                     --  It is an error if a process statement with the
                     --  reserved word ALL as its process sensitivity list
                     --  is the parent of a subprogram declared in a design
                     --  unit other than that containing the process statement
                     --  and the subprogram reads an explicitly declared
                     --  signal that is not a formal signal parameter or
                     --  member of a formal signal parameter of the
                     --  subprogram or of any of its parents.  Similarly,
                     --  it is an error if such subprogram reads an implicit
                     --  signal whose explicit ancestor is not a formal signal
                     --  parameter or member of a formal parameter of
                     --  the subprogram or of any of its parents.
                     Report_Start_Group;
                     Error_Msg_Sem (+Loc, "all-sensitized %n can't call %n",
                                    (+Subprg, +Callee));
                     Error_Msg_Sem
                       (+Loc,
                        " (as this subprogram reads (indirectly) a signal)");
                     Report_End_Group;
                  end if;
               when Iir_Kind_Process_Statement =>
                  return;
               when Iir_Kind_Function_Declaration
                 | Iir_Kind_Procedure_Declaration =>
                  Set_All_Sensitized_State (Subprg, Invalid_Signal);
               when others =>
                  Error_Kind ("sem_call_all_sensitized_check", Subprg);
            end case;
         when Read_Signal =>
            --  Put this subprogram in callees list as it may read a signal.
            --  Used by canon to build the sensitivity list.
            Add_In_Callees_List (Subprg, Callee);
            if Get_Kind (Subprg) in Iir_Kinds_Subprogram_Declaration then
               if Get_All_Sensitized_State (Subprg) < Read_Signal then
                  Set_All_Sensitized_State (Subprg, Read_Signal);
               end if;
            end if;
         when Unknown =>
            --  Put this subprogram in callees list as it may read a signal.
            --  Used by canon to build the sensitivity list.
            Add_In_Callees_List (Subprg, Callee);
         when No_Signal =>
            null;
      end case;
   end Sem_Call_All_Sensitized_Check;

   --  Set IMP as the implementation to being called by EXPR.
   --  If the context is a subprogram or a process (ie, if current_subprogram
   --  is not NULL), then mark IMP as callee of current_subprogram, and
   --  update states.
   procedure Sem_Subprogram_Call_Finish (Expr : Iir; Imp : Iir)
   is
      Subprg : constant Iir := Get_Current_Subprogram;
   begin
      Set_Function_Call_Staticness (Expr, Imp);
      Sem_Decls.Mark_Subprogram_Used (Imp);

      --  Check purity/wait/passive.

      if Subprg = Null_Iir then
         --  Not inside a suprogram or a process.
         return;
      end if;
      if Subprg = Imp then
         --  Recursive call.
         return;
      end if;

      if Is_Implicit_Subprogram (Imp) then
         --  FIXME: impure predefined functions.
         null;
      else
         Sem_Call_Purity_Check (Subprg, Imp, Expr);
         Sem_Call_All_Sensitized_Check (Subprg, Imp, Expr);
         if Get_Kind (Imp) = Iir_Kind_Procedure_Declaration then
            Sem_Call_Wait_Check (Subprg, Imp, Expr);
            --  Check passive.
            if Get_Passive_Flag (Imp) = False then
               case Get_Kind (Subprg) is
                  when Iir_Kinds_Process_Statement =>
                     if Get_Passive_Flag (Subprg) then
                        Error_Msg_Sem
                          (+Expr, "%n is passive, but calls non-passive %n",
                           (+Subprg, +Imp));
                     end if;
                  when others =>
                     null;
               end case;
            end if;
         end if;
      end if;
   end Sem_Subprogram_Call_Finish;

   --  EXPR is a function or procedure call.
   function Sem_Subprogram_Call_Stage1
     (Expr : Iir; A_Type : Iir; Is_Func_Call : Boolean) return Iir
   is
      Imp : Iir;
      A_Func: Iir;
      Imp_List: Iir_List;
      New_List : Iir_List;
      Assoc_Chain: Iir;
      Inter_Chain : Iir;
      Res_Type: Iir_List;
      Imp_It : List_Iterator;
      Inter: Iir;
      Match : Compatibility_Level;
      Match_Max : Compatibility_Level;
   begin
      --  Sem_Name has gathered all the possible names for the prefix of this
      --  call.  Reduce this list to only names that match the types.
      Imp := Get_Implementation (Expr);
      Imp_List := Get_Overload_List (Imp);
      Assoc_Chain := Get_Parameter_Association_Chain (Expr);
      Match_Max := Via_Conversion;

      New_List := Create_Iir_List;
      Imp_It := List_Iterate (Imp_List);
      while Is_Valid (Imp_It) loop
         A_Func := Get_Element (Imp_It);

         case Get_Kind (A_Func) is
            when Iir_Kinds_Functions_And_Literals
              | Iir_Kind_Interface_Function_Declaration =>
               if not Is_Func_Call then
                  --  The identifier of a function call must be a function or
                  --  an enumeration literal.
                  goto Continue;
               end if;
            when Iir_Kind_Procedure_Declaration
              | Iir_Kind_Interface_Procedure_Declaration =>
               if Is_Func_Call then
                  --  The identifier of a procedure call must be a procedure.
                  goto Continue;
               end if;
            when others =>
               Error_Kind ("sem_subprogram_call_stage1", A_Func);
         end case;

         --  Keep this interpretation only if compatible.
         if A_Type = Null_Iir
           or else (Compatibility_Nodes (A_Type, Get_Return_Type (A_Func))
                      /= Not_Compatible)
         then
            Sem_Association_Chain
              (Get_Interface_Declaration_Chain (A_Func),
               Assoc_Chain, False, Missing_Parameter, Expr, Match);
            if Match >= Match_Max then
               --  Only previous interpretations were only Via_Conversion
               --  compatible, and this one is fully compatible, discard
               --  previous and future Via_Conversion interpretations.
               if Match > Match_Max then
                  Destroy_Iir_List (New_List);
                  New_List := Create_Iir_List;
                  Match_Max := Match;
               end if;
               Append_Element (New_List, A_Func);
            end if;
         end if;

         << Continue >> null;
         Next (Imp_It);
      end loop;
      Destroy_Iir_List (Imp_List);
      Imp_List := New_List;
      Set_Overload_List (Imp, Imp_List);

      -- Set_Implementation (Expr, Inter_List);
      -- A set of possible functions to call is in INTER_LIST.
      -- Create a set of possible return type in RES_TYPE.
      case Get_Nbr_Elements (Imp_List) is
         when 0 =>
            --  FIXME: display subprogram name.
            Error_Msg_Sem
              (+Expr, "cannot resolve overloading for subprogram call");
            return Null_Iir;

         when 1 =>
            --  Simple case: no overloading.
            Inter := Get_First_Element (Imp_List);
            Free_Overload_List (Imp);
            Set_Implementation (Expr, Inter);
            if Is_Func_Call then
               Set_Type (Expr, Get_Return_Type (Inter));
            end if;
            Inter_Chain := Get_Interface_Declaration_Chain (Inter);
            Sem_Association_Chain
              (Inter_Chain, Assoc_Chain,
               True, Missing_Parameter, Expr, Match);
            Set_Parameter_Association_Chain (Expr, Assoc_Chain);
            pragma Assert (Match /= Not_Compatible);
            Check_Subprogram_Associations (Inter_Chain, Assoc_Chain);
            Sem_Subprogram_Call_Finish (Expr, Inter);
            return Expr;

         when others =>
            if Is_Func_Call then
               if A_Type /= Null_Iir then
                  -- Cannot find a single interpretation for a given
                  -- type.
                  Report_Start_Group;
                  Error_Overload (Expr);
                  Disp_Overload_List (Imp_List, Expr);
                  Report_End_Group;
                  return Null_Iir;
               end if;

               --  Create the list of types for the result.
               Res_Type := Create_Iir_List;
               Imp_It := List_Iterate (Imp_List);
               while Is_Valid (Imp_It) loop
                  Add_Element
                    (Res_Type, Get_Return_Type (Get_Element (Imp_It)));
                  Next (Imp_It);
               end loop;

               if Get_Nbr_Elements (Res_Type) = 1 then
                  -- several implementations but one profile.
                  Report_Start_Group;
                  Error_Overload (Expr);
                  Disp_Overload_List (Imp_List, Expr);
                  Report_End_Group;
                  return Null_Iir;
               end if;
               Set_Type (Expr, Create_Overload_List (Res_Type));
            else
               --  For a procedure call, the context does't help to resolve
               --  overload.
               Report_Start_Group;
               Error_Overload (Expr);
               Disp_Overload_List (Imp_List, Expr);
               Report_End_Group;
            end if;
            return Expr;
      end case;
   end Sem_Subprogram_Call_Stage1;

   -- For a procedure call, A_TYPE must be null.
   --  Associations must have already been analyzed by sem_association_list.
   function Sem_Subprogram_Call (Expr: Iir; A_Type: Iir) return Iir
   is
      Is_Func: constant Boolean := Get_Kind (Expr) = Iir_Kind_Function_Call;
      Res_Type: Iir;
      Res: Iir;
      Inter_List: Iir;
      Param_Chain : Iir;
      Inter: Iir;
      Assoc_Chain : Iir;
      Match : Compatibility_Level;
      Overload_List : Iir_List;
      Overload_It : List_Iterator;
   begin
      if Is_Func then
         Res_Type := Get_Type (Expr);
      end if;

      if not Is_Func or else Res_Type = Null_Iir then
         -- First call to sem_subprogram_call.
         -- Create the list of possible implementations and possible
         -- return types, according to arguments and A_TYPE.

         -- Select possible interpretations among all interpretations.
         -- NOTE: the list of possible implementations was already created
         --  during the transformation of iir_kind_parenthesis_name to
         --  iir_kind_function_call.
         Inter_List := Get_Implementation (Expr);
         if Is_Error (Inter_List) then
            return Null_Iir;
         elsif Is_Overload_List (Inter_List) then
            --  Subprogram name is overloaded.
            return Sem_Subprogram_Call_Stage1 (Expr, A_Type, Is_Func);
         else
            --  Only one interpretation for the subprogram name.
            if Is_Func then
               if not Is_Function_Declaration (Inter_List) then
                  Report_Start_Group;
                  Error_Msg_Sem (+Expr, "name does not designate a function");
                  Error_Msg_Sem (+Expr, "name is %n defined at %l",
                                 (+Inter_List, +Inter_List));
                  Report_End_Group;
                  return Null_Iir;
               end if;
            else
               if not Is_Procedure_Declaration (Inter_List) then
                  Report_Start_Group;
                  Error_Msg_Sem (+Expr, "name does not designate a procedure");
                  Error_Msg_Sem (+Expr, "name is %n defined at %l",
                                 (+Inter_List, +Inter_List));
                  Report_End_Group;
                  return Null_Iir;
               end if;
            end if;

            Assoc_Chain := Get_Parameter_Association_Chain (Expr);
            Param_Chain := Get_Interface_Declaration_Chain (Inter_List);
            Sem_Association_Chain
              (Param_Chain, Assoc_Chain,
               True, Missing_Parameter, Expr, Match);
            Set_Parameter_Association_Chain (Expr, Assoc_Chain);
            if Match = Not_Compatible then
               --  No need to disp an error message, this is done by
               --  sem_subprogram_arguments.
               return Null_Iir;
            end if;
            if Is_Func then
               Set_Type (Expr, Get_Return_Type (Inter_List));
            end if;
            Check_Subprogram_Associations (Param_Chain, Assoc_Chain);
            Set_Implementation (Expr, Inter_List);
            Sem_Subprogram_Call_Finish (Expr, Inter_List);
            return Expr;
         end if;
      end if;

      --  Second call to Sem_Function_Call (only for functions).
      pragma Assert (Is_Func);
      pragma Assert (A_Type /= Null_Iir);

      -- The implementation list was set.
      -- The return type was set.
      -- A_TYPE is not null, A_TYPE is *the* return type.

      Inter_List := Get_Implementation (Expr);

      -- Find a single implementation.
      Res := Null_Iir;
      if Is_Overload_List (Inter_List) then
         -- INTER_LIST is a list of possible declaration to call.
         -- Find one, based on the return type A_TYPE.
         Overload_List := Get_Overload_List (Inter_List);
         Overload_It := List_Iterate (Overload_List);
         while Is_Valid (Overload_It) loop
            Inter := Get_Element (Overload_It);
            if Are_Basetypes_Compatible
              (A_Type, Get_Base_Type (Get_Return_Type (Inter)))
              /= Not_Compatible
            then
               if Res /= Null_Iir then
                  Report_Start_Group;
                  Error_Overload (Expr);
                  Disp_Overload_List (Overload_List, Expr);
                  Report_End_Group;
                  return Null_Iir;
               else
                  Res := Inter;
               end if;
            end if;
            Next (Overload_It);
         end loop;
      else
         if Are_Basetypes_Compatible
           (A_Type, Get_Base_Type (Get_Return_Type (Inter_List)))
           /= Not_Compatible
         then
            Res := Inter_List;
         end if;
      end if;
      if Res = Null_Iir then
         Error_Not_Match (Expr, A_Type);
         return Null_Iir;
      end if;

      -- Clean up.
      if Res_Type /= Null_Iir and then Is_Overload_List (Res_Type) then
         Free_Iir (Res_Type);
      end if;

      if Is_Overload_List (Inter_List) then
         Free_Iir (Inter_List);
      end if;

      --  Simple case: this is not a call to a function, but an enumeration
      --  literal.
      if Get_Kind (Res) = Iir_Kind_Enumeration_Literal then
         -- Free_Iir (Expr);
         return Res;
      end if;

      -- Set types.
      Set_Type (Expr, Get_Return_Type (Res));
      Assoc_Chain := Get_Parameter_Association_Chain (Expr);
      Param_Chain := Get_Interface_Declaration_Chain (Res);
      Sem_Association_Chain
        (Param_Chain, Assoc_Chain, True, Missing_Parameter, Expr, Match);
      Set_Parameter_Association_Chain (Expr, Assoc_Chain);
      if Match = Not_Compatible then
         return Null_Iir;
      end if;
      Check_Subprogram_Associations (Param_Chain, Assoc_Chain);
      Set_Implementation (Expr, Res);
      Sem_Subprogram_Call_Finish (Expr, Res);
      return Expr;
   end Sem_Subprogram_Call;

   procedure Sem_Procedure_Call (Call : Iir_Procedure_Call; Stmt : Iir)
   is
      Imp: Iir;
      Name : Iir;
      Parameters_Chain : Iir;
      Param : Iir;
      Formal : Iir;
      Prefix : Iir;
      Inter : Iir;
   begin
      Name := Get_Prefix (Call);
      if Name = Null_Iir
        or else Is_Error (Name)
        or else Get_Kind (Name) = Iir_Kind_String_Literal8
      then
         pragma Assert (Flags.Flag_Force_Analysis);
         return;
      end if;

      --  FIXME: check for denoting name.
      Sem_Name (Name);

      --  Return now if the procedure declaration wasn't found.
      Imp := Get_Named_Entity (Name);
      if Is_Error (Imp) then
         return;
      end if;
      Set_Implementation (Call, Imp);

      Name_To_Method_Object (Call, Name);
      Parameters_Chain := Get_Parameter_Association_Chain (Call);
      if Sem_Actual_Of_Association_Chain (Parameters_Chain) = False then
         return;
      end if;
      if Sem_Subprogram_Call (Call, Null_Iir) /= Call then
         return;
      end if;
      Imp := Get_Implementation (Call);
      if Is_Overload_List (Imp) then
         --  Failed to resolve overload.
         return;
      end if;
      Set_Named_Entity (Name, Imp);
      Set_Prefix (Call, Finish_Sem_Name (Name));

      --  LRM 2.1.1.2 Signal Parameters
      --  A process statement contains a driver for each actual signal
      --  associated with a formal signal parameter of mode OUT or INOUT in
      --  a subprogram call.
      --  Similarly, a subprogram contains a driver for each formal signal
      --  parameter of mode OUT or INOUT declared in its subrogram
      --  specification.
      Param := Parameters_Chain;
      Inter := Get_Interface_Declaration_Chain (Imp);
      while Param /= Null_Iir loop
         --  Association_Element_By_Individual duplicates existing
         --  associations.
         if Get_Kind (Param) /= Iir_Kind_Association_Element_By_Individual
         then
            Formal := Get_Formal (Param);
            if Formal = Null_Iir then
               Formal := Inter;
               Inter := Get_Chain (Inter);
            else
               Formal := Get_Base_Name (Formal);
               Inter := Null_Iir;
            end if;
            if Get_Kind (Formal) = Iir_Kind_Interface_Signal_Declaration
              and then Get_Mode (Formal) in Iir_Out_Modes
            then
               Prefix := Name_To_Object (Get_Actual (Param));
               if Prefix /= Null_Iir then
                  case Get_Kind (Get_Object_Prefix (Prefix)) is
                     when Iir_Kind_Signal_Declaration
                       | Iir_Kind_Interface_Signal_Declaration =>
                        Prefix := Get_Longest_Static_Prefix (Prefix);
                        Sem_Stmts.Sem_Add_Driver (Prefix, Stmt);
                     when others =>
                        null;
                  end case;
               end if;
            end if;
         end if;
         Param := Get_Chain (Param);
      end loop;
   end Sem_Procedure_Call;

   --  List must be an overload list containing subprograms declarations.
   --  Try to resolve overload and return the uniq interpretation if one,
   --  NULL_IIR otherwise.
   --
   --  If there are two functions, one primitive of a universal
   --  type and the other not, return the primitive of the universal type.
   --  This implements implicit type conversions rules.
   --  Cf Sem_Names.Extract_Call_Without_Implicit_Conversion
   --
   --  The typical case is the use of comparison operator with literals or
   --  attributes, like: s'length = 0
   function Get_Non_Implicit_Subprogram (List : Iir_List) return Iir
   is
      It : List_Iterator;
      El : Iir;
      Res : Iir;
      Ref_Type : Iir;
   begin
      --  Conditions:
      --  1. All the possible functions must return boolean.
      --  2. There is only one implicit function for universal or real.
      Res := Null_Iir;
      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);

         --  Only comparison operators need this special handling.
         if Get_Base_Type (Get_Return_Type (El)) /= Boolean_Type_Definition
         then
            return Null_Iir;
         end if;

         if Is_Implicit_Subprogram (El) then
            Ref_Type := Get_Type (Get_Interface_Declaration_Chain (El));
            if Ref_Type = Universal_Integer_Type_Definition
              or Ref_Type = Universal_Real_Type_Definition
            then
               --  There could be only one such subprogram.
               pragma Assert (Res = Null_Iir);
               Res := El;
            end if;
         end if;
         Next (It);
      end loop;
      return Res;
   end Get_Non_Implicit_Subprogram;

   --  Honor the -fexplicit flag.
   --  If LIST is composed of 2 declarations that matches the 'explicit' rule,
   --   return the explicit declaration.
   --  Otherwise, return NULL_IIR.
   function Get_Explicit_Subprogram (List : Iir_List) return Iir
   is
      Sub1 : Iir;
      Sub2 : Iir;
      It : List_Iterator;
      Res : Iir;
   begin
      if Get_Nbr_Elements (List) /= 2 then
         return Null_Iir;
      end if;

      It := List_Iterate (List);
      Sub1 := Get_Element (It);
      Next (It);
      Sub2 := Get_Element (It);
      Next (It);
      pragma Assert (not Is_Valid (It));

      --  One must be an implicit declaration, the other must be an explicit
      --  declaration.
      pragma Assert (Get_Kind (Sub1) = Iir_Kind_Function_Declaration);
      pragma Assert (Get_Kind (Sub2) = Iir_Kind_Function_Declaration);
      if Is_Implicit_Subprogram (Sub1) then
         if Is_Implicit_Subprogram (Sub2) then
            return Null_Iir;
         end if;
         Res := Sub2;
      else
         if not Is_Implicit_Subprogram (Sub2) then
            return Null_Iir;
         end if;
         Res := Sub1;
      end if;

      --  They must have the same profile.
      if Get_Subprogram_Hash (Sub1) /= Get_Subprogram_Hash (Sub2)
        or else not Is_Same_Profile (Sub1, Sub2)
      then
         return Null_Iir;
      end if;

      --  They must be declared in a package.
      if Get_Kind (Get_Parent (Sub1)) /= Iir_Kind_Package_Declaration
        or else Get_Kind (Get_Parent (Sub2)) /= Iir_Kind_Package_Declaration
      then
         return Null_Iir;
      end if;

      return Res;
   end Get_Explicit_Subprogram;

   --  Set when the -fexplicit option was adviced.
   Explicit_Advice_Given : Boolean := False;

   -- LEFT and RIGHT must be set.
   function Set_Operator_Unique_Interpretation
     (Expr : Iir; Decl : Iir) return Iir
   is
      Is_Dyadic : constant Boolean :=
        Get_Kind (Expr) in Iir_Kinds_Dyadic_Operator;
      Inter : Iir;
      Err        : Boolean;
      Left       : Iir;
      Left_Type  : Iir;
      Right      : Iir;
      Right_Type : Iir;
   begin
      Set_Type (Expr, Get_Return_Type (Decl));
      Inter := Get_Interface_Declaration_Chain (Decl);
      Err := False;

      --  Left operand (or single operand)
      Left := Get_Left (Expr);
      Left_Type := Get_Type (Inter);
      if Is_Overloaded (Left) then
         Left := Sem_Expression_Ov (Left, Get_Base_Type (Left_Type));
         if Left = Null_Iir then
            Err := True;
         end if;
      end if;
      Check_Subprogram_Association_Expression (Inter, Left, Null_Iir, Left);
      Set_Left (Expr, Left);

      --  Right operand
      if Is_Dyadic then
         Right := Get_Right (Expr);
         Inter := Get_Chain (Inter);
         Right_Type := Get_Type (Inter);
         if Is_Overloaded (Right) then
            Right := Sem_Expression_Ov (Right, Get_Base_Type (Right_Type));
            if Right = Null_Iir then
               Err := True;
            end if;
         end if;
         Check_Subprogram_Association_Expression
           (Inter, Right, Null_Iir, Right);
         Set_Right (Expr, Right);
      end if;

      if not Err then
         Set_Implementation (Expr, Decl);
         Sem_Subprogram_Call_Finish (Expr, Decl);
         if Get_Expr_Staticness (Expr) = Locally then
            return Eval_Expr_If_Static (Expr);
         else
            --  The result is not static, but an operand may be static.
            --  Evaluate it.
            Left := Eval_Expr_Check_If_Static (Left, Left_Type);
            Set_Left (Expr, Left);
            if Is_Dyadic then
               Right := Eval_Expr_Check_If_Static (Right, Right_Type);
               Set_Right (Expr, Right);
            end if;
         end if;
      end if;
      return Expr;
   end Set_Operator_Unique_Interpretation;

   --  Display an error message for sem_operator.
   procedure Error_Operator_Overload (Expr : Iir; List : Iir_List)
   is
      Operator : Name_Id;
   begin
      Operator := Utils.Get_Operator_Name (Expr);
      Report_Start_Group;
      Error_Msg_Sem (+Expr, "operator %i is overloaded", +Operator);
      Disp_Overload_List (List, Expr);
      Report_End_Group;
   end Error_Operator_Overload;

   --  Return False in case of error.
   function Sem_Operator_Operands (Expr : Iir) return Boolean
   is
      Is_Dyadic : constant Boolean :=
        Get_Kind (Expr) in Iir_Kinds_Dyadic_Operator;
      Left, Right: Iir;
   begin
      --  First pass.
      --  Analyze operands.
      --  FIXME: should try to analyze right operand even if analyze
      --  of left operand has failed ??
      Left := Get_Left (Expr);
      if Get_Type (Left) = Null_Iir then
         Left := Sem_Expression_Ov (Left, Null_Iir);
         if Left = Null_Iir then
            return False;
         end if;
         Set_Left (Expr, Left);
      end if;
      if Is_Dyadic then
         Right := Get_Right (Expr);
         if Get_Type (Right) = Null_Iir then
            Right := Sem_Expression_Ov (Right, Null_Iir);
            if Right = Null_Iir then
               return False;
            end if;
            Set_Right (Expr, Right);
         end if;
      end if;
      return True;
   end Sem_Operator_Operands;

   --  Return the compatibility level between operation EXPR (either monadic
   --  or dyadic) and operator DECL (also monadic or dyadic).
   --  RES_TYPE is the expected expression type, which can be NULL_IIR.
   --  Note: even if the result is fully_compatible, at the end the
   --  compatibility could be via_conversion if the result has be to be
   --  converted.
   function Sem_Operator_Compatibility
     (Decl : Iir; Expr : Iir; Is_Dyadic : Boolean; Res_Type : Iir)
     return Compatibility_Level
   is
      Left_Inter, Right_Inter : Iir;
      Res, Level : Compatibility_Level;
   begin
      --  Check return type.
      if Res_Type /= Null_Iir then
         Res := Are_Types_Compatible (Res_Type, Get_Return_Type (Decl));
         if Res = Not_Compatible then
            return Not_Compatible;
         end if;
      else
         Res := Fully_Compatible;
      end if;

      Left_Inter := Get_Interface_Declaration_Chain (Decl);
      Right_Inter := Get_Chain (Left_Inter);

      --  Operator can be either monadic or dyadic.
      pragma Assert (Right_Inter = Null_Iir
                       or else Get_Chain (Right_Inter) = Null_Iir);

      --  Check arity.

      --  LRM93 2.5.2 Operator overloading
      --  The subprogram specification of a unary operator must have
      --  a single parameter [...]
      --  The subprogram specification of a binary operator must have
      --  two parameters [...]
      --
      --  GHDL: So even in presence of default expression in a parameter,
      --  a unary operation has to match with a binary operator.
      if (Right_Inter /= Null_Iir) /= Is_Dyadic then
         return Not_Compatible;
      end if;

      -- Check operands.
      Level := Is_Expr_Compatible (Get_Type (Left_Inter), Get_Left (Expr));
      if Level = Not_Compatible then
         return Not_Compatible;
      end if;
      Res := Compatibility_Level'Min (Res, Level);

      if Is_Dyadic then
         Level := Is_Expr_Compatible (Get_Type (Right_Inter),
                                      Get_Right (Expr));
         if Level = Not_Compatible then
            return Not_Compatible;
         end if;
         Res := Compatibility_Level'Min (Res, Level);
      end if;

      return Res;
   end Sem_Operator_Compatibility;

   function Sem_Operator_Pass1 (Expr : Iir; Res_Type : Iir) return Iir
   is
      Is_Dyadic : constant Boolean :=
        Get_Kind (Expr) in Iir_Kinds_Dyadic_Operator;
      Operator : constant Name_Id := Utils.Get_Operator_Name (Expr);
      Interpretation : Name_Interpretation_Type;
      Level : Compatibility_Level;
      Decl : Iir;
      Overload_List : Iir_List;
      Res_Type_List : Iir;
      It : List_Iterator;
   begin
      --  First pass.
      --  Analyze operands.
      --  FIXME: should try to analyze right operand even if analyze
      --  of left operand has failed ??
      if not Sem_Operator_Operands (Expr) then
         return Null_Iir;
      end if;

      Overload_List := Create_Iir_List;

      --  Try to find an implementation among user defined function
      Interpretation := Get_Interpretation (Operator);
      while Valid_Interpretation (Interpretation) loop
         Decl := Get_Non_Alias_Declaration (Interpretation);

         --  It is compatible with operand types ?
         pragma Assert (Is_Function_Declaration (Decl));

         --  LRM08 12.3 Visibility
         --  [...] or all visible declarations denote the same named entity.
         --
         --  GHDL: If DECL has already been seen, then skip it.
         if not Get_Seen_Flag (Decl) then
            Level := Sem_Operator_Compatibility
              (Decl, Expr, Is_Dyadic, Res_Type);
            if Level /= Not_Compatible then
               --  Match.
               Set_Seen_Flag (Decl, True);
               Append_Element (Overload_List, Decl);
            end if;
         end if;

         Interpretation := Get_Next_Interpretation (Interpretation);
      end loop;

      --  Clear seen_flags.
      It := List_Iterate (Overload_List);
      while Is_Valid (It) loop
         Set_Seen_Flag (Get_Element (It), False);
         Next (It);
      end loop;

      --  The list of possible implementations was computed.
      case Get_Nbr_Elements (Overload_List) is
         when 0 =>
            if Get_Kind (Expr) = Iir_Kind_Implicit_Condition_Operator then
               --  TODO: display expression type.
               Error_Msg_Sem (+Expr, "cannot convert expression to boolean "
                                & "(no ""??"" found)");
            else
               Error_Msg_Sem (+Expr,
                              "no function declarations for %n", +Expr);
            end if;
            Destroy_Iir_List (Overload_List);
            return Null_Iir;

         when 1 =>
            Decl := Get_First_Element (Overload_List);
            Destroy_Iir_List (Overload_List);
            return Set_Operator_Unique_Interpretation (Expr, Decl);

         when others =>
            --  Preference for universal operator.
            --  This roughly corresponds to:
            --
            --  LRM 7.3.5
            --  An implicit conversion of a convertible universal operand
            --  is applied if and only if the innermost complete context
            --  determines a unique (numeric) target type for the implicit
            --  conversion, and there is no legal interpretation of this
            --  context without this conversion.
            if Is_Dyadic then
               Decl := Get_Non_Implicit_Subprogram (Overload_List);
               if Decl /= Null_Iir then
                  Destroy_Iir_List (Overload_List);
                  return Set_Operator_Unique_Interpretation (Expr, Decl);
               end if;
            end if;

            Set_Implementation (Expr, Create_Overload_List (Overload_List));

            --  Create the list of possible return types, if it is not yet
            --  determined.
            if Res_Type = Null_Iir then
               Res_Type_List := Create_List_Of_Types (Overload_List);
               if Is_Overload_List (Res_Type_List) then
                  --  There are many possible return types.
                  --  Try again.
                  Set_Type (Expr, Res_Type_List);
                  return Expr;
               end if;
            end if;

            --  The return type is known.
            --  Search for explicit subprogram.

            --  It was impossible to find one solution.
            Error_Operator_Overload (Expr, Overload_List);

            --  Give an advice.
            if not Flags.Flag_Explicit
              and then not Explicit_Advice_Given
              and then Flags.Vhdl_Std < Vhdl_08
            then
               Decl := Get_Explicit_Subprogram (Overload_List);
               if Decl /= Null_Iir then
                  Error_Msg_Sem
                    (+Expr, "(you may want to use the -fexplicit option)");
                  Explicit_Advice_Given := True;
               end if;
            end if;

            return Null_Iir;
      end case;
   end Sem_Operator_Pass1;

   function Sem_Operator_Pass2_Interpretation
     (Expr : Iir; Res_Type : Iir) return Iir
   is
      Is_Dyadic : constant Boolean :=
        Get_Kind (Expr) in Iir_Kinds_Dyadic_Operator;
      Decl : Iir;
      Overload : Iir;
      Overload_List : Iir_List;
      Full_Compat : Iir;
      Conv_Compat : Iir;
      It : List_Iterator;
      Level : Compatibility_Level;
   begin
      --  Second pass
      --  Find the uniq implementation for this call.
      Overload := Get_Implementation (Expr);
      Overload_List := Get_Overload_List (Overload);

      Full_Compat := Null_Iir;
      Conv_Compat := Null_Iir;

      It := List_Iterate (Overload_List);
      while Is_Valid (It) loop
         Decl := Get_Element (It);
         Level := Sem_Operator_Compatibility (Decl, Expr, Is_Dyadic, Res_Type);
         case Level is
            when Not_Compatible =>
               --  Ignored
               null;
            when Fully_Compatible =>
               if Full_Compat = Null_Iir then
                  Full_Compat := Decl;
               else
                  --  There are several fully compatible functions.
                  --  TODO: remove non-fully compatible functions from the list
                  --   before displaying the list.
                  Error_Operator_Overload (Expr, Overload_List);
                  return Null_Iir;
               end if;
            when Via_Conversion =>
               if Conv_Compat = Null_Iir then
                  Conv_Compat := Decl;
               else
                  --  Not yet an error, as there can be one fully compatible
                  --  function.
                  Conv_Compat := Overload;
               end if;
         end case;
         Next (It);
      end loop;

      if Full_Compat = Null_Iir then
         if Conv_Compat = Overload then
            --  Several results through implicit conversion.
            --  TODO: remove incompatible declarations from the list before
            --   displaying it.
            Error_Operator_Overload (Expr, Overload_List);
            return Null_Iir;
         else
            Full_Compat := Conv_Compat;
         end if;
      end if;

      Free_Iir (Overload);
      Overload := Get_Type (Expr);
      Free_Overload_List (Overload);
      Destroy_Iir_List (Overload_List);
      if Full_Compat = Null_Iir then
         Error_Msg_Sem (+Expr,
                        "no matching function declarations for %n", +Expr);
         return Null_Iir;
      else
         return Full_Compat;
      end if;
   end Sem_Operator_Pass2_Interpretation;

   function Sem_Operator (Expr : Iir; Res_Type : Iir) return Iir
   is
      Interpretation : Iir;
   begin
      if Get_Type (Expr) = Null_Iir then
         return Sem_Operator_Pass1 (Expr, Res_Type);
      else
         Interpretation := Sem_Operator_Pass2_Interpretation (Expr, Res_Type);
         if Interpretation = Null_Iir then
            return Null_Iir;
         else
            return Set_Operator_Unique_Interpretation (Expr, Interpretation);
         end if;
      end if;
   end Sem_Operator;

   --  Analyze LIT whose elements must be of type EL_TYPE, and return
   --  the length.
   --  FIXME: the errors are reported, but there is no mark of that.
   function Sem_String_Literal (Str : Iir; El_Type : Iir) return Natural
   is
      function Find_Literal (Etype : Iir_Enumeration_Type_Definition;
                             C : Character)
                            return Iir_Enumeration_Literal
      is
         Id : constant Name_Id := Name_Table.Get_Identifier (C);
         Inter : Name_Interpretation_Type;
         Decl : Iir;
      begin
         Inter := Get_Interpretation (Id);
         while Valid_Interpretation (Inter) loop
            Decl := Get_Non_Alias_Declaration (Inter);
            if Get_Kind (Decl) = Iir_Kind_Enumeration_Literal
              and then Get_Type (Decl) = Etype
            then
               return Decl;
            end if;
            Inter := Get_Next_Interpretation (Inter);
         end loop;

         --  LRM08 9.3 Operands
         --  The character literals corresponding to the graphic characters
         --  contained within a string literal or a bit string literal shall
         --  be visible at the place of the string literal.

         --  Character C is not visible...
         if Find_Name_In_Flist (Get_Enumeration_Literal_List (Etype), Id)
           = Null_Iir
         then
            --  ... because it is not defined.
            Error_Msg_Sem
              (+Str, "type %n does not define character %c", (+Etype, +C));
         else
            --  ... because it is not visible.
            Error_Msg_Sem (+Str, "character %c of type %n is not visible",
                           (+C, +Etype));
         end if;
         return Null_Iir;
      end Find_Literal;

      type Characters_Pos is array (Character range <>) of Nat8;
      Len : constant Nat32 := Get_String_Length (Str);
      Id : constant String8_Id := Get_String8_Id (Str);
      El : Iir;
      Enum_Pos : Iir_Int32;
      Ch : Character;

      --  Create a cache of literals, to speed-up a little bit the
      --  search.
      No_Pos : constant Nat8 := Nat8'Last;
      Map : Characters_Pos (' ' .. Character'Last) := (others => No_Pos);
      Res : Nat8;
   begin
      for I in 1 .. Len loop
         Ch := Str_Table.Char_String8 (Id, I);
         if Ch not in Map'Range then
            --  Invalid character.
            pragma Assert (Flags.Flag_Force_Analysis);
            Res := 0;
         else
            Res := Map (Ch);
            if Res = No_Pos then
               El := Find_Literal (El_Type, Ch);
               if El = Null_Iir then
                  Res := 0;
               else
               Enum_Pos := Get_Enum_Pos (El);
               Res := Nat8 (Enum_Pos);
               Map (Ch) := Res;
               end if;
            end if;
         end if;
         Str_Table.Set_Element_String8 (Id, I, Res);
      end loop;

      --  LRM08 9.4.2 Locally static primaries
      --  a) A literal of any type other than type TIME
      Set_Expr_Staticness (Str, Locally);

      return Natural (Len);
   end Sem_String_Literal;

   procedure Sem_String_Literal (Lit: Iir)
   is
      Lit_Type : constant Iir := Get_Type (Lit);
      Lit_Base_Type : constant Iir := Get_Base_Type (Lit_Type);

      -- The subtype created for the literal.
      N_Type: Iir;
      -- type of the index of the array type.
      Index_Type: Iir;
      Len : Natural;
      El_Type : Iir;
   begin
      El_Type := Get_Base_Type (Get_Element_Subtype (Lit_Base_Type));
      Len := Sem_String_Literal (Lit, El_Type);

      if Get_Constraint_State (Lit_Type) = Fully_Constrained then
         --  The type of the context is constrained.
         Index_Type := Get_Index_Type (Lit_Type, 0);
         if Get_Type_Staticness (Index_Type) = Locally then
            if Eval_Discrete_Type_Length (Index_Type) = Int64 (Len) then
               return;
            else
               Error_Msg_Sem (+Lit, "string length does not match that of %n",
                              +Index_Type);
               --  Change the type.
            end if;
         else
            --  FIXME: emit a warning because of dubious construct (the type
            --  of the string is not locally constrained) ?
            return;
         end if;
      end if;

      -- Context type is not constained.  Set type of the string literal,
      -- according to LRM93 7.3.2.2.
      N_Type := Create_Unidim_Array_By_Length
        (Lit_Base_Type, Int64 (Len), Lit);
      Set_Type (Lit, N_Type);
      Set_Literal_Subtype (Lit, N_Type);
   end Sem_String_Literal;

   procedure Count_Choices (Info : out Choice_Info_Type;
                            Choice_Chain : Iir)
   is
      Choice : Iir;
   begin
      Info := (Nbr_Choices => 0,
               Nbr_Alternatives => 0,
               Others_Choice => Null_Iir,
               Arr => null,
               Annex_Arr => null);
      Choice := Choice_Chain;
      while Is_Valid (Choice) loop
         case Iir_Kinds_Case_Choice (Get_Kind (Choice)) is
            when Iir_Kind_Choice_By_Expression
              | Iir_Kind_Choice_By_Range =>
               if Get_Choice_Staticness (Choice) = Locally then
                  Info.Nbr_Choices := Info.Nbr_Choices + 1;
               end if;
            when Iir_Kind_Choice_By_Others =>
               Info.Others_Choice := Choice;
         end case;
         if not Get_Same_Alternative_Flag (Choice) then
            Info.Nbr_Alternatives := Info.Nbr_Alternatives + 1;
         end if;
         Choice := Get_Chain (Choice);
      end loop;
   end Count_Choices;

   procedure Fill_Choices_Array (Info : in out Choice_Info_Type;
                                 Choice_Chain : Iir)
   is
      Index : Natural;
      Choice : Iir;
      Expr : Iir;
   begin
      Info.Arr := new Iir_Array (1 .. Info.Nbr_Choices);

      --  Fill the array.
      Index := 0;
      Choice := Choice_Chain;
      while Choice /= Null_Iir loop
         case Iir_Kinds_Case_Choice (Get_Kind (Choice)) is
            when Iir_Kind_Choice_By_Expression =>
               Expr := Get_Choice_Expression (Choice);
            when Iir_Kind_Choice_By_Range =>
               Expr := Get_Choice_Range (Choice);
               Expr := Get_Range_From_Discrete_Range (Expr);
            when Iir_Kind_Choice_By_Others =>
               Expr := Null_Iir;
         end case;
         if Is_Valid (Expr) and then Get_Expr_Staticness (Expr) = Locally
         then
            Index := Index + 1;
            Info.Arr (Index) := Choice;
         end if;
         Choice := Get_Chain (Choice);
      end loop;

      pragma Assert (Index = Info.Nbr_Choices);
   end Fill_Choices_Array;

   procedure Swap_Choice_Info (Info : Choice_Info_Type;
                               From : Natural; To : Natural)
   is
      Tmp : Iir;
   begin
      Tmp := Info.Arr (To);
      Info.Arr (To) := Info.Arr (From);
      Info.Arr (From) := Tmp;

      if Info.Annex_Arr /= null then
         declare
            T : Int32;
         begin
            T := Info.Annex_Arr (To);
            Info.Annex_Arr (To) := Info.Annex_Arr (From);
            Info.Annex_Arr (From) := T;
         end;
      end if;
   end Swap_Choice_Info;

   procedure Sort_String_Choices (Info : in out Choice_Info_Type)
   is
      --  Compare two elements of ARR.
      --  Return true iff OP1 < OP2.
      function Lt (Op1, Op2 : Natural) return Boolean
      is
         E1 : constant Iir := Get_Choice_Expression (Info.Arr (Op1));
         E2 : constant Iir := Get_Choice_Expression (Info.Arr (Op2));
      begin
         return Compare_String_Literals (E1, E2) = Compare_Lt;
      end Lt;

      procedure Swap (From : Natural; To : Natural) is
      begin
         Swap_Choice_Info (Info, From, To);
      end Swap;

      procedure Str_Heap_Sort is
         new Grt.Algos.Heap_Sort (Lt => Lt, Swap => Swap);
   begin
      Str_Heap_Sort (Info.Nbr_Choices);
   end Sort_String_Choices;

   procedure Sem_String_Choices_Range (Choice_Chain : Iir; Sel : Iir)
   is
      --  Type of SEL.
      Sel_Type : Iir;

      --  Type of the element of SEL.
      Sel_El_Type : Iir;
      --  Number of literals in the element type.
      Sel_El_Length : Int64;

      --  Length of SEL (number of characters in SEL).
      Sel_Length : Int64;

      --  True if length of a choice mismatches
      Has_Length_Error : Boolean := False;

      El : Iir;

      Info : Choice_Info_Type;

      procedure Sem_Simple_Choice (Choice : Iir)
      is
         Expr : Iir;
         Choice_Len : Int64;
      begin
         --  LRM93 8.8
         --  In such case, each choice appearing in any of the case statement
         --  alternative must be a locally static expression whose value is of
         --  the same length as that of the case expression.
         Expr := Sem_Expression (Get_Choice_Expression (Choice), Sel_Type);
         if Expr = Null_Iir then
            Has_Length_Error := True;
            return;
         end if;
         Set_Choice_Expression (Choice, Expr);
         if Get_Expr_Staticness (Expr) < Locally then
            Error_Msg_Sem (+Expr, "choice must be locally static expression");
            Has_Length_Error := True;
            return;
         end if;
         Set_Choice_Staticness (Choice, Locally);
         Expr := Eval_Expr (Expr);
         Set_Choice_Expression (Choice, Expr);
         if Get_Kind (Expr) = Iir_Kind_Overflow_Literal then
            Error_Msg_Sem
              (+Expr, "bound error during evaluation of choice expression");
            Has_Length_Error := True;
            return;
         end if;

         --  If the choice is an aggregate (which could be static in vhdl08),
         --  transform it into a simple aggregate to ease the comparisons.
         if Get_Kind (Expr) = Iir_Kind_Aggregate then
            Expr := Eval_String_Literal (Expr);
            Set_Choice_Expression (Choice, Expr);
         end if;

         Choice_Len := Eval_Discrete_Type_Length
           (Get_String_Type_Bound_Type (Get_Type (Expr)));
         if Sel_Length = -1 then
            Sel_Length := Choice_Len;
         else
            if Choice_Len /= Sel_Length then
               Has_Length_Error := True;
               Error_Msg_Sem (+Expr, "incorrect length for the choice value");
               return;
            end if;
         end if;
      end Sem_Simple_Choice;

      function Eq (Op1, Op2 : Natural) return Boolean is
      begin
         return Compare_String_Literals
           (Get_Choice_Expression (Info.Arr (Op1)),
            Get_Choice_Expression (Info.Arr (Op2)))
           = Compare_Eq;
      end Eq;
   begin
      --  LRM93 8.8
      --  If the expression is of one-dimensional character array type, then
      --  the expression must be one of the following:
      --  FIXME: to complete.
      Sel_Type := Get_Type (Sel);
      if not Is_One_Dimensional_Array_Type (Sel_Type) then
         Error_Msg_Sem
           (+Sel,
            "expression must be discrete or one-dimension array subtype");
         return;
      end if;
      if Get_Type_Staticness (Sel_Type) = Locally then
         Sel_Length := Eval_Discrete_Type_Length
           (Get_String_Type_Bound_Type (Sel_Type));
      else
         --  LRM08 10.9 Case statement
         --  If the expression is of a one-dimensional character array type and
         --  is not described by either of the preceding two paragraphs, then
         --  the values of all of the choices, except the OTHERS choice, if
         --  present, shall be of the same length.
         if Flags.Vhdl_Std >= Vhdl_08 then
            Sel_Length := -1;
         else
            Error_Msg_Sem (+Sel, "array type must be locally static");
            return;
         end if;
         --  Use the base type so that the subtype of the choices is computed.
         Sel_Type := Get_Base_Type (Sel_Type);
      end if;
      Sel_El_Type := Get_Element_Subtype (Sel_Type);
      Sel_El_Length := Eval_Discrete_Type_Length (Sel_El_Type);

      El := Choice_Chain;
      Info.Others_Choice := Null_Iir;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               raise Internal_Error;
            when Iir_Kind_Choice_By_Range =>
               Error_Msg_Sem
                 (+El, "range choice are not allowed for non-discrete type");
            when Iir_Kind_Choice_By_Expression =>
               Sem_Simple_Choice (El);
            when Iir_Kind_Choice_By_Others =>
               if Info.Others_Choice /= Null_Iir then
                  Error_Msg_Sem (+El, "duplicate others choice");
               elsif Get_Chain (El) /= Null_Iir then
                  Error_Msg_Sem
                    (+El, "choice others must be the last alternative");
               end if;
               Info.Others_Choice := El;
            when others =>
               Error_Kind ("sem_string_choices_range", El);
         end case;
         El := Get_Chain (El);
      end loop;

      --  Null choices.
      if Sel_Length = 0 then
         return;
      end if;
      if Has_Length_Error then
         return;
      end if;

      --  LRM 8.8
      --
      --  If the expression is the name of an object whose subtype is locally
      --  static, whether a scalar type or an array type, then each value of
      --  the subtype must be represented once and only once in the set of
      --  choices of the case statement and no other value is allowed; [...]

      -- 1. Allocate Arr, fill it and sort
      Count_Choices (Info, Choice_Chain);
      Fill_Choices_Array (Info, Choice_Chain);
      Sort_String_Choices (Info);

      -- 2. Check for duplicate choices
      for I in 1 .. Info.Nbr_Choices - 1 loop
         if Eq (I, I + 1) then
            Error_Msg_Sem
              (+Info.Arr (I),
               "duplicate choice with choice at %l", +Info.Arr (I + 1));
            exit;
         end if;
      end loop;

      -- 3. Free Arr
      Free (Info.Arr);

      --  Check for missing choice.
      --  Do not try to compute the expected number of choices as this can
      --  easily overflow.
      if Info.Others_Choice = Null_Iir then
         declare
            Nbr : Int64 := Int64 (Info.Nbr_Choices);
         begin
            for I in 1 .. Sel_Length loop
               Nbr := Nbr / Sel_El_Length;
               if Nbr = 0 and then Choice_Chain /= Null_Iir then
                  --  An error has already been reported by parse if there is
                  --  no choices.
                  Error_Msg_Sem (+Choice_Chain, "missing choice(s)");
                  exit;
               end if;
            end loop;
         end;
      end if;
   end Sem_String_Choices_Range;

   --  Get low limit of ASSOC.
   --  First, get the expression of the association, then the low limit.
   --  ASSOC may be either association_by_range (in this case the low limit
   --   is to be fetched), or association_by_expression (and the low limit
   --   is the expression).
   function Get_Assoc_Low (Assoc : Iir) return Iir
   is
      Expr : Iir;
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Choice_By_Expression =>
            return Get_Choice_Expression (Assoc);
         when Iir_Kind_Choice_By_Range =>
            Expr := Get_Choice_Range (Assoc);
            Expr := Get_Range_From_Discrete_Range (Expr);
            case Get_Kind (Expr) is
               when Iir_Kind_Range_Expression =>
                  return Get_Low_Limit (Expr);
               when others =>
                  return Expr;
            end case;
         when others =>
            Error_Kind ("get_assoc_low", Assoc);
      end case;
   end Get_Assoc_Low;

   function Get_Assoc_High (Assoc : Iir) return Iir
   is
      Expr : Iir;
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Choice_By_Expression =>
            return Get_Choice_Expression (Assoc);
         when Iir_Kind_Choice_By_Range =>
            Expr := Get_Choice_Range (Assoc);
            Expr := Get_Range_From_Discrete_Range (Expr);
            case Get_Kind (Expr) is
               when Iir_Kind_Range_Expression =>
                  return Get_High_Limit (Expr);
               when others =>
                  return Expr;
            end case;
         when others =>
            Error_Kind ("get_assoc_high", Assoc);
      end case;
   end Get_Assoc_High;

   procedure Sort_Discrete_Choices (Info : in out Choice_Info_Type)
   is
      --  Compare two elements of ARR.
      --  Return true iff OP1 < OP2.
      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return (Eval_Pos (Get_Assoc_Low (Info.Arr (Op1)))
                   < Eval_Pos (Get_Assoc_Low (Info.Arr (Op2))));
      end Lt;

      procedure Swap (From : Natural; To : Natural) is
      begin
         Swap_Choice_Info (Info, From, To);
      end Swap;

      procedure Disc_Heap_Sort is
         new Grt.Algos.Heap_Sort (Lt => Lt, Swap => Swap);
   begin
      Disc_Heap_Sort (Info.Nbr_Choices);
   end Sort_Discrete_Choices;

   procedure Sem_Check_Continuous_Choices (Choice_Chain : Iir;
                                           Choice_Type : Iir;
                                           Low : out Iir;
                                           High : out Iir;
                                           Loc : Location_Type;
                                           Is_Sub_Range : Boolean)
   is
      --  Nodes that can appear.
      Info : Choice_Info_Type;

      Type_Has_Bounds : Boolean;
   begin
      --  Set TYPE_HAS_BOUNDS
      case Get_Kind (Choice_Type) is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            Type_Has_Bounds := True;
         when Iir_Kind_Integer_Type_Definition =>
            Type_Has_Bounds := False;
         when others =>
            Error_Kind ("sem_check_continuous_choices(3)", Choice_Type);
      end case;

      --  Check the choices are within the bounds.
      if Type_Has_Bounds
        and then Get_Type_Staticness (Choice_Type) = Locally
      then
         declare
            Choice : Iir;
            Ok : Boolean;
            Has_Err : Boolean;
            Expr : Iir;
         begin
            Has_Err := False;
            Choice := Choice_Chain;
            while Choice /= Null_Iir loop
               Ok := True;
               case Iir_Kinds_Case_Choice (Get_Kind (Choice)) is
                  when Iir_Kind_Choice_By_Expression =>
                     Expr := Get_Choice_Expression (Choice);
                     if Get_Expr_Staticness (Expr) = Locally then
                        Ok := Eval_Is_In_Bound (Expr, Choice_Type);
                     end if;
                  when Iir_Kind_Choice_By_Range =>
                     Expr := Get_Choice_Range (Choice);
                     Expr := Get_Range_From_Discrete_Range (Expr);
                     if Get_Expr_Staticness (Expr) = Locally then
                        Ok := Eval_Is_Range_In_Bound (Expr, Choice_Type, True);
                     end if;
                  when Iir_Kind_Choice_By_Others =>
                     null;
               end case;
               if not Ok then
                  Error_Msg_Sem (+Choice, "%n out of index range", +Expr);
                  Has_Err := True;
               end if;
               Choice := Get_Chain (Choice);
            end loop;

            --  In case of error (value not in range), don't try to extract
            --  bounds or to sort values.
            if Has_Err then
               High := Null_Iir;
               Low := Null_Iir;
               return;
            end if;
         end;
      end if;

      --  Compute the number of elements and sort.
      Count_Choices (Info, Choice_Chain);
      Fill_Choices_Array (Info, Choice_Chain);
      Sort_Discrete_Choices (Info);

      --  Set low and high bounds.
      if Info.Nbr_Choices > 0 then
         Low := Get_Assoc_Low (Info.Arr (Info.Arr'First));
         High := Get_Assoc_High (Info.Arr (Info.Arr'Last));
      else
         Low := Null_Iir;
         High := Null_Iir;
      end if;

      --  Fourth:
      --  check for lacking choice (if no others)
      --  check for overlapping choices
      declare
         --  Emit an error message for absence of choices in position L to H
         --  of index type BT at location LOC.
         procedure Error_No_Choice (Bt : Iir;
                                    L, H : Int64;
                                    Loc : Location_Type) is
         begin
            if L = H then
               Error_Msg_Sem (+Loc, "no choice for " & Disp_Discrete (Bt, L));
            else
               Error_Msg_Sem
                 (+Loc, "no choices for " & Disp_Discrete (Bt, L)
                    & " to " & Disp_Discrete (Bt, H));
            end if;
         end Error_No_Choice;

         --  Lowest and highest bounds.
         Lb, Hb : Iir;
         Pos : Int64;
         Pos_Max : Int64;
         E_Pos : Int64;
         Choice : Iir;
         Need_Others : Boolean;

         Bt : constant Iir := Get_Base_Type (Choice_Type);
      begin
         if not Is_Sub_Range
           and then Get_Type_Staticness (Choice_Type) = Locally
           and then Type_Has_Bounds
         then
            Get_Low_High_Limit (Get_Range_Constraint (Choice_Type), Lb, Hb);
         else
            Lb := Low;
            Hb := High;
         end if;
         if Lb = Null_Iir or else Hb = Null_Iir then
            --  Return now in case of error.
            Free (Info.Arr);
            return;
         end if;
         --  Checks all values between POS and POS_MAX are handled.
         Pos := Eval_Pos (Lb);
         Pos_Max := Eval_Pos (Hb);
         if Pos > Pos_Max then
            --  Null range.
            Free (Info.Arr);
            return;
         end if;
         Need_Others := False;
         for I in Info.Arr'Range loop
            Choice := Info.Arr (I);
            E_Pos := Eval_Pos (Get_Assoc_Low (Choice));
            if E_Pos > Pos_Max then
               --  Choice out of bound, already handled.
               Error_No_Choice (Bt, Pos, Pos_Max, Get_Location (Choice));
               --  Avoid other errors.
               Pos := Pos_Max + 1;
               exit;
            end if;
            if Pos < E_Pos then
               Need_Others := True;
               if Info.Others_Choice = Null_Iir then
                  Error_No_Choice (Bt, Pos, E_Pos - 1, Get_Location (Choice));
               end if;
            elsif Pos > E_Pos then
               Need_Others := True;
               if Pos = E_Pos + 1 then
                  Error_Msg_Sem
                    (+Choice,
                     "duplicate choice for " & Disp_Discrete (Bt, E_Pos));
               else
                  Error_Msg_Sem
                    (+Choice, "duplicate choices for "
                       & Disp_Discrete (Bt, E_Pos)
                       & " to " & Disp_Discrete (Bt, Pos));
               end if;
            end if;

            if Get_Kind (Choice) = Iir_Kind_Choice_By_Range then
               Pos := Eval_Pos (Get_Assoc_High (Choice)) + 1;
            else
               Pos := E_Pos + 1;
            end if;
         end loop;
         if Pos /= Pos_Max + 1 then
            Need_Others := True;
            if Info.Others_Choice = Null_Iir then
               Error_No_Choice (Bt, Pos, Pos_Max, Loc);
            end if;
         end if;

         if not Need_Others and then Info.Others_Choice /= Null_Iir then
            Warning_Msg_Sem (Warnid_Others, +Info.Others_Choice,
                             "redundant 'others' choices");
         end if;
      end;

      --  LRM93 7.3.2.2 Array aggregates
      --  An others choice is locally static if the applicable index constraint
      --  if locally static.
      if Info.Nbr_Choices > 0
        and then Info.Others_Choice /= Null_Iir
        and then Get_Type_Staticness (Choice_Type) /= Locally
      then
         Warning_Msg_Sem
           (Warnid_Static, +Info.Others_Choice,
            "'others' choice allowed only if the index constraint is static");
      end if;

      Free (Info.Arr);
   end Sem_Check_Continuous_Choices;

   procedure Sem_Choices_Range (Choice_Chain : in out Iir;
                                Choice_Type : Iir;
                                Low : out Iir;
                                High : out Iir;
                                Loc : Location_Type;
                                Is_Sub_Range : Boolean;
                                Is_Case_Stmt : Boolean)
   is
      --  Number of positionnal choice.
      Nbr_Pos : Int64;

      --  Number of named choices.
      Nbr_Named : Natural;

      --  True if others choice is present.
      Has_Others : Boolean;

      --  True if one association doesn't have the element_type flag (ie the
      --  expression is of the same type as an aggregate).
      Has_Array : Boolean;

      Has_Error : Boolean;

      Pos_Max : Int64;
      El : Iir;
      Prev_El : Iir;

      --  Staticness of the current choice.
      Choice_Staticness : Iir_Staticness;

      --  Staticness of all the choices.
      Staticness : Iir_Staticness;

      --  The choice was parsed as a choice by expression, but in fact the
      --  expression is a range (eg: a subtype name).  Change the choice by
      --  a range choice.
      function Replace_By_Range_Choice (Name : Iir; Range_Type : Iir)
                                       return Boolean
      is
         N_Choice : Iir;
         Name1 : Iir;
      begin
         if Are_Types_Compatible (Range_Type, Choice_Type) = Not_Compatible
         then
            Error_Not_Match (Name, Choice_Type);
            return False;
         end if;

         Name1 := Finish_Sem_Name (Name);
         N_Choice := Create_Iir (Iir_Kind_Choice_By_Range);
         Location_Copy (N_Choice, El);
         Set_Chain (N_Choice, Get_Chain (El));
         Set_Associated_Expr (N_Choice, Get_Associated_Expr (El));
         Set_Associated_Chain (N_Choice, Get_Associated_Chain (El));
         Set_Same_Alternative_Flag (N_Choice, Get_Same_Alternative_Flag (El));
         Set_Choice_Range (N_Choice, Eval_Range_If_Static (Name1));
         Set_Choice_Staticness (N_Choice, Get_Type_Staticness (Range_Type));
         Set_Element_Type_Flag (N_Choice, Get_Element_Type_Flag (El));
         Free_Iir (El);

         if Prev_El = Null_Iir then
            Choice_Chain := N_Choice;
         else
            Set_Chain (Prev_El, N_Choice);
         end if;
         El := N_Choice;

         return True;
      end Replace_By_Range_Choice;

      --  Analyze a simple (by expression or by range) choice.
      --  Return FALSE in case of error.
      function Sem_Simple_Choice return Boolean
      is
         Expr : Iir;
         Ent : Iir;
      begin
         if Get_Kind (El) = Iir_Kind_Choice_By_Range then
            Expr := Get_Choice_Range (El);
            Expr := Sem_Discrete_Range (Expr, Choice_Type, True);
            if Expr = Null_Iir then
               return False;
            end if;
            case Get_Kind (Expr) is
               when Iir_Kind_Range_Expression
                 | Iir_Kinds_Range_Attribute
                 | Iir_Kinds_Denoting_Name =>
                  Expr := Eval_Range_If_Static (Expr);
                  Set_Choice_Staticness (El, Get_Expr_Staticness (Expr));
               when Iir_Kinds_Scalar_Subtype_Definition =>
                  Set_Choice_Staticness (El, Get_Type_Staticness (Expr));
               when others =>
                  Error_Kind ("sem_sime_choice(1)", Expr);
            end case;
            Set_Choice_Range (El, Expr);
         else
            Expr := Get_Choice_Expression (El);
            case Get_Kind (Expr) is
               when Iir_Kind_Selected_Name
                 | Iir_Kind_Simple_Name
                 | Iir_Kind_Character_Literal
                 | Iir_Kind_Parenthesis_Name
                 | Iir_Kind_Selected_By_All_Name
                 | Iir_Kind_Attribute_Name =>
                  Sem_Name (Expr);
                  Ent := Get_Named_Entity (Expr);
                  if Ent = Error_Mark then
                     return False;
                  end if;

                  --  So range or expression ?
                  --  FIXME: share code with sem_name for slice/index.
                  case Get_Kind (Ent) is
                     when Iir_Kind_Range_Array_Attribute
                       | Iir_Kind_Reverse_Range_Array_Attribute
                       | Iir_Kind_Range_Expression =>
                        return Replace_By_Range_Choice (Expr, Ent);
                     when Iir_Kind_Subtype_Declaration
                       | Iir_Kind_Type_Declaration =>
                        Ent := Is_Type_Name (Expr);
                        Set_Expr_Staticness (Expr, Get_Type_Staticness (Ent));
                        return Replace_By_Range_Choice (Expr, Ent);
                     when others =>
                        Expr := Name_To_Expression
                          (Expr, Get_Base_Type (Choice_Type));
                  end case;
               when others =>
                  Expr :=
                    Sem_Expression_Ov (Expr, Get_Base_Type (Choice_Type));
            end case;
            if Expr = Null_Iir then
               return False;
            end if;
            Expr := Eval_Expr_If_Static (Expr);
            Set_Choice_Expression (El, Expr);
            Set_Choice_Staticness (El, Get_Expr_Staticness (Expr));
         end if;
         return True;
      end Sem_Simple_Choice;
   begin
      Low := Null_Iir;
      High := Null_Iir;

      --  First:
      --  Analyze the choices
      --  compute the range of positionnal choices
      --  compute the number of choice elements (extracted from lists).
      --  check for others presence.
      Nbr_Pos := 0;
      Nbr_Named := 0;
      Has_Others := False;
      Has_Error := False;
      Has_Array := False;
      Staticness := Locally;
      El := Choice_Chain;
      Prev_El := Null_Iir;
      while El /= Null_Iir loop
         if not Get_Element_Type_Flag (El) then
            Has_Array := True;
         end if;
         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               Nbr_Pos := Nbr_Pos + 1;
            when Iir_Kind_Choice_By_Expression
              | Iir_Kind_Choice_By_Range =>
               if Sem_Simple_Choice then
                  Choice_Staticness := Get_Choice_Staticness (El);
                  Staticness := Min (Staticness, Choice_Staticness);
                  if Choice_Staticness /= Locally
                    and then Is_Case_Stmt
                  then
                     --  FIXME: explain why
                     Error_Msg_Sem (+El, "choice is not locally static");
                  end if;
               else
                  Has_Error := True;
               end if;
               Nbr_Named := Nbr_Named + 1;
            when Iir_Kind_Choice_By_Name =>
               --  It is not possible to have such a choice in an array
               --  aggregate.
               --  Should have been caught previously.
               raise Internal_Error;
            when Iir_Kind_Choice_By_Others =>
               if Has_Others then
                  Error_Msg_Sem (+El, "duplicate others choice");
               elsif Get_Chain (El) /= Null_Iir then
                  Error_Msg_Sem
                    (+El, "choice others should be the last alternative");
               end if;
               Has_Others := True;
            when others =>
               Error_Kind ("sem_choices_range", El);
         end case;
         Prev_El := El;
         El := Get_Chain (El);
      end loop;

      if Has_Error then
         --  Nothing can be done here...
         return;
      end if;
      if Nbr_Pos > 0 and then Nbr_Named > 0 then
         --  LRM93 7.3.2.2
         --  Apart from the final element with the single choice OTHERS, the
         --  rest (if any) of the element associations of an array aggregate
         --  must be either all positionnal or all named.
         Error_Msg_Sem
           (+Loc, "element associations must be all positional or all named");
         return;
      end if;

      --  For a positional aggregate.
      if Nbr_Pos > 0 then
         --  Check number of elements match, but only if it is possible.
         if Get_Type_Staticness (Choice_Type) /= Locally then
            return;
         end if;
         Pos_Max := Eval_Discrete_Type_Length (Choice_Type);
         if (not Has_Others and not Is_Sub_Range)
           and then Nbr_Pos < Pos_Max
           --  For aggregates, a positional association can be a vector.
           and then (Vhdl_Std < Vhdl_08 or Is_Case_Stmt or not Has_Array)
         then
            Error_Msg_Sem (+Loc, "not enough elements associated");
         elsif Nbr_Pos > Pos_Max then
            Error_Msg_Sem (+Loc, "too many elements associated");
         end if;
         return;
      end if;

      --  Second:
      --  Create the list of choices
      if Nbr_Named = 0 and then Has_Others then
         --  This is only a others association.
         return;
      end if;
      if Staticness /= Locally then
         --  Emit a message for aggregrate.  The message has already been
         --  emitted for a case stmt.
         --  FIXME: what about individual associations?
         if not Is_Case_Stmt then
            --  LRM93 7.3.2.2
            --  A named association of an array aggregate is allowed to have
            --  a choice that is not locally static, or likewise a choice that
            --  is a null range, only if the aggregate includes a single
            --  element association and the element association has a single
            --  choice.
            if Nbr_Named > 1 or Has_Others then
               Error_Msg_Sem (+Loc, "not static choice exclude others choice");
            end if;
         end if;
         return;
      end if;

      Sem_Check_Continuous_Choices
        (Choice_Chain, Choice_Type, Low, High, Loc, Is_Sub_Range);
   end Sem_Choices_Range;

   -- Perform semantisation on a (sub)aggregate AGGR, which is of type
   -- A_TYPE.
   -- return FALSE is case of failure
   function Sem_Record_Aggregate
     (Aggr : Iir_Aggregate; A_Type : Iir; Constrained : Boolean)
     return boolean
   is
      El_List : constant Iir_Flist := Get_Elements_Declaration_List (A_Type);

      --  Type of the element.
      El_Type : Iir;

      Matches: Iir_Array (0 .. Get_Nbr_Elements (El_List) - 1);
      Ok : Boolean;

      --  Add a choice for element REC_EL.
      --  Checks the element is not already associated.
      --  Checks type of expression is compatible with type of element.
      procedure Add_Match (El : Iir; Rec_El : Iir_Element_Declaration)
      is
         Ass_Type : Iir;
         Pos : constant Natural := Natural (Get_Element_Position (Rec_El));
      begin
         if Matches (Pos) /= Null_Iir then
            Error_Msg_Sem (+El, "%n was already associated", +Matches (Pos));
            Ok := False;
            return;
         end if;
         Matches (Pos) := El;

         --  LRM 7.3.2.1  Record aggregates
         --  An element association with more than once choice, [...], is
         --  only allowed if the elements specified are all of the same type.
         Ass_Type := Get_Type (Rec_El);
         if El_Type = Null_Iir then
            El_Type := Ass_Type;
         elsif Are_Types_Compatible (El_Type, Ass_Type) = Not_Compatible then
            Error_Msg_Sem (+El, "elements are not of the same type");
            Ok := False;
         end if;
      end Add_Match;

      --  Analyze a simple choice: extract the record element corresponding
      --  to the expression, and create a choice_by_name.
      --  FIXME: should mutate the node.
      function Sem_Simple_Choice (Ass : Iir) return Iir
      is
         Expr : constant Iir := Get_Choice_Expression (Ass);
         N_El : Iir;
         Aggr_El : Iir_Element_Declaration;
      begin
         if Get_Kind (Expr) /= Iir_Kind_Simple_Name then
            Error_Msg_Sem (+Ass, "element association must be a simple name");
            Ok := False;
            return Ass;
         end if;
         Aggr_El := Find_Name_In_Flist (El_List, Get_Identifier (Expr));
         if Aggr_El = Null_Iir then
            Error_Msg_Sem (+Ass, "record has no such element %n", +Ass);
            Ok := False;
            return Ass;
         end if;
         Set_Named_Entity (Expr, Aggr_El);
         Xref_Ref (Expr, Aggr_El);

         --  Was a choice_by_expression, now by_name.
         N_El := Create_Iir (Iir_Kind_Choice_By_Name);
         Location_Copy (N_El, Ass);
         Set_Choice_Name (N_El, Expr);
         Set_Associated_Expr (N_El, Get_Associated_Expr (Ass));
         Set_Associated_Chain (N_El, Get_Associated_Chain (Ass));
         Set_Chain (N_El, Get_Chain (Ass));
         Set_Same_Alternative_Flag (N_El, Get_Same_Alternative_Flag (Ass));

         Free_Iir (Ass);
         Add_Match (N_El, Aggr_El);
         return N_El;
      end Sem_Simple_Choice;

      Assoc_Chain : Iir;
      El, Prev_El : Iir;
      Expr: Iir;
      Has_Named : Boolean;
      Rec_El_Index : Natural;
      Expr_Staticness : Iir_Staticness;

      --  True if at least one element constrains the subtype.  For unbounded
      --  records.
      Add_Constraints : Boolean;
   begin
      Set_Aggregate_Expand_Flag (Aggr, True);

      Ok := True;
      Assoc_Chain := Get_Association_Choices_Chain (Aggr);
      Matches := (others => Null_Iir);
      Expr_Staticness := Locally;
      Add_Constraints := False;

      El_Type := Null_Iir;
      Has_Named := False;
      Rec_El_Index := 0;
      Prev_El := Null_Iir;
      El := Assoc_Chain;
      while El /= Null_Iir loop
         Expr := Get_Associated_Expr (El);

         --  If there is an associated expression with the choice, then the
         --  choice is a new alternative, and has no expected type.
         if not Get_Same_Alternative_Flag (El) then
            pragma Assert (Expr /= Null_Iir);
            El_Type := Null_Iir;
         end if;

         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               if Has_Named then
                  Error_Msg_Sem
                    (+El, "positional association after named one");
                  Ok := False;
               elsif Rec_El_Index > Matches'Last then
                  Error_Msg_Sem (+El, "too many elements");
                  exit;
               else
                  Add_Match (El, Get_Nth_Element (El_List, Rec_El_Index));
                  Rec_El_Index := Rec_El_Index + 1;
               end if;
            when Iir_Kind_Choice_By_Expression =>
               Has_Named := True;
               El := Sem_Simple_Choice (El);
               --  This creates a choice_by_name, which replaces the
               --  choice_by_expression.
               if Prev_El = Null_Iir then
                  Set_Association_Choices_Chain (Aggr, El);
               else
                  Set_Chain (Prev_El, El);
               end if;
            when Iir_Kind_Choice_By_Others =>
               Has_Named := True;
               if Get_Chain (El) /= Null_Iir then
                  Error_Msg_Sem
                    (+El, "choice others must be the last alternative");
               end if;
               declare
                  Found : Boolean := False;
               begin
                  for I in Matches'Range loop
                     if Matches (I) = Null_Iir then
                        Add_Match (El, Get_Nth_Element (El_List, I));
                        Found := True;
                     end if;
                  end loop;
                  if not Found then
                     --  LRM08 9.3.3.2 Record aggregates
                     --  If the choise OTHERS is given as a choice, it shall
                     --  represent at least one element.
                     --  GHDL: so that the type of the associated expression
                     --   is known.
                     Error_Msg_Sem (+El, "no element for choice others");
                     Ok := False;
                  end if;
               end;
            when others =>
               Error_Kind ("sem_record_aggregate", El);
         end case;

         --  Analyze the expression associated.
         if not Get_Same_Alternative_Flag (El) then
            if El_Type /= Null_Iir then
               --  Analyze the expression only if the choice is correct.
               Expr := Sem_Expression_Wildcard
                 (Expr, El_Type, Constrained);
               if Expr /= Null_Iir then
                  Set_Associated_Expr
                    (El, Eval_Expr_Check_If_Static (Expr, El_Type));
                  Expr_Staticness := Min (Expr_Staticness,
                                          Get_Expr_Staticness (Expr));
                  if not Add_Constraints
                    and then Is_Fully_Constrained_Type (Get_Type (Expr))
                    and then not Is_Fully_Constrained_Type (El_Type)
                  then
                     Add_Constraints := True;
                  end if;
                  if not Is_Static_Construct (Expr) then
                     Set_Aggregate_Expand_Flag (Aggr, False);
                  end if;
               else
                  Ok := False;
               end if;
            else
               --  This case is not possible unless there is an error.
               pragma Assert (not Ok);
               null;
            end if;
         end if;

         Prev_El := El;
         El := Get_Chain (El);
      end loop;

      if Has_Named then
         --  TODO: support named element on expanded aggregate
         Set_Aggregate_Expand_Flag (Aggr, False);
      end if;

      --  Check for missing associations.
      for I in Matches'Range loop
         if Matches (I) = Null_Iir then
            Error_Msg_Sem
              (+Aggr, "no value for %n", +Get_Nth_Element (El_List, I));
            Ok := False;
         end if;
      end loop;
      Set_Expr_Staticness (Aggr, Min (Get_Expr_Staticness (Aggr),
                                      Expr_Staticness));

      --  Create a constrained subtype for the aggregate type
      if Ok and Add_Constraints then
         declare
            Rec_Type : Iir;
            Rec_El_List : Iir_Flist;
            Rec_El : Iir;
            Rec_El_Type : Iir;
            New_Rec_El : Iir;
            Assoc_Expr : Iir;
            Constraint : Iir_Constraint;
            Composite_Found : Boolean;
            Staticness : Iir_Staticness;
         begin
            Rec_Type := Sem_Types.Copy_Subtype_Indication (Get_Type (Aggr));
            Rec_El_List := Get_Elements_Declaration_List (Rec_Type);
            Constraint := Fully_Constrained;
            Composite_Found := False;
            Staticness := Locally;
            for I in Flist_First .. Flist_Last (El_List) loop
               El := Matches (I);
               Assoc_Expr := Get_Associated_Expr (El);
               El_Type := Get_Type (Assoc_Expr);
               Rec_El := Get_Nth_Element (Rec_El_List, I);
               Rec_El_Type := Get_Type (Rec_El);
               if Is_Fully_Constrained_Type (El_Type)
                 and then not Is_Fully_Constrained_Type (Rec_El_Type)
               then
                  Rec_El_Type := El_Type;
                  New_Rec_El :=
                    Create_Iir (Iir_Kind_Record_Element_Constraint);
                  Location_Copy (New_Rec_El, Rec_El);
                  Set_Parent (New_Rec_El, Rec_Type);
                  Set_Identifier (New_Rec_El, Get_Identifier (Rec_El));
                  pragma Assert (I = Natural (Get_Element_Position (Rec_El)));
                  Set_Element_Position (New_Rec_El, Iir_Index32 (I));
                  Set_Nth_Element (Rec_El_List, I, New_Rec_El);
                  Set_Type (New_Rec_El, Rec_El_Type);
                  Append_Owned_Element_Constraint (Rec_Type, New_Rec_El);
               end if;
               Staticness := Min (Staticness,
                                  Get_Type_Staticness (Rec_El_Type));
               Sem_Types.Update_Record_Constraint
                 (Constraint, Composite_Found, Rec_El_Type);
            end loop;
            Set_Type_Staticness (Rec_Type, Staticness);
            Set_Constraint_State (Rec_Type, Constraint);
            Set_Type (Aggr, Rec_Type);
            Set_Literal_Subtype (Aggr, Rec_Type);
         end;
      end if;

      return Ok;
   end Sem_Record_Aggregate;

   --  Information for each dimension of an aggregate.
   type Array_Aggr_Info is record
      --  False if one sub-aggregate has no others choices.
      --  If FALSE, the dimension is constrained.
      Has_Others : Boolean := True;

      --  True if one sub-aggregate is by named/by position.
      Has_Named : Boolean := False;

      --  True if one sub-aggregate is dynamic.
      Has_Dynamic : Boolean := False;

      --  LOW and HIGH limits for the dimension.
      Low : Iir := Null_Iir;
      High : Iir := Null_Iir;

      --  Minimum length of the dimension.  This is a minimax.
      Min_Length : Natural := 0;

      --  If not NULL_IIR, this is the bounds of the dimension.
      --  If every dimension has bounds, then the aggregate is constrained.
      Index_Subtype : Iir := Null_Iir;

      --  Number of associations in last-level (not for sub-aggregate).  This
      --  is used only to decide whether or not a static aggregate can be
      --  expanded.
      Nbr_Assocs : Natural := 0;

      --  True if there is an error.
      Error : Boolean := False;

      --  True if one element doesn't match the bounds.
      Has_Bound_Error : Boolean := False;
   end record;

   type Array_Aggr_Info_Arr is array (Natural range <>) of Array_Aggr_Info;

   procedure Sem_Array_Aggregate_Elements
     (Aggr : Iir;
      A_Type : Iir;
      Expr_Staticness : in out Iir_Staticness;
      Info : in out Array_Aggr_Info)
   is
      Element_Type : constant Iir := Get_Element_Subtype (A_Type);
      El : Iir;
      El_Expr : Iir;
      Expr : Iir;
      El_Staticness : Iir_Staticness;
      Assoc_Chain : Iir;
      Res_Type : Iir;

      --  True if the type of the expression is the type of the aggregate.
      Is_Array : Boolean;

      --  Null_Iir if the type of aggregagte elements myst be of the element
      --  type.
      Elements_Types : Iir;
      Elements_Types_List : Iir_List;
   begin
      --  LRM93 7.3.2.2 Array aggregates
      --  [...] the expression of each element association must be of the
      --  element type.

      --  LRM08 9.3.3.3 Array aggregates
      --  For an aggregate of a one-dimensional array type, [each choice shall
      --  specify values of the index type], and the expression of each element
      --  association shall be of either the element type or the type of the
      --  aggregate.
      if Flags.Vhdl_Std >= Vhdl_08
        and then Is_One_Dimensional_Array_Type (A_Type)
      then
         Elements_Types_List := Create_Iir_List;
         Append_Element (Elements_Types_List, Element_Type);
         Append_Element (Elements_Types_List, Get_Base_Type (A_Type));
         Elements_Types := Create_Overload_List (Elements_Types_List);
      else
         Elements_Types := Null_Iir;
      end if;

      Assoc_Chain := Get_Association_Choices_Chain (Aggr);

      El := Assoc_Chain;
      while El /= Null_Iir loop
         if not Get_Same_Alternative_Flag (El) then
            El_Expr := Get_Associated_Expr (El);
            Is_Array := False;

            --  Directly analyze the expression with the type of the element
            --  if it cannot be the type of the aggregate.
            --  In VHDL-2008, also do it when the expression is an aggregate.
            --  This is not in the LRM, but otherwise this would create a lot
            --  of ambiguities when the element type is a composite type.  Eg:
            --
            --    type time_unit is record
            --      val : time;
            --      name : string (1 to 3);
            --    end record;
            --    type time_names_type is array (1 to 2) of time_unit;
            --    constant time_names : time_names_type :=
            --      ((fs, "fs "), (ps, "ps "));
            --
            --  The type of the first sub-aggregate could be either time_unit
            --  or time_names_type.  Because it's determined by the context,
            --  it is ambiguous.  But there is no point in using aggregates
            --  to specify a range of choices.
            --  FIXME: fix LRM ?

            --  LRM08 9.3.3.3 Array aggregates
            --  If the type of the expression of an element association is the
            --  type of the aggregate, then either the element association
            --  shall be positional or the choice shall be a discrete range.
            if Elements_Types = Null_Iir
              or else not Kind_In (El, Iir_Kind_Choice_By_None,
                                   Iir_Kind_Choice_By_Range)
              or else Get_Kind (El_Expr) = Iir_Kind_Aggregate
            then
               Expr := Sem_Expression (El_Expr, Element_Type);
            else
               Expr := Sem_Expression_Wildcard (El_Expr, Null_Iir);
               if Expr /= Null_Iir then
                  Res_Type := Compatible_Types_Intersect
                    (Get_Type (Expr), Elements_Types);
                  if Res_Type = Null_Iir then
                     Error_Msg_Sem
                       (+Get_Associated_Expr (El),
                        "type of element not compatible with the "
                          & "expected type");
                     Set_Type (Expr, Error_Type);
                     Set_Associated_Expr (El, Expr);
                     Expr := Null_Iir;
                  elsif Is_Overload_List (Res_Type) then
                     Error_Msg_Sem
                       (+Expr, "type of element is ambiguous");
                     Free_Overload_List (Res_Type);
                     Set_Type (El_Expr, Error_Type);
                     Expr := Null_Iir;
                  else
                     pragma Assert (Is_Defined_Type (Res_Type));
                     Is_Array :=
                       Get_Base_Type (Res_Type) = Get_Base_Type (A_Type);
                     Expr := Sem_Expression_Wildcard (Expr, Res_Type);
                  end if;
               end if;
            end if;

            if Expr /= Null_Iir then
               El_Staticness := Get_Expr_Staticness (Expr);
               Expr := Eval_Expr_If_Static (Expr);
               Set_Associated_Expr (El, Expr);

               if not Is_Static_Construct (Expr) then
                  Set_Aggregate_Expand_Flag (Aggr, False);
               end if;

               if not Is_Array
                 and then not Eval_Is_In_Bound (Expr, Element_Type)
               then
                  Info.Has_Bound_Error := True;
                  Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                                   "element is out of the bounds");
               end if;

               Expr_Staticness := Min (Expr_Staticness, El_Staticness);

               Info.Nbr_Assocs := Info.Nbr_Assocs + 1;
            else
               Info.Error := True;
            end if;
         end if;

         Set_Element_Type_Flag (El, not Is_Array);

         if Is_Array then
            --  LRM08 9.3.3.3 Array aggregates
            --  If the type of the expression of an element association
            --  is the type of the aggregate, then either the element
            --  association shall be positional or the choice shall be
            --  a discrete range.

            --  GHDL: must be checked for all associations, so do it outside
            --  the above 'if' statement.
            --  GHDL: improve error message.
            case Get_Kind (El) is
               when Iir_Kind_Choice_By_None
                 | Iir_Kind_Choice_By_Range =>
                  null;
               when Iir_Kind_Choice_By_Others =>
                  Error_Msg_Sem
                    (+El, "expression for 'others' must be an element");
               when others =>
                  Error_Msg_Sem
                    (+El, "positional association or "
                       & "discrete range choice required");
            end case;
         end if;

         El := Get_Chain (El);
      end loop;

      if Elements_Types /= Null_Iir then
         Free_Overload_List (Elements_Types);
      end if;
   end Sem_Array_Aggregate_Elements;

   procedure Sem_Array_Aggregate_Choice_Length
     (Choice : Iir;
      Len : in out Natural;
      Len_Staticness : in out Iir_Staticness)
   is
      --  Extract length from associated expression.
      --  Always has an associated expr, as not named.
      Expr : constant Iir := Get_Associated_Expr (Choice);
      Expr_Type : constant Iir := Get_Type (Expr);
      Expr_Index : Iir;
      Index_Staticness : Iir_Staticness;
   begin
      if Is_Error (Expr_Type) then
         return;
      end if;
      if Get_Constraint_State (Expr_Type) /= Fully_Constrained then
         Len_Staticness := None;
         return;
      end if;

      Expr_Index := Get_Index_Type (Expr_Type, 0);
      Index_Staticness := Get_Type_Staticness (Expr_Index);
      case Index_Staticness is
         when Locally =>
            Len := Len + Natural
              (Eval_Discrete_Type_Length (Expr_Index));
         when Globally | None =>
            Len_Staticness := Nodes.Min
              (Len_Staticness, Index_Staticness);
         when Unknown =>
            --  Must have been caught by Is_Error.
            raise Internal_Error;
      end case;
   end Sem_Array_Aggregate_Choice_Length;

   procedure Sem_Array_Aggregate_Extract_Element_Subtype
     (Aggr : Iir; Dim : Natural; Nbr_Dim : Natural; El_Subtype : in out Iir)
   is
      Assoc : Iir;
      Sub_Aggr : Iir;
      New_El_Subtype : Iir;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      while Assoc /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Sub_Aggr := Get_Associated_Expr (Assoc);
            if Dim < Nbr_Dim then
               case Get_Kind (Sub_Aggr) is
                  when Iir_Kind_Aggregate =>
                     Sem_Array_Aggregate_Extract_Element_Subtype
                       (Sub_Aggr, Dim + 1, Nbr_Dim, El_Subtype);
                     --  TODO: only if locally static ?
                     if El_Subtype /= Null_Iir then
                        return;
                     end if;
                  when Iir_Kind_String_Literal8 =>
                     --  If a string is a proper subaggregate, then the element
                     --  subtype must be fully bounded.
                     raise Internal_Error;
                  when others =>
                     null;
               end case;
            else
               New_El_Subtype := Get_Type (Sub_Aggr);
               if not Get_Element_Type_Flag (Assoc) then
                  New_El_Subtype := Get_Element_Subtype (New_El_Subtype);
               end if;
               --  TODO: try to extract the 'best' element subtype: with
               --   static indexes, with constrained sub-elements.
               --   Possibly create an hybrid subtype (for records).
               if Get_Constraint_State (New_El_Subtype) = Fully_Constrained
               then
                  El_Subtype := New_El_Subtype;
                  return;
               end if;
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Sem_Array_Aggregate_Extract_Element_Subtype;

   procedure Check_Matching_Subtype (Expr : Iir; St : Iir)
   is
      Et : constant Iir := Get_Type (Expr);
   begin
      case Get_Kind (St) is
         when Iir_Kind_Array_Subtype_Definition =>
            if Get_Kind (Et) /= Iir_Kind_Array_Subtype_Definition then
               return;
            end if;
            --  Fast check.
            if Et = St then
               return;
            end if;

            --  Check indexes.
            if Get_Index_Constraint_Flag (St)
              and then Get_Index_Constraint_Flag (Et)
            then
               declare
                  Eil : constant Iir_Flist := Get_Index_Subtype_List (Et);
                  Sil : constant Iir_Flist := Get_Index_Subtype_List (St);
                  Ei, Si : Iir;
               begin
                  for I in Flist_First .. Flist_Last (Eil) loop
                     Ei := Get_Nth_Element (Eil, I);
                     Si := Get_Nth_Element (Sil, I);
                     if Get_Type_Staticness (Ei) = Locally
                       and then Get_Type_Staticness (Si) = Locally
                       and then (Eval_Discrete_Type_Length (Si)
                                   /= Eval_Discrete_Type_Length (Ei))
                     then
                        Warning_Msg_Sem
                          (Warnid_Runtime_Error, +Expr,
                           "expression subtype doesn't match "
                             & "aggregate element subtype");
                        return;
                     end if;
                  end loop;
               end;
            end if;

            --  TODO: element array element ?
         when Iir_Kind_Record_Subtype_Definition =>
            --  TODO
            null;
         when others =>
            null;
      end case;
   end Check_Matching_Subtype;

   --  Check the subtype of all elements of AGGR match EL_SUBTYPE.
   --  Used only if the aggregate element subtype is extracted from an
   --  element of the aggregate.  In that case, we should check the match.
   procedure Sem_Array_Aggregate_Check_Element_Subtype (El_Subtype : Iir;
                                                        Aggr : Iir;
                                                        Dim : Natural;
                                                        Nbr_Dim : Natural)
   is
      Assoc : Iir;
      Sub_Aggr : Iir;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      while Assoc /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Sub_Aggr := Get_Associated_Expr (Assoc);
            if Dim < Nbr_Dim then
               --  If a string is a proper subaggregate, then the element
               --  subtype must be fully bounded.
               pragma Assert (Get_Kind (Sub_Aggr) = Iir_Kind_Aggregate);
               Sem_Array_Aggregate_Check_Element_Subtype
                 (El_Subtype, Sub_Aggr, Dim + 1, Nbr_Dim);
            else
               if Get_Element_Type_Flag (Assoc) then
                  --  TODO: only report the first error ?
                  Check_Matching_Subtype (Sub_Aggr, El_Subtype);
               end if;
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Sem_Array_Aggregate_Check_Element_Subtype;

   --  Analyze an array aggregate AGGR of *base type* A_TYPE.
   --  The type of the array is computed into A_SUBTYPE.
   --  DIM is the dimension index in A_TYPE.
   --  Return FALSE in case of error.
   procedure Sem_Array_Aggregate_1 (Aggr: Iir;
                                    A_Type: Iir;
                                    Infos : in out Array_Aggr_Info_Arr;
                                    Constrained : Boolean;
                                    Dim: Natural)
   is
      Index_List : constant Iir_Flist := Get_Index_Subtype_List (A_Type);

      --  Type of the index (this is also the type of the choices).
      Index_Type : constant Iir := Get_Index_Type (Index_List, Dim - 1);

      Assoc_Chain : Iir;
      Choice: Iir;
      Is_Positional: Tri_State_Type;
      Has_Positional_Choice: Boolean;
      Low, High : Iir;
      Has_Others : Boolean;

      Len : Natural;

      Index_Subtype_Constraint : Iir_Range_Expression;
      Index_Constraint : Iir_Range_Expression; -- FIXME: 'range.
      Dir : Direction_Type;
      Choice_Staticness : Iir_Staticness;
      Len_Staticness : Iir_Staticness;
      Expr_Staticness : Iir_Staticness;

      Info : Array_Aggr_Info renames Infos (Dim);
   begin
      --  Analyze choices (for aggregate but not for strings).
      if Get_Kind (Aggr) = Iir_Kind_Aggregate then
         --  By default, consider the aggregate can be statically built.
         Set_Aggregate_Expand_Flag (Aggr, True);

         Assoc_Chain := Get_Association_Choices_Chain (Aggr);
         Sem_Choices_Range (Assoc_Chain, Index_Type, Low, High,
                            Get_Location (Aggr), not Constrained, False);
         Set_Association_Choices_Chain (Aggr, Assoc_Chain);

         --  Update infos.
         if Low /= Null_Iir
           and then (Info.Low = Null_Iir
                       or else Eval_Pos (Low) < Eval_Pos (Info.Low))
         then
            Info.Low := Low;
         end if;
         if High /= Null_Iir
           and then (Info.High = Null_Iir
                       or else Eval_Pos (High) > Eval_Pos (Info.High))
         then
            Info.High := High;
         end if;
      end if;

      --  Analyze aggregate elements.
      if Constrained then
         Expr_Staticness := Get_Type_Staticness (Index_Type);
         if Expr_Staticness /= Locally then
            --  Cannot be statically built as the bounds are not known and
            --  must be checked at run-time.
            Set_Aggregate_Expand_Flag (Aggr, False);
         end if;
      else
         Expr_Staticness := Locally;
      end if;

      if Dim = Get_Nbr_Elements (Index_List) then
         --  A type has been found for AGGR, analyze AGGR as if it was
         --  an aggregate with a subtype (and not a string).
         if Get_Kind (Aggr) = Iir_Kind_Aggregate then
            Sem_Array_Aggregate_Elements (Aggr, A_Type, Expr_Staticness, Info);
         else
            --  Nothing to do for a string.
            null;
         end if;
      else
         --  A sub-aggregate: recurse.
         declare
            Sub_Aggr : Iir;
         begin
            --  Here we know that AGGR is an aggregate because:
            --  * either this is the first call (ie DIM = 1) and therefore
            --    AGGR is an aggregate (an aggregate is being analyzed).
            --  * or DIM > 1 and the use of strings is checked (just bellow).
            Assoc_Chain := Get_Association_Choices_Chain (Aggr);
            Choice := Assoc_Chain;
            while Choice /= Null_Iir loop
               if not Get_Same_Alternative_Flag (Choice) then
                  Sub_Aggr := Get_Associated_Expr (Choice);
                  case Get_Kind (Sub_Aggr) is
                     when Iir_Kind_Aggregate =>
                        Sem_Array_Aggregate_1
                          (Sub_Aggr, A_Type, Infos, Constrained, Dim + 1);
                        if not Get_Aggregate_Expand_Flag (Sub_Aggr) then
                           Set_Aggregate_Expand_Flag (Aggr, False);
                        end if;
                     when Iir_Kind_String_Literal8 =>
                        if Dim + 1 = Get_Nbr_Elements (Index_List) then
                           Sem_Array_Aggregate_1
                             (Sub_Aggr, A_Type, Infos, Constrained, Dim + 1);
                        else
                           Error_Msg_Sem
                             (+Sub_Aggr, "string literal not allowed here");
                           Infos (Dim + 1).Error := True;
                        end if;
                     when others =>
                        Error_Msg_Sem (+Sub_Aggr, "sub-aggregate expected");
                        Infos (Dim + 1).Error := True;
                  end case;
               end if;

               --  Always true for a sub-aggregate.
               Set_Element_Type_Flag (Choice, True);

               Choice := Get_Chain (Choice);
            end loop;
         end;
      end if;
      Set_Expr_Staticness
        (Aggr, Min (Expr_Staticness, Get_Expr_Staticness (Aggr)));

      --  Compute length.
      Len_Staticness := Locally;
      case Get_Kind (Aggr) is
         when Iir_Kind_Aggregate =>
            --  Determine if the aggregate is positionnal or named;
            --    and compute choice staticness.
            Is_Positional := Unknown;
            Choice_Staticness := Locally;
            Has_Positional_Choice := False;
            Has_Others := False;
            Len := 0;
            Choice := Assoc_Chain;
            while Choice /= Null_Iir loop
               case Get_Kind (Choice) is
                  when Iir_Kind_Choice_By_Range
                    | Iir_Kind_Choice_By_Expression =>
                     Is_Positional := False;
                     Choice_Staticness := Min (Choice_Staticness,
                                               Get_Choice_Staticness (Choice));
                     --  FIXME: not true for range.
                     Len := Len + 1;
                  when Iir_Kind_Choice_By_None =>
                     Has_Positional_Choice := True;
                     if Get_Element_Type_Flag (Choice) then
                        Len := Len + 1;
                     else
                        --  Extract length from associated expression.
                        Sem_Array_Aggregate_Choice_Length
                          (Choice, Len, Len_Staticness);
                     end if;
                  when Iir_Kind_Choice_By_Others =>
                     if not Constrained then
                        Error_Msg_Sem (+Aggr, "'others' choice not allowed "
                                         & "for an aggregate in this context");
                        Infos (Dim).Error := True;
                        return;
                     end if;
                     Has_Others := True;
                  when others =>
                     Error_Kind ("sem_array_aggregate", Choice);
               end case;
               --  LRM93 7.3.2.2
               --  Apart from the final element with the single choice
               --  OTHERS, the rest (if any) of the element
               --  associations of an array aggregate must be either
               --  all positionnal or all named.
               if Has_Positional_Choice then
                  if Is_Positional = False then
                     --  The error has already been emited
                     --  by sem_choices_range.
                     Infos (Dim).Error := True;
                     return;
                  end if;
                  Is_Positional := True;
               end if;
               Choice := Get_Chain (Choice);
            end loop;

            Info.Min_Length := Integer'Max (Info.Min_Length, Len);

            if Choice_Staticness = Unknown then
               --  This is possible when a choice is erroneous.
               Infos (Dim).Error := True;
               return;
            end if;

         when Iir_Kind_String_Literal8 =>
            Len := Sem_String_Literal
              (Aggr, Get_Base_Type (Get_Element_Subtype (A_Type)));
            Assoc_Chain := Null_Iir;
            Info.Min_Length := Integer'Max (Info.Min_Length, Len);
            Is_Positional := True;
            Has_Others := False;
            Choice_Staticness := Locally;
            Info.Nbr_Assocs := Info.Nbr_Assocs + Len;

         when others =>
            Error_Kind ("sem_array_aggregate(1)", Aggr);
      end case;

      if Is_Positional = False then
         Info.Has_Named := True;
      end if;
      if not Has_Others then
         Info.Has_Others := False;
      end if;

      --  LRM93 7.3.2.2
      --  A named association of an array aggregate is allowed to have a choice
      --  that is not locally static, [or likewise a choice that is a null
      --  range], only if the aggregate includes a single element association
      --  and this element association has a single choice.
      if Is_Positional = False and then Choice_Staticness /= Locally then
         Choice := Assoc_Chain;
         if not Is_Chain_Length_One (Assoc_Chain) or else
           (Get_Kind (Choice) /= Iir_Kind_Choice_By_Expression
            and then Get_Kind (Choice) /= Iir_Kind_Choice_By_Range)
         then
            Error_Msg_Sem (+Aggr, "non-locally static choice for an aggregate "
                             & "is allowed only if only choice");
            Infos (Dim).Error := True;
            return;
         end if;
         Info.Has_Dynamic := True;
         Set_Aggregate_Expand_Flag (Aggr, False);
      end if;

      --  Compute bounds of the index if there is no index subtype.
      if Info.Index_Subtype = Null_Iir and then Has_Others = False then
         --  LRM93 7.3.2.2
         --  the direction of the index subtype of the aggregate is that of the
         --  index subtype of the base type of the aggregate.

         if Is_Positional = True then
            --  LRM93 7.3.2.2
            --  For a positionnal aggregate, [...] the leftmost bound is given
            --  by S'LEFT where S is the index subtype of the base type of the
            --  array; [...] the rightmost bound is determined by the direction
            --  of the index subtype and the number of element.
            if Get_Type_Staticness (Index_Type) = Locally
              and then Len_Staticness = Locally
            then
               Info.Index_Subtype := Create_Range_Subtype_By_Length
                 (Index_Type, Int64 (Len), Get_Location (Aggr));

               --  In vhdl08 and later, the number of elements may also depend
               --  from associated expressions.
               if Vhdl_Std >= Vhdl_08
                 and then Get_Index_Constraint_Flag (A_Type)
                 and then Eval_Discrete_Type_Length (Index_Type) /= Int64 (Len)
               then
                  Error_Msg_Sem (+Aggr, "incorrect number of elements");
               end if;
            end if;
         else
            --  Create an index subtype.
            case Get_Kind (Index_Type) is
               when Iir_Kind_Integer_Subtype_Definition =>
                  Info.Index_Subtype :=
                    Create_Iir (Iir_Kind_Integer_Subtype_Definition);
               when Iir_Kind_Enumeration_Type_Definition
                 | Iir_Kind_Enumeration_Subtype_Definition =>
                  Info.Index_Subtype :=
                    Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
               when others =>
                  Error_Kind ("sem_array_aggregate(2)", Index_Type);
            end case;
            Location_Copy (Info.Index_Subtype, Aggr);
            Set_Parent_Type (Info.Index_Subtype, Get_Base_Type (Index_Type));
            Index_Constraint := Get_Range_Constraint (Index_Type);

            --  LRM93 7.3.2.2
            --  If the aggregate appears in one of the above contexts, then the
            --  direction of the index subtype of the aggregate is that of the
            --  corresponding constrained array subtype; [...]
            Index_Subtype_Constraint := Create_Iir (Iir_Kind_Range_Expression);
            Location_Copy (Index_Subtype_Constraint, Aggr);
            Set_Range_Constraint
              (Info.Index_Subtype, Index_Subtype_Constraint);
            Set_Type_Staticness (Info.Index_Subtype, Choice_Staticness);
            Set_Expr_Staticness (Index_Subtype_Constraint, Choice_Staticness);
            Set_Type (Index_Subtype_Constraint, Index_Type);
            if Get_Kind (Index_Constraint) = Iir_Kind_Range_Expression then
               Dir := Get_Direction (Index_Constraint);
            else
               --  This is not correct, as the direction must be the one of
               --  the corresponding constraint.  But it may not be determined
               --  at analysis time (if 'Range), and it doesn't really matter
               --  because of implicit subtype conversion.  So choose one
               --  arbitrary direction.
               Dir := Dir_To;
            end if;

            --  LRM93 7.3.2.2
            --  For an aggregate that has named associations, the leftmost and
            --  the rightmost bounds are determined by the direction of the
            --  index subtype of the aggregate and the smallest and largest
            --  choice given.
            if Choice_Staticness = Locally then
               if Low = Null_Iir or High = Null_Iir then
                  --  Avoid error propagation.
                  Set_Range_Constraint (Info.Index_Subtype,
                                        Get_Range_Constraint (Index_Type));
                  Free_Iir (Index_Subtype_Constraint);
               else
                  Set_Direction (Index_Subtype_Constraint, Dir);
                  case Dir is
                     when Dir_To =>
                        Set_Left_Limit (Index_Subtype_Constraint, Low);
                        Set_Right_Limit (Index_Subtype_Constraint, High);
                     when Dir_Downto =>
                        Set_Left_Limit (Index_Subtype_Constraint, High);
                        Set_Right_Limit (Index_Subtype_Constraint, Low);
                  end case;
               end if;
            else
               --  Dynamic aggregate.
               Set_Aggregate_Expand_Flag (Aggr, False);

               declare
                  --  There is only one choice.
                  Choice : constant Iir := Assoc_Chain;
                  Expr : Iir;
               begin
                  case Get_Kind (Choice) is
                     when Iir_Kind_Choice_By_Expression =>
                        Expr := Get_Choice_Expression (Choice);
                        Set_Direction (Index_Subtype_Constraint, Dir);
                        Set_Left_Limit (Index_Subtype_Constraint, Expr);
                        Set_Right_Limit (Index_Subtype_Constraint, Expr);
                     when Iir_Kind_Choice_By_Range =>
                        Expr := Get_Choice_Range (Choice);
                        Set_Range_Constraint (Info.Index_Subtype, Expr);
                        Set_Is_Ref (Info.Index_Subtype, True);
                        -- FIXME: avoid allocation-free.
                        Free_Iir (Index_Subtype_Constraint);
                     when others =>
                        raise Internal_Error;
                  end case;
               end;
            end if;
         end if;
         --Set_Type_Staticness
         --  (A_Subtype, Iirs.Min (Get_Type_Staticness (A_Subtype),
         --                        Get_Type_Staticness (Index_Subtype)));
         --Append_Element (Get_Index_List (A_Subtype), Index_Subtype);
      elsif Has_Others = False then
         --  Check the subaggregate bounds are the same.
         if Is_Positional = True then
            if Eval_Pos (Eval_Discrete_Range_Left (Get_Range_Constraint
                                                   (Info.Index_Subtype)))
              /= Eval_Pos (Eval_Discrete_Range_Left (Get_Range_Constraint
                                                     (Index_Type)))
            then
               Error_Msg_Sem (+Aggr, "subaggregate bounds mismatch");
            else
               if Eval_Discrete_Type_Length (Info.Index_Subtype)
                 /= Int64 (Len)
               then
                  Error_Msg_Sem (+Aggr, "subaggregate length mismatch");
               end if;
            end if;
         else
            declare
               L, H : Iir;
            begin
               Get_Low_High_Limit
                 (Get_Range_Constraint (Info.Index_Subtype), L, H);
               if Eval_Pos (L) /= Eval_Pos (Low)
                 or else Eval_Pos (H) /= Eval_Pos (H)
               then
                  Error_Msg_Sem (+Aggr, "subaggregate bounds mismatch");
               end if;
            end;
         end if;
      end if;

      Expr_Staticness := Min (Get_Expr_Staticness (Aggr), Choice_Staticness);
      Set_Expr_Staticness (Aggr, Expr_Staticness);
   end Sem_Array_Aggregate_1;

   --  Analyze an array aggregate whose type is AGGR_TYPE.
   --  If CONSTRAINED is true, then the aggregate appears in one of the
   --  context and can have an 'others' choice.
   --  If CONSTRAINED is false, the aggregate can not have an 'others' choice.
   --  Create a subtype for this aggregate.
   --  Return NULL_IIR in case of error, or AGGR if not.
   function Sem_Array_Aggregate
     (Aggr : Iir; Aggr_Type : Iir; Constrained : Boolean) return Iir
   is
      Index_List : constant Iir_Flist := Get_Index_Subtype_List (Aggr_Type);
      Nbr_Dim : constant Natural := Get_Nbr_Elements (Index_List);
      El_Type : constant Iir := Get_Element_Subtype (Aggr_Type);
      El_Subtype : Iir;
      Infos : Array_Aggr_Info_Arr (1 .. Nbr_Dim);
      A_Subtype: Iir;
      Base_Type : Iir;
      Aggr_Constrained : Boolean;
      Info, Prev_Info : Iir_Aggregate_Info;
      Type_Staticness : Iir_Staticness;
   begin
      --  Analyze the aggregate.
      Sem_Array_Aggregate_1 (Aggr, Aggr_Type, Infos, Constrained, 1);

      --  The aggregate is constrained if all indexes are known.
      Aggr_Constrained := True;
      for I in Infos'Range loop
         --  Return now in case of error.
         if Infos (I).Error then
            Set_Aggregate_Expand_Flag (Aggr, False);
            return Null_Iir;
         end if;
         if Infos (I).Index_Subtype = Null_Iir then
            Aggr_Constrained := False;
         end if;
      end loop;
      Base_Type := Get_Base_Type (Aggr_Type);

      --  Extract element subtype (if needed and if possible).
      if not Is_Fully_Constrained_Type (El_Type) then
         --  Need to extract the element subtype.
         --  First, extract it - try to find the best one.
         El_Subtype := Null_Iir;
         Sem_Array_Aggregate_Extract_Element_Subtype
           (Aggr, 1, Nbr_Dim, El_Subtype);
         if El_Subtype = Null_Iir then
            El_Subtype := El_Type;
         else
            --  TODO: check constraints of elements (if El_Subtype is static)
            null;
         end if;
      else
         El_Subtype := El_Type;
      end if;

      --  Reuse AGGR_TYPE iff AGGR_TYPE is fully constrained
      --  and statically match the subtype of the aggregate.
      if Aggr_Constrained then
         Type_Staticness := Locally;
         for I in Infos'Range loop
            Type_Staticness := Min
              (Type_Staticness, Get_Type_Staticness (Infos (I).Index_Subtype));
         end loop;

         if Get_Constraint_State (Aggr_Type) = Fully_Constrained
           and then Get_Type_Staticness (Aggr_Type) = Locally
           and then Type_Staticness = Locally
         then
            Set_Type (Aggr, Aggr_Type);
         else
            A_Subtype := Create_Array_Subtype (Base_Type, Get_Location (Aggr));
            Set_Element_Subtype (A_Subtype, El_Subtype);
            if El_Subtype /= El_Type then
               Sem_Array_Aggregate_Check_Element_Subtype
                 (El_Subtype, Aggr, 1, Nbr_Dim);
            end if;
            Type_Staticness := Min (Type_Staticness,
                                    Get_Type_Staticness (El_Subtype));
            declare
               Idx_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (A_Subtype);
            begin
               for I in Infos'Range loop
                  Set_Nth_Element (Idx_List, I - 1, Infos (I).Index_Subtype);
               end loop;
            end;
            Set_Type_Staticness (A_Subtype, Type_Staticness);
            Set_Index_Constraint_Flag (A_Subtype, True);
            --  FIXME: the element can be unconstrained.
            Set_Constraint_State (A_Subtype, Fully_Constrained);
            Set_Type (Aggr, A_Subtype);
            Set_Literal_Subtype (Aggr, A_Subtype);
         end if;
         if Type_Staticness = Locally and then Get_Aggregate_Expand_Flag (Aggr)
         then
            --  Compute ratio of elements vs size of the aggregate to determine
            --  if the aggregate can be expanded.
            declare
               Size : Int64;
            begin
               Size := 1;
               for I in Infos'Range loop
                  Size := Size
                    * Eval_Discrete_Type_Length (Infos (I).Index_Subtype);
               end loop;
               Set_Aggregate_Expand_Flag
                 (Aggr, Infos (Nbr_Dim).Nbr_Assocs >= Natural (Size / 10));
            end;
         else
            Set_Aggregate_Expand_Flag (Aggr, False);
         end if;
      else
         --  If the array is not constrained, expression cannot be more
         --  static than the type.  In particular, if the type is not
         --  constrained, the expression cannot be locally static.
         Set_Expr_Staticness (Aggr, Min (Get_Type_Staticness (Aggr_Type),
                                         Get_Expr_Staticness (Aggr)));

         --  Free unused indexes subtype.
         for I in Infos'Range loop
            declare
               St : constant Iir := Infos (I).Index_Subtype;
               Rng : Iir;
            begin
               if St /= Null_Iir then
                  Rng := Get_Range_Constraint (St);
                  Free_Iir (Get_Right_Limit_Expr (Rng));
                  Free_Iir (Rng);
                  Free_Iir (St);
               end if;
            end;
         end loop;

         --  If bounds are not known, the aggregate cannot be statically built.
         Set_Aggregate_Expand_Flag (Aggr, False);

         if Get_Constraint_State (Aggr_Type) /= Fully_Constrained
           and then El_Subtype /= El_Type
         then
            A_Subtype := Create_Array_Subtype (Base_Type, Get_Location (Aggr));
            Set_Element_Subtype (A_Subtype, El_Subtype);
            Sem_Array_Aggregate_Check_Element_Subtype
              (El_Subtype, Aggr, 1, Nbr_Dim);
            Type_Staticness := Get_Type_Staticness (El_Subtype);
            if Get_Index_Constraint_Flag (Aggr_Type) then
               declare
                  Idx_Src_List : constant Iir_Flist :=
                    Get_Index_Subtype_List (Aggr_Type);
                  Idx_Dest_List : constant Iir_Flist :=
                    Get_Index_Subtype_List (A_Subtype);
                  Idx : Iir;
               begin
                  for I in 1 .. Nbr_Dim loop
                     Idx := Get_Nth_Element (Idx_Src_List, I - 1);
                     Type_Staticness := Min (Type_Staticness,
                                             Get_Type_Staticness (Idx));
                     Set_Nth_Element (Idx_Dest_List, I - 1, Idx);
                  end loop;
               end;
               Set_Index_Constraint_Flag (A_Subtype, True);
               Set_Constraint_State (A_Subtype,
                                     Get_Constraint_State (El_Subtype));
            else
               Set_Constraint_State
                 (A_Subtype,
                  Iir_Constraint'Min (Partially_Constrained,
                                      Get_Constraint_State (El_Subtype)));
            end if;
            Set_Type_Staticness (A_Subtype, Type_Staticness);
            Set_Type (Aggr, A_Subtype);
            Set_Literal_Subtype (Aggr, A_Subtype);
         end if;
      end if;

      if Infos (Nbr_Dim).Has_Bound_Error then
         return Build_Overflow (Aggr, Get_Type (Aggr));
      end if;

      Prev_Info := Null_Iir;
      for I in Infos'Range loop
         --  Create info and link.
         Info := Create_Iir (Iir_Kind_Aggregate_Info);
         if I = 1 then
            Set_Aggregate_Info (Aggr, Info);
         else
            Set_Sub_Aggregate_Info (Prev_Info, Info);
         end if;
         Prev_Info := Info;

         --  Fill info.
         Set_Aggr_Dynamic_Flag (Info, Infos (I).Has_Dynamic);
         Set_Aggr_Named_Flag (Info, Infos (I).Has_Named);
         Set_Aggr_Low_Limit (Info, Infos (I).Low);
         Set_Aggr_High_Limit (Info, Infos (I).High);
         Set_Aggr_Min_Length (Info, Iir_Int32 (Infos (I).Min_Length));
         Set_Aggr_Others_Flag (Info, Infos (I).Has_Others);
      end loop;
      return Aggr;
   end Sem_Array_Aggregate;

   --  Analyze aggregate EXPR whose type is expected to be A_TYPE.
   --  A_TYPE cannot be null_iir (this case is handled in sem_expression_ov)
   --  If CONSTRAINED is true, the aggregate type is constrained by the
   --  context, even if its type isn't.  This is to deal with cases like:
   --    procedure set (v : out string) is
   --    begin
   --      v := (others => ' ');
   --    end set;
   --  but this is not allowed by:
   --  LRM08 9.3.3.3 Array aggregates
   --  e) As a value expression in an assignment statement, where the target
   --     is a declared object (or member thereof), and either the subtype of
   --     the target is a fully constrained array subtype or the target is a
   --     slice name.
   function Sem_Aggregate
     (Expr: Iir_Aggregate; A_Type: Iir; Constrained : Boolean) return Iir is
   begin
      pragma Assert (A_Type /= Null_Iir);

      if Flags.Vhdl_Std >= Vhdl_08 then
         --  An aggregate can be a locally static primary according to LRM08
         --  9.4.2 Locally static primaries l) and m).
         Set_Expr_Staticness (Expr, Locally);
      else
         --  An aggregate is at most globally static.
         Set_Expr_Staticness (Expr, Globally);
      end if;

      Set_Type (Expr, A_Type); -- FIXME: should free old type
      case Get_Kind (A_Type) is
         when Iir_Kind_Array_Subtype_Definition =>
            return Sem_Array_Aggregate
              (Expr, A_Type,
               Constrained or Get_Index_Constraint_Flag (A_Type));
         when Iir_Kind_Array_Type_Definition =>
            return Sem_Array_Aggregate (Expr, A_Type, Constrained);
         when Iir_Kind_Record_Type_Definition
            | Iir_Kind_Record_Subtype_Definition =>
            if not Sem_Record_Aggregate (Expr, A_Type, Constrained) then
               return Null_Iir;
            end if;
            return Expr;
         when Iir_Kind_Error =>
            return Null_Iir;
         when others =>
            Error_Msg_Sem (+Expr, "type %n is not composite", +A_Type);
            return Null_Iir;
      end case;
   end Sem_Aggregate;

   function Is_Physical_Literal_Zero (Lit : Iir) return Boolean is
   begin
      case Iir_Kinds_Physical_Literal (Get_Kind (Lit)) is
         when Iir_Kind_Physical_Int_Literal =>
            return Get_Value (Lit) = 0;
         when Iir_Kind_Physical_Fp_Literal =>
            return Get_Fp_Value (Lit) = 0.0;
      end case;
   end Is_Physical_Literal_Zero;

   --  Transform LIT into a physical_literal.
   --  LIT can be either a not analyzed physical literal or
   --   a simple name that is a physical unit.  In the later case, a physical
   --   literal is created.
   function Sem_Physical_Literal (Lit: Iir) return Iir
   is
      Unit_Name : Iir;
      Unit : Iir;
      Unit_Type : Iir;
      Res: Iir;
   begin
      case Get_Kind (Lit) is
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            Unit_Name := Get_Unit_Name (Lit);
            Res := Lit;
         when Iir_Kinds_Denoting_Name =>
            Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
            Location_Copy (Res, Lit);
            Set_Value (Res, 1);
            Set_Literal_Origin (Res, Lit);
            Unit_Name := Lit;
         when others =>
            Error_Kind ("sem_physical_literal", Lit);
      end case;
      if Is_Error (Unit_Name) then
         return Create_Error_Expr (Res, Error_Mark);
      end if;

      case Get_Kind (Unit_Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Unit_Name := Sem_Denoting_Name (Unit_Name);
            Unit := Get_Named_Entity (Unit_Name);
         when others =>
            pragma Assert (Flags.Flag_Force_Analysis);
            Unit := Null_Iir;
      end case;

      if Unit = Null_Iir
        or else Get_Kind (Unit) /= Iir_Kind_Unit_Declaration
      then
         if Unit /= Null_Iir and then not Is_Error (Unit) then
            Error_Class_Match (Unit_Name, "unit");
         end if;
         Set_Named_Entity (Unit_Name, Create_Error_Name (Unit_Name));
      else
         --  Note: there is corresponding code for physical literal without
         --  literal (so only the unit) in vhdl.sem_expr.name_to_expression.

         --  Physical unit is used.
         Set_Use_Flag (Unit, True);

         if Get_Type (Unit) = Time_Type_Definition
           and then Get_Value (Get_Physical_Literal (Unit)) = 0
           and then not Is_Physical_Literal_Zero (Res)
         then
            --  LRM08 5.2.4.2 Predefined physical types
            --  It is an error if a given unit of type TIME appears anywhere
            --  within the design hierarchy defining a model to be elaborated,
            --  and if the position number of that unit is less than that of
            --  the secondary unit selected as the resolution limit for type
            --  TIME during the elaboration of the model, unless that unit is
            --  part of a physical literal whose abstract literal is either
            --  the integer value zero or the floating-point value zero.
            Error_Msg_Sem
              (+Res, "physical unit %i is below the time resolution", +Unit);
         end if;
      end if;
      Set_Unit_Name (Res, Unit_Name);
      Unit_Type := Get_Type (Unit_Name);
      Set_Type (Res, Unit_Type);

      --  LRM93 7.4.2
      --  1. a literal of type TIME.
      --
      --  LRM93 7.4.1
      --  1. a literal of any type other than type TIME;
      Set_Expr_Staticness (Res, Get_Expr_Staticness (Unit_Name));
      --Eval_Check_Constraints (Res);
      return Res;
   end Sem_Physical_Literal;

   --  Analyze an allocator by expression or an allocator by subtype.
   function Sem_Allocator (Expr : Iir; A_Type : Iir) return Iir
   is
      Arg: Iir;
      Arg_Type : Iir;
   begin
      Set_Expr_Staticness (Expr, None);

      Arg_Type := Get_Allocator_Designated_Type (Expr);

      if Arg_Type = Null_Iir then
         --  Expression was not analyzed.
         case Iir_Kinds_Allocator (Get_Kind (Expr)) is
            when Iir_Kind_Allocator_By_Expression =>
               Arg := Get_Expression (Expr);
               pragma Assert (Get_Kind (Arg) = Iir_Kind_Qualified_Expression);
               Arg := Sem_Expression (Arg, Null_Iir);
               if Arg = Null_Iir then
                  return Null_Iir;
               end if;
               Check_Read (Arg);
               Set_Expression (Expr, Arg);
               Arg_Type := Get_Type (Arg);

            when Iir_Kind_Allocator_By_Subtype =>
               --  Analyze subtype indication.
               Arg := Get_Subtype_Indication (Expr);
               Arg := Sem_Types.Sem_Subtype_Indication (Arg);
               Set_Subtype_Indication (Expr, Arg);
               Arg := Get_Type_Of_Subtype_Indication (Arg);
               if Arg = Null_Iir or else Is_Error (Arg) then
                  return Null_Iir;
               end if;
               if Is_Anonymous_Type_Definition (Arg) then
                  Set_Allocator_Subtype (Expr, Get_Subtype_Indication (Expr));
               end if;

               --  LRM93 7.3.6
               --  If an allocator includes a subtype indication and if the
               --  type of the object created is an array type, then the
               --  subtype indication must either denote a constrained
               --  subtype or include an explicit index constraint.
               if not Is_Fully_Constrained_Type (Arg) then
                  Error_Msg_Sem
                    (+Expr, "allocator of unconstrained %n is not allowed",
                     +Arg);
               end if;
               --  LRM93 7.3.6
               --  A subtype indication that is part of an allocator must
               --  not include a resolution function.
               if Is_Anonymous_Type_Definition (Arg)
                 and then Get_Kind (Arg) /= Iir_Kind_Access_Subtype_Definition
                 and then Get_Resolution_Indication (Arg) /= Null_Iir
               then
                  Error_Msg_Sem (+Expr, "subtype indication must not include"
                                   & " a resolution function");
               end if;
               Arg_Type := Arg;
         end case;
         Set_Allocator_Designated_Type (Expr, Arg_Type);
      end if;

      --  LRM 7.3.6 Allocators
      --  The type of the access value returned by an allocator must be
      --  determinable solely from the context, but using the fact that the
      --  value returned is of an access type having the named designated
      --  type.
      if A_Type = Null_Iir then
         --  Type of the context is not yet known.
         return Expr;
      else
         if not Is_Allocator_Type (A_Type, Expr) then
            if Get_Kind (A_Type) /= Iir_Kind_Access_Type_Definition then
               if not Is_Error (A_Type) then
                  Error_Msg_Sem (+Expr, "expected type is not an access type");
               end if;
            else
               Error_Not_Match (Expr, A_Type);
            end if;
            return Null_Iir;
         end if;
         Set_Type (Expr, A_Type);
         return Expr;
      end if;
   end Sem_Allocator;

   function Sem_Qualified_Expression (Expr : Iir; A_Type : Iir) return Iir
   is
      N_Type: Iir;
      Res: Iir;
   begin
      N_Type := Sem_Type_Mark (Get_Type_Mark (Expr));
      Set_Type_Mark (Expr, N_Type);
      N_Type := Get_Type (N_Type);
      if N_Type = Null_Iir then
         --  Stop now in case of error.  It is highly possible that the
         --  expression is ambiguous.
         return Null_Iir;
      end if;

      Set_Type (Expr, N_Type);
      if A_Type /= Null_Iir
        and then Are_Types_Compatible (A_Type, N_Type) = Not_Compatible
      then
         Error_Not_Match (Expr, A_Type);
         return Null_Iir;
      end if;
      Res := Sem_Expression (Get_Expression (Expr), N_Type);
      if Res = Null_Iir then
         return Null_Iir;
      end if;
      Check_Read (Res);
      Res := Eval_Expr_If_Static (Res);
      Set_Expression (Expr, Res);

      --  LRM93 7.4.1 Locally static primaries
      --  h) A qualified expression whose operand is a locally static
      --     expression.
      --
      --  LRM08 9.4.2 Locally static primaries
      --  i) A qualified expression whose type mark denotes a locally static
      --     subtype and whose operand is a locally static expression.
      --
      --  We always use the vhdl08, because it is weird to have locally
      --  static expression with a non-locally static subtype.
      Set_Expr_Staticness (Expr, Min (Get_Expr_Staticness (Res),
                                      Get_Type_Staticness (N_Type)));

      --  When possible, use the type of the expression as the type of the
      --  qualified expression.
      --  TODO: also handle unbounded subtypes, but only if this is a proper
      --   subtype.
      case Get_Kind (N_Type) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Record_Type_Definition =>
            Set_Type (Expr, Get_Type (Res));
         when others =>
            null;
      end case;

      --  Emit a warning if the value is known not to be within the bounds.
      if Get_Expr_Staticness (Res) = Locally
        and then not Eval_Is_In_Bound (Res, N_Type)
      then
         Warning_Msg_Sem
           (Warnid_Runtime_Error, +Expr,
            "static expression out of prefix type bounds");
         return Build_Overflow (Expr, N_Type);
      end if;

      return Expr;
   end Sem_Qualified_Expression;

   function Is_Signal_Parameter (Obj : Iir) return Boolean is
   begin
      return Get_Kind (Obj) = Iir_Kind_Interface_Signal_Declaration
        and then
        Get_Kind (Get_Parent (Obj)) in Iir_Kinds_Subprogram_Declaration;
   end Is_Signal_Parameter;

   function Can_Interface_Be_Read (Inter : Iir) return Boolean is
   begin
      case Get_Mode (Inter) is
         when Iir_In_Mode
           | Iir_Inout_Mode
           | Iir_Buffer_Mode =>
            --  LRM08 6.5.3 Interface object declarations
            --  - in. The value of the interface object is allowed
            --     to be read, [...]
            --  - inout or buffer.  Reading and updating the value of
            --     the interface object is allowed. [...]
            null;
         when Iir_Out_Mode =>
            --  LRM93 4.3.2 Interface declarations
            --  - out. The value of the interface object is allowed to be
            --    updated, but it must not be read.
            --
            --  LRM08 6.5.3 Interface object declarations
            --  - out. The value of the interface object is allowed
            --    [to be updated and,]  provided it is not a signal
            --    parameter, read.
            if Vhdl_Std < Vhdl_08 or else Is_Signal_Parameter (Inter) then
               return False;
            end if;
         when Iir_Linkage_Mode =>
            --  LRM08 6.5.3 Interface object declarations
            --  - linkage.  Reading and updating the value of the
            --    interface object is allowed, but only by appearing
            --    as an actual corresponding to an interface object
            --    of mode LINKAGE.  No other reading or updating is
            --    permitted.
            return False;
         when Iir_Unknown_Mode =>
            raise Internal_Error;
      end case;
      return True;
   end Can_Interface_Be_Read;

   function Can_Interface_Be_Updated (Inter : Iir) return Boolean is
   begin
      case Get_Mode (Inter) is
         when Iir_In_Mode =>
            --  LRM08 6.5.3 Interface object declarations
            --  - in. The value of the interface object is allowed to be read,
            --    but it shall not be updated.
            return False;
         when Iir_Out_Mode =>
            --  LRM08 6.5.3 Interface object declarations
            --  - out. The value of the interface object is allowed
            --    to be updated [and, ...]
            return True;
         when Iir_Inout_Mode
           | Iir_Buffer_Mode =>
            --  LRM08 6.5.3 Interface object declarations
            --  - inout or buffer.  Reading and updating the value of the
            --    interface is allowed.
            return True;
         when Iir_Linkage_Mode =>
            --  LRM08 6.5.3 Interface object declarations
            --  - linkage.  Reading and updating the value of the
            --    interface object is allowed, but only by appearing
            --    as an actual corresponding to an interface object
            --    of mode LINKAGE.  No other reading or updating is
            --    permitted.
            return False;
         when Iir_Unknown_Mode =>
            raise Internal_Error;
      end case;
   end Can_Interface_Be_Updated;

   procedure Check_Read_Aggregate (Aggr : Iir)
   is
      pragma Unreferenced (Aggr);
   begin
      --  FIXME: todo.
      null;
   end Check_Read_Aggregate;

   --  Check EXPR can be read.
   procedure Check_Read (Expr : Iir)
   is
      Obj : Iir;
   begin
      if Expr = Null_Iir then
         return;
      end if;

      Obj := Expr;
      loop
         case Get_Kind (Obj) is
            when Iir_Kind_Signal_Declaration
              | Iir_Kind_Variable_Declaration =>
               Set_Use_Flag (Obj, True);
               return;
            when Iir_Kind_Constant_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Attribute_Value
              | Iir_Kind_Iterator_Declaration
              | Iir_Kind_Guard_Signal_Declaration =>
               return;
            when Iir_Kinds_Quantity_Declaration
              | Iir_Kind_Interface_Quantity_Declaration =>
               return;
            when Iir_Kinds_External_Name =>
               return;
            when Iir_Kind_Psl_Endpoint_Declaration =>
               return;
            when Iir_Kind_File_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               --  LRM 4.3.2  Interface declarations
               --  The value of an object is said to be read [...]
               --   -  When the object is a file and a READ operation is
               --      performed on the file.
               return;
            when Iir_Kind_Object_Alias_Declaration =>
               Obj := Get_Name (Obj);
            when Iir_Kind_Interface_Signal_Declaration
              | Iir_Kind_Interface_Variable_Declaration =>
               if not Can_Interface_Be_Read (Obj) then
                  Error_Msg_Sem (+Expr, "%n cannot be read", +Obj);
               end if;
               return;
            when Iir_Kind_Enumeration_Literal
              | Iir_Kind_Physical_Int_Literal
              | Iir_Kind_Physical_Fp_Literal
              | Iir_Kind_String_Literal8
              | Iir_Kind_Character_Literal
              | Iir_Kind_Integer_Literal
              | Iir_Kind_Floating_Point_Literal
              | Iir_Kind_Null_Literal
              | Iir_Kind_Unit_Declaration
              | Iir_Kind_Simple_Aggregate
              | Iir_Kind_Overflow_Literal =>
               return;
            when Iir_Kinds_Monadic_Operator
              | Iir_Kinds_Dyadic_Operator
              | Iir_Kind_Function_Call =>
               return;
            when Iir_Kind_Parenthesis_Expression =>
               Obj := Get_Expression (Obj);
            when Iir_Kind_Qualified_Expression =>
               return;
            when Iir_Kind_Type_Conversion
              | Iir_Kind_Allocator_By_Expression
              | Iir_Kind_Allocator_By_Subtype
              | Iir_Kind_Implicit_Dereference
              | Iir_Kind_Dereference
              | Iir_Kind_Attribute_Name =>
               return;
            when Iir_Kinds_Scalar_Type_Attribute
              | Iir_Kinds_Type_Attribute
              | Iir_Kinds_Array_Attribute
              | Iir_Kind_Image_Attribute
              | Iir_Kind_Value_Attribute
              | Iir_Kinds_Name_Attribute
              | Iir_Kinds_Signal_Attribute
              | Iir_Kinds_Signal_Value_Attribute
              | Iir_Kind_Above_Attribute
              | Iir_Kind_Zoh_Attribute
              | Iir_Kind_Ltf_Attribute
              | Iir_Kind_Ztf_Attribute
              | Iir_Kind_Dot_Attribute
              | Iir_Kind_Integ_Attribute
              | Iir_Kind_Ramp_Attribute
              | Iir_Kind_Quantity_Delayed_Attribute =>
               return;
            when Iir_Kind_Aggregate =>
               Check_Read_Aggregate (Obj);
               return;
            when Iir_Kind_Indexed_Name
              | Iir_Kind_Slice_Name
              | Iir_Kind_Selected_Element =>
               --  FIXME: speed up using Base_Name
               --  Obj := Get_Base_Name (Obj);
               Obj := Get_Prefix (Obj);
            when Iir_Kind_Simple_Name
              | Iir_Kind_Selected_Name =>
               Obj := Get_Named_Entity (Obj);
            when Iir_Kinds_Psl_Builtin =>
               return;
            when Iir_Kind_Parenthesis_Name
              | Iir_Kind_Error =>
               return;
            when others =>
               Error_Kind ("check_read", Obj);
         end case;
      end loop;
   end Check_Read;

   --  Emit an error if the constant EXPR is deferred and cannot be used in
   --  the current context.
   procedure Check_Constant_Restriction (Expr : Iir; Loc : Iir)
   is
      Lib : Iir;
      Cur_Lib : Iir;
   begin
      --  LRM93 2.6
      --  Within a package declaration that contains the declaration
      --  of a deferred constant, and within the body of that package,
      --  before the end of the corresponding full declaration, the
      --  use of a name that denotes the deferred constant is only
      --  allowed in the default expression for a local generic,
      --  local port or formal parameter.
      if Get_Deferred_Declaration_Flag (Expr) = False
        or else Get_Deferred_Declaration (Expr) /= Null_Iir
      then
         --  The constant declaration is not deferred
         --  or the it has been fully declared.
         return;
      end if;

      Lib := Get_Parent (Expr);
      Cur_Lib := Get_Library_Unit (Sem.Get_Current_Design_Unit);
      if (Get_Kind (Cur_Lib) = Iir_Kind_Package_Declaration
          and then Lib = Cur_Lib)
        or else (Get_Kind (Cur_Lib) = Iir_Kind_Package_Body
                 and then Get_Package (Cur_Lib) = Lib)
      then
         Error_Msg_Sem (+Loc, "invalid use of a deferred constant");
      end if;
   end Check_Constant_Restriction;

   function Sem_Dyadic_Operator (Expr : Iir; Atype : Iir) return Iir
   is
      Arr : Iir_Array (1 .. 128);
      Len : Natural;
   begin
      --  Try to linearize the tree in order to reduce recursion depth
      --  and also improve speed of evaluation.
      --  This is particularly useful for repeated concatenations.
      declare
         Left : Iir;
      begin
         Len := 0;
         Left := Expr;
         while Len < Arr'Last
           and then Get_Kind (Left) in Iir_Kinds_Dyadic_Operator
         loop
            Len := Len + 1;
            Arr (Len) := Left;
            Left := Get_Left (Left);
         end loop;
      end;

      --  No possibility to linearize...
      if Len = 1 then
         return Sem_Operator (Expr, Atype);
      end if;

      if Get_Type (Expr) = Null_Iir then
         --  First pass.
         Arr (Len) := Sem_Operator_Pass1 (Arr (Len), Null_Iir);
         if Arr (Len) = Null_Iir then
            return Null_Iir;
         end if;
         for I in reverse 2 .. Len - 1 loop
            Set_Left (Arr (I), Arr (I + 1));
            Arr (I) := Sem_Operator_Pass1 (Arr (I), Null_Iir);
            if Arr (I) = Null_Iir then
               return Null_Iir;
            end if;
         end loop;
         Set_Left (Arr (1), Arr (2));
         Arr (1) := Sem_Operator_Pass1 (Arr (1), Atype);
         return Arr (1);
      else
         --  Second pass.
         declare
            Op_Type : Iir;
            Decl : Iir;
            Interfaces : Iir;
            Left, Right : Iir;
            Is_All_Concat : Boolean;
            Imp : Iir;
            Err : Boolean;
         begin
            Op_Type := Atype;
            Err := False;
            for I in 1 .. Len loop
               if not Is_Overloaded (Arr (I)) then
                  pragma Assert (I > 1);
                  exit;
               end if;
               Decl := Sem_Operator_Pass2_Interpretation
                 (Arr (I), Op_Type);
               if Decl = Null_Iir then
                  --  Stop in case of error.
                  return Null_Iir;
               end if;
               Set_Type (Arr (I), Get_Return_Type (Decl));
               Set_Implementation (Arr (I), Decl);
               Interfaces := Get_Interface_Declaration_Chain (Decl);
               Op_Type := Get_Base_Type (Get_Type (Interfaces));

               --  Right operand.
               Right := Get_Right (Arr (I));
               if Is_Overloaded (Right) then
                  Right := Get_Right (Arr (I));
                  Right := Sem_Expression_Ov
                    (Right,
                     Get_Base_Type (Get_Type (Get_Chain (Interfaces))));
                  if Right = Null_Iir then
                     Err := True;
                  else
                     Set_Right (Arr (I), Right);
                  end if;
               end if;
               Check_Read (Right);
            end loop;

            Left := Get_Left (Arr (Len));
            if Is_Overloaded (Left) then
               Left := Sem_Expression_Ov
                 (Left, Get_Base_Type (Get_Type (Interfaces)));
               if Left = Null_Iir then
                  Err := True;
               else
                  Set_Left (Arr (Len), Left);
               end if;
            end if;

            --  Finish

            if not Err then
               Is_All_Concat := True;
               for I in reverse 1 .. Len loop
                  Imp := Get_Implementation (Arr (I));
                  Sem_Subprogram_Call_Finish (Arr (I), Imp);
                  Is_All_Concat := Is_All_Concat
                    and then (Get_Implicit_Definition (Imp)
                                in Iir_Predefined_Concat_Functions);
               end loop;
               if Get_Expr_Staticness (Arr (1)) = Locally then
                  if Is_All_Concat
                  then
                     Arr (1) := Eval_Concatenation (Arr (1 .. Len));
                  else
                     Arr (1) := Eval_Expr_If_Static (Arr (1));
                  end if;
               else
                  for I in reverse 1 .. Len loop
                     exit when Get_Expr_Staticness (Arr (I)) /= Locally;
                     Arr (I) := Eval_Expr_If_Static (Arr (I));
                     if I > 1 then
                        Set_Left (Arr (I - 1), Arr (I));
                     end if;
                  end loop;
               end if;
            end if;
            return Arr (1);
         end;
      end if;
   end Sem_Dyadic_Operator;

   function Sem_Parenthesis_Expression (Expr : Iir; Atype: Iir) return Iir
   is
      Sub_Expr : Iir;
   begin
      Sub_Expr := Get_Expression (Expr);
      Sub_Expr := Sem_Expression_Ov (Sub_Expr, Atype);
      if Sub_Expr = Null_Iir then
         return Null_Iir;
      end if;
      Set_Expression (Expr, Sub_Expr);
      Set_Type (Expr, Get_Type (Sub_Expr));
      Set_Expr_Staticness (Expr, Get_Expr_Staticness (Sub_Expr));
      return Expr;
   end Sem_Parenthesis_Expression;

   -- Set semantic to EXPR.
   --  Replace simple_name with the referenced node,
   --  Set type to nodes,
   --  Resolve overloading

   -- If A_TYPE is not null, then EXPR must be of type A_TYPE.
   -- Return null in case of error.
   function Sem_Expression_Ov (Expr: Iir; A_Type1: Iir) return Iir
   is
      A_Type: Iir;
   begin
--     -- Avoid to run sem_expression_ov when a node was already analyzed
--     -- except to resolve overload.
--     if Get_Type (Expr) /= Null_Iir then
--        --  EXPR was already analyzed.
--        if A_Type1 = null or else not Is_Overload_List (Get_Type (Expr)) then
--           --  This call to sem_expression_ov do not add any informations.
--           Check_Restrictions (Expr, Restriction);
--           return Expr;
--        end if;
--        -- This is an overload list that will be reduced.
--     end if;

      -- A_TYPE must be a type definition and not a subtype.
      if A_Type1 /= Null_Iir then
         A_Type := Get_Base_Type (A_Type1);
         if A_Type /= A_Type1 then
            raise Internal_Error;
         end if;
      else
         A_Type := Null_Iir;
      end if;

      case Get_Kind (Expr) is
         when Iir_Kind_Selected_Name
           | Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Attribute_Name =>
            declare
               E : Iir;
            begin
               E := Get_Named_Entity (Expr);
               if E = Null_Iir then
                  Sem_Name (Expr);
                  E := Get_Named_Entity (Expr);
                  pragma Assert (E /= Null_Iir);
               end if;
               if E = Error_Mark then
                  return Null_Iir;
               end if;
               case Get_Kind (E) is
                  when Iir_Kind_Constant_Declaration =>
                     if not Deferred_Constant_Allowed then
                        Check_Constant_Restriction (E, Expr);
                     end if;
                  when Iir_Kind_Enumeration_Literal =>
                     Set_Use_Flag (E, True);
                  when others =>
                     null;
               end case;
               E := Name_To_Expression (Expr, A_Type);
               return E;
            end;

         when Iir_Kinds_External_Name =>
            Sem_External_Name (Expr);
            return Expr;

         when Iir_Kinds_Monadic_Operator =>
            return Sem_Operator (Expr, A_Type);

         when Iir_Kinds_Dyadic_Operator =>
            return Sem_Dyadic_Operator (Expr, A_Type);

         when Iir_Kind_Enumeration_Literal
           | Iir_Kinds_Object_Declaration =>
            -- All these case have already a type.
            if Get_Type (Expr) = Null_Iir then
               return Null_Iir;
            end if;
            if A_Type /= Null_Iir
              and then Are_Basetypes_Compatible
              (A_Type, Get_Base_Type (Get_Type (Expr))) = Not_Compatible
            then
               Error_Not_Match (Expr, A_Type);
               return Null_Iir;
            end if;
            return Expr;

         when Iir_Kind_Integer_Literal =>
            Set_Expr_Staticness (Expr, Locally);
            if A_Type = Null_Iir then
               Set_Type (Expr, Convertible_Integer_Type_Definition);
               return Expr;
            elsif Get_Kind (A_Type) = Iir_Kind_Integer_Type_Definition then
               Set_Type (Expr, A_Type);
               return Expr;
            else
               Error_Not_Match (Expr, A_Type);
               return Null_Iir;
            end if;

         when Iir_Kind_Floating_Point_Literal =>
            Set_Expr_Staticness (Expr, Locally);
            if A_Type = Null_Iir then
               Set_Type (Expr, Convertible_Real_Type_Definition);
               return Expr;
            elsif Get_Kind (A_Type) = Iir_Kind_Floating_Type_Definition then
               Set_Type (Expr, A_Type);
               return Expr;
            else
               Error_Not_Match (Expr, A_Type);
               return Null_Iir;
            end if;

         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Unit_Declaration =>
            declare
               Res: Iir;
               Res_Type : Iir;
            begin
               Res := Sem_Physical_Literal (Expr);
               Res_Type := Get_Type (Res);
               if Is_Null (Res_Type) then
                  return Null_Iir;
               end if;
               if A_Type /= Null_Iir and then Res_Type /= A_Type then
                  Error_Not_Match (Res, A_Type);
                  return Null_Iir;
               end if;
               return Res;
            end;

         when Iir_Kind_String_Literal8 =>
            --  LRM93 7.3.1 Literals
            --  The type of a string or bit string literal must be
            --  determinable solely from the context in whcih the literal
            --  appears, excluding the literal itself [...]
            if A_Type = Null_Iir then
               return Expr;
            end if;

            if not Is_String_Literal_Type (A_Type, Expr) then
               Error_Not_Match (Expr, A_Type);
               return Null_Iir;
            else
               Replace_Type (Expr, A_Type);
               Sem_String_Literal (Expr);
               return Expr;
            end if;

         when Iir_Kind_Null_Literal =>
            Set_Expr_Staticness (Expr, Locally);
            --  GHDL: the LRM doesn't explain how the type of NULL is
            --  determined.  Use the same rule as string or aggregates.
            if A_Type = Null_Iir then
               return Expr;
            end if;
            if not Is_Null_Literal_Type (A_Type) then
               Error_Msg_Sem (+Expr, "null literal can only be access type");
               return Null_Iir;
            else
               Set_Type (Expr, A_Type);
               return Expr;
            end if;

         when Iir_Kind_Aggregate =>
            --  LRM93 7.3.2 Aggregates
            --  The type of an aggregate must be determinable solely from the
            --  context in which the aggregate appears, excluding the aggregate
            --  itself but [...]
            if A_Type = Null_Iir then
               return Expr;
            else
               return Sem_Aggregate (Expr, A_Type, False);
            end if;

         when Iir_Kind_Parenthesis_Expression =>
            return Sem_Parenthesis_Expression (Expr, A_Type1);

         when Iir_Kind_Qualified_Expression =>
            return Sem_Qualified_Expression (Expr, A_Type);

         when Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype =>
            return Sem_Allocator (Expr, A_Type);

         when Iir_Kind_Procedure_Declaration =>
            Error_Msg_Sem (+Expr, "%n cannot be used as an expression", +Expr);
            return Null_Iir;

         when Iir_Kind_Range_Expression =>
            --  That's an error.  Can happen for:
            --    c (1 downto 0);
            --  which is first parsed as a target of a concurrent assignment,
            --  and then as a concurrent procedure call.
            declare
               Res : Iir;
            begin
               Res := Sem_Simple_Range_Expression (Expr, A_Type, True);
               return Create_Error_Expr (Res, A_Type);
            end;

         when Iir_Kind_Psl_Prev =>
            return Sem_Psl.Sem_Prev_Builtin (Expr, A_Type);

         when Iir_Kind_Psl_Stable
            | Iir_Kind_Psl_Rose
            | Iir_Kind_Psl_Fell =>
            return Sem_Psl.Sem_Clock_Builtin (Expr);

         when Iir_Kind_Psl_Onehot
            | Iir_Kind_Psl_Onehot0 =>
            return Sem_Psl.Sem_Onehot_Builtin (Expr);

         when Iir_Kind_Error =>
            --  Always ok.
            --  Use the error as a type.
            Set_Type (Expr, Expr);
            return Expr;

         when others =>
            Error_Kind ("sem_expression_ov", Expr);
            return Null_Iir;
      end case;
   end Sem_Expression_Ov;

   function Is_Expr_Not_Analyzed (Expr : Iir) return Boolean is
   begin
      return Get_Type (Expr) = Null_Iir;
   end Is_Expr_Not_Analyzed;

   function Is_Expr_Fully_Analyzed (Expr : Iir) return Boolean is
   begin
      return Is_Defined_Type (Get_Type (Expr));
   end Is_Expr_Fully_Analyzed;

   function Get_Wildcard_Type (Wildcard : Iir; Atype : Iir) return Iir is
   begin
      if Atype in Iir_Wildcard_Types then
         --  Special wildcard vs wildcard.
         case Iir_Wildcard_Types (Wildcard) is
            when Wildcard_Any_Type =>
               return Atype;
            when Wildcard_Any_Aggregate_Type =>
               case Iir_Wildcard_Types (Atype) is
                  when Wildcard_Any_Type
                    | Wildcard_Any_Aggregate_Type =>
                     return Wildcard_Any_Aggregate_Type;
                  when Wildcard_Any_String_Type =>
                     return Wildcard_Any_String_Type;
                  when Wildcard_Psl_Bitvector_Type =>
                     return Wildcard_Psl_Bitvector_Type;
                  when Wildcard_Any_Access_Type
                     | Wildcard_Any_Integer_Type
                     | Wildcard_Psl_Bit_Type
                     | Wildcard_Psl_Boolean_Type =>
                     return Null_Iir;
               end case;
            when Wildcard_Any_String_Type =>
               case Iir_Wildcard_Types (Atype) is
                  when Wildcard_Any_Type
                     | Wildcard_Any_Aggregate_Type
                     | Wildcard_Any_String_Type =>
                     return Wildcard_Any_String_Type;
                  when Wildcard_Psl_Bitvector_Type =>
                     return Wildcard_Psl_Bitvector_Type;
                  when Wildcard_Any_Access_Type
                     | Wildcard_Any_Integer_Type
                     | Wildcard_Psl_Bit_Type
                     | Wildcard_Psl_Boolean_Type =>
                     return Null_Iir;
               end case;
            when Wildcard_Any_Access_Type =>
               case Iir_Wildcard_Types (Atype) is
                  when Wildcard_Any_Type
                     | Wildcard_Any_Access_Type =>
                     return Wildcard_Any_Access_Type;
                  when Wildcard_Any_Aggregate_Type
                     | Wildcard_Any_String_Type
                     | Wildcard_Any_Integer_Type
                     | Wildcard_Psl_Bit_Type
                     | Wildcard_Psl_Bitvector_Type
                     | Wildcard_Psl_Boolean_Type =>
                     return Null_Iir;
               end case;
            when Wildcard_Any_Integer_Type =>
               case Iir_Wildcard_Types (Atype) is
                  when Wildcard_Any_Type
                     | Wildcard_Any_Integer_Type =>
                     return Wildcard_Any_Integer_Type;
                  when Wildcard_Any_Access_Type
                     | Wildcard_Any_Aggregate_Type
                     | Wildcard_Any_String_Type
                     | Wildcard_Psl_Bit_Type
                     | Wildcard_Psl_Boolean_Type
                     | Wildcard_Psl_Bitvector_Type =>
                     return Null_Iir;
               end case;
            when Wildcard_Psl_Bit_Type =>
               case Iir_Wildcard_Types (Atype) is
                  when Wildcard_Any_Type
                     | Wildcard_Psl_Bit_Type =>
                     return Wildcard_Psl_Bit_Type;
                  when Wildcard_Any_Access_Type
                     | Wildcard_Any_Aggregate_Type
                     | Wildcard_Any_String_Type
                     | Wildcard_Any_Integer_Type
                     | Wildcard_Psl_Bitvector_Type
                     | Wildcard_Psl_Boolean_Type =>
                     return Null_Iir;
               end case;
            when Wildcard_Psl_Bitvector_Type =>
               case Iir_Wildcard_Types (Atype) is
                  when Wildcard_Any_Type
                     | Wildcard_Any_Aggregate_Type
                     | Wildcard_Any_String_Type
                     | Wildcard_Psl_Bitvector_Type =>
                     return Wildcard_Psl_Bitvector_Type;
                  when Wildcard_Any_Access_Type
                     | Wildcard_Any_Integer_Type
                     | Wildcard_Psl_Bit_Type
                     | Wildcard_Psl_Boolean_Type =>
                     return Null_Iir;
               end case;
            when Wildcard_Psl_Boolean_Type =>
               case Iir_Wildcard_Types (Atype) is
                  when Wildcard_Any_Type
                     | Wildcard_Psl_Boolean_Type =>
                     return Wildcard_Psl_Boolean_Type;
                  when Wildcard_Psl_Bit_Type =>
                     return Wildcard_Psl_Bit_Type;
                  when Wildcard_Any_Access_Type
                     | Wildcard_Any_Aggregate_Type
                     | Wildcard_Any_String_Type
                     | Wildcard_Any_Integer_Type
                     | Wildcard_Psl_Bitvector_Type =>
                     return Null_Iir;
               end case;
         end case;
      else
         case Iir_Wildcard_Types (Wildcard) is
            when Wildcard_Any_Type =>
               --  Match with any type.
               return Atype;
            when Wildcard_Any_Aggregate_Type =>
               if Is_Aggregate_Type (Atype) then
                  return Atype;
               end if;
            when Wildcard_Any_String_Type =>
               if Is_String_Type (Atype) then
                  return Atype;
               end if;
            when Wildcard_Any_Access_Type =>
               if Get_Kind (Get_Base_Type (Atype))
                 = Iir_Kind_Access_Type_Definition
               then
                  return Atype;
               end if;
            when Wildcard_Any_Integer_Type =>
               if Get_Kind (Get_Base_Type (Atype))
                 = Iir_Kind_Integer_Type_Definition
               then
                  return Atype;
               end if;
            when Wildcard_Psl_Bit_Type =>
               if Sem_Psl.Is_Psl_Bit_Type (Atype) then
                  return Atype;
               end if;
            when Wildcard_Psl_Bitvector_Type =>
               if Sem_Psl.Is_Psl_Bitvector_Type (Atype) then
                  return Atype;
               end if;
            when Wildcard_Psl_Boolean_Type =>
               if Sem_Psl.Is_Psl_Boolean_Type (Atype) then
                  return Atype;
               end if;
         end case;
         return Null_Iir;
      end if;
   end Get_Wildcard_Type;

   function Compatible_Types_Intersect_Single (T1, T2 : Iir) return Iir is
   begin
      if T1 = T2 then
         return T1;
      end if;
      if T1 in Iir_Wildcard_Types then
         return Get_Wildcard_Type (T1, T2);
      elsif T2 in Iir_Wildcard_Types then
         return Get_Wildcard_Type (T2, T1);
      else
         return Get_Common_Basetype (Get_Base_Type (T1), Get_Base_Type (T2));
      end if;
   end Compatible_Types_Intersect_Single;

   function Compatible_Types_Intersect_Single_List (A_Type, Types_List : Iir)
                                                   return Iir
   is
      Types_List_List : Iir_List;
      It : List_Iterator;
      El: Iir;
      Com : Iir;
      Res : Iir;
   begin
      if not Is_Overload_List (Types_List) then
         return Compatible_Types_Intersect_Single (A_Type, Types_List);
      else
         Types_List_List := Get_Overload_List (Types_List);
         Res := Null_Iir;
         It := List_Iterate (Types_List_List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            Com := Compatible_Types_Intersect_Single (El, A_Type);
            if Com /= Null_Iir then
               Add_Result (Res, Com);
            end if;
            Next (It);
         end loop;
         return Res;
      end if;
   end Compatible_Types_Intersect_Single_List;

   function Compatible_Types_Intersect (List1, List2 : Iir) return Iir
   is
      List1_List : Iir_List;
      It1 : List_Iterator;
      Res : Iir;
      El : Iir;
      Tmp : Iir;
   begin
      if List1 = Null_Iir or else List2 = Null_Iir then
         return Null_Iir;
      end if;

      if Is_Overload_List (List1) then
         List1_List := Get_Overload_List (List1);
         Res := Null_Iir;
         It1 := List_Iterate (List1_List);
         while Is_Valid (It1) loop
            El := Get_Element (It1);
            Tmp := Compatible_Types_Intersect_Single_List (El, List2);
            if Tmp /= Null_Iir then
               Add_Result (Res, Tmp);
            end if;
            Next (It1);
         end loop;
         return Res;
      else
         return Compatible_Types_Intersect_Single_List (List1, List2);
      end if;
   end Compatible_Types_Intersect;

   function Sem_Expression_Wildcard
     (Expr : Iir; Atype : Iir; Constrained : Boolean := False)
     return Iir
   is
      Expr_Type : constant Iir := Get_Type (Expr);
      Atype_Defined : constant Boolean := Is_Defined_Type (Atype);
      Expr_Type_Defined : constant Boolean := Is_Defined_Type (Expr_Type);
   begin
      if Expr_Type /= Null_Iir then
         --  EXPR is at least partially analyzed.
         if Expr_Type_Defined or else not Atype_Defined then
            --  Nothing to do if:
            --  - Expression is already fully analyzed: caller has to merge
            --    types
            --  - Expression is partially analyzed but ATYPE is not defined:
            --    caller has to merge types.
            return Expr;
         end if;
      end if;

      case Get_Kind (Expr) is
         when Iir_Kind_Aggregate =>
            if Atype_Defined then
               return Sem_Aggregate (Expr, Atype, Constrained);
            else
               pragma Assert (Expr_Type = Null_Iir);
               Set_Type (Expr, Wildcard_Any_Aggregate_Type);
            end if;
            return Expr;

         when Iir_Kind_String_Literal8 =>
            if Atype_Defined then
               if not Is_String_Literal_Type (Atype, Expr) then
                  Error_Not_Match (Expr, Atype);
                  Set_Type (Expr, Error_Type);
               else
                  Set_Type (Expr, Atype);
                  Sem_String_Literal (Expr);
               end if;
            else
               pragma Assert (Expr_Type = Null_Iir);
               Set_Type (Expr, Wildcard_Any_String_Type);
            end if;
            return Expr;

         when Iir_Kind_Null_Literal =>
            if Atype_Defined then
               if not Is_Null_Literal_Type (Atype) then
                  Error_Not_Match (Expr, Atype);
                  Set_Type (Expr, Error_Type);
               else
                  Set_Type (Expr, Atype);
                  Set_Expr_Staticness (Expr, Locally);
               end if;
            else
               pragma Assert (Expr_Type = Null_Iir);
               Set_Type (Expr, Wildcard_Any_Access_Type);
            end if;
            return Expr;

         when Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype =>
            if Atype_Defined then
               if not Is_Null_Literal_Type (Atype) then
                  Error_Not_Match (Expr, Atype);
                  Set_Type (Expr, Error_Type);
               else
                  return Sem_Allocator (Expr, Atype);
               end if;
            else
               pragma Assert (Expr_Type = Null_Iir);
               Set_Type (Expr, Wildcard_Any_Access_Type);
            end if;
            return Expr;

         when Iir_Kind_Parenthesis_Expression =>
            declare
               Sub_Expr : Iir;
               Ntype : Iir;
            begin
               Sub_Expr := Get_Expression (Expr);
               if Atype_Defined and then not Flag_Relaxed_Rules then
                  --  Very important: loose the subtype due to
                  --  LRM93 7.3.2.2 Array aggregate.
                  Ntype := Get_Base_Type (Atype);
               else
                  Ntype := Atype;
               end if;
               Sub_Expr := Sem_Expression_Wildcard (Sub_Expr, Ntype);
               if Sub_Expr /= Null_Iir then
                  Set_Expression (Expr, Sub_Expr);
                  Set_Type (Expr, Get_Type (Sub_Expr));
                  Set_Expr_Staticness (Expr, Get_Expr_Staticness (Sub_Expr));
               else
                  Set_Type (Expr, Error_Type);
               end if;
            end;
            return Expr;

         when others =>
            if Atype_Defined then
               return Sem_Expression_Ov (Expr, Get_Base_Type (Atype));
            else
               declare
                  Res : Iir;
                  Res_Type : Iir;
                  Prev_Res_Type : Iir;
               begin
                  pragma Assert (Expr_Type = Null_Iir);
                  if Atype in Iir_Wildcard_Types then
                     --  Analyze without known type.
                     Res := Sem_Expression_Ov (Expr, Null_Iir);
                     if Res = Null_Iir or else Is_Error (Res) then
                        Set_Type (Expr, Error_Type);
                        return Expr;
                     end if;
                     Prev_Res_Type := Get_Type (Res);

                     --  Filter possible type.
                     Res_Type := Compatible_Types_Intersect_Single_List
                       (Atype, Prev_Res_Type);

                     if Res_Type = Null_Iir then
                        --  No matching type.  This is an error.
                        Error_Not_Match (Expr, Atype);
                        Set_Type (Expr, Error_Type);
                     elsif Is_Defined_Type (Res_Type) then
                        --  Known and defined matching type.
                        if Res_Type /= Prev_Res_Type then
                           --  Need to refine analysis.
                           Res := Sem_Expression_Ov (Expr, Res_Type);
                        end if;
                     else
                        --  Matching but not defined type (overload).
                        Set_Type (Expr, Res_Type);
                     end if;
                     if Is_Overload_List (Prev_Res_Type) then
                        Free_Overload_List (Prev_Res_Type);
                     end if;
                     return Res;
                  else
                     pragma Assert (Atype = Null_Iir);
                     return Sem_Expression_Ov (Expr, Atype);
                  end if;
               end;
            end if;
      end case;
   end Sem_Expression_Wildcard;

   procedure Merge_Wildcard_Type (Expr : Iir; Atype : in out Iir)
   is
      Result_Type : Iir;
      Expr_Type : Iir;
   begin
      if Is_Error (Expr) then
         return;
      end if;

      Expr_Type := Get_Type (Expr);
      if Is_Error (Expr_Type) then
         return;
      end if;

      if not Is_Overload_List (Expr_Type) then
         --  Use the base type; EXPR may define its own subtype (like in
         --  qualified expression with forwarding) which must not be
         --  referenced before it is defined (so by a parent).  In any case,
         --  that also makes sense: we need to deal with types, not with
         --  subtypes.
         Expr_Type := Get_Base_Type (Expr_Type);
         pragma Assert (Expr_Type /= Null_Iir);
      end if;

      Result_Type := Compatible_Types_Intersect (Atype, Expr_Type);
      if Atype /= Null_Iir and then Is_Overload_List (Atype) then
         Free_Overload_List (Atype);
      end if;
      if Result_Type /= Null_Iir then
         if Is_Defined_Type (Atype) then
            --  If ATYPE was already defined, keep it.  So that subtypes
            --  are kept (this is needed for aggregates and always helpful).
            null;
         else
            Atype := Result_Type;
         end if;
      else
         Atype := Result_Type;
      end if;
   end Merge_Wildcard_Type;

   -- If A_TYPE is not null, then EXPR must be of type A_TYPE.
   -- Return null in case of error.
   function Sem_Expression (Expr: Iir; A_Type: Iir) return Iir
   is
      A_Type1: Iir;
      Res: Iir;
      Expr_Type : Iir;
   begin
      if Check_Is_Expression (Expr, Expr) = Null_Iir then
         return Null_Iir;
      end if;

      --  Can't try to run sem_expression_ov when a node was already analyzed
      Expr_Type := Get_Type (Expr);
      if Expr_Type /= Null_Iir and then not Is_Overload_List (Expr_Type) then
         --  Checks types.
         --  This is necessary when the first call to sem_expression was done
         --  with A_TYPE set to NULL_IIR and results in setting the type of
         --  EXPR.
         if A_Type /= Null_Iir
           and then Are_Types_Compatible (A_Type, Expr_Type) = Not_Compatible
         then
            if not Is_Error (Expr_Type) then
               Error_Not_Match (Expr, A_Type);
            end if;
            return Null_Iir;
         end if;
         return Expr;
      end if;

      -- A_TYPE must be a type definition and not a subtype.
      if A_Type /= Null_Iir then
         A_Type1 := Get_Base_Type (A_Type);
      else
         A_Type1 := Null_Iir;
      end if;

      case Get_Kind (Expr) is
         when Iir_Kind_Aggregate =>
            Res := Sem_Aggregate (Expr, A_Type, False);
         when Iir_Kind_String_Literal8 =>
            if A_Type = Null_Iir then
               Res := Sem_Expression_Ov (Expr, Null_Iir);
            else
               if not Is_String_Literal_Type (A_Type, Expr) then
                  Error_Not_Match (Expr, A_Type);
                  return Null_Iir;
               end if;
               Set_Type (Expr, A_Type);
               Sem_String_Literal (Expr);
               return Expr;
            end if;
         when Iir_Kind_Parenthesis_Expression =>
            if Flag_Relaxed_Rules then
               --  With -frelaxed, consider parentheses as a no-op.
               --  The difference is significant for aggregates with 'others'
               --  choice.
               declare
                  Sub_Expr : Iir;
               begin
                  Sub_Expr := Get_Expression (Expr);
                  Sub_Expr := Sem_Expression (Sub_Expr, A_Type);
                  if Sub_Expr = Null_Iir then
                     return Null_Iir;
                  end if;
                  Set_Expression (Expr, Sub_Expr);
                  Set_Type (Expr, Get_Type (Sub_Expr));
                  Set_Expr_Staticness (Expr, Get_Expr_Staticness (Sub_Expr));
                  return Expr;
               end;
            else
               --  Loose the subtype, use the type.
               Res := Sem_Parenthesis_Expression (Expr, A_Type1);
            end if;
         when others =>
            Res := Sem_Expression_Ov (Expr, A_Type1);
      end case;

      if Res /= Null_Iir and then Is_Overloaded (Res) then
         --  FIXME: clarify between overload and not determinable from the
         --  context.
         if not Is_Error (Expr) then
            Report_Start_Group;
            Error_Overload (Expr);
            if Get_Type (Res) /= Null_Iir then
               Disp_Overload_List (Get_Overload_List (Get_Type (Res)), Expr);
            end if;
            Report_End_Group;
         end if;
         return Null_Iir;
      end if;
      return Res;
   end Sem_Expression;

   function Sem_Composite_Expression (Expr : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Sem_Expression_Ov (Expr, Null_Iir);
      if Res = Null_Iir or else Get_Type (Res) = Null_Iir then
         return Res;
      elsif Is_Overload_List (Get_Type (Res)) then
         declare
            List : constant Iir_List := Get_Overload_List (Get_Type (Res));
            It : List_Iterator;
            Res_Type : Iir;
            Atype : Iir;
         begin
            Res_Type := Null_Iir;
            It := List_Iterate (List);
            while Is_Valid (It) loop
               Atype := Get_Element (It);
               if Is_Aggregate_Type (Atype) then
                  Add_Result (Res_Type, Atype);
               end if;
               Next (It);
            end loop;

            if Res_Type = Null_Iir then
               Error_Overload (Expr);
               return Null_Iir;
            elsif Is_Overload_List (Res_Type) then
               Report_Start_Group;
               Error_Overload (Expr);
               Disp_Overload_List (Get_Overload_List (Res_Type), Expr);
               Report_End_Group;
               Free_Overload_List (Res_Type);
               return Null_Iir;
            else
               return Sem_Expression_Ov (Expr, Res_Type);
            end if;
         end;
      else
         --  Either an error (already handled) or not overloaded.  Type
         --  matching will be done later (when the target is analyzed).
         return Res;
      end if;
   end Sem_Composite_Expression;

   --  EXPR must be an expression with type is an overload list.
   --  Extract and finish the analysis of the expression that is of universal
   --  type, if there is one and if all types are either integer types or
   --  floating point types.
   --  This is used to get rid of implicit conversions.
   function Sem_Favour_Universal_Type (Expr : Iir) return Iir
   is
      Expr_Type : constant Iir := Get_Type (Expr);
      Type_List : constant Iir_List := Get_Overload_List (Expr_Type);
      --  Extract kind (from the first element).
      First_El : constant Iir := Get_First_Element (Type_List);
      Kind : constant Iir_Kind := Get_Kind (Get_Base_Type (First_El));
      Res : Iir;
      El : Iir;

      It : List_Iterator;
   begin
      Res := Null_Iir;

      It := List_Iterate (Type_List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         if Get_Kind (Get_Base_Type (El)) /= Kind then
            --  Must be of the same kind.
            Res := Null_Iir;
            exit;
         end if;
         if El = Universal_Integer_Type_Definition
           or El = Convertible_Integer_Type_Definition
           or El = Universal_Real_Type_Definition
           or El = Convertible_Real_Type_Definition
         then
            if Res = Null_Iir then
               Res := El;
            else
               Res := Null_Iir;
               exit;
            end if;
         end if;
         Next (It);
      end loop;

      if Res = Null_Iir then
         Report_Start_Group;
         Error_Overload (Expr);
         Disp_Overload_List (Type_List, Expr);
         Report_End_Group;
         return Null_Iir;
      end if;

      return Sem_Expression_Ov (Expr, Res);
   end Sem_Favour_Universal_Type;

   function Sem_Expression_Universal (Expr : Iir) return Iir
   is
      Expr1 : Iir;
      Expr_Type : Iir;
   begin
      Expr1 := Sem_Expression_Wildcard (Expr, Wildcard_Any_Type);
      Expr_Type := Get_Type (Expr1);
      if Is_Error (Expr_Type) then
         return Null_Iir;
      end if;
      if not Is_Overload_List (Expr_Type) then
         return Expr1;
      else
         return Sem_Favour_Universal_Type (Expr1);
      end if;
   end Sem_Expression_Universal;

   function Sem_Case_Expression (Expr : Iir) return Iir
   is
      Expr1 : Iir;
      Expr_Type : Iir;
      El : Iir;
      Res : Iir;
      List : Iir_List;
      It : List_Iterator;
   begin
      Expr1 := Sem_Expression_Ov (Expr, Null_Iir);
      if Expr1 = Null_Iir then
         return Null_Iir;
      end if;
      Expr_Type := Get_Type (Expr1);
      if Expr_Type = Null_Iir then
         --  Possible only if the type cannot be determined without the
         --  context (aggregate or string literal).
         Error_Msg_Sem
           (+Expr, "cannot determine the type of choice expression");
         if Get_Kind (Expr1) = Iir_Kind_Aggregate then
            Error_Msg_Sem
              (+Expr, "(use a qualified expression of the form T'(xxx).)");
         end if;
         return Null_Iir;
      end if;
      if not Is_Overload_List (Expr_Type) then
         return Expr1;
      end if;

      --  In case of overload, try to find one match.
      --  FIXME: match only character types.

      --  LRM93 8.8  Case statement
      --  This type must be determinable independently of the context in which
      --  the expression occurs, but using the fact that the expression must be
      --  of a discrete type or a one-dimensional character array type.
      List := Get_Overload_List (Expr_Type);
      Res := Null_Iir;
      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         if Get_Kind (El) in Iir_Kinds_Discrete_Type_Definition
           or else Is_One_Dimensional_Array_Type (El)
         then
            if Res = Null_Iir then
               Res := El;
            else
               Report_Start_Group;
               Error_Overload (Expr1);
               Disp_Overload_List (List, Expr1);
               Report_End_Group;
               return Null_Iir;
            end if;
         end if;
         Next (It);
      end loop;
      if Res = Null_Iir then
         Report_Start_Group;
         Error_Overload (Expr1);
         Disp_Overload_List (List, Expr1);
         Report_End_Group;
         return Null_Iir;
      end if;
      return Sem_Expression_Ov (Expr1, Get_Base_Type (Res));
   end Sem_Case_Expression;

   function Insert_Condition_Operator (Cond : Iir) return Iir
   is
      Op : Iir;
      Res : Iir;
   begin
      Op := Create_Iir (Iir_Kind_Implicit_Condition_Operator);
      Location_Copy (Op, Cond);
      Set_Operand (Op, Cond);

      Res := Sem_Operator (Op, Boolean_Type_Definition);
      Check_Read (Res);
      return Res;
   end Insert_Condition_Operator;

   function Sem_Condition_Pass2 (Cond : Iir) return Iir
   is
      Cond_Type : Iir;
   begin
      Cond_Type := Get_Type (Cond);
      if Cond_Type = Null_Iir then
         --  Error.
         return Cond;
      end if;

      if not Is_Overload_List (Cond_Type) then
         --  Only one result.  Operator "??" is not applied if the result
         --  is of type boolean.
         if Are_Types_Compatible (Cond_Type, Boolean_Type_Definition)
           /= Not_Compatible
         then
            Check_Read (Cond);
            return Cond;
         end if;
      else
         --  Many interpretations.
         declare
            Res_List : constant Iir_List := Get_Overload_List (Cond_Type);
            It : List_Iterator;
            El : Iir;
            Nbr_Booleans : Natural;
            Res : Iir;
         begin
            Nbr_Booleans := 0;

            --  Extract boolean interpretations.
            It := List_Iterate (Res_List);
            while Is_Valid (It) loop
               El := Get_Element (It);
               if Are_Types_Compatible (El, Boolean_Type_Definition)
                 /= Not_Compatible
               then
                  Nbr_Booleans := Nbr_Booleans + 1;
               end if;
               Next (It);
            end loop;

            if Nbr_Booleans >= 1 then
               --  There is one or more boolean interpretations: keep them.
               --  In case of multiple boolean interpretations, an error
               --  message will be generated.
               Res := Sem_Expression_Ov (Cond, Boolean_Type_Definition);
               Check_Read (Res);
               return Res;
            end if;
         end;
      end if;

      --  LRM08 9.2.9
      --  Otherwise, the condition operator is implicitely applied, and the
      --  type of the expresion with the implicit application shall be
      --  BOOLEAN defined in package STANDARD.

      return Insert_Condition_Operator (Cond);
   end Sem_Condition_Pass2;

   function Sem_Condition (Cond : Iir) return Iir
   is
      Res : Iir;
   begin
      --  This function fully analyze COND, so it supposes COND is not yet
      --  analyzed.
      pragma Assert (Is_Expr_Not_Analyzed (Cond));

      if Vhdl_Std < Vhdl_08 then
         Res := Sem_Expression (Cond, Boolean_Type_Definition);

         Check_Read (Res);
         return Res;
      else
         --  LRM08 9.2.9
         --  If, without overload resolution (see 12.5), the expression is
         --  of type BOOLEAN defined in package STANDARD, or if, assuming a
         --  rule requiring the expression to be of type BOOLEAN defined in
         --  package STANDARD, overload resolution can determine at least one
         --  interpretation of each constituent of the innermost complete
         --  context including the expression, then the condition operator is
         --  not applied.

         Res := Sem_Expression_Wildcard (Cond, Null_Iir);

         if Res = Null_Iir then
            --  Error occurred.
            return Null_Iir;
         end if;

         return Sem_Condition_Pass2 (Res);
      end if;
   end Sem_Condition;

end Vhdl.Sem_Expr;
