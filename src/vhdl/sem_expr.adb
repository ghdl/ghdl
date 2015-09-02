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
with Std_Package; use Std_Package;
with Errorout; use Errorout;
with Flags; use Flags;
with Sem_Scopes; use Sem_Scopes;
with Sem_Names; use Sem_Names;
with Sem;
with Name_Table;
with Str_Table;
with Iirs_Utils; use Iirs_Utils;
with Evaluation; use Evaluation;
with Iir_Chains; use Iir_Chains;
with Sem_Types;
with Sem_Stmts; use Sem_Stmts;
with Sem_Assocs; use Sem_Assocs;
with Xrefs; use Xrefs;

package body Sem_Expr is
   procedure Not_Match (Expr: Iir; A_Type: Iir)
   is
      pragma Inline (Not_Match);
   begin
      Error_Not_Match (Expr, A_Type, Expr);
   end Not_Match;

--    procedure Not_Match (Expr: Iir; Type1: Iir; Type2: Iir) is
--    begin
--       Error_Msg_Sem
--         ("can't match '" & Disp_Node (Expr) & "' with type '"
--          & Disp_Node (Type1) & "' or type '" & Disp_Node (Type2) & "'",
--          Expr);
--    end Not_Match;

--    procedure Overloaded (Expr: Iir) is
--    begin
--       Error_Msg_Sem
--         ("cant resolve overloaded identifier '" & Get_String (Expr) & "'",
--          Expr);
--    end Overloaded;

   -- Replace type of TARGET by A_TYPE.
   -- If TARGET has already a type, it must be an overload list, and in this
   -- case, this list is freed, or it must be A_TYPE.
   -- A_TYPE can't be an overload list.
   --
   -- This procedure can be called in the second pass, when the type is known.
   procedure Replace_Type (Target: Iir; A_Type: Iir) is
      Old_Type: Iir;
   begin
      Old_Type := Get_Type (Target);
      if Old_Type /= Null_Iir then
         if Is_Overload_List (Old_Type) then
            Free_Iir (Old_Type);
         elsif Old_Type = A_Type then
            return;
         else
            -- Cannot replace a type.
            raise Internal_Error;
         end if;
      end if;
      if A_Type = Null_Iir then
         return;
      end if;
      if Is_Overload_List (A_Type) then
         raise Internal_Error;
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

   -- Return the common type of base types LEFT and RIGHT.
   -- LEFT are RIGHT must be really base types (not subtypes).
   -- Roughly speaking, it returns LEFT (= RIGHT) if LEFT = RIGHT (ie, same
   -- type), null otherwise.
   -- However, it handles implicite conversions of universal types.
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
      Level : Compatibility_Level;
   begin
      pragma Assert (not Is_Overload_List (Left_Type));

      if Is_Overload_List (Right_Types) then
         Right_List := Get_Overload_List (Right_Types);
         Level := Not_Compatible;
         for I in Natural loop
            El := Get_Nth_Element (Right_List, I);
            exit when El = Null_Iir;
            Level := Compatibility_Level'Max
              (Level, Are_Types_Compatible (Left_Type, El));
            if Level = Fully_Compatible then
               return Fully_Compatible;
            end if;
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
      Left_Type, Right_Type : Iir;
   begin
      Left_Type := Get_Base_Type (Get_Type (Left));
      Right_Type := Get_Type (Right);

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
            Error_Kind ("are_node_compatible_ov", Left_Type);
      end case;

      return Compatibility_Types1 (Left_Type, Right_Type);
   end Compatibility_Nodes;

   --  Return TRUE iff A_TYPE can be the type of string or bit string literal
   --  EXPR.  EXPR is needed to distinguish between string and bit string
   --  for VHDL87 rule about the type of a bit string.
   function Is_String_Literal_Type (A_Type : Iir; Expr : Iir) return Boolean
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
           | Iir_Kind_Psl_Declaration =>
            Error_Msg_Sem (Disp_Node (Expr)
                           & " not allowed in an expression", Loc);
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
           | Iir_Kind_Qualified_Expression =>
            return Expr;
         when Iir_Kinds_Quantity_Declaration =>
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

   function Check_Implicit_Conversion (Targ_Type : Iir; Expr : Iir)
                                      return Boolean
   is
      Expr_Type : Iir;
      Targ_Indexes : Iir_List;
      Expr_Indexes : Iir_List;
      Targ_Index : Iir;
      Expr_Index : Iir;
   begin
      --  Handle errors.
      if Targ_Type = Null_Iir or else Expr = Null_Iir then
         return True;
      end if;
      if Get_Kind (Targ_Type) /= Iir_Kind_Array_Subtype_Definition
        or else Get_Constraint_State (Targ_Type) /= Fully_Constrained
      then
         return True;
      end if;
      Expr_Type := Get_Type (Expr);
      if Expr_Type = Null_Iir
        or else Get_Kind (Expr_Type) /= Iir_Kind_Array_Subtype_Definition
        or else Get_Constraint_State (Expr_Type) /= Fully_Constrained
      then
         return True;
      end if;
      Targ_Indexes := Get_Index_Subtype_List (Targ_Type);
      Expr_Indexes := Get_Index_Subtype_List (Expr_Type);
      for I in Natural loop
         Targ_Index := Get_Index_Type (Targ_Indexes, I);
         Expr_Index := Get_Index_Type (Expr_Indexes, I);
         exit when Targ_Index = Null_Iir and Expr_Index = Null_Iir;
         if Targ_Index = Null_Iir or Expr_Index = Null_Iir then
            --  Types does not match.
            raise Internal_Error;
         end if;
         if Get_Type_Staticness (Targ_Index) = Locally
           and then Get_Type_Staticness (Expr_Index) = Locally
         then
            if Eval_Discrete_Type_Length (Targ_Index)
              /= Eval_Discrete_Type_Length (Expr_Index)
            then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Check_Implicit_Conversion;

   -- Find a type compatible with A_TYPE in TYPE_LIST (which can be an
   -- overload list or a simple type) and return it.
   -- In case of failure, return null.
   function Search_Overloaded_Type (Type_List: Iir; A_Type: Iir)
     return Iir
   is
      Type_List_List : Iir_List;
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
         for I in Natural loop
            El := Get_Nth_Element (Type_List_List, I);
            exit when El = Null_Iir;
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
      Res : Iir;
      El : Iir;
      Tmp : Iir;
   begin
      if Is_Overload_List (List1) then
         List1_List := Get_Overload_List (List1);
         Res := Null_Iir;
         for I in Natural loop
            El := Get_Nth_Element (List1_List, I);
            exit when El = Null_Iir;
            Tmp := Search_Overloaded_Type (List2, El);
            if Tmp /= Null_Iir then
               if Res = Null_Iir then
                  Res := Tmp;
               else
                  --  Several types match.
                  return Null_Iir;
               end if;
            end if;
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
      Left := Get_Left_Limit (Expr);
      Right := Get_Right_Limit (Expr);

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
            --  Error.
            return Null_Iir;
         end if;

         Left_Type := Get_Type (Left);
         Right_Type := Get_Type (Right);
         --  Check for string or aggregate literals
         --  FIXME: improve error message
         if Left_Type = Null_Iir then
            Error_Msg_Sem ("bad expression for a scalar", Left);
            return Null_Iir;
         end if;
         if Right_Type = Null_Iir then
            Error_Msg_Sem ("bad expression for a scalar", Right);
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
                    ("left and right expressions of range are not compatible",
                     Expr);
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
                 ("left and right expressions of range are not compatible",
                  Expr);
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
                 ("type of range doesn't match expected type", Expr);
               return Null_Iir;
            end if;

            return Expr;
         end if;
      end if;

      Left := Eval_Expr_If_Static (Left);
      Right := Eval_Expr_If_Static (Right);
      Set_Left_Limit (Expr, Left);
      Set_Right_Limit (Expr, Right);
      Set_Expr_Staticness (Expr, Min (Get_Expr_Staticness (Left),
                                      Get_Expr_Staticness (Right)));

      if A_Type /= Null_Iir
        and then Are_Types_Compatible (Expr_Type, A_Type) = Not_Compatible
      then
         Error_Msg_Sem ("type of range doesn't match expected type", Expr);
         return Null_Iir;
      end if;

      Set_Type (Expr, Expr_Type);
      if Get_Kind (Get_Base_Type (Expr_Type))
        not in Iir_Kinds_Scalar_Type_Definition
      then
         Error_Msg_Sem ("type of range is not a scalar type", Expr);
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
            if Res = Null_Iir then
               return Null_Iir;
            end if;
            Res_Type := Get_Type (Res);

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
                  Error_Msg_Sem ("name must denote a range", Expr);
                  return Null_Iir;
            end case;
            if A_Type /= Null_Iir
              and then Get_Base_Type (Res_Type) /= Get_Base_Type (A_Type)
            then
               Not_Match (Expr, A_Type);
               return Null_Iir;
            end if;

         when others =>
            Error_Msg_Sem ("range expression required", Expr);
            return Null_Iir;
      end case;

      if Get_Kind (Res_Type) not in Iir_Kinds_Scalar_Type_Definition then
         Error_Msg_Sem (Disp_Node (Res) & " is not a range type", Expr);
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

   function Sem_Discrete_Range_Expression
     (Expr: Iir; A_Type: Iir; Any_Dir : Boolean)
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
            Error_Msg_Sem ("subtype " & Disp_Node (Res)
                             & " doesn't match expected type "
                             & Disp_Node (A_Type), Expr);
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
               Error_Msg_Sem ("range is not discrete", Res);
            else
               Error_Msg_Sem
                 (Disp_Node (Res) & " is not a discrete range type", Expr);
            end if;
         end if;
         return Null_Iir;
      end if;

      return Res;
   end Sem_Discrete_Range_Expression;

   function Sem_Discrete_Range_Integer (Expr: Iir) return Iir
   is
      Res : Iir;
      Range_Type : Iir;
   begin
      Res := Sem_Discrete_Range_Expression (Expr, Null_Iir, True);
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
         elsif Vhdl_Std = Vhdl_93c or else Flag_Relaxed_Rules then
            --  GHDL: this is not allowed, however often used:
            --  eg: for i in 0 to v'length + 1 loop
            --  eg: for i in -1 to 1 loop

            --  Be tolerant.
            Warning_Msg_Sem ("universal integer bound must be numeric literal "
                             & "or attribute", Res);
         else
            Error_Msg_Sem ("universal integer bound must be numeric literal "
                           & "or attribute", Res);
         end if;
         Set_Type (Res, Integer_Type_Definition);
      end if;
      return Res;
   end Sem_Discrete_Range_Integer;

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
      case Get_Implicit_Definition (Imp) is
         when Iir_Predefined_Error =>
            raise Internal_Error;
         when Iir_Predefined_Pure_Functions =>
            null;
         when Iir_Predefined_Impure_Functions =>
            --  Predefined functions such as Now, Endfile are not static.
            Staticness := None;
         when Iir_Predefined_Explicit =>
            if Get_Pure_Flag (Imp) then
               Staticness := Min (Staticness, Globally);
            else
               Staticness := None;
            end if;
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
   procedure Sem_Call_Purity_Check (Subprg : Iir; Callee : Iir; Loc : Iir)
   is
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
         when Iir_Kind_Function_Declaration =>
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
         when others =>
            Error_Kind ("sem_call_purity_check", Callee);
      end case;
   end Sem_Call_Purity_Check;

   procedure Sem_Call_Wait_Check (Subprg : Iir; Callee : Iir; Loc : Iir)
   is
      procedure Error_Wait is
      begin
         Error_Msg_Sem
           (Disp_Node (Subprg) & " must not contain wait statement, but calls",
            Loc);
         Error_Msg_Sem
           (Disp_Node (Callee) & " which has (indirectly) a wait statement",
            Callee);
         --Error_Msg_Sem
         --  ("(indirect) wait statement not allowed in " & Where, Loc);
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
                     Error_Msg_Sem
                       ("all-sensitized " & Disp_Node (Subprg)
                          & " can't call " & Disp_Node (Callee), Loc);
                     Error_Msg_Sem
                       (" (as this subprogram reads (indirectly) a signal)",
                        Loc);
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
      Mark_Subprogram_Used (Imp);

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
                          (Disp_Node (Subprg)
                           & " is passive, but calls non-passive "
                           & Disp_Node (Imp), Expr);
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
      Nbr_Inter: Natural;
      A_Func: Iir;
      Imp_List: Iir_List;
      Assoc_Chain: Iir;
      Inter_Chain : Iir;
      Res_Type: Iir_List;
      Inter: Iir;
      Match : Compatibility_Level;
      Match_Max : Compatibility_Level;
   begin
      --  Sem_Name has gathered all the possible names for the prefix of this
      --  call.  Reduce this list to only names that match the types.
      Nbr_Inter := 0;
      Imp := Get_Implementation (Expr);
      Imp_List := Get_Overload_List (Imp);
      Assoc_Chain := Get_Parameter_Association_Chain (Expr);
      Match_Max := Via_Conversion;

      for I in Natural loop
         A_Func := Get_Nth_Element (Imp_List, I);
         exit when A_Func = Null_Iir;

         case Get_Kind (A_Func) is
            when Iir_Kinds_Functions_And_Literals =>
               if not Is_Func_Call then
                  --  The identifier of a function call must be a function or
                  --  an enumeration literal.
                  goto Continue;
               end if;
            when Iir_Kind_Procedure_Declaration =>
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
                  Nbr_Inter := 0;
                  Match_Max := Match;
               end if;
               Replace_Nth_Element (Imp_List, Nbr_Inter, A_Func);
               Nbr_Inter := Nbr_Inter + 1;
            end if;
         end if;

         << Continue >> null;
      end loop;
      Set_Nbr_Elements (Imp_List, Nbr_Inter);

      -- Set_Implementation (Expr, Inter_List);
      -- A set of possible functions to call is in INTER_LIST.
      -- Create a set of possible return type in RES_TYPE.
      case Nbr_Inter is
         when 0 =>
            --  FIXME: display subprogram name.
            Error_Msg_Sem
              ("cannot resolve overloading for subprogram call", Expr);
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
                  Error_Overload (Expr);
                  Disp_Overload_List (Imp_List, Expr);
                  return Null_Iir;
               end if;

               --  Create the list of types for the result.
               Res_Type := Create_Iir_List;
               for I in 0 .. Nbr_Inter - 1 loop
                  Add_Element
                    (Res_Type,
                     Get_Return_Type (Get_Nth_Element (Imp_List, I)));
               end loop;

               if Get_Nbr_Elements (Res_Type) = 1 then
                  -- several implementations but one profile.
                  Error_Overload (Expr);
                  Disp_Overload_List (Imp_List, Expr);
                  return Null_Iir;
               end if;
               Set_Type (Expr, Create_Overload_List (Res_Type));
            else
               --  For a procedure call, the context does't help to resolve
               --  overload.
               Error_Overload (Expr);
               Disp_Overload_List (Imp_List, Expr);
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
         if Get_Kind (Inter_List) = Iir_Kind_Error then
            return Null_Iir;
         elsif Is_Overload_List (Inter_List) then
            --  Subprogram name is overloaded.
            return Sem_Subprogram_Call_Stage1 (Expr, A_Type, Is_Func);
         else
            --  Only one interpretation for the subprogram name.
            if Is_Func then
               if Get_Kind (Inter_List) /= Iir_Kind_Function_Declaration then
                  Error_Msg_Sem ("name does not designate a function", Expr);
                  return Null_Iir;
               end if;
            else
               if Get_Kind (Inter_List) /= Iir_Kind_Procedure_Declaration then
                  Error_Msg_Sem ("name does not designate a procedure", Expr);
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
         for I in Natural loop
            Inter := Get_Nth_Element (Get_Overload_List (Inter_List), I);
            exit when Inter = Null_Iir;
            if Are_Basetypes_Compatible
              (A_Type, Get_Base_Type (Get_Return_Type (Inter)))
              /= Not_Compatible
            then
               if Res /= Null_Iir then
                  Error_Overload (Expr);
                  Disp_Overload_List (Get_Overload_List (Inter_List), Expr);
                  return Null_Iir;
               else
                  Res := Inter;
               end if;
            end if;
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
         Not_Match (Expr, A_Type);
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
                        Prefix := Get_Longuest_Static_Prefix (Prefix);
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
   function Get_Non_Implicit_Subprogram (List : Iir_List) return Iir
   is
      El : Iir;
      Res : Iir;
      Ref_Type : Iir;
   begin
      --  Conditions:
      --  1. All the possible functions must return boolean.
      --  2. There is only one implicit function for universal or real.
      Res := Null_Iir;
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         if Get_Base_Type (Get_Return_Type (El)) /= Boolean_Type_Definition
         then
            return Null_Iir;
         end if;

         if Is_Implicit_Subprogram (El) then
            Ref_Type := Get_Type_Reference (El);
            if Ref_Type = Universal_Integer_Type_Declaration
              or Ref_Type = Universal_Real_Type_Declaration
            then
               if Res = Null_Iir then
                  Res := El;
               else
                  return Null_Iir;
               end if;
            end if;
         end if;
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
      Res : Iir;
   begin
      if Get_Nbr_Elements (List) /= 2 then
         return Null_Iir;
      end if;

      Sub1 := Get_Nth_Element (List, 0);
      Sub2 := Get_Nth_Element (List, 1);

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

   function Sem_Operator (Expr : Iir; Res_Type : Iir; Arity : Positive)
      return Iir
   is
      Operator : Name_Id;
      Left, Right: Iir;
      Interpretation : Name_Interpretation_Type;
      Decl : Iir;
      Overload_List : Iir_List;
      Overload : Iir;
      Res_Type_List : Iir;
      Full_Compat : Iir;

      -- LEFT and RIGHT must be set.
      function Set_Uniq_Interpretation (Decl : Iir) return Iir
      is
         Interface_Chain : Iir;
         Err : Boolean;
      begin
         Set_Type (Expr, Get_Return_Type (Decl));
         Interface_Chain := Get_Interface_Declaration_Chain (Decl);
         Err := False;
         if Is_Overloaded (Left) then
            Left := Sem_Expression_Ov
              (Left, Get_Base_Type (Get_Type (Interface_Chain)));
            if Left = Null_Iir then
               Err := True;
            else
               if Arity = 1 then
                  Set_Operand (Expr, Left);
               else
                  Set_Left (Expr, Left);
               end if;
            end if;
         end if;
         Check_Read (Left);
         if Arity = 2 then
            if Is_Overloaded (Right) then
               Right := Sem_Expression_Ov
                 (Right,
                  Get_Base_Type (Get_Type (Get_Chain (Interface_Chain))));
               if Right = Null_Iir then
                  Err := True;
               else
                  Set_Right (Expr, Right);
               end if;
            end if;
            Check_Read (Right);
         end if;
         Destroy_Iir_List (Overload_List);
         if not Err then
            Set_Implementation (Expr, Decl);
            Sem_Subprogram_Call_Finish (Expr, Decl);
            return Eval_Expr_If_Static (Expr);
         else
            return Expr;
         end if;
      end Set_Uniq_Interpretation;

      --  Note: operator and implementation node of expr must be set.
      procedure Error_Operator_Overload (List : Iir_List) is
      begin
         Error_Msg_Sem ("operator """ & Name_Table.Image (Operator)
                        & """ is overloaded", Expr);
         Disp_Overload_List (List, Expr);
      end Error_Operator_Overload;

      Interface_Chain : Iir;
   begin
      if Arity = 1 then
         Left := Get_Operand (Expr);
         Right := Null_Iir;
      else
         Left := Get_Left (Expr);
         Right := Get_Right (Expr);
      end if;
      Operator := Iirs_Utils.Get_Operator_Name (Expr);

      if Get_Type (Expr) = Null_Iir then
         --  First pass.
         --  Analyze operands.
         --  FIXME: should try to analyze right operand even if analyze
         --  of left operand has failed ??
         if Get_Type (Left) = Null_Iir then
            Left := Sem_Expression_Ov (Left, Null_Iir);
            if Left = Null_Iir then
               return Null_Iir;
            end if;
            if Arity = 1 then
               Set_Operand (Expr, Left);
            else
               Set_Left (Expr, Left);
            end if;
         end if;
         if Arity = 2 and then Get_Type (Right) = Null_Iir then
            Right := Sem_Expression_Ov (Right, Null_Iir);
            if Right = Null_Iir then
               return Null_Iir;
            end if;
            Set_Right (Expr, Right);
         end if;

         Overload_List := Create_Iir_List;

         --  Try to find an implementation among user defined function
         Interpretation := Get_Interpretation (Operator);
         while Valid_Interpretation (Interpretation) loop
            Decl := Get_Non_Alias_Declaration (Interpretation);

            --  It is compatible with operand types ?
            if Get_Kind (Decl) /= Iir_Kind_Function_Declaration then
               raise Internal_Error;
            end if;

            --  LRM08 12.3 Visibility
            --  [...] or all visible declarations denote the same named entity.
            --
            --  GHDL: If DECL has already been seen, then skip it.
            if Get_Seen_Flag (Decl) then
               goto Next;
            end if;

            --  Check return type.
            if Res_Type /= Null_Iir
              and then (Are_Types_Compatible (Res_Type, Get_Return_Type (Decl))
                          = Not_Compatible)
            then
               goto Next;
            end if;

            Interface_Chain := Get_Interface_Declaration_Chain (Decl);

            --  Check arity.

            --  LRM93 2.5.2 Operator overloading
            --  The subprogram specification of a unary operator must have
            --  a single parameter [...]
            --  The subprogram specification of a binary operator must have
            --  two parameters [...]
            --
            --  GHDL: So even in presence of default expression in a parameter,
            --  a unary operation has to match with a binary operator.
            if Iir_Chains.Get_Chain_Length (Interface_Chain) /= Arity then
               goto Next;
            end if;

            -- Check operands.
            if Is_Expr_Compatible (Get_Type (Interface_Chain), Left)
              = Not_Compatible
            then
               goto Next;
            end if;
            if Arity = 2 then
               if Is_Expr_Compatible (Get_Type (Get_Chain (Interface_Chain)),
                                      Right)
                 = Not_Compatible
               then
                  goto Next;
               end if;
            end if;

            --  Match.
            Set_Seen_Flag (Decl, True);
            Append_Element (Overload_List, Decl);

            << Next >> null;
            Interpretation := Get_Next_Interpretation (Interpretation);
         end loop;

         --  Clear seen_flags.
         for I in Natural loop
            Decl := Get_Nth_Element (Overload_List, I);
            exit when Decl = Null_Iir;
            Set_Seen_Flag (Decl, False);
         end loop;

         --  The list of possible implementations was computed.
         case Get_Nbr_Elements (Overload_List) is
            when 0 =>
               Error_Msg_Sem
                 ("no function declarations for " & Disp_Node (Expr), Expr);
               Destroy_Iir_List (Overload_List);
               return Null_Iir;

            when 1 =>
               Decl := Get_First_Element (Overload_List);
               return Set_Uniq_Interpretation (Decl);

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
               if Arity = 2 then
                  Decl := Get_Non_Implicit_Subprogram (Overload_List);
                  if Decl /= Null_Iir then
                     return Set_Uniq_Interpretation (Decl);
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
               Error_Operator_Overload (Overload_List);

               --  Give an advice.
               if not Flags.Flag_Explicit
                 and then not Explicit_Advice_Given
                 and then Flags.Vhdl_Std < Vhdl_08
               then
                  Decl := Get_Explicit_Subprogram (Overload_List);
                  if Decl /= Null_Iir then
                     Error_Msg_Sem
                       ("(you may want to use the -fexplicit option)", Expr);
                     Explicit_Advice_Given := True;
                  end if;
               end if;

               return Null_Iir;
         end case;
      else
         --  Second pass
         --  Find the uniq implementation for this call.
         Overload := Get_Implementation (Expr);
         Overload_List := Get_Overload_List (Overload);
         Full_Compat := Null_Iir;
         for I in Natural loop
            Decl := Get_Nth_Element (Overload_List, I);
            exit when Decl = Null_Iir;
            --  FIXME: wrong: compatibilty with return type and args.
            if Are_Types_Compatible (Get_Return_Type (Decl), Res_Type)
              /= Not_Compatible
            then
               if Full_Compat /= Null_Iir then
                  Error_Operator_Overload (Overload_List);
                  return Null_Iir;
               else
                  Full_Compat := Decl;
               end if;
            end if;
         end loop;
         Free_Iir (Overload);
         Overload := Get_Type (Expr);
         Free_Overload_List (Overload);
         return Set_Uniq_Interpretation (Full_Compat);
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
            Decl := Get_Declaration (Inter);
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
         if Find_Name_In_List (Get_Enumeration_Literal_List (Etype), Id)
           = Null_Iir
         then
            --  ... because it is not defined.
            Error_Msg_Sem
              ("type " & Disp_Node (Etype) & " does not define character '"
               & C & "'", Str);
         else
            --  ... because it is not visible.
            Error_Msg_Sem ("character '" & C & "' of type "
                           & Disp_Node (Etype) & " is not visible", Str);
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
            if Eval_Discrete_Type_Length (Index_Type) /= Iir_Int64 (Len) then
               Error_Msg_Sem ("string length does not match that of "
                                & Disp_Node (Index_Type), Lit);
            end if;
         else
            --  FIXME: emit a warning because of dubious construct (the type
            --  of the string is not locally constrained) ?
            null;
         end if;
      else
         -- Context type is not constained.  Set type of the string literal,
         -- according to LRM93 7.3.2.2.
         N_Type := Create_Unidim_Array_By_Length
           (Lit_Base_Type, Iir_Int64 (Len), Lit);
         Set_Type (Lit, N_Type);
         Set_Literal_Subtype (Lit, N_Type);
      end if;
   end Sem_String_Literal;

   generic
      --  Compare two elements, return true iff OP1 < OP2.
      with function Lt (Op1, Op2 : Natural) return Boolean;

      --  Swap two elements.
      with procedure Swap (From : Natural; To : Natural);
   package Heap_Sort is
      --  Heap sort the N elements.
      procedure Sort (N : Natural);
   end Heap_Sort;

   package body Heap_Sort is
      --  An heap is an almost complete binary tree whose each edge is less
      --  than or equal as its decendent.

      --  Bubble down element I of a partially ordered heap of length N in
      --  array ARR.
      procedure Bubble_Down (I, N : Natural)
      is
         Child : Natural;
         Parent : Natural := I;
      begin
         loop
            Child := 2 * Parent;
            if Child < N and then Lt (Child, Child + 1) then
               Child := Child + 1;
            end if;
            exit when Child > N;
            exit when not Lt (Parent, Child);
            Swap (Parent, Child);
            Parent := Child;
         end loop;
      end Bubble_Down;

      --  Heap sort of ARR.
      procedure Sort (N : Natural)
      is
      begin
         --  Heapify
         for I in reverse 1 .. N / 2 loop
            Bubble_Down (I, N);
         end loop;

         --  Sort
         for I in reverse 2 .. N loop
            Swap (1, I);
            Bubble_Down (1, I - 1);
         end loop;
      end Sort;
   end Heap_Sort;

   procedure Sem_String_Choices_Range (Choice_Chain : Iir; Sel : Iir)
   is
      --  True if others choice is present.
      Has_Others : Boolean;

      --  Number of simple choices.
      Nbr_Choices : Natural;

      --  Type of SEL.
      Sel_Type : Iir;

      --  Type of the element of SEL.
      Sel_El_Type : Iir;
      --  Number of literals in the element type.
      Sel_El_Length : Iir_Int64;

      --  Length of SEL (number of characters in SEL).
      Sel_Length : Iir_Int64;

      --  Array of choices.
      Arr : Iir_Array_Acc;
      Index : Natural;

      --  True if length of a choice mismatches
      Has_Length_Error : Boolean := False;

      El : Iir;

      --  Compare two elements of ARR.
      --  Return true iff OP1 < OP2.
      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Compare_String_Literals (Get_Choice_Expression (Arr (Op1)),
                                         Get_Choice_Expression (Arr (Op2)))
           = Compare_Lt;
      end Lt;

      function Eq (Op1, Op2 : Natural) return Boolean is
      begin
         return Compare_String_Literals (Get_Choice_Expression (Arr (Op1)),
                                         Get_Choice_Expression (Arr (Op2)))
           = Compare_Eq;
      end Eq;

      procedure Swap (From : Natural; To : Natural)
      is
         Tmp : Iir;
      begin
         Tmp := Arr (To);
         Arr (To) := Arr (From);
         Arr (From) := Tmp;
      end Swap;

      package Str_Heap_Sort is new Heap_Sort (Lt => Lt, Swap => Swap);

      procedure Sem_Simple_Choice (Choice : Iir)
      is
         Expr : Iir;
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
            Error_Msg_Sem ("choice must be locally static expression", Expr);
            Has_Length_Error := True;
            return;
         end if;
         Expr := Eval_Expr (Expr);
         Set_Choice_Expression (Choice, Expr);
         if Get_Kind (Expr) = Iir_Kind_Overflow_Literal then
            Error_Msg_Sem
              ("bound error during evaluation of choice expression", Expr);
            Has_Length_Error := True;
         elsif Eval_Discrete_Type_Length
           (Get_String_Type_Bound_Type (Get_Type (Expr))) /= Sel_Length
         then
            Has_Length_Error := True;
            Error_Msg_Sem
              ("value not of the same length of the case expression", Expr);
            return;
         end if;
      end Sem_Simple_Choice;
   begin
      --  LRM93 8.8
      --  If the expression is of one-dimensional character array type, then
      --  the expression must be one of the following:
      --  FIXME: to complete.
      Sel_Type := Get_Type (Sel);
      if not Is_One_Dimensional_Array_Type (Sel_Type) then
         Error_Msg_Sem
           ("expression must be discrete or one-dimension array subtype", Sel);
         return;
      end if;
      if Get_Type_Staticness (Sel_Type) /= Locally then
         Error_Msg_Sem ("array type must be locally static", Sel);
         return;
      end if;
      Sel_Length := Eval_Discrete_Type_Length
        (Get_String_Type_Bound_Type (Sel_Type));
      Sel_El_Type := Get_Element_Subtype (Sel_Type);
      Sel_El_Length := Eval_Discrete_Type_Length (Sel_El_Type);

      Has_Others := False;
      Nbr_Choices := 0;
      El := Choice_Chain;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               raise Internal_Error;
            when Iir_Kind_Choice_By_Range =>
               Error_Msg_Sem
                 ("range choice are not allowed for non-discrete type", El);
            when Iir_Kind_Choice_By_Expression =>
               Nbr_Choices := Nbr_Choices + 1;
               Sem_Simple_Choice (El);
            when Iir_Kind_Choice_By_Others =>
               if Has_Others then
                  Error_Msg_Sem ("duplicate others choice", El);
               elsif Get_Chain (El) /= Null_Iir then
                  Error_Msg_Sem
                    ("choice others must be the last alternative", El);
               end if;
               Has_Others := True;
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
      --  static, wether a scalar type or an array type, then each value of the
      --  subtype must be represented once and only once in the set of choices
      --  of the case statement and no other value is allowed; [...]

      -- 1. Allocate Arr and fill it
      Arr := new Iir_Array (1 .. Nbr_Choices);
      Index := 0;
      El := Choice_Chain;
      while El /= Null_Iir loop
         if Get_Kind (El) = Iir_Kind_Choice_By_Expression then
            Index := Index + 1;
            Arr (Index) := El;
         end if;
         El := Get_Chain (El);
      end loop;

      -- 2. Sort Arr
      Str_Heap_Sort.Sort (Nbr_Choices);

      -- 3. Check for duplicate choices
      for I in 1 .. Nbr_Choices - 1 loop
         if Eq (I, I + 1) then
            Error_Msg_Sem ("duplicate choice with choice at " &
                             Disp_Location (Arr (I + 1)),
                           Arr (I));
            exit;
         end if;
      end loop;

      -- 4. Free Arr
      Free (Arr);

      --  Check for missing choice.
      --  Do not try to compute the expected number of choices as this can
      --  easily overflow.
      if not Has_Others then
         declare
            Nbr : Iir_Int64 := Iir_Int64 (Nbr_Choices);
         begin
            for I in 1 .. Sel_Length loop
               Nbr := Nbr / Sel_El_Length;
               if Nbr = 0 then
                  Error_Msg_Sem ("missing choice(s)", Choice_Chain);
                  exit;
               end if;
            end loop;
         end;
      end if;
   end Sem_String_Choices_Range;

   procedure Sem_Choices_Range
     (Choice_Chain : in out Iir;
      Sub_Type : Iir;
      Is_Sub_Range : Boolean;
      Is_Case_Stmt : Boolean;
      Loc : Location_Type;
      Low : out Iir;
      High : out Iir)
   is
      --  Number of positionnal choice.
      Nbr_Pos : Iir_Int64;

      --  Number of named choices.
      Nbr_Named : Natural;

      --  True if others choice is present.
      Has_Others : Boolean;

      Has_Error : Boolean;

      --  True if SUB_TYPE has bounds.
      Type_Has_Bounds : Boolean;

      Arr : Iir_Array_Acc;
      Index : Natural;
      Pos_Max : Iir_Int64;
      El : Iir;
      Prev_El : Iir;

      --  Staticness of the current choice.
      Choice_Staticness : Iir_Staticness;

      --  Staticness of all the choices.
      Staticness : Iir_Staticness;

      function Replace_By_Range_Choice (Name : Iir; Range_Type : Iir)
                                       return Boolean
      is
         N_Choice : Iir;
         Name1 : Iir;
      begin
         if Are_Types_Compatible (Range_Type, Sub_Type) = Not_Compatible then
            Not_Match (Name, Sub_Type);
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
            Expr := Sem_Discrete_Range_Expression (Expr, Sub_Type, True);
            if Expr = Null_Iir then
               return False;
            end if;
            Expr := Eval_Range_If_Static (Expr);
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
                          (Expr, Get_Base_Type (Sub_Type));
                  end case;
               when others =>
                  Expr := Sem_Expression_Ov (Expr, Get_Base_Type (Sub_Type));
            end case;
            if Expr = Null_Iir then
               return False;
            end if;
            Expr := Eval_Expr_If_Static (Expr);
            Set_Choice_Expression (El, Expr);
         end if;
         Set_Choice_Staticness (El, Get_Expr_Staticness (Expr));
         return True;
      end Sem_Simple_Choice;

      --  Get low limit of ASSOC.
      --  First, get the expression of the association, then the low limit.
      --  ASSOC may be either association_by_range (in this case the low limit
      --   is to be fetched), or association_by_expression (and the low limit
      --   is the expression).
      function Get_Low (Assoc : Iir) return Iir
      is
         Expr : Iir;
      begin
         case Get_Kind (Assoc) is
            when Iir_Kind_Choice_By_Expression =>
               return Get_Choice_Expression (Assoc);
            when Iir_Kind_Choice_By_Range =>
               Expr := Get_Choice_Range (Assoc);
               case Get_Kind (Expr) is
                  when Iir_Kind_Range_Expression =>
                     case Get_Direction (Expr) is
                        when Iir_To =>
                           return Get_Left_Limit (Expr);
                        when Iir_Downto =>
                           return Get_Right_Limit (Expr);
                     end case;
                  when others =>
                     return Expr;
               end case;
            when others =>
               Error_Kind ("get_low", Assoc);
         end case;
      end Get_Low;

      function Get_High (Assoc : Iir) return Iir
      is
         Expr : Iir;
      begin
         case Get_Kind (Assoc) is
            when Iir_Kind_Choice_By_Expression =>
               return Get_Choice_Expression (Assoc);
            when Iir_Kind_Choice_By_Range =>
               Expr := Get_Choice_Range (Assoc);
               case Get_Kind (Expr) is
                  when Iir_Kind_Range_Expression =>
                     case Get_Direction (Expr) is
                        when Iir_To =>
                           return Get_Right_Limit (Expr);
                        when Iir_Downto =>
                           return Get_Left_Limit (Expr);
                     end case;
                  when others =>
                     return Expr;
               end case;
            when others =>
               Error_Kind ("get_high", Assoc);
         end case;
      end Get_High;

      --  Compare two elements of ARR.
      --  Return true iff OP1 < OP2.
      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return
           Eval_Pos (Get_Low (Arr (Op1))) < Eval_Pos (Get_Low (Arr (Op2)));
      end Lt;

      --  Swap two elements of ARR.
      procedure Swap (From : Natural; To : Natural)
      is
         Tmp : Iir;
      begin
         Tmp := Arr (To);
         Arr (To) := Arr (From);
         Arr (From) := Tmp;
      end Swap;

      package Disc_Heap_Sort is new Heap_Sort (Lt => Lt, Swap => Swap);
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
      Staticness := Locally;
      El := Choice_Chain;
      Prev_El := Null_Iir;
      while El /= Null_Iir loop
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
                     Error_Msg_Sem ("choice is not locally static", El);
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
                  Error_Msg_Sem ("duplicate others choice", El);
               elsif Get_Chain (El) /= Null_Iir then
                  Error_Msg_Sem
                    ("choice others should be the last alternative", El);
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
           ("element associations must be all positional or all named", Loc);
         return;
      end if;

      --  For a positional aggregate.
      if Nbr_Pos > 0 then
         --  Check number of elements match, but only if it is possible.
         if Get_Type_Staticness (Sub_Type) /= Locally then
            return;
         end if;
         Pos_Max := Eval_Discrete_Type_Length (Sub_Type);
         if (not Has_Others and not Is_Sub_Range)
           and then Nbr_Pos < Pos_Max
         then
            Error_Msg_Sem ("not enough elements associated", Loc);
         elsif Nbr_Pos > Pos_Max then
            Error_Msg_Sem ("too many elements associated", Loc);
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
            --  LRM93 §7.3.2.2
            --  A named association of an array aggregate is allowed to have
            --  a choice that is not locally static, or likewise a choice that
            --  is a null range, only if the aggregate includes a single
            --  element association and the element association has a single
            --  choice.
            if Nbr_Named > 1 or Has_Others then
               Error_Msg_Sem ("not static choice exclude others choice", Loc);
            end if;
         end if;
         return;
      end if;

      --  Set TYPE_HAS_BOUNDS
      case Get_Kind (Sub_Type) is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            Type_Has_Bounds := True;
         when Iir_Kind_Integer_Type_Definition =>
            Type_Has_Bounds := False;
         when others =>
            Error_Kind ("sem_choice_range(3)", Sub_Type);
      end case;

      Arr := new Iir_Array (1 .. Nbr_Named);
      Index := 0;

      declare
         procedure Add_Choice (Choice : Iir; A_Type : Iir)
         is
            Ok : Boolean;
            Expr : Iir;
         begin
            Ok := True;
            if Type_Has_Bounds
              and then Get_Type_Staticness (A_Type) = Locally
            then
               if Get_Kind (Choice) = Iir_Kind_Choice_By_Range then
                  Expr := Get_Choice_Range (Choice);
                  if Get_Expr_Staticness (Expr) = Locally then
                     Ok := Eval_Is_Range_In_Bound (Expr, A_Type, True);
                  end if;
               else
                  Expr := Get_Choice_Expression (Choice);
                  if Get_Expr_Staticness (Expr) = Locally then
                     Ok := Eval_Is_In_Bound (Expr, A_Type);
                  end if;
               end if;
               if not Ok then
                  Error_Msg_Sem
                    (Disp_Node (Expr) & " out of index range", Choice);
               end if;
            end if;
            if Ok then
               Index := Index + 1;
               Arr (Index) := Choice;
            end if;
         end Add_Choice;
      begin
         --  Fill the array.
         El := Choice_Chain;
         while El /= Null_Iir loop
            case Get_Kind (El) is
               when Iir_Kind_Choice_By_None =>
                  --  Only named associations are considered.
                  raise Internal_Error;
               when Iir_Kind_Choice_By_Expression
                 | Iir_Kind_Choice_By_Range =>
                  Add_Choice (El, Sub_Type);
               when Iir_Kind_Choice_By_Others =>
                  null;
               when others =>
                  Error_Kind ("sem_choices_range(2)", El);
            end case;
            El := Get_Chain (El);
         end loop;
      end;

      --  Third:
      --  Sort the list
      Disc_Heap_Sort.Sort (Index);

      --  Set low and high bounds.
      if Index > 0 then
         Low := Get_Low (Arr (1));
         High := Get_High (Arr (Index));
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
                                    L, H : Iir_Int64;
                                    Loc : Location_Type)
         is
         begin
            if L = H then
               Error_Msg_Sem ("no choice for " & Disp_Discrete (Bt, L), Loc);
            else
               Error_Msg_Sem
                 ("no choices for " & Disp_Discrete (Bt, L)
                     & " to " & Disp_Discrete (Bt, H), Loc);
            end if;
         end Error_No_Choice;

         --  Lowest and highest bounds.
         Lb, Hb : Iir;
         Pos : Iir_Int64;
         Pos_Max : Iir_Int64;
         E_Pos : Iir_Int64;

         Bt : constant Iir := Get_Base_Type (Sub_Type);
      begin
         if not Is_Sub_Range
           and then Get_Type_Staticness (Sub_Type) = Locally
           and then Type_Has_Bounds
         then
            Get_Low_High_Limit (Get_Range_Constraint (Sub_Type), Lb, Hb);
         else
            Lb := Low;
            Hb := High;
         end if;
         if Lb = Null_Iir or else Hb = Null_Iir then
            --  Return now in case of error.
            Free (Arr);
            return;
         end if;
         --  Checks all values between POS and POS_MAX are handled.
         Pos := Eval_Pos (Lb);
         Pos_Max := Eval_Pos (Hb);
         if Pos > Pos_Max then
            --  Null range.
            Free (Arr);
            return;
         end if;
         for I in 1 .. Index loop
            E_Pos := Eval_Pos (Get_Low (Arr (I)));
            if E_Pos > Pos_Max then
               --  Choice out of bound, already handled.
               Error_No_Choice (Bt, Pos, Pos_Max, Get_Location (Arr (I)));
               --  Avoid other errors.
               Pos := Pos_Max + 1;
               exit;
            end if;
            if Pos < E_Pos and then not Has_Others then
               Error_No_Choice (Bt, Pos, E_Pos - 1, Get_Location (Arr (I)));
            elsif Pos > E_Pos then
               if Pos + 1 = E_Pos then
                  Error_Msg_Sem
                    ("duplicate choice for " & Disp_Discrete (Bt, Pos),
                     Arr (I));
               else
                  Error_Msg_Sem
                    ("duplicate choices for " & Disp_Discrete (Bt, E_Pos)
                     & " to " & Disp_Discrete (Bt, Pos), Arr (I));
               end if;
            end if;
            Pos := Eval_Pos (Get_High (Arr (I))) + 1;
         end loop;
         if Pos /= Pos_Max + 1 and then not Has_Others then
            Error_No_Choice (Bt, Pos, Pos_Max, Loc);
         end if;
      end;

      Free (Arr);
   end Sem_Choices_Range;

--    -- Find out the MIN and the MAX of an all named association choice list.
--    -- It also returns the number of elements associed (counting range).
--    procedure Sem_Find_Min_Max_Association_Choice_List
--      (List: Iir_Association_Choices_List;
--       Min: out Iir;
--       Max: out Iir;
--       Length: out natural)
--    is
--       Min_Res: Iir := null;
--       Max_Res: Iir := null;
--       procedure Update_With_Value (Val: Iir) is
--       begin
--          if Min_Res = null then
--             Min_Res := Val;
--             Max_Res := Val;
--          elsif Get_Value (Val) < Get_Value (Min_Res) then
--             Min_Res := Val;
--          elsif Get_Value (Val) > Get_Value (Max_Res) then
--             Max_Res := Val;
--          end if;
--       end Update_With_Value;

--       Number_Elements: Natural;

--       procedure Update (Choice: Iir) is
--          Left, Right: Iir;
--          Expr: Iir;
--       begin
--          case Get_Kind (Choice) is
--             when Iir_Kind_Choice_By_Expression =>
--                Update_With_Value (Get_Expression (Choice));
--                Number_Elements := Number_Elements + 1;
--             when Iir_Kind_Choice_By_Range =>
--                Expr := Get_Expression (Choice);
--                Left := Get_Left_Limit (Expr);
--                Right := Get_Right_Limit (Expr);
--                Update_With_Value (Left);
--                Update_With_Value (Right);
--                -- There can't be null range.
--                case Get_Direction (Expr) is
--                   when Iir_To =>
--                      Number_Elements := Number_Elements +
--                        Natural (Get_Value (Right) - Get_Value (Left) + 1);
--                   when Iir_Downto =>
--                      Number_Elements := Number_Elements +
--                        Natural (Get_Value (Left) - Get_Value (Right) + 1);
--                end case;
--             when others =>
--             Error_Kind ("sem_find_min_max_association_choice_list", Choice);
--          end case;
--       end Update;

--       El: Iir;
--       Sub_List: Iir_Association_Choices_List;
--       Sub_El: Iir;
--    begin
--       Number_Elements := 0;
--       for I in Natural loop
--          El := Get_Nth_Element (List, I);
--          exit when El = null;
--          case Get_Kind (El) is
--             when Iir_Kind_Choice_By_List =>
--                Sub_List := Get_Choice_List (El);
--                for J in Natural loop
--                   Sub_El := Get_Nth_Element (Sub_List, J);
--                   exit when Sub_El = null;
--                   Update (Sub_El);
--                end loop;
--             when others =>
--                Update (El);
--          end case;
--       end loop;
--       Min := Min_Res;
--       Max := Max_Res;
--       Length := Number_Elements;
--    end Sem_Find_Min_Max_Association_Choice_List;

   -- Perform semantisation on a (sub)aggregate AGGR, which is of type
   -- A_TYPE.
   -- return FALSE is case of failure
   function Sem_Record_Aggregate (Aggr: Iir_Aggregate; A_Type: Iir)
     return boolean
   is
      Base_Type : constant Iir := Get_Base_Type (A_Type);
      El_List : constant Iir_List := Get_Elements_Declaration_List (Base_Type);

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
            Error_Msg_Sem
              (Disp_Node (Matches (Pos)) & " was already associated", El);
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
            Error_Msg_Sem ("elements are not of the same type", El);
            Ok := False;
         end if;
      end Add_Match;

      --  Analyze a simple choice: extract the record element corresponding
      --  to the expression, and create a choice_by_name.
      --  FIXME: should mutate the node.
      function Sem_Simple_Choice (Ass : Iir) return Iir
      is
         N_El : Iir;
         Expr : Iir;
         Aggr_El : Iir_Element_Declaration;
      begin
         Expr := Get_Choice_Expression (Ass);
         if Get_Kind (Expr) /= Iir_Kind_Simple_Name then
            Error_Msg_Sem ("element association must be a simple name", Ass);
            Ok := False;
            return Ass;
         end if;
         Aggr_El := Find_Name_In_List
           (Get_Elements_Declaration_List (Base_Type), Get_Identifier (Expr));
         if Aggr_El = Null_Iir then
            Error_Msg_Sem
              ("record has no such element " & Disp_Node (Ass), Ass);
            Ok := False;
            return Ass;
         end if;

         N_El := Create_Iir (Iir_Kind_Choice_By_Name);
         Location_Copy (N_El, Ass);
         Set_Choice_Name (N_El, Aggr_El);
         Set_Associated_Expr (N_El, Get_Associated_Expr (Ass));
         Set_Associated_Chain (N_El, Get_Associated_Chain (Ass));
         Set_Chain (N_El, Get_Chain (Ass));
         Set_Same_Alternative_Flag (N_El, Get_Same_Alternative_Flag (Ass));

         Xref_Ref (Expr, Aggr_El);
         Free_Iir (Ass);
         Free_Iir (Expr);
         Add_Match (N_El, Aggr_El);
         return N_El;
      end Sem_Simple_Choice;

      Assoc_Chain : Iir;
      El, Prev_El : Iir;
      Expr: Iir;
      Has_Named : Boolean;
      Rec_El_Index : Natural;
      Value_Staticness : Iir_Staticness;
   begin
      Ok := True;
      Assoc_Chain := Get_Association_Choices_Chain (Aggr);
      Matches := (others => Null_Iir);
      Value_Staticness := Locally;

      El_Type := Null_Iir;
      Has_Named := False;
      Rec_El_Index := 0;
      Prev_El := Null_Iir;
      El := Assoc_Chain;
      while El /= Null_Iir loop
         Expr := Get_Associated_Expr (El);

         --  If there is an associated expression with the choice, then the
         --  choice is a new alternative, and has no expected type.
         if Expr /= Null_Iir then
            El_Type := Null_Iir;
         end if;

         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               if Has_Named then
                  Error_Msg_Sem ("positional association after named one", El);
                  Ok := False;
               elsif Rec_El_Index > Matches'Last then
                  Error_Msg_Sem ("too many elements", El);
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
                    ("choice others must be the last alternative", El);
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
                     Error_Msg_Sem ("no element for choice others", El);
                     Ok := False;
                  end if;
               end;
            when others =>
               Error_Kind ("sem_record_aggregate", El);
         end case;

         --  Analyze the expression associated.
         if Expr /= Null_Iir then
            if El_Type /= Null_Iir then
               Expr := Sem_Expression (Expr, El_Type);
               if Expr /= Null_Iir then
                  Set_Associated_Expr (El, Eval_Expr_If_Static (Expr));
                  Value_Staticness := Min (Value_Staticness,
                                           Get_Expr_Staticness (Expr));
               else
                  Ok := False;
               end if;
            else
               --  This case is not possible unless there is an error.
               if Ok then
                  raise Internal_Error;
               end if;
            end if;
         end if;

         Prev_El := El;
         El := Get_Chain (El);
      end loop;

      --  Check for missing associations.
      for I in Matches'Range loop
         if Matches (I) = Null_Iir then
            Error_Msg_Sem
              ("no value for " & Disp_Node (Get_Nth_Element (El_List, I)),
               Aggr);
            Ok := False;
         end if;
      end loop;
      Set_Value_Staticness (Aggr, Value_Staticness);
      Set_Expr_Staticness (Aggr, Min (Globally, Value_Staticness));
      return Ok;
   end Sem_Record_Aggregate;

   --  Information for each dimension of an aggregate.
   type Array_Aggr_Info is record
      --  False if one sub-aggregate has no others choices.
      --  If FALSE, the dimension is constrained.
      Has_Others : Boolean := True;

      --  True if one sub-aggregate is by named/by position.
      Has_Named : Boolean := False;
      Has_Positional : Boolean := False;

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

      --  True if there is an error.
      Error : Boolean := False;

      --  True if one element doesn't match the bounds.
      Has_Bound_Error : Boolean := False;
   end record;

   type Array_Aggr_Info_Arr is array (Natural range <>) of Array_Aggr_Info;

   --  Analyze an array aggregate AGGR of *base type* A_TYPE.
   --  The type of the array is computed into A_SUBTYPE.
   --  DIM is the dimension index in A_TYPE.
   --  Return FALSE in case of error.
   procedure Sem_Array_Aggregate_Type_1 (Aggr: Iir;
                                         A_Type: Iir;
                                         Infos : in out Array_Aggr_Info_Arr;
                                         Constrained : Boolean;
                                         Dim: Natural)
   is
      Index_List : constant Iir_List := Get_Index_Subtype_List (A_Type);

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
      Choice_Staticness : Iir_Staticness;

      Info : Array_Aggr_Info renames Infos (Dim);
   begin
      --  Sem choices.
      case Get_Kind (Aggr) is
         when Iir_Kind_Aggregate =>
            Assoc_Chain := Get_Association_Choices_Chain (Aggr);
            Sem_Choices_Range (Assoc_Chain, Index_Type, not Constrained, False,
                               Get_Location (Aggr), Low, High);
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
                     Choice_Staticness :=
                       Iirs.Min (Choice_Staticness,
                                 Get_Choice_Staticness (Choice));
                     --  FIXME: not true for range.
                     Len := Len + 1;
                  when Iir_Kind_Choice_By_None =>
                     Has_Positional_Choice := True;
                     Len := Len + 1;
                  when Iir_Kind_Choice_By_Others =>
                     if not Constrained then
                        Error_Msg_Sem ("'others' choice not allowed for an "
                                       & "aggregate in this context", Aggr);
                        Infos (Dim).Error := True;
                        return;
                     end if;
                     Has_Others := True;
                  when others =>
                     Error_Kind ("sem_array_aggregate_type", Choice);
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

         when others =>
            Error_Kind ("sem_array_aggregate_type_1", Aggr);
      end case;

      if Is_Positional = True then
         Info.Has_Positional := True;
      end if;
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
            Error_Msg_Sem ("non-locally static choice for an aggregate is "
                           & "allowed only if only choice", Aggr);
            Infos (Dim).Error := True;
            return;
         end if;
         Info.Has_Dynamic := True;
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
            if Get_Type_Staticness (Index_Type) = Locally then
               Info.Index_Subtype := Create_Range_Subtype_By_Length
                 (Index_Type, Iir_Int64 (Len), Get_Location (Aggr));
            end if;
         else
            --  Create an index subtype.
            case Get_Kind (Index_Type) is
               when Iir_Kind_Integer_Subtype_Definition =>
                  Info.Index_Subtype := Create_Iir (Get_Kind (Index_Type));
               when Iir_Kind_Enumeration_Type_Definition
                 | Iir_Kind_Enumeration_Subtype_Definition =>
                  Info.Index_Subtype :=
                    Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
               when others =>
                  Error_Kind ("sem_array_aggregate_type2", Index_Type);
            end case;
            Location_Copy (Info.Index_Subtype, Aggr);
            Set_Base_Type (Info.Index_Subtype, Get_Base_Type (Index_Type));
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
                  Set_Direction (Index_Subtype_Constraint,
                                 Get_Direction (Index_Constraint));
                  case Get_Direction (Index_Constraint) is
                     when Iir_To =>
                        Set_Left_Limit (Index_Subtype_Constraint, Low);
                        Set_Right_Limit (Index_Subtype_Constraint, High);
                     when Iir_Downto =>
                        Set_Left_Limit (Index_Subtype_Constraint, High);
                        Set_Right_Limit (Index_Subtype_Constraint, Low);
                  end case;
               end if;
            else
               --  Dynamic aggregate.
               declare
                  Expr : Iir;
                  Choice : Iir;
               begin
                  Choice := Assoc_Chain;
                  case Get_Kind (Choice) is
                     when Iir_Kind_Choice_By_Expression =>
                        Expr := Get_Choice_Expression (Choice);
                        Set_Direction (Index_Subtype_Constraint,
                                       Get_Direction (Index_Constraint));
                        Set_Left_Limit (Index_Subtype_Constraint, Expr);
                        Set_Right_Limit (Index_Subtype_Constraint, Expr);
                     when Iir_Kind_Choice_By_Range =>
                        Expr := Get_Choice_Range (Choice);
                        Set_Range_Constraint (Info.Index_Subtype, Expr);
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
               Error_Msg_Sem ("subaggregate bounds mismatch", Aggr);
            else
               if Eval_Discrete_Type_Length (Info.Index_Subtype)
                 /= Iir_Int64 (Len)
               then
                  Error_Msg_Sem ("subaggregate length mismatch", Aggr);
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
                  Error_Msg_Sem ("subagregate bounds mismatch", Aggr);
               end if;
            end;
         end if;
      end if;

      --  Analyze aggregate elements.
      if Dim = Get_Nbr_Elements (Index_List) then
         --  A type has been found for AGGR, analyze AGGR as if it was
         --  an aggregate with a subtype (and not a string).

         if Get_Kind (Aggr) /= Iir_Kind_Aggregate then
            return;
         end if;

         -- LRM93 7.3.2.2:
         --   the expression of each element association must be of the
         --   element type.
         declare
            Element_Type : constant Iir := Get_Element_Subtype (A_Type);
            El : Iir;
            Expr : Iir;
            Value_Staticness : Iir_Staticness;
            Expr_Staticness : Iir_Staticness;
         begin
            El := Assoc_Chain;
            Value_Staticness := Locally;
            while El /= Null_Iir loop
               Expr := Get_Associated_Expr (El);
               if Expr /= Null_Iir then
                  Expr := Sem_Expression (Expr, Element_Type);
                  if Expr /= Null_Iir then
                     Expr_Staticness := Get_Expr_Staticness (Expr);
                     Set_Expr_Staticness (Aggr,
                                          Min (Get_Expr_Staticness (Aggr),
                                               Expr_Staticness));
                     Expr := Eval_Expr_If_Static (Expr);
                     Set_Associated_Expr (El, Expr);

                     if not Eval_Is_In_Bound (Expr, Element_Type)
                     then
                        Info.Has_Bound_Error := True;
                        Warning_Msg_Sem ("element is out of the bounds", Expr);
                     end if;

                     --  FIXME: handle name/others in translate.
                     --  if Get_Kind (Expr) = Iir_Kind_Aggregate then
                     --     Expr_Staticness := Get_Value_Staticness (Expr);
                     --  end if;
                     Value_Staticness := Min (Value_Staticness,
                                              Expr_Staticness);
                  else
                     Info.Error := True;
                  end if;
               end if;
               El := Get_Chain (El);
            end loop;
            Set_Value_Staticness (Aggr, Value_Staticness);
         end;
      else
         declare
            Assoc : Iir;
            Value_Staticness : Iir_Staticness;
         begin
            Assoc := Null_Iir;
            Choice := Assoc_Chain;
            Value_Staticness := Locally;
            while Choice /= Null_Iir loop
               if Get_Associated_Expr (Choice) /= Null_Iir then
                  Assoc := Get_Associated_Expr (Choice);
               end if;
               case Get_Kind (Assoc) is
                  when Iir_Kind_Aggregate =>
                     Sem_Array_Aggregate_Type_1
                       (Assoc, A_Type, Infos, Constrained, Dim + 1);
                     Value_Staticness := Min (Value_Staticness,
                                              Get_Value_Staticness (Assoc));
                  when Iir_Kind_String_Literal8 =>
                     if Dim + 1 = Get_Nbr_Elements (Index_List) then
                        Sem_Array_Aggregate_Type_1
                          (Assoc, A_Type, Infos, Constrained, Dim + 1);
                     else
                        Error_Msg_Sem
                          ("string literal not allowed here", Assoc);
                        Infos (Dim + 1).Error := True;
                     end if;
                  when others =>
                     Error_Msg_Sem ("sub-aggregate expected", Assoc);
                     Infos (Dim + 1).Error := True;
               end case;
               Choice := Get_Chain (Choice);
            end loop;
            Set_Value_Staticness (Aggr, Value_Staticness);
         end;
      end if;
   end Sem_Array_Aggregate_Type_1;

   --  Analyze an array aggregate whose type is AGGR_TYPE.
   --  If CONSTRAINED is true, then the aggregate appears in one of the
   --  context and can have an 'others' choice.
   --  If CONSTRAINED is false, the aggregate can not have an 'others' choice.
   --  Create a subtype for this aggregate.
   --  Return NULL_IIR in case of error, or AGGR if not.
   function Sem_Array_Aggregate_Type
     (Aggr : Iir; Aggr_Type : Iir; Constrained : Boolean) return Iir
   is
      A_Subtype: Iir;
      Base_Type : Iir;
      Index_List : constant Iir_List := Get_Index_Subtype_List (Aggr_Type);
      Nbr_Dim : constant Natural := Get_Nbr_Elements (Index_List);
      Infos : Array_Aggr_Info_Arr (1 .. Nbr_Dim);
      Aggr_Constrained : Boolean;
      Info, Prev_Info : Iir_Aggregate_Info;
   begin
      --  Analyze the aggregate.
      Sem_Array_Aggregate_Type_1 (Aggr, Aggr_Type, Infos, Constrained, 1);

      Aggr_Constrained := True;
      for I in Infos'Range loop
         --  Return now in case of error.
         if Infos (I).Error then
            return Null_Iir;
         end if;
         if Infos (I).Index_Subtype = Null_Iir then
            Aggr_Constrained := False;
         end if;
      end loop;
      Base_Type := Get_Base_Type (Aggr_Type);

      --  FIXME: should reuse AGGR_TYPE iff AGGR_TYPE is fully constrained
      --  and statically match the subtype of the aggregate.
      if Aggr_Constrained then
         A_Subtype := Create_Array_Subtype (Base_Type, Get_Location (Aggr));
         for I in Infos'Range loop
            Append_Element (Get_Index_Subtype_List (A_Subtype),
                            Infos (I).Index_Subtype);
            Set_Type_Staticness
              (A_Subtype,
               Iirs.Min (Get_Type_Staticness (A_Subtype),
                         Get_Type_Staticness (Infos (I).Index_Subtype)));
         end loop;
         Set_Index_Constraint_Flag (A_Subtype, True);
         Set_Constraint_State (A_Subtype, Fully_Constrained);
         Set_Type (Aggr, A_Subtype);
         Set_Literal_Subtype (Aggr, A_Subtype);
      else
         --  Free unused indexes subtype.
         for I in Infos'Range loop
            declare
               St : constant Iir := Infos (I).Index_Subtype;
            begin
               if St /= Null_Iir then
                  Free_Iir (Get_Range_Constraint (St));
                  Free_Iir (St);
               end if;
            end;
         end loop;
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
   end Sem_Array_Aggregate_Type;

   --  Semantize aggregate EXPR whose type is expected to be A_TYPE.
   --  A_TYPE cannot be null_iir (this case is handled in sem_expression_ov)
   function Sem_Aggregate (Expr: Iir_Aggregate; A_Type: Iir)
                          return Iir_Aggregate is
   begin
      pragma Assert (A_Type /= Null_Iir);

      --  An aggregate is at most globally static.
      Set_Expr_Staticness (Expr, Globally);

      Set_Type (Expr, A_Type); -- FIXME: should free old type
      case Get_Kind (A_Type) is
         when Iir_Kind_Array_Subtype_Definition =>
            return Sem_Array_Aggregate_Type
              (Expr, A_Type, Get_Index_Constraint_Flag (A_Type));
         when Iir_Kind_Array_Type_Definition =>
            return Sem_Array_Aggregate_Type (Expr, A_Type, False);
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            if not Sem_Record_Aggregate (Expr, A_Type) then
               return Null_Iir;
            end if;
            return Expr;
         when others =>
            Error_Msg_Sem ("type " & Disp_Node (A_Type) & " is not composite",
                           Expr);
            return Null_Iir;
      end case;
   end Sem_Aggregate;

   -- Transform LIT into a physical_literal.
   -- LIT can be either a not semantized physical literal or
   --  a simple name that is a physical unit.  In the later case, a physical
   --  literal is created.
   function Sem_Physical_Literal (Lit: Iir) return Iir
   is
      Unit_Name : Iir;
      Unit_Type : Iir;
      Res: Iir;
   begin
      case Get_Kind (Lit) is
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            Unit_Name := Get_Unit_Name (Lit);
            Res := Lit;
         when Iir_Kind_Unit_Declaration =>
            Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
            Location_Copy (Res, Lit);
            Set_Value (Res, 1);
            Unit_Name := Null_Iir;
            raise Program_Error;
         when Iir_Kinds_Denoting_Name =>
            Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
            Location_Copy (Res, Lit);
            Set_Value (Res, 1);
            Unit_Name := Lit;
         when others =>
            Error_Kind ("sem_physical_literal", Lit);
      end case;
      Unit_Name := Sem_Denoting_Name (Unit_Name);
      if Get_Kind (Get_Named_Entity (Unit_Name)) /= Iir_Kind_Unit_Declaration
      then
         Error_Class_Match (Unit_Name, "unit");
         Set_Named_Entity (Unit_Name, Create_Error_Name (Unit_Name));
      end if;
      Set_Unit_Name (Res, Unit_Name);
      Unit_Type := Get_Type (Unit_Name);
      Set_Type (Res, Unit_Type);

      -- LRM93 7.4.2
      -- 1. a literal of type TIME.
      --
      -- LRM93 7.4.1
      -- 1. a literal of any type other than type TIME;
      Set_Expr_Staticness (Res, Get_Expr_Staticness (Unit_Name));
      --Eval_Check_Constraints (Res);
      return Res;
   end Sem_Physical_Literal;

   --  Semantize an allocator by expression or an allocator by subtype.
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
                    ("allocator of unconstrained " &
                       Disp_Node (Arg) & " is not allowed", Expr);
               end if;
               --  LRM93 7.3.6
               --  A subtype indication that is part of an allocator must
               --  not include a resolution function.
               if Is_Anonymous_Type_Definition (Arg)
                 and then Get_Resolution_Indication (Arg) /= Null_Iir
               then
                  Error_Msg_Sem ("subtype indication must not include"
                                   & " a resolution function", Expr);
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
               if Get_Kind (A_Type) /= Iir_Kind_Error then
                  Error_Msg_Sem ("expected type is not an access type", Expr);
               end if;
            else
               Not_Match (Expr, A_Type);
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
      Set_Type (Expr, N_Type);
      if A_Type /= Null_Iir
        and then Are_Types_Compatible (A_Type, N_Type) = Not_Compatible
      then
         Not_Match (Expr, A_Type);
         return Null_Iir;
      end if;
      Res := Sem_Expression (Get_Expression (Expr), N_Type);
      if Res = Null_Iir then
         return Null_Iir;
      end if;
      Check_Read (Res);
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
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Variable_Declaration
              | Iir_Kind_Attribute_Value
              | Iir_Kind_Iterator_Declaration
              | Iir_Kind_Guard_Signal_Declaration =>
               return;
            when Iir_Kinds_Quantity_Declaration =>
               return;
            when Iir_Kinds_External_Name =>
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
                  Error_Msg_Sem (Disp_Node (Obj) & " cannot be read", Expr);
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
              | Iir_Kinds_Signal_Value_Attribute =>
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
            when Iir_Kind_Error =>
               return;
            when others =>
               Error_Kind ("check_read", Obj);
         end case;
      end loop;
   end Check_Read;

   procedure Check_Update (Expr : Iir)
   is
      pragma Unreferenced (Expr);
   begin
      null;
   end Check_Update;

   --  Emit an error if the constant EXPR is deferred and cannot be used in
   --  the current context.
   procedure Check_Constant_Restriction (Expr : Iir; Loc : Iir)
   is
      Lib : Iir;
      Cur_Lib : Iir;
   begin
      --  LRM93 §2.6
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
      if Get_Kind (Lib) = Iir_Kind_Design_Unit then
         Lib := Get_Library_Unit (Lib);
         --  FIXME: the parent of the constant is the library unit or
         --  the design unit ?
         raise Internal_Error;
      end if;
      Cur_Lib := Get_Library_Unit (Sem.Get_Current_Design_Unit);
      if (Get_Kind (Cur_Lib) = Iir_Kind_Package_Declaration
          and then Lib = Cur_Lib)
        or else (Get_Kind (Cur_Lib) = Iir_Kind_Package_Body
                 and then Get_Package (Cur_Lib) = Lib)
      then
         Error_Msg_Sem ("invalid use of a deferred constant", Loc);
      end if;
   end Check_Constant_Restriction;

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
--     -- Avoid to run sem_expression_ov when a node was already semantized
--     -- except to resolve overload.
--     if Get_Type (Expr) /= Null_Iir then
--        --  EXPR was already semantized.
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
               if Get_Kind (E) = Iir_Kind_Constant_Declaration
                 and then not Deferred_Constant_Allowed
               then
                  Check_Constant_Restriction (E, Expr);
               end if;
               E := Name_To_Expression (Expr, A_Type);
               return E;
            end;

         when Iir_Kinds_External_Name =>
            Sem_External_Name (Expr);
            return Expr;

         when Iir_Kinds_Monadic_Operator =>
            return Sem_Operator (Expr, A_Type, 1);

         when Iir_Kinds_Dyadic_Operator =>
            return Sem_Operator (Expr, A_Type, 2);

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
               Not_Match (Expr, A_Type);
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
               Not_Match (Expr, A_Type);
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
               Not_Match (Expr, A_Type);
               return Null_Iir;
            end if;

         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Unit_Declaration =>
            declare
               Res: Iir;
            begin
               Res := Sem_Physical_Literal (Expr);
               if Res = Null_Iir then
                  return Null_Iir;
               end if;
               if A_Type /= Null_Iir and then Get_Type (Res) /= A_Type then
                  Not_Match (Res, A_Type);
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
               Not_Match (Expr, A_Type);
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
               Error_Msg_Sem ("null literal can only be access type", Expr);
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
               return Sem_Aggregate (Expr, A_Type);
            end if;

         when Iir_Kind_Parenthesis_Expression =>
            declare
               Sub_Expr : Iir;
            begin
               Sub_Expr := Get_Expression (Expr);
               Sub_Expr := Sem_Expression_Ov (Sub_Expr, A_Type1);
               if Sub_Expr = Null_Iir then
                  return Null_Iir;
               end if;
               Set_Expression (Expr, Sub_Expr);
               Set_Type (Expr, Get_Type (Sub_Expr));
               Set_Expr_Staticness (Expr, Get_Expr_Staticness (Sub_Expr));
               return Expr;
            end;

         when Iir_Kind_Qualified_Expression =>
            return Sem_Qualified_Expression (Expr, A_Type);

         when Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype =>
            return Sem_Allocator (Expr, A_Type);

         when Iir_Kind_Procedure_Declaration =>
            Error_Msg_Sem
              (Disp_Node (Expr) & " cannot be used as an expression", Expr);
            return Null_Iir;

         when others =>
            Error_Kind ("sem_expression_ov", Expr);
            return Null_Iir;
      end case;
   end Sem_Expression_Ov;

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

      -- Can't try to run sem_expression_ov when a node was already semantized
      Expr_Type := Get_Type (Expr);
      if Expr_Type /= Null_Iir and then not Is_Overload_List (Expr_Type) then
         --  Checks types.
         --  This is necessary when the first call to sem_expression was done
         --  with A_TYPE set to NULL_IIR and results in setting the type of
         --  EXPR.
         if A_Type /= Null_Iir
           and then Are_Types_Compatible (Expr_Type, A_Type) = Not_Compatible
         then
            Not_Match (Expr, A_Type);
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
            Res := Sem_Aggregate (Expr, A_Type);
         when Iir_Kind_String_Literal8 =>
            if A_Type = Null_Iir then
               Res := Sem_Expression_Ov (Expr, Null_Iir);
            else
               if not Is_String_Literal_Type (A_Type, Expr) then
                  Not_Match (Expr, A_Type);
                  return Null_Iir;
               end if;
               Set_Type (Expr, A_Type);
               Sem_String_Literal (Expr);
               return Expr;
            end if;
         when others =>
            Res := Sem_Expression_Ov (Expr, A_Type1);
      end case;

      if Res /= Null_Iir and then Is_Overloaded (Res) then
         --  FIXME: clarify between overload and not determinable from the
         --  context.
         Error_Overload (Expr);
         if Get_Type (Res) /= Null_Iir then
            Disp_Overload_List (Get_Overload_List (Get_Type (Res)), Expr);
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
            Res_Type : Iir;
            Atype : Iir;
         begin
            Res_Type := Null_Iir;
            for I in Natural loop
               Atype := Get_Nth_Element (List, I);
               exit when Atype = Null_Iir;
               if Is_Aggregate_Type (Atype) then
                  Add_Result (Res_Type, Atype);
               end if;
            end loop;

            if Res_Type = Null_Iir then
               Error_Overload (Expr);
               return Null_Iir;
            elsif Is_Overload_List (Res_Type) then
               Error_Overload (Expr);
               Disp_Overload_List (Get_Overload_List (Res_Type), Expr);
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

   function Sem_Expression_Universal (Expr : Iir) return Iir
   is
      Expr1 : Iir;
      Expr_Type : Iir;
      El : Iir;
      Res : Iir;
      List : Iir_List;
   begin
      Expr1 := Sem_Expression_Ov (Expr, Null_Iir);
      if Expr1 = Null_Iir then
         return Null_Iir;
      end if;
      Expr_Type := Get_Type (Expr1);
      if Expr_Type = Null_Iir then
         --  FIXME: improve message
         Error_Msg_Sem ("bad expression for a scalar", Expr);
         return Null_Iir;
      end if;
      if not Is_Overload_List (Expr_Type) then
         return Expr1;
      end if;

      List := Get_Overload_List (Expr_Type);
      Res := Null_Iir;
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         if El = Universal_Integer_Type_Definition
           or El = Convertible_Integer_Type_Definition
           or El = Universal_Real_Type_Definition
           or El = Convertible_Real_Type_Definition
         then
            if Res = Null_Iir then
               Res := El;
            else
               Error_Overload (Expr1);
               Disp_Overload_List (List, Expr1);
               return Null_Iir;
            end if;
         end if;
      end loop;
      if Res = Null_Iir then
         Error_Overload (Expr1);
         Disp_Overload_List (List, Expr1);
         return Null_Iir;
      end if;
      return Sem_Expression_Ov (Expr1, Res);
   end Sem_Expression_Universal;

   function Sem_Case_Expression (Expr : Iir) return Iir
   is
      Expr1 : Iir;
      Expr_Type : Iir;
      El : Iir;
      Res : Iir;
      List : Iir_List;
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
           ("cannot determine the type of choice expression", Expr);
         if Get_Kind (Expr1) = Iir_Kind_Aggregate then
            Error_Msg_Sem
              ("(use a qualified expression of the form T'(xxx).)", Expr);
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
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         if Get_Kind (El) in Iir_Kinds_Discrete_Type_Definition
           or else Is_One_Dimensional_Array_Type (El)
         then
            if Res = Null_Iir then
               Res := El;
            else
               Error_Overload (Expr1);
               Disp_Overload_List (List, Expr1);
               return Null_Iir;
            end if;
         end if;
      end loop;
      if Res = Null_Iir then
         Error_Overload (Expr1);
         Disp_Overload_List (List, Expr1);
         return Null_Iir;
      end if;
      return Sem_Expression_Ov (Expr1, Get_Base_Type (Res));
   end Sem_Case_Expression;

   function Sem_Condition (Cond : Iir) return Iir
   is
      Res : Iir;
      Op : Iir;
   begin
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

         Res := Sem_Expression_Ov (Cond, Null_Iir);

         if Res = Null_Iir then
            --  Error occured.
            return Res;
         end if;

         if not Is_Overloaded (Res) then
            --  Only one result.  Operator "??" is not applied if the result
            --  is of type boolean.
            if Are_Types_Compatible (Get_Type (Res), Boolean_Type_Definition)
              /= Not_Compatible
            then
               Check_Read (Res);
               return Res;
            end if;
         else
            --  Many interpretations.
            declare
               El : Iir;
               Res_List : constant Iir_List :=
                 Get_Overload_List (Get_Type (Res));
               Nbr_Booleans : Natural;
            begin
               Nbr_Booleans := 0;

               --  Extract boolean interpretations.
               for I in Natural loop
                  El := Get_Nth_Element (Res_List, I);
                  exit when El = Null_Iir;
                  if Are_Types_Compatible (El, Boolean_Type_Definition)
                    /= Not_Compatible
                  then
                     Nbr_Booleans := Nbr_Booleans + 1;
                  end if;
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

         Op := Create_Iir (Iir_Kind_Condition_Operator);
         Location_Copy (Op, Res);
         Set_Operand (Op, Res);

         Res := Sem_Operator (Op, Boolean_Type_Definition, 1);
         Check_Read (Res);
         return Res;
      end if;
   end Sem_Condition;

end Sem_Expr;
