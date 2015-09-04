--  Common operations on nodes.
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
with Scanner; use Scanner;
with Tokens; use Tokens;
with Errorout; use Errorout;
with Name_Table;
with Str_Table;
with Std_Names; use Std_Names;
with Std_Package;
with Flags; use Flags;
with PSL.Nodes;
with Sem_Inst;

package body Iirs_Utils is
   -- Transform the current token into an iir literal.
   -- The current token must be either a character or an identifier.
   function Current_Text return Iir is
      Res: Iir;
   begin
      case Current_Token is
         when Tok_Identifier =>
            Res := Create_Iir (Iir_Kind_Simple_Name);
         when Tok_Character =>
            Res := Create_Iir (Iir_Kind_Character_Literal);
         when others =>
            raise Internal_Error;
      end case;
      Set_Identifier (Res, Current_Identifier);
      Invalidate_Current_Identifier;
      Invalidate_Current_Token;
      Set_Location (Res, Get_Token_Location);
      return Res;
   end Current_Text;

   function Is_Error (N : Iir) return Boolean is
   begin
      return Get_Kind (N) = Iir_Kind_Error;
   end Is_Error;

   function Is_Overflow_Literal (N : Iir) return Boolean is
   begin
      return Get_Kind (N) = Iir_Kind_Overflow_Literal;
   end Is_Overflow_Literal;

   function Get_Operator_Name (Op : Iir) return Name_Id is
   begin
      case Get_Kind (Op) is
         when Iir_Kind_And_Operator
           | Iir_Kind_Reduction_And_Operator =>
            return Name_And;
         when Iir_Kind_Or_Operator
           | Iir_Kind_Reduction_Or_Operator =>
            return Name_Or;
         when Iir_Kind_Nand_Operator
           | Iir_Kind_Reduction_Nand_Operator =>
            return Name_Nand;
         when Iir_Kind_Nor_Operator
           | Iir_Kind_Reduction_Nor_Operator =>
            return Name_Nor;
         when Iir_Kind_Xor_Operator
           | Iir_Kind_Reduction_Xor_Operator =>
            return Name_Xor;
         when Iir_Kind_Xnor_Operator
           | Iir_Kind_Reduction_Xnor_Operator =>
            return Name_Xnor;

         when Iir_Kind_Equality_Operator =>
            return Name_Op_Equality;
         when Iir_Kind_Inequality_Operator =>
            return Name_Op_Inequality;
         when Iir_Kind_Less_Than_Operator =>
            return Name_Op_Less;
         when Iir_Kind_Less_Than_Or_Equal_Operator =>
            return Name_Op_Less_Equal;
         when Iir_Kind_Greater_Than_Operator =>
            return Name_Op_Greater;
         when Iir_Kind_Greater_Than_Or_Equal_Operator =>
            return Name_Op_Greater_Equal;

         when Iir_Kind_Match_Equality_Operator =>
            return Name_Op_Match_Equality;
         when Iir_Kind_Match_Inequality_Operator =>
            return Name_Op_Match_Inequality;
         when Iir_Kind_Match_Less_Than_Operator =>
            return Name_Op_Match_Less;
         when Iir_Kind_Match_Less_Than_Or_Equal_Operator =>
            return Name_Op_Match_Less_Equal;
         when Iir_Kind_Match_Greater_Than_Operator =>
            return Name_Op_Match_Greater;
         when Iir_Kind_Match_Greater_Than_Or_Equal_Operator =>
            return Name_Op_Match_Greater_Equal;

         when Iir_Kind_Sll_Operator =>
            return Name_Sll;
         when Iir_Kind_Sla_Operator =>
            return Name_Sla;
         when Iir_Kind_Srl_Operator =>
            return Name_Srl;
         when Iir_Kind_Sra_Operator =>
            return Name_Sra;
         when Iir_Kind_Rol_Operator =>
            return Name_Rol;
         when Iir_Kind_Ror_Operator =>
            return Name_Ror;
         when Iir_Kind_Addition_Operator =>
            return Name_Op_Plus;
         when Iir_Kind_Substraction_Operator =>
            return Name_Op_Minus;
         when Iir_Kind_Concatenation_Operator =>
            return Name_Op_Concatenation;
         when Iir_Kind_Multiplication_Operator =>
            return Name_Op_Mul;
         when Iir_Kind_Division_Operator =>
            return Name_Op_Div;
         when Iir_Kind_Modulus_Operator =>
            return Name_Mod;
         when Iir_Kind_Remainder_Operator =>
            return Name_Rem;
         when Iir_Kind_Exponentiation_Operator =>
            return Name_Op_Exp;
         when Iir_Kind_Not_Operator =>
            return Name_Not;
         when Iir_Kind_Negation_Operator =>
            return Name_Op_Minus;
         when Iir_Kind_Identity_Operator =>
            return Name_Op_Plus;
         when Iir_Kind_Absolute_Operator =>
            return Name_Abs;
         when Iir_Kind_Condition_Operator =>
            return Name_Op_Condition;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Operator_Name;

   function Get_Longuest_Static_Prefix (Expr: Iir) return Iir is
      Adecl: Iir;
   begin
      Adecl := Expr;
      loop
         case Get_Kind (Adecl) is
            when Iir_Kind_Variable_Declaration
              | Iir_Kind_Interface_Variable_Declaration =>
               return Adecl;
            when Iir_Kind_Constant_Declaration
              | Iir_Kind_Interface_Constant_Declaration =>
               return Adecl;
            when Iir_Kind_Signal_Declaration
              | Iir_Kind_Guard_Signal_Declaration
              | Iir_Kind_Interface_Signal_Declaration =>
               return Adecl;
            when Iir_Kind_Object_Alias_Declaration =>
               --  LRM 4.3.3.1 Object Aliases
               --  2.  The name must be a static name [...]
               return Adecl;
            when Iir_Kind_Slice_Name
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Selected_Element =>
               if Get_Name_Staticness (Adecl) >= Globally then
                  return Adecl;
               else
                  Adecl := Get_Prefix (Adecl);
               end if;
            when Iir_Kind_Simple_Name
              | Iir_Kind_Selected_Name =>
               Adecl := Get_Named_Entity (Adecl);
            when Iir_Kind_Type_Conversion =>
               return Null_Iir;
            when others =>
               Error_Kind ("get_longuest_static_prefix", Adecl);
         end case;
      end loop;
   end Get_Longuest_Static_Prefix;

   function Get_Object_Prefix (Name: Iir; With_Alias : Boolean := True)
                              return Iir
   is
      Adecl : Iir;
   begin
      Adecl := Name;
      loop
         case Get_Kind (Adecl) is
            when Iir_Kind_Variable_Declaration
              | Iir_Kind_Interface_Variable_Declaration
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Signal_Declaration
              | Iir_Kind_Guard_Signal_Declaration
              | Iir_Kind_Interface_Signal_Declaration
              | Iir_Kind_File_Declaration
              | Iir_Kind_Interface_File_Declaration
              | Iir_Kind_Iterator_Declaration =>
               return Adecl;
            when Iir_Kind_Object_Alias_Declaration =>
               if With_Alias then
                  Adecl := Get_Name (Adecl);
               else
                  return Adecl;
               end if;
            when Iir_Kind_Indexed_Name
              | Iir_Kind_Slice_Name
              | Iir_Kind_Selected_Element
              | Iir_Kind_Selected_By_All_Name =>
               Adecl := Get_Base_Name (Adecl);
            when Iir_Kinds_Literal
              | Iir_Kind_Enumeration_Literal
              | Iir_Kinds_Monadic_Operator
              | Iir_Kinds_Dyadic_Operator
              | Iir_Kind_Function_Call
              | Iir_Kind_Qualified_Expression
              | Iir_Kind_Type_Conversion
              | Iir_Kind_Allocator_By_Expression
              | Iir_Kind_Allocator_By_Subtype
              | Iir_Kinds_Attribute
              | Iir_Kind_Attribute_Value
              | Iir_Kind_Aggregate
              | Iir_Kind_Simple_Aggregate
              | Iir_Kind_Dereference
              | Iir_Kind_Implicit_Dereference
              | Iir_Kind_Unit_Declaration
              | Iir_Kinds_Concurrent_Statement =>
               return Adecl;
            when Iir_Kind_Simple_Name
              | Iir_Kind_Selected_Name =>
               Adecl := Get_Named_Entity (Adecl);
            when Iir_Kind_Attribute_Name =>
               return Get_Named_Entity (Adecl);
            when others =>
               Error_Kind ("get_object_prefix", Adecl);
         end case;
      end loop;
   end Get_Object_Prefix;

   function Is_Object_Name (Name : Iir) return Boolean
   is
      Obj : constant Iir := Name_To_Object (Name);
   begin
      return Obj /= Null_Iir;
   end Is_Object_Name;

   function Name_To_Object (Name : Iir) return Iir is
   begin
      --  LRM08 6.4 Objects
      --  An object is a named entity that contains (has) a value of a type.
      --  An object is obe of the following:
      case Get_Kind (Name) is
         --  An object declared by an object declaration (see 6.4.2)
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Constant_Declaration =>
            return Name;

         --  A loop of generate parameter.
         when Iir_Kind_Iterator_Declaration =>
            return Name;

         --  A formal parameter of a subprogram
         --  A formal port
         --  A formal generic constant
         --  A local port
         --  A local generic constant
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            return Name;

         --  An implicit signak GUARD defined by the guard expression of a
         --   block statement
         when Iir_Kind_Guard_Signal_Declaration =>
            return Name;

         --  In addition, the following are objects [ but are not named
         --   entities]:
         --  An implicit signal defined by any of the predefined attributes
         --  'DELAYED, 'STABLE, 'QUIET, and 'TRANSACTION
         when Iir_Kinds_Signal_Attribute =>
            return Name;

         --  An element or a slice of another object
         when Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element =>
            return Name;

         --  An object designated by a value of an access type
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            return Name;

         --  LRM08 6.6 Alias declarations
         --  An object alias is an alias whose alias designatore denotes an
         --  object.
         when Iir_Kind_Object_Alias_Declaration =>
            return Name;

         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            --  LRM08 8 Names
            --  Names can denote declared entities [...]
            --  GHDL: in particular, names can denote objects.
            return Name_To_Object (Get_Named_Entity (Name));

         when others =>
            return Null_Iir;
      end case;
   end Name_To_Object;

   function Name_To_Value (Name : Iir) return Iir is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Attribute_Value
           | Iir_Kind_Function_Call
           | Iir_Kinds_Expression_Attribute =>
            return Name;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Name_To_Value (Get_Named_Entity (Name));
         when others =>
            return Name_To_Object (Name);
      end case;
   end Name_To_Value;

   --  Return TRUE if EXPR is a signal name.
   function Is_Signal_Name (Expr : Iir) return Boolean
   is
      Obj : Iir;
   begin
      Obj := Name_To_Object (Expr);
      if Obj /= Null_Iir then
         return Is_Signal_Object (Obj);
      else
         return False;
      end if;
   end Is_Signal_Name;

   function Is_Signal_Object (Name : Iir) return Boolean
   is
      Adecl: Iir;
   begin
      Adecl := Get_Object_Prefix (Name, True);
      case Get_Kind (Adecl) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kinds_Signal_Attribute =>
            return True;
         when Iir_Kind_Object_Alias_Declaration =>
            --  Must have been handled by Get_Object_Prefix.
            raise Internal_Error;
         when others =>
            return False;
      end case;
   end Is_Signal_Object;

   function Get_Association_Interface (Assoc : Iir) return Iir
   is
      Formal : Iir;
   begin
      Formal := Get_Formal (Assoc);
      loop
         case Get_Kind (Formal) is
            when Iir_Kind_Simple_Name =>
               return Get_Named_Entity (Formal);
            when Iir_Kinds_Interface_Object_Declaration =>
               return Formal;
            when Iir_Kind_Slice_Name
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Selected_Element =>
               Formal := Get_Prefix (Formal);
            when others =>
               Error_Kind ("get_association_interface", Formal);
         end case;
      end loop;
   end Get_Association_Interface;

   function Find_Name_In_List (List: Iir_List; Lit: Name_Id) return Iir is
      El: Iir;
      Ident: Name_Id;
   begin
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         Ident := Get_Identifier (El);
         if Ident = Lit then
            return El;
         end if;
      end loop;
      return Null_Iir;
   end Find_Name_In_List;

   function Find_Name_In_Chain (Chain: Iir; Lit: Name_Id) return Iir
   is
      El: Iir := Chain;
   begin
      while El /= Null_Iir loop
         if Get_Identifier (El) = Lit then
            return El;
         end if;
         El := Get_Chain (El);
      end loop;
      return Null_Iir;
   end Find_Name_In_Chain;

   function Is_In_Chain (Chain : Iir; El : Iir) return Boolean
   is
      Chain_El : Iir;
   begin
      Chain_El := Chain;
      while Chain_El /= Null_Iir loop
         if Chain_El = El then
            return True;
         end if;
         Chain_El := Get_Chain (Chain_El);
      end loop;
      return False;
   end Is_In_Chain;

   procedure Add_Dependence (Target: Iir_Design_Unit; Unit: Iir) is
   begin
      --  Do not add self-dependency
      if Unit = Target then
         return;
      end if;

      case Get_Kind (Unit) is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Entity_Aspect_Entity =>
            null;
         when others =>
            Error_Kind ("add_dependence", Unit);
      end case;

      Add_Element (Get_Dependence_List (Target), Unit);
   end Add_Dependence;

   procedure Clear_Instantiation_Configuration_Vhdl87
     (Parent : Iir; In_Generate : Boolean; Full : Boolean)
   is
      El : Iir;
      Prev : Iir;
   begin
      El := Get_Concurrent_Statement_Chain (Parent);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Component_Instantiation_Statement =>
               if In_Generate and not Full then
                  Prev := Get_Component_Configuration (El);
                  if Prev /= Null_Iir then
                     case Get_Kind (Prev) is
                        when Iir_Kind_Configuration_Specification =>
                           --  Keep it.
                           null;
                        when Iir_Kind_Component_Configuration =>
                           Set_Component_Configuration (El, Null_Iir);
                        when others =>
                           Error_Kind
                             ("clear_instantiation_configuration_vhdl87",
                              Prev);
                     end case;
                  end if;
               else
                  Set_Component_Configuration (El, Null_Iir);
               end if;
            when Iir_Kind_For_Generate_Statement =>
               Set_Generate_Block_Configuration (El, Null_Iir);
               --  Clear inside a generate statement.
               Clear_Instantiation_Configuration_Vhdl87 (El, True, Full);
            when Iir_Kind_Block_Statement =>
               Set_Block_Block_Configuration (El, Null_Iir);
            when others =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;
   end Clear_Instantiation_Configuration_Vhdl87;

   procedure Clear_Instantiation_Configuration (Parent : Iir; Full : Boolean)
   is
      El : Iir;
   begin
      if False and then Flags.Vhdl_Std = Vhdl_87 then
         Clear_Instantiation_Configuration_Vhdl87
           (Parent, Get_Kind (Parent) = Iir_Kind_For_Generate_Statement, Full);
      else
         El := Get_Concurrent_Statement_Chain (Parent);
         while El /= Null_Iir loop
            case Get_Kind (El) is
               when Iir_Kind_Component_Instantiation_Statement =>
                  Set_Component_Configuration (El, Null_Iir);
               when Iir_Kind_For_Generate_Statement =>
                  declare
                     Bod : constant Iir := Get_Generate_Statement_Body (El);
                  begin
                     Set_Generate_Block_Configuration (Bod, Null_Iir);
                  end;
               when Iir_Kind_If_Generate_Statement =>
                  declare
                     Clause : Iir;
                     Bod : Iir;
                  begin
                     Clause := El;
                     while Clause /= Null_Iir loop
                        Bod := Get_Generate_Statement_Body (Clause);
                        Set_Generate_Block_Configuration (Bod, Null_Iir);
                        Clause := Get_Generate_Else_Clause (Clause);
                     end loop;
                  end;
               when Iir_Kind_Block_Statement =>
                  Set_Block_Block_Configuration (El, Null_Iir);
               when others =>
                  null;
            end case;
            El := Get_Chain (El);
         end loop;
      end if;
   end Clear_Instantiation_Configuration;

   --  Get identifier of NODE as a string.
   function Image_Identifier (Node : Iir) return String is
   begin
      return Name_Table.Image (Iirs.Get_Identifier (Node));
   end Image_Identifier;

   function Image_String_Lit (Str : Iir) return String is
   begin
      return Str_Table.String_String8
        (Get_String8_Id (Str), Get_String_Length (Str));
   end Image_String_Lit;

   function Copy_Enumeration_Literal (Lit : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Enumeration_Literal);
      Set_Identifier (Res, Get_Identifier (Lit));
      Location_Copy (Res, Lit);
      Set_Parent (Res, Get_Parent (Lit));
      Set_Type (Res, Get_Type (Lit));
      Set_Enum_Pos (Res, Get_Enum_Pos (Lit));
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Copy_Enumeration_Literal;

   procedure Create_Range_Constraint_For_Enumeration_Type
     (Def : Iir_Enumeration_Type_Definition)
   is
      Range_Expr : Iir_Range_Expression;
      Literal_List : constant Iir_List := Get_Enumeration_Literal_List (Def);
   begin
      --  Create a constraint.
      Range_Expr := Create_Iir (Iir_Kind_Range_Expression);
      Location_Copy (Range_Expr, Def);
      Set_Type (Range_Expr, Def);
      Set_Direction (Range_Expr, Iir_To);
      Set_Left_Limit
        (Range_Expr,
         Copy_Enumeration_Literal (Get_First_Element (Literal_List)));
      Set_Right_Limit
        (Range_Expr,
         Copy_Enumeration_Literal (Get_Last_Element (Literal_List)));
      Set_Expr_Staticness (Range_Expr, Locally);
      Set_Range_Constraint (Def, Range_Expr);
   end Create_Range_Constraint_For_Enumeration_Type;

   procedure Free_Name (Node : Iir)
   is
      N : Iir;
      N1 : Iir;
   begin
      if Node = Null_Iir then
         return;
      end if;
      N := Node;
      case Get_Kind (N) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Subtype_Definition =>
            Free_Iir (N);
         when Iir_Kind_Selected_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Selected_By_All_Name =>
            N1 := Get_Prefix (N);
            Free_Iir (N);
            Free_Name (N1);
         when Iir_Kind_Library_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Design_Unit
           | Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement =>
            return;
         when others =>
            Error_Kind ("free_name", Node);
            --Free_Iir (N);
      end case;
   end Free_Name;

   procedure Free_Recursive_List (List : Iir_List)
   is
      El : Iir;
   begin
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         Free_Recursive (El);
      end loop;
   end Free_Recursive_List;

   procedure Free_Recursive (Node : Iir; Free_List : Boolean := False)
   is
      N : Iir;
   begin
      if Node = Null_Iir then
         return;
      end if;
      N := Node;
      case Get_Kind (N) is
         when Iir_Kind_Library_Declaration =>
            return;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Character_Literal =>
            null;
         when Iir_Kind_Enumeration_Literal =>
            return;
         when Iir_Kind_Selected_Name =>
            Free_Recursive (Get_Prefix (N));
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            Free_Recursive (Get_Type (N));
            Free_Recursive (Get_Default_Value (N));
         when Iir_Kind_Range_Expression =>
            Free_Recursive (Get_Left_Limit (N));
            Free_Recursive (Get_Right_Limit (N));
         when Iir_Kind_Subtype_Definition =>
            Free_Recursive (Get_Base_Type (N));
         when Iir_Kind_Integer_Literal =>
            null;
         when Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration =>
            null;
         when Iir_Kind_File_Type_Definition
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            return;
         when Iir_Kind_Architecture_Body =>
            Free_Recursive (Get_Entity_Name (N));
         when Iir_Kind_Overload_List =>
            Free_Recursive_List (Get_Overload_List (N));
            if not Free_List then
               return;
            end if;
         when Iir_Kind_Array_Subtype_Definition =>
            Free_Recursive_List (Get_Index_List (N));
            Free_Recursive (Get_Base_Type (N));
         when Iir_Kind_Entity_Aspect_Entity =>
            Free_Recursive (Get_Entity (N));
            Free_Recursive (Get_Architecture (N));
         when others =>
            Error_Kind ("free_recursive", Node);
      end case;
      Free_Iir (N);
   end Free_Recursive;

   function Get_Predefined_Function_Name (Func : Iir_Predefined_Functions)
                                          return String
   is
   begin
      return Iir_Predefined_Functions'Image (Func);
   end Get_Predefined_Function_Name;

   procedure Mark_Subprogram_Used (Subprg : Iir)
   is
      N : Iir;
   begin
      N := Subprg;
      loop
         exit when Get_Use_Flag (N);
         Set_Use_Flag (N, True);
         N := Sem_Inst.Get_Origin (N);
         --  The origin may also be an instance.
         exit when N = Null_Iir;
      end loop;
   end Mark_Subprogram_Used;

   function Get_Callees_List_Holder (Subprg : Iir) return Iir is
   begin
      case Get_Kind (Subprg) is
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            return Get_Subprogram_Body (Subprg);
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            return Subprg;
         when others =>
            Error_Kind ("get_callees_list_holder", Subprg);
      end case;
   end Get_Callees_List_Holder;

   procedure Clear_Seen_Flag (Top : Iir)
   is
      Callees_List : Iir_Callees_List;
      El: Iir;
   begin
      if Get_Seen_Flag (Top) then
         Set_Seen_Flag (Top, False);
         Callees_List := Get_Callees_List (Get_Callees_List_Holder (Top));
         if Callees_List /= Null_Iir_List then
            for I in Natural loop
               El := Get_Nth_Element (Callees_List, I);
               exit when El = Null_Iir;
               if Get_Seen_Flag (El) = False then
                  Clear_Seen_Flag (El);
               end if;
            end loop;
         end if;
      end if;
   end Clear_Seen_Flag;

   function Is_Anonymous_Type_Definition (Def : Iir) return Boolean is
   begin
      return Get_Type_Declarator (Def) = Null_Iir;
   end Is_Anonymous_Type_Definition;

   function Is_Fully_Constrained_Type (Def : Iir) return Boolean is
   begin
      return Get_Kind (Def) not in Iir_Kinds_Composite_Type_Definition
        or else Get_Constraint_State (Def) = Fully_Constrained;
   end Is_Fully_Constrained_Type;

   function Strip_Denoting_Name (Name : Iir) return Iir is
   begin
      if Get_Kind (Name) in Iir_Kinds_Denoting_Name then
         return Get_Named_Entity (Name);
      else
         return Name;
      end if;
   end Strip_Denoting_Name;

   function Build_Simple_Name (Ref : Iir; Loc : Location_Type) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Simple_Name);
      Set_Location (Res, Loc);
      Set_Identifier (Res, Get_Identifier (Ref));
      Set_Named_Entity (Res, Ref);
      Set_Base_Name (Res, Res);
      --  FIXME: set type and expr staticness ?
      return Res;
   end Build_Simple_Name;

   function Build_Simple_Name (Ref : Iir; Loc : Iir) return Iir is
   begin
      return Build_Simple_Name (Ref, Get_Location (Loc));
   end Build_Simple_Name;

   function Has_Resolution_Function (Subtyp : Iir) return Iir
   is
      Ind : constant Iir := Get_Resolution_Indication (Subtyp);
   begin
      if Ind /= Null_Iir
        and then Get_Kind (Ind) in Iir_Kinds_Denoting_Name
      then
         return Get_Named_Entity (Ind);
      else
         return Null_Iir;
      end if;
   end Has_Resolution_Function;

   function Get_Primary_Unit_Name (Physical_Def : Iir) return Iir
   is
      Unit : constant Iir := Get_Primary_Unit (Physical_Def);
   begin
      return Get_Unit_Name (Get_Physical_Unit_Value (Unit));
   end Get_Primary_Unit_Name;

   function Is_Type_Name (Name : Iir) return Iir
   is
      Ent : Iir;
   begin
      if Get_Kind (Name) in Iir_Kinds_Denoting_Name then
         Ent := Get_Named_Entity (Name);
         case Get_Kind (Ent) is
            when Iir_Kind_Type_Declaration =>
               return Get_Type_Definition (Ent);
            when Iir_Kind_Subtype_Declaration
              | Iir_Kind_Base_Attribute =>
               return Get_Type (Ent);
            when others =>
               return Null_Iir;
         end case;
      else
         return Null_Iir;
      end if;
   end Is_Type_Name;

   function Get_Type_Of_Subtype_Indication (Ind : Iir) return Iir is
   begin
      case Get_Kind (Ind) is
         when Iir_Kinds_Denoting_Name =>
            return Get_Type (Ind);
         when Iir_Kinds_Subtype_Definition =>
            return Ind;
         when others =>
            Error_Kind ("get_type_of_subtype_indication", Ind);
      end case;
   end Get_Type_Of_Subtype_Indication;

   function Get_Index_Type (Indexes : Iir_List; Idx : Natural) return Iir
   is
      Index : constant Iir := Get_Nth_Element (Indexes, Idx);
   begin
      if Index = Null_Iir then
         return Null_Iir;
      else
         return Get_Index_Type (Index);
      end if;
   end Get_Index_Type;

   function Get_Index_Type (Array_Type : Iir; Idx : Natural) return Iir is
   begin
      return Get_Index_Type (Get_Index_Subtype_List (Array_Type), Idx);
   end Get_Index_Type;

   function Get_Denoted_Type_Mark (Subtyp : Iir) return Iir
   is
      Type_Mark_Name : constant Iir := Get_Subtype_Type_Mark (Subtyp);
   begin
      if Type_Mark_Name = Null_Iir then
         --  No type_mark (for array subtype created by constrained array
         --  definition.
         return Null_Iir;
      else
         return Get_Type (Get_Named_Entity (Type_Mark_Name));
      end if;
   end Get_Denoted_Type_Mark;

   function Is_Second_Subprogram_Specification (Spec : Iir) return Boolean
   is
      Bod : constant Iir := Get_Subprogram_Body (Spec);
   begin
      return Bod /= Null_Iir
        and then Get_Subprogram_Specification (Bod) /= Spec;
   end Is_Second_Subprogram_Specification;

   function Is_Implicit_Subprogram (Spec : Iir) return Boolean is
   begin
      return Get_Kind (Spec) in Iir_Kinds_Subprogram_Declaration
        and then Get_Implicit_Definition (Spec) in Iir_Predefined_Implicit;
   end Is_Implicit_Subprogram;

   function Is_Same_Profile (L, R: Iir) return Boolean
   is
      L1, R1 : Iir;
      L_Kind, R_Kind : Iir_Kind;
      El_L, El_R : Iir;
   begin
      L_Kind := Get_Kind (L);
      if L_Kind = Iir_Kind_Non_Object_Alias_Declaration then
         L1 := Get_Named_Entity (Get_Name (L));
         L_Kind := Get_Kind (L1);
      else
         L1 := L;
      end if;
      R_Kind := Get_Kind (R);
      if R_Kind = Iir_Kind_Non_Object_Alias_Declaration then
         R1 := Get_Named_Entity (Get_Name (R));
         R_Kind := Get_Kind (R1);
      else
         R1 := R;
      end if;

      --  Check L and R are both of the same 'kind'.
      --  Also the return profile for functions.
      if L_Kind = Iir_Kind_Function_Declaration
        and then R_Kind = Iir_Kind_Function_Declaration
      then
         if Get_Base_Type (Get_Return_Type (L1)) /=
           Get_Base_Type (Get_Return_Type (R1))
         then
            return False;
         end if;
      elsif L_Kind = Iir_Kind_Procedure_Declaration
        and then R_Kind = Iir_Kind_Procedure_Declaration
      then
         null;
      elsif L_Kind = Iir_Kind_Enumeration_Literal
        and then R_Kind = Iir_Kind_Enumeration_Literal
      then
         return Get_Type (L1) = Get_Type (R1);
      else
         --  Kind mismatch.
         return False;
      end if;

      --  Check parameters profile.
      El_L := Get_Interface_Declaration_Chain (L1);
      El_R := Get_Interface_Declaration_Chain (R1);
      loop
         exit when El_L = Null_Iir and El_R = Null_Iir;
         if El_L = Null_Iir or El_R = Null_Iir then
            return False;
         end if;
         if Get_Base_Type (Get_Type (El_L)) /= Get_Base_Type (Get_Type (El_R))
         then
            return False;
         end if;
         El_L := Get_Chain (El_L);
         El_R := Get_Chain (El_R);
      end loop;

      return True;
   end Is_Same_Profile;

   -- From a block_specification, returns the block.
   function Get_Block_From_Block_Specification (Block_Spec : Iir)
     return Iir
   is
      Res : Iir;
   begin
      case Get_Kind (Block_Spec) is
         when Iir_Kind_Design_Unit =>
            Res := Get_Library_Unit (Block_Spec);
            if Get_Kind (Res) /= Iir_Kind_Architecture_Body then
               raise Internal_Error;
            end if;
            return Res;
         when Iir_Kind_Block_Statement
           | Iir_Kind_Architecture_Body
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_If_Generate_Statement =>
            return Block_Spec;
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Slice_Name =>
            return Get_Named_Entity (Get_Prefix (Block_Spec));
         when Iir_Kind_Simple_Name =>
            return Get_Named_Entity (Block_Spec);
         when Iir_Kind_Parenthesis_Name =>
            --  An alternative label.
            return Get_Named_Entity (Block_Spec);
         when others =>
            Error_Kind ("get_block_from_block_specification", Block_Spec);
            return Null_Iir;
      end case;
   end Get_Block_From_Block_Specification;

   function Get_Entity (Decl : Iir) return Iir
   is
      Name : constant Iir := Get_Entity_Name (Decl);
      Res : constant Iir := Get_Named_Entity (Name);
   begin
      if Res = Std_Package.Error_Mark then
         return Null_Iir;
      end if;

      pragma Assert (Res = Null_Iir
                       or else Get_Kind (Res) = Iir_Kind_Entity_Declaration);
      return Res;
   end Get_Entity;

   function Get_Configuration (Aspect : Iir) return Iir
   is
      Name : constant Iir := Get_Configuration_Name (Aspect);
      Res : constant Iir := Get_Named_Entity (Name);
   begin
      pragma Assert (Get_Kind (Res) = Iir_Kind_Configuration_Declaration);
      return Res;
   end Get_Configuration;

   function Get_Entity_Identifier_Of_Architecture (Arch : Iir) return Name_Id
   is
      Name : constant Iir := Get_Entity_Name (Arch);
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Get_Identifier (Name);
         when others =>
            Error_Kind ("get_entity_identifier_of_architecture", Name);
      end case;
   end Get_Entity_Identifier_Of_Architecture;

   function Is_Component_Instantiation
     (Inst : Iir_Component_Instantiation_Statement)
     return Boolean is
   begin
      case Get_Kind (Get_Instantiated_Unit (Inst)) is
         when Iir_Kinds_Denoting_Name =>
            return True;
         when Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Entity_Aspect_Configuration =>
            return False;
         when others =>
            Error_Kind ("is_component_instantiation", Inst);
      end case;
   end Is_Component_Instantiation;

   function Is_Entity_Instantiation
     (Inst : Iir_Component_Instantiation_Statement)
     return Boolean is
   begin
      case Get_Kind (Get_Instantiated_Unit (Inst)) is
         when Iir_Kinds_Denoting_Name =>
            return False;
         when Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Entity_Aspect_Configuration =>
            return True;
         when others =>
            Error_Kind ("is_entity_instantiation", Inst);
      end case;
   end Is_Entity_Instantiation;

   function Get_String_Type_Bound_Type (Sub_Type : Iir) return Iir is
   begin
      if Get_Kind (Sub_Type) /= Iir_Kind_Array_Subtype_Definition then
         Error_Kind ("get_string_type_bound_type", Sub_Type);
      end if;
      return Get_First_Element (Get_Index_Subtype_List (Sub_Type));
   end Get_String_Type_Bound_Type;

   procedure Get_Low_High_Limit (Arange : Iir_Range_Expression;
                                 Low, High : out Iir)
   is
   begin
      case Get_Direction (Arange) is
         when Iir_To =>
            Low := Get_Left_Limit (Arange);
            High := Get_Right_Limit (Arange);
         when Iir_Downto =>
            High := Get_Left_Limit (Arange);
            Low := Get_Right_Limit (Arange);
      end case;
   end Get_Low_High_Limit;

   function Get_Low_Limit (Arange : Iir_Range_Expression) return Iir is
   begin
      case Get_Direction (Arange) is
         when Iir_To =>
            return Get_Left_Limit (Arange);
         when Iir_Downto =>
            return Get_Right_Limit (Arange);
      end case;
   end Get_Low_Limit;

   function Get_High_Limit (Arange : Iir_Range_Expression) return Iir is
   begin
      case Get_Direction (Arange) is
         when Iir_To =>
            return Get_Right_Limit (Arange);
         when Iir_Downto =>
            return Get_Left_Limit (Arange);
      end case;
   end Get_High_Limit;

   function Is_One_Dimensional_Array_Type (A_Type : Iir) return Boolean
   is
      Base_Type : constant Iir := Get_Base_Type (A_Type);
   begin
      if Get_Kind (Base_Type) = Iir_Kind_Array_Type_Definition
        and then Get_Nbr_Elements (Get_Index_Subtype_List (Base_Type)) = 1
      then
         return True;
      else
         return False;
      end if;
   end Is_One_Dimensional_Array_Type;

   function Is_Range_Attribute_Name (Expr : Iir) return Boolean
   is
      Attr : Iir;
      Id : Name_Id;
   begin
      if Get_Kind (Expr) = Iir_Kind_Parenthesis_Name then
         Attr := Get_Prefix (Expr);
      else
         Attr := Expr;
      end if;
      if Get_Kind (Attr) /= Iir_Kind_Attribute_Name then
         return False;
      end if;
      Id := Get_Identifier (Attr);
      return Id = Name_Range or Id = Name_Reverse_Range;
   end Is_Range_Attribute_Name;

   function Create_Array_Subtype (Arr_Type : Iir; Loc : Location_Type)
     return Iir_Array_Subtype_Definition
   is
      Res : Iir_Array_Subtype_Definition;
      Base_Type : Iir;
      List : Iir_List;
   begin
      Res := Create_Iir (Iir_Kind_Array_Subtype_Definition);
      Set_Location (Res, Loc);
      Base_Type := Get_Base_Type (Arr_Type);
      Set_Base_Type (Res, Base_Type);
      Set_Element_Subtype (Res, Get_Element_Subtype (Base_Type));
      if Get_Kind (Arr_Type) = Iir_Kind_Array_Subtype_Definition then
         Set_Resolution_Indication (Res, Get_Resolution_Indication (Arr_Type));
      end if;
      Set_Resolved_Flag (Res, Get_Resolved_Flag (Arr_Type));
      Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Arr_Type));
      Set_Type_Staticness (Res, Get_Type_Staticness (Base_Type));
      List := Create_Iir_List;
      Set_Index_Subtype_List (Res, List);
      Set_Index_Constraint_List (Res, List);
      return Res;
   end Create_Array_Subtype;

   function Is_Subprogram_Method (Spec : Iir) return Boolean is
   begin
      case Get_Kind (Get_Parent (Spec)) is
         when Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Protected_Type_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Subprogram_Method;

   function Get_Method_Type (Spec : Iir) return Iir
   is
      Parent : Iir;
   begin
      Parent := Get_Parent (Spec);
      case Get_Kind (Parent) is
         when Iir_Kind_Protected_Type_Declaration =>
            return Parent;
         when Iir_Kind_Protected_Type_Body =>
            return Get_Protected_Type_Declaration (Parent);
         when others =>
            return Null_Iir;
      end case;
   end Get_Method_Type;

   function Create_Error (Orig : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Error);
      Set_Error_Origin (Res, Orig);
      Location_Copy (Res, Orig);
      return Res;
   end Create_Error;

   function Create_Error_Expr (Orig : Iir; Atype : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Error (Orig);
      Set_Expr_Staticness (Res, None);
      Set_Type (Res, Atype);
      return Res;
   end Create_Error_Expr;

   function Create_Error_Type (Orig : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Error (Orig);
      --Set_Expr_Staticness (Res, Locally);
      Set_Base_Type (Res, Res);
      Set_Type_Declarator (Res, Null_Iir);
      Set_Resolved_Flag (Res, True);
      Set_Signal_Type_Flag (Res, True);
      return Res;
   end Create_Error_Type;

   --  Extract the entity from ASPECT.
   --  Note: if ASPECT is a component declaration, returns ASPECT.
   function Get_Entity_From_Entity_Aspect (Aspect : Iir) return Iir
   is
      Inst : Iir;
   begin
      case Get_Kind (Aspect) is
         when Iir_Kinds_Denoting_Name =>
            --  A component declaration.
            Inst := Get_Named_Entity (Aspect);
            pragma Assert (Get_Kind (Inst) = Iir_Kind_Component_Declaration);
            return Inst;
         when Iir_Kind_Component_Declaration =>
            return Aspect;
         when Iir_Kind_Entity_Aspect_Entity =>
            return Get_Entity (Aspect);
         when Iir_Kind_Entity_Aspect_Configuration =>
            Inst := Get_Configuration (Aspect);
            return Get_Entity (Inst);
         when Iir_Kind_Entity_Aspect_Open =>
            return Null_Iir;
         when others =>
            Error_Kind ("get_entity_from_entity_aspect", Aspect);
      end case;
   end Get_Entity_From_Entity_Aspect;

   --  LRM08 4.7 Package declarations
   --  If the package header is empty, the package declared by a package
   --  declaration is called a simple package.
   function Is_Simple_Package (Pkg : Iir) return Boolean is
   begin
      return Get_Package_Header (Pkg) = Null_Iir;
   end Is_Simple_Package;

   --  LRM08 4.7 Package declarations
   --  If the package header contains a generic clause and no generic map
   --  aspect, the package is called an uninstantiated package.
   function Is_Uninstantiated_Package (Pkg : Iir) return Boolean
   is
      Header : constant Iir := Get_Package_Header (Pkg);
   begin
      return Header /= Null_Iir
        and then Get_Generic_Map_Aspect_Chain (Header) = Null_Iir;
   end Is_Uninstantiated_Package;

   --  LRM08 4.7 Package declarations
   --  If the package header contains both a generic clause and a generic
   --  map aspect, the package is declared a generic-mapped package.
   function Is_Generic_Mapped_Package (Pkg : Iir) return Boolean
   is
      Header : constant Iir := Get_Package_Header (Pkg);
   begin
      return Header /= Null_Iir
        and then Get_Generic_Map_Aspect_Chain (Header) /= Null_Iir;
   end Is_Generic_Mapped_Package;

   function Kind_In (N : Iir; K1, K2 : Iir_Kind) return Boolean
   is
      K : constant Iir_Kind := Get_Kind (N);
   begin
      return K = K1 or K = K2;
   end Kind_In;

   function Get_HDL_Node (N : PSL_Node) return Iir is
   begin
      return Iir (PSL.Nodes.Get_HDL_Node (N));
   end Get_HDL_Node;

   procedure Set_HDL_Node (N : PSL_Node; Expr : Iir) is
   begin
      PSL.Nodes.Set_HDL_Node (N, PSL.Nodes.HDL_Node (Expr));
   end Set_HDL_Node;
end Iirs_Utils;
