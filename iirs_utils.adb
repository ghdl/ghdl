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
with Scan; use Scan;
with Tokens; use Tokens;
with Errorout; use Errorout;
with Name_Table;
with Str_Table;
with Std_Names; use Std_Names;
with Flags; use Flags;
with PSL.Nodes;

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

   function Get_Operator_Name (Op : Iir) return Name_Id is
   begin
      case Get_Kind (Op) is
         when Iir_Kind_And_Operator =>
            return Name_And;
         when Iir_Kind_Or_Operator =>
            return Name_Or;
         when Iir_Kind_Nand_Operator =>
            return Name_Nand;
         when Iir_Kind_Nor_Operator =>
            return Name_Nor;
         when Iir_Kind_Xor_Operator =>
            return Name_Xor;
         when Iir_Kind_Xnor_Operator =>
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
              | Iir_Kind_Variable_Interface_Declaration =>
               return Adecl;
            when Iir_Kind_Constant_Declaration
              | Iir_Kind_Constant_Interface_Declaration =>
               return Adecl;
            when Iir_Kind_Signal_Declaration
              | Iir_Kind_Guard_Signal_Declaration
              | Iir_Kind_Signal_Interface_Declaration =>
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

   function Get_Object_Prefix (Decl: Iir) return Iir is
      Adecl: Iir;
   begin
      Adecl := Decl;
      loop
         case Get_Kind (Adecl) is
            when Iir_Kind_Variable_Declaration
              | Iir_Kind_Variable_Interface_Declaration
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Constant_Interface_Declaration
              | Iir_Kind_Signal_Declaration
              | Iir_Kind_Guard_Signal_Declaration
              | Iir_Kind_Signal_Interface_Declaration
              | Iir_Kind_File_Declaration
              | Iir_Kind_File_Interface_Declaration
              | Iir_Kind_Iterator_Declaration =>
               return Adecl;
            when Iir_Kind_Object_Alias_Declaration =>
               Adecl := Get_Name (Adecl);
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
              | Iir_Kind_Unit_Declaration =>
               return Adecl;
            when Iir_Kind_Simple_Name
              | Iir_Kind_Selected_Name =>
               Adecl := Get_Named_Entity (Adecl);
            when others =>
               Error_Kind ("get_object_prefix", Adecl);
         end case;
      end loop;
   end Get_Object_Prefix;

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
      if Unit = Target then
         return;
      end if;
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
            when Iir_Kind_Generate_Statement =>
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
           (Parent, Get_Kind (Parent) = Iir_Kind_Generate_Statement, Full);
      else
         El := Get_Concurrent_Statement_Chain (Parent);
         while El /= Null_Iir loop
            case Get_Kind (El) is
               when Iir_Kind_Component_Instantiation_Statement =>
                  Set_Component_Configuration (El, Null_Iir);
               when Iir_Kind_Generate_Statement =>
                  Set_Generate_Block_Configuration (El, Null_Iir);
               when Iir_Kind_Block_Statement =>
                  Set_Block_Block_Configuration (El, Null_Iir);
               when others =>
                  null;
            end case;
            El := Get_Chain (El);
         end loop;
      end if;
   end Clear_Instantiation_Configuration;

   function Get_String_Fat_Acc (Str : Iir) return String_Fat_Acc is
   begin
      return Str_Table.Get_String_Fat_Acc (Get_String_Id (Str));
   end Get_String_Fat_Acc;

   --  Get identifier of NODE as a string.
   function Image_Identifier (Node : Iir) return String is
   begin
      return Name_Table.Image (Iirs.Get_Identifier (Node));
   end Image_Identifier;

   function Image_String_Lit (Str : Iir) return String
   is
      Ptr : String_Fat_Acc;
      Len : Nat32;
   begin
      Ptr := Get_String_Fat_Acc (Str);
      Len := Get_String_Length (Str);
      return String (Ptr (1 .. Len));
   end Image_String_Lit;

   procedure Create_Range_Constraint_For_Enumeration_Type
     (Def : Iir_Enumeration_Type_Definition)
   is
      Range_Expr : Iir_Range_Expression;
      Literal_List: Iir_List;
   begin
      Literal_List := Get_Enumeration_Literal_List (Def);

      --  Create a constraint.
      Range_Expr := Create_Iir (Iir_Kind_Range_Expression);
      Location_Copy (Range_Expr, Def);
      Set_Type (Range_Expr, Def);
      Set_Direction (Range_Expr, Iir_To);
      Set_Left_Limit (Range_Expr, Get_First_Element (Literal_List));
      Set_Right_Limit (Range_Expr, Get_Last_Element (Literal_List));
      Set_Expr_Staticness (Range_Expr, Locally);
      Set_Range_Constraint (Def, Range_Expr);
   end Create_Range_Constraint_For_Enumeration_Type;

   procedure Free_Old_Iir (Node: in Iir)
   is
      N : Iir;
   begin
      N := Node;
      Free_Iir (N);
   end Free_Old_Iir;

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
           | Iir_Kind_String_Literal
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
           | Iir_Kind_Architecture_Declaration
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
         when Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration =>
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
           | Iir_Kind_Configuration_Declaration =>
            null;
         when Iir_Kind_File_Type_Definition
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            return;
         when Iir_Kind_Architecture_Declaration =>
            Free_Recursive (Get_Entity (N));
         when Iir_Kind_Proxy =>
            null;
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

   procedure Clear_Seen_Flag (Top : Iir)
   is
      Callees_List : Iir_Callees_List;
      El: Iir;
   begin
      if Get_Seen_Flag (Top) then
         Set_Seen_Flag (Top, False);
         Callees_List := Get_Callees_List (Top);
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

   function Is_Same_Profile (L, R: Iir) return Boolean
   is
      L1, R1 : Iir;
      L_Kind, R_Kind : Iir_Kind;
      El_L, El_R : Iir;
   begin
      L_Kind := Get_Kind (L);
      if L_Kind = Iir_Kind_Non_Object_Alias_Declaration then
         L1 := Get_Name (L);
         L_Kind := Get_Kind (L1);
      else
         L1 := L;
      end if;
      R_Kind := Get_Kind (R);
      if R_Kind = Iir_Kind_Non_Object_Alias_Declaration then
         R1 := Get_Name (R);
         R_Kind := Get_Kind (R1);
      else
         R1 := R;
      end if;

      --  Check L and R are both of the same 'kind'.
      --  Also the return profile for functions.
      if L_Kind in Iir_Kinds_Function_Declaration
        and then R_Kind in Iir_Kinds_Function_Declaration
      then
         if Get_Base_Type (Get_Return_Type (L1)) /=
           Get_Base_Type (Get_Return_Type (R1))
         then
            return False;
         end if;
      elsif L_Kind in Iir_Kinds_Procedure_Declaration
        and then R_Kind in Iir_Kinds_Procedure_Declaration
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
            if Get_Kind (Res) /= Iir_Kind_Architecture_Declaration then
               raise Internal_Error;
            end if;
            return Res;
         when Iir_Kind_Block_Statement
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Generate_Statement =>
            return Block_Spec;
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Slice_Name =>
            return Get_Prefix (Block_Spec);
         when others =>
            Error_Kind ("get_block_from_block_specification", Block_Spec);
            return Null_Iir;
      end case;
   end Get_Block_From_Block_Specification;

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

   function Is_Unidim_Array_Type (A_Type : Iir) return Boolean
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
   end Is_Unidim_Array_Type;

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
      Id := Get_Attribute_Identifier (Attr);
      return Id = Name_Range or Id = Name_Reverse_Range;
   end Is_Range_Attribute_Name;

   function Create_Array_Subtype (Arr_Type : Iir; Loc : Location_Type)
     return Iir_Array_Subtype_Definition
   is
      Res : Iir_Array_Subtype_Definition;
      Base_Type : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Array_Subtype_Definition);
      Set_Location (Res, Loc);
      Base_Type := Get_Base_Type (Arr_Type);
      Set_Base_Type (Res, Base_Type);
      Set_Element_Subtype (Res, Get_Element_Subtype (Base_Type));
      if Get_Kind (Arr_Type) /= Iir_Kind_Array_Type_Definition then
         Set_Resolution_Function (Res, Get_Resolution_Function (Arr_Type));
      end if;
      Set_Resolved_Flag (Res, Get_Resolved_Flag (Arr_Type));
      Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Arr_Type));
      Set_Type_Staticness (Res, Get_Type_Staticness (Base_Type));
      Set_Index_Subtype_List (Res, Create_Iir_List);
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

   function Create_Error_Expr (Orig : Iir; Atype : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Error);
      Set_Expr_Staticness (Res, Locally);
      Set_Type (Res, Atype);
      Set_Error_Origin (Res, Orig);
      Location_Copy (Res, Orig);
      return Res;
   end Create_Error_Expr;

   function Create_Error_Type (Orig : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Error);
      --Set_Expr_Staticness (Res, Locally);
      Set_Base_Type (Res, Res);
      Set_Error_Origin (Res, Orig);
      Location_Copy (Res, Orig);
      Set_Type_Declarator (Res, Null_Iir);
      Set_Resolved_Flag (Res, True);
      Set_Signal_Type_Flag (Res, True);
      return Res;
   end Create_Error_Type;

   function Get_Associated_Formal (Assoc : Iir) return Iir
   is
      Formal : Iir;
   begin
      Formal := Get_Formal (Assoc);
      case Get_Kind (Formal) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Formal := Get_Named_Entity (Formal);
         when others =>
            null;
      end case;
      return Get_Base_Name (Formal);
   end Get_Associated_Formal;

   --  Extract the entity from ASPECT.
   --  Note: if ASPECT is a component declaration, returns ASPECT.
   function Get_Entity_From_Entity_Aspect (Aspect : Iir) return Iir
   is
      Inst : Iir;
   begin
      case Get_Kind (Aspect) is
         when Iir_Kind_Component_Declaration =>
            return Aspect;
         when Iir_Kind_Entity_Aspect_Entity =>
            return Get_Library_Unit (Get_Entity (Aspect));
         when Iir_Kind_Entity_Aspect_Configuration =>
            Inst := Get_Library_Unit (Get_Configuration (Aspect));
            return Get_Library_Unit (Get_Entity (Inst));
         when Iir_Kind_Entity_Aspect_Open =>
            return Null_Iir;
         when others =>
            Error_Kind ("get_entity_from_entity_aspect", Aspect);
      end case;
   end Get_Entity_From_Entity_Aspect;

   function Get_Physical_Literal_Value (Lit : Iir) return Iir_Int64
   is
   begin
      case Get_Kind (Lit) is
         when Iir_Kind_Physical_Int_Literal =>
            return Get_Value (Lit)
              * Get_Value (Get_Physical_Unit_Value (Get_Unit_Name (Lit)));
         when Iir_Kind_Unit_Declaration =>
            return Get_Value (Get_Physical_Unit_Value (Lit));
         when Iir_Kind_Physical_Fp_Literal =>
            return Iir_Int64
              (Get_Fp_Value (Lit)
               * Iir_Fp64 (Get_Value (Get_Physical_Unit_Value
                                      (Get_Unit_Name (Lit)))));
         when others =>
            Error_Kind ("get_physical_literal_value", Lit);
      end case;
   end Get_Physical_Literal_Value;

   function Is_Signal_Object (Name : Iir) return Boolean
   is
      Adecl: Iir;
   begin
      Adecl := Get_Base_Name (Name);
      loop
         case Get_Kind (Adecl) is
            when Iir_Kind_Variable_Declaration
              | Iir_Kind_Variable_Interface_Declaration
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Constant_Interface_Declaration
              | Iir_Kind_Implicit_Dereference
              | Iir_Kind_Dereference
              | Iir_Kind_Attribute_Value
              | Iir_Kind_Function_Call =>
               return False;
            when Iir_Kind_Signal_Declaration
              | Iir_Kind_Signal_Interface_Declaration
              | Iir_Kind_Guard_Signal_Declaration
              | Iir_Kinds_Signal_Attribute =>
               return True;
            when Iir_Kind_Object_Alias_Declaration =>
               Adecl := Get_Base_Name (Get_Name (Adecl));
            when others =>
               Error_Kind ("is_signal_object", Adecl);
         end case;
      end loop;
   end Is_Signal_Object;

   function Get_HDL_Node (N : PSL_Node) return Iir is
   begin
      return Iir (PSL.Nodes.Get_HDL_Node (N));
   end Get_HDL_Node;
end Iirs_Utils;
