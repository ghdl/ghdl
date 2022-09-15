--  Common operations on nodes.
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

with Name_Table;
with Str_Table;
with Std_Names; use Std_Names;
with Flags;
with Files_Map;

with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with PSL.Nodes;

package body Vhdl.Utils is
   function Is_Error (N : Iir) return Boolean is
   begin
      return Get_Kind (N) = Iir_Kind_Error;
   end Is_Error;

   function Is_Overflow_Literal (N : Iir) return Boolean is
   begin
      return Get_Kind (N) = Iir_Kind_Overflow_Literal;
   end Is_Overflow_Literal;

   function Strip_Literal_Origin (N : Iir) return Iir
   is
      Orig : Iir;
   begin
      if N = Null_Iir then
         return N;
      end if;
      case Get_Kind (N) is
         when Iir_Kind_String_Literal8
           |  Iir_Kind_Integer_Literal
           |  Iir_Kind_Floating_Point_Literal
           |  Iir_Kind_Physical_Int_Literal
           |  Iir_Kind_Physical_Fp_Literal
           |  Iir_Kind_Simple_Aggregate
           |  Iir_Kind_Overflow_Literal
           |  Iir_Kind_Enumeration_Literal
           |  Iir_Kind_Aggregate =>
            Orig := Get_Literal_Origin (N);
            if Orig /= Null_Iir then
               return Orig;
            else
               return N;
            end if;
         when others =>
            return N;
      end case;
   end Strip_Literal_Origin;

   function List_To_Flist (L : Iir_List) return Iir_Flist
   is
      Len : constant Natural := Get_Nbr_Elements (L);
      It : List_Iterator;
      Temp_L : Iir_List;
      Res : Iir_Flist;
   begin
      Res := Create_Iir_Flist (Len);
      It := List_Iterate (L);
      for I in 0 .. Len - 1 loop
         pragma Assert (Is_Valid (It));
         Set_Nth_Element (Res, I, Get_Element (It));
         Next (It);
      end loop;
      pragma Assert (not Is_Valid (It));

      Temp_L := L;
      Destroy_Iir_List (Temp_L);

      return Res;
   end List_To_Flist;

   function Truncate_Flist (L : Iir_Flist; Len : Natural) return Iir_Flist
   is
      Res : Iir_Flist;
      Temp_L : Iir_Flist;
   begin
      Res := Create_Iir_Flist (Len);
      for I in 0 .. Len - 1 loop
         Set_Nth_Element (Res, I, Get_Nth_Element (L, I));
      end loop;
      Temp_L := L;
      Destroy_Iir_Flist (Temp_L);
      return Res;
   end Truncate_Flist;

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
         when Iir_Kind_Condition_Operator
           | Iir_Kind_Implicit_Condition_Operator =>
            return Name_Op_Condition;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Operator_Name;

   function Get_Longest_Static_Prefix (Expr: Iir) return Iir
   is
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
               Error_Kind ("get_longest_static_prefix", Adecl);
         end case;
      end loop;
   end Get_Longest_Static_Prefix;

   function Get_Object_Prefix (Name: Iir; With_Alias : Boolean := True)
                              return Iir
   is
      Adecl : Iir;
   begin
      Adecl := Name;
      loop
         case Get_Kind (Adecl) is
            when Iir_Kinds_Non_Alias_Object_Declaration
               | Iir_Kinds_Quantity_Declaration
               | Iir_Kind_Terminal_Declaration
               | Iir_Kind_Interface_Quantity_Declaration
               | Iir_Kind_Interface_Terminal_Declaration
               | Iir_Kind_Interface_Type_Declaration
               | Iir_Kind_Interface_Package_Declaration
               | Iir_Kind_Interface_Function_Declaration
               | Iir_Kind_Interface_Procedure_Declaration
               | Iir_Kind_External_Signal_Name
               | Iir_Kind_External_Constant_Name
               | Iir_Kind_External_Variable_Name =>
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
               | Iir_Kind_Overflow_Literal
               | Iir_Kind_Enumeration_Literal
               | Iir_Kinds_Monadic_Operator
               | Iir_Kinds_Dyadic_Operator
               | Iir_Kind_Function_Call
               | Iir_Kind_Qualified_Expression
               | Iir_Kind_Type_Conversion
               | Iir_Kind_Allocator_By_Expression
               | Iir_Kind_Allocator_By_Subtype
               | Iir_Kind_Parenthesis_Expression
               | Iir_Kinds_Attribute
               | Iir_Kind_Attribute_Value
               | Iir_Kind_Aggregate
               | Iir_Kind_Simple_Aggregate
               | Iir_Kind_Dereference
               | Iir_Kind_Implicit_Dereference
               | Iir_Kind_Unit_Declaration
               | Iir_Kind_Psl_Expression
               | Iir_Kinds_Concurrent_Statement
               | Iir_Kinds_Sequential_Statement
               | Iir_Kinds_Simultaneous_Statement
               | Iir_Kind_Suspend_State_Statement =>
               return Adecl;
            when Iir_Kind_Simple_Name
               | Iir_Kind_Selected_Name =>
               Adecl := Get_Named_Entity (Adecl);
            when Iir_Kind_Attribute_Name =>
               return Get_Named_Entity (Adecl);
            when Iir_Kind_Error
               | Iir_Kind_Unused
               | Iir_Kind_Parenthesis_Name
               | Iir_Kind_Conditional_Expression
               | Iir_Kind_Character_Literal
               | Iir_Kind_Operator_Symbol
               | Iir_Kind_Design_File
               | Iir_Kind_Design_Unit
               | Iir_Kind_Library_Clause
               | Iir_Kind_Use_Clause
               | Iir_Kind_Context_Reference
               | Iir_Kind_PSL_Inherit_Spec
               | Iir_Kind_Library_Declaration
               | Iir_Kinds_Library_Unit
               | Iir_Kind_Component_Declaration
               | Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration
               | Iir_Kind_Function_Instantiation_Declaration
               | Iir_Kind_Procedure_Instantiation_Declaration
               | Iir_Kind_Attribute_Declaration
               | Iir_Kind_Nature_Declaration
               | Iir_Kind_Subnature_Declaration
               | Iir_Kinds_Type_Declaration
               | Iir_Kinds_Type_And_Subtype_Definition
               | Iir_Kind_Foreign_Vector_Type_Definition
               | Iir_Kinds_Nature_Definition
               | Iir_Kinds_Subnature_Definition
               | Iir_Kind_Wildcard_Type_Definition
               | Iir_Kind_Subtype_Definition
               | Iir_Kind_Group_Template_Declaration
               | Iir_Kind_Group_Declaration
               | Iir_Kind_Attribute_Implicit_Declaration
               | Iir_Kind_Suspend_State_Declaration
               | Iir_Kind_Unaffected_Waveform
               | Iir_Kind_Waveform_Element
               | Iir_Kind_Conditional_Waveform
               | Iir_Kind_Binding_Indication
               | Iir_Kind_Component_Configuration
               | Iir_Kind_Block_Configuration
               | Iir_Kinds_Specification
               | Iir_Kind_Non_Object_Alias_Declaration
               | Iir_Kinds_Subprogram_Body
               | Iir_Kind_Protected_Type_Body
               | Iir_Kind_Generate_Statement_Body
               | Iir_Kind_Procedure_Call
               | Iir_Kind_Aggregate_Info
               | Iir_Kind_Entity_Class
               | Iir_Kind_Signature
               | Iir_Kind_Break_Element
               | Iir_Kind_Reference_Name
               | Iir_Kind_Package_Header
               | Iir_Kind_Block_Header
               | Iir_Kinds_Association_Element
               | Iir_Kinds_Choice
               | Iir_Kinds_Entity_Aspect
               | Iir_Kind_Psl_Hierarchical_Name
               | Iir_Kind_Psl_Prev
               | Iir_Kind_Psl_Stable
               | Iir_Kind_Psl_Rose
               | Iir_Kind_Psl_Fell
               | Iir_Kind_Psl_Onehot
               | Iir_Kind_Psl_Onehot0
               | Iir_Kind_If_Generate_Else_Clause
               | Iir_Kind_Elsif
               | Iir_Kind_Simultaneous_Elsif
               | Iir_Kind_Record_Element_Constraint
               | Iir_Kind_Array_Element_Resolution
               | Iir_Kind_Record_Resolution
               | Iir_Kind_Record_Element_Resolution
               | Iir_Kind_Element_Declaration
               | Iir_Kind_Nature_Element_Declaration
               | Iir_Kind_Psl_Endpoint_Declaration
               | Iir_Kind_Psl_Boolean_Parameter
               | Iir_Kind_Psl_Declaration
               | Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Package_Pathname
               | Iir_Kind_Absolute_Pathname
               | Iir_Kind_Relative_Pathname
               | Iir_Kind_Pathname_Element
               | Iir_Kind_Range_Expression
               | Iir_Kind_Overload_List =>
               return Adecl;
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
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
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
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration =>
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
            if Name_To_Object (Get_Prefix (Name)) = Null_Iir then
               --  The prefix may not be an object.
               return Null_Iir;
            end if;
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

         when Iir_Kinds_External_Name =>
            return Name;

         --  AMS-LRM17 6.4 Objects
         --  An implicit signal defined by any of the predefined attributes
         --  'above, [...]
         when Iir_Kind_Above_Attribute =>
            return Name;

         --  AMS-LRM17 6.4 Objects
         --  An implicit quantity defined by any of the predefined attributes
         --  'DOT, 'INTEG, 'DELAYED, 'ZOH, 'LTF, 'ZTF, 'REFERENCE,
         --  'CONTRIBUTION, 'RAMP, and 'SLEW.
         when Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute =>
            return Name;

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
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Slice_Name =>
            --  Already a value.
            return Name;
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

   function Is_Quantity_Object (Name : Iir) return Boolean
   is
      Adecl: Iir;
   begin
      Adecl := Get_Object_Prefix (Name, True);
      case Get_Kind (Adecl) is
         when Iir_Kinds_Quantity_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Dot_Attribute =>
            return True;
         when Iir_Kind_Object_Alias_Declaration =>
            --  Must have been handled by Get_Object_Prefix.
            raise Internal_Error;
         when others =>
            return False;
      end case;
   end Is_Quantity_Object;

   function Is_Quantity_Name (Expr : Iir) return Boolean
   is
      Obj : Iir;
   begin
      Obj := Name_To_Object (Expr);
      if Obj /= Null_Iir then
         return Is_Quantity_Object (Obj);
      else
         return False;
      end if;
   end Is_Quantity_Name;

   function Get_Interface_Of_Formal (Formal : Iir) return Iir
   is
      El : Iir;
   begin
      El := Formal;
      loop
         case Get_Kind (El) is
            when Iir_Kind_Simple_Name
              | Iir_Kind_Reference_Name
              | Iir_Kind_Operator_Symbol =>
               --  Operator is for subprogram interfaces.
               return Get_Named_Entity (El);
            when Iir_Kinds_Interface_Declaration =>
               return El;
            when Iir_Kind_Slice_Name
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Selected_Element =>
               --  FIXME: use get_base_name ?
               El := Get_Prefix (El);
            when others =>
               Error_Kind ("get_interface_of_formal", El);
         end case;
      end loop;
   end Get_Interface_Of_Formal;

   function Get_Association_Interface (Assoc : Iir; Inter : Iir) return Iir
   is
      Formal : constant Iir := Get_Formal (Assoc);
   begin
      if Formal /= Null_Iir then
         return Get_Interface_Of_Formal (Formal);
      else
         return Inter;
      end if;
   end Get_Association_Interface;

   procedure Next_Association_Interface
     (Assoc : in out Iir; Inter : in out Iir)
   is
      Formal : Iir;
   begin
      --  In canon, open association can be inserted after an association by
      --  name.  So do not assume there is no association by position after
      --  association by name.
      Assoc := Get_Chain (Assoc);
      if Assoc = Null_Iir then
         --  End of the chain
         Inter := Null_Iir;
         return;
      end if;

      Formal := Get_Formal (Assoc);
      if Is_Valid (Formal) then
         Inter := Get_Interface_Of_Formal (Formal);
      else
         Inter := Get_Chain (Inter);
      end if;

      --  If INTER is null, this is an extra association.  Should it be
      --  skipped here ?  Or add a _Safe variant ?
   end Next_Association_Interface;

   function Get_Association_Formal (Assoc : Iir; Inter : Iir) return Iir
   is
      Formal : constant Iir := Get_Formal (Assoc);
   begin
      if Formal /= Null_Iir then
         --  Strip denoting name
         case Get_Kind (Formal) is
            when Iir_Kind_Simple_Name
              | Iir_Kind_Reference_Name
              | Iir_Kind_Operator_Symbol =>
               return Get_Named_Entity (Formal);
            when Iir_Kinds_Interface_Declaration =>
               --  Shouldn't happen.
               raise Internal_Error;
            when Iir_Kind_Slice_Name
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Selected_Element =>
               return Formal;
            when others =>
               Error_Kind ("get_association_formal", Formal);
         end case;
      else
         return Inter;
      end if;
   end Get_Association_Formal;

   function Find_First_Association_For_Interface
     (Assoc_Chain : Iir; Inter_Chain : Iir; Inter : Iir) return Iir
   is
      Assoc_El : Iir;
      Inter_El : Iir;
   begin
      Assoc_El := Assoc_Chain;
      Inter_El := Inter_Chain;
      while Is_Valid (Assoc_El) loop
         if Get_Association_Interface (Assoc_El, Inter_El) = Inter then
            return Assoc_El;
         end if;
         Next_Association_Interface (Assoc_El, Inter_El);
      end loop;
      return Null_Iir;
   end Find_First_Association_For_Interface;

   function Is_Parameter (Inter : Iir) return Boolean is
   begin
      case Get_Kind (Get_Parent (Inter)) is
         when Iir_Kinds_Subprogram_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration =>
            return True;
         when others =>
            --  Port
            return False;
      end case;
   end Is_Parameter;

   function Is_Copyback_Parameter (Inter : Iir) return Boolean is
   begin
      if Get_Kind (Inter) = Iir_Kind_Interface_Variable_Declaration
        and then Get_Mode (Inter) in Iir_Out_Mode .. Iir_Inout_Mode
      then
         --  For Vhdl-87, files are not copyback.
         return Get_Kind (Get_Type (Inter)) /= Iir_Kind_File_Type_Definition;
      else
         return False;
      end if;
   end Is_Copyback_Parameter;

   function Find_Name_In_Flist (List : Iir_Flist; Lit : Name_Id) return Iir
   is
      El : Iir;
   begin
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if Get_Identifier (El) = Lit then
            return El;
         end if;
      end loop;
      return Null_Iir;
   end Find_Name_In_Flist;

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

      pragma Assert (Kind_In (Unit, Iir_Kind_Design_Unit,
                              Iir_Kind_Foreign_Module,
                              Iir_Kind_Entity_Aspect_Entity));

      Add_Element (Get_Dependence_List (Target), Unit);
   end Add_Dependence;

   function Get_Unit_From_Dependence (Dep : Iir) return Iir is
   begin
      case Get_Kind (Dep) is
         when Iir_Kind_Design_Unit =>
            return Dep;
         when Iir_Kind_Entity_Aspect_Entity =>
            return Get_Design_Unit (Get_Entity (Dep));
         when others =>
            Error_Kind ("get_unit_from_dependence", Dep);
      end case;
   end Get_Unit_From_Dependence;

   procedure Clear_Instantiation_Configuration (Parent : Iir)
   is
      El : Iir;
   begin
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
   end Clear_Instantiation_Configuration;

   --  Get identifier of NODE as a string.
   function Image_Identifier (Node : Iir) return String is
   begin
      return Name_Table.Image (Vhdl.Nodes.Get_Identifier (Node));
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
      Literal_List : constant Iir_Flist := Get_Enumeration_Literal_List (Def);
      List_Len : constant Natural := Get_Nbr_Elements (Literal_List);
   begin
      --  Create a constraint.
      Range_Expr := Create_Iir (Iir_Kind_Range_Expression);
      Location_Copy (Range_Expr, Def);
      Set_Type (Range_Expr, Def);
      Set_Direction (Range_Expr, Dir_To);
      if List_Len >= 1 then
         Set_Left_Limit
           (Range_Expr, Get_Nth_Element (Literal_List, 0));
         Set_Right_Limit
           (Range_Expr, Get_Nth_Element (Literal_List, List_Len - 1));
      end if;
      Set_Expr_Staticness (Range_Expr, Locally);
      Set_Range_Constraint (Def, Range_Expr);
   end Create_Range_Constraint_For_Enumeration_Type;

   function Is_Static_Construct (Expr : Iir) return Boolean is
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Aggregate =>
            return Get_Aggregate_Expand_Flag (Expr);
         when Iir_Kinds_Literal =>
            return True;
         when Iir_Kind_Simple_Aggregate
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Character_Literal =>
            return True;
         when Iir_Kind_Overflow_Literal =>
            --  Needs to generate an error.
            return False;
         when others =>
            return False;
      end case;
   end Is_Static_Construct;

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
      It : List_Iterator;
   begin
      It := List_Iterate (List);
      while Is_Valid (It) loop
         Free_Recursive (Get_Element (It));
         Next (It);
      end loop;
   end Free_Recursive_List;

   procedure Free_Recursive_Flist (List : Iir_Flist)
   is
      El : Iir;
   begin
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         Free_Recursive (El);
      end loop;
   end Free_Recursive_Flist;

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
            Free_Recursive_Flist (Get_Index_List (N));
            Free_Recursive (Get_Base_Type (N));
         when Iir_Kind_Entity_Aspect_Entity =>
            Free_Recursive (Get_Entity_Name (N));
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
      It : List_Iterator;
      El: Iir;
   begin
      if Get_Seen_Flag (Top) then
         Set_Seen_Flag (Top, False);
         Callees_List := Get_Callees_List (Get_Callees_List_Holder (Top));
         if Callees_List /= Null_Iir_List then
            It := List_Iterate (Callees_List);
            while Is_Valid (It) loop
               El := Get_Element (It);
               if Get_Seen_Flag (El) = False then
                  Clear_Seen_Flag (El);
               end if;
               Next (It);
            end loop;
         end if;
      end if;
   end Clear_Seen_Flag;

   function Get_Base_Type (Atype : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Atype;
      loop
         case Get_Kind (Res) is
            when Iir_Kind_Access_Type_Definition
               | Iir_Kind_Integer_Type_Definition
               | Iir_Kind_Floating_Type_Definition
               | Iir_Kind_Enumeration_Type_Definition
               | Iir_Kind_Physical_Type_Definition
               | Iir_Kind_Array_Type_Definition
               | Iir_Kind_Record_Type_Definition
               | Iir_Kind_Protected_Type_Declaration
               | Iir_Kind_File_Type_Definition
               | Iir_Kind_Incomplete_Type_Definition
               | Iir_Kind_Interface_Type_Definition
               | Iir_Kind_Wildcard_Type_Definition
               | Iir_Kind_Foreign_Vector_Type_Definition
               | Iir_Kind_Error =>
               return Res;
            when Iir_Kind_Access_Subtype_Definition
               | Iir_Kind_File_Subtype_Definition
               | Iir_Kind_Integer_Subtype_Definition
               | Iir_Kind_Floating_Subtype_Definition
               | Iir_Kind_Enumeration_Subtype_Definition
               | Iir_Kind_Physical_Subtype_Definition
               | Iir_Kind_Array_Subtype_Definition
               | Iir_Kind_Record_Subtype_Definition =>
               Res := Get_Parent_Type (Res);
            when others =>
               Error_Kind ("get_base_type", Res);
         end case;
      end loop;
   end Get_Base_Type;

   function Is_Anonymous_Type_Definition (Def : Iir) return Boolean is
   begin
      return Get_Type_Declarator (Def) = Null_Iir;
   end Is_Anonymous_Type_Definition;

   function Is_Anonymous_Nature_Definition (Def : Iir) return Boolean is
   begin
      return Get_Nature_Declarator (Def) = Null_Iir;
   end Is_Anonymous_Nature_Definition;

   function Is_Array_Type (Def : Iir) return Boolean is
   begin
      return Get_Kind (Def) in Iir_Kinds_Array_Type_Definition;
   end Is_Array_Type;

   function Is_Fully_Constrained_Type (Def : Iir) return Boolean is
   begin
      return Get_Kind (Def) not in Iir_Kinds_Composite_Type_Definition
        or else Get_Constraint_State (Def) = Fully_Constrained;
   end Is_Fully_Constrained_Type;

   function Is_Object_Fully_Constrained (Decl : Iir) return Boolean is
   begin
      --  That's true if the object type is constrained.
      if Is_Fully_Constrained_Type (Get_Type (Decl)) then
         return True;
      end if;

      --  That's also true if the object is declared with a subtype attribute.
      if Get_Kind (Get_Subtype_Indication (Decl)) = Iir_Kind_Subtype_Attribute
      then
         return True;
      end if;

      --  Otherwise this is false.
      return False;
   end Is_Object_Fully_Constrained;

   function Is_Object_Name_Fully_Constrained (Obj : Iir) return Boolean
   is
      Base : Iir;
   begin
      --  That's true if the object type is constrained.
      if Flags.Flag_Relaxed_Rules
        or else Is_Fully_Constrained_Type (Get_Type (Obj))
      then
         return True;
      end if;

      --  That's also true if the object is declared with a subtype attribute.
      Base := Get_Base_Name (Obj);
      case Get_Kind (Base) is
         when Iir_Kind_Variable_Declaration
            | Iir_Kind_Signal_Declaration
            | Iir_Kind_Interface_Variable_Declaration
            | Iir_Kind_Interface_Signal_Declaration
            | Iir_Kind_Object_Alias_Declaration =>
            declare
               Ind : constant Iir := Get_Subtype_Indication (Base);
            begin
               --  Note: an object alias may not have subtype indication.
               if Ind /= Null_Iir
                 and then Get_Kind (Ind) = Iir_Kind_Subtype_Attribute
               then
                  return True;
               end if;
            end;
         when Iir_Kind_Dereference
            | Iir_Kind_Implicit_Dereference =>
            null;
         when others =>
            Error_Kind ("is_object_name_fully_constrained", Base);
      end case;

      --  Otherwise this is false.
      return False;
   end Is_Object_Name_Fully_Constrained;

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

   function Build_Reference_Name (Name : Iir) return Iir
   is
      Res : Iir;
   begin
      pragma Assert (Get_Kind (Name) in Iir_Kinds_Denoting_Name);

      Res := Create_Iir (Iir_Kind_Reference_Name);
      Location_Copy (Res, Name);
      Set_Referenced_Name (Res, Name);
      Set_Is_Forward_Ref (Res, True);
      Set_Named_Entity (Res, Get_Named_Entity (Name));
      return Res;
   end Build_Reference_Name;

   function Strip_Reference_Name (N : Iir) return Iir is
   begin
      if Get_Kind (N) = Iir_Kind_Reference_Name then
         return Get_Named_Entity (N);
      else
         return N;
      end if;
   end Strip_Reference_Name;

   function Has_Resolution_Function (Subtyp : Iir) return Iir
   is
      Ind : constant Iir := Get_Resolution_Indication (Subtyp);
   begin
      if Ind /= Null_Iir
        and then Get_Kind (Ind) in Iir_Kinds_Denoting_Name
      then
         --  A resolution indication can be an array/record element resolution.
         return Get_Named_Entity (Ind);
      else
         return Null_Iir;
      end if;
   end Has_Resolution_Function;

   function Is_Type_Name (Name : Iir) return Iir
   is
      Ent : Iir;
   begin
      case Get_Kind (Name) is
         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Attribute_Name =>
            Ent := Get_Named_Entity (Name);
            case Get_Kind (Ent) is
               when Iir_Kind_Type_Declaration =>
                  return Get_Type_Definition (Ent);
               when Iir_Kind_Subtype_Declaration
                 | Iir_Kind_Base_Attribute
                 | Iir_Kind_Subtype_Attribute
                 | Iir_Kind_Element_Attribute =>
                  return Get_Type (Ent);
               when others =>
                  return Null_Iir;
            end case;
         when Iir_Kind_Subtype_Attribute =>
            return Get_Type (Ent);
         when Iir_Kind_Element_Attribute =>
            return Get_Type (Name);
         when others =>
            return Null_Iir;
      end case;
   end Is_Type_Name;

   function Get_Type_Of_Subtype_Indication (Ind : Iir) return Iir is
   begin
      case Get_Kind (Ind) is
         when Iir_Kinds_Denoting_Name =>
            return Get_Type (Ind);
         when Iir_Kinds_Subtype_Definition =>
            return Ind;
         when Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute =>
            return Get_Type (Ind);
         when Iir_Kind_Interface_Type_Definition =>
            return Ind;
         when Iir_Kind_Error =>
            return Ind;
         when others =>
            Error_Kind ("get_type_of_subtype_indication", Ind);
      end case;
   end Get_Type_Of_Subtype_Indication;

   function Get_Nature_Of_Subnature_Indication (Ind : Iir) return Iir is
   begin
      case Get_Kind (Ind) is
         when Iir_Kinds_Denoting_Name =>
            --  Name of a nature.
            return Get_Nature (Get_Named_Entity (Ind));
         when Iir_Kind_Array_Subnature_Definition =>
            return Ind;
         when others =>
            Error_Kind ("get_nature_of_subnature_indication", Ind);
      end case;
   end Get_Nature_Of_Subnature_Indication;

   function Get_Index_Type (Indexes : Iir_Flist; Idx : Natural) return Iir
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

   function Get_Nbr_Dimensions (Array_Type : Iir) return Natural is
   begin
      return Get_Nbr_Elements (Get_Index_Subtype_List (Array_Type));
   end Get_Nbr_Dimensions;

   function Is_One_Dimensional_Array_Type (A_Type : Iir) return Boolean
   is
      Base_Type : constant Iir := Get_Base_Type (A_Type);
   begin
      return Get_Kind (Base_Type) = Iir_Kind_Array_Type_Definition
        and then Get_Nbr_Dimensions (Base_Type) = 1;
   end Is_One_Dimensional_Array_Type;

   function Are_Array_Indexes_Locally_Static (Array_Type : Iir) return Boolean
   is
      Indexes : constant Iir_Flist := Get_Index_Subtype_List (Array_Type);
      Index : Iir;
   begin
      for I in Flist_First .. Flist_Last (Indexes) loop
         Index := Get_Index_Type (Indexes, I);
         if Get_Type_Staticness (Index) /= Locally then
            return False;
         end if;
      end loop;
      return True;
   end Are_Array_Indexes_Locally_Static;

   function Are_Bounds_Locally_Static (Def : Iir) return Boolean is
   begin
      if Get_Type_Staticness (Def) = Locally then
         return True;
      end if;

      case Iir_Kinds_Type_And_Subtype_Definition (Get_Kind (Def)) is
         when Iir_Kind_Array_Subtype_Definition =>
            pragma Assert (Get_Constraint_State (Def) = Fully_Constrained);

            --  Indexes.
            if not Are_Array_Indexes_Locally_Static (Def) then
               return False;
            end if;

            --  Element.
            return Are_Bounds_Locally_Static (Get_Element_Subtype (Def));
         when Iir_Kind_Array_Type_Definition =>
            return False;
         when Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Record_Type_Definition =>
            pragma Assert (Get_Constraint_State (Def) = Fully_Constrained);

            declare
               El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Def);
               El : Iir;
            begin
               for I in Flist_First .. Flist_Last (El_List) loop
                  El := Get_Nth_Element (El_List, I);
                  if not Are_Bounds_Locally_Static (Get_Type (El)) then
                     return False;
                  end if;
               end loop;
               return True;
            end;
         when Iir_Kinds_Scalar_Type_And_Subtype_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            return True;
         when Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_File_Subtype_Definition
           | Iir_Kind_Interface_Type_Definition =>
            Error_Kind ("are_bounds_locally_static", Def);
      end case;
   end Are_Bounds_Locally_Static;

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

   function Get_Base_Element_Declaration (El : Iir) return Iir
   is
      Rec_Type : constant Iir := Get_Base_Type (Get_Parent (El));
      Els_List : constant Iir_Flist :=
        Get_Elements_Declaration_List (Rec_Type);
   begin
      return Get_Nth_Element
        (Els_List, Natural (Get_Element_Position (El)));
   end Get_Base_Element_Declaration;

   procedure Append_Owned_Element_Constraint (Rec_Type : Iir; El : Iir) is
   begin
      pragma Assert (Get_Parent (El) = Rec_Type);
      Set_Chain (El, Get_Owned_Elements_Chain (Rec_Type));
      Set_Owned_Elements_Chain (Rec_Type, El);
   end Append_Owned_Element_Constraint;


   function Is_Second_Subprogram_Specification (Spec : Iir) return Boolean
   is
      Bod : constant Iir := Get_Chain (Spec);
   begin
      --  FIXME: don't directly use Subprogram_Body as it is not yet correctly
      --  set during instantiation.
      return Get_Has_Body (Spec)
        and then Get_Subprogram_Specification (Bod) /= Spec;
   end Is_Second_Subprogram_Specification;

   function Is_Implicit_Subprogram (Spec : Iir) return Boolean is
   begin
      return Get_Kind (Spec) in Iir_Kinds_Subprogram_Declaration
        and then Get_Implicit_Definition (Spec) in Iir_Predefined_Implicit;
   end Is_Implicit_Subprogram;

   function Is_Function_Declaration (N : Iir) return Boolean is
   begin
      return Kind_In (N, Iir_Kind_Function_Declaration,
                      Iir_Kind_Interface_Function_Declaration);
   end Is_Function_Declaration;

   function Is_Procedure_Declaration (N : Iir) return Boolean is
   begin
      return Kind_In (N, Iir_Kind_Procedure_Declaration,
                      Iir_Kind_Interface_Procedure_Declaration);
   end Is_Procedure_Declaration;

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
      elsif L_Kind = Iir_Kind_Enumeration_Literal
        and then R_Kind = Iir_Kind_Function_Declaration
      then
         return Get_Interface_Declaration_Chain (R1) = Null_Iir
           and then Get_Base_Type (Get_Return_Type (R1)) = Get_Type (L1);
      elsif L_Kind = Iir_Kind_Function_Declaration
        and then R_Kind = Iir_Kind_Enumeration_Literal
      then
         return Get_Interface_Declaration_Chain (L1) = Null_Iir
           and then Get_Base_Type (Get_Return_Type (L1)) = Get_Type (R1);
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

   function Is_Operation_For_Type (Subprg : Iir; Atype : Iir) return Boolean
   is
      pragma Assert (Get_Kind (Subprg) in Iir_Kinds_Subprogram_Declaration);
      Base_Type : constant Iir := Get_Base_Type (Atype);
      Inter : Iir;
   begin
      Inter := Get_Interface_Declaration_Chain (Subprg);
      while Inter /= Null_Iir loop
         if Get_Base_Type (Get_Type (Inter)) = Base_Type then
            return True;
         end if;
         Inter := Get_Chain (Inter);
      end loop;
      if Get_Kind (Subprg) = Iir_Kind_Function_Declaration
        and then Get_Base_Type (Get_Return_Type (Subprg)) = Base_Type
      then
         return True;
      end if;
      return False;
   end Is_Operation_For_Type;

   -- From a block_specification, returns the block.
   function Get_Block_From_Block_Specification (Block_Spec : Iir) return Iir
   is
      Res : Iir;
   begin
      case Get_Kind (Block_Spec) is
         when Iir_Kind_Design_Unit =>
            Res := Get_Library_Unit (Block_Spec);
            pragma Assert (Get_Kind (Res) = Iir_Kind_Architecture_Body);
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
      if Res = Null_Iir or else Res = Vhdl.Std_Package.Error_Mark then
         return Null_Iir;
      end if;

      pragma Assert (Kind_In (Res, Iir_Kind_Entity_Declaration,
                              Iir_Kind_Foreign_Module));
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
         when Iir_Kind_Error =>
            return Null_Identifier;
         when others =>
            Error_Kind ("get_entity_identifier_of_architecture", Name);
      end case;
   end Get_Entity_Identifier_Of_Architecture;

   function Is_Component_Instantiation
     (Inst : Iir_Component_Instantiation_Statement) return Boolean is
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
     (Inst : Iir_Component_Instantiation_Statement) return Boolean is
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

   function Get_Attribute_Name_Expression (Name : Iir) return Iir
   is
      Attr_Val : constant Iir := Get_Named_Entity (Name);
      Attr_Spec : constant Iir := Get_Attribute_Specification (Attr_Val);
      Attr_Expr : constant Iir := Get_Expression (Attr_Spec);
   begin
      return Attr_Expr;
   end Get_Attribute_Name_Expression;

   function Get_String_Type_Bound_Type (Sub_Type : Iir) return Iir is
   begin
      if Get_Kind (Sub_Type) /= Iir_Kind_Array_Subtype_Definition then
         Error_Kind ("get_string_type_bound_type", Sub_Type);
      end if;
      return Get_Nth_Element (Get_Index_Subtype_List (Sub_Type), 0);
   end Get_String_Type_Bound_Type;

   procedure Get_Low_High_Limit (Arange : Iir_Range_Expression;
                                 Low, High : out Iir)
   is
   begin
      case Get_Direction (Arange) is
         when Dir_To =>
            Low := Get_Left_Limit (Arange);
            High := Get_Right_Limit (Arange);
         when Dir_Downto =>
            High := Get_Left_Limit (Arange);
            Low := Get_Right_Limit (Arange);
      end case;
   end Get_Low_High_Limit;

   function Get_Low_Limit (Arange : Iir_Range_Expression) return Iir is
   begin
      case Get_Direction (Arange) is
         when Dir_To =>
            return Get_Left_Limit (Arange);
         when Dir_Downto =>
            return Get_Right_Limit (Arange);
      end case;
   end Get_Low_Limit;

   function Get_High_Limit (Arange : Iir_Range_Expression) return Iir is
   begin
      case Get_Direction (Arange) is
         when Dir_To =>
            return Get_Right_Limit (Arange);
         when Dir_Downto =>
            return Get_Left_Limit (Arange);
      end case;
   end Get_High_Limit;

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

   function Get_Range_From_Discrete_Range (Rng : Iir) return Iir is
   begin
      case Get_Kind (Rng) is
         when Iir_Kinds_Denoting_Name =>
            return Get_Range_From_Discrete_Range (Get_Named_Entity (Rng));
         when Iir_Kinds_Scalar_Subtype_Definition =>
            return Get_Range_Constraint (Rng);
         when Iir_Kind_Range_Expression =>
            return Rng;
         when Iir_Kinds_Range_Attribute =>
            return Rng;
         when others =>
            Error_Kind ("get_range_from_discrete_range", Rng);
      end case;
   end Get_Range_From_Discrete_Range;

   function Create_Array_Subtype (Arr_Type : Iir; Loc : Location_Type)
     return Iir_Array_Subtype_Definition
   is
      Base_Type : constant Iir := Get_Base_Type (Arr_Type);
      El_Type : constant Iir := Get_Element_Subtype (Base_Type);
      Res : Iir_Array_Subtype_Definition;
      List : Iir_Flist;
   begin
      Res := Create_Iir (Iir_Kind_Array_Subtype_Definition);
      Set_Location (Res, Loc);
      Set_Parent_Type (Res, Base_Type);
      Set_Element_Subtype (Res, El_Type);
      if Get_Kind (Arr_Type) = Iir_Kind_Array_Subtype_Definition then
         Set_Resolution_Indication (Res, Get_Resolution_Indication (Arr_Type));
      end if;
      Set_Resolved_Flag (Res, Get_Resolved_Flag (Arr_Type));
      Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Arr_Type));
      Set_Type_Staticness (Res, Get_Type_Staticness (El_Type));
      List := Create_Iir_Flist (Get_Nbr_Dimensions (Base_Type));
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

   function Get_Actual_Or_Default (Assoc : Iir; Inter : Iir) return Iir is
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Association_Element_By_Expression =>
            return Get_Actual (Assoc);
         when Iir_Kind_Association_Element_Open =>
            return Get_Default_Value (Inter);
         when others =>
            Error_Kind ("get_actual_or_default", Assoc);
      end case;
   end Get_Actual_Or_Default;

   function Create_Error (Orig : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Error);
      if Orig /= Null_Iir then
         Set_Error_Origin (Res, Orig);
         Location_Copy (Res, Orig);
      end if;
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
      Set_Type_Declarator (Res, Null_Iir);
      Set_Resolved_Flag (Res, True);
      Set_Signal_Type_Flag (Res, True);
      return Res;
   end Create_Error_Type;

   function Create_Error_Name (Orig : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Error);
      Set_Expr_Staticness (Res, None);
      Set_Error_Origin (Res, Orig);
      Location_Copy (Res, Orig);
      return Res;
   end Create_Error_Name;

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

   function Get_Entity_From_Configuration (Config : Iir) return Iir
   is
      Conf_Unit : constant Iir := Get_Library_Unit (Config);
      Arch : constant Iir := Get_Named_Entity
        (Get_Block_Specification (Get_Block_Configuration (Conf_Unit)));
      Entity : constant Iir := Vhdl.Utils.Get_Entity (Arch);
   begin
      return Entity;
   end Get_Entity_From_Configuration;

   function Is_Nested_Package (Pkg : Iir) return Boolean is
   begin
      return Get_Kind (Get_Parent (Pkg)) /= Iir_Kind_Design_Unit;
   end Is_Nested_Package;

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

   --  LRM08 4.2 Subprogram declarations
   --  If the subprogram header contains the reserved word GENERIC, a generic
   --  list, and no generic map aspect, the subprogram is called an
   --  uninstantiated subprogram.
   function Is_Uninstantiated_Subprogram (Subprg : Iir) return Boolean is
   begin
      return Get_Generic_Chain (Subprg) /= Null_Iir;
   end Is_Uninstantiated_Subprogram;

   function Kind_In (N : Iir; K1, K2 : Iir_Kind) return Boolean
   is
      K : constant Iir_Kind := Get_Kind (N);
   begin
      return K = K1 or K = K2;
   end Kind_In;

   function Kind_In (N : Iir; K1, K2, K3 : Iir_Kind) return Boolean
   is
      K : constant Iir_Kind := Get_Kind (N);
   begin
      return K = K1 or K = K2 or K = K3;
   end Kind_In;

   procedure Set_Attribute_Parameter
     (Attr : Iir; N : Parameter_Index; Param : Iir) is
   begin
      case N is
         when 1 =>
            Set_Parameter (Attr, Param);
         when 2 =>
            Set_Parameter_2 (Attr, Param);
         when 3 =>
            Set_Parameter_3 (Attr, Param);
         when 4 =>
            Set_Parameter_4 (Attr, Param);
      end case;
   end Set_Attribute_Parameter;

   function Get_Attribute_Parameter
     (Attr : Iir; N : Parameter_Index) return Iir is
   begin
      case N is
         when 1 =>
            return Get_Parameter (Attr);
         when 2 =>
            return Get_Parameter_2 (Attr);
         when 3 =>
            return Get_Parameter_3 (Attr);
         when 4 =>
            return Get_Parameter_4 (Attr);
      end case;
   end Get_Attribute_Parameter;

   function Get_File_Signature_Length (Def : Iir) return Natural is
   begin
      case Get_Kind (Def) is
         when Iir_Kinds_Scalar_Type_And_Subtype_Definition =>
            return 1;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            return 2
              + Get_File_Signature_Length (Get_Element_Subtype (Def));
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Def));
               El : Iir;
               Res : Natural;
            begin
               Res := 2;
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Res := Res + Get_File_Signature_Length (Get_Type (El));
               end loop;
               return Res;
            end;
         when others =>
            Error_Kind ("get_file_signature_length", Def);
      end case;
   end Get_File_Signature_Length;

   procedure Get_File_Signature (Def : Iir;
                                 Res : in out String;
                                 Off : in out Natural)
   is
      Base_Type : constant Iir := Get_Base_Type (Def);
   begin
      case Get_Kind (Base_Type) is
         when Iir_Kind_Integer_Type_Definition =>
            case Get_Scalar_Size (Base_Type) is
               when Scalar_32 =>
                  Res (Off) := 'i';
               when Scalar_64 =>
                  Res (Off) := 'I';
               when others =>
                  raise Internal_Error;
            end case;
            Off := Off + 1;
         when Iir_Kind_Physical_Type_Definition =>
            case Get_Scalar_Size (Base_Type) is
               when Scalar_32 =>
                  Res (Off) := 'p';
               when Scalar_64 =>
                  Res (Off) := 'P';
               when others =>
                  raise Internal_Error;
            end case;
            Off := Off + 1;
         when Iir_Kind_Floating_Type_Definition =>
            Res (Off) := 'F';
            Off := Off + 1;
         when Iir_Kind_Enumeration_Type_Definition =>
            if Base_Type = Std_Package.Boolean_Type_Definition then
               Res (Off) := 'b';
            else
               case Get_Scalar_Size (Base_Type) is
                  when Scalar_8 =>
                     Res (Off) := 'e';
                  when Scalar_32 =>
                     Res (Off) := 'E';
                  when others =>
                     raise Internal_Error;
               end case;
            end if;
            Off := Off + 1;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            Res (Off) := '[';
            Off := Off + 1;
            Get_File_Signature (Get_Element_Subtype (Def), Res, Off);
            Res (Off) := ']';
            Off := Off + 1;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Def));
               El : Iir;
            begin
               Res (Off) := '<';
               Off := Off + 1;
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Get_File_Signature (Get_Type (El), Res, Off);
               end loop;
               Res (Off) := '>';
               Off := Off + 1;
            end;
         when others =>
            Error_Kind ("get_file_signature", Def);
      end case;
   end Get_File_Signature;

   function Get_Source_Identifier (Decl : Iir) return Name_Id
   is
      use Files_Map;
      use Name_Table;
      Loc : constant Location_Type := Get_Location (Decl);
      Len : constant Natural := Get_Name_Length (Get_Identifier (Decl));
      subtype Ident_Str is String (1 .. Len);
      File : Source_File_Entry;
      Pos : Source_Ptr;
      Buf : File_Buffer_Acc;
   begin
      Location_To_File_Pos (Loc, File, Pos);
      Buf := Get_File_Source (File);
      return Get_Identifier
        (Ident_Str (Buf (Pos .. Pos + Source_Ptr (Len - 1))));
   end Get_Source_Identifier;

   function Get_HDL_Node (N : PSL_Node) return Iir is
   begin
      return Iir (PSL.Nodes.Get_HDL_Node (N));
   end Get_HDL_Node;

   procedure Set_HDL_Node (N : PSL_Node; Expr : Iir) is
   begin
      PSL.Nodes.Set_HDL_Node (N, PSL.Nodes.HDL_Node (Expr));
   end Set_HDL_Node;
end Vhdl.Utils;
