--  Node garbage collector (for debugging).
--  Copyright (C) 2014 Tristan Gingold
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

with Ada.Text_IO;
with Types; use Types;
with Nodes;
with Iirs; use Iirs;
with Libraries;
with Disp_Tree;
with Std_Package;

package body Nodes_GC is

   type Marker_Array is array (Iir range <>) of Boolean;
   type Marker_Array_Acc is access Marker_Array;

   Markers : Marker_Array_Acc;

   procedure Mark_Iir (N : Iir);

   procedure Mark_Iir_List (N : Iir_List)
   is
      El : Iir;
   begin
      case N is
         when Null_Iir_List
           | Iir_List_All
           | Iir_List_Others =>
            null;
         when others =>
            for I in Natural loop
               El := Get_Nth_Element (N, I);
               exit when El = Null_Iir;
               Mark_Iir (El);
            end loop;
      end case;
   end Mark_Iir_List;

   procedure Mark_PSL_Node (N : PSL_Node) is
   begin
      null;
   end Mark_PSL_Node;

   procedure Mark_PSL_NFA (N : PSL_NFA) is
   begin
      null;
   end Mark_PSL_NFA;

   procedure Report_Already_Marked (N : Iir)
   is
      use Ada.Text_IO;
   begin
      Disp_Tree.Disp_Tree (N, True);
      return;
   end Report_Already_Marked;

   procedure Already_Marked (N : Iir) is
   begin
      --  An unused node mustn't be referenced.
      if Get_Kind (N) = Iir_Kind_Unused then
         raise Internal_Error;
      end if;

      if not Flag_Disp_Multiref then
         return;
      end if;

      case Get_Kind (N) is
         when Iir_Kind_Constant_Interface_Declaration =>
            if Get_Identifier (N) = Null_Identifier then
               --  Anonymous interfaces are shared by predefined functions.
               return;
            end if;
         when Iir_Kind_Enumeration_Literal =>
            if Get_Enum_Pos (N) = 0
              or else N = Get_Right_Limit (Get_Range_Constraint
                                             (Get_Type (N)))
            then
               return;
            end if;
         when others =>
            null;
      end case;

      Report_Already_Marked (N);
   end Already_Marked;

   procedure Mark_Chain (Head : Iir)
   is
      El : Iir;
   begin
      El := Head;
      while El /= Null_Iir loop
         Mark_Iir (El);
         El := Get_Chain (El);
      end loop;
   end Mark_Chain;

   procedure Report_Unreferenced_Node (N : Iir) is
   begin
      Disp_Tree.Disp_Tree (N, True);
   end Report_Unreferenced_Node;

   --  Subprograms
   procedure Mark_Iir (N : Iir) is
   begin
      if N = Null_Iir then
         return;
      elsif Markers (N) then
         Already_Marked (N);
         return;
      else
         Markers (N) := True;
      end if;

      case Get_Kind (N) is
         when Iir_Kind_Unused
           | Iir_Kind_Entity_Aspect_Open
           | Iir_Kind_Behavior_Attribute
           | Iir_Kind_Structure_Attribute =>
            null;
         when Iir_Kind_Error =>
            Mark_Iir (Get_Error_Origin (N));
         when Iir_Kind_Design_File =>
            Mark_Iir_List (Get_File_Dependence_List (N));
            Mark_Chain (Get_First_Design_Unit (N));
         when Iir_Kind_Design_Unit =>
            Mark_Chain (Get_Context_Items (N));
            Mark_Iir (Get_Library_Unit (N));
            Mark_Iir_List (Get_Analysis_Checks_List (N));
         when Iir_Kind_Library_Clause =>
            Mark_Iir (Get_Library_Declaration (N));
         when Iir_Kind_Use_Clause =>
            Mark_Iir (Get_Selected_Name (N));
            Mark_Iir (Get_Use_Clause_Chain (N));
         when Iir_Kind_Integer_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
         when Iir_Kind_Floating_Point_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
         when Iir_Kind_Null_Literal =>
            null;
         when Iir_Kind_String_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
            Mark_Iir (Get_Literal_Subtype (N));
         when Iir_Kind_Physical_Int_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
            Mark_Iir (Get_Unit_Name (N));
         when Iir_Kind_Physical_Fp_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
            Mark_Iir (Get_Unit_Name (N));
         when Iir_Kind_Bit_String_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
            Mark_Iir (Get_Literal_Subtype (N));
            Mark_Iir (Get_Bit_String_0 (N));
            Mark_Iir (Get_Bit_String_1 (N));
         when Iir_Kind_Simple_Aggregate =>
            Mark_Iir (Get_Literal_Origin (N));
            Mark_Iir_List (Get_Simple_Aggregate_List (N));
            Mark_Iir (Get_Literal_Subtype (N));
         when Iir_Kind_Overflow_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
         when Iir_Kind_Waveform_Element =>
            Mark_Iir (Get_We_Value (N));
            Mark_Iir (Get_Time (N));
         when Iir_Kind_Conditional_Waveform =>
            Mark_Iir (Get_Condition (N));
            Mark_Chain (Get_Waveform_Chain (N));
         when Iir_Kind_Association_Element_By_Expression =>
            Mark_Iir (Get_Formal (N));
            Mark_Iir (Get_Actual (N));
            Mark_Iir (Get_In_Conversion (N));
            Mark_Iir (Get_Out_Conversion (N));
         when Iir_Kind_Association_Element_By_Individual =>
            Mark_Iir (Get_Formal (N));
            Mark_Iir (Get_Actual_Type (N));
            Mark_Chain (Get_Individual_Association_Chain (N));
         when Iir_Kind_Association_Element_Open =>
            Mark_Iir (Get_Formal (N));
         when Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None =>
            Mark_Iir (Get_Associated_Expr (N));
            Mark_Chain (Get_Associated_Chain (N));
         when Iir_Kind_Choice_By_Expression =>
            Mark_Iir (Get_Associated_Expr (N));
            Mark_Chain (Get_Associated_Chain (N));
            Mark_Iir (Get_Choice_Expression (N));
         when Iir_Kind_Choice_By_Range =>
            Mark_Iir (Get_Associated_Expr (N));
            Mark_Chain (Get_Associated_Chain (N));
            Mark_Iir (Get_Choice_Range (N));
         when Iir_Kind_Choice_By_Name =>
            Mark_Iir (Get_Associated_Expr (N));
            Mark_Chain (Get_Associated_Chain (N));
            Mark_Iir (Get_Choice_Name (N));
         when Iir_Kind_Entity_Aspect_Entity =>
            Mark_Iir (Get_Entity_Name (N));
            Mark_Iir (Get_Architecture (N));
         when Iir_Kind_Entity_Aspect_Configuration =>
            Mark_Iir (Get_Configuration_Name (N));
         when Iir_Kind_Block_Configuration =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Configuration_Item_Chain (N));
            Mark_Iir (Get_Block_Specification (N));
         when Iir_Kind_Block_Header =>
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Chain (Get_Port_Chain (N));
            Mark_Chain (Get_Generic_Map_Aspect_Chain (N));
            Mark_Chain (Get_Port_Map_Aspect_Chain (N));
         when Iir_Kind_Component_Configuration =>
            Mark_Iir_List (Get_Instantiation_List (N));
            Mark_Iir (Get_Binding_Indication (N));
            Mark_Iir (Get_Component_Name (N));
            Mark_Iir (Get_Block_Configuration (N));
         when Iir_Kind_Binding_Indication =>
            Mark_Iir (Get_Default_Entity_Aspect (N));
            Mark_Iir (Get_Entity_Aspect (N));
            Mark_Chain (Get_Default_Generic_Map_Aspect_Chain (N));
            Mark_Chain (Get_Default_Port_Map_Aspect_Chain (N));
            Mark_Chain (Get_Generic_Map_Aspect_Chain (N));
            Mark_Chain (Get_Port_Map_Aspect_Chain (N));
         when Iir_Kind_Entity_Class =>
            null;
         when Iir_Kind_Attribute_Value =>
            Mark_Iir (Get_Spec_Chain (N));
         when Iir_Kind_Signature =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir_List (Get_Type_Marks_List (N));
            Mark_Iir (Get_Return_Type_Mark (N));
         when Iir_Kind_Aggregate_Info =>
            Mark_Iir (Get_Sub_Aggregate_Info (N));
            Mark_Iir (Get_Aggr_Low_Limit (N));
            Mark_Iir (Get_Aggr_High_Limit (N));
         when Iir_Kind_Procedure_Call =>
            Mark_Iir (Get_Prefix (N));
            Mark_Chain (Get_Parameter_Association_Chain (N));
            Mark_Iir (Get_Method_Object (N));
         when Iir_Kind_Record_Element_Constraint =>
            Mark_Iir (Get_Element_Declaration (N));
         when Iir_Kind_Attribute_Specification =>
            Mark_Iir_List (Get_Entity_Name_List (N));
            Mark_Iir (Get_Attribute_Value_Spec_Chain (N));
            Mark_Iir (Get_Expression (N));
            Mark_Iir (Get_Attribute_Designator (N));
            Mark_Iir (Get_Attribute_Specification_Chain (N));
         when Iir_Kind_Disconnection_Specification =>
            Mark_Iir_List (Get_Signal_List (N));
            Mark_Iir (Get_Type_Mark (N));
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_Configuration_Specification =>
            Mark_Iir_List (Get_Instantiation_List (N));
            Mark_Iir (Get_Binding_Indication (N));
            Mark_Iir (Get_Component_Name (N));
         when Iir_Kind_Access_Type_Definition =>
            Mark_Iir (Get_Designated_Subtype_Indication (N));
         when Iir_Kind_Incomplete_Type_Definition =>
            Mark_Iir_List (Get_Incomplete_Type_List (N));
         when Iir_Kind_File_Type_Definition =>
            Mark_Iir (Get_File_Type_Mark (N));
         when Iir_Kind_Protected_Type_Declaration =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Protected_Type_Body (N));
         when Iir_Kind_Record_Type_Definition =>
            Mark_Iir_List (Get_Elements_Declaration_List (N));
         when Iir_Kind_Array_Type_Definition =>
            Mark_Iir (Get_Element_Subtype_Indication (N));
            Mark_Iir_List (Get_Index_Subtype_List (N));
         when Iir_Kind_Array_Subtype_Definition =>
            Mark_Iir (Get_Element_Subtype_Indication (N));
            Mark_Iir (Get_Subtype_Type_Mark (N));
            Mark_Iir (Get_Resolution_Function (N));
            Mark_Iir_List (Get_Index_Subtype_List (N));
            Mark_Iir (Get_Tolerance (N));
         when Iir_Kind_Record_Subtype_Definition =>
            Mark_Iir_List (Get_Elements_Declaration_List (N));
            Mark_Iir (Get_Subtype_Type_Mark (N));
            Mark_Iir (Get_Resolution_Function (N));
            Mark_Iir (Get_Tolerance (N));
         when Iir_Kind_Access_Subtype_Definition =>
            Mark_Iir (Get_Subtype_Type_Mark (N));
            Mark_Iir (Get_Designated_Subtype_Indication (N));
         when Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            Mark_Iir (Get_Range_Constraint (N));
            Mark_Iir (Get_Subtype_Type_Mark (N));
            Mark_Iir (Get_Resolution_Function (N));
         when Iir_Kind_Floating_Subtype_Definition =>
            Mark_Iir (Get_Range_Constraint (N));
            Mark_Iir (Get_Subtype_Type_Mark (N));
            Mark_Iir (Get_Resolution_Function (N));
            Mark_Iir (Get_Tolerance (N));
         when Iir_Kind_Enumeration_Type_Definition =>
            Mark_Iir (Get_Range_Constraint (N));
            Mark_Iir_List (Get_Enumeration_Literal_List (N));
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition =>
            null;
         when Iir_Kind_Physical_Type_Definition =>
            Mark_Chain (Get_Unit_Chain (N));
         when Iir_Kind_Range_Expression =>
            Mark_Iir (Get_Left_Limit (N));
            Mark_Iir (Get_Right_Limit (N));
            Mark_Iir (Get_Range_Origin (N));
         when Iir_Kind_Protected_Type_Body =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Protected_Type_Declaration (N));
         when Iir_Kind_Subtype_Definition =>
            Mark_Iir (Get_Range_Constraint (N));
            Mark_Iir (Get_Subtype_Type_Mark (N));
            Mark_Iir (Get_Resolution_Function (N));
            Mark_Iir (Get_Tolerance (N));
         when Iir_Kind_Scalar_Nature_Definition =>
            Mark_Iir (Get_Reference (N));
            Mark_Iir (Get_Nature_Declarator (N));
            Mark_Iir (Get_Across_Type (N));
            Mark_Iir (Get_Through_Type (N));
         when Iir_Kind_Overload_List =>
            null;
         when Iir_Kind_Type_Declaration =>
            Mark_Iir (Get_Type_Definition (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
         when Iir_Kind_Anonymous_Type_Declaration =>
            Mark_Iir (Get_Type_Definition (N));
            Mark_Iir (Get_Subtype_Definition (N));
         when Iir_Kind_Subtype_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
         when Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration =>
            Mark_Iir (Get_Nature (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
         when Iir_Kind_Configuration_Declaration =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Entity_Name (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Block_Configuration (N));
         when Iir_Kind_Entity_Declaration =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Concurrent_Statement_Chain (N));
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Chain (Get_Port_Chain (N));
         when Iir_Kind_Package_Declaration =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Package_Body (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Package_Header (N));
         when Iir_Kind_Package_Body =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Package (N));
         when Iir_Kind_Architecture_Body =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Entity_Name (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Concurrent_Statement_Chain (N));
            Mark_Iir (Get_Default_Configuration_Declaration (N));
         when Iir_Kind_Package_Instantiation_Declaration =>
            Mark_Iir (Get_Uninstantiated_Name (N));
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Chain (Get_Generic_Map_Aspect_Chain (N));
         when Iir_Kind_Package_Header =>
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Chain (Get_Generic_Map_Aspect_Chain (N));
         when Iir_Kind_Unit_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Physical_Literal (N));
            Mark_Iir (Get_Physical_Unit_Value (N));
         when Iir_Kind_Library_Declaration =>
            Mark_Chain (Get_Design_File_Chain (N));
         when Iir_Kind_Component_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Chain (Get_Port_Chain (N));
         when Iir_Kind_Attribute_Declaration =>
            Mark_Iir (Get_Type_Mark (N));
         when Iir_Kind_Group_Template_Declaration =>
            Mark_Chain (Get_Entity_Class_Entry_Chain (N));
         when Iir_Kind_Group_Declaration =>
            Mark_Iir_List (Get_Group_Constituent_List (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Group_Template_Name (N));
         when Iir_Kind_Element_Declaration =>
            Mark_Iir (Get_Subtype_Indication (N));
         when Iir_Kind_Non_Object_Alias_Declaration =>
            Mark_Iir (Get_Name (N));
            Mark_Iir (Get_Alias_Signature (N));
         when Iir_Kind_Psl_Declaration =>
            Mark_PSL_Node (Get_Psl_Declaration (N));
            Mark_PSL_Node (Get_PSL_Clock (N));
            Mark_PSL_NFA (Get_PSL_NFA (N));
         when Iir_Kind_Terminal_Declaration =>
            Mark_Iir (Get_Nature (N));
         when Iir_Kind_Free_Quantity_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Default_Value (N));
         when Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Default_Value (N));
            Mark_Iir (Get_Tolerance (N));
            Mark_Iir (Get_Plus_Terminal (N));
            Mark_Iir (Get_Minus_Terminal (N));
         when Iir_Kind_Enumeration_Literal =>
            Mark_Iir (Get_Literal_Origin (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
         when Iir_Kind_Function_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Interface_Declaration_Chain (N));
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Iir_List (Get_Callees_List (N));
            Mark_Iir (Get_Return_Type_Mark (N));
            Mark_Iir (Get_Subprogram_Body (N));
         when Iir_Kind_Implicit_Function_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Interface_Declaration_Chain (N));
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Iir_List (Get_Callees_List (N));
            Mark_Chain (Get_Generic_Map_Aspect_Chain (N));
         when Iir_Kind_Implicit_Procedure_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Interface_Declaration_Chain (N));
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Iir_List (Get_Callees_List (N));
            Mark_Chain (Get_Generic_Map_Aspect_Chain (N));
         when Iir_Kind_Procedure_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Interface_Declaration_Chain (N));
            Mark_Chain (Get_Generic_Chain (N));
            Mark_Iir_List (Get_Callees_List (N));
            Mark_Iir (Get_Return_Type_Mark (N));
            Mark_Iir (Get_Subprogram_Body (N));
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Subprogram_Specification (N));
            Mark_Chain (Get_Sequential_Statement_Chain (N));
         when Iir_Kind_Object_Alias_Declaration =>
            Mark_Iir (Get_Name (N));
            Mark_Iir (Get_Subtype_Indication (N));
         when Iir_Kind_File_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
            Mark_Iir (Get_File_Logical_Name (N));
            Mark_Iir (Get_File_Open_Kind (N));
         when Iir_Kind_Guard_Signal_Declaration =>
            Mark_Iir (Get_Guard_Expression (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir_List (Get_Guard_Sensitivity_List (N));
            Mark_Iir (Get_Block_Statement (N));
         when Iir_Kind_Signal_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
            Mark_Iir (Get_Default_Value (N));
            Mark_Iir (Get_Signal_Driver (N));
         when Iir_Kind_Variable_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
            Mark_Iir (Get_Default_Value (N));
         when Iir_Kind_Constant_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
            Mark_Iir (Get_Default_Value (N));
            Mark_Iir (Get_Deferred_Declaration (N));
         when Iir_Kind_Iterator_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
            Mark_Iir (Get_Discrete_Range (N));
         when Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
            Mark_Iir (Get_Default_Value (N));
         when Iir_Kind_Signal_Interface_Declaration =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Subtype_Indication (N));
            Mark_Iir (Get_Default_Value (N));
         when Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator =>
            Mark_Iir (Get_Operand (N));
         when Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Match_Equality_Operator
           | Iir_Kind_Match_Inequality_Operator
           | Iir_Kind_Match_Less_Than_Operator
           | Iir_Kind_Match_Less_Than_Or_Equal_Operator
           | Iir_Kind_Match_Greater_Than_Operator
           | Iir_Kind_Match_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator =>
            Mark_Iir (Get_Left (N));
            Mark_Iir (Get_Right (N));
         when Iir_Kind_Function_Call =>
            Mark_Iir (Get_Prefix (N));
            Mark_Chain (Get_Parameter_Association_Chain (N));
            Mark_Iir (Get_Method_Object (N));
         when Iir_Kind_Aggregate =>
            Mark_Iir (Get_Aggregate_Info (N));
            Mark_Chain (Get_Association_Choices_Chain (N));
            Mark_Iir (Get_Literal_Subtype (N));
         when Iir_Kind_Parenthesis_Expression =>
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_Qualified_Expression =>
            Mark_Iir (Get_Type_Mark (N));
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_Type_Conversion =>
            Mark_Iir (Get_Type_Conversion_Subtype (N));
            Mark_Iir (Get_Type_Mark (N));
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_Allocator_By_Expression =>
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_Allocator_By_Subtype =>
            Mark_Iir (Get_Subtype_Indication (N));
         when Iir_Kind_Selected_Element =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Selected_Element (N));
         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute =>
            Mark_Iir (Get_Prefix (N));
         when Iir_Kind_Slice_Name =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Suffix (N));
            Mark_Iir (Get_Slice_Subtype (N));
         when Iir_Kind_Indexed_Name =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir_List (Get_Index_List (N));
         when Iir_Kind_Psl_Expression =>
            Mark_PSL_Node (Get_Psl_Expression (N));
         when Iir_Kind_Sensitized_Process_Statement =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Sequential_Statement_Chain (N));
            Mark_Iir_List (Get_Sensitivity_List (N));
            Mark_Iir_List (Get_Callees_List (N));
            Mark_Iir (Get_Process_Origin (N));
         when Iir_Kind_Process_Statement =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Sequential_Statement_Chain (N));
            Mark_Iir_List (Get_Callees_List (N));
            Mark_Iir (Get_Process_Origin (N));
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Mark_Iir (Get_Target (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Reject_Time_Expression (N));
            Mark_Chain (Get_Conditional_Waveform_Chain (N));
            Mark_Iir (Get_Guard (N));
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Mark_Iir (Get_Target (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Expression (N));
            Mark_Iir (Get_Reject_Time_Expression (N));
            Mark_Chain (Get_Selected_Waveform_Chain (N));
            Mark_Iir (Get_Guard (N));
         when Iir_Kind_Concurrent_Assertion_Statement =>
            Mark_Iir (Get_Assertion_Condition (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Severity_Expression (N));
            Mark_Iir (Get_Report_Expression (N));
         when Iir_Kind_Psl_Default_Clock =>
            Mark_PSL_Node (Get_Psl_Boolean (N));
         when Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Psl_Cover_Statement =>
            Mark_PSL_Node (Get_Psl_Property (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Severity_Expression (N));
            Mark_Iir (Get_Report_Expression (N));
            Mark_PSL_Node (Get_PSL_Clock (N));
            Mark_PSL_NFA (Get_PSL_NFA (N));
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Mark_Iir (Get_Procedure_Call (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
         when Iir_Kind_Block_Statement =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Concurrent_Statement_Chain (N));
            Mark_Iir (Get_Block_Block_Configuration (N));
            Mark_Iir (Get_Block_Header (N));
            Mark_Iir (Get_Guard_Decl (N));
         when Iir_Kind_Generate_Statement =>
            Mark_Chain (Get_Declaration_Chain (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Concurrent_Statement_Chain (N));
            Mark_Iir (Get_Generation_Scheme (N));
            Mark_Iir (Get_Generate_Block_Configuration (N));
         when Iir_Kind_Component_Instantiation_Statement =>
            Mark_Iir (Get_Instantiated_Unit (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Default_Binding_Indication (N));
            Mark_Iir (Get_Component_Configuration (N));
            Mark_Iir (Get_Configuration_Specification (N));
            Mark_Chain (Get_Generic_Map_Aspect_Chain (N));
            Mark_Chain (Get_Port_Map_Aspect_Chain (N));
         when Iir_Kind_Simple_Simultaneous_Statement =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Simultaneous_Left (N));
            Mark_Iir (Get_Simultaneous_Right (N));
            Mark_Iir (Get_Tolerance (N));
         when Iir_Kind_Signal_Assignment_Statement =>
            Mark_Iir (Get_Target (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Waveform_Chain (N));
            Mark_Iir (Get_Reject_Time_Expression (N));
         when Iir_Kind_Null_Statement =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
         when Iir_Kind_Assertion_Statement =>
            Mark_Iir (Get_Assertion_Condition (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Severity_Expression (N));
            Mark_Iir (Get_Report_Expression (N));
         when Iir_Kind_Report_Statement =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Severity_Expression (N));
            Mark_Iir (Get_Report_Expression (N));
         when Iir_Kind_Wait_Statement =>
            Mark_Iir (Get_Timeout_Clause (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Condition_Clause (N));
            Mark_Iir_List (Get_Sensitivity_List (N));
         when Iir_Kind_Variable_Assignment_Statement =>
            Mark_Iir (Get_Target (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_Return_Statement =>
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_For_Loop_Statement =>
            Mark_Iir (Get_Parameter_Specification (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Sequential_Statement_Chain (N));
         when Iir_Kind_While_Loop_Statement =>
            Mark_Iir (Get_Condition (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Sequential_Statement_Chain (N));
         when Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement =>
            Mark_Iir (Get_Condition (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Loop_Label (N));
         when Iir_Kind_Case_Statement =>
            Mark_Chain (Get_Case_Statement_Alternative_Chain (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Iir (Get_Expression (N));
         when Iir_Kind_Procedure_Call_Statement =>
            Mark_Iir (Get_Procedure_Call (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
         when Iir_Kind_If_Statement =>
            Mark_Iir (Get_Condition (N));
            Mark_Iir (Get_Attribute_Value_Chain (N));
            Mark_Chain (Get_Sequential_Statement_Chain (N));
            Mark_Iir (Get_Else_Clause (N));
         when Iir_Kind_Elsif =>
            Mark_Iir (Get_Condition (N));
            Mark_Chain (Get_Sequential_Statement_Chain (N));
            Mark_Iir (Get_Else_Clause (N));
         when Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name =>
            Mark_Iir (Get_Alias_Declaration (N));
         when Iir_Kind_Selected_Name =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Alias_Declaration (N));
         when Iir_Kind_Operator_Symbol =>
            Mark_Iir (Get_Alias_Declaration (N));
         when Iir_Kind_Selected_By_All_Name =>
            Mark_Iir (Get_Prefix (N));
         when Iir_Kind_Parenthesis_Name =>
            Mark_Iir (Get_Prefix (N));
            Mark_Chain (Get_Association_Chain (N));
         when Iir_Kind_Base_Attribute =>
            Mark_Iir (Get_Prefix (N));
         when Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Parameter (N));
         when Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Parameter (N));
         when Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute =>
            Mark_Iir (Get_Prefix (N));
         when Iir_Kind_Simple_Name_Attribute =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Simple_Name_Subtype (N));
         when Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Index_Subtype (N));
            Mark_Iir (Get_Parameter (N));
         when Iir_Kind_Attribute_Name =>
            Mark_Iir (Get_Prefix (N));
            Mark_Iir (Get_Attribute_Signature (N));
      end case;
   end Mark_Iir;


   procedure Report_Unreferenced
   is
      use Ada.Text_IO;
      use Std_Package;
      El : Iir;
      Nbr_Unreferenced : Natural;
   begin
      Markers := new Marker_Array'(Null_Iir .. Iirs.Get_Last_Node => False);

      if Flag_Disp_Multiref then
         Put_Line ("** nodes already marked:");
      end if;

      Mark_Chain (Libraries.Get_Libraries_Chain);
      Mark_Chain (Libraries.Obsoleted_Design_Units);
      Mark_Iir (Convertible_Integer_Type_Declaration);
      Mark_Iir (Convertible_Integer_Subtype_Declaration);
      Mark_Iir (Convertible_Real_Type_Declaration);
      Mark_Iir (Universal_Integer_One);
      Mark_Iir (Error_Mark);

      El := Error_Mark;
      Nbr_Unreferenced := 0;
      while El in Markers'Range loop
         if not Markers (El) and then Get_Kind (El) /= Iir_Kind_Unused then
            if Nbr_Unreferenced = 0 then
               Put_Line ("** unreferenced nodes:");
            end if;
            Nbr_Unreferenced := Nbr_Unreferenced + 1;
            Report_Unreferenced_Node (El);
         end if;
         El := Iir (Nodes.Next_Node (Nodes.Node_Type (El)));
      end loop;

      if Nbr_Unreferenced /= 0 then
         raise Internal_Error;
      end if;
   end Report_Unreferenced;
end Nodes_GC;
