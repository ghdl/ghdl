--  Meta description of nodes.
--  Copyright (C) 2014 Tristan Gingold
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

package body Vhdl.Nodes_Meta is
   Fields_Type : constant array (Fields_Enum) of Types_Enum :=
     (
      Field_First_Design_Unit => Type_Iir,
      Field_Last_Design_Unit => Type_Iir,
      Field_Library_Declaration => Type_Iir,
      Field_File_Checksum => Type_File_Checksum_Id,
      Field_Analysis_Time_Stamp => Type_Time_Stamp_Id,
      Field_Design_File_Source => Type_Source_File_Entry,
      Field_Library => Type_Iir,
      Field_File_Dependence_List => Type_Iir_List,
      Field_Design_File_Filename => Type_Name_Id,
      Field_Design_File_Directory => Type_Name_Id,
      Field_Design_File => Type_Iir,
      Field_Design_File_Chain => Type_Iir,
      Field_Library_Directory => Type_Name_Id,
      Field_Date => Type_Date_Type,
      Field_Context_Items => Type_Iir,
      Field_Dependence_List => Type_Iir_List,
      Field_Analysis_Checks_List => Type_Iir_List,
      Field_Date_State => Type_Date_State_Type,
      Field_Guarded_Target_State => Type_Tri_State_Type,
      Field_Library_Unit => Type_Iir,
      Field_Hash_Chain => Type_Iir,
      Field_Design_Unit_Source_Pos => Type_Source_Ptr,
      Field_Design_Unit_Source_Line => Type_Int32,
      Field_Design_Unit_Source_Col => Type_Int32,
      Field_Value => Type_Int64,
      Field_Enum_Pos => Type_Iir_Int32,
      Field_Physical_Literal => Type_Iir,
      Field_Fp_Value => Type_Fp64,
      Field_Simple_Aggregate_List => Type_Iir_Flist,
      Field_String8_Id => Type_String8_Id,
      Field_String_Length => Type_Int32,
      Field_Bit_String_Base => Type_Number_Base_Type,
      Field_Has_Signed => Type_Boolean,
      Field_Has_Sign => Type_Boolean,
      Field_Has_Length => Type_Boolean,
      Field_Literal_Length => Type_Int32,
      Field_Literal_Origin => Type_Iir,
      Field_Range_Origin => Type_Iir,
      Field_Literal_Subtype => Type_Iir,
      Field_Allocator_Subtype => Type_Iir,
      Field_Entity_Class => Type_Token_Type,
      Field_Entity_Name_List => Type_Iir_Flist,
      Field_Attribute_Designator => Type_Iir,
      Field_Attribute_Specification_Chain => Type_Iir,
      Field_Attribute_Specification => Type_Iir,
      Field_Static_Attribute_Flag => Type_Boolean,
      Field_Signal_List => Type_Iir_Flist,
      Field_Quantity_List => Type_Iir_Flist,
      Field_Designated_Entity => Type_Iir,
      Field_Formal => Type_Iir,
      Field_Actual => Type_Iir,
      Field_Actual_Conversion => Type_Iir,
      Field_Formal_Conversion => Type_Iir,
      Field_Whole_Association_Flag => Type_Boolean,
      Field_Collapse_Signal_Flag => Type_Boolean,
      Field_Artificial_Flag => Type_Boolean,
      Field_Open_Flag => Type_Boolean,
      Field_After_Drivers_Flag => Type_Boolean,
      Field_We_Value => Type_Iir,
      Field_Time => Type_Iir,
      Field_Associated_Expr => Type_Iir,
      Field_Associated_Block => Type_Iir,
      Field_Associated_Chain => Type_Iir,
      Field_Choice_Name => Type_Iir,
      Field_Choice_Expression => Type_Iir,
      Field_Choice_Range => Type_Iir,
      Field_Same_Alternative_Flag => Type_Boolean,
      Field_Element_Type_Flag => Type_Boolean,
      Field_Architecture => Type_Iir,
      Field_Block_Specification => Type_Iir,
      Field_Prev_Block_Configuration => Type_Iir,
      Field_Configuration_Item_Chain => Type_Iir,
      Field_Attribute_Value_Chain => Type_Iir,
      Field_Spec_Chain => Type_Iir,
      Field_Value_Chain => Type_Iir,
      Field_Attribute_Value_Spec_Chain => Type_Iir,
      Field_Entity_Name => Type_Iir,
      Field_Package => Type_Iir,
      Field_Package_Body => Type_Iir,
      Field_Instance_Package_Body => Type_Iir,
      Field_Need_Body => Type_Boolean,
      Field_Macro_Expanded_Flag => Type_Boolean,
      Field_Need_Instance_Bodies => Type_Boolean,
      Field_Hierarchical_Name => Type_Iir,
      Field_Vunit_Item_Chain => Type_Iir,
      Field_Bound_Vunit_Chain => Type_Iir,
      Field_Verification_Block_Configuration => Type_Iir,
      Field_Block_Configuration => Type_Iir,
      Field_Concurrent_Statement_Chain => Type_Iir,
      Field_Chain => Type_Iir,
      Field_Port_Chain => Type_Iir,
      Field_Generic_Chain => Type_Iir,
      Field_Type => Type_Iir,
      Field_Subtype_Indication => Type_Iir,
      Field_Discrete_Range => Type_Iir,
      Field_Type_Definition => Type_Iir,
      Field_Subtype_Definition => Type_Iir,
      Field_Incomplete_Type_Declaration => Type_Iir,
      Field_Interface_Type_Subprograms => Type_Iir,
      Field_Nature_Definition => Type_Iir,
      Field_Nature => Type_Iir,
      Field_Subnature_Indication => Type_Iir,
      Field_Mode => Type_Iir_Mode,
      Field_Guarded_Signal_Flag => Type_Boolean,
      Field_Signal_Kind => Type_Iir_Signal_Kind,
      Field_Base_Name => Type_Iir,
      Field_Interface_Declaration_Chain => Type_Iir,
      Field_Subprogram_Specification => Type_Iir,
      Field_Sequential_Statement_Chain => Type_Iir,
      Field_Simultaneous_Statement_Chain => Type_Iir,
      Field_Subprogram_Body => Type_Iir,
      Field_Overload_Number => Type_Iir_Int32,
      Field_Subprogram_Depth => Type_Iir_Int32,
      Field_Subprogram_Hash => Type_Iir_Int32,
      Field_Impure_Depth => Type_Iir_Int32,
      Field_Return_Type => Type_Iir,
      Field_Implicit_Definition => Type_Iir_Predefined_Functions,
      Field_Uninstantiated_Subprogram_Name => Type_Iir,
      Field_Default_Value => Type_Iir,
      Field_Deferred_Declaration => Type_Iir,
      Field_Deferred_Declaration_Flag => Type_Boolean,
      Field_Shared_Flag => Type_Boolean,
      Field_Design_Unit => Type_Iir,
      Field_Block_Statement => Type_Iir,
      Field_Signal_Driver => Type_Iir,
      Field_Declaration_Chain => Type_Iir,
      Field_File_Logical_Name => Type_Iir,
      Field_File_Open_Kind => Type_Iir,
      Field_Element_Position => Type_Iir_Index32,
      Field_Use_Clause_Chain => Type_Iir,
      Field_Context_Reference_Chain => Type_Iir,
      Field_Inherit_Spec_Chain => Type_Iir,
      Field_Selected_Name => Type_Iir,
      Field_Type_Declarator => Type_Iir,
      Field_Complete_Type_Definition => Type_Iir,
      Field_Incomplete_Type_Ref_Chain => Type_Iir,
      Field_Associated_Type => Type_Iir,
      Field_Enumeration_Literal_List => Type_Iir_Flist,
      Field_Entity_Class_Entry_Chain => Type_Iir,
      Field_Group_Constituent_List => Type_Iir_Flist,
      Field_Unit_Chain => Type_Iir,
      Field_Primary_Unit => Type_Iir,
      Field_Identifier => Type_Name_Id,
      Field_Label => Type_Name_Id,
      Field_Return_Identifier => Type_Iir,
      Field_Visible_Flag => Type_Boolean,
      Field_Range_Constraint => Type_Iir,
      Field_Direction => Type_Direction_Type,
      Field_Left_Limit => Type_Iir,
      Field_Right_Limit => Type_Iir,
      Field_Left_Limit_Expr => Type_Iir,
      Field_Right_Limit_Expr => Type_Iir,
      Field_Parent_Type => Type_Iir,
      Field_Simple_Nature => Type_Iir,
      Field_Base_Nature => Type_Iir,
      Field_Resolution_Indication => Type_Iir,
      Field_Record_Element_Resolution_Chain => Type_Iir,
      Field_Tolerance => Type_Iir,
      Field_Plus_Terminal_Name => Type_Iir,
      Field_Minus_Terminal_Name => Type_Iir,
      Field_Plus_Terminal => Type_Iir,
      Field_Minus_Terminal => Type_Iir,
      Field_Magnitude_Expression => Type_Iir,
      Field_Phase_Expression => Type_Iir,
      Field_Power_Expression => Type_Iir,
      Field_Simultaneous_Left => Type_Iir,
      Field_Simultaneous_Right => Type_Iir,
      Field_Text_File_Flag => Type_Boolean,
      Field_Only_Characters_Flag => Type_Boolean,
      Field_Is_Character_Type => Type_Boolean,
      Field_Nature_Staticness => Type_Iir_Staticness,
      Field_Type_Staticness => Type_Iir_Staticness,
      Field_Constraint_State => Type_Iir_Constraint,
      Field_Index_Subtype_List => Type_Iir_Flist,
      Field_Index_Subtype_Definition_List => Type_Iir_Flist,
      Field_Element_Subtype_Indication => Type_Iir,
      Field_Element_Subtype => Type_Iir,
      Field_Element_Subnature_Indication => Type_Iir,
      Field_Element_Subnature => Type_Iir,
      Field_Index_Constraint_List => Type_Iir_Flist,
      Field_Array_Element_Constraint => Type_Iir,
      Field_Has_Array_Constraint_Flag => Type_Boolean,
      Field_Has_Element_Constraint_Flag => Type_Boolean,
      Field_Elements_Declaration_List => Type_Iir_Flist,
      Field_Owned_Elements_Chain => Type_Iir,
      Field_Designated_Type => Type_Iir,
      Field_Designated_Subtype_Indication => Type_Iir,
      Field_Index_List => Type_Iir_Flist,
      Field_Reference => Type_Iir,
      Field_Nature_Declarator => Type_Iir,
      Field_Across_Type_Mark => Type_Iir,
      Field_Through_Type_Mark => Type_Iir,
      Field_Across_Type_Definition => Type_Iir,
      Field_Through_Type_Definition => Type_Iir,
      Field_Across_Type => Type_Iir,
      Field_Through_Type => Type_Iir,
      Field_Target => Type_Iir,
      Field_Waveform_Chain => Type_Iir,
      Field_Guard => Type_Iir,
      Field_Delay_Mechanism => Type_Iir_Delay_Mechanism,
      Field_Reject_Time_Expression => Type_Iir,
      Field_Force_Mode => Type_Iir_Force_Mode,
      Field_Has_Force_Mode => Type_Boolean,
      Field_Sensitivity_List => Type_Iir_List,
      Field_Process_Origin => Type_Iir,
      Field_Package_Origin => Type_Iir,
      Field_Condition_Clause => Type_Iir,
      Field_Break_Element => Type_Iir,
      Field_Selector_Quantity => Type_Iir,
      Field_Break_Quantity => Type_Iir,
      Field_Timeout_Clause => Type_Iir,
      Field_Postponed_Flag => Type_Boolean,
      Field_Callees_List => Type_Iir_List,
      Field_Passive_Flag => Type_Boolean,
      Field_Resolution_Function_Flag => Type_Boolean,
      Field_Wait_State => Type_Tri_State_Type,
      Field_All_Sensitized_State => Type_Iir_All_Sensitized,
      Field_Seen_Flag => Type_Boolean,
      Field_Pure_Flag => Type_Boolean,
      Field_Foreign_Flag => Type_Boolean,
      Field_Resolved_Flag => Type_Boolean,
      Field_Signal_Type_Flag => Type_Boolean,
      Field_Has_Signal_Flag => Type_Boolean,
      Field_Purity_State => Type_Iir_Pure_State,
      Field_Elab_Flag => Type_Boolean,
      Field_Vendor_Library_Flag => Type_Boolean,
      Field_Configuration_Mark_Flag => Type_Boolean,
      Field_Configuration_Done_Flag => Type_Boolean,
      Field_Index_Constraint_Flag => Type_Boolean,
      Field_Hide_Implicit_Flag => Type_Boolean,
      Field_Assertion_Condition => Type_Iir,
      Field_Report_Expression => Type_Iir,
      Field_Severity_Expression => Type_Iir,
      Field_Instantiated_Unit => Type_Iir,
      Field_Generic_Map_Aspect_Chain => Type_Iir,
      Field_Port_Map_Aspect_Chain => Type_Iir,
      Field_Configuration_Name => Type_Iir,
      Field_Component_Configuration => Type_Iir,
      Field_Configuration_Specification => Type_Iir,
      Field_Default_Binding_Indication => Type_Iir,
      Field_Default_Configuration_Declaration => Type_Iir,
      Field_Expression => Type_Iir,
      Field_Conditional_Expression_Chain => Type_Iir,
      Field_Allocator_Designated_Type => Type_Iir,
      Field_Selected_Waveform_Chain => Type_Iir,
      Field_Conditional_Waveform_Chain => Type_Iir,
      Field_Guard_Expression => Type_Iir,
      Field_Guard_Decl => Type_Iir,
      Field_Guard_Sensitivity_List => Type_Iir_List,
      Field_Signal_Attribute_Chain => Type_Iir,
      Field_Block_Block_Configuration => Type_Iir,
      Field_Package_Header => Type_Iir,
      Field_Block_Header => Type_Iir,
      Field_Uninstantiated_Package_Name => Type_Iir,
      Field_Uninstantiated_Package_Decl => Type_Iir,
      Field_Instance_Source_File => Type_Source_File_Entry,
      Field_Generate_Block_Configuration => Type_Iir,
      Field_Generate_Statement_Body => Type_Iir,
      Field_Alternative_Label => Type_Name_Id,
      Field_Generate_Else_Clause => Type_Iir,
      Field_Condition => Type_Iir,
      Field_Else_Clause => Type_Iir,
      Field_Parameter_Specification => Type_Iir,
      Field_Parent => Type_Iir,
      Field_Loop_Label => Type_Iir,
      Field_Exit_Flag => Type_Boolean,
      Field_Next_Flag => Type_Boolean,
      Field_Component_Name => Type_Iir,
      Field_Instantiation_List => Type_Iir_Flist,
      Field_Entity_Aspect => Type_Iir,
      Field_Default_Entity_Aspect => Type_Iir,
      Field_Binding_Indication => Type_Iir,
      Field_Named_Entity => Type_Iir,
      Field_Referenced_Name => Type_Iir,
      Field_Expr_Staticness => Type_Iir_Staticness,
      Field_Scalar_Size => Type_Scalar_Size,
      Field_Error_Origin => Type_Iir,
      Field_Operand => Type_Iir,
      Field_Left => Type_Iir,
      Field_Right => Type_Iir,
      Field_Unit_Name => Type_Iir,
      Field_Name => Type_Iir,
      Field_Group_Template_Name => Type_Iir,
      Field_Name_Staticness => Type_Iir_Staticness,
      Field_Prefix => Type_Iir,
      Field_Signature_Prefix => Type_Iir,
      Field_External_Pathname => Type_Iir,
      Field_Pathname_Suffix => Type_Iir,
      Field_Pathname_Expression => Type_Iir,
      Field_In_Formal_Flag => Type_Boolean,
      Field_Slice_Subtype => Type_Iir,
      Field_Suffix => Type_Iir,
      Field_Index_Subtype => Type_Iir,
      Field_Parameter => Type_Iir,
      Field_Parameter_2 => Type_Iir,
      Field_Parameter_3 => Type_Iir,
      Field_Parameter_4 => Type_Iir,
      Field_Attr_Chain => Type_Iir,
      Field_Signal_Attribute_Declaration => Type_Iir,
      Field_Actual_Type => Type_Iir,
      Field_Actual_Type_Definition => Type_Iir,
      Field_Association_Chain => Type_Iir,
      Field_Individual_Association_Chain => Type_Iir,
      Field_Subprogram_Association_Chain => Type_Iir,
      Field_Aggregate_Info => Type_Iir,
      Field_Sub_Aggregate_Info => Type_Iir,
      Field_Aggr_Dynamic_Flag => Type_Boolean,
      Field_Aggr_Min_Length => Type_Iir_Int32,
      Field_Aggr_Low_Limit => Type_Iir,
      Field_Aggr_High_Limit => Type_Iir,
      Field_Aggr_Others_Flag => Type_Boolean,
      Field_Aggr_Named_Flag => Type_Boolean,
      Field_Aggregate_Expand_Flag => Type_Boolean,
      Field_Association_Choices_Chain => Type_Iir,
      Field_Case_Statement_Alternative_Chain => Type_Iir,
      Field_Matching_Flag => Type_Boolean,
      Field_Choice_Staticness => Type_Iir_Staticness,
      Field_Procedure_Call => Type_Iir,
      Field_Implementation => Type_Iir,
      Field_Parameter_Association_Chain => Type_Iir,
      Field_Method_Object => Type_Iir,
      Field_Subtype_Type_Mark => Type_Iir,
      Field_Subnature_Nature_Mark => Type_Iir,
      Field_Type_Conversion_Subtype => Type_Iir,
      Field_Type_Mark => Type_Iir,
      Field_File_Type_Mark => Type_Iir,
      Field_Return_Type_Mark => Type_Iir,
      Field_Has_Disconnect_Flag => Type_Boolean,
      Field_Has_Active_Flag => Type_Boolean,
      Field_Is_Within_Flag => Type_Boolean,
      Field_Type_Marks_List => Type_Iir_Flist,
      Field_Implicit_Alias_Flag => Type_Boolean,
      Field_Alias_Signature => Type_Iir,
      Field_Attribute_Signature => Type_Iir,
      Field_Overload_List => Type_Iir_List,
      Field_Simple_Name_Identifier => Type_Name_Id,
      Field_Simple_Name_Subtype => Type_Iir,
      Field_Protected_Type_Body => Type_Iir,
      Field_Protected_Type_Declaration => Type_Iir,
      Field_Use_Flag => Type_Boolean,
      Field_End_Has_Reserved_Id => Type_Boolean,
      Field_End_Has_Identifier => Type_Boolean,
      Field_End_Has_Postponed => Type_Boolean,
      Field_Has_Label => Type_Boolean,
      Field_Has_Begin => Type_Boolean,
      Field_Has_End => Type_Boolean,
      Field_Has_Is => Type_Boolean,
      Field_Has_Pure => Type_Boolean,
      Field_Has_Body => Type_Boolean,
      Field_Has_Parameter => Type_Boolean,
      Field_Has_Component => Type_Boolean,
      Field_Has_Identifier_List => Type_Boolean,
      Field_Has_Mode => Type_Boolean,
      Field_Has_Class => Type_Boolean,
      Field_Has_Delay_Mechanism => Type_Boolean,
      Field_Suspend_Flag => Type_Boolean,
      Field_Is_Ref => Type_Boolean,
      Field_Is_Forward_Ref => Type_Boolean,
      Field_Psl_Property => Type_PSL_Node,
      Field_Psl_Sequence => Type_PSL_Node,
      Field_Psl_Declaration => Type_PSL_Node,
      Field_Psl_Expression => Type_PSL_Node,
      Field_Psl_Boolean => Type_PSL_Node,
      Field_PSL_Clock => Type_PSL_Node,
      Field_PSL_NFA => Type_PSL_NFA,
      Field_PSL_Nbr_States => Type_Int32,
      Field_PSL_Clock_Sensitivity => Type_Iir_List,
      Field_PSL_EOS_Flag => Type_Boolean,
      Field_PSL_Abort_Flag => Type_Boolean,
      Field_Count_Expression => Type_Iir,
      Field_Clock_Expression => Type_Iir,
      Field_Default_Clock => Type_Iir,
      Field_Foreign_Node => Type_Int32,
      Field_Suspend_State_Index => Type_Int32,
      Field_Suspend_State_Chain => Type_Iir
     );

   function Get_Field_Type (F : Fields_Enum) return Types_Enum is
   begin
      return Fields_Type (F);
   end Get_Field_Type;

   function Get_Field_Image (F : Fields_Enum) return String is
   begin
      case F is
         when Field_First_Design_Unit =>
            return "first_design_unit";
         when Field_Last_Design_Unit =>
            return "last_design_unit";
         when Field_Library_Declaration =>
            return "library_declaration";
         when Field_File_Checksum =>
            return "file_checksum";
         when Field_Analysis_Time_Stamp =>
            return "analysis_time_stamp";
         when Field_Design_File_Source =>
            return "design_file_source";
         when Field_Library =>
            return "library";
         when Field_File_Dependence_List =>
            return "file_dependence_list";
         when Field_Design_File_Filename =>
            return "design_file_filename";
         when Field_Design_File_Directory =>
            return "design_file_directory";
         when Field_Design_File =>
            return "design_file";
         when Field_Design_File_Chain =>
            return "design_file_chain";
         when Field_Library_Directory =>
            return "library_directory";
         when Field_Date =>
            return "date";
         when Field_Context_Items =>
            return "context_items";
         when Field_Dependence_List =>
            return "dependence_list";
         when Field_Analysis_Checks_List =>
            return "analysis_checks_list";
         when Field_Date_State =>
            return "date_state";
         when Field_Guarded_Target_State =>
            return "guarded_target_state";
         when Field_Library_Unit =>
            return "library_unit";
         when Field_Hash_Chain =>
            return "hash_chain";
         when Field_Design_Unit_Source_Pos =>
            return "design_unit_source_pos";
         when Field_Design_Unit_Source_Line =>
            return "design_unit_source_line";
         when Field_Design_Unit_Source_Col =>
            return "design_unit_source_col";
         when Field_Value =>
            return "value";
         when Field_Enum_Pos =>
            return "enum_pos";
         when Field_Physical_Literal =>
            return "physical_literal";
         when Field_Fp_Value =>
            return "fp_value";
         when Field_Simple_Aggregate_List =>
            return "simple_aggregate_list";
         when Field_String8_Id =>
            return "string8_id";
         when Field_String_Length =>
            return "string_length";
         when Field_Bit_String_Base =>
            return "bit_string_base";
         when Field_Has_Signed =>
            return "has_signed";
         when Field_Has_Sign =>
            return "has_sign";
         when Field_Has_Length =>
            return "has_length";
         when Field_Literal_Length =>
            return "literal_length";
         when Field_Literal_Origin =>
            return "literal_origin";
         when Field_Range_Origin =>
            return "range_origin";
         when Field_Literal_Subtype =>
            return "literal_subtype";
         when Field_Allocator_Subtype =>
            return "allocator_subtype";
         when Field_Entity_Class =>
            return "entity_class";
         when Field_Entity_Name_List =>
            return "entity_name_list";
         when Field_Attribute_Designator =>
            return "attribute_designator";
         when Field_Attribute_Specification_Chain =>
            return "attribute_specification_chain";
         when Field_Attribute_Specification =>
            return "attribute_specification";
         when Field_Static_Attribute_Flag =>
            return "static_attribute_flag";
         when Field_Signal_List =>
            return "signal_list";
         when Field_Quantity_List =>
            return "quantity_list";
         when Field_Designated_Entity =>
            return "designated_entity";
         when Field_Formal =>
            return "formal";
         when Field_Actual =>
            return "actual";
         when Field_Actual_Conversion =>
            return "actual_conversion";
         when Field_Formal_Conversion =>
            return "formal_conversion";
         when Field_Whole_Association_Flag =>
            return "whole_association_flag";
         when Field_Collapse_Signal_Flag =>
            return "collapse_signal_flag";
         when Field_Artificial_Flag =>
            return "artificial_flag";
         when Field_Open_Flag =>
            return "open_flag";
         when Field_After_Drivers_Flag =>
            return "after_drivers_flag";
         when Field_We_Value =>
            return "we_value";
         when Field_Time =>
            return "time";
         when Field_Associated_Expr =>
            return "associated_expr";
         when Field_Associated_Block =>
            return "associated_block";
         when Field_Associated_Chain =>
            return "associated_chain";
         when Field_Choice_Name =>
            return "choice_name";
         when Field_Choice_Expression =>
            return "choice_expression";
         when Field_Choice_Range =>
            return "choice_range";
         when Field_Same_Alternative_Flag =>
            return "same_alternative_flag";
         when Field_Element_Type_Flag =>
            return "element_type_flag";
         when Field_Architecture =>
            return "architecture";
         when Field_Block_Specification =>
            return "block_specification";
         when Field_Prev_Block_Configuration =>
            return "prev_block_configuration";
         when Field_Configuration_Item_Chain =>
            return "configuration_item_chain";
         when Field_Attribute_Value_Chain =>
            return "attribute_value_chain";
         when Field_Spec_Chain =>
            return "spec_chain";
         when Field_Value_Chain =>
            return "value_chain";
         when Field_Attribute_Value_Spec_Chain =>
            return "attribute_value_spec_chain";
         when Field_Entity_Name =>
            return "entity_name";
         when Field_Package =>
            return "package";
         when Field_Package_Body =>
            return "package_body";
         when Field_Instance_Package_Body =>
            return "instance_package_body";
         when Field_Need_Body =>
            return "need_body";
         when Field_Macro_Expanded_Flag =>
            return "macro_expanded_flag";
         when Field_Need_Instance_Bodies =>
            return "need_instance_bodies";
         when Field_Hierarchical_Name =>
            return "hierarchical_name";
         when Field_Vunit_Item_Chain =>
            return "vunit_item_chain";
         when Field_Bound_Vunit_Chain =>
            return "bound_vunit_chain";
         when Field_Verification_Block_Configuration =>
            return "verification_block_configuration";
         when Field_Block_Configuration =>
            return "block_configuration";
         when Field_Concurrent_Statement_Chain =>
            return "concurrent_statement_chain";
         when Field_Chain =>
            return "chain";
         when Field_Port_Chain =>
            return "port_chain";
         when Field_Generic_Chain =>
            return "generic_chain";
         when Field_Type =>
            return "type";
         when Field_Subtype_Indication =>
            return "subtype_indication";
         when Field_Discrete_Range =>
            return "discrete_range";
         when Field_Type_Definition =>
            return "type_definition";
         when Field_Subtype_Definition =>
            return "subtype_definition";
         when Field_Incomplete_Type_Declaration =>
            return "incomplete_type_declaration";
         when Field_Interface_Type_Subprograms =>
            return "interface_type_subprograms";
         when Field_Nature_Definition =>
            return "nature_definition";
         when Field_Nature =>
            return "nature";
         when Field_Subnature_Indication =>
            return "subnature_indication";
         when Field_Mode =>
            return "mode";
         when Field_Guarded_Signal_Flag =>
            return "guarded_signal_flag";
         when Field_Signal_Kind =>
            return "signal_kind";
         when Field_Base_Name =>
            return "base_name";
         when Field_Interface_Declaration_Chain =>
            return "interface_declaration_chain";
         when Field_Subprogram_Specification =>
            return "subprogram_specification";
         when Field_Sequential_Statement_Chain =>
            return "sequential_statement_chain";
         when Field_Simultaneous_Statement_Chain =>
            return "simultaneous_statement_chain";
         when Field_Subprogram_Body =>
            return "subprogram_body";
         when Field_Overload_Number =>
            return "overload_number";
         when Field_Subprogram_Depth =>
            return "subprogram_depth";
         when Field_Subprogram_Hash =>
            return "subprogram_hash";
         when Field_Impure_Depth =>
            return "impure_depth";
         when Field_Return_Type =>
            return "return_type";
         when Field_Implicit_Definition =>
            return "implicit_definition";
         when Field_Uninstantiated_Subprogram_Name =>
            return "uninstantiated_subprogram_name";
         when Field_Default_Value =>
            return "default_value";
         when Field_Deferred_Declaration =>
            return "deferred_declaration";
         when Field_Deferred_Declaration_Flag =>
            return "deferred_declaration_flag";
         when Field_Shared_Flag =>
            return "shared_flag";
         when Field_Design_Unit =>
            return "design_unit";
         when Field_Block_Statement =>
            return "block_statement";
         when Field_Signal_Driver =>
            return "signal_driver";
         when Field_Declaration_Chain =>
            return "declaration_chain";
         when Field_File_Logical_Name =>
            return "file_logical_name";
         when Field_File_Open_Kind =>
            return "file_open_kind";
         when Field_Element_Position =>
            return "element_position";
         when Field_Use_Clause_Chain =>
            return "use_clause_chain";
         when Field_Context_Reference_Chain =>
            return "context_reference_chain";
         when Field_Inherit_Spec_Chain =>
            return "inherit_spec_chain";
         when Field_Selected_Name =>
            return "selected_name";
         when Field_Type_Declarator =>
            return "type_declarator";
         when Field_Complete_Type_Definition =>
            return "complete_type_definition";
         when Field_Incomplete_Type_Ref_Chain =>
            return "incomplete_type_ref_chain";
         when Field_Associated_Type =>
            return "associated_type";
         when Field_Enumeration_Literal_List =>
            return "enumeration_literal_list";
         when Field_Entity_Class_Entry_Chain =>
            return "entity_class_entry_chain";
         when Field_Group_Constituent_List =>
            return "group_constituent_list";
         when Field_Unit_Chain =>
            return "unit_chain";
         when Field_Primary_Unit =>
            return "primary_unit";
         when Field_Identifier =>
            return "identifier";
         when Field_Label =>
            return "label";
         when Field_Return_Identifier =>
            return "return_identifier";
         when Field_Visible_Flag =>
            return "visible_flag";
         when Field_Range_Constraint =>
            return "range_constraint";
         when Field_Direction =>
            return "direction";
         when Field_Left_Limit =>
            return "left_limit";
         when Field_Right_Limit =>
            return "right_limit";
         when Field_Left_Limit_Expr =>
            return "left_limit_expr";
         when Field_Right_Limit_Expr =>
            return "right_limit_expr";
         when Field_Parent_Type =>
            return "parent_type";
         when Field_Simple_Nature =>
            return "simple_nature";
         when Field_Base_Nature =>
            return "base_nature";
         when Field_Resolution_Indication =>
            return "resolution_indication";
         when Field_Record_Element_Resolution_Chain =>
            return "record_element_resolution_chain";
         when Field_Tolerance =>
            return "tolerance";
         when Field_Plus_Terminal_Name =>
            return "plus_terminal_name";
         when Field_Minus_Terminal_Name =>
            return "minus_terminal_name";
         when Field_Plus_Terminal =>
            return "plus_terminal";
         when Field_Minus_Terminal =>
            return "minus_terminal";
         when Field_Magnitude_Expression =>
            return "magnitude_expression";
         when Field_Phase_Expression =>
            return "phase_expression";
         when Field_Power_Expression =>
            return "power_expression";
         when Field_Simultaneous_Left =>
            return "simultaneous_left";
         when Field_Simultaneous_Right =>
            return "simultaneous_right";
         when Field_Text_File_Flag =>
            return "text_file_flag";
         when Field_Only_Characters_Flag =>
            return "only_characters_flag";
         when Field_Is_Character_Type =>
            return "is_character_type";
         when Field_Nature_Staticness =>
            return "nature_staticness";
         when Field_Type_Staticness =>
            return "type_staticness";
         when Field_Constraint_State =>
            return "constraint_state";
         when Field_Index_Subtype_List =>
            return "index_subtype_list";
         when Field_Index_Subtype_Definition_List =>
            return "index_subtype_definition_list";
         when Field_Element_Subtype_Indication =>
            return "element_subtype_indication";
         when Field_Element_Subtype =>
            return "element_subtype";
         when Field_Element_Subnature_Indication =>
            return "element_subnature_indication";
         when Field_Element_Subnature =>
            return "element_subnature";
         when Field_Index_Constraint_List =>
            return "index_constraint_list";
         when Field_Array_Element_Constraint =>
            return "array_element_constraint";
         when Field_Has_Array_Constraint_Flag =>
            return "has_array_constraint_flag";
         when Field_Has_Element_Constraint_Flag =>
            return "has_element_constraint_flag";
         when Field_Elements_Declaration_List =>
            return "elements_declaration_list";
         when Field_Owned_Elements_Chain =>
            return "owned_elements_chain";
         when Field_Designated_Type =>
            return "designated_type";
         when Field_Designated_Subtype_Indication =>
            return "designated_subtype_indication";
         when Field_Index_List =>
            return "index_list";
         when Field_Reference =>
            return "reference";
         when Field_Nature_Declarator =>
            return "nature_declarator";
         when Field_Across_Type_Mark =>
            return "across_type_mark";
         when Field_Through_Type_Mark =>
            return "through_type_mark";
         when Field_Across_Type_Definition =>
            return "across_type_definition";
         when Field_Through_Type_Definition =>
            return "through_type_definition";
         when Field_Across_Type =>
            return "across_type";
         when Field_Through_Type =>
            return "through_type";
         when Field_Target =>
            return "target";
         when Field_Waveform_Chain =>
            return "waveform_chain";
         when Field_Guard =>
            return "guard";
         when Field_Delay_Mechanism =>
            return "delay_mechanism";
         when Field_Reject_Time_Expression =>
            return "reject_time_expression";
         when Field_Force_Mode =>
            return "force_mode";
         when Field_Has_Force_Mode =>
            return "has_force_mode";
         when Field_Sensitivity_List =>
            return "sensitivity_list";
         when Field_Process_Origin =>
            return "process_origin";
         when Field_Package_Origin =>
            return "package_origin";
         when Field_Condition_Clause =>
            return "condition_clause";
         when Field_Break_Element =>
            return "break_element";
         when Field_Selector_Quantity =>
            return "selector_quantity";
         when Field_Break_Quantity =>
            return "break_quantity";
         when Field_Timeout_Clause =>
            return "timeout_clause";
         when Field_Postponed_Flag =>
            return "postponed_flag";
         when Field_Callees_List =>
            return "callees_list";
         when Field_Passive_Flag =>
            return "passive_flag";
         when Field_Resolution_Function_Flag =>
            return "resolution_function_flag";
         when Field_Wait_State =>
            return "wait_state";
         when Field_All_Sensitized_State =>
            return "all_sensitized_state";
         when Field_Seen_Flag =>
            return "seen_flag";
         when Field_Pure_Flag =>
            return "pure_flag";
         when Field_Foreign_Flag =>
            return "foreign_flag";
         when Field_Resolved_Flag =>
            return "resolved_flag";
         when Field_Signal_Type_Flag =>
            return "signal_type_flag";
         when Field_Has_Signal_Flag =>
            return "has_signal_flag";
         when Field_Purity_State =>
            return "purity_state";
         when Field_Elab_Flag =>
            return "elab_flag";
         when Field_Vendor_Library_Flag =>
            return "vendor_library_flag";
         when Field_Configuration_Mark_Flag =>
            return "configuration_mark_flag";
         when Field_Configuration_Done_Flag =>
            return "configuration_done_flag";
         when Field_Index_Constraint_Flag =>
            return "index_constraint_flag";
         when Field_Hide_Implicit_Flag =>
            return "hide_implicit_flag";
         when Field_Assertion_Condition =>
            return "assertion_condition";
         when Field_Report_Expression =>
            return "report_expression";
         when Field_Severity_Expression =>
            return "severity_expression";
         when Field_Instantiated_Unit =>
            return "instantiated_unit";
         when Field_Generic_Map_Aspect_Chain =>
            return "generic_map_aspect_chain";
         when Field_Port_Map_Aspect_Chain =>
            return "port_map_aspect_chain";
         when Field_Configuration_Name =>
            return "configuration_name";
         when Field_Component_Configuration =>
            return "component_configuration";
         when Field_Configuration_Specification =>
            return "configuration_specification";
         when Field_Default_Binding_Indication =>
            return "default_binding_indication";
         when Field_Default_Configuration_Declaration =>
            return "default_configuration_declaration";
         when Field_Expression =>
            return "expression";
         when Field_Conditional_Expression_Chain =>
            return "conditional_expression_chain";
         when Field_Allocator_Designated_Type =>
            return "allocator_designated_type";
         when Field_Selected_Waveform_Chain =>
            return "selected_waveform_chain";
         when Field_Conditional_Waveform_Chain =>
            return "conditional_waveform_chain";
         when Field_Guard_Expression =>
            return "guard_expression";
         when Field_Guard_Decl =>
            return "guard_decl";
         when Field_Guard_Sensitivity_List =>
            return "guard_sensitivity_list";
         when Field_Signal_Attribute_Chain =>
            return "signal_attribute_chain";
         when Field_Block_Block_Configuration =>
            return "block_block_configuration";
         when Field_Package_Header =>
            return "package_header";
         when Field_Block_Header =>
            return "block_header";
         when Field_Uninstantiated_Package_Name =>
            return "uninstantiated_package_name";
         when Field_Uninstantiated_Package_Decl =>
            return "uninstantiated_package_decl";
         when Field_Instance_Source_File =>
            return "instance_source_file";
         when Field_Generate_Block_Configuration =>
            return "generate_block_configuration";
         when Field_Generate_Statement_Body =>
            return "generate_statement_body";
         when Field_Alternative_Label =>
            return "alternative_label";
         when Field_Generate_Else_Clause =>
            return "generate_else_clause";
         when Field_Condition =>
            return "condition";
         when Field_Else_Clause =>
            return "else_clause";
         when Field_Parameter_Specification =>
            return "parameter_specification";
         when Field_Parent =>
            return "parent";
         when Field_Loop_Label =>
            return "loop_label";
         when Field_Exit_Flag =>
            return "exit_flag";
         when Field_Next_Flag =>
            return "next_flag";
         when Field_Component_Name =>
            return "component_name";
         when Field_Instantiation_List =>
            return "instantiation_list";
         when Field_Entity_Aspect =>
            return "entity_aspect";
         when Field_Default_Entity_Aspect =>
            return "default_entity_aspect";
         when Field_Binding_Indication =>
            return "binding_indication";
         when Field_Named_Entity =>
            return "named_entity";
         when Field_Referenced_Name =>
            return "referenced_name";
         when Field_Expr_Staticness =>
            return "expr_staticness";
         when Field_Scalar_Size =>
            return "scalar_size";
         when Field_Error_Origin =>
            return "error_origin";
         when Field_Operand =>
            return "operand";
         when Field_Left =>
            return "left";
         when Field_Right =>
            return "right";
         when Field_Unit_Name =>
            return "unit_name";
         when Field_Name =>
            return "name";
         when Field_Group_Template_Name =>
            return "group_template_name";
         when Field_Name_Staticness =>
            return "name_staticness";
         when Field_Prefix =>
            return "prefix";
         when Field_Signature_Prefix =>
            return "signature_prefix";
         when Field_External_Pathname =>
            return "external_pathname";
         when Field_Pathname_Suffix =>
            return "pathname_suffix";
         when Field_Pathname_Expression =>
            return "pathname_expression";
         when Field_In_Formal_Flag =>
            return "in_formal_flag";
         when Field_Slice_Subtype =>
            return "slice_subtype";
         when Field_Suffix =>
            return "suffix";
         when Field_Index_Subtype =>
            return "index_subtype";
         when Field_Parameter =>
            return "parameter";
         when Field_Parameter_2 =>
            return "parameter_2";
         when Field_Parameter_3 =>
            return "parameter_3";
         when Field_Parameter_4 =>
            return "parameter_4";
         when Field_Attr_Chain =>
            return "attr_chain";
         when Field_Signal_Attribute_Declaration =>
            return "signal_attribute_declaration";
         when Field_Actual_Type =>
            return "actual_type";
         when Field_Actual_Type_Definition =>
            return "actual_type_definition";
         when Field_Association_Chain =>
            return "association_chain";
         when Field_Individual_Association_Chain =>
            return "individual_association_chain";
         when Field_Subprogram_Association_Chain =>
            return "subprogram_association_chain";
         when Field_Aggregate_Info =>
            return "aggregate_info";
         when Field_Sub_Aggregate_Info =>
            return "sub_aggregate_info";
         when Field_Aggr_Dynamic_Flag =>
            return "aggr_dynamic_flag";
         when Field_Aggr_Min_Length =>
            return "aggr_min_length";
         when Field_Aggr_Low_Limit =>
            return "aggr_low_limit";
         when Field_Aggr_High_Limit =>
            return "aggr_high_limit";
         when Field_Aggr_Others_Flag =>
            return "aggr_others_flag";
         when Field_Aggr_Named_Flag =>
            return "aggr_named_flag";
         when Field_Aggregate_Expand_Flag =>
            return "aggregate_expand_flag";
         when Field_Association_Choices_Chain =>
            return "association_choices_chain";
         when Field_Case_Statement_Alternative_Chain =>
            return "case_statement_alternative_chain";
         when Field_Matching_Flag =>
            return "matching_flag";
         when Field_Choice_Staticness =>
            return "choice_staticness";
         when Field_Procedure_Call =>
            return "procedure_call";
         when Field_Implementation =>
            return "implementation";
         when Field_Parameter_Association_Chain =>
            return "parameter_association_chain";
         when Field_Method_Object =>
            return "method_object";
         when Field_Subtype_Type_Mark =>
            return "subtype_type_mark";
         when Field_Subnature_Nature_Mark =>
            return "subnature_nature_mark";
         when Field_Type_Conversion_Subtype =>
            return "type_conversion_subtype";
         when Field_Type_Mark =>
            return "type_mark";
         when Field_File_Type_Mark =>
            return "file_type_mark";
         when Field_Return_Type_Mark =>
            return "return_type_mark";
         when Field_Has_Disconnect_Flag =>
            return "has_disconnect_flag";
         when Field_Has_Active_Flag =>
            return "has_active_flag";
         when Field_Is_Within_Flag =>
            return "is_within_flag";
         when Field_Type_Marks_List =>
            return "type_marks_list";
         when Field_Implicit_Alias_Flag =>
            return "implicit_alias_flag";
         when Field_Alias_Signature =>
            return "alias_signature";
         when Field_Attribute_Signature =>
            return "attribute_signature";
         when Field_Overload_List =>
            return "overload_list";
         when Field_Simple_Name_Identifier =>
            return "simple_name_identifier";
         when Field_Simple_Name_Subtype =>
            return "simple_name_subtype";
         when Field_Protected_Type_Body =>
            return "protected_type_body";
         when Field_Protected_Type_Declaration =>
            return "protected_type_declaration";
         when Field_Use_Flag =>
            return "use_flag";
         when Field_End_Has_Reserved_Id =>
            return "end_has_reserved_id";
         when Field_End_Has_Identifier =>
            return "end_has_identifier";
         when Field_End_Has_Postponed =>
            return "end_has_postponed";
         when Field_Has_Label =>
            return "has_label";
         when Field_Has_Begin =>
            return "has_begin";
         when Field_Has_End =>
            return "has_end";
         when Field_Has_Is =>
            return "has_is";
         when Field_Has_Pure =>
            return "has_pure";
         when Field_Has_Body =>
            return "has_body";
         when Field_Has_Parameter =>
            return "has_parameter";
         when Field_Has_Component =>
            return "has_component";
         when Field_Has_Identifier_List =>
            return "has_identifier_list";
         when Field_Has_Mode =>
            return "has_mode";
         when Field_Has_Class =>
            return "has_class";
         when Field_Has_Delay_Mechanism =>
            return "has_delay_mechanism";
         when Field_Suspend_Flag =>
            return "suspend_flag";
         when Field_Is_Ref =>
            return "is_ref";
         when Field_Is_Forward_Ref =>
            return "is_forward_ref";
         when Field_Psl_Property =>
            return "psl_property";
         when Field_Psl_Sequence =>
            return "psl_sequence";
         when Field_Psl_Declaration =>
            return "psl_declaration";
         when Field_Psl_Expression =>
            return "psl_expression";
         when Field_Psl_Boolean =>
            return "psl_boolean";
         when Field_PSL_Clock =>
            return "psl_clock";
         when Field_PSL_NFA =>
            return "psl_nfa";
         when Field_PSL_Nbr_States =>
            return "psl_nbr_states";
         when Field_PSL_Clock_Sensitivity =>
            return "psl_clock_sensitivity";
         when Field_PSL_EOS_Flag =>
            return "psl_eos_flag";
         when Field_PSL_Abort_Flag =>
            return "psl_abort_flag";
         when Field_Count_Expression =>
            return "count_expression";
         when Field_Clock_Expression =>
            return "clock_expression";
         when Field_Default_Clock =>
            return "default_clock";
         when Field_Foreign_Node =>
            return "foreign_node";
         when Field_Suspend_State_Index =>
            return "suspend_state_index";
         when Field_Suspend_State_Chain =>
            return "suspend_state_chain";
      end case;
   end Get_Field_Image;

   function Get_Iir_Image (K : Iir_Kind) return String is
   begin
      case K is
         when Iir_Kind_Unused =>
            return "unused";
         when Iir_Kind_Error =>
            return "error";
         when Iir_Kind_Design_File =>
            return "design_file";
         when Iir_Kind_Design_Unit =>
            return "design_unit";
         when Iir_Kind_Library_Clause =>
            return "library_clause";
         when Iir_Kind_Use_Clause =>
            return "use_clause";
         when Iir_Kind_Context_Reference =>
            return "context_reference";
         when Iir_Kind_PSL_Inherit_Spec =>
            return "psl_inherit_spec";
         when Iir_Kind_Integer_Literal =>
            return "integer_literal";
         when Iir_Kind_Floating_Point_Literal =>
            return "floating_point_literal";
         when Iir_Kind_Null_Literal =>
            return "null_literal";
         when Iir_Kind_String_Literal8 =>
            return "string_literal8";
         when Iir_Kind_Physical_Int_Literal =>
            return "physical_int_literal";
         when Iir_Kind_Physical_Fp_Literal =>
            return "physical_fp_literal";
         when Iir_Kind_Simple_Aggregate =>
            return "simple_aggregate";
         when Iir_Kind_Overflow_Literal =>
            return "overflow_literal";
         when Iir_Kind_Unaffected_Waveform =>
            return "unaffected_waveform";
         when Iir_Kind_Waveform_Element =>
            return "waveform_element";
         when Iir_Kind_Conditional_Waveform =>
            return "conditional_waveform";
         when Iir_Kind_Conditional_Expression =>
            return "conditional_expression";
         when Iir_Kind_Association_Element_By_Expression =>
            return "association_element_by_expression";
         when Iir_Kind_Association_Element_By_Name =>
            return "association_element_by_name";
         when Iir_Kind_Association_Element_By_Individual =>
            return "association_element_by_individual";
         when Iir_Kind_Association_Element_Open =>
            return "association_element_open";
         when Iir_Kind_Association_Element_Package =>
            return "association_element_package";
         when Iir_Kind_Association_Element_Type =>
            return "association_element_type";
         when Iir_Kind_Association_Element_Subprogram =>
            return "association_element_subprogram";
         when Iir_Kind_Association_Element_Terminal =>
            return "association_element_terminal";
         when Iir_Kind_Choice_By_Range =>
            return "choice_by_range";
         when Iir_Kind_Choice_By_Expression =>
            return "choice_by_expression";
         when Iir_Kind_Choice_By_Others =>
            return "choice_by_others";
         when Iir_Kind_Choice_By_None =>
            return "choice_by_none";
         when Iir_Kind_Choice_By_Name =>
            return "choice_by_name";
         when Iir_Kind_Entity_Aspect_Entity =>
            return "entity_aspect_entity";
         when Iir_Kind_Entity_Aspect_Configuration =>
            return "entity_aspect_configuration";
         when Iir_Kind_Entity_Aspect_Open =>
            return "entity_aspect_open";
         when Iir_Kind_Psl_Hierarchical_Name =>
            return "psl_hierarchical_name";
         when Iir_Kind_Block_Configuration =>
            return "block_configuration";
         when Iir_Kind_Block_Header =>
            return "block_header";
         when Iir_Kind_Component_Configuration =>
            return "component_configuration";
         when Iir_Kind_Binding_Indication =>
            return "binding_indication";
         when Iir_Kind_Entity_Class =>
            return "entity_class";
         when Iir_Kind_Attribute_Value =>
            return "attribute_value";
         when Iir_Kind_Signature =>
            return "signature";
         when Iir_Kind_Aggregate_Info =>
            return "aggregate_info";
         when Iir_Kind_Procedure_Call =>
            return "procedure_call";
         when Iir_Kind_Record_Element_Constraint =>
            return "record_element_constraint";
         when Iir_Kind_Array_Element_Resolution =>
            return "array_element_resolution";
         when Iir_Kind_Record_Resolution =>
            return "record_resolution";
         when Iir_Kind_Record_Element_Resolution =>
            return "record_element_resolution";
         when Iir_Kind_Break_Element =>
            return "break_element";
         when Iir_Kind_Attribute_Specification =>
            return "attribute_specification";
         when Iir_Kind_Disconnection_Specification =>
            return "disconnection_specification";
         when Iir_Kind_Step_Limit_Specification =>
            return "step_limit_specification";
         when Iir_Kind_Configuration_Specification =>
            return "configuration_specification";
         when Iir_Kind_Access_Type_Definition =>
            return "access_type_definition";
         when Iir_Kind_Incomplete_Type_Definition =>
            return "incomplete_type_definition";
         when Iir_Kind_Interface_Type_Definition =>
            return "interface_type_definition";
         when Iir_Kind_File_Type_Definition =>
            return "file_type_definition";
         when Iir_Kind_Protected_Type_Declaration =>
            return "protected_type_declaration";
         when Iir_Kind_Record_Type_Definition =>
            return "record_type_definition";
         when Iir_Kind_Array_Type_Definition =>
            return "array_type_definition";
         when Iir_Kind_Array_Subtype_Definition =>
            return "array_subtype_definition";
         when Iir_Kind_Record_Subtype_Definition =>
            return "record_subtype_definition";
         when Iir_Kind_Access_Subtype_Definition =>
            return "access_subtype_definition";
         when Iir_Kind_Physical_Subtype_Definition =>
            return "physical_subtype_definition";
         when Iir_Kind_Floating_Subtype_Definition =>
            return "floating_subtype_definition";
         when Iir_Kind_Integer_Subtype_Definition =>
            return "integer_subtype_definition";
         when Iir_Kind_Enumeration_Subtype_Definition =>
            return "enumeration_subtype_definition";
         when Iir_Kind_Enumeration_Type_Definition =>
            return "enumeration_type_definition";
         when Iir_Kind_Integer_Type_Definition =>
            return "integer_type_definition";
         when Iir_Kind_Floating_Type_Definition =>
            return "floating_type_definition";
         when Iir_Kind_Physical_Type_Definition =>
            return "physical_type_definition";
         when Iir_Kind_Range_Expression =>
            return "range_expression";
         when Iir_Kind_Protected_Type_Body =>
            return "protected_type_body";
         when Iir_Kind_Wildcard_Type_Definition =>
            return "wildcard_type_definition";
         when Iir_Kind_Foreign_Vector_Type_Definition =>
            return "foreign_vector_type_definition";
         when Iir_Kind_Subtype_Definition =>
            return "subtype_definition";
         when Iir_Kind_Scalar_Nature_Definition =>
            return "scalar_nature_definition";
         when Iir_Kind_Record_Nature_Definition =>
            return "record_nature_definition";
         when Iir_Kind_Array_Nature_Definition =>
            return "array_nature_definition";
         when Iir_Kind_Array_Subnature_Definition =>
            return "array_subnature_definition";
         when Iir_Kind_Overload_List =>
            return "overload_list";
         when Iir_Kind_Foreign_Module =>
            return "foreign_module";
         when Iir_Kind_Entity_Declaration =>
            return "entity_declaration";
         when Iir_Kind_Configuration_Declaration =>
            return "configuration_declaration";
         when Iir_Kind_Context_Declaration =>
            return "context_declaration";
         when Iir_Kind_Package_Declaration =>
            return "package_declaration";
         when Iir_Kind_Package_Instantiation_Declaration =>
            return "package_instantiation_declaration";
         when Iir_Kind_Vmode_Declaration =>
            return "vmode_declaration";
         when Iir_Kind_Vprop_Declaration =>
            return "vprop_declaration";
         when Iir_Kind_Vunit_Declaration =>
            return "vunit_declaration";
         when Iir_Kind_Package_Body =>
            return "package_body";
         when Iir_Kind_Architecture_Body =>
            return "architecture_body";
         when Iir_Kind_Type_Declaration =>
            return "type_declaration";
         when Iir_Kind_Anonymous_Type_Declaration =>
            return "anonymous_type_declaration";
         when Iir_Kind_Subtype_Declaration =>
            return "subtype_declaration";
         when Iir_Kind_Nature_Declaration =>
            return "nature_declaration";
         when Iir_Kind_Subnature_Declaration =>
            return "subnature_declaration";
         when Iir_Kind_Package_Header =>
            return "package_header";
         when Iir_Kind_Unit_Declaration =>
            return "unit_declaration";
         when Iir_Kind_Library_Declaration =>
            return "library_declaration";
         when Iir_Kind_Component_Declaration =>
            return "component_declaration";
         when Iir_Kind_Attribute_Declaration =>
            return "attribute_declaration";
         when Iir_Kind_Group_Template_Declaration =>
            return "group_template_declaration";
         when Iir_Kind_Group_Declaration =>
            return "group_declaration";
         when Iir_Kind_Element_Declaration =>
            return "element_declaration";
         when Iir_Kind_Nature_Element_Declaration =>
            return "nature_element_declaration";
         when Iir_Kind_Non_Object_Alias_Declaration =>
            return "non_object_alias_declaration";
         when Iir_Kind_Psl_Declaration =>
            return "psl_declaration";
         when Iir_Kind_Psl_Endpoint_Declaration =>
            return "psl_endpoint_declaration";
         when Iir_Kind_Enumeration_Literal =>
            return "enumeration_literal";
         when Iir_Kind_Function_Declaration =>
            return "function_declaration";
         when Iir_Kind_Procedure_Declaration =>
            return "procedure_declaration";
         when Iir_Kind_Function_Body =>
            return "function_body";
         when Iir_Kind_Procedure_Body =>
            return "procedure_body";
         when Iir_Kind_Function_Instantiation_Declaration =>
            return "function_instantiation_declaration";
         when Iir_Kind_Procedure_Instantiation_Declaration =>
            return "procedure_instantiation_declaration";
         when Iir_Kind_Terminal_Declaration =>
            return "terminal_declaration";
         when Iir_Kind_Object_Alias_Declaration =>
            return "object_alias_declaration";
         when Iir_Kind_Free_Quantity_Declaration =>
            return "free_quantity_declaration";
         when Iir_Kind_Spectrum_Quantity_Declaration =>
            return "spectrum_quantity_declaration";
         when Iir_Kind_Noise_Quantity_Declaration =>
            return "noise_quantity_declaration";
         when Iir_Kind_Across_Quantity_Declaration =>
            return "across_quantity_declaration";
         when Iir_Kind_Through_Quantity_Declaration =>
            return "through_quantity_declaration";
         when Iir_Kind_File_Declaration =>
            return "file_declaration";
         when Iir_Kind_Guard_Signal_Declaration =>
            return "guard_signal_declaration";
         when Iir_Kind_Signal_Declaration =>
            return "signal_declaration";
         when Iir_Kind_Variable_Declaration =>
            return "variable_declaration";
         when Iir_Kind_Constant_Declaration =>
            return "constant_declaration";
         when Iir_Kind_Iterator_Declaration =>
            return "iterator_declaration";
         when Iir_Kind_Interface_Constant_Declaration =>
            return "interface_constant_declaration";
         when Iir_Kind_Interface_Variable_Declaration =>
            return "interface_variable_declaration";
         when Iir_Kind_Interface_Signal_Declaration =>
            return "interface_signal_declaration";
         when Iir_Kind_Interface_File_Declaration =>
            return "interface_file_declaration";
         when Iir_Kind_Interface_Quantity_Declaration =>
            return "interface_quantity_declaration";
         when Iir_Kind_Interface_Terminal_Declaration =>
            return "interface_terminal_declaration";
         when Iir_Kind_Interface_Type_Declaration =>
            return "interface_type_declaration";
         when Iir_Kind_Interface_Package_Declaration =>
            return "interface_package_declaration";
         when Iir_Kind_Interface_Function_Declaration =>
            return "interface_function_declaration";
         when Iir_Kind_Interface_Procedure_Declaration =>
            return "interface_procedure_declaration";
         when Iir_Kind_Signal_Attribute_Declaration =>
            return "signal_attribute_declaration";
         when Iir_Kind_Suspend_State_Declaration =>
            return "suspend_state_declaration";
         when Iir_Kind_Identity_Operator =>
            return "identity_operator";
         when Iir_Kind_Negation_Operator =>
            return "negation_operator";
         when Iir_Kind_Absolute_Operator =>
            return "absolute_operator";
         when Iir_Kind_Not_Operator =>
            return "not_operator";
         when Iir_Kind_Implicit_Condition_Operator =>
            return "implicit_condition_operator";
         when Iir_Kind_Condition_Operator =>
            return "condition_operator";
         when Iir_Kind_Reduction_And_Operator =>
            return "reduction_and_operator";
         when Iir_Kind_Reduction_Or_Operator =>
            return "reduction_or_operator";
         when Iir_Kind_Reduction_Nand_Operator =>
            return "reduction_nand_operator";
         when Iir_Kind_Reduction_Nor_Operator =>
            return "reduction_nor_operator";
         when Iir_Kind_Reduction_Xor_Operator =>
            return "reduction_xor_operator";
         when Iir_Kind_Reduction_Xnor_Operator =>
            return "reduction_xnor_operator";
         when Iir_Kind_And_Operator =>
            return "and_operator";
         when Iir_Kind_Or_Operator =>
            return "or_operator";
         when Iir_Kind_Nand_Operator =>
            return "nand_operator";
         when Iir_Kind_Nor_Operator =>
            return "nor_operator";
         when Iir_Kind_Xor_Operator =>
            return "xor_operator";
         when Iir_Kind_Xnor_Operator =>
            return "xnor_operator";
         when Iir_Kind_Equality_Operator =>
            return "equality_operator";
         when Iir_Kind_Inequality_Operator =>
            return "inequality_operator";
         when Iir_Kind_Less_Than_Operator =>
            return "less_than_operator";
         when Iir_Kind_Less_Than_Or_Equal_Operator =>
            return "less_than_or_equal_operator";
         when Iir_Kind_Greater_Than_Operator =>
            return "greater_than_operator";
         when Iir_Kind_Greater_Than_Or_Equal_Operator =>
            return "greater_than_or_equal_operator";
         when Iir_Kind_Match_Equality_Operator =>
            return "match_equality_operator";
         when Iir_Kind_Match_Inequality_Operator =>
            return "match_inequality_operator";
         when Iir_Kind_Match_Less_Than_Operator =>
            return "match_less_than_operator";
         when Iir_Kind_Match_Less_Than_Or_Equal_Operator =>
            return "match_less_than_or_equal_operator";
         when Iir_Kind_Match_Greater_Than_Operator =>
            return "match_greater_than_operator";
         when Iir_Kind_Match_Greater_Than_Or_Equal_Operator =>
            return "match_greater_than_or_equal_operator";
         when Iir_Kind_Sll_Operator =>
            return "sll_operator";
         when Iir_Kind_Sla_Operator =>
            return "sla_operator";
         when Iir_Kind_Srl_Operator =>
            return "srl_operator";
         when Iir_Kind_Sra_Operator =>
            return "sra_operator";
         when Iir_Kind_Rol_Operator =>
            return "rol_operator";
         when Iir_Kind_Ror_Operator =>
            return "ror_operator";
         when Iir_Kind_Addition_Operator =>
            return "addition_operator";
         when Iir_Kind_Substraction_Operator =>
            return "substraction_operator";
         when Iir_Kind_Concatenation_Operator =>
            return "concatenation_operator";
         when Iir_Kind_Multiplication_Operator =>
            return "multiplication_operator";
         when Iir_Kind_Division_Operator =>
            return "division_operator";
         when Iir_Kind_Modulus_Operator =>
            return "modulus_operator";
         when Iir_Kind_Remainder_Operator =>
            return "remainder_operator";
         when Iir_Kind_Exponentiation_Operator =>
            return "exponentiation_operator";
         when Iir_Kind_Function_Call =>
            return "function_call";
         when Iir_Kind_Aggregate =>
            return "aggregate";
         when Iir_Kind_Parenthesis_Expression =>
            return "parenthesis_expression";
         when Iir_Kind_Qualified_Expression =>
            return "qualified_expression";
         when Iir_Kind_Type_Conversion =>
            return "type_conversion";
         when Iir_Kind_Allocator_By_Expression =>
            return "allocator_by_expression";
         when Iir_Kind_Allocator_By_Subtype =>
            return "allocator_by_subtype";
         when Iir_Kind_Selected_Element =>
            return "selected_element";
         when Iir_Kind_Dereference =>
            return "dereference";
         when Iir_Kind_Implicit_Dereference =>
            return "implicit_dereference";
         when Iir_Kind_Slice_Name =>
            return "slice_name";
         when Iir_Kind_Indexed_Name =>
            return "indexed_name";
         when Iir_Kind_Psl_Prev =>
            return "psl_prev";
         when Iir_Kind_Psl_Stable =>
            return "psl_stable";
         when Iir_Kind_Psl_Rose =>
            return "psl_rose";
         when Iir_Kind_Psl_Fell =>
            return "psl_fell";
         when Iir_Kind_Psl_Onehot =>
            return "psl_onehot";
         when Iir_Kind_Psl_Onehot0 =>
            return "psl_onehot0";
         when Iir_Kind_Psl_Expression =>
            return "psl_expression";
         when Iir_Kind_Sensitized_Process_Statement =>
            return "sensitized_process_statement";
         when Iir_Kind_Process_Statement =>
            return "process_statement";
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            return "concurrent_simple_signal_assignment";
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            return "concurrent_conditional_signal_assignment";
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            return "concurrent_selected_signal_assignment";
         when Iir_Kind_Concurrent_Assertion_Statement =>
            return "concurrent_assertion_statement";
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            return "concurrent_procedure_call_statement";
         when Iir_Kind_Concurrent_Break_Statement =>
            return "concurrent_break_statement";
         when Iir_Kind_Psl_Assert_Directive =>
            return "psl_assert_directive";
         when Iir_Kind_Psl_Assume_Directive =>
            return "psl_assume_directive";
         when Iir_Kind_Psl_Cover_Directive =>
            return "psl_cover_directive";
         when Iir_Kind_Psl_Restrict_Directive =>
            return "psl_restrict_directive";
         when Iir_Kind_Block_Statement =>
            return "block_statement";
         when Iir_Kind_If_Generate_Statement =>
            return "if_generate_statement";
         when Iir_Kind_Case_Generate_Statement =>
            return "case_generate_statement";
         when Iir_Kind_For_Generate_Statement =>
            return "for_generate_statement";
         when Iir_Kind_Component_Instantiation_Statement =>
            return "component_instantiation_statement";
         when Iir_Kind_Psl_Default_Clock =>
            return "psl_default_clock";
         when Iir_Kind_Generate_Statement_Body =>
            return "generate_statement_body";
         when Iir_Kind_If_Generate_Else_Clause =>
            return "if_generate_else_clause";
         when Iir_Kind_Simple_Simultaneous_Statement =>
            return "simple_simultaneous_statement";
         when Iir_Kind_Simultaneous_Null_Statement =>
            return "simultaneous_null_statement";
         when Iir_Kind_Simultaneous_Procedural_Statement =>
            return "simultaneous_procedural_statement";
         when Iir_Kind_Simultaneous_Case_Statement =>
            return "simultaneous_case_statement";
         when Iir_Kind_Simultaneous_If_Statement =>
            return "simultaneous_if_statement";
         when Iir_Kind_Simultaneous_Elsif =>
            return "simultaneous_elsif";
         when Iir_Kind_Simple_Signal_Assignment_Statement =>
            return "simple_signal_assignment_statement";
         when Iir_Kind_Conditional_Signal_Assignment_Statement =>
            return "conditional_signal_assignment_statement";
         when Iir_Kind_Selected_Waveform_Assignment_Statement =>
            return "selected_waveform_assignment_statement";
         when Iir_Kind_Signal_Force_Assignment_Statement =>
            return "signal_force_assignment_statement";
         when Iir_Kind_Signal_Release_Assignment_Statement =>
            return "signal_release_assignment_statement";
         when Iir_Kind_Null_Statement =>
            return "null_statement";
         when Iir_Kind_Assertion_Statement =>
            return "assertion_statement";
         when Iir_Kind_Report_Statement =>
            return "report_statement";
         when Iir_Kind_Wait_Statement =>
            return "wait_statement";
         when Iir_Kind_Variable_Assignment_Statement =>
            return "variable_assignment_statement";
         when Iir_Kind_Conditional_Variable_Assignment_Statement =>
            return "conditional_variable_assignment_statement";
         when Iir_Kind_Return_Statement =>
            return "return_statement";
         when Iir_Kind_For_Loop_Statement =>
            return "for_loop_statement";
         when Iir_Kind_While_Loop_Statement =>
            return "while_loop_statement";
         when Iir_Kind_Next_Statement =>
            return "next_statement";
         when Iir_Kind_Exit_Statement =>
            return "exit_statement";
         when Iir_Kind_Case_Statement =>
            return "case_statement";
         when Iir_Kind_Procedure_Call_Statement =>
            return "procedure_call_statement";
         when Iir_Kind_Break_Statement =>
            return "break_statement";
         when Iir_Kind_If_Statement =>
            return "if_statement";
         when Iir_Kind_Suspend_State_Statement =>
            return "suspend_state_statement";
         when Iir_Kind_Elsif =>
            return "elsif";
         when Iir_Kind_Character_Literal =>
            return "character_literal";
         when Iir_Kind_Simple_Name =>
            return "simple_name";
         when Iir_Kind_Selected_Name =>
            return "selected_name";
         when Iir_Kind_Operator_Symbol =>
            return "operator_symbol";
         when Iir_Kind_Reference_Name =>
            return "reference_name";
         when Iir_Kind_External_Constant_Name =>
            return "external_constant_name";
         when Iir_Kind_External_Signal_Name =>
            return "external_signal_name";
         when Iir_Kind_External_Variable_Name =>
            return "external_variable_name";
         when Iir_Kind_Selected_By_All_Name =>
            return "selected_by_all_name";
         when Iir_Kind_Parenthesis_Name =>
            return "parenthesis_name";
         when Iir_Kind_Package_Pathname =>
            return "package_pathname";
         when Iir_Kind_Absolute_Pathname =>
            return "absolute_pathname";
         when Iir_Kind_Relative_Pathname =>
            return "relative_pathname";
         when Iir_Kind_Pathname_Element =>
            return "pathname_element";
         when Iir_Kind_Base_Attribute =>
            return "base_attribute";
         when Iir_Kind_Subtype_Attribute =>
            return "subtype_attribute";
         when Iir_Kind_Element_Attribute =>
            return "element_attribute";
         when Iir_Kind_Across_Attribute =>
            return "across_attribute";
         when Iir_Kind_Through_Attribute =>
            return "through_attribute";
         when Iir_Kind_Nature_Reference_Attribute =>
            return "nature_reference_attribute";
         when Iir_Kind_Left_Type_Attribute =>
            return "left_type_attribute";
         when Iir_Kind_Right_Type_Attribute =>
            return "right_type_attribute";
         when Iir_Kind_High_Type_Attribute =>
            return "high_type_attribute";
         when Iir_Kind_Low_Type_Attribute =>
            return "low_type_attribute";
         when Iir_Kind_Ascending_Type_Attribute =>
            return "ascending_type_attribute";
         when Iir_Kind_Image_Attribute =>
            return "image_attribute";
         when Iir_Kind_Value_Attribute =>
            return "value_attribute";
         when Iir_Kind_Pos_Attribute =>
            return "pos_attribute";
         when Iir_Kind_Val_Attribute =>
            return "val_attribute";
         when Iir_Kind_Succ_Attribute =>
            return "succ_attribute";
         when Iir_Kind_Pred_Attribute =>
            return "pred_attribute";
         when Iir_Kind_Leftof_Attribute =>
            return "leftof_attribute";
         when Iir_Kind_Rightof_Attribute =>
            return "rightof_attribute";
         when Iir_Kind_Signal_Slew_Attribute =>
            return "signal_slew_attribute";
         when Iir_Kind_Quantity_Slew_Attribute =>
            return "quantity_slew_attribute";
         when Iir_Kind_Ramp_Attribute =>
            return "ramp_attribute";
         when Iir_Kind_Zoh_Attribute =>
            return "zoh_attribute";
         when Iir_Kind_Ltf_Attribute =>
            return "ltf_attribute";
         when Iir_Kind_Ztf_Attribute =>
            return "ztf_attribute";
         when Iir_Kind_Dot_Attribute =>
            return "dot_attribute";
         when Iir_Kind_Integ_Attribute =>
            return "integ_attribute";
         when Iir_Kind_Above_Attribute =>
            return "above_attribute";
         when Iir_Kind_Quantity_Delayed_Attribute =>
            return "quantity_delayed_attribute";
         when Iir_Kind_Delayed_Attribute =>
            return "delayed_attribute";
         when Iir_Kind_Stable_Attribute =>
            return "stable_attribute";
         when Iir_Kind_Quiet_Attribute =>
            return "quiet_attribute";
         when Iir_Kind_Transaction_Attribute =>
            return "transaction_attribute";
         when Iir_Kind_Event_Attribute =>
            return "event_attribute";
         when Iir_Kind_Active_Attribute =>
            return "active_attribute";
         when Iir_Kind_Last_Event_Attribute =>
            return "last_event_attribute";
         when Iir_Kind_Last_Active_Attribute =>
            return "last_active_attribute";
         when Iir_Kind_Last_Value_Attribute =>
            return "last_value_attribute";
         when Iir_Kind_Driving_Attribute =>
            return "driving_attribute";
         when Iir_Kind_Driving_Value_Attribute =>
            return "driving_value_attribute";
         when Iir_Kind_Behavior_Attribute =>
            return "behavior_attribute";
         when Iir_Kind_Structure_Attribute =>
            return "structure_attribute";
         when Iir_Kind_Simple_Name_Attribute =>
            return "simple_name_attribute";
         when Iir_Kind_Instance_Name_Attribute =>
            return "instance_name_attribute";
         when Iir_Kind_Path_Name_Attribute =>
            return "path_name_attribute";
         when Iir_Kind_Left_Array_Attribute =>
            return "left_array_attribute";
         when Iir_Kind_Right_Array_Attribute =>
            return "right_array_attribute";
         when Iir_Kind_High_Array_Attribute =>
            return "high_array_attribute";
         when Iir_Kind_Low_Array_Attribute =>
            return "low_array_attribute";
         when Iir_Kind_Length_Array_Attribute =>
            return "length_array_attribute";
         when Iir_Kind_Ascending_Array_Attribute =>
            return "ascending_array_attribute";
         when Iir_Kind_Range_Array_Attribute =>
            return "range_array_attribute";
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            return "reverse_range_array_attribute";
         when Iir_Kind_Attribute_Name =>
            return "attribute_name";
      end case;
   end Get_Iir_Image;

   function Get_Field_Attribute (F : Fields_Enum) return Field_Attribute is
   begin
      case F is
         when Field_First_Design_Unit =>
            return Attr_Chain;
         when Field_Last_Design_Unit =>
            return Attr_Ref;
         when Field_Library_Declaration =>
            return Attr_Forward_Ref;
         when Field_File_Checksum =>
            return Attr_None;
         when Field_Analysis_Time_Stamp =>
            return Attr_None;
         when Field_Design_File_Source =>
            return Attr_None;
         when Field_Library =>
            return Attr_Ref;
         when Field_File_Dependence_List =>
            return Attr_None;
         when Field_Design_File_Filename =>
            return Attr_None;
         when Field_Design_File_Directory =>
            return Attr_None;
         when Field_Design_File =>
            return Attr_Ref;
         when Field_Design_File_Chain =>
            return Attr_Chain;
         when Field_Library_Directory =>
            return Attr_None;
         when Field_Date =>
            return Attr_None;
         when Field_Context_Items =>
            return Attr_Chain;
         when Field_Dependence_List =>
            return Attr_Of_Ref;
         when Field_Analysis_Checks_List =>
            return Attr_Of_Ref;
         when Field_Date_State =>
            return Attr_None;
         when Field_Guarded_Target_State =>
            return Attr_None;
         when Field_Library_Unit =>
            return Attr_None;
         when Field_Hash_Chain =>
            return Attr_Forward_Ref;
         when Field_Design_Unit_Source_Pos =>
            return Attr_None;
         when Field_Design_Unit_Source_Line =>
            return Attr_None;
         when Field_Design_Unit_Source_Col =>
            return Attr_None;
         when Field_Value =>
            return Attr_None;
         when Field_Enum_Pos =>
            return Attr_None;
         when Field_Physical_Literal =>
            return Attr_None;
         when Field_Fp_Value =>
            return Attr_None;
         when Field_Simple_Aggregate_List =>
            return Attr_Ref;
         when Field_String8_Id =>
            return Attr_None;
         when Field_String_Length =>
            return Attr_None;
         when Field_Bit_String_Base =>
            return Attr_None;
         when Field_Has_Signed =>
            return Attr_None;
         when Field_Has_Sign =>
            return Attr_None;
         when Field_Has_Length =>
            return Attr_None;
         when Field_Literal_Length =>
            return Attr_None;
         when Field_Literal_Origin =>
            return Attr_None;
         when Field_Range_Origin =>
            return Attr_None;
         when Field_Literal_Subtype =>
            return Attr_None;
         when Field_Allocator_Subtype =>
            return Attr_Ref;
         when Field_Entity_Class =>
            return Attr_None;
         when Field_Entity_Name_List =>
            return Attr_None;
         when Field_Attribute_Designator =>
            return Attr_None;
         when Field_Attribute_Specification_Chain =>
            return Attr_Ref;
         when Field_Attribute_Specification =>
            return Attr_Ref;
         when Field_Static_Attribute_Flag =>
            return Attr_None;
         when Field_Signal_List =>
            return Attr_Of_Maybe_Ref;
         when Field_Quantity_List =>
            return Attr_Of_Maybe_Ref;
         when Field_Designated_Entity =>
            return Attr_Forward_Ref;
         when Field_Formal =>
            return Attr_None;
         when Field_Actual =>
            return Attr_None;
         when Field_Actual_Conversion =>
            return Attr_None;
         when Field_Formal_Conversion =>
            return Attr_None;
         when Field_Whole_Association_Flag =>
            return Attr_None;
         when Field_Collapse_Signal_Flag =>
            return Attr_None;
         when Field_Artificial_Flag =>
            return Attr_None;
         when Field_Open_Flag =>
            return Attr_None;
         when Field_After_Drivers_Flag =>
            return Attr_None;
         when Field_We_Value =>
            return Attr_None;
         when Field_Time =>
            return Attr_None;
         when Field_Associated_Expr =>
            return Attr_None;
         when Field_Associated_Block =>
            return Attr_None;
         when Field_Associated_Chain =>
            return Attr_Chain;
         when Field_Choice_Name =>
            return Attr_None;
         when Field_Choice_Expression =>
            return Attr_None;
         when Field_Choice_Range =>
            return Attr_None;
         when Field_Same_Alternative_Flag =>
            return Attr_None;
         when Field_Element_Type_Flag =>
            return Attr_None;
         when Field_Architecture =>
            return Attr_None;
         when Field_Block_Specification =>
            return Attr_None;
         when Field_Prev_Block_Configuration =>
            return Attr_Ref;
         when Field_Configuration_Item_Chain =>
            return Attr_Chain;
         when Field_Attribute_Value_Chain =>
            return Attr_Ref;
         when Field_Spec_Chain =>
            return Attr_None;
         when Field_Value_Chain =>
            return Attr_Ref;
         when Field_Attribute_Value_Spec_Chain =>
            return Attr_None;
         when Field_Entity_Name =>
            return Attr_None;
         when Field_Package =>
            return Attr_Ref;
         when Field_Package_Body =>
            return Attr_Forward_Ref;
         when Field_Instance_Package_Body =>
            return Attr_None;
         when Field_Need_Body =>
            return Attr_None;
         when Field_Macro_Expanded_Flag =>
            return Attr_None;
         when Field_Need_Instance_Bodies =>
            return Attr_None;
         when Field_Hierarchical_Name =>
            return Attr_None;
         when Field_Vunit_Item_Chain =>
            return Attr_Chain;
         when Field_Bound_Vunit_Chain =>
            return Attr_Chain;
         when Field_Verification_Block_Configuration =>
            return Attr_None;
         when Field_Block_Configuration =>
            return Attr_None;
         when Field_Concurrent_Statement_Chain =>
            return Attr_Chain;
         when Field_Chain =>
            return Attr_Chain_Next;
         when Field_Port_Chain =>
            return Attr_Chain;
         when Field_Generic_Chain =>
            return Attr_Chain;
         when Field_Type =>
            return Attr_Ref;
         when Field_Subtype_Indication =>
            return Attr_Maybe_Ref;
         when Field_Discrete_Range =>
            return Attr_None;
         when Field_Type_Definition =>
            return Attr_None;
         when Field_Subtype_Definition =>
            return Attr_Forward_Ref;
         when Field_Incomplete_Type_Declaration =>
            return Attr_Ref;
         when Field_Interface_Type_Subprograms =>
            return Attr_Chain;
         when Field_Nature_Definition =>
            return Attr_None;
         when Field_Nature =>
            return Attr_Ref;
         when Field_Subnature_Indication =>
            return Attr_None;
         when Field_Mode =>
            return Attr_None;
         when Field_Guarded_Signal_Flag =>
            return Attr_None;
         when Field_Signal_Kind =>
            return Attr_None;
         when Field_Base_Name =>
            return Attr_Ref;
         when Field_Interface_Declaration_Chain =>
            return Attr_Chain;
         when Field_Subprogram_Specification =>
            return Attr_Ref;
         when Field_Sequential_Statement_Chain =>
            return Attr_Chain;
         when Field_Simultaneous_Statement_Chain =>
            return Attr_Chain;
         when Field_Subprogram_Body =>
            return Attr_Forward_Ref;
         when Field_Overload_Number =>
            return Attr_None;
         when Field_Subprogram_Depth =>
            return Attr_None;
         when Field_Subprogram_Hash =>
            return Attr_None;
         when Field_Impure_Depth =>
            return Attr_None;
         when Field_Return_Type =>
            return Attr_Ref;
         when Field_Implicit_Definition =>
            return Attr_None;
         when Field_Uninstantiated_Subprogram_Name =>
            return Attr_None;
         when Field_Default_Value =>
            return Attr_Maybe_Ref;
         when Field_Deferred_Declaration =>
            return Attr_Forward_Ref;
         when Field_Deferred_Declaration_Flag =>
            return Attr_None;
         when Field_Shared_Flag =>
            return Attr_None;
         when Field_Design_Unit =>
            return Attr_None;
         when Field_Block_Statement =>
            return Attr_Ref;
         when Field_Signal_Driver =>
            return Attr_None;
         when Field_Declaration_Chain =>
            return Attr_Chain;
         when Field_File_Logical_Name =>
            return Attr_None;
         when Field_File_Open_Kind =>
            return Attr_None;
         when Field_Element_Position =>
            return Attr_None;
         when Field_Use_Clause_Chain =>
            return Attr_None;
         when Field_Context_Reference_Chain =>
            return Attr_None;
         when Field_Inherit_Spec_Chain =>
            return Attr_Chain;
         when Field_Selected_Name =>
            return Attr_None;
         when Field_Type_Declarator =>
            return Attr_Ref;
         when Field_Complete_Type_Definition =>
            return Attr_Forward_Ref;
         when Field_Incomplete_Type_Ref_Chain =>
            return Attr_Forward_Ref;
         when Field_Associated_Type =>
            return Attr_Ref;
         when Field_Enumeration_Literal_List =>
            return Attr_None;
         when Field_Entity_Class_Entry_Chain =>
            return Attr_Chain;
         when Field_Group_Constituent_List =>
            return Attr_None;
         when Field_Unit_Chain =>
            return Attr_Chain;
         when Field_Primary_Unit =>
            return Attr_Ref;
         when Field_Identifier =>
            return Attr_None;
         when Field_Label =>
            return Attr_None;
         when Field_Return_Identifier =>
            return Attr_None;
         when Field_Visible_Flag =>
            return Attr_None;
         when Field_Range_Constraint =>
            return Attr_Maybe_Ref;
         when Field_Direction =>
            return Attr_None;
         when Field_Left_Limit =>
            return Attr_Ref;
         when Field_Right_Limit =>
            return Attr_Ref;
         when Field_Left_Limit_Expr =>
            return Attr_None;
         when Field_Right_Limit_Expr =>
            return Attr_None;
         when Field_Parent_Type =>
            return Attr_Ref;
         when Field_Simple_Nature =>
            return Attr_Ref;
         when Field_Base_Nature =>
            return Attr_Ref;
         when Field_Resolution_Indication =>
            return Attr_None;
         when Field_Record_Element_Resolution_Chain =>
            return Attr_Chain;
         when Field_Tolerance =>
            return Attr_None;
         when Field_Plus_Terminal_Name =>
            return Attr_None;
         when Field_Minus_Terminal_Name =>
            return Attr_None;
         when Field_Plus_Terminal =>
            return Attr_Ref;
         when Field_Minus_Terminal =>
            return Attr_Ref;
         when Field_Magnitude_Expression =>
            return Attr_None;
         when Field_Phase_Expression =>
            return Attr_None;
         when Field_Power_Expression =>
            return Attr_None;
         when Field_Simultaneous_Left =>
            return Attr_None;
         when Field_Simultaneous_Right =>
            return Attr_None;
         when Field_Text_File_Flag =>
            return Attr_None;
         when Field_Only_Characters_Flag =>
            return Attr_None;
         when Field_Is_Character_Type =>
            return Attr_None;
         when Field_Nature_Staticness =>
            return Attr_None;
         when Field_Type_Staticness =>
            return Attr_None;
         when Field_Constraint_State =>
            return Attr_None;
         when Field_Index_Subtype_List =>
            return Attr_Ref;
         when Field_Index_Subtype_Definition_List =>
            return Attr_None;
         when Field_Element_Subtype_Indication =>
            return Attr_None;
         when Field_Element_Subtype =>
            return Attr_Ref;
         when Field_Element_Subnature_Indication =>
            return Attr_None;
         when Field_Element_Subnature =>
            return Attr_Ref;
         when Field_Index_Constraint_List =>
            return Attr_None;
         when Field_Array_Element_Constraint =>
            return Attr_None;
         when Field_Has_Array_Constraint_Flag =>
            return Attr_None;
         when Field_Has_Element_Constraint_Flag =>
            return Attr_None;
         when Field_Elements_Declaration_List =>
            return Attr_Of_Maybe_Ref;
         when Field_Owned_Elements_Chain =>
            return Attr_Chain;
         when Field_Designated_Type =>
            return Attr_Forward_Ref;
         when Field_Designated_Subtype_Indication =>
            return Attr_None;
         when Field_Index_List =>
            return Attr_None;
         when Field_Reference =>
            return Attr_Forward_Ref;
         when Field_Nature_Declarator =>
            return Attr_Ref;
         when Field_Across_Type_Mark =>
            return Attr_None;
         when Field_Through_Type_Mark =>
            return Attr_None;
         when Field_Across_Type_Definition =>
            return Attr_None;
         when Field_Through_Type_Definition =>
            return Attr_None;
         when Field_Across_Type =>
            return Attr_Ref;
         when Field_Through_Type =>
            return Attr_Ref;
         when Field_Target =>
            return Attr_Maybe_Ref;
         when Field_Waveform_Chain =>
            return Attr_Chain;
         when Field_Guard =>
            return Attr_Ref;
         when Field_Delay_Mechanism =>
            return Attr_None;
         when Field_Reject_Time_Expression =>
            return Attr_None;
         when Field_Force_Mode =>
            return Attr_None;
         when Field_Has_Force_Mode =>
            return Attr_None;
         when Field_Sensitivity_List =>
            return Attr_Of_Maybe_Ref;
         when Field_Process_Origin =>
            return Attr_None;
         when Field_Package_Origin =>
            return Attr_None;
         when Field_Condition_Clause =>
            return Attr_None;
         when Field_Break_Element =>
            return Attr_Chain;
         when Field_Selector_Quantity =>
            return Attr_None;
         when Field_Break_Quantity =>
            return Attr_None;
         when Field_Timeout_Clause =>
            return Attr_None;
         when Field_Postponed_Flag =>
            return Attr_None;
         when Field_Callees_List =>
            return Attr_Of_Ref;
         when Field_Passive_Flag =>
            return Attr_None;
         when Field_Resolution_Function_Flag =>
            return Attr_None;
         when Field_Wait_State =>
            return Attr_None;
         when Field_All_Sensitized_State =>
            return Attr_None;
         when Field_Seen_Flag =>
            return Attr_None;
         when Field_Pure_Flag =>
            return Attr_None;
         when Field_Foreign_Flag =>
            return Attr_None;
         when Field_Resolved_Flag =>
            return Attr_None;
         when Field_Signal_Type_Flag =>
            return Attr_None;
         when Field_Has_Signal_Flag =>
            return Attr_None;
         when Field_Purity_State =>
            return Attr_None;
         when Field_Elab_Flag =>
            return Attr_None;
         when Field_Vendor_Library_Flag =>
            return Attr_None;
         when Field_Configuration_Mark_Flag =>
            return Attr_None;
         when Field_Configuration_Done_Flag =>
            return Attr_None;
         when Field_Index_Constraint_Flag =>
            return Attr_None;
         when Field_Hide_Implicit_Flag =>
            return Attr_None;
         when Field_Assertion_Condition =>
            return Attr_None;
         when Field_Report_Expression =>
            return Attr_None;
         when Field_Severity_Expression =>
            return Attr_None;
         when Field_Instantiated_Unit =>
            return Attr_None;
         when Field_Generic_Map_Aspect_Chain =>
            return Attr_Chain;
         when Field_Port_Map_Aspect_Chain =>
            return Attr_Chain;
         when Field_Configuration_Name =>
            return Attr_None;
         when Field_Component_Configuration =>
            return Attr_Forward_Ref;
         when Field_Configuration_Specification =>
            return Attr_Ref;
         when Field_Default_Binding_Indication =>
            return Attr_None;
         when Field_Default_Configuration_Declaration =>
            return Attr_None;
         when Field_Expression =>
            return Attr_None;
         when Field_Conditional_Expression_Chain =>
            return Attr_Chain;
         when Field_Allocator_Designated_Type =>
            return Attr_Ref;
         when Field_Selected_Waveform_Chain =>
            return Attr_Chain;
         when Field_Conditional_Waveform_Chain =>
            return Attr_Chain;
         when Field_Guard_Expression =>
            return Attr_None;
         when Field_Guard_Decl =>
            return Attr_None;
         when Field_Guard_Sensitivity_List =>
            return Attr_Of_Ref;
         when Field_Signal_Attribute_Chain =>
            return Attr_Forward_Ref;
         when Field_Block_Block_Configuration =>
            return Attr_Forward_Ref;
         when Field_Package_Header =>
            return Attr_None;
         when Field_Block_Header =>
            return Attr_None;
         when Field_Uninstantiated_Package_Name =>
            return Attr_None;
         when Field_Uninstantiated_Package_Decl =>
            return Attr_Ref;
         when Field_Instance_Source_File =>
            return Attr_None;
         when Field_Generate_Block_Configuration =>
            return Attr_Forward_Ref;
         when Field_Generate_Statement_Body =>
            return Attr_None;
         when Field_Alternative_Label =>
            return Attr_None;
         when Field_Generate_Else_Clause =>
            return Attr_None;
         when Field_Condition =>
            return Attr_Maybe_Ref;
         when Field_Else_Clause =>
            return Attr_None;
         when Field_Parameter_Specification =>
            return Attr_None;
         when Field_Parent =>
            return Attr_Ref;
         when Field_Loop_Label =>
            return Attr_None;
         when Field_Exit_Flag =>
            return Attr_None;
         when Field_Next_Flag =>
            return Attr_None;
         when Field_Component_Name =>
            return Attr_None;
         when Field_Instantiation_List =>
            return Attr_None;
         when Field_Entity_Aspect =>
            return Attr_None;
         when Field_Default_Entity_Aspect =>
            return Attr_None;
         when Field_Binding_Indication =>
            return Attr_Maybe_Ref;
         when Field_Named_Entity =>
            return Attr_Maybe_Forward_Ref;
         when Field_Referenced_Name =>
            return Attr_Ref;
         when Field_Expr_Staticness =>
            return Attr_None;
         when Field_Scalar_Size =>
            return Attr_None;
         when Field_Error_Origin =>
            return Attr_None;
         when Field_Operand =>
            return Attr_None;
         when Field_Left =>
            return Attr_None;
         when Field_Right =>
            return Attr_None;
         when Field_Unit_Name =>
            return Attr_None;
         when Field_Name =>
            return Attr_None;
         when Field_Group_Template_Name =>
            return Attr_None;
         when Field_Name_Staticness =>
            return Attr_None;
         when Field_Prefix =>
            return Attr_None;
         when Field_Signature_Prefix =>
            return Attr_None;
         when Field_External_Pathname =>
            return Attr_None;
         when Field_Pathname_Suffix =>
            return Attr_None;
         when Field_Pathname_Expression =>
            return Attr_None;
         when Field_In_Formal_Flag =>
            return Attr_None;
         when Field_Slice_Subtype =>
            return Attr_None;
         when Field_Suffix =>
            return Attr_None;
         when Field_Index_Subtype =>
            return Attr_Ref;
         when Field_Parameter =>
            return Attr_None;
         when Field_Parameter_2 =>
            return Attr_None;
         when Field_Parameter_3 =>
            return Attr_None;
         when Field_Parameter_4 =>
            return Attr_None;
         when Field_Attr_Chain =>
            return Attr_Forward_Ref;
         when Field_Signal_Attribute_Declaration =>
            return Attr_Forward_Ref;
         when Field_Actual_Type =>
            return Attr_Ref;
         when Field_Actual_Type_Definition =>
            return Attr_None;
         when Field_Association_Chain =>
            return Attr_Chain;
         when Field_Individual_Association_Chain =>
            return Attr_Chain;
         when Field_Subprogram_Association_Chain =>
            return Attr_Chain;
         when Field_Aggregate_Info =>
            return Attr_None;
         when Field_Sub_Aggregate_Info =>
            return Attr_None;
         when Field_Aggr_Dynamic_Flag =>
            return Attr_None;
         when Field_Aggr_Min_Length =>
            return Attr_None;
         when Field_Aggr_Low_Limit =>
            return Attr_Ref;
         when Field_Aggr_High_Limit =>
            return Attr_Ref;
         when Field_Aggr_Others_Flag =>
            return Attr_None;
         when Field_Aggr_Named_Flag =>
            return Attr_None;
         when Field_Aggregate_Expand_Flag =>
            return Attr_None;
         when Field_Association_Choices_Chain =>
            return Attr_Chain;
         when Field_Case_Statement_Alternative_Chain =>
            return Attr_Chain;
         when Field_Matching_Flag =>
            return Attr_None;
         when Field_Choice_Staticness =>
            return Attr_None;
         when Field_Procedure_Call =>
            return Attr_None;
         when Field_Implementation =>
            return Attr_Ref;
         when Field_Parameter_Association_Chain =>
            return Attr_Chain;
         when Field_Method_Object =>
            return Attr_Ref;
         when Field_Subtype_Type_Mark =>
            return Attr_None;
         when Field_Subnature_Nature_Mark =>
            return Attr_None;
         when Field_Type_Conversion_Subtype =>
            return Attr_None;
         when Field_Type_Mark =>
            return Attr_None;
         when Field_File_Type_Mark =>
            return Attr_None;
         when Field_Return_Type_Mark =>
            return Attr_None;
         when Field_Has_Disconnect_Flag =>
            return Attr_None;
         when Field_Has_Active_Flag =>
            return Attr_None;
         when Field_Is_Within_Flag =>
            return Attr_None;
         when Field_Type_Marks_List =>
            return Attr_None;
         when Field_Implicit_Alias_Flag =>
            return Attr_None;
         when Field_Alias_Signature =>
            return Attr_None;
         when Field_Attribute_Signature =>
            return Attr_None;
         when Field_Overload_List =>
            return Attr_Of_Ref;
         when Field_Simple_Name_Identifier =>
            return Attr_None;
         when Field_Simple_Name_Subtype =>
            return Attr_None;
         when Field_Protected_Type_Body =>
            return Attr_Forward_Ref;
         when Field_Protected_Type_Declaration =>
            return Attr_Ref;
         when Field_Use_Flag =>
            return Attr_None;
         when Field_End_Has_Reserved_Id =>
            return Attr_None;
         when Field_End_Has_Identifier =>
            return Attr_None;
         when Field_End_Has_Postponed =>
            return Attr_None;
         when Field_Has_Label =>
            return Attr_None;
         when Field_Has_Begin =>
            return Attr_None;
         when Field_Has_End =>
            return Attr_None;
         when Field_Has_Is =>
            return Attr_None;
         when Field_Has_Pure =>
            return Attr_None;
         when Field_Has_Body =>
            return Attr_None;
         when Field_Has_Parameter =>
            return Attr_None;
         when Field_Has_Component =>
            return Attr_None;
         when Field_Has_Identifier_List =>
            return Attr_None;
         when Field_Has_Mode =>
            return Attr_None;
         when Field_Has_Class =>
            return Attr_None;
         when Field_Has_Delay_Mechanism =>
            return Attr_None;
         when Field_Suspend_Flag =>
            return Attr_None;
         when Field_Is_Ref =>
            return Attr_None;
         when Field_Is_Forward_Ref =>
            return Attr_None;
         when Field_Psl_Property =>
            return Attr_None;
         when Field_Psl_Sequence =>
            return Attr_None;
         when Field_Psl_Declaration =>
            return Attr_None;
         when Field_Psl_Expression =>
            return Attr_None;
         when Field_Psl_Boolean =>
            return Attr_None;
         when Field_PSL_Clock =>
            return Attr_None;
         when Field_PSL_NFA =>
            return Attr_None;
         when Field_PSL_Nbr_States =>
            return Attr_None;
         when Field_PSL_Clock_Sensitivity =>
            return Attr_None;
         when Field_PSL_EOS_Flag =>
            return Attr_None;
         when Field_PSL_Abort_Flag =>
            return Attr_None;
         when Field_Count_Expression =>
            return Attr_None;
         when Field_Clock_Expression =>
            return Attr_None;
         when Field_Default_Clock =>
            return Attr_Ref;
         when Field_Foreign_Node =>
            return Attr_None;
         when Field_Suspend_State_Index =>
            return Attr_None;
         when Field_Suspend_State_Chain =>
            return Attr_Forward_Ref;
      end case;
   end Get_Field_Attribute;

   Fields_Of_Iir : constant Fields_Array :=
     (
      --  Iir_Kind_Unused
      --  Iir_Kind_Error
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Expr_Staticness,
      Field_Type,
      Field_Error_Origin,
      Field_Type_Declarator,
      --  Iir_Kind_Design_File
      Field_Design_File_Directory,
      Field_Design_File_Filename,
      Field_Analysis_Time_Stamp,
      Field_File_Checksum,
      Field_Design_File_Source,
      Field_Elab_Flag,
      Field_Library,
      Field_File_Dependence_List,
      Field_Chain,
      Field_First_Design_Unit,
      Field_Last_Design_Unit,
      --  Iir_Kind_Design_Unit
      Field_Design_Unit_Source_Pos,
      Field_Design_Unit_Source_Line,
      Field_Design_Unit_Source_Col,
      Field_Identifier,
      Field_Date,
      Field_Elab_Flag,
      Field_Configuration_Mark_Flag,
      Field_Configuration_Done_Flag,
      Field_Date_State,
      Field_Design_File,
      Field_Context_Items,
      Field_Chain,
      Field_Library_Unit,
      Field_Hash_Chain,
      Field_Dependence_List,
      Field_Analysis_Checks_List,
      --  Iir_Kind_Library_Clause
      Field_Identifier,
      Field_Has_Identifier_List,
      Field_Parent,
      Field_Library_Declaration,
      Field_Chain,
      --  Iir_Kind_Use_Clause
      Field_Parent,
      Field_Selected_Name,
      Field_Chain,
      Field_Use_Clause_Chain,
      --  Iir_Kind_Context_Reference
      Field_Parent,
      Field_Selected_Name,
      Field_Chain,
      Field_Context_Reference_Chain,
      --  Iir_Kind_PSL_Inherit_Spec
      Field_Parent,
      Field_Name,
      Field_Inherit_Spec_Chain,
      Field_Chain,
      --  Iir_Kind_Integer_Literal
      Field_Literal_Length,
      Field_Value,
      Field_Expr_Staticness,
      Field_Literal_Origin,
      Field_Type,
      --  Iir_Kind_Floating_Point_Literal
      Field_Literal_Length,
      Field_Fp_Value,
      Field_Expr_Staticness,
      Field_Literal_Origin,
      Field_Type,
      --  Iir_Kind_Null_Literal
      Field_Expr_Staticness,
      Field_Type,
      --  Iir_Kind_String_Literal8
      Field_Literal_Length,
      Field_String_Length,
      Field_String8_Id,
      Field_Has_Signed,
      Field_Bit_String_Base,
      Field_Has_Sign,
      Field_Has_Length,
      Field_Expr_Staticness,
      Field_Literal_Origin,
      Field_Literal_Subtype,
      Field_Type,
      --  Iir_Kind_Physical_Int_Literal
      Field_Literal_Length,
      Field_Value,
      Field_Expr_Staticness,
      Field_Unit_Name,
      Field_Literal_Origin,
      Field_Type,
      --  Iir_Kind_Physical_Fp_Literal
      Field_Literal_Length,
      Field_Fp_Value,
      Field_Expr_Staticness,
      Field_Unit_Name,
      Field_Literal_Origin,
      Field_Type,
      --  Iir_Kind_Simple_Aggregate
      Field_Expr_Staticness,
      Field_Literal_Subtype,
      Field_Literal_Origin,
      Field_Simple_Aggregate_List,
      Field_Type,
      --  Iir_Kind_Overflow_Literal
      Field_Expr_Staticness,
      Field_Literal_Origin,
      Field_Type,
      --  Iir_Kind_Unaffected_Waveform
      Field_Chain,
      --  Iir_Kind_Waveform_Element
      Field_We_Value,
      Field_Time,
      Field_Chain,
      --  Iir_Kind_Conditional_Waveform
      Field_Is_Ref,
      Field_Condition,
      Field_Waveform_Chain,
      Field_Chain,
      --  Iir_Kind_Conditional_Expression
      Field_Is_Ref,
      Field_Condition,
      Field_Expression,
      Field_Chain,
      --  Iir_Kind_Association_Element_By_Expression
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_In_Formal_Flag,
      Field_Formal,
      Field_Chain,
      Field_Actual,
      Field_Actual_Conversion,
      Field_Formal_Conversion,
      --  Iir_Kind_Association_Element_By_Name
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_In_Formal_Flag,
      Field_Formal,
      Field_Chain,
      Field_Actual,
      Field_Actual_Conversion,
      Field_Formal_Conversion,
      --  Iir_Kind_Association_Element_By_Individual
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_In_Formal_Flag,
      Field_Choice_Staticness,
      Field_Formal,
      Field_Chain,
      Field_Individual_Association_Chain,
      Field_Actual_Type_Definition,
      Field_Actual_Type,
      --  Iir_Kind_Association_Element_Open
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_Artificial_Flag,
      Field_In_Formal_Flag,
      Field_Formal,
      Field_Chain,
      --  Iir_Kind_Association_Element_Package
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_In_Formal_Flag,
      Field_Formal,
      Field_Chain,
      Field_Actual,
      --  Iir_Kind_Association_Element_Type
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_In_Formal_Flag,
      Field_Formal,
      Field_Chain,
      Field_Actual,
      Field_Subprogram_Association_Chain,
      Field_Actual_Type,
      --  Iir_Kind_Association_Element_Subprogram
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_In_Formal_Flag,
      Field_Formal,
      Field_Chain,
      Field_Actual,
      --  Iir_Kind_Association_Element_Terminal
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_In_Formal_Flag,
      Field_Formal,
      Field_Chain,
      Field_Actual,
      --  Iir_Kind_Choice_By_Range
      Field_Same_Alternative_Flag,
      Field_Element_Type_Flag,
      Field_Choice_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Choice_Range,
      Field_Associated_Expr,
      Field_Associated_Chain,
      --  Iir_Kind_Choice_By_Expression
      Field_Same_Alternative_Flag,
      Field_Element_Type_Flag,
      Field_Choice_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Choice_Expression,
      Field_Associated_Expr,
      Field_Associated_Chain,
      --  Iir_Kind_Choice_By_Others
      Field_Same_Alternative_Flag,
      Field_Element_Type_Flag,
      Field_Parent,
      Field_Chain,
      Field_Associated_Expr,
      Field_Associated_Chain,
      --  Iir_Kind_Choice_By_None
      Field_Same_Alternative_Flag,
      Field_Element_Type_Flag,
      Field_Parent,
      Field_Chain,
      Field_Associated_Expr,
      Field_Associated_Chain,
      --  Iir_Kind_Choice_By_Name
      Field_Same_Alternative_Flag,
      Field_Element_Type_Flag,
      Field_Parent,
      Field_Chain,
      Field_Choice_Name,
      Field_Associated_Expr,
      Field_Associated_Chain,
      --  Iir_Kind_Entity_Aspect_Entity
      Field_Entity_Name,
      Field_Architecture,
      --  Iir_Kind_Entity_Aspect_Configuration
      Field_Configuration_Name,
      --  Iir_Kind_Entity_Aspect_Open
      --  Iir_Kind_Psl_Hierarchical_Name
      Field_Entity_Name,
      Field_Architecture,
      --  Iir_Kind_Block_Configuration
      Field_Parent,
      Field_Block_Specification,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Configuration_Item_Chain,
      Field_Prev_Block_Configuration,
      --  Iir_Kind_Block_Header
      Field_Generic_Chain,
      Field_Port_Chain,
      Field_Generic_Map_Aspect_Chain,
      Field_Port_Map_Aspect_Chain,
      --  Iir_Kind_Component_Configuration
      Field_Is_Ref,
      Field_Parent,
      Field_Component_Name,
      Field_Instantiation_List,
      Field_Block_Configuration,
      Field_Binding_Indication,
      Field_Chain,
      --  Iir_Kind_Binding_Indication
      Field_Default_Entity_Aspect,
      Field_Entity_Aspect,
      Field_Generic_Map_Aspect_Chain,
      Field_Port_Map_Aspect_Chain,
      --  Iir_Kind_Entity_Class
      Field_Entity_Class,
      Field_Chain,
      --  Iir_Kind_Attribute_Value
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Spec_Chain,
      Field_Type,
      Field_Value_Chain,
      Field_Designated_Entity,
      Field_Attribute_Specification,
      Field_Base_Name,
      --  Iir_Kind_Signature
      Field_Signature_Prefix,
      Field_Type_Marks_List,
      Field_Return_Type_Mark,
      --  Iir_Kind_Aggregate_Info
      Field_Aggr_Min_Length,
      Field_Aggr_Others_Flag,
      Field_Aggr_Dynamic_Flag,
      Field_Aggr_Named_Flag,
      Field_Sub_Aggregate_Info,
      Field_Aggr_Low_Limit,
      Field_Aggr_High_Limit,
      --  Iir_Kind_Procedure_Call
      Field_Prefix,
      Field_Parameter_Association_Chain,
      Field_Implementation,
      Field_Method_Object,
      --  Iir_Kind_Record_Element_Constraint
      Field_Identifier,
      Field_Element_Position,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Type,
      --  Iir_Kind_Array_Element_Resolution
      Field_Resolution_Indication,
      Field_Element_Subtype_Indication,
      --  Iir_Kind_Record_Resolution
      Field_Record_Element_Resolution_Chain,
      --  Iir_Kind_Record_Element_Resolution
      Field_Identifier,
      Field_Chain,
      Field_Resolution_Indication,
      --  Iir_Kind_Break_Element
      Field_Chain,
      Field_Selector_Quantity,
      Field_Break_Quantity,
      Field_Expression,
      --  Iir_Kind_Attribute_Specification
      Field_Entity_Class,
      Field_Static_Attribute_Flag,
      Field_Parent,
      Field_Chain,
      Field_Entity_Name_List,
      Field_Expression,
      Field_Attribute_Value_Spec_Chain,
      Field_Attribute_Designator,
      Field_Attribute_Specification_Chain,
      --  Iir_Kind_Disconnection_Specification
      Field_Is_Ref,
      Field_Parent,
      Field_Signal_List,
      Field_Type_Mark,
      Field_Expression,
      Field_Chain,
      --  Iir_Kind_Step_Limit_Specification
      Field_Is_Ref,
      Field_Parent,
      Field_Quantity_List,
      Field_Type_Mark,
      Field_Expression,
      Field_Chain,
      --  Iir_Kind_Configuration_Specification
      Field_Is_Ref,
      Field_Parent,
      Field_Component_Name,
      Field_Instantiation_List,
      Field_Binding_Indication,
      Field_Chain,
      --  Iir_Kind_Access_Type_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Type_Staticness,
      Field_Designated_Subtype_Indication,
      Field_Designated_Type,
      Field_Type_Declarator,
      Field_Incomplete_Type_Ref_Chain,
      --  Iir_Kind_Incomplete_Type_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Type_Staticness,
      Field_Incomplete_Type_Ref_Chain,
      Field_Type_Declarator,
      Field_Complete_Type_Definition,
      --  Iir_Kind_Interface_Type_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Type_Staticness,
      Field_Type_Declarator,
      Field_Associated_Type,
      --  Iir_Kind_File_Type_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Text_File_Flag,
      Field_Type_Staticness,
      Field_File_Type_Mark,
      Field_Type_Declarator,
      --  Iir_Kind_Protected_Type_Declaration
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Type_Staticness,
      Field_Declaration_Chain,
      Field_Protected_Type_Body,
      Field_Type_Declarator,
      Field_Attribute_Value_Chain,
      --  Iir_Kind_Record_Type_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Type_Staticness,
      Field_Constraint_State,
      Field_Elements_Declaration_List,
      Field_Type_Declarator,
      --  Iir_Kind_Array_Type_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Index_Constraint_Flag,
      Field_Type_Staticness,
      Field_Constraint_State,
      Field_Index_Subtype_Definition_List,
      Field_Element_Subtype_Indication,
      Field_Index_Subtype_List,
      Field_Element_Subtype,
      Field_Type_Declarator,
      --  Iir_Kind_Array_Subtype_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Index_Constraint_Flag,
      Field_Has_Array_Constraint_Flag,
      Field_Has_Element_Constraint_Flag,
      Field_Type_Staticness,
      Field_Constraint_State,
      Field_Subtype_Type_Mark,
      Field_Resolution_Indication,
      Field_Index_Constraint_List,
      Field_Index_Subtype_List,
      Field_Array_Element_Constraint,
      Field_Tolerance,
      Field_Element_Subtype,
      Field_Type_Declarator,
      Field_Parent_Type,
      --  Iir_Kind_Record_Subtype_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Type_Staticness,
      Field_Constraint_State,
      Field_Owned_Elements_Chain,
      Field_Elements_Declaration_List,
      Field_Subtype_Type_Mark,
      Field_Type_Declarator,
      Field_Parent_Type,
      Field_Resolution_Indication,
      Field_Tolerance,
      --  Iir_Kind_Access_Subtype_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Type_Staticness,
      Field_Designated_Type,
      Field_Subtype_Type_Mark,
      Field_Type_Declarator,
      Field_Parent_Type,
      Field_Designated_Subtype_Indication,
      --  Iir_Kind_Physical_Subtype_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Type_Staticness,
      Field_Range_Constraint,
      Field_Subtype_Type_Mark,
      Field_Type_Declarator,
      Field_Parent_Type,
      Field_Resolution_Indication,
      --  Iir_Kind_Floating_Subtype_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Type_Staticness,
      Field_Range_Constraint,
      Field_Subtype_Type_Mark,
      Field_Type_Declarator,
      Field_Parent_Type,
      Field_Resolution_Indication,
      Field_Tolerance,
      --  Iir_Kind_Integer_Subtype_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Type_Staticness,
      Field_Range_Constraint,
      Field_Subtype_Type_Mark,
      Field_Type_Declarator,
      Field_Parent_Type,
      Field_Resolution_Indication,
      --  Iir_Kind_Enumeration_Subtype_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Type_Staticness,
      Field_Range_Constraint,
      Field_Subtype_Type_Mark,
      Field_Type_Declarator,
      Field_Parent_Type,
      Field_Resolution_Indication,
      --  Iir_Kind_Enumeration_Type_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Only_Characters_Flag,
      Field_Is_Character_Type,
      Field_Scalar_Size,
      Field_Type_Staticness,
      Field_Enumeration_Literal_List,
      Field_Range_Constraint,
      Field_Type_Declarator,
      --  Iir_Kind_Integer_Type_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Scalar_Size,
      Field_Type_Staticness,
      Field_Range_Constraint,
      Field_Type_Declarator,
      --  Iir_Kind_Floating_Type_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Scalar_Size,
      Field_Type_Staticness,
      Field_Range_Constraint,
      Field_Type_Declarator,
      --  Iir_Kind_Physical_Type_Definition
      Field_Resolved_Flag,
      Field_Is_Ref,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Scalar_Size,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Type_Staticness,
      Field_Range_Constraint,
      Field_Unit_Chain,
      Field_Type_Declarator,
      --  Iir_Kind_Range_Expression
      Field_Direction,
      Field_Expr_Staticness,
      Field_Left_Limit_Expr,
      Field_Right_Limit_Expr,
      Field_Range_Origin,
      Field_Type,
      Field_Left_Limit,
      Field_Right_Limit,
      --  Iir_Kind_Protected_Type_Body
      Field_Identifier,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Protected_Type_Declaration,
      Field_Attribute_Value_Chain,
      --  Iir_Kind_Wildcard_Type_Definition
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Type_Staticness,
      Field_Type_Declarator,
      --  Iir_Kind_Foreign_Vector_Type_Definition
      Field_Type_Declarator,
      --  Iir_Kind_Subtype_Definition
      Field_Is_Ref,
      Field_Range_Constraint,
      Field_Subtype_Type_Mark,
      Field_Type_Declarator,
      Field_Parent_Type,
      Field_Resolution_Indication,
      Field_Tolerance,
      --  Iir_Kind_Scalar_Nature_Definition
      Field_Nature_Staticness,
      Field_Reference,
      Field_Nature_Declarator,
      Field_Base_Nature,
      Field_Across_Type_Mark,
      Field_Through_Type_Mark,
      Field_Across_Type,
      Field_Through_Type,
      --  Iir_Kind_Record_Nature_Definition
      Field_Is_Ref,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Nature_Staticness,
      Field_Constraint_State,
      Field_Elements_Declaration_List,
      Field_Nature_Declarator,
      Field_Base_Nature,
      Field_Across_Type_Definition,
      Field_Through_Type_Definition,
      Field_Across_Type,
      Field_Through_Type,
      Field_Simple_Nature,
      --  Iir_Kind_Array_Nature_Definition
      Field_Index_Constraint_Flag,
      Field_Nature_Staticness,
      Field_Constraint_State,
      Field_Index_Subtype_Definition_List,
      Field_Element_Subnature_Indication,
      Field_Index_Subtype_List,
      Field_Element_Subnature,
      Field_Nature_Declarator,
      Field_Base_Nature,
      Field_Simple_Nature,
      Field_Across_Type_Definition,
      Field_Through_Type_Definition,
      Field_Across_Type,
      Field_Through_Type,
      --  Iir_Kind_Array_Subnature_Definition
      Field_Index_Constraint_Flag,
      Field_Nature_Staticness,
      Field_Constraint_State,
      Field_Subnature_Nature_Mark,
      Field_Index_Constraint_List,
      Field_Index_Subtype_List,
      Field_Array_Element_Constraint,
      Field_Tolerance,
      Field_Element_Subnature,
      Field_Nature_Declarator,
      Field_Base_Nature,
      Field_Across_Type_Definition,
      Field_Through_Type_Definition,
      Field_Across_Type,
      Field_Through_Type,
      --  Iir_Kind_Overload_List
      Field_Overload_List,
      --  Iir_Kind_Foreign_Module
      Field_Foreign_Node,
      Field_Identifier,
      Field_Design_Unit,
      Field_Generic_Chain,
      Field_Port_Chain,
      --  Iir_Kind_Entity_Declaration
      Field_Identifier,
      Field_Has_Begin,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Generic_Chain,
      Field_Port_Chain,
      Field_Declaration_Chain,
      Field_Concurrent_Statement_Chain,
      Field_Attribute_Value_Chain,
      Field_Bound_Vunit_Chain,
      --  Iir_Kind_Configuration_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Entity_Name,
      Field_Attribute_Value_Chain,
      Field_Block_Configuration,
      --  Iir_Kind_Context_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Context_Items,
      --  Iir_Kind_Package_Declaration
      Field_Identifier,
      Field_Need_Body,
      Field_Macro_Expanded_Flag,
      Field_Need_Instance_Bodies,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Package_Header,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Package_Body,
      Field_Package_Origin,
      --  Iir_Kind_Package_Instantiation_Declaration
      Field_Instance_Source_File,
      Field_Identifier,
      Field_Visible_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Uninstantiated_Package_Name,
      Field_Uninstantiated_Package_Decl,
      Field_Generic_Chain,
      Field_Generic_Map_Aspect_Chain,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Instance_Package_Body,
      --  Iir_Kind_Vmode_Declaration
      Field_Identifier,
      Field_Has_Begin,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Chain,
      Field_Hierarchical_Name,
      Field_Attribute_Value_Chain,
      Field_Vunit_Item_Chain,
      Field_Verification_Block_Configuration,
      --  Iir_Kind_Vprop_Declaration
      Field_Identifier,
      Field_Has_Begin,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Chain,
      Field_Hierarchical_Name,
      Field_Attribute_Value_Chain,
      Field_Vunit_Item_Chain,
      Field_Verification_Block_Configuration,
      --  Iir_Kind_Vunit_Declaration
      Field_Identifier,
      Field_Has_Begin,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Chain,
      Field_Hierarchical_Name,
      Field_Attribute_Value_Chain,
      Field_Vunit_Item_Chain,
      Field_Verification_Block_Configuration,
      Field_Bound_Vunit_Chain,
      --  Iir_Kind_Package_Body
      Field_Identifier,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Package,
      --  Iir_Kind_Architecture_Body
      Field_Identifier,
      Field_Foreign_Flag,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Entity_Name,
      Field_Declaration_Chain,
      Field_Concurrent_Statement_Chain,
      Field_Attribute_Value_Chain,
      Field_Default_Configuration_Declaration,
      Field_Bound_Vunit_Chain,
      --  Iir_Kind_Type_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Type_Definition,
      Field_Chain,
      Field_Incomplete_Type_Declaration,
      --  Iir_Kind_Anonymous_Type_Declaration
      Field_Identifier,
      Field_Parent,
      Field_Type_Definition,
      Field_Chain,
      Field_Subtype_Definition,
      Field_Incomplete_Type_Declaration,
      --  Iir_Kind_Subtype_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Type,
      --  Iir_Kind_Nature_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Nature_Definition,
      Field_Chain,
      --  Iir_Kind_Subnature_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Subnature_Indication,
      Field_Nature,
      Field_Chain,
      --  Iir_Kind_Package_Header
      Field_Generic_Chain,
      Field_Generic_Map_Aspect_Chain,
      --  Iir_Kind_Unit_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_Physical_Literal,
      --  Iir_Kind_Library_Declaration
      Field_Identifier,
      Field_Date,
      Field_Library_Directory,
      Field_Vendor_Library_Flag,
      Field_Elab_Flag,
      Field_Visible_Flag,
      Field_Design_File_Chain,
      Field_Chain,
      --  Iir_Kind_Component_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Has_Is,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Chain,
      Field_Generic_Chain,
      Field_Port_Chain,
      --  Iir_Kind_Attribute_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_Type_Mark,
      --  Iir_Kind_Group_Template_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Entity_Class_Entry_Chain,
      Field_Chain,
      --  Iir_Kind_Group_Declaration
      Field_Identifier,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Group_Constituent_List,
      Field_Chain,
      Field_Group_Template_Name,
      --  Iir_Kind_Element_Declaration
      Field_Identifier,
      Field_Element_Position,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Parent,
      Field_Subtype_Indication,
      Field_Type,
      --  Iir_Kind_Nature_Element_Declaration
      Field_Identifier,
      Field_Element_Position,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Parent,
      Field_Subnature_Indication,
      Field_Nature,
      --  Iir_Kind_Non_Object_Alias_Declaration
      Field_Identifier,
      Field_Implicit_Alias_Flag,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Chain,
      Field_Name,
      Field_Alias_Signature,
      --  Iir_Kind_Psl_Declaration
      Field_Identifier,
      Field_Psl_Declaration,
      Field_PSL_Clock,
      Field_PSL_NFA,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Parent,
      Field_Chain,
      --  Iir_Kind_Psl_Endpoint_Declaration
      Field_Identifier,
      Field_Psl_Declaration,
      Field_PSL_Clock,
      Field_PSL_NFA,
      Field_PSL_Nbr_States,
      Field_PSL_EOS_Flag,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_PSL_Clock_Sensitivity,
      --  Iir_Kind_Enumeration_Literal
      Field_Identifier,
      Field_Subprogram_Hash,
      Field_Enum_Pos,
      Field_Seen_Flag,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Literal_Origin,
      --  Iir_Kind_Function_Declaration
      Field_Subprogram_Depth,
      Field_Overload_Number,
      Field_Identifier,
      Field_Subprogram_Hash,
      Field_Implicit_Definition,
      Field_Seen_Flag,
      Field_Has_Parameter,
      Field_Hide_Implicit_Flag,
      Field_Resolution_Function_Flag,
      Field_Pure_Flag,
      Field_Foreign_Flag,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Use_Flag,
      Field_Has_Pure,
      Field_Has_Body,
      Field_Wait_State,
      Field_All_Sensitized_State,
      Field_Parent,
      Field_Return_Type,
      Field_Chain,
      Field_Interface_Declaration_Chain,
      Field_Generic_Chain,
      Field_Return_Type_Mark,
      Field_Subprogram_Body,
      Field_Return_Identifier,
      --  Iir_Kind_Procedure_Declaration
      Field_Subprogram_Depth,
      Field_Overload_Number,
      Field_Identifier,
      Field_Subprogram_Hash,
      Field_Implicit_Definition,
      Field_Seen_Flag,
      Field_Has_Parameter,
      Field_Suspend_Flag,
      Field_Hide_Implicit_Flag,
      Field_Passive_Flag,
      Field_Foreign_Flag,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Use_Flag,
      Field_Has_Body,
      Field_Wait_State,
      Field_Purity_State,
      Field_All_Sensitized_State,
      Field_Parent,
      Field_Chain,
      Field_Interface_Declaration_Chain,
      Field_Generic_Chain,
      Field_Subprogram_Body,
      --  Iir_Kind_Function_Body
      Field_Impure_Depth,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Sequential_Statement_Chain,
      Field_Subprogram_Specification,
      Field_Callees_List,
      --  Iir_Kind_Procedure_Body
      Field_Impure_Depth,
      Field_Suspend_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Sequential_Statement_Chain,
      Field_Subprogram_Specification,
      Field_Callees_List,
      --  Iir_Kind_Function_Instantiation_Declaration
      Field_Instance_Source_File,
      Field_Identifier,
      Field_Subprogram_Hash,
      Field_Visible_Flag,
      Field_Parent,
      Field_Return_Type,
      Field_Chain,
      Field_Interface_Declaration_Chain,
      Field_Generic_Chain,
      Field_Uninstantiated_Subprogram_Name,
      Field_Generic_Map_Aspect_Chain,
      --  Iir_Kind_Procedure_Instantiation_Declaration
      Field_Instance_Source_File,
      Field_Identifier,
      Field_Subprogram_Hash,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_Interface_Declaration_Chain,
      Field_Generic_Chain,
      Field_Uninstantiated_Subprogram_Name,
      Field_Generic_Map_Aspect_Chain,
      --  Iir_Kind_Terminal_Declaration
      Field_Identifier,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subnature_Indication,
      Field_Nature,
      --  Iir_Kind_Object_Alias_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_After_Drivers_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Name,
      Field_Subtype_Indication,
      Field_Type,
      --  Iir_Kind_Free_Quantity_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Spectrum_Quantity_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Magnitude_Expression,
      Field_Phase_Expression,
      Field_Type,
      --  Iir_Kind_Noise_Quantity_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Power_Expression,
      Field_Type,
      --  Iir_Kind_Across_Quantity_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_Default_Value,
      Field_Tolerance,
      Field_Plus_Terminal_Name,
      Field_Minus_Terminal_Name,
      Field_Plus_Terminal,
      Field_Minus_Terminal,
      --  Iir_Kind_Through_Quantity_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_Default_Value,
      Field_Tolerance,
      Field_Plus_Terminal_Name,
      Field_Minus_Terminal_Name,
      Field_Plus_Terminal,
      Field_Minus_Terminal,
      --  Iir_Kind_File_Declaration
      Field_Identifier,
      Field_Has_Mode,
      Field_Is_Ref,
      Field_Mode,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_Subtype_Indication,
      Field_File_Logical_Name,
      Field_File_Open_Kind,
      --  Iir_Kind_Guard_Signal_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Active_Flag,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Guarded_Signal_Flag,
      Field_Signal_Kind,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Guard_Expression,
      Field_Guard_Sensitivity_List,
      Field_Block_Statement,
      --  Iir_Kind_Signal_Declaration
      Field_Identifier,
      Field_Has_Disconnect_Flag,
      Field_Is_Ref,
      Field_Has_Active_Flag,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_After_Drivers_Flag,
      Field_Use_Flag,
      Field_Guarded_Signal_Flag,
      Field_Signal_Kind,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Variable_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Shared_Flag,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Constant_Declaration
      Field_Identifier,
      Field_Deferred_Declaration_Flag,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      Field_Deferred_Declaration,
      --  Iir_Kind_Iterator_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Discrete_Range,
      Field_Type,
      --  Iir_Kind_Interface_Constant_Declaration
      Field_Identifier,
      Field_Has_Mode,
      Field_Has_Class,
      Field_Is_Ref,
      Field_Mode,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_After_Drivers_Flag,
      Field_Use_Flag,
      Field_Open_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Interface_Variable_Declaration
      Field_Identifier,
      Field_Has_Mode,
      Field_Has_Class,
      Field_Is_Ref,
      Field_Mode,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_After_Drivers_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Interface_Signal_Declaration
      Field_Identifier,
      Field_Has_Disconnect_Flag,
      Field_Has_Mode,
      Field_Has_Class,
      Field_Is_Ref,
      Field_Mode,
      Field_Has_Active_Flag,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_After_Drivers_Flag,
      Field_Use_Flag,
      Field_Open_Flag,
      Field_Guarded_Signal_Flag,
      Field_Signal_Kind,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Interface_File_Declaration
      Field_Identifier,
      Field_Has_Mode,
      Field_Has_Class,
      Field_Is_Ref,
      Field_Mode,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_After_Drivers_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Interface_Quantity_Declaration
      Field_Identifier,
      Field_Has_Mode,
      Field_Has_Class,
      Field_Is_Ref,
      Field_Mode,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_After_Drivers_Flag,
      Field_Use_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subtype_Indication,
      Field_Default_Value,
      Field_Type,
      --  Iir_Kind_Interface_Terminal_Declaration
      Field_Identifier,
      Field_Has_Mode,
      Field_Has_Class,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Name_Staticness,
      Field_Parent,
      Field_Chain,
      Field_Subnature_Indication,
      Field_Nature,
      --  Iir_Kind_Interface_Type_Declaration
      Field_Identifier,
      Field_Is_Ref,
      Field_Has_Identifier_List,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Open_Flag,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_Interface_Type_Subprograms,
      --  Iir_Kind_Interface_Package_Declaration
      Field_Instance_Source_File,
      Field_Identifier,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Open_Flag,
      Field_Parent,
      Field_Uninstantiated_Package_Name,
      Field_Uninstantiated_Package_Decl,
      Field_Generic_Chain,
      Field_Generic_Map_Aspect_Chain,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      --  Iir_Kind_Interface_Function_Declaration
      Field_Subprogram_Depth,
      Field_Identifier,
      Field_Subprogram_Hash,
      Field_Seen_Flag,
      Field_Has_Parameter,
      Field_Resolution_Function_Flag,
      Field_Pure_Flag,
      Field_Foreign_Flag,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Open_Flag,
      Field_Has_Pure,
      Field_All_Sensitized_State,
      Field_Parent,
      Field_Return_Type,
      Field_Chain,
      Field_Interface_Declaration_Chain,
      Field_Return_Type_Mark,
      --  Iir_Kind_Interface_Procedure_Declaration
      Field_Subprogram_Depth,
      Field_Identifier,
      Field_Subprogram_Hash,
      Field_Seen_Flag,
      Field_Has_Parameter,
      Field_Foreign_Flag,
      Field_Visible_Flag,
      Field_Use_Flag,
      Field_Open_Flag,
      Field_All_Sensitized_State,
      Field_Parent,
      Field_Chain,
      Field_Interface_Declaration_Chain,
      Field_Return_Type_Mark,
      --  Iir_Kind_Signal_Attribute_Declaration
      Field_Parent,
      Field_Chain,
      Field_Signal_Attribute_Chain,
      --  Iir_Kind_Suspend_State_Declaration
      Field_Parent,
      Field_Chain,
      Field_Suspend_State_Chain,
      --  Iir_Kind_Identity_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Negation_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Absolute_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Not_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Implicit_Condition_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Condition_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Reduction_And_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Reduction_Or_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Reduction_Nand_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Reduction_Nor_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Reduction_Xor_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_Reduction_Xnor_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Operand,
      Field_Implementation,
      --  Iir_Kind_And_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Or_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Nand_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Nor_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Xor_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Xnor_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Equality_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Inequality_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Less_Than_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Less_Than_Or_Equal_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Greater_Than_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Greater_Than_Or_Equal_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Match_Equality_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Match_Inequality_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Match_Less_Than_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Match_Less_Than_Or_Equal_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Match_Greater_Than_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Match_Greater_Than_Or_Equal_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Sll_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Sla_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Srl_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Sra_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Rol_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Ror_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Addition_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Substraction_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Concatenation_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Multiplication_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Division_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Modulus_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Remainder_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Exponentiation_Operator
      Field_Expr_Staticness,
      Field_Type,
      Field_Left,
      Field_Implementation,
      Field_Right,
      --  Iir_Kind_Function_Call
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter_Association_Chain,
      Field_Implementation,
      Field_Method_Object,
      Field_Base_Name,
      --  Iir_Kind_Aggregate
      Field_Aggregate_Expand_Flag,
      Field_Expr_Staticness,
      Field_Association_Choices_Chain,
      Field_Literal_Subtype,
      Field_Literal_Origin,
      Field_Aggregate_Info,
      Field_Type,
      --  Iir_Kind_Parenthesis_Expression
      Field_Expr_Staticness,
      Field_Expression,
      Field_Type,
      --  Iir_Kind_Qualified_Expression
      Field_Expr_Staticness,
      Field_Type_Mark,
      Field_Expression,
      Field_Type,
      --  Iir_Kind_Type_Conversion
      Field_Expr_Staticness,
      Field_Type,
      Field_Type_Conversion_Subtype,
      Field_Type_Mark,
      Field_Expression,
      --  Iir_Kind_Allocator_By_Expression
      Field_Is_Ref,
      Field_Expr_Staticness,
      Field_Type,
      Field_Expression,
      Field_Allocator_Designated_Type,
      --  Iir_Kind_Allocator_By_Subtype
      Field_Is_Ref,
      Field_Expr_Staticness,
      Field_Type,
      Field_Subtype_Indication,
      Field_Allocator_Subtype,
      Field_Allocator_Designated_Type,
      --  Iir_Kind_Selected_Element
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Named_Entity,
      Field_Base_Name,
      --  Iir_Kind_Dereference
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Implicit_Dereference
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Slice_Name
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Suffix,
      Field_Slice_Subtype,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Indexed_Name
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_List,
      Field_Base_Name,
      --  Iir_Kind_Psl_Prev
      Field_Expr_Staticness,
      Field_Type,
      Field_Expression,
      Field_Count_Expression,
      Field_Clock_Expression,
      Field_Default_Clock,
      --  Iir_Kind_Psl_Stable
      Field_Expr_Staticness,
      Field_Type,
      Field_Expression,
      Field_Clock_Expression,
      Field_Default_Clock,
      --  Iir_Kind_Psl_Rose
      Field_Expr_Staticness,
      Field_Type,
      Field_Expression,
      Field_Clock_Expression,
      Field_Default_Clock,
      --  Iir_Kind_Psl_Fell
      Field_Expr_Staticness,
      Field_Type,
      Field_Expression,
      Field_Clock_Expression,
      Field_Default_Clock,
      --  Iir_Kind_Psl_Onehot
      Field_Expr_Staticness,
      Field_Type,
      Field_Expression,
      --  Iir_Kind_Psl_Onehot0
      Field_Expr_Staticness,
      Field_Type,
      Field_Expression,
      --  Iir_Kind_Psl_Expression
      Field_Psl_Expression,
      Field_Type,
      --  Iir_Kind_Sensitized_Process_Statement
      Field_Label,
      Field_Seen_Flag,
      Field_End_Has_Postponed,
      Field_Is_Ref,
      Field_Passive_Flag,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Has_Label,
      Field_Has_Is,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Wait_State,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Process_Origin,
      Field_Sequential_Statement_Chain,
      Field_Sensitivity_List,
      Field_Callees_List,
      --  Iir_Kind_Process_Statement
      Field_Label,
      Field_Seen_Flag,
      Field_End_Has_Postponed,
      Field_Suspend_Flag,
      Field_Passive_Flag,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Has_Label,
      Field_Has_Is,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Wait_State,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Process_Origin,
      Field_Sequential_Statement_Chain,
      Field_Callees_List,
      --  Iir_Kind_Concurrent_Simple_Signal_Assignment
      Field_Label,
      Field_Delay_Mechanism,
      Field_Is_Ref,
      Field_Has_Delay_Mechanism,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Reject_Time_Expression,
      Field_Waveform_Chain,
      Field_Guard,
      --  Iir_Kind_Concurrent_Conditional_Signal_Assignment
      Field_Label,
      Field_Delay_Mechanism,
      Field_Is_Ref,
      Field_Has_Delay_Mechanism,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Reject_Time_Expression,
      Field_Conditional_Waveform_Chain,
      Field_Guard,
      --  Iir_Kind_Concurrent_Selected_Signal_Assignment
      Field_Label,
      Field_Delay_Mechanism,
      Field_Is_Ref,
      Field_Has_Delay_Mechanism,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Reject_Time_Expression,
      Field_Expression,
      Field_Selected_Waveform_Chain,
      Field_Guard,
      --  Iir_Kind_Concurrent_Assertion_Statement
      Field_Label,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Parent,
      Field_Assertion_Condition,
      Field_Chain,
      Field_Severity_Expression,
      Field_Report_Expression,
      --  Iir_Kind_Concurrent_Procedure_Call_Statement
      Field_Label,
      Field_Suspend_Flag,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Parent,
      Field_Procedure_Call,
      Field_Chain,
      --  Iir_Kind_Concurrent_Break_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Condition,
      Field_Chain,
      Field_Sensitivity_List,
      Field_Break_Element,
      --  Iir_Kind_Psl_Assert_Directive
      Field_Psl_Property,
      Field_Label,
      Field_PSL_Clock,
      Field_PSL_NFA,
      Field_PSL_Nbr_States,
      Field_PSL_EOS_Flag,
      Field_PSL_Abort_Flag,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_Severity_Expression,
      Field_Report_Expression,
      Field_PSL_Clock_Sensitivity,
      --  Iir_Kind_Psl_Assume_Directive
      Field_Psl_Property,
      Field_Label,
      Field_PSL_Clock,
      Field_PSL_NFA,
      Field_PSL_Nbr_States,
      Field_PSL_EOS_Flag,
      Field_PSL_Abort_Flag,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_PSL_Clock_Sensitivity,
      --  Iir_Kind_Psl_Cover_Directive
      Field_Psl_Sequence,
      Field_Label,
      Field_PSL_Clock,
      Field_PSL_NFA,
      Field_PSL_Nbr_States,
      Field_PSL_EOS_Flag,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_Report_Expression,
      Field_PSL_Clock_Sensitivity,
      --  Iir_Kind_Psl_Restrict_Directive
      Field_Psl_Sequence,
      Field_Label,
      Field_PSL_Clock,
      Field_PSL_NFA,
      Field_PSL_Nbr_States,
      Field_PSL_EOS_Flag,
      Field_Postponed_Flag,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_PSL_Clock_Sensitivity,
      --  Iir_Kind_Block_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Has_Is,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Guard_Decl,
      Field_Block_Header,
      Field_Declaration_Chain,
      Field_Chain,
      Field_Attribute_Value_Chain,
      Field_Concurrent_Statement_Chain,
      Field_Block_Block_Configuration,
      --  Iir_Kind_If_Generate_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Condition,
      Field_Chain,
      Field_Generate_Statement_Body,
      Field_Generate_Else_Clause,
      --  Iir_Kind_Case_Generate_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Case_Statement_Alternative_Chain,
      Field_Chain,
      Field_Expression,
      --  Iir_Kind_For_Generate_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Parameter_Specification,
      Field_Chain,
      Field_Generate_Statement_Body,
      --  Iir_Kind_Component_Instantiation_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Has_Component,
      Field_Parent,
      Field_Instantiated_Unit,
      Field_Chain,
      Field_Default_Binding_Indication,
      Field_Generic_Map_Aspect_Chain,
      Field_Port_Map_Aspect_Chain,
      Field_Configuration_Specification,
      Field_Component_Configuration,
      --  Iir_Kind_Psl_Default_Clock
      Field_Psl_Boolean,
      Field_Parent,
      Field_Chain,
      --  Iir_Kind_Generate_Statement_Body
      Field_Alternative_Label,
      Field_Has_Begin,
      Field_Has_End,
      Field_Is_Within_Flag,
      Field_Has_Label,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Declaration_Chain,
      Field_Generate_Block_Configuration,
      Field_Attribute_Value_Chain,
      Field_Concurrent_Statement_Chain,
      --  Iir_Kind_If_Generate_Else_Clause
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Condition,
      Field_Generate_Statement_Body,
      Field_Generate_Else_Clause,
      --  Iir_Kind_Simple_Simultaneous_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_Simultaneous_Left,
      Field_Simultaneous_Right,
      Field_Tolerance,
      --  Iir_Kind_Simultaneous_Null_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      --  Iir_Kind_Simultaneous_Procedural_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_Has_Is,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Chain,
      Field_Declaration_Chain,
      Field_Sequential_Statement_Chain,
      Field_Attribute_Value_Chain,
      --  Iir_Kind_Simultaneous_Case_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Case_Statement_Alternative_Chain,
      Field_Chain,
      Field_Expression,
      --  Iir_Kind_Simultaneous_If_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Condition,
      Field_Simultaneous_Statement_Chain,
      Field_Else_Clause,
      Field_Chain,
      --  Iir_Kind_Simultaneous_Elsif
      Field_Is_Ref,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Condition,
      Field_Simultaneous_Statement_Chain,
      Field_Else_Clause,
      --  Iir_Kind_Simple_Signal_Assignment_Statement
      Field_Label,
      Field_Delay_Mechanism,
      Field_Is_Ref,
      Field_Has_Delay_Mechanism,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Reject_Time_Expression,
      Field_Waveform_Chain,
      --  Iir_Kind_Conditional_Signal_Assignment_Statement
      Field_Label,
      Field_Delay_Mechanism,
      Field_Is_Ref,
      Field_Has_Delay_Mechanism,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Reject_Time_Expression,
      Field_Conditional_Waveform_Chain,
      --  Iir_Kind_Selected_Waveform_Assignment_Statement
      Field_Label,
      Field_Delay_Mechanism,
      Field_Is_Ref,
      Field_Has_Delay_Mechanism,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Reject_Time_Expression,
      Field_Expression,
      Field_Selected_Waveform_Chain,
      --  Iir_Kind_Signal_Force_Assignment_Statement
      Field_Label,
      Field_Force_Mode,
      Field_Is_Ref,
      Field_Has_Force_Mode,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Expression,
      --  Iir_Kind_Signal_Release_Assignment_Statement
      Field_Label,
      Field_Force_Mode,
      Field_Is_Ref,
      Field_Has_Force_Mode,
      Field_Visible_Flag,
      Field_Guarded_Target_State,
      Field_Parent,
      Field_Target,
      Field_Chain,
      --  Iir_Kind_Null_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      --  Iir_Kind_Assertion_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Parent,
      Field_Assertion_Condition,
      Field_Chain,
      Field_Severity_Expression,
      Field_Report_Expression,
      --  Iir_Kind_Report_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Parent,
      Field_Chain,
      Field_Severity_Expression,
      Field_Report_Expression,
      --  Iir_Kind_Wait_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Timeout_Clause,
      Field_Chain,
      Field_Condition_Clause,
      Field_Sensitivity_List,
      --  Iir_Kind_Variable_Assignment_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Expression,
      --  Iir_Kind_Conditional_Variable_Assignment_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Target,
      Field_Chain,
      Field_Conditional_Expression_Chain,
      --  Iir_Kind_Return_Statement
      Field_Label,
      Field_Visible_Flag,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_Expression,
      --  Iir_Kind_For_Loop_Statement
      Field_Label,
      Field_Exit_Flag,
      Field_Suspend_Flag,
      Field_Next_Flag,
      Field_Visible_Flag,
      Field_Is_Within_Flag,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Parameter_Specification,
      Field_Chain,
      Field_Sequential_Statement_Chain,
      --  Iir_Kind_While_Loop_Statement
      Field_Label,
      Field_Exit_Flag,
      Field_Suspend_Flag,
      Field_Is_Ref,
      Field_Next_Flag,
      Field_Visible_Flag,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Condition,
      Field_Chain,
      Field_Sequential_Statement_Chain,
      --  Iir_Kind_Next_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Condition,
      Field_Chain,
      Field_Loop_Label,
      --  Iir_Kind_Exit_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Condition,
      Field_Chain,
      Field_Loop_Label,
      --  Iir_Kind_Case_Statement
      Field_Label,
      Field_Matching_Flag,
      Field_Suspend_Flag,
      Field_Visible_Flag,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Expression,
      Field_Case_Statement_Alternative_Chain,
      Field_Chain,
      --  Iir_Kind_Procedure_Call_Statement
      Field_Label,
      Field_Suspend_Flag,
      Field_Visible_Flag,
      Field_Parent,
      Field_Procedure_Call,
      Field_Chain,
      --  Iir_Kind_Break_Statement
      Field_Label,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_Parent,
      Field_Condition,
      Field_Chain,
      Field_Break_Element,
      --  Iir_Kind_If_Statement
      Field_Label,
      Field_Suspend_Flag,
      Field_Is_Ref,
      Field_Visible_Flag,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Condition,
      Field_Sequential_Statement_Chain,
      Field_Else_Clause,
      Field_Chain,
      --  Iir_Kind_Suspend_State_Statement
      Field_Suspend_State_Index,
      Field_Parent,
      Field_Chain,
      Field_Suspend_State_Chain,
      --  Iir_Kind_Elsif
      Field_Is_Ref,
      Field_End_Has_Identifier,
      Field_Parent,
      Field_Condition,
      Field_Sequential_Statement_Chain,
      Field_Else_Clause,
      --  Iir_Kind_Character_Literal
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Type,
      Field_Named_Entity,
      Field_Base_Name,
      --  Iir_Kind_Simple_Name
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Type,
      Field_Named_Entity,
      Field_Base_Name,
      --  Iir_Kind_Selected_Name
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Named_Entity,
      Field_Base_Name,
      --  Iir_Kind_Operator_Symbol
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Type,
      Field_Named_Entity,
      Field_Base_Name,
      --  Iir_Kind_Reference_Name
      Field_Is_Forward_Ref,
      Field_Expr_Staticness,
      Field_Type,
      Field_Named_Entity,
      Field_Referenced_Name,
      --  Iir_Kind_External_Constant_Name
      Field_Is_Ref,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_External_Pathname,
      Field_Named_Entity,
      Field_Subtype_Indication,
      --  Iir_Kind_External_Signal_Name
      Field_Is_Ref,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_External_Pathname,
      Field_Named_Entity,
      Field_Subtype_Indication,
      --  Iir_Kind_External_Variable_Name
      Field_Is_Ref,
      Field_Shared_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Parent,
      Field_Type,
      Field_Chain,
      Field_External_Pathname,
      Field_Named_Entity,
      Field_Subtype_Indication,
      --  Iir_Kind_Selected_By_All_Name
      Field_Is_Forward_Ref,
      Field_Expr_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Named_Entity,
      Field_Base_Name,
      --  Iir_Kind_Parenthesis_Name
      Field_Is_Forward_Ref,
      Field_Prefix,
      Field_Type,
      Field_Association_Chain,
      Field_Named_Entity,
      --  Iir_Kind_Package_Pathname
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Pathname_Suffix,
      Field_Named_Entity,
      --  Iir_Kind_Absolute_Pathname
      Field_Pathname_Suffix,
      --  Iir_Kind_Relative_Pathname
      Field_Pathname_Suffix,
      --  Iir_Kind_Pathname_Element
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Pathname_Suffix,
      Field_Named_Entity,
      Field_Pathname_Expression,
      --  Iir_Kind_Base_Attribute
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Subtype_Attribute
      Field_Type_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Element_Attribute
      Field_Type_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Across_Attribute
      Field_Type_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Through_Attribute
      Field_Type_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Nature_Reference_Attribute
      Field_Name_Staticness,
      Field_Prefix,
      Field_Nature,
      Field_Base_Name,
      --  Iir_Kind_Left_Type_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Right_Type_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_High_Type_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Low_Type_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Ascending_Type_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Image_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Value_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Pos_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Val_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Succ_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Pred_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Leftof_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Rightof_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Signal_Slew_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Parameter,
      Field_Parameter_2,
      Field_Base_Name,
      --  Iir_Kind_Quantity_Slew_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Parameter,
      Field_Parameter_2,
      Field_Base_Name,
      --  Iir_Kind_Ramp_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Parameter,
      Field_Parameter_2,
      Field_Base_Name,
      --  Iir_Kind_Zoh_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Parameter,
      Field_Parameter_2,
      Field_Base_Name,
      --  Iir_Kind_Ltf_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Parameter,
      Field_Parameter_2,
      Field_Base_Name,
      --  Iir_Kind_Ztf_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Parameter,
      Field_Parameter_2,
      Field_Parameter_3,
      Field_Parameter_4,
      Field_Base_Name,
      --  Iir_Kind_Dot_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Base_Name,
      --  Iir_Kind_Integ_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Base_Name,
      --  Iir_Kind_Above_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Quantity_Delayed_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Delayed_Attribute
      Field_Has_Active_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Parameter,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Base_Name,
      --  Iir_Kind_Stable_Attribute
      Field_Has_Active_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Parameter,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Base_Name,
      --  Iir_Kind_Quiet_Attribute
      Field_Has_Active_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Parameter,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Base_Name,
      --  Iir_Kind_Transaction_Attribute
      Field_Has_Active_Flag,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Parameter,
      Field_Type,
      Field_Attr_Chain,
      Field_Signal_Attribute_Declaration,
      Field_Base_Name,
      --  Iir_Kind_Event_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Active_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Last_Event_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Last_Active_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Last_Value_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Driving_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Driving_Value_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      --  Iir_Kind_Behavior_Attribute
      --  Iir_Kind_Structure_Attribute
      --  Iir_Kind_Simple_Name_Attribute
      Field_Simple_Name_Identifier,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Simple_Name_Subtype,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Instance_Name_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Path_Name_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Base_Name,
      --  Iir_Kind_Left_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Right_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_High_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Low_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Length_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Ascending_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Range_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Reverse_Range_Array_Attribute
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Base_Name,
      --  Iir_Kind_Attribute_Name
      Field_Identifier,
      Field_Is_Forward_Ref,
      Field_Expr_Staticness,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Type,
      Field_Attribute_Signature,
      Field_Named_Entity,
      Field_Base_Name
     );

   Fields_Of_Iir_Last : constant array (Iir_Kind) of Fields_Index_Extended :=
     (
      Iir_Kind_Unused => -1,
      Iir_Kind_Error => 6,
      Iir_Kind_Design_File => 17,
      Iir_Kind_Design_Unit => 33,
      Iir_Kind_Library_Clause => 38,
      Iir_Kind_Use_Clause => 42,
      Iir_Kind_Context_Reference => 46,
      Iir_Kind_PSL_Inherit_Spec => 50,
      Iir_Kind_Integer_Literal => 55,
      Iir_Kind_Floating_Point_Literal => 60,
      Iir_Kind_Null_Literal => 62,
      Iir_Kind_String_Literal8 => 73,
      Iir_Kind_Physical_Int_Literal => 79,
      Iir_Kind_Physical_Fp_Literal => 85,
      Iir_Kind_Simple_Aggregate => 90,
      Iir_Kind_Overflow_Literal => 93,
      Iir_Kind_Unaffected_Waveform => 94,
      Iir_Kind_Waveform_Element => 97,
      Iir_Kind_Conditional_Waveform => 101,
      Iir_Kind_Conditional_Expression => 105,
      Iir_Kind_Association_Element_By_Expression => 113,
      Iir_Kind_Association_Element_By_Name => 121,
      Iir_Kind_Association_Element_By_Individual => 130,
      Iir_Kind_Association_Element_Open => 136,
      Iir_Kind_Association_Element_Package => 142,
      Iir_Kind_Association_Element_Type => 150,
      Iir_Kind_Association_Element_Subprogram => 156,
      Iir_Kind_Association_Element_Terminal => 162,
      Iir_Kind_Choice_By_Range => 170,
      Iir_Kind_Choice_By_Expression => 178,
      Iir_Kind_Choice_By_Others => 184,
      Iir_Kind_Choice_By_None => 190,
      Iir_Kind_Choice_By_Name => 197,
      Iir_Kind_Entity_Aspect_Entity => 199,
      Iir_Kind_Entity_Aspect_Configuration => 200,
      Iir_Kind_Entity_Aspect_Open => 200,
      Iir_Kind_Psl_Hierarchical_Name => 202,
      Iir_Kind_Block_Configuration => 208,
      Iir_Kind_Block_Header => 212,
      Iir_Kind_Component_Configuration => 219,
      Iir_Kind_Binding_Indication => 223,
      Iir_Kind_Entity_Class => 225,
      Iir_Kind_Attribute_Value => 233,
      Iir_Kind_Signature => 236,
      Iir_Kind_Aggregate_Info => 243,
      Iir_Kind_Procedure_Call => 247,
      Iir_Kind_Record_Element_Constraint => 255,
      Iir_Kind_Array_Element_Resolution => 257,
      Iir_Kind_Record_Resolution => 258,
      Iir_Kind_Record_Element_Resolution => 261,
      Iir_Kind_Break_Element => 265,
      Iir_Kind_Attribute_Specification => 274,
      Iir_Kind_Disconnection_Specification => 280,
      Iir_Kind_Step_Limit_Specification => 286,
      Iir_Kind_Configuration_Specification => 292,
      Iir_Kind_Access_Type_Definition => 299,
      Iir_Kind_Incomplete_Type_Definition => 306,
      Iir_Kind_Interface_Type_Definition => 312,
      Iir_Kind_File_Type_Definition => 318,
      Iir_Kind_Protected_Type_Declaration => 327,
      Iir_Kind_Record_Type_Definition => 337,
      Iir_Kind_Array_Type_Definition => 348,
      Iir_Kind_Array_Subtype_Definition => 365,
      Iir_Kind_Record_Subtype_Definition => 378,
      Iir_Kind_Access_Subtype_Definition => 386,
      Iir_Kind_Physical_Subtype_Definition => 396,
      Iir_Kind_Floating_Subtype_Definition => 407,
      Iir_Kind_Integer_Subtype_Definition => 417,
      Iir_Kind_Enumeration_Subtype_Definition => 427,
      Iir_Kind_Enumeration_Type_Definition => 438,
      Iir_Kind_Integer_Type_Definition => 446,
      Iir_Kind_Floating_Type_Definition => 454,
      Iir_Kind_Physical_Type_Definition => 465,
      Iir_Kind_Range_Expression => 473,
      Iir_Kind_Protected_Type_Body => 481,
      Iir_Kind_Wildcard_Type_Definition => 485,
      Iir_Kind_Foreign_Vector_Type_Definition => 486,
      Iir_Kind_Subtype_Definition => 493,
      Iir_Kind_Scalar_Nature_Definition => 501,
      Iir_Kind_Record_Nature_Definition => 514,
      Iir_Kind_Array_Nature_Definition => 528,
      Iir_Kind_Array_Subnature_Definition => 543,
      Iir_Kind_Overload_List => 544,
      Iir_Kind_Foreign_Module => 549,
      Iir_Kind_Entity_Declaration => 562,
      Iir_Kind_Configuration_Declaration => 572,
      Iir_Kind_Context_Declaration => 578,
      Iir_Kind_Package_Declaration => 593,
      Iir_Kind_Package_Instantiation_Declaration => 607,
      Iir_Kind_Vmode_Declaration => 619,
      Iir_Kind_Vprop_Declaration => 631,
      Iir_Kind_Vunit_Declaration => 644,
      Iir_Kind_Package_Body => 652,
      Iir_Kind_Architecture_Body => 665,
      Iir_Kind_Type_Declaration => 672,
      Iir_Kind_Anonymous_Type_Declaration => 678,
      Iir_Kind_Subtype_Declaration => 686,
      Iir_Kind_Nature_Declaration => 692,
      Iir_Kind_Subnature_Declaration => 699,
      Iir_Kind_Package_Header => 701,
      Iir_Kind_Unit_Declaration => 710,
      Iir_Kind_Library_Declaration => 718,
      Iir_Kind_Component_Declaration => 728,
      Iir_Kind_Attribute_Declaration => 735,
      Iir_Kind_Group_Template_Declaration => 741,
      Iir_Kind_Group_Declaration => 748,
      Iir_Kind_Element_Declaration => 756,
      Iir_Kind_Nature_Element_Declaration => 763,
      Iir_Kind_Non_Object_Alias_Declaration => 771,
      Iir_Kind_Psl_Declaration => 779,
      Iir_Kind_Psl_Endpoint_Declaration => 793,
      Iir_Kind_Enumeration_Literal => 805,
      Iir_Kind_Function_Declaration => 831,
      Iir_Kind_Procedure_Declaration => 854,
      Iir_Kind_Function_Body => 864,
      Iir_Kind_Procedure_Body => 875,
      Iir_Kind_Function_Instantiation_Declaration => 886,
      Iir_Kind_Procedure_Instantiation_Declaration => 896,
      Iir_Kind_Terminal_Declaration => 905,
      Iir_Kind_Object_Alias_Declaration => 917,
      Iir_Kind_Free_Quantity_Declaration => 929,
      Iir_Kind_Spectrum_Quantity_Declaration => 942,
      Iir_Kind_Noise_Quantity_Declaration => 954,
      Iir_Kind_Across_Quantity_Declaration => 970,
      Iir_Kind_Through_Quantity_Declaration => 986,
      Iir_Kind_File_Declaration => 1001,
      Iir_Kind_Guard_Signal_Declaration => 1015,
      Iir_Kind_Signal_Declaration => 1032,
      Iir_Kind_Variable_Declaration => 1045,
      Iir_Kind_Constant_Declaration => 1059,
      Iir_Kind_Iterator_Declaration => 1071,
      Iir_Kind_Interface_Constant_Declaration => 1088,
      Iir_Kind_Interface_Variable_Declaration => 1104,
      Iir_Kind_Interface_Signal_Declaration => 1125,
      Iir_Kind_Interface_File_Declaration => 1141,
      Iir_Kind_Interface_Quantity_Declaration => 1157,
      Iir_Kind_Interface_Terminal_Declaration => 1169,
      Iir_Kind_Interface_Type_Declaration => 1180,
      Iir_Kind_Interface_Package_Declaration => 1193,
      Iir_Kind_Interface_Function_Declaration => 1211,
      Iir_Kind_Interface_Procedure_Declaration => 1225,
      Iir_Kind_Signal_Attribute_Declaration => 1228,
      Iir_Kind_Suspend_State_Declaration => 1231,
      Iir_Kind_Identity_Operator => 1235,
      Iir_Kind_Negation_Operator => 1239,
      Iir_Kind_Absolute_Operator => 1243,
      Iir_Kind_Not_Operator => 1247,
      Iir_Kind_Implicit_Condition_Operator => 1251,
      Iir_Kind_Condition_Operator => 1255,
      Iir_Kind_Reduction_And_Operator => 1259,
      Iir_Kind_Reduction_Or_Operator => 1263,
      Iir_Kind_Reduction_Nand_Operator => 1267,
      Iir_Kind_Reduction_Nor_Operator => 1271,
      Iir_Kind_Reduction_Xor_Operator => 1275,
      Iir_Kind_Reduction_Xnor_Operator => 1279,
      Iir_Kind_And_Operator => 1284,
      Iir_Kind_Or_Operator => 1289,
      Iir_Kind_Nand_Operator => 1294,
      Iir_Kind_Nor_Operator => 1299,
      Iir_Kind_Xor_Operator => 1304,
      Iir_Kind_Xnor_Operator => 1309,
      Iir_Kind_Equality_Operator => 1314,
      Iir_Kind_Inequality_Operator => 1319,
      Iir_Kind_Less_Than_Operator => 1324,
      Iir_Kind_Less_Than_Or_Equal_Operator => 1329,
      Iir_Kind_Greater_Than_Operator => 1334,
      Iir_Kind_Greater_Than_Or_Equal_Operator => 1339,
      Iir_Kind_Match_Equality_Operator => 1344,
      Iir_Kind_Match_Inequality_Operator => 1349,
      Iir_Kind_Match_Less_Than_Operator => 1354,
      Iir_Kind_Match_Less_Than_Or_Equal_Operator => 1359,
      Iir_Kind_Match_Greater_Than_Operator => 1364,
      Iir_Kind_Match_Greater_Than_Or_Equal_Operator => 1369,
      Iir_Kind_Sll_Operator => 1374,
      Iir_Kind_Sla_Operator => 1379,
      Iir_Kind_Srl_Operator => 1384,
      Iir_Kind_Sra_Operator => 1389,
      Iir_Kind_Rol_Operator => 1394,
      Iir_Kind_Ror_Operator => 1399,
      Iir_Kind_Addition_Operator => 1404,
      Iir_Kind_Substraction_Operator => 1409,
      Iir_Kind_Concatenation_Operator => 1414,
      Iir_Kind_Multiplication_Operator => 1419,
      Iir_Kind_Division_Operator => 1424,
      Iir_Kind_Modulus_Operator => 1429,
      Iir_Kind_Remainder_Operator => 1434,
      Iir_Kind_Exponentiation_Operator => 1439,
      Iir_Kind_Function_Call => 1447,
      Iir_Kind_Aggregate => 1454,
      Iir_Kind_Parenthesis_Expression => 1457,
      Iir_Kind_Qualified_Expression => 1461,
      Iir_Kind_Type_Conversion => 1466,
      Iir_Kind_Allocator_By_Expression => 1471,
      Iir_Kind_Allocator_By_Subtype => 1477,
      Iir_Kind_Selected_Element => 1485,
      Iir_Kind_Dereference => 1490,
      Iir_Kind_Implicit_Dereference => 1495,
      Iir_Kind_Slice_Name => 1502,
      Iir_Kind_Indexed_Name => 1508,
      Iir_Kind_Psl_Prev => 1514,
      Iir_Kind_Psl_Stable => 1519,
      Iir_Kind_Psl_Rose => 1524,
      Iir_Kind_Psl_Fell => 1529,
      Iir_Kind_Psl_Onehot => 1532,
      Iir_Kind_Psl_Onehot0 => 1535,
      Iir_Kind_Psl_Expression => 1537,
      Iir_Kind_Sensitized_Process_Statement => 1558,
      Iir_Kind_Process_Statement => 1578,
      Iir_Kind_Concurrent_Simple_Signal_Assignment => 1591,
      Iir_Kind_Concurrent_Conditional_Signal_Assignment => 1604,
      Iir_Kind_Concurrent_Selected_Signal_Assignment => 1618,
      Iir_Kind_Concurrent_Assertion_Statement => 1626,
      Iir_Kind_Concurrent_Procedure_Call_Statement => 1633,
      Iir_Kind_Concurrent_Break_Statement => 1641,
      Iir_Kind_Psl_Assert_Directive => 1655,
      Iir_Kind_Psl_Assume_Directive => 1667,
      Iir_Kind_Psl_Cover_Directive => 1679,
      Iir_Kind_Psl_Restrict_Directive => 1690,
      Iir_Kind_Block_Statement => 1704,
      Iir_Kind_If_Generate_Statement => 1715,
      Iir_Kind_Case_Generate_Statement => 1724,
      Iir_Kind_For_Generate_Statement => 1733,
      Iir_Kind_Component_Instantiation_Statement => 1744,
      Iir_Kind_Psl_Default_Clock => 1747,
      Iir_Kind_Generate_Statement_Body => 1758,
      Iir_Kind_If_Generate_Else_Clause => 1764,
      Iir_Kind_Simple_Simultaneous_Statement => 1771,
      Iir_Kind_Simultaneous_Null_Statement => 1775,
      Iir_Kind_Simultaneous_Procedural_Statement => 1786,
      Iir_Kind_Simultaneous_Case_Statement => 1795,
      Iir_Kind_Simultaneous_If_Statement => 1804,
      Iir_Kind_Simultaneous_Elsif => 1810,
      Iir_Kind_Simple_Signal_Assignment_Statement => 1821,
      Iir_Kind_Conditional_Signal_Assignment_Statement => 1832,
      Iir_Kind_Selected_Waveform_Assignment_Statement => 1844,
      Iir_Kind_Signal_Force_Assignment_Statement => 1854,
      Iir_Kind_Signal_Release_Assignment_Statement => 1863,
      Iir_Kind_Null_Statement => 1867,
      Iir_Kind_Assertion_Statement => 1874,
      Iir_Kind_Report_Statement => 1880,
      Iir_Kind_Wait_Statement => 1888,
      Iir_Kind_Variable_Assignment_Statement => 1895,
      Iir_Kind_Conditional_Variable_Assignment_Statement => 1902,
      Iir_Kind_Return_Statement => 1908,
      Iir_Kind_For_Loop_Statement => 1919,
      Iir_Kind_While_Loop_Statement => 1930,
      Iir_Kind_Next_Statement => 1937,
      Iir_Kind_Exit_Statement => 1944,
      Iir_Kind_Case_Statement => 1953,
      Iir_Kind_Procedure_Call_Statement => 1959,
      Iir_Kind_Break_Statement => 1966,
      Iir_Kind_If_Statement => 1976,
      Iir_Kind_Suspend_State_Statement => 1980,
      Iir_Kind_Elsif => 1986,
      Iir_Kind_Character_Literal => 1993,
      Iir_Kind_Simple_Name => 2000,
      Iir_Kind_Selected_Name => 2008,
      Iir_Kind_Operator_Symbol => 2013,
      Iir_Kind_Reference_Name => 2018,
      Iir_Kind_External_Constant_Name => 2027,
      Iir_Kind_External_Signal_Name => 2036,
      Iir_Kind_External_Variable_Name => 2046,
      Iir_Kind_Selected_By_All_Name => 2052,
      Iir_Kind_Parenthesis_Name => 2057,
      Iir_Kind_Package_Pathname => 2061,
      Iir_Kind_Absolute_Pathname => 2062,
      Iir_Kind_Relative_Pathname => 2063,
      Iir_Kind_Pathname_Element => 2068,
      Iir_Kind_Base_Attribute => 2070,
      Iir_Kind_Subtype_Attribute => 2075,
      Iir_Kind_Element_Attribute => 2080,
      Iir_Kind_Across_Attribute => 2085,
      Iir_Kind_Through_Attribute => 2090,
      Iir_Kind_Nature_Reference_Attribute => 2094,
      Iir_Kind_Left_Type_Attribute => 2099,
      Iir_Kind_Right_Type_Attribute => 2104,
      Iir_Kind_High_Type_Attribute => 2109,
      Iir_Kind_Low_Type_Attribute => 2114,
      Iir_Kind_Ascending_Type_Attribute => 2119,
      Iir_Kind_Image_Attribute => 2125,
      Iir_Kind_Value_Attribute => 2131,
      Iir_Kind_Pos_Attribute => 2137,
      Iir_Kind_Val_Attribute => 2143,
      Iir_Kind_Succ_Attribute => 2149,
      Iir_Kind_Pred_Attribute => 2155,
      Iir_Kind_Leftof_Attribute => 2161,
      Iir_Kind_Rightof_Attribute => 2167,
      Iir_Kind_Signal_Slew_Attribute => 2175,
      Iir_Kind_Quantity_Slew_Attribute => 2183,
      Iir_Kind_Ramp_Attribute => 2191,
      Iir_Kind_Zoh_Attribute => 2199,
      Iir_Kind_Ltf_Attribute => 2207,
      Iir_Kind_Ztf_Attribute => 2217,
      Iir_Kind_Dot_Attribute => 2224,
      Iir_Kind_Integ_Attribute => 2231,
      Iir_Kind_Above_Attribute => 2239,
      Iir_Kind_Quantity_Delayed_Attribute => 2247,
      Iir_Kind_Delayed_Attribute => 2256,
      Iir_Kind_Stable_Attribute => 2265,
      Iir_Kind_Quiet_Attribute => 2274,
      Iir_Kind_Transaction_Attribute => 2283,
      Iir_Kind_Event_Attribute => 2287,
      Iir_Kind_Active_Attribute => 2291,
      Iir_Kind_Last_Event_Attribute => 2295,
      Iir_Kind_Last_Active_Attribute => 2299,
      Iir_Kind_Last_Value_Attribute => 2303,
      Iir_Kind_Driving_Attribute => 2307,
      Iir_Kind_Driving_Value_Attribute => 2311,
      Iir_Kind_Behavior_Attribute => 2311,
      Iir_Kind_Structure_Attribute => 2311,
      Iir_Kind_Simple_Name_Attribute => 2318,
      Iir_Kind_Instance_Name_Attribute => 2323,
      Iir_Kind_Path_Name_Attribute => 2328,
      Iir_Kind_Left_Array_Attribute => 2335,
      Iir_Kind_Right_Array_Attribute => 2342,
      Iir_Kind_High_Array_Attribute => 2349,
      Iir_Kind_Low_Array_Attribute => 2356,
      Iir_Kind_Length_Array_Attribute => 2363,
      Iir_Kind_Ascending_Array_Attribute => 2370,
      Iir_Kind_Range_Array_Attribute => 2377,
      Iir_Kind_Reverse_Range_Array_Attribute => 2384,
      Iir_Kind_Attribute_Name => 2393
     );

   function Get_Fields_First (K : Iir_Kind) return Fields_Index is
   begin
      if K = Iir_Kind'First then
         return Fields_Of_Iir'First;
      else
         return Fields_Of_Iir_Last (Iir_Kind'Pred (K)) + 1;
      end if;
   end Get_Fields_First;

   function Get_Fields_Last (K : Iir_Kind) return Fields_Index is
   begin
      return Fields_Of_Iir_Last (K);
   end Get_Fields_Last;

   function Get_Field_By_Index (Idx : Fields_Index) return Fields_Enum is
   begin
      return Fields_Of_Iir (Idx);
   end Get_Field_By_Index;

   function Get_Fields (K : Iir_Kind) return Fields_Array
   is
      First : constant Fields_Index := Get_Fields_First (K);
      Last : constant Fields_Index := Fields_Of_Iir_Last (K);
   begin
      return Fields_Of_Iir (First .. Last);
   end Get_Fields;

   function Get_Boolean
      (N : Iir; F : Fields_Enum) return Boolean is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Has_Signed =>
            return Get_Has_Signed (N);
         when Field_Has_Sign =>
            return Get_Has_Sign (N);
         when Field_Has_Length =>
            return Get_Has_Length (N);
         when Field_Static_Attribute_Flag =>
            return Get_Static_Attribute_Flag (N);
         when Field_Whole_Association_Flag =>
            return Get_Whole_Association_Flag (N);
         when Field_Collapse_Signal_Flag =>
            return Get_Collapse_Signal_Flag (N);
         when Field_Artificial_Flag =>
            return Get_Artificial_Flag (N);
         when Field_Open_Flag =>
            return Get_Open_Flag (N);
         when Field_After_Drivers_Flag =>
            return Get_After_Drivers_Flag (N);
         when Field_Same_Alternative_Flag =>
            return Get_Same_Alternative_Flag (N);
         when Field_Element_Type_Flag =>
            return Get_Element_Type_Flag (N);
         when Field_Need_Body =>
            return Get_Need_Body (N);
         when Field_Macro_Expanded_Flag =>
            return Get_Macro_Expanded_Flag (N);
         when Field_Need_Instance_Bodies =>
            return Get_Need_Instance_Bodies (N);
         when Field_Guarded_Signal_Flag =>
            return Get_Guarded_Signal_Flag (N);
         when Field_Deferred_Declaration_Flag =>
            return Get_Deferred_Declaration_Flag (N);
         when Field_Shared_Flag =>
            return Get_Shared_Flag (N);
         when Field_Visible_Flag =>
            return Get_Visible_Flag (N);
         when Field_Text_File_Flag =>
            return Get_Text_File_Flag (N);
         when Field_Only_Characters_Flag =>
            return Get_Only_Characters_Flag (N);
         when Field_Is_Character_Type =>
            return Get_Is_Character_Type (N);
         when Field_Has_Array_Constraint_Flag =>
            return Get_Has_Array_Constraint_Flag (N);
         when Field_Has_Element_Constraint_Flag =>
            return Get_Has_Element_Constraint_Flag (N);
         when Field_Has_Force_Mode =>
            return Get_Has_Force_Mode (N);
         when Field_Postponed_Flag =>
            return Get_Postponed_Flag (N);
         when Field_Passive_Flag =>
            return Get_Passive_Flag (N);
         when Field_Resolution_Function_Flag =>
            return Get_Resolution_Function_Flag (N);
         when Field_Seen_Flag =>
            return Get_Seen_Flag (N);
         when Field_Pure_Flag =>
            return Get_Pure_Flag (N);
         when Field_Foreign_Flag =>
            return Get_Foreign_Flag (N);
         when Field_Resolved_Flag =>
            return Get_Resolved_Flag (N);
         when Field_Signal_Type_Flag =>
            return Get_Signal_Type_Flag (N);
         when Field_Has_Signal_Flag =>
            return Get_Has_Signal_Flag (N);
         when Field_Elab_Flag =>
            return Get_Elab_Flag (N);
         when Field_Vendor_Library_Flag =>
            return Get_Vendor_Library_Flag (N);
         when Field_Configuration_Mark_Flag =>
            return Get_Configuration_Mark_Flag (N);
         when Field_Configuration_Done_Flag =>
            return Get_Configuration_Done_Flag (N);
         when Field_Index_Constraint_Flag =>
            return Get_Index_Constraint_Flag (N);
         when Field_Hide_Implicit_Flag =>
            return Get_Hide_Implicit_Flag (N);
         when Field_Exit_Flag =>
            return Get_Exit_Flag (N);
         when Field_Next_Flag =>
            return Get_Next_Flag (N);
         when Field_In_Formal_Flag =>
            return Get_In_Formal_Flag (N);
         when Field_Aggr_Dynamic_Flag =>
            return Get_Aggr_Dynamic_Flag (N);
         when Field_Aggr_Others_Flag =>
            return Get_Aggr_Others_Flag (N);
         when Field_Aggr_Named_Flag =>
            return Get_Aggr_Named_Flag (N);
         when Field_Aggregate_Expand_Flag =>
            return Get_Aggregate_Expand_Flag (N);
         when Field_Matching_Flag =>
            return Get_Matching_Flag (N);
         when Field_Has_Disconnect_Flag =>
            return Get_Has_Disconnect_Flag (N);
         when Field_Has_Active_Flag =>
            return Get_Has_Active_Flag (N);
         when Field_Is_Within_Flag =>
            return Get_Is_Within_Flag (N);
         when Field_Implicit_Alias_Flag =>
            return Get_Implicit_Alias_Flag (N);
         when Field_Use_Flag =>
            return Get_Use_Flag (N);
         when Field_End_Has_Reserved_Id =>
            return Get_End_Has_Reserved_Id (N);
         when Field_End_Has_Identifier =>
            return Get_End_Has_Identifier (N);
         when Field_End_Has_Postponed =>
            return Get_End_Has_Postponed (N);
         when Field_Has_Label =>
            return Get_Has_Label (N);
         when Field_Has_Begin =>
            return Get_Has_Begin (N);
         when Field_Has_End =>
            return Get_Has_End (N);
         when Field_Has_Is =>
            return Get_Has_Is (N);
         when Field_Has_Pure =>
            return Get_Has_Pure (N);
         when Field_Has_Body =>
            return Get_Has_Body (N);
         when Field_Has_Parameter =>
            return Get_Has_Parameter (N);
         when Field_Has_Component =>
            return Get_Has_Component (N);
         when Field_Has_Identifier_List =>
            return Get_Has_Identifier_List (N);
         when Field_Has_Mode =>
            return Get_Has_Mode (N);
         when Field_Has_Class =>
            return Get_Has_Class (N);
         when Field_Has_Delay_Mechanism =>
            return Get_Has_Delay_Mechanism (N);
         when Field_Suspend_Flag =>
            return Get_Suspend_Flag (N);
         when Field_Is_Ref =>
            return Get_Is_Ref (N);
         when Field_Is_Forward_Ref =>
            return Get_Is_Forward_Ref (N);
         when Field_PSL_EOS_Flag =>
            return Get_PSL_EOS_Flag (N);
         when Field_PSL_Abort_Flag =>
            return Get_PSL_Abort_Flag (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Boolean;

   procedure Set_Boolean
      (N : Iir; F : Fields_Enum; V: Boolean) is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Has_Signed =>
            Set_Has_Signed (N, V);
         when Field_Has_Sign =>
            Set_Has_Sign (N, V);
         when Field_Has_Length =>
            Set_Has_Length (N, V);
         when Field_Static_Attribute_Flag =>
            Set_Static_Attribute_Flag (N, V);
         when Field_Whole_Association_Flag =>
            Set_Whole_Association_Flag (N, V);
         when Field_Collapse_Signal_Flag =>
            Set_Collapse_Signal_Flag (N, V);
         when Field_Artificial_Flag =>
            Set_Artificial_Flag (N, V);
         when Field_Open_Flag =>
            Set_Open_Flag (N, V);
         when Field_After_Drivers_Flag =>
            Set_After_Drivers_Flag (N, V);
         when Field_Same_Alternative_Flag =>
            Set_Same_Alternative_Flag (N, V);
         when Field_Element_Type_Flag =>
            Set_Element_Type_Flag (N, V);
         when Field_Need_Body =>
            Set_Need_Body (N, V);
         when Field_Macro_Expanded_Flag =>
            Set_Macro_Expanded_Flag (N, V);
         when Field_Need_Instance_Bodies =>
            Set_Need_Instance_Bodies (N, V);
         when Field_Guarded_Signal_Flag =>
            Set_Guarded_Signal_Flag (N, V);
         when Field_Deferred_Declaration_Flag =>
            Set_Deferred_Declaration_Flag (N, V);
         when Field_Shared_Flag =>
            Set_Shared_Flag (N, V);
         when Field_Visible_Flag =>
            Set_Visible_Flag (N, V);
         when Field_Text_File_Flag =>
            Set_Text_File_Flag (N, V);
         when Field_Only_Characters_Flag =>
            Set_Only_Characters_Flag (N, V);
         when Field_Is_Character_Type =>
            Set_Is_Character_Type (N, V);
         when Field_Has_Array_Constraint_Flag =>
            Set_Has_Array_Constraint_Flag (N, V);
         when Field_Has_Element_Constraint_Flag =>
            Set_Has_Element_Constraint_Flag (N, V);
         when Field_Has_Force_Mode =>
            Set_Has_Force_Mode (N, V);
         when Field_Postponed_Flag =>
            Set_Postponed_Flag (N, V);
         when Field_Passive_Flag =>
            Set_Passive_Flag (N, V);
         when Field_Resolution_Function_Flag =>
            Set_Resolution_Function_Flag (N, V);
         when Field_Seen_Flag =>
            Set_Seen_Flag (N, V);
         when Field_Pure_Flag =>
            Set_Pure_Flag (N, V);
         when Field_Foreign_Flag =>
            Set_Foreign_Flag (N, V);
         when Field_Resolved_Flag =>
            Set_Resolved_Flag (N, V);
         when Field_Signal_Type_Flag =>
            Set_Signal_Type_Flag (N, V);
         when Field_Has_Signal_Flag =>
            Set_Has_Signal_Flag (N, V);
         when Field_Elab_Flag =>
            Set_Elab_Flag (N, V);
         when Field_Vendor_Library_Flag =>
            Set_Vendor_Library_Flag (N, V);
         when Field_Configuration_Mark_Flag =>
            Set_Configuration_Mark_Flag (N, V);
         when Field_Configuration_Done_Flag =>
            Set_Configuration_Done_Flag (N, V);
         when Field_Index_Constraint_Flag =>
            Set_Index_Constraint_Flag (N, V);
         when Field_Hide_Implicit_Flag =>
            Set_Hide_Implicit_Flag (N, V);
         when Field_Exit_Flag =>
            Set_Exit_Flag (N, V);
         when Field_Next_Flag =>
            Set_Next_Flag (N, V);
         when Field_In_Formal_Flag =>
            Set_In_Formal_Flag (N, V);
         when Field_Aggr_Dynamic_Flag =>
            Set_Aggr_Dynamic_Flag (N, V);
         when Field_Aggr_Others_Flag =>
            Set_Aggr_Others_Flag (N, V);
         when Field_Aggr_Named_Flag =>
            Set_Aggr_Named_Flag (N, V);
         when Field_Aggregate_Expand_Flag =>
            Set_Aggregate_Expand_Flag (N, V);
         when Field_Matching_Flag =>
            Set_Matching_Flag (N, V);
         when Field_Has_Disconnect_Flag =>
            Set_Has_Disconnect_Flag (N, V);
         when Field_Has_Active_Flag =>
            Set_Has_Active_Flag (N, V);
         when Field_Is_Within_Flag =>
            Set_Is_Within_Flag (N, V);
         when Field_Implicit_Alias_Flag =>
            Set_Implicit_Alias_Flag (N, V);
         when Field_Use_Flag =>
            Set_Use_Flag (N, V);
         when Field_End_Has_Reserved_Id =>
            Set_End_Has_Reserved_Id (N, V);
         when Field_End_Has_Identifier =>
            Set_End_Has_Identifier (N, V);
         when Field_End_Has_Postponed =>
            Set_End_Has_Postponed (N, V);
         when Field_Has_Label =>
            Set_Has_Label (N, V);
         when Field_Has_Begin =>
            Set_Has_Begin (N, V);
         when Field_Has_End =>
            Set_Has_End (N, V);
         when Field_Has_Is =>
            Set_Has_Is (N, V);
         when Field_Has_Pure =>
            Set_Has_Pure (N, V);
         when Field_Has_Body =>
            Set_Has_Body (N, V);
         when Field_Has_Parameter =>
            Set_Has_Parameter (N, V);
         when Field_Has_Component =>
            Set_Has_Component (N, V);
         when Field_Has_Identifier_List =>
            Set_Has_Identifier_List (N, V);
         when Field_Has_Mode =>
            Set_Has_Mode (N, V);
         when Field_Has_Class =>
            Set_Has_Class (N, V);
         when Field_Has_Delay_Mechanism =>
            Set_Has_Delay_Mechanism (N, V);
         when Field_Suspend_Flag =>
            Set_Suspend_Flag (N, V);
         when Field_Is_Ref =>
            Set_Is_Ref (N, V);
         when Field_Is_Forward_Ref =>
            Set_Is_Forward_Ref (N, V);
         when Field_PSL_EOS_Flag =>
            Set_PSL_EOS_Flag (N, V);
         when Field_PSL_Abort_Flag =>
            Set_PSL_Abort_Flag (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Boolean;

   function Get_Date_State_Type
      (N : Iir; F : Fields_Enum) return Date_State_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Date_State_Type);
      case F is
         when Field_Date_State =>
            return Get_Date_State (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Date_State_Type;

   procedure Set_Date_State_Type
      (N : Iir; F : Fields_Enum; V: Date_State_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Date_State_Type);
      case F is
         when Field_Date_State =>
            Set_Date_State (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Date_State_Type;

   function Get_Date_Type
      (N : Iir; F : Fields_Enum) return Date_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Date_Type);
      case F is
         when Field_Date =>
            return Get_Date (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Date_Type;

   procedure Set_Date_Type
      (N : Iir; F : Fields_Enum; V: Date_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Date_Type);
      case F is
         when Field_Date =>
            Set_Date (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Date_Type;

   function Get_Direction_Type
      (N : Iir; F : Fields_Enum) return Direction_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Direction_Type);
      case F is
         when Field_Direction =>
            return Get_Direction (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Direction_Type;

   procedure Set_Direction_Type
      (N : Iir; F : Fields_Enum; V: Direction_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Direction_Type);
      case F is
         when Field_Direction =>
            Set_Direction (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Direction_Type;

   function Get_File_Checksum_Id
      (N : Iir; F : Fields_Enum) return File_Checksum_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_File_Checksum_Id);
      case F is
         when Field_File_Checksum =>
            return Get_File_Checksum (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_File_Checksum_Id;

   procedure Set_File_Checksum_Id
      (N : Iir; F : Fields_Enum; V: File_Checksum_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_File_Checksum_Id);
      case F is
         when Field_File_Checksum =>
            Set_File_Checksum (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_File_Checksum_Id;

   function Get_Fp64
      (N : Iir; F : Fields_Enum) return Fp64 is
   begin
      pragma Assert (Fields_Type (F) = Type_Fp64);
      case F is
         when Field_Fp_Value =>
            return Get_Fp_Value (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Fp64;

   procedure Set_Fp64
      (N : Iir; F : Fields_Enum; V: Fp64) is
   begin
      pragma Assert (Fields_Type (F) = Type_Fp64);
      case F is
         when Field_Fp_Value =>
            Set_Fp_Value (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Fp64;

   function Get_Iir
      (N : Iir; F : Fields_Enum) return Iir is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir);
      case F is
         when Field_First_Design_Unit =>
            return Get_First_Design_Unit (N);
         when Field_Last_Design_Unit =>
            return Get_Last_Design_Unit (N);
         when Field_Library_Declaration =>
            return Get_Library_Declaration (N);
         when Field_Library =>
            return Get_Library (N);
         when Field_Design_File =>
            return Get_Design_File (N);
         when Field_Design_File_Chain =>
            return Get_Design_File_Chain (N);
         when Field_Context_Items =>
            return Get_Context_Items (N);
         when Field_Library_Unit =>
            return Get_Library_Unit (N);
         when Field_Hash_Chain =>
            return Get_Hash_Chain (N);
         when Field_Physical_Literal =>
            return Get_Physical_Literal (N);
         when Field_Literal_Origin =>
            return Get_Literal_Origin (N);
         when Field_Range_Origin =>
            return Get_Range_Origin (N);
         when Field_Literal_Subtype =>
            return Get_Literal_Subtype (N);
         when Field_Allocator_Subtype =>
            return Get_Allocator_Subtype (N);
         when Field_Attribute_Designator =>
            return Get_Attribute_Designator (N);
         when Field_Attribute_Specification_Chain =>
            return Get_Attribute_Specification_Chain (N);
         when Field_Attribute_Specification =>
            return Get_Attribute_Specification (N);
         when Field_Designated_Entity =>
            return Get_Designated_Entity (N);
         when Field_Formal =>
            return Get_Formal (N);
         when Field_Actual =>
            return Get_Actual (N);
         when Field_Actual_Conversion =>
            return Get_Actual_Conversion (N);
         when Field_Formal_Conversion =>
            return Get_Formal_Conversion (N);
         when Field_We_Value =>
            return Get_We_Value (N);
         when Field_Time =>
            return Get_Time (N);
         when Field_Associated_Expr =>
            return Get_Associated_Expr (N);
         when Field_Associated_Block =>
            return Get_Associated_Block (N);
         when Field_Associated_Chain =>
            return Get_Associated_Chain (N);
         when Field_Choice_Name =>
            return Get_Choice_Name (N);
         when Field_Choice_Expression =>
            return Get_Choice_Expression (N);
         when Field_Choice_Range =>
            return Get_Choice_Range (N);
         when Field_Architecture =>
            return Get_Architecture (N);
         when Field_Block_Specification =>
            return Get_Block_Specification (N);
         when Field_Prev_Block_Configuration =>
            return Get_Prev_Block_Configuration (N);
         when Field_Configuration_Item_Chain =>
            return Get_Configuration_Item_Chain (N);
         when Field_Attribute_Value_Chain =>
            return Get_Attribute_Value_Chain (N);
         when Field_Spec_Chain =>
            return Get_Spec_Chain (N);
         when Field_Value_Chain =>
            return Get_Value_Chain (N);
         when Field_Attribute_Value_Spec_Chain =>
            return Get_Attribute_Value_Spec_Chain (N);
         when Field_Entity_Name =>
            return Get_Entity_Name (N);
         when Field_Package =>
            return Get_Package (N);
         when Field_Package_Body =>
            return Get_Package_Body (N);
         when Field_Instance_Package_Body =>
            return Get_Instance_Package_Body (N);
         when Field_Hierarchical_Name =>
            return Get_Hierarchical_Name (N);
         when Field_Vunit_Item_Chain =>
            return Get_Vunit_Item_Chain (N);
         when Field_Bound_Vunit_Chain =>
            return Get_Bound_Vunit_Chain (N);
         when Field_Verification_Block_Configuration =>
            return Get_Verification_Block_Configuration (N);
         when Field_Block_Configuration =>
            return Get_Block_Configuration (N);
         when Field_Concurrent_Statement_Chain =>
            return Get_Concurrent_Statement_Chain (N);
         when Field_Chain =>
            return Get_Chain (N);
         when Field_Port_Chain =>
            return Get_Port_Chain (N);
         when Field_Generic_Chain =>
            return Get_Generic_Chain (N);
         when Field_Type =>
            return Get_Type (N);
         when Field_Subtype_Indication =>
            return Get_Subtype_Indication (N);
         when Field_Discrete_Range =>
            return Get_Discrete_Range (N);
         when Field_Type_Definition =>
            return Get_Type_Definition (N);
         when Field_Subtype_Definition =>
            return Get_Subtype_Definition (N);
         when Field_Incomplete_Type_Declaration =>
            return Get_Incomplete_Type_Declaration (N);
         when Field_Interface_Type_Subprograms =>
            return Get_Interface_Type_Subprograms (N);
         when Field_Nature_Definition =>
            return Get_Nature_Definition (N);
         when Field_Nature =>
            return Get_Nature (N);
         when Field_Subnature_Indication =>
            return Get_Subnature_Indication (N);
         when Field_Base_Name =>
            return Get_Base_Name (N);
         when Field_Interface_Declaration_Chain =>
            return Get_Interface_Declaration_Chain (N);
         when Field_Subprogram_Specification =>
            return Get_Subprogram_Specification (N);
         when Field_Sequential_Statement_Chain =>
            return Get_Sequential_Statement_Chain (N);
         when Field_Simultaneous_Statement_Chain =>
            return Get_Simultaneous_Statement_Chain (N);
         when Field_Subprogram_Body =>
            return Get_Subprogram_Body (N);
         when Field_Return_Type =>
            return Get_Return_Type (N);
         when Field_Uninstantiated_Subprogram_Name =>
            return Get_Uninstantiated_Subprogram_Name (N);
         when Field_Default_Value =>
            return Get_Default_Value (N);
         when Field_Deferred_Declaration =>
            return Get_Deferred_Declaration (N);
         when Field_Design_Unit =>
            return Get_Design_Unit (N);
         when Field_Block_Statement =>
            return Get_Block_Statement (N);
         when Field_Signal_Driver =>
            return Get_Signal_Driver (N);
         when Field_Declaration_Chain =>
            return Get_Declaration_Chain (N);
         when Field_File_Logical_Name =>
            return Get_File_Logical_Name (N);
         when Field_File_Open_Kind =>
            return Get_File_Open_Kind (N);
         when Field_Use_Clause_Chain =>
            return Get_Use_Clause_Chain (N);
         when Field_Context_Reference_Chain =>
            return Get_Context_Reference_Chain (N);
         when Field_Inherit_Spec_Chain =>
            return Get_Inherit_Spec_Chain (N);
         when Field_Selected_Name =>
            return Get_Selected_Name (N);
         when Field_Type_Declarator =>
            return Get_Type_Declarator (N);
         when Field_Complete_Type_Definition =>
            return Get_Complete_Type_Definition (N);
         when Field_Incomplete_Type_Ref_Chain =>
            return Get_Incomplete_Type_Ref_Chain (N);
         when Field_Associated_Type =>
            return Get_Associated_Type (N);
         when Field_Entity_Class_Entry_Chain =>
            return Get_Entity_Class_Entry_Chain (N);
         when Field_Unit_Chain =>
            return Get_Unit_Chain (N);
         when Field_Primary_Unit =>
            return Get_Primary_Unit (N);
         when Field_Return_Identifier =>
            return Get_Return_Identifier (N);
         when Field_Range_Constraint =>
            return Get_Range_Constraint (N);
         when Field_Left_Limit =>
            return Get_Left_Limit (N);
         when Field_Right_Limit =>
            return Get_Right_Limit (N);
         when Field_Left_Limit_Expr =>
            return Get_Left_Limit_Expr (N);
         when Field_Right_Limit_Expr =>
            return Get_Right_Limit_Expr (N);
         when Field_Parent_Type =>
            return Get_Parent_Type (N);
         when Field_Simple_Nature =>
            return Get_Simple_Nature (N);
         when Field_Base_Nature =>
            return Get_Base_Nature (N);
         when Field_Resolution_Indication =>
            return Get_Resolution_Indication (N);
         when Field_Record_Element_Resolution_Chain =>
            return Get_Record_Element_Resolution_Chain (N);
         when Field_Tolerance =>
            return Get_Tolerance (N);
         when Field_Plus_Terminal_Name =>
            return Get_Plus_Terminal_Name (N);
         when Field_Minus_Terminal_Name =>
            return Get_Minus_Terminal_Name (N);
         when Field_Plus_Terminal =>
            return Get_Plus_Terminal (N);
         when Field_Minus_Terminal =>
            return Get_Minus_Terminal (N);
         when Field_Magnitude_Expression =>
            return Get_Magnitude_Expression (N);
         when Field_Phase_Expression =>
            return Get_Phase_Expression (N);
         when Field_Power_Expression =>
            return Get_Power_Expression (N);
         when Field_Simultaneous_Left =>
            return Get_Simultaneous_Left (N);
         when Field_Simultaneous_Right =>
            return Get_Simultaneous_Right (N);
         when Field_Element_Subtype_Indication =>
            return Get_Element_Subtype_Indication (N);
         when Field_Element_Subtype =>
            return Get_Element_Subtype (N);
         when Field_Element_Subnature_Indication =>
            return Get_Element_Subnature_Indication (N);
         when Field_Element_Subnature =>
            return Get_Element_Subnature (N);
         when Field_Array_Element_Constraint =>
            return Get_Array_Element_Constraint (N);
         when Field_Owned_Elements_Chain =>
            return Get_Owned_Elements_Chain (N);
         when Field_Designated_Type =>
            return Get_Designated_Type (N);
         when Field_Designated_Subtype_Indication =>
            return Get_Designated_Subtype_Indication (N);
         when Field_Reference =>
            return Get_Reference (N);
         when Field_Nature_Declarator =>
            return Get_Nature_Declarator (N);
         when Field_Across_Type_Mark =>
            return Get_Across_Type_Mark (N);
         when Field_Through_Type_Mark =>
            return Get_Through_Type_Mark (N);
         when Field_Across_Type_Definition =>
            return Get_Across_Type_Definition (N);
         when Field_Through_Type_Definition =>
            return Get_Through_Type_Definition (N);
         when Field_Across_Type =>
            return Get_Across_Type (N);
         when Field_Through_Type =>
            return Get_Through_Type (N);
         when Field_Target =>
            return Get_Target (N);
         when Field_Waveform_Chain =>
            return Get_Waveform_Chain (N);
         when Field_Guard =>
            return Get_Guard (N);
         when Field_Reject_Time_Expression =>
            return Get_Reject_Time_Expression (N);
         when Field_Process_Origin =>
            return Get_Process_Origin (N);
         when Field_Package_Origin =>
            return Get_Package_Origin (N);
         when Field_Condition_Clause =>
            return Get_Condition_Clause (N);
         when Field_Break_Element =>
            return Get_Break_Element (N);
         when Field_Selector_Quantity =>
            return Get_Selector_Quantity (N);
         when Field_Break_Quantity =>
            return Get_Break_Quantity (N);
         when Field_Timeout_Clause =>
            return Get_Timeout_Clause (N);
         when Field_Assertion_Condition =>
            return Get_Assertion_Condition (N);
         when Field_Report_Expression =>
            return Get_Report_Expression (N);
         when Field_Severity_Expression =>
            return Get_Severity_Expression (N);
         when Field_Instantiated_Unit =>
            return Get_Instantiated_Unit (N);
         when Field_Generic_Map_Aspect_Chain =>
            return Get_Generic_Map_Aspect_Chain (N);
         when Field_Port_Map_Aspect_Chain =>
            return Get_Port_Map_Aspect_Chain (N);
         when Field_Configuration_Name =>
            return Get_Configuration_Name (N);
         when Field_Component_Configuration =>
            return Get_Component_Configuration (N);
         when Field_Configuration_Specification =>
            return Get_Configuration_Specification (N);
         when Field_Default_Binding_Indication =>
            return Get_Default_Binding_Indication (N);
         when Field_Default_Configuration_Declaration =>
            return Get_Default_Configuration_Declaration (N);
         when Field_Expression =>
            return Get_Expression (N);
         when Field_Conditional_Expression_Chain =>
            return Get_Conditional_Expression_Chain (N);
         when Field_Allocator_Designated_Type =>
            return Get_Allocator_Designated_Type (N);
         when Field_Selected_Waveform_Chain =>
            return Get_Selected_Waveform_Chain (N);
         when Field_Conditional_Waveform_Chain =>
            return Get_Conditional_Waveform_Chain (N);
         when Field_Guard_Expression =>
            return Get_Guard_Expression (N);
         when Field_Guard_Decl =>
            return Get_Guard_Decl (N);
         when Field_Signal_Attribute_Chain =>
            return Get_Signal_Attribute_Chain (N);
         when Field_Block_Block_Configuration =>
            return Get_Block_Block_Configuration (N);
         when Field_Package_Header =>
            return Get_Package_Header (N);
         when Field_Block_Header =>
            return Get_Block_Header (N);
         when Field_Uninstantiated_Package_Name =>
            return Get_Uninstantiated_Package_Name (N);
         when Field_Uninstantiated_Package_Decl =>
            return Get_Uninstantiated_Package_Decl (N);
         when Field_Generate_Block_Configuration =>
            return Get_Generate_Block_Configuration (N);
         when Field_Generate_Statement_Body =>
            return Get_Generate_Statement_Body (N);
         when Field_Generate_Else_Clause =>
            return Get_Generate_Else_Clause (N);
         when Field_Condition =>
            return Get_Condition (N);
         when Field_Else_Clause =>
            return Get_Else_Clause (N);
         when Field_Parameter_Specification =>
            return Get_Parameter_Specification (N);
         when Field_Parent =>
            return Get_Parent (N);
         when Field_Loop_Label =>
            return Get_Loop_Label (N);
         when Field_Component_Name =>
            return Get_Component_Name (N);
         when Field_Entity_Aspect =>
            return Get_Entity_Aspect (N);
         when Field_Default_Entity_Aspect =>
            return Get_Default_Entity_Aspect (N);
         when Field_Binding_Indication =>
            return Get_Binding_Indication (N);
         when Field_Named_Entity =>
            return Get_Named_Entity (N);
         when Field_Referenced_Name =>
            return Get_Referenced_Name (N);
         when Field_Error_Origin =>
            return Get_Error_Origin (N);
         when Field_Operand =>
            return Get_Operand (N);
         when Field_Left =>
            return Get_Left (N);
         when Field_Right =>
            return Get_Right (N);
         when Field_Unit_Name =>
            return Get_Unit_Name (N);
         when Field_Name =>
            return Get_Name (N);
         when Field_Group_Template_Name =>
            return Get_Group_Template_Name (N);
         when Field_Prefix =>
            return Get_Prefix (N);
         when Field_Signature_Prefix =>
            return Get_Signature_Prefix (N);
         when Field_External_Pathname =>
            return Get_External_Pathname (N);
         when Field_Pathname_Suffix =>
            return Get_Pathname_Suffix (N);
         when Field_Pathname_Expression =>
            return Get_Pathname_Expression (N);
         when Field_Slice_Subtype =>
            return Get_Slice_Subtype (N);
         when Field_Suffix =>
            return Get_Suffix (N);
         when Field_Index_Subtype =>
            return Get_Index_Subtype (N);
         when Field_Parameter =>
            return Get_Parameter (N);
         when Field_Parameter_2 =>
            return Get_Parameter_2 (N);
         when Field_Parameter_3 =>
            return Get_Parameter_3 (N);
         when Field_Parameter_4 =>
            return Get_Parameter_4 (N);
         when Field_Attr_Chain =>
            return Get_Attr_Chain (N);
         when Field_Signal_Attribute_Declaration =>
            return Get_Signal_Attribute_Declaration (N);
         when Field_Actual_Type =>
            return Get_Actual_Type (N);
         when Field_Actual_Type_Definition =>
            return Get_Actual_Type_Definition (N);
         when Field_Association_Chain =>
            return Get_Association_Chain (N);
         when Field_Individual_Association_Chain =>
            return Get_Individual_Association_Chain (N);
         when Field_Subprogram_Association_Chain =>
            return Get_Subprogram_Association_Chain (N);
         when Field_Aggregate_Info =>
            return Get_Aggregate_Info (N);
         when Field_Sub_Aggregate_Info =>
            return Get_Sub_Aggregate_Info (N);
         when Field_Aggr_Low_Limit =>
            return Get_Aggr_Low_Limit (N);
         when Field_Aggr_High_Limit =>
            return Get_Aggr_High_Limit (N);
         when Field_Association_Choices_Chain =>
            return Get_Association_Choices_Chain (N);
         when Field_Case_Statement_Alternative_Chain =>
            return Get_Case_Statement_Alternative_Chain (N);
         when Field_Procedure_Call =>
            return Get_Procedure_Call (N);
         when Field_Implementation =>
            return Get_Implementation (N);
         when Field_Parameter_Association_Chain =>
            return Get_Parameter_Association_Chain (N);
         when Field_Method_Object =>
            return Get_Method_Object (N);
         when Field_Subtype_Type_Mark =>
            return Get_Subtype_Type_Mark (N);
         when Field_Subnature_Nature_Mark =>
            return Get_Subnature_Nature_Mark (N);
         when Field_Type_Conversion_Subtype =>
            return Get_Type_Conversion_Subtype (N);
         when Field_Type_Mark =>
            return Get_Type_Mark (N);
         when Field_File_Type_Mark =>
            return Get_File_Type_Mark (N);
         when Field_Return_Type_Mark =>
            return Get_Return_Type_Mark (N);
         when Field_Alias_Signature =>
            return Get_Alias_Signature (N);
         when Field_Attribute_Signature =>
            return Get_Attribute_Signature (N);
         when Field_Simple_Name_Subtype =>
            return Get_Simple_Name_Subtype (N);
         when Field_Protected_Type_Body =>
            return Get_Protected_Type_Body (N);
         when Field_Protected_Type_Declaration =>
            return Get_Protected_Type_Declaration (N);
         when Field_Count_Expression =>
            return Get_Count_Expression (N);
         when Field_Clock_Expression =>
            return Get_Clock_Expression (N);
         when Field_Default_Clock =>
            return Get_Default_Clock (N);
         when Field_Suspend_State_Chain =>
            return Get_Suspend_State_Chain (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir;

   procedure Set_Iir
      (N : Iir; F : Fields_Enum; V: Iir) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir);
      case F is
         when Field_First_Design_Unit =>
            Set_First_Design_Unit (N, V);
         when Field_Last_Design_Unit =>
            Set_Last_Design_Unit (N, V);
         when Field_Library_Declaration =>
            Set_Library_Declaration (N, V);
         when Field_Library =>
            Set_Library (N, V);
         when Field_Design_File =>
            Set_Design_File (N, V);
         when Field_Design_File_Chain =>
            Set_Design_File_Chain (N, V);
         when Field_Context_Items =>
            Set_Context_Items (N, V);
         when Field_Library_Unit =>
            Set_Library_Unit (N, V);
         when Field_Hash_Chain =>
            Set_Hash_Chain (N, V);
         when Field_Physical_Literal =>
            Set_Physical_Literal (N, V);
         when Field_Literal_Origin =>
            Set_Literal_Origin (N, V);
         when Field_Range_Origin =>
            Set_Range_Origin (N, V);
         when Field_Literal_Subtype =>
            Set_Literal_Subtype (N, V);
         when Field_Allocator_Subtype =>
            Set_Allocator_Subtype (N, V);
         when Field_Attribute_Designator =>
            Set_Attribute_Designator (N, V);
         when Field_Attribute_Specification_Chain =>
            Set_Attribute_Specification_Chain (N, V);
         when Field_Attribute_Specification =>
            Set_Attribute_Specification (N, V);
         when Field_Designated_Entity =>
            Set_Designated_Entity (N, V);
         when Field_Formal =>
            Set_Formal (N, V);
         when Field_Actual =>
            Set_Actual (N, V);
         when Field_Actual_Conversion =>
            Set_Actual_Conversion (N, V);
         when Field_Formal_Conversion =>
            Set_Formal_Conversion (N, V);
         when Field_We_Value =>
            Set_We_Value (N, V);
         when Field_Time =>
            Set_Time (N, V);
         when Field_Associated_Expr =>
            Set_Associated_Expr (N, V);
         when Field_Associated_Block =>
            Set_Associated_Block (N, V);
         when Field_Associated_Chain =>
            Set_Associated_Chain (N, V);
         when Field_Choice_Name =>
            Set_Choice_Name (N, V);
         when Field_Choice_Expression =>
            Set_Choice_Expression (N, V);
         when Field_Choice_Range =>
            Set_Choice_Range (N, V);
         when Field_Architecture =>
            Set_Architecture (N, V);
         when Field_Block_Specification =>
            Set_Block_Specification (N, V);
         when Field_Prev_Block_Configuration =>
            Set_Prev_Block_Configuration (N, V);
         when Field_Configuration_Item_Chain =>
            Set_Configuration_Item_Chain (N, V);
         when Field_Attribute_Value_Chain =>
            Set_Attribute_Value_Chain (N, V);
         when Field_Spec_Chain =>
            Set_Spec_Chain (N, V);
         when Field_Value_Chain =>
            Set_Value_Chain (N, V);
         when Field_Attribute_Value_Spec_Chain =>
            Set_Attribute_Value_Spec_Chain (N, V);
         when Field_Entity_Name =>
            Set_Entity_Name (N, V);
         when Field_Package =>
            Set_Package (N, V);
         when Field_Package_Body =>
            Set_Package_Body (N, V);
         when Field_Instance_Package_Body =>
            Set_Instance_Package_Body (N, V);
         when Field_Hierarchical_Name =>
            Set_Hierarchical_Name (N, V);
         when Field_Vunit_Item_Chain =>
            Set_Vunit_Item_Chain (N, V);
         when Field_Bound_Vunit_Chain =>
            Set_Bound_Vunit_Chain (N, V);
         when Field_Verification_Block_Configuration =>
            Set_Verification_Block_Configuration (N, V);
         when Field_Block_Configuration =>
            Set_Block_Configuration (N, V);
         when Field_Concurrent_Statement_Chain =>
            Set_Concurrent_Statement_Chain (N, V);
         when Field_Chain =>
            Set_Chain (N, V);
         when Field_Port_Chain =>
            Set_Port_Chain (N, V);
         when Field_Generic_Chain =>
            Set_Generic_Chain (N, V);
         when Field_Type =>
            Set_Type (N, V);
         when Field_Subtype_Indication =>
            Set_Subtype_Indication (N, V);
         when Field_Discrete_Range =>
            Set_Discrete_Range (N, V);
         when Field_Type_Definition =>
            Set_Type_Definition (N, V);
         when Field_Subtype_Definition =>
            Set_Subtype_Definition (N, V);
         when Field_Incomplete_Type_Declaration =>
            Set_Incomplete_Type_Declaration (N, V);
         when Field_Interface_Type_Subprograms =>
            Set_Interface_Type_Subprograms (N, V);
         when Field_Nature_Definition =>
            Set_Nature_Definition (N, V);
         when Field_Nature =>
            Set_Nature (N, V);
         when Field_Subnature_Indication =>
            Set_Subnature_Indication (N, V);
         when Field_Base_Name =>
            Set_Base_Name (N, V);
         when Field_Interface_Declaration_Chain =>
            Set_Interface_Declaration_Chain (N, V);
         when Field_Subprogram_Specification =>
            Set_Subprogram_Specification (N, V);
         when Field_Sequential_Statement_Chain =>
            Set_Sequential_Statement_Chain (N, V);
         when Field_Simultaneous_Statement_Chain =>
            Set_Simultaneous_Statement_Chain (N, V);
         when Field_Subprogram_Body =>
            Set_Subprogram_Body (N, V);
         when Field_Return_Type =>
            Set_Return_Type (N, V);
         when Field_Uninstantiated_Subprogram_Name =>
            Set_Uninstantiated_Subprogram_Name (N, V);
         when Field_Default_Value =>
            Set_Default_Value (N, V);
         when Field_Deferred_Declaration =>
            Set_Deferred_Declaration (N, V);
         when Field_Design_Unit =>
            Set_Design_Unit (N, V);
         when Field_Block_Statement =>
            Set_Block_Statement (N, V);
         when Field_Signal_Driver =>
            Set_Signal_Driver (N, V);
         when Field_Declaration_Chain =>
            Set_Declaration_Chain (N, V);
         when Field_File_Logical_Name =>
            Set_File_Logical_Name (N, V);
         when Field_File_Open_Kind =>
            Set_File_Open_Kind (N, V);
         when Field_Use_Clause_Chain =>
            Set_Use_Clause_Chain (N, V);
         when Field_Context_Reference_Chain =>
            Set_Context_Reference_Chain (N, V);
         when Field_Inherit_Spec_Chain =>
            Set_Inherit_Spec_Chain (N, V);
         when Field_Selected_Name =>
            Set_Selected_Name (N, V);
         when Field_Type_Declarator =>
            Set_Type_Declarator (N, V);
         when Field_Complete_Type_Definition =>
            Set_Complete_Type_Definition (N, V);
         when Field_Incomplete_Type_Ref_Chain =>
            Set_Incomplete_Type_Ref_Chain (N, V);
         when Field_Associated_Type =>
            Set_Associated_Type (N, V);
         when Field_Entity_Class_Entry_Chain =>
            Set_Entity_Class_Entry_Chain (N, V);
         when Field_Unit_Chain =>
            Set_Unit_Chain (N, V);
         when Field_Primary_Unit =>
            Set_Primary_Unit (N, V);
         when Field_Return_Identifier =>
            Set_Return_Identifier (N, V);
         when Field_Range_Constraint =>
            Set_Range_Constraint (N, V);
         when Field_Left_Limit =>
            Set_Left_Limit (N, V);
         when Field_Right_Limit =>
            Set_Right_Limit (N, V);
         when Field_Left_Limit_Expr =>
            Set_Left_Limit_Expr (N, V);
         when Field_Right_Limit_Expr =>
            Set_Right_Limit_Expr (N, V);
         when Field_Parent_Type =>
            Set_Parent_Type (N, V);
         when Field_Simple_Nature =>
            Set_Simple_Nature (N, V);
         when Field_Base_Nature =>
            Set_Base_Nature (N, V);
         when Field_Resolution_Indication =>
            Set_Resolution_Indication (N, V);
         when Field_Record_Element_Resolution_Chain =>
            Set_Record_Element_Resolution_Chain (N, V);
         when Field_Tolerance =>
            Set_Tolerance (N, V);
         when Field_Plus_Terminal_Name =>
            Set_Plus_Terminal_Name (N, V);
         when Field_Minus_Terminal_Name =>
            Set_Minus_Terminal_Name (N, V);
         when Field_Plus_Terminal =>
            Set_Plus_Terminal (N, V);
         when Field_Minus_Terminal =>
            Set_Minus_Terminal (N, V);
         when Field_Magnitude_Expression =>
            Set_Magnitude_Expression (N, V);
         when Field_Phase_Expression =>
            Set_Phase_Expression (N, V);
         when Field_Power_Expression =>
            Set_Power_Expression (N, V);
         when Field_Simultaneous_Left =>
            Set_Simultaneous_Left (N, V);
         when Field_Simultaneous_Right =>
            Set_Simultaneous_Right (N, V);
         when Field_Element_Subtype_Indication =>
            Set_Element_Subtype_Indication (N, V);
         when Field_Element_Subtype =>
            Set_Element_Subtype (N, V);
         when Field_Element_Subnature_Indication =>
            Set_Element_Subnature_Indication (N, V);
         when Field_Element_Subnature =>
            Set_Element_Subnature (N, V);
         when Field_Array_Element_Constraint =>
            Set_Array_Element_Constraint (N, V);
         when Field_Owned_Elements_Chain =>
            Set_Owned_Elements_Chain (N, V);
         when Field_Designated_Type =>
            Set_Designated_Type (N, V);
         when Field_Designated_Subtype_Indication =>
            Set_Designated_Subtype_Indication (N, V);
         when Field_Reference =>
            Set_Reference (N, V);
         when Field_Nature_Declarator =>
            Set_Nature_Declarator (N, V);
         when Field_Across_Type_Mark =>
            Set_Across_Type_Mark (N, V);
         when Field_Through_Type_Mark =>
            Set_Through_Type_Mark (N, V);
         when Field_Across_Type_Definition =>
            Set_Across_Type_Definition (N, V);
         when Field_Through_Type_Definition =>
            Set_Through_Type_Definition (N, V);
         when Field_Across_Type =>
            Set_Across_Type (N, V);
         when Field_Through_Type =>
            Set_Through_Type (N, V);
         when Field_Target =>
            Set_Target (N, V);
         when Field_Waveform_Chain =>
            Set_Waveform_Chain (N, V);
         when Field_Guard =>
            Set_Guard (N, V);
         when Field_Reject_Time_Expression =>
            Set_Reject_Time_Expression (N, V);
         when Field_Process_Origin =>
            Set_Process_Origin (N, V);
         when Field_Package_Origin =>
            Set_Package_Origin (N, V);
         when Field_Condition_Clause =>
            Set_Condition_Clause (N, V);
         when Field_Break_Element =>
            Set_Break_Element (N, V);
         when Field_Selector_Quantity =>
            Set_Selector_Quantity (N, V);
         when Field_Break_Quantity =>
            Set_Break_Quantity (N, V);
         when Field_Timeout_Clause =>
            Set_Timeout_Clause (N, V);
         when Field_Assertion_Condition =>
            Set_Assertion_Condition (N, V);
         when Field_Report_Expression =>
            Set_Report_Expression (N, V);
         when Field_Severity_Expression =>
            Set_Severity_Expression (N, V);
         when Field_Instantiated_Unit =>
            Set_Instantiated_Unit (N, V);
         when Field_Generic_Map_Aspect_Chain =>
            Set_Generic_Map_Aspect_Chain (N, V);
         when Field_Port_Map_Aspect_Chain =>
            Set_Port_Map_Aspect_Chain (N, V);
         when Field_Configuration_Name =>
            Set_Configuration_Name (N, V);
         when Field_Component_Configuration =>
            Set_Component_Configuration (N, V);
         when Field_Configuration_Specification =>
            Set_Configuration_Specification (N, V);
         when Field_Default_Binding_Indication =>
            Set_Default_Binding_Indication (N, V);
         when Field_Default_Configuration_Declaration =>
            Set_Default_Configuration_Declaration (N, V);
         when Field_Expression =>
            Set_Expression (N, V);
         when Field_Conditional_Expression_Chain =>
            Set_Conditional_Expression_Chain (N, V);
         when Field_Allocator_Designated_Type =>
            Set_Allocator_Designated_Type (N, V);
         when Field_Selected_Waveform_Chain =>
            Set_Selected_Waveform_Chain (N, V);
         when Field_Conditional_Waveform_Chain =>
            Set_Conditional_Waveform_Chain (N, V);
         when Field_Guard_Expression =>
            Set_Guard_Expression (N, V);
         when Field_Guard_Decl =>
            Set_Guard_Decl (N, V);
         when Field_Signal_Attribute_Chain =>
            Set_Signal_Attribute_Chain (N, V);
         when Field_Block_Block_Configuration =>
            Set_Block_Block_Configuration (N, V);
         when Field_Package_Header =>
            Set_Package_Header (N, V);
         when Field_Block_Header =>
            Set_Block_Header (N, V);
         when Field_Uninstantiated_Package_Name =>
            Set_Uninstantiated_Package_Name (N, V);
         when Field_Uninstantiated_Package_Decl =>
            Set_Uninstantiated_Package_Decl (N, V);
         when Field_Generate_Block_Configuration =>
            Set_Generate_Block_Configuration (N, V);
         when Field_Generate_Statement_Body =>
            Set_Generate_Statement_Body (N, V);
         when Field_Generate_Else_Clause =>
            Set_Generate_Else_Clause (N, V);
         when Field_Condition =>
            Set_Condition (N, V);
         when Field_Else_Clause =>
            Set_Else_Clause (N, V);
         when Field_Parameter_Specification =>
            Set_Parameter_Specification (N, V);
         when Field_Parent =>
            Set_Parent (N, V);
         when Field_Loop_Label =>
            Set_Loop_Label (N, V);
         when Field_Component_Name =>
            Set_Component_Name (N, V);
         when Field_Entity_Aspect =>
            Set_Entity_Aspect (N, V);
         when Field_Default_Entity_Aspect =>
            Set_Default_Entity_Aspect (N, V);
         when Field_Binding_Indication =>
            Set_Binding_Indication (N, V);
         when Field_Named_Entity =>
            Set_Named_Entity (N, V);
         when Field_Referenced_Name =>
            Set_Referenced_Name (N, V);
         when Field_Error_Origin =>
            Set_Error_Origin (N, V);
         when Field_Operand =>
            Set_Operand (N, V);
         when Field_Left =>
            Set_Left (N, V);
         when Field_Right =>
            Set_Right (N, V);
         when Field_Unit_Name =>
            Set_Unit_Name (N, V);
         when Field_Name =>
            Set_Name (N, V);
         when Field_Group_Template_Name =>
            Set_Group_Template_Name (N, V);
         when Field_Prefix =>
            Set_Prefix (N, V);
         when Field_Signature_Prefix =>
            Set_Signature_Prefix (N, V);
         when Field_External_Pathname =>
            Set_External_Pathname (N, V);
         when Field_Pathname_Suffix =>
            Set_Pathname_Suffix (N, V);
         when Field_Pathname_Expression =>
            Set_Pathname_Expression (N, V);
         when Field_Slice_Subtype =>
            Set_Slice_Subtype (N, V);
         when Field_Suffix =>
            Set_Suffix (N, V);
         when Field_Index_Subtype =>
            Set_Index_Subtype (N, V);
         when Field_Parameter =>
            Set_Parameter (N, V);
         when Field_Parameter_2 =>
            Set_Parameter_2 (N, V);
         when Field_Parameter_3 =>
            Set_Parameter_3 (N, V);
         when Field_Parameter_4 =>
            Set_Parameter_4 (N, V);
         when Field_Attr_Chain =>
            Set_Attr_Chain (N, V);
         when Field_Signal_Attribute_Declaration =>
            Set_Signal_Attribute_Declaration (N, V);
         when Field_Actual_Type =>
            Set_Actual_Type (N, V);
         when Field_Actual_Type_Definition =>
            Set_Actual_Type_Definition (N, V);
         when Field_Association_Chain =>
            Set_Association_Chain (N, V);
         when Field_Individual_Association_Chain =>
            Set_Individual_Association_Chain (N, V);
         when Field_Subprogram_Association_Chain =>
            Set_Subprogram_Association_Chain (N, V);
         when Field_Aggregate_Info =>
            Set_Aggregate_Info (N, V);
         when Field_Sub_Aggregate_Info =>
            Set_Sub_Aggregate_Info (N, V);
         when Field_Aggr_Low_Limit =>
            Set_Aggr_Low_Limit (N, V);
         when Field_Aggr_High_Limit =>
            Set_Aggr_High_Limit (N, V);
         when Field_Association_Choices_Chain =>
            Set_Association_Choices_Chain (N, V);
         when Field_Case_Statement_Alternative_Chain =>
            Set_Case_Statement_Alternative_Chain (N, V);
         when Field_Procedure_Call =>
            Set_Procedure_Call (N, V);
         when Field_Implementation =>
            Set_Implementation (N, V);
         when Field_Parameter_Association_Chain =>
            Set_Parameter_Association_Chain (N, V);
         when Field_Method_Object =>
            Set_Method_Object (N, V);
         when Field_Subtype_Type_Mark =>
            Set_Subtype_Type_Mark (N, V);
         when Field_Subnature_Nature_Mark =>
            Set_Subnature_Nature_Mark (N, V);
         when Field_Type_Conversion_Subtype =>
            Set_Type_Conversion_Subtype (N, V);
         when Field_Type_Mark =>
            Set_Type_Mark (N, V);
         when Field_File_Type_Mark =>
            Set_File_Type_Mark (N, V);
         when Field_Return_Type_Mark =>
            Set_Return_Type_Mark (N, V);
         when Field_Alias_Signature =>
            Set_Alias_Signature (N, V);
         when Field_Attribute_Signature =>
            Set_Attribute_Signature (N, V);
         when Field_Simple_Name_Subtype =>
            Set_Simple_Name_Subtype (N, V);
         when Field_Protected_Type_Body =>
            Set_Protected_Type_Body (N, V);
         when Field_Protected_Type_Declaration =>
            Set_Protected_Type_Declaration (N, V);
         when Field_Count_Expression =>
            Set_Count_Expression (N, V);
         when Field_Clock_Expression =>
            Set_Clock_Expression (N, V);
         when Field_Default_Clock =>
            Set_Default_Clock (N, V);
         when Field_Suspend_State_Chain =>
            Set_Suspend_State_Chain (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir;

   function Get_Iir_All_Sensitized
      (N : Iir; F : Fields_Enum) return Iir_All_Sensitized is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_All_Sensitized);
      case F is
         when Field_All_Sensitized_State =>
            return Get_All_Sensitized_State (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_All_Sensitized;

   procedure Set_Iir_All_Sensitized
      (N : Iir; F : Fields_Enum; V: Iir_All_Sensitized) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_All_Sensitized);
      case F is
         when Field_All_Sensitized_State =>
            Set_All_Sensitized_State (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_All_Sensitized;

   function Get_Iir_Constraint
      (N : Iir; F : Fields_Enum) return Iir_Constraint is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Constraint);
      case F is
         when Field_Constraint_State =>
            return Get_Constraint_State (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Constraint;

   procedure Set_Iir_Constraint
      (N : Iir; F : Fields_Enum; V: Iir_Constraint) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Constraint);
      case F is
         when Field_Constraint_State =>
            Set_Constraint_State (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Constraint;

   function Get_Iir_Delay_Mechanism
      (N : Iir; F : Fields_Enum) return Iir_Delay_Mechanism is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Delay_Mechanism);
      case F is
         when Field_Delay_Mechanism =>
            return Get_Delay_Mechanism (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Delay_Mechanism;

   procedure Set_Iir_Delay_Mechanism
      (N : Iir; F : Fields_Enum; V: Iir_Delay_Mechanism) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Delay_Mechanism);
      case F is
         when Field_Delay_Mechanism =>
            Set_Delay_Mechanism (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Delay_Mechanism;

   function Get_Iir_Flist
      (N : Iir; F : Fields_Enum) return Iir_Flist is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Flist);
      case F is
         when Field_Simple_Aggregate_List =>
            return Get_Simple_Aggregate_List (N);
         when Field_Entity_Name_List =>
            return Get_Entity_Name_List (N);
         when Field_Signal_List =>
            return Get_Signal_List (N);
         when Field_Quantity_List =>
            return Get_Quantity_List (N);
         when Field_Enumeration_Literal_List =>
            return Get_Enumeration_Literal_List (N);
         when Field_Group_Constituent_List =>
            return Get_Group_Constituent_List (N);
         when Field_Index_Subtype_List =>
            return Get_Index_Subtype_List (N);
         when Field_Index_Subtype_Definition_List =>
            return Get_Index_Subtype_Definition_List (N);
         when Field_Index_Constraint_List =>
            return Get_Index_Constraint_List (N);
         when Field_Elements_Declaration_List =>
            return Get_Elements_Declaration_List (N);
         when Field_Index_List =>
            return Get_Index_List (N);
         when Field_Instantiation_List =>
            return Get_Instantiation_List (N);
         when Field_Type_Marks_List =>
            return Get_Type_Marks_List (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Flist;

   procedure Set_Iir_Flist
      (N : Iir; F : Fields_Enum; V: Iir_Flist) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Flist);
      case F is
         when Field_Simple_Aggregate_List =>
            Set_Simple_Aggregate_List (N, V);
         when Field_Entity_Name_List =>
            Set_Entity_Name_List (N, V);
         when Field_Signal_List =>
            Set_Signal_List (N, V);
         when Field_Quantity_List =>
            Set_Quantity_List (N, V);
         when Field_Enumeration_Literal_List =>
            Set_Enumeration_Literal_List (N, V);
         when Field_Group_Constituent_List =>
            Set_Group_Constituent_List (N, V);
         when Field_Index_Subtype_List =>
            Set_Index_Subtype_List (N, V);
         when Field_Index_Subtype_Definition_List =>
            Set_Index_Subtype_Definition_List (N, V);
         when Field_Index_Constraint_List =>
            Set_Index_Constraint_List (N, V);
         when Field_Elements_Declaration_List =>
            Set_Elements_Declaration_List (N, V);
         when Field_Index_List =>
            Set_Index_List (N, V);
         when Field_Instantiation_List =>
            Set_Instantiation_List (N, V);
         when Field_Type_Marks_List =>
            Set_Type_Marks_List (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Flist;

   function Get_Iir_Force_Mode
      (N : Iir; F : Fields_Enum) return Iir_Force_Mode is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Force_Mode);
      case F is
         when Field_Force_Mode =>
            return Get_Force_Mode (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Force_Mode;

   procedure Set_Iir_Force_Mode
      (N : Iir; F : Fields_Enum; V: Iir_Force_Mode) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Force_Mode);
      case F is
         when Field_Force_Mode =>
            Set_Force_Mode (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Force_Mode;

   function Get_Iir_Index32
      (N : Iir; F : Fields_Enum) return Iir_Index32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Index32);
      case F is
         when Field_Element_Position =>
            return Get_Element_Position (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Index32;

   procedure Set_Iir_Index32
      (N : Iir; F : Fields_Enum; V: Iir_Index32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Index32);
      case F is
         when Field_Element_Position =>
            Set_Element_Position (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Index32;

   function Get_Iir_Int32
      (N : Iir; F : Fields_Enum) return Iir_Int32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Int32);
      case F is
         when Field_Enum_Pos =>
            return Get_Enum_Pos (N);
         when Field_Overload_Number =>
            return Get_Overload_Number (N);
         when Field_Subprogram_Depth =>
            return Get_Subprogram_Depth (N);
         when Field_Subprogram_Hash =>
            return Get_Subprogram_Hash (N);
         when Field_Impure_Depth =>
            return Get_Impure_Depth (N);
         when Field_Aggr_Min_Length =>
            return Get_Aggr_Min_Length (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Int32;

   procedure Set_Iir_Int32
      (N : Iir; F : Fields_Enum; V: Iir_Int32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Int32);
      case F is
         when Field_Enum_Pos =>
            Set_Enum_Pos (N, V);
         when Field_Overload_Number =>
            Set_Overload_Number (N, V);
         when Field_Subprogram_Depth =>
            Set_Subprogram_Depth (N, V);
         when Field_Subprogram_Hash =>
            Set_Subprogram_Hash (N, V);
         when Field_Impure_Depth =>
            Set_Impure_Depth (N, V);
         when Field_Aggr_Min_Length =>
            Set_Aggr_Min_Length (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Int32;

   function Get_Iir_List
      (N : Iir; F : Fields_Enum) return Iir_List is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_List);
      case F is
         when Field_File_Dependence_List =>
            return Get_File_Dependence_List (N);
         when Field_Dependence_List =>
            return Get_Dependence_List (N);
         when Field_Analysis_Checks_List =>
            return Get_Analysis_Checks_List (N);
         when Field_Sensitivity_List =>
            return Get_Sensitivity_List (N);
         when Field_Callees_List =>
            return Get_Callees_List (N);
         when Field_Guard_Sensitivity_List =>
            return Get_Guard_Sensitivity_List (N);
         when Field_Overload_List =>
            return Get_Overload_List (N);
         when Field_PSL_Clock_Sensitivity =>
            return Get_PSL_Clock_Sensitivity (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_List;

   procedure Set_Iir_List
      (N : Iir; F : Fields_Enum; V: Iir_List) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_List);
      case F is
         when Field_File_Dependence_List =>
            Set_File_Dependence_List (N, V);
         when Field_Dependence_List =>
            Set_Dependence_List (N, V);
         when Field_Analysis_Checks_List =>
            Set_Analysis_Checks_List (N, V);
         when Field_Sensitivity_List =>
            Set_Sensitivity_List (N, V);
         when Field_Callees_List =>
            Set_Callees_List (N, V);
         when Field_Guard_Sensitivity_List =>
            Set_Guard_Sensitivity_List (N, V);
         when Field_Overload_List =>
            Set_Overload_List (N, V);
         when Field_PSL_Clock_Sensitivity =>
            Set_PSL_Clock_Sensitivity (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_List;

   function Get_Iir_Mode
      (N : Iir; F : Fields_Enum) return Iir_Mode is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Mode);
      case F is
         when Field_Mode =>
            return Get_Mode (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Mode;

   procedure Set_Iir_Mode
      (N : Iir; F : Fields_Enum; V: Iir_Mode) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Mode);
      case F is
         when Field_Mode =>
            Set_Mode (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Mode;

   function Get_Iir_Predefined_Functions
      (N : Iir; F : Fields_Enum) return Iir_Predefined_Functions is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Predefined_Functions);
      case F is
         when Field_Implicit_Definition =>
            return Get_Implicit_Definition (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Predefined_Functions;

   procedure Set_Iir_Predefined_Functions
      (N : Iir; F : Fields_Enum; V: Iir_Predefined_Functions) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Predefined_Functions);
      case F is
         when Field_Implicit_Definition =>
            Set_Implicit_Definition (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Predefined_Functions;

   function Get_Iir_Pure_State
      (N : Iir; F : Fields_Enum) return Iir_Pure_State is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Pure_State);
      case F is
         when Field_Purity_State =>
            return Get_Purity_State (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Pure_State;

   procedure Set_Iir_Pure_State
      (N : Iir; F : Fields_Enum; V: Iir_Pure_State) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Pure_State);
      case F is
         when Field_Purity_State =>
            Set_Purity_State (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Pure_State;

   function Get_Iir_Signal_Kind
      (N : Iir; F : Fields_Enum) return Iir_Signal_Kind is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Signal_Kind);
      case F is
         when Field_Signal_Kind =>
            return Get_Signal_Kind (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Signal_Kind;

   procedure Set_Iir_Signal_Kind
      (N : Iir; F : Fields_Enum; V: Iir_Signal_Kind) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Signal_Kind);
      case F is
         when Field_Signal_Kind =>
            Set_Signal_Kind (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Signal_Kind;

   function Get_Iir_Staticness
      (N : Iir; F : Fields_Enum) return Iir_Staticness is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Staticness);
      case F is
         when Field_Nature_Staticness =>
            return Get_Nature_Staticness (N);
         when Field_Type_Staticness =>
            return Get_Type_Staticness (N);
         when Field_Expr_Staticness =>
            return Get_Expr_Staticness (N);
         when Field_Name_Staticness =>
            return Get_Name_Staticness (N);
         when Field_Choice_Staticness =>
            return Get_Choice_Staticness (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Iir_Staticness;

   procedure Set_Iir_Staticness
      (N : Iir; F : Fields_Enum; V: Iir_Staticness) is
   begin
      pragma Assert (Fields_Type (F) = Type_Iir_Staticness);
      case F is
         when Field_Nature_Staticness =>
            Set_Nature_Staticness (N, V);
         when Field_Type_Staticness =>
            Set_Type_Staticness (N, V);
         when Field_Expr_Staticness =>
            Set_Expr_Staticness (N, V);
         when Field_Name_Staticness =>
            Set_Name_Staticness (N, V);
         when Field_Choice_Staticness =>
            Set_Choice_Staticness (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Iir_Staticness;

   function Get_Int32
      (N : Iir; F : Fields_Enum) return Int32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_Design_Unit_Source_Line =>
            return Get_Design_Unit_Source_Line (N);
         when Field_Design_Unit_Source_Col =>
            return Get_Design_Unit_Source_Col (N);
         when Field_String_Length =>
            return Get_String_Length (N);
         when Field_Literal_Length =>
            return Get_Literal_Length (N);
         when Field_PSL_Nbr_States =>
            return Get_PSL_Nbr_States (N);
         when Field_Foreign_Node =>
            return Get_Foreign_Node (N);
         when Field_Suspend_State_Index =>
            return Get_Suspend_State_Index (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Int32;

   procedure Set_Int32
      (N : Iir; F : Fields_Enum; V: Int32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_Design_Unit_Source_Line =>
            Set_Design_Unit_Source_Line (N, V);
         when Field_Design_Unit_Source_Col =>
            Set_Design_Unit_Source_Col (N, V);
         when Field_String_Length =>
            Set_String_Length (N, V);
         when Field_Literal_Length =>
            Set_Literal_Length (N, V);
         when Field_PSL_Nbr_States =>
            Set_PSL_Nbr_States (N, V);
         when Field_Foreign_Node =>
            Set_Foreign_Node (N, V);
         when Field_Suspend_State_Index =>
            Set_Suspend_State_Index (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Int32;

   function Get_Int64
      (N : Iir; F : Fields_Enum) return Int64 is
   begin
      pragma Assert (Fields_Type (F) = Type_Int64);
      case F is
         when Field_Value =>
            return Get_Value (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Int64;

   procedure Set_Int64
      (N : Iir; F : Fields_Enum; V: Int64) is
   begin
      pragma Assert (Fields_Type (F) = Type_Int64);
      case F is
         when Field_Value =>
            Set_Value (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Int64;

   function Get_Name_Id
      (N : Iir; F : Fields_Enum) return Name_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Design_File_Filename =>
            return Get_Design_File_Filename (N);
         when Field_Design_File_Directory =>
            return Get_Design_File_Directory (N);
         when Field_Library_Directory =>
            return Get_Library_Directory (N);
         when Field_Identifier =>
            return Get_Identifier (N);
         when Field_Label =>
            return Get_Label (N);
         when Field_Alternative_Label =>
            return Get_Alternative_Label (N);
         when Field_Simple_Name_Identifier =>
            return Get_Simple_Name_Identifier (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Name_Id;

   procedure Set_Name_Id
      (N : Iir; F : Fields_Enum; V: Name_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Design_File_Filename =>
            Set_Design_File_Filename (N, V);
         when Field_Design_File_Directory =>
            Set_Design_File_Directory (N, V);
         when Field_Library_Directory =>
            Set_Library_Directory (N, V);
         when Field_Identifier =>
            Set_Identifier (N, V);
         when Field_Label =>
            Set_Label (N, V);
         when Field_Alternative_Label =>
            Set_Alternative_Label (N, V);
         when Field_Simple_Name_Identifier =>
            Set_Simple_Name_Identifier (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Name_Id;

   function Get_Number_Base_Type
      (N : Iir; F : Fields_Enum) return Number_Base_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Number_Base_Type);
      case F is
         when Field_Bit_String_Base =>
            return Get_Bit_String_Base (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Number_Base_Type;

   procedure Set_Number_Base_Type
      (N : Iir; F : Fields_Enum; V: Number_Base_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Number_Base_Type);
      case F is
         when Field_Bit_String_Base =>
            Set_Bit_String_Base (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Number_Base_Type;

   function Get_PSL_NFA
      (N : Iir; F : Fields_Enum) return PSL_NFA is
   begin
      pragma Assert (Fields_Type (F) = Type_PSL_NFA);
      case F is
         when Field_PSL_NFA =>
            return Get_PSL_NFA (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_PSL_NFA;

   procedure Set_PSL_NFA
      (N : Iir; F : Fields_Enum; V: PSL_NFA) is
   begin
      pragma Assert (Fields_Type (F) = Type_PSL_NFA);
      case F is
         when Field_PSL_NFA =>
            Set_PSL_NFA (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_PSL_NFA;

   function Get_PSL_Node
      (N : Iir; F : Fields_Enum) return PSL_Node is
   begin
      pragma Assert (Fields_Type (F) = Type_PSL_Node);
      case F is
         when Field_Psl_Property =>
            return Get_Psl_Property (N);
         when Field_Psl_Sequence =>
            return Get_Psl_Sequence (N);
         when Field_Psl_Declaration =>
            return Get_Psl_Declaration (N);
         when Field_Psl_Expression =>
            return Get_Psl_Expression (N);
         when Field_Psl_Boolean =>
            return Get_Psl_Boolean (N);
         when Field_PSL_Clock =>
            return Get_PSL_Clock (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_PSL_Node;

   procedure Set_PSL_Node
      (N : Iir; F : Fields_Enum; V: PSL_Node) is
   begin
      pragma Assert (Fields_Type (F) = Type_PSL_Node);
      case F is
         when Field_Psl_Property =>
            Set_Psl_Property (N, V);
         when Field_Psl_Sequence =>
            Set_Psl_Sequence (N, V);
         when Field_Psl_Declaration =>
            Set_Psl_Declaration (N, V);
         when Field_Psl_Expression =>
            Set_Psl_Expression (N, V);
         when Field_Psl_Boolean =>
            Set_Psl_Boolean (N, V);
         when Field_PSL_Clock =>
            Set_PSL_Clock (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_PSL_Node;

   function Get_Scalar_Size
      (N : Iir; F : Fields_Enum) return Scalar_Size is
   begin
      pragma Assert (Fields_Type (F) = Type_Scalar_Size);
      case F is
         when Field_Scalar_Size =>
            return Get_Scalar_Size (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Scalar_Size;

   procedure Set_Scalar_Size
      (N : Iir; F : Fields_Enum; V: Scalar_Size) is
   begin
      pragma Assert (Fields_Type (F) = Type_Scalar_Size);
      case F is
         when Field_Scalar_Size =>
            Set_Scalar_Size (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Scalar_Size;

   function Get_Source_File_Entry
      (N : Iir; F : Fields_Enum) return Source_File_Entry is
   begin
      pragma Assert (Fields_Type (F) = Type_Source_File_Entry);
      case F is
         when Field_Design_File_Source =>
            return Get_Design_File_Source (N);
         when Field_Instance_Source_File =>
            return Get_Instance_Source_File (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Source_File_Entry;

   procedure Set_Source_File_Entry
      (N : Iir; F : Fields_Enum; V: Source_File_Entry) is
   begin
      pragma Assert (Fields_Type (F) = Type_Source_File_Entry);
      case F is
         when Field_Design_File_Source =>
            Set_Design_File_Source (N, V);
         when Field_Instance_Source_File =>
            Set_Instance_Source_File (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Source_File_Entry;

   function Get_Source_Ptr
      (N : Iir; F : Fields_Enum) return Source_Ptr is
   begin
      pragma Assert (Fields_Type (F) = Type_Source_Ptr);
      case F is
         when Field_Design_Unit_Source_Pos =>
            return Get_Design_Unit_Source_Pos (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Source_Ptr;

   procedure Set_Source_Ptr
      (N : Iir; F : Fields_Enum; V: Source_Ptr) is
   begin
      pragma Assert (Fields_Type (F) = Type_Source_Ptr);
      case F is
         when Field_Design_Unit_Source_Pos =>
            Set_Design_Unit_Source_Pos (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Source_Ptr;

   function Get_String8_Id
      (N : Iir; F : Fields_Enum) return String8_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_String8_Id);
      case F is
         when Field_String8_Id =>
            return Get_String8_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_String8_Id;

   procedure Set_String8_Id
      (N : Iir; F : Fields_Enum; V: String8_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_String8_Id);
      case F is
         when Field_String8_Id =>
            Set_String8_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_String8_Id;

   function Get_Time_Stamp_Id
      (N : Iir; F : Fields_Enum) return Time_Stamp_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Time_Stamp_Id);
      case F is
         when Field_Analysis_Time_Stamp =>
            return Get_Analysis_Time_Stamp (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Time_Stamp_Id;

   procedure Set_Time_Stamp_Id
      (N : Iir; F : Fields_Enum; V: Time_Stamp_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Time_Stamp_Id);
      case F is
         when Field_Analysis_Time_Stamp =>
            Set_Analysis_Time_Stamp (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Time_Stamp_Id;

   function Get_Token_Type
      (N : Iir; F : Fields_Enum) return Token_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Token_Type);
      case F is
         when Field_Entity_Class =>
            return Get_Entity_Class (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Token_Type;

   procedure Set_Token_Type
      (N : Iir; F : Fields_Enum; V: Token_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Token_Type);
      case F is
         when Field_Entity_Class =>
            Set_Entity_Class (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Token_Type;

   function Get_Tri_State_Type
      (N : Iir; F : Fields_Enum) return Tri_State_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Tri_State_Type);
      case F is
         when Field_Guarded_Target_State =>
            return Get_Guarded_Target_State (N);
         when Field_Wait_State =>
            return Get_Wait_State (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Tri_State_Type;

   procedure Set_Tri_State_Type
      (N : Iir; F : Fields_Enum; V: Tri_State_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Tri_State_Type);
      case F is
         when Field_Guarded_Target_State =>
            Set_Guarded_Target_State (N, V);
         when Field_Wait_State =>
            Set_Wait_State (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Tri_State_Type;

   function Has_First_Design_Unit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_First_Design_Unit;

   function Has_Last_Design_Unit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_Last_Design_Unit;

   function Has_Library_Declaration (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Library_Clause;
   end Has_Library_Declaration;

   function Has_File_Checksum (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_File_Checksum;

   function Has_Analysis_Time_Stamp (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_Analysis_Time_Stamp;

   function Has_Design_File_Source (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_Design_File_Source;

   function Has_Library (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_Library;

   function Has_File_Dependence_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_File_Dependence_List;

   function Has_Design_File_Filename (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_Design_File_Filename;

   function Has_Design_File_Directory (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_File;
   end Has_Design_File_Directory;

   function Has_Design_File (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Design_File;

   function Has_Design_File_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Library_Declaration;
   end Has_Design_File_Chain;

   function Has_Library_Directory (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Library_Declaration;
   end Has_Library_Directory;

   function Has_Date (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Library_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Date;

   function Has_Context_Items (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Context_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Context_Items;

   function Has_Dependence_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Dependence_List;

   function Has_Analysis_Checks_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Analysis_Checks_List;

   function Has_Date_State (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Date_State;

   function Has_Guarded_Target_State (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Guarded_Target_State;

   function Has_Library_Unit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Library_Unit;

   function Has_Hash_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Hash_Chain;

   function Has_Design_Unit_Source_Pos (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Design_Unit_Source_Pos;

   function Has_Design_Unit_Source_Line (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Design_Unit_Source_Line;

   function Has_Design_Unit_Source_Col (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Design_Unit_Source_Col;

   function Has_Value (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Integer_Literal
           | Iir_Kind_Physical_Int_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Value;

   function Has_Enum_Pos (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Enumeration_Literal;
   end Has_Enum_Pos;

   function Has_Physical_Literal (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Unit_Declaration;
   end Has_Physical_Literal;

   function Has_Fp_Value (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Fp_Value;

   function Has_Simple_Aggregate_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Simple_Aggregate;
   end Has_Simple_Aggregate_List;

   function Has_String8_Id (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_String_Literal8;
   end Has_String8_Id;

   function Has_String_Length (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_String_Literal8;
   end Has_String_Length;

   function Has_Bit_String_Base (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_String_Literal8;
   end Has_Bit_String_Base;

   function Has_Has_Signed (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_String_Literal8;
   end Has_Has_Signed;

   function Has_Has_Sign (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_String_Literal8;
   end Has_Has_Sign;

   function Has_Has_Length (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_String_Literal8;
   end Has_Has_Length;

   function Has_Literal_Length (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Literal_Length;

   function Has_Literal_Origin (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Overflow_Literal
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Aggregate =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Literal_Origin;

   function Has_Range_Origin (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Range_Expression;
   end Has_Range_Origin;

   function Has_Literal_Subtype (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_String_Literal8
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Aggregate =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Literal_Subtype;

   function Has_Allocator_Subtype (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Allocator_By_Subtype;
   end Has_Allocator_Subtype;

   function Has_Entity_Class (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Class
           | Iir_Kind_Attribute_Specification =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Entity_Class;

   function Has_Entity_Name_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Specification;
   end Has_Entity_Name_List;

   function Has_Attribute_Designator (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Specification;
   end Has_Attribute_Designator;

   function Has_Attribute_Specification_Chain (K : Iir_Kind)
      return Boolean is
   begin
      return K = Iir_Kind_Attribute_Specification;
   end Has_Attribute_Specification_Chain;

   function Has_Attribute_Specification (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Value;
   end Has_Attribute_Specification;

   function Has_Static_Attribute_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Specification;
   end Has_Static_Attribute_Flag;

   function Has_Signal_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Disconnection_Specification;
   end Has_Signal_List;

   function Has_Quantity_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Step_Limit_Specification;
   end Has_Quantity_List;

   function Has_Designated_Entity (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Value;
   end Has_Designated_Entity;

   function Has_Formal (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Formal;

   function Has_Actual (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Actual;

   function Has_Actual_Conversion (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Actual_Conversion;

   function Has_Formal_Conversion (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Formal_Conversion;

   function Has_Whole_Association_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Whole_Association_Flag;

   function Has_Collapse_Signal_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Collapse_Signal_Flag;

   function Has_Artificial_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Association_Element_Open;
   end Has_Artificial_Flag;

   function Has_Open_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Open_Flag;

   function Has_After_Drivers_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_After_Drivers_Flag;

   function Has_We_Value (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Waveform_Element;
   end Has_We_Value;

   function Has_Time (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Waveform_Element;
   end Has_Time;

   function Has_Associated_Expr (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Associated_Expr;

   function Has_Associated_Block (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Associated_Block;

   function Has_Associated_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Associated_Chain;

   function Has_Choice_Name (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Choice_By_Name;
   end Has_Choice_Name;

   function Has_Choice_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Choice_By_Expression;
   end Has_Choice_Expression;

   function Has_Choice_Range (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Choice_By_Range;
   end Has_Choice_Range;

   function Has_Same_Alternative_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Same_Alternative_Flag;

   function Has_Element_Type_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Element_Type_Flag;

   function Has_Architecture (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Psl_Hierarchical_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Architecture;

   function Has_Block_Specification (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Block_Configuration;
   end Has_Block_Specification;

   function Has_Prev_Block_Configuration (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Block_Configuration;
   end Has_Prev_Block_Configuration;

   function Has_Configuration_Item_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Block_Configuration;
   end Has_Configuration_Item_Chain;

   function Has_Attribute_Value_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Simultaneous_Procedural_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Attribute_Value_Chain;

   function Has_Spec_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Value;
   end Has_Spec_Chain;

   function Has_Value_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Value;
   end Has_Value_Chain;

   function Has_Attribute_Value_Spec_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Specification;
   end Has_Attribute_Value_Spec_Chain;

   function Has_Entity_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Psl_Hierarchical_Name
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Architecture_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Entity_Name;

   function Has_Package (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Body;
   end Has_Package;

   function Has_Package_Body (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Declaration;
   end Has_Package_Body;

   function Has_Instance_Package_Body (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Instantiation_Declaration;
   end Has_Instance_Package_Body;

   function Has_Need_Body (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Declaration;
   end Has_Need_Body;

   function Has_Macro_Expanded_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Declaration;
   end Has_Macro_Expanded_Flag;

   function Has_Need_Instance_Bodies (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Declaration;
   end Has_Need_Instance_Bodies;

   function Has_Hierarchical_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Hierarchical_Name;

   function Has_Vunit_Item_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Vunit_Item_Chain;

   function Has_Bound_Vunit_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Architecture_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Bound_Vunit_Chain;

   function Has_Verification_Block_Configuration (K : Iir_Kind)
      return Boolean is
   begin
      case K is
         when Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Verification_Block_Configuration;

   function Has_Block_Configuration (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Block_Configuration;

   function Has_Concurrent_Statement_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Concurrent_Statement_Chain;

   function Has_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Library_Clause
           | Iir_Kind_Use_Clause
           | Iir_Kind_Context_Reference
           | Iir_Kind_PSL_Inherit_Spec
           | Iir_Kind_Unaffected_Waveform
           | Iir_Kind_Waveform_Element
           | Iir_Kind_Conditional_Waveform
           | Iir_Kind_Conditional_Expression
           | Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Entity_Class
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Record_Element_Resolution
           | Iir_Kind_Break_Element
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Step_Limit_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Signal_Attribute_Declaration
           | Iir_Kind_Suspend_State_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Simultaneous_Null_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Suspend_State_Statement
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Chain;

   function Has_Port_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Foreign_Module
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Component_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Port_Chain;

   function Has_Generic_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Foreign_Module
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Package_Header
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generic_Chain;

   function Has_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Null_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Overflow_Literal
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Range_Expression
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Implicit_Condition_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator
           | Iir_Kind_And_Operator
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
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call
           | Iir_Kind_Aggregate
           | Iir_Kind_Parenthesis_Expression
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell
           | Iir_Kind_Psl_Onehot
           | Iir_Kind_Psl_Onehot0
           | Iir_Kind_Psl_Expression
           | Iir_Kind_Return_Statement
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Reference_Name
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type;

   function Has_Subtype_Indication (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subtype_Indication;

   function Has_Discrete_Range (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Iterator_Declaration;
   end Has_Discrete_Range;

   function Has_Type_Definition (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Definition;

   function Has_Subtype_Definition (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Anonymous_Type_Declaration;
   end Has_Subtype_Definition;

   function Has_Incomplete_Type_Declaration (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Incomplete_Type_Declaration;

   function Has_Interface_Type_Subprograms (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Interface_Type_Declaration;
   end Has_Interface_Type_Subprograms;

   function Has_Nature_Definition (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Nature_Declaration;
   end Has_Nature_Definition;

   function Has_Nature (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Nature_Reference_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Nature;

   function Has_Subnature_Indication (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Subnature_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Interface_Terminal_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subnature_Indication;

   function Has_Mode (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_File_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Mode;

   function Has_Guarded_Signal_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Guarded_Signal_Flag;

   function Has_Signal_Kind (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Signal_Kind;

   function Has_Base_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Attribute_Value
           | Iir_Kind_Function_Call
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute
           | Iir_Kind_Nature_Reference_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Base_Name;

   function Has_Interface_Declaration_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Interface_Declaration_Chain;

   function Has_Subprogram_Specification (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subprogram_Specification;

   function Has_Sequential_Statement_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Sequential_Statement_Chain;

   function Has_Simultaneous_Statement_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Simultaneous_Statement_Chain;

   function Has_Subprogram_Body (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subprogram_Body;

   function Has_Overload_Number (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Overload_Number;

   function Has_Subprogram_Depth (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subprogram_Depth;

   function Has_Subprogram_Hash (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subprogram_Hash;

   function Has_Impure_Depth (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Impure_Depth;

   function Has_Return_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Return_Type;

   function Has_Implicit_Definition (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Implicit_Definition;

   function Has_Uninstantiated_Subprogram_Name (K : Iir_Kind)
      return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Uninstantiated_Subprogram_Name;

   function Has_Default_Value (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Default_Value;

   function Has_Deferred_Declaration (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Constant_Declaration;
   end Has_Deferred_Declaration;

   function Has_Deferred_Declaration_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Constant_Declaration;
   end Has_Deferred_Declaration_Flag;

   function Has_Shared_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Variable_Declaration
           | Iir_Kind_External_Variable_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Shared_Flag;

   function Has_Design_Unit (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Foreign_Module
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Design_Unit;

   function Has_Block_Statement (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Guard_Signal_Declaration;
   end Has_Block_Statement;

   function Has_Signal_Driver (K : Iir_Kind) return Boolean is
      pragma Unreferenced (K);
   begin
      return False;
   end Has_Signal_Driver;

   function Has_Declaration_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Configuration
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Simultaneous_Procedural_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Declaration_Chain;

   function Has_File_Logical_Name (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_File_Declaration;
   end Has_File_Logical_Name;

   function Has_File_Open_Kind (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_File_Declaration;
   end Has_File_Open_Kind;

   function Has_Element_Position (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Element_Position;

   function Has_Use_Clause_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Use_Clause;
   end Has_Use_Clause_Chain;

   function Has_Context_Reference_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Context_Reference;
   end Has_Context_Reference_Chain;

   function Has_Inherit_Spec_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_PSL_Inherit_Spec;
   end Has_Inherit_Spec_Chain;

   function Has_Selected_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Use_Clause
           | Iir_Kind_Context_Reference =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Selected_Name;

   function Has_Type_Declarator (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Wildcard_Type_Definition
           | Iir_Kind_Foreign_Vector_Type_Definition
           | Iir_Kind_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Declarator;

   function Has_Complete_Type_Definition (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Incomplete_Type_Definition;
   end Has_Complete_Type_Definition;

   function Has_Incomplete_Type_Ref_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Incomplete_Type_Ref_Chain;

   function Has_Associated_Type (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Interface_Type_Definition;
   end Has_Associated_Type;

   function Has_Enumeration_Literal_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Enumeration_Type_Definition;
   end Has_Enumeration_Literal_List;

   function Has_Entity_Class_Entry_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Group_Template_Declaration;
   end Has_Entity_Class_Entry_Chain;

   function Has_Group_Constituent_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Group_Declaration;
   end Has_Group_Constituent_List;

   function Has_Unit_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Physical_Type_Definition;
   end Has_Unit_Chain;

   function Has_Primary_Unit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Physical_Type_Definition;
   end Has_Primary_Unit;

   function Has_Identifier (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Library_Clause
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Record_Element_Resolution
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Foreign_Module
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Selected_Element
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Simultaneous_Null_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Package_Pathname
           | Iir_Kind_Pathname_Element
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Identifier;

   function Has_Label (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Simultaneous_Null_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Label;

   function Has_Return_Identifier (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Function_Declaration;
   end Has_Return_Identifier;

   function Has_Visible_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Simultaneous_Null_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Visible_Flag;

   function Has_Range_Constraint (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Range_Constraint;

   function Has_Direction (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Range_Expression;
   end Has_Direction;

   function Has_Left_Limit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Range_Expression;
   end Has_Left_Limit;

   function Has_Right_Limit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Range_Expression;
   end Has_Right_Limit;

   function Has_Left_Limit_Expr (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Range_Expression;
   end Has_Left_Limit_Expr;

   function Has_Right_Limit_Expr (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Range_Expression;
   end Has_Right_Limit_Expr;

   function Has_Parent_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parent_Type;

   function Has_Simple_Nature (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Simple_Nature;

   function Has_Base_Nature (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Base_Nature;

   function Has_Resolution_Indication (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Element_Resolution
           | Iir_Kind_Record_Element_Resolution
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Resolution_Indication;

   function Has_Record_Element_Resolution_Chain (K : Iir_Kind)
      return Boolean is
   begin
      return K = Iir_Kind_Record_Resolution;
   end Has_Record_Element_Resolution_Chain;

   function Has_Tolerance (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Subtype_Definition
           | Iir_Kind_Array_Subnature_Definition
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_Simple_Simultaneous_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Tolerance;

   function Has_Plus_Terminal_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Plus_Terminal_Name;

   function Has_Minus_Terminal_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Minus_Terminal_Name;

   function Has_Plus_Terminal (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Plus_Terminal;

   function Has_Minus_Terminal (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Minus_Terminal;

   function Has_Magnitude_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Spectrum_Quantity_Declaration;
   end Has_Magnitude_Expression;

   function Has_Phase_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Spectrum_Quantity_Declaration;
   end Has_Phase_Expression;

   function Has_Power_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Noise_Quantity_Declaration;
   end Has_Power_Expression;

   function Has_Simultaneous_Left (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Simple_Simultaneous_Statement;
   end Has_Simultaneous_Left;

   function Has_Simultaneous_Right (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Simple_Simultaneous_Statement;
   end Has_Simultaneous_Right;

   function Has_Text_File_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_File_Type_Definition;
   end Has_Text_File_Flag;

   function Has_Only_Characters_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Enumeration_Type_Definition;
   end Has_Only_Characters_Flag;

   function Has_Is_Character_Type (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Enumeration_Type_Definition;
   end Has_Is_Character_Type;

   function Has_Nature_Staticness (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Nature_Staticness;

   function Has_Type_Staticness (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Wildcard_Type_Definition
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Staticness;

   function Has_Constraint_State (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Constraint_State;

   function Has_Index_Subtype_List (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Index_Subtype_List;

   function Has_Index_Subtype_Definition_List (K : Iir_Kind)
      return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Nature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Index_Subtype_Definition_List;

   function Has_Element_Subtype_Indication (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Element_Resolution
           | Iir_Kind_Array_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Element_Subtype_Indication;

   function Has_Element_Subtype (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Element_Subtype;

   function Has_Element_Subnature_Indication (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Array_Nature_Definition;
   end Has_Element_Subnature_Indication;

   function Has_Element_Subnature (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Element_Subnature;

   function Has_Index_Constraint_List (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Index_Constraint_List;

   function Has_Array_Element_Constraint (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Array_Element_Constraint;

   function Has_Has_Array_Constraint_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Array_Subtype_Definition;
   end Has_Has_Array_Constraint_Flag;

   function Has_Has_Element_Constraint_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Array_Subtype_Definition;
   end Has_Has_Element_Constraint_Flag;

   function Has_Elements_Declaration_List (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Record_Nature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Elements_Declaration_List;

   function Has_Owned_Elements_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Record_Subtype_Definition;
   end Has_Owned_Elements_Chain;

   function Has_Designated_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Designated_Type;

   function Has_Designated_Subtype_Indication (K : Iir_Kind)
      return Boolean is
   begin
      case K is
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Designated_Subtype_Indication;

   function Has_Index_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Indexed_Name;
   end Has_Index_List;

   function Has_Reference (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Scalar_Nature_Definition;
   end Has_Reference;

   function Has_Nature_Declarator (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Nature_Declarator;

   function Has_Across_Type_Mark (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Scalar_Nature_Definition;
   end Has_Across_Type_Mark;

   function Has_Through_Type_Mark (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Scalar_Nature_Definition;
   end Has_Through_Type_Mark;

   function Has_Across_Type_Definition (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Across_Type_Definition;

   function Has_Through_Type_Definition (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Through_Type_Definition;

   function Has_Across_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Across_Type;

   function Has_Through_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Through_Type;

   function Has_Target (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Target;

   function Has_Waveform_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Conditional_Waveform
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Simple_Signal_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Waveform_Chain;

   function Has_Guard (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Guard;

   function Has_Delay_Mechanism (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Delay_Mechanism;

   function Has_Reject_Time_Expression (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Reject_Time_Expression;

   function Has_Force_Mode (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Force_Mode;

   function Has_Has_Force_Mode (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Force_Mode;

   function Has_Sensitivity_List (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Wait_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Sensitivity_List;

   function Has_Process_Origin (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Process_Origin;

   function Has_Package_Origin (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Declaration;
   end Has_Package_Origin;

   function Has_Condition_Clause (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Wait_Statement;
   end Has_Condition_Clause;

   function Has_Break_Element (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Break_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Break_Element;

   function Has_Selector_Quantity (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Break_Element;
   end Has_Selector_Quantity;

   function Has_Break_Quantity (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Break_Element;
   end Has_Break_Quantity;

   function Has_Timeout_Clause (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Wait_Statement;
   end Has_Timeout_Clause;

   function Has_Postponed_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Postponed_Flag;

   function Has_Callees_List (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Callees_List;

   function Has_Passive_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Passive_Flag;

   function Has_Resolution_Function_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Resolution_Function_Flag;

   function Has_Wait_State (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Wait_State;

   function Has_All_Sensitized_State (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_All_Sensitized_State;

   function Has_Seen_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Seen_Flag;

   function Has_Pure_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Pure_Flag;

   function Has_Foreign_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Architecture_Body
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Foreign_Flag;

   function Has_Resolved_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Wildcard_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Resolved_Flag;

   function Has_Signal_Type_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Wildcard_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Signal_Type_Flag;

   function Has_Has_Signal_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Signal_Flag;

   function Has_Purity_State (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Procedure_Declaration;
   end Has_Purity_State;

   function Has_Elab_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Library_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Elab_Flag;

   function Has_Vendor_Library_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Library_Declaration;
   end Has_Vendor_Library_Flag;

   function Has_Configuration_Mark_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Configuration_Mark_Flag;

   function Has_Configuration_Done_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Design_Unit;
   end Has_Configuration_Done_Flag;

   function Has_Index_Constraint_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Index_Constraint_Flag;

   function Has_Hide_Implicit_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Hide_Implicit_Flag;

   function Has_Assertion_Condition (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Assertion_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Assertion_Condition;

   function Has_Report_Expression (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Report_Expression;

   function Has_Severity_Expression (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Severity_Expression;

   function Has_Instantiated_Unit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Component_Instantiation_Statement;
   end Has_Instantiated_Unit;

   function Has_Generic_Map_Aspect_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Package_Header
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Component_Instantiation_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generic_Map_Aspect_Chain;

   function Has_Port_Map_Aspect_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Component_Instantiation_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Port_Map_Aspect_Chain;

   function Has_Configuration_Name (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Entity_Aspect_Configuration;
   end Has_Configuration_Name;

   function Has_Component_Configuration (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Component_Instantiation_Statement;
   end Has_Component_Configuration;

   function Has_Configuration_Specification (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Component_Instantiation_Statement;
   end Has_Configuration_Specification;

   function Has_Default_Binding_Indication (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Component_Instantiation_Statement;
   end Has_Default_Binding_Indication;

   function Has_Default_Configuration_Declaration (K : Iir_Kind)
      return Boolean is
   begin
      return K = Iir_Kind_Architecture_Body;
   end Has_Default_Configuration_Declaration;

   function Has_Expression (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Conditional_Expression
           | Iir_Kind_Break_Element
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Step_Limit_Specification
           | Iir_Kind_Parenthesis_Expression
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell
           | Iir_Kind_Psl_Onehot
           | Iir_Kind_Psl_Onehot0
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_Case_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Expression;

   function Has_Conditional_Expression_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Conditional_Variable_Assignment_Statement;
   end Has_Conditional_Expression_Chain;

   function Has_Allocator_Designated_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Allocator_Designated_Type;

   function Has_Selected_Waveform_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Selected_Waveform_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Selected_Waveform_Chain;

   function Has_Conditional_Waveform_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Conditional_Signal_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Conditional_Waveform_Chain;

   function Has_Guard_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Guard_Signal_Declaration;
   end Has_Guard_Expression;

   function Has_Guard_Decl (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Block_Statement;
   end Has_Guard_Decl;

   function Has_Guard_Sensitivity_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Guard_Signal_Declaration;
   end Has_Guard_Sensitivity_List;

   function Has_Signal_Attribute_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Signal_Attribute_Declaration;
   end Has_Signal_Attribute_Chain;

   function Has_Block_Block_Configuration (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Block_Statement;
   end Has_Block_Block_Configuration;

   function Has_Package_Header (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Package_Declaration;
   end Has_Package_Header;

   function Has_Block_Header (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Block_Statement;
   end Has_Block_Header;

   function Has_Uninstantiated_Package_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Uninstantiated_Package_Name;

   function Has_Uninstantiated_Package_Decl (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Uninstantiated_Package_Decl;

   function Has_Instance_Source_File (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Instance_Source_File;

   function Has_Generate_Block_Configuration (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Generate_Statement_Body;
   end Has_Generate_Block_Configuration;

   function Has_Generate_Statement_Body (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_If_Generate_Else_Clause =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generate_Statement_Body;

   function Has_Alternative_Label (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Generate_Statement_Body;
   end Has_Alternative_Label;

   function Has_Generate_Else_Clause (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_If_Generate_Else_Clause =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generate_Else_Clause;

   function Has_Condition (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Conditional_Waveform
           | Iir_Kind_Conditional_Expression
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Condition;

   function Has_Else_Clause (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Else_Clause;

   function Has_Parameter_Specification (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_For_Generate_Statement
           | Iir_Kind_For_Loop_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter_Specification;

   function Has_Parent (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Library_Clause
           | Iir_Kind_Use_Clause
           | Iir_Kind_Context_Reference
           | Iir_Kind_PSL_Inherit_Spec
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Step_Limit_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Foreign_Module
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Signal_Attribute_Declaration
           | Iir_Kind_Suspend_State_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Simultaneous_Null_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Suspend_State_Statement
           | Iir_Kind_Elsif
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parent;

   function Has_Loop_Label (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Loop_Label;

   function Has_Exit_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Exit_Flag;

   function Has_Next_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Next_Flag;

   function Has_Component_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Specification =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Component_Name;

   function Has_Instantiation_List (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Specification =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Instantiation_List;

   function Has_Entity_Aspect (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Binding_Indication;
   end Has_Entity_Aspect;

   function Has_Default_Entity_Aspect (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Binding_Indication;
   end Has_Default_Entity_Aspect;

   function Has_Binding_Indication (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Specification =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Binding_Indication;

   function Has_Named_Entity (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Selected_Element
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Reference_Name
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Package_Pathname
           | Iir_Kind_Pathname_Element
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Named_Entity;

   function Has_Referenced_Name (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Reference_Name;
   end Has_Referenced_Name;

   function Has_Expr_Staticness (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Error
           | Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Null_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Overflow_Literal
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Range_Expression
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Implicit_Condition_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator
           | Iir_Kind_And_Operator
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
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call
           | Iir_Kind_Aggregate
           | Iir_Kind_Parenthesis_Expression
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell
           | Iir_Kind_Psl_Onehot
           | Iir_Kind_Psl_Onehot0
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Reference_Name
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Expr_Staticness;

   function Has_Scalar_Size (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Scalar_Size;

   function Has_Error_Origin (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Error;
   end Has_Error_Origin;

   function Has_Operand (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Implicit_Condition_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Operand;

   function Has_Left (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Implicit_Condition_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator
           | Iir_Kind_And_Operator
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
            return True;
         when others =>
            return False;
      end case;
   end Has_Left;

   function Has_Right (K : Iir_Kind) return Boolean is
   begin
      case K is
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
            return True;
         when others =>
            return False;
      end case;
   end Has_Right;

   function Has_Unit_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Unit_Name;

   function Has_Name (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_PSL_Inherit_Spec
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Name;

   function Has_Group_Template_Name (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Group_Declaration;
   end Has_Group_Template_Name;

   function Has_Name_Staticness (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Attribute_Value
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Function_Call
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute
           | Iir_Kind_Nature_Reference_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Name_Staticness;

   function Has_Prefix (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Procedure_Call
           | Iir_Kind_Function_Call
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute
           | Iir_Kind_Nature_Reference_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Prefix;

   function Has_Signature_Prefix (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Signature;
   end Has_Signature_Prefix;

   function Has_External_Pathname (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_External_Pathname;

   function Has_Pathname_Suffix (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Package_Pathname
           | Iir_Kind_Absolute_Pathname
           | Iir_Kind_Relative_Pathname
           | Iir_Kind_Pathname_Element =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Pathname_Suffix;

   function Has_Pathname_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Pathname_Element;
   end Has_Pathname_Expression;

   function Has_In_Formal_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_In_Formal_Flag;

   function Has_Slice_Subtype (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Slice_Name;
   end Has_Slice_Subtype;

   function Has_Suffix (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Slice_Name;
   end Has_Suffix;

   function Has_Index_Subtype (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Index_Subtype;

   function Has_Parameter (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter;

   function Has_Parameter_2 (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter_2;

   function Has_Parameter_3 (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Ztf_Attribute;
   end Has_Parameter_3;

   function Has_Parameter_4 (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Ztf_Attribute;
   end Has_Parameter_4;

   function Has_Attr_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Attr_Chain;

   function Has_Signal_Attribute_Declaration (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Signal_Attribute_Declaration;

   function Has_Actual_Type (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Actual_Type;

   function Has_Actual_Type_Definition (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Association_Element_By_Individual;
   end Has_Actual_Type_Definition;

   function Has_Association_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Parenthesis_Name;
   end Has_Association_Chain;

   function Has_Individual_Association_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Association_Element_By_Individual;
   end Has_Individual_Association_Chain;

   function Has_Subprogram_Association_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Association_Element_Type;
   end Has_Subprogram_Association_Chain;

   function Has_Aggregate_Info (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate;
   end Has_Aggregate_Info;

   function Has_Sub_Aggregate_Info (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate_Info;
   end Has_Sub_Aggregate_Info;

   function Has_Aggr_Dynamic_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate_Info;
   end Has_Aggr_Dynamic_Flag;

   function Has_Aggr_Min_Length (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate_Info;
   end Has_Aggr_Min_Length;

   function Has_Aggr_Low_Limit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate_Info;
   end Has_Aggr_Low_Limit;

   function Has_Aggr_High_Limit (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate_Info;
   end Has_Aggr_High_Limit;

   function Has_Aggr_Others_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate_Info;
   end Has_Aggr_Others_Flag;

   function Has_Aggr_Named_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate_Info;
   end Has_Aggr_Named_Flag;

   function Has_Aggregate_Expand_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate;
   end Has_Aggregate_Expand_Flag;

   function Has_Association_Choices_Chain (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Aggregate;
   end Has_Association_Choices_Chain;

   function Has_Case_Statement_Alternative_Chain (K : Iir_Kind)
      return Boolean is
   begin
      case K is
         when Iir_Kind_Case_Generate_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Case_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Case_Statement_Alternative_Chain;

   function Has_Matching_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Case_Statement;
   end Has_Matching_Flag;

   function Has_Choice_Staticness (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Choice_Staticness;

   function Has_Procedure_Call (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Procedure_Call_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Procedure_Call;

   function Has_Implementation (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Procedure_Call
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Implicit_Condition_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator
           | Iir_Kind_And_Operator
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
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Implementation;

   function Has_Parameter_Association_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Procedure_Call
           | Iir_Kind_Function_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter_Association_Chain;

   function Has_Method_Object (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Procedure_Call
           | Iir_Kind_Function_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Method_Object;

   function Has_Subtype_Type_Mark (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Subtype_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subtype_Type_Mark;

   function Has_Subnature_Nature_Mark (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Array_Subnature_Definition;
   end Has_Subnature_Nature_Mark;

   function Has_Type_Conversion_Subtype (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Type_Conversion;
   end Has_Type_Conversion_Subtype;

   function Has_Type_Mark (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Disconnection_Specification
           | Iir_Kind_Step_Limit_Specification
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Mark;

   function Has_File_Type_Mark (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_File_Type_Definition;
   end Has_File_Type_Mark;

   function Has_Return_Type_Mark (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Signature
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Return_Type_Mark;

   function Has_Has_Disconnect_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Disconnect_Flag;

   function Has_Has_Active_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Active_Flag;

   function Has_Is_Within_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_For_Loop_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Is_Within_Flag;

   function Has_Type_Marks_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Signature;
   end Has_Type_Marks_List;

   function Has_Implicit_Alias_Flag (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Non_Object_Alias_Declaration;
   end Has_Implicit_Alias_Flag;

   function Has_Alias_Signature (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Non_Object_Alias_Declaration;
   end Has_Alias_Signature;

   function Has_Attribute_Signature (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Attribute_Name;
   end Has_Attribute_Signature;

   function Has_Overload_List (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Overload_List;
   end Has_Overload_List;

   function Has_Simple_Name_Identifier (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Simple_Name_Attribute;
   end Has_Simple_Name_Identifier;

   function Has_Simple_Name_Subtype (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Simple_Name_Attribute;
   end Has_Simple_Name_Subtype;

   function Has_Protected_Type_Body (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Protected_Type_Declaration;
   end Has_Protected_Type_Body;

   function Has_Protected_Type_Declaration (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Protected_Type_Body;
   end Has_Protected_Type_Declaration;

   function Has_Use_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Use_Flag;

   function Has_End_Has_Reserved_Id (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_End_Has_Reserved_Id;

   function Has_End_Has_Identifier (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_End_Has_Identifier;

   function Has_End_Has_Postponed (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_End_Has_Postponed;

   function Has_Has_Label (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Generate_Statement_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Label;

   function Has_Has_Begin (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Generate_Statement_Body =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Begin;

   function Has_Has_End (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Generate_Statement_Body;
   end Has_Has_End;

   function Has_Has_Is (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Component_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Is;

   function Has_Has_Pure (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Pure;

   function Has_Has_Body (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Body;

   function Has_Has_Parameter (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Parameter;

   function Has_Has_Component (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Component_Instantiation_Statement;
   end Has_Has_Component;

   function Has_Has_Identifier_List (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Library_Clause
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Identifier_List;

   function Has_Has_Mode (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_File_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Mode;

   function Has_Has_Class (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Class;

   function Has_Has_Delay_Mechanism (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Delay_Mechanism;

   function Has_Suspend_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Suspend_Flag;

   function Has_Is_Ref (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Conditional_Waveform
           | Iir_Kind_Conditional_Expression
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Step_Limit_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Subtype_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Is_Ref;

   function Has_Is_Forward_Ref (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Selected_Element
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Reference_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Package_Pathname
           | Iir_Kind_Pathname_Element
           | Iir_Kind_Attribute_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Is_Forward_Ref;

   function Has_Psl_Property (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Psl_Property;

   function Has_Psl_Sequence (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Psl_Sequence;

   function Has_Psl_Declaration (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Psl_Declaration;

   function Has_Psl_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Psl_Expression;
   end Has_Psl_Expression;

   function Has_Psl_Boolean (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Psl_Default_Clock;
   end Has_Psl_Boolean;

   function Has_PSL_Clock (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_PSL_Clock;

   function Has_PSL_NFA (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_PSL_NFA;

   function Has_PSL_Nbr_States (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_PSL_Nbr_States;

   function Has_PSL_Clock_Sensitivity (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_PSL_Clock_Sensitivity;

   function Has_PSL_EOS_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_PSL_EOS_Flag;

   function Has_PSL_Abort_Flag (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive =>
            return True;
         when others =>
            return False;
      end case;
   end Has_PSL_Abort_Flag;

   function Has_Count_Expression (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Psl_Prev;
   end Has_Count_Expression;

   function Has_Clock_Expression (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Clock_Expression;

   function Has_Default_Clock (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Default_Clock;

   function Has_Foreign_Node (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Foreign_Module;
   end Has_Foreign_Node;

   function Has_Suspend_State_Index (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Suspend_State_Statement;
   end Has_Suspend_State_Index;

   function Has_Suspend_State_Chain (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Suspend_State_Declaration
           | Iir_Kind_Suspend_State_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Suspend_State_Chain;

end Vhdl.Nodes_Meta;
