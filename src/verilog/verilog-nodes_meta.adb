--  Meta description of nodes.
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

package body Verilog.Nodes_Meta is
   Fields_Type : constant array (Fields_Enum) of Types_Enum :=
     (
      Field_Parent => Type_Node,
      Field_Call_Scope => Type_Node,
      Field_Identifier => Type_Name_Id,
      Field_C_Identifier => Type_Name_Id,
      Field_Ports_Chain => Type_Node,
      Field_Tf_Ports_Chain => Type_Node,
      Field_Package_Import_Chain => Type_Node,
      Field_Parameter_Port_Chain => Type_Node,
      Field_Parameter => Type_Node,
      Field_Foreign_Node => Type_Int32,
      Field_Descriptions => Type_Node,
      Field_Class_Item_Chain => Type_Node,
      Field_Package_Item_Chain => Type_Node,
      Field_Items_Chain => Type_Node,
      Field_Clocking_Item_Chain => Type_Node,
      Field_Tf_Item_Declaration_Chain => Type_Node,
      Field_Block_Item_Declaration_Chain => Type_Node,
      Field_Generate_Item_Chain => Type_Node,
      Field_Specify_Item_Chain => Type_Node,
      Field_Statements_Chain => Type_Node,
      Field_Modport_Ports_Chain => Type_Node,
      Field_Chain => Type_Node,
      Field_Constraint_Block_Chain => Type_Node,
      Field_Constraint_Set => Type_Node,
      Field_OOB_Prefix => Type_Node,
      Field_Out_Of_Block_Declaration => Type_Node,
      Field_Generate_Index => Type_Int32,
      Field_Return_Variable => Type_Node,
      Field_Return_Variable_Ref => Type_Node,
      Field_This_Variable => Type_Node,
      Field_Expression => Type_Node,
      Field_Reject_Limit => Type_Node,
      Field_Error_Limit => Type_Node,
      Field_Sequence => Type_Node,
      Field_Init_Expression => Type_Node,
      Field_Size_Expression => Type_Node,
      Field_Override_Stmt => Type_Node,
      Field_Parameter_Expression => Type_Node,
      Field_Parameter_Type => Type_Node,
      Field_Value_Range => Type_Node,
      Field_Lsb_Include_Flag => Type_Boolean,
      Field_Msb_Include_Flag => Type_Boolean,
      Field_Range => Type_Node,
      Field_Msb => Type_Node,
      Field_Lsb => Type_Node,
      Field_Msb_Cst => Type_Int32,
      Field_Lsb_Cst => Type_Int32,
      Field_Base_Expr => Type_Node,
      Field_Width_Expr => Type_Node,
      Field_Width_Cst => Type_Int32,
      Field_Type_Width => Type_Width_Type,
      Field_Type_Size => Type_Tsize_Type,
      Field_Stride_Width => Type_Width_Type,
      Field_Stride_Size => Type_Tsize_Type,
      Field_Type_Hash => Type_Uns32,
      Field_Maximum_Size_Expr => Type_Node,
      Field_Maximum_Size_Cst => Type_Int32,
      Field_Lvalue => Type_Node,
      Field_Name => Type_Node,
      Field_Item_Name => Type_Node,
      Field_Pattern_Key => Type_Node,
      Field_Left => Type_Node,
      Field_Right => Type_Node,
      Field_Repeat_Expression => Type_Node,
      Field_Op_Attributes => Type_Node,
      Field_Attributes_Chain => Type_Node,
      Field_Condition => Type_Node,
      Field_Cond_True => Type_Node,
      Field_Cond_False => Type_Node,
      Field_True_Stmt => Type_Node,
      Field_False_Stmt => Type_Node,
      Field_Pass_Stmt => Type_Node,
      Field_Else_Stmt => Type_Node,
      Field_Clocking_Event => Type_Node,
      Field_Disable_Expression => Type_Node,
      Field_Property_Expression => Type_Node,
      Field_True_Block => Type_Node,
      Field_False_Block => Type_Node,
      Field_Statement => Type_Node,
      Field_Foreach_Array => Type_Node,
      Field_Foreach_Variables => Type_Node,
      Field_Control => Type_Node,
      Field_Replication => Type_Node,
      Field_Replication_Cst => Type_Int32,
      Field_Expressions => Type_Node,
      Field_Elements => Type_Node,
      Field_Slice_Size_Expr => Type_Node,
      Field_Slice_Size_Type => Type_Node,
      Field_Members => Type_Node,
      Field_Nbr_Members => Type_Int32,
      Field_Member_Index => Type_Int32,
      Field_Packed_Member_Offset => Type_Uns32,
      Field_Nature_Items => Type_Node,
      Field_Discipline_Items => Type_Node,
      Field_Continuous_Flag => Type_Boolean,
      Field_Potential_Flag => Type_Boolean,
      Field_Nature => Type_Node,
      Field_Connections => Type_Node,
      Field_Gate_Terminals => Type_Node,
      Field_Parameter_Values => Type_Node,
      Field_Case_Items => Type_Node,
      Field_Delay => Type_Node,
      Field_Net_Delay => Type_Node,
      Field_Gate_Delay => Type_Node,
      Field_Assign_Delay => Type_Node,
      Field_Rising_Delay => Type_Node,
      Field_Falling_Delay => Type_Node,
      Field_Highz_Delay => Type_Node,
      Field_For_Initialization => Type_Node,
      Field_Step_Assign => Type_Node,
      Field_Arguments => Type_Node,
      Field_Iterator_Argument => Type_Node,
      Field_Task => Type_Node,
      Field_Signed_Flag => Type_Boolean,
      Field_Scope_Flag => Type_Boolean,
      Field_Number_Base => Type_Base_Type,
      Field_Number_Hi_Val => Type_Uns32,
      Field_Number_Lo_Val => Type_Uns32,
      Field_Number_Hi_Zx => Type_Uns32,
      Field_Number_Lo_Zx => Type_Uns32,
      Field_Number_Size => Type_Width_Type,
      Field_Expr_Origin => Type_Node,
      Field_Bignum_Index => Type_Bn_Index,
      Field_Bignum_Len => Type_Uns32,
      Field_Real_Number => Type_Fp64,
      Field_Time_Unit => Type_Int32,
      Field_Scale_Factor => Type_Int32,
      Field_Time_Precision => Type_Int32,
      Field_Timescale => Type_Node,
      Field_String_Size => Type_Uns32,
      Field_Data_Type => Type_Node,
      Field_Expr_Type => Type_Node,
      Field_Param_Type => Type_Node,
      Field_Element_Data_Type => Type_Node,
      Field_Type_Element_Type => Type_Node,
      Field_Cast_Data_Type => Type_Node,
      Field_Base_Class_Type => Type_Node,
      Field_Class_Constructor => Type_Node,
      Field_Inheritance_Depth => Type_Int32,
      Field_Enum_Base_Data_Type => Type_Node,
      Field_Enum_Base_Type => Type_Node,
      Field_Packed_Base_Type => Type_Node,
      Field_Default_Type => Type_Node,
      Field_Type_Owner => Type_Boolean,
      Field_Type_Owner_2 => Type_Boolean,
      Field_Forward_Type => Type_Node,
      Field_Enum_Names => Type_Node,
      Field_Index_Data_Type => Type_Node,
      Field_Type_Index_Type => Type_Node,
      Field_Type_Argument => Type_Node,
      Field_Type_Signed => Type_Boolean,
      Field_Subroutine => Type_Node,
      Field_Object => Type_Node,
      Field_With_Expression => Type_Node,
      Field_Drive_Strength => Type_Int32,
      Field_Net_Drive_Strength => Type_Int32,
      Field_Charge_Strength => Type_Int32,
      Field_Module => Type_Node,
      Field_Class_Name => Type_Node,
      Field_Interface => Type_Node,
      Field_Interface_Name => Type_Node,
      Field_Instance => Type_Node,
      Field_Instance_Ref => Type_Node,
      Field_Port => Type_Node,
      Field_Collapse_Flag => Type_Boolean,
      Field_Unary_Op => Type_Unary_Ops,
      Field_Binary_Op => Type_Binary_Ops,
      Field_Conversion_Op => Type_Conv_Ops,
      Field_Declaration => Type_Node,
      Field_Redeclaration => Type_Node,
      Field_This_Declaration => Type_Node,
      Field_Default_Value => Type_Node,
      Field_Instantiated_Flag => Type_Boolean,
      Field_Ansi_Port_Flag => Type_Boolean,
      Field_Event => Type_Node,
      Field_Min_Expr => Type_Node,
      Field_Typ_Expr => Type_Node,
      Field_Max_Expr => Type_Node,
      Field_Udp_Port_Declaration_Chain => Type_Node,
      Field_Udp_Kind => Type_Udp_Kind,
      Field_Udp_Initial => Type_Node,
      Field_Udp_Entries_Chain => Type_Node,
      Field_Input_Chain => Type_Node,
      Field_Output_Symbol => Type_Udp_Symbol,
      Field_Current_State => Type_Udp_Symbol,
      Field_Next_State => Type_Udp_Symbol,
      Field_Symbol => Type_Udp_Symbol,
      Field_From_Symbol => Type_Udp_Symbol,
      Field_To_Symbol => Type_Udp_Symbol,
      Field_Specify_Input => Type_Node,
      Field_Specify_Output => Type_Node,
      Field_Path_Delay => Type_Node,
      Field_Data_Source => Type_Node,
      Field_Polarity => Type_Polarity_Type,
      Field_Delay_Rise => Type_Node,
      Field_Delay_Fall => Type_Node,
      Field_Delay_Z => Type_Node,
      Field_Delay_01 => Type_Node,
      Field_Delay_10 => Type_Node,
      Field_Delay_0z => Type_Node,
      Field_Delay_z1 => Type_Node,
      Field_Delay_1z => Type_Node,
      Field_Delay_z0 => Type_Node,
      Field_Delay_0x => Type_Node,
      Field_Delay_x1 => Type_Node,
      Field_Delay_1x => Type_Node,
      Field_Delay_x0 => Type_Node,
      Field_Delay_xz => Type_Node,
      Field_Delay_zx => Type_Node,
      Field_String_Id => Type_String8_Id,
      Field_Label => Type_Node,
      Field_Label_Number => Type_Int32,
      Field_Label_Chain => Type_Node,
      Field_Label_Use => Type_Int32,
      Field_Suspend_Flag => Type_Boolean,
      Field_Same_Case_Flag => Type_Boolean,
      Field_Obj_Id => Type_Obj_Id,
      Field_Scope_Id => Type_Scope_Id,
      Field_Process_Id => Type_Proc_Id,
      Field_Sys_Tf_Id => Type_Sys_Tf_Id,
      Field_Lit_Id => Type_Lit_Id,
      Field_Generate_Block => Type_Node,
      Field_Input_Skew => Type_Node,
      Field_Output_Skew => Type_Node,
      Field_Delay_Control => Type_Node,
      Field_Attribute_Item => Type_Node,
      Field_Has_Identifier_List => Type_Boolean,
      Field_Has_Sign => Type_Boolean,
      Field_Connected_Flag => Type_Boolean,
      Field_Complete_Flag => Type_Boolean,
      Field_Implicit_Flag => Type_Boolean,
      Field_Redeclaration_Flag => Type_Boolean,
      Field_Is_Automatic => Type_Boolean,
      Field_Lifetime => Type_Lifetime_Type,
      Field_Has_Lifetime => Type_Boolean,
      Field_Has_End_Name => Type_Boolean,
      Field_Call => Type_Node,
      Field_Timeunit => Type_Node,
      Field_Timeprecision => Type_Node,
      Field_Error_Origin => Type_Node,
      Field_Has_Void_Cast => Type_Boolean,
      Field_Is_Const => Type_Boolean,
      Field_Has_Var => Type_Boolean,
      Field_Has_Type => Type_Boolean,
      Field_Has_Direction => Type_Boolean,
      Field_Has_Parenthesis => Type_Boolean,
      Field_Has_Argument => Type_Boolean,
      Field_Fully_Analyzed_Flag => Type_Boolean,
      Field_Resolved_Flag => Type_Boolean,
      Field_Mark_Flag => Type_Boolean,
      Field_Is_Constant => Type_Boolean,
      Field_Static_Flag => Type_Boolean,
      Field_Has_Attribute => Type_Boolean,
      Field_Attribute_Full => Type_Boolean,
      Field_Attribute_Parallel => Type_Boolean,
      Field_Other_Attributes => Type_Boolean,
      Field_Pure_Property => Type_Boolean,
      Field_Context_Property => Type_Boolean,
      Field_Has_Extern_Flag => Type_Boolean,
      Field_Virtual_Flag => Type_Boolean,
      Field_Pure_Flag => Type_Boolean,
      Field_Join_Option => Type_Join_Type,
      Field_Edge_Identifier => Type_Edge_Type,
      Field_DPI_Spec => Type_DPI_Spec_Type,
      Field_Visibility => Type_Visibility_Type,
      Field_Class_Visibility => Type_Visibility_Type,
      Field_Has_Visibility => Type_Boolean,
      Field_Violation => Type_Violation_Type,
      Field_Random_Flag => Type_Boolean,
      Field_Randc_Flag => Type_Boolean,
      Field_Size_Flag => Type_Boolean,
      Field_Type_Analyzed_Flag => Type_Boolean,
      Field_Forward_Typedef_Flag => Type_Boolean,
      Field_Access => Type_Node,
      Field_Arg1 => Type_Node,
      Field_Arg2 => Type_Node
     );

   function Get_Field_Type (F : Fields_Enum) return Types_Enum is
   begin
      return Fields_Type (F);
   end Get_Field_Type;

   function Get_Field_Image (F : Fields_Enum) return String is
   begin
      case F is
         when Field_Parent =>
            return "parent";
         when Field_Call_Scope =>
            return "call_scope";
         when Field_Identifier =>
            return "identifier";
         when Field_C_Identifier =>
            return "c_identifier";
         when Field_Ports_Chain =>
            return "ports_chain";
         when Field_Tf_Ports_Chain =>
            return "tf_ports_chain";
         when Field_Package_Import_Chain =>
            return "package_import_chain";
         when Field_Parameter_Port_Chain =>
            return "parameter_port_chain";
         when Field_Parameter =>
            return "parameter";
         when Field_Foreign_Node =>
            return "foreign_node";
         when Field_Descriptions =>
            return "descriptions";
         when Field_Class_Item_Chain =>
            return "class_item_chain";
         when Field_Package_Item_Chain =>
            return "package_item_chain";
         when Field_Items_Chain =>
            return "items_chain";
         when Field_Clocking_Item_Chain =>
            return "clocking_item_chain";
         when Field_Tf_Item_Declaration_Chain =>
            return "tf_item_declaration_chain";
         when Field_Block_Item_Declaration_Chain =>
            return "block_item_declaration_chain";
         when Field_Generate_Item_Chain =>
            return "generate_item_chain";
         when Field_Specify_Item_Chain =>
            return "specify_item_chain";
         when Field_Statements_Chain =>
            return "statements_chain";
         when Field_Modport_Ports_Chain =>
            return "modport_ports_chain";
         when Field_Chain =>
            return "chain";
         when Field_Constraint_Block_Chain =>
            return "constraint_block_chain";
         when Field_Constraint_Set =>
            return "constraint_set";
         when Field_OOB_Prefix =>
            return "oob_prefix";
         when Field_Out_Of_Block_Declaration =>
            return "out_of_block_declaration";
         when Field_Generate_Index =>
            return "generate_index";
         when Field_Return_Variable =>
            return "return_variable";
         when Field_Return_Variable_Ref =>
            return "return_variable_ref";
         when Field_This_Variable =>
            return "this_variable";
         when Field_Expression =>
            return "expression";
         when Field_Reject_Limit =>
            return "reject_limit";
         when Field_Error_Limit =>
            return "error_limit";
         when Field_Sequence =>
            return "sequence";
         when Field_Init_Expression =>
            return "init_expression";
         when Field_Size_Expression =>
            return "size_expression";
         when Field_Override_Stmt =>
            return "override_stmt";
         when Field_Parameter_Expression =>
            return "parameter_expression";
         when Field_Parameter_Type =>
            return "parameter_type";
         when Field_Value_Range =>
            return "value_range";
         when Field_Lsb_Include_Flag =>
            return "lsb_include_flag";
         when Field_Msb_Include_Flag =>
            return "msb_include_flag";
         when Field_Range =>
            return "range";
         when Field_Msb =>
            return "msb";
         when Field_Lsb =>
            return "lsb";
         when Field_Msb_Cst =>
            return "msb_cst";
         when Field_Lsb_Cst =>
            return "lsb_cst";
         when Field_Base_Expr =>
            return "base_expr";
         when Field_Width_Expr =>
            return "width_expr";
         when Field_Width_Cst =>
            return "width_cst";
         when Field_Type_Width =>
            return "type_width";
         when Field_Type_Size =>
            return "type_size";
         when Field_Stride_Width =>
            return "stride_width";
         when Field_Stride_Size =>
            return "stride_size";
         when Field_Type_Hash =>
            return "type_hash";
         when Field_Maximum_Size_Expr =>
            return "maximum_size_expr";
         when Field_Maximum_Size_Cst =>
            return "maximum_size_cst";
         when Field_Lvalue =>
            return "lvalue";
         when Field_Name =>
            return "name";
         when Field_Item_Name =>
            return "item_name";
         when Field_Pattern_Key =>
            return "pattern_key";
         when Field_Left =>
            return "left";
         when Field_Right =>
            return "right";
         when Field_Repeat_Expression =>
            return "repeat_expression";
         when Field_Op_Attributes =>
            return "op_attributes";
         when Field_Attributes_Chain =>
            return "attributes_chain";
         when Field_Condition =>
            return "condition";
         when Field_Cond_True =>
            return "cond_true";
         when Field_Cond_False =>
            return "cond_false";
         when Field_True_Stmt =>
            return "true_stmt";
         when Field_False_Stmt =>
            return "false_stmt";
         when Field_Pass_Stmt =>
            return "pass_stmt";
         when Field_Else_Stmt =>
            return "else_stmt";
         when Field_Clocking_Event =>
            return "clocking_event";
         when Field_Disable_Expression =>
            return "disable_expression";
         when Field_Property_Expression =>
            return "property_expression";
         when Field_True_Block =>
            return "true_block";
         when Field_False_Block =>
            return "false_block";
         when Field_Statement =>
            return "statement";
         when Field_Foreach_Array =>
            return "foreach_array";
         when Field_Foreach_Variables =>
            return "foreach_variables";
         when Field_Control =>
            return "control";
         when Field_Replication =>
            return "replication";
         when Field_Replication_Cst =>
            return "replication_cst";
         when Field_Expressions =>
            return "expressions";
         when Field_Elements =>
            return "elements";
         when Field_Slice_Size_Expr =>
            return "slice_size_expr";
         when Field_Slice_Size_Type =>
            return "slice_size_type";
         when Field_Members =>
            return "members";
         when Field_Nbr_Members =>
            return "nbr_members";
         when Field_Member_Index =>
            return "member_index";
         when Field_Packed_Member_Offset =>
            return "packed_member_offset";
         when Field_Nature_Items =>
            return "nature_items";
         when Field_Discipline_Items =>
            return "discipline_items";
         when Field_Continuous_Flag =>
            return "continuous_flag";
         when Field_Potential_Flag =>
            return "potential_flag";
         when Field_Nature =>
            return "nature";
         when Field_Connections =>
            return "connections";
         when Field_Gate_Terminals =>
            return "gate_terminals";
         when Field_Parameter_Values =>
            return "parameter_values";
         when Field_Case_Items =>
            return "case_items";
         when Field_Delay =>
            return "delay";
         when Field_Net_Delay =>
            return "net_delay";
         when Field_Gate_Delay =>
            return "gate_delay";
         when Field_Assign_Delay =>
            return "assign_delay";
         when Field_Rising_Delay =>
            return "rising_delay";
         when Field_Falling_Delay =>
            return "falling_delay";
         when Field_Highz_Delay =>
            return "highz_delay";
         when Field_For_Initialization =>
            return "for_initialization";
         when Field_Step_Assign =>
            return "step_assign";
         when Field_Arguments =>
            return "arguments";
         when Field_Iterator_Argument =>
            return "iterator_argument";
         when Field_Task =>
            return "task";
         when Field_Signed_Flag =>
            return "signed_flag";
         when Field_Scope_Flag =>
            return "scope_flag";
         when Field_Number_Base =>
            return "number_base";
         when Field_Number_Hi_Val =>
            return "number_hi_val";
         when Field_Number_Lo_Val =>
            return "number_lo_val";
         when Field_Number_Hi_Zx =>
            return "number_hi_zx";
         when Field_Number_Lo_Zx =>
            return "number_lo_zx";
         when Field_Number_Size =>
            return "number_size";
         when Field_Expr_Origin =>
            return "expr_origin";
         when Field_Bignum_Index =>
            return "bignum_index";
         when Field_Bignum_Len =>
            return "bignum_len";
         when Field_Real_Number =>
            return "real_number";
         when Field_Time_Unit =>
            return "time_unit";
         when Field_Scale_Factor =>
            return "scale_factor";
         when Field_Time_Precision =>
            return "time_precision";
         when Field_Timescale =>
            return "timescale";
         when Field_String_Size =>
            return "string_size";
         when Field_Data_Type =>
            return "data_type";
         when Field_Expr_Type =>
            return "expr_type";
         when Field_Param_Type =>
            return "param_type";
         when Field_Element_Data_Type =>
            return "element_data_type";
         when Field_Type_Element_Type =>
            return "type_element_type";
         when Field_Cast_Data_Type =>
            return "cast_data_type";
         when Field_Base_Class_Type =>
            return "base_class_type";
         when Field_Class_Constructor =>
            return "class_constructor";
         when Field_Inheritance_Depth =>
            return "inheritance_depth";
         when Field_Enum_Base_Data_Type =>
            return "enum_base_data_type";
         when Field_Enum_Base_Type =>
            return "enum_base_type";
         when Field_Packed_Base_Type =>
            return "packed_base_type";
         when Field_Default_Type =>
            return "default_type";
         when Field_Type_Owner =>
            return "type_owner";
         when Field_Type_Owner_2 =>
            return "type_owner_2";
         when Field_Forward_Type =>
            return "forward_type";
         when Field_Enum_Names =>
            return "enum_names";
         when Field_Index_Data_Type =>
            return "index_data_type";
         when Field_Type_Index_Type =>
            return "type_index_type";
         when Field_Type_Argument =>
            return "type_argument";
         when Field_Type_Signed =>
            return "type_signed";
         when Field_Subroutine =>
            return "subroutine";
         when Field_Object =>
            return "object";
         when Field_With_Expression =>
            return "with_expression";
         when Field_Drive_Strength =>
            return "drive_strength";
         when Field_Net_Drive_Strength =>
            return "net_drive_strength";
         when Field_Charge_Strength =>
            return "charge_strength";
         when Field_Module =>
            return "module";
         when Field_Class_Name =>
            return "class_name";
         when Field_Interface =>
            return "interface";
         when Field_Interface_Name =>
            return "interface_name";
         when Field_Instance =>
            return "instance";
         when Field_Instance_Ref =>
            return "instance_ref";
         when Field_Port =>
            return "port";
         when Field_Collapse_Flag =>
            return "collapse_flag";
         when Field_Unary_Op =>
            return "unary_op";
         when Field_Binary_Op =>
            return "binary_op";
         when Field_Conversion_Op =>
            return "conversion_op";
         when Field_Declaration =>
            return "declaration";
         when Field_Redeclaration =>
            return "redeclaration";
         when Field_This_Declaration =>
            return "this_declaration";
         when Field_Default_Value =>
            return "default_value";
         when Field_Instantiated_Flag =>
            return "instantiated_flag";
         when Field_Ansi_Port_Flag =>
            return "ansi_port_flag";
         when Field_Event =>
            return "event";
         when Field_Min_Expr =>
            return "min_expr";
         when Field_Typ_Expr =>
            return "typ_expr";
         when Field_Max_Expr =>
            return "max_expr";
         when Field_Udp_Port_Declaration_Chain =>
            return "udp_port_declaration_chain";
         when Field_Udp_Kind =>
            return "udp_kind";
         when Field_Udp_Initial =>
            return "udp_initial";
         when Field_Udp_Entries_Chain =>
            return "udp_entries_chain";
         when Field_Input_Chain =>
            return "input_chain";
         when Field_Output_Symbol =>
            return "output_symbol";
         when Field_Current_State =>
            return "current_state";
         when Field_Next_State =>
            return "next_state";
         when Field_Symbol =>
            return "symbol";
         when Field_From_Symbol =>
            return "from_symbol";
         when Field_To_Symbol =>
            return "to_symbol";
         when Field_Specify_Input =>
            return "specify_input";
         when Field_Specify_Output =>
            return "specify_output";
         when Field_Path_Delay =>
            return "path_delay";
         when Field_Data_Source =>
            return "data_source";
         when Field_Polarity =>
            return "polarity";
         when Field_Delay_Rise =>
            return "delay_rise";
         when Field_Delay_Fall =>
            return "delay_fall";
         when Field_Delay_Z =>
            return "delay_z";
         when Field_Delay_01 =>
            return "delay_01";
         when Field_Delay_10 =>
            return "delay_10";
         when Field_Delay_0z =>
            return "delay_0z";
         when Field_Delay_z1 =>
            return "delay_z1";
         when Field_Delay_1z =>
            return "delay_1z";
         when Field_Delay_z0 =>
            return "delay_z0";
         when Field_Delay_0x =>
            return "delay_0x";
         when Field_Delay_x1 =>
            return "delay_x1";
         when Field_Delay_1x =>
            return "delay_1x";
         when Field_Delay_x0 =>
            return "delay_x0";
         when Field_Delay_xz =>
            return "delay_xz";
         when Field_Delay_zx =>
            return "delay_zx";
         when Field_String_Id =>
            return "string_id";
         when Field_Label =>
            return "label";
         when Field_Label_Number =>
            return "label_number";
         when Field_Label_Chain =>
            return "label_chain";
         when Field_Label_Use =>
            return "label_use";
         when Field_Suspend_Flag =>
            return "suspend_flag";
         when Field_Same_Case_Flag =>
            return "same_case_flag";
         when Field_Obj_Id =>
            return "obj_id";
         when Field_Scope_Id =>
            return "scope_id";
         when Field_Process_Id =>
            return "process_id";
         when Field_Sys_Tf_Id =>
            return "sys_tf_id";
         when Field_Lit_Id =>
            return "lit_id";
         when Field_Generate_Block =>
            return "generate_block";
         when Field_Input_Skew =>
            return "input_skew";
         when Field_Output_Skew =>
            return "output_skew";
         when Field_Delay_Control =>
            return "delay_control";
         when Field_Attribute_Item =>
            return "attribute_item";
         when Field_Has_Identifier_List =>
            return "has_identifier_list";
         when Field_Has_Sign =>
            return "has_sign";
         when Field_Connected_Flag =>
            return "connected_flag";
         when Field_Complete_Flag =>
            return "complete_flag";
         when Field_Implicit_Flag =>
            return "implicit_flag";
         when Field_Redeclaration_Flag =>
            return "redeclaration_flag";
         when Field_Is_Automatic =>
            return "is_automatic";
         when Field_Lifetime =>
            return "lifetime";
         when Field_Has_Lifetime =>
            return "has_lifetime";
         when Field_Has_End_Name =>
            return "has_end_name";
         when Field_Call =>
            return "call";
         when Field_Timeunit =>
            return "timeunit";
         when Field_Timeprecision =>
            return "timeprecision";
         when Field_Error_Origin =>
            return "error_origin";
         when Field_Has_Void_Cast =>
            return "has_void_cast";
         when Field_Is_Const =>
            return "is_const";
         when Field_Has_Var =>
            return "has_var";
         when Field_Has_Type =>
            return "has_type";
         when Field_Has_Direction =>
            return "has_direction";
         when Field_Has_Parenthesis =>
            return "has_parenthesis";
         when Field_Has_Argument =>
            return "has_argument";
         when Field_Fully_Analyzed_Flag =>
            return "fully_analyzed_flag";
         when Field_Resolved_Flag =>
            return "resolved_flag";
         when Field_Mark_Flag =>
            return "mark_flag";
         when Field_Is_Constant =>
            return "is_constant";
         when Field_Static_Flag =>
            return "static_flag";
         when Field_Has_Attribute =>
            return "has_attribute";
         when Field_Attribute_Full =>
            return "attribute_full";
         when Field_Attribute_Parallel =>
            return "attribute_parallel";
         when Field_Other_Attributes =>
            return "other_attributes";
         when Field_Pure_Property =>
            return "pure_property";
         when Field_Context_Property =>
            return "context_property";
         when Field_Has_Extern_Flag =>
            return "has_extern_flag";
         when Field_Virtual_Flag =>
            return "virtual_flag";
         when Field_Pure_Flag =>
            return "pure_flag";
         when Field_Join_Option =>
            return "join_option";
         when Field_Edge_Identifier =>
            return "edge_identifier";
         when Field_DPI_Spec =>
            return "dpi_spec";
         when Field_Visibility =>
            return "visibility";
         when Field_Class_Visibility =>
            return "class_visibility";
         when Field_Has_Visibility =>
            return "has_visibility";
         when Field_Violation =>
            return "violation";
         when Field_Random_Flag =>
            return "random_flag";
         when Field_Randc_Flag =>
            return "randc_flag";
         when Field_Size_Flag =>
            return "size_flag";
         when Field_Type_Analyzed_Flag =>
            return "type_analyzed_flag";
         when Field_Forward_Typedef_Flag =>
            return "forward_typedef_flag";
         when Field_Access =>
            return "access";
         when Field_Arg1 =>
            return "arg1";
         when Field_Arg2 =>
            return "arg2";
      end case;
   end Get_Field_Image;

   function Get_Nkind_Image (K : Nkind) return String is
   begin
      case K is
         when N_Error =>
            return "error";
         when N_Error_Expr =>
            return "error_expr";
         when N_Timescale_Directive =>
            return "timescale_directive";
         when N_Timeunits_Declaration =>
            return "timeunits_declaration";
         when N_Timeunit =>
            return "timeunit";
         when N_Timeprecision =>
            return "timeprecision";
         when N_Logic_Type =>
            return "logic_type";
         when N_Bit_Type =>
            return "bit_type";
         when N_Real_Type =>
            return "real_type";
         when N_Shortreal_Type =>
            return "shortreal_type";
         when N_Log_Packed_Array_Cst =>
            return "log_packed_array_cst";
         when N_Bit_Packed_Array_Cst =>
            return "bit_packed_array_cst";
         when N_Array_Cst =>
            return "array_cst";
         when N_Packed_Array =>
            return "packed_array";
         when N_Array =>
            return "array";
         when N_Struct_Type =>
            return "struct_type";
         when N_Packed_Struct_Type =>
            return "packed_struct_type";
         when N_Union_Type =>
            return "union_type";
         when N_Packed_Union_Type =>
            return "packed_union_type";
         when N_Queue =>
            return "queue";
         when N_Queue_Cst =>
            return "queue_cst";
         when N_Dynamic_Array_Cst =>
            return "dynamic_array_cst";
         when N_Dynamic_Array =>
            return "dynamic_array";
         when N_Associative_Array =>
            return "associative_array";
         when N_Associative_Array_Cst =>
            return "associative_array_cst";
         when N_Enum_Type =>
            return "enum_type";
         when N_String_Type =>
            return "string_type";
         when N_Chandle_Type =>
            return "chandle_type";
         when N_Event_Type =>
            return "event_type";
         when N_Virtual_Interface =>
            return "virtual_interface";
         when N_Void_Type =>
            return "void_type";
         when N_Error_Type =>
            return "error_type";
         when N_Null_Type =>
            return "null_type";
         when N_Nature =>
            return "nature";
         when N_Class =>
            return "class";
         when N_Instantiated_Class =>
            return "instantiated_class";
         when N_Class_Instance =>
            return "class_instance";
         when N_Generic_Class =>
            return "generic_class";
         when N_Wildcard_Type =>
            return "wildcard_type";
         when N_Compilation_Unit =>
            return "compilation_unit";
         when N_Foreign_Module =>
            return "foreign_module";
         when N_Module =>
            return "module";
         when N_Primitive =>
            return "primitive";
         when N_Interface_Declaration =>
            return "interface_declaration";
         when N_Package =>
            return "package";
         when N_Program_Declaration =>
            return "program_declaration";
         when N_Port =>
            return "port";
         when N_Task =>
            return "task";
         when N_Function =>
            return "function";
         when N_OOB_Task =>
            return "oob_task";
         when N_OOB_Function =>
            return "oob_function";
         when N_Extern_Task =>
            return "extern_task";
         when N_Extern_Function =>
            return "extern_function";
         when N_Import_DPI_Function =>
            return "import_dpi_function";
         when N_Export_DPI_Function =>
            return "export_dpi_function";
         when N_Export_DPI_Task =>
            return "export_dpi_task";
         when N_Clocking =>
            return "clocking";
         when N_Default_Clocking =>
            return "default_clocking";
         when N_Disable_Iff =>
            return "disable_iff";
         when N_Specify =>
            return "specify";
         when N_Property_Declaration =>
            return "property_declaration";
         when N_Input =>
            return "input";
         when N_Inout =>
            return "inout";
         when N_Output =>
            return "output";
         when N_Interface_Port =>
            return "interface_port";
         when N_Modport_Port =>
            return "modport_port";
         when N_Tf_Input =>
            return "tf_input";
         when N_Tf_Inout =>
            return "tf_inout";
         when N_Tf_Output =>
            return "tf_output";
         when N_Tf_Ref =>
            return "tf_ref";
         when N_Tf_Const_Ref =>
            return "tf_const_ref";
         when N_Parameter =>
            return "parameter";
         when N_Type_Parameter =>
            return "type_parameter";
         when N_Localparam =>
            return "localparam";
         when N_Type_Localparam =>
            return "type_localparam";
         when N_Var =>
            return "var";
         when N_Return_Var =>
            return "return_var";
         when N_This_Var =>
            return "this_var";
         when N_Iterator_Argument =>
            return "iterator_argument";
         when N_Wire_Direct =>
            return "wire_direct";
         when N_Wire =>
            return "wire";
         when N_Tri =>
            return "tri";
         when N_Wand =>
            return "wand";
         when N_Triand =>
            return "triand";
         when N_Wor =>
            return "wor";
         when N_Trior =>
            return "trior";
         when N_Tri0 =>
            return "tri0";
         when N_Tri1 =>
            return "tri1";
         when N_Supply0 =>
            return "supply0";
         when N_Supply1 =>
            return "supply1";
         when N_Uwire =>
            return "uwire";
         when N_Trireg =>
            return "trireg";
         when N_Typedef =>
            return "typedef";
         when N_Typedef_Class =>
            return "typedef_class";
         when N_Typedef_Struct =>
            return "typedef_struct";
         when N_Typedef_Forward =>
            return "typedef_forward";
         when N_Predefined_Typedef =>
            return "predefined_typedef";
         when N_Package_Import =>
            return "package_import";
         when N_Genvar =>
            return "genvar";
         when N_Enum_Name =>
            return "enum_name";
         when N_Enum_Range =>
            return "enum_range";
         when N_Foreach_Variable =>
            return "foreach_variable";
         when N_Clock_Var =>
            return "clock_var";
         when N_Modport =>
            return "modport";
         when N_Modport_Input =>
            return "modport_input";
         when N_Modport_Output =>
            return "modport_output";
         when N_Modport_Inout =>
            return "modport_inout";
         when N_Modport_Ref =>
            return "modport_ref";
         when N_Modport_Clocking =>
            return "modport_clocking";
         when N_Modport_Import_Tf =>
            return "modport_import_tf";
         when N_Modport_Export_Tf =>
            return "modport_export_tf";
         when N_Constraint =>
            return "constraint";
         when N_Constraint_Expression =>
            return "constraint_expression";
         when N_Constraint_If =>
            return "constraint_if";
         when N_Constraint_Foreach =>
            return "constraint_foreach";
         when N_Discipline =>
            return "discipline";
         when N_Branch =>
            return "branch";
         when N_Port_Branch =>
            return "port_branch";
         when N_Nature_Attribute =>
            return "nature_attribute";
         when N_Nature_Access =>
            return "nature_access";
         when N_Discipline_Domain =>
            return "discipline_domain";
         when N_Discipline_Potential =>
            return "discipline_potential";
         when N_Discipline_Flow =>
            return "discipline_flow";
         when N_Discipline_Attribute =>
            return "discipline_attribute";
         when N_From_Range =>
            return "from_range";
         when N_Exclude_Range =>
            return "exclude_range";
         when N_Assign =>
            return "assign";
         when N_Decl_Assign =>
            return "decl_assign";
         when N_Always =>
            return "always";
         when N_Always_Comb =>
            return "always_comb";
         when N_Always_Latch =>
            return "always_latch";
         when N_Always_Ff =>
            return "always_ff";
         when N_Initial =>
            return "initial";
         when N_Final =>
            return "final";
         when N_Debug =>
            return "debug";
         when N_Module_Instance =>
            return "module_instance";
         when N_Primitive_Instance =>
            return "primitive_instance";
         when N_Interface_Instance =>
            return "interface_instance";
         when N_Program_Instance =>
            return "program_instance";
         when N_Parameter_Value_Type =>
            return "parameter_value_type";
         when N_Parameter_Value_Expr =>
            return "parameter_value_expr";
         when N_Defparam =>
            return "defparam";
         when N_Generate_Region =>
            return "generate_region";
         when N_Loop_Generate =>
            return "loop_generate";
         when N_If_Generate =>
            return "if_generate";
         when N_Case_Generate =>
            return "case_generate";
         when N_Generate_Block =>
            return "generate_block";
         when N_Array_Generate_Block =>
            return "array_generate_block";
         when N_Indexed_Generate_Block =>
            return "indexed_generate_block";
         when N_Analog =>
            return "analog";
         when N_Assert_Property =>
            return "assert_property";
         when N_Assume_Property =>
            return "assume_property";
         when N_Gate_And =>
            return "gate_and";
         when N_Gate_Nand =>
            return "gate_nand";
         when N_Gate_Or =>
            return "gate_or";
         when N_Gate_Nor =>
            return "gate_nor";
         when N_Gate_Xor =>
            return "gate_xor";
         when N_Gate_Xnor =>
            return "gate_xnor";
         when N_Gate_Buf =>
            return "gate_buf";
         when N_Gate_Not =>
            return "gate_not";
         when N_Gate_Bufif0 =>
            return "gate_bufif0";
         when N_Gate_Bufif1 =>
            return "gate_bufif1";
         when N_Gate_Notif0 =>
            return "gate_notif0";
         when N_Gate_Notif1 =>
            return "gate_notif1";
         when N_Gate_Nmos =>
            return "gate_nmos";
         when N_Gate_Pmos =>
            return "gate_pmos";
         when N_Gate_Rnmos =>
            return "gate_rnmos";
         when N_Gate_Rpmos =>
            return "gate_rpmos";
         when N_Gate_Tran =>
            return "gate_tran";
         when N_Gate_Rtran =>
            return "gate_rtran";
         when N_Gate_Tranif0 =>
            return "gate_tranif0";
         when N_Gate_Tranif1 =>
            return "gate_tranif1";
         when N_Gate_Rtranif0 =>
            return "gate_rtranif0";
         when N_Gate_Rtranif1 =>
            return "gate_rtranif1";
         when N_Gate_Cmos =>
            return "gate_cmos";
         when N_Gate_Rcmos =>
            return "gate_rcmos";
         when N_Gate_Pullup =>
            return "gate_pullup";
         when N_Gate_Pulldown =>
            return "gate_pulldown";
         when N_Default_Skew =>
            return "default_skew";
         when N_Clocking_Skew =>
            return "clocking_skew";
         when N_Control_Terminal =>
            return "control_terminal";
         when N_Input_Terminal =>
            return "input_terminal";
         when N_Inout_Terminal =>
            return "inout_terminal";
         when N_Output_Terminal =>
            return "output_terminal";
         when N_Port_Connection =>
            return "port_connection";
         when N_Wildcard_Connection =>
            return "wildcard_connection";
         when N_Implicit_Connection =>
            return "implicit_connection";
         when N_Default_Connection =>
            return "default_connection";
         when N_Seq_Block =>
            return "seq_block";
         when N_Par_Block =>
            return "par_block";
         when N_If =>
            return "if";
         when N_For =>
            return "for";
         when N_While =>
            return "while";
         when N_Do_While =>
            return "do_while";
         when N_Foreach =>
            return "foreach";
         when N_Repeat =>
            return "repeat";
         when N_Forever =>
            return "forever";
         when N_Wait =>
            return "wait";
         when N_Wait_Fork =>
            return "wait_fork";
         when N_Trigger =>
            return "trigger";
         when N_Disable =>
            return "disable";
         when N_Disable_Fork =>
            return "disable_fork";
         when N_Proc_Assign =>
            return "proc_assign";
         when N_Proc_Deassign =>
            return "proc_deassign";
         when N_Noblk_Assign =>
            return "noblk_assign";
         when N_Blocking_Assign =>
            return "blocking_assign";
         when N_Unpack_Assign =>
            return "unpack_assign";
         when N_Pack_Assign =>
            return "pack_assign";
         when N_Pack_Unpack_Assign =>
            return "pack_unpack_assign";
         when N_Assign_Operator =>
            return "assign_operator";
         when N_Force_Assign =>
            return "force_assign";
         when N_Release =>
            return "release";
         when N_Case =>
            return "case";
         when N_Casex =>
            return "casex";
         when N_Casez =>
            return "casez";
         when N_Case_Item =>
            return "case_item";
         when N_Default_Case_Item =>
            return "default_case_item";
         when N_Subroutine_Call_Stmt =>
            return "subroutine_call_stmt";
         when N_Return_Stmt =>
            return "return_stmt";
         when N_Break_Stmt =>
            return "break_stmt";
         when N_Continue_Stmt =>
            return "continue_stmt";
         when N_Label_Stmt =>
            return "label_stmt";
         when N_Simple_Immediate_Assert =>
            return "simple_immediate_assert";
         when N_Argument =>
            return "argument";
         when N_Contribution =>
            return "contribution";
         when N_Name =>
            return "name";
         when N_This_Name =>
            return "this_name";
         when N_Dotted_Name =>
            return "dotted_name";
         when N_Scoped_Name =>
            return "scoped_name";
         when N_Interface_Item =>
            return "interface_item";
         when N_Modport_Item =>
            return "modport_item";
         when N_Wildcard_Name =>
            return "wildcard_name";
         when N_Property_Name =>
            return "property_name";
         when N_Class_Qualified_Name =>
            return "class_qualified_name";
         when N_Method_Name =>
            return "method_name";
         when N_Member_Name =>
            return "member_name";
         when N_Hierarchical =>
            return "hierarchical";
         when N_Number =>
            return "number";
         when N_Computed_Number =>
            return "computed_number";
         when N_Bignum =>
            return "bignum";
         when N_Unbased_Literal =>
            return "unbased_literal";
         when N_Time_Literal =>
            return "time_literal";
         when N_Step_Literal =>
            return "step_literal";
         when N_Infinity =>
            return "infinity";
         when N_Real_Number =>
            return "real_number";
         when N_Scale_Number =>
            return "scale_number";
         when N_Mintypmax =>
            return "mintypmax";
         when N_Bit_Select =>
            return "bit_select";
         when N_Part_Select =>
            return "part_select";
         when N_Plus_Part_Select =>
            return "plus_part_select";
         when N_Minus_Part_Select =>
            return "minus_part_select";
         when N_Indexed_Name =>
            return "indexed_name";
         when N_String_Index =>
            return "string_index";
         when N_Associative_Index =>
            return "associative_index";
         when N_Slice_Name =>
            return "slice_name";
         when N_Part_Select_Cst =>
            return "part_select_cst";
         when N_Plus_Part_Select_Cst =>
            return "plus_part_select_cst";
         when N_Minus_Part_Select_Cst =>
            return "minus_part_select_cst";
         when N_Slice_Name_Cst =>
            return "slice_name_cst";
         when N_Member_Select =>
            return "member_select";
         when N_String_Literal =>
            return "string_literal";
         when N_Implicit_Event =>
            return "implicit_event";
         when N_New_Call =>
            return "new_call";
         when N_New_Expression =>
            return "new_expression";
         when N_Dynamic_Array_New =>
            return "dynamic_array_new";
         when N_Parenthesis_Expr =>
            return "parenthesis_expr";
         when N_Type_Cast =>
            return "type_cast";
         when N_Size_Cast =>
            return "size_cast";
         when N_Null =>
            return "null";
         when N_This =>
            return "this";
         when N_Super =>
            return "super";
         when N_Default =>
            return "default";
         when N_Aggregate_Literal =>
            return "aggregate_literal";
         when N_Aggregate_Literal_Cst =>
            return "aggregate_literal_cst";
         when N_Aggregate_Element =>
            return "aggregate_element";
         when N_Event_Control =>
            return "event_control";
         when N_Delay_Control =>
            return "delay_control";
         when N_Repeat_Control =>
            return "repeat_control";
         when N_Cycle_Delay =>
            return "cycle_delay";
         when N_Posedge =>
            return "posedge";
         when N_Negedge =>
            return "negedge";
         when N_Or =>
            return "or";
         when N_Delay =>
            return "delay";
         when N_Element =>
            return "element";
         when N_Value_Range =>
            return "value_range";
         when N_Stream_Expression =>
            return "stream_expression";
         when N_Left_Streaming_Expr =>
            return "left_streaming_expr";
         when N_Right_Streaming_Expr =>
            return "right_streaming_expr";
         when N_Left_Streaming_Type =>
            return "left_streaming_type";
         when N_Right_Streaming_Type =>
            return "right_streaming_type";
         when N_Concatenation =>
            return "concatenation";
         when N_Membership =>
            return "membership";
         when N_Replication_Cst =>
            return "replication_cst";
         when N_Cond_Op =>
            return "cond_op";
         when N_Call =>
            return "call";
         when N_Array_Method_Call =>
            return "array_method_call";
         when N_Randomize_Call =>
            return "randomize_call";
         when N_System_Call =>
            return "system_call";
         when N_Bits_Expr =>
            return "bits_expr";
         when N_Bits_Type =>
            return "bits_type";
         when N_Binary_Op =>
            return "binary_op";
         when N_Short_Circuit_Op =>
            return "short_circuit_op";
         when N_Unary_Op =>
            return "unary_op";
         when N_Post_Increment =>
            return "post_increment";
         when N_Pre_Increment =>
            return "pre_increment";
         when N_Post_Decrement =>
            return "post_decrement";
         when N_Pre_Decrement =>
            return "pre_decrement";
         when N_Access_Call =>
            return "access_call";
         when N_Conversion =>
            return "conversion";
         when N_Seq_Repeat =>
            return "seq_repeat";
         when N_Seq_Plus_Repeat =>
            return "seq_plus_repeat";
         when N_Seq_Star_Repeat =>
            return "seq_star_repeat";
         when N_Seq_Star_Concat =>
            return "seq_star_concat";
         when N_Seq_Plus_Concat =>
            return "seq_plus_concat";
         when N_Seq_Const_Concat =>
            return "seq_const_concat";
         when N_Seq_Range_Concat =>
            return "seq_range_concat";
         when N_Seq_Throughout =>
            return "seq_throughout";
         when N_Seq_Parenthesis =>
            return "seq_parenthesis";
         when N_Prop_Not =>
            return "prop_not";
         when N_Prop_Or =>
            return "prop_or";
         when N_Prop_And =>
            return "prop_and";
         when N_Prop_Overlap_Imp =>
            return "prop_overlap_imp";
         when N_Prop_Non_Overlap_Imp =>
            return "prop_non_overlap_imp";
         when N_Prop_Until =>
            return "prop_until";
         when N_Specparam =>
            return "specparam";
         when N_Pulse_Control_Specparam =>
            return "pulse_control_specparam";
         when N_Ifnone =>
            return "ifnone";
         when N_Timing_Check =>
            return "timing_check";
         when N_Par_Path =>
            return "par_path";
         when N_Full_Path =>
            return "full_path";
         when N_Par_Edge_Path =>
            return "par_edge_path";
         when N_Full_Edge_Path =>
            return "full_edge_path";
         when N_Path_Element =>
            return "path_element";
         when N_Path_Delay3 =>
            return "path_delay3";
         when N_Path_Delay6 =>
            return "path_delay6";
         when N_Path_Delay12 =>
            return "path_delay12";
         when N_Member =>
            return "member";
         when N_Packed_Member =>
            return "packed_member";
         when N_Udp_Combinational_Entry =>
            return "udp_combinational_entry";
         when N_Udp_Sequential_Entry =>
            return "udp_sequential_entry";
         when N_Udp_Level_Symbol =>
            return "udp_level_symbol";
         when N_Udp_Change_Symbol =>
            return "udp_change_symbol";
         when N_Attribute =>
            return "attribute";
         when N_Label =>
            return "label";
         when N_Goto =>
            return "goto";
      end case;
   end Get_Nkind_Image;

   function Get_Field_Attribute (F : Fields_Enum) return Field_Attribute is
   begin
      case F is
         when Field_Parent =>
            return Attr_Ref;
         when Field_Call_Scope =>
            return Attr_Ref;
         when Field_Identifier =>
            return Attr_None;
         when Field_C_Identifier =>
            return Attr_None;
         when Field_Ports_Chain =>
            return Attr_Chain;
         when Field_Tf_Ports_Chain =>
            return Attr_Chain;
         when Field_Package_Import_Chain =>
            return Attr_Chain;
         when Field_Parameter_Port_Chain =>
            return Attr_Chain;
         when Field_Parameter =>
            return Attr_Ref;
         when Field_Foreign_Node =>
            return Attr_None;
         when Field_Descriptions =>
            return Attr_Chain;
         when Field_Class_Item_Chain =>
            return Attr_Chain;
         when Field_Package_Item_Chain =>
            return Attr_Chain;
         when Field_Items_Chain =>
            return Attr_Chain;
         when Field_Clocking_Item_Chain =>
            return Attr_Chain;
         when Field_Tf_Item_Declaration_Chain =>
            return Attr_Chain;
         when Field_Block_Item_Declaration_Chain =>
            return Attr_Chain;
         when Field_Generate_Item_Chain =>
            return Attr_Chain;
         when Field_Specify_Item_Chain =>
            return Attr_Chain;
         when Field_Statements_Chain =>
            return Attr_Chain;
         when Field_Modport_Ports_Chain =>
            return Attr_Chain;
         when Field_Chain =>
            return Attr_Chain_Next;
         when Field_Constraint_Block_Chain =>
            return Attr_Chain;
         when Field_Constraint_Set =>
            return Attr_Chain;
         when Field_OOB_Prefix =>
            return Attr_None;
         when Field_Out_Of_Block_Declaration =>
            return Attr_Ref;
         when Field_Generate_Index =>
            return Attr_None;
         when Field_Return_Variable =>
            return Attr_None;
         when Field_Return_Variable_Ref =>
            return Attr_Ref;
         when Field_This_Variable =>
            return Attr_None;
         when Field_Expression =>
            return Attr_None;
         when Field_Reject_Limit =>
            return Attr_None;
         when Field_Error_Limit =>
            return Attr_None;
         when Field_Sequence =>
            return Attr_None;
         when Field_Init_Expression =>
            return Attr_None;
         when Field_Size_Expression =>
            return Attr_None;
         when Field_Override_Stmt =>
            return Attr_Ref;
         when Field_Parameter_Expression =>
            return Attr_Ref;
         when Field_Parameter_Type =>
            return Attr_None;
         when Field_Value_Range =>
            return Attr_Chain;
         when Field_Lsb_Include_Flag =>
            return Attr_None;
         when Field_Msb_Include_Flag =>
            return Attr_None;
         when Field_Range =>
            return Attr_None;
         when Field_Msb =>
            return Attr_None;
         when Field_Lsb =>
            return Attr_None;
         when Field_Msb_Cst =>
            return Attr_None;
         when Field_Lsb_Cst =>
            return Attr_None;
         when Field_Base_Expr =>
            return Attr_None;
         when Field_Width_Expr =>
            return Attr_None;
         when Field_Width_Cst =>
            return Attr_None;
         when Field_Type_Width =>
            return Attr_None;
         when Field_Type_Size =>
            return Attr_None;
         when Field_Stride_Width =>
            return Attr_None;
         when Field_Stride_Size =>
            return Attr_None;
         when Field_Type_Hash =>
            return Attr_None;
         when Field_Maximum_Size_Expr =>
            return Attr_None;
         when Field_Maximum_Size_Cst =>
            return Attr_None;
         when Field_Lvalue =>
            return Attr_None;
         when Field_Name =>
            return Attr_None;
         when Field_Item_Name =>
            return Attr_None;
         when Field_Pattern_Key =>
            return Attr_None;
         when Field_Left =>
            return Attr_None;
         when Field_Right =>
            return Attr_None;
         when Field_Repeat_Expression =>
            return Attr_None;
         when Field_Op_Attributes =>
            return Attr_Chain;
         when Field_Attributes_Chain =>
            return Attr_Chain;
         when Field_Condition =>
            return Attr_None;
         when Field_Cond_True =>
            return Attr_None;
         when Field_Cond_False =>
            return Attr_None;
         when Field_True_Stmt =>
            return Attr_None;
         when Field_False_Stmt =>
            return Attr_None;
         when Field_Pass_Stmt =>
            return Attr_None;
         when Field_Else_Stmt =>
            return Attr_None;
         when Field_Clocking_Event =>
            return Attr_None;
         when Field_Disable_Expression =>
            return Attr_None;
         when Field_Property_Expression =>
            return Attr_None;
         when Field_True_Block =>
            return Attr_None;
         when Field_False_Block =>
            return Attr_None;
         when Field_Statement =>
            return Attr_None;
         when Field_Foreach_Array =>
            return Attr_None;
         when Field_Foreach_Variables =>
            return Attr_None;
         when Field_Control =>
            return Attr_None;
         when Field_Replication =>
            return Attr_None;
         when Field_Replication_Cst =>
            return Attr_None;
         when Field_Expressions =>
            return Attr_Chain;
         when Field_Elements =>
            return Attr_Chain;
         when Field_Slice_Size_Expr =>
            return Attr_None;
         when Field_Slice_Size_Type =>
            return Attr_Maybe_Ref2;
         when Field_Members =>
            return Attr_Chain;
         when Field_Nbr_Members =>
            return Attr_None;
         when Field_Member_Index =>
            return Attr_None;
         when Field_Packed_Member_Offset =>
            return Attr_None;
         when Field_Nature_Items =>
            return Attr_Chain;
         when Field_Discipline_Items =>
            return Attr_Chain;
         when Field_Continuous_Flag =>
            return Attr_None;
         when Field_Potential_Flag =>
            return Attr_None;
         when Field_Nature =>
            return Attr_None;
         when Field_Connections =>
            return Attr_Chain;
         when Field_Gate_Terminals =>
            return Attr_Chain;
         when Field_Parameter_Values =>
            return Attr_Chain;
         when Field_Case_Items =>
            return Attr_Chain;
         when Field_Delay =>
            return Attr_None;
         when Field_Net_Delay =>
            return Attr_None;
         when Field_Gate_Delay =>
            return Attr_None;
         when Field_Assign_Delay =>
            return Attr_None;
         when Field_Rising_Delay =>
            return Attr_None;
         when Field_Falling_Delay =>
            return Attr_None;
         when Field_Highz_Delay =>
            return Attr_None;
         when Field_For_Initialization =>
            return Attr_None;
         when Field_Step_Assign =>
            return Attr_None;
         when Field_Arguments =>
            return Attr_Chain;
         when Field_Iterator_Argument =>
            return Attr_None;
         when Field_Task =>
            return Attr_None;
         when Field_Signed_Flag =>
            return Attr_None;
         when Field_Scope_Flag =>
            return Attr_None;
         when Field_Number_Base =>
            return Attr_None;
         when Field_Number_Hi_Val =>
            return Attr_None;
         when Field_Number_Lo_Val =>
            return Attr_None;
         when Field_Number_Hi_Zx =>
            return Attr_None;
         when Field_Number_Lo_Zx =>
            return Attr_None;
         when Field_Number_Size =>
            return Attr_None;
         when Field_Expr_Origin =>
            return Attr_None;
         when Field_Bignum_Index =>
            return Attr_None;
         when Field_Bignum_Len =>
            return Attr_None;
         when Field_Real_Number =>
            return Attr_None;
         when Field_Time_Unit =>
            return Attr_None;
         when Field_Scale_Factor =>
            return Attr_None;
         when Field_Time_Precision =>
            return Attr_None;
         when Field_Timescale =>
            return Attr_Ref;
         when Field_String_Size =>
            return Attr_None;
         when Field_Data_Type =>
            return Attr_Maybe_Ref;
         when Field_Expr_Type =>
            return Attr_Ref;
         when Field_Param_Type =>
            return Attr_Ref;
         when Field_Element_Data_Type =>
            return Attr_Maybe_Ref;
         when Field_Type_Element_Type =>
            return Attr_Ref;
         when Field_Cast_Data_Type =>
            return Attr_Maybe_Ref;
         when Field_Base_Class_Type =>
            return Attr_Maybe_Ref;
         when Field_Class_Constructor =>
            return Attr_Forward_Ref;
         when Field_Inheritance_Depth =>
            return Attr_None;
         when Field_Enum_Base_Data_Type =>
            return Attr_Maybe_Ref;
         when Field_Enum_Base_Type =>
            return Attr_Ref;
         when Field_Packed_Base_Type =>
            return Attr_Ref;
         when Field_Default_Type =>
            return Attr_Maybe_Ref;
         when Field_Type_Owner =>
            return Attr_None;
         when Field_Type_Owner_2 =>
            return Attr_None;
         when Field_Forward_Type =>
            return Attr_Ref;
         when Field_Enum_Names =>
            return Attr_Chain;
         when Field_Index_Data_Type =>
            return Attr_Maybe_Ref2;
         when Field_Type_Index_Type =>
            return Attr_Ref;
         when Field_Type_Argument =>
            return Attr_Maybe_Ref;
         when Field_Type_Signed =>
            return Attr_None;
         when Field_Subroutine =>
            return Attr_None;
         when Field_Object =>
            return Attr_None;
         when Field_With_Expression =>
            return Attr_None;
         when Field_Drive_Strength =>
            return Attr_None;
         when Field_Net_Drive_Strength =>
            return Attr_None;
         when Field_Charge_Strength =>
            return Attr_None;
         when Field_Module =>
            return Attr_None;
         when Field_Class_Name =>
            return Attr_None;
         when Field_Interface =>
            return Attr_None;
         when Field_Interface_Name =>
            return Attr_None;
         when Field_Instance =>
            return Attr_None;
         when Field_Instance_Ref =>
            return Attr_Ref;
         when Field_Port =>
            return Attr_Ref;
         when Field_Collapse_Flag =>
            return Attr_None;
         when Field_Unary_Op =>
            return Attr_None;
         when Field_Binary_Op =>
            return Attr_None;
         when Field_Conversion_Op =>
            return Attr_None;
         when Field_Declaration =>
            return Attr_Ref;
         when Field_Redeclaration =>
            return Attr_Forward_Ref;
         when Field_This_Declaration =>
            return Attr_Ref;
         when Field_Default_Value =>
            return Attr_None;
         when Field_Instantiated_Flag =>
            return Attr_None;
         when Field_Ansi_Port_Flag =>
            return Attr_None;
         when Field_Event =>
            return Attr_None;
         when Field_Min_Expr =>
            return Attr_None;
         when Field_Typ_Expr =>
            return Attr_None;
         when Field_Max_Expr =>
            return Attr_None;
         when Field_Udp_Port_Declaration_Chain =>
            return Attr_Chain;
         when Field_Udp_Kind =>
            return Attr_None;
         when Field_Udp_Initial =>
            return Attr_None;
         when Field_Udp_Entries_Chain =>
            return Attr_Chain;
         when Field_Input_Chain =>
            return Attr_Chain;
         when Field_Output_Symbol =>
            return Attr_None;
         when Field_Current_State =>
            return Attr_None;
         when Field_Next_State =>
            return Attr_None;
         when Field_Symbol =>
            return Attr_None;
         when Field_From_Symbol =>
            return Attr_None;
         when Field_To_Symbol =>
            return Attr_None;
         when Field_Specify_Input =>
            return Attr_None;
         when Field_Specify_Output =>
            return Attr_None;
         when Field_Path_Delay =>
            return Attr_None;
         when Field_Data_Source =>
            return Attr_None;
         when Field_Polarity =>
            return Attr_None;
         when Field_Delay_Rise =>
            return Attr_None;
         when Field_Delay_Fall =>
            return Attr_None;
         when Field_Delay_Z =>
            return Attr_None;
         when Field_Delay_01 =>
            return Attr_None;
         when Field_Delay_10 =>
            return Attr_None;
         when Field_Delay_0z =>
            return Attr_None;
         when Field_Delay_z1 =>
            return Attr_None;
         when Field_Delay_1z =>
            return Attr_None;
         when Field_Delay_z0 =>
            return Attr_None;
         when Field_Delay_0x =>
            return Attr_None;
         when Field_Delay_x1 =>
            return Attr_None;
         when Field_Delay_1x =>
            return Attr_None;
         when Field_Delay_x0 =>
            return Attr_None;
         when Field_Delay_xz =>
            return Attr_None;
         when Field_Delay_zx =>
            return Attr_None;
         when Field_String_Id =>
            return Attr_None;
         when Field_Label =>
            return Attr_None;
         when Field_Label_Number =>
            return Attr_None;
         when Field_Label_Chain =>
            return Attr_None;
         when Field_Label_Use =>
            return Attr_None;
         when Field_Suspend_Flag =>
            return Attr_None;
         when Field_Same_Case_Flag =>
            return Attr_None;
         when Field_Obj_Id =>
            return Attr_None;
         when Field_Scope_Id =>
            return Attr_None;
         when Field_Process_Id =>
            return Attr_None;
         when Field_Sys_Tf_Id =>
            return Attr_None;
         when Field_Lit_Id =>
            return Attr_None;
         when Field_Generate_Block =>
            return Attr_None;
         when Field_Input_Skew =>
            return Attr_None;
         when Field_Output_Skew =>
            return Attr_None;
         when Field_Delay_Control =>
            return Attr_None;
         when Field_Attribute_Item =>
            return Attr_Forward_Ref;
         when Field_Has_Identifier_List =>
            return Attr_None;
         when Field_Has_Sign =>
            return Attr_None;
         when Field_Connected_Flag =>
            return Attr_None;
         when Field_Complete_Flag =>
            return Attr_None;
         when Field_Implicit_Flag =>
            return Attr_None;
         when Field_Redeclaration_Flag =>
            return Attr_None;
         when Field_Is_Automatic =>
            return Attr_None;
         when Field_Lifetime =>
            return Attr_None;
         when Field_Has_Lifetime =>
            return Attr_None;
         when Field_Has_End_Name =>
            return Attr_None;
         when Field_Call =>
            return Attr_None;
         when Field_Timeunit =>
            return Attr_None;
         when Field_Timeprecision =>
            return Attr_None;
         when Field_Error_Origin =>
            return Attr_None;
         when Field_Has_Void_Cast =>
            return Attr_None;
         when Field_Is_Const =>
            return Attr_None;
         when Field_Has_Var =>
            return Attr_None;
         when Field_Has_Type =>
            return Attr_None;
         when Field_Has_Direction =>
            return Attr_None;
         when Field_Has_Parenthesis =>
            return Attr_None;
         when Field_Has_Argument =>
            return Attr_None;
         when Field_Fully_Analyzed_Flag =>
            return Attr_None;
         when Field_Resolved_Flag =>
            return Attr_None;
         when Field_Mark_Flag =>
            return Attr_None;
         when Field_Is_Constant =>
            return Attr_None;
         when Field_Static_Flag =>
            return Attr_None;
         when Field_Has_Attribute =>
            return Attr_None;
         when Field_Attribute_Full =>
            return Attr_None;
         when Field_Attribute_Parallel =>
            return Attr_None;
         when Field_Other_Attributes =>
            return Attr_None;
         when Field_Pure_Property =>
            return Attr_None;
         when Field_Context_Property =>
            return Attr_None;
         when Field_Has_Extern_Flag =>
            return Attr_None;
         when Field_Virtual_Flag =>
            return Attr_None;
         when Field_Pure_Flag =>
            return Attr_None;
         when Field_Join_Option =>
            return Attr_None;
         when Field_Edge_Identifier =>
            return Attr_None;
         when Field_DPI_Spec =>
            return Attr_None;
         when Field_Visibility =>
            return Attr_None;
         when Field_Class_Visibility =>
            return Attr_None;
         when Field_Has_Visibility =>
            return Attr_None;
         when Field_Violation =>
            return Attr_None;
         when Field_Random_Flag =>
            return Attr_None;
         when Field_Randc_Flag =>
            return Attr_None;
         when Field_Size_Flag =>
            return Attr_None;
         when Field_Type_Analyzed_Flag =>
            return Attr_None;
         when Field_Forward_Typedef_Flag =>
            return Attr_None;
         when Field_Access =>
            return Attr_None;
         when Field_Arg1 =>
            return Attr_None;
         when Field_Arg2 =>
            return Attr_None;
      end case;
   end Get_Field_Attribute;

   function Get_Field_Actual_Attribute (N : Node; F : Fields_Enum)
                                       return Field_Actual_Attribute
   is
      Res : constant Field_Attribute := Get_Field_Attribute (F);
   begin
      case Res is
         when Attr_Maybe_Ref =>
            if Get_Type_Owner (N) then
               return Attr_None;
            else
               return Attr_Ref;
            end if;
         when Attr_Maybe_Ref2 =>
            if Get_Type_Owner_2 (N) then
               return Attr_None;
            else
               return Attr_Ref;
            end if;
         when others =>
            return Res;
      end case;
   end Get_Field_Actual_Attribute;

   Fields_Of_Iir : constant Fields_Array :=
     (
      --  N_Error
      --  N_Error_Expr
      Field_Expr_Type,
      Field_Error_Origin,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Timescale_Directive
      Field_Time_Unit,
      Field_Time_Precision,
      Field_Chain,
      --  N_Timeunits_Declaration
      Field_Time_Unit,
      Field_Time_Precision,
      Field_Chain,
      --  N_Timeunit
      Field_Timeunit,
      Field_Timeprecision,
      Field_Chain,
      --  N_Timeprecision
      Field_Timeprecision,
      Field_Chain,
      --  N_Logic_Type
      Field_Signed_Flag,
      Field_Type_Width,
      Field_Size_Flag,
      --  N_Bit_Type
      Field_Signed_Flag,
      Field_Type_Width,
      Field_Size_Flag,
      --  N_Real_Type
      Field_Signed_Flag,
      Field_Type_Width,
      Field_Size_Flag,
      --  N_Shortreal_Type
      Field_Signed_Flag,
      Field_Type_Width,
      Field_Size_Flag,
      --  N_Log_Packed_Array_Cst
      Field_Msb_Cst,
      Field_Lsb_Cst,
      Field_Type_Element_Type,
      Field_Type_Width,
      Field_Stride_Width,
      Field_Signed_Flag,
      Field_Size_Flag,
      --  N_Bit_Packed_Array_Cst
      Field_Msb_Cst,
      Field_Lsb_Cst,
      Field_Type_Element_Type,
      Field_Type_Width,
      Field_Stride_Width,
      Field_Signed_Flag,
      Field_Size_Flag,
      --  N_Array_Cst
      Field_Msb_Cst,
      Field_Lsb_Cst,
      Field_Type_Element_Type,
      Field_Type_Size,
      Field_Stride_Size,
      Field_Signed_Flag,
      Field_Size_Flag,
      --  N_Packed_Array
      Field_Msb,
      Field_Lsb,
      Field_Element_Data_Type,
      Field_Signed_Flag,
      Field_Expr_Type,
      Field_Has_Sign,
      Field_Type_Owner,
      --  N_Array
      Field_Msb,
      Field_Lsb,
      Field_Element_Data_Type,
      Field_Signed_Flag,
      Field_Expr_Type,
      Field_Has_Sign,
      Field_Type_Owner,
      --  N_Struct_Type
      Field_Members,
      Field_Nbr_Members,
      Field_Type_Size,
      Field_Expr_Type,
      Field_Scope_Id,
      Field_Size_Flag,
      --  N_Packed_Struct_Type
      Field_Members,
      Field_Nbr_Members,
      Field_Signed_Flag,
      Field_Has_Sign,
      Field_Expr_Type,
      Field_Packed_Base_Type,
      Field_Type_Width,
      Field_Size_Flag,
      --  N_Union_Type
      Field_Members,
      Field_Nbr_Members,
      Field_Type_Size,
      Field_Expr_Type,
      Field_Scope_Id,
      Field_Size_Flag,
      --  N_Packed_Union_Type
      Field_Members,
      Field_Nbr_Members,
      Field_Signed_Flag,
      Field_Has_Sign,
      Field_Expr_Type,
      Field_Packed_Base_Type,
      Field_Type_Width,
      Field_Size_Flag,
      --  N_Queue
      Field_Maximum_Size_Expr,
      Field_Expr_Type,
      Field_Element_Data_Type,
      Field_Type_Owner,
      --  N_Queue_Cst
      Field_Maximum_Size_Cst,
      Field_Type_Element_Type,
      Field_Size_Flag,
      --  N_Dynamic_Array_Cst
      Field_Stride_Size,
      Field_Type_Element_Type,
      Field_Size_Flag,
      --  N_Dynamic_Array
      Field_Element_Data_Type,
      Field_Expr_Type,
      Field_Type_Owner,
      --  N_Associative_Array
      Field_Index_Data_Type,
      Field_Element_Data_Type,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Type_Owner_2,
      --  N_Associative_Array_Cst
      Field_Type_Index_Type,
      Field_Type_Element_Type,
      Field_Size_Flag,
      --  N_Enum_Type
      Field_Enum_Base_Data_Type,
      Field_Enum_Names,
      Field_Expr_Type,
      Field_Type_Width,
      Field_Signed_Flag,
      Field_Enum_Base_Type,
      Field_Type_Owner,
      Field_Size_Flag,
      --  N_String_Type
      Field_Size_Flag,
      --  N_Chandle_Type
      Field_Size_Flag,
      --  N_Event_Type
      Field_Size_Flag,
      --  N_Virtual_Interface
      Field_Interface,
      Field_Parameter_Values,
      Field_Instance,
      --  N_Void_Type
      Field_Size_Flag,
      --  N_Error_Type
      Field_Signed_Flag,
      Field_Type_Width,
      Field_Size_Flag,
      --  N_Null_Type
      Field_Size_Flag,
      --  N_Nature
      Field_Identifier,
      Field_Nature_Items,
      Field_Chain,
      Field_Parent,
      --  N_Class
      Field_Identifier,
      Field_Parameter_Port_Chain,
      Field_Base_Class_Type,
      Field_Scope_Id,
      Field_Class_Item_Chain,
      Field_Class_Constructor,
      Field_Inheritance_Depth,
      Field_Parent,
      Field_Chain,
      Field_Lifetime,
      Field_Has_Lifetime,
      Field_Virtual_Flag,
      Field_Type_Owner,
      Field_Has_End_Name,
      Field_Class_Visibility,
      Field_Type_Analyzed_Flag,
      Field_Size_Flag,
      Field_Forward_Typedef_Flag,
      Field_Has_Extern_Flag,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      --  N_Instantiated_Class
      Field_Identifier,
      Field_Parameter_Port_Chain,
      Field_Base_Class_Type,
      Field_Scope_Id,
      Field_Class_Item_Chain,
      Field_Class_Constructor,
      Field_Inheritance_Depth,
      Field_Parent,
      Field_Chain,
      Field_Lifetime,
      Field_Has_Lifetime,
      Field_Virtual_Flag,
      Field_Type_Owner,
      Field_Has_End_Name,
      Field_Class_Visibility,
      Field_Type_Analyzed_Flag,
      Field_Size_Flag,
      Field_Forward_Typedef_Flag,
      Field_Has_Extern_Flag,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      --  N_Class_Instance
      Field_Class_Name,
      Field_Parameter_Values,
      Field_Expr_Type,
      Field_Declaration,
      --  N_Generic_Class
      Field_Identifier,
      Field_Parameter_Port_Chain,
      Field_Base_Class_Type,
      Field_Scope_Id,
      Field_Class_Item_Chain,
      Field_Class_Constructor,
      Field_Inheritance_Depth,
      Field_Parent,
      Field_Chain,
      Field_Lifetime,
      Field_Has_Lifetime,
      Field_Virtual_Flag,
      Field_Type_Owner,
      Field_Has_End_Name,
      Field_Class_Visibility,
      Field_Type_Analyzed_Flag,
      Field_Size_Flag,
      Field_Forward_Typedef_Flag,
      Field_Has_Extern_Flag,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      --  N_Wildcard_Type
      --  N_Compilation_Unit
      Field_Identifier,
      Field_Chain,
      Field_Descriptions,
      Field_Scope_Id,
      --  N_Foreign_Module
      Field_Identifier,
      Field_Parameter_Port_Chain,
      Field_Ports_Chain,
      Field_Items_Chain,
      Field_Scope_Id,
      Field_Parent,
      Field_Chain,
      Field_Foreign_Node,
      Field_Instantiated_Flag,
      Field_Ansi_Port_Flag,
      --  N_Module
      Field_Identifier,
      Field_Package_Import_Chain,
      Field_Parameter_Port_Chain,
      Field_Ports_Chain,
      Field_Items_Chain,
      Field_Scope_Id,
      Field_Parent,
      Field_Chain,
      Field_Attributes_Chain,
      Field_Instantiated_Flag,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      --  N_Primitive
      Field_Identifier,
      Field_Udp_Kind,
      Field_Ports_Chain,
      Field_Udp_Port_Declaration_Chain,
      Field_Udp_Initial,
      Field_Udp_Entries_Chain,
      Field_Chain,
      Field_Parent,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      --  N_Interface_Declaration
      Field_Identifier,
      Field_Package_Import_Chain,
      Field_Parameter_Port_Chain,
      Field_Scope_Id,
      Field_Ports_Chain,
      Field_Items_Chain,
      Field_Parent,
      Field_Chain,
      Field_Instantiated_Flag,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      --  N_Package
      Field_Identifier,
      Field_Package_Item_Chain,
      Field_Parent,
      Field_Chain,
      Field_Lifetime,
      Field_Has_Lifetime,
      Field_Has_End_Name,
      --  N_Program_Declaration
      Field_Identifier,
      Field_Package_Import_Chain,
      Field_Parameter_Port_Chain,
      Field_Ports_Chain,
      Field_Items_Chain,
      Field_Parent,
      Field_Chain,
      Field_Instantiated_Flag,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      Field_Lifetime,
      Field_Has_Lifetime,
      --  N_Port
      Field_Identifier,
      Field_Expression,
      Field_Data_Type,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Connected_Flag,
      Field_Type_Owner,
      --  N_Task
      Field_Identifier,
      Field_Statements_Chain,
      Field_Tf_Ports_Chain,
      Field_Tf_Item_Declaration_Chain,
      Field_This_Variable,
      Field_Scope_Id,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Virtual_Flag,
      Field_Pure_Flag,
      Field_Has_Lifetime,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      Field_Has_Visibility,
      Field_Static_Flag,
      Field_Visibility,
      --  N_Function
      Field_Identifier,
      Field_Data_Type,
      Field_Tf_Ports_Chain,
      Field_Return_Variable,
      Field_This_Variable,
      Field_Tf_Item_Declaration_Chain,
      Field_Statements_Chain,
      Field_Scope_Id,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Virtual_Flag,
      Field_Pure_Flag,
      Field_Has_Lifetime,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      Field_Has_Visibility,
      Field_Visibility,
      Field_Static_Flag,
      --  N_OOB_Task
      Field_Identifier,
      Field_Statements_Chain,
      Field_Tf_Ports_Chain,
      Field_Tf_Item_Declaration_Chain,
      Field_This_Variable,
      Field_OOB_Prefix,
      Field_Scope_Id,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Virtual_Flag,
      Field_Pure_Flag,
      Field_Has_Lifetime,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      Field_Has_Visibility,
      Field_Static_Flag,
      Field_Visibility,
      --  N_OOB_Function
      Field_Identifier,
      Field_Data_Type,
      Field_Tf_Ports_Chain,
      Field_Return_Variable,
      Field_This_Variable,
      Field_Tf_Item_Declaration_Chain,
      Field_Statements_Chain,
      Field_OOB_Prefix,
      Field_Scope_Id,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Virtual_Flag,
      Field_Pure_Flag,
      Field_Has_Lifetime,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      Field_Has_Visibility,
      Field_Visibility,
      Field_Static_Flag,
      --  N_Extern_Task
      Field_Identifier,
      Field_Statements_Chain,
      Field_Tf_Ports_Chain,
      Field_Tf_Item_Declaration_Chain,
      Field_This_Variable,
      Field_Out_Of_Block_Declaration,
      Field_Scope_Id,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Virtual_Flag,
      Field_Pure_Flag,
      Field_Has_Lifetime,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      Field_Has_Visibility,
      Field_Static_Flag,
      Field_Visibility,
      --  N_Extern_Function
      Field_Identifier,
      Field_Data_Type,
      Field_Tf_Ports_Chain,
      Field_Return_Variable,
      Field_This_Variable,
      Field_Tf_Item_Declaration_Chain,
      Field_Statements_Chain,
      Field_Out_Of_Block_Declaration,
      Field_Scope_Id,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Ansi_Port_Flag,
      Field_Has_End_Name,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Virtual_Flag,
      Field_Pure_Flag,
      Field_Has_Lifetime,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      Field_Has_Visibility,
      Field_Visibility,
      Field_Static_Flag,
      --  N_Import_DPI_Function
      Field_Identifier,
      Field_Data_Type,
      Field_C_Identifier,
      Field_Tf_Ports_Chain,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Pure_Property,
      Field_Context_Property,
      Field_Ansi_Port_Flag,
      Field_Lifetime,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      Field_DPI_Spec,
      Field_Has_End_Name,
      --  N_Export_DPI_Function
      Field_Identifier,
      Field_C_Identifier,
      Field_Chain,
      Field_Parent,
      Field_DPI_Spec,
      --  N_Export_DPI_Task
      Field_Identifier,
      Field_C_Identifier,
      Field_Chain,
      Field_Parent,
      Field_DPI_Spec,
      --  N_Clocking
      Field_Identifier,
      Field_Event,
      Field_Clocking_Item_Chain,
      Field_Parent,
      Field_Chain,
      Field_Has_End_Name,
      --  N_Default_Clocking
      Field_Identifier,
      Field_Event,
      Field_Clocking_Item_Chain,
      Field_Parent,
      Field_Chain,
      Field_Has_End_Name,
      --  N_Disable_Iff
      Field_Expression,
      Field_Parent,
      Field_Chain,
      --  N_Specify
      Field_Specify_Item_Chain,
      Field_Parent,
      Field_Chain,
      --  N_Property_Declaration
      Field_Identifier,
      Field_Ports_Chain,
      Field_Items_Chain,
      Field_Parent,
      Field_Chain,
      Field_Clocking_Event,
      Field_Disable_Expression,
      Field_Property_Expression,
      Field_Has_End_Name,
      --  N_Input
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Default_Value,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Inout
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Output
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Interface_Port
      Field_Identifier,
      Field_Data_Type,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_Modport_Port
      Field_Identifier,
      Field_Data_Type,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_Tf_Input
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Default_Value,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Tf_Inout
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Default_Value,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Tf_Output
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Default_Value,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Tf_Ref
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Default_Value,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Tf_Const_Ref
      Field_Identifier,
      Field_Data_Type,
      Field_Redeclaration,
      Field_Default_Value,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Type_Owner,
      Field_Has_Direction,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      --  N_Parameter
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Override_Stmt,
      Field_Parameter_Expression,
      Field_Param_Type,
      Field_Value_Range,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Is_Constant,
      Field_Is_Automatic,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      --  N_Type_Parameter
      Field_Identifier,
      Field_Default_Type,
      Field_Parameter_Type,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Has_Type,
      --  N_Localparam
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Param_Type,
      Field_Value_Range,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Is_Constant,
      Field_Is_Automatic,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      --  N_Type_Localparam
      Field_Identifier,
      Field_Default_Type,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Has_Type,
      --  N_Var
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Is_Const,
      Field_Type_Owner,
      Field_Has_Var,
      Field_Redeclaration_Flag,
      Field_Lifetime,
      Field_Has_Lifetime,
      Field_Is_Automatic,
      Field_Has_Visibility,
      Field_Visibility,
      Field_Random_Flag,
      Field_Randc_Flag,
      Field_Static_Flag,
      --  N_Return_Var
      Field_Identifier,
      Field_Expr_Type,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Is_Automatic,
      --  N_This_Var
      Field_Identifier,
      Field_Expr_Type,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Is_Automatic,
      --  N_Iterator_Argument
      Field_Identifier,
      Field_Expr_Type,
      Field_Obj_Id,
      Field_Parent,
      Field_Type_Owner,
      Field_Is_Automatic,
      --  N_Wire_Direct
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Wire
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Tri
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Wand
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Triand
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Wor
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Trior
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Tri0
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Tri1
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Supply0
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Supply1
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Uwire
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Net_Delay,
      Field_Net_Drive_Strength,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Implicit_Flag,
      Field_Type_Owner,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      --  N_Trireg
      Field_Identifier,
      Field_Data_Type,
      Field_Obj_Id,
      Field_Charge_Strength,
      Field_Net_Delay,
      Field_Chain,
      Field_Parent,
      Field_Has_Identifier_List,
      Field_Type_Owner,
      Field_Is_Automatic,
      --  N_Typedef
      Field_Identifier,
      Field_Data_Type,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Forward_Typedef_Flag,
      Field_Resolved_Flag,
      Field_Mark_Flag,
      --  N_Typedef_Class
      Field_Identifier,
      Field_Forward_Type,
      Field_Chain,
      Field_Parent,
      Field_Forward_Typedef_Flag,
      --  N_Typedef_Struct
      Field_Identifier,
      Field_Forward_Type,
      Field_Chain,
      Field_Parent,
      Field_Forward_Typedef_Flag,
      --  N_Typedef_Forward
      Field_Identifier,
      Field_Forward_Type,
      Field_Chain,
      Field_Parent,
      Field_Forward_Typedef_Flag,
      --  N_Predefined_Typedef
      Field_Identifier,
      Field_Expr_Type,
      Field_Chain,
      Field_Parent,
      --  N_Package_Import
      Field_Item_Name,
      Field_Chain,
      Field_Parent,
      --  N_Genvar
      Field_Identifier,
      Field_Expression,
      Field_Generate_Index,
      Field_Chain,
      Field_Expr_Type,
      Field_Parent,
      Field_Type_Owner,
      --  N_Enum_Name
      Field_Identifier,
      Field_Chain,
      Field_Expr_Type,
      Field_Expression,
      Field_Type_Owner,
      Field_Parent,
      --  N_Enum_Range
      Field_Identifier,
      Field_Chain,
      Field_Msb,
      Field_Lsb,
      Field_Expr_Type,
      Field_Expression,
      Field_Type_Owner,
      --  N_Foreach_Variable
      Field_Identifier,
      Field_Expr_Type,
      Field_Chain,
      Field_Parent,
      Field_Obj_Id,
      Field_Type_Owner,
      Field_Is_Automatic,
      --  N_Clock_Var
      Field_Identifier,
      Field_Input_Skew,
      Field_Output_Skew,
      Field_Expression,
      Field_Parent,
      Field_Chain,
      --  N_Modport
      Field_Identifier,
      Field_Modport_Ports_Chain,
      Field_Parent,
      Field_Chain,
      --  N_Modport_Input
      Field_Identifier,
      Field_Expression,
      Field_Data_Type,
      Field_Parent,
      Field_Chain,
      Field_Type_Owner,
      --  N_Modport_Output
      Field_Identifier,
      Field_Expression,
      Field_Data_Type,
      Field_Parent,
      Field_Chain,
      Field_Type_Owner,
      --  N_Modport_Inout
      Field_Identifier,
      Field_Expression,
      Field_Data_Type,
      Field_Parent,
      Field_Chain,
      Field_Type_Owner,
      --  N_Modport_Ref
      Field_Identifier,
      Field_Expression,
      Field_Data_Type,
      Field_Parent,
      Field_Chain,
      Field_Type_Owner,
      --  N_Modport_Clocking
      Field_Identifier,
      Field_Parent,
      Field_Chain,
      --  N_Modport_Import_Tf
      Field_Identifier,
      Field_Parent,
      Field_Chain,
      --  N_Modport_Export_Tf
      Field_Identifier,
      Field_Parent,
      Field_Chain,
      --  N_Constraint
      Field_Identifier,
      Field_Constraint_Block_Chain,
      Field_Parent,
      Field_Chain,
      --  N_Constraint_Expression
      Field_Expression,
      Field_Parent,
      Field_Chain,
      --  N_Constraint_If
      Field_Condition,
      Field_Cond_True,
      Field_Cond_False,
      Field_Parent,
      Field_Chain,
      --  N_Constraint_Foreach
      Field_Foreach_Array,
      Field_Foreach_Variables,
      Field_Constraint_Set,
      Field_Chain,
      Field_Parent,
      --  N_Discipline
      Field_Identifier,
      Field_Discipline_Items,
      Field_Chain,
      Field_Parent,
      --  N_Branch
      Field_Identifier,
      Field_Arg1,
      Field_Arg2,
      Field_Chain,
      Field_Parent,
      --  N_Port_Branch
      Field_Identifier,
      Field_Port,
      --  N_Nature_Attribute
      Field_Identifier,
      Field_Expression,
      Field_Chain,
      Field_Parent,
      --  N_Nature_Access
      Field_Identifier,
      Field_Chain,
      Field_Parent,
      --  N_Discipline_Domain
      Field_Identifier,
      Field_Continuous_Flag,
      Field_Chain,
      Field_Parent,
      --  N_Discipline_Potential
      Field_Identifier,
      Field_Nature,
      Field_Chain,
      Field_Parent,
      --  N_Discipline_Flow
      Field_Identifier,
      Field_Nature,
      Field_Chain,
      Field_Parent,
      --  N_Discipline_Attribute
      Field_Identifier,
      Field_Expression,
      Field_Potential_Flag,
      Field_Chain,
      Field_Parent,
      --  N_From_Range
      Field_Lsb,
      Field_Msb,
      Field_Lsb_Include_Flag,
      Field_Msb_Include_Flag,
      Field_Chain,
      --  N_Exclude_Range
      Field_Lsb,
      Field_Msb,
      Field_Lsb_Include_Flag,
      Field_Msb_Include_Flag,
      Field_Chain,
      --  N_Assign
      Field_Lvalue,
      Field_Chain,
      Field_Assign_Delay,
      Field_Expression,
      Field_Drive_Strength,
      Field_Parent,
      --  N_Decl_Assign
      Field_Lvalue,
      Field_Chain,
      Field_Assign_Delay,
      Field_Expression,
      Field_Drive_Strength,
      Field_Parent,
      --  N_Always
      Field_Statement,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      --  N_Always_Comb
      Field_Statement,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      --  N_Always_Latch
      Field_Statement,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      --  N_Always_Ff
      Field_Statement,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      --  N_Initial
      Field_Statement,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      --  N_Final
      Field_Statement,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      --  N_Debug
      Field_Statement,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      --  N_Module_Instance
      Field_Module,
      Field_Identifier,
      Field_Range,
      Field_Parameter_Values,
      Field_Connections,
      Field_Instance,
      Field_Chain,
      Field_Parent,
      --  N_Primitive_Instance
      Field_Module,
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Interface_Instance
      Field_Interface_Name,
      Field_Identifier,
      Field_Range,
      Field_Obj_Id,
      Field_Parameter_Values,
      Field_Connections,
      Field_Instance_Ref,
      Field_Chain,
      Field_Parent,
      --  N_Program_Instance
      Field_Module,
      Field_Identifier,
      Field_Range,
      Field_Parameter_Values,
      Field_Connections,
      Field_Instance,
      Field_Chain,
      Field_Parent,
      --  N_Parameter_Value_Type
      Field_Identifier,
      Field_Data_Type,
      Field_Parameter,
      Field_Chain,
      Field_Type_Owner,
      --  N_Parameter_Value_Expr
      Field_Identifier,
      Field_Expression,
      Field_Parameter,
      Field_Chain,
      --  N_Defparam
      Field_Lvalue,
      Field_Chain,
      Field_Expression,
      Field_Parameter,
      Field_Parent,
      --  N_Generate_Region
      Field_Chain,
      Field_Generate_Item_Chain,
      Field_Parent,
      --  N_Loop_Generate
      Field_For_Initialization,
      Field_Condition,
      Field_Step_Assign,
      Field_Generate_Block,
      Field_Chain,
      Field_Parent,
      --  N_If_Generate
      Field_Condition,
      Field_True_Block,
      Field_False_Block,
      Field_Chain,
      Field_Parent,
      --  N_Case_Generate
      Field_Expression,
      Field_Case_Items,
      Field_Chain,
      Field_Parent,
      --  N_Generate_Block
      Field_Identifier,
      Field_Chain,
      Field_Generate_Item_Chain,
      Field_Parent,
      Field_Has_End_Name,
      --  N_Array_Generate_Block
      Field_Identifier,
      Field_Chain,
      Field_Generate_Item_Chain,
      Field_Parent,
      Field_Has_End_Name,
      --  N_Indexed_Generate_Block
      Field_Identifier,
      Field_Generate_Index,
      Field_Generate_Item_Chain,
      Field_Chain,
      Field_Parent,
      --  N_Analog
      Field_Statement,
      Field_Chain,
      Field_Parent,
      --  N_Assert_Property
      Field_Identifier,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      Field_Clocking_Event,
      Field_Disable_Expression,
      Field_Property_Expression,
      Field_Pass_Stmt,
      Field_Else_Stmt,
      --  N_Assume_Property
      Field_Identifier,
      Field_Chain,
      Field_Process_Id,
      Field_Parent,
      Field_Clocking_Event,
      Field_Disable_Expression,
      Field_Property_Expression,
      Field_Pass_Stmt,
      Field_Else_Stmt,
      --  N_Gate_And
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Nand
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Or
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Nor
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Xor
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Xnor
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Buf
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Not
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Bufif0
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Bufif1
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Notif0
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Notif1
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Nmos
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Pmos
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Rnmos
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Rpmos
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Tran
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Rtran
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Tranif0
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Tranif1
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Rtranif0
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Rtranif1
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Cmos
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Rcmos
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Pullup
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Gate_Pulldown
      Field_Identifier,
      Field_Drive_Strength,
      Field_Gate_Delay,
      Field_Range,
      Field_Gate_Terminals,
      Field_Chain,
      Field_Parent,
      --  N_Default_Skew
      Field_Input_Skew,
      Field_Output_Skew,
      Field_Parent,
      Field_Chain,
      --  N_Clocking_Skew
      Field_Delay_Control,
      Field_Edge_Identifier,
      --  N_Control_Terminal
      Field_Expression,
      Field_Chain,
      --  N_Input_Terminal
      Field_Expression,
      Field_Chain,
      --  N_Inout_Terminal
      Field_Expression,
      Field_Chain,
      --  N_Output_Terminal
      Field_Expression,
      Field_Chain,
      --  N_Port_Connection
      Field_Identifier,
      Field_Port,
      Field_Expression,
      Field_Chain,
      Field_Collapse_Flag,
      --  N_Wildcard_Connection
      Field_Chain,
      --  N_Implicit_Connection
      Field_Identifier,
      Field_Port,
      Field_Expression,
      Field_Chain,
      Field_Collapse_Flag,
      --  N_Default_Connection
      Field_Port,
      Field_Chain,
      --  N_Seq_Block
      Field_Identifier,
      Field_Chain,
      Field_Block_Item_Declaration_Chain,
      Field_Statements_Chain,
      Field_Attributes_Chain,
      Field_Scope_Id,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_End_Name,
      --  N_Par_Block
      Field_Identifier,
      Field_Chain,
      Field_Block_Item_Declaration_Chain,
      Field_Statements_Chain,
      Field_Attributes_Chain,
      Field_Scope_Id,
      Field_Parent,
      Field_Join_Option,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_End_Name,
      --  N_If
      Field_Condition,
      Field_True_Stmt,
      Field_False_Stmt,
      Field_Chain,
      Field_Parent,
      Field_Violation,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_For
      Field_For_Initialization,
      Field_Condition,
      Field_Step_Assign,
      Field_Statement,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Scope_Flag,
      --  N_While
      Field_Condition,
      Field_Statement,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_Do_While
      Field_Condition,
      Field_Statement,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_Foreach
      Field_Foreach_Array,
      Field_Foreach_Variables,
      Field_Statement,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_Repeat
      Field_Expression,
      Field_Statement,
      Field_Data_Type,
      Field_Chain,
      Field_Obj_Id,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Type_Owner,
      --  N_Forever
      Field_Statement,
      Field_Chain,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Parent,
      --  N_Wait
      Field_Condition,
      Field_Statement,
      Field_Chain,
      Field_Lifetime,
      Field_Parent,
      Field_Is_Automatic,
      --  N_Wait_Fork
      Field_Chain,
      Field_Parent,
      --  N_Trigger
      Field_Chain,
      Field_Event,
      Field_Parent,
      --  N_Disable
      Field_Statement,
      Field_Chain,
      Field_Parent,
      --  N_Disable_Fork
      Field_Chain,
      Field_Parent,
      --  N_Proc_Assign
      Field_Lvalue,
      Field_Expression,
      Field_Chain,
      Field_Parent,
      --  N_Proc_Deassign
      Field_Lvalue,
      Field_Chain,
      Field_Parent,
      --  N_Noblk_Assign
      Field_Lvalue,
      Field_Control,
      Field_Expression,
      Field_Expr_Type,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      --  N_Blocking_Assign
      Field_Lvalue,
      Field_Control,
      Field_Expression,
      Field_Expr_Type,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      --  N_Unpack_Assign
      Field_Lvalue,
      Field_Control,
      Field_Expression,
      Field_Chain,
      Field_Parent,
      --  N_Pack_Assign
      Field_Lvalue,
      Field_Control,
      Field_Expression,
      Field_Chain,
      Field_Parent,
      --  N_Pack_Unpack_Assign
      Field_Lvalue,
      Field_Control,
      Field_Expression,
      Field_Chain,
      Field_Parent,
      --  N_Assign_Operator
      Field_Lvalue,
      Field_Expression,
      Field_Binary_Op,
      Field_Expr_Type,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      --  N_Force_Assign
      Field_Lvalue,
      Field_Expression,
      Field_Chain,
      Field_Parent,
      --  N_Release
      Field_Lvalue,
      Field_Chain,
      Field_Parent,
      --  N_Case
      Field_Expression,
      Field_Case_Items,
      Field_Chain,
      Field_Parent,
      Field_Violation,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      Field_Attribute_Full,
      Field_Attribute_Parallel,
      Field_Other_Attributes,
      --  N_Casex
      Field_Expression,
      Field_Case_Items,
      Field_Chain,
      Field_Parent,
      Field_Violation,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      Field_Attribute_Full,
      Field_Attribute_Parallel,
      Field_Other_Attributes,
      --  N_Casez
      Field_Expression,
      Field_Case_Items,
      Field_Chain,
      Field_Parent,
      Field_Violation,
      Field_Lifetime,
      Field_Is_Automatic,
      Field_Has_Attribute,
      Field_Attribute_Full,
      Field_Attribute_Parallel,
      Field_Other_Attributes,
      --  N_Case_Item
      Field_Expression,
      Field_Statement,
      Field_Same_Case_Flag,
      Field_Chain,
      Field_Lifetime,
      --  N_Default_Case_Item
      Field_Statement,
      Field_Chain,
      Field_Lifetime,
      --  N_Subroutine_Call_Stmt
      Field_Call,
      Field_Has_Void_Cast,
      Field_Chain,
      Field_Parent,
      --  N_Return_Stmt
      Field_Expression,
      Field_Chain,
      Field_Return_Variable_Ref,
      Field_Parent,
      --  N_Break_Stmt
      Field_Chain,
      Field_Parent,
      --  N_Continue_Stmt
      Field_Chain,
      Field_Parent,
      --  N_Label_Stmt
      Field_Identifier,
      Field_Chain,
      Field_Statements_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_Simple_Immediate_Assert
      Field_Condition,
      Field_Pass_Stmt,
      Field_Else_Stmt,
      Field_Chain,
      Field_Parent,
      Field_Lifetime,
      Field_Is_Automatic,
      --  N_Argument
      Field_Chain,
      Field_Expression,
      Field_Port,
      --  N_Contribution
      Field_Lvalue,
      Field_Chain,
      Field_Expression,
      Field_Parent,
      --  N_Name
      Field_Identifier,
      Field_Expr_Type,
      Field_Declaration,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_This_Name
      Field_Identifier,
      Field_This_Declaration,
      Field_Expr_Type,
      Field_Declaration,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Dotted_Name
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Scoped_Name
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Interface_Item
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Modport_Item
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Wildcard_Name
      Field_Name,
      --  N_Property_Name
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Class_Qualified_Name
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Method_Name
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Member_Name
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Hierarchical
      Field_Name,
      Field_Identifier,
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Number
      Field_Number_Hi_Val,
      Field_Number_Lo_Val,
      Field_Expr_Type,
      Field_Number_Hi_Zx,
      Field_Number_Lo_Zx,
      Field_Number_Size,
      Field_Number_Base,
      Field_Signed_Flag,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Computed_Number
      Field_Number_Hi_Val,
      Field_Number_Lo_Val,
      Field_Expr_Type,
      Field_Number_Hi_Zx,
      Field_Number_Lo_Zx,
      Field_Expr_Origin,
      Field_Number_Base,
      Field_Signed_Flag,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Bignum
      Field_Bignum_Index,
      Field_Bignum_Len,
      Field_Expr_Type,
      Field_Number_Size,
      Field_Number_Base,
      Field_Signed_Flag,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Unbased_Literal
      Field_Number_Lo_Val,
      Field_Expr_Type,
      Field_Number_Lo_Zx,
      Field_Signed_Flag,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Time_Literal
      Field_Real_Number,
      Field_Expr_Type,
      Field_Time_Unit,
      Field_Timescale,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Step_Literal
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Infinity
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Real_Number
      Field_Real_Number,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Scale_Number
      Field_Real_Number,
      Field_Expr_Type,
      Field_Scale_Factor,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Mintypmax
      Field_Min_Expr,
      Field_Typ_Expr,
      Field_Max_Expr,
      Field_Is_Constant,
      --  N_Bit_Select
      Field_Expr_Type,
      Field_Name,
      Field_Expression,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Part_Select
      Field_Msb,
      Field_Lsb,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      --  N_Plus_Part_Select
      Field_Base_Expr,
      Field_Width_Expr,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      --  N_Minus_Part_Select
      Field_Base_Expr,
      Field_Width_Expr,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      --  N_Indexed_Name
      Field_Expr_Type,
      Field_Name,
      Field_Expression,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_String_Index
      Field_Expr_Type,
      Field_Name,
      Field_Expression,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Associative_Index
      Field_Expr_Type,
      Field_Name,
      Field_Expression,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Slice_Name
      Field_Msb,
      Field_Lsb,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      --  N_Part_Select_Cst
      Field_Msb_Cst,
      Field_Lsb_Cst,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Plus_Part_Select_Cst
      Field_Base_Expr,
      Field_Width_Cst,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Minus_Part_Select_Cst
      Field_Base_Expr,
      Field_Width_Cst,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Slice_Name_Cst
      Field_Msb_Cst,
      Field_Lsb_Cst,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Member_Select
      Field_Identifier,
      Field_Expr_Type,
      Field_Name,
      Field_Type_Owner,
      --  N_String_Literal
      Field_String_Id,
      Field_Expr_Type,
      Field_String_Size,
      Field_Lit_Id,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Implicit_Event
      --  N_New_Call
      Field_Expr_Type,
      Field_Arguments,
      Field_Has_Parenthesis,
      Field_Type_Owner,
      --  N_New_Expression
      Field_Expr_Type,
      Field_Expression,
      Field_Type_Owner,
      --  N_Dynamic_Array_New
      Field_Expr_Type,
      Field_Size_Expression,
      Field_Init_Expression,
      Field_Type_Owner,
      --  N_Parenthesis_Expr
      Field_Expr_Type,
      Field_Expression,
      Field_Conversion_Op,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Type_Cast
      Field_Cast_Data_Type,
      Field_Expression,
      Field_Expr_Type,
      Field_Conversion_Op,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Size_Cast
      Field_Expr_Type,
      Field_Size_Expression,
      Field_Expression,
      Field_Conversion_Op,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Null
      Field_Expr_Type,
      Field_Type_Owner,
      --  N_This
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      --  N_Super
      Field_Declaration,
      Field_Expr_Type,
      Field_Type_Owner,
      --  N_Default
      Field_Expr_Type,
      Field_Type_Owner,
      --  N_Aggregate_Literal
      Field_Replication,
      Field_Expr_Type,
      Field_Elements,
      Field_Type_Owner,
      --  N_Aggregate_Literal_Cst
      Field_Replication_Cst,
      Field_Expr_Type,
      Field_Elements,
      Field_Type_Owner,
      --  N_Aggregate_Element
      Field_Chain,
      Field_Expr_Type,
      Field_Expression,
      Field_Pattern_Key,
      Field_Type_Owner,
      --  N_Event_Control
      Field_Expression,
      Field_Statement,
      Field_Chain,
      Field_Parent,
      --  N_Delay_Control
      Field_Expression,
      Field_Statement,
      Field_Chain,
      Field_Timescale,
      Field_Parent,
      --  N_Repeat_Control
      Field_Statement,
      Field_Chain,
      Field_Control,
      Field_Expression,
      Field_Parent,
      --  N_Cycle_Delay
      Field_Expression,
      Field_Statement,
      Field_Chain,
      Field_Parent,
      --  N_Posedge
      Field_Expression,
      --  N_Negedge
      Field_Expression,
      --  N_Or
      Field_Left,
      Field_Right,
      --  N_Delay
      Field_Rising_Delay,
      Field_Falling_Delay,
      Field_Highz_Delay,
      --  N_Element
      Field_Chain,
      Field_Expression,
      --  N_Value_Range
      Field_Msb,
      Field_Lsb,
      Field_Expr_Type,
      Field_Type_Owner,
      --  N_Stream_Expression
      Field_Chain,
      Field_Expression,
      --  N_Left_Streaming_Expr
      Field_Expression,
      Field_Expressions,
      --  N_Right_Streaming_Expr
      Field_Expression,
      Field_Expressions,
      --  N_Left_Streaming_Type
      Field_Slice_Size_Type,
      Field_Expressions,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Type_Owner_2,
      --  N_Right_Streaming_Type
      Field_Slice_Size_Type,
      Field_Expressions,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Type_Owner_2,
      --  N_Concatenation
      Field_Replication,
      Field_Expr_Type,
      Field_Expressions,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Membership
      Field_Expression,
      Field_Expr_Type,
      Field_Expressions,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Replication_Cst
      Field_Replication_Cst,
      Field_Expr_Type,
      Field_Expressions,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Cond_Op
      Field_Expr_Type,
      Field_Cond_True,
      Field_Cond_False,
      Field_Condition,
      Field_Op_Attributes,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Call
      Field_Subroutine,
      Field_Object,
      Field_Expr_Type,
      Field_Arguments,
      Field_Has_Parenthesis,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Array_Method_Call
      Field_Subroutine,
      Field_Object,
      Field_Expr_Type,
      Field_Expression,
      Field_Iterator_Argument,
      Field_With_Expression,
      Field_Has_Parenthesis,
      Field_Type_Owner,
      Field_Has_Argument,
      --  N_Randomize_Call
      Field_Subroutine,
      Field_Object,
      Field_Expr_Type,
      Field_Arguments,
      Field_With_Expression,
      Field_Constraint_Block_Chain,
      Field_Has_Parenthesis,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_System_Call
      Field_Identifier,
      Field_Call_Scope,
      Field_Sys_Tf_Id,
      Field_Arguments,
      Field_Expr_Type,
      Field_Has_Parenthesis,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Bits_Expr
      Field_Expression,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Bits_Type
      Field_Type_Argument,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Binary_Op
      Field_Binary_Op,
      Field_Expr_Type,
      Field_Left,
      Field_Right,
      Field_Op_Attributes,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Short_Circuit_Op
      Field_Binary_Op,
      Field_Expr_Type,
      Field_Left,
      Field_Right,
      Field_Op_Attributes,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Unary_Op
      Field_Unary_Op,
      Field_Expr_Type,
      Field_Expression,
      Field_Op_Attributes,
      Field_Type_Owner,
      Field_Is_Constant,
      --  N_Post_Increment
      Field_Lvalue,
      Field_Chain,
      Field_Expr_Type,
      Field_Parent,
      Field_Type_Owner,
      --  N_Pre_Increment
      Field_Lvalue,
      Field_Chain,
      Field_Expr_Type,
      Field_Parent,
      Field_Type_Owner,
      --  N_Post_Decrement
      Field_Lvalue,
      Field_Chain,
      Field_Expr_Type,
      Field_Parent,
      Field_Type_Owner,
      --  N_Pre_Decrement
      Field_Lvalue,
      Field_Chain,
      Field_Expr_Type,
      Field_Parent,
      Field_Type_Owner,
      --  N_Access_Call
      Field_Access,
      Field_Expr_Type,
      Field_Type_Owner,
      Field_Arg1,
      Field_Arg2,
      Field_Is_Constant,
      --  N_Conversion
      Field_Conversion_Op,
      Field_Expr_Type,
      Field_Expression,
      Field_Type_Owner,
      --  N_Seq_Repeat
      Field_Sequence,
      Field_Msb,
      Field_Lsb,
      --  N_Seq_Plus_Repeat
      Field_Sequence,
      --  N_Seq_Star_Repeat
      Field_Sequence,
      --  N_Seq_Star_Concat
      Field_Left,
      Field_Right,
      --  N_Seq_Plus_Concat
      Field_Left,
      Field_Right,
      --  N_Seq_Const_Concat
      Field_Left,
      Field_Right,
      Field_Repeat_Expression,
      --  N_Seq_Range_Concat
      Field_Left,
      Field_Right,
      Field_Repeat_Expression,
      --  N_Seq_Throughout
      Field_Left,
      Field_Right,
      --  N_Seq_Parenthesis
      Field_Sequence,
      --  N_Prop_Not
      Field_Expression,
      --  N_Prop_Or
      Field_Left,
      Field_Right,
      --  N_Prop_And
      Field_Left,
      Field_Right,
      --  N_Prop_Overlap_Imp
      Field_Left,
      Field_Right,
      --  N_Prop_Non_Overlap_Imp
      Field_Left,
      Field_Right,
      --  N_Prop_Until
      Field_Left,
      Field_Right,
      --  N_Specparam
      Field_Identifier,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Param_Type,
      Field_Value_Range,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Is_Constant,
      Field_Is_Automatic,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      --  N_Pulse_Control_Specparam
      Field_Identifier,
      Field_Data_Type,
      Field_Reject_Limit,
      Field_Error_Limit,
      Field_Obj_Id,
      Field_Param_Type,
      Field_Chain,
      Field_Parent,
      Field_Type_Owner,
      Field_Is_Constant,
      Field_Is_Automatic,
      Field_Fully_Analyzed_Flag,
      Field_Mark_Flag,
      --  N_Ifnone
      Field_True_Stmt,
      Field_Chain,
      Field_Parent,
      --  N_Timing_Check
      Field_Identifier,
      Field_Arguments,
      Field_Chain,
      Field_Parent,
      --  N_Par_Path
      Field_Specify_Input,
      Field_Chain,
      Field_Specify_Output,
      Field_Path_Delay,
      Field_Parent,
      Field_Polarity,
      --  N_Full_Path
      Field_Specify_Input,
      Field_Chain,
      Field_Specify_Output,
      Field_Path_Delay,
      Field_Parent,
      Field_Polarity,
      --  N_Par_Edge_Path
      Field_Specify_Input,
      Field_Chain,
      Field_Specify_Output,
      Field_Path_Delay,
      Field_Data_Source,
      Field_Parent,
      Field_Polarity,
      --  N_Full_Edge_Path
      Field_Specify_Input,
      Field_Chain,
      Field_Specify_Output,
      Field_Path_Delay,
      Field_Data_Source,
      Field_Parent,
      Field_Polarity,
      --  N_Path_Element
      Field_Lvalue,
      Field_Chain,
      --  N_Path_Delay3
      Field_Delay_Rise,
      Field_Delay_Fall,
      Field_Delay_Z,
      --  N_Path_Delay6
      Field_Delay_01,
      Field_Delay_10,
      Field_Delay_0z,
      Field_Delay_z1,
      Field_Delay_1z,
      Field_Delay_z0,
      --  N_Path_Delay12
      Field_Delay_01,
      Field_Delay_10,
      Field_Delay_0z,
      Field_Delay_z1,
      Field_Delay_1z,
      Field_Delay_z0,
      Field_Delay_0x,
      Field_Delay_x1,
      Field_Delay_1x,
      Field_Delay_x0,
      Field_Delay_xz,
      Field_Delay_zx,
      --  N_Member
      Field_Identifier,
      Field_Chain,
      Field_Data_Type,
      Field_Expression,
      Field_Obj_Id,
      Field_Parent,
      Field_Member_Index,
      Field_Has_Identifier_List,
      Field_Type_Owner,
      --  N_Packed_Member
      Field_Identifier,
      Field_Chain,
      Field_Data_Type,
      Field_Expression,
      Field_Packed_Member_Offset,
      Field_Parent,
      Field_Member_Index,
      Field_Has_Identifier_List,
      Field_Type_Owner,
      --  N_Udp_Combinational_Entry
      Field_Input_Chain,
      Field_Output_Symbol,
      Field_Chain,
      Field_Parent,
      --  N_Udp_Sequential_Entry
      Field_Input_Chain,
      Field_Current_State,
      Field_Next_State,
      Field_Chain,
      Field_Parent,
      --  N_Udp_Level_Symbol
      Field_Symbol,
      Field_Chain,
      Field_Parent,
      --  N_Udp_Change_Symbol
      Field_From_Symbol,
      Field_To_Symbol,
      Field_Chain,
      Field_Parent,
      --  N_Attribute
      Field_Identifier,
      Field_Chain,
      Field_Expression,
      Field_Attribute_Item,
      --  N_Label
      Field_Label_Number,
      Field_Chain,
      Field_Label_Chain,
      Field_Label_Use,
      --  N_Goto
      Field_Label,
      Field_Chain,
      Field_Suspend_Flag
     );

   Fields_Of_Iir_Last : constant array (Nkind) of Integer :=
     (
      N_Error => -1,
      N_Error_Expr => 3,
      N_Timescale_Directive => 6,
      N_Timeunits_Declaration => 9,
      N_Timeunit => 12,
      N_Timeprecision => 14,
      N_Logic_Type => 17,
      N_Bit_Type => 20,
      N_Real_Type => 23,
      N_Shortreal_Type => 26,
      N_Log_Packed_Array_Cst => 33,
      N_Bit_Packed_Array_Cst => 40,
      N_Array_Cst => 47,
      N_Packed_Array => 54,
      N_Array => 61,
      N_Struct_Type => 67,
      N_Packed_Struct_Type => 75,
      N_Union_Type => 81,
      N_Packed_Union_Type => 89,
      N_Queue => 93,
      N_Queue_Cst => 96,
      N_Dynamic_Array_Cst => 99,
      N_Dynamic_Array => 102,
      N_Associative_Array => 107,
      N_Associative_Array_Cst => 110,
      N_Enum_Type => 118,
      N_String_Type => 119,
      N_Chandle_Type => 120,
      N_Event_Type => 121,
      N_Virtual_Interface => 124,
      N_Void_Type => 125,
      N_Error_Type => 128,
      N_Null_Type => 129,
      N_Nature => 133,
      N_Class => 154,
      N_Instantiated_Class => 175,
      N_Class_Instance => 179,
      N_Generic_Class => 200,
      N_Wildcard_Type => 200,
      N_Compilation_Unit => 204,
      N_Foreign_Module => 214,
      N_Module => 226,
      N_Primitive => 236,
      N_Interface_Declaration => 247,
      N_Package => 254,
      N_Program_Declaration => 266,
      N_Port => 274,
      N_Task => 294,
      N_Function => 317,
      N_OOB_Task => 338,
      N_OOB_Function => 362,
      N_Extern_Task => 383,
      N_Extern_Function => 407,
      N_Import_DPI_Function => 422,
      N_Export_DPI_Function => 427,
      N_Export_DPI_Task => 432,
      N_Clocking => 438,
      N_Default_Clocking => 444,
      N_Disable_Iff => 447,
      N_Specify => 450,
      N_Property_Declaration => 459,
      N_Input => 474,
      N_Inout => 488,
      N_Output => 502,
      N_Interface_Port => 513,
      N_Modport_Port => 524,
      N_Tf_Input => 539,
      N_Tf_Inout => 554,
      N_Tf_Output => 569,
      N_Tf_Ref => 584,
      N_Tf_Const_Ref => 599,
      N_Parameter => 614,
      N_Type_Parameter => 621,
      N_Localparam => 634,
      N_Type_Localparam => 640,
      N_Var => 659,
      N_Return_Var => 665,
      N_This_Var => 671,
      N_Iterator_Argument => 677,
      N_Wire_Direct => 688,
      N_Wire => 701,
      N_Tri => 714,
      N_Wand => 727,
      N_Triand => 740,
      N_Wor => 753,
      N_Trior => 766,
      N_Tri0 => 779,
      N_Tri1 => 792,
      N_Supply0 => 805,
      N_Supply1 => 818,
      N_Uwire => 831,
      N_Trireg => 841,
      N_Typedef => 849,
      N_Typedef_Class => 854,
      N_Typedef_Struct => 859,
      N_Typedef_Forward => 864,
      N_Predefined_Typedef => 868,
      N_Package_Import => 871,
      N_Genvar => 878,
      N_Enum_Name => 884,
      N_Enum_Range => 891,
      N_Foreach_Variable => 898,
      N_Clock_Var => 904,
      N_Modport => 908,
      N_Modport_Input => 914,
      N_Modport_Output => 920,
      N_Modport_Inout => 926,
      N_Modport_Ref => 932,
      N_Modport_Clocking => 935,
      N_Modport_Import_Tf => 938,
      N_Modport_Export_Tf => 941,
      N_Constraint => 945,
      N_Constraint_Expression => 948,
      N_Constraint_If => 953,
      N_Constraint_Foreach => 958,
      N_Discipline => 962,
      N_Branch => 967,
      N_Port_Branch => 969,
      N_Nature_Attribute => 973,
      N_Nature_Access => 976,
      N_Discipline_Domain => 980,
      N_Discipline_Potential => 984,
      N_Discipline_Flow => 988,
      N_Discipline_Attribute => 993,
      N_From_Range => 998,
      N_Exclude_Range => 1003,
      N_Assign => 1009,
      N_Decl_Assign => 1015,
      N_Always => 1019,
      N_Always_Comb => 1023,
      N_Always_Latch => 1027,
      N_Always_Ff => 1031,
      N_Initial => 1035,
      N_Final => 1039,
      N_Debug => 1043,
      N_Module_Instance => 1051,
      N_Primitive_Instance => 1058,
      N_Interface_Instance => 1067,
      N_Program_Instance => 1075,
      N_Parameter_Value_Type => 1080,
      N_Parameter_Value_Expr => 1084,
      N_Defparam => 1089,
      N_Generate_Region => 1092,
      N_Loop_Generate => 1098,
      N_If_Generate => 1103,
      N_Case_Generate => 1107,
      N_Generate_Block => 1112,
      N_Array_Generate_Block => 1117,
      N_Indexed_Generate_Block => 1122,
      N_Analog => 1125,
      N_Assert_Property => 1134,
      N_Assume_Property => 1143,
      N_Gate_And => 1150,
      N_Gate_Nand => 1157,
      N_Gate_Or => 1164,
      N_Gate_Nor => 1171,
      N_Gate_Xor => 1178,
      N_Gate_Xnor => 1185,
      N_Gate_Buf => 1192,
      N_Gate_Not => 1199,
      N_Gate_Bufif0 => 1206,
      N_Gate_Bufif1 => 1213,
      N_Gate_Notif0 => 1220,
      N_Gate_Notif1 => 1227,
      N_Gate_Nmos => 1234,
      N_Gate_Pmos => 1241,
      N_Gate_Rnmos => 1248,
      N_Gate_Rpmos => 1255,
      N_Gate_Tran => 1262,
      N_Gate_Rtran => 1269,
      N_Gate_Tranif0 => 1276,
      N_Gate_Tranif1 => 1283,
      N_Gate_Rtranif0 => 1290,
      N_Gate_Rtranif1 => 1297,
      N_Gate_Cmos => 1304,
      N_Gate_Rcmos => 1311,
      N_Gate_Pullup => 1318,
      N_Gate_Pulldown => 1325,
      N_Default_Skew => 1329,
      N_Clocking_Skew => 1331,
      N_Control_Terminal => 1333,
      N_Input_Terminal => 1335,
      N_Inout_Terminal => 1337,
      N_Output_Terminal => 1339,
      N_Port_Connection => 1344,
      N_Wildcard_Connection => 1345,
      N_Implicit_Connection => 1350,
      N_Default_Connection => 1352,
      N_Seq_Block => 1362,
      N_Par_Block => 1373,
      N_If => 1381,
      N_For => 1390,
      N_While => 1396,
      N_Do_While => 1402,
      N_Foreach => 1409,
      N_Repeat => 1418,
      N_Forever => 1423,
      N_Wait => 1429,
      N_Wait_Fork => 1431,
      N_Trigger => 1434,
      N_Disable => 1437,
      N_Disable_Fork => 1439,
      N_Proc_Assign => 1443,
      N_Proc_Deassign => 1446,
      N_Noblk_Assign => 1453,
      N_Blocking_Assign => 1460,
      N_Unpack_Assign => 1465,
      N_Pack_Assign => 1470,
      N_Pack_Unpack_Assign => 1475,
      N_Assign_Operator => 1482,
      N_Force_Assign => 1486,
      N_Release => 1489,
      N_Case => 1500,
      N_Casex => 1511,
      N_Casez => 1522,
      N_Case_Item => 1527,
      N_Default_Case_Item => 1530,
      N_Subroutine_Call_Stmt => 1534,
      N_Return_Stmt => 1538,
      N_Break_Stmt => 1540,
      N_Continue_Stmt => 1542,
      N_Label_Stmt => 1548,
      N_Simple_Immediate_Assert => 1555,
      N_Argument => 1558,
      N_Contribution => 1562,
      N_Name => 1567,
      N_This_Name => 1573,
      N_Dotted_Name => 1579,
      N_Scoped_Name => 1585,
      N_Interface_Item => 1591,
      N_Modport_Item => 1597,
      N_Wildcard_Name => 1598,
      N_Property_Name => 1604,
      N_Class_Qualified_Name => 1610,
      N_Method_Name => 1616,
      N_Member_Name => 1622,
      N_Hierarchical => 1628,
      N_Number => 1638,
      N_Computed_Number => 1648,
      N_Bignum => 1656,
      N_Unbased_Literal => 1662,
      N_Time_Literal => 1668,
      N_Step_Literal => 1671,
      N_Infinity => 1674,
      N_Real_Number => 1678,
      N_Scale_Number => 1683,
      N_Mintypmax => 1687,
      N_Bit_Select => 1692,
      N_Part_Select => 1697,
      N_Plus_Part_Select => 1702,
      N_Minus_Part_Select => 1707,
      N_Indexed_Name => 1712,
      N_String_Index => 1717,
      N_Associative_Index => 1722,
      N_Slice_Name => 1727,
      N_Part_Select_Cst => 1733,
      N_Plus_Part_Select_Cst => 1739,
      N_Minus_Part_Select_Cst => 1745,
      N_Slice_Name_Cst => 1751,
      N_Member_Select => 1755,
      N_String_Literal => 1761,
      N_Implicit_Event => 1761,
      N_New_Call => 1765,
      N_New_Expression => 1768,
      N_Dynamic_Array_New => 1772,
      N_Parenthesis_Expr => 1777,
      N_Type_Cast => 1783,
      N_Size_Cast => 1789,
      N_Null => 1791,
      N_This => 1794,
      N_Super => 1797,
      N_Default => 1799,
      N_Aggregate_Literal => 1803,
      N_Aggregate_Literal_Cst => 1807,
      N_Aggregate_Element => 1812,
      N_Event_Control => 1816,
      N_Delay_Control => 1821,
      N_Repeat_Control => 1826,
      N_Cycle_Delay => 1830,
      N_Posedge => 1831,
      N_Negedge => 1832,
      N_Or => 1834,
      N_Delay => 1837,
      N_Element => 1839,
      N_Value_Range => 1843,
      N_Stream_Expression => 1845,
      N_Left_Streaming_Expr => 1847,
      N_Right_Streaming_Expr => 1849,
      N_Left_Streaming_Type => 1854,
      N_Right_Streaming_Type => 1859,
      N_Concatenation => 1864,
      N_Membership => 1869,
      N_Replication_Cst => 1874,
      N_Cond_Op => 1881,
      N_Call => 1888,
      N_Array_Method_Call => 1897,
      N_Randomize_Call => 1906,
      N_System_Call => 1914,
      N_Bits_Expr => 1918,
      N_Bits_Type => 1922,
      N_Binary_Op => 1929,
      N_Short_Circuit_Op => 1936,
      N_Unary_Op => 1942,
      N_Post_Increment => 1947,
      N_Pre_Increment => 1952,
      N_Post_Decrement => 1957,
      N_Pre_Decrement => 1962,
      N_Access_Call => 1968,
      N_Conversion => 1972,
      N_Seq_Repeat => 1975,
      N_Seq_Plus_Repeat => 1976,
      N_Seq_Star_Repeat => 1977,
      N_Seq_Star_Concat => 1979,
      N_Seq_Plus_Concat => 1981,
      N_Seq_Const_Concat => 1984,
      N_Seq_Range_Concat => 1987,
      N_Seq_Throughout => 1989,
      N_Seq_Parenthesis => 1990,
      N_Prop_Not => 1991,
      N_Prop_Or => 1993,
      N_Prop_And => 1995,
      N_Prop_Overlap_Imp => 1997,
      N_Prop_Non_Overlap_Imp => 1999,
      N_Prop_Until => 2001,
      N_Specparam => 2014,
      N_Pulse_Control_Specparam => 2027,
      N_Ifnone => 2030,
      N_Timing_Check => 2034,
      N_Par_Path => 2040,
      N_Full_Path => 2046,
      N_Par_Edge_Path => 2053,
      N_Full_Edge_Path => 2060,
      N_Path_Element => 2062,
      N_Path_Delay3 => 2065,
      N_Path_Delay6 => 2071,
      N_Path_Delay12 => 2083,
      N_Member => 2092,
      N_Packed_Member => 2101,
      N_Udp_Combinational_Entry => 2105,
      N_Udp_Sequential_Entry => 2110,
      N_Udp_Level_Symbol => 2113,
      N_Udp_Change_Symbol => 2117,
      N_Attribute => 2121,
      N_Label => 2125,
      N_Goto => 2128
     );

   function Get_Fields (K : Nkind) return Fields_Array
   is
      First : Natural;
      Last : Integer;
   begin
      if K = Nkind'First then
         First := Fields_Of_Iir'First;
      else
         First := Fields_Of_Iir_Last (Nkind'Pred (K)) + 1;
      end if;
      Last := Fields_Of_Iir_Last (K);
      return Fields_Of_Iir (First .. Last);
   end Get_Fields;

   function Get_Base_Type
      (N : Node; F : Fields_Enum) return Base_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Base_Type);
      case F is
         when Field_Number_Base =>
            return Get_Number_Base (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Base_Type;

   procedure Set_Base_Type
      (N : Node; F : Fields_Enum; V: Base_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Base_Type);
      case F is
         when Field_Number_Base =>
            Set_Number_Base (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Base_Type;

   function Get_Binary_Ops
      (N : Node; F : Fields_Enum) return Binary_Ops is
   begin
      pragma Assert (Fields_Type (F) = Type_Binary_Ops);
      case F is
         when Field_Binary_Op =>
            return Get_Binary_Op (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Binary_Ops;

   procedure Set_Binary_Ops
      (N : Node; F : Fields_Enum; V: Binary_Ops) is
   begin
      pragma Assert (Fields_Type (F) = Type_Binary_Ops);
      case F is
         when Field_Binary_Op =>
            Set_Binary_Op (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Binary_Ops;

   function Get_Bn_Index
      (N : Node; F : Fields_Enum) return Bn_Index is
   begin
      pragma Assert (Fields_Type (F) = Type_Bn_Index);
      case F is
         when Field_Bignum_Index =>
            return Get_Bignum_Index (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Bn_Index;

   procedure Set_Bn_Index
      (N : Node; F : Fields_Enum; V: Bn_Index) is
   begin
      pragma Assert (Fields_Type (F) = Type_Bn_Index);
      case F is
         when Field_Bignum_Index =>
            Set_Bignum_Index (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Bn_Index;

   function Get_Boolean
      (N : Node; F : Fields_Enum) return Boolean is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Lsb_Include_Flag =>
            return Get_Lsb_Include_Flag (N);
         when Field_Msb_Include_Flag =>
            return Get_Msb_Include_Flag (N);
         when Field_Continuous_Flag =>
            return Get_Continuous_Flag (N);
         when Field_Potential_Flag =>
            return Get_Potential_Flag (N);
         when Field_Signed_Flag =>
            return Get_Signed_Flag (N);
         when Field_Scope_Flag =>
            return Get_Scope_Flag (N);
         when Field_Type_Owner =>
            return Get_Type_Owner (N);
         when Field_Type_Owner_2 =>
            return Get_Type_Owner_2 (N);
         when Field_Type_Signed =>
            return Get_Type_Signed (N);
         when Field_Collapse_Flag =>
            return Get_Collapse_Flag (N);
         when Field_Instantiated_Flag =>
            return Get_Instantiated_Flag (N);
         when Field_Ansi_Port_Flag =>
            return Get_Ansi_Port_Flag (N);
         when Field_Suspend_Flag =>
            return Get_Suspend_Flag (N);
         when Field_Same_Case_Flag =>
            return Get_Same_Case_Flag (N);
         when Field_Has_Identifier_List =>
            return Get_Has_Identifier_List (N);
         when Field_Has_Sign =>
            return Get_Has_Sign (N);
         when Field_Connected_Flag =>
            return Get_Connected_Flag (N);
         when Field_Complete_Flag =>
            return Get_Complete_Flag (N);
         when Field_Implicit_Flag =>
            return Get_Implicit_Flag (N);
         when Field_Redeclaration_Flag =>
            return Get_Redeclaration_Flag (N);
         when Field_Is_Automatic =>
            return Get_Is_Automatic (N);
         when Field_Has_Lifetime =>
            return Get_Has_Lifetime (N);
         when Field_Has_End_Name =>
            return Get_Has_End_Name (N);
         when Field_Has_Void_Cast =>
            return Get_Has_Void_Cast (N);
         when Field_Is_Const =>
            return Get_Is_Const (N);
         when Field_Has_Var =>
            return Get_Has_Var (N);
         when Field_Has_Type =>
            return Get_Has_Type (N);
         when Field_Has_Direction =>
            return Get_Has_Direction (N);
         when Field_Has_Parenthesis =>
            return Get_Has_Parenthesis (N);
         when Field_Has_Argument =>
            return Get_Has_Argument (N);
         when Field_Fully_Analyzed_Flag =>
            return Get_Fully_Analyzed_Flag (N);
         when Field_Resolved_Flag =>
            return Get_Resolved_Flag (N);
         when Field_Mark_Flag =>
            return Get_Mark_Flag (N);
         when Field_Is_Constant =>
            return Get_Is_Constant (N);
         when Field_Static_Flag =>
            return Get_Static_Flag (N);
         when Field_Has_Attribute =>
            return Get_Has_Attribute (N);
         when Field_Attribute_Full =>
            return Get_Attribute_Full (N);
         when Field_Attribute_Parallel =>
            return Get_Attribute_Parallel (N);
         when Field_Other_Attributes =>
            return Get_Other_Attributes (N);
         when Field_Pure_Property =>
            return Get_Pure_Property (N);
         when Field_Context_Property =>
            return Get_Context_Property (N);
         when Field_Has_Extern_Flag =>
            return Get_Has_Extern_Flag (N);
         when Field_Virtual_Flag =>
            return Get_Virtual_Flag (N);
         when Field_Pure_Flag =>
            return Get_Pure_Flag (N);
         when Field_Has_Visibility =>
            return Get_Has_Visibility (N);
         when Field_Random_Flag =>
            return Get_Random_Flag (N);
         when Field_Randc_Flag =>
            return Get_Randc_Flag (N);
         when Field_Size_Flag =>
            return Get_Size_Flag (N);
         when Field_Type_Analyzed_Flag =>
            return Get_Type_Analyzed_Flag (N);
         when Field_Forward_Typedef_Flag =>
            return Get_Forward_Typedef_Flag (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Boolean;

   procedure Set_Boolean
      (N : Node; F : Fields_Enum; V: Boolean) is
   begin
      pragma Assert (Fields_Type (F) = Type_Boolean);
      case F is
         when Field_Lsb_Include_Flag =>
            Set_Lsb_Include_Flag (N, V);
         when Field_Msb_Include_Flag =>
            Set_Msb_Include_Flag (N, V);
         when Field_Continuous_Flag =>
            Set_Continuous_Flag (N, V);
         when Field_Potential_Flag =>
            Set_Potential_Flag (N, V);
         when Field_Signed_Flag =>
            Set_Signed_Flag (N, V);
         when Field_Scope_Flag =>
            Set_Scope_Flag (N, V);
         when Field_Type_Owner =>
            Set_Type_Owner (N, V);
         when Field_Type_Owner_2 =>
            Set_Type_Owner_2 (N, V);
         when Field_Type_Signed =>
            Set_Type_Signed (N, V);
         when Field_Collapse_Flag =>
            Set_Collapse_Flag (N, V);
         when Field_Instantiated_Flag =>
            Set_Instantiated_Flag (N, V);
         when Field_Ansi_Port_Flag =>
            Set_Ansi_Port_Flag (N, V);
         when Field_Suspend_Flag =>
            Set_Suspend_Flag (N, V);
         when Field_Same_Case_Flag =>
            Set_Same_Case_Flag (N, V);
         when Field_Has_Identifier_List =>
            Set_Has_Identifier_List (N, V);
         when Field_Has_Sign =>
            Set_Has_Sign (N, V);
         when Field_Connected_Flag =>
            Set_Connected_Flag (N, V);
         when Field_Complete_Flag =>
            Set_Complete_Flag (N, V);
         when Field_Implicit_Flag =>
            Set_Implicit_Flag (N, V);
         when Field_Redeclaration_Flag =>
            Set_Redeclaration_Flag (N, V);
         when Field_Is_Automatic =>
            Set_Is_Automatic (N, V);
         when Field_Has_Lifetime =>
            Set_Has_Lifetime (N, V);
         when Field_Has_End_Name =>
            Set_Has_End_Name (N, V);
         when Field_Has_Void_Cast =>
            Set_Has_Void_Cast (N, V);
         when Field_Is_Const =>
            Set_Is_Const (N, V);
         when Field_Has_Var =>
            Set_Has_Var (N, V);
         when Field_Has_Type =>
            Set_Has_Type (N, V);
         when Field_Has_Direction =>
            Set_Has_Direction (N, V);
         when Field_Has_Parenthesis =>
            Set_Has_Parenthesis (N, V);
         when Field_Has_Argument =>
            Set_Has_Argument (N, V);
         when Field_Fully_Analyzed_Flag =>
            Set_Fully_Analyzed_Flag (N, V);
         when Field_Resolved_Flag =>
            Set_Resolved_Flag (N, V);
         when Field_Mark_Flag =>
            Set_Mark_Flag (N, V);
         when Field_Is_Constant =>
            Set_Is_Constant (N, V);
         when Field_Static_Flag =>
            Set_Static_Flag (N, V);
         when Field_Has_Attribute =>
            Set_Has_Attribute (N, V);
         when Field_Attribute_Full =>
            Set_Attribute_Full (N, V);
         when Field_Attribute_Parallel =>
            Set_Attribute_Parallel (N, V);
         when Field_Other_Attributes =>
            Set_Other_Attributes (N, V);
         when Field_Pure_Property =>
            Set_Pure_Property (N, V);
         when Field_Context_Property =>
            Set_Context_Property (N, V);
         when Field_Has_Extern_Flag =>
            Set_Has_Extern_Flag (N, V);
         when Field_Virtual_Flag =>
            Set_Virtual_Flag (N, V);
         when Field_Pure_Flag =>
            Set_Pure_Flag (N, V);
         when Field_Has_Visibility =>
            Set_Has_Visibility (N, V);
         when Field_Random_Flag =>
            Set_Random_Flag (N, V);
         when Field_Randc_Flag =>
            Set_Randc_Flag (N, V);
         when Field_Size_Flag =>
            Set_Size_Flag (N, V);
         when Field_Type_Analyzed_Flag =>
            Set_Type_Analyzed_Flag (N, V);
         when Field_Forward_Typedef_Flag =>
            Set_Forward_Typedef_Flag (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Boolean;

   function Get_Conv_Ops
      (N : Node; F : Fields_Enum) return Conv_Ops is
   begin
      pragma Assert (Fields_Type (F) = Type_Conv_Ops);
      case F is
         when Field_Conversion_Op =>
            return Get_Conversion_Op (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Conv_Ops;

   procedure Set_Conv_Ops
      (N : Node; F : Fields_Enum; V: Conv_Ops) is
   begin
      pragma Assert (Fields_Type (F) = Type_Conv_Ops);
      case F is
         when Field_Conversion_Op =>
            Set_Conversion_Op (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Conv_Ops;

   function Get_DPI_Spec_Type
      (N : Node; F : Fields_Enum) return DPI_Spec_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_DPI_Spec_Type);
      case F is
         when Field_DPI_Spec =>
            return Get_DPI_Spec (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_DPI_Spec_Type;

   procedure Set_DPI_Spec_Type
      (N : Node; F : Fields_Enum; V: DPI_Spec_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_DPI_Spec_Type);
      case F is
         when Field_DPI_Spec =>
            Set_DPI_Spec (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_DPI_Spec_Type;

   function Get_Edge_Type
      (N : Node; F : Fields_Enum) return Edge_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Edge_Type);
      case F is
         when Field_Edge_Identifier =>
            return Get_Edge_Identifier (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Edge_Type;

   procedure Set_Edge_Type
      (N : Node; F : Fields_Enum; V: Edge_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Edge_Type);
      case F is
         when Field_Edge_Identifier =>
            Set_Edge_Identifier (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Edge_Type;

   function Get_Fp64
      (N : Node; F : Fields_Enum) return Fp64 is
   begin
      pragma Assert (Fields_Type (F) = Type_Fp64);
      case F is
         when Field_Real_Number =>
            return Get_Real_Number (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Fp64;

   procedure Set_Fp64
      (N : Node; F : Fields_Enum; V: Fp64) is
   begin
      pragma Assert (Fields_Type (F) = Type_Fp64);
      case F is
         when Field_Real_Number =>
            Set_Real_Number (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Fp64;

   function Get_Int32
      (N : Node; F : Fields_Enum) return Int32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_Foreign_Node =>
            return Get_Foreign_Node (N);
         when Field_Generate_Index =>
            return Get_Generate_Index (N);
         when Field_Msb_Cst =>
            return Get_Msb_Cst (N);
         when Field_Lsb_Cst =>
            return Get_Lsb_Cst (N);
         when Field_Width_Cst =>
            return Get_Width_Cst (N);
         when Field_Maximum_Size_Cst =>
            return Get_Maximum_Size_Cst (N);
         when Field_Replication_Cst =>
            return Get_Replication_Cst (N);
         when Field_Nbr_Members =>
            return Get_Nbr_Members (N);
         when Field_Member_Index =>
            return Get_Member_Index (N);
         when Field_Time_Unit =>
            return Get_Time_Unit (N);
         when Field_Scale_Factor =>
            return Get_Scale_Factor (N);
         when Field_Time_Precision =>
            return Get_Time_Precision (N);
         when Field_Inheritance_Depth =>
            return Get_Inheritance_Depth (N);
         when Field_Drive_Strength =>
            return Get_Drive_Strength (N);
         when Field_Net_Drive_Strength =>
            return Get_Net_Drive_Strength (N);
         when Field_Charge_Strength =>
            return Get_Charge_Strength (N);
         when Field_Label_Number =>
            return Get_Label_Number (N);
         when Field_Label_Use =>
            return Get_Label_Use (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Int32;

   procedure Set_Int32
      (N : Node; F : Fields_Enum; V: Int32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Int32);
      case F is
         when Field_Foreign_Node =>
            Set_Foreign_Node (N, V);
         when Field_Generate_Index =>
            Set_Generate_Index (N, V);
         when Field_Msb_Cst =>
            Set_Msb_Cst (N, V);
         when Field_Lsb_Cst =>
            Set_Lsb_Cst (N, V);
         when Field_Width_Cst =>
            Set_Width_Cst (N, V);
         when Field_Maximum_Size_Cst =>
            Set_Maximum_Size_Cst (N, V);
         when Field_Replication_Cst =>
            Set_Replication_Cst (N, V);
         when Field_Nbr_Members =>
            Set_Nbr_Members (N, V);
         when Field_Member_Index =>
            Set_Member_Index (N, V);
         when Field_Time_Unit =>
            Set_Time_Unit (N, V);
         when Field_Scale_Factor =>
            Set_Scale_Factor (N, V);
         when Field_Time_Precision =>
            Set_Time_Precision (N, V);
         when Field_Inheritance_Depth =>
            Set_Inheritance_Depth (N, V);
         when Field_Drive_Strength =>
            Set_Drive_Strength (N, V);
         when Field_Net_Drive_Strength =>
            Set_Net_Drive_Strength (N, V);
         when Field_Charge_Strength =>
            Set_Charge_Strength (N, V);
         when Field_Label_Number =>
            Set_Label_Number (N, V);
         when Field_Label_Use =>
            Set_Label_Use (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Int32;

   function Get_Join_Type
      (N : Node; F : Fields_Enum) return Join_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Join_Type);
      case F is
         when Field_Join_Option =>
            return Get_Join_Option (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Join_Type;

   procedure Set_Join_Type
      (N : Node; F : Fields_Enum; V: Join_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Join_Type);
      case F is
         when Field_Join_Option =>
            Set_Join_Option (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Join_Type;

   function Get_Lifetime_Type
      (N : Node; F : Fields_Enum) return Lifetime_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Lifetime_Type);
      case F is
         when Field_Lifetime =>
            return Get_Lifetime (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Lifetime_Type;

   procedure Set_Lifetime_Type
      (N : Node; F : Fields_Enum; V: Lifetime_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Lifetime_Type);
      case F is
         when Field_Lifetime =>
            Set_Lifetime (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Lifetime_Type;

   function Get_Lit_Id
      (N : Node; F : Fields_Enum) return Lit_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Lit_Id);
      case F is
         when Field_Lit_Id =>
            return Get_Lit_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Lit_Id;

   procedure Set_Lit_Id
      (N : Node; F : Fields_Enum; V: Lit_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Lit_Id);
      case F is
         when Field_Lit_Id =>
            Set_Lit_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Lit_Id;

   function Get_Name_Id
      (N : Node; F : Fields_Enum) return Name_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Identifier =>
            return Get_Identifier (N);
         when Field_C_Identifier =>
            return Get_C_Identifier (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Name_Id;

   procedure Set_Name_Id
      (N : Node; F : Fields_Enum; V: Name_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Name_Id);
      case F is
         when Field_Identifier =>
            Set_Identifier (N, V);
         when Field_C_Identifier =>
            Set_C_Identifier (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Name_Id;

   function Get_Node
      (N : Node; F : Fields_Enum) return Node is
   begin
      pragma Assert (Fields_Type (F) = Type_Node);
      case F is
         when Field_Parent =>
            return Get_Parent (N);
         when Field_Call_Scope =>
            return Get_Call_Scope (N);
         when Field_Ports_Chain =>
            return Get_Ports_Chain (N);
         when Field_Tf_Ports_Chain =>
            return Get_Tf_Ports_Chain (N);
         when Field_Package_Import_Chain =>
            return Get_Package_Import_Chain (N);
         when Field_Parameter_Port_Chain =>
            return Get_Parameter_Port_Chain (N);
         when Field_Parameter =>
            return Get_Parameter (N);
         when Field_Descriptions =>
            return Get_Descriptions (N);
         when Field_Class_Item_Chain =>
            return Get_Class_Item_Chain (N);
         when Field_Package_Item_Chain =>
            return Get_Package_Item_Chain (N);
         when Field_Items_Chain =>
            return Get_Items_Chain (N);
         when Field_Clocking_Item_Chain =>
            return Get_Clocking_Item_Chain (N);
         when Field_Tf_Item_Declaration_Chain =>
            return Get_Tf_Item_Declaration_Chain (N);
         when Field_Block_Item_Declaration_Chain =>
            return Get_Block_Item_Declaration_Chain (N);
         when Field_Generate_Item_Chain =>
            return Get_Generate_Item_Chain (N);
         when Field_Specify_Item_Chain =>
            return Get_Specify_Item_Chain (N);
         when Field_Statements_Chain =>
            return Get_Statements_Chain (N);
         when Field_Modport_Ports_Chain =>
            return Get_Modport_Ports_Chain (N);
         when Field_Chain =>
            return Get_Chain (N);
         when Field_Constraint_Block_Chain =>
            return Get_Constraint_Block_Chain (N);
         when Field_Constraint_Set =>
            return Get_Constraint_Set (N);
         when Field_OOB_Prefix =>
            return Get_OOB_Prefix (N);
         when Field_Out_Of_Block_Declaration =>
            return Get_Out_Of_Block_Declaration (N);
         when Field_Return_Variable =>
            return Get_Return_Variable (N);
         when Field_Return_Variable_Ref =>
            return Get_Return_Variable_Ref (N);
         when Field_This_Variable =>
            return Get_This_Variable (N);
         when Field_Expression =>
            return Get_Expression (N);
         when Field_Reject_Limit =>
            return Get_Reject_Limit (N);
         when Field_Error_Limit =>
            return Get_Error_Limit (N);
         when Field_Sequence =>
            return Get_Sequence (N);
         when Field_Init_Expression =>
            return Get_Init_Expression (N);
         when Field_Size_Expression =>
            return Get_Size_Expression (N);
         when Field_Override_Stmt =>
            return Get_Override_Stmt (N);
         when Field_Parameter_Expression =>
            return Get_Parameter_Expression (N);
         when Field_Parameter_Type =>
            return Get_Parameter_Type (N);
         when Field_Value_Range =>
            return Get_Value_Range (N);
         when Field_Range =>
            return Get_Range (N);
         when Field_Msb =>
            return Get_Msb (N);
         when Field_Lsb =>
            return Get_Lsb (N);
         when Field_Base_Expr =>
            return Get_Base_Expr (N);
         when Field_Width_Expr =>
            return Get_Width_Expr (N);
         when Field_Maximum_Size_Expr =>
            return Get_Maximum_Size_Expr (N);
         when Field_Lvalue =>
            return Get_Lvalue (N);
         when Field_Name =>
            return Get_Name (N);
         when Field_Item_Name =>
            return Get_Item_Name (N);
         when Field_Pattern_Key =>
            return Get_Pattern_Key (N);
         when Field_Left =>
            return Get_Left (N);
         when Field_Right =>
            return Get_Right (N);
         when Field_Repeat_Expression =>
            return Get_Repeat_Expression (N);
         when Field_Op_Attributes =>
            return Get_Op_Attributes (N);
         when Field_Attributes_Chain =>
            return Get_Attributes_Chain (N);
         when Field_Condition =>
            return Get_Condition (N);
         when Field_Cond_True =>
            return Get_Cond_True (N);
         when Field_Cond_False =>
            return Get_Cond_False (N);
         when Field_True_Stmt =>
            return Get_True_Stmt (N);
         when Field_False_Stmt =>
            return Get_False_Stmt (N);
         when Field_Pass_Stmt =>
            return Get_Pass_Stmt (N);
         when Field_Else_Stmt =>
            return Get_Else_Stmt (N);
         when Field_Clocking_Event =>
            return Get_Clocking_Event (N);
         when Field_Disable_Expression =>
            return Get_Disable_Expression (N);
         when Field_Property_Expression =>
            return Get_Property_Expression (N);
         when Field_True_Block =>
            return Get_True_Block (N);
         when Field_False_Block =>
            return Get_False_Block (N);
         when Field_Statement =>
            return Get_Statement (N);
         when Field_Foreach_Array =>
            return Get_Foreach_Array (N);
         when Field_Foreach_Variables =>
            return Get_Foreach_Variables (N);
         when Field_Control =>
            return Get_Control (N);
         when Field_Replication =>
            return Get_Replication (N);
         when Field_Expressions =>
            return Get_Expressions (N);
         when Field_Elements =>
            return Get_Elements (N);
         when Field_Slice_Size_Expr =>
            return Get_Slice_Size_Expr (N);
         when Field_Slice_Size_Type =>
            return Get_Slice_Size_Type (N);
         when Field_Members =>
            return Get_Members (N);
         when Field_Nature_Items =>
            return Get_Nature_Items (N);
         when Field_Discipline_Items =>
            return Get_Discipline_Items (N);
         when Field_Nature =>
            return Get_Nature (N);
         when Field_Connections =>
            return Get_Connections (N);
         when Field_Gate_Terminals =>
            return Get_Gate_Terminals (N);
         when Field_Parameter_Values =>
            return Get_Parameter_Values (N);
         when Field_Case_Items =>
            return Get_Case_Items (N);
         when Field_Delay =>
            return Get_Delay (N);
         when Field_Net_Delay =>
            return Get_Net_Delay (N);
         when Field_Gate_Delay =>
            return Get_Gate_Delay (N);
         when Field_Assign_Delay =>
            return Get_Assign_Delay (N);
         when Field_Rising_Delay =>
            return Get_Rising_Delay (N);
         when Field_Falling_Delay =>
            return Get_Falling_Delay (N);
         when Field_Highz_Delay =>
            return Get_Highz_Delay (N);
         when Field_For_Initialization =>
            return Get_For_Initialization (N);
         when Field_Step_Assign =>
            return Get_Step_Assign (N);
         when Field_Arguments =>
            return Get_Arguments (N);
         when Field_Iterator_Argument =>
            return Get_Iterator_Argument (N);
         when Field_Task =>
            return Get_Task (N);
         when Field_Expr_Origin =>
            return Get_Expr_Origin (N);
         when Field_Timescale =>
            return Get_Timescale (N);
         when Field_Data_Type =>
            return Get_Data_Type (N);
         when Field_Expr_Type =>
            return Get_Expr_Type (N);
         when Field_Param_Type =>
            return Get_Param_Type (N);
         when Field_Element_Data_Type =>
            return Get_Element_Data_Type (N);
         when Field_Type_Element_Type =>
            return Get_Type_Element_Type (N);
         when Field_Cast_Data_Type =>
            return Get_Cast_Data_Type (N);
         when Field_Base_Class_Type =>
            return Get_Base_Class_Type (N);
         when Field_Class_Constructor =>
            return Get_Class_Constructor (N);
         when Field_Enum_Base_Data_Type =>
            return Get_Enum_Base_Data_Type (N);
         when Field_Enum_Base_Type =>
            return Get_Enum_Base_Type (N);
         when Field_Packed_Base_Type =>
            return Get_Packed_Base_Type (N);
         when Field_Default_Type =>
            return Get_Default_Type (N);
         when Field_Forward_Type =>
            return Get_Forward_Type (N);
         when Field_Enum_Names =>
            return Get_Enum_Names (N);
         when Field_Index_Data_Type =>
            return Get_Index_Data_Type (N);
         when Field_Type_Index_Type =>
            return Get_Type_Index_Type (N);
         when Field_Type_Argument =>
            return Get_Type_Argument (N);
         when Field_Subroutine =>
            return Get_Subroutine (N);
         when Field_Object =>
            return Get_Object (N);
         when Field_With_Expression =>
            return Get_With_Expression (N);
         when Field_Module =>
            return Get_Module (N);
         when Field_Class_Name =>
            return Get_Class_Name (N);
         when Field_Interface =>
            return Get_Interface (N);
         when Field_Interface_Name =>
            return Get_Interface_Name (N);
         when Field_Instance =>
            return Get_Instance (N);
         when Field_Instance_Ref =>
            return Get_Instance_Ref (N);
         when Field_Port =>
            return Get_Port (N);
         when Field_Declaration =>
            return Get_Declaration (N);
         when Field_Redeclaration =>
            return Get_Redeclaration (N);
         when Field_This_Declaration =>
            return Get_This_Declaration (N);
         when Field_Default_Value =>
            return Get_Default_Value (N);
         when Field_Event =>
            return Get_Event (N);
         when Field_Min_Expr =>
            return Get_Min_Expr (N);
         when Field_Typ_Expr =>
            return Get_Typ_Expr (N);
         when Field_Max_Expr =>
            return Get_Max_Expr (N);
         when Field_Udp_Port_Declaration_Chain =>
            return Get_Udp_Port_Declaration_Chain (N);
         when Field_Udp_Initial =>
            return Get_Udp_Initial (N);
         when Field_Udp_Entries_Chain =>
            return Get_Udp_Entries_Chain (N);
         when Field_Input_Chain =>
            return Get_Input_Chain (N);
         when Field_Specify_Input =>
            return Get_Specify_Input (N);
         when Field_Specify_Output =>
            return Get_Specify_Output (N);
         when Field_Path_Delay =>
            return Get_Path_Delay (N);
         when Field_Data_Source =>
            return Get_Data_Source (N);
         when Field_Delay_Rise =>
            return Get_Delay_Rise (N);
         when Field_Delay_Fall =>
            return Get_Delay_Fall (N);
         when Field_Delay_Z =>
            return Get_Delay_Z (N);
         when Field_Delay_01 =>
            return Get_Delay_01 (N);
         when Field_Delay_10 =>
            return Get_Delay_10 (N);
         when Field_Delay_0z =>
            return Get_Delay_0z (N);
         when Field_Delay_z1 =>
            return Get_Delay_z1 (N);
         when Field_Delay_1z =>
            return Get_Delay_1z (N);
         when Field_Delay_z0 =>
            return Get_Delay_z0 (N);
         when Field_Delay_0x =>
            return Get_Delay_0x (N);
         when Field_Delay_x1 =>
            return Get_Delay_x1 (N);
         when Field_Delay_1x =>
            return Get_Delay_1x (N);
         when Field_Delay_x0 =>
            return Get_Delay_x0 (N);
         when Field_Delay_xz =>
            return Get_Delay_xz (N);
         when Field_Delay_zx =>
            return Get_Delay_zx (N);
         when Field_Label =>
            return Get_Label (N);
         when Field_Label_Chain =>
            return Get_Label_Chain (N);
         when Field_Generate_Block =>
            return Get_Generate_Block (N);
         when Field_Input_Skew =>
            return Get_Input_Skew (N);
         when Field_Output_Skew =>
            return Get_Output_Skew (N);
         when Field_Delay_Control =>
            return Get_Delay_Control (N);
         when Field_Attribute_Item =>
            return Get_Attribute_Item (N);
         when Field_Call =>
            return Get_Call (N);
         when Field_Timeunit =>
            return Get_Timeunit (N);
         when Field_Timeprecision =>
            return Get_Timeprecision (N);
         when Field_Error_Origin =>
            return Get_Error_Origin (N);
         when Field_Access =>
            return Get_Access (N);
         when Field_Arg1 =>
            return Get_Arg1 (N);
         when Field_Arg2 =>
            return Get_Arg2 (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Node;

   procedure Set_Node
      (N : Node; F : Fields_Enum; V: Node) is
   begin
      pragma Assert (Fields_Type (F) = Type_Node);
      case F is
         when Field_Parent =>
            Set_Parent (N, V);
         when Field_Call_Scope =>
            Set_Call_Scope (N, V);
         when Field_Ports_Chain =>
            Set_Ports_Chain (N, V);
         when Field_Tf_Ports_Chain =>
            Set_Tf_Ports_Chain (N, V);
         when Field_Package_Import_Chain =>
            Set_Package_Import_Chain (N, V);
         when Field_Parameter_Port_Chain =>
            Set_Parameter_Port_Chain (N, V);
         when Field_Parameter =>
            Set_Parameter (N, V);
         when Field_Descriptions =>
            Set_Descriptions (N, V);
         when Field_Class_Item_Chain =>
            Set_Class_Item_Chain (N, V);
         when Field_Package_Item_Chain =>
            Set_Package_Item_Chain (N, V);
         when Field_Items_Chain =>
            Set_Items_Chain (N, V);
         when Field_Clocking_Item_Chain =>
            Set_Clocking_Item_Chain (N, V);
         when Field_Tf_Item_Declaration_Chain =>
            Set_Tf_Item_Declaration_Chain (N, V);
         when Field_Block_Item_Declaration_Chain =>
            Set_Block_Item_Declaration_Chain (N, V);
         when Field_Generate_Item_Chain =>
            Set_Generate_Item_Chain (N, V);
         when Field_Specify_Item_Chain =>
            Set_Specify_Item_Chain (N, V);
         when Field_Statements_Chain =>
            Set_Statements_Chain (N, V);
         when Field_Modport_Ports_Chain =>
            Set_Modport_Ports_Chain (N, V);
         when Field_Chain =>
            Set_Chain (N, V);
         when Field_Constraint_Block_Chain =>
            Set_Constraint_Block_Chain (N, V);
         when Field_Constraint_Set =>
            Set_Constraint_Set (N, V);
         when Field_OOB_Prefix =>
            Set_OOB_Prefix (N, V);
         when Field_Out_Of_Block_Declaration =>
            Set_Out_Of_Block_Declaration (N, V);
         when Field_Return_Variable =>
            Set_Return_Variable (N, V);
         when Field_Return_Variable_Ref =>
            Set_Return_Variable_Ref (N, V);
         when Field_This_Variable =>
            Set_This_Variable (N, V);
         when Field_Expression =>
            Set_Expression (N, V);
         when Field_Reject_Limit =>
            Set_Reject_Limit (N, V);
         when Field_Error_Limit =>
            Set_Error_Limit (N, V);
         when Field_Sequence =>
            Set_Sequence (N, V);
         when Field_Init_Expression =>
            Set_Init_Expression (N, V);
         when Field_Size_Expression =>
            Set_Size_Expression (N, V);
         when Field_Override_Stmt =>
            Set_Override_Stmt (N, V);
         when Field_Parameter_Expression =>
            Set_Parameter_Expression (N, V);
         when Field_Parameter_Type =>
            Set_Parameter_Type (N, V);
         when Field_Value_Range =>
            Set_Value_Range (N, V);
         when Field_Range =>
            Set_Range (N, V);
         when Field_Msb =>
            Set_Msb (N, V);
         when Field_Lsb =>
            Set_Lsb (N, V);
         when Field_Base_Expr =>
            Set_Base_Expr (N, V);
         when Field_Width_Expr =>
            Set_Width_Expr (N, V);
         when Field_Maximum_Size_Expr =>
            Set_Maximum_Size_Expr (N, V);
         when Field_Lvalue =>
            Set_Lvalue (N, V);
         when Field_Name =>
            Set_Name (N, V);
         when Field_Item_Name =>
            Set_Item_Name (N, V);
         when Field_Pattern_Key =>
            Set_Pattern_Key (N, V);
         when Field_Left =>
            Set_Left (N, V);
         when Field_Right =>
            Set_Right (N, V);
         when Field_Repeat_Expression =>
            Set_Repeat_Expression (N, V);
         when Field_Op_Attributes =>
            Set_Op_Attributes (N, V);
         when Field_Attributes_Chain =>
            Set_Attributes_Chain (N, V);
         when Field_Condition =>
            Set_Condition (N, V);
         when Field_Cond_True =>
            Set_Cond_True (N, V);
         when Field_Cond_False =>
            Set_Cond_False (N, V);
         when Field_True_Stmt =>
            Set_True_Stmt (N, V);
         when Field_False_Stmt =>
            Set_False_Stmt (N, V);
         when Field_Pass_Stmt =>
            Set_Pass_Stmt (N, V);
         when Field_Else_Stmt =>
            Set_Else_Stmt (N, V);
         when Field_Clocking_Event =>
            Set_Clocking_Event (N, V);
         when Field_Disable_Expression =>
            Set_Disable_Expression (N, V);
         when Field_Property_Expression =>
            Set_Property_Expression (N, V);
         when Field_True_Block =>
            Set_True_Block (N, V);
         when Field_False_Block =>
            Set_False_Block (N, V);
         when Field_Statement =>
            Set_Statement (N, V);
         when Field_Foreach_Array =>
            Set_Foreach_Array (N, V);
         when Field_Foreach_Variables =>
            Set_Foreach_Variables (N, V);
         when Field_Control =>
            Set_Control (N, V);
         when Field_Replication =>
            Set_Replication (N, V);
         when Field_Expressions =>
            Set_Expressions (N, V);
         when Field_Elements =>
            Set_Elements (N, V);
         when Field_Slice_Size_Expr =>
            Set_Slice_Size_Expr (N, V);
         when Field_Slice_Size_Type =>
            Set_Slice_Size_Type (N, V);
         when Field_Members =>
            Set_Members (N, V);
         when Field_Nature_Items =>
            Set_Nature_Items (N, V);
         when Field_Discipline_Items =>
            Set_Discipline_Items (N, V);
         when Field_Nature =>
            Set_Nature (N, V);
         when Field_Connections =>
            Set_Connections (N, V);
         when Field_Gate_Terminals =>
            Set_Gate_Terminals (N, V);
         when Field_Parameter_Values =>
            Set_Parameter_Values (N, V);
         when Field_Case_Items =>
            Set_Case_Items (N, V);
         when Field_Delay =>
            Set_Delay (N, V);
         when Field_Net_Delay =>
            Set_Net_Delay (N, V);
         when Field_Gate_Delay =>
            Set_Gate_Delay (N, V);
         when Field_Assign_Delay =>
            Set_Assign_Delay (N, V);
         when Field_Rising_Delay =>
            Set_Rising_Delay (N, V);
         when Field_Falling_Delay =>
            Set_Falling_Delay (N, V);
         when Field_Highz_Delay =>
            Set_Highz_Delay (N, V);
         when Field_For_Initialization =>
            Set_For_Initialization (N, V);
         when Field_Step_Assign =>
            Set_Step_Assign (N, V);
         when Field_Arguments =>
            Set_Arguments (N, V);
         when Field_Iterator_Argument =>
            Set_Iterator_Argument (N, V);
         when Field_Task =>
            Set_Task (N, V);
         when Field_Expr_Origin =>
            Set_Expr_Origin (N, V);
         when Field_Timescale =>
            Set_Timescale (N, V);
         when Field_Data_Type =>
            Set_Data_Type (N, V);
         when Field_Expr_Type =>
            Set_Expr_Type (N, V);
         when Field_Param_Type =>
            Set_Param_Type (N, V);
         when Field_Element_Data_Type =>
            Set_Element_Data_Type (N, V);
         when Field_Type_Element_Type =>
            Set_Type_Element_Type (N, V);
         when Field_Cast_Data_Type =>
            Set_Cast_Data_Type (N, V);
         when Field_Base_Class_Type =>
            Set_Base_Class_Type (N, V);
         when Field_Class_Constructor =>
            Set_Class_Constructor (N, V);
         when Field_Enum_Base_Data_Type =>
            Set_Enum_Base_Data_Type (N, V);
         when Field_Enum_Base_Type =>
            Set_Enum_Base_Type (N, V);
         when Field_Packed_Base_Type =>
            Set_Packed_Base_Type (N, V);
         when Field_Default_Type =>
            Set_Default_Type (N, V);
         when Field_Forward_Type =>
            Set_Forward_Type (N, V);
         when Field_Enum_Names =>
            Set_Enum_Names (N, V);
         when Field_Index_Data_Type =>
            Set_Index_Data_Type (N, V);
         when Field_Type_Index_Type =>
            Set_Type_Index_Type (N, V);
         when Field_Type_Argument =>
            Set_Type_Argument (N, V);
         when Field_Subroutine =>
            Set_Subroutine (N, V);
         when Field_Object =>
            Set_Object (N, V);
         when Field_With_Expression =>
            Set_With_Expression (N, V);
         when Field_Module =>
            Set_Module (N, V);
         when Field_Class_Name =>
            Set_Class_Name (N, V);
         when Field_Interface =>
            Set_Interface (N, V);
         when Field_Interface_Name =>
            Set_Interface_Name (N, V);
         when Field_Instance =>
            Set_Instance (N, V);
         when Field_Instance_Ref =>
            Set_Instance_Ref (N, V);
         when Field_Port =>
            Set_Port (N, V);
         when Field_Declaration =>
            Set_Declaration (N, V);
         when Field_Redeclaration =>
            Set_Redeclaration (N, V);
         when Field_This_Declaration =>
            Set_This_Declaration (N, V);
         when Field_Default_Value =>
            Set_Default_Value (N, V);
         when Field_Event =>
            Set_Event (N, V);
         when Field_Min_Expr =>
            Set_Min_Expr (N, V);
         when Field_Typ_Expr =>
            Set_Typ_Expr (N, V);
         when Field_Max_Expr =>
            Set_Max_Expr (N, V);
         when Field_Udp_Port_Declaration_Chain =>
            Set_Udp_Port_Declaration_Chain (N, V);
         when Field_Udp_Initial =>
            Set_Udp_Initial (N, V);
         when Field_Udp_Entries_Chain =>
            Set_Udp_Entries_Chain (N, V);
         when Field_Input_Chain =>
            Set_Input_Chain (N, V);
         when Field_Specify_Input =>
            Set_Specify_Input (N, V);
         when Field_Specify_Output =>
            Set_Specify_Output (N, V);
         when Field_Path_Delay =>
            Set_Path_Delay (N, V);
         when Field_Data_Source =>
            Set_Data_Source (N, V);
         when Field_Delay_Rise =>
            Set_Delay_Rise (N, V);
         when Field_Delay_Fall =>
            Set_Delay_Fall (N, V);
         when Field_Delay_Z =>
            Set_Delay_Z (N, V);
         when Field_Delay_01 =>
            Set_Delay_01 (N, V);
         when Field_Delay_10 =>
            Set_Delay_10 (N, V);
         when Field_Delay_0z =>
            Set_Delay_0z (N, V);
         when Field_Delay_z1 =>
            Set_Delay_z1 (N, V);
         when Field_Delay_1z =>
            Set_Delay_1z (N, V);
         when Field_Delay_z0 =>
            Set_Delay_z0 (N, V);
         when Field_Delay_0x =>
            Set_Delay_0x (N, V);
         when Field_Delay_x1 =>
            Set_Delay_x1 (N, V);
         when Field_Delay_1x =>
            Set_Delay_1x (N, V);
         when Field_Delay_x0 =>
            Set_Delay_x0 (N, V);
         when Field_Delay_xz =>
            Set_Delay_xz (N, V);
         when Field_Delay_zx =>
            Set_Delay_zx (N, V);
         when Field_Label =>
            Set_Label (N, V);
         when Field_Label_Chain =>
            Set_Label_Chain (N, V);
         when Field_Generate_Block =>
            Set_Generate_Block (N, V);
         when Field_Input_Skew =>
            Set_Input_Skew (N, V);
         when Field_Output_Skew =>
            Set_Output_Skew (N, V);
         when Field_Delay_Control =>
            Set_Delay_Control (N, V);
         when Field_Attribute_Item =>
            Set_Attribute_Item (N, V);
         when Field_Call =>
            Set_Call (N, V);
         when Field_Timeunit =>
            Set_Timeunit (N, V);
         when Field_Timeprecision =>
            Set_Timeprecision (N, V);
         when Field_Error_Origin =>
            Set_Error_Origin (N, V);
         when Field_Access =>
            Set_Access (N, V);
         when Field_Arg1 =>
            Set_Arg1 (N, V);
         when Field_Arg2 =>
            Set_Arg2 (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Node;

   function Get_Obj_Id
      (N : Node; F : Fields_Enum) return Obj_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Obj_Id);
      case F is
         when Field_Obj_Id =>
            return Get_Obj_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Obj_Id;

   procedure Set_Obj_Id
      (N : Node; F : Fields_Enum; V: Obj_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Obj_Id);
      case F is
         when Field_Obj_Id =>
            Set_Obj_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Obj_Id;

   function Get_Polarity_Type
      (N : Node; F : Fields_Enum) return Polarity_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Polarity_Type);
      case F is
         when Field_Polarity =>
            return Get_Polarity (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Polarity_Type;

   procedure Set_Polarity_Type
      (N : Node; F : Fields_Enum; V: Polarity_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Polarity_Type);
      case F is
         when Field_Polarity =>
            Set_Polarity (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Polarity_Type;

   function Get_Proc_Id
      (N : Node; F : Fields_Enum) return Proc_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Proc_Id);
      case F is
         when Field_Process_Id =>
            return Get_Process_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Proc_Id;

   procedure Set_Proc_Id
      (N : Node; F : Fields_Enum; V: Proc_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Proc_Id);
      case F is
         when Field_Process_Id =>
            Set_Process_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Proc_Id;

   function Get_Scope_Id
      (N : Node; F : Fields_Enum) return Scope_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Scope_Id);
      case F is
         when Field_Scope_Id =>
            return Get_Scope_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Scope_Id;

   procedure Set_Scope_Id
      (N : Node; F : Fields_Enum; V: Scope_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Scope_Id);
      case F is
         when Field_Scope_Id =>
            Set_Scope_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Scope_Id;

   function Get_String8_Id
      (N : Node; F : Fields_Enum) return String8_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_String8_Id);
      case F is
         when Field_String_Id =>
            return Get_String_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_String8_Id;

   procedure Set_String8_Id
      (N : Node; F : Fields_Enum; V: String8_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_String8_Id);
      case F is
         when Field_String_Id =>
            Set_String_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_String8_Id;

   function Get_Sys_Tf_Id
      (N : Node; F : Fields_Enum) return Sys_Tf_Id is
   begin
      pragma Assert (Fields_Type (F) = Type_Sys_Tf_Id);
      case F is
         when Field_Sys_Tf_Id =>
            return Get_Sys_Tf_Id (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Sys_Tf_Id;

   procedure Set_Sys_Tf_Id
      (N : Node; F : Fields_Enum; V: Sys_Tf_Id) is
   begin
      pragma Assert (Fields_Type (F) = Type_Sys_Tf_Id);
      case F is
         when Field_Sys_Tf_Id =>
            Set_Sys_Tf_Id (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Sys_Tf_Id;

   function Get_Tsize_Type
      (N : Node; F : Fields_Enum) return Tsize_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Tsize_Type);
      case F is
         when Field_Type_Size =>
            return Get_Type_Size (N);
         when Field_Stride_Size =>
            return Get_Stride_Size (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Tsize_Type;

   procedure Set_Tsize_Type
      (N : Node; F : Fields_Enum; V: Tsize_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Tsize_Type);
      case F is
         when Field_Type_Size =>
            Set_Type_Size (N, V);
         when Field_Stride_Size =>
            Set_Stride_Size (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Tsize_Type;

   function Get_Udp_Kind
      (N : Node; F : Fields_Enum) return Udp_Kind is
   begin
      pragma Assert (Fields_Type (F) = Type_Udp_Kind);
      case F is
         when Field_Udp_Kind =>
            return Get_Udp_Kind (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Udp_Kind;

   procedure Set_Udp_Kind
      (N : Node; F : Fields_Enum; V: Udp_Kind) is
   begin
      pragma Assert (Fields_Type (F) = Type_Udp_Kind);
      case F is
         when Field_Udp_Kind =>
            Set_Udp_Kind (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Udp_Kind;

   function Get_Udp_Symbol
      (N : Node; F : Fields_Enum) return Udp_Symbol is
   begin
      pragma Assert (Fields_Type (F) = Type_Udp_Symbol);
      case F is
         when Field_Output_Symbol =>
            return Get_Output_Symbol (N);
         when Field_Current_State =>
            return Get_Current_State (N);
         when Field_Next_State =>
            return Get_Next_State (N);
         when Field_Symbol =>
            return Get_Symbol (N);
         when Field_From_Symbol =>
            return Get_From_Symbol (N);
         when Field_To_Symbol =>
            return Get_To_Symbol (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Udp_Symbol;

   procedure Set_Udp_Symbol
      (N : Node; F : Fields_Enum; V: Udp_Symbol) is
   begin
      pragma Assert (Fields_Type (F) = Type_Udp_Symbol);
      case F is
         when Field_Output_Symbol =>
            Set_Output_Symbol (N, V);
         when Field_Current_State =>
            Set_Current_State (N, V);
         when Field_Next_State =>
            Set_Next_State (N, V);
         when Field_Symbol =>
            Set_Symbol (N, V);
         when Field_From_Symbol =>
            Set_From_Symbol (N, V);
         when Field_To_Symbol =>
            Set_To_Symbol (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Udp_Symbol;

   function Get_Unary_Ops
      (N : Node; F : Fields_Enum) return Unary_Ops is
   begin
      pragma Assert (Fields_Type (F) = Type_Unary_Ops);
      case F is
         when Field_Unary_Op =>
            return Get_Unary_Op (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Unary_Ops;

   procedure Set_Unary_Ops
      (N : Node; F : Fields_Enum; V: Unary_Ops) is
   begin
      pragma Assert (Fields_Type (F) = Type_Unary_Ops);
      case F is
         when Field_Unary_Op =>
            Set_Unary_Op (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Unary_Ops;

   function Get_Uns32
      (N : Node; F : Fields_Enum) return Uns32 is
   begin
      pragma Assert (Fields_Type (F) = Type_Uns32);
      case F is
         when Field_Type_Hash =>
            return Get_Type_Hash (N);
         when Field_Packed_Member_Offset =>
            return Get_Packed_Member_Offset (N);
         when Field_Number_Hi_Val =>
            return Get_Number_Hi_Val (N);
         when Field_Number_Lo_Val =>
            return Get_Number_Lo_Val (N);
         when Field_Number_Hi_Zx =>
            return Get_Number_Hi_Zx (N);
         when Field_Number_Lo_Zx =>
            return Get_Number_Lo_Zx (N);
         when Field_Bignum_Len =>
            return Get_Bignum_Len (N);
         when Field_String_Size =>
            return Get_String_Size (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Uns32;

   procedure Set_Uns32
      (N : Node; F : Fields_Enum; V: Uns32) is
   begin
      pragma Assert (Fields_Type (F) = Type_Uns32);
      case F is
         when Field_Type_Hash =>
            Set_Type_Hash (N, V);
         when Field_Packed_Member_Offset =>
            Set_Packed_Member_Offset (N, V);
         when Field_Number_Hi_Val =>
            Set_Number_Hi_Val (N, V);
         when Field_Number_Lo_Val =>
            Set_Number_Lo_Val (N, V);
         when Field_Number_Hi_Zx =>
            Set_Number_Hi_Zx (N, V);
         when Field_Number_Lo_Zx =>
            Set_Number_Lo_Zx (N, V);
         when Field_Bignum_Len =>
            Set_Bignum_Len (N, V);
         when Field_String_Size =>
            Set_String_Size (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Uns32;

   function Get_Violation_Type
      (N : Node; F : Fields_Enum) return Violation_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Violation_Type);
      case F is
         when Field_Violation =>
            return Get_Violation (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Violation_Type;

   procedure Set_Violation_Type
      (N : Node; F : Fields_Enum; V: Violation_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Violation_Type);
      case F is
         when Field_Violation =>
            Set_Violation (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Violation_Type;

   function Get_Visibility_Type
      (N : Node; F : Fields_Enum) return Visibility_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Visibility_Type);
      case F is
         when Field_Visibility =>
            return Get_Visibility (N);
         when Field_Class_Visibility =>
            return Get_Class_Visibility (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Visibility_Type;

   procedure Set_Visibility_Type
      (N : Node; F : Fields_Enum; V: Visibility_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Visibility_Type);
      case F is
         when Field_Visibility =>
            Set_Visibility (N, V);
         when Field_Class_Visibility =>
            Set_Class_Visibility (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Visibility_Type;

   function Get_Width_Type
      (N : Node; F : Fields_Enum) return Width_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Width_Type);
      case F is
         when Field_Type_Width =>
            return Get_Type_Width (N);
         when Field_Stride_Width =>
            return Get_Stride_Width (N);
         when Field_Number_Size =>
            return Get_Number_Size (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Width_Type;

   procedure Set_Width_Type
      (N : Node; F : Fields_Enum; V: Width_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Width_Type);
      case F is
         when Field_Type_Width =>
            Set_Type_Width (N, V);
         when Field_Stride_Width =>
            Set_Stride_Width (N, V);
         when Field_Number_Size =>
            Set_Number_Size (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Width_Type;

   function Has_Parent (K : Nkind) return Boolean is
   begin
      case K is
         when N_Nature
           | N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Foreign_Module
           | N_Module
           | N_Primitive
           | N_Interface_Declaration
           | N_Package
           | N_Program_Declaration
           | N_Port
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Export_DPI_Function
           | N_Export_DPI_Task
           | N_Clocking
           | N_Default_Clocking
           | N_Disable_Iff
           | N_Specify
           | N_Property_Declaration
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Type_Parameter
           | N_Localparam
           | N_Type_Localparam
           | N_Var
           | N_Return_Var
           | N_This_Var
           | N_Iterator_Argument
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Typedef
           | N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward
           | N_Predefined_Typedef
           | N_Package_Import
           | N_Genvar
           | N_Enum_Name
           | N_Foreach_Variable
           | N_Clock_Var
           | N_Modport
           | N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref
           | N_Modport_Clocking
           | N_Modport_Import_Tf
           | N_Modport_Export_Tf
           | N_Constraint
           | N_Constraint_Expression
           | N_Constraint_If
           | N_Constraint_Foreach
           | N_Discipline
           | N_Branch
           | N_Nature_Attribute
           | N_Nature_Access
           | N_Discipline_Domain
           | N_Discipline_Potential
           | N_Discipline_Flow
           | N_Discipline_Attribute
           | N_Assign
           | N_Decl_Assign
           | N_Always
           | N_Always_Comb
           | N_Always_Latch
           | N_Always_Ff
           | N_Initial
           | N_Final
           | N_Debug
           | N_Module_Instance
           | N_Primitive_Instance
           | N_Interface_Instance
           | N_Program_Instance
           | N_Defparam
           | N_Generate_Region
           | N_Loop_Generate
           | N_If_Generate
           | N_Case_Generate
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block
           | N_Analog
           | N_Assert_Property
           | N_Assume_Property
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown
           | N_Default_Skew
           | N_Seq_Block
           | N_Par_Block
           | N_If
           | N_For
           | N_While
           | N_Do_While
           | N_Foreach
           | N_Repeat
           | N_Forever
           | N_Wait
           | N_Wait_Fork
           | N_Trigger
           | N_Disable
           | N_Disable_Fork
           | N_Proc_Assign
           | N_Proc_Deassign
           | N_Noblk_Assign
           | N_Blocking_Assign
           | N_Unpack_Assign
           | N_Pack_Assign
           | N_Pack_Unpack_Assign
           | N_Assign_Operator
           | N_Force_Assign
           | N_Release
           | N_Case
           | N_Casex
           | N_Casez
           | N_Subroutine_Call_Stmt
           | N_Return_Stmt
           | N_Break_Stmt
           | N_Continue_Stmt
           | N_Label_Stmt
           | N_Simple_Immediate_Assert
           | N_Contribution
           | N_Event_Control
           | N_Delay_Control
           | N_Repeat_Control
           | N_Cycle_Delay
           | N_Post_Increment
           | N_Pre_Increment
           | N_Post_Decrement
           | N_Pre_Decrement
           | N_Specparam
           | N_Pulse_Control_Specparam
           | N_Ifnone
           | N_Timing_Check
           | N_Par_Path
           | N_Full_Path
           | N_Par_Edge_Path
           | N_Full_Edge_Path
           | N_Member
           | N_Packed_Member
           | N_Udp_Combinational_Entry
           | N_Udp_Sequential_Entry
           | N_Udp_Level_Symbol
           | N_Udp_Change_Symbol =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parent;

   function Has_Call_Scope (K : Nkind) return Boolean is
   begin
      return K = N_System_Call;
   end Has_Call_Scope;

   function Has_Identifier (K : Nkind) return Boolean is
   begin
      case K is
         when N_Nature
           | N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Compilation_Unit
           | N_Foreign_Module
           | N_Module
           | N_Primitive
           | N_Interface_Declaration
           | N_Package
           | N_Program_Declaration
           | N_Port
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Export_DPI_Function
           | N_Export_DPI_Task
           | N_Clocking
           | N_Default_Clocking
           | N_Property_Declaration
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Type_Parameter
           | N_Localparam
           | N_Type_Localparam
           | N_Var
           | N_Return_Var
           | N_This_Var
           | N_Iterator_Argument
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Typedef
           | N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward
           | N_Predefined_Typedef
           | N_Genvar
           | N_Enum_Name
           | N_Enum_Range
           | N_Foreach_Variable
           | N_Clock_Var
           | N_Modport
           | N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref
           | N_Modport_Clocking
           | N_Modport_Import_Tf
           | N_Modport_Export_Tf
           | N_Constraint
           | N_Discipline
           | N_Branch
           | N_Port_Branch
           | N_Nature_Attribute
           | N_Nature_Access
           | N_Discipline_Domain
           | N_Discipline_Potential
           | N_Discipline_Flow
           | N_Discipline_Attribute
           | N_Module_Instance
           | N_Primitive_Instance
           | N_Interface_Instance
           | N_Program_Instance
           | N_Parameter_Value_Type
           | N_Parameter_Value_Expr
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block
           | N_Assert_Property
           | N_Assume_Property
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown
           | N_Port_Connection
           | N_Implicit_Connection
           | N_Seq_Block
           | N_Par_Block
           | N_Label_Stmt
           | N_Name
           | N_This_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item
           | N_Property_Name
           | N_Class_Qualified_Name
           | N_Method_Name
           | N_Member_Name
           | N_Hierarchical
           | N_Member_Select
           | N_System_Call
           | N_Specparam
           | N_Pulse_Control_Specparam
           | N_Timing_Check
           | N_Member
           | N_Packed_Member
           | N_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Identifier;

   function Has_C_Identifier (K : Nkind) return Boolean is
   begin
      case K is
         when N_Import_DPI_Function
           | N_Export_DPI_Function
           | N_Export_DPI_Task =>
            return True;
         when others =>
            return False;
      end case;
   end Has_C_Identifier;

   function Has_Ports_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Foreign_Module
           | N_Module
           | N_Primitive
           | N_Interface_Declaration
           | N_Program_Declaration
           | N_Property_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Ports_Chain;

   function Has_Tf_Ports_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Tf_Ports_Chain;

   function Has_Package_Import_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Module
           | N_Interface_Declaration
           | N_Program_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Package_Import_Chain;

   function Has_Parameter_Port_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Foreign_Module
           | N_Module
           | N_Interface_Declaration
           | N_Program_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter_Port_Chain;

   function Has_Parameter (K : Nkind) return Boolean is
   begin
      case K is
         when N_Parameter_Value_Type
           | N_Parameter_Value_Expr
           | N_Defparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter;

   function Has_Foreign_Node (K : Nkind) return Boolean is
   begin
      return K = N_Foreign_Module;
   end Has_Foreign_Node;

   function Has_Descriptions (K : Nkind) return Boolean is
   begin
      return K = N_Compilation_Unit;
   end Has_Descriptions;

   function Has_Class_Item_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Class_Item_Chain;

   function Has_Package_Item_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Package;
   end Has_Package_Item_Chain;

   function Has_Items_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Foreign_Module
           | N_Module
           | N_Interface_Declaration
           | N_Program_Declaration
           | N_Property_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Items_Chain;

   function Has_Clocking_Item_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Clocking
           | N_Default_Clocking =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Clocking_Item_Chain;

   function Has_Tf_Item_Declaration_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Tf_Item_Declaration_Chain;

   function Has_Block_Item_Declaration_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Seq_Block
           | N_Par_Block =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Block_Item_Declaration_Chain;

   function Has_Generate_Item_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Generate_Region
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generate_Item_Chain;

   function Has_Specify_Item_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Specify;
   end Has_Specify_Item_Chain;

   function Has_Statements_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Seq_Block
           | N_Par_Block
           | N_Label_Stmt =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Statements_Chain;

   function Has_Modport_Ports_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Modport;
   end Has_Modport_Ports_Chain;

   function Has_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Timescale_Directive
           | N_Timeunits_Declaration
           | N_Timeunit
           | N_Timeprecision
           | N_Nature
           | N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Compilation_Unit
           | N_Foreign_Module
           | N_Module
           | N_Primitive
           | N_Interface_Declaration
           | N_Package
           | N_Program_Declaration
           | N_Port
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Export_DPI_Function
           | N_Export_DPI_Task
           | N_Clocking
           | N_Default_Clocking
           | N_Disable_Iff
           | N_Specify
           | N_Property_Declaration
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Type_Parameter
           | N_Localparam
           | N_Type_Localparam
           | N_Var
           | N_Return_Var
           | N_This_Var
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Typedef
           | N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward
           | N_Predefined_Typedef
           | N_Package_Import
           | N_Genvar
           | N_Enum_Name
           | N_Enum_Range
           | N_Foreach_Variable
           | N_Clock_Var
           | N_Modport
           | N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref
           | N_Modport_Clocking
           | N_Modport_Import_Tf
           | N_Modport_Export_Tf
           | N_Constraint
           | N_Constraint_Expression
           | N_Constraint_If
           | N_Constraint_Foreach
           | N_Discipline
           | N_Branch
           | N_Nature_Attribute
           | N_Nature_Access
           | N_Discipline_Domain
           | N_Discipline_Potential
           | N_Discipline_Flow
           | N_Discipline_Attribute
           | N_From_Range
           | N_Exclude_Range
           | N_Assign
           | N_Decl_Assign
           | N_Always
           | N_Always_Comb
           | N_Always_Latch
           | N_Always_Ff
           | N_Initial
           | N_Final
           | N_Debug
           | N_Module_Instance
           | N_Primitive_Instance
           | N_Interface_Instance
           | N_Program_Instance
           | N_Parameter_Value_Type
           | N_Parameter_Value_Expr
           | N_Defparam
           | N_Generate_Region
           | N_Loop_Generate
           | N_If_Generate
           | N_Case_Generate
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block
           | N_Analog
           | N_Assert_Property
           | N_Assume_Property
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown
           | N_Default_Skew
           | N_Control_Terminal
           | N_Input_Terminal
           | N_Inout_Terminal
           | N_Output_Terminal
           | N_Port_Connection
           | N_Wildcard_Connection
           | N_Implicit_Connection
           | N_Default_Connection
           | N_Seq_Block
           | N_Par_Block
           | N_If
           | N_For
           | N_While
           | N_Do_While
           | N_Foreach
           | N_Repeat
           | N_Forever
           | N_Wait
           | N_Wait_Fork
           | N_Trigger
           | N_Disable
           | N_Disable_Fork
           | N_Proc_Assign
           | N_Proc_Deassign
           | N_Noblk_Assign
           | N_Blocking_Assign
           | N_Unpack_Assign
           | N_Pack_Assign
           | N_Pack_Unpack_Assign
           | N_Assign_Operator
           | N_Force_Assign
           | N_Release
           | N_Case
           | N_Casex
           | N_Casez
           | N_Case_Item
           | N_Default_Case_Item
           | N_Subroutine_Call_Stmt
           | N_Return_Stmt
           | N_Break_Stmt
           | N_Continue_Stmt
           | N_Label_Stmt
           | N_Simple_Immediate_Assert
           | N_Argument
           | N_Contribution
           | N_Aggregate_Element
           | N_Event_Control
           | N_Delay_Control
           | N_Repeat_Control
           | N_Cycle_Delay
           | N_Element
           | N_Stream_Expression
           | N_Post_Increment
           | N_Pre_Increment
           | N_Post_Decrement
           | N_Pre_Decrement
           | N_Specparam
           | N_Pulse_Control_Specparam
           | N_Ifnone
           | N_Timing_Check
           | N_Par_Path
           | N_Full_Path
           | N_Par_Edge_Path
           | N_Full_Edge_Path
           | N_Path_Element
           | N_Member
           | N_Packed_Member
           | N_Udp_Combinational_Entry
           | N_Udp_Sequential_Entry
           | N_Udp_Level_Symbol
           | N_Udp_Change_Symbol
           | N_Attribute
           | N_Label
           | N_Goto =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Chain;

   function Has_Constraint_Block_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Constraint
           | N_Randomize_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Constraint_Block_Chain;

   function Has_Constraint_Set (K : Nkind) return Boolean is
   begin
      return K = N_Constraint_Foreach;
   end Has_Constraint_Set;

   function Has_OOB_Prefix (K : Nkind) return Boolean is
   begin
      case K is
         when N_OOB_Task
           | N_OOB_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_OOB_Prefix;

   function Has_Out_Of_Block_Declaration (K : Nkind) return Boolean is
   begin
      case K is
         when N_Extern_Task
           | N_Extern_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Out_Of_Block_Declaration;

   function Has_Generate_Index (K : Nkind) return Boolean is
   begin
      case K is
         when N_Genvar
           | N_Indexed_Generate_Block =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generate_Index;

   function Has_Return_Variable (K : Nkind) return Boolean is
   begin
      case K is
         when N_Function
           | N_OOB_Function
           | N_Extern_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Return_Variable;

   function Has_Return_Variable_Ref (K : Nkind) return Boolean is
   begin
      return K = N_Return_Stmt;
   end Has_Return_Variable_Ref;

   function Has_This_Variable (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_This_Variable;

   function Has_Expression (K : Nkind) return Boolean is
   begin
      case K is
         when N_Port
           | N_Disable_Iff
           | N_Parameter
           | N_Localparam
           | N_Var
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Genvar
           | N_Enum_Name
           | N_Enum_Range
           | N_Clock_Var
           | N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref
           | N_Constraint_Expression
           | N_Nature_Attribute
           | N_Discipline_Attribute
           | N_Assign
           | N_Decl_Assign
           | N_Parameter_Value_Expr
           | N_Defparam
           | N_Case_Generate
           | N_Control_Terminal
           | N_Input_Terminal
           | N_Inout_Terminal
           | N_Output_Terminal
           | N_Port_Connection
           | N_Implicit_Connection
           | N_Repeat
           | N_Proc_Assign
           | N_Noblk_Assign
           | N_Blocking_Assign
           | N_Unpack_Assign
           | N_Pack_Assign
           | N_Pack_Unpack_Assign
           | N_Assign_Operator
           | N_Force_Assign
           | N_Case
           | N_Casex
           | N_Casez
           | N_Case_Item
           | N_Return_Stmt
           | N_Argument
           | N_Contribution
           | N_Bit_Select
           | N_Indexed_Name
           | N_String_Index
           | N_Associative_Index
           | N_New_Expression
           | N_Parenthesis_Expr
           | N_Type_Cast
           | N_Size_Cast
           | N_Aggregate_Element
           | N_Event_Control
           | N_Delay_Control
           | N_Repeat_Control
           | N_Cycle_Delay
           | N_Posedge
           | N_Negedge
           | N_Element
           | N_Stream_Expression
           | N_Left_Streaming_Expr
           | N_Right_Streaming_Expr
           | N_Membership
           | N_Array_Method_Call
           | N_Bits_Expr
           | N_Unary_Op
           | N_Conversion
           | N_Prop_Not
           | N_Specparam
           | N_Member
           | N_Packed_Member
           | N_Attribute =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Expression;

   function Has_Reject_Limit (K : Nkind) return Boolean is
   begin
      return K = N_Pulse_Control_Specparam;
   end Has_Reject_Limit;

   function Has_Error_Limit (K : Nkind) return Boolean is
   begin
      return K = N_Pulse_Control_Specparam;
   end Has_Error_Limit;

   function Has_Sequence (K : Nkind) return Boolean is
   begin
      case K is
         when N_Seq_Repeat
           | N_Seq_Plus_Repeat
           | N_Seq_Star_Repeat
           | N_Seq_Parenthesis =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Sequence;

   function Has_Init_Expression (K : Nkind) return Boolean is
   begin
      return K = N_Dynamic_Array_New;
   end Has_Init_Expression;

   function Has_Size_Expression (K : Nkind) return Boolean is
   begin
      case K is
         when N_Dynamic_Array_New
           | N_Size_Cast =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Size_Expression;

   function Has_Override_Stmt (K : Nkind) return Boolean is
   begin
      return K = N_Parameter;
   end Has_Override_Stmt;

   function Has_Parameter_Expression (K : Nkind) return Boolean is
   begin
      return K = N_Parameter;
   end Has_Parameter_Expression;

   function Has_Parameter_Type (K : Nkind) return Boolean is
   begin
      return K = N_Type_Parameter;
   end Has_Parameter_Type;

   function Has_Value_Range (K : Nkind) return Boolean is
   begin
      case K is
         when N_Parameter
           | N_Localparam
           | N_Specparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Value_Range;

   function Has_Lsb_Include_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_From_Range
           | N_Exclude_Range =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Lsb_Include_Flag;

   function Has_Msb_Include_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_From_Range
           | N_Exclude_Range =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Msb_Include_Flag;

   function Has_Range (K : Nkind) return Boolean is
   begin
      case K is
         when N_Module_Instance
           | N_Interface_Instance
           | N_Program_Instance
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Range;

   function Has_Msb (K : Nkind) return Boolean is
   begin
      case K is
         when N_Packed_Array
           | N_Array
           | N_Enum_Range
           | N_From_Range
           | N_Exclude_Range
           | N_Part_Select
           | N_Slice_Name
           | N_Value_Range
           | N_Seq_Repeat =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Msb;

   function Has_Lsb (K : Nkind) return Boolean is
   begin
      case K is
         when N_Packed_Array
           | N_Array
           | N_Enum_Range
           | N_From_Range
           | N_Exclude_Range
           | N_Part_Select
           | N_Slice_Name
           | N_Value_Range
           | N_Seq_Repeat =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Lsb;

   function Has_Msb_Cst (K : Nkind) return Boolean is
   begin
      case K is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst
           | N_Part_Select_Cst
           | N_Slice_Name_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Msb_Cst;

   function Has_Lsb_Cst (K : Nkind) return Boolean is
   begin
      case K is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst
           | N_Part_Select_Cst
           | N_Slice_Name_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Lsb_Cst;

   function Has_Base_Expr (K : Nkind) return Boolean is
   begin
      case K is
         when N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Base_Expr;

   function Has_Width_Expr (K : Nkind) return Boolean is
   begin
      case K is
         when N_Plus_Part_Select
           | N_Minus_Part_Select =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Width_Expr;

   function Has_Width_Cst (K : Nkind) return Boolean is
   begin
      case K is
         when N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Width_Cst;

   function Has_Type_Width (K : Nkind) return Boolean is
   begin
      case K is
         when N_Logic_Type
           | N_Bit_Type
           | N_Real_Type
           | N_Shortreal_Type
           | N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Packed_Struct_Type
           | N_Packed_Union_Type
           | N_Enum_Type
           | N_Error_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Width;

   function Has_Type_Size (K : Nkind) return Boolean is
   begin
      case K is
         when N_Array_Cst
           | N_Struct_Type
           | N_Union_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Size;

   function Has_Stride_Width (K : Nkind) return Boolean is
   begin
      case K is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Stride_Width;

   function Has_Stride_Size (K : Nkind) return Boolean is
   begin
      case K is
         when N_Array_Cst
           | N_Dynamic_Array_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Stride_Size;

   function Has_Type_Hash (K : Nkind) return Boolean is
      pragma Unreferenced (K);
   begin
      return False;
   end Has_Type_Hash;

   function Has_Maximum_Size_Expr (K : Nkind) return Boolean is
   begin
      return K = N_Queue;
   end Has_Maximum_Size_Expr;

   function Has_Maximum_Size_Cst (K : Nkind) return Boolean is
   begin
      return K = N_Queue_Cst;
   end Has_Maximum_Size_Cst;

   function Has_Lvalue (K : Nkind) return Boolean is
   begin
      case K is
         when N_Assign
           | N_Decl_Assign
           | N_Defparam
           | N_Proc_Assign
           | N_Proc_Deassign
           | N_Noblk_Assign
           | N_Blocking_Assign
           | N_Unpack_Assign
           | N_Pack_Assign
           | N_Pack_Unpack_Assign
           | N_Assign_Operator
           | N_Force_Assign
           | N_Release
           | N_Contribution
           | N_Post_Increment
           | N_Pre_Increment
           | N_Post_Decrement
           | N_Pre_Decrement
           | N_Path_Element =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Lvalue;

   function Has_Name (K : Nkind) return Boolean is
   begin
      case K is
         when N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item
           | N_Wildcard_Name
           | N_Property_Name
           | N_Class_Qualified_Name
           | N_Method_Name
           | N_Member_Name
           | N_Hierarchical
           | N_Bit_Select
           | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Indexed_Name
           | N_String_Index
           | N_Associative_Index
           | N_Slice_Name
           | N_Part_Select_Cst
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst
           | N_Slice_Name_Cst
           | N_Member_Select =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Name;

   function Has_Item_Name (K : Nkind) return Boolean is
   begin
      return K = N_Package_Import;
   end Has_Item_Name;

   function Has_Pattern_Key (K : Nkind) return Boolean is
   begin
      return K = N_Aggregate_Element;
   end Has_Pattern_Key;

   function Has_Left (K : Nkind) return Boolean is
   begin
      case K is
         when N_Or
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Seq_Star_Concat
           | N_Seq_Plus_Concat
           | N_Seq_Const_Concat
           | N_Seq_Range_Concat
           | N_Seq_Throughout
           | N_Prop_Or
           | N_Prop_And
           | N_Prop_Overlap_Imp
           | N_Prop_Non_Overlap_Imp
           | N_Prop_Until =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Left;

   function Has_Right (K : Nkind) return Boolean is
   begin
      case K is
         when N_Or
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Seq_Star_Concat
           | N_Seq_Plus_Concat
           | N_Seq_Const_Concat
           | N_Seq_Range_Concat
           | N_Seq_Throughout
           | N_Prop_Or
           | N_Prop_And
           | N_Prop_Overlap_Imp
           | N_Prop_Non_Overlap_Imp
           | N_Prop_Until =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Right;

   function Has_Repeat_Expression (K : Nkind) return Boolean is
   begin
      case K is
         when N_Seq_Const_Concat
           | N_Seq_Range_Concat =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Repeat_Expression;

   function Has_Op_Attributes (K : Nkind) return Boolean is
   begin
      case K is
         when N_Cond_Op
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Unary_Op =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Op_Attributes;

   function Has_Attributes_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Module
           | N_Seq_Block
           | N_Par_Block =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Attributes_Chain;

   function Has_Condition (K : Nkind) return Boolean is
   begin
      case K is
         when N_Constraint_If
           | N_Loop_Generate
           | N_If_Generate
           | N_If
           | N_For
           | N_While
           | N_Do_While
           | N_Wait
           | N_Simple_Immediate_Assert
           | N_Cond_Op =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Condition;

   function Has_Cond_True (K : Nkind) return Boolean is
   begin
      case K is
         when N_Constraint_If
           | N_Cond_Op =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Cond_True;

   function Has_Cond_False (K : Nkind) return Boolean is
   begin
      case K is
         when N_Constraint_If
           | N_Cond_Op =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Cond_False;

   function Has_True_Stmt (K : Nkind) return Boolean is
   begin
      case K is
         when N_If
           | N_Ifnone =>
            return True;
         when others =>
            return False;
      end case;
   end Has_True_Stmt;

   function Has_False_Stmt (K : Nkind) return Boolean is
   begin
      return K = N_If;
   end Has_False_Stmt;

   function Has_Pass_Stmt (K : Nkind) return Boolean is
   begin
      case K is
         when N_Assert_Property
           | N_Assume_Property
           | N_Simple_Immediate_Assert =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Pass_Stmt;

   function Has_Else_Stmt (K : Nkind) return Boolean is
   begin
      case K is
         when N_Assert_Property
           | N_Assume_Property
           | N_Simple_Immediate_Assert =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Else_Stmt;

   function Has_Clocking_Event (K : Nkind) return Boolean is
   begin
      case K is
         when N_Property_Declaration
           | N_Assert_Property
           | N_Assume_Property =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Clocking_Event;

   function Has_Disable_Expression (K : Nkind) return Boolean is
   begin
      case K is
         when N_Property_Declaration
           | N_Assert_Property
           | N_Assume_Property =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Disable_Expression;

   function Has_Property_Expression (K : Nkind) return Boolean is
   begin
      case K is
         when N_Property_Declaration
           | N_Assert_Property
           | N_Assume_Property =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Property_Expression;

   function Has_True_Block (K : Nkind) return Boolean is
   begin
      return K = N_If_Generate;
   end Has_True_Block;

   function Has_False_Block (K : Nkind) return Boolean is
   begin
      return K = N_If_Generate;
   end Has_False_Block;

   function Has_Statement (K : Nkind) return Boolean is
   begin
      case K is
         when N_Always
           | N_Always_Comb
           | N_Always_Latch
           | N_Always_Ff
           | N_Initial
           | N_Final
           | N_Debug
           | N_Analog
           | N_For
           | N_While
           | N_Do_While
           | N_Foreach
           | N_Repeat
           | N_Forever
           | N_Wait
           | N_Disable
           | N_Case_Item
           | N_Default_Case_Item
           | N_Event_Control
           | N_Delay_Control
           | N_Repeat_Control
           | N_Cycle_Delay =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Statement;

   function Has_Foreach_Array (K : Nkind) return Boolean is
   begin
      case K is
         when N_Constraint_Foreach
           | N_Foreach =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Foreach_Array;

   function Has_Foreach_Variables (K : Nkind) return Boolean is
   begin
      case K is
         when N_Constraint_Foreach
           | N_Foreach =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Foreach_Variables;

   function Has_Control (K : Nkind) return Boolean is
   begin
      case K is
         when N_Noblk_Assign
           | N_Blocking_Assign
           | N_Unpack_Assign
           | N_Pack_Assign
           | N_Pack_Unpack_Assign
           | N_Repeat_Control =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Control;

   function Has_Replication (K : Nkind) return Boolean is
   begin
      case K is
         when N_Aggregate_Literal
           | N_Concatenation =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Replication;

   function Has_Replication_Cst (K : Nkind) return Boolean is
   begin
      case K is
         when N_Aggregate_Literal_Cst
           | N_Replication_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Replication_Cst;

   function Has_Expressions (K : Nkind) return Boolean is
   begin
      case K is
         when N_Left_Streaming_Expr
           | N_Right_Streaming_Expr
           | N_Left_Streaming_Type
           | N_Right_Streaming_Type
           | N_Concatenation
           | N_Membership
           | N_Replication_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Expressions;

   function Has_Elements (K : Nkind) return Boolean is
   begin
      case K is
         when N_Aggregate_Literal
           | N_Aggregate_Literal_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Elements;

   function Has_Slice_Size_Expr (K : Nkind) return Boolean is
      pragma Unreferenced (K);
   begin
      return False;
   end Has_Slice_Size_Expr;

   function Has_Slice_Size_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Left_Streaming_Type
           | N_Right_Streaming_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Slice_Size_Type;

   function Has_Members (K : Nkind) return Boolean is
   begin
      case K is
         when N_Struct_Type
           | N_Packed_Struct_Type
           | N_Union_Type
           | N_Packed_Union_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Members;

   function Has_Nbr_Members (K : Nkind) return Boolean is
   begin
      case K is
         when N_Struct_Type
           | N_Packed_Struct_Type
           | N_Union_Type
           | N_Packed_Union_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Nbr_Members;

   function Has_Member_Index (K : Nkind) return Boolean is
   begin
      case K is
         when N_Member
           | N_Packed_Member =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Member_Index;

   function Has_Packed_Member_Offset (K : Nkind) return Boolean is
   begin
      return K = N_Packed_Member;
   end Has_Packed_Member_Offset;

   function Has_Nature_Items (K : Nkind) return Boolean is
   begin
      return K = N_Nature;
   end Has_Nature_Items;

   function Has_Discipline_Items (K : Nkind) return Boolean is
   begin
      return K = N_Discipline;
   end Has_Discipline_Items;

   function Has_Continuous_Flag (K : Nkind) return Boolean is
   begin
      return K = N_Discipline_Domain;
   end Has_Continuous_Flag;

   function Has_Potential_Flag (K : Nkind) return Boolean is
   begin
      return K = N_Discipline_Attribute;
   end Has_Potential_Flag;

   function Has_Nature (K : Nkind) return Boolean is
   begin
      case K is
         when N_Discipline_Potential
           | N_Discipline_Flow =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Nature;

   function Has_Connections (K : Nkind) return Boolean is
   begin
      case K is
         when N_Module_Instance
           | N_Interface_Instance
           | N_Program_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Connections;

   function Has_Gate_Terminals (K : Nkind) return Boolean is
   begin
      case K is
         when N_Primitive_Instance
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Gate_Terminals;

   function Has_Parameter_Values (K : Nkind) return Boolean is
   begin
      case K is
         when N_Virtual_Interface
           | N_Class_Instance
           | N_Module_Instance
           | N_Interface_Instance
           | N_Program_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Parameter_Values;

   function Has_Case_Items (K : Nkind) return Boolean is
   begin
      case K is
         when N_Case_Generate
           | N_Case
           | N_Casex
           | N_Casez =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Case_Items;

   function Has_Delay (K : Nkind) return Boolean is
      pragma Unreferenced (K);
   begin
      return False;
   end Has_Delay;

   function Has_Net_Delay (K : Nkind) return Boolean is
   begin
      case K is
         when N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Net_Delay;

   function Has_Gate_Delay (K : Nkind) return Boolean is
   begin
      case K is
         when N_Primitive_Instance
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Gate_Delay;

   function Has_Assign_Delay (K : Nkind) return Boolean is
   begin
      case K is
         when N_Assign
           | N_Decl_Assign =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Assign_Delay;

   function Has_Rising_Delay (K : Nkind) return Boolean is
   begin
      return K = N_Delay;
   end Has_Rising_Delay;

   function Has_Falling_Delay (K : Nkind) return Boolean is
   begin
      return K = N_Delay;
   end Has_Falling_Delay;

   function Has_Highz_Delay (K : Nkind) return Boolean is
   begin
      return K = N_Delay;
   end Has_Highz_Delay;

   function Has_For_Initialization (K : Nkind) return Boolean is
   begin
      case K is
         when N_Loop_Generate
           | N_For =>
            return True;
         when others =>
            return False;
      end case;
   end Has_For_Initialization;

   function Has_Step_Assign (K : Nkind) return Boolean is
   begin
      case K is
         when N_Loop_Generate
           | N_For =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Step_Assign;

   function Has_Arguments (K : Nkind) return Boolean is
   begin
      case K is
         when N_New_Call
           | N_Call
           | N_Randomize_Call
           | N_System_Call
           | N_Timing_Check =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Arguments;

   function Has_Iterator_Argument (K : Nkind) return Boolean is
   begin
      return K = N_Array_Method_Call;
   end Has_Iterator_Argument;

   function Has_Task (K : Nkind) return Boolean is
      pragma Unreferenced (K);
   begin
      return False;
   end Has_Task;

   function Has_Signed_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Logic_Type
           | N_Bit_Type
           | N_Real_Type
           | N_Shortreal_Type
           | N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst
           | N_Packed_Array
           | N_Array
           | N_Packed_Struct_Type
           | N_Packed_Union_Type
           | N_Enum_Type
           | N_Error_Type
           | N_Number
           | N_Computed_Number
           | N_Bignum
           | N_Unbased_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Signed_Flag;

   function Has_Scope_Flag (K : Nkind) return Boolean is
   begin
      return K = N_For;
   end Has_Scope_Flag;

   function Has_Number_Base (K : Nkind) return Boolean is
   begin
      case K is
         when N_Number
           | N_Computed_Number
           | N_Bignum =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Number_Base;

   function Has_Number_Hi_Val (K : Nkind) return Boolean is
   begin
      case K is
         when N_Number
           | N_Computed_Number =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Number_Hi_Val;

   function Has_Number_Lo_Val (K : Nkind) return Boolean is
   begin
      case K is
         when N_Number
           | N_Computed_Number
           | N_Unbased_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Number_Lo_Val;

   function Has_Number_Hi_Zx (K : Nkind) return Boolean is
   begin
      case K is
         when N_Number
           | N_Computed_Number =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Number_Hi_Zx;

   function Has_Number_Lo_Zx (K : Nkind) return Boolean is
   begin
      case K is
         when N_Number
           | N_Computed_Number
           | N_Unbased_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Number_Lo_Zx;

   function Has_Number_Size (K : Nkind) return Boolean is
   begin
      case K is
         when N_Number
           | N_Bignum =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Number_Size;

   function Has_Expr_Origin (K : Nkind) return Boolean is
   begin
      return K = N_Computed_Number;
   end Has_Expr_Origin;

   function Has_Bignum_Index (K : Nkind) return Boolean is
   begin
      return K = N_Bignum;
   end Has_Bignum_Index;

   function Has_Bignum_Len (K : Nkind) return Boolean is
   begin
      return K = N_Bignum;
   end Has_Bignum_Len;

   function Has_Real_Number (K : Nkind) return Boolean is
   begin
      case K is
         when N_Time_Literal
           | N_Real_Number
           | N_Scale_Number =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Real_Number;

   function Has_Time_Unit (K : Nkind) return Boolean is
   begin
      case K is
         when N_Timescale_Directive
           | N_Timeunits_Declaration
           | N_Time_Literal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Time_Unit;

   function Has_Scale_Factor (K : Nkind) return Boolean is
   begin
      return K = N_Scale_Number;
   end Has_Scale_Factor;

   function Has_Time_Precision (K : Nkind) return Boolean is
   begin
      case K is
         when N_Timescale_Directive
           | N_Timeunits_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Time_Precision;

   function Has_Timescale (K : Nkind) return Boolean is
   begin
      case K is
         when N_Time_Literal
           | N_Delay_Control =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Timescale;

   function Has_String_Size (K : Nkind) return Boolean is
   begin
      return K = N_String_Literal;
   end Has_String_Size;

   function Has_Data_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Port
           | N_Function
           | N_OOB_Function
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Localparam
           | N_Var
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Typedef
           | N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref
           | N_Parameter_Value_Type
           | N_Repeat
           | N_Specparam
           | N_Pulse_Control_Specparam
           | N_Member
           | N_Packed_Member =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Data_Type;

   function Has_Expr_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Error_Expr
           | N_Packed_Array
           | N_Array
           | N_Struct_Type
           | N_Packed_Struct_Type
           | N_Union_Type
           | N_Packed_Union_Type
           | N_Queue
           | N_Dynamic_Array
           | N_Associative_Array
           | N_Enum_Type
           | N_Class_Instance
           | N_Return_Var
           | N_This_Var
           | N_Iterator_Argument
           | N_Predefined_Typedef
           | N_Genvar
           | N_Enum_Name
           | N_Enum_Range
           | N_Foreach_Variable
           | N_Noblk_Assign
           | N_Blocking_Assign
           | N_Assign_Operator
           | N_Name
           | N_This_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item
           | N_Property_Name
           | N_Class_Qualified_Name
           | N_Method_Name
           | N_Member_Name
           | N_Hierarchical
           | N_Number
           | N_Computed_Number
           | N_Bignum
           | N_Unbased_Literal
           | N_Time_Literal
           | N_Step_Literal
           | N_Infinity
           | N_Real_Number
           | N_Scale_Number
           | N_Bit_Select
           | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Indexed_Name
           | N_String_Index
           | N_Associative_Index
           | N_Slice_Name
           | N_Part_Select_Cst
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst
           | N_Slice_Name_Cst
           | N_Member_Select
           | N_String_Literal
           | N_New_Call
           | N_New_Expression
           | N_Dynamic_Array_New
           | N_Parenthesis_Expr
           | N_Type_Cast
           | N_Size_Cast
           | N_Null
           | N_This
           | N_Super
           | N_Default
           | N_Aggregate_Literal
           | N_Aggregate_Literal_Cst
           | N_Aggregate_Element
           | N_Value_Range
           | N_Left_Streaming_Type
           | N_Right_Streaming_Type
           | N_Concatenation
           | N_Membership
           | N_Replication_Cst
           | N_Cond_Op
           | N_Call
           | N_Array_Method_Call
           | N_Randomize_Call
           | N_System_Call
           | N_Bits_Expr
           | N_Bits_Type
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Unary_Op
           | N_Post_Increment
           | N_Pre_Increment
           | N_Post_Decrement
           | N_Pre_Decrement
           | N_Access_Call
           | N_Conversion =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Expr_Type;

   function Has_Param_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Parameter
           | N_Localparam
           | N_Specparam
           | N_Pulse_Control_Specparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Param_Type;

   function Has_Element_Data_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Packed_Array
           | N_Array
           | N_Queue
           | N_Dynamic_Array
           | N_Associative_Array =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Element_Data_Type;

   function Has_Type_Element_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst
           | N_Queue_Cst
           | N_Dynamic_Array_Cst
           | N_Associative_Array_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Element_Type;

   function Has_Cast_Data_Type (K : Nkind) return Boolean is
   begin
      return K = N_Type_Cast;
   end Has_Cast_Data_Type;

   function Has_Base_Class_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Base_Class_Type;

   function Has_Class_Constructor (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Class_Constructor;

   function Has_Inheritance_Depth (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Inheritance_Depth;

   function Has_Enum_Base_Data_Type (K : Nkind) return Boolean is
   begin
      return K = N_Enum_Type;
   end Has_Enum_Base_Data_Type;

   function Has_Enum_Base_Type (K : Nkind) return Boolean is
   begin
      return K = N_Enum_Type;
   end Has_Enum_Base_Type;

   function Has_Packed_Base_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Packed_Struct_Type
           | N_Packed_Union_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Packed_Base_Type;

   function Has_Default_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Type_Parameter
           | N_Type_Localparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Default_Type;

   function Has_Type_Owner (K : Nkind) return Boolean is
   begin
      case K is
         when N_Error_Expr
           | N_Packed_Array
           | N_Array
           | N_Queue
           | N_Dynamic_Array
           | N_Associative_Array
           | N_Enum_Type
           | N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Port
           | N_Function
           | N_OOB_Function
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Type_Parameter
           | N_Localparam
           | N_Type_Localparam
           | N_Var
           | N_Iterator_Argument
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Typedef
           | N_Genvar
           | N_Enum_Name
           | N_Enum_Range
           | N_Foreach_Variable
           | N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref
           | N_Parameter_Value_Type
           | N_Repeat
           | N_Noblk_Assign
           | N_Blocking_Assign
           | N_Assign_Operator
           | N_Name
           | N_This_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item
           | N_Property_Name
           | N_Class_Qualified_Name
           | N_Method_Name
           | N_Member_Name
           | N_Hierarchical
           | N_Number
           | N_Computed_Number
           | N_Bignum
           | N_Unbased_Literal
           | N_Time_Literal
           | N_Step_Literal
           | N_Infinity
           | N_Real_Number
           | N_Scale_Number
           | N_Bit_Select
           | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Indexed_Name
           | N_String_Index
           | N_Associative_Index
           | N_Slice_Name
           | N_Part_Select_Cst
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst
           | N_Slice_Name_Cst
           | N_Member_Select
           | N_String_Literal
           | N_New_Call
           | N_New_Expression
           | N_Dynamic_Array_New
           | N_Parenthesis_Expr
           | N_Type_Cast
           | N_Size_Cast
           | N_Null
           | N_This
           | N_Super
           | N_Default
           | N_Aggregate_Literal
           | N_Aggregate_Literal_Cst
           | N_Aggregate_Element
           | N_Value_Range
           | N_Left_Streaming_Type
           | N_Right_Streaming_Type
           | N_Concatenation
           | N_Membership
           | N_Replication_Cst
           | N_Cond_Op
           | N_Call
           | N_Array_Method_Call
           | N_Randomize_Call
           | N_System_Call
           | N_Bits_Expr
           | N_Bits_Type
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Unary_Op
           | N_Post_Increment
           | N_Pre_Increment
           | N_Post_Decrement
           | N_Pre_Decrement
           | N_Access_Call
           | N_Conversion
           | N_Specparam
           | N_Pulse_Control_Specparam
           | N_Member
           | N_Packed_Member =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Owner;

   function Has_Type_Owner_2 (K : Nkind) return Boolean is
   begin
      case K is
         when N_Associative_Array
           | N_Left_Streaming_Type
           | N_Right_Streaming_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Owner_2;

   function Has_Forward_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Forward_Type;

   function Has_Enum_Names (K : Nkind) return Boolean is
   begin
      return K = N_Enum_Type;
   end Has_Enum_Names;

   function Has_Index_Data_Type (K : Nkind) return Boolean is
   begin
      return K = N_Associative_Array;
   end Has_Index_Data_Type;

   function Has_Type_Index_Type (K : Nkind) return Boolean is
   begin
      return K = N_Associative_Array_Cst;
   end Has_Type_Index_Type;

   function Has_Type_Argument (K : Nkind) return Boolean is
   begin
      return K = N_Bits_Type;
   end Has_Type_Argument;

   function Has_Type_Signed (K : Nkind) return Boolean is
      pragma Unreferenced (K);
   begin
      return False;
   end Has_Type_Signed;

   function Has_Subroutine (K : Nkind) return Boolean is
   begin
      case K is
         when N_Call
           | N_Array_Method_Call
           | N_Randomize_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Subroutine;

   function Has_Object (K : Nkind) return Boolean is
   begin
      case K is
         when N_Call
           | N_Array_Method_Call
           | N_Randomize_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Object;

   function Has_With_Expression (K : Nkind) return Boolean is
   begin
      case K is
         when N_Array_Method_Call
           | N_Randomize_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_With_Expression;

   function Has_Drive_Strength (K : Nkind) return Boolean is
   begin
      case K is
         when N_Assign
           | N_Decl_Assign
           | N_Primitive_Instance
           | N_Gate_And
           | N_Gate_Nand
           | N_Gate_Or
           | N_Gate_Nor
           | N_Gate_Xor
           | N_Gate_Xnor
           | N_Gate_Buf
           | N_Gate_Not
           | N_Gate_Bufif0
           | N_Gate_Bufif1
           | N_Gate_Notif0
           | N_Gate_Notif1
           | N_Gate_Nmos
           | N_Gate_Pmos
           | N_Gate_Rnmos
           | N_Gate_Rpmos
           | N_Gate_Tran
           | N_Gate_Rtran
           | N_Gate_Tranif0
           | N_Gate_Tranif1
           | N_Gate_Rtranif0
           | N_Gate_Rtranif1
           | N_Gate_Cmos
           | N_Gate_Rcmos
           | N_Gate_Pullup
           | N_Gate_Pulldown =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Drive_Strength;

   function Has_Net_Drive_Strength (K : Nkind) return Boolean is
   begin
      case K is
         when N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Net_Drive_Strength;

   function Has_Charge_Strength (K : Nkind) return Boolean is
   begin
      return K = N_Trireg;
   end Has_Charge_Strength;

   function Has_Module (K : Nkind) return Boolean is
   begin
      case K is
         when N_Module_Instance
           | N_Primitive_Instance
           | N_Program_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Module;

   function Has_Class_Name (K : Nkind) return Boolean is
   begin
      return K = N_Class_Instance;
   end Has_Class_Name;

   function Has_Interface (K : Nkind) return Boolean is
   begin
      return K = N_Virtual_Interface;
   end Has_Interface;

   function Has_Interface_Name (K : Nkind) return Boolean is
   begin
      return K = N_Interface_Instance;
   end Has_Interface_Name;

   function Has_Instance (K : Nkind) return Boolean is
   begin
      case K is
         when N_Virtual_Interface
           | N_Module_Instance
           | N_Program_Instance =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Instance;

   function Has_Instance_Ref (K : Nkind) return Boolean is
   begin
      return K = N_Interface_Instance;
   end Has_Instance_Ref;

   function Has_Port (K : Nkind) return Boolean is
   begin
      case K is
         when N_Port_Branch
           | N_Port_Connection
           | N_Implicit_Connection
           | N_Default_Connection
           | N_Argument =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Port;

   function Has_Collapse_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Port_Connection
           | N_Implicit_Connection =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Collapse_Flag;

   function Has_Unary_Op (K : Nkind) return Boolean is
   begin
      return K = N_Unary_Op;
   end Has_Unary_Op;

   function Has_Binary_Op (K : Nkind) return Boolean is
   begin
      case K is
         when N_Assign_Operator
           | N_Binary_Op
           | N_Short_Circuit_Op =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Binary_Op;

   function Has_Conversion_Op (K : Nkind) return Boolean is
   begin
      case K is
         when N_Parenthesis_Expr
           | N_Type_Cast
           | N_Size_Cast
           | N_Conversion =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Conversion_Op;

   function Has_Declaration (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class_Instance
           | N_Name
           | N_This_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item
           | N_Property_Name
           | N_Class_Qualified_Name
           | N_Method_Name
           | N_Member_Name
           | N_Hierarchical
           | N_This
           | N_Super =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Declaration;

   function Has_Redeclaration (K : Nkind) return Boolean is
   begin
      case K is
         when N_Input
           | N_Inout
           | N_Output
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Redeclaration;

   function Has_This_Declaration (K : Nkind) return Boolean is
   begin
      return K = N_This_Name;
   end Has_This_Declaration;

   function Has_Default_Value (K : Nkind) return Boolean is
   begin
      case K is
         when N_Input
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Default_Value;

   function Has_Instantiated_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Foreign_Module
           | N_Module
           | N_Interface_Declaration
           | N_Program_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Instantiated_Flag;

   function Has_Ansi_Port_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Foreign_Module
           | N_Module
           | N_Primitive
           | N_Interface_Declaration
           | N_Program_Declaration
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Ansi_Port_Flag;

   function Has_Event (K : Nkind) return Boolean is
   begin
      case K is
         when N_Clocking
           | N_Default_Clocking
           | N_Trigger =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Event;

   function Has_Min_Expr (K : Nkind) return Boolean is
   begin
      return K = N_Mintypmax;
   end Has_Min_Expr;

   function Has_Typ_Expr (K : Nkind) return Boolean is
   begin
      return K = N_Mintypmax;
   end Has_Typ_Expr;

   function Has_Max_Expr (K : Nkind) return Boolean is
   begin
      return K = N_Mintypmax;
   end Has_Max_Expr;

   function Has_Udp_Port_Declaration_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Primitive;
   end Has_Udp_Port_Declaration_Chain;

   function Has_Udp_Kind (K : Nkind) return Boolean is
   begin
      return K = N_Primitive;
   end Has_Udp_Kind;

   function Has_Udp_Initial (K : Nkind) return Boolean is
   begin
      return K = N_Primitive;
   end Has_Udp_Initial;

   function Has_Udp_Entries_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Primitive;
   end Has_Udp_Entries_Chain;

   function Has_Input_Chain (K : Nkind) return Boolean is
   begin
      case K is
         when N_Udp_Combinational_Entry
           | N_Udp_Sequential_Entry =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Input_Chain;

   function Has_Output_Symbol (K : Nkind) return Boolean is
   begin
      return K = N_Udp_Combinational_Entry;
   end Has_Output_Symbol;

   function Has_Current_State (K : Nkind) return Boolean is
   begin
      return K = N_Udp_Sequential_Entry;
   end Has_Current_State;

   function Has_Next_State (K : Nkind) return Boolean is
   begin
      return K = N_Udp_Sequential_Entry;
   end Has_Next_State;

   function Has_Symbol (K : Nkind) return Boolean is
   begin
      return K = N_Udp_Level_Symbol;
   end Has_Symbol;

   function Has_From_Symbol (K : Nkind) return Boolean is
   begin
      return K = N_Udp_Change_Symbol;
   end Has_From_Symbol;

   function Has_To_Symbol (K : Nkind) return Boolean is
   begin
      return K = N_Udp_Change_Symbol;
   end Has_To_Symbol;

   function Has_Specify_Input (K : Nkind) return Boolean is
   begin
      case K is
         when N_Par_Path
           | N_Full_Path
           | N_Par_Edge_Path
           | N_Full_Edge_Path =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Specify_Input;

   function Has_Specify_Output (K : Nkind) return Boolean is
   begin
      case K is
         when N_Par_Path
           | N_Full_Path
           | N_Par_Edge_Path
           | N_Full_Edge_Path =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Specify_Output;

   function Has_Path_Delay (K : Nkind) return Boolean is
   begin
      case K is
         when N_Par_Path
           | N_Full_Path
           | N_Par_Edge_Path
           | N_Full_Edge_Path =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Path_Delay;

   function Has_Data_Source (K : Nkind) return Boolean is
   begin
      case K is
         when N_Par_Edge_Path
           | N_Full_Edge_Path =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Data_Source;

   function Has_Polarity (K : Nkind) return Boolean is
   begin
      case K is
         when N_Par_Path
           | N_Full_Path
           | N_Par_Edge_Path
           | N_Full_Edge_Path =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Polarity;

   function Has_Delay_Rise (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay3;
   end Has_Delay_Rise;

   function Has_Delay_Fall (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay3;
   end Has_Delay_Fall;

   function Has_Delay_Z (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay3;
   end Has_Delay_Z;

   function Has_Delay_01 (K : Nkind) return Boolean is
   begin
      case K is
         when N_Path_Delay6
           | N_Path_Delay12 =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Delay_01;

   function Has_Delay_10 (K : Nkind) return Boolean is
   begin
      case K is
         when N_Path_Delay6
           | N_Path_Delay12 =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Delay_10;

   function Has_Delay_0z (K : Nkind) return Boolean is
   begin
      case K is
         when N_Path_Delay6
           | N_Path_Delay12 =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Delay_0z;

   function Has_Delay_z1 (K : Nkind) return Boolean is
   begin
      case K is
         when N_Path_Delay6
           | N_Path_Delay12 =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Delay_z1;

   function Has_Delay_1z (K : Nkind) return Boolean is
   begin
      case K is
         when N_Path_Delay6
           | N_Path_Delay12 =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Delay_1z;

   function Has_Delay_z0 (K : Nkind) return Boolean is
   begin
      case K is
         when N_Path_Delay6
           | N_Path_Delay12 =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Delay_z0;

   function Has_Delay_0x (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay12;
   end Has_Delay_0x;

   function Has_Delay_x1 (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay12;
   end Has_Delay_x1;

   function Has_Delay_1x (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay12;
   end Has_Delay_1x;

   function Has_Delay_x0 (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay12;
   end Has_Delay_x0;

   function Has_Delay_xz (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay12;
   end Has_Delay_xz;

   function Has_Delay_zx (K : Nkind) return Boolean is
   begin
      return K = N_Path_Delay12;
   end Has_Delay_zx;

   function Has_String_Id (K : Nkind) return Boolean is
   begin
      return K = N_String_Literal;
   end Has_String_Id;

   function Has_Label (K : Nkind) return Boolean is
   begin
      return K = N_Goto;
   end Has_Label;

   function Has_Label_Number (K : Nkind) return Boolean is
   begin
      return K = N_Label;
   end Has_Label_Number;

   function Has_Label_Chain (K : Nkind) return Boolean is
   begin
      return K = N_Label;
   end Has_Label_Chain;

   function Has_Label_Use (K : Nkind) return Boolean is
   begin
      return K = N_Label;
   end Has_Label_Use;

   function Has_Suspend_Flag (K : Nkind) return Boolean is
   begin
      return K = N_Goto;
   end Has_Suspend_Flag;

   function Has_Same_Case_Flag (K : Nkind) return Boolean is
   begin
      return K = N_Case_Item;
   end Has_Same_Case_Flag;

   function Has_Obj_Id (K : Nkind) return Boolean is
   begin
      case K is
         when N_Port
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Localparam
           | N_Var
           | N_Return_Var
           | N_This_Var
           | N_Iterator_Argument
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Foreach_Variable
           | N_Interface_Instance
           | N_Repeat
           | N_Specparam
           | N_Pulse_Control_Specparam
           | N_Member =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Obj_Id;

   function Has_Scope_Id (K : Nkind) return Boolean is
   begin
      case K is
         when N_Struct_Type
           | N_Union_Type
           | N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Compilation_Unit
           | N_Foreign_Module
           | N_Module
           | N_Interface_Declaration
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Seq_Block
           | N_Par_Block =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Scope_Id;

   function Has_Process_Id (K : Nkind) return Boolean is
   begin
      case K is
         when N_Always
           | N_Always_Comb
           | N_Always_Latch
           | N_Always_Ff
           | N_Initial
           | N_Final
           | N_Debug
           | N_Assert_Property
           | N_Assume_Property =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Process_Id;

   function Has_Sys_Tf_Id (K : Nkind) return Boolean is
   begin
      return K = N_System_Call;
   end Has_Sys_Tf_Id;

   function Has_Lit_Id (K : Nkind) return Boolean is
   begin
      return K = N_String_Literal;
   end Has_Lit_Id;

   function Has_Generate_Block (K : Nkind) return Boolean is
   begin
      return K = N_Loop_Generate;
   end Has_Generate_Block;

   function Has_Input_Skew (K : Nkind) return Boolean is
   begin
      case K is
         when N_Clock_Var
           | N_Default_Skew =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Input_Skew;

   function Has_Output_Skew (K : Nkind) return Boolean is
   begin
      case K is
         when N_Clock_Var
           | N_Default_Skew =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Output_Skew;

   function Has_Delay_Control (K : Nkind) return Boolean is
   begin
      return K = N_Clocking_Skew;
   end Has_Delay_Control;

   function Has_Attribute_Item (K : Nkind) return Boolean is
   begin
      return K = N_Attribute;
   end Has_Attribute_Item;

   function Has_Has_Identifier_List (K : Nkind) return Boolean is
   begin
      case K is
         when N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Var
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Member
           | N_Packed_Member =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Identifier_List;

   function Has_Has_Sign (K : Nkind) return Boolean is
   begin
      case K is
         when N_Packed_Array
           | N_Array
           | N_Packed_Struct_Type
           | N_Packed_Union_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Sign;

   function Has_Connected_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Port
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Connected_Flag;

   function Has_Complete_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Complete_Flag;

   function Has_Implicit_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Implicit_Flag;

   function Has_Redeclaration_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Var
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Redeclaration_Flag;

   function Has_Is_Automatic (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Parameter
           | N_Localparam
           | N_Var
           | N_Return_Var
           | N_This_Var
           | N_Iterator_Argument
           | N_Wire_Direct
           | N_Wire
           | N_Tri
           | N_Wand
           | N_Triand
           | N_Wor
           | N_Trior
           | N_Tri0
           | N_Tri1
           | N_Supply0
           | N_Supply1
           | N_Uwire
           | N_Trireg
           | N_Foreach_Variable
           | N_Seq_Block
           | N_Par_Block
           | N_If
           | N_For
           | N_While
           | N_Do_While
           | N_Foreach
           | N_Repeat
           | N_Forever
           | N_Wait
           | N_Case
           | N_Casex
           | N_Casez
           | N_Label_Stmt
           | N_Simple_Immediate_Assert
           | N_Specparam
           | N_Pulse_Control_Specparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Is_Automatic;

   function Has_Lifetime (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Package
           | N_Program_Declaration
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Input
           | N_Inout
           | N_Output
           | N_Interface_Port
           | N_Modport_Port
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Var
           | N_Seq_Block
           | N_Par_Block
           | N_If
           | N_For
           | N_While
           | N_Do_While
           | N_Foreach
           | N_Repeat
           | N_Forever
           | N_Wait
           | N_Case
           | N_Casex
           | N_Casez
           | N_Case_Item
           | N_Default_Case_Item
           | N_Label_Stmt
           | N_Simple_Immediate_Assert =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Lifetime;

   function Has_Has_Lifetime (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Package
           | N_Program_Declaration
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Var =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Lifetime;

   function Has_Has_End_Name (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Module
           | N_Primitive
           | N_Interface_Declaration
           | N_Package
           | N_Program_Declaration
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Clocking
           | N_Default_Clocking
           | N_Property_Declaration
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Seq_Block
           | N_Par_Block =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_End_Name;

   function Has_Call (K : Nkind) return Boolean is
   begin
      return K = N_Subroutine_Call_Stmt;
   end Has_Call;

   function Has_Timeunit (K : Nkind) return Boolean is
   begin
      return K = N_Timeunit;
   end Has_Timeunit;

   function Has_Timeprecision (K : Nkind) return Boolean is
   begin
      case K is
         when N_Timeunit
           | N_Timeprecision =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Timeprecision;

   function Has_Error_Origin (K : Nkind) return Boolean is
   begin
      return K = N_Error_Expr;
   end Has_Error_Origin;

   function Has_Has_Void_Cast (K : Nkind) return Boolean is
   begin
      return K = N_Subroutine_Call_Stmt;
   end Has_Has_Void_Cast;

   function Has_Is_Const (K : Nkind) return Boolean is
   begin
      return K = N_Var;
   end Has_Is_Const;

   function Has_Has_Var (K : Nkind) return Boolean is
   begin
      return K = N_Var;
   end Has_Has_Var;

   function Has_Has_Type (K : Nkind) return Boolean is
   begin
      case K is
         when N_Type_Parameter
           | N_Type_Localparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Type;

   function Has_Has_Direction (K : Nkind) return Boolean is
   begin
      case K is
         when N_Input
           | N_Inout
           | N_Output
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Direction;

   function Has_Has_Parenthesis (K : Nkind) return Boolean is
   begin
      case K is
         when N_New_Call
           | N_Call
           | N_Array_Method_Call
           | N_Randomize_Call
           | N_System_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Parenthesis;

   function Has_Has_Argument (K : Nkind) return Boolean is
   begin
      return K = N_Array_Method_Call;
   end Has_Has_Argument;

   function Has_Fully_Analyzed_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Parameter
           | N_Localparam
           | N_Specparam
           | N_Pulse_Control_Specparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Fully_Analyzed_Flag;

   function Has_Resolved_Flag (K : Nkind) return Boolean is
   begin
      return K = N_Typedef;
   end Has_Resolved_Flag;

   function Has_Mark_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Import_DPI_Function
           | N_Parameter
           | N_Localparam
           | N_Typedef
           | N_Specparam
           | N_Pulse_Control_Specparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Mark_Flag;

   function Has_Is_Constant (K : Nkind) return Boolean is
   begin
      case K is
         when N_Error_Expr
           | N_Parameter
           | N_Localparam
           | N_Name
           | N_This_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item
           | N_Property_Name
           | N_Class_Qualified_Name
           | N_Method_Name
           | N_Member_Name
           | N_Hierarchical
           | N_Number
           | N_Computed_Number
           | N_Bignum
           | N_Unbased_Literal
           | N_Time_Literal
           | N_Step_Literal
           | N_Infinity
           | N_Real_Number
           | N_Scale_Number
           | N_Mintypmax
           | N_Bit_Select
           | N_Indexed_Name
           | N_String_Index
           | N_Associative_Index
           | N_Part_Select_Cst
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst
           | N_Slice_Name_Cst
           | N_String_Literal
           | N_Parenthesis_Expr
           | N_Type_Cast
           | N_Size_Cast
           | N_Concatenation
           | N_Membership
           | N_Replication_Cst
           | N_Cond_Op
           | N_Call
           | N_Randomize_Call
           | N_System_Call
           | N_Bits_Expr
           | N_Bits_Type
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Unary_Op
           | N_Access_Call
           | N_Specparam
           | N_Pulse_Control_Specparam =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Is_Constant;

   function Has_Static_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Var =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Static_Flag;

   function Has_Has_Attribute (K : Nkind) return Boolean is
   begin
      case K is
         when N_Input
           | N_Inout
           | N_Output
           | N_Tf_Input
           | N_Tf_Inout
           | N_Tf_Output
           | N_Tf_Ref
           | N_Tf_Const_Ref
           | N_Case
           | N_Casex
           | N_Casez =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Attribute;

   function Has_Attribute_Full (K : Nkind) return Boolean is
   begin
      case K is
         when N_Case
           | N_Casex
           | N_Casez =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Attribute_Full;

   function Has_Attribute_Parallel (K : Nkind) return Boolean is
   begin
      case K is
         when N_Case
           | N_Casex
           | N_Casez =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Attribute_Parallel;

   function Has_Other_Attributes (K : Nkind) return Boolean is
   begin
      case K is
         when N_Case
           | N_Casex
           | N_Casez =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Other_Attributes;

   function Has_Pure_Property (K : Nkind) return Boolean is
   begin
      return K = N_Import_DPI_Function;
   end Has_Pure_Property;

   function Has_Context_Property (K : Nkind) return Boolean is
   begin
      return K = N_Import_DPI_Function;
   end Has_Context_Property;

   function Has_Has_Extern_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Extern_Flag;

   function Has_Virtual_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Virtual_Flag;

   function Has_Pure_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Pure_Flag;

   function Has_Join_Option (K : Nkind) return Boolean is
   begin
      return K = N_Par_Block;
   end Has_Join_Option;

   function Has_Edge_Identifier (K : Nkind) return Boolean is
   begin
      return K = N_Clocking_Skew;
   end Has_Edge_Identifier;

   function Has_DPI_Spec (K : Nkind) return Boolean is
   begin
      case K is
         when N_Import_DPI_Function
           | N_Export_DPI_Function
           | N_Export_DPI_Task =>
            return True;
         when others =>
            return False;
      end case;
   end Has_DPI_Spec;

   function Has_Visibility (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Var =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Visibility;

   function Has_Class_Visibility (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Class_Visibility;

   function Has_Has_Visibility (K : Nkind) return Boolean is
   begin
      case K is
         when N_Task
           | N_Function
           | N_OOB_Task
           | N_OOB_Function
           | N_Extern_Task
           | N_Extern_Function
           | N_Var =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Has_Visibility;

   function Has_Violation (K : Nkind) return Boolean is
   begin
      case K is
         when N_If
           | N_Case
           | N_Casex
           | N_Casez =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Violation;

   function Has_Random_Flag (K : Nkind) return Boolean is
   begin
      return K = N_Var;
   end Has_Random_Flag;

   function Has_Randc_Flag (K : Nkind) return Boolean is
   begin
      return K = N_Var;
   end Has_Randc_Flag;

   function Has_Size_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Logic_Type
           | N_Bit_Type
           | N_Real_Type
           | N_Shortreal_Type
           | N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst
           | N_Struct_Type
           | N_Packed_Struct_Type
           | N_Union_Type
           | N_Packed_Union_Type
           | N_Queue_Cst
           | N_Dynamic_Array_Cst
           | N_Associative_Array_Cst
           | N_Enum_Type
           | N_String_Type
           | N_Chandle_Type
           | N_Event_Type
           | N_Void_Type
           | N_Error_Type
           | N_Null_Type
           | N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Size_Flag;

   function Has_Type_Analyzed_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Type_Analyzed_Flag;

   function Has_Forward_Typedef_Flag (K : Nkind) return Boolean is
   begin
      case K is
         when N_Class
           | N_Instantiated_Class
           | N_Generic_Class
           | N_Typedef
           | N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Forward_Typedef_Flag;

   function Has_Access (K : Nkind) return Boolean is
   begin
      return K = N_Access_Call;
   end Has_Access;

   function Has_Arg1 (K : Nkind) return Boolean is
   begin
      case K is
         when N_Branch
           | N_Access_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Arg1;

   function Has_Arg2 (K : Nkind) return Boolean is
   begin
      case K is
         when N_Branch
           | N_Access_Call =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Arg2;

end Verilog.Nodes_Meta;
