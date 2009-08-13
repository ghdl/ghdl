--  Internal node type and operations.
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

package Nodes is
   type Node_Type is new Int32;
   for Node_Type'Size use 32;

   Null_Node : constant Node_Type := 0;
   Error_Node : constant Node_Type := 1;

   --  A simple type that needs only 2 bits.
   type Bit2_Type is range 0 .. 2 ** 2 - 1;
   type Bit3_Type is range 0 .. 2 ** 3 - 1;

   type Kind_Type is range 0 .. 255;

   --  Format of a node.
   type Format_Type is
     (
      Format_Short,
      Format_Medium,
      Format_Fp,
      Format_Int
     );

   --  Future layout:    (rem)
   --   Format: 0 bits    32
   --   Nkind: 16 bits    16
   --   Flags: 8*1 bits    8
   --   State: 2*2 bits    4
   --   Odigit is to be removed.

   --  Future layout (2):(rem)
   --   Format: 2 bits    30
   --   Nkind:  8 bits    22 (vhdl: 216 nodes)
   --   Flags: 8*1 bits   14
   --   State: 2*2 bits   10
   --   Lang:   2 bits     8
   --   Odigit: 1*3 bits   5

   -- Common fields are:
   --   Flag1 : Boolean
   --   Flag2 : Boolean
   --   Flag3 : Boolean
   --   Flag4 : Boolean
   --   Flag5 : Boolean
   --   Flag6 : Boolean
   --   Flag7 : Boolean
   --   Nkind : Kind_Type
   --   State1 : Bit2_Type
   --   State2 : Bit2_Type
   --   Location : Location_Type
   --   Field0 : Iir
   --   Field1 : Iir
   --   Field2 : Iir
   --   Field3 : Iir

   -- Fields of Format_Fp:
   --   Fp64 : Iir_Fp64

   -- Fields of Format_Int:
   --   Int64 : Iir_Int64

   -- Fields of Format_Short:
   --   Field4 : Iir
   --   Field5 : Iir

   -- Fields of Format_Medium:
   --   Odigit1 : Bit3_Type
   --   Odigit2 : Bit3_Type (odigit1)
   --   State3 : Bit2_Type
   --   State4 : Bit2_Type
   --   Field4 : Iir
   --   Field5 : Iir
   --   Field6 : Iir (location)
   --   Field7 : Iir (field0)
   --   Field8 : Iir (field1)
   --   Field9 : Iir (field2)
   --   Field10 : Iir (field3)
   --   Field11 : Iir (field4)
   --   Field12 : Iir (field5)

   function Create_Node (Format : Format_Type) return Node_Type;
   procedure Free_Node (N : Node_Type);

   function Get_Nkind (N : Node_Type) return Kind_Type;
   pragma Inline (Get_Nkind);
   procedure Set_Nkind (N : Node_Type; Kind : Kind_Type);
   pragma Inline (Set_Nkind);

   function Get_Location (N: Node_Type) return Location_Type;
   pragma Inline (Get_Location);
   procedure Set_Location (N : Node_Type; Location: Location_Type);
   pragma Inline (Set_Location);

   function Get_Field0 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field0);
   procedure Set_Field0 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field0);

   function Get_Field1 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field1);
   procedure Set_Field1 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field1);

   function Get_Field2 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field2);
   procedure Set_Field2 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field2);

   function Get_Field3 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field3);
   procedure Set_Field3 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field3);

   function Get_Field4 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field4);
   procedure Set_Field4 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field4);


   function Get_Field5 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field5);
   procedure Set_Field5 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field5);

   function Get_Field6 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field6);
   procedure Set_Field6 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field6);

   function Get_Field7 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field7);
   procedure Set_Field7 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field7);

   function Get_Field8 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field8);
   procedure Set_Field8 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field8);

   function Get_Field9 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field9);
   procedure Set_Field9 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field9);

   function Get_Field10 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field10);
   procedure Set_Field10 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field10);

   function Get_Field11 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field11);
   procedure Set_Field11 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field11);

   function Get_Field12 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field12);
   procedure Set_Field12 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field12);


   function Get_Flag1 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag1);
   procedure Set_Flag1 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag1);

   function Get_Flag2 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag2);
   procedure Set_Flag2 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag2);

   function Get_Flag3 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag3);
   procedure Set_Flag3 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag3);

   function Get_Flag4 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag4);
   procedure Set_Flag4 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag4);

   function Get_Flag5 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag5);
   procedure Set_Flag5 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag5);

   function Get_Flag6 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag6);
   procedure Set_Flag6 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag6);

   function Get_Flag7 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag7);
   procedure Set_Flag7 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag7);


   function Get_State1 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State1);
   procedure Set_State1 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State1);

   function Get_State2 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State2);
   procedure Set_State2 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State2);

   function Get_State3 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State3);
   procedure Set_State3 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State3);

   function Get_State4 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State4);
   procedure Set_State4 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State4);


   function Get_Odigit1 (N : Node_Type) return Bit3_Type;
   pragma Inline (Get_Odigit1);
   procedure Set_Odigit1 (N : Node_Type; V : Bit3_Type);
   pragma Inline (Set_Odigit1);

   function Get_Odigit2 (N : Node_Type) return Bit3_Type;
   pragma Inline (Get_Odigit2);
   procedure Set_Odigit2 (N : Node_Type; V : Bit3_Type);
   pragma Inline (Set_Odigit2);


   function Get_Fp64 (N : Node_Type) return Iir_Fp64;
   pragma Inline (Get_Fp64);
   procedure Set_Fp64 (N : Node_Type; V : Iir_Fp64);
   pragma Inline (Set_Fp64);

   function Get_Int64 (N : Node_Type) return Iir_Int64;
   pragma Inline (Get_Int64);
   procedure Set_Int64 (N : Node_Type; V : Iir_Int64);
   pragma Inline (Set_Int64);

   --  Get the last node allocated.
   function Get_Last_Node return Node_Type;
   pragma Inline (Get_Last_Node);

   --  Free all and reinit.
   procedure Initialize;
private
   type Node_Record (Format : Format_Type := Format_Short) is record

      --  Usages of Flag1:
      --  seen_flag for iir_kind_process_statement
      --  seen_flag for iir_kind_sensitized_process_statement
      --  seen_flag for iir_kinds_procedure_specification
      --  seen_flag for iir_kinds_function_specification
      --  seen_flag for iir_kind_design_file
      --  deferred_declaration_flag for iir_kind_constant_declaration
      --  loaded_flag for iir_kind_design_unit
      --  resolved_flag for iir_kinds_type_definition
      --  need_body for iir_kind_package_declaration
      --  whole_association_flag for iir_kind_association_element_by_expression
      --  has_disconnect_flag for iir_kind_signal_declaration
      Flag1 : Boolean := False;

      --  Usages of Flag2:
      --  pure_flag for iir_kinds_function_specification
      --  passive_flag for iir_kinds_process_statement
      --  shared_flag for iir_kind_variable_declaration
      --  aggr_others_flag for iir_kind_aggregate_info
      --  signal_type_flag for iir_kinds_type_definition
      Flag2 : Boolean := False;

      --  Usages of Flag3:
      --  (postponed_flag for iir_kinds_process_statement)
      --  elab_flag for iir_kind_design_file
      --  elab_flag for iir_kind_design_unit
      --  dynamic_flag for iir_kind_aggregate_info
      --  text_file_flag for iir_kind_file_type_definition
      --  foreign_flag for iir_kind_architecture_declaration
      --  foreign_flag for iir_kinds_function_specification
      --  foreign_flag for iir_kinds_procedure_specification
      Flag3 : Boolean := False;

      --  Usages of Flag4:
      --  visible_flag for iir_kind_type_declaration
      --  aggr_named_flag for iir_kind_aggregate_info
      Flag4 : Boolean := False;

      --  Usages of Flag5:
      --  is_within_flag for named entities
      Flag5 : Boolean := False;

      --  Usages of Flag6:
      Flag6 : Boolean := False;

      --  Kind field use 8 bits.
      --  So, on 32 bits systems, there are 24 bits left.
      --  + 8 (8 * 1)
      --  + 10 (5 * 2)
      --  + 6 (2 * 3)
      --  = 24

      Kind : Kind_Type;

      -- expr_staticness for iir_kind_string_literal
      -- expr_staticness for iir_kind_bit_string_literal
      -- expr_staticness for iir_kind_integer_literal
      -- expr_staticness for iir_kind_floating_point_literal
      -- expr_staticness for iir_kind_physical_int_literal
      -- expr_staticness for iir_kind_physical_fp_literal
      -- expr_staticness for iir_kind_enumeration_literal
      -- expr_staticness for iir_kind_monadic_operator
      -- expr_staticness for iir_kind_dyadic_operator
      -- expr_staticness for iir_kinds_name
      -- expr_staticness for iir_kinds_alias_declaration
      -- expr_staticness for iir_kind_constant_declaration
      -- expr_staticness for iir_kind_iterator_declaration
      -- expr_staticness for iir_kind_constant_interface_declaration
      -- expr_staticness for iir_kind_aggregate
      -- expr_staticness for iir_kind_qualified_expression
      -- expr_staticness for iir_kind_type_conversion
      -- expr_staticness for iir_kind_length_array_attribute
      -- expr_staticness for iir_kind_low_type_attribute
      -- expr_staticness for iir_kind_high_type_attribute
      -- expr_staticness for iir_kind_left_type_attribute
      -- expr_staticness for iir_kind_right_type_attribute
      -- expr_staticness for iir_kind_pos_attribute
      -- expr_staticness for iir_kind_val_attribute
      -- expr_staticness for iir_kind_event_attribute
      -- expr_staticness for iir_kind_last_value_attribute
      -- expr_staticness for iir_kind_last_active_attribute
      -- expr_staticness for iir_kind_active_attribute
      -- expr_staticness for iir_kind_range_expression
      -- expr_staticness for iir_kind_selected_element
      -- expr_staticness for iir_kind_function_call
      -- expr_staticness for iir_kind_attribute_value
      -- expr_staticness for iir_kind_signal_declaration
      -- expr_staticness for iir_kind_guard_signal_declaration
      -- expr_staticness for iir_kind_variable_declaration
      -- expr_staticness for iir_kind_file_declaration
      -- expr_staticness for iir_kinds_discrete_type_attribute
      -- type_staticness for iir_kinds_type_and_subtype_definition
      State1 : Bit2_Type := 0;

      -- name_staticness for iir_kinds_name
      -- name_staticness for iir_kind_object_alias_declaration
      -- name_staticness for iir_kind_selected_element
      -- name_staticness for iir_kind_selected_by_all_name
      -- choice_staticness for iir_kind_choice_by_range
      -- choice_staticness for iir_kind_choice_by_expression
      State2 : Bit2_Type := 0;

      Flag7 : Boolean := False;
      Flag8 : Boolean := False;
      Flag9 : Boolean := False;
      Flag10 : Boolean := False;
      Flag11 : Boolean := False;
      Flag12 : Boolean := False;

      -- 3bits fields (1 -> 3 bits)
      --  Usages of odigit1:
      --  lexical_layout for iir_kinds_interface_declaration
      --  iir_mode
      Odigit1 : Bit3_Type := 0;

      Unused_Odigit2 : Bit3_Type := 0;

      -- Location.
      Location: Location_Type := Location_Nil;

      --  The parent node.
      -- parent for iir_kind_if_statement
      -- parent for iir_kind_elsif_statement
      -- parent for iir_kind_for_loop_statement
      -- parent for iir_kind_while_loop_statement
      -- parent for iir_kind_case_statement
      -- parent for iir_kind_exit_statement
      -- parent for iir_kind_next_statement
      -- parent (library_declaration) for iir_kind_design_file
      -- parent (design_unit_list) for iir_kind_design_file
      -- interface_parent for iir_kind_signal_interface_declaration
      -- interface_parent for iir_kind_constant_interface_declaration
      -- interface_parent for iir_kind_variable_interface_declaration
      -- interface_parent for iir_kind_file_interface_declaration
      Field0 : Node_Type := Null_Node;

      -- usages of field1:
      -- type for iir_kind_character_literal
      -- type for iir_kind_type_computed_literal
      -- type for iir_kind_integer_literal
      -- type for iir_kind_floating_point_literal
      -- type for iir_type_declaration.
      -- type for iir_subtype_declaration.
      -- type for iir_kind_identifier
      -- type for iir_kind_string_literal
      -- type for iir_kind_bit_string_literal
      -- type for iir_kind_base_attribute
      -- list_element for iir_kinds_list
      -- port_chain for iir_kind_entity_declaration
      -- port_chain for iir_kind_component_declaration
      -- port_chain for iir_kind_block_header
      -- entity for iir_kind_architecture_declaration
      -- entity for iir_kind_configuration_declaration
      -- entity for iir_kind_entity_aspect_entity
      -- package for iir_kind_package_body
      -- primary_units(iir_library_unit_list) for iir_kind_library_declaration
      -- selected_name for iir_kind_use_clause
      -- type_declaration for iir_kinds_type_definition
      -- type_definition for iir_kind_signal_declaration
      -- type_definition for iir_kind_guard_signal_declaration
      -- type_definition for iir_kind_signal_interface_declaration.
      -- type_definition for iir_kind_variable_declaration
      -- type_definition for iir_kind_variable_interface_declaration.
      -- type_definition for iir_kind_constant_declaration
      -- type_definition for iir_kind_iterator_declaration
      -- type_definition for iir_kind_constant_interface_declaration.
      -- type_definition for iir_kind_file_declaration
      -- type_definition for iir_kind_file_interface_declaration.
      -- type_definition for iir_kind_enumeration_literal
      -- type_definition for iir_kind_unit_declaration
      -- type_definition for iir_kind_component_port
      -- type_definition for iir_kind_element_declaration
      -- type_definition for iir_kinds_attribute_declaration
      -- type_definition for iir_kinds_attribute
      -- type_definition for iir_kinds_name
      -- type_definition for iir_kind_return_statement
      -- type_definition for iir_kind_aggregate
      -- type_definition for iir_kind_physical_int_literal
      -- type_definition for iir_kind_physical_fp_literal
      -- type_definition for iir_kind_object_alias_declaration
      -- type_definition for iir_kind_null_literal
      -- type_definition for iir_kind_qualified_expression
      -- type_definition for iir_kind_type_conversion
      -- type_definition for iir_kind_function_call
      -- type_definition for iir_kind_allocator_by_expression
      -- type_definition for iir_kind_allocator_by_subtype
      -- type_definition for iir_kind_attribute_value
      -- type_definition for iir_kind_selected_element
      -- type_definition for iir_kind_implicit_dereference.
      -- type_definition for iir_kind_disconnection_specification
      -- type_definition for iir_kinds_monadic_operator
      -- type_definition for iir_kinds_dyadic_operator
      -- null_iir for iir_kind_signal_assignment_statement
      -- null_iir for iir_kind_variable_assignment_statement
      -- we_value for iir_kind_waveform_element
      -- condition for iir_kind_conditional_waveform
      -- condition for iir_kind_if_statement
      -- condition for iir_kind_elsif
      -- condition for iir_kind_while_loop_statement
      -- condition for iir_kind_next_statement
      -- condition for iir_kind_exit_statement
      -- design_unit_chain for iir_kind_design_file
      -- formal for iir_kinds_association_element
      -- iterator_scheme for iir_kind_for_loop_statement
      -- associated for iir_kinds_association_by_choice
      -- context_items for iir_kind_design_unit
      -- design_file_chain for iir_kind_library_declaration
      -- proxy for iir_kind_proxy
      -- selected_waveform_l for iir_kind_concurrent_selected_signal_assignment
      -- block_specification for iir_kind_block_configuration
      -- instantiation_list for iir_kind_component_configuration
      -- instantiation_list for iir_kind_configuration_specification
      -- component_configuration for iir_kind_component_instantiation_statement
      -- configuration for iir_kind_entity_aspect_configuration
      -- guard_decl for iir_kind_block_statement
      -- entity_class_entry_chain for iir_kind_group_template_declaration
      -- group_constituent_chain for iir_kind_group_declaration
      -- entity_name_list for iir_kind_attribute_specification
      -- generate_block_configuration for iir_kind_generate_statement
      -- type_declarator for Iir_Kind_Enumeration_Type_Definition
      -- type_declarator for Iir_Kind_Enumeration_Subtype_Definition
      -- type_declarator for Iir_Kind_Integer_Type_Definition
      -- type_declarator for Iir_Kind_Integer_Subtype_Definition
      -- type_declarator for Iir_Kind_Floating_Type_Definition
      -- type_declarator for Iir_Kind_Floating_Subtype_Definition
      -- type_declarator for Iir_Kind_Physical_Type_Definition
      -- type_declarator for Iir_Kind_Physical_Subtype_Definition
      -- type_declarator for Iir_Kind_Record_Type_Definition
      -- type_declarator for Iir_Kind_Record_Subtype_Definition
      -- type_declarator for Iir_Kind_Array_Type_Definition
      -- type_declarator for Iir_Kind_Array_Subtype_Definition
      -- type_declarator for Iir_Kind_Unconstrained_Array_Subtype_Definition
      -- type_declarator for Iir_Kind_Access_Type_Definition
      -- type_declarator for Iir_Kind_Access_Subtype_Definition
      -- type_declarator for Iir_Kind_Incomplete_Type_Definition
      -- type_declarator for Iir_Kind_File_Type_Definition
      -- return_type for iir_kind_function_specification
      -- return_type for iir_kind_function_spec_body
      -- return_type for iir_kind_implicit_function_declaration
      -- default_entity_aspect for iir_kind_binding_indication
      -- sub_aggregate_info for iir_kind_aggregate_info
      Field1: Node_Type := Null_Node;

      -- usages of field2:
      -- concurrent_statement_list for iir_kind_architecture_declaration
      -- concurrent_statement_list for iir_kind_block_statement
      -- concurrent_statement_list for iir_kind_entity_declaration
      -- concurrent_statement_list for iir_kind_generate_statement
      -- block_configuration for iir_kind_configuration_declaration
      -- block_configuration for iir_kind_component_configuration
      -- subprogram_body for iir_kind_function_specification
      -- subprogram_body for iir_kind_procedure_specification
      -- range_constraint for iir_kind_integer_subtype_definition
      -- range_constraint for iir_kind_floating_subtype_definition
      -- range_constraint for iir_kind_subtype_definition
      -- range_constraint for iir_kind_enumeration_subtype_definition
      -- range_constraint for iir_kind_physical_subtype_definition
      -- range_constraint for iir_kind_enumeration_type_definition
      -- left_limit for iir_kind_range_expression
      -- designated_type for iir_kind_access_type_definition
      -- index_subtype for iir_array_type_definition
      -- index_subtype for iir_array_subtype_definition
      -- suffix for iir_kinds_attribute
      -- suffix for iir_kind_user_attribute
      -- suffix for iir_kind_slice_name
      -- selected_element for iir_kind_selected_element
      -- parameter for iir_kind_val_attribute
      -- parameter for iir_kind_pos_attribute
      -- parameter for iir_kind_delayed_attribute
      -- parameter for iir_kind_stable_attribute
      -- parameter for iir_kind_quiet_attribute
      -- parameter for iir_kind_attribute
      -- index_list for iir_kind_indexed_name
      -- index_list for iir_kind_array_type_definition
      -- index_list for iir_kind_array_subtype_definition
      -- target for iir_kind_signal_assignment_statement
      -- target for iir_kind_variable_assignment_statement
      -- time for iir_kind_waveform_element
      -- target for iir_kind_concurrent_conditional_signal_assignment
      -- target for iir_kind_concurrent_selected_signal_assignment
      -- assertion_condition for iir_kind_concurrent_assertion_statement
      -- assertion_condition for iir_kind_assertion_statement
      -- null_iir for iir_kind_conditional_waveform
      -- sequential_statement_chain for iir_kind_if_statement
      -- sequential_statement_chain for iir_kind_elsif
      -- sequential_statement_chain for iir_kind_sensitized_process_statement
      -- sequential_statement_chain for iir_kind_process_statement
      -- sequential_statement_chain for iir_kind_for_loop_statement
      -- sequential_statement_chain for iir_kind_while_loop_statement
      -- sequential_statement_chain for iir_kind_function_Body
      -- sequential_statement_chain for iir_kind_function_Spec_Body
      -- sequential_statement_chain for iir_kind_procedure_Body
      -- sequential_statement_chain for iir_kind_procedure_Spec_Body
      -- name for iir_kind_object_alias_declaration
      -- name for iir_kind_physical_int_literal
      -- name for iir_kind_physical_fp_literal
      -- name for iir_kind_association_choice_by_name
      -- name for iir_kind_group_declaration
      -- default_value for iir_kind_signal_declaration
      -- default_value for iir_kind_guard_signal_declaration
      -- default_value for iir_kind_variable_declaration
      -- default_value for iir_kind_constant_declaration
      -- default_value for iir_kind_signal_interface_declaration
      -- default_value for iir_kind_variable_interface_declaration
      -- default_value for iir_kind_constant_interface_declaration
      -- default_value for iir_kind_file_interface_declaration
      -- guard_expression for iir_kind_guard_signal_declaration
      -- operand for iir_kinds_monadic_operator
      -- left for iir_kinds_dyadic_operator
      -- actual for iir_kind_association_element_by_expression
      -- instantiated_unit for Iir_Kind_Component_Instantiation_Statement
      -- parameter_association_chain for iir_kind_function_call
      -- parameter_association_chain for iir_kind_procedure_call
      -- parameter_association_chain for iir_kind_concurrent_procedure_call_st.
      -- library_unit for iir_kind_design_unit
      -- multiplier for iir_kind_unit_declaration
      -- primary_unit for iir_kind_physical_type_definition
      -- condition_clause for iir_kind_wait_statement
      -- element_declaration_list for iir_kind_record_type_definition
      -- loop for iir_kind_exit_statement
      -- loop for iir_kind_next_statement
      -- file_logical_name for iir_kind_file_declaration
      -- configuration_item_chain for iir_kind_block_configuration
      -- architecture for iir_kind_entity_aspect_entity
      -- library_declaration for iir_kind_library_clause
      -- attribute_designator for iir_kind_attribute_specification
      -- attribute_specification for iir_kind_attribute_value
      -- signal_list for iir_kind_disconnection_specification
      -- generation_scheme for iir_kind_generate_statement
      -- incomplete_type_List for iir_kind_incomplete_type_definition
      -- file_time_stamp for iir_kind_design_file
      -- default_generic_map_aspect_list for iir_kind_binding_indication
      -- aggr_low_limit for iir_kind_aggregate_info
      -- enumeration_decl for iir_kind_enumeration_literal
      -- simple_aggregate_list for iir_kind_simple_aggregate
      Field2: Node_Type := Null_Node;

      -- Usages of field3:
      -- dependence_list for iir_kind_design_unit
      -- block_statement for iir_kind_signal_declaration
      -- block_statement for iir_kind_guard_signal_declaration
      -- subprogram_declaration for iir_kind_function_Spec_Body
      -- subprogram_declaration for iir_kind_function_Body
      -- subprogram_declaration for iir_kind_Procedure_Spec_Body
      -- subprogram_declaration for iir_kind_Procedure_Body
      -- body for iir_kind_function_specification
      -- body for iir_kind_procedure_specification
      -- declaration_list for iir_kind_entity_declaration
      -- declaration_list for iir_kind_architecture_declaration
      -- declaration_list for iir_kind_configuration_declaration
      -- declaration_list for iir_kind_block_statement
      -- declaration_list for iir_kind_package_declaration
      -- declaration_list for iir_kind_package_body
      -- declaration_list for iir_kind_sensitized_process_statement
      -- declaration_list for iir_kind_process_statement
      -- declaration_list for iir_kind_block_configuration
      -- declaration_list for iir_kind_generate_statement
      -- enumeration_literal_list for iir_enumeration_type_definition
      -- right_limit for iir_kind_range_expression
      -- element_subtype for iir_array_type_definition
      -- element_subtype for iir_array_subtype_definition
      -- report_expression for iir_kind_concurrent_assertion_statement
      -- report_expression for iir_kind_assertion_statement
      -- report_expression for iir_kind_report_statement
      -- waveform_chain for iir_kind_signal_assignment_statement
      -- conditional_waveform_chain for iir_kind_conc_conditional_signal_assign
      -- waveform_chain for iir_kind_conditional_waveform
      -- else_clause for iir_kind_if_statement
      -- else_clause for iir_kind_elsif
      -- expression of iir_kind_concurrent_selected_signal_assignment
      -- expression of iir_kind_variable_assignment_statement
      -- prefix for iir_kinds_attribute
      -- prefix for iir_kind_indexed_name
      -- prefix for iir_kind_slice_name
      -- prefix for iir_kind_selected_name
      -- prefix for iir_kind_selected_by_all_name
      -- prefix for iir_kind_parenthesis_name
      -- prefix for iir_kind_selected_element
      -- prefix for iir_kind_implicit_dereference
      -- port_map_aspect for Iir_Kind_Component_Instantiation_Statement
      -- port_map_aspect for Iir_Kind_binding_indication
      -- port_map_aspect for Iir_Kind_block_header
      -- binding_indication for iir_kind_Component_configuration
      -- binding_indication for Iir_Kind_Configuration_specifiation
      -- expression for iir_kind_return_statement
      -- expression for iir_kind_association_choice_by_expression
      -- expression for iir_kind_case_statement
      -- expression for iir_kind_qualified_expression
      -- expression for iir_kind_type_conversion
      -- expression for iir_kind_allocator_by_expression
      -- expression for iir_kind_allocator_by_subtype
      -- expression for iir_kind_attribute_specification
      -- expression for iir_kind_disconnection_specification
      -- unit_chain for iir_kind_physical_type_definition
      -- timeout_clause for iir_kind_wait_statement
      -- file_open_kind for iir_kind_file_declaration
      -- designated_entity for iir_kind_attribute_value
      -- associated_formal for iir_kinds_association_element
      -- deferred_declaration for iir_kind_constant_declaration
      -- literal_origin for iir_kind_character_literal
      -- literal_origin for iir_kind_string_literal
      -- literal_origin for iir_kind_bit_string_literal
      -- literal_origin for iir_kind_integer_literal
      -- literal_origin for iir_kind_floating_point_literal
      -- literal_origin for iir_kind_physical_int_literal
      -- literal_origin for iir_kind_physical_fp_literal
      -- literal_origin for iir_kind_enumeration_literal
      -- analysis_time_stamp for iir_kind_design_file
      -- aggr_high_limit for iir_kind_aggregate_info
      -- aggregate_info for iir_kind_aggregate
      -- implementation for iir_kind_function_call
      -- implementation for iir_kind_procedure_call
      -- implementation for iir_kind_concurrent_procedure_call_statement
      -- implementation for iir_kind_dyadic_operator
      -- implementation for iir_kind_monadic_operator
      Field3: Node_Type := Null_Node;

      -- Usages of field4:
      -- design_file for iir_kind_design_unit
      -- generic_chain for iir_kind_entity_declaration
      -- generic_chain for iir_kind_component_declaration
      -- generic_chain for iir_kind_block_header
      -- base_type for iir_kind_integer_type_definition
      -- base_type for iir_kind_integer_subtype_definition
      -- base_type for iir_kind_floating_type_definition
      -- base_type for iir_kind_floating_subtype_definition
      -- base_type for iir_kind_subtype_definition
      -- base_type for iir_kind_enumeration_type_definition
      -- base_type for iir_kind_enumeration_subtype_definition
      -- base_type for iir_kind_array_type_definition
      -- base_type for iir_kind_array_subtype_definition
      -- base_type for iir_kind_unconstrained_array_subtype_definition
      -- base_type for iir_kind_range_attribute
      -- base_type for iir_kind_physical_type_definition
      -- base_type for iir_kind_physical_subtype_definition
      -- base_type for iir_kind_record_type_definition
      -- base_type for iir_kind_record_subtype_definition
      -- base_type for iir_kind_access_type_definition
      -- base_type for iir_kind_access_subtype_definition
      -- base_type for iir_kind_incomplete_type_definition
      -- base_type for iir_kind_file_type_definition
      -- severity_expression for iir_kind_concurrent_assertion_statement
      -- severity_expression for iir_kind_assertion_statement
      -- severity_expression for iir_kind_report_statement
      -- sensitivity_list for iir_kind_sensitized_process_statement
      -- sensitivity_list for iir_kind_wait_statement
      -- name_value of iir_kind_simple_name
      -- association_chain for iir_kind_association_element_by_individual
      -- association_chain for iir_kind_parenthesis_name
      -- association_choices_list for iir_kind_aggregate
      -- association_choices_list for iir_kind_case_statement
      -- guard for iir_kind_concurrent_conditional_signal_assignment
      -- guard for iir_kind_concurrent_selected_signal_assignment
      -- entity_aspect for iir_kind_binding_indication
      -- default_binding_indicat for iir_kind_component_instantiation_statement
      -- component_name for iir_kind_component_configuration
      -- component_name for iir_kind_configuration_specification
      -- prev_block_configuration for iir_kind_block_configuration
      -- interface_declaration for iir_kind_function_Specification
      -- interface_declaration for iir_kind_function_Spec_Body
      -- interface_declaration for iir_kind_procedure_Specification
      -- interface_declaration for iir_kind_procedure_Spec_Body
      -- interface_declaration for iir_kind_implicit_function_declaration
      -- interface_declaration for iir_kind_implicit_procedure_declaration
      -- subprogram_specification for iir_kind_function_Body
      -- subprogram_specification for iir_kind_procedure_Body
      -- in_conversion for iir_kind_association_element_by_expression
      -- default_configuration for iir_kind_architecture_declaration
      -- bit_string_0 for iir_kind_bit_string_literal
      -- base_name for iir_kind_object_alias_declaration
      -- base_name for iir_kind_signal_declaration
      -- base_name for iir_kind_guard_signal_declaration
      -- base_name for iir_kind_variable_declaration
      -- base_name for iir_kind_file_declaration
      -- base_name for iir_kind_constant_declaration
      -- base_name for iir_kind_iterator_declaration
      -- base_name for iir_kind_slice_name
      -- base_name for iir_kind_indexed_name
      -- base_name for iir_kind_selected_element
      -- base_name for iir_kind_selected_by_all_name
      -- base_name for iir_kind_implicit_dereference
      -- base_name for iir_kind_attribute_value
      -- base_name for iir_kind_function_call
      -- block_block_configuration for iir_kind_block_statement
      -- right for iir_kinds_dyadic_operator
      --Field4: Node_Type := Null_Node;

      -- Usages of field5 (aka nbr1).
      -- driver_list for iir_kind_sensitized_process_statement
      -- driver_list for iir_kind_process_statement
      -- driver_list for iir_kinds_function_specification
      -- driver_list for iir_kinds_procedure_specification
      -- guard_sensitivity_list for iir_kind_guard_signal_declaration
      -- signal_driver for iir_kind_signal_declaration
      -- reject_time for iir_kind_concurrent_selected_signal_assignment
      -- reject_time for iir_kind_concurrent_conditionnal_signal_assignment
      -- reject_time for iir_kind_signal_assignment_statement
      -- resolution_function for iir_kind_integer_subtype_definition
      -- resolution_function for iir_kind_floating_subtype_definition
      -- resolution_function for iir_kind_enumeration_subtype_definition
      -- resolution_function for iir_kind_physical_subtype_definition
      -- resolution_function for iir_kind_array_subtype_definition
      -- resolution_function for iir_kind_unconstrained_array_subtype_definit.
      -- resolution_function for iir_kind_record_subtype_definition
      -- date for iir_kind_library_declaration
      -- date for iir_kind_design_unit
      -- generic_map_aspect for Iir_Kind_Component_Instantiation_Statement
      -- generic_map_aspect for Iir_Kind_block_header
      -- generic_map_aspect for Iir_Kind_binding_indication
      -- generation_scheme for iir_kind_generate_statement
      -- design_unit for iir_kind_constant_declaration
      -- design_unit for iir_kind_entity_declaration
      -- design_unit for iir_kind_configuration_declaration
      -- design_unit for iir_kind_package_declaration
      -- design_unit for iir_kind_body_declaration
      -- design_unit for iir_kind_architecture_declaration
      -- out_conversion for iir_kind_association_element_by_expression
      -- bit_string_1 for iir_kind_bit_string_literal
      --Field5: Node_Type := Null_Node;

      -- Usages of Field6:
      -- offset for iir_kind_design_unit
      -- number of element for iir_kinds_list
      -- base for iir_kind_bit_string_literal
      -- element_position for iir_kind_element_declaration
      -- type_mark for iir_kind_qualified_expression
      -- type_mark for iir_kind_file_type_definition
      -- type_mark for iir_kind_integer_subtype_definition
      -- type_mark for iir_kind_floating_subtype_definition
      -- type_mark for iir_kind_enumeration_subtype_definition
      -- type_mark for iir_kind_physical_subtype_definition
      -- type_mark for iir_kind_access_subtype_definition
      -- type_mark for iir_kind_record_subtype_definition
      -- type_mark for iir_kind_unconstrained_array_subtype_definition
      -- bit_string_base for iir_kind_bit_string_literal
      -- default_port_map_aspect_list for iir_kind_binding_indication

      -- Usages of nbr3/field7:
      -- line for iir_kind_design_unit
      -- max number of elements for iir_kinds_list
      -- implicit_definition for iir_kind_implicit_function_declaration
      -- implicit_definition for iir_kind_implicit_procedure_declaration
      -- block_header for iir_kind_block_statement
      -- delay_mechanism for iir_kind_concurrent_selected_signal_assignment
      -- delay_mechanism for iir_kind_concurrent_conditionnal_signal_assignment
      -- delay_mechanism for iir_kind_signal_assignment_statement
      -- value for iir_kind_integer_literal
      -- value for iir_kind_enumeration_literal
      -- value for iir_kind_unit_declaration
      -- value for iir_kind_physical_int_literal
      -- fp_value for iir_kind_physical_fp_literal
      -- fp_value for iir_kind_floating_point_literal
      -- entity_kind for iir_kind_entity_class
      -- entity_kind for iir_kind_attribute_specification
      -- callees_list for iir_kind_process_declaration
      -- callees_list for iir_kind_sensitized_process_declaration
      -- library_directory for iir_kind_library_declaration
      -- filename for iir_kind_design_file
      -- directory for iir_kind_design_file
      -- aggr_max_length for iir_kind_aggregate_info
      case Format is
         when Format_Short
           | Format_Medium =>
            Field4: Node_Type := Null_Node;
            Field5: Node_Type := Null_Node;
         when Format_Fp =>
            Fp64 : Iir_Fp64;
         when Format_Int =>
            Int64 : Iir_Int64;
      end case;
   end record;

   pragma Pack (Node_Record);
   for Node_Record'Size use 8*32;
   for Node_Record'Alignment use 4;
end Nodes;
