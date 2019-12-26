from libghdl import libghdl



# From nodes_meta
get_fields_first = libghdl.vhdl__nodes_meta__get_fields_first

get_fields_last = libghdl.vhdl__nodes_meta__get_fields_last

get_field_by_index = libghdl.vhdl__nodes_meta__get_field_by_index

get_field_type = libghdl.vhdl__nodes_meta__get_field_type

get_field_attribute = libghdl.vhdl__nodes_meta__get_field_attribute


class types:
    Boolean = 0
    Date_State_Type = 1
    Date_Type = 2
    File_Checksum_Id = 3
    Fp64 = 4
    Iir = 5
    Iir_All_Sensitized = 6
    Iir_Constraint = 7
    Iir_Delay_Mechanism = 8
    Iir_Direction = 9
    Iir_Flist = 10
    Iir_Index32 = 11
    Iir_Int32 = 12
    Iir_List = 13
    Iir_Mode = 14
    Iir_Predefined_Functions = 15
    Iir_Pure_State = 16
    Iir_Signal_Kind = 17
    Iir_Staticness = 18
    Int32 = 19
    Int64 = 20
    Name_Id = 21
    Number_Base_Type = 22
    PSL_NFA = 23
    PSL_Node = 24
    Source_File_Entry = 25
    Source_Ptr = 26
    String8_Id = 27
    Time_Stamp_Id = 28
    Token_Type = 29
    Tri_State_Type = 30


class Attr:
    ANone = 0
    Chain = 1
    Chain_Next = 2
    Forward_Ref = 3
    Maybe_Forward_Ref = 4
    Maybe_Ref = 5
    Of_Maybe_Ref = 6
    Of_Ref = 7
    Ref = 8


class fields:
    First_Design_Unit = 0
    Last_Design_Unit = 1
    Library_Declaration = 2
    File_Checksum = 3
    Analysis_Time_Stamp = 4
    Design_File_Source = 5
    Library = 6
    File_Dependence_List = 7
    Design_File_Filename = 8
    Design_File_Directory = 9
    Design_File = 10
    Design_File_Chain = 11
    Library_Directory = 12
    Date = 13
    Context_Items = 14
    Dependence_List = 15
    Analysis_Checks_List = 16
    Date_State = 17
    Guarded_Target_State = 18
    Library_Unit = 19
    Hash_Chain = 20
    Design_Unit_Source_Pos = 21
    Design_Unit_Source_Line = 22
    Design_Unit_Source_Col = 23
    Value = 24
    Enum_Pos = 25
    Physical_Literal = 26
    Fp_Value = 27
    Simple_Aggregate_List = 28
    String8_Id = 29
    String_Length = 30
    Bit_String_Base = 31
    Has_Signed = 32
    Has_Sign = 33
    Has_Length = 34
    Literal_Length = 35
    Literal_Origin = 36
    Range_Origin = 37
    Literal_Subtype = 38
    Allocator_Subtype = 39
    Entity_Class = 40
    Entity_Name_List = 41
    Attribute_Designator = 42
    Attribute_Specification_Chain = 43
    Attribute_Specification = 44
    Signal_List = 45
    Designated_Entity = 46
    Formal = 47
    Actual = 48
    Actual_Conversion = 49
    Formal_Conversion = 50
    Whole_Association_Flag = 51
    Collapse_Signal_Flag = 52
    Artificial_Flag = 53
    Open_Flag = 54
    After_Drivers_Flag = 55
    We_Value = 56
    Time = 57
    Associated_Expr = 58
    Associated_Block = 59
    Associated_Chain = 60
    Choice_Name = 61
    Choice_Expression = 62
    Choice_Range = 63
    Same_Alternative_Flag = 64
    Element_Type_Flag = 65
    Architecture = 66
    Block_Specification = 67
    Prev_Block_Configuration = 68
    Configuration_Item_Chain = 69
    Attribute_Value_Chain = 70
    Spec_Chain = 71
    Value_Chain = 72
    Attribute_Value_Spec_Chain = 73
    Entity_Name = 74
    Package = 75
    Package_Body = 76
    Instance_Package_Body = 77
    Need_Body = 78
    Macro_Expanded_Flag = 79
    Need_Instance_Bodies = 80
    Hierarchical_Name = 81
    Inherit_Spec_Chain = 82
    Vunit_Item_Chain = 83
    Bound_Vunit_Chain = 84
    Block_Configuration = 85
    Concurrent_Statement_Chain = 86
    Chain = 87
    Port_Chain = 88
    Generic_Chain = 89
    Type = 90
    Subtype_Indication = 91
    Discrete_Range = 92
    Type_Definition = 93
    Subtype_Definition = 94
    Incomplete_Type_Declaration = 95
    Interface_Type_Subprograms = 96
    Nature = 97
    Mode = 98
    Guarded_Signal_Flag = 99
    Signal_Kind = 100
    Base_Name = 101
    Interface_Declaration_Chain = 102
    Subprogram_Specification = 103
    Sequential_Statement_Chain = 104
    Subprogram_Body = 105
    Overload_Number = 106
    Subprogram_Depth = 107
    Subprogram_Hash = 108
    Impure_Depth = 109
    Return_Type = 110
    Implicit_Definition = 111
    Default_Value = 112
    Deferred_Declaration = 113
    Deferred_Declaration_Flag = 114
    Shared_Flag = 115
    Design_Unit = 116
    Block_Statement = 117
    Signal_Driver = 118
    Declaration_Chain = 119
    File_Logical_Name = 120
    File_Open_Kind = 121
    Element_Position = 122
    Use_Clause_Chain = 123
    Context_Reference_Chain = 124
    Selected_Name = 125
    Type_Declarator = 126
    Complete_Type_Definition = 127
    Incomplete_Type_Ref_Chain = 128
    Associated_Type = 129
    Enumeration_Literal_List = 130
    Entity_Class_Entry_Chain = 131
    Group_Constituent_List = 132
    Unit_Chain = 133
    Primary_Unit = 134
    Identifier = 135
    Label = 136
    Visible_Flag = 137
    Range_Constraint = 138
    Direction = 139
    Left_Limit = 140
    Right_Limit = 141
    Left_Limit_Expr = 142
    Right_Limit_Expr = 143
    Base_Type = 144
    Resolution_Indication = 145
    Record_Element_Resolution_Chain = 146
    Tolerance = 147
    Plus_Terminal = 148
    Minus_Terminal = 149
    Simultaneous_Left = 150
    Simultaneous_Right = 151
    Text_File_Flag = 152
    Only_Characters_Flag = 153
    Is_Character_Type = 154
    Type_Staticness = 155
    Constraint_State = 156
    Index_Subtype_List = 157
    Index_Subtype_Definition_List = 158
    Element_Subtype_Indication = 159
    Element_Subtype = 160
    Index_Constraint_List = 161
    Array_Element_Constraint = 162
    Elements_Declaration_List = 163
    Owned_Elements_Chain = 164
    Designated_Type = 165
    Designated_Subtype_Indication = 166
    Index_List = 167
    Reference = 168
    Nature_Declarator = 169
    Across_Type = 170
    Through_Type = 171
    Target = 172
    Waveform_Chain = 173
    Guard = 174
    Delay_Mechanism = 175
    Reject_Time_Expression = 176
    Sensitivity_List = 177
    Process_Origin = 178
    Package_Origin = 179
    Condition_Clause = 180
    Timeout_Clause = 181
    Postponed_Flag = 182
    Callees_List = 183
    Passive_Flag = 184
    Resolution_Function_Flag = 185
    Wait_State = 186
    All_Sensitized_State = 187
    Seen_Flag = 188
    Pure_Flag = 189
    Foreign_Flag = 190
    Resolved_Flag = 191
    Signal_Type_Flag = 192
    Has_Signal_Flag = 193
    Purity_State = 194
    Elab_Flag = 195
    Configuration_Mark_Flag = 196
    Configuration_Done_Flag = 197
    Index_Constraint_Flag = 198
    Hide_Implicit_Flag = 199
    Assertion_Condition = 200
    Report_Expression = 201
    Severity_Expression = 202
    Instantiated_Unit = 203
    Generic_Map_Aspect_Chain = 204
    Port_Map_Aspect_Chain = 205
    Configuration_Name = 206
    Component_Configuration = 207
    Configuration_Specification = 208
    Default_Binding_Indication = 209
    Default_Configuration_Declaration = 210
    Expression = 211
    Conditional_Expression_Chain = 212
    Allocator_Designated_Type = 213
    Selected_Waveform_Chain = 214
    Conditional_Waveform_Chain = 215
    Guard_Expression = 216
    Guard_Decl = 217
    Guard_Sensitivity_List = 218
    Signal_Attribute_Chain = 219
    Block_Block_Configuration = 220
    Package_Header = 221
    Block_Header = 222
    Uninstantiated_Package_Name = 223
    Uninstantiated_Package_Decl = 224
    Instance_Source_File = 225
    Generate_Block_Configuration = 226
    Generate_Statement_Body = 227
    Alternative_Label = 228
    Generate_Else_Clause = 229
    Condition = 230
    Else_Clause = 231
    Parameter_Specification = 232
    Parent = 233
    Loop_Label = 234
    Exit_Flag = 235
    Next_Flag = 236
    Component_Name = 237
    Instantiation_List = 238
    Entity_Aspect = 239
    Default_Entity_Aspect = 240
    Binding_Indication = 241
    Named_Entity = 242
    Alias_Declaration = 243
    Referenced_Name = 244
    Expr_Staticness = 245
    Error_Origin = 246
    Operand = 247
    Left = 248
    Right = 249
    Unit_Name = 250
    Name = 251
    Group_Template_Name = 252
    Name_Staticness = 253
    Prefix = 254
    Signature_Prefix = 255
    External_Pathname = 256
    Pathname_Suffix = 257
    Pathname_Expression = 258
    In_Formal_Flag = 259
    Slice_Subtype = 260
    Suffix = 261
    Index_Subtype = 262
    Parameter = 263
    Attr_Chain = 264
    Signal_Attribute_Declaration = 265
    Actual_Type = 266
    Actual_Type_Definition = 267
    Association_Chain = 268
    Individual_Association_Chain = 269
    Subprogram_Association_Chain = 270
    Aggregate_Info = 271
    Sub_Aggregate_Info = 272
    Aggr_Dynamic_Flag = 273
    Aggr_Min_Length = 274
    Aggr_Low_Limit = 275
    Aggr_High_Limit = 276
    Aggr_Others_Flag = 277
    Aggr_Named_Flag = 278
    Aggregate_Expand_Flag = 279
    Association_Choices_Chain = 280
    Case_Statement_Alternative_Chain = 281
    Choice_Staticness = 282
    Procedure_Call = 283
    Implementation = 284
    Parameter_Association_Chain = 285
    Method_Object = 286
    Subtype_Type_Mark = 287
    Type_Conversion_Subtype = 288
    Type_Mark = 289
    File_Type_Mark = 290
    Return_Type_Mark = 291
    Has_Disconnect_Flag = 292
    Has_Active_Flag = 293
    Is_Within_Flag = 294
    Type_Marks_List = 295
    Implicit_Alias_Flag = 296
    Alias_Signature = 297
    Attribute_Signature = 298
    Overload_List = 299
    Simple_Name_Identifier = 300
    Simple_Name_Subtype = 301
    Protected_Type_Body = 302
    Protected_Type_Declaration = 303
    Use_Flag = 304
    End_Has_Reserved_Id = 305
    End_Has_Identifier = 306
    End_Has_Postponed = 307
    Has_Label = 308
    Has_Begin = 309
    Has_End = 310
    Has_Is = 311
    Has_Pure = 312
    Has_Body = 313
    Has_Parameter = 314
    Has_Component = 315
    Has_Identifier_List = 316
    Has_Mode = 317
    Has_Class = 318
    Has_Delay_Mechanism = 319
    Suspend_Flag = 320
    Is_Ref = 321
    Is_Forward_Ref = 322
    Psl_Property = 323
    Psl_Sequence = 324
    Psl_Declaration = 325
    Psl_Expression = 326
    Psl_Boolean = 327
    PSL_Clock = 328
    PSL_NFA = 329
    PSL_Nbr_States = 330
    PSL_Clock_Sensitivity = 331
    PSL_EOS_Flag = 332


Get_Boolean = libghdl.vhdl__nodes_meta__get_boolean

Get_Date_State_Type = libghdl.vhdl__nodes_meta__get_date_state_type

Get_Date_Type = libghdl.vhdl__nodes_meta__get_date_type

Get_File_Checksum_Id = libghdl.vhdl__nodes_meta__get_file_checksum_id

Get_Fp64 = libghdl.vhdl__nodes_meta__get_fp64

Get_Iir = libghdl.vhdl__nodes_meta__get_iir

Get_Iir_All_Sensitized = libghdl.vhdl__nodes_meta__get_iir_all_sensitized

Get_Iir_Constraint = libghdl.vhdl__nodes_meta__get_iir_constraint

Get_Iir_Delay_Mechanism = libghdl.vhdl__nodes_meta__get_iir_delay_mechanism

Get_Iir_Direction = libghdl.vhdl__nodes_meta__get_iir_direction

Get_Iir_Flist = libghdl.vhdl__nodes_meta__get_iir_flist

Get_Iir_Index32 = libghdl.vhdl__nodes_meta__get_iir_index32

Get_Iir_Int32 = libghdl.vhdl__nodes_meta__get_iir_int32

Get_Iir_List = libghdl.vhdl__nodes_meta__get_iir_list

Get_Iir_Mode = libghdl.vhdl__nodes_meta__get_iir_mode

Get_Iir_Predefined_Functions = libghdl.vhdl__nodes_meta__get_iir_predefined_functions

Get_Iir_Pure_State = libghdl.vhdl__nodes_meta__get_iir_pure_state

Get_Iir_Signal_Kind = libghdl.vhdl__nodes_meta__get_iir_signal_kind

Get_Iir_Staticness = libghdl.vhdl__nodes_meta__get_iir_staticness

Get_Int32 = libghdl.vhdl__nodes_meta__get_int32

Get_Int64 = libghdl.vhdl__nodes_meta__get_int64

Get_Name_Id = libghdl.vhdl__nodes_meta__get_name_id

Get_Number_Base_Type = libghdl.vhdl__nodes_meta__get_number_base_type

Get_PSL_NFA = libghdl.vhdl__nodes_meta__get_psl_nfa

Get_PSL_Node = libghdl.vhdl__nodes_meta__get_psl_node

Get_Source_File_Entry = libghdl.vhdl__nodes_meta__get_source_file_entry

Get_Source_Ptr = libghdl.vhdl__nodes_meta__get_source_ptr

Get_String8_Id = libghdl.vhdl__nodes_meta__get_string8_id

Get_Time_Stamp_Id = libghdl.vhdl__nodes_meta__get_time_stamp_id

Get_Token_Type = libghdl.vhdl__nodes_meta__get_token_type

Get_Tri_State_Type = libghdl.vhdl__nodes_meta__get_tri_state_type


Has_First_Design_Unit =\
    libghdl.vhdl__nodes_meta__has_first_design_unit

Has_Last_Design_Unit =\
    libghdl.vhdl__nodes_meta__has_last_design_unit

Has_Library_Declaration =\
    libghdl.vhdl__nodes_meta__has_library_declaration

Has_File_Checksum =\
    libghdl.vhdl__nodes_meta__has_file_checksum

Has_Analysis_Time_Stamp =\
    libghdl.vhdl__nodes_meta__has_analysis_time_stamp

Has_Design_File_Source =\
    libghdl.vhdl__nodes_meta__has_design_file_source

Has_Library =\
    libghdl.vhdl__nodes_meta__has_library

Has_File_Dependence_List =\
    libghdl.vhdl__nodes_meta__has_file_dependence_list

Has_Design_File_Filename =\
    libghdl.vhdl__nodes_meta__has_design_file_filename

Has_Design_File_Directory =\
    libghdl.vhdl__nodes_meta__has_design_file_directory

Has_Design_File =\
    libghdl.vhdl__nodes_meta__has_design_file

Has_Design_File_Chain =\
    libghdl.vhdl__nodes_meta__has_design_file_chain

Has_Library_Directory =\
    libghdl.vhdl__nodes_meta__has_library_directory

Has_Date =\
    libghdl.vhdl__nodes_meta__has_date

Has_Context_Items =\
    libghdl.vhdl__nodes_meta__has_context_items

Has_Dependence_List =\
    libghdl.vhdl__nodes_meta__has_dependence_list

Has_Analysis_Checks_List =\
    libghdl.vhdl__nodes_meta__has_analysis_checks_list

Has_Date_State =\
    libghdl.vhdl__nodes_meta__has_date_state

Has_Guarded_Target_State =\
    libghdl.vhdl__nodes_meta__has_guarded_target_state

Has_Library_Unit =\
    libghdl.vhdl__nodes_meta__has_library_unit

Has_Hash_Chain =\
    libghdl.vhdl__nodes_meta__has_hash_chain

Has_Design_Unit_Source_Pos =\
    libghdl.vhdl__nodes_meta__has_design_unit_source_pos

Has_Design_Unit_Source_Line =\
    libghdl.vhdl__nodes_meta__has_design_unit_source_line

Has_Design_Unit_Source_Col =\
    libghdl.vhdl__nodes_meta__has_design_unit_source_col

Has_Value =\
    libghdl.vhdl__nodes_meta__has_value

Has_Enum_Pos =\
    libghdl.vhdl__nodes_meta__has_enum_pos

Has_Physical_Literal =\
    libghdl.vhdl__nodes_meta__has_physical_literal

Has_Fp_Value =\
    libghdl.vhdl__nodes_meta__has_fp_value

Has_Simple_Aggregate_List =\
    libghdl.vhdl__nodes_meta__has_simple_aggregate_list

Has_String8_Id =\
    libghdl.vhdl__nodes_meta__has_string8_id

Has_String_Length =\
    libghdl.vhdl__nodes_meta__has_string_length

Has_Bit_String_Base =\
    libghdl.vhdl__nodes_meta__has_bit_string_base

Has_Has_Signed =\
    libghdl.vhdl__nodes_meta__has_has_signed

Has_Has_Sign =\
    libghdl.vhdl__nodes_meta__has_has_sign

Has_Has_Length =\
    libghdl.vhdl__nodes_meta__has_has_length

Has_Literal_Length =\
    libghdl.vhdl__nodes_meta__has_literal_length

Has_Literal_Origin =\
    libghdl.vhdl__nodes_meta__has_literal_origin

Has_Range_Origin =\
    libghdl.vhdl__nodes_meta__has_range_origin

Has_Literal_Subtype =\
    libghdl.vhdl__nodes_meta__has_literal_subtype

Has_Allocator_Subtype =\
    libghdl.vhdl__nodes_meta__has_allocator_subtype

Has_Entity_Class =\
    libghdl.vhdl__nodes_meta__has_entity_class

Has_Entity_Name_List =\
    libghdl.vhdl__nodes_meta__has_entity_name_list

Has_Attribute_Designator =\
    libghdl.vhdl__nodes_meta__has_attribute_designator

Has_Attribute_Specification_Chain =\
    libghdl.vhdl__nodes_meta__has_attribute_specification_chain

Has_Attribute_Specification =\
    libghdl.vhdl__nodes_meta__has_attribute_specification

Has_Signal_List =\
    libghdl.vhdl__nodes_meta__has_signal_list

Has_Designated_Entity =\
    libghdl.vhdl__nodes_meta__has_designated_entity

Has_Formal =\
    libghdl.vhdl__nodes_meta__has_formal

Has_Actual =\
    libghdl.vhdl__nodes_meta__has_actual

Has_Actual_Conversion =\
    libghdl.vhdl__nodes_meta__has_actual_conversion

Has_Formal_Conversion =\
    libghdl.vhdl__nodes_meta__has_formal_conversion

Has_Whole_Association_Flag =\
    libghdl.vhdl__nodes_meta__has_whole_association_flag

Has_Collapse_Signal_Flag =\
    libghdl.vhdl__nodes_meta__has_collapse_signal_flag

Has_Artificial_Flag =\
    libghdl.vhdl__nodes_meta__has_artificial_flag

Has_Open_Flag =\
    libghdl.vhdl__nodes_meta__has_open_flag

Has_After_Drivers_Flag =\
    libghdl.vhdl__nodes_meta__has_after_drivers_flag

Has_We_Value =\
    libghdl.vhdl__nodes_meta__has_we_value

Has_Time =\
    libghdl.vhdl__nodes_meta__has_time

Has_Associated_Expr =\
    libghdl.vhdl__nodes_meta__has_associated_expr

Has_Associated_Block =\
    libghdl.vhdl__nodes_meta__has_associated_block

Has_Associated_Chain =\
    libghdl.vhdl__nodes_meta__has_associated_chain

Has_Choice_Name =\
    libghdl.vhdl__nodes_meta__has_choice_name

Has_Choice_Expression =\
    libghdl.vhdl__nodes_meta__has_choice_expression

Has_Choice_Range =\
    libghdl.vhdl__nodes_meta__has_choice_range

Has_Same_Alternative_Flag =\
    libghdl.vhdl__nodes_meta__has_same_alternative_flag

Has_Element_Type_Flag =\
    libghdl.vhdl__nodes_meta__has_element_type_flag

Has_Architecture =\
    libghdl.vhdl__nodes_meta__has_architecture

Has_Block_Specification =\
    libghdl.vhdl__nodes_meta__has_block_specification

Has_Prev_Block_Configuration =\
    libghdl.vhdl__nodes_meta__has_prev_block_configuration

Has_Configuration_Item_Chain =\
    libghdl.vhdl__nodes_meta__has_configuration_item_chain

Has_Attribute_Value_Chain =\
    libghdl.vhdl__nodes_meta__has_attribute_value_chain

Has_Spec_Chain =\
    libghdl.vhdl__nodes_meta__has_spec_chain

Has_Value_Chain =\
    libghdl.vhdl__nodes_meta__has_value_chain

Has_Attribute_Value_Spec_Chain =\
    libghdl.vhdl__nodes_meta__has_attribute_value_spec_chain

Has_Entity_Name =\
    libghdl.vhdl__nodes_meta__has_entity_name

Has_Package =\
    libghdl.vhdl__nodes_meta__has_package

Has_Package_Body =\
    libghdl.vhdl__nodes_meta__has_package_body

Has_Instance_Package_Body =\
    libghdl.vhdl__nodes_meta__has_instance_package_body

Has_Need_Body =\
    libghdl.vhdl__nodes_meta__has_need_body

Has_Macro_Expanded_Flag =\
    libghdl.vhdl__nodes_meta__has_macro_expanded_flag

Has_Need_Instance_Bodies =\
    libghdl.vhdl__nodes_meta__has_need_instance_bodies

Has_Hierarchical_Name =\
    libghdl.vhdl__nodes_meta__has_hierarchical_name

Has_Inherit_Spec_Chain =\
    libghdl.vhdl__nodes_meta__has_inherit_spec_chain

Has_Vunit_Item_Chain =\
    libghdl.vhdl__nodes_meta__has_vunit_item_chain

Has_Bound_Vunit_Chain =\
    libghdl.vhdl__nodes_meta__has_bound_vunit_chain

Has_Block_Configuration =\
    libghdl.vhdl__nodes_meta__has_block_configuration

Has_Concurrent_Statement_Chain =\
    libghdl.vhdl__nodes_meta__has_concurrent_statement_chain

Has_Chain =\
    libghdl.vhdl__nodes_meta__has_chain

Has_Port_Chain =\
    libghdl.vhdl__nodes_meta__has_port_chain

Has_Generic_Chain =\
    libghdl.vhdl__nodes_meta__has_generic_chain

Has_Type =\
    libghdl.vhdl__nodes_meta__has_type

Has_Subtype_Indication =\
    libghdl.vhdl__nodes_meta__has_subtype_indication

Has_Discrete_Range =\
    libghdl.vhdl__nodes_meta__has_discrete_range

Has_Type_Definition =\
    libghdl.vhdl__nodes_meta__has_type_definition

Has_Subtype_Definition =\
    libghdl.vhdl__nodes_meta__has_subtype_definition

Has_Incomplete_Type_Declaration =\
    libghdl.vhdl__nodes_meta__has_incomplete_type_declaration

Has_Interface_Type_Subprograms =\
    libghdl.vhdl__nodes_meta__has_interface_type_subprograms

Has_Nature =\
    libghdl.vhdl__nodes_meta__has_nature

Has_Mode =\
    libghdl.vhdl__nodes_meta__has_mode

Has_Guarded_Signal_Flag =\
    libghdl.vhdl__nodes_meta__has_guarded_signal_flag

Has_Signal_Kind =\
    libghdl.vhdl__nodes_meta__has_signal_kind

Has_Base_Name =\
    libghdl.vhdl__nodes_meta__has_base_name

Has_Interface_Declaration_Chain =\
    libghdl.vhdl__nodes_meta__has_interface_declaration_chain

Has_Subprogram_Specification =\
    libghdl.vhdl__nodes_meta__has_subprogram_specification

Has_Sequential_Statement_Chain =\
    libghdl.vhdl__nodes_meta__has_sequential_statement_chain

Has_Subprogram_Body =\
    libghdl.vhdl__nodes_meta__has_subprogram_body

Has_Overload_Number =\
    libghdl.vhdl__nodes_meta__has_overload_number

Has_Subprogram_Depth =\
    libghdl.vhdl__nodes_meta__has_subprogram_depth

Has_Subprogram_Hash =\
    libghdl.vhdl__nodes_meta__has_subprogram_hash

Has_Impure_Depth =\
    libghdl.vhdl__nodes_meta__has_impure_depth

Has_Return_Type =\
    libghdl.vhdl__nodes_meta__has_return_type

Has_Implicit_Definition =\
    libghdl.vhdl__nodes_meta__has_implicit_definition

Has_Default_Value =\
    libghdl.vhdl__nodes_meta__has_default_value

Has_Deferred_Declaration =\
    libghdl.vhdl__nodes_meta__has_deferred_declaration

Has_Deferred_Declaration_Flag =\
    libghdl.vhdl__nodes_meta__has_deferred_declaration_flag

Has_Shared_Flag =\
    libghdl.vhdl__nodes_meta__has_shared_flag

Has_Design_Unit =\
    libghdl.vhdl__nodes_meta__has_design_unit

Has_Block_Statement =\
    libghdl.vhdl__nodes_meta__has_block_statement

Has_Signal_Driver =\
    libghdl.vhdl__nodes_meta__has_signal_driver

Has_Declaration_Chain =\
    libghdl.vhdl__nodes_meta__has_declaration_chain

Has_File_Logical_Name =\
    libghdl.vhdl__nodes_meta__has_file_logical_name

Has_File_Open_Kind =\
    libghdl.vhdl__nodes_meta__has_file_open_kind

Has_Element_Position =\
    libghdl.vhdl__nodes_meta__has_element_position

Has_Use_Clause_Chain =\
    libghdl.vhdl__nodes_meta__has_use_clause_chain

Has_Context_Reference_Chain =\
    libghdl.vhdl__nodes_meta__has_context_reference_chain

Has_Selected_Name =\
    libghdl.vhdl__nodes_meta__has_selected_name

Has_Type_Declarator =\
    libghdl.vhdl__nodes_meta__has_type_declarator

Has_Complete_Type_Definition =\
    libghdl.vhdl__nodes_meta__has_complete_type_definition

Has_Incomplete_Type_Ref_Chain =\
    libghdl.vhdl__nodes_meta__has_incomplete_type_ref_chain

Has_Associated_Type =\
    libghdl.vhdl__nodes_meta__has_associated_type

Has_Enumeration_Literal_List =\
    libghdl.vhdl__nodes_meta__has_enumeration_literal_list

Has_Entity_Class_Entry_Chain =\
    libghdl.vhdl__nodes_meta__has_entity_class_entry_chain

Has_Group_Constituent_List =\
    libghdl.vhdl__nodes_meta__has_group_constituent_list

Has_Unit_Chain =\
    libghdl.vhdl__nodes_meta__has_unit_chain

Has_Primary_Unit =\
    libghdl.vhdl__nodes_meta__has_primary_unit

Has_Identifier =\
    libghdl.vhdl__nodes_meta__has_identifier

Has_Label =\
    libghdl.vhdl__nodes_meta__has_label

Has_Visible_Flag =\
    libghdl.vhdl__nodes_meta__has_visible_flag

Has_Range_Constraint =\
    libghdl.vhdl__nodes_meta__has_range_constraint

Has_Direction =\
    libghdl.vhdl__nodes_meta__has_direction

Has_Left_Limit =\
    libghdl.vhdl__nodes_meta__has_left_limit

Has_Right_Limit =\
    libghdl.vhdl__nodes_meta__has_right_limit

Has_Left_Limit_Expr =\
    libghdl.vhdl__nodes_meta__has_left_limit_expr

Has_Right_Limit_Expr =\
    libghdl.vhdl__nodes_meta__has_right_limit_expr

Has_Base_Type =\
    libghdl.vhdl__nodes_meta__has_base_type

Has_Resolution_Indication =\
    libghdl.vhdl__nodes_meta__has_resolution_indication

Has_Record_Element_Resolution_Chain =\
    libghdl.vhdl__nodes_meta__has_record_element_resolution_chain

Has_Tolerance =\
    libghdl.vhdl__nodes_meta__has_tolerance

Has_Plus_Terminal =\
    libghdl.vhdl__nodes_meta__has_plus_terminal

Has_Minus_Terminal =\
    libghdl.vhdl__nodes_meta__has_minus_terminal

Has_Simultaneous_Left =\
    libghdl.vhdl__nodes_meta__has_simultaneous_left

Has_Simultaneous_Right =\
    libghdl.vhdl__nodes_meta__has_simultaneous_right

Has_Text_File_Flag =\
    libghdl.vhdl__nodes_meta__has_text_file_flag

Has_Only_Characters_Flag =\
    libghdl.vhdl__nodes_meta__has_only_characters_flag

Has_Is_Character_Type =\
    libghdl.vhdl__nodes_meta__has_is_character_type

Has_Type_Staticness =\
    libghdl.vhdl__nodes_meta__has_type_staticness

Has_Constraint_State =\
    libghdl.vhdl__nodes_meta__has_constraint_state

Has_Index_Subtype_List =\
    libghdl.vhdl__nodes_meta__has_index_subtype_list

Has_Index_Subtype_Definition_List =\
    libghdl.vhdl__nodes_meta__has_index_subtype_definition_list

Has_Element_Subtype_Indication =\
    libghdl.vhdl__nodes_meta__has_element_subtype_indication

Has_Element_Subtype =\
    libghdl.vhdl__nodes_meta__has_element_subtype

Has_Index_Constraint_List =\
    libghdl.vhdl__nodes_meta__has_index_constraint_list

Has_Array_Element_Constraint =\
    libghdl.vhdl__nodes_meta__has_array_element_constraint

Has_Elements_Declaration_List =\
    libghdl.vhdl__nodes_meta__has_elements_declaration_list

Has_Owned_Elements_Chain =\
    libghdl.vhdl__nodes_meta__has_owned_elements_chain

Has_Designated_Type =\
    libghdl.vhdl__nodes_meta__has_designated_type

Has_Designated_Subtype_Indication =\
    libghdl.vhdl__nodes_meta__has_designated_subtype_indication

Has_Index_List =\
    libghdl.vhdl__nodes_meta__has_index_list

Has_Reference =\
    libghdl.vhdl__nodes_meta__has_reference

Has_Nature_Declarator =\
    libghdl.vhdl__nodes_meta__has_nature_declarator

Has_Across_Type =\
    libghdl.vhdl__nodes_meta__has_across_type

Has_Through_Type =\
    libghdl.vhdl__nodes_meta__has_through_type

Has_Target =\
    libghdl.vhdl__nodes_meta__has_target

Has_Waveform_Chain =\
    libghdl.vhdl__nodes_meta__has_waveform_chain

Has_Guard =\
    libghdl.vhdl__nodes_meta__has_guard

Has_Delay_Mechanism =\
    libghdl.vhdl__nodes_meta__has_delay_mechanism

Has_Reject_Time_Expression =\
    libghdl.vhdl__nodes_meta__has_reject_time_expression

Has_Sensitivity_List =\
    libghdl.vhdl__nodes_meta__has_sensitivity_list

Has_Process_Origin =\
    libghdl.vhdl__nodes_meta__has_process_origin

Has_Package_Origin =\
    libghdl.vhdl__nodes_meta__has_package_origin

Has_Condition_Clause =\
    libghdl.vhdl__nodes_meta__has_condition_clause

Has_Timeout_Clause =\
    libghdl.vhdl__nodes_meta__has_timeout_clause

Has_Postponed_Flag =\
    libghdl.vhdl__nodes_meta__has_postponed_flag

Has_Callees_List =\
    libghdl.vhdl__nodes_meta__has_callees_list

Has_Passive_Flag =\
    libghdl.vhdl__nodes_meta__has_passive_flag

Has_Resolution_Function_Flag =\
    libghdl.vhdl__nodes_meta__has_resolution_function_flag

Has_Wait_State =\
    libghdl.vhdl__nodes_meta__has_wait_state

Has_All_Sensitized_State =\
    libghdl.vhdl__nodes_meta__has_all_sensitized_state

Has_Seen_Flag =\
    libghdl.vhdl__nodes_meta__has_seen_flag

Has_Pure_Flag =\
    libghdl.vhdl__nodes_meta__has_pure_flag

Has_Foreign_Flag =\
    libghdl.vhdl__nodes_meta__has_foreign_flag

Has_Resolved_Flag =\
    libghdl.vhdl__nodes_meta__has_resolved_flag

Has_Signal_Type_Flag =\
    libghdl.vhdl__nodes_meta__has_signal_type_flag

Has_Has_Signal_Flag =\
    libghdl.vhdl__nodes_meta__has_has_signal_flag

Has_Purity_State =\
    libghdl.vhdl__nodes_meta__has_purity_state

Has_Elab_Flag =\
    libghdl.vhdl__nodes_meta__has_elab_flag

Has_Configuration_Mark_Flag =\
    libghdl.vhdl__nodes_meta__has_configuration_mark_flag

Has_Configuration_Done_Flag =\
    libghdl.vhdl__nodes_meta__has_configuration_done_flag

Has_Index_Constraint_Flag =\
    libghdl.vhdl__nodes_meta__has_index_constraint_flag

Has_Hide_Implicit_Flag =\
    libghdl.vhdl__nodes_meta__has_hide_implicit_flag

Has_Assertion_Condition =\
    libghdl.vhdl__nodes_meta__has_assertion_condition

Has_Report_Expression =\
    libghdl.vhdl__nodes_meta__has_report_expression

Has_Severity_Expression =\
    libghdl.vhdl__nodes_meta__has_severity_expression

Has_Instantiated_Unit =\
    libghdl.vhdl__nodes_meta__has_instantiated_unit

Has_Generic_Map_Aspect_Chain =\
    libghdl.vhdl__nodes_meta__has_generic_map_aspect_chain

Has_Port_Map_Aspect_Chain =\
    libghdl.vhdl__nodes_meta__has_port_map_aspect_chain

Has_Configuration_Name =\
    libghdl.vhdl__nodes_meta__has_configuration_name

Has_Component_Configuration =\
    libghdl.vhdl__nodes_meta__has_component_configuration

Has_Configuration_Specification =\
    libghdl.vhdl__nodes_meta__has_configuration_specification

Has_Default_Binding_Indication =\
    libghdl.vhdl__nodes_meta__has_default_binding_indication

Has_Default_Configuration_Declaration =\
    libghdl.vhdl__nodes_meta__has_default_configuration_declaration

Has_Expression =\
    libghdl.vhdl__nodes_meta__has_expression

Has_Conditional_Expression_Chain =\
    libghdl.vhdl__nodes_meta__has_conditional_expression_chain

Has_Allocator_Designated_Type =\
    libghdl.vhdl__nodes_meta__has_allocator_designated_type

Has_Selected_Waveform_Chain =\
    libghdl.vhdl__nodes_meta__has_selected_waveform_chain

Has_Conditional_Waveform_Chain =\
    libghdl.vhdl__nodes_meta__has_conditional_waveform_chain

Has_Guard_Expression =\
    libghdl.vhdl__nodes_meta__has_guard_expression

Has_Guard_Decl =\
    libghdl.vhdl__nodes_meta__has_guard_decl

Has_Guard_Sensitivity_List =\
    libghdl.vhdl__nodes_meta__has_guard_sensitivity_list

Has_Signal_Attribute_Chain =\
    libghdl.vhdl__nodes_meta__has_signal_attribute_chain

Has_Block_Block_Configuration =\
    libghdl.vhdl__nodes_meta__has_block_block_configuration

Has_Package_Header =\
    libghdl.vhdl__nodes_meta__has_package_header

Has_Block_Header =\
    libghdl.vhdl__nodes_meta__has_block_header

Has_Uninstantiated_Package_Name =\
    libghdl.vhdl__nodes_meta__has_uninstantiated_package_name

Has_Uninstantiated_Package_Decl =\
    libghdl.vhdl__nodes_meta__has_uninstantiated_package_decl

Has_Instance_Source_File =\
    libghdl.vhdl__nodes_meta__has_instance_source_file

Has_Generate_Block_Configuration =\
    libghdl.vhdl__nodes_meta__has_generate_block_configuration

Has_Generate_Statement_Body =\
    libghdl.vhdl__nodes_meta__has_generate_statement_body

Has_Alternative_Label =\
    libghdl.vhdl__nodes_meta__has_alternative_label

Has_Generate_Else_Clause =\
    libghdl.vhdl__nodes_meta__has_generate_else_clause

Has_Condition =\
    libghdl.vhdl__nodes_meta__has_condition

Has_Else_Clause =\
    libghdl.vhdl__nodes_meta__has_else_clause

Has_Parameter_Specification =\
    libghdl.vhdl__nodes_meta__has_parameter_specification

Has_Parent =\
    libghdl.vhdl__nodes_meta__has_parent

Has_Loop_Label =\
    libghdl.vhdl__nodes_meta__has_loop_label

Has_Exit_Flag =\
    libghdl.vhdl__nodes_meta__has_exit_flag

Has_Next_Flag =\
    libghdl.vhdl__nodes_meta__has_next_flag

Has_Component_Name =\
    libghdl.vhdl__nodes_meta__has_component_name

Has_Instantiation_List =\
    libghdl.vhdl__nodes_meta__has_instantiation_list

Has_Entity_Aspect =\
    libghdl.vhdl__nodes_meta__has_entity_aspect

Has_Default_Entity_Aspect =\
    libghdl.vhdl__nodes_meta__has_default_entity_aspect

Has_Binding_Indication =\
    libghdl.vhdl__nodes_meta__has_binding_indication

Has_Named_Entity =\
    libghdl.vhdl__nodes_meta__has_named_entity

Has_Alias_Declaration =\
    libghdl.vhdl__nodes_meta__has_alias_declaration

Has_Referenced_Name =\
    libghdl.vhdl__nodes_meta__has_referenced_name

Has_Expr_Staticness =\
    libghdl.vhdl__nodes_meta__has_expr_staticness

Has_Error_Origin =\
    libghdl.vhdl__nodes_meta__has_error_origin

Has_Operand =\
    libghdl.vhdl__nodes_meta__has_operand

Has_Left =\
    libghdl.vhdl__nodes_meta__has_left

Has_Right =\
    libghdl.vhdl__nodes_meta__has_right

Has_Unit_Name =\
    libghdl.vhdl__nodes_meta__has_unit_name

Has_Name =\
    libghdl.vhdl__nodes_meta__has_name

Has_Group_Template_Name =\
    libghdl.vhdl__nodes_meta__has_group_template_name

Has_Name_Staticness =\
    libghdl.vhdl__nodes_meta__has_name_staticness

Has_Prefix =\
    libghdl.vhdl__nodes_meta__has_prefix

Has_Signature_Prefix =\
    libghdl.vhdl__nodes_meta__has_signature_prefix

Has_External_Pathname =\
    libghdl.vhdl__nodes_meta__has_external_pathname

Has_Pathname_Suffix =\
    libghdl.vhdl__nodes_meta__has_pathname_suffix

Has_Pathname_Expression =\
    libghdl.vhdl__nodes_meta__has_pathname_expression

Has_In_Formal_Flag =\
    libghdl.vhdl__nodes_meta__has_in_formal_flag

Has_Slice_Subtype =\
    libghdl.vhdl__nodes_meta__has_slice_subtype

Has_Suffix =\
    libghdl.vhdl__nodes_meta__has_suffix

Has_Index_Subtype =\
    libghdl.vhdl__nodes_meta__has_index_subtype

Has_Parameter =\
    libghdl.vhdl__nodes_meta__has_parameter

Has_Attr_Chain =\
    libghdl.vhdl__nodes_meta__has_attr_chain

Has_Signal_Attribute_Declaration =\
    libghdl.vhdl__nodes_meta__has_signal_attribute_declaration

Has_Actual_Type =\
    libghdl.vhdl__nodes_meta__has_actual_type

Has_Actual_Type_Definition =\
    libghdl.vhdl__nodes_meta__has_actual_type_definition

Has_Association_Chain =\
    libghdl.vhdl__nodes_meta__has_association_chain

Has_Individual_Association_Chain =\
    libghdl.vhdl__nodes_meta__has_individual_association_chain

Has_Subprogram_Association_Chain =\
    libghdl.vhdl__nodes_meta__has_subprogram_association_chain

Has_Aggregate_Info =\
    libghdl.vhdl__nodes_meta__has_aggregate_info

Has_Sub_Aggregate_Info =\
    libghdl.vhdl__nodes_meta__has_sub_aggregate_info

Has_Aggr_Dynamic_Flag =\
    libghdl.vhdl__nodes_meta__has_aggr_dynamic_flag

Has_Aggr_Min_Length =\
    libghdl.vhdl__nodes_meta__has_aggr_min_length

Has_Aggr_Low_Limit =\
    libghdl.vhdl__nodes_meta__has_aggr_low_limit

Has_Aggr_High_Limit =\
    libghdl.vhdl__nodes_meta__has_aggr_high_limit

Has_Aggr_Others_Flag =\
    libghdl.vhdl__nodes_meta__has_aggr_others_flag

Has_Aggr_Named_Flag =\
    libghdl.vhdl__nodes_meta__has_aggr_named_flag

Has_Aggregate_Expand_Flag =\
    libghdl.vhdl__nodes_meta__has_aggregate_expand_flag

Has_Association_Choices_Chain =\
    libghdl.vhdl__nodes_meta__has_association_choices_chain

Has_Case_Statement_Alternative_Chain =\
    libghdl.vhdl__nodes_meta__has_case_statement_alternative_chain

Has_Choice_Staticness =\
    libghdl.vhdl__nodes_meta__has_choice_staticness

Has_Procedure_Call =\
    libghdl.vhdl__nodes_meta__has_procedure_call

Has_Implementation =\
    libghdl.vhdl__nodes_meta__has_implementation

Has_Parameter_Association_Chain =\
    libghdl.vhdl__nodes_meta__has_parameter_association_chain

Has_Method_Object =\
    libghdl.vhdl__nodes_meta__has_method_object

Has_Subtype_Type_Mark =\
    libghdl.vhdl__nodes_meta__has_subtype_type_mark

Has_Type_Conversion_Subtype =\
    libghdl.vhdl__nodes_meta__has_type_conversion_subtype

Has_Type_Mark =\
    libghdl.vhdl__nodes_meta__has_type_mark

Has_File_Type_Mark =\
    libghdl.vhdl__nodes_meta__has_file_type_mark

Has_Return_Type_Mark =\
    libghdl.vhdl__nodes_meta__has_return_type_mark

Has_Has_Disconnect_Flag =\
    libghdl.vhdl__nodes_meta__has_has_disconnect_flag

Has_Has_Active_Flag =\
    libghdl.vhdl__nodes_meta__has_has_active_flag

Has_Is_Within_Flag =\
    libghdl.vhdl__nodes_meta__has_is_within_flag

Has_Type_Marks_List =\
    libghdl.vhdl__nodes_meta__has_type_marks_list

Has_Implicit_Alias_Flag =\
    libghdl.vhdl__nodes_meta__has_implicit_alias_flag

Has_Alias_Signature =\
    libghdl.vhdl__nodes_meta__has_alias_signature

Has_Attribute_Signature =\
    libghdl.vhdl__nodes_meta__has_attribute_signature

Has_Overload_List =\
    libghdl.vhdl__nodes_meta__has_overload_list

Has_Simple_Name_Identifier =\
    libghdl.vhdl__nodes_meta__has_simple_name_identifier

Has_Simple_Name_Subtype =\
    libghdl.vhdl__nodes_meta__has_simple_name_subtype

Has_Protected_Type_Body =\
    libghdl.vhdl__nodes_meta__has_protected_type_body

Has_Protected_Type_Declaration =\
    libghdl.vhdl__nodes_meta__has_protected_type_declaration

Has_Use_Flag =\
    libghdl.vhdl__nodes_meta__has_use_flag

Has_End_Has_Reserved_Id =\
    libghdl.vhdl__nodes_meta__has_end_has_reserved_id

Has_End_Has_Identifier =\
    libghdl.vhdl__nodes_meta__has_end_has_identifier

Has_End_Has_Postponed =\
    libghdl.vhdl__nodes_meta__has_end_has_postponed

Has_Has_Label =\
    libghdl.vhdl__nodes_meta__has_has_label

Has_Has_Begin =\
    libghdl.vhdl__nodes_meta__has_has_begin

Has_Has_End =\
    libghdl.vhdl__nodes_meta__has_has_end

Has_Has_Is =\
    libghdl.vhdl__nodes_meta__has_has_is

Has_Has_Pure =\
    libghdl.vhdl__nodes_meta__has_has_pure

Has_Has_Body =\
    libghdl.vhdl__nodes_meta__has_has_body

Has_Has_Parameter =\
    libghdl.vhdl__nodes_meta__has_has_parameter

Has_Has_Component =\
    libghdl.vhdl__nodes_meta__has_has_component

Has_Has_Identifier_List =\
    libghdl.vhdl__nodes_meta__has_has_identifier_list

Has_Has_Mode =\
    libghdl.vhdl__nodes_meta__has_has_mode

Has_Has_Class =\
    libghdl.vhdl__nodes_meta__has_has_class

Has_Has_Delay_Mechanism =\
    libghdl.vhdl__nodes_meta__has_has_delay_mechanism

Has_Suspend_Flag =\
    libghdl.vhdl__nodes_meta__has_suspend_flag

Has_Is_Ref =\
    libghdl.vhdl__nodes_meta__has_is_ref

Has_Is_Forward_Ref =\
    libghdl.vhdl__nodes_meta__has_is_forward_ref

Has_Psl_Property =\
    libghdl.vhdl__nodes_meta__has_psl_property

Has_Psl_Sequence =\
    libghdl.vhdl__nodes_meta__has_psl_sequence

Has_Psl_Declaration =\
    libghdl.vhdl__nodes_meta__has_psl_declaration

Has_Psl_Expression =\
    libghdl.vhdl__nodes_meta__has_psl_expression

Has_Psl_Boolean =\
    libghdl.vhdl__nodes_meta__has_psl_boolean

Has_PSL_Clock =\
    libghdl.vhdl__nodes_meta__has_psl_clock

Has_PSL_NFA =\
    libghdl.vhdl__nodes_meta__has_psl_nfa

Has_PSL_Nbr_States =\
    libghdl.vhdl__nodes_meta__has_psl_nbr_states

Has_PSL_Clock_Sensitivity =\
    libghdl.vhdl__nodes_meta__has_psl_clock_sensitivity

Has_PSL_EOS_Flag =\
    libghdl.vhdl__nodes_meta__has_psl_eos_flag
