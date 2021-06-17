# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
from enum import IntEnum, unique
from pydecor import export
from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import IirKind


# From nodes_meta
@export
def get_fields_first(K: IirKind) -> int:
    """
    Return the list of fields for node :obj:`K`.

    In Ada ``Vhdl.Nodes_Meta.Get_Fields`` returns a ``Fields_Array``. To emulate
    this array access, the API provides ``get_fields_first`` and :func:`get_fields_last`.

    The fields are sorted: first the non nodes/list of nodes, then the
    nodes/lists that aren't reference, and then the reference.

    :param K: Node to get first array index from.
    """
    return libghdl.vhdl__nodes_meta__get_fields_first(K)


@export
def get_fields_last(K: IirKind) -> int:
    """
    Return the list of fields for node :obj:`K`.

    In Ada ``Vhdl.Nodes_Meta.Get_Fields`` returns a ``Fields_Array``. To emulate
    this array access, the API provides :func:`get_fields_first` and ``get_fields_last``.

    The fields are sorted: first the non nodes/list of nodes, then the
    nodes/lists that aren't reference, and then the reference.

    :param K: Node to get last array index from.
    """
    return libghdl.vhdl__nodes_meta__get_fields_last(K)


@export
def get_field_by_index(K: IirKind) -> int:
    return libghdl.vhdl__nodes_meta__get_field_by_index(K)


@export
def get_field_type(*args):
    return libghdl.vhdl__nodes_meta__get_field_type(*args)


@export
def get_field_attribute(*args):
    return libghdl.vhdl__nodes_meta__get_field_attribute(*args)


@export
@unique
class types(IntEnum):
    Boolean = 0
    Date_State_Type = 1
    Date_Type = 2
    Direction_Type = 3
    File_Checksum_Id = 4
    Fp64 = 5
    Iir = 6
    Iir_All_Sensitized = 7
    Iir_Constraint = 8
    Iir_Delay_Mechanism = 9
    Iir_Flist = 10
    Iir_Force_Mode = 11
    Iir_Index32 = 12
    Iir_Int32 = 13
    Iir_List = 14
    Iir_Mode = 15
    Iir_Predefined_Functions = 16
    Iir_Pure_State = 17
    Iir_Signal_Kind = 18
    Iir_Staticness = 19
    Int32 = 20
    Int64 = 21
    Name_Id = 22
    Number_Base_Type = 23
    PSL_NFA = 24
    PSL_Node = 25
    Scalar_Size = 26
    Source_File_Entry = 27
    Source_Ptr = 28
    String8_Id = 29
    Time_Stamp_Id = 30
    Token_Type = 31
    Tri_State_Type = 32


@export
@unique
class Attr(IntEnum):
    ANone = 0
    Chain = 1
    Chain_Next = 2
    Forward_Ref = 3
    Maybe_Forward_Ref = 4
    Maybe_Ref = 5
    Of_Maybe_Ref = 6
    Of_Ref = 7
    Ref = 8


@export
@unique
class fields(IntEnum):
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
    Static_Attribute_Flag = 45
    Signal_List = 46
    Quantity_List = 47
    Designated_Entity = 48
    Formal = 49
    Actual = 50
    Actual_Conversion = 51
    Formal_Conversion = 52
    Whole_Association_Flag = 53
    Collapse_Signal_Flag = 54
    Artificial_Flag = 55
    Open_Flag = 56
    After_Drivers_Flag = 57
    We_Value = 58
    Time = 59
    Associated_Expr = 60
    Associated_Block = 61
    Associated_Chain = 62
    Choice_Name = 63
    Choice_Expression = 64
    Choice_Range = 65
    Same_Alternative_Flag = 66
    Element_Type_Flag = 67
    Architecture = 68
    Block_Specification = 69
    Prev_Block_Configuration = 70
    Configuration_Item_Chain = 71
    Attribute_Value_Chain = 72
    Spec_Chain = 73
    Value_Chain = 74
    Attribute_Value_Spec_Chain = 75
    Entity_Name = 76
    Package = 77
    Package_Body = 78
    Instance_Package_Body = 79
    Need_Body = 80
    Macro_Expanded_Flag = 81
    Need_Instance_Bodies = 82
    Hierarchical_Name = 83
    Inherit_Spec_Chain = 84
    Vunit_Item_Chain = 85
    Bound_Vunit_Chain = 86
    Verification_Block_Configuration = 87
    Block_Configuration = 88
    Concurrent_Statement_Chain = 89
    Chain = 90
    Port_Chain = 91
    Generic_Chain = 92
    Type = 93
    Subtype_Indication = 94
    Discrete_Range = 95
    Type_Definition = 96
    Subtype_Definition = 97
    Incomplete_Type_Declaration = 98
    Interface_Type_Subprograms = 99
    Nature_Definition = 100
    Nature = 101
    Subnature_Indication = 102
    Mode = 103
    Guarded_Signal_Flag = 104
    Signal_Kind = 105
    Base_Name = 106
    Interface_Declaration_Chain = 107
    Subprogram_Specification = 108
    Sequential_Statement_Chain = 109
    Simultaneous_Statement_Chain = 110
    Subprogram_Body = 111
    Overload_Number = 112
    Subprogram_Depth = 113
    Subprogram_Hash = 114
    Impure_Depth = 115
    Return_Type = 116
    Implicit_Definition = 117
    Uninstantiated_Subprogram_Name = 118
    Default_Value = 119
    Deferred_Declaration = 120
    Deferred_Declaration_Flag = 121
    Shared_Flag = 122
    Design_Unit = 123
    Block_Statement = 124
    Signal_Driver = 125
    Declaration_Chain = 126
    File_Logical_Name = 127
    File_Open_Kind = 128
    Element_Position = 129
    Use_Clause_Chain = 130
    Context_Reference_Chain = 131
    Selected_Name = 132
    Type_Declarator = 133
    Complete_Type_Definition = 134
    Incomplete_Type_Ref_Chain = 135
    Associated_Type = 136
    Enumeration_Literal_List = 137
    Entity_Class_Entry_Chain = 138
    Group_Constituent_List = 139
    Unit_Chain = 140
    Primary_Unit = 141
    Identifier = 142
    Label = 143
    Visible_Flag = 144
    Range_Constraint = 145
    Direction = 146
    Left_Limit = 147
    Right_Limit = 148
    Left_Limit_Expr = 149
    Right_Limit_Expr = 150
    Parent_Type = 151
    Simple_Nature = 152
    Base_Nature = 153
    Resolution_Indication = 154
    Record_Element_Resolution_Chain = 155
    Tolerance = 156
    Plus_Terminal_Name = 157
    Minus_Terminal_Name = 158
    Plus_Terminal = 159
    Minus_Terminal = 160
    Magnitude_Expression = 161
    Phase_Expression = 162
    Power_Expression = 163
    Simultaneous_Left = 164
    Simultaneous_Right = 165
    Text_File_Flag = 166
    Only_Characters_Flag = 167
    Is_Character_Type = 168
    Nature_Staticness = 169
    Type_Staticness = 170
    Constraint_State = 171
    Index_Subtype_List = 172
    Index_Subtype_Definition_List = 173
    Element_Subtype_Indication = 174
    Element_Subtype = 175
    Element_Subnature_Indication = 176
    Element_Subnature = 177
    Index_Constraint_List = 178
    Array_Element_Constraint = 179
    Has_Array_Constraint_Flag = 180
    Has_Element_Constraint_Flag = 181
    Elements_Declaration_List = 182
    Owned_Elements_Chain = 183
    Designated_Type = 184
    Designated_Subtype_Indication = 185
    Index_List = 186
    Reference = 187
    Nature_Declarator = 188
    Across_Type_Mark = 189
    Through_Type_Mark = 190
    Across_Type_Definition = 191
    Through_Type_Definition = 192
    Across_Type = 193
    Through_Type = 194
    Target = 195
    Waveform_Chain = 196
    Guard = 197
    Delay_Mechanism = 198
    Reject_Time_Expression = 199
    Force_Mode = 200
    Has_Force_Mode = 201
    Sensitivity_List = 202
    Process_Origin = 203
    Package_Origin = 204
    Condition_Clause = 205
    Break_Element = 206
    Selector_Quantity = 207
    Break_Quantity = 208
    Timeout_Clause = 209
    Postponed_Flag = 210
    Callees_List = 211
    Passive_Flag = 212
    Resolution_Function_Flag = 213
    Wait_State = 214
    All_Sensitized_State = 215
    Seen_Flag = 216
    Pure_Flag = 217
    Foreign_Flag = 218
    Resolved_Flag = 219
    Signal_Type_Flag = 220
    Has_Signal_Flag = 221
    Purity_State = 222
    Elab_Flag = 223
    Vendor_Library_Flag = 224
    Configuration_Mark_Flag = 225
    Configuration_Done_Flag = 226
    Index_Constraint_Flag = 227
    Hide_Implicit_Flag = 228
    Assertion_Condition = 229
    Report_Expression = 230
    Severity_Expression = 231
    Instantiated_Unit = 232
    Generic_Map_Aspect_Chain = 233
    Port_Map_Aspect_Chain = 234
    Configuration_Name = 235
    Component_Configuration = 236
    Configuration_Specification = 237
    Default_Binding_Indication = 238
    Default_Configuration_Declaration = 239
    Expression = 240
    Conditional_Expression_Chain = 241
    Allocator_Designated_Type = 242
    Selected_Waveform_Chain = 243
    Conditional_Waveform_Chain = 244
    Guard_Expression = 245
    Guard_Decl = 246
    Guard_Sensitivity_List = 247
    Signal_Attribute_Chain = 248
    Block_Block_Configuration = 249
    Package_Header = 250
    Block_Header = 251
    Uninstantiated_Package_Name = 252
    Uninstantiated_Package_Decl = 253
    Instance_Source_File = 254
    Generate_Block_Configuration = 255
    Generate_Statement_Body = 256
    Alternative_Label = 257
    Generate_Else_Clause = 258
    Condition = 259
    Else_Clause = 260
    Parameter_Specification = 261
    Parent = 262
    Loop_Label = 263
    Exit_Flag = 264
    Next_Flag = 265
    Component_Name = 266
    Instantiation_List = 267
    Entity_Aspect = 268
    Default_Entity_Aspect = 269
    Binding_Indication = 270
    Named_Entity = 271
    Referenced_Name = 272
    Expr_Staticness = 273
    Scalar_Size = 274
    Error_Origin = 275
    Operand = 276
    Left = 277
    Right = 278
    Unit_Name = 279
    Name = 280
    Group_Template_Name = 281
    Name_Staticness = 282
    Prefix = 283
    Signature_Prefix = 284
    External_Pathname = 285
    Pathname_Suffix = 286
    Pathname_Expression = 287
    In_Formal_Flag = 288
    Slice_Subtype = 289
    Suffix = 290
    Index_Subtype = 291
    Parameter = 292
    Parameter_2 = 293
    Parameter_3 = 294
    Parameter_4 = 295
    Attr_Chain = 296
    Signal_Attribute_Declaration = 297
    Actual_Type = 298
    Actual_Type_Definition = 299
    Association_Chain = 300
    Individual_Association_Chain = 301
    Subprogram_Association_Chain = 302
    Aggregate_Info = 303
    Sub_Aggregate_Info = 304
    Aggr_Dynamic_Flag = 305
    Aggr_Min_Length = 306
    Aggr_Low_Limit = 307
    Aggr_High_Limit = 308
    Aggr_Others_Flag = 309
    Aggr_Named_Flag = 310
    Aggregate_Expand_Flag = 311
    Association_Choices_Chain = 312
    Case_Statement_Alternative_Chain = 313
    Choice_Staticness = 314
    Procedure_Call = 315
    Implementation = 316
    Parameter_Association_Chain = 317
    Method_Object = 318
    Subtype_Type_Mark = 319
    Subnature_Nature_Mark = 320
    Type_Conversion_Subtype = 321
    Type_Mark = 322
    File_Type_Mark = 323
    Return_Type_Mark = 324
    Has_Disconnect_Flag = 325
    Has_Active_Flag = 326
    Is_Within_Flag = 327
    Type_Marks_List = 328
    Implicit_Alias_Flag = 329
    Alias_Signature = 330
    Attribute_Signature = 331
    Overload_List = 332
    Simple_Name_Identifier = 333
    Simple_Name_Subtype = 334
    Protected_Type_Body = 335
    Protected_Type_Declaration = 336
    Use_Flag = 337
    End_Has_Reserved_Id = 338
    End_Has_Identifier = 339
    End_Has_Postponed = 340
    Has_Label = 341
    Has_Begin = 342
    Has_End = 343
    Has_Is = 344
    Has_Pure = 345
    Has_Body = 346
    Has_Parameter = 347
    Has_Component = 348
    Has_Identifier_List = 349
    Has_Mode = 350
    Has_Class = 351
    Has_Delay_Mechanism = 352
    Suspend_Flag = 353
    Is_Ref = 354
    Is_Forward_Ref = 355
    Psl_Property = 356
    Psl_Sequence = 357
    Psl_Declaration = 358
    Psl_Expression = 359
    Psl_Boolean = 360
    PSL_Clock = 361
    PSL_NFA = 362
    PSL_Nbr_States = 363
    PSL_Clock_Sensitivity = 364
    PSL_EOS_Flag = 365
    Count_Expression = 366
    Clock_Expression = 367
    Default_Clock = 368
    Foreign_Node = 369


def Get_Boolean(node, field):
    return libghdl.vhdl__nodes_meta__get_boolean(node, field)


def Get_Date_State_Type(node, field):
    return libghdl.vhdl__nodes_meta__get_date_state_type(node, field)


def Get_Date_Type(node, field):
    return libghdl.vhdl__nodes_meta__get_date_type(node, field)


def Get_Direction_Type(node, field):
    return libghdl.vhdl__nodes_meta__get_direction_type(node, field)


def Get_File_Checksum_Id(node, field):
    return libghdl.vhdl__nodes_meta__get_file_checksum_id(node, field)


def Get_Fp64(node, field):
    return libghdl.vhdl__nodes_meta__get_fp64(node, field)


def Get_Iir(node, field):
    return libghdl.vhdl__nodes_meta__get_iir(node, field)


def Get_Iir_All_Sensitized(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_all_sensitized(node, field)


def Get_Iir_Constraint(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_constraint(node, field)


def Get_Iir_Delay_Mechanism(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_delay_mechanism(node, field)


def Get_Iir_Flist(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_flist(node, field)


def Get_Iir_Force_Mode(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_force_mode(node, field)


def Get_Iir_Index32(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_index32(node, field)


def Get_Iir_Int32(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_int32(node, field)


def Get_Iir_List(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_list(node, field)


def Get_Iir_Mode(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_mode(node, field)


def Get_Iir_Predefined_Functions(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_predefined_functions(node, field)


def Get_Iir_Pure_State(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_pure_state(node, field)


def Get_Iir_Signal_Kind(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_signal_kind(node, field)


def Get_Iir_Staticness(node, field):
    return libghdl.vhdl__nodes_meta__get_iir_staticness(node, field)


def Get_Int32(node, field):
    return libghdl.vhdl__nodes_meta__get_int32(node, field)


def Get_Int64(node, field):
    return libghdl.vhdl__nodes_meta__get_int64(node, field)


def Get_Name_Id(node, field):
    return libghdl.vhdl__nodes_meta__get_name_id(node, field)


def Get_Number_Base_Type(node, field):
    return libghdl.vhdl__nodes_meta__get_number_base_type(node, field)


def Get_PSL_NFA(node, field):
    return libghdl.vhdl__nodes_meta__get_psl_nfa(node, field)


def Get_PSL_Node(node, field):
    return libghdl.vhdl__nodes_meta__get_psl_node(node, field)


def Get_Scalar_Size(node, field):
    return libghdl.vhdl__nodes_meta__get_scalar_size(node, field)


def Get_Source_File_Entry(node, field):
    return libghdl.vhdl__nodes_meta__get_source_file_entry(node, field)


def Get_Source_Ptr(node, field):
    return libghdl.vhdl__nodes_meta__get_source_ptr(node, field)


def Get_String8_Id(node, field):
    return libghdl.vhdl__nodes_meta__get_string8_id(node, field)


def Get_Time_Stamp_Id(node, field):
    return libghdl.vhdl__nodes_meta__get_time_stamp_id(node, field)


def Get_Token_Type(node, field):
    return libghdl.vhdl__nodes_meta__get_token_type(node, field)


def Get_Tri_State_Type(node, field):
    return libghdl.vhdl__nodes_meta__get_tri_state_type(node, field)


def Has_First_Design_Unit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_first_design_unit(kind)


def Has_Last_Design_Unit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_last_design_unit(kind)


def Has_Library_Declaration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_library_declaration(kind)


def Has_File_Checksum(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_file_checksum(kind)


def Has_Analysis_Time_Stamp(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_analysis_time_stamp(kind)


def Has_Design_File_Source(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_file_source(kind)


def Has_Library(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_library(kind)


def Has_File_Dependence_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_file_dependence_list(kind)


def Has_Design_File_Filename(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_file_filename(kind)


def Has_Design_File_Directory(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_file_directory(kind)


def Has_Design_File(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_file(kind)


def Has_Design_File_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_file_chain(kind)


def Has_Library_Directory(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_library_directory(kind)


def Has_Date(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_date(kind)


def Has_Context_Items(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_context_items(kind)


def Has_Dependence_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_dependence_list(kind)


def Has_Analysis_Checks_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_analysis_checks_list(kind)


def Has_Date_State(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_date_state(kind)


def Has_Guarded_Target_State(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_guarded_target_state(kind)


def Has_Library_Unit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_library_unit(kind)


def Has_Hash_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_hash_chain(kind)


def Has_Design_Unit_Source_Pos(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_unit_source_pos(kind)


def Has_Design_Unit_Source_Line(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_unit_source_line(kind)


def Has_Design_Unit_Source_Col(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_unit_source_col(kind)


def Has_Value(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_value(kind)


def Has_Enum_Pos(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_enum_pos(kind)


def Has_Physical_Literal(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_physical_literal(kind)


def Has_Fp_Value(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_fp_value(kind)


def Has_Simple_Aggregate_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_simple_aggregate_list(kind)


def Has_String8_Id(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_string8_id(kind)


def Has_String_Length(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_string_length(kind)


def Has_Bit_String_Base(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_bit_string_base(kind)


def Has_Has_Signed(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_signed(kind)


def Has_Has_Sign(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_sign(kind)


def Has_Has_Length(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_length(kind)


def Has_Literal_Length(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_literal_length(kind)


def Has_Literal_Origin(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_literal_origin(kind)


def Has_Range_Origin(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_range_origin(kind)


def Has_Literal_Subtype(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_literal_subtype(kind)


def Has_Allocator_Subtype(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_allocator_subtype(kind)


def Has_Entity_Class(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_entity_class(kind)


def Has_Entity_Name_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_entity_name_list(kind)


def Has_Attribute_Designator(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_attribute_designator(kind)


def Has_Attribute_Specification_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_attribute_specification_chain(kind)


def Has_Attribute_Specification(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_attribute_specification(kind)


def Has_Static_Attribute_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_static_attribute_flag(kind)


def Has_Signal_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_signal_list(kind)


def Has_Quantity_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_quantity_list(kind)


def Has_Designated_Entity(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_designated_entity(kind)


def Has_Formal(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_formal(kind)


def Has_Actual(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_actual(kind)


def Has_Actual_Conversion(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_actual_conversion(kind)


def Has_Formal_Conversion(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_formal_conversion(kind)


def Has_Whole_Association_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_whole_association_flag(kind)


def Has_Collapse_Signal_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_collapse_signal_flag(kind)


def Has_Artificial_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_artificial_flag(kind)


def Has_Open_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_open_flag(kind)


def Has_After_Drivers_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_after_drivers_flag(kind)


def Has_We_Value(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_we_value(kind)


def Has_Time(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_time(kind)


def Has_Associated_Expr(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_associated_expr(kind)


def Has_Associated_Block(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_associated_block(kind)


def Has_Associated_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_associated_chain(kind)


def Has_Choice_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_choice_name(kind)


def Has_Choice_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_choice_expression(kind)


def Has_Choice_Range(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_choice_range(kind)


def Has_Same_Alternative_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_same_alternative_flag(kind)


def Has_Element_Type_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_element_type_flag(kind)


def Has_Architecture(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_architecture(kind)


def Has_Block_Specification(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_block_specification(kind)


def Has_Prev_Block_Configuration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_prev_block_configuration(kind)


def Has_Configuration_Item_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_configuration_item_chain(kind)


def Has_Attribute_Value_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_attribute_value_chain(kind)


def Has_Spec_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_spec_chain(kind)


def Has_Value_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_value_chain(kind)


def Has_Attribute_Value_Spec_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_attribute_value_spec_chain(kind)


def Has_Entity_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_entity_name(kind)


def Has_Package(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_package(kind)


def Has_Package_Body(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_package_body(kind)


def Has_Instance_Package_Body(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_instance_package_body(kind)


def Has_Need_Body(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_need_body(kind)


def Has_Macro_Expanded_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_macro_expanded_flag(kind)


def Has_Need_Instance_Bodies(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_need_instance_bodies(kind)


def Has_Hierarchical_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_hierarchical_name(kind)


def Has_Inherit_Spec_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_inherit_spec_chain(kind)


def Has_Vunit_Item_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_vunit_item_chain(kind)


def Has_Bound_Vunit_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_bound_vunit_chain(kind)


def Has_Verification_Block_Configuration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_verification_block_configuration(kind)


def Has_Block_Configuration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_block_configuration(kind)


def Has_Concurrent_Statement_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_concurrent_statement_chain(kind)


def Has_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_chain(kind)


def Has_Port_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_port_chain(kind)


def Has_Generic_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_generic_chain(kind)


def Has_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_type(kind)


def Has_Subtype_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subtype_indication(kind)


def Has_Discrete_Range(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_discrete_range(kind)


def Has_Type_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_type_definition(kind)


def Has_Subtype_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subtype_definition(kind)


def Has_Incomplete_Type_Declaration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_incomplete_type_declaration(kind)


def Has_Interface_Type_Subprograms(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_interface_type_subprograms(kind)


def Has_Nature_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_nature_definition(kind)


def Has_Nature(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_nature(kind)


def Has_Subnature_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subnature_indication(kind)


def Has_Mode(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_mode(kind)


def Has_Guarded_Signal_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_guarded_signal_flag(kind)


def Has_Signal_Kind(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_signal_kind(kind)


def Has_Base_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_base_name(kind)


def Has_Interface_Declaration_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_interface_declaration_chain(kind)


def Has_Subprogram_Specification(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subprogram_specification(kind)


def Has_Sequential_Statement_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_sequential_statement_chain(kind)


def Has_Simultaneous_Statement_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_simultaneous_statement_chain(kind)


def Has_Subprogram_Body(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subprogram_body(kind)


def Has_Overload_Number(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_overload_number(kind)


def Has_Subprogram_Depth(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subprogram_depth(kind)


def Has_Subprogram_Hash(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subprogram_hash(kind)


def Has_Impure_Depth(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_impure_depth(kind)


def Has_Return_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_return_type(kind)


def Has_Implicit_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_implicit_definition(kind)


def Has_Uninstantiated_Subprogram_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_uninstantiated_subprogram_name(kind)


def Has_Default_Value(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_default_value(kind)


def Has_Deferred_Declaration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_deferred_declaration(kind)


def Has_Deferred_Declaration_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_deferred_declaration_flag(kind)


def Has_Shared_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_shared_flag(kind)


def Has_Design_Unit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_design_unit(kind)


def Has_Block_Statement(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_block_statement(kind)


def Has_Signal_Driver(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_signal_driver(kind)


def Has_Declaration_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_declaration_chain(kind)


def Has_File_Logical_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_file_logical_name(kind)


def Has_File_Open_Kind(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_file_open_kind(kind)


def Has_Element_Position(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_element_position(kind)


def Has_Use_Clause_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_use_clause_chain(kind)


def Has_Context_Reference_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_context_reference_chain(kind)


def Has_Selected_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_selected_name(kind)


def Has_Type_Declarator(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_type_declarator(kind)


def Has_Complete_Type_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_complete_type_definition(kind)


def Has_Incomplete_Type_Ref_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_incomplete_type_ref_chain(kind)


def Has_Associated_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_associated_type(kind)


def Has_Enumeration_Literal_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_enumeration_literal_list(kind)


def Has_Entity_Class_Entry_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_entity_class_entry_chain(kind)


def Has_Group_Constituent_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_group_constituent_list(kind)


def Has_Unit_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_unit_chain(kind)


def Has_Primary_Unit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_primary_unit(kind)


def Has_Identifier(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_identifier(kind)


def Has_Label(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_label(kind)


def Has_Visible_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_visible_flag(kind)


def Has_Range_Constraint(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_range_constraint(kind)


def Has_Direction(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_direction(kind)


def Has_Left_Limit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_left_limit(kind)


def Has_Right_Limit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_right_limit(kind)


def Has_Left_Limit_Expr(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_left_limit_expr(kind)


def Has_Right_Limit_Expr(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_right_limit_expr(kind)


def Has_Parent_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parent_type(kind)


def Has_Simple_Nature(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_simple_nature(kind)


def Has_Base_Nature(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_base_nature(kind)


def Has_Resolution_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_resolution_indication(kind)


def Has_Record_Element_Resolution_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_record_element_resolution_chain(kind)


def Has_Tolerance(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_tolerance(kind)


def Has_Plus_Terminal_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_plus_terminal_name(kind)


def Has_Minus_Terminal_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_minus_terminal_name(kind)


def Has_Plus_Terminal(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_plus_terminal(kind)


def Has_Minus_Terminal(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_minus_terminal(kind)


def Has_Magnitude_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_magnitude_expression(kind)


def Has_Phase_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_phase_expression(kind)


def Has_Power_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_power_expression(kind)


def Has_Simultaneous_Left(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_simultaneous_left(kind)


def Has_Simultaneous_Right(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_simultaneous_right(kind)


def Has_Text_File_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_text_file_flag(kind)


def Has_Only_Characters_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_only_characters_flag(kind)


def Has_Is_Character_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_is_character_type(kind)


def Has_Nature_Staticness(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_nature_staticness(kind)


def Has_Type_Staticness(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_type_staticness(kind)


def Has_Constraint_State(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_constraint_state(kind)


def Has_Index_Subtype_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_index_subtype_list(kind)


def Has_Index_Subtype_Definition_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_index_subtype_definition_list(kind)


def Has_Element_Subtype_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_element_subtype_indication(kind)


def Has_Element_Subtype(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_element_subtype(kind)


def Has_Element_Subnature_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_element_subnature_indication(kind)


def Has_Element_Subnature(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_element_subnature(kind)


def Has_Index_Constraint_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_index_constraint_list(kind)


def Has_Array_Element_Constraint(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_array_element_constraint(kind)


def Has_Has_Array_Constraint_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_array_constraint_flag(kind)


def Has_Has_Element_Constraint_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_element_constraint_flag(kind)


def Has_Elements_Declaration_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_elements_declaration_list(kind)


def Has_Owned_Elements_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_owned_elements_chain(kind)


def Has_Designated_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_designated_type(kind)


def Has_Designated_Subtype_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_designated_subtype_indication(kind)


def Has_Index_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_index_list(kind)


def Has_Reference(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_reference(kind)


def Has_Nature_Declarator(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_nature_declarator(kind)


def Has_Across_Type_Mark(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_across_type_mark(kind)


def Has_Through_Type_Mark(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_through_type_mark(kind)


def Has_Across_Type_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_across_type_definition(kind)


def Has_Through_Type_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_through_type_definition(kind)


def Has_Across_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_across_type(kind)


def Has_Through_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_through_type(kind)


def Has_Target(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_target(kind)


def Has_Waveform_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_waveform_chain(kind)


def Has_Guard(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_guard(kind)


def Has_Delay_Mechanism(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_delay_mechanism(kind)


def Has_Reject_Time_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_reject_time_expression(kind)


def Has_Force_Mode(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_force_mode(kind)


def Has_Has_Force_Mode(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_force_mode(kind)


def Has_Sensitivity_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_sensitivity_list(kind)


def Has_Process_Origin(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_process_origin(kind)


def Has_Package_Origin(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_package_origin(kind)


def Has_Condition_Clause(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_condition_clause(kind)


def Has_Break_Element(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_break_element(kind)


def Has_Selector_Quantity(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_selector_quantity(kind)


def Has_Break_Quantity(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_break_quantity(kind)


def Has_Timeout_Clause(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_timeout_clause(kind)


def Has_Postponed_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_postponed_flag(kind)


def Has_Callees_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_callees_list(kind)


def Has_Passive_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_passive_flag(kind)


def Has_Resolution_Function_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_resolution_function_flag(kind)


def Has_Wait_State(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_wait_state(kind)


def Has_All_Sensitized_State(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_all_sensitized_state(kind)


def Has_Seen_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_seen_flag(kind)


def Has_Pure_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_pure_flag(kind)


def Has_Foreign_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_foreign_flag(kind)


def Has_Resolved_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_resolved_flag(kind)


def Has_Signal_Type_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_signal_type_flag(kind)


def Has_Has_Signal_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_signal_flag(kind)


def Has_Purity_State(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_purity_state(kind)


def Has_Elab_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_elab_flag(kind)


def Has_Vendor_Library_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_vendor_library_flag(kind)


def Has_Configuration_Mark_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_configuration_mark_flag(kind)


def Has_Configuration_Done_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_configuration_done_flag(kind)


def Has_Index_Constraint_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_index_constraint_flag(kind)


def Has_Hide_Implicit_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_hide_implicit_flag(kind)


def Has_Assertion_Condition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_assertion_condition(kind)


def Has_Report_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_report_expression(kind)


def Has_Severity_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_severity_expression(kind)


def Has_Instantiated_Unit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_instantiated_unit(kind)


def Has_Generic_Map_Aspect_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_generic_map_aspect_chain(kind)


def Has_Port_Map_Aspect_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_port_map_aspect_chain(kind)


def Has_Configuration_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_configuration_name(kind)


def Has_Component_Configuration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_component_configuration(kind)


def Has_Configuration_Specification(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_configuration_specification(kind)


def Has_Default_Binding_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_default_binding_indication(kind)


def Has_Default_Configuration_Declaration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_default_configuration_declaration(kind)


def Has_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_expression(kind)


def Has_Conditional_Expression_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_conditional_expression_chain(kind)


def Has_Allocator_Designated_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_allocator_designated_type(kind)


def Has_Selected_Waveform_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_selected_waveform_chain(kind)


def Has_Conditional_Waveform_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_conditional_waveform_chain(kind)


def Has_Guard_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_guard_expression(kind)


def Has_Guard_Decl(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_guard_decl(kind)


def Has_Guard_Sensitivity_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_guard_sensitivity_list(kind)


def Has_Signal_Attribute_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_signal_attribute_chain(kind)


def Has_Block_Block_Configuration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_block_block_configuration(kind)


def Has_Package_Header(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_package_header(kind)


def Has_Block_Header(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_block_header(kind)


def Has_Uninstantiated_Package_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_uninstantiated_package_name(kind)


def Has_Uninstantiated_Package_Decl(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_uninstantiated_package_decl(kind)


def Has_Instance_Source_File(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_instance_source_file(kind)


def Has_Generate_Block_Configuration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_generate_block_configuration(kind)


def Has_Generate_Statement_Body(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_generate_statement_body(kind)


def Has_Alternative_Label(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_alternative_label(kind)


def Has_Generate_Else_Clause(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_generate_else_clause(kind)


def Has_Condition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_condition(kind)


def Has_Else_Clause(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_else_clause(kind)


def Has_Parameter_Specification(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parameter_specification(kind)


def Has_Parent(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parent(kind)


def Has_Loop_Label(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_loop_label(kind)


def Has_Exit_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_exit_flag(kind)


def Has_Next_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_next_flag(kind)


def Has_Component_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_component_name(kind)


def Has_Instantiation_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_instantiation_list(kind)


def Has_Entity_Aspect(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_entity_aspect(kind)


def Has_Default_Entity_Aspect(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_default_entity_aspect(kind)


def Has_Binding_Indication(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_binding_indication(kind)


def Has_Named_Entity(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_named_entity(kind)


def Has_Referenced_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_referenced_name(kind)


def Has_Expr_Staticness(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_expr_staticness(kind)


def Has_Scalar_Size(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_scalar_size(kind)


def Has_Error_Origin(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_error_origin(kind)


def Has_Operand(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_operand(kind)


def Has_Left(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_left(kind)


def Has_Right(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_right(kind)


def Has_Unit_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_unit_name(kind)


def Has_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_name(kind)


def Has_Group_Template_Name(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_group_template_name(kind)


def Has_Name_Staticness(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_name_staticness(kind)


def Has_Prefix(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_prefix(kind)


def Has_Signature_Prefix(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_signature_prefix(kind)


def Has_External_Pathname(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_external_pathname(kind)


def Has_Pathname_Suffix(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_pathname_suffix(kind)


def Has_Pathname_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_pathname_expression(kind)


def Has_In_Formal_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_in_formal_flag(kind)


def Has_Slice_Subtype(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_slice_subtype(kind)


def Has_Suffix(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_suffix(kind)


def Has_Index_Subtype(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_index_subtype(kind)


def Has_Parameter(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parameter(kind)


def Has_Parameter_2(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parameter_2(kind)


def Has_Parameter_3(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parameter_3(kind)


def Has_Parameter_4(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parameter_4(kind)


def Has_Attr_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_attr_chain(kind)


def Has_Signal_Attribute_Declaration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_signal_attribute_declaration(kind)


def Has_Actual_Type(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_actual_type(kind)


def Has_Actual_Type_Definition(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_actual_type_definition(kind)


def Has_Association_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_association_chain(kind)


def Has_Individual_Association_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_individual_association_chain(kind)


def Has_Subprogram_Association_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subprogram_association_chain(kind)


def Has_Aggregate_Info(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggregate_info(kind)


def Has_Sub_Aggregate_Info(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_sub_aggregate_info(kind)


def Has_Aggr_Dynamic_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggr_dynamic_flag(kind)


def Has_Aggr_Min_Length(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggr_min_length(kind)


def Has_Aggr_Low_Limit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggr_low_limit(kind)


def Has_Aggr_High_Limit(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggr_high_limit(kind)


def Has_Aggr_Others_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggr_others_flag(kind)


def Has_Aggr_Named_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggr_named_flag(kind)


def Has_Aggregate_Expand_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_aggregate_expand_flag(kind)


def Has_Association_Choices_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_association_choices_chain(kind)


def Has_Case_Statement_Alternative_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_case_statement_alternative_chain(kind)


def Has_Choice_Staticness(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_choice_staticness(kind)


def Has_Procedure_Call(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_procedure_call(kind)


def Has_Implementation(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_implementation(kind)


def Has_Parameter_Association_Chain(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_parameter_association_chain(kind)


def Has_Method_Object(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_method_object(kind)


def Has_Subtype_Type_Mark(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subtype_type_mark(kind)


def Has_Subnature_Nature_Mark(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_subnature_nature_mark(kind)


def Has_Type_Conversion_Subtype(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_type_conversion_subtype(kind)


def Has_Type_Mark(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_type_mark(kind)


def Has_File_Type_Mark(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_file_type_mark(kind)


def Has_Return_Type_Mark(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_return_type_mark(kind)


def Has_Has_Disconnect_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_disconnect_flag(kind)


def Has_Has_Active_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_active_flag(kind)


def Has_Is_Within_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_is_within_flag(kind)


def Has_Type_Marks_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_type_marks_list(kind)


def Has_Implicit_Alias_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_implicit_alias_flag(kind)


def Has_Alias_Signature(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_alias_signature(kind)


def Has_Attribute_Signature(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_attribute_signature(kind)


def Has_Overload_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_overload_list(kind)


def Has_Simple_Name_Identifier(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_simple_name_identifier(kind)


def Has_Simple_Name_Subtype(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_simple_name_subtype(kind)


def Has_Protected_Type_Body(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_protected_type_body(kind)


def Has_Protected_Type_Declaration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_protected_type_declaration(kind)


def Has_Use_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_use_flag(kind)


def Has_End_Has_Reserved_Id(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_end_has_reserved_id(kind)


def Has_End_Has_Identifier(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_end_has_identifier(kind)


def Has_End_Has_Postponed(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_end_has_postponed(kind)


def Has_Has_Label(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_label(kind)


def Has_Has_Begin(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_begin(kind)


def Has_Has_End(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_end(kind)


def Has_Has_Is(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_is(kind)


def Has_Has_Pure(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_pure(kind)


def Has_Has_Body(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_body(kind)


def Has_Has_Parameter(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_parameter(kind)


def Has_Has_Component(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_component(kind)


def Has_Has_Identifier_List(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_identifier_list(kind)


def Has_Has_Mode(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_mode(kind)


def Has_Has_Class(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_class(kind)


def Has_Has_Delay_Mechanism(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_has_delay_mechanism(kind)


def Has_Suspend_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_suspend_flag(kind)


def Has_Is_Ref(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_is_ref(kind)


def Has_Is_Forward_Ref(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_is_forward_ref(kind)


def Has_Psl_Property(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_property(kind)


def Has_Psl_Sequence(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_sequence(kind)


def Has_Psl_Declaration(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_declaration(kind)


def Has_Psl_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_expression(kind)


def Has_Psl_Boolean(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_boolean(kind)


def Has_PSL_Clock(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_clock(kind)


def Has_PSL_NFA(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_nfa(kind)


def Has_PSL_Nbr_States(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_nbr_states(kind)


def Has_PSL_Clock_Sensitivity(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_clock_sensitivity(kind)


def Has_PSL_EOS_Flag(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_psl_eos_flag(kind)


def Has_Count_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_count_expression(kind)


def Has_Clock_Expression(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_clock_expression(kind)


def Has_Default_Clock(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_default_clock(kind)


def Has_Foreign_Node(kind) -> bool:
    return libghdl.vhdl__nodes_meta__has_foreign_node(kind)
