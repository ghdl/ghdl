# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
from enum import IntEnum, unique
from pyTooling.Decorators import export

from pyGHDL.libghdl._decorator import BindToLibGHDL

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import IirKind


# From nodes_meta
@export
@BindToLibGHDL("vhdl__nodes_meta__get_fields_first")
def get_fields_first(K: IirKind) -> int:
    """
    Return the list of fields for node :obj:`K`.

    In Ada ``Vhdl.Nodes_Meta.Get_Fields`` returns a ``Fields_Array``. To emulate
    this array access, the API provides ``get_fields_first`` and :func:`get_fields_last`.

    The fields are sorted: first the non nodes/list of nodes, then the
    nodes/lists that aren't reference, and then the reference.

    :param K: Node to get first array index from.
    """
    return 0


@export
@BindToLibGHDL("vhdl__nodes_meta__get_fields_last")
def get_fields_last(K: IirKind) -> int:
    """
    Return the list of fields for node :obj:`K`.

    In Ada ``Vhdl.Nodes_Meta.Get_Fields`` returns a ``Fields_Array``. To emulate
    this array access, the API provides :func:`get_fields_first` and ``get_fields_last``.

    The fields are sorted: first the non nodes/list of nodes, then the
    nodes/lists that aren't reference, and then the reference.

    :param K: Node to get last array index from.
    """
    return 0


@export
@BindToLibGHDL("vhdl__nodes_meta__get_field_by_index")
def get_field_by_index(K: IirKind) -> int:
    """"""
    return 0


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
    Open_Actual = 51
    Actual_Conversion = 52
    Formal_Conversion = 53
    Whole_Association_Flag = 54
    Collapse_Signal_Flag = 55
    Artificial_Flag = 56
    Open_Flag = 57
    After_Drivers_Flag = 58
    We_Value = 59
    Time = 60
    Associated_Expr = 61
    Associated_Block = 62
    Associated_Chain = 63
    Choice_Name = 64
    Choice_Expression = 65
    Choice_Range = 66
    Same_Alternative_Flag = 67
    Element_Type_Flag = 68
    Architecture = 69
    Block_Specification = 70
    Prev_Block_Configuration = 71
    Configuration_Item_Chain = 72
    Attribute_Value_Chain = 73
    Spec_Chain = 74
    Value_Chain = 75
    Attribute_Value_Spec_Chain = 76
    Entity_Name = 77
    Package = 78
    Package_Body = 79
    Instance_Package_Body = 80
    Need_Body = 81
    Macro_Expanded_Flag = 82
    Need_Instance_Bodies = 83
    Hierarchical_Name = 84
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
    Interface_Type_Definition = 100
    Nature_Definition = 101
    Nature = 102
    Subnature_Indication = 103
    Reference_Terminal_Flag = 104
    Mode = 105
    Guarded_Signal_Flag = 106
    Signal_Kind = 107
    Base_Name = 108
    Interface_Declaration_Chain = 109
    Default_Subprogram = 110
    Subprogram_Specification = 111
    Sequential_Statement_Chain = 112
    Simultaneous_Statement_Chain = 113
    Subprogram_Body = 114
    Overload_Number = 115
    Subprogram_Depth = 116
    Subprogram_Hash = 117
    Impure_Depth = 118
    Return_Type = 119
    Implicit_Definition = 120
    Uninstantiated_Subprogram_Name = 121
    Default_Value = 122
    Deferred_Declaration = 123
    Deferred_Declaration_Flag = 124
    Shared_Flag = 125
    Design_Unit = 126
    Block_Statement = 127
    Signal_Driver = 128
    Declaration_Chain = 129
    File_Logical_Name = 130
    File_Open_Kind = 131
    Element_Position = 132
    Use_Clause_Chain = 133
    Context_Reference_Chain = 134
    Inherit_Spec_Chain = 135
    Selected_Name = 136
    Type_Declarator = 137
    Complete_Type_Definition = 138
    Incomplete_Type_Ref_Chain = 139
    Associated_Type = 140
    Enumeration_Literal_List = 141
    Entity_Class_Entry_Chain = 142
    Group_Constituent_List = 143
    Unit_Chain = 144
    Primary_Unit = 145
    Identifier = 146
    Label = 147
    Return_Identifier = 148
    Visible_Flag = 149
    Range_Constraint = 150
    Direction = 151
    Left_Limit = 152
    Right_Limit = 153
    Left_Limit_Expr = 154
    Right_Limit_Expr = 155
    Parent_Type = 156
    Simple_Nature = 157
    Base_Nature = 158
    Resolution_Indication = 159
    Record_Element_Resolution_Chain = 160
    Tolerance = 161
    Plus_Terminal_Name = 162
    Minus_Terminal_Name = 163
    Plus_Terminal = 164
    Minus_Terminal = 165
    Magnitude_Expression = 166
    Phase_Expression = 167
    Power_Expression = 168
    Simultaneous_Left = 169
    Simultaneous_Right = 170
    Text_File_Flag = 171
    Only_Characters_Flag = 172
    Is_Character_Type = 173
    Nature_Staticness = 174
    Type_Staticness = 175
    Constraint_State = 176
    Index_Subtype_List = 177
    Index_Subtype_Definition_List = 178
    Element_Subtype_Indication = 179
    Element_Subtype = 180
    Element_Subnature_Indication = 181
    Element_Subnature = 182
    Index_Constraint_List = 183
    Array_Element_Constraint = 184
    Has_Array_Constraint_Flag = 185
    Has_Element_Constraint_Flag = 186
    Elements_Declaration_List = 187
    Owned_Elements_Chain = 188
    Designated_Type = 189
    Designated_Subtype_Indication = 190
    Index_List = 191
    Reference = 192
    Nature_Declarator = 193
    Across_Type_Mark = 194
    Through_Type_Mark = 195
    Across_Type_Definition = 196
    Through_Type_Definition = 197
    Across_Type = 198
    Through_Type = 199
    Target = 200
    Waveform_Chain = 201
    Guard = 202
    Delay_Mechanism = 203
    Reject_Time_Expression = 204
    Force_Mode = 205
    Has_Force_Mode = 206
    Sensitivity_List = 207
    Process_Origin = 208
    Package_Origin = 209
    Condition_Clause = 210
    Break_Element = 211
    Selector_Quantity = 212
    Break_Quantity = 213
    Timeout_Clause = 214
    Postponed_Flag = 215
    Callees_List = 216
    Passive_Flag = 217
    Resolution_Function_Flag = 218
    Wait_State = 219
    All_Sensitized_State = 220
    Seen_Flag = 221
    Pure_Flag = 222
    Foreign_Flag = 223
    Resolved_Flag = 224
    Signal_Type_Flag = 225
    Has_Signal_Flag = 226
    Purity_State = 227
    Elab_Flag = 228
    Vendor_Library_Flag = 229
    Configuration_Mark_Flag = 230
    Configuration_Done_Flag = 231
    Index_Constraint_Flag = 232
    Hide_Implicit_Flag = 233
    Assertion_Condition = 234
    Report_Expression = 235
    Severity_Expression = 236
    Instantiated_Unit = 237
    Generic_Map_Aspect_Chain = 238
    Port_Map_Aspect_Chain = 239
    Configuration_Name = 240
    Component_Configuration = 241
    Configuration_Specification = 242
    Default_Binding_Indication = 243
    Default_Configuration_Declaration = 244
    Expression = 245
    Conditional_Expression_Chain = 246
    Allocator_Designated_Type = 247
    Selected_Waveform_Chain = 248
    Conditional_Waveform_Chain = 249
    Guard_Expression = 250
    Guard_Decl = 251
    Guard_Sensitivity_List = 252
    Attribute_Implicit_Chain = 253
    Block_Block_Configuration = 254
    Package_Header = 255
    Block_Header = 256
    Uninstantiated_Package_Name = 257
    Uninstantiated_Package_Decl = 258
    Instance_Source_File = 259
    Generate_Block_Configuration = 260
    Generate_Statement_Body = 261
    Alternative_Label = 262
    Generate_Else_Clause = 263
    Condition = 264
    Else_Clause = 265
    Parameter_Specification = 266
    Parent = 267
    Loop_Label = 268
    Exit_Flag = 269
    Next_Flag = 270
    Component_Name = 271
    Instantiation_List = 272
    Entity_Aspect = 273
    Default_Entity_Aspect = 274
    Binding_Indication = 275
    Named_Entity = 276
    Referenced_Name = 277
    Expr_Staticness = 278
    Scalar_Size = 279
    Error_Origin = 280
    Operand = 281
    Left = 282
    Right = 283
    Unit_Name = 284
    Name = 285
    Group_Template_Name = 286
    Name_Staticness = 287
    Prefix = 288
    Signature_Prefix = 289
    External_Pathname = 290
    Pathname_Suffix = 291
    Pathname_Expression = 292
    In_Formal_Flag = 293
    Inertial_Flag = 294
    Slice_Subtype = 295
    Suffix = 296
    Index_Subtype = 297
    Parameter = 298
    Parameter_2 = 299
    Parameter_3 = 300
    Parameter_4 = 301
    Attr_Chain = 302
    Attribute_Implicit_Declaration = 303
    Actual_Type = 304
    Actual_Type_Definition = 305
    Association_Chain = 306
    Individual_Association_Chain = 307
    Subprogram_Association_Chain = 308
    Aggregate_Info = 309
    Sub_Aggregate_Info = 310
    Aggr_Dynamic_Flag = 311
    Aggr_Min_Length = 312
    Aggr_Low_Limit = 313
    Aggr_High_Limit = 314
    Aggr_Others_Flag = 315
    Aggr_Named_Flag = 316
    Aggregate_Expand_Flag = 317
    Determined_Aggregate_Flag = 318
    Association_Choices_Chain = 319
    Case_Statement_Alternative_Chain = 320
    Matching_Flag = 321
    Choice_Staticness = 322
    Procedure_Call = 323
    Implementation = 324
    Parameter_Association_Chain = 325
    Method_Object = 326
    Subtype_Type_Mark = 327
    Subnature_Nature_Mark = 328
    Type_Conversion_Subtype = 329
    Type_Mark = 330
    File_Type_Mark = 331
    Return_Type_Mark = 332
    Has_Disconnect_Flag = 333
    Has_Active_Flag = 334
    Is_Within_Flag = 335
    Type_Marks_List = 336
    Implicit_Alias_Flag = 337
    Alias_Signature = 338
    Attribute_Signature = 339
    Overload_List = 340
    Simple_Name_Identifier = 341
    Simple_Name_Subtype = 342
    Protected_Type_Body = 343
    Protected_Type_Declaration = 344
    Use_Flag = 345
    End_Has_Reserved_Id = 346
    End_Has_Identifier = 347
    End_Has_Postponed = 348
    Has_Label = 349
    Has_Begin = 350
    Has_End = 351
    Has_Is = 352
    Has_Pure = 353
    Has_Body = 354
    Has_Parameter = 355
    Has_Component = 356
    Has_Identifier_List = 357
    Has_Mode = 358
    Has_Class = 359
    Has_Delay_Mechanism = 360
    Suspend_Flag = 361
    Stop_Flag = 362
    Is_Ref = 363
    Is_Forward_Ref = 364
    Psl_Property = 365
    Psl_Sequence = 366
    Psl_Declaration = 367
    Psl_Expression = 368
    Psl_Boolean = 369
    PSL_Clock = 370
    PSL_NFA = 371
    PSL_Nbr_States = 372
    PSL_Clock_Sensitivity = 373
    PSL_EOS_Flag = 374
    PSL_Abort_Flag = 375
    Count_Expression = 376
    Clock_Expression = 377
    Default_Clock = 378
    Foreign_Node = 379
    Suspend_State_Index = 380
    Suspend_State_Chain = 381


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


@export
@BindToLibGHDL("vhdl__nodes_meta__has_first_design_unit")
def Has_First_Design_Unit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_last_design_unit")
def Has_Last_Design_Unit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library_declaration")
def Has_Library_Declaration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_checksum")
def Has_File_Checksum(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_analysis_time_stamp")
def Has_Analysis_Time_Stamp(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_source")
def Has_Design_File_Source(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library")
def Has_Library(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_dependence_list")
def Has_File_Dependence_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_filename")
def Has_Design_File_Filename(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_directory")
def Has_Design_File_Directory(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file")
def Has_Design_File(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_chain")
def Has_Design_File_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library_directory")
def Has_Library_Directory(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_date")
def Has_Date(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_context_items")
def Has_Context_Items(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_dependence_list")
def Has_Dependence_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_analysis_checks_list")
def Has_Analysis_Checks_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_date_state")
def Has_Date_State(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guarded_target_state")
def Has_Guarded_Target_State(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library_unit")
def Has_Library_Unit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_hash_chain")
def Has_Hash_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit_source_pos")
def Has_Design_Unit_Source_Pos(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit_source_line")
def Has_Design_Unit_Source_Line(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit_source_col")
def Has_Design_Unit_Source_Col(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_value")
def Has_Value(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_enum_pos")
def Has_Enum_Pos(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_physical_literal")
def Has_Physical_Literal(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_fp_value")
def Has_Fp_Value(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_aggregate_list")
def Has_Simple_Aggregate_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_string8_id")
def Has_String8_Id(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_string_length")
def Has_String_Length(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_bit_string_base")
def Has_Bit_String_Base(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_signed")
def Has_Has_Signed(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_sign")
def Has_Has_Sign(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_length")
def Has_Has_Length(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_literal_length")
def Has_Literal_Length(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_literal_origin")
def Has_Literal_Origin(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_range_origin")
def Has_Range_Origin(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_literal_subtype")
def Has_Literal_Subtype(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_allocator_subtype")
def Has_Allocator_Subtype(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_class")
def Has_Entity_Class(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_name_list")
def Has_Entity_Name_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_designator")
def Has_Attribute_Designator(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_specification_chain")
def Has_Attribute_Specification_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_specification")
def Has_Attribute_Specification(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_static_attribute_flag")
def Has_Static_Attribute_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_list")
def Has_Signal_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_quantity_list")
def Has_Quantity_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_designated_entity")
def Has_Designated_Entity(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_formal")
def Has_Formal(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual")
def Has_Actual(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_open_actual")
def Has_Open_Actual(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual_conversion")
def Has_Actual_Conversion(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_formal_conversion")
def Has_Formal_Conversion(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_whole_association_flag")
def Has_Whole_Association_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_collapse_signal_flag")
def Has_Collapse_Signal_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_artificial_flag")
def Has_Artificial_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_open_flag")
def Has_Open_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_after_drivers_flag")
def Has_After_Drivers_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_we_value")
def Has_We_Value(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_time")
def Has_Time(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_expr")
def Has_Associated_Expr(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_block")
def Has_Associated_Block(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_chain")
def Has_Associated_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_name")
def Has_Choice_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_expression")
def Has_Choice_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_range")
def Has_Choice_Range(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_same_alternative_flag")
def Has_Same_Alternative_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_type_flag")
def Has_Element_Type_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_architecture")
def Has_Architecture(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_specification")
def Has_Block_Specification(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_prev_block_configuration")
def Has_Prev_Block_Configuration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_item_chain")
def Has_Configuration_Item_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_value_chain")
def Has_Attribute_Value_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_spec_chain")
def Has_Spec_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_value_chain")
def Has_Value_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_value_spec_chain")
def Has_Attribute_Value_Spec_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_name")
def Has_Entity_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package")
def Has_Package(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package_body")
def Has_Package_Body(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instance_package_body")
def Has_Instance_Package_Body(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_need_body")
def Has_Need_Body(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_macro_expanded_flag")
def Has_Macro_Expanded_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_need_instance_bodies")
def Has_Need_Instance_Bodies(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_hierarchical_name")
def Has_Hierarchical_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_vunit_item_chain")
def Has_Vunit_Item_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_bound_vunit_chain")
def Has_Bound_Vunit_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_verification_block_configuration")
def Has_Verification_Block_Configuration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_configuration")
def Has_Block_Configuration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_concurrent_statement_chain")
def Has_Concurrent_Statement_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_chain")
def Has_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_port_chain")
def Has_Port_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generic_chain")
def Has_Generic_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type")
def Has_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subtype_indication")
def Has_Subtype_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_discrete_range")
def Has_Discrete_Range(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_definition")
def Has_Type_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subtype_definition")
def Has_Subtype_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_incomplete_type_declaration")
def Has_Incomplete_Type_Declaration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_interface_type_subprograms")
def Has_Interface_Type_Subprograms(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_interface_type_definition")
def Has_Interface_Type_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature_definition")
def Has_Nature_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature")
def Has_Nature(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subnature_indication")
def Has_Subnature_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_reference_terminal_flag")
def Has_Reference_Terminal_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_mode")
def Has_Mode(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guarded_signal_flag")
def Has_Guarded_Signal_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_kind")
def Has_Signal_Kind(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_base_name")
def Has_Base_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_interface_declaration_chain")
def Has_Interface_Declaration_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_subprogram")
def Has_Default_Subprogram(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_specification")
def Has_Subprogram_Specification(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_sequential_statement_chain")
def Has_Sequential_Statement_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simultaneous_statement_chain")
def Has_Simultaneous_Statement_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_body")
def Has_Subprogram_Body(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_overload_number")
def Has_Overload_Number(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_depth")
def Has_Subprogram_Depth(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_hash")
def Has_Subprogram_Hash(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_impure_depth")
def Has_Impure_Depth(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_return_type")
def Has_Return_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_implicit_definition")
def Has_Implicit_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_uninstantiated_subprogram_name")
def Has_Uninstantiated_Subprogram_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_value")
def Has_Default_Value(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_deferred_declaration")
def Has_Deferred_Declaration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_deferred_declaration_flag")
def Has_Deferred_Declaration_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_shared_flag")
def Has_Shared_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit")
def Has_Design_Unit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_statement")
def Has_Block_Statement(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_driver")
def Has_Signal_Driver(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_declaration_chain")
def Has_Declaration_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_logical_name")
def Has_File_Logical_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_open_kind")
def Has_File_Open_Kind(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_position")
def Has_Element_Position(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_use_clause_chain")
def Has_Use_Clause_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_context_reference_chain")
def Has_Context_Reference_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_inherit_spec_chain")
def Has_Inherit_Spec_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_selected_name")
def Has_Selected_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_declarator")
def Has_Type_Declarator(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_complete_type_definition")
def Has_Complete_Type_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_incomplete_type_ref_chain")
def Has_Incomplete_Type_Ref_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_type")
def Has_Associated_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_enumeration_literal_list")
def Has_Enumeration_Literal_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_class_entry_chain")
def Has_Entity_Class_Entry_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_group_constituent_list")
def Has_Group_Constituent_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_unit_chain")
def Has_Unit_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_primary_unit")
def Has_Primary_Unit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_identifier")
def Has_Identifier(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_label")
def Has_Label(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_return_identifier")
def Has_Return_Identifier(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_visible_flag")
def Has_Visible_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_range_constraint")
def Has_Range_Constraint(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_direction")
def Has_Direction(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_left_limit")
def Has_Left_Limit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_right_limit")
def Has_Right_Limit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_left_limit_expr")
def Has_Left_Limit_Expr(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_right_limit_expr")
def Has_Right_Limit_Expr(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parent_type")
def Has_Parent_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_nature")
def Has_Simple_Nature(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_base_nature")
def Has_Base_Nature(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_resolution_indication")
def Has_Resolution_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_record_element_resolution_chain")
def Has_Record_Element_Resolution_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_tolerance")
def Has_Tolerance(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_plus_terminal_name")
def Has_Plus_Terminal_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_minus_terminal_name")
def Has_Minus_Terminal_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_plus_terminal")
def Has_Plus_Terminal(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_minus_terminal")
def Has_Minus_Terminal(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_magnitude_expression")
def Has_Magnitude_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_phase_expression")
def Has_Phase_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_power_expression")
def Has_Power_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simultaneous_left")
def Has_Simultaneous_Left(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simultaneous_right")
def Has_Simultaneous_Right(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_text_file_flag")
def Has_Text_File_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_only_characters_flag")
def Has_Only_Characters_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_character_type")
def Has_Is_Character_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature_staticness")
def Has_Nature_Staticness(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_staticness")
def Has_Type_Staticness(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_constraint_state")
def Has_Constraint_State(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_subtype_list")
def Has_Index_Subtype_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_subtype_definition_list")
def Has_Index_Subtype_Definition_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subtype_indication")
def Has_Element_Subtype_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subtype")
def Has_Element_Subtype(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subnature_indication")
def Has_Element_Subnature_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subnature")
def Has_Element_Subnature(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_constraint_list")
def Has_Index_Constraint_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_array_element_constraint")
def Has_Array_Element_Constraint(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_array_constraint_flag")
def Has_Has_Array_Constraint_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_element_constraint_flag")
def Has_Has_Element_Constraint_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_elements_declaration_list")
def Has_Elements_Declaration_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_owned_elements_chain")
def Has_Owned_Elements_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_designated_type")
def Has_Designated_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_designated_subtype_indication")
def Has_Designated_Subtype_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_list")
def Has_Index_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_reference")
def Has_Reference(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature_declarator")
def Has_Nature_Declarator(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_across_type_mark")
def Has_Across_Type_Mark(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_through_type_mark")
def Has_Through_Type_Mark(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_across_type_definition")
def Has_Across_Type_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_through_type_definition")
def Has_Through_Type_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_across_type")
def Has_Across_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_through_type")
def Has_Through_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_target")
def Has_Target(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_waveform_chain")
def Has_Waveform_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard")
def Has_Guard(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_delay_mechanism")
def Has_Delay_Mechanism(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_reject_time_expression")
def Has_Reject_Time_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_force_mode")
def Has_Force_Mode(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_force_mode")
def Has_Has_Force_Mode(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_sensitivity_list")
def Has_Sensitivity_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_process_origin")
def Has_Process_Origin(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package_origin")
def Has_Package_Origin(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_condition_clause")
def Has_Condition_Clause(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_break_element")
def Has_Break_Element(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_selector_quantity")
def Has_Selector_Quantity(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_break_quantity")
def Has_Break_Quantity(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_timeout_clause")
def Has_Timeout_Clause(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_postponed_flag")
def Has_Postponed_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_callees_list")
def Has_Callees_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_passive_flag")
def Has_Passive_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_resolution_function_flag")
def Has_Resolution_Function_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_wait_state")
def Has_Wait_State(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_all_sensitized_state")
def Has_All_Sensitized_State(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_seen_flag")
def Has_Seen_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_pure_flag")
def Has_Pure_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_foreign_flag")
def Has_Foreign_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_resolved_flag")
def Has_Resolved_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_type_flag")
def Has_Signal_Type_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_signal_flag")
def Has_Has_Signal_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_purity_state")
def Has_Purity_State(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_elab_flag")
def Has_Elab_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_vendor_library_flag")
def Has_Vendor_Library_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_mark_flag")
def Has_Configuration_Mark_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_done_flag")
def Has_Configuration_Done_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_constraint_flag")
def Has_Index_Constraint_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_hide_implicit_flag")
def Has_Hide_Implicit_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_assertion_condition")
def Has_Assertion_Condition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_report_expression")
def Has_Report_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_severity_expression")
def Has_Severity_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instantiated_unit")
def Has_Instantiated_Unit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generic_map_aspect_chain")
def Has_Generic_Map_Aspect_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_port_map_aspect_chain")
def Has_Port_Map_Aspect_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_name")
def Has_Configuration_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_component_configuration")
def Has_Component_Configuration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_specification")
def Has_Configuration_Specification(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_binding_indication")
def Has_Default_Binding_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_configuration_declaration")
def Has_Default_Configuration_Declaration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_expression")
def Has_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_conditional_expression_chain")
def Has_Conditional_Expression_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_allocator_designated_type")
def Has_Allocator_Designated_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_selected_waveform_chain")
def Has_Selected_Waveform_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_conditional_waveform_chain")
def Has_Conditional_Waveform_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard_expression")
def Has_Guard_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard_decl")
def Has_Guard_Decl(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard_sensitivity_list")
def Has_Guard_Sensitivity_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_implicit_chain")
def Has_Attribute_Implicit_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_block_configuration")
def Has_Block_Block_Configuration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package_header")
def Has_Package_Header(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_header")
def Has_Block_Header(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_uninstantiated_package_name")
def Has_Uninstantiated_Package_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_uninstantiated_package_decl")
def Has_Uninstantiated_Package_Decl(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instance_source_file")
def Has_Instance_Source_File(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generate_block_configuration")
def Has_Generate_Block_Configuration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generate_statement_body")
def Has_Generate_Statement_Body(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_alternative_label")
def Has_Alternative_Label(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generate_else_clause")
def Has_Generate_Else_Clause(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_condition")
def Has_Condition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_else_clause")
def Has_Else_Clause(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_specification")
def Has_Parameter_Specification(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parent")
def Has_Parent(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_loop_label")
def Has_Loop_Label(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_exit_flag")
def Has_Exit_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_next_flag")
def Has_Next_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_component_name")
def Has_Component_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instantiation_list")
def Has_Instantiation_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_aspect")
def Has_Entity_Aspect(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_entity_aspect")
def Has_Default_Entity_Aspect(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_binding_indication")
def Has_Binding_Indication(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_named_entity")
def Has_Named_Entity(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_referenced_name")
def Has_Referenced_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_expr_staticness")
def Has_Expr_Staticness(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_scalar_size")
def Has_Scalar_Size(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_error_origin")
def Has_Error_Origin(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_operand")
def Has_Operand(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_left")
def Has_Left(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_right")
def Has_Right(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_unit_name")
def Has_Unit_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_name")
def Has_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_group_template_name")
def Has_Group_Template_Name(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_name_staticness")
def Has_Name_Staticness(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_prefix")
def Has_Prefix(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signature_prefix")
def Has_Signature_Prefix(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_external_pathname")
def Has_External_Pathname(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_pathname_suffix")
def Has_Pathname_Suffix(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_pathname_expression")
def Has_Pathname_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_in_formal_flag")
def Has_In_Formal_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_inertial_flag")
def Has_Inertial_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_slice_subtype")
def Has_Slice_Subtype(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suffix")
def Has_Suffix(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_subtype")
def Has_Index_Subtype(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter")
def Has_Parameter(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_2")
def Has_Parameter_2(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_3")
def Has_Parameter_3(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_4")
def Has_Parameter_4(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attr_chain")
def Has_Attr_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_implicit_declaration")
def Has_Attribute_Implicit_Declaration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual_type")
def Has_Actual_Type(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual_type_definition")
def Has_Actual_Type_Definition(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_association_chain")
def Has_Association_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_individual_association_chain")
def Has_Individual_Association_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_association_chain")
def Has_Subprogram_Association_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggregate_info")
def Has_Aggregate_Info(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_sub_aggregate_info")
def Has_Sub_Aggregate_Info(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_dynamic_flag")
def Has_Aggr_Dynamic_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_min_length")
def Has_Aggr_Min_Length(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_low_limit")
def Has_Aggr_Low_Limit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_high_limit")
def Has_Aggr_High_Limit(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_others_flag")
def Has_Aggr_Others_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_named_flag")
def Has_Aggr_Named_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggregate_expand_flag")
def Has_Aggregate_Expand_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_determined_aggregate_flag")
def Has_Determined_Aggregate_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_association_choices_chain")
def Has_Association_Choices_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_case_statement_alternative_chain")
def Has_Case_Statement_Alternative_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_matching_flag")
def Has_Matching_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_staticness")
def Has_Choice_Staticness(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_procedure_call")
def Has_Procedure_Call(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_implementation")
def Has_Implementation(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_association_chain")
def Has_Parameter_Association_Chain(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_method_object")
def Has_Method_Object(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subtype_type_mark")
def Has_Subtype_Type_Mark(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subnature_nature_mark")
def Has_Subnature_Nature_Mark(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_conversion_subtype")
def Has_Type_Conversion_Subtype(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_mark")
def Has_Type_Mark(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_type_mark")
def Has_File_Type_Mark(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_return_type_mark")
def Has_Return_Type_Mark(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_disconnect_flag")
def Has_Has_Disconnect_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_active_flag")
def Has_Has_Active_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_within_flag")
def Has_Is_Within_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_marks_list")
def Has_Type_Marks_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_implicit_alias_flag")
def Has_Implicit_Alias_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_alias_signature")
def Has_Alias_Signature(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_signature")
def Has_Attribute_Signature(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_overload_list")
def Has_Overload_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_name_identifier")
def Has_Simple_Name_Identifier(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_name_subtype")
def Has_Simple_Name_Subtype(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_protected_type_body")
def Has_Protected_Type_Body(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_protected_type_declaration")
def Has_Protected_Type_Declaration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_use_flag")
def Has_Use_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_end_has_reserved_id")
def Has_End_Has_Reserved_Id(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_end_has_identifier")
def Has_End_Has_Identifier(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_end_has_postponed")
def Has_End_Has_Postponed(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_label")
def Has_Has_Label(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_begin")
def Has_Has_Begin(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_end")
def Has_Has_End(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_is")
def Has_Has_Is(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_pure")
def Has_Has_Pure(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_body")
def Has_Has_Body(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_parameter")
def Has_Has_Parameter(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_component")
def Has_Has_Component(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_identifier_list")
def Has_Has_Identifier_List(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_mode")
def Has_Has_Mode(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_class")
def Has_Has_Class(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_delay_mechanism")
def Has_Has_Delay_Mechanism(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_flag")
def Has_Suspend_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_stop_flag")
def Has_Stop_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_ref")
def Has_Is_Ref(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_forward_ref")
def Has_Is_Forward_Ref(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_property")
def Has_Psl_Property(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_sequence")
def Has_Psl_Sequence(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_declaration")
def Has_Psl_Declaration(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_expression")
def Has_Psl_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_boolean")
def Has_Psl_Boolean(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_clock")
def Has_PSL_Clock(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_nfa")
def Has_PSL_NFA(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_nbr_states")
def Has_PSL_Nbr_States(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_clock_sensitivity")
def Has_PSL_Clock_Sensitivity(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_eos_flag")
def Has_PSL_EOS_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_abort_flag")
def Has_PSL_Abort_Flag(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_count_expression")
def Has_Count_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_clock_expression")
def Has_Clock_Expression(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_clock")
def Has_Default_Clock(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_foreign_node")
def Has_Foreign_Node(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_state_index")
def Has_Suspend_State_Index(kind: IirKind) -> bool:
    """"""


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_state_chain")
def Has_Suspend_State_Chain(kind: IirKind) -> bool:
    """"""
