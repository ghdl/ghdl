# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
"""
Python binding for the Ada package ``Vhdl.Nodes_Meta`` in *libghdl*.

The meta-model: which fields a node kind has, of what type, and with what access attribute. It is what lets an
algorithm walk any node without knowing its kind.
"""

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
    return 0  # pragma: no cover


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
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes_meta__get_field_by_index")
def get_field_by_index(K: IirKind) -> int:
    """
    Get the field at a given index of the fields array.

    :param K: The index into the fields array.
    :returns: The field at that index.
    """
    return 0  # pragma: no cover


@export
def get_field_type(*args):
    """
    Get the type of a field.

    :param args: The field to query, from :class:`fields`.
    :returns:    The field's type, from :class:`types`.
    """
    return libghdl.vhdl__nodes_meta__get_field_type(*args)


@export
def get_field_attribute(*args):
    """
    Get the access attribute of a field.

    :param args: The field to query, from :class:`fields`.
    :returns:    The field's attribute, from :class:`Attr`.
    """
    return libghdl.vhdl__nodes_meta__get_field_attribute(*args)


@export
@unique
class types(IntEnum):
    """
    The types a field of the meta-model can have.
    """

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
    """
    The access attribute of a field: a reference, a chain, or owned.
    """

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
    """
    Every field of the meta-model, as an enumeration.
    """

    First_Design_Unit = 0
    Last_Design_Unit = 1
    Library_Declaration = 2
    File_Checksum = 3
    Analysis_Time_Stamp = 4
    Design_File_Source = 5
    Library = 6
    Design_File_Filename = 7
    Design_File_Directory = 8
    Design_File = 9
    Design_File_Chain = 10
    Library_Directory = 11
    Date = 12
    Context_Items = 13
    Dependence_List = 14
    Analysis_Checks_List = 15
    Date_State = 16
    Guarded_Target_State = 17
    Library_Unit = 18
    Hash_Chain = 19
    Design_Unit_Source_Pos = 20
    Design_Unit_Source_Line = 21
    Design_Unit_Source_Col = 22
    Value = 23
    Enum_Pos = 24
    Physical_Literal = 25
    Fp_Value = 26
    Simple_Aggregate_List = 27
    String8_Id = 28
    String_Length = 29
    Bit_String_Base = 30
    Has_Signed = 31
    Has_Sign = 32
    Has_Length = 33
    Literal_Length = 34
    Literal_Origin = 35
    Range_Origin = 36
    Literal_Subtype = 37
    Allocator_Subtype = 38
    Entity_Class = 39
    Entity_Name_List = 40
    Attribute_Designator = 41
    Attribute_Specification_Chain = 42
    Attribute_Specification = 43
    Static_Attribute_Flag = 44
    Signal_List = 45
    Quantity_List = 46
    Designated_Entity = 47
    Formal = 48
    Actual = 49
    Open_Actual = 50
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
    Owned_Instance_Package_Body = 80
    Instance_Subprogram_Body = 81
    Need_Body = 82
    Immediate_Body_Flag = 83
    Macro_Expand_Flag = 84
    Need_Instance_Bodies = 85
    Hierarchical_Name = 86
    Vunit_Item_Chain = 87
    Bound_Vunit_Chain = 88
    Verification_Block_Configuration = 89
    Block_Configuration = 90
    Concurrent_Statement_Chain = 91
    Chain = 92
    Port_Chain = 93
    Generic_Chain = 94
    Type = 95
    Subtype_Indication = 96
    Discrete_Range = 97
    Type_Definition = 98
    Subtype_Definition = 99
    Incomplete_Type_Declaration = 100
    Interface_Type_Subprograms = 101
    Interface_Type_Definition = 102
    Nature_Definition = 103
    Nature = 104
    Subnature_Indication = 105
    Reference_Terminal_Flag = 106
    Mode = 107
    Guarded_Signal_Flag = 108
    Signal_Kind = 109
    Base_Name = 110
    Interface_Declaration_Chain = 111
    Default_Subprogram = 112
    Associated_Subprogram = 113
    Subprogram_Specification = 114
    Sequential_Statement_Chain = 115
    Simultaneous_Statement_Chain = 116
    Subprogram_Body = 117
    Overload_Number = 118
    Subprogram_Depth = 119
    Subprogram_Hash = 120
    Impure_Depth = 121
    Return_Type = 122
    Implicit_Definition = 123
    Uninstantiated_Subprogram_Name = 124
    Default_Value = 125
    Mode_View_Indication = 126
    Deferred_Declaration = 127
    Deferred_Declaration_Flag = 128
    Shared_Flag = 129
    Design_Unit = 130
    Block_Statement = 131
    Signal_Driver = 132
    Declaration_Chain = 133
    File_Logical_Name = 134
    File_Open_Kind = 135
    Element_Position = 136
    Use_Clause_Chain = 137
    Context_Reference_Chain = 138
    Inherit_Spec_Chain = 139
    Selected_Name = 140
    Mode_View_Name = 141
    Type_Declarator = 142
    Complete_Type_Definition = 143
    Incomplete_Type_Ref_Chain = 144
    Associated_Type = 145
    Enumeration_Literal_List = 146
    Entity_Class_Entry_Chain = 147
    Group_Constituent_List = 148
    Unit_Chain = 149
    Primary_Unit = 150
    Identifier = 151
    Label = 152
    Return_Identifier = 153
    Visible_Flag = 154
    Range_Constraint = 155
    Direction = 156
    Left_Limit = 157
    Right_Limit = 158
    Left_Limit_Expr = 159
    Right_Limit_Expr = 160
    Parent_Type = 161
    Simple_Nature = 162
    Base_Nature = 163
    Resolution_Indication = 164
    Record_Element_Resolution_Chain = 165
    Tolerance = 166
    Plus_Terminal_Name = 167
    Minus_Terminal_Name = 168
    Plus_Terminal = 169
    Minus_Terminal = 170
    Magnitude_Expression = 171
    Phase_Expression = 172
    Power_Expression = 173
    Simultaneous_Left = 174
    Simultaneous_Right = 175
    Text_File_Flag = 176
    Only_Characters_Flag = 177
    Is_Character_Type = 178
    Nature_Staticness = 179
    Type_Staticness = 180
    Constraint_State = 181
    Index_Subtype_List = 182
    Index_Subtype_Definition_List = 183
    Element_Subtype_Indication = 184
    Element_Subtype = 185
    Element_Subnature_Indication = 186
    Element_Subnature = 187
    Index_Constraint_List = 188
    Array_Element_Constraint = 189
    Has_Array_Constraint_Flag = 190
    Has_Element_Constraint_Flag = 191
    Elements_Declaration_List = 192
    Elements_Definition_Chain = 193
    Elements_Definition_List = 194
    Owned_Elements_Chain = 195
    Designated_Type = 196
    Designated_Subtype_Indication = 197
    Index_List = 198
    Reference = 199
    Nature_Declarator = 200
    Across_Type_Mark = 201
    Through_Type_Mark = 202
    Across_Type_Definition = 203
    Through_Type_Definition = 204
    Across_Type = 205
    Through_Type = 206
    Target = 207
    Waveform_Chain = 208
    Guard = 209
    Delay_Mechanism = 210
    Reject_Time_Expression = 211
    Force_Mode = 212
    Has_Force_Mode = 213
    Sensitivity_List = 214
    Process_Origin = 215
    Package_Origin = 216
    Condition_Clause = 217
    Break_Element = 218
    Selector_Quantity = 219
    Break_Quantity = 220
    Timeout_Clause = 221
    Postponed_Flag = 222
    Callees_List = 223
    Passive_Flag = 224
    Resolution_Function_Flag = 225
    Wait_State = 226
    All_Sensitized_State = 227
    Seen_Flag = 228
    Pure_Flag = 229
    Foreign_Flag = 230
    Resolved_Flag = 231
    Signal_Type_Flag = 232
    Has_Signal_Flag = 233
    Purity_State = 234
    Elab_Flag = 235
    Vendor_Library_Flag = 236
    Configuration_Mark_Flag = 237
    Configuration_Done_Flag = 238
    Index_Constraint_Flag = 239
    Hide_Implicit_Flag = 240
    Assertion_Condition = 241
    Report_Expression = 242
    Severity_Expression = 243
    Instantiated_Unit = 244
    Instantiated_Header = 245
    Generic_Map_Aspect_Chain = 246
    Port_Map_Aspect_Chain = 247
    Configuration_Name = 248
    Component_Configuration = 249
    Configuration_Specification = 250
    Default_Binding_Indication = 251
    Default_Configuration_Declaration = 252
    Expression = 253
    Conditional_Expression_Chain = 254
    Allocator_Designated_Type = 255
    Selected_Waveform_Chain = 256
    Selected_Expressions_Chain = 257
    Conditional_Waveform_Chain = 258
    Guard_Expression = 259
    Guard_Decl = 260
    Guard_Sensitivity_List = 261
    Attribute_Implicit_Chain = 262
    Block_Block_Configuration = 263
    Package_Header = 264
    Block_Header = 265
    Uninstantiated_Package_Name = 266
    Uninstantiated_Package_Decl = 267
    Associated_Package = 268
    Instance_Source_File = 269
    Generate_Block_Configuration = 270
    Generate_Statement_Body = 271
    Alternative_Label = 272
    Generate_Else_Clause = 273
    Condition = 274
    Else_Clause = 275
    Parameter_Specification = 276
    Parent = 277
    Loop_Label = 278
    Exit_Flag = 279
    Next_Flag = 280
    Component_Name = 281
    Instantiation_List = 282
    Entity_Aspect = 283
    Default_Entity_Aspect = 284
    Binding_Indication = 285
    Named_Entity = 286
    Referenced_Name = 287
    Expr_Staticness = 288
    Scalar_Size = 289
    Error_Origin = 290
    Operand = 291
    Left = 292
    Right = 293
    Unit_Name = 294
    Name = 295
    Group_Template_Name = 296
    Name_Staticness = 297
    Prefix = 298
    Signature_Prefix = 299
    External_Pathname = 300
    Pathname_Suffix = 301
    Pathname_Expression = 302
    In_Formal_Flag = 303
    Inertial_Flag = 304
    Slice_Subtype = 305
    Suffix = 306
    Index_Subtype = 307
    Parameter = 308
    Parameter_2 = 309
    Parameter_3 = 310
    Parameter_4 = 311
    Attr_Chain = 312
    Actual_Type = 313
    Actual_Type_Definition = 314
    Association_Chain = 315
    Individual_Association_Chain = 316
    Subprogram_Association_Chain = 317
    Aggregate_Info = 318
    Sub_Aggregate_Info = 319
    Aggr_Dynamic_Flag = 320
    Aggr_Min_Length = 321
    Aggr_Low_Limit = 322
    Aggr_High_Limit = 323
    Aggr_Others_Flag = 324
    Aggr_Named_Flag = 325
    Aggregate_Expand_Flag = 326
    Determined_Aggregate_Flag = 327
    Association_Choices_Chain = 328
    Case_Statement_Alternative_Chain = 329
    Matching_Flag = 330
    Choice_Staticness = 331
    Procedure_Call = 332
    Implementation = 333
    Parameter_Association_Chain = 334
    Method_Object = 335
    Subtype_Type_Mark = 336
    Subnature_Nature_Mark = 337
    Type_Conversion_Subtype = 338
    Type_Mark = 339
    File_Type_Mark = 340
    Return_Type_Mark = 341
    Has_Disconnect_Flag = 342
    Has_Active_Flag = 343
    Is_Within_Flag = 344
    Type_Marks_List = 345
    Implicit_Alias_Flag = 346
    Alias_Signature = 347
    Attribute_Signature = 348
    Overload_List = 349
    Simple_Name_Identifier = 350
    Simple_Name_Subtype = 351
    Protected_Type_Body = 352
    Protected_Type_Declaration = 353
    Use_Flag = 354
    Elaborated_Flag = 355
    End_Has_Reserved_Id = 356
    End_Has_Identifier = 357
    End_Has_Postponed = 358
    Has_Begin = 359
    Has_End = 360
    Has_Is = 361
    Has_Pure = 362
    Has_Body = 363
    Has_Parameter = 364
    Has_Component = 365
    Has_Identifier_List = 366
    Has_Mode = 367
    Has_Class = 368
    Has_Semicolon = 369
    Has_Delay_Mechanism = 370
    Suspend_Flag = 371
    Covered_Flag = 372
    Stop_Flag = 373
    Is_Ref = 374
    Is_Forward_Ref = 375
    Psl_Property = 376
    Psl_Sequence = 377
    Psl_Declaration = 378
    Psl_Expression = 379
    Psl_Boolean = 380
    PSL_Clock = 381
    PSL_Abort = 382
    PSL_NFA = 383
    PSL_Nbr_States = 384
    PSL_Clock_Sensitivity = 385
    PSL_EOS_Flag = 386
    Count_Expression = 387
    Clock_Expression = 388
    Default_Clock = 389
    Foreign_Node = 390
    Suspend_State_Index = 391
    Suspend_State_Chain = 392
    Suspend_State_Last = 393
    Suspend_State_Decl = 394


def Get_Boolean(node, field):
    """
    Read a field of type ``Boolean`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_boolean(node, field)


def Get_Date_State_Type(node, field):
    """
    Read a field of type ``Date_State_Type`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_date_state_type(node, field)


def Get_Date_Type(node, field):
    """
    Read a field of type ``Date_Type`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_date_type(node, field)


def Get_Direction_Type(node, field):
    """
    Read a field of type ``Direction_Type`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_direction_type(node, field)


def Get_File_Checksum_Id(node, field):
    """
    Read a field of type ``File_Checksum_Id`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_file_checksum_id(node, field)


def Get_Fp64(node, field):
    """
    Read a field of type ``Fp64`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_fp64(node, field)


def Get_Iir(node, field):
    """
    Read a field of type ``Iir`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir(node, field)


def Get_Iir_All_Sensitized(node, field):
    """
    Read a field of type ``Iir_All_Sensitized`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_all_sensitized(node, field)


def Get_Iir_Constraint(node, field):
    """
    Read a field of type ``Iir_Constraint`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_constraint(node, field)


def Get_Iir_Delay_Mechanism(node, field):
    """
    Read a field of type ``Iir_Delay_Mechanism`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_delay_mechanism(node, field)


def Get_Iir_Flist(node, field):
    """
    Read a field of type ``Iir_Flist`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_flist(node, field)


def Get_Iir_Force_Mode(node, field):
    """
    Read a field of type ``Iir_Force_Mode`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_force_mode(node, field)


def Get_Iir_Index32(node, field):
    """
    Read a field of type ``Iir_Index32`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_index32(node, field)


def Get_Iir_Int32(node, field):
    """
    Read a field of type ``Iir_Int32`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_int32(node, field)


def Get_Iir_List(node, field):
    """
    Read a field of type ``Iir_List`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_list(node, field)


def Get_Iir_Mode(node, field):
    """
    Read a field of type ``Iir_Mode`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_mode(node, field)


def Get_Iir_Predefined_Functions(node, field):
    """
    Read a field of type ``Iir_Predefined_Functions`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_predefined_functions(node, field)


def Get_Iir_Pure_State(node, field):
    """
    Read a field of type ``Iir_Pure_State`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_pure_state(node, field)


def Get_Iir_Signal_Kind(node, field):
    """
    Read a field of type ``Iir_Signal_Kind`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_signal_kind(node, field)


def Get_Iir_Staticness(node, field):
    """
    Read a field of type ``Iir_Staticness`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_iir_staticness(node, field)


def Get_Int32(node, field):
    """
    Read a field of type ``Int32`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_int32(node, field)


def Get_Int64(node, field):
    """
    Read a field of type ``Int64`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_int64(node, field)


def Get_Name_Id(node, field):
    """
    Read a field of type ``Name_Id`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_name_id(node, field)


def Get_Number_Base_Type(node, field):
    """
    Read a field of type ``Number_Base_Type`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_number_base_type(node, field)


def Get_PSL_NFA(node, field):
    """
    Read a field of type ``PSL_NFA`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_psl_nfa(node, field)


def Get_PSL_Node(node, field):
    """
    Read a field of type ``PSL_Node`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_psl_node(node, field)


def Get_Scalar_Size(node, field):
    """
    Read a field of type ``Scalar_Size`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_scalar_size(node, field)


def Get_Source_File_Entry(node, field):
    """
    Read a field of type ``Source_File_Entry`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_source_file_entry(node, field)


def Get_Source_Ptr(node, field):
    """
    Read a field of type ``Source_Ptr`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_source_ptr(node, field)


def Get_String8_Id(node, field):
    """
    Read a field of type ``String8_Id`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_string8_id(node, field)


def Get_Time_Stamp_Id(node, field):
    """
    Read a field of type ``Time_Stamp_Id`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_time_stamp_id(node, field)


def Get_Token_Type(node, field):
    """
    Read a field of type ``Token_Type`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_token_type(node, field)


def Get_Tri_State_Type(node, field):
    """
    Read a field of type ``Tri_State_Type`` from a node, through the meta-model.

    :param node:  The node to read the field of.
    :param field: The field to read, from :class:`fields`.
    :returns:     The field's value.
    """
    return libghdl.vhdl__nodes_meta__get_tri_state_type(node, field)


@export
@BindToLibGHDL("vhdl__nodes_meta__has_first_design_unit")
def Has_First_Design_Unit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``First_Design_Unit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_last_design_unit")
def Has_Last_Design_Unit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Last_Design_Unit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library_declaration")
def Has_Library_Declaration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Library_Declaration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_checksum")
def Has_File_Checksum(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``File_Checksum`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_analysis_time_stamp")
def Has_Analysis_Time_Stamp(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Analysis_Time_Stamp`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_source")
def Has_Design_File_Source(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_File_Source`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library")
def Has_Library(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Library`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_filename")
def Has_Design_File_Filename(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_File_Filename`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_directory")
def Has_Design_File_Directory(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_File_Directory`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file")
def Has_Design_File(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_File`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_file_chain")
def Has_Design_File_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_File_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library_directory")
def Has_Library_Directory(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Library_Directory`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_date")
def Has_Date(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Date`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_context_items")
def Has_Context_Items(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Context_Items`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_dependence_list")
def Has_Dependence_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Dependence_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_analysis_checks_list")
def Has_Analysis_Checks_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Analysis_Checks_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_date_state")
def Has_Date_State(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Date_State`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guarded_target_state")
def Has_Guarded_Target_State(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Guarded_Target_State`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_library_unit")
def Has_Library_Unit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Library_Unit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_hash_chain")
def Has_Hash_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Hash_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit_source_pos")
def Has_Design_Unit_Source_Pos(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_Unit_Source_Pos`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit_source_line")
def Has_Design_Unit_Source_Line(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_Unit_Source_Line`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit_source_col")
def Has_Design_Unit_Source_Col(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_Unit_Source_Col`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_value")
def Has_Value(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Value`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_enum_pos")
def Has_Enum_Pos(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Enum_Pos`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_physical_literal")
def Has_Physical_Literal(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Physical_Literal`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_fp_value")
def Has_Fp_Value(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Fp_Value`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_aggregate_list")
def Has_Simple_Aggregate_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Simple_Aggregate_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_string8_id")
def Has_String8_Id(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``String8_Id`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_string_length")
def Has_String_Length(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``String_Length`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_bit_string_base")
def Has_Bit_String_Base(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Bit_String_Base`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_signed")
def Has_Has_Signed(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Signed`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_sign")
def Has_Has_Sign(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Sign`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_length")
def Has_Has_Length(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Length`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_literal_length")
def Has_Literal_Length(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Literal_Length`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_literal_origin")
def Has_Literal_Origin(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Literal_Origin`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_range_origin")
def Has_Range_Origin(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Range_Origin`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_literal_subtype")
def Has_Literal_Subtype(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Literal_Subtype`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_allocator_subtype")
def Has_Allocator_Subtype(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Allocator_Subtype`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_class")
def Has_Entity_Class(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Entity_Class`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_name_list")
def Has_Entity_Name_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Entity_Name_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_designator")
def Has_Attribute_Designator(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attribute_Designator`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_specification_chain")
def Has_Attribute_Specification_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attribute_Specification_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_specification")
def Has_Attribute_Specification(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attribute_Specification`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_static_attribute_flag")
def Has_Static_Attribute_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Static_Attribute_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_list")
def Has_Signal_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Signal_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_quantity_list")
def Has_Quantity_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Quantity_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_designated_entity")
def Has_Designated_Entity(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Designated_Entity`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_formal")
def Has_Formal(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Formal`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual")
def Has_Actual(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Actual`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_open_actual")
def Has_Open_Actual(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Open_Actual`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual_conversion")
def Has_Actual_Conversion(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Actual_Conversion`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_formal_conversion")
def Has_Formal_Conversion(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Formal_Conversion`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_whole_association_flag")
def Has_Whole_Association_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Whole_Association_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_collapse_signal_flag")
def Has_Collapse_Signal_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Collapse_Signal_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_artificial_flag")
def Has_Artificial_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Artificial_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_open_flag")
def Has_Open_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Open_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_after_drivers_flag")
def Has_After_Drivers_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``After_Drivers_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_we_value")
def Has_We_Value(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``We_Value`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_time")
def Has_Time(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Time`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_expr")
def Has_Associated_Expr(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Associated_Expr`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_block")
def Has_Associated_Block(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Associated_Block`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_chain")
def Has_Associated_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Associated_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_name")
def Has_Choice_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Choice_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_expression")
def Has_Choice_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Choice_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_range")
def Has_Choice_Range(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Choice_Range`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_same_alternative_flag")
def Has_Same_Alternative_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Same_Alternative_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_type_flag")
def Has_Element_Type_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Element_Type_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_architecture")
def Has_Architecture(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Architecture`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_specification")
def Has_Block_Specification(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Block_Specification`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_prev_block_configuration")
def Has_Prev_Block_Configuration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Prev_Block_Configuration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_item_chain")
def Has_Configuration_Item_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Configuration_Item_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_value_chain")
def Has_Attribute_Value_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attribute_Value_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_spec_chain")
def Has_Spec_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Spec_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_value_chain")
def Has_Value_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Value_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_value_spec_chain")
def Has_Attribute_Value_Spec_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attribute_Value_Spec_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_name")
def Has_Entity_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Entity_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package")
def Has_Package(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Package`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package_body")
def Has_Package_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Package_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instance_package_body")
def Has_Instance_Package_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Instance_Package_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_owned_instance_package_body")
def Has_Owned_Instance_Package_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Owned_Instance_Package_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instance_subprogram_body")
def Has_Instance_Subprogram_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Instance_Subprogram_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_need_body")
def Has_Need_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Need_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_immediate_body_flag")
def Has_Immediate_Body_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Immediate_Body_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_macro_expand_flag")
def Has_Macro_Expand_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Macro_Expand_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_need_instance_bodies")
def Has_Need_Instance_Bodies(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Need_Instance_Bodies`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_hierarchical_name")
def Has_Hierarchical_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Hierarchical_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_vunit_item_chain")
def Has_Vunit_Item_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Vunit_Item_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_bound_vunit_chain")
def Has_Bound_Vunit_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Bound_Vunit_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_verification_block_configuration")
def Has_Verification_Block_Configuration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Verification_Block_Configuration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_configuration")
def Has_Block_Configuration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Block_Configuration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_concurrent_statement_chain")
def Has_Concurrent_Statement_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Concurrent_Statement_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_chain")
def Has_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_port_chain")
def Has_Port_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Port_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generic_chain")
def Has_Generic_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Generic_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type")
def Has_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subtype_indication")
def Has_Subtype_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subtype_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_discrete_range")
def Has_Discrete_Range(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Discrete_Range`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_definition")
def Has_Type_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Type_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subtype_definition")
def Has_Subtype_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subtype_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_incomplete_type_declaration")
def Has_Incomplete_Type_Declaration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Incomplete_Type_Declaration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_interface_type_subprograms")
def Has_Interface_Type_Subprograms(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Interface_Type_Subprograms`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_interface_type_definition")
def Has_Interface_Type_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Interface_Type_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature_definition")
def Has_Nature_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Nature_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature")
def Has_Nature(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Nature`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subnature_indication")
def Has_Subnature_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subnature_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_reference_terminal_flag")
def Has_Reference_Terminal_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Reference_Terminal_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_mode")
def Has_Mode(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Mode`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guarded_signal_flag")
def Has_Guarded_Signal_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Guarded_Signal_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_kind")
def Has_Signal_Kind(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Signal_Kind`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_base_name")
def Has_Base_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Base_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_interface_declaration_chain")
def Has_Interface_Declaration_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Interface_Declaration_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_subprogram")
def Has_Default_Subprogram(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Default_Subprogram`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_subprogram")
def Has_Associated_Subprogram(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Associated_Subprogram`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_specification")
def Has_Subprogram_Specification(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subprogram_Specification`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_sequential_statement_chain")
def Has_Sequential_Statement_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Sequential_Statement_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simultaneous_statement_chain")
def Has_Simultaneous_Statement_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Simultaneous_Statement_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_body")
def Has_Subprogram_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subprogram_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_overload_number")
def Has_Overload_Number(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Overload_Number`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_depth")
def Has_Subprogram_Depth(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subprogram_Depth`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_hash")
def Has_Subprogram_Hash(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subprogram_Hash`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_impure_depth")
def Has_Impure_Depth(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Impure_Depth`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_return_type")
def Has_Return_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Return_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_implicit_definition")
def Has_Implicit_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Implicit_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_uninstantiated_subprogram_name")
def Has_Uninstantiated_Subprogram_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Uninstantiated_Subprogram_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_value")
def Has_Default_Value(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Default_Value`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_mode_view_indication")
def Has_Mode_View_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Mode_View_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_deferred_declaration")
def Has_Deferred_Declaration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Deferred_Declaration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_deferred_declaration_flag")
def Has_Deferred_Declaration_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Deferred_Declaration_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_shared_flag")
def Has_Shared_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Shared_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_design_unit")
def Has_Design_Unit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Design_Unit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_statement")
def Has_Block_Statement(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Block_Statement`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_driver")
def Has_Signal_Driver(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Signal_Driver`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_declaration_chain")
def Has_Declaration_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Declaration_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_logical_name")
def Has_File_Logical_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``File_Logical_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_open_kind")
def Has_File_Open_Kind(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``File_Open_Kind`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_position")
def Has_Element_Position(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Element_Position`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_use_clause_chain")
def Has_Use_Clause_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Use_Clause_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_context_reference_chain")
def Has_Context_Reference_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Context_Reference_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_inherit_spec_chain")
def Has_Inherit_Spec_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Inherit_Spec_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_selected_name")
def Has_Selected_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Selected_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_mode_view_name")
def Has_Mode_View_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Mode_View_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_declarator")
def Has_Type_Declarator(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Type_Declarator`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_complete_type_definition")
def Has_Complete_Type_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Complete_Type_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_incomplete_type_ref_chain")
def Has_Incomplete_Type_Ref_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Incomplete_Type_Ref_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_type")
def Has_Associated_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Associated_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_enumeration_literal_list")
def Has_Enumeration_Literal_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Enumeration_Literal_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_class_entry_chain")
def Has_Entity_Class_Entry_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Entity_Class_Entry_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_group_constituent_list")
def Has_Group_Constituent_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Group_Constituent_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_unit_chain")
def Has_Unit_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Unit_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_primary_unit")
def Has_Primary_Unit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Primary_Unit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_identifier")
def Has_Identifier(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Identifier`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_label")
def Has_Label(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Label`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_return_identifier")
def Has_Return_Identifier(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Return_Identifier`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_visible_flag")
def Has_Visible_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Visible_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_range_constraint")
def Has_Range_Constraint(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Range_Constraint`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_direction")
def Has_Direction(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Direction`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_left_limit")
def Has_Left_Limit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Left_Limit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_right_limit")
def Has_Right_Limit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Right_Limit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_left_limit_expr")
def Has_Left_Limit_Expr(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Left_Limit_Expr`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_right_limit_expr")
def Has_Right_Limit_Expr(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Right_Limit_Expr`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parent_type")
def Has_Parent_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parent_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_nature")
def Has_Simple_Nature(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Simple_Nature`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_base_nature")
def Has_Base_Nature(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Base_Nature`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_resolution_indication")
def Has_Resolution_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Resolution_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_record_element_resolution_chain")
def Has_Record_Element_Resolution_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Record_Element_Resolution_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_tolerance")
def Has_Tolerance(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Tolerance`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_plus_terminal_name")
def Has_Plus_Terminal_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Plus_Terminal_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_minus_terminal_name")
def Has_Minus_Terminal_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Minus_Terminal_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_plus_terminal")
def Has_Plus_Terminal(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Plus_Terminal`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_minus_terminal")
def Has_Minus_Terminal(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Minus_Terminal`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_magnitude_expression")
def Has_Magnitude_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Magnitude_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_phase_expression")
def Has_Phase_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Phase_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_power_expression")
def Has_Power_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Power_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simultaneous_left")
def Has_Simultaneous_Left(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Simultaneous_Left`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simultaneous_right")
def Has_Simultaneous_Right(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Simultaneous_Right`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_text_file_flag")
def Has_Text_File_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Text_File_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_only_characters_flag")
def Has_Only_Characters_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Only_Characters_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_character_type")
def Has_Is_Character_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Is_Character_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature_staticness")
def Has_Nature_Staticness(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Nature_Staticness`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_staticness")
def Has_Type_Staticness(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Type_Staticness`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_constraint_state")
def Has_Constraint_State(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Constraint_State`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_subtype_list")
def Has_Index_Subtype_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Index_Subtype_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_subtype_definition_list")
def Has_Index_Subtype_Definition_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Index_Subtype_Definition_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subtype_indication")
def Has_Element_Subtype_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Element_Subtype_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subtype")
def Has_Element_Subtype(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Element_Subtype`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subnature_indication")
def Has_Element_Subnature_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Element_Subnature_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_element_subnature")
def Has_Element_Subnature(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Element_Subnature`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_constraint_list")
def Has_Index_Constraint_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Index_Constraint_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_array_element_constraint")
def Has_Array_Element_Constraint(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Array_Element_Constraint`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_array_constraint_flag")
def Has_Has_Array_Constraint_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Array_Constraint_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_element_constraint_flag")
def Has_Has_Element_Constraint_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Element_Constraint_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_elements_declaration_list")
def Has_Elements_Declaration_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Elements_Declaration_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_elements_definition_chain")
def Has_Elements_Definition_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Elements_Definition_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_elements_definition_list")
def Has_Elements_Definition_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Elements_Definition_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_owned_elements_chain")
def Has_Owned_Elements_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Owned_Elements_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_designated_type")
def Has_Designated_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Designated_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_designated_subtype_indication")
def Has_Designated_Subtype_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Designated_Subtype_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_list")
def Has_Index_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Index_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_reference")
def Has_Reference(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Reference`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_nature_declarator")
def Has_Nature_Declarator(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Nature_Declarator`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_across_type_mark")
def Has_Across_Type_Mark(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Across_Type_Mark`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_through_type_mark")
def Has_Through_Type_Mark(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Through_Type_Mark`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_across_type_definition")
def Has_Across_Type_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Across_Type_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_through_type_definition")
def Has_Through_Type_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Through_Type_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_across_type")
def Has_Across_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Across_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_through_type")
def Has_Through_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Through_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_target")
def Has_Target(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Target`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_waveform_chain")
def Has_Waveform_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Waveform_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard")
def Has_Guard(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Guard`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_delay_mechanism")
def Has_Delay_Mechanism(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Delay_Mechanism`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_reject_time_expression")
def Has_Reject_Time_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Reject_Time_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_force_mode")
def Has_Force_Mode(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Force_Mode`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_force_mode")
def Has_Has_Force_Mode(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Force_Mode`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_sensitivity_list")
def Has_Sensitivity_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Sensitivity_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_process_origin")
def Has_Process_Origin(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Process_Origin`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package_origin")
def Has_Package_Origin(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Package_Origin`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_condition_clause")
def Has_Condition_Clause(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Condition_Clause`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_break_element")
def Has_Break_Element(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Break_Element`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_selector_quantity")
def Has_Selector_Quantity(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Selector_Quantity`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_break_quantity")
def Has_Break_Quantity(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Break_Quantity`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_timeout_clause")
def Has_Timeout_Clause(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Timeout_Clause`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_postponed_flag")
def Has_Postponed_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Postponed_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_callees_list")
def Has_Callees_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Callees_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_passive_flag")
def Has_Passive_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Passive_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_resolution_function_flag")
def Has_Resolution_Function_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Resolution_Function_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_wait_state")
def Has_Wait_State(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Wait_State`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_all_sensitized_state")
def Has_All_Sensitized_State(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``All_Sensitized_State`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_seen_flag")
def Has_Seen_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Seen_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_pure_flag")
def Has_Pure_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Pure_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_foreign_flag")
def Has_Foreign_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Foreign_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_resolved_flag")
def Has_Resolved_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Resolved_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signal_type_flag")
def Has_Signal_Type_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Signal_Type_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_signal_flag")
def Has_Has_Signal_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Signal_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_purity_state")
def Has_Purity_State(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Purity_State`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_elab_flag")
def Has_Elab_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Elab_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_vendor_library_flag")
def Has_Vendor_Library_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Vendor_Library_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_mark_flag")
def Has_Configuration_Mark_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Configuration_Mark_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_done_flag")
def Has_Configuration_Done_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Configuration_Done_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_constraint_flag")
def Has_Index_Constraint_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Index_Constraint_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_hide_implicit_flag")
def Has_Hide_Implicit_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Hide_Implicit_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_assertion_condition")
def Has_Assertion_Condition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Assertion_Condition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_report_expression")
def Has_Report_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Report_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_severity_expression")
def Has_Severity_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Severity_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instantiated_unit")
def Has_Instantiated_Unit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Instantiated_Unit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instantiated_header")
def Has_Instantiated_Header(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Instantiated_Header`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generic_map_aspect_chain")
def Has_Generic_Map_Aspect_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Generic_Map_Aspect_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_port_map_aspect_chain")
def Has_Port_Map_Aspect_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Port_Map_Aspect_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_name")
def Has_Configuration_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Configuration_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_component_configuration")
def Has_Component_Configuration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Component_Configuration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_configuration_specification")
def Has_Configuration_Specification(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Configuration_Specification`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_binding_indication")
def Has_Default_Binding_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Default_Binding_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_configuration_declaration")
def Has_Default_Configuration_Declaration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Default_Configuration_Declaration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_expression")
def Has_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_conditional_expression_chain")
def Has_Conditional_Expression_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Conditional_Expression_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_allocator_designated_type")
def Has_Allocator_Designated_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Allocator_Designated_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_selected_waveform_chain")
def Has_Selected_Waveform_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Selected_Waveform_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_selected_expressions_chain")
def Has_Selected_Expressions_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Selected_Expressions_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_conditional_waveform_chain")
def Has_Conditional_Waveform_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Conditional_Waveform_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard_expression")
def Has_Guard_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Guard_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard_decl")
def Has_Guard_Decl(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Guard_Decl`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_guard_sensitivity_list")
def Has_Guard_Sensitivity_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Guard_Sensitivity_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_implicit_chain")
def Has_Attribute_Implicit_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attribute_Implicit_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_block_configuration")
def Has_Block_Block_Configuration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Block_Block_Configuration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_package_header")
def Has_Package_Header(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Package_Header`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_block_header")
def Has_Block_Header(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Block_Header`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_uninstantiated_package_name")
def Has_Uninstantiated_Package_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Uninstantiated_Package_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_uninstantiated_package_decl")
def Has_Uninstantiated_Package_Decl(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Uninstantiated_Package_Decl`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_associated_package")
def Has_Associated_Package(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Associated_Package`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instance_source_file")
def Has_Instance_Source_File(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Instance_Source_File`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generate_block_configuration")
def Has_Generate_Block_Configuration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Generate_Block_Configuration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generate_statement_body")
def Has_Generate_Statement_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Generate_Statement_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_alternative_label")
def Has_Alternative_Label(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Alternative_Label`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_generate_else_clause")
def Has_Generate_Else_Clause(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Generate_Else_Clause`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_condition")
def Has_Condition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Condition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_else_clause")
def Has_Else_Clause(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Else_Clause`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_specification")
def Has_Parameter_Specification(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parameter_Specification`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parent")
def Has_Parent(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parent`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_loop_label")
def Has_Loop_Label(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Loop_Label`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_exit_flag")
def Has_Exit_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Exit_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_next_flag")
def Has_Next_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Next_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_component_name")
def Has_Component_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Component_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_instantiation_list")
def Has_Instantiation_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Instantiation_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_entity_aspect")
def Has_Entity_Aspect(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Entity_Aspect`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_entity_aspect")
def Has_Default_Entity_Aspect(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Default_Entity_Aspect`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_binding_indication")
def Has_Binding_Indication(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Binding_Indication`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_named_entity")
def Has_Named_Entity(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Named_Entity`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_referenced_name")
def Has_Referenced_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Referenced_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_expr_staticness")
def Has_Expr_Staticness(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Expr_Staticness`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_scalar_size")
def Has_Scalar_Size(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Scalar_Size`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_error_origin")
def Has_Error_Origin(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Error_Origin`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_operand")
def Has_Operand(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Operand`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_left")
def Has_Left(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Left`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_right")
def Has_Right(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Right`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_unit_name")
def Has_Unit_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Unit_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_name")
def Has_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_group_template_name")
def Has_Group_Template_Name(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Group_Template_Name`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_name_staticness")
def Has_Name_Staticness(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Name_Staticness`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_prefix")
def Has_Prefix(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Prefix`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_signature_prefix")
def Has_Signature_Prefix(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Signature_Prefix`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_external_pathname")
def Has_External_Pathname(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``External_Pathname`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_pathname_suffix")
def Has_Pathname_Suffix(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Pathname_Suffix`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_pathname_expression")
def Has_Pathname_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Pathname_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_in_formal_flag")
def Has_In_Formal_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``In_Formal_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_inertial_flag")
def Has_Inertial_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Inertial_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_slice_subtype")
def Has_Slice_Subtype(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Slice_Subtype`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suffix")
def Has_Suffix(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Suffix`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_index_subtype")
def Has_Index_Subtype(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Index_Subtype`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter")
def Has_Parameter(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parameter`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_2")
def Has_Parameter_2(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parameter_2`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_3")
def Has_Parameter_3(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parameter_3`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_4")
def Has_Parameter_4(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parameter_4`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attr_chain")
def Has_Attr_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attr_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual_type")
def Has_Actual_Type(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Actual_Type`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_actual_type_definition")
def Has_Actual_Type_Definition(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Actual_Type_Definition`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_association_chain")
def Has_Association_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Association_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_individual_association_chain")
def Has_Individual_Association_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Individual_Association_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subprogram_association_chain")
def Has_Subprogram_Association_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subprogram_Association_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggregate_info")
def Has_Aggregate_Info(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggregate_Info`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_sub_aggregate_info")
def Has_Sub_Aggregate_Info(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Sub_Aggregate_Info`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_dynamic_flag")
def Has_Aggr_Dynamic_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggr_Dynamic_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_min_length")
def Has_Aggr_Min_Length(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggr_Min_Length`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_low_limit")
def Has_Aggr_Low_Limit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggr_Low_Limit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_high_limit")
def Has_Aggr_High_Limit(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggr_High_Limit`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_others_flag")
def Has_Aggr_Others_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggr_Others_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggr_named_flag")
def Has_Aggr_Named_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggr_Named_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_aggregate_expand_flag")
def Has_Aggregate_Expand_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Aggregate_Expand_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_determined_aggregate_flag")
def Has_Determined_Aggregate_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Determined_Aggregate_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_association_choices_chain")
def Has_Association_Choices_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Association_Choices_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_case_statement_alternative_chain")
def Has_Case_Statement_Alternative_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Case_Statement_Alternative_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_matching_flag")
def Has_Matching_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Matching_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_choice_staticness")
def Has_Choice_Staticness(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Choice_Staticness`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_procedure_call")
def Has_Procedure_Call(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Procedure_Call`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_implementation")
def Has_Implementation(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Implementation`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_parameter_association_chain")
def Has_Parameter_Association_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Parameter_Association_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_method_object")
def Has_Method_Object(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Method_Object`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subtype_type_mark")
def Has_Subtype_Type_Mark(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subtype_Type_Mark`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_subnature_nature_mark")
def Has_Subnature_Nature_Mark(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Subnature_Nature_Mark`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_conversion_subtype")
def Has_Type_Conversion_Subtype(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Type_Conversion_Subtype`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_mark")
def Has_Type_Mark(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Type_Mark`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_file_type_mark")
def Has_File_Type_Mark(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``File_Type_Mark`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_return_type_mark")
def Has_Return_Type_Mark(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Return_Type_Mark`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_disconnect_flag")
def Has_Has_Disconnect_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Disconnect_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_active_flag")
def Has_Has_Active_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Active_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_within_flag")
def Has_Is_Within_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Is_Within_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_type_marks_list")
def Has_Type_Marks_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Type_Marks_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_implicit_alias_flag")
def Has_Implicit_Alias_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Implicit_Alias_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_alias_signature")
def Has_Alias_Signature(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Alias_Signature`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_attribute_signature")
def Has_Attribute_Signature(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Attribute_Signature`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_overload_list")
def Has_Overload_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Overload_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_name_identifier")
def Has_Simple_Name_Identifier(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Simple_Name_Identifier`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_simple_name_subtype")
def Has_Simple_Name_Subtype(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Simple_Name_Subtype`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_protected_type_body")
def Has_Protected_Type_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Protected_Type_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_protected_type_declaration")
def Has_Protected_Type_Declaration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Protected_Type_Declaration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_use_flag")
def Has_Use_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Use_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_elaborated_flag")
def Has_Elaborated_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Elaborated_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_end_has_reserved_id")
def Has_End_Has_Reserved_Id(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``End_Has_Reserved_Id`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_end_has_identifier")
def Has_End_Has_Identifier(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``End_Has_Identifier`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_end_has_postponed")
def Has_End_Has_Postponed(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``End_Has_Postponed`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_begin")
def Has_Has_Begin(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Begin`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_end")
def Has_Has_End(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_End`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_is")
def Has_Has_Is(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Is`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_pure")
def Has_Has_Pure(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Pure`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_body")
def Has_Has_Body(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Body`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_parameter")
def Has_Has_Parameter(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Parameter`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_component")
def Has_Has_Component(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Component`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_identifier_list")
def Has_Has_Identifier_List(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Identifier_List`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_mode")
def Has_Has_Mode(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Mode`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_class")
def Has_Has_Class(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Class`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_semicolon")
def Has_Has_Semicolon(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Semicolon`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_has_delay_mechanism")
def Has_Has_Delay_Mechanism(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Has_Delay_Mechanism`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_flag")
def Has_Suspend_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Suspend_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_covered_flag")
def Has_Covered_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Covered_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_stop_flag")
def Has_Stop_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Stop_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_ref")
def Has_Is_Ref(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Is_Ref`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_is_forward_ref")
def Has_Is_Forward_Ref(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Is_Forward_Ref`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_property")
def Has_Psl_Property(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Psl_Property`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_sequence")
def Has_Psl_Sequence(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Psl_Sequence`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_declaration")
def Has_Psl_Declaration(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Psl_Declaration`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_expression")
def Has_Psl_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Psl_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_boolean")
def Has_Psl_Boolean(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Psl_Boolean`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_clock")
def Has_PSL_Clock(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``PSL_Clock`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_abort")
def Has_PSL_Abort(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``PSL_Abort`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_nfa")
def Has_PSL_NFA(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``PSL_NFA`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_nbr_states")
def Has_PSL_Nbr_States(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``PSL_Nbr_States`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_clock_sensitivity")
def Has_PSL_Clock_Sensitivity(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``PSL_Clock_Sensitivity`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_psl_eos_flag")
def Has_PSL_EOS_Flag(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``PSL_EOS_Flag`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_count_expression")
def Has_Count_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Count_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_clock_expression")
def Has_Clock_Expression(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Clock_Expression`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_default_clock")
def Has_Default_Clock(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Default_Clock`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_foreign_node")
def Has_Foreign_Node(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Foreign_Node`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_state_index")
def Has_Suspend_State_Index(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Suspend_State_Index`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_state_chain")
def Has_Suspend_State_Chain(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Suspend_State_Chain`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_state_last")
def Has_Suspend_State_Last(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Suspend_State_Last`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """


@export
@BindToLibGHDL("vhdl__nodes_meta__has_suspend_state_decl")
def Has_Suspend_State_Decl(kind: IirKind) -> bool:
    """
    Check whether a node of the given kind has a ``Suspend_State_Decl`` field.

    :param kind: The node kind to check.
    :returns:    ``True`` if a node of that kind has the field.
    """
