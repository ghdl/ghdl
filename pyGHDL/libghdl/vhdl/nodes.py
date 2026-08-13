# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
"""
Python binding for the Ada package ``Vhdl.Nodes`` in *libghdl*.

The IIR tree: the node kinds, the enumerations their fields use, and the accessor pair for every field.
See :ref:`INT:AST` for what a node is and how the fields are addressed.
"""

from enum import IntEnum, unique

from pyTooling.Decorators import export

from pyGHDL.libghdl._decorator import BindToLibGHDL

from typing import TypeVar
from ctypes import c_int32
from pyGHDL.libghdl._types import (
    Iir,
    IirKind,
    LocationType,
    FileChecksumId,
    TimeStampId,
    SourceFileEntry,
    NameId,
    TriStateType,
    SourcePtr,
    Int32,
    Int64,
    Fp64,
    String8Id,
    Boolean,
    DirectionType,
    PSLNode,
    PSLNFA,
)
from pyGHDL.libghdl.vhdl.tokens import Tok

__all__ = [
    "Null_Iir",
    "Null_Iir_List",
    "Iir_List_All",
    "Null_Iir_Flist",
    "Iir_Flist_Others",
    "Iir_Flist_All",
]

Null_Iir = 0
"""
Null element for an IIR node reference.
"""

Null_Iir_List = 0
Iir_List_All = 1

Null_Iir_Flist = 0
Iir_Flist_Others = 1
Iir_Flist_All = 2

DateType = TypeVar("DateType", bound=c_int32)


@export
@unique
class Iir_Kind(IntEnum):
    """
    The kind of an IIR node, which decides what its physical fields mean.
    """

    Unused = 0
    Error = 1
    Design_File = 2
    Design_Unit = 3
    Library_Clause = 4
    Use_Clause = 5
    Context_Reference = 6
    PSL_Inherit_Spec = 7
    Integer_Literal = 8
    Floating_Point_Literal = 9
    Null_Literal = 10
    String_Literal8 = 11
    Physical_Int_Literal = 12
    Physical_Fp_Literal = 13
    Simple_Aggregate = 14
    Overflow_Literal = 15
    Unaffected_Waveform = 16
    Waveform_Element = 17
    Conditional_Waveform = 18
    Conditional_Expression = 19
    Association_Element_By_Expression = 20
    Association_Element_By_Name = 21
    Association_Element_By_Individual = 22
    Association_Element_Open = 23
    Association_Element_Package = 24
    Association_Element_Type = 25
    Association_Element_Subprogram = 26
    Association_Element_Terminal = 27
    Choice_By_Range = 28
    Choice_By_Expression = 29
    Choice_By_Others = 30
    Choice_By_None = 31
    Choice_By_Name = 32
    Entity_Aspect_Entity = 33
    Entity_Aspect_Configuration = 34
    Entity_Aspect_Open = 35
    Psl_Hierarchical_Name = 36
    Block_Configuration = 37
    Block_Header = 38
    Component_Configuration = 39
    Binding_Indication = 40
    Entity_Class = 41
    Attribute_Value = 42
    Signature = 43
    Aggregate_Info = 44
    Procedure_Call = 45
    Record_Element_Constraint = 46
    Array_Element_Resolution = 47
    Record_Resolution = 48
    Record_Element_Resolution = 49
    Simple_Mode_View_Element = 50
    Array_Mode_View_Element = 51
    Record_Mode_View_Element = 52
    Break_Element = 53
    Attribute_Specification = 54
    Disconnection_Specification = 55
    Step_Limit_Specification = 56
    Configuration_Specification = 57
    Access_Type_Definition = 58
    Incomplete_Type_Definition = 59
    Interface_Type_Definition = 60
    File_Type_Definition = 61
    Protected_Type_Declaration = 62
    Record_Type_Definition = 63
    Array_Type_Definition = 64
    Array_Subtype_Definition = 65
    Record_Subtype_Definition = 66
    Access_Subtype_Definition = 67
    File_Subtype_Definition = 68
    Physical_Subtype_Definition = 69
    Floating_Subtype_Definition = 70
    Integer_Subtype_Definition = 71
    Enumeration_Subtype_Definition = 72
    Enumeration_Type_Definition = 73
    Integer_Type_Definition = 74
    Floating_Type_Definition = 75
    Physical_Type_Definition = 76
    Range_Expression = 77
    Protected_Type_Body = 78
    Wildcard_Type_Definition = 79
    Foreign_Vector_Type_Definition = 80
    Subtype_Definition = 81
    Record_Mode_View_Indication = 82
    Array_Mode_View_Indication = 83
    Scalar_Nature_Definition = 84
    Record_Nature_Definition = 85
    Array_Nature_Definition = 86
    Array_Subnature_Definition = 87
    Overload_List = 88
    Foreign_Module = 89
    Entity_Declaration = 90
    Configuration_Declaration = 91
    Context_Declaration = 92
    Package_Declaration = 93
    Package_Instantiation_Declaration = 94
    Vmode_Declaration = 95
    Vprop_Declaration = 96
    Vunit_Declaration = 97
    Package_Body = 98
    Architecture_Body = 99
    Package_Instantiation_Body = 100
    Type_Declaration = 101
    Anonymous_Type_Declaration = 102
    Subtype_Declaration = 103
    Nature_Declaration = 104
    Subnature_Declaration = 105
    Package_Header = 106
    Unit_Declaration = 107
    Library_Declaration = 108
    Component_Declaration = 109
    Attribute_Declaration = 110
    Group_Template_Declaration = 111
    Group_Declaration = 112
    Element_Declaration = 113
    Nature_Element_Declaration = 114
    Non_Object_Alias_Declaration = 115
    Mode_View_Declaration = 116
    Psl_Declaration = 117
    Psl_Boolean_Parameter = 118
    Psl_Endpoint_Declaration = 119
    Enumeration_Literal = 120
    Function_Declaration = 121
    Procedure_Declaration = 122
    Function_Body = 123
    Procedure_Body = 124
    Subprogram_Instantiation_Body = 125
    Function_Instantiation_Declaration = 126
    Procedure_Instantiation_Declaration = 127
    Terminal_Declaration = 128
    Object_Alias_Declaration = 129
    Free_Quantity_Declaration = 130
    Spectrum_Quantity_Declaration = 131
    Noise_Quantity_Declaration = 132
    Across_Quantity_Declaration = 133
    Through_Quantity_Declaration = 134
    File_Declaration = 135
    Guard_Signal_Declaration = 136
    Signal_Declaration = 137
    Variable_Declaration = 138
    Constant_Declaration = 139
    Iterator_Declaration = 140
    Interface_Constant_Declaration = 141
    Interface_Variable_Declaration = 142
    Interface_Signal_Declaration = 143
    Interface_View_Declaration = 144
    Interface_File_Declaration = 145
    Interface_Quantity_Declaration = 146
    Interface_Terminal_Declaration = 147
    Interface_Type_Declaration = 148
    Interface_Package_Declaration = 149
    Interface_Function_Declaration = 150
    Interface_Procedure_Declaration = 151
    Attribute_Implicit_Declaration = 152
    Suspend_State_Declaration = 153
    Identity_Operator = 154
    Negation_Operator = 155
    Absolute_Operator = 156
    Not_Operator = 157
    Implicit_Condition_Operator = 158
    Condition_Operator = 159
    Reduction_And_Operator = 160
    Reduction_Or_Operator = 161
    Reduction_Nand_Operator = 162
    Reduction_Nor_Operator = 163
    Reduction_Xor_Operator = 164
    Reduction_Xnor_Operator = 165
    And_Operator = 166
    Or_Operator = 167
    Nand_Operator = 168
    Nor_Operator = 169
    Xor_Operator = 170
    Xnor_Operator = 171
    Equality_Operator = 172
    Inequality_Operator = 173
    Less_Than_Operator = 174
    Less_Than_Or_Equal_Operator = 175
    Greater_Than_Operator = 176
    Greater_Than_Or_Equal_Operator = 177
    Match_Equality_Operator = 178
    Match_Inequality_Operator = 179
    Match_Less_Than_Operator = 180
    Match_Less_Than_Or_Equal_Operator = 181
    Match_Greater_Than_Operator = 182
    Match_Greater_Than_Or_Equal_Operator = 183
    Sll_Operator = 184
    Sla_Operator = 185
    Srl_Operator = 186
    Sra_Operator = 187
    Rol_Operator = 188
    Ror_Operator = 189
    Addition_Operator = 190
    Substraction_Operator = 191
    Concatenation_Operator = 192
    Multiplication_Operator = 193
    Division_Operator = 194
    Modulus_Operator = 195
    Remainder_Operator = 196
    Exponentiation_Operator = 197
    Function_Call = 198
    Aggregate = 199
    Parenthesis_Expression = 200
    Qualified_Expression = 201
    Type_Conversion = 202
    Allocator_By_Expression = 203
    Allocator_By_Subtype = 204
    Selected_Element = 205
    Dereference = 206
    Implicit_Dereference = 207
    Slice_Name = 208
    Indexed_Name = 209
    Psl_Prev = 210
    Psl_Stable = 211
    Psl_Rose = 212
    Psl_Fell = 213
    Psl_Onehot = 214
    Psl_Onehot0 = 215
    Psl_Expression = 216
    Sensitized_Process_Statement = 217
    Process_Statement = 218
    Concurrent_Simple_Signal_Assignment = 219
    Concurrent_Conditional_Signal_Assignment = 220
    Concurrent_Selected_Signal_Assignment = 221
    Concurrent_Assertion_Statement = 222
    Concurrent_Procedure_Call_Statement = 223
    Concurrent_Break_Statement = 224
    Psl_Assert_Directive = 225
    Psl_Assume_Directive = 226
    Psl_Cover_Directive = 227
    Psl_Restrict_Directive = 228
    Block_Statement = 229
    If_Generate_Statement = 230
    Case_Generate_Statement = 231
    For_Generate_Statement = 232
    Component_Instantiation_Statement = 233
    Psl_Default_Clock = 234
    Generate_Statement_Body = 235
    If_Generate_Else_Clause = 236
    Simple_Simultaneous_Statement = 237
    Simultaneous_Null_Statement = 238
    Simultaneous_Procedural_Statement = 239
    Simultaneous_Case_Statement = 240
    Simultaneous_If_Statement = 241
    Simultaneous_Elsif = 242
    Simple_Signal_Assignment_Statement = 243
    Conditional_Signal_Assignment_Statement = 244
    Selected_Waveform_Assignment_Statement = 245
    Signal_Force_Assignment_Statement = 246
    Signal_Release_Assignment_Statement = 247
    Variable_Assignment_Statement = 248
    Conditional_Variable_Assignment_Statement = 249
    Selected_Variable_Assignment_Statement = 250
    Null_Statement = 251
    Assertion_Statement = 252
    Report_Statement = 253
    Next_Statement = 254
    Exit_Statement = 255
    Return_Statement = 256
    Procedure_Call_Statement = 257
    Wait_Statement = 258
    Break_Statement = 259
    For_Loop_Statement = 260
    While_Loop_Statement = 261
    Case_Statement = 262
    If_Statement = 263
    Suspend_State_Statement = 264
    Elsif = 265
    Character_Literal = 266
    Simple_Name = 267
    Selected_Name = 268
    Operator_Symbol = 269
    Reference_Name = 270
    External_Constant_Name = 271
    External_Signal_Name = 272
    External_Variable_Name = 273
    Selected_By_All_Name = 274
    Parenthesis_Name = 275
    Package_Pathname = 276
    Absolute_Pathname = 277
    Relative_Pathname = 278
    Pathname_Element = 279
    Box_Name = 280
    Base_Attribute = 281
    Subtype_Attribute = 282
    Element_Attribute = 283
    Across_Attribute = 284
    Through_Attribute = 285
    Nature_Reference_Attribute = 286
    Left_Type_Attribute = 287
    Right_Type_Attribute = 288
    High_Type_Attribute = 289
    Low_Type_Attribute = 290
    Ascending_Type_Attribute = 291
    Image_Attribute = 292
    Value_Attribute = 293
    Pos_Attribute = 294
    Val_Attribute = 295
    Succ_Attribute = 296
    Pred_Attribute = 297
    Leftof_Attribute = 298
    Rightof_Attribute = 299
    Signal_Slew_Attribute = 300
    Quantity_Slew_Attribute = 301
    Ramp_Attribute = 302
    Zoh_Attribute = 303
    Ltf_Attribute = 304
    Ztf_Attribute = 305
    Dot_Attribute = 306
    Integ_Attribute = 307
    Quantity_Delayed_Attribute = 308
    Above_Attribute = 309
    Delayed_Attribute = 310
    Stable_Attribute = 311
    Quiet_Attribute = 312
    Transaction_Attribute = 313
    Event_Attribute = 314
    Active_Attribute = 315
    Last_Event_Attribute = 316
    Last_Active_Attribute = 317
    Last_Value_Attribute = 318
    Driving_Attribute = 319
    Driving_Value_Attribute = 320
    Behavior_Attribute = 321
    Structure_Attribute = 322
    Simple_Name_Attribute = 323
    Instance_Name_Attribute = 324
    Path_Name_Attribute = 325
    Converse_Attribute = 326
    Left_Array_Attribute = 327
    Right_Array_Attribute = 328
    High_Array_Attribute = 329
    Low_Array_Attribute = 330
    Length_Array_Attribute = 331
    Ascending_Array_Attribute = 332
    Range_Array_Attribute = 333
    Reverse_Range_Array_Attribute = 334
    Attribute_Name = 335


@export
class Iir_Kinds:
    """
    The ``Iir_Kinds_*`` subtype ranges, each listing the consecutive node kinds it covers.
    """

    Library_Unit = [
        Iir_Kind.Foreign_Module,
        Iir_Kind.Entity_Declaration,
        Iir_Kind.Configuration_Declaration,
        Iir_Kind.Context_Declaration,
        Iir_Kind.Package_Declaration,
        Iir_Kind.Package_Instantiation_Declaration,
        Iir_Kind.Vmode_Declaration,
        Iir_Kind.Vprop_Declaration,
        Iir_Kind.Vunit_Declaration,
        Iir_Kind.Package_Body,
        Iir_Kind.Architecture_Body,
    ]

    Primary_Unit = [
        Iir_Kind.Entity_Declaration,
        Iir_Kind.Configuration_Declaration,
        Iir_Kind.Context_Declaration,
        Iir_Kind.Package_Declaration,
        Iir_Kind.Package_Instantiation_Declaration,
        Iir_Kind.Vmode_Declaration,
        Iir_Kind.Vprop_Declaration,
        Iir_Kind.Vunit_Declaration,
    ]

    Secondary_Unit = [
        Iir_Kind.Package_Body,
        Iir_Kind.Architecture_Body,
    ]

    Package_Declaration = [
        Iir_Kind.Package_Declaration,
        Iir_Kind.Package_Instantiation_Declaration,
    ]

    Verification_Unit = [
        Iir_Kind.Vmode_Declaration,
        Iir_Kind.Vprop_Declaration,
        Iir_Kind.Vunit_Declaration,
    ]

    Literal = [
        Iir_Kind.Integer_Literal,
        Iir_Kind.Floating_Point_Literal,
        Iir_Kind.Null_Literal,
        Iir_Kind.String_Literal8,
        Iir_Kind.Physical_Int_Literal,
        Iir_Kind.Physical_Fp_Literal,
    ]

    Physical_Literal = [
        Iir_Kind.Physical_Int_Literal,
        Iir_Kind.Physical_Fp_Literal,
    ]

    Array_Type_Definition = [
        Iir_Kind.Array_Type_Definition,
        Iir_Kind.Array_Subtype_Definition,
    ]

    Type_And_Subtype_Definition = [
        Iir_Kind.Access_Type_Definition,
        Iir_Kind.Incomplete_Type_Definition,
        Iir_Kind.Interface_Type_Definition,
        Iir_Kind.File_Type_Definition,
        Iir_Kind.Protected_Type_Declaration,
        Iir_Kind.Record_Type_Definition,
        Iir_Kind.Array_Type_Definition,
        Iir_Kind.Array_Subtype_Definition,
        Iir_Kind.Record_Subtype_Definition,
        Iir_Kind.Access_Subtype_Definition,
        Iir_Kind.File_Subtype_Definition,
        Iir_Kind.Physical_Subtype_Definition,
        Iir_Kind.Floating_Subtype_Definition,
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
        Iir_Kind.Enumeration_Type_Definition,
        Iir_Kind.Integer_Type_Definition,
        Iir_Kind.Floating_Type_Definition,
        Iir_Kind.Physical_Type_Definition,
    ]

    Subtype_Definition = [
        Iir_Kind.Array_Subtype_Definition,
        Iir_Kind.Record_Subtype_Definition,
        Iir_Kind.Access_Subtype_Definition,
        Iir_Kind.File_Subtype_Definition,
        Iir_Kind.Physical_Subtype_Definition,
        Iir_Kind.Floating_Subtype_Definition,
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
    ]

    Scalar_Subtype_Definition = [
        Iir_Kind.Physical_Subtype_Definition,
        Iir_Kind.Floating_Subtype_Definition,
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
    ]

    Scalar_Type_And_Subtype_Definition = [
        Iir_Kind.Physical_Subtype_Definition,
        Iir_Kind.Floating_Subtype_Definition,
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
        Iir_Kind.Enumeration_Type_Definition,
        Iir_Kind.Integer_Type_Definition,
        Iir_Kind.Floating_Type_Definition,
        Iir_Kind.Physical_Type_Definition,
    ]

    Range_Type_Definition = [
        Iir_Kind.Physical_Subtype_Definition,
        Iir_Kind.Floating_Subtype_Definition,
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
        Iir_Kind.Enumeration_Type_Definition,
    ]

    Discrete_Type_Definition = [
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
        Iir_Kind.Enumeration_Type_Definition,
        Iir_Kind.Integer_Type_Definition,
    ]

    Composite_Type_Definition = [
        Iir_Kind.Record_Type_Definition,
        Iir_Kind.Array_Type_Definition,
        Iir_Kind.Array_Subtype_Definition,
        Iir_Kind.Record_Subtype_Definition,
    ]

    Composite_Subtype_Definition = [
        Iir_Kind.Array_Subtype_Definition,
        Iir_Kind.Record_Subtype_Definition,
    ]

    Type_Declaration = [
        Iir_Kind.Type_Declaration,
        Iir_Kind.Anonymous_Type_Declaration,
        Iir_Kind.Subtype_Declaration,
    ]

    Nature_Definition = [
        Iir_Kind.Scalar_Nature_Definition,
        Iir_Kind.Record_Nature_Definition,
        Iir_Kind.Array_Nature_Definition,
    ]

    Subnature_Definition = [
        Iir_Kind.Array_Subnature_Definition,
    ]

    Nature_Indication = [
        Iir_Kind.Scalar_Nature_Definition,
        Iir_Kind.Record_Nature_Definition,
        Iir_Kind.Array_Nature_Definition,
        Iir_Kind.Array_Subnature_Definition,
    ]

    Nonoverloadable_Declaration = [
        Iir_Kind.Type_Declaration,
        Iir_Kind.Anonymous_Type_Declaration,
        Iir_Kind.Subtype_Declaration,
        Iir_Kind.Nature_Declaration,
        Iir_Kind.Subnature_Declaration,
        Iir_Kind.Package_Header,
        Iir_Kind.Unit_Declaration,
        Iir_Kind.Library_Declaration,
        Iir_Kind.Component_Declaration,
        Iir_Kind.Attribute_Declaration,
        Iir_Kind.Group_Template_Declaration,
        Iir_Kind.Group_Declaration,
        Iir_Kind.Element_Declaration,
        Iir_Kind.Nature_Element_Declaration,
    ]

    Monadic_Operator = [
        Iir_Kind.Identity_Operator,
        Iir_Kind.Negation_Operator,
        Iir_Kind.Absolute_Operator,
        Iir_Kind.Not_Operator,
        Iir_Kind.Implicit_Condition_Operator,
        Iir_Kind.Condition_Operator,
        Iir_Kind.Reduction_And_Operator,
        Iir_Kind.Reduction_Or_Operator,
        Iir_Kind.Reduction_Nand_Operator,
        Iir_Kind.Reduction_Nor_Operator,
        Iir_Kind.Reduction_Xor_Operator,
        Iir_Kind.Reduction_Xnor_Operator,
    ]

    Dyadic_Operator = [
        Iir_Kind.And_Operator,
        Iir_Kind.Or_Operator,
        Iir_Kind.Nand_Operator,
        Iir_Kind.Nor_Operator,
        Iir_Kind.Xor_Operator,
        Iir_Kind.Xnor_Operator,
        Iir_Kind.Equality_Operator,
        Iir_Kind.Inequality_Operator,
        Iir_Kind.Less_Than_Operator,
        Iir_Kind.Less_Than_Or_Equal_Operator,
        Iir_Kind.Greater_Than_Operator,
        Iir_Kind.Greater_Than_Or_Equal_Operator,
        Iir_Kind.Match_Equality_Operator,
        Iir_Kind.Match_Inequality_Operator,
        Iir_Kind.Match_Less_Than_Operator,
        Iir_Kind.Match_Less_Than_Or_Equal_Operator,
        Iir_Kind.Match_Greater_Than_Operator,
        Iir_Kind.Match_Greater_Than_Or_Equal_Operator,
        Iir_Kind.Sll_Operator,
        Iir_Kind.Sla_Operator,
        Iir_Kind.Srl_Operator,
        Iir_Kind.Sra_Operator,
        Iir_Kind.Rol_Operator,
        Iir_Kind.Ror_Operator,
        Iir_Kind.Addition_Operator,
        Iir_Kind.Substraction_Operator,
        Iir_Kind.Concatenation_Operator,
        Iir_Kind.Multiplication_Operator,
        Iir_Kind.Division_Operator,
        Iir_Kind.Modulus_Operator,
        Iir_Kind.Remainder_Operator,
        Iir_Kind.Exponentiation_Operator,
    ]

    Psl_Builtin = [
        Iir_Kind.Psl_Prev,
        Iir_Kind.Psl_Stable,
        Iir_Kind.Psl_Rose,
        Iir_Kind.Psl_Fell,
        Iir_Kind.Psl_Onehot,
        Iir_Kind.Psl_Onehot0,
    ]

    Functions_And_Literals = [
        Iir_Kind.Enumeration_Literal,
        Iir_Kind.Function_Declaration,
    ]

    Subprogram_Declaration = [
        Iir_Kind.Function_Declaration,
        Iir_Kind.Procedure_Declaration,
    ]

    Subprogram_Body = [
        Iir_Kind.Function_Body,
        Iir_Kind.Procedure_Body,
    ]

    Subprogram_Instantiation_Declaration = [
        Iir_Kind.Function_Instantiation_Declaration,
        Iir_Kind.Procedure_Instantiation_Declaration,
    ]

    Interface_Object_Declaration = [
        Iir_Kind.Interface_Constant_Declaration,
        Iir_Kind.Interface_Variable_Declaration,
        Iir_Kind.Interface_Signal_Declaration,
        Iir_Kind.Interface_View_Declaration,
        Iir_Kind.Interface_File_Declaration,
        Iir_Kind.Interface_Quantity_Declaration,
    ]

    Interface_Subprogram_Declaration = [
        Iir_Kind.Interface_Function_Declaration,
        Iir_Kind.Interface_Procedure_Declaration,
    ]

    Interface_Declaration = [
        Iir_Kind.Interface_Constant_Declaration,
        Iir_Kind.Interface_Variable_Declaration,
        Iir_Kind.Interface_Signal_Declaration,
        Iir_Kind.Interface_View_Declaration,
        Iir_Kind.Interface_File_Declaration,
        Iir_Kind.Interface_Quantity_Declaration,
        Iir_Kind.Interface_Terminal_Declaration,
        Iir_Kind.Interface_Type_Declaration,
        Iir_Kind.Interface_Package_Declaration,
        Iir_Kind.Interface_Function_Declaration,
        Iir_Kind.Interface_Procedure_Declaration,
    ]

    Object_Declaration = [
        Iir_Kind.Object_Alias_Declaration,
        Iir_Kind.Free_Quantity_Declaration,
        Iir_Kind.Spectrum_Quantity_Declaration,
        Iir_Kind.Noise_Quantity_Declaration,
        Iir_Kind.Across_Quantity_Declaration,
        Iir_Kind.Through_Quantity_Declaration,
        Iir_Kind.File_Declaration,
        Iir_Kind.Guard_Signal_Declaration,
        Iir_Kind.Signal_Declaration,
        Iir_Kind.Variable_Declaration,
        Iir_Kind.Constant_Declaration,
        Iir_Kind.Iterator_Declaration,
        Iir_Kind.Interface_Constant_Declaration,
        Iir_Kind.Interface_Variable_Declaration,
        Iir_Kind.Interface_Signal_Declaration,
        Iir_Kind.Interface_View_Declaration,
        Iir_Kind.Interface_File_Declaration,
        Iir_Kind.Interface_Quantity_Declaration,
    ]

    Branch_Quantity_Declaration = [
        Iir_Kind.Across_Quantity_Declaration,
        Iir_Kind.Through_Quantity_Declaration,
    ]

    Source_Quantity_Declaration = [
        Iir_Kind.Spectrum_Quantity_Declaration,
        Iir_Kind.Noise_Quantity_Declaration,
    ]

    Quantity_Declaration = [
        Iir_Kind.Free_Quantity_Declaration,
        Iir_Kind.Spectrum_Quantity_Declaration,
        Iir_Kind.Noise_Quantity_Declaration,
        Iir_Kind.Across_Quantity_Declaration,
        Iir_Kind.Through_Quantity_Declaration,
    ]

    Non_Alias_Object_Declaration = [
        Iir_Kind.File_Declaration,
        Iir_Kind.Guard_Signal_Declaration,
        Iir_Kind.Signal_Declaration,
        Iir_Kind.Variable_Declaration,
        Iir_Kind.Constant_Declaration,
        Iir_Kind.Iterator_Declaration,
        Iir_Kind.Interface_Constant_Declaration,
        Iir_Kind.Interface_Variable_Declaration,
        Iir_Kind.Interface_Signal_Declaration,
        Iir_Kind.Interface_View_Declaration,
        Iir_Kind.Interface_File_Declaration,
    ]

    Association_Element_Parameters = [
        Iir_Kind.Association_Element_By_Expression,
        Iir_Kind.Association_Element_By_Name,
        Iir_Kind.Association_Element_By_Individual,
        Iir_Kind.Association_Element_Open,
    ]

    Association_Element_By_Actual = [
        Iir_Kind.Association_Element_By_Expression,
        Iir_Kind.Association_Element_By_Name,
    ]

    Association_Element = [
        Iir_Kind.Association_Element_By_Expression,
        Iir_Kind.Association_Element_By_Name,
        Iir_Kind.Association_Element_By_Individual,
        Iir_Kind.Association_Element_Open,
        Iir_Kind.Association_Element_Package,
        Iir_Kind.Association_Element_Type,
        Iir_Kind.Association_Element_Subprogram,
        Iir_Kind.Association_Element_Terminal,
    ]

    Choice = [
        Iir_Kind.Choice_By_Range,
        Iir_Kind.Choice_By_Expression,
        Iir_Kind.Choice_By_Others,
        Iir_Kind.Choice_By_None,
        Iir_Kind.Choice_By_Name,
    ]

    Case_Choice = [
        Iir_Kind.Choice_By_Range,
        Iir_Kind.Choice_By_Expression,
        Iir_Kind.Choice_By_Others,
    ]

    Array_Choice = [
        Iir_Kind.Choice_By_Range,
        Iir_Kind.Choice_By_Expression,
        Iir_Kind.Choice_By_Others,
        Iir_Kind.Choice_By_None,
    ]

    Record_Choice = [
        Iir_Kind.Choice_By_Others,
        Iir_Kind.Choice_By_None,
        Iir_Kind.Choice_By_Name,
    ]

    Entity_Aspect = [
        Iir_Kind.Entity_Aspect_Entity,
        Iir_Kind.Entity_Aspect_Configuration,
        Iir_Kind.Entity_Aspect_Open,
    ]

    Denoting_Name = [
        Iir_Kind.Character_Literal,
        Iir_Kind.Simple_Name,
        Iir_Kind.Selected_Name,
        Iir_Kind.Operator_Symbol,
        Iir_Kind.Reference_Name,
    ]

    Denoting_And_External_Name = [
        Iir_Kind.Character_Literal,
        Iir_Kind.Simple_Name,
        Iir_Kind.Selected_Name,
        Iir_Kind.Operator_Symbol,
        Iir_Kind.Reference_Name,
        Iir_Kind.External_Constant_Name,
        Iir_Kind.External_Signal_Name,
        Iir_Kind.External_Variable_Name,
    ]

    Name = [
        Iir_Kind.Character_Literal,
        Iir_Kind.Simple_Name,
        Iir_Kind.Selected_Name,
        Iir_Kind.Operator_Symbol,
        Iir_Kind.Reference_Name,
        Iir_Kind.External_Constant_Name,
        Iir_Kind.External_Signal_Name,
        Iir_Kind.External_Variable_Name,
        Iir_Kind.Selected_By_All_Name,
        Iir_Kind.Parenthesis_Name,
    ]

    Dereference = [
        Iir_Kind.Dereference,
        Iir_Kind.Implicit_Dereference,
    ]

    External_Name = [
        Iir_Kind.External_Constant_Name,
        Iir_Kind.External_Signal_Name,
        Iir_Kind.External_Variable_Name,
    ]

    Pathname = [
        Iir_Kind.Package_Pathname,
        Iir_Kind.Absolute_Pathname,
        Iir_Kind.Relative_Pathname,
        Iir_Kind.Pathname_Element,
    ]

    Expression_Attribute = [
        Iir_Kind.Left_Type_Attribute,
        Iir_Kind.Right_Type_Attribute,
        Iir_Kind.High_Type_Attribute,
        Iir_Kind.Low_Type_Attribute,
        Iir_Kind.Ascending_Type_Attribute,
        Iir_Kind.Image_Attribute,
        Iir_Kind.Value_Attribute,
        Iir_Kind.Pos_Attribute,
        Iir_Kind.Val_Attribute,
        Iir_Kind.Succ_Attribute,
        Iir_Kind.Pred_Attribute,
        Iir_Kind.Leftof_Attribute,
        Iir_Kind.Rightof_Attribute,
        Iir_Kind.Signal_Slew_Attribute,
        Iir_Kind.Quantity_Slew_Attribute,
        Iir_Kind.Ramp_Attribute,
        Iir_Kind.Zoh_Attribute,
        Iir_Kind.Ltf_Attribute,
        Iir_Kind.Ztf_Attribute,
        Iir_Kind.Dot_Attribute,
        Iir_Kind.Integ_Attribute,
        Iir_Kind.Quantity_Delayed_Attribute,
        Iir_Kind.Above_Attribute,
        Iir_Kind.Delayed_Attribute,
        Iir_Kind.Stable_Attribute,
        Iir_Kind.Quiet_Attribute,
        Iir_Kind.Transaction_Attribute,
        Iir_Kind.Event_Attribute,
        Iir_Kind.Active_Attribute,
        Iir_Kind.Last_Event_Attribute,
        Iir_Kind.Last_Active_Attribute,
        Iir_Kind.Last_Value_Attribute,
        Iir_Kind.Driving_Attribute,
        Iir_Kind.Driving_Value_Attribute,
        Iir_Kind.Behavior_Attribute,
        Iir_Kind.Structure_Attribute,
        Iir_Kind.Simple_Name_Attribute,
        Iir_Kind.Instance_Name_Attribute,
        Iir_Kind.Path_Name_Attribute,
        Iir_Kind.Converse_Attribute,
        Iir_Kind.Left_Array_Attribute,
        Iir_Kind.Right_Array_Attribute,
        Iir_Kind.High_Array_Attribute,
        Iir_Kind.Low_Array_Attribute,
        Iir_Kind.Length_Array_Attribute,
        Iir_Kind.Ascending_Array_Attribute,
    ]

    Attribute = [
        Iir_Kind.Base_Attribute,
        Iir_Kind.Subtype_Attribute,
        Iir_Kind.Element_Attribute,
        Iir_Kind.Across_Attribute,
        Iir_Kind.Through_Attribute,
        Iir_Kind.Nature_Reference_Attribute,
        Iir_Kind.Left_Type_Attribute,
        Iir_Kind.Right_Type_Attribute,
        Iir_Kind.High_Type_Attribute,
        Iir_Kind.Low_Type_Attribute,
        Iir_Kind.Ascending_Type_Attribute,
        Iir_Kind.Image_Attribute,
        Iir_Kind.Value_Attribute,
        Iir_Kind.Pos_Attribute,
        Iir_Kind.Val_Attribute,
        Iir_Kind.Succ_Attribute,
        Iir_Kind.Pred_Attribute,
        Iir_Kind.Leftof_Attribute,
        Iir_Kind.Rightof_Attribute,
        Iir_Kind.Signal_Slew_Attribute,
        Iir_Kind.Quantity_Slew_Attribute,
        Iir_Kind.Ramp_Attribute,
        Iir_Kind.Zoh_Attribute,
        Iir_Kind.Ltf_Attribute,
        Iir_Kind.Ztf_Attribute,
        Iir_Kind.Dot_Attribute,
        Iir_Kind.Integ_Attribute,
        Iir_Kind.Quantity_Delayed_Attribute,
        Iir_Kind.Above_Attribute,
        Iir_Kind.Delayed_Attribute,
        Iir_Kind.Stable_Attribute,
        Iir_Kind.Quiet_Attribute,
        Iir_Kind.Transaction_Attribute,
        Iir_Kind.Event_Attribute,
        Iir_Kind.Active_Attribute,
        Iir_Kind.Last_Event_Attribute,
        Iir_Kind.Last_Active_Attribute,
        Iir_Kind.Last_Value_Attribute,
        Iir_Kind.Driving_Attribute,
        Iir_Kind.Driving_Value_Attribute,
        Iir_Kind.Behavior_Attribute,
        Iir_Kind.Structure_Attribute,
        Iir_Kind.Simple_Name_Attribute,
        Iir_Kind.Instance_Name_Attribute,
        Iir_Kind.Path_Name_Attribute,
        Iir_Kind.Converse_Attribute,
        Iir_Kind.Left_Array_Attribute,
        Iir_Kind.Right_Array_Attribute,
        Iir_Kind.High_Array_Attribute,
        Iir_Kind.Low_Array_Attribute,
        Iir_Kind.Length_Array_Attribute,
        Iir_Kind.Ascending_Array_Attribute,
        Iir_Kind.Range_Array_Attribute,
        Iir_Kind.Reverse_Range_Array_Attribute,
    ]

    Type_Attribute = [
        Iir_Kind.Left_Type_Attribute,
        Iir_Kind.Right_Type_Attribute,
        Iir_Kind.High_Type_Attribute,
        Iir_Kind.Low_Type_Attribute,
        Iir_Kind.Ascending_Type_Attribute,
    ]

    Subtype_Attribute = [
        Iir_Kind.Base_Attribute,
        Iir_Kind.Subtype_Attribute,
        Iir_Kind.Element_Attribute,
    ]

    Scalar_Type_Attribute = [
        Iir_Kind.Pos_Attribute,
        Iir_Kind.Val_Attribute,
        Iir_Kind.Succ_Attribute,
        Iir_Kind.Pred_Attribute,
        Iir_Kind.Leftof_Attribute,
        Iir_Kind.Rightof_Attribute,
    ]

    Array_Attribute = [
        Iir_Kind.Left_Array_Attribute,
        Iir_Kind.Right_Array_Attribute,
        Iir_Kind.High_Array_Attribute,
        Iir_Kind.Low_Array_Attribute,
        Iir_Kind.Length_Array_Attribute,
        Iir_Kind.Ascending_Array_Attribute,
        Iir_Kind.Range_Array_Attribute,
        Iir_Kind.Reverse_Range_Array_Attribute,
    ]

    Range_Attribute = [
        Iir_Kind.Range_Array_Attribute,
        Iir_Kind.Reverse_Range_Array_Attribute,
    ]

    Signal_Attribute = [
        Iir_Kind.Delayed_Attribute,
        Iir_Kind.Stable_Attribute,
        Iir_Kind.Quiet_Attribute,
        Iir_Kind.Transaction_Attribute,
    ]

    AMS_Signal_Attribute = [
        Iir_Kind.Above_Attribute,
        Iir_Kind.Delayed_Attribute,
        Iir_Kind.Stable_Attribute,
        Iir_Kind.Quiet_Attribute,
        Iir_Kind.Transaction_Attribute,
    ]

    Signal_Value_Attribute = [
        Iir_Kind.Event_Attribute,
        Iir_Kind.Active_Attribute,
        Iir_Kind.Last_Event_Attribute,
        Iir_Kind.Last_Active_Attribute,
        Iir_Kind.Last_Value_Attribute,
        Iir_Kind.Driving_Attribute,
        Iir_Kind.Driving_Value_Attribute,
    ]

    Name_Attribute = [
        Iir_Kind.Simple_Name_Attribute,
        Iir_Kind.Instance_Name_Attribute,
        Iir_Kind.Path_Name_Attribute,
    ]

    Concurrent_Statement = [
        Iir_Kind.Sensitized_Process_Statement,
        Iir_Kind.Process_Statement,
        Iir_Kind.Concurrent_Simple_Signal_Assignment,
        Iir_Kind.Concurrent_Conditional_Signal_Assignment,
        Iir_Kind.Concurrent_Selected_Signal_Assignment,
        Iir_Kind.Concurrent_Assertion_Statement,
        Iir_Kind.Concurrent_Procedure_Call_Statement,
        Iir_Kind.Concurrent_Break_Statement,
        Iir_Kind.Psl_Assert_Directive,
        Iir_Kind.Psl_Assume_Directive,
        Iir_Kind.Psl_Cover_Directive,
        Iir_Kind.Psl_Restrict_Directive,
        Iir_Kind.Block_Statement,
        Iir_Kind.If_Generate_Statement,
        Iir_Kind.Case_Generate_Statement,
        Iir_Kind.For_Generate_Statement,
        Iir_Kind.Component_Instantiation_Statement,
    ]

    Structural_Statement = [
        Iir_Kind.Block_Statement,
        Iir_Kind.If_Generate_Statement,
        Iir_Kind.Case_Generate_Statement,
        Iir_Kind.For_Generate_Statement,
        Iir_Kind.Component_Instantiation_Statement,
    ]

    Simple_Concurrent_Statement = [
        Iir_Kind.Sensitized_Process_Statement,
        Iir_Kind.Process_Statement,
        Iir_Kind.Concurrent_Simple_Signal_Assignment,
        Iir_Kind.Concurrent_Conditional_Signal_Assignment,
        Iir_Kind.Concurrent_Selected_Signal_Assignment,
        Iir_Kind.Concurrent_Assertion_Statement,
        Iir_Kind.Concurrent_Procedure_Call_Statement,
        Iir_Kind.Concurrent_Break_Statement,
        Iir_Kind.Psl_Assert_Directive,
        Iir_Kind.Psl_Assume_Directive,
        Iir_Kind.Psl_Cover_Directive,
        Iir_Kind.Psl_Restrict_Directive,
    ]

    Process_Statement = [
        Iir_Kind.Sensitized_Process_Statement,
        Iir_Kind.Process_Statement,
    ]

    Concurrent_Signal_Assignment = [
        Iir_Kind.Concurrent_Simple_Signal_Assignment,
        Iir_Kind.Concurrent_Conditional_Signal_Assignment,
        Iir_Kind.Concurrent_Selected_Signal_Assignment,
    ]

    Psl_Property_Directive = [
        Iir_Kind.Psl_Assert_Directive,
        Iir_Kind.Psl_Assume_Directive,
    ]

    Psl_Sequence_Directive = [
        Iir_Kind.Psl_Cover_Directive,
        Iir_Kind.Psl_Restrict_Directive,
    ]

    Psl_Directive = [
        Iir_Kind.Psl_Assert_Directive,
        Iir_Kind.Psl_Assume_Directive,
        Iir_Kind.Psl_Cover_Directive,
        Iir_Kind.Psl_Restrict_Directive,
    ]

    Generate_Statement = [
        Iir_Kind.If_Generate_Statement,
        Iir_Kind.Case_Generate_Statement,
        Iir_Kind.For_Generate_Statement,
    ]

    If_Case_Generate_Statement = [
        Iir_Kind.If_Generate_Statement,
        Iir_Kind.Case_Generate_Statement,
    ]

    Simultaneous_Statement = [
        Iir_Kind.Simple_Simultaneous_Statement,
        Iir_Kind.Simultaneous_Null_Statement,
        Iir_Kind.Simultaneous_Procedural_Statement,
        Iir_Kind.Simultaneous_Case_Statement,
        Iir_Kind.Simultaneous_If_Statement,
    ]

    Sequential_Statement = [
        Iir_Kind.Simple_Signal_Assignment_Statement,
        Iir_Kind.Conditional_Signal_Assignment_Statement,
        Iir_Kind.Selected_Waveform_Assignment_Statement,
        Iir_Kind.Signal_Force_Assignment_Statement,
        Iir_Kind.Signal_Release_Assignment_Statement,
        Iir_Kind.Variable_Assignment_Statement,
        Iir_Kind.Conditional_Variable_Assignment_Statement,
        Iir_Kind.Selected_Variable_Assignment_Statement,
        Iir_Kind.Null_Statement,
        Iir_Kind.Assertion_Statement,
        Iir_Kind.Report_Statement,
        Iir_Kind.Next_Statement,
        Iir_Kind.Exit_Statement,
        Iir_Kind.Return_Statement,
        Iir_Kind.Procedure_Call_Statement,
        Iir_Kind.Wait_Statement,
        Iir_Kind.Break_Statement,
        Iir_Kind.For_Loop_Statement,
        Iir_Kind.While_Loop_Statement,
        Iir_Kind.Case_Statement,
        Iir_Kind.If_Statement,
    ]

    Signal_Assignment_Statement = [
        Iir_Kind.Simple_Signal_Assignment_Statement,
        Iir_Kind.Conditional_Signal_Assignment_Statement,
        Iir_Kind.Selected_Waveform_Assignment_Statement,
        Iir_Kind.Signal_Force_Assignment_Statement,
        Iir_Kind.Signal_Release_Assignment_Statement,
    ]

    Sequential_Statement_Ext = [
        Iir_Kind.Simple_Signal_Assignment_Statement,
        Iir_Kind.Conditional_Signal_Assignment_Statement,
        Iir_Kind.Selected_Waveform_Assignment_Statement,
        Iir_Kind.Signal_Force_Assignment_Statement,
        Iir_Kind.Signal_Release_Assignment_Statement,
        Iir_Kind.Variable_Assignment_Statement,
        Iir_Kind.Conditional_Variable_Assignment_Statement,
        Iir_Kind.Selected_Variable_Assignment_Statement,
        Iir_Kind.Null_Statement,
        Iir_Kind.Assertion_Statement,
        Iir_Kind.Report_Statement,
        Iir_Kind.Next_Statement,
        Iir_Kind.Exit_Statement,
        Iir_Kind.Return_Statement,
        Iir_Kind.Procedure_Call_Statement,
        Iir_Kind.Wait_Statement,
        Iir_Kind.Break_Statement,
        Iir_Kind.For_Loop_Statement,
        Iir_Kind.While_Loop_Statement,
        Iir_Kind.Case_Statement,
        Iir_Kind.If_Statement,
        Iir_Kind.Suspend_State_Statement,
    ]

    Next_Exit_Statement = [
        Iir_Kind.Next_Statement,
        Iir_Kind.Exit_Statement,
    ]

    Variable_Assignment_Statement = [
        Iir_Kind.Variable_Assignment_Statement,
        Iir_Kind.Conditional_Variable_Assignment_Statement,
        Iir_Kind.Selected_Variable_Assignment_Statement,
    ]

    Allocator = [
        Iir_Kind.Allocator_By_Expression,
        Iir_Kind.Allocator_By_Subtype,
    ]

    Clause = [
        Iir_Kind.Library_Clause,
        Iir_Kind.Use_Clause,
        Iir_Kind.Context_Reference,
    ]

    Specification = [
        Iir_Kind.Attribute_Specification,
        Iir_Kind.Disconnection_Specification,
        Iir_Kind.Step_Limit_Specification,
        Iir_Kind.Configuration_Specification,
    ]

    Mode_View_Element_Definition = [
        Iir_Kind.Simple_Mode_View_Element,
        Iir_Kind.Array_Mode_View_Element,
        Iir_Kind.Record_Mode_View_Element,
    ]

    Mode_View_Indication = [
        Iir_Kind.Record_Mode_View_Indication,
        Iir_Kind.Array_Mode_View_Indication,
    ]


@export
@unique
class Iir_Mode(IntEnum):
    """
    The mode of an interface object: ``in``, ``out``, ``inout``, ``buffer`` or ``linkage``.
    """

    Unknown_Mode = 0
    Linkage_Mode = 1
    Buffer_Mode = 2
    Out_Mode = 3
    Inout_Mode = 4
    In_Mode = 5


@export
@unique
class ScalarSize(IntEnum):
    """
    The storage size of a scalar type.
    """

    Scalar_8 = 0
    Scalar_16 = 1
    Scalar_32 = 2
    Scalar_64 = 3


@export
@unique
class Iir_Staticness(IntEnum):
    """
    How static an expression or a type is: unknown, none, globally or locally.
    """

    Unknown = 0
    PNone = 1
    Globally = 2
    Locally = 3


@export
@unique
class Iir_Constraint(IntEnum):
    """
    How constrained a composite type is: unconstrained, partially or fully.
    """

    Unconstrained = 0
    Partially_Constrained = 1
    Fully_Constrained = 2


@export
@unique
class Iir_Delay_Mechanism(IntEnum):
    """
    The delay mechanism of a signal assignment: ``inertial`` or ``transport``.
    """

    Inertial_Delay = 0
    Transport_Delay = 1


@export
@unique
class DateStateType(IntEnum):
    """
    How far a design unit has been processed: extern, disk, parse or analyze.
    """

    Extern = 0
    Disk = 1
    Parse = 2
    Analyze = 3


@export
@unique
class NumberBaseType(IntEnum):
    """
    The base a literal was written in.
    """

    Base_None = 0
    Base_2 = 1
    Base_8 = 2
    Base_10 = 3
    Base_16 = 4


@export
@unique
class Iir_Predefined(IntEnum):
    """
    The predefined operation an implicit subprogram implements.
    """

    Error = 0
    Boolean_And = 1
    Boolean_Or = 2
    Boolean_Nand = 3
    Boolean_Nor = 4
    Boolean_Xor = 5
    Boolean_Xnor = 6
    Boolean_Not = 7
    Boolean_Rising_Edge = 8
    Boolean_Falling_Edge = 9
    Enum_Equality = 10
    Enum_Inequality = 11
    Enum_Less = 12
    Enum_Less_Equal = 13
    Enum_Greater = 14
    Enum_Greater_Equal = 15
    Bit_And = 16
    Bit_Or = 17
    Bit_Nand = 18
    Bit_Nor = 19
    Bit_Xor = 20
    Bit_Xnor = 21
    Bit_Not = 22
    Bit_Match_Equality = 23
    Bit_Match_Inequality = 24
    Bit_Match_Less = 25
    Bit_Match_Less_Equal = 26
    Bit_Match_Greater = 27
    Bit_Match_Greater_Equal = 28
    Bit_Condition = 29
    Integer_Equality = 30
    Integer_Inequality = 31
    Integer_Less = 32
    Integer_Less_Equal = 33
    Integer_Greater = 34
    Integer_Greater_Equal = 35
    Integer_Identity = 36
    Integer_Negation = 37
    Integer_Absolute = 38
    Integer_Plus = 39
    Integer_Minus = 40
    Integer_Mul = 41
    Integer_Div = 42
    Integer_Mod = 43
    Integer_Rem = 44
    Integer_Exp = 45
    Floating_Equality = 46
    Floating_Inequality = 47
    Floating_Less = 48
    Floating_Less_Equal = 49
    Floating_Greater = 50
    Floating_Greater_Equal = 51
    Floating_Identity = 52
    Floating_Negation = 53
    Floating_Absolute = 54
    Floating_Plus = 55
    Floating_Minus = 56
    Floating_Mul = 57
    Floating_Div = 58
    Floating_Exp = 59
    Universal_R_I_Mul = 60
    Universal_I_R_Mul = 61
    Universal_R_I_Div = 62
    Physical_Equality = 63
    Physical_Inequality = 64
    Physical_Less = 65
    Physical_Less_Equal = 66
    Physical_Greater = 67
    Physical_Greater_Equal = 68
    Physical_Identity = 69
    Physical_Negation = 70
    Physical_Absolute = 71
    Physical_Plus = 72
    Physical_Minus = 73
    Physical_Integer_Mul = 74
    Physical_Real_Mul = 75
    Integer_Physical_Mul = 76
    Real_Physical_Mul = 77
    Physical_Integer_Div = 78
    Physical_Real_Div = 79
    Physical_Physical_Div = 80
    Physical_Mod = 81
    Physical_Rem = 82
    Access_Equality = 83
    Access_Inequality = 84
    Record_Equality = 85
    Record_Inequality = 86
    Array_Equality = 87
    Array_Inequality = 88
    Array_Less = 89
    Array_Less_Equal = 90
    Array_Greater = 91
    Array_Greater_Equal = 92
    Array_Array_Concat = 93
    Array_Element_Concat = 94
    Element_Array_Concat = 95
    Element_Element_Concat = 96
    Array_Minimum = 97
    Array_Maximum = 98
    Vector_Minimum = 99
    Vector_Maximum = 100
    Array_Sll = 101
    Array_Srl = 102
    Array_Sla = 103
    Array_Sra = 104
    Array_Rol = 105
    Array_Ror = 106
    TF_Array_And = 107
    TF_Array_Or = 108
    TF_Array_Nand = 109
    TF_Array_Nor = 110
    TF_Array_Xor = 111
    TF_Array_Xnor = 112
    TF_Array_Not = 113
    TF_Reduction_And = 114
    TF_Reduction_Or = 115
    TF_Reduction_Nand = 116
    TF_Reduction_Nor = 117
    TF_Reduction_Xor = 118
    TF_Reduction_Xnor = 119
    TF_Reduction_Not = 120
    TF_Array_Element_And = 121
    TF_Element_Array_And = 122
    TF_Array_Element_Or = 123
    TF_Element_Array_Or = 124
    TF_Array_Element_Nand = 125
    TF_Element_Array_Nand = 126
    TF_Array_Element_Nor = 127
    TF_Element_Array_Nor = 128
    TF_Array_Element_Xor = 129
    TF_Element_Array_Xor = 130
    TF_Array_Element_Xnor = 131
    TF_Element_Array_Xnor = 132
    Bit_Array_Match_Equality = 133
    Bit_Array_Match_Inequality = 134
    Std_Ulogic_Match_Equality = 135
    Std_Ulogic_Match_Inequality = 136
    Std_Ulogic_Match_Less = 137
    Std_Ulogic_Match_Less_Equal = 138
    Std_Ulogic_Match_Greater = 139
    Std_Ulogic_Match_Greater_Equal = 140
    Std_Ulogic_Array_Match_Equality = 141
    Std_Ulogic_Array_Match_Inequality = 142
    Enum_Minimum = 143
    Enum_Maximum = 144
    Enum_To_String = 145
    Integer_Minimum = 146
    Integer_Maximum = 147
    Integer_To_String = 148
    Bit_Rising_Edge = 149
    Bit_Falling_Edge = 150
    Floating_Minimum = 151
    Floating_Maximum = 152
    Floating_To_String = 153
    Real_To_String_Digits = 154
    Real_To_String_Format = 155
    Physical_Minimum = 156
    Physical_Maximum = 157
    Physical_To_String = 158
    Time_To_String_Unit = 159
    Array_Char_To_String = 160
    Bit_Vector_To_Ostring = 161
    Bit_Vector_To_Hstring = 162
    Deallocate = 163
    File_Open = 164
    File_Open_Status = 165
    File_Close = 166
    Read = 167
    Read_Length = 168
    Flush = 169
    Write = 170
    Endfile = 171
    Now_Function = 172
    Real_Now_Function = 173
    Frequency_Function = 174
    PNone = 175
    Foreign_Untruncated_Text_Read = 176
    Foreign_Textio_Read_Real = 177
    Foreign_Textio_Write_Real = 178
    Std_Env_Stop_Status = 179
    Std_Env_Stop = 180
    Std_Env_Finish_Status = 181
    Std_Env_Finish = 182
    Std_Env_Resolution_Limit = 183
    Ieee_1164_Scalar_And = 184
    Ieee_1164_Scalar_Nand = 185
    Ieee_1164_Scalar_Or = 186
    Ieee_1164_Scalar_Nor = 187
    Ieee_1164_Scalar_Xor = 188
    Ieee_1164_Scalar_Xnor = 189
    Ieee_1164_Scalar_Not = 190
    Ieee_1164_Vector_And = 191
    Ieee_1164_Vector_Nand = 192
    Ieee_1164_Vector_Or = 193
    Ieee_1164_Vector_Nor = 194
    Ieee_1164_Vector_Xor = 195
    Ieee_1164_Vector_Xnor = 196
    Ieee_1164_Vector_Not = 197
    Ieee_1164_To_Bit = 198
    Ieee_1164_To_Bitvector = 199
    Ieee_1164_To_Stdulogic = 200
    Ieee_1164_To_Stdlogicvector_Bv = 201
    Ieee_1164_To_Stdlogicvector_Suv = 202
    Ieee_1164_To_Stdulogicvector_Bv = 203
    Ieee_1164_To_Stdulogicvector_Slv = 204
    Ieee_1164_To_X01_Slv = 205
    Ieee_1164_To_X01_Suv = 206
    Ieee_1164_To_X01_Log = 207
    Ieee_1164_To_X01_Bv_Slv = 208
    Ieee_1164_To_X01_Bv_Suv = 209
    Ieee_1164_To_X01_Bit_Log = 210
    Ieee_1164_To_X01Z_Slv = 211
    Ieee_1164_To_X01Z_Suv = 212
    Ieee_1164_To_X01Z_Log = 213
    Ieee_1164_To_X01Z_Bv_Slv = 214
    Ieee_1164_To_X01Z_Bv_Suv = 215
    Ieee_1164_To_X01Z_Bit_Log = 216
    Ieee_1164_To_UX01_Slv = 217
    Ieee_1164_To_UX01_Suv = 218
    Ieee_1164_To_UX01_Log = 219
    Ieee_1164_To_UX01_Bv_Slv = 220
    Ieee_1164_To_UX01_Bv_Suv = 221
    Ieee_1164_To_UX01_Bit_Log = 222
    Ieee_1164_Is_X_Slv = 223
    Ieee_1164_Is_X_Log = 224
    Ieee_1164_Rising_Edge = 225
    Ieee_1164_Falling_Edge = 226
    Ieee_1164_And_Suv_Log = 227
    Ieee_1164_And_Log_Suv = 228
    Ieee_1164_Nand_Suv_Log = 229
    Ieee_1164_Nand_Log_Suv = 230
    Ieee_1164_Or_Suv_Log = 231
    Ieee_1164_Or_Log_Suv = 232
    Ieee_1164_Nor_Suv_Log = 233
    Ieee_1164_Nor_Log_Suv = 234
    Ieee_1164_Xor_Suv_Log = 235
    Ieee_1164_Xor_Log_Suv = 236
    Ieee_1164_Xnor_Suv_Log = 237
    Ieee_1164_Xnor_Log_Suv = 238
    Ieee_1164_And_Suv = 239
    Ieee_1164_Nand_Suv = 240
    Ieee_1164_Or_Suv = 241
    Ieee_1164_Nor_Suv = 242
    Ieee_1164_Xor_Suv = 243
    Ieee_1164_Xnor_Suv = 244
    Ieee_1164_Vector_Sll = 245
    Ieee_1164_Vector_Srl = 246
    Ieee_1164_Vector_Rol = 247
    Ieee_1164_Vector_Ror = 248
    Ieee_1164_Condition_Operator = 249
    Ieee_1164_To_01_Log_Log = 250
    Ieee_1164_To_01_Slv_Log = 251
    Ieee_1164_To_Hstring = 252
    Ieee_1164_To_Ostring = 253
    Ieee_Numeric_Std_Toint_Uns_Nat = 254
    Ieee_Numeric_Std_Toint_Sgn_Int = 255
    Ieee_Numeric_Std_Touns_Nat_Nat_Uns = 256
    Ieee_Numeric_Std_Touns_Nat_Uns_Uns = 257
    Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn = 258
    Ieee_Numeric_Std_Tosgn_Int_Sgn_Sgn = 259
    Ieee_Numeric_Std_Resize_Uns_Nat = 260
    Ieee_Numeric_Std_Resize_Sgn_Nat = 261
    Ieee_Numeric_Std_Resize_Uns_Uns = 262
    Ieee_Numeric_Std_Resize_Sgn_Sgn = 263
    Ieee_Numeric_Std_Add_Uns_Uns = 264
    Ieee_Numeric_Std_Add_Uns_Nat = 265
    Ieee_Numeric_Std_Add_Nat_Uns = 266
    Ieee_Numeric_Std_Add_Uns_Log = 267
    Ieee_Numeric_Std_Add_Log_Uns = 268
    Ieee_Numeric_Std_Add_Sgn_Sgn = 269
    Ieee_Numeric_Std_Add_Sgn_Int = 270
    Ieee_Numeric_Std_Add_Int_Sgn = 271
    Ieee_Numeric_Std_Add_Sgn_Log = 272
    Ieee_Numeric_Std_Add_Log_Sgn = 273
    Ieee_Numeric_Std_Sub_Uns_Uns = 274
    Ieee_Numeric_Std_Sub_Uns_Nat = 275
    Ieee_Numeric_Std_Sub_Nat_Uns = 276
    Ieee_Numeric_Std_Sub_Uns_Log = 277
    Ieee_Numeric_Std_Sub_Log_Uns = 278
    Ieee_Numeric_Std_Sub_Sgn_Sgn = 279
    Ieee_Numeric_Std_Sub_Sgn_Int = 280
    Ieee_Numeric_Std_Sub_Int_Sgn = 281
    Ieee_Numeric_Std_Sub_Sgn_Log = 282
    Ieee_Numeric_Std_Sub_Log_Sgn = 283
    Ieee_Numeric_Std_Mul_Uns_Uns = 284
    Ieee_Numeric_Std_Mul_Uns_Nat = 285
    Ieee_Numeric_Std_Mul_Nat_Uns = 286
    Ieee_Numeric_Std_Mul_Sgn_Sgn = 287
    Ieee_Numeric_Std_Mul_Sgn_Int = 288
    Ieee_Numeric_Std_Mul_Int_Sgn = 289
    Ieee_Numeric_Std_Div_Uns_Uns = 290
    Ieee_Numeric_Std_Div_Uns_Nat = 291
    Ieee_Numeric_Std_Div_Nat_Uns = 292
    Ieee_Numeric_Std_Div_Sgn_Sgn = 293
    Ieee_Numeric_Std_Div_Sgn_Int = 294
    Ieee_Numeric_Std_Div_Int_Sgn = 295
    Ieee_Numeric_Std_Rem_Uns_Uns = 296
    Ieee_Numeric_Std_Rem_Uns_Nat = 297
    Ieee_Numeric_Std_Rem_Nat_Uns = 298
    Ieee_Numeric_Std_Rem_Sgn_Sgn = 299
    Ieee_Numeric_Std_Rem_Sgn_Int = 300
    Ieee_Numeric_Std_Rem_Int_Sgn = 301
    Ieee_Numeric_Std_Mod_Uns_Uns = 302
    Ieee_Numeric_Std_Mod_Uns_Nat = 303
    Ieee_Numeric_Std_Mod_Nat_Uns = 304
    Ieee_Numeric_Std_Mod_Sgn_Sgn = 305
    Ieee_Numeric_Std_Mod_Sgn_Int = 306
    Ieee_Numeric_Std_Mod_Int_Sgn = 307
    Ieee_Numeric_Std_Gt_Uns_Uns = 308
    Ieee_Numeric_Std_Gt_Uns_Nat = 309
    Ieee_Numeric_Std_Gt_Nat_Uns = 310
    Ieee_Numeric_Std_Gt_Sgn_Sgn = 311
    Ieee_Numeric_Std_Gt_Sgn_Int = 312
    Ieee_Numeric_Std_Gt_Int_Sgn = 313
    Ieee_Numeric_Std_Lt_Uns_Uns = 314
    Ieee_Numeric_Std_Lt_Uns_Nat = 315
    Ieee_Numeric_Std_Lt_Nat_Uns = 316
    Ieee_Numeric_Std_Lt_Sgn_Sgn = 317
    Ieee_Numeric_Std_Lt_Sgn_Int = 318
    Ieee_Numeric_Std_Lt_Int_Sgn = 319
    Ieee_Numeric_Std_Le_Uns_Uns = 320
    Ieee_Numeric_Std_Le_Uns_Nat = 321
    Ieee_Numeric_Std_Le_Nat_Uns = 322
    Ieee_Numeric_Std_Le_Sgn_Sgn = 323
    Ieee_Numeric_Std_Le_Sgn_Int = 324
    Ieee_Numeric_Std_Le_Int_Sgn = 325
    Ieee_Numeric_Std_Ge_Uns_Uns = 326
    Ieee_Numeric_Std_Ge_Uns_Nat = 327
    Ieee_Numeric_Std_Ge_Nat_Uns = 328
    Ieee_Numeric_Std_Ge_Sgn_Sgn = 329
    Ieee_Numeric_Std_Ge_Sgn_Int = 330
    Ieee_Numeric_Std_Ge_Int_Sgn = 331
    Ieee_Numeric_Std_Eq_Uns_Uns = 332
    Ieee_Numeric_Std_Eq_Uns_Nat = 333
    Ieee_Numeric_Std_Eq_Nat_Uns = 334
    Ieee_Numeric_Std_Eq_Sgn_Sgn = 335
    Ieee_Numeric_Std_Eq_Sgn_Int = 336
    Ieee_Numeric_Std_Eq_Int_Sgn = 337
    Ieee_Numeric_Std_Ne_Uns_Uns = 338
    Ieee_Numeric_Std_Ne_Uns_Nat = 339
    Ieee_Numeric_Std_Ne_Nat_Uns = 340
    Ieee_Numeric_Std_Ne_Sgn_Sgn = 341
    Ieee_Numeric_Std_Ne_Sgn_Int = 342
    Ieee_Numeric_Std_Ne_Int_Sgn = 343
    Ieee_Numeric_Std_Match_Gt_Uns_Uns = 344
    Ieee_Numeric_Std_Match_Gt_Uns_Nat = 345
    Ieee_Numeric_Std_Match_Gt_Nat_Uns = 346
    Ieee_Numeric_Std_Match_Gt_Sgn_Sgn = 347
    Ieee_Numeric_Std_Match_Gt_Sgn_Int = 348
    Ieee_Numeric_Std_Match_Gt_Int_Sgn = 349
    Ieee_Numeric_Std_Match_Lt_Uns_Uns = 350
    Ieee_Numeric_Std_Match_Lt_Uns_Nat = 351
    Ieee_Numeric_Std_Match_Lt_Nat_Uns = 352
    Ieee_Numeric_Std_Match_Lt_Sgn_Sgn = 353
    Ieee_Numeric_Std_Match_Lt_Sgn_Int = 354
    Ieee_Numeric_Std_Match_Lt_Int_Sgn = 355
    Ieee_Numeric_Std_Match_Le_Uns_Uns = 356
    Ieee_Numeric_Std_Match_Le_Uns_Nat = 357
    Ieee_Numeric_Std_Match_Le_Nat_Uns = 358
    Ieee_Numeric_Std_Match_Le_Sgn_Sgn = 359
    Ieee_Numeric_Std_Match_Le_Sgn_Int = 360
    Ieee_Numeric_Std_Match_Le_Int_Sgn = 361
    Ieee_Numeric_Std_Match_Ge_Uns_Uns = 362
    Ieee_Numeric_Std_Match_Ge_Uns_Nat = 363
    Ieee_Numeric_Std_Match_Ge_Nat_Uns = 364
    Ieee_Numeric_Std_Match_Ge_Sgn_Sgn = 365
    Ieee_Numeric_Std_Match_Ge_Sgn_Int = 366
    Ieee_Numeric_Std_Match_Ge_Int_Sgn = 367
    Ieee_Numeric_Std_Match_Eq_Uns_Uns = 368
    Ieee_Numeric_Std_Match_Eq_Uns_Nat = 369
    Ieee_Numeric_Std_Match_Eq_Nat_Uns = 370
    Ieee_Numeric_Std_Match_Eq_Sgn_Sgn = 371
    Ieee_Numeric_Std_Match_Eq_Sgn_Int = 372
    Ieee_Numeric_Std_Match_Eq_Int_Sgn = 373
    Ieee_Numeric_Std_Match_Ne_Uns_Uns = 374
    Ieee_Numeric_Std_Match_Ne_Uns_Nat = 375
    Ieee_Numeric_Std_Match_Ne_Nat_Uns = 376
    Ieee_Numeric_Std_Match_Ne_Sgn_Sgn = 377
    Ieee_Numeric_Std_Match_Ne_Sgn_Int = 378
    Ieee_Numeric_Std_Match_Ne_Int_Sgn = 379
    Ieee_Numeric_Std_Sll_Uns_Int = 380
    Ieee_Numeric_Std_Sll_Sgn_Int = 381
    Ieee_Numeric_Std_Srl_Uns_Int = 382
    Ieee_Numeric_Std_Srl_Sgn_Int = 383
    Ieee_Numeric_Std_Sla_Uns_Int = 384
    Ieee_Numeric_Std_Sla_Sgn_Int = 385
    Ieee_Numeric_Std_Sra_Uns_Int = 386
    Ieee_Numeric_Std_Sra_Sgn_Int = 387
    Ieee_Numeric_Std_Rol_Uns_Int = 388
    Ieee_Numeric_Std_Rol_Sgn_Int = 389
    Ieee_Numeric_Std_Ror_Uns_Int = 390
    Ieee_Numeric_Std_Ror_Sgn_Int = 391
    Ieee_Numeric_Std_And_Uns_Uns = 392
    Ieee_Numeric_Std_And_Uns_Log = 393
    Ieee_Numeric_Std_And_Log_Uns = 394
    Ieee_Numeric_Std_And_Sgn_Sgn = 395
    Ieee_Numeric_Std_And_Sgn_Log = 396
    Ieee_Numeric_Std_And_Log_Sgn = 397
    Ieee_Numeric_Std_Nand_Uns_Uns = 398
    Ieee_Numeric_Std_Nand_Uns_Log = 399
    Ieee_Numeric_Std_Nand_Log_Uns = 400
    Ieee_Numeric_Std_Nand_Sgn_Sgn = 401
    Ieee_Numeric_Std_Nand_Sgn_Log = 402
    Ieee_Numeric_Std_Nand_Log_Sgn = 403
    Ieee_Numeric_Std_Or_Uns_Uns = 404
    Ieee_Numeric_Std_Or_Uns_Log = 405
    Ieee_Numeric_Std_Or_Log_Uns = 406
    Ieee_Numeric_Std_Or_Sgn_Sgn = 407
    Ieee_Numeric_Std_Or_Sgn_Log = 408
    Ieee_Numeric_Std_Or_Log_Sgn = 409
    Ieee_Numeric_Std_Nor_Uns_Uns = 410
    Ieee_Numeric_Std_Nor_Uns_Log = 411
    Ieee_Numeric_Std_Nor_Log_Uns = 412
    Ieee_Numeric_Std_Nor_Sgn_Sgn = 413
    Ieee_Numeric_Std_Nor_Sgn_Log = 414
    Ieee_Numeric_Std_Nor_Log_Sgn = 415
    Ieee_Numeric_Std_Xor_Uns_Uns = 416
    Ieee_Numeric_Std_Xor_Uns_Log = 417
    Ieee_Numeric_Std_Xor_Log_Uns = 418
    Ieee_Numeric_Std_Xor_Sgn_Sgn = 419
    Ieee_Numeric_Std_Xor_Sgn_Log = 420
    Ieee_Numeric_Std_Xor_Log_Sgn = 421
    Ieee_Numeric_Std_Xnor_Uns_Uns = 422
    Ieee_Numeric_Std_Xnor_Uns_Log = 423
    Ieee_Numeric_Std_Xnor_Log_Uns = 424
    Ieee_Numeric_Std_Xnor_Sgn_Sgn = 425
    Ieee_Numeric_Std_Xnor_Sgn_Log = 426
    Ieee_Numeric_Std_Xnor_Log_Sgn = 427
    Ieee_Numeric_Std_Not_Uns = 428
    Ieee_Numeric_Std_Not_Sgn = 429
    Ieee_Numeric_Std_Abs_Sgn = 430
    Ieee_Numeric_Std_Neg_Uns = 431
    Ieee_Numeric_Std_Neg_Sgn = 432
    Ieee_Numeric_Std_Min_Uns_Uns = 433
    Ieee_Numeric_Std_Min_Uns_Nat = 434
    Ieee_Numeric_Std_Min_Nat_Uns = 435
    Ieee_Numeric_Std_Min_Sgn_Sgn = 436
    Ieee_Numeric_Std_Min_Sgn_Int = 437
    Ieee_Numeric_Std_Min_Int_Sgn = 438
    Ieee_Numeric_Std_Max_Uns_Uns = 439
    Ieee_Numeric_Std_Max_Uns_Nat = 440
    Ieee_Numeric_Std_Max_Nat_Uns = 441
    Ieee_Numeric_Std_Max_Sgn_Sgn = 442
    Ieee_Numeric_Std_Max_Sgn_Int = 443
    Ieee_Numeric_Std_Max_Int_Sgn = 444
    Ieee_Numeric_Std_Shf_Left_Uns_Nat = 445
    Ieee_Numeric_Std_Shf_Right_Uns_Nat = 446
    Ieee_Numeric_Std_Shf_Left_Sgn_Nat = 447
    Ieee_Numeric_Std_Shf_Right_Sgn_Nat = 448
    Ieee_Numeric_Std_Rot_Left_Uns_Nat = 449
    Ieee_Numeric_Std_Rot_Right_Uns_Nat = 450
    Ieee_Numeric_Std_Rot_Left_Sgn_Nat = 451
    Ieee_Numeric_Std_Rot_Right_Sgn_Nat = 452
    Ieee_Numeric_Std_And_Sgn = 453
    Ieee_Numeric_Std_Nand_Sgn = 454
    Ieee_Numeric_Std_Or_Sgn = 455
    Ieee_Numeric_Std_Nor_Sgn = 456
    Ieee_Numeric_Std_Xor_Sgn = 457
    Ieee_Numeric_Std_Xnor_Sgn = 458
    Ieee_Numeric_Std_And_Uns = 459
    Ieee_Numeric_Std_Nand_Uns = 460
    Ieee_Numeric_Std_Or_Uns = 461
    Ieee_Numeric_Std_Nor_Uns = 462
    Ieee_Numeric_Std_Xor_Uns = 463
    Ieee_Numeric_Std_Xnor_Uns = 464
    Ieee_Numeric_Std_Find_Leftmost_Uns = 465
    Ieee_Numeric_Std_Find_Rightmost_Uns = 466
    Ieee_Numeric_Std_Find_Leftmost_Sgn = 467
    Ieee_Numeric_Std_Find_Rightmost_Sgn = 468
    Ieee_Numeric_Std_Match_Log = 469
    Ieee_Numeric_Std_Match_Uns = 470
    Ieee_Numeric_Std_Match_Sgn = 471
    Ieee_Numeric_Std_Match_Slv = 472
    Ieee_Numeric_Std_Match_Suv = 473
    Ieee_Numeric_Std_To_01_Uns = 474
    Ieee_Numeric_Std_To_01_Sgn = 475
    Ieee_Numeric_Std_To_X01_Uns = 476
    Ieee_Numeric_Std_To_X01_Sgn = 477
    Ieee_Numeric_Std_To_X01Z_Uns = 478
    Ieee_Numeric_Std_To_X01Z_Sgn = 479
    Ieee_Numeric_Std_To_UX01_Uns = 480
    Ieee_Numeric_Std_To_UX01_Sgn = 481
    Ieee_Numeric_Std_Is_X_Uns = 482
    Ieee_Numeric_Std_Is_X_Sgn = 483
    Ieee_Numeric_Std_To_Hstring_Uns = 484
    Ieee_Numeric_Std_To_Ostring_Uns = 485
    Ieee_Numeric_Std_To_Hstring_Sgn = 486
    Ieee_Numeric_Std_To_Ostring_Sgn = 487
    Ieee_Numeric_Bit_Not_Uns = 488
    Ieee_Numeric_Bit_Not_Sgn = 489
    Ieee_Numeric_Bit_Abs_Sgn = 490
    Ieee_Numeric_Bit_Neg_Sgn = 491
    Ieee_Numeric_Bit_Add_Uns_Uns = 492
    Ieee_Numeric_Bit_Add_Uns_Nat = 493
    Ieee_Numeric_Bit_Add_Nat_Uns = 494
    Ieee_Numeric_Bit_Add_Uns_Bit = 495
    Ieee_Numeric_Bit_Add_Bit_Uns = 496
    Ieee_Numeric_Bit_Add_Sgn_Sgn = 497
    Ieee_Numeric_Bit_Add_Sgn_Int = 498
    Ieee_Numeric_Bit_Add_Int_Sgn = 499
    Ieee_Numeric_Bit_Add_Sgn_Bit = 500
    Ieee_Numeric_Bit_Add_Bit_Sgn = 501
    Ieee_Numeric_Bit_Sub_Uns_Uns = 502
    Ieee_Numeric_Bit_Sub_Uns_Nat = 503
    Ieee_Numeric_Bit_Sub_Nat_Uns = 504
    Ieee_Numeric_Bit_Sub_Uns_Bit = 505
    Ieee_Numeric_Bit_Sub_Bit_Uns = 506
    Ieee_Numeric_Bit_Sub_Sgn_Sgn = 507
    Ieee_Numeric_Bit_Sub_Sgn_Int = 508
    Ieee_Numeric_Bit_Sub_Int_Sgn = 509
    Ieee_Numeric_Bit_Sub_Sgn_Bit = 510
    Ieee_Numeric_Bit_Sub_Bit_Sgn = 511
    Ieee_Numeric_Bit_Mul_Uns_Uns = 512
    Ieee_Numeric_Bit_Mul_Uns_Nat = 513
    Ieee_Numeric_Bit_Mul_Nat_Uns = 514
    Ieee_Numeric_Bit_Mul_Sgn_Sgn = 515
    Ieee_Numeric_Bit_Mul_Sgn_Int = 516
    Ieee_Numeric_Bit_Mul_Int_Sgn = 517
    Ieee_Numeric_Bit_Div_Uns_Uns = 518
    Ieee_Numeric_Bit_Div_Uns_Nat = 519
    Ieee_Numeric_Bit_Div_Nat_Uns = 520
    Ieee_Numeric_Bit_Div_Sgn_Sgn = 521
    Ieee_Numeric_Bit_Div_Sgn_Int = 522
    Ieee_Numeric_Bit_Div_Int_Sgn = 523
    Ieee_Numeric_Bit_Rem_Uns_Uns = 524
    Ieee_Numeric_Bit_Rem_Uns_Nat = 525
    Ieee_Numeric_Bit_Rem_Nat_Uns = 526
    Ieee_Numeric_Bit_Rem_Sgn_Sgn = 527
    Ieee_Numeric_Bit_Rem_Sgn_Int = 528
    Ieee_Numeric_Bit_Rem_Int_Sgn = 529
    Ieee_Numeric_Bit_Mod_Uns_Uns = 530
    Ieee_Numeric_Bit_Mod_Uns_Nat = 531
    Ieee_Numeric_Bit_Mod_Nat_Uns = 532
    Ieee_Numeric_Bit_Mod_Sgn_Sgn = 533
    Ieee_Numeric_Bit_Mod_Sgn_Int = 534
    Ieee_Numeric_Bit_Mod_Int_Sgn = 535
    Ieee_Numeric_Bit_Gt_Uns_Uns = 536
    Ieee_Numeric_Bit_Gt_Uns_Nat = 537
    Ieee_Numeric_Bit_Gt_Nat_Uns = 538
    Ieee_Numeric_Bit_Gt_Sgn_Sgn = 539
    Ieee_Numeric_Bit_Gt_Sgn_Int = 540
    Ieee_Numeric_Bit_Gt_Int_Sgn = 541
    Ieee_Numeric_Bit_Lt_Uns_Uns = 542
    Ieee_Numeric_Bit_Lt_Uns_Nat = 543
    Ieee_Numeric_Bit_Lt_Nat_Uns = 544
    Ieee_Numeric_Bit_Lt_Sgn_Sgn = 545
    Ieee_Numeric_Bit_Lt_Sgn_Int = 546
    Ieee_Numeric_Bit_Lt_Int_Sgn = 547
    Ieee_Numeric_Bit_Le_Uns_Uns = 548
    Ieee_Numeric_Bit_Le_Uns_Nat = 549
    Ieee_Numeric_Bit_Le_Nat_Uns = 550
    Ieee_Numeric_Bit_Le_Sgn_Sgn = 551
    Ieee_Numeric_Bit_Le_Sgn_Int = 552
    Ieee_Numeric_Bit_Le_Int_Sgn = 553
    Ieee_Numeric_Bit_Ge_Uns_Uns = 554
    Ieee_Numeric_Bit_Ge_Uns_Nat = 555
    Ieee_Numeric_Bit_Ge_Nat_Uns = 556
    Ieee_Numeric_Bit_Ge_Sgn_Sgn = 557
    Ieee_Numeric_Bit_Ge_Sgn_Int = 558
    Ieee_Numeric_Bit_Ge_Int_Sgn = 559
    Ieee_Numeric_Bit_Eq_Uns_Uns = 560
    Ieee_Numeric_Bit_Eq_Uns_Nat = 561
    Ieee_Numeric_Bit_Eq_Nat_Uns = 562
    Ieee_Numeric_Bit_Eq_Sgn_Sgn = 563
    Ieee_Numeric_Bit_Eq_Sgn_Int = 564
    Ieee_Numeric_Bit_Eq_Int_Sgn = 565
    Ieee_Numeric_Bit_Ne_Uns_Uns = 566
    Ieee_Numeric_Bit_Ne_Uns_Nat = 567
    Ieee_Numeric_Bit_Ne_Nat_Uns = 568
    Ieee_Numeric_Bit_Ne_Sgn_Sgn = 569
    Ieee_Numeric_Bit_Ne_Sgn_Int = 570
    Ieee_Numeric_Bit_Ne_Int_Sgn = 571
    Ieee_Numeric_Bit_Match_Gt_Uns_Uns = 572
    Ieee_Numeric_Bit_Match_Gt_Uns_Nat = 573
    Ieee_Numeric_Bit_Match_Gt_Nat_Uns = 574
    Ieee_Numeric_Bit_Match_Gt_Sgn_Sgn = 575
    Ieee_Numeric_Bit_Match_Gt_Sgn_Int = 576
    Ieee_Numeric_Bit_Match_Gt_Int_Sgn = 577
    Ieee_Numeric_Bit_Match_Lt_Uns_Uns = 578
    Ieee_Numeric_Bit_Match_Lt_Uns_Nat = 579
    Ieee_Numeric_Bit_Match_Lt_Nat_Uns = 580
    Ieee_Numeric_Bit_Match_Lt_Sgn_Sgn = 581
    Ieee_Numeric_Bit_Match_Lt_Sgn_Int = 582
    Ieee_Numeric_Bit_Match_Lt_Int_Sgn = 583
    Ieee_Numeric_Bit_Match_Le_Uns_Uns = 584
    Ieee_Numeric_Bit_Match_Le_Uns_Nat = 585
    Ieee_Numeric_Bit_Match_Le_Nat_Uns = 586
    Ieee_Numeric_Bit_Match_Le_Sgn_Sgn = 587
    Ieee_Numeric_Bit_Match_Le_Sgn_Int = 588
    Ieee_Numeric_Bit_Match_Le_Int_Sgn = 589
    Ieee_Numeric_Bit_Match_Ge_Uns_Uns = 590
    Ieee_Numeric_Bit_Match_Ge_Uns_Nat = 591
    Ieee_Numeric_Bit_Match_Ge_Nat_Uns = 592
    Ieee_Numeric_Bit_Match_Ge_Sgn_Sgn = 593
    Ieee_Numeric_Bit_Match_Ge_Sgn_Int = 594
    Ieee_Numeric_Bit_Match_Ge_Int_Sgn = 595
    Ieee_Numeric_Bit_Match_Eq_Uns_Uns = 596
    Ieee_Numeric_Bit_Match_Eq_Uns_Nat = 597
    Ieee_Numeric_Bit_Match_Eq_Nat_Uns = 598
    Ieee_Numeric_Bit_Match_Eq_Sgn_Sgn = 599
    Ieee_Numeric_Bit_Match_Eq_Sgn_Int = 600
    Ieee_Numeric_Bit_Match_Eq_Int_Sgn = 601
    Ieee_Numeric_Bit_Match_Ne_Uns_Uns = 602
    Ieee_Numeric_Bit_Match_Ne_Uns_Nat = 603
    Ieee_Numeric_Bit_Match_Ne_Nat_Uns = 604
    Ieee_Numeric_Bit_Match_Ne_Sgn_Sgn = 605
    Ieee_Numeric_Bit_Match_Ne_Sgn_Int = 606
    Ieee_Numeric_Bit_Match_Ne_Int_Sgn = 607
    Ieee_Numeric_Bit_Shf_Left_Uns_Nat = 608
    Ieee_Numeric_Bit_Shf_Right_Uns_Nat = 609
    Ieee_Numeric_Bit_Shf_Left_Sgn_Nat = 610
    Ieee_Numeric_Bit_Shf_Right_Sgn_Nat = 611
    Ieee_Numeric_Bit_Rot_Left_Uns_Nat = 612
    Ieee_Numeric_Bit_Rot_Right_Uns_Nat = 613
    Ieee_Numeric_Bit_Rot_Left_Sgn_Nat = 614
    Ieee_Numeric_Bit_Rot_Right_Sgn_Nat = 615
    Ieee_Numeric_Bit_Resize_Uns_Nat = 616
    Ieee_Numeric_Bit_Resize_Sgn_Nat = 617
    Ieee_Numeric_Bit_Resize_Uns_Uns = 618
    Ieee_Numeric_Bit_Resize_Sgn_Sgn = 619
    Ieee_Numeric_Bit_Toint_Uns_Nat = 620
    Ieee_Numeric_Bit_Toint_Sgn_Int = 621
    Ieee_Numeric_Bit_Touns_Nat_Nat_Uns = 622
    Ieee_Numeric_Bit_Touns_Nat_Uns_Uns = 623
    Ieee_Numeric_Bit_Tosgn_Int_Nat_Sgn = 624
    Ieee_Numeric_Bit_Tosgn_Int_Sgn_Sgn = 625
    Ieee_Numeric_Bit_And_Uns_Uns = 626
    Ieee_Numeric_Bit_And_Uns_Bit = 627
    Ieee_Numeric_Bit_And_Bit_Uns = 628
    Ieee_Numeric_Bit_And_Sgn_Sgn = 629
    Ieee_Numeric_Bit_And_Sgn_Bit = 630
    Ieee_Numeric_Bit_And_Bit_Sgn = 631
    Ieee_Numeric_Bit_Nand_Uns_Uns = 632
    Ieee_Numeric_Bit_Nand_Uns_Bit = 633
    Ieee_Numeric_Bit_Nand_Bit_Uns = 634
    Ieee_Numeric_Bit_Nand_Sgn_Sgn = 635
    Ieee_Numeric_Bit_Nand_Sgn_Bit = 636
    Ieee_Numeric_Bit_Nand_Bit_Sgn = 637
    Ieee_Numeric_Bit_Or_Uns_Uns = 638
    Ieee_Numeric_Bit_Or_Uns_Bit = 639
    Ieee_Numeric_Bit_Or_Bit_Uns = 640
    Ieee_Numeric_Bit_Or_Sgn_Sgn = 641
    Ieee_Numeric_Bit_Or_Sgn_Bit = 642
    Ieee_Numeric_Bit_Or_Bit_Sgn = 643
    Ieee_Numeric_Bit_Nor_Uns_Uns = 644
    Ieee_Numeric_Bit_Nor_Uns_Bit = 645
    Ieee_Numeric_Bit_Nor_Bit_Uns = 646
    Ieee_Numeric_Bit_Nor_Sgn_Sgn = 647
    Ieee_Numeric_Bit_Nor_Sgn_Bit = 648
    Ieee_Numeric_Bit_Nor_Bit_Sgn = 649
    Ieee_Numeric_Bit_Xor_Uns_Uns = 650
    Ieee_Numeric_Bit_Xor_Uns_Bit = 651
    Ieee_Numeric_Bit_Xor_Bit_Uns = 652
    Ieee_Numeric_Bit_Xor_Sgn_Sgn = 653
    Ieee_Numeric_Bit_Xor_Sgn_Bit = 654
    Ieee_Numeric_Bit_Xor_Bit_Sgn = 655
    Ieee_Numeric_Bit_Xnor_Uns_Uns = 656
    Ieee_Numeric_Bit_Xnor_Uns_Bit = 657
    Ieee_Numeric_Bit_Xnor_Bit_Uns = 658
    Ieee_Numeric_Bit_Xnor_Sgn_Sgn = 659
    Ieee_Numeric_Bit_Xnor_Sgn_Bit = 660
    Ieee_Numeric_Bit_Xnor_Bit_Sgn = 661
    Ieee_Numeric_Bit_Sll_Uns_Int = 662
    Ieee_Numeric_Bit_Sll_Sgn_Int = 663
    Ieee_Numeric_Bit_Srl_Uns_Int = 664
    Ieee_Numeric_Bit_Srl_Sgn_Int = 665
    Ieee_Numeric_Bit_Sla_Uns_Int = 666
    Ieee_Numeric_Bit_Sla_Sgn_Int = 667
    Ieee_Numeric_Bit_Sra_Uns_Int = 668
    Ieee_Numeric_Bit_Sra_Sgn_Int = 669
    Ieee_Numeric_Bit_Rol_Uns_Int = 670
    Ieee_Numeric_Bit_Rol_Sgn_Int = 671
    Ieee_Numeric_Bit_Ror_Uns_Int = 672
    Ieee_Numeric_Bit_Ror_Sgn_Int = 673
    Ieee_Numeric_Bit_Find_Leftmost_Uns = 674
    Ieee_Numeric_Bit_Find_Rightmost_Uns = 675
    Ieee_Numeric_Bit_Find_Leftmost_Sgn = 676
    Ieee_Numeric_Bit_Find_Rightmost_Sgn = 677
    Ieee_Numeric_Bit_Min_Uns_Uns = 678
    Ieee_Numeric_Bit_Min_Uns_Nat = 679
    Ieee_Numeric_Bit_Min_Nat_Uns = 680
    Ieee_Numeric_Bit_Min_Sgn_Sgn = 681
    Ieee_Numeric_Bit_Min_Sgn_Int = 682
    Ieee_Numeric_Bit_Min_Int_Sgn = 683
    Ieee_Numeric_Bit_Max_Uns_Uns = 684
    Ieee_Numeric_Bit_Max_Uns_Nat = 685
    Ieee_Numeric_Bit_Max_Nat_Uns = 686
    Ieee_Numeric_Bit_Max_Sgn_Sgn = 687
    Ieee_Numeric_Bit_Max_Sgn_Int = 688
    Ieee_Numeric_Bit_Max_Int_Sgn = 689
    Ieee_Numeric_Std_Unsigned_Add_Slv_Slv = 690
    Ieee_Numeric_Std_Unsigned_Add_Slv_Nat = 691
    Ieee_Numeric_Std_Unsigned_Add_Nat_Slv = 692
    Ieee_Numeric_Std_Unsigned_Sub_Slv_Slv = 693
    Ieee_Numeric_Std_Unsigned_Sub_Slv_Nat = 694
    Ieee_Numeric_Std_Unsigned_Sub_Nat_Slv = 695
    Ieee_Numeric_Std_Unsigned_Find_Rightmost = 696
    Ieee_Numeric_Std_Unsigned_Find_Leftmost = 697
    Ieee_Numeric_Std_Unsigned_Shift_Left = 698
    Ieee_Numeric_Std_Unsigned_Shift_Right = 699
    Ieee_Numeric_Std_Unsigned_Rotate_Left = 700
    Ieee_Numeric_Std_Unsigned_Rotate_Right = 701
    Ieee_Numeric_Std_Unsigned_To_Integer_Slv_Nat = 702
    Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Nat = 703
    Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Slv = 704
    Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Nat = 705
    Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Suv = 706
    Ieee_Numeric_Std_Unsigned_Resize_Slv_Nat = 707
    Ieee_Numeric_Std_Unsigned_Resize_Slv_Slv = 708
    Ieee_Numeric_Std_Unsigned_Maximum_Slv_Slv = 709
    Ieee_Numeric_Std_Unsigned_Minimum_Slv_Slv = 710
    Ieee_Math_Real_Sign = 711
    Ieee_Math_Real_Ceil = 712
    Ieee_Math_Real_Floor = 713
    Ieee_Math_Real_Round = 714
    Ieee_Math_Real_Trunc = 715
    Ieee_Math_Real_Mod = 716
    Ieee_Math_Real_Realmax = 717
    Ieee_Math_Real_Realmin = 718
    Ieee_Math_Real_Sqrt = 719
    Ieee_Math_Real_Cbrt = 720
    Ieee_Math_Real_Pow_Int_Real = 721
    Ieee_Math_Real_Pow_Real_Real = 722
    Ieee_Math_Real_Exp = 723
    Ieee_Math_Real_Log = 724
    Ieee_Math_Real_Log2 = 725
    Ieee_Math_Real_Log10 = 726
    Ieee_Math_Real_Log_Real_Real = 727
    Ieee_Math_Real_Sin = 728
    Ieee_Math_Real_Cos = 729
    Ieee_Math_Real_Tan = 730
    Ieee_Math_Real_Arcsin = 731
    Ieee_Math_Real_Arccos = 732
    Ieee_Math_Real_Arctan = 733
    Ieee_Math_Real_Arctan_Real_Real = 734
    Ieee_Math_Real_Sinh = 735
    Ieee_Math_Real_Cosh = 736
    Ieee_Math_Real_Tanh = 737
    Ieee_Math_Real_Arcsinh = 738
    Ieee_Math_Real_Arccosh = 739
    Ieee_Math_Real_Arctanh = 740
    Ieee_Std_Logic_Unsigned_Add_Slv_Slv = 741
    Ieee_Std_Logic_Unsigned_Add_Slv_Int = 742
    Ieee_Std_Logic_Unsigned_Add_Int_Slv = 743
    Ieee_Std_Logic_Unsigned_Add_Slv_Log = 744
    Ieee_Std_Logic_Unsigned_Add_Log_Slv = 745
    Ieee_Std_Logic_Unsigned_Sub_Slv_Slv = 746
    Ieee_Std_Logic_Unsigned_Sub_Slv_Int = 747
    Ieee_Std_Logic_Unsigned_Sub_Int_Slv = 748
    Ieee_Std_Logic_Unsigned_Sub_Slv_Log = 749
    Ieee_Std_Logic_Unsigned_Sub_Log_Slv = 750
    Ieee_Std_Logic_Unsigned_Id_Slv = 751
    Ieee_Std_Logic_Unsigned_Mul_Slv_Slv = 752
    Ieee_Std_Logic_Unsigned_Lt_Slv_Slv = 753
    Ieee_Std_Logic_Unsigned_Lt_Slv_Int = 754
    Ieee_Std_Logic_Unsigned_Lt_Int_Slv = 755
    Ieee_Std_Logic_Unsigned_Le_Slv_Slv = 756
    Ieee_Std_Logic_Unsigned_Le_Slv_Int = 757
    Ieee_Std_Logic_Unsigned_Le_Int_Slv = 758
    Ieee_Std_Logic_Unsigned_Gt_Slv_Slv = 759
    Ieee_Std_Logic_Unsigned_Gt_Slv_Int = 760
    Ieee_Std_Logic_Unsigned_Gt_Int_Slv = 761
    Ieee_Std_Logic_Unsigned_Ge_Slv_Slv = 762
    Ieee_Std_Logic_Unsigned_Ge_Slv_Int = 763
    Ieee_Std_Logic_Unsigned_Ge_Int_Slv = 764
    Ieee_Std_Logic_Unsigned_Eq_Slv_Slv = 765
    Ieee_Std_Logic_Unsigned_Eq_Slv_Int = 766
    Ieee_Std_Logic_Unsigned_Eq_Int_Slv = 767
    Ieee_Std_Logic_Unsigned_Ne_Slv_Slv = 768
    Ieee_Std_Logic_Unsigned_Ne_Slv_Int = 769
    Ieee_Std_Logic_Unsigned_Ne_Int_Slv = 770
    Ieee_Std_Logic_Unsigned_Conv_Integer = 771
    Ieee_Std_Logic_Unsigned_Shl = 772
    Ieee_Std_Logic_Unsigned_Shr = 773
    Ieee_Std_Logic_Signed_Add_Slv_Slv = 774
    Ieee_Std_Logic_Signed_Add_Slv_Int = 775
    Ieee_Std_Logic_Signed_Add_Int_Slv = 776
    Ieee_Std_Logic_Signed_Add_Slv_Log = 777
    Ieee_Std_Logic_Signed_Add_Log_Slv = 778
    Ieee_Std_Logic_Signed_Sub_Slv_Slv = 779
    Ieee_Std_Logic_Signed_Sub_Slv_Int = 780
    Ieee_Std_Logic_Signed_Sub_Int_Slv = 781
    Ieee_Std_Logic_Signed_Sub_Slv_Log = 782
    Ieee_Std_Logic_Signed_Sub_Log_Slv = 783
    Ieee_Std_Logic_Signed_Id_Slv = 784
    Ieee_Std_Logic_Signed_Neg_Slv = 785
    Ieee_Std_Logic_Signed_Abs_Slv = 786
    Ieee_Std_Logic_Signed_Mul_Slv_Slv = 787
    Ieee_Std_Logic_Signed_Lt_Slv_Slv = 788
    Ieee_Std_Logic_Signed_Lt_Slv_Int = 789
    Ieee_Std_Logic_Signed_Lt_Int_Slv = 790
    Ieee_Std_Logic_Signed_Le_Slv_Slv = 791
    Ieee_Std_Logic_Signed_Le_Slv_Int = 792
    Ieee_Std_Logic_Signed_Le_Int_Slv = 793
    Ieee_Std_Logic_Signed_Gt_Slv_Slv = 794
    Ieee_Std_Logic_Signed_Gt_Slv_Int = 795
    Ieee_Std_Logic_Signed_Gt_Int_Slv = 796
    Ieee_Std_Logic_Signed_Ge_Slv_Slv = 797
    Ieee_Std_Logic_Signed_Ge_Slv_Int = 798
    Ieee_Std_Logic_Signed_Ge_Int_Slv = 799
    Ieee_Std_Logic_Signed_Eq_Slv_Slv = 800
    Ieee_Std_Logic_Signed_Eq_Slv_Int = 801
    Ieee_Std_Logic_Signed_Eq_Int_Slv = 802
    Ieee_Std_Logic_Signed_Ne_Slv_Slv = 803
    Ieee_Std_Logic_Signed_Ne_Slv_Int = 804
    Ieee_Std_Logic_Signed_Ne_Int_Slv = 805
    Ieee_Std_Logic_Signed_Conv_Integer = 806
    Ieee_Std_Logic_Signed_Shl = 807
    Ieee_Std_Logic_Signed_Shr = 808
    Ieee_Std_Logic_Arith_Conv_Unsigned_Int = 809
    Ieee_Std_Logic_Arith_Conv_Unsigned_Uns = 810
    Ieee_Std_Logic_Arith_Conv_Unsigned_Sgn = 811
    Ieee_Std_Logic_Arith_Conv_Unsigned_Log = 812
    Ieee_Std_Logic_Arith_Conv_Signed_Int = 813
    Ieee_Std_Logic_Arith_Conv_Signed_Uns = 814
    Ieee_Std_Logic_Arith_Conv_Signed_Sgn = 815
    Ieee_Std_Logic_Arith_Conv_Signed_Log = 816
    Ieee_Std_Logic_Arith_Conv_Integer_Int = 817
    Ieee_Std_Logic_Arith_Conv_Integer_Uns = 818
    Ieee_Std_Logic_Arith_Conv_Integer_Sgn = 819
    Ieee_Std_Logic_Arith_Conv_Integer_Log = 820
    Ieee_Std_Logic_Arith_Conv_Vector_Int = 821
    Ieee_Std_Logic_Arith_Conv_Vector_Uns = 822
    Ieee_Std_Logic_Arith_Conv_Vector_Sgn = 823
    Ieee_Std_Logic_Arith_Conv_Vector_Log = 824
    Ieee_Std_Logic_Arith_Ext = 825
    Ieee_Std_Logic_Arith_Sxt = 826
    Ieee_Std_Logic_Arith_Id_Uns_Uns = 827
    Ieee_Std_Logic_Arith_Id_Sgn_Sgn = 828
    Ieee_Std_Logic_Arith_Neg_Sgn_Sgn = 829
    Ieee_Std_Logic_Arith_Abs_Sgn_Sgn = 830
    Ieee_Std_Logic_Arith_Shl_Uns = 831
    Ieee_Std_Logic_Arith_Shl_Sgn = 832
    Ieee_Std_Logic_Arith_Shr_Uns = 833
    Ieee_Std_Logic_Arith_Shr_Sgn = 834
    Ieee_Std_Logic_Arith_Id_Uns_Slv = 835
    Ieee_Std_Logic_Arith_Id_Sgn_Slv = 836
    Ieee_Std_Logic_Arith_Neg_Sgn_Slv = 837
    Ieee_Std_Logic_Arith_Abs_Sgn_Slv = 838
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns = 839
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn = 840
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn = 841
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn = 842
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv = 843
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv = 844
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv = 845
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv = 846
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns = 847
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn = 848
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn = 849
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn = 850
    Ieee_Std_Logic_Arith_Add_Uns_Int_Uns = 851
    Ieee_Std_Logic_Arith_Add_Int_Uns_Uns = 852
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn = 853
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn = 854
    Ieee_Std_Logic_Arith_Add_Uns_Log_Uns = 855
    Ieee_Std_Logic_Arith_Add_Log_Uns_Uns = 856
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn = 857
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn = 858
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv = 859
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv = 860
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv = 861
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv = 862
    Ieee_Std_Logic_Arith_Add_Uns_Int_Slv = 863
    Ieee_Std_Logic_Arith_Add_Int_Uns_Slv = 864
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv = 865
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv = 866
    Ieee_Std_Logic_Arith_Add_Uns_Log_Slv = 867
    Ieee_Std_Logic_Arith_Add_Log_Uns_Slv = 868
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv = 869
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv = 870
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns = 871
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn = 872
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn = 873
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn = 874
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns = 875
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns = 876
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn = 877
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn = 878
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns = 879
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns = 880
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn = 881
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn = 882
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv = 883
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv = 884
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv = 885
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv = 886
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv = 887
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv = 888
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv = 889
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv = 890
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv = 891
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv = 892
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv = 893
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv = 894
    Ieee_Std_Logic_Arith_Lt_Uns_Uns = 895
    Ieee_Std_Logic_Arith_Lt_Sgn_Sgn = 896
    Ieee_Std_Logic_Arith_Lt_Uns_Sgn = 897
    Ieee_Std_Logic_Arith_Lt_Sgn_Uns = 898
    Ieee_Std_Logic_Arith_Lt_Uns_Int = 899
    Ieee_Std_Logic_Arith_Lt_Int_Uns = 900
    Ieee_Std_Logic_Arith_Lt_Sgn_Int = 901
    Ieee_Std_Logic_Arith_Lt_Int_Sgn = 902
    Ieee_Std_Logic_Arith_Le_Uns_Uns = 903
    Ieee_Std_Logic_Arith_Le_Sgn_Sgn = 904
    Ieee_Std_Logic_Arith_Le_Uns_Sgn = 905
    Ieee_Std_Logic_Arith_Le_Sgn_Uns = 906
    Ieee_Std_Logic_Arith_Le_Uns_Int = 907
    Ieee_Std_Logic_Arith_Le_Int_Uns = 908
    Ieee_Std_Logic_Arith_Le_Sgn_Int = 909
    Ieee_Std_Logic_Arith_Le_Int_Sgn = 910
    Ieee_Std_Logic_Arith_Gt_Uns_Uns = 911
    Ieee_Std_Logic_Arith_Gt_Sgn_Sgn = 912
    Ieee_Std_Logic_Arith_Gt_Uns_Sgn = 913
    Ieee_Std_Logic_Arith_Gt_Sgn_Uns = 914
    Ieee_Std_Logic_Arith_Gt_Uns_Int = 915
    Ieee_Std_Logic_Arith_Gt_Int_Uns = 916
    Ieee_Std_Logic_Arith_Gt_Sgn_Int = 917
    Ieee_Std_Logic_Arith_Gt_Int_Sgn = 918
    Ieee_Std_Logic_Arith_Ge_Uns_Uns = 919
    Ieee_Std_Logic_Arith_Ge_Sgn_Sgn = 920
    Ieee_Std_Logic_Arith_Ge_Uns_Sgn = 921
    Ieee_Std_Logic_Arith_Ge_Sgn_Uns = 922
    Ieee_Std_Logic_Arith_Ge_Uns_Int = 923
    Ieee_Std_Logic_Arith_Ge_Int_Uns = 924
    Ieee_Std_Logic_Arith_Ge_Sgn_Int = 925
    Ieee_Std_Logic_Arith_Ge_Int_Sgn = 926
    Ieee_Std_Logic_Arith_Eq_Uns_Uns = 927
    Ieee_Std_Logic_Arith_Eq_Sgn_Sgn = 928
    Ieee_Std_Logic_Arith_Eq_Uns_Sgn = 929
    Ieee_Std_Logic_Arith_Eq_Sgn_Uns = 930
    Ieee_Std_Logic_Arith_Eq_Uns_Int = 931
    Ieee_Std_Logic_Arith_Eq_Int_Uns = 932
    Ieee_Std_Logic_Arith_Eq_Sgn_Int = 933
    Ieee_Std_Logic_Arith_Eq_Int_Sgn = 934
    Ieee_Std_Logic_Arith_Ne_Uns_Uns = 935
    Ieee_Std_Logic_Arith_Ne_Sgn_Sgn = 936
    Ieee_Std_Logic_Arith_Ne_Uns_Sgn = 937
    Ieee_Std_Logic_Arith_Ne_Sgn_Uns = 938
    Ieee_Std_Logic_Arith_Ne_Uns_Int = 939
    Ieee_Std_Logic_Arith_Ne_Int_Uns = 940
    Ieee_Std_Logic_Arith_Ne_Sgn_Int = 941
    Ieee_Std_Logic_Arith_Ne_Int_Sgn = 942
    Ieee_Std_Logic_Misc_And_Reduce_Slv = 943
    Ieee_Std_Logic_Misc_And_Reduce_Suv = 944
    Ieee_Std_Logic_Misc_Nand_Reduce_Slv = 945
    Ieee_Std_Logic_Misc_Nand_Reduce_Suv = 946
    Ieee_Std_Logic_Misc_Or_Reduce_Slv = 947
    Ieee_Std_Logic_Misc_Or_Reduce_Suv = 948
    Ieee_Std_Logic_Misc_Nor_Reduce_Slv = 949
    Ieee_Std_Logic_Misc_Nor_Reduce_Suv = 950
    Ieee_Std_Logic_Misc_Xor_Reduce_Slv = 951
    Ieee_Std_Logic_Misc_Xor_Reduce_Suv = 952
    Ieee_Std_Logic_Misc_Xnor_Reduce_Slv = 953
    Ieee_Std_Logic_Misc_Xnor_Reduce_Suv = 954


@export
@BindToLibGHDL("vhdl__nodes__get_kind")
def Get_Kind(node: Iir) -> IirKind:
    """Get node kind."""
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__get_location")
def Get_Location(node: Iir) -> LocationType:
    """
    Get the source location of a node.

    :param node: The node to read the location of.
    :returns:    The node's location, to be resolved with :mod:`pyGHDL.libghdl.files_map`.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__get_first_design_unit")
def Get_First_Design_Unit(obj: Iir) -> Iir:
    """
    Design units contained in a design file.

    :param obj: The node to read the ``First_Design_Unit`` field of.
    :returns:   The node's ``First_Design_Unit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_first_design_unit")
def Set_First_Design_Unit(obj: Iir, value: Iir) -> None:
    """
    Design units contained in a design file.

    :param obj:   The node to write the ``First_Design_Unit`` field of.
    :param value: The value to write into the ``First_Design_Unit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_last_design_unit")
def Get_Last_Design_Unit(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Last_Design_Unit`` field of.
    :returns:   The node's ``Last_Design_Unit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_last_design_unit")
def Set_Last_Design_Unit(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Last_Design_Unit`` field of.
    :param value: The value to write into the ``Last_Design_Unit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_library_declaration")
def Get_Library_Declaration(obj: Iir) -> Iir:
    """
    Library declaration of a library clause.  This is Forward_Ref as the dependency of the unit on the library is not
    tracked.

    :param obj: The node to read the ``Library_Declaration`` field of.
    :returns:   The node's ``Library_Declaration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_library_declaration")
def Set_Library_Declaration(obj: Iir, value: Iir) -> None:
    """
    Library declaration of a library clause.  This is Forward_Ref as the dependency of the unit on the library is not
    tracked.

    :param obj:   The node to write the ``Library_Declaration`` field of.
    :param value: The value to write into the ``Library_Declaration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_file_checksum")
def Get_File_Checksum(obj: Iir) -> FileChecksumId:
    """
    File time stamp is the system time of the file last modification.

    :param obj: The node to read the ``File_Checksum`` field of.
    :returns:   The node's ``File_Checksum`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_file_checksum")
def Set_File_Checksum(obj: Iir, value: FileChecksumId) -> None:
    """
    File time stamp is the system time of the file last modification.

    :param obj:   The node to write the ``File_Checksum`` field of.
    :param value: The value to write into the ``File_Checksum`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_analysis_time_stamp")
def Get_Analysis_Time_Stamp(obj: Iir) -> TimeStampId:
    """
    Time stamp of the last analysis system time.

    :param obj: The node to read the ``Analysis_Time_Stamp`` field of.
    :returns:   The node's ``Analysis_Time_Stamp`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_analysis_time_stamp")
def Set_Analysis_Time_Stamp(obj: Iir, value: TimeStampId) -> None:
    """
    Time stamp of the last analysis system time.

    :param obj:   The node to write the ``Analysis_Time_Stamp`` field of.
    :param value: The value to write into the ``Analysis_Time_Stamp`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_source")
def Get_Design_File_Source(obj: Iir) -> SourceFileEntry:
    """
    :param obj: The node to read the ``Design_File_Source`` field of.
    :returns:   The node's ``Design_File_Source`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_source")
def Set_Design_File_Source(obj: Iir, value: SourceFileEntry) -> None:
    """
    :param obj:   The node to write the ``Design_File_Source`` field of.
    :param value: The value to write into the ``Design_File_Source`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_library")
def Get_Library(obj: Iir) -> Iir:
    """
    The library which FILE belongs to.

    :param obj: The node to read the ``Library`` field of.
    :returns:   The node's ``Library`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_library")
def Set_Library(obj: Iir, value: Iir) -> None:
    """
    The library which FILE belongs to.

    :param obj:   The node to write the ``Library`` field of.
    :param value: The value to write into the ``Library`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_filename")
def Get_Design_File_Filename(obj: Iir) -> NameId:
    """
    Identifier for the design file file name.

    :param obj: The node to read the ``Design_File_Filename`` field of.
    :returns:   The node's ``Design_File_Filename`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_filename")
def Set_Design_File_Filename(obj: Iir, value: NameId) -> None:
    """
    Identifier for the design file file name.

    :param obj:   The node to write the ``Design_File_Filename`` field of.
    :param value: The value to write into the ``Design_File_Filename`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_directory")
def Get_Design_File_Directory(obj: Iir) -> NameId:
    """
    Directory of a design file.

    :param obj: The node to read the ``Design_File_Directory`` field of.
    :returns:   The node's ``Design_File_Directory`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_directory")
def Set_Design_File_Directory(obj: Iir, value: NameId) -> None:
    """
    Directory of a design file.

    :param obj:   The node to write the ``Design_File_Directory`` field of.
    :param value: The value to write into the ``Design_File_Directory`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_file")
def Get_Design_File(obj: Iir) -> Iir:
    """
    The parent of a design unit is a design file.

    :param obj: The node to read the ``Design_File`` field of.
    :returns:   The node's ``Design_File`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_file")
def Set_Design_File(obj: Iir, value: Iir) -> None:
    """
    The parent of a design unit is a design file.

    :param obj:   The node to write the ``Design_File`` field of.
    :param value: The value to write into the ``Design_File`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_chain")
def Get_Design_File_Chain(obj: Iir) -> Iir:
    """
    Design files of a library.

    :param obj: The node to read the ``Design_File_Chain`` field of.
    :returns:   The node's ``Design_File_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_chain")
def Set_Design_File_Chain(obj: Iir, value: Iir) -> None:
    """
    Design files of a library.

    :param obj:   The node to write the ``Design_File_Chain`` field of.
    :param value: The value to write into the ``Design_File_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_library_directory")
def Get_Library_Directory(obj: Iir) -> NameId:
    """
    System directory where the library is stored.

    :param obj: The node to read the ``Library_Directory`` field of.
    :returns:   The node's ``Library_Directory`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_library_directory")
def Set_Library_Directory(obj: Iir, value: NameId) -> None:
    """
    System directory where the library is stored.

    :param obj:   The node to write the ``Library_Directory`` field of.
    :param value: The value to write into the ``Library_Directory`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_date")
def Get_Date(obj: Iir) -> DateType:
    """
    Symbolic date, used to order design units in a library.

    :param obj: The node to read the ``Date`` field of.
    :returns:   The node's ``Date`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_date")
def Set_Date(obj: Iir, value: DateType) -> None:
    """
    Symbolic date, used to order design units in a library.

    :param obj:   The node to write the ``Date`` field of.
    :param value: The value to write into the ``Date`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_context_items")
def Get_Context_Items(obj: Iir) -> Iir:
    """
    Chain of context clauses.

    :param obj: The node to read the ``Context_Items`` field of.
    :returns:   The node's ``Context_Items`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_context_items")
def Set_Context_Items(obj: Iir, value: Iir) -> None:
    """
    Chain of context clauses.

    :param obj:   The node to write the ``Context_Items`` field of.
    :param value: The value to write into the ``Context_Items`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_dependence_list")
def Get_Dependence_List(obj: Iir) -> Iir:
    """
    List of design units on which the design unit depends. There is an exception: the architecture of an entity aspect
    (of a component instantiation) may not have been analyzed.  The Entity_Aspect_Entity is added to this list (instead
    of the non-existing design unit).

    :param obj: The node to read the ``Dependence_List`` field of.
    :returns:   The node's ``Dependence_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_dependence_list")
def Set_Dependence_List(obj: Iir, value: Iir) -> None:
    """
    List of design units on which the design unit depends. There is an exception: the architecture of an entity aspect
    (of a component instantiation) may not have been analyzed.  The Entity_Aspect_Entity is added to this list (instead
    of the non-existing design unit).

    :param obj:   The node to write the ``Dependence_List`` field of.
    :param value: The value to write into the ``Dependence_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_analysis_checks_list")
def Get_Analysis_Checks_List(obj: Iir) -> Iir:
    """
    List of functions or sensitized processes whose analysis checks are not complete. These elements have direct or
    indirect calls to procedure whose body is not yet analyzed.  Therefore, purity or wait checks are not complete.

    :param obj: The node to read the ``Analysis_Checks_List`` field of.
    :returns:   The node's ``Analysis_Checks_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_analysis_checks_list")
def Set_Analysis_Checks_List(obj: Iir, value: Iir) -> None:
    """
    List of functions or sensitized processes whose analysis checks are not complete. These elements have direct or
    indirect calls to procedure whose body is not yet analyzed.  Therefore, purity or wait checks are not complete.

    :param obj:   The node to write the ``Analysis_Checks_List`` field of.
    :param value: The value to write into the ``Analysis_Checks_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_date_state")
def Get_Date_State(obj: Iir) -> DateStateType:
    """
    Whether the unit is on disk, parsed or analyzed.

    :param obj: The node to read the ``Date_State`` field of.
    :returns:   The node's ``Date_State`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_date_state")
def Set_Date_State(obj: Iir, value: DateStateType) -> None:
    """
    Whether the unit is on disk, parsed or analyzed.

    :param obj:   The node to write the ``Date_State`` field of.
    :param value: The value to write into the ``Date_State`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_guarded_target_state")
def Get_Guarded_Target_State(obj: Iir) -> TriStateType:
    """
    If TRUE, the target of the signal assignment is guarded. If FALSE, the target is not guarded. This is determined
    during sem by examining the declaration(s) of the target (there may be several declarations in the case of a
    aggregate target). If UNKNOWN, this is not determined at compile time but at run-time. This is the case for formal
    signal interfaces of subprograms.

    :param obj: The node to read the ``Guarded_Target_State`` field of.
    :returns:   The node's ``Guarded_Target_State`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_guarded_target_state")
def Set_Guarded_Target_State(obj: Iir, value: TriStateType) -> None:
    """
    If TRUE, the target of the signal assignment is guarded. If FALSE, the target is not guarded. This is determined
    during sem by examining the declaration(s) of the target (there may be several declarations in the case of a
    aggregate target). If UNKNOWN, this is not determined at compile time but at run-time. This is the case for formal
    signal interfaces of subprograms.

    :param obj:   The node to write the ``Guarded_Target_State`` field of.
    :param value: The value to write into the ``Guarded_Target_State`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_library_unit")
def Get_Library_Unit(obj: Iir) -> Iir:
    """
    Library unit of a design unit.

    :param obj: The node to read the ``Library_Unit`` field of.
    :returns:   The node's ``Library_Unit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_library_unit")
def Set_Library_Unit(obj: Iir, value: Iir) -> None:
    """
    Library unit of a design unit.

    :param obj:   The node to write the ``Library_Unit`` field of.
    :param value: The value to write into the ``Library_Unit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_hash_chain")
def Get_Hash_Chain(obj: Iir) -> Iir:
    """
    Every design unit is put in an hash table to find quickly found by its name.  This field is a single chain for
    collisions.

    :param obj: The node to read the ``Hash_Chain`` field of.
    :returns:   The node's ``Hash_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_hash_chain")
def Set_Hash_Chain(obj: Iir, value: Iir) -> None:
    """
    Every design unit is put in an hash table to find quickly found by its name.  This field is a single chain for
    collisions.

    :param obj:   The node to write the ``Hash_Chain`` field of.
    :param value: The value to write into the ``Hash_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_pos")
def Get_Design_Unit_Source_Pos(obj: Iir) -> SourcePtr:
    """
    Set the line and the offset in the line, only for the library manager. This is valid until the file is really loaded
    in memory.  On loading, location will contain all this information.

    :param obj: The node to read the ``Design_Unit_Source_Pos`` field of.
    :returns:   The node's ``Design_Unit_Source_Pos`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_pos")
def Set_Design_Unit_Source_Pos(obj: Iir, value: SourcePtr) -> None:
    """
    Set the line and the offset in the line, only for the library manager. This is valid until the file is really loaded
    in memory.  On loading, location will contain all this information.

    :param obj:   The node to write the ``Design_Unit_Source_Pos`` field of.
    :param value: The value to write into the ``Design_Unit_Source_Pos`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_line")
def Get_Design_Unit_Source_Line(obj: Iir) -> Int32:
    """
    :param obj: The node to read the ``Design_Unit_Source_Line`` field of.
    :returns:   The node's ``Design_Unit_Source_Line`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_line")
def Set_Design_Unit_Source_Line(obj: Iir, value: Int32) -> None:
    """
    :param obj:   The node to write the ``Design_Unit_Source_Line`` field of.
    :param value: The value to write into the ``Design_Unit_Source_Line`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_col")
def Get_Design_Unit_Source_Col(obj: Iir) -> Int32:
    """
    :param obj: The node to read the ``Design_Unit_Source_Col`` field of.
    :returns:   The node's ``Design_Unit_Source_Col`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_col")
def Set_Design_Unit_Source_Col(obj: Iir, value: Int32) -> None:
    """
    :param obj:   The node to write the ``Design_Unit_Source_Col`` field of.
    :param value: The value to write into the ``Design_Unit_Source_Col`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_value")
def Get_Value(obj: Iir) -> Int64:
    """
    Value of an integer/physical literal.

    :param obj: The node to read the ``Value`` field of.
    :returns:   The node's ``Value`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_value")
def Set_Value(obj: Iir, value: Int64) -> None:
    """
    Value of an integer/physical literal.

    :param obj:   The node to write the ``Value`` field of.
    :param value: The value to write into the ``Value`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_enum_pos")
def Get_Enum_Pos(obj: Iir) -> Iir:
    """
    Position (same as lit_type'pos) of an enumeration literal.

    :param obj: The node to read the ``Enum_Pos`` field of.
    :returns:   The node's ``Enum_Pos`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_enum_pos")
def Set_Enum_Pos(obj: Iir, value: Iir) -> None:
    """
    Position (same as lit_type'pos) of an enumeration literal.

    :param obj:   The node to write the ``Enum_Pos`` field of.
    :param value: The value to write into the ``Enum_Pos`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_physical_literal")
def Get_Physical_Literal(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Physical_Literal`` field of.
    :returns:   The node's ``Physical_Literal`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_physical_literal")
def Set_Physical_Literal(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Physical_Literal`` field of.
    :param value: The value to write into the ``Physical_Literal`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_fp_value")
def Get_Fp_Value(obj: Iir) -> Fp64:
    """
    Value of a floating point literal.

    :param obj: The node to read the ``Fp_Value`` field of.
    :returns:   The node's ``Fp_Value`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_fp_value")
def Set_Fp_Value(obj: Iir, value: Fp64) -> None:
    """
    Value of a floating point literal.

    :param obj:   The node to write the ``Fp_Value`` field of.
    :param value: The value to write into the ``Fp_Value`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_simple_aggregate_list")
def Get_Simple_Aggregate_List(obj: Iir) -> Iir:
    """
    List of elements of a simple aggregate.

    :param obj: The node to read the ``Simple_Aggregate_List`` field of.
    :returns:   The node's ``Simple_Aggregate_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_simple_aggregate_list")
def Set_Simple_Aggregate_List(obj: Iir, value: Iir) -> None:
    """
    List of elements of a simple aggregate.

    :param obj:   The node to write the ``Simple_Aggregate_List`` field of.
    :param value: The value to write into the ``Simple_Aggregate_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_string8_id")
def Get_String8_Id(obj: Iir) -> String8Id:
    """
    For a string literal: the string identifier.

    :param obj: The node to read the ``String8_Id`` field of.
    :returns:   The node's ``String8_Id`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_string8_id")
def Set_String8_Id(obj: Iir, value: String8Id) -> None:
    """
    For a string literal: the string identifier.

    :param obj:   The node to write the ``String8_Id`` field of.
    :param value: The value to write into the ``String8_Id`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_string_length")
def Get_String_Length(obj: Iir) -> Int32:
    """
    For a string literal: the string length.

    :param obj: The node to read the ``String_Length`` field of.
    :returns:   The node's ``String_Length`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_string_length")
def Set_String_Length(obj: Iir, value: Int32) -> None:
    """
    For a string literal: the string length.

    :param obj:   The node to write the ``String_Length`` field of.
    :param value: The value to write into the ``String_Length`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_bit_string_base")
def Get_Bit_String_Base(obj: Iir) -> NumberBaseType:
    """
    Base of a bit string.  Base_None for a string literal.

    :param obj: The node to read the ``Bit_String_Base`` field of.
    :returns:   The node's ``Bit_String_Base`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_bit_string_base")
def Set_Bit_String_Base(obj: Iir, value: NumberBaseType) -> None:
    """
    Base of a bit string.  Base_None for a string literal.

    :param obj:   The node to write the ``Bit_String_Base`` field of.
    :param value: The value to write into the ``Bit_String_Base`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_signed")
def Get_Has_Signed(obj: Iir) -> Boolean:
    """
    Bit string is signed.

    :param obj: The node to read the ``Has_Signed`` field of.
    :returns:   The node's ``Has_Signed`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_signed")
def Set_Has_Signed(obj: Iir, value: Boolean) -> None:
    """
    Bit string is signed.

    :param obj:   The node to write the ``Has_Signed`` field of.
    :param value: The value to write into the ``Has_Signed`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_sign")
def Get_Has_Sign(obj: Iir) -> Boolean:
    """
    Bit string sign is explicit

    :param obj: The node to read the ``Has_Sign`` field of.
    :returns:   The node's ``Has_Sign`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_sign")
def Set_Has_Sign(obj: Iir, value: Boolean) -> None:
    """
    Bit string sign is explicit

    :param obj:   The node to write the ``Has_Sign`` field of.
    :param value: The value to write into the ``Has_Sign`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_length")
def Get_Has_Length(obj: Iir) -> Boolean:
    """
    Bit string length is explicit

    :param obj: The node to read the ``Has_Length`` field of.
    :returns:   The node's ``Has_Length`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_length")
def Set_Has_Length(obj: Iir, value: Boolean) -> None:
    """
    Bit string length is explicit

    :param obj:   The node to write the ``Has_Length`` field of.
    :param value: The value to write into the ``Has_Length`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_literal_length")
def Get_Literal_Length(obj: Iir) -> Int32:
    """
    Length of the literal in characters.  Used for pretty print.  Set to 0 when doesn't come from the sources.

    :param obj: The node to read the ``Literal_Length`` field of.
    :returns:   The node's ``Literal_Length`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_literal_length")
def Set_Literal_Length(obj: Iir, value: Int32) -> None:
    """
    Length of the literal in characters.  Used for pretty print.  Set to 0 when doesn't come from the sources.

    :param obj:   The node to write the ``Literal_Length`` field of.
    :param value: The value to write into the ``Literal_Length`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_literal_origin")
def Get_Literal_Origin(obj: Iir) -> Iir:
    """
    The origin of a literal can be null_iir for a literal generated by the parser, or a node which was statically
    evaluated to this literal. Such nodes are created by eval_expr.

    :param obj: The node to read the ``Literal_Origin`` field of.
    :returns:   The node's ``Literal_Origin`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_literal_origin")
def Set_Literal_Origin(obj: Iir, value: Iir) -> None:
    """
    The origin of a literal can be null_iir for a literal generated by the parser, or a node which was statically
    evaluated to this literal. Such nodes are created by eval_expr.

    :param obj:   The node to write the ``Literal_Origin`` field of.
    :param value: The value to write into the ``Literal_Origin`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_range_origin")
def Get_Range_Origin(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Range_Origin`` field of.
    :returns:   The node's ``Range_Origin`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_range_origin")
def Set_Range_Origin(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Range_Origin`` field of.
    :param value: The value to write into the ``Range_Origin`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_literal_subtype")
def Get_Literal_Subtype(obj: Iir) -> Iir:
    """
    Same as Type, but not marked as Ref.  This is when a literal has a subtype (such as string or bit_string) created
    specially for the literal.

    :param obj: The node to read the ``Literal_Subtype`` field of.
    :returns:   The node's ``Literal_Subtype`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_literal_subtype")
def Set_Literal_Subtype(obj: Iir, value: Iir) -> None:
    """
    Same as Type, but not marked as Ref.  This is when a literal has a subtype (such as string or bit_string) created
    specially for the literal.

    :param obj:   The node to write the ``Literal_Subtype`` field of.
    :param value: The value to write into the ``Literal_Subtype`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_allocator_subtype")
def Get_Allocator_Subtype(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Allocator_Subtype`` field of.
    :returns:   The node's ``Allocator_Subtype`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_allocator_subtype")
def Set_Allocator_Subtype(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Allocator_Subtype`` field of.
    :param value: The value to write into the ``Allocator_Subtype`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_entity_class")
def Get_Entity_Class(obj: Iir) -> Tok:
    """
    :param obj: The node to read the ``Entity_Class`` field of.
    :returns:   The node's ``Entity_Class`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_entity_class")
def Set_Entity_Class(obj: Iir, value: Tok) -> None:
    """
    :param obj:   The node to write the ``Entity_Class`` field of.
    :param value: The value to write into the ``Entity_Class`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_entity_name_list")
def Get_Entity_Name_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Entity_Name_List`` field of.
    :returns:   The node's ``Entity_Name_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_entity_name_list")
def Set_Entity_Name_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Entity_Name_List`` field of.
    :param value: The value to write into the ``Entity_Name_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_designator")
def Get_Attribute_Designator(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Attribute_Designator`` field of.
    :returns:   The node's ``Attribute_Designator`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_designator")
def Set_Attribute_Designator(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Attribute_Designator`` field of.
    :param value: The value to write into the ``Attribute_Designator`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_specification_chain")
def Get_Attribute_Specification_Chain(obj: Iir) -> Iir:
    """
    Chain of attribute specifications.  This is used only during sem, to check that no named entity of a given class
    appear after an attr. spec. with the entity name list OTHERS or ALL.

    :param obj: The node to read the ``Attribute_Specification_Chain`` field of.
    :returns:   The node's ``Attribute_Specification_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_specification_chain")
def Set_Attribute_Specification_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of attribute specifications.  This is used only during sem, to check that no named entity of a given class
    appear after an attr. spec. with the entity name list OTHERS or ALL.

    :param obj:   The node to write the ``Attribute_Specification_Chain`` field of.
    :param value: The value to write into the ``Attribute_Specification_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_specification")
def Get_Attribute_Specification(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Attribute_Specification`` field of.
    :returns:   The node's ``Attribute_Specification`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_specification")
def Set_Attribute_Specification(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Attribute_Specification`` field of.
    :param value: The value to write into the ``Attribute_Specification`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_static_attribute_flag")
def Get_Static_Attribute_Flag(obj: Iir) -> Boolean:
    """
    True for attributes on entity, configuration and architecture.  They are expected to be read from anywhere so the
    value is expected to be locally static, but this is not followed by many users and implementations.

    :param obj: The node to read the ``Static_Attribute_Flag`` field of.
    :returns:   The node's ``Static_Attribute_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_static_attribute_flag")
def Set_Static_Attribute_Flag(obj: Iir, value: Boolean) -> None:
    """
    True for attributes on entity, configuration and architecture.  They are expected to be read from anywhere so the
    value is expected to be locally static, but this is not followed by many users and implementations.

    :param obj:   The node to write the ``Static_Attribute_Flag`` field of.
    :param value: The value to write into the ``Static_Attribute_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_signal_list")
def Get_Signal_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Signal_List`` field of.
    :returns:   The node's ``Signal_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_signal_list")
def Set_Signal_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Signal_List`` field of.
    :param value: The value to write into the ``Signal_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_quantity_list")
def Get_Quantity_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Quantity_List`` field of.
    :returns:   The node's ``Quantity_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_quantity_list")
def Set_Quantity_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Quantity_List`` field of.
    :param value: The value to write into the ``Quantity_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_designated_entity")
def Get_Designated_Entity(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Designated_Entity`` field of.
    :returns:   The node's ``Designated_Entity`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_designated_entity")
def Set_Designated_Entity(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Designated_Entity`` field of.
    :param value: The value to write into the ``Designated_Entity`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_formal")
def Get_Formal(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Formal`` field of.
    :returns:   The node's ``Formal`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_formal")
def Set_Formal(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Formal`` field of.
    :param value: The value to write into the ``Formal`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_actual")
def Get_Actual(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Actual`` field of.
    :returns:   The node's ``Actual`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_actual")
def Set_Actual(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Actual`` field of.
    :param value: The value to write into the ``Actual`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_open_actual")
def Get_Open_Actual(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Open_Actual`` field of.
    :returns:   The node's ``Open_Actual`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_open_actual")
def Set_Open_Actual(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Open_Actual`` field of.
    :param value: The value to write into the ``Open_Actual`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_actual_conversion")
def Get_Actual_Conversion(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Actual_Conversion`` field of.
    :returns:   The node's ``Actual_Conversion`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_actual_conversion")
def Set_Actual_Conversion(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Actual_Conversion`` field of.
    :param value: The value to write into the ``Actual_Conversion`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_formal_conversion")
def Get_Formal_Conversion(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Formal_Conversion`` field of.
    :returns:   The node's ``Formal_Conversion`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_formal_conversion")
def Set_Formal_Conversion(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Formal_Conversion`` field of.
    :param value: The value to write into the ``Formal_Conversion`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_whole_association_flag")
def Get_Whole_Association_Flag(obj: Iir) -> Boolean:
    """
    This flag is set when the formal is associated in whole (ie, not individually).

    :param obj: The node to read the ``Whole_Association_Flag`` field of.
    :returns:   The node's ``Whole_Association_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_whole_association_flag")
def Set_Whole_Association_Flag(obj: Iir, value: Boolean) -> None:
    """
    This flag is set when the formal is associated in whole (ie, not individually).

    :param obj:   The node to write the ``Whole_Association_Flag`` field of.
    :param value: The value to write into the ``Whole_Association_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_collapse_signal_flag")
def Get_Collapse_Signal_Flag(obj: Iir) -> Boolean:
    """
    This flag is set when the formal signal can be the actual signal.  In this case, the formal signal is not created,
    and the actual is shared. This is the signal collapsing optimisation.

    :param obj: The node to read the ``Collapse_Signal_Flag`` field of.
    :returns:   The node's ``Collapse_Signal_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_collapse_signal_flag")
def Set_Collapse_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """
    This flag is set when the formal signal can be the actual signal.  In this case, the formal signal is not created,
    and the actual is shared. This is the signal collapsing optimisation.

    :param obj:   The node to write the ``Collapse_Signal_Flag`` field of.
    :param value: The value to write into the ``Collapse_Signal_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_artificial_flag")
def Get_Artificial_Flag(obj: Iir) -> Boolean:
    """
    Set when the node was artificially created, eg by canon. Currently used only by association_element_open.

    :param obj: The node to read the ``Artificial_Flag`` field of.
    :returns:   The node's ``Artificial_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_artificial_flag")
def Set_Artificial_Flag(obj: Iir, value: Boolean) -> None:
    """
    Set when the node was artificially created, eg by canon. Currently used only by association_element_open.

    :param obj:   The node to write the ``Artificial_Flag`` field of.
    :param value: The value to write into the ``Artificial_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_open_flag")
def Get_Open_Flag(obj: Iir) -> Boolean:
    """
    This flag is set for a very short time during the check that no in port is unconnected.

    :param obj: The node to read the ``Open_Flag`` field of.
    :returns:   The node's ``Open_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_open_flag")
def Set_Open_Flag(obj: Iir, value: Boolean) -> None:
    """
    This flag is set for a very short time during the check that no in port is unconnected.

    :param obj:   The node to write the ``Open_Flag`` field of.
    :param value: The value to write into the ``Open_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_after_drivers_flag")
def Get_After_Drivers_Flag(obj: Iir) -> Boolean:
    """
    This flag is set by trans_analyze if there is a projected waveform assignment in the process.

    :param obj: The node to read the ``After_Drivers_Flag`` field of.
    :returns:   The node's ``After_Drivers_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_after_drivers_flag")
def Set_After_Drivers_Flag(obj: Iir, value: Boolean) -> None:
    """
    This flag is set by trans_analyze if there is a projected waveform assignment in the process.

    :param obj:   The node to write the ``After_Drivers_Flag`` field of.
    :param value: The value to write into the ``After_Drivers_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_we_value")
def Get_We_Value(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``We_Value`` field of.
    :returns:   The node's ``We_Value`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_we_value")
def Set_We_Value(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``We_Value`` field of.
    :param value: The value to write into the ``We_Value`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_time")
def Get_Time(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Time`` field of.
    :returns:   The node's ``Time`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_time")
def Set_Time(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Time`` field of.
    :param value: The value to write into the ``Time`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_associated_expr")
def Get_Associated_Expr(obj: Iir) -> Iir:
    """
    Node associated with a choice.

    :param obj: The node to read the ``Associated_Expr`` field of.
    :returns:   The node's ``Associated_Expr`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_associated_expr")
def Set_Associated_Expr(obj: Iir, value: Iir) -> None:
    """
    Node associated with a choice.

    :param obj:   The node to write the ``Associated_Expr`` field of.
    :param value: The value to write into the ``Associated_Expr`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_associated_block")
def Get_Associated_Block(obj: Iir) -> Iir:
    """
    Node associated with a choice.

    :param obj: The node to read the ``Associated_Block`` field of.
    :returns:   The node's ``Associated_Block`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_associated_block")
def Set_Associated_Block(obj: Iir, value: Iir) -> None:
    """
    Node associated with a choice.

    :param obj:   The node to write the ``Associated_Block`` field of.
    :param value: The value to write into the ``Associated_Block`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_associated_chain")
def Get_Associated_Chain(obj: Iir) -> Iir:
    """
    Chain associated with a choice.

    :param obj: The node to read the ``Associated_Chain`` field of.
    :returns:   The node's ``Associated_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_associated_chain")
def Set_Associated_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain associated with a choice.

    :param obj:   The node to write the ``Associated_Chain`` field of.
    :param value: The value to write into the ``Associated_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_choice_name")
def Get_Choice_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Choice_Name`` field of.
    :returns:   The node's ``Choice_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_choice_name")
def Set_Choice_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Choice_Name`` field of.
    :param value: The value to write into the ``Choice_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_choice_expression")
def Get_Choice_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Choice_Expression`` field of.
    :returns:   The node's ``Choice_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_choice_expression")
def Set_Choice_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Choice_Expression`` field of.
    :param value: The value to write into the ``Choice_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_choice_range")
def Get_Choice_Range(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Choice_Range`` field of.
    :returns:   The node's ``Choice_Range`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_choice_range")
def Set_Choice_Range(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Choice_Range`` field of.
    :param value: The value to write into the ``Choice_Range`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_same_alternative_flag")
def Get_Same_Alternative_Flag(obj: Iir) -> Boolean:
    """
    Set when a choice belongs to the same alternative as the previous one.

    :param obj: The node to read the ``Same_Alternative_Flag`` field of.
    :returns:   The node's ``Same_Alternative_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_same_alternative_flag")
def Set_Same_Alternative_Flag(obj: Iir, value: Boolean) -> None:
    """
    Set when a choice belongs to the same alternative as the previous one.

    :param obj:   The node to write the ``Same_Alternative_Flag`` field of.
    :param value: The value to write into the ``Same_Alternative_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_element_type_flag")
def Get_Element_Type_Flag(obj: Iir) -> Boolean:
    """
    For one-dimensional aggregates: the value associated of the type of the element (vs of the type of the aggregate).
    Always true before vhdl-08.

    :param obj: The node to read the ``Element_Type_Flag`` field of.
    :returns:   The node's ``Element_Type_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_element_type_flag")
def Set_Element_Type_Flag(obj: Iir, value: Boolean) -> None:
    """
    For one-dimensional aggregates: the value associated of the type of the element (vs of the type of the aggregate).
    Always true before vhdl-08.

    :param obj:   The node to write the ``Element_Type_Flag`` field of.
    :param value: The value to write into the ``Element_Type_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_architecture")
def Get_Architecture(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Architecture`` field of.
    :returns:   The node's ``Architecture`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_architecture")
def Set_Architecture(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Architecture`` field of.
    :param value: The value to write into the ``Architecture`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_block_specification")
def Get_Block_Specification(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Block_Specification`` field of.
    :returns:   The node's ``Block_Specification`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_block_specification")
def Set_Block_Specification(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Block_Specification`` field of.
    :param value: The value to write into the ``Block_Specification`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_prev_block_configuration")
def Get_Prev_Block_Configuration(obj: Iir) -> Iir:
    """
    Return the link of the previous block_configuration of a block_configuration. This single linked list is used to
    list all the block_configuration that configuration the same block (which can only be an iterative generate
    statement). All elements of this list must belong to the same block configuration. The order is not important.

    :param obj: The node to read the ``Prev_Block_Configuration`` field of.
    :returns:   The node's ``Prev_Block_Configuration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_prev_block_configuration")
def Set_Prev_Block_Configuration(obj: Iir, value: Iir) -> None:
    """
    Return the link of the previous block_configuration of a block_configuration. This single linked list is used to
    list all the block_configuration that configuration the same block (which can only be an iterative generate
    statement). All elements of this list must belong to the same block configuration. The order is not important.

    :param obj:   The node to write the ``Prev_Block_Configuration`` field of.
    :param value: The value to write into the ``Prev_Block_Configuration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_item_chain")
def Get_Configuration_Item_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Configuration_Item_Chain`` field of.
    :returns:   The node's ``Configuration_Item_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_item_chain")
def Set_Configuration_Item_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Configuration_Item_Chain`` field of.
    :param value: The value to write into the ``Configuration_Item_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_value_chain")
def Get_Attribute_Value_Chain(obj: Iir) -> Iir:
    """
    Chain of attribute values for declared items. To be used with Get/Set_Value_Chain. There is no order, therefore, a
    new attribute value may be always prepended.

    :param obj: The node to read the ``Attribute_Value_Chain`` field of.
    :returns:   The node's ``Attribute_Value_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_value_chain")
def Set_Attribute_Value_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of attribute values for declared items. To be used with Get/Set_Value_Chain. There is no order, therefore, a
    new attribute value may be always prepended.

    :param obj:   The node to write the ``Attribute_Value_Chain`` field of.
    :param value: The value to write into the ``Attribute_Value_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_spec_chain")
def Get_Spec_Chain(obj: Iir) -> Iir:
    """
    Next attribute value in the attribute specification chain (of attribute value). FIXME: should be a Chain.

    :param obj: The node to read the ``Spec_Chain`` field of.
    :returns:   The node's ``Spec_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_spec_chain")
def Set_Spec_Chain(obj: Iir, value: Iir) -> None:
    """
    Next attribute value in the attribute specification chain (of attribute value). FIXME: should be a Chain.

    :param obj:   The node to write the ``Spec_Chain`` field of.
    :param value: The value to write into the ``Spec_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_value_chain")
def Get_Value_Chain(obj: Iir) -> Iir:
    """
    Next attribute value in the parent chain (of attribute value).

    :param obj: The node to read the ``Value_Chain`` field of.
    :returns:   The node's ``Value_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_value_chain")
def Set_Value_Chain(obj: Iir, value: Iir) -> None:
    """
    Next attribute value in the parent chain (of attribute value).

    :param obj:   The node to write the ``Value_Chain`` field of.
    :param value: The value to write into the ``Value_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_value_spec_chain")
def Get_Attribute_Value_Spec_Chain(obj: Iir) -> Iir:
    """
    Chain of attribute values for attribute specification. To be used with Get/Set_Spec_Chain.

    :param obj: The node to read the ``Attribute_Value_Spec_Chain`` field of.
    :returns:   The node's ``Attribute_Value_Spec_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_value_spec_chain")
def Set_Attribute_Value_Spec_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of attribute values for attribute specification. To be used with Get/Set_Spec_Chain.

    :param obj:   The node to write the ``Attribute_Value_Spec_Chain`` field of.
    :param value: The value to write into the ``Attribute_Value_Spec_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_entity_name")
def Get_Entity_Name(obj: Iir) -> Iir:
    """
    The entity name for an architecture or a configuration.

    :param obj: The node to read the ``Entity_Name`` field of.
    :returns:   The node's ``Entity_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_entity_name")
def Set_Entity_Name(obj: Iir, value: Iir) -> None:
    """
    The entity name for an architecture or a configuration.

    :param obj:   The node to write the ``Entity_Name`` field of.
    :param value: The value to write into the ``Entity_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_package")
def Get_Package(obj: Iir) -> Iir:
    """
    The package declaration corresponding to the body.

    :param obj: The node to read the ``Package`` field of.
    :returns:   The node's ``Package`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_package")
def Set_Package(obj: Iir, value: Iir) -> None:
    """
    The package declaration corresponding to the body.

    :param obj:   The node to write the ``Package`` field of.
    :param value: The value to write into the ``Package`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_package_body")
def Get_Package_Body(obj: Iir) -> Iir:
    """
    The package body corresponding to the package declaration.

    :param obj: The node to read the ``Package_Body`` field of.
    :returns:   The node's ``Package_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_package_body")
def Set_Package_Body(obj: Iir, value: Iir) -> None:
    """
    The package body corresponding to the package declaration.

    :param obj:   The node to write the ``Package_Body`` field of.
    :param value: The value to write into the ``Package_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_instance_package_body")
def Get_Instance_Package_Body(obj: Iir) -> Iir:
    """
    The package body corresponding to the package declaration.

    :param obj: The node to read the ``Instance_Package_Body`` field of.
    :returns:   The node's ``Instance_Package_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_instance_package_body")
def Set_Instance_Package_Body(obj: Iir, value: Iir) -> None:
    """
    The package body corresponding to the package declaration.

    :param obj:   The node to write the ``Instance_Package_Body`` field of.
    :param value: The value to write into the ``Instance_Package_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_owned_instance_package_body")
def Get_Owned_Instance_Package_Body(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Owned_Instance_Package_Body`` field of.
    :returns:   The node's ``Owned_Instance_Package_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_owned_instance_package_body")
def Set_Owned_Instance_Package_Body(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Owned_Instance_Package_Body`` field of.
    :param value: The value to write into the ``Owned_Instance_Package_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_instance_subprogram_body")
def Get_Instance_Subprogram_Body(obj: Iir) -> Iir:
    """
    The subprogram body corresponding to the subprogram declaration.

    :param obj: The node to read the ``Instance_Subprogram_Body`` field of.
    :returns:   The node's ``Instance_Subprogram_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_instance_subprogram_body")
def Set_Instance_Subprogram_Body(obj: Iir, value: Iir) -> None:
    """
    The subprogram body corresponding to the subprogram declaration.

    :param obj:   The node to write the ``Instance_Subprogram_Body`` field of.
    :param value: The value to write into the ``Instance_Subprogram_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_need_body")
def Get_Need_Body(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Need_Body`` field of.
    :returns:   The node's ``Need_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_need_body")
def Set_Need_Body(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Need_Body`` field of.
    :param value: The value to write into the ``Need_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_immediate_body_flag")
def Get_Immediate_Body_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Immediate_Body_Flag`` field of.
    :returns:   The node's ``Immediate_Body_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_immediate_body_flag")
def Set_Immediate_Body_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Immediate_Body_Flag`` field of.
    :param value: The value to write into the ``Immediate_Body_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_macro_expand_flag")
def Get_Macro_Expand_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Macro_Expand_Flag`` field of.
    :returns:   The node's ``Macro_Expand_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_macro_expand_flag")
def Set_Macro_Expand_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Macro_Expand_Flag`` field of.
    :param value: The value to write into the ``Macro_Expand_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_need_instance_bodies")
def Get_Need_Instance_Bodies(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Need_Instance_Bodies`` field of.
    :returns:   The node's ``Need_Instance_Bodies`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_need_instance_bodies")
def Set_Need_Instance_Bodies(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Need_Instance_Bodies`` field of.
    :param value: The value to write into the ``Need_Instance_Bodies`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_hierarchical_name")
def Get_Hierarchical_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Hierarchical_Name`` field of.
    :returns:   The node's ``Hierarchical_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_hierarchical_name")
def Set_Hierarchical_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Hierarchical_Name`` field of.
    :param value: The value to write into the ``Hierarchical_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_vunit_item_chain")
def Get_Vunit_Item_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Vunit_Item_Chain`` field of.
    :returns:   The node's ``Vunit_Item_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_vunit_item_chain")
def Set_Vunit_Item_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Vunit_Item_Chain`` field of.
    :param value: The value to write into the ``Vunit_Item_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_bound_vunit_chain")
def Get_Bound_Vunit_Chain(obj: Iir) -> Iir:
    """
    Chain of vunit declarations bound to an entity or an architecture.

    :param obj: The node to read the ``Bound_Vunit_Chain`` field of.
    :returns:   The node's ``Bound_Vunit_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_bound_vunit_chain")
def Set_Bound_Vunit_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of vunit declarations bound to an entity or an architecture.

    :param obj:   The node to write the ``Bound_Vunit_Chain`` field of.
    :param value: The value to write into the ``Bound_Vunit_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_verification_block_configuration")
def Get_Verification_Block_Configuration(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Verification_Block_Configuration`` field of.
    :returns:   The node's ``Verification_Block_Configuration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_verification_block_configuration")
def Set_Verification_Block_Configuration(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Verification_Block_Configuration`` field of.
    :param value: The value to write into the ``Verification_Block_Configuration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_block_configuration")
def Get_Block_Configuration(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Block_Configuration`` field of.
    :returns:   The node's ``Block_Configuration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_block_configuration")
def Set_Block_Configuration(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Block_Configuration`` field of.
    :param value: The value to write into the ``Block_Configuration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_concurrent_statement_chain")
def Get_Concurrent_Statement_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Concurrent_Statement_Chain`` field of.
    :returns:   The node's ``Concurrent_Statement_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_concurrent_statement_chain")
def Set_Concurrent_Statement_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Concurrent_Statement_Chain`` field of.
    :param value: The value to write into the ``Concurrent_Statement_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_chain")
def Get_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Chain`` field of.
    :returns:   The node's ``Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_chain")
def Set_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Chain`` field of.
    :param value: The value to write into the ``Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_port_chain")
def Get_Port_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Port_Chain`` field of.
    :returns:   The node's ``Port_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_port_chain")
def Set_Port_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Port_Chain`` field of.
    :param value: The value to write into the ``Port_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_generic_chain")
def Get_Generic_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Generic_Chain`` field of.
    :returns:   The node's ``Generic_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_generic_chain")
def Set_Generic_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Generic_Chain`` field of.
    :param value: The value to write into the ``Generic_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_type")
def Get_Type(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Type`` field of.
    :returns:   The node's ``Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_type")
def Set_Type(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Type`` field of.
    :param value: The value to write into the ``Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_indication")
def Get_Subtype_Indication(obj: Iir) -> Iir:
    """
    The subtype indication of a declaration.  If several declarations share the same subtype_indication like in:

      variable a, b : integer := 5;
    then only the first declaration is the owner of the subtype_indication.

    :param obj: The node to read the ``Subtype_Indication`` field of.
    :returns:   The node's ``Subtype_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_indication")
def Set_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """
    The subtype indication of a declaration.  If several declarations share the same subtype_indication like in:

      variable a, b : integer := 5;
    then only the first declaration is the owner of the subtype_indication.

    :param obj:   The node to write the ``Subtype_Indication`` field of.
    :param value: The value to write into the ``Subtype_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_discrete_range")
def Get_Discrete_Range(obj: Iir) -> Iir:
    """
    Discrete range of an iterator.  During analysis, a subtype indication is created from this range.

    :param obj: The node to read the ``Discrete_Range`` field of.
    :returns:   The node's ``Discrete_Range`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_discrete_range")
def Set_Discrete_Range(obj: Iir, value: Iir) -> None:
    """
    Discrete range of an iterator.  During analysis, a subtype indication is created from this range.

    :param obj:   The node to write the ``Discrete_Range`` field of.
    :param value: The value to write into the ``Discrete_Range`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_type_definition")
def Get_Type_Definition(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Type_Definition`` field of.
    :returns:   The node's ``Type_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_type_definition")
def Set_Type_Definition(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Type_Definition`` field of.
    :param value: The value to write into the ``Type_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_definition")
def Get_Subtype_Definition(obj: Iir) -> Iir:
    """
    The subtype definition associated with the type declaration (if any).

    :param obj: The node to read the ``Subtype_Definition`` field of.
    :returns:   The node's ``Subtype_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_definition")
def Set_Subtype_Definition(obj: Iir, value: Iir) -> None:
    """
    The subtype definition associated with the type declaration (if any).

    :param obj:   The node to write the ``Subtype_Definition`` field of.
    :param value: The value to write into the ``Subtype_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_incomplete_type_declaration")
def Get_Incomplete_Type_Declaration(obj: Iir) -> Iir:
    """
    Set if the type declaration completes an incomplete type declaration

    :param obj: The node to read the ``Incomplete_Type_Declaration`` field of.
    :returns:   The node's ``Incomplete_Type_Declaration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_incomplete_type_declaration")
def Set_Incomplete_Type_Declaration(obj: Iir, value: Iir) -> None:
    """
    Set if the type declaration completes an incomplete type declaration

    :param obj:   The node to write the ``Incomplete_Type_Declaration`` field of.
    :param value: The value to write into the ``Incomplete_Type_Declaration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_interface_type_subprograms")
def Get_Interface_Type_Subprograms(obj: Iir) -> Iir:
    """
    Implicit operations of an interface type declaration.

    :param obj: The node to read the ``Interface_Type_Subprograms`` field of.
    :returns:   The node's ``Interface_Type_Subprograms`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_interface_type_subprograms")
def Set_Interface_Type_Subprograms(obj: Iir, value: Iir) -> None:
    """
    Implicit operations of an interface type declaration.

    :param obj:   The node to write the ``Interface_Type_Subprograms`` field of.
    :param value: The value to write into the ``Interface_Type_Subprograms`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_interface_type_definition")
def Get_Interface_Type_Definition(obj: Iir) -> Iir:
    """
    Owner of the interface type definition.

    :param obj: The node to read the ``Interface_Type_Definition`` field of.
    :returns:   The node's ``Interface_Type_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_interface_type_definition")
def Set_Interface_Type_Definition(obj: Iir, value: Iir) -> None:
    """
    Owner of the interface type definition.

    :param obj:   The node to write the ``Interface_Type_Definition`` field of.
    :param value: The value to write into the ``Interface_Type_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_nature_definition")
def Get_Nature_Definition(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Nature_Definition`` field of.
    :returns:   The node's ``Nature_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_nature_definition")
def Set_Nature_Definition(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Nature_Definition`` field of.
    :param value: The value to write into the ``Nature_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_nature")
def Get_Nature(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Nature`` field of.
    :returns:   The node's ``Nature`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_nature")
def Set_Nature(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Nature`` field of.
    :param value: The value to write into the ``Nature`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subnature_indication")
def Get_Subnature_Indication(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Subnature_Indication`` field of.
    :returns:   The node's ``Subnature_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subnature_indication")
def Set_Subnature_Indication(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Subnature_Indication`` field of.
    :param value: The value to write into the ``Subnature_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_reference_terminal_flag")
def Get_Reference_Terminal_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Reference_Terminal_Flag`` field of.
    :returns:   The node's ``Reference_Terminal_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_reference_terminal_flag")
def Set_Reference_Terminal_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Reference_Terminal_Flag`` field of.
    :param value: The value to write into the ``Reference_Terminal_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_mode")
def Get_Mode(obj: Iir) -> Iir:
    """
    Mode of interfaces or file (v87).

    :param obj: The node to read the ``Mode`` field of.
    :returns:   The node's ``Mode`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_mode")
def Set_Mode(obj: Iir, value: Iir) -> None:
    """
    Mode of interfaces or file (v87).

    :param obj:   The node to write the ``Mode`` field of.
    :param value: The value to write into the ``Mode`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_guarded_signal_flag")
def Get_Guarded_Signal_Flag(obj: Iir) -> Boolean:
    """
    True if the signal is guarded (has a signal kind).

    :param obj: The node to read the ``Guarded_Signal_Flag`` field of.
    :returns:   The node's ``Guarded_Signal_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_guarded_signal_flag")
def Set_Guarded_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if the signal is guarded (has a signal kind).

    :param obj:   The node to write the ``Guarded_Signal_Flag`` field of.
    :param value: The value to write into the ``Guarded_Signal_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_signal_kind")
def Get_Signal_Kind(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Signal_Kind`` field of.
    :returns:   The node's ``Signal_Kind`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_signal_kind")
def Set_Signal_Kind(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Signal_Kind`` field of.
    :param value: The value to write into the ``Signal_Kind`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_base_name")
def Get_Base_Name(obj: Iir) -> Iir:
    """
    The base name of a name is the node at the origin of the name. The base name is a declaration (signal, object,
    constant or interface), a selected_by_all name, an implicit_dereference name.

    :param obj: The node to read the ``Base_Name`` field of.
    :returns:   The node's ``Base_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_base_name")
def Set_Base_Name(obj: Iir, value: Iir) -> None:
    """
    The base name of a name is the node at the origin of the name. The base name is a declaration (signal, object,
    constant or interface), a selected_by_all name, an implicit_dereference name.

    :param obj:   The node to write the ``Base_Name`` field of.
    :param value: The value to write into the ``Base_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_interface_declaration_chain")
def Get_Interface_Declaration_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Interface_Declaration_Chain`` field of.
    :returns:   The node's ``Interface_Declaration_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_interface_declaration_chain")
def Set_Interface_Declaration_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Interface_Declaration_Chain`` field of.
    :param value: The value to write into the ``Interface_Declaration_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_default_subprogram")
def Get_Default_Subprogram(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Default_Subprogram`` field of.
    :returns:   The node's ``Default_Subprogram`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_default_subprogram")
def Set_Default_Subprogram(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Default_Subprogram`` field of.
    :param value: The value to write into the ``Default_Subprogram`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_associated_subprogram")
def Get_Associated_Subprogram(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Associated_Subprogram`` field of.
    :returns:   The node's ``Associated_Subprogram`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_associated_subprogram")
def Set_Associated_Subprogram(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Associated_Subprogram`` field of.
    :param value: The value to write into the ``Associated_Subprogram`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_specification")
def Get_Subprogram_Specification(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Subprogram_Specification`` field of.
    :returns:   The node's ``Subprogram_Specification`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_specification")
def Set_Subprogram_Specification(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Subprogram_Specification`` field of.
    :param value: The value to write into the ``Subprogram_Specification`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_sequential_statement_chain")
def Get_Sequential_Statement_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Sequential_Statement_Chain`` field of.
    :returns:   The node's ``Sequential_Statement_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_sequential_statement_chain")
def Set_Sequential_Statement_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Sequential_Statement_Chain`` field of.
    :param value: The value to write into the ``Sequential_Statement_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_statement_chain")
def Get_Simultaneous_Statement_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Simultaneous_Statement_Chain`` field of.
    :returns:   The node's ``Simultaneous_Statement_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_statement_chain")
def Set_Simultaneous_Statement_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Simultaneous_Statement_Chain`` field of.
    :param value: The value to write into the ``Simultaneous_Statement_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_body")
def Get_Subprogram_Body(obj: Iir) -> Iir:
    """
    The body of a subprogram (from the subprogram specification). Note that this field is only set when the body has
    been analyzed (ok, that's obvious).  For subprogram specifications in instantiated package, this field is in general
    not set because the package specification may be instantiated before the package body is analyzed and there is no
    tracking of all instantiated packages.  So when the package body is analyzed, there is no way to set this field for
    the subprograms in all instantiated specifications. You could use Get_Subprogram_Body_Origin to extract the body.
    It uses the Origin link to find the original specification which has this field set.

    :param obj: The node to read the ``Subprogram_Body`` field of.
    :returns:   The node's ``Subprogram_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_body")
def Set_Subprogram_Body(obj: Iir, value: Iir) -> None:
    """
    The body of a subprogram (from the subprogram specification). Note that this field is only set when the body has
    been analyzed (ok, that's obvious).  For subprogram specifications in instantiated package, this field is in general
    not set because the package specification may be instantiated before the package body is analyzed and there is no
    tracking of all instantiated packages.  So when the package body is analyzed, there is no way to set this field for
    the subprograms in all instantiated specifications. You could use Get_Subprogram_Body_Origin to extract the body.
    It uses the Origin link to find the original specification which has this field set.

    :param obj:   The node to write the ``Subprogram_Body`` field of.
    :param value: The value to write into the ``Subprogram_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_overload_number")
def Get_Overload_Number(obj: Iir) -> Iir:
    """
    Several subprograms in a declarative region may have the same identifier.  If the overload number is not 0, it is
    the rank of the subprogram.  If the overload number is 0, then the identifier is not overloaded in the declarative
    region.

    :param obj: The node to read the ``Overload_Number`` field of.
    :returns:   The node's ``Overload_Number`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_overload_number")
def Set_Overload_Number(obj: Iir, value: Iir) -> None:
    """
    Several subprograms in a declarative region may have the same identifier.  If the overload number is not 0, it is
    the rank of the subprogram.  If the overload number is 0, then the identifier is not overloaded in the declarative
    region.

    :param obj:   The node to write the ``Overload_Number`` field of.
    :param value: The value to write into the ``Overload_Number`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_depth")
def Get_Subprogram_Depth(obj: Iir) -> Iir:
    """
    Depth of a subprogram. For a subprogram declared immediately within an entity, architecture, package, process,
    block, generate, the depth is 0. For a subprogram declared immediately within a subprogram of level N, the depth is
    N + 1. Depth is used with depth of impure objects to check purity rules.

    :param obj: The node to read the ``Subprogram_Depth`` field of.
    :returns:   The node's ``Subprogram_Depth`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_depth")
def Set_Subprogram_Depth(obj: Iir, value: Iir) -> None:
    """
    Depth of a subprogram. For a subprogram declared immediately within an entity, architecture, package, process,
    block, generate, the depth is 0. For a subprogram declared immediately within a subprogram of level N, the depth is
    N + 1. Depth is used with depth of impure objects to check purity rules.

    :param obj:   The node to write the ``Subprogram_Depth`` field of.
    :param value: The value to write into the ``Subprogram_Depth`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_hash")
def Get_Subprogram_Hash(obj: Iir) -> Iir:
    """
    Hash of a subprogram profile. This is used to speed up subprogram profile comparison, which is very often used by
    overload.

    :param obj: The node to read the ``Subprogram_Hash`` field of.
    :returns:   The node's ``Subprogram_Hash`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_hash")
def Set_Subprogram_Hash(obj: Iir, value: Iir) -> None:
    """
    Hash of a subprogram profile. This is used to speed up subprogram profile comparison, which is very often used by
    overload.

    :param obj:   The node to write the ``Subprogram_Hash`` field of.
    :param value: The value to write into the ``Subprogram_Hash`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_impure_depth")
def Get_Impure_Depth(obj: Iir) -> Iir:
    """
    Depth of the deepest impure object.

    :param obj: The node to read the ``Impure_Depth`` field of.
    :returns:   The node's ``Impure_Depth`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_impure_depth")
def Set_Impure_Depth(obj: Iir, value: Iir) -> None:
    """
    Depth of the deepest impure object.

    :param obj:   The node to write the ``Impure_Depth`` field of.
    :param value: The value to write into the ``Impure_Depth`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_return_type")
def Get_Return_Type(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Return_Type`` field of.
    :returns:   The node's ``Return_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_return_type")
def Set_Return_Type(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Return_Type`` field of.
    :param value: The value to write into the ``Return_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_implicit_definition")
def Get_Implicit_Definition(obj: Iir) -> Iir:
    """
    Code of an implicit subprogram definition.

    :param obj: The node to read the ``Implicit_Definition`` field of.
    :returns:   The node's ``Implicit_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_implicit_definition")
def Set_Implicit_Definition(obj: Iir, value: Iir) -> None:
    """
    Code of an implicit subprogram definition.

    :param obj:   The node to write the ``Implicit_Definition`` field of.
    :param value: The value to write into the ``Implicit_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_subprogram_name")
def Get_Uninstantiated_Subprogram_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Uninstantiated_Subprogram_Name`` field of.
    :returns:   The node's ``Uninstantiated_Subprogram_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_subprogram_name")
def Set_Uninstantiated_Subprogram_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Uninstantiated_Subprogram_Name`` field of.
    :param value: The value to write into the ``Uninstantiated_Subprogram_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_default_value")
def Get_Default_Value(obj: Iir) -> Iir:
    """
    Get the default value of an object declaration. Null_iir if no default value. Note that this node can be shared
    between declarations if they are separated by comma, such as in:

      variable a, b : integer := 5;
      procedure p (a, b : natural := 7);

    :param obj: The node to read the ``Default_Value`` field of.
    :returns:   The node's ``Default_Value`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_default_value")
def Set_Default_Value(obj: Iir, value: Iir) -> None:
    """
    Get the default value of an object declaration. Null_iir if no default value. Note that this node can be shared
    between declarations if they are separated by comma, such as in:

      variable a, b : integer := 5;
      procedure p (a, b : natural := 7);

    :param obj:   The node to write the ``Default_Value`` field of.
    :param value: The value to write into the ``Default_Value`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_mode_view_indication")
def Get_Mode_View_Indication(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Mode_View_Indication`` field of.
    :returns:   The node's ``Mode_View_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_mode_view_indication")
def Set_Mode_View_Indication(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Mode_View_Indication`` field of.
    :param value: The value to write into the ``Mode_View_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_deferred_declaration")
def Get_Deferred_Declaration(obj: Iir) -> Iir:
    """
    The deferred_declaration field points to the deferred constant declaration for a full constant declaration, or is
    null_iir for a usual or deferred constant declaration. Set only during sem.

    :param obj: The node to read the ``Deferred_Declaration`` field of.
    :returns:   The node's ``Deferred_Declaration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_deferred_declaration")
def Set_Deferred_Declaration(obj: Iir, value: Iir) -> None:
    """
    The deferred_declaration field points to the deferred constant declaration for a full constant declaration, or is
    null_iir for a usual or deferred constant declaration. Set only during sem.

    :param obj:   The node to write the ``Deferred_Declaration`` field of.
    :param value: The value to write into the ``Deferred_Declaration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_deferred_declaration_flag")
def Get_Deferred_Declaration_Flag(obj: Iir) -> Boolean:
    """
    The deferred_declaration_flag must be set if the constant declaration is a deferred_constant declaration. Set only
    during sem.

    :param obj: The node to read the ``Deferred_Declaration_Flag`` field of.
    :returns:   The node's ``Deferred_Declaration_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_deferred_declaration_flag")
def Set_Deferred_Declaration_Flag(obj: Iir, value: Boolean) -> None:
    """
    The deferred_declaration_flag must be set if the constant declaration is a deferred_constant declaration. Set only
    during sem.

    :param obj:   The node to write the ``Deferred_Declaration_Flag`` field of.
    :param value: The value to write into the ``Deferred_Declaration_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_shared_flag")
def Get_Shared_Flag(obj: Iir) -> Boolean:
    """
    If true, the variable is declared shared.

    :param obj: The node to read the ``Shared_Flag`` field of.
    :returns:   The node's ``Shared_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_shared_flag")
def Set_Shared_Flag(obj: Iir, value: Boolean) -> None:
    """
    If true, the variable is declared shared.

    :param obj:   The node to write the ``Shared_Flag`` field of.
    :param value: The value to write into the ``Shared_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit")
def Get_Design_Unit(obj: Iir) -> Iir:
    """
    Get the design unit in which the target is declared. For a library unit, this is to get the design unit node.

    :param obj: The node to read the ``Design_Unit`` field of.
    :returns:   The node's ``Design_Unit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit")
def Set_Design_Unit(obj: Iir, value: Iir) -> None:
    """
    Get the design unit in which the target is declared. For a library unit, this is to get the design unit node.

    :param obj:   The node to write the ``Design_Unit`` field of.
    :param value: The value to write into the ``Design_Unit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_block_statement")
def Get_Block_Statement(obj: Iir) -> Iir:
    """
    Corresponding block statement for an implicit guard signal.

    :param obj: The node to read the ``Block_Statement`` field of.
    :returns:   The node's ``Block_Statement`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_block_statement")
def Set_Block_Statement(obj: Iir, value: Iir) -> None:
    """
    Corresponding block statement for an implicit guard signal.

    :param obj:   The node to write the ``Block_Statement`` field of.
    :param value: The value to write into the ``Block_Statement`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_signal_driver")
def Get_Signal_Driver(obj: Iir) -> Iir:
    """
    For a non-resolved signal: null_iir if the signal has no driver, or a process/concurrent_statement for which the
    signal should have a driver.  This is used to catch at analyse time unresolved signals with several drivers.

    :param obj: The node to read the ``Signal_Driver`` field of.
    :returns:   The node's ``Signal_Driver`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_signal_driver")
def Set_Signal_Driver(obj: Iir, value: Iir) -> None:
    """
    For a non-resolved signal: null_iir if the signal has no driver, or a process/concurrent_statement for which the
    signal should have a driver.  This is used to catch at analyse time unresolved signals with several drivers.

    :param obj:   The node to write the ``Signal_Driver`` field of.
    :param value: The value to write into the ``Signal_Driver`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_declaration_chain")
def Get_Declaration_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Declaration_Chain`` field of.
    :returns:   The node's ``Declaration_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_declaration_chain")
def Set_Declaration_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Declaration_Chain`` field of.
    :param value: The value to write into the ``Declaration_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_file_logical_name")
def Get_File_Logical_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``File_Logical_Name`` field of.
    :returns:   The node's ``File_Logical_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_file_logical_name")
def Set_File_Logical_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``File_Logical_Name`` field of.
    :param value: The value to write into the ``File_Logical_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_file_open_kind")
def Get_File_Open_Kind(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``File_Open_Kind`` field of.
    :returns:   The node's ``File_Open_Kind`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_file_open_kind")
def Set_File_Open_Kind(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``File_Open_Kind`` field of.
    :param value: The value to write into the ``File_Open_Kind`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_element_position")
def Get_Element_Position(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Element_Position`` field of.
    :returns:   The node's ``Element_Position`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_element_position")
def Set_Element_Position(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Element_Position`` field of.
    :param value: The value to write into the ``Element_Position`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_use_clause_chain")
def Get_Use_Clause_Chain(obj: Iir) -> Iir:
    """
    Selected names of an use_clause are chained.

    :param obj: The node to read the ``Use_Clause_Chain`` field of.
    :returns:   The node's ``Use_Clause_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_use_clause_chain")
def Set_Use_Clause_Chain(obj: Iir, value: Iir) -> None:
    """
    Selected names of an use_clause are chained.

    :param obj:   The node to write the ``Use_Clause_Chain`` field of.
    :param value: The value to write into the ``Use_Clause_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_context_reference_chain")
def Get_Context_Reference_Chain(obj: Iir) -> Iir:
    """
    Selected names of a context_reference are chained.

    :param obj: The node to read the ``Context_Reference_Chain`` field of.
    :returns:   The node's ``Context_Reference_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_context_reference_chain")
def Set_Context_Reference_Chain(obj: Iir, value: Iir) -> None:
    """
    Selected names of a context_reference are chained.

    :param obj:   The node to write the ``Context_Reference_Chain`` field of.
    :param value: The value to write into the ``Context_Reference_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_inherit_spec_chain")
def Get_Inherit_Spec_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Inherit_Spec_Chain`` field of.
    :returns:   The node's ``Inherit_Spec_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_inherit_spec_chain")
def Set_Inherit_Spec_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Inherit_Spec_Chain`` field of.
    :param value: The value to write into the ``Inherit_Spec_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_selected_name")
def Get_Selected_Name(obj: Iir) -> Iir:
    """
    Selected name of an use_clause or context_reference

    :param obj: The node to read the ``Selected_Name`` field of.
    :returns:   The node's ``Selected_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_selected_name")
def Set_Selected_Name(obj: Iir, value: Iir) -> None:
    """
    Selected name of an use_clause or context_reference

    :param obj:   The node to write the ``Selected_Name`` field of.
    :param value: The value to write into the ``Selected_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_mode_view_name")
def Get_Mode_View_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Mode_View_Name`` field of.
    :returns:   The node's ``Mode_View_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_mode_view_name")
def Set_Mode_View_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Mode_View_Name`` field of.
    :param value: The value to write into the ``Mode_View_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_type_declarator")
def Get_Type_Declarator(obj: Iir) -> Iir:
    """
    The type declarator which declares the type definition DEF.  Can also be a nature declarator for composite nature
    definition.

    :param obj: The node to read the ``Type_Declarator`` field of.
    :returns:   The node's ``Type_Declarator`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_type_declarator")
def Set_Type_Declarator(obj: Iir, value: Iir) -> None:
    """
    The type declarator which declares the type definition DEF.  Can also be a nature declarator for composite nature
    definition.

    :param obj:   The node to write the ``Type_Declarator`` field of.
    :param value: The value to write into the ``Type_Declarator`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_complete_type_definition")
def Get_Complete_Type_Definition(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Complete_Type_Definition`` field of.
    :returns:   The node's ``Complete_Type_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_complete_type_definition")
def Set_Complete_Type_Definition(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Complete_Type_Definition`` field of.
    :param value: The value to write into the ``Complete_Type_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_incomplete_type_ref_chain")
def Get_Incomplete_Type_Ref_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Incomplete_Type_Ref_Chain`` field of.
    :returns:   The node's ``Incomplete_Type_Ref_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_incomplete_type_ref_chain")
def Set_Incomplete_Type_Ref_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Incomplete_Type_Ref_Chain`` field of.
    :param value: The value to write into the ``Incomplete_Type_Ref_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_associated_type")
def Get_Associated_Type(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Associated_Type`` field of.
    :returns:   The node's ``Associated_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_associated_type")
def Set_Associated_Type(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Associated_Type`` field of.
    :param value: The value to write into the ``Associated_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_enumeration_literal_list")
def Get_Enumeration_Literal_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Enumeration_Literal_List`` field of.
    :returns:   The node's ``Enumeration_Literal_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_enumeration_literal_list")
def Set_Enumeration_Literal_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Enumeration_Literal_List`` field of.
    :param value: The value to write into the ``Enumeration_Literal_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_entity_class_entry_chain")
def Get_Entity_Class_Entry_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Entity_Class_Entry_Chain`` field of.
    :returns:   The node's ``Entity_Class_Entry_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_entity_class_entry_chain")
def Set_Entity_Class_Entry_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Entity_Class_Entry_Chain`` field of.
    :param value: The value to write into the ``Entity_Class_Entry_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_group_constituent_list")
def Get_Group_Constituent_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Group_Constituent_List`` field of.
    :returns:   The node's ``Group_Constituent_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_group_constituent_list")
def Set_Group_Constituent_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Group_Constituent_List`` field of.
    :param value: The value to write into the ``Group_Constituent_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_unit_chain")
def Get_Unit_Chain(obj: Iir) -> Iir:
    """
    Chain of physical type units. The first unit is the primary unit.  If you really need the primary unit (and not the
    chain), you'd better to use Get_Primary_Unit.

    :param obj: The node to read the ``Unit_Chain`` field of.
    :returns:   The node's ``Unit_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_unit_chain")
def Set_Unit_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of physical type units. The first unit is the primary unit.  If you really need the primary unit (and not the
    chain), you'd better to use Get_Primary_Unit.

    :param obj:   The node to write the ``Unit_Chain`` field of.
    :param value: The value to write into the ``Unit_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_primary_unit")
def Get_Primary_Unit(obj: Iir) -> Iir:
    """
    Alias of Get_Unit_Chain. Return the primary unit of a physical type.

    :param obj: The node to read the ``Primary_Unit`` field of.
    :returns:   The node's ``Primary_Unit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_primary_unit")
def Set_Primary_Unit(obj: Iir, value: Iir) -> None:
    """
    Alias of Get_Unit_Chain. Return the primary unit of a physical type.

    :param obj:   The node to write the ``Primary_Unit`` field of.
    :param value: The value to write into the ``Primary_Unit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_identifier")
def Get_Identifier(obj: Iir) -> NameId:
    """
    Get/Set the identifier of a declaration. Can also be used instead of get/set_label.

    :param obj: The node to read the ``Identifier`` field of.
    :returns:   The node's ``Identifier`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_identifier")
def Set_Identifier(obj: Iir, value: NameId) -> None:
    """
    Get/Set the identifier of a declaration. Can also be used instead of get/set_label.

    :param obj:   The node to write the ``Identifier`` field of.
    :param value: The value to write into the ``Identifier`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_label")
def Get_Label(obj: Iir) -> NameId:
    """
    :param obj: The node to read the ``Label`` field of.
    :returns:   The node's ``Label`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_label")
def Set_Label(obj: Iir, value: NameId) -> None:
    """
    :param obj:   The node to write the ``Label`` field of.
    :param value: The value to write into the ``Label`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_return_identifier")
def Get_Return_Identifier(obj: Iir) -> Iir:
    """
    Return a subtype declaration for the return subtype (vhdl-19)

    :param obj: The node to read the ``Return_Identifier`` field of.
    :returns:   The node's ``Return_Identifier`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_return_identifier")
def Set_Return_Identifier(obj: Iir, value: Iir) -> None:
    """
    Return a subtype declaration for the return subtype (vhdl-19)

    :param obj:   The node to write the ``Return_Identifier`` field of.
    :param value: The value to write into the ``Return_Identifier`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_visible_flag")
def Get_Visible_Flag(obj: Iir) -> Boolean:
    """
    Get/Set the visible flag of a declaration. The visible flag is true to make invalid the use of the identifier during
    its declaration.  It is set to false when the identifier is added to the name table, and set to true when the
    declaration is finished.

    :param obj: The node to read the ``Visible_Flag`` field of.
    :returns:   The node's ``Visible_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_visible_flag")
def Set_Visible_Flag(obj: Iir, value: Boolean) -> None:
    """
    Get/Set the visible flag of a declaration. The visible flag is true to make invalid the use of the identifier during
    its declaration.  It is set to false when the identifier is added to the name table, and set to true when the
    declaration is finished.

    :param obj:   The node to write the ``Visible_Flag`` field of.
    :param value: The value to write into the ``Visible_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_range_constraint")
def Get_Range_Constraint(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Range_Constraint`` field of.
    :returns:   The node's ``Range_Constraint`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_range_constraint")
def Set_Range_Constraint(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Range_Constraint`` field of.
    :param value: The value to write into the ``Range_Constraint`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_direction")
def Get_Direction(obj: Iir) -> DirectionType:
    """
    :param obj: The node to read the ``Direction`` field of.
    :returns:   The node's ``Direction`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_direction")
def Set_Direction(obj: Iir, value: DirectionType) -> None:
    """
    :param obj:   The node to write the ``Direction`` field of.
    :param value: The value to write into the ``Direction`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_left_limit")
def Get_Left_Limit(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Left_Limit`` field of.
    :returns:   The node's ``Left_Limit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_left_limit")
def Set_Left_Limit(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Left_Limit`` field of.
    :param value: The value to write into the ``Left_Limit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_right_limit")
def Get_Right_Limit(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Right_Limit`` field of.
    :returns:   The node's ``Right_Limit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_right_limit")
def Set_Right_Limit(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Right_Limit`` field of.
    :param value: The value to write into the ``Right_Limit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_left_limit_expr")
def Get_Left_Limit_Expr(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Left_Limit_Expr`` field of.
    :returns:   The node's ``Left_Limit_Expr`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_left_limit_expr")
def Set_Left_Limit_Expr(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Left_Limit_Expr`` field of.
    :param value: The value to write into the ``Left_Limit_Expr`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_right_limit_expr")
def Get_Right_Limit_Expr(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Right_Limit_Expr`` field of.
    :returns:   The node's ``Right_Limit_Expr`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_right_limit_expr")
def Set_Right_Limit_Expr(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Right_Limit_Expr`` field of.
    :param value: The value to write into the ``Right_Limit_Expr`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parent_type")
def Get_Parent_Type(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Parent_Type`` field of.
    :returns:   The node's ``Parent_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parent_type")
def Set_Parent_Type(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Parent_Type`` field of.
    :param value: The value to write into the ``Parent_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_simple_nature")
def Get_Simple_Nature(obj: Iir) -> Iir:
    """
    Only for composite base nature: the simple nature.

    :param obj: The node to read the ``Simple_Nature`` field of.
    :returns:   The node's ``Simple_Nature`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_simple_nature")
def Set_Simple_Nature(obj: Iir, value: Iir) -> None:
    """
    Only for composite base nature: the simple nature.

    :param obj:   The node to write the ``Simple_Nature`` field of.
    :param value: The value to write into the ``Simple_Nature`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_base_nature")
def Get_Base_Nature(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Base_Nature`` field of.
    :returns:   The node's ``Base_Nature`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_base_nature")
def Set_Base_Nature(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Base_Nature`` field of.
    :param value: The value to write into the ``Base_Nature`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_resolution_indication")
def Get_Resolution_Indication(obj: Iir) -> Iir:
    """
    Either a resolution function name, an array_element_resolution or a record_resolution

    :param obj: The node to read the ``Resolution_Indication`` field of.
    :returns:   The node's ``Resolution_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_resolution_indication")
def Set_Resolution_Indication(obj: Iir, value: Iir) -> None:
    """
    Either a resolution function name, an array_element_resolution or a record_resolution

    :param obj:   The node to write the ``Resolution_Indication`` field of.
    :param value: The value to write into the ``Resolution_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_record_element_resolution_chain")
def Get_Record_Element_Resolution_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Record_Element_Resolution_Chain`` field of.
    :returns:   The node's ``Record_Element_Resolution_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_record_element_resolution_chain")
def Set_Record_Element_Resolution_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Record_Element_Resolution_Chain`` field of.
    :param value: The value to write into the ``Record_Element_Resolution_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_tolerance")
def Get_Tolerance(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Tolerance`` field of.
    :returns:   The node's ``Tolerance`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_tolerance")
def Set_Tolerance(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Tolerance`` field of.
    :param value: The value to write into the ``Tolerance`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_plus_terminal_name")
def Get_Plus_Terminal_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Plus_Terminal_Name`` field of.
    :returns:   The node's ``Plus_Terminal_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_plus_terminal_name")
def Set_Plus_Terminal_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Plus_Terminal_Name`` field of.
    :param value: The value to write into the ``Plus_Terminal_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_minus_terminal_name")
def Get_Minus_Terminal_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Minus_Terminal_Name`` field of.
    :returns:   The node's ``Minus_Terminal_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_minus_terminal_name")
def Set_Minus_Terminal_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Minus_Terminal_Name`` field of.
    :param value: The value to write into the ``Minus_Terminal_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_plus_terminal")
def Get_Plus_Terminal(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Plus_Terminal`` field of.
    :returns:   The node's ``Plus_Terminal`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_plus_terminal")
def Set_Plus_Terminal(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Plus_Terminal`` field of.
    :param value: The value to write into the ``Plus_Terminal`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_minus_terminal")
def Get_Minus_Terminal(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Minus_Terminal`` field of.
    :returns:   The node's ``Minus_Terminal`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_minus_terminal")
def Set_Minus_Terminal(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Minus_Terminal`` field of.
    :param value: The value to write into the ``Minus_Terminal`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_magnitude_expression")
def Get_Magnitude_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Magnitude_Expression`` field of.
    :returns:   The node's ``Magnitude_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_magnitude_expression")
def Set_Magnitude_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Magnitude_Expression`` field of.
    :param value: The value to write into the ``Magnitude_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_phase_expression")
def Get_Phase_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Phase_Expression`` field of.
    :returns:   The node's ``Phase_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_phase_expression")
def Set_Phase_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Phase_Expression`` field of.
    :param value: The value to write into the ``Phase_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_power_expression")
def Get_Power_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Power_Expression`` field of.
    :returns:   The node's ``Power_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_power_expression")
def Set_Power_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Power_Expression`` field of.
    :param value: The value to write into the ``Power_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_left")
def Get_Simultaneous_Left(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Simultaneous_Left`` field of.
    :returns:   The node's ``Simultaneous_Left`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_left")
def Set_Simultaneous_Left(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Simultaneous_Left`` field of.
    :param value: The value to write into the ``Simultaneous_Left`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_right")
def Get_Simultaneous_Right(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Simultaneous_Right`` field of.
    :returns:   The node's ``Simultaneous_Right`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_right")
def Set_Simultaneous_Right(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Simultaneous_Right`` field of.
    :param value: The value to write into the ``Simultaneous_Right`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_text_file_flag")
def Get_Text_File_Flag(obj: Iir) -> Boolean:
    """
    True if ATYPE defines std.textio.text file type.

    :param obj: The node to read the ``Text_File_Flag`` field of.
    :returns:   The node's ``Text_File_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_text_file_flag")
def Set_Text_File_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if ATYPE defines std.textio.text file type.

    :param obj:   The node to write the ``Text_File_Flag`` field of.
    :param value: The value to write into the ``Text_File_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_only_characters_flag")
def Get_Only_Characters_Flag(obj: Iir) -> Boolean:
    """
    True if enumeration type ATYPE has only character literals.

    :param obj: The node to read the ``Only_Characters_Flag`` field of.
    :returns:   The node's ``Only_Characters_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_only_characters_flag")
def Set_Only_Characters_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if enumeration type ATYPE has only character literals.

    :param obj:   The node to write the ``Only_Characters_Flag`` field of.
    :param value: The value to write into the ``Only_Characters_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_is_character_type")
def Get_Is_Character_Type(obj: Iir) -> Boolean:
    """
    True if enumeration type ATYPE is a character type (at least one literal is a character).

    :param obj: The node to read the ``Is_Character_Type`` field of.
    :returns:   The node's ``Is_Character_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_is_character_type")
def Set_Is_Character_Type(obj: Iir, value: Boolean) -> None:
    """
    True if enumeration type ATYPE is a character type (at least one literal is a character).

    :param obj:   The node to write the ``Is_Character_Type`` field of.
    :param value: The value to write into the ``Is_Character_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_nature_staticness")
def Get_Nature_Staticness(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Nature_Staticness`` field of.
    :returns:   The node's ``Nature_Staticness`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_nature_staticness")
def Set_Nature_Staticness(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Nature_Staticness`` field of.
    :param value: The value to write into the ``Nature_Staticness`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_type_staticness")
def Get_Type_Staticness(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Type_Staticness`` field of.
    :returns:   The node's ``Type_Staticness`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_type_staticness")
def Set_Type_Staticness(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Type_Staticness`` field of.
    :param value: The value to write into the ``Type_Staticness`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_constraint_state")
def Get_Constraint_State(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Constraint_State`` field of.
    :returns:   The node's ``Constraint_State`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_constraint_state")
def Set_Constraint_State(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Constraint_State`` field of.
    :param value: The value to write into the ``Constraint_State`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype_list")
def Get_Index_Subtype_List(obj: Iir) -> Iir:
    """
    Reference either index_subtype_definition_list of array_type_definition or index_constraint_list of
    array_subtype_definition.  Set only when the index_sutype is constrained (to differentiate with unconstrained index
    type).

    :param obj: The node to read the ``Index_Subtype_List`` field of.
    :returns:   The node's ``Index_Subtype_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype_list")
def Set_Index_Subtype_List(obj: Iir, value: Iir) -> None:
    """
    Reference either index_subtype_definition_list of array_type_definition or index_constraint_list of
    array_subtype_definition.  Set only when the index_sutype is constrained (to differentiate with unconstrained index
    type).

    :param obj:   The node to write the ``Index_Subtype_List`` field of.
    :param value: The value to write into the ``Index_Subtype_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype_definition_list")
def Get_Index_Subtype_Definition_List(obj: Iir) -> Iir:
    """
    List of type marks for indexes type of array types.

    :param obj: The node to read the ``Index_Subtype_Definition_List`` field of.
    :returns:   The node's ``Index_Subtype_Definition_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype_definition_list")
def Set_Index_Subtype_Definition_List(obj: Iir, value: Iir) -> None:
    """
    List of type marks for indexes type of array types.

    :param obj:   The node to write the ``Index_Subtype_Definition_List`` field of.
    :param value: The value to write into the ``Index_Subtype_Definition_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_element_subtype_indication")
def Get_Element_Subtype_Indication(obj: Iir) -> Iir:
    """
    The subtype_indication as it appears in a array type declaration.

    :param obj: The node to read the ``Element_Subtype_Indication`` field of.
    :returns:   The node's ``Element_Subtype_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_element_subtype_indication")
def Set_Element_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """
    The subtype_indication as it appears in a array type declaration.

    :param obj:   The node to write the ``Element_Subtype_Indication`` field of.
    :param value: The value to write into the ``Element_Subtype_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_element_subtype")
def Get_Element_Subtype(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Element_Subtype`` field of.
    :returns:   The node's ``Element_Subtype`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_element_subtype")
def Set_Element_Subtype(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Element_Subtype`` field of.
    :param value: The value to write into the ``Element_Subtype`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_element_subnature_indication")
def Get_Element_Subnature_Indication(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Element_Subnature_Indication`` field of.
    :returns:   The node's ``Element_Subnature_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_element_subnature_indication")
def Set_Element_Subnature_Indication(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Element_Subnature_Indication`` field of.
    :param value: The value to write into the ``Element_Subnature_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_element_subnature")
def Get_Element_Subnature(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Element_Subnature`` field of.
    :returns:   The node's ``Element_Subnature`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_element_subnature")
def Set_Element_Subnature(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Element_Subnature`` field of.
    :param value: The value to write into the ``Element_Subnature`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_index_constraint_list")
def Get_Index_Constraint_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Index_Constraint_List`` field of.
    :returns:   The node's ``Index_Constraint_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_index_constraint_list")
def Set_Index_Constraint_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Index_Constraint_List`` field of.
    :param value: The value to write into the ``Index_Constraint_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_array_element_constraint")
def Get_Array_Element_Constraint(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Array_Element_Constraint`` field of.
    :returns:   The node's ``Array_Element_Constraint`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_array_element_constraint")
def Set_Array_Element_Constraint(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Array_Element_Constraint`` field of.
    :param value: The value to write into the ``Array_Element_Constraint`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_array_constraint_flag")
def Get_Has_Array_Constraint_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Has_Array_Constraint_Flag`` field of.
    :returns:   The node's ``Has_Array_Constraint_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_array_constraint_flag")
def Set_Has_Array_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Has_Array_Constraint_Flag`` field of.
    :param value: The value to write into the ``Has_Array_Constraint_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_element_constraint_flag")
def Get_Has_Element_Constraint_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Has_Element_Constraint_Flag`` field of.
    :returns:   The node's ``Has_Element_Constraint_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_element_constraint_flag")
def Set_Has_Element_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Has_Element_Constraint_Flag`` field of.
    :param value: The value to write into the ``Has_Element_Constraint_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_elements_declaration_list")
def Get_Elements_Declaration_List(obj: Iir) -> Iir:
    """
    List of elements of a record. For a record_type_definition: Is_Ref is false, as the elements

     declaration are owned by the type definition.
    For a record_subtype_definition: Is_Ref is false, as new constrained

     elements are owned through the Owned_Elements_Chain list.

    :param obj: The node to read the ``Elements_Declaration_List`` field of.
    :returns:   The node's ``Elements_Declaration_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_elements_declaration_list")
def Set_Elements_Declaration_List(obj: Iir, value: Iir) -> None:
    """
    List of elements of a record. For a record_type_definition: Is_Ref is false, as the elements

     declaration are owned by the type definition.
    For a record_subtype_definition: Is_Ref is false, as new constrained

     elements are owned through the Owned_Elements_Chain list.

    :param obj:   The node to write the ``Elements_Declaration_List`` field of.
    :param value: The value to write into the ``Elements_Declaration_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_elements_definition_chain")
def Get_Elements_Definition_Chain(obj: Iir) -> Iir:
    """
    For mode view elements.

    :param obj: The node to read the ``Elements_Definition_Chain`` field of.
    :returns:   The node's ``Elements_Definition_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_elements_definition_chain")
def Set_Elements_Definition_Chain(obj: Iir, value: Iir) -> None:
    """
    For mode view elements.

    :param obj:   The node to write the ``Elements_Definition_Chain`` field of.
    :param value: The value to write into the ``Elements_Definition_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_elements_definition_list")
def Get_Elements_Definition_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Elements_Definition_List`` field of.
    :returns:   The node's ``Elements_Definition_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_elements_definition_list")
def Set_Elements_Definition_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Elements_Definition_List`` field of.
    :param value: The value to write into the ``Elements_Definition_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_owned_elements_chain")
def Get_Owned_Elements_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Owned_Elements_Chain`` field of.
    :returns:   The node's ``Owned_Elements_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_owned_elements_chain")
def Set_Owned_Elements_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Owned_Elements_Chain`` field of.
    :param value: The value to write into the ``Owned_Elements_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_designated_type")
def Get_Designated_Type(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Designated_Type`` field of.
    :returns:   The node's ``Designated_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_designated_type")
def Set_Designated_Type(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Designated_Type`` field of.
    :param value: The value to write into the ``Designated_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_designated_subtype_indication")
def Get_Designated_Subtype_Indication(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Designated_Subtype_Indication`` field of.
    :returns:   The node's ``Designated_Subtype_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_designated_subtype_indication")
def Set_Designated_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Designated_Subtype_Indication`` field of.
    :param value: The value to write into the ``Designated_Subtype_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_index_list")
def Get_Index_List(obj: Iir) -> Iir:
    """
    List of indexes for indexed name.

    :param obj: The node to read the ``Index_List`` field of.
    :returns:   The node's ``Index_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_index_list")
def Set_Index_List(obj: Iir, value: Iir) -> None:
    """
    List of indexes for indexed name.

    :param obj:   The node to write the ``Index_List`` field of.
    :param value: The value to write into the ``Index_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_reference")
def Get_Reference(obj: Iir) -> Iir:
    """
    The terminal declaration for the reference (ground) of a nature

    :param obj: The node to read the ``Reference`` field of.
    :returns:   The node's ``Reference`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_reference")
def Set_Reference(obj: Iir, value: Iir) -> None:
    """
    The terminal declaration for the reference (ground) of a nature

    :param obj:   The node to write the ``Reference`` field of.
    :param value: The value to write into the ``Reference`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_nature_declarator")
def Get_Nature_Declarator(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Nature_Declarator`` field of.
    :returns:   The node's ``Nature_Declarator`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_nature_declarator")
def Set_Nature_Declarator(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Nature_Declarator`` field of.
    :param value: The value to write into the ``Nature_Declarator`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_across_type_mark")
def Get_Across_Type_Mark(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Across_Type_Mark`` field of.
    :returns:   The node's ``Across_Type_Mark`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_across_type_mark")
def Set_Across_Type_Mark(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Across_Type_Mark`` field of.
    :param value: The value to write into the ``Across_Type_Mark`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_through_type_mark")
def Get_Through_Type_Mark(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Through_Type_Mark`` field of.
    :returns:   The node's ``Through_Type_Mark`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_through_type_mark")
def Set_Through_Type_Mark(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Through_Type_Mark`` field of.
    :param value: The value to write into the ``Through_Type_Mark`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_across_type_definition")
def Get_Across_Type_Definition(obj: Iir) -> Iir:
    """
    For array and record nature: the owner of the across type.

    :param obj: The node to read the ``Across_Type_Definition`` field of.
    :returns:   The node's ``Across_Type_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_across_type_definition")
def Set_Across_Type_Definition(obj: Iir, value: Iir) -> None:
    """
    For array and record nature: the owner of the across type.

    :param obj:   The node to write the ``Across_Type_Definition`` field of.
    :param value: The value to write into the ``Across_Type_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_through_type_definition")
def Get_Through_Type_Definition(obj: Iir) -> Iir:
    """
    For array and record nature: the owner of the through type.

    :param obj: The node to read the ``Through_Type_Definition`` field of.
    :returns:   The node's ``Through_Type_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_through_type_definition")
def Set_Through_Type_Definition(obj: Iir, value: Iir) -> None:
    """
    For array and record nature: the owner of the through type.

    :param obj:   The node to write the ``Through_Type_Definition`` field of.
    :param value: The value to write into the ``Through_Type_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_across_type")
def Get_Across_Type(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Across_Type`` field of.
    :returns:   The node's ``Across_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_across_type")
def Set_Across_Type(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Across_Type`` field of.
    :param value: The value to write into the ``Across_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_through_type")
def Get_Through_Type(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Through_Type`` field of.
    :returns:   The node's ``Through_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_through_type")
def Set_Through_Type(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Through_Type`` field of.
    :param value: The value to write into the ``Through_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_target")
def Get_Target(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Target`` field of.
    :returns:   The node's ``Target`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_target")
def Set_Target(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Target`` field of.
    :param value: The value to write into the ``Target`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_waveform_chain")
def Get_Waveform_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Waveform_Chain`` field of.
    :returns:   The node's ``Waveform_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_waveform_chain")
def Set_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Waveform_Chain`` field of.
    :param value: The value to write into the ``Waveform_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_guard")
def Get_Guard(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Guard`` field of.
    :returns:   The node's ``Guard`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_guard")
def Set_Guard(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Guard`` field of.
    :param value: The value to write into the ``Guard`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_delay_mechanism")
def Get_Delay_Mechanism(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Delay_Mechanism`` field of.
    :returns:   The node's ``Delay_Mechanism`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_delay_mechanism")
def Set_Delay_Mechanism(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Delay_Mechanism`` field of.
    :param value: The value to write into the ``Delay_Mechanism`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_reject_time_expression")
def Get_Reject_Time_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Reject_Time_Expression`` field of.
    :returns:   The node's ``Reject_Time_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_reject_time_expression")
def Set_Reject_Time_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Reject_Time_Expression`` field of.
    :param value: The value to write into the ``Reject_Time_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_force_mode")
def Get_Force_Mode(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Force_Mode`` field of.
    :returns:   The node's ``Force_Mode`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_force_mode")
def Set_Force_Mode(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Force_Mode`` field of.
    :param value: The value to write into the ``Force_Mode`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_force_mode")
def Get_Has_Force_Mode(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Has_Force_Mode`` field of.
    :returns:   The node's ``Has_Force_Mode`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_force_mode")
def Set_Has_Force_Mode(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Has_Force_Mode`` field of.
    :param value: The value to write into the ``Has_Force_Mode`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_sensitivity_list")
def Get_Sensitivity_List(obj: Iir) -> Iir:
    """
    The Is_Ref flag is set for extracted sensitivity lists.

    :param obj: The node to read the ``Sensitivity_List`` field of.
    :returns:   The node's ``Sensitivity_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_sensitivity_list")
def Set_Sensitivity_List(obj: Iir, value: Iir) -> None:
    """
    The Is_Ref flag is set for extracted sensitivity lists.

    :param obj:   The node to write the ``Sensitivity_List`` field of.
    :param value: The value to write into the ``Sensitivity_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_process_origin")
def Get_Process_Origin(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Process_Origin`` field of.
    :returns:   The node's ``Process_Origin`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_process_origin")
def Set_Process_Origin(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Process_Origin`` field of.
    :param value: The value to write into the ``Process_Origin`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_package_origin")
def Get_Package_Origin(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Package_Origin`` field of.
    :returns:   The node's ``Package_Origin`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_package_origin")
def Set_Package_Origin(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Package_Origin`` field of.
    :param value: The value to write into the ``Package_Origin`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_condition_clause")
def Get_Condition_Clause(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Condition_Clause`` field of.
    :returns:   The node's ``Condition_Clause`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_condition_clause")
def Set_Condition_Clause(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Condition_Clause`` field of.
    :param value: The value to write into the ``Condition_Clause`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_break_element")
def Get_Break_Element(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Break_Element`` field of.
    :returns:   The node's ``Break_Element`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_break_element")
def Set_Break_Element(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Break_Element`` field of.
    :param value: The value to write into the ``Break_Element`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_selector_quantity")
def Get_Selector_Quantity(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Selector_Quantity`` field of.
    :returns:   The node's ``Selector_Quantity`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_selector_quantity")
def Set_Selector_Quantity(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Selector_Quantity`` field of.
    :param value: The value to write into the ``Selector_Quantity`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_break_quantity")
def Get_Break_Quantity(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Break_Quantity`` field of.
    :returns:   The node's ``Break_Quantity`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_break_quantity")
def Set_Break_Quantity(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Break_Quantity`` field of.
    :param value: The value to write into the ``Break_Quantity`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_timeout_clause")
def Get_Timeout_Clause(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Timeout_Clause`` field of.
    :returns:   The node's ``Timeout_Clause`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_timeout_clause")
def Set_Timeout_Clause(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Timeout_Clause`` field of.
    :param value: The value to write into the ``Timeout_Clause`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_postponed_flag")
def Get_Postponed_Flag(obj: Iir) -> Boolean:
    """
    If set, the concurrent statement is postponed.

    :param obj: The node to read the ``Postponed_Flag`` field of.
    :returns:   The node's ``Postponed_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_postponed_flag")
def Set_Postponed_Flag(obj: Iir, value: Boolean) -> None:
    """
    If set, the concurrent statement is postponed.

    :param obj:   The node to write the ``Postponed_Flag`` field of.
    :param value: The value to write into the ``Postponed_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_callees_list")
def Get_Callees_List(obj: Iir) -> Iir:
    """
    Returns the list of subprogram called in this subprogram or process. Note: implicit function (such as implicit
    operators) are omitted from this list, since the purpose of this list is to correctly set flags for side effects
    (purity_state, wait_state). Can return null_iir if there is no subprogram called.

    :param obj: The node to read the ``Callees_List`` field of.
    :returns:   The node's ``Callees_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_callees_list")
def Set_Callees_List(obj: Iir, value: Iir) -> None:
    """
    Returns the list of subprogram called in this subprogram or process. Note: implicit function (such as implicit
    operators) are omitted from this list, since the purpose of this list is to correctly set flags for side effects
    (purity_state, wait_state). Can return null_iir if there is no subprogram called.

    :param obj:   The node to write the ``Callees_List`` field of.
    :param value: The value to write into the ``Callees_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_passive_flag")
def Get_Passive_Flag(obj: Iir) -> Boolean:
    """
    Get/Set the passive flag of a process.

     TRUE if the process must be passive.
     FALSE if the process may be not passive.
    For a procedure declaration, set if it is passive.

    :param obj: The node to read the ``Passive_Flag`` field of.
    :returns:   The node's ``Passive_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_passive_flag")
def Set_Passive_Flag(obj: Iir, value: Boolean) -> None:
    """
    Get/Set the passive flag of a process.

     TRUE if the process must be passive.
     FALSE if the process may be not passive.
    For a procedure declaration, set if it is passive.

    :param obj:   The node to write the ``Passive_Flag`` field of.
    :param value: The value to write into the ``Passive_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_resolution_function_flag")
def Get_Resolution_Function_Flag(obj: Iir) -> Boolean:
    """
    True if the function is used as a resolution function.

    :param obj: The node to read the ``Resolution_Function_Flag`` field of.
    :returns:   The node's ``Resolution_Function_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_resolution_function_flag")
def Set_Resolution_Function_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if the function is used as a resolution function.

    :param obj:   The node to write the ``Resolution_Function_Flag`` field of.
    :param value: The value to write into the ``Resolution_Function_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_wait_state")
def Get_Wait_State(obj: Iir) -> TriStateType:
    """
    Get/Set the wait state of the current subprogram or process. TRUE if it contains a wait statement, either directly
    or

     indirectly.
    FALSE if it doesn't contain a wait statement. UNKNOWN if the wait status is not yet known.

    :param obj: The node to read the ``Wait_State`` field of.
    :returns:   The node's ``Wait_State`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_wait_state")
def Set_Wait_State(obj: Iir, value: TriStateType) -> None:
    """
    Get/Set the wait state of the current subprogram or process. TRUE if it contains a wait statement, either directly
    or

     indirectly.
    FALSE if it doesn't contain a wait statement. UNKNOWN if the wait status is not yet known.

    :param obj:   The node to write the ``Wait_State`` field of.
    :param value: The value to write into the ``Wait_State`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_all_sensitized_state")
def Get_All_Sensitized_State(obj: Iir) -> Iir:
    """
    Get/Set whether the subprogram may be called by a sensitized process whose sensitivity list is ALL. FALSE if
    declared in a package unit and reads a signal that is not

      one of its interface, or if it calls such a subprogram.
    TRUE if it doesn't call a subprogram whose state is False and

      either doesn't read a signal or declared within an entity or
      architecture.
    UNKNOWN if the status is not yet known.

    :param obj: The node to read the ``All_Sensitized_State`` field of.
    :returns:   The node's ``All_Sensitized_State`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_all_sensitized_state")
def Set_All_Sensitized_State(obj: Iir, value: Iir) -> None:
    """
    Get/Set whether the subprogram may be called by a sensitized process whose sensitivity list is ALL. FALSE if
    declared in a package unit and reads a signal that is not

      one of its interface, or if it calls such a subprogram.
    TRUE if it doesn't call a subprogram whose state is False and

      either doesn't read a signal or declared within an entity or
      architecture.
    UNKNOWN if the status is not yet known.

    :param obj:   The node to write the ``All_Sensitized_State`` field of.
    :param value: The value to write into the ``All_Sensitized_State`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_seen_flag")
def Get_Seen_Flag(obj: Iir) -> Boolean:
    """
    Get/Set the seen flag. Used when the graph of callees is walked, to avoid infinite loops, since the graph is not a
    DAG (there may be cycles).

    :param obj: The node to read the ``Seen_Flag`` field of.
    :returns:   The node's ``Seen_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_seen_flag")
def Set_Seen_Flag(obj: Iir, value: Boolean) -> None:
    """
    Get/Set the seen flag. Used when the graph of callees is walked, to avoid infinite loops, since the graph is not a
    DAG (there may be cycles).

    :param obj:   The node to write the ``Seen_Flag`` field of.
    :param value: The value to write into the ``Seen_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_pure_flag")
def Get_Pure_Flag(obj: Iir) -> Boolean:
    """
    Get/Set the pure flag of a function. TRUE if the function is declared pure. FALSE if the function is declared
    impure.

    :param obj: The node to read the ``Pure_Flag`` field of.
    :returns:   The node's ``Pure_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_pure_flag")
def Set_Pure_Flag(obj: Iir, value: Boolean) -> None:
    """
    Get/Set the pure flag of a function. TRUE if the function is declared pure. FALSE if the function is declared
    impure.

    :param obj:   The node to write the ``Pure_Flag`` field of.
    :param value: The value to write into the ``Pure_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_foreign_flag")
def Get_Foreign_Flag(obj: Iir) -> Boolean:
    """
    Get/Set the foreign flag of a declaration. TRUE if the declaration was decorated with the std.foreign attribute.

    :param obj: The node to read the ``Foreign_Flag`` field of.
    :returns:   The node's ``Foreign_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_foreign_flag")
def Set_Foreign_Flag(obj: Iir, value: Boolean) -> None:
    """
    Get/Set the foreign flag of a declaration. TRUE if the declaration was decorated with the std.foreign attribute.

    :param obj:   The node to write the ``Foreign_Flag`` field of.
    :param value: The value to write into the ``Foreign_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_resolved_flag")
def Get_Resolved_Flag(obj: Iir) -> Boolean:
    """
    Get/Set the resolved flag of a subtype definition. A subtype definition may be resolved either because a
    resolution_indication is present in the subtype_indication, or because all elements type are resolved.

    :param obj: The node to read the ``Resolved_Flag`` field of.
    :returns:   The node's ``Resolved_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_resolved_flag")
def Set_Resolved_Flag(obj: Iir, value: Boolean) -> None:
    """
    Get/Set the resolved flag of a subtype definition. A subtype definition may be resolved either because a
    resolution_indication is present in the subtype_indication, or because all elements type are resolved.

    :param obj:   The node to write the ``Resolved_Flag`` field of.
    :param value: The value to write into the ``Resolved_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_signal_type_flag")
def Get_Signal_Type_Flag(obj: Iir) -> Boolean:
    """
    Get/Set the signal_type flag of a type/subtype definition. This flags indicates whether the type can be used as a
    signal type. Access types, file types and composite types whose a sub-element is an access type cannot be used as a
    signal type.

    :param obj: The node to read the ``Signal_Type_Flag`` field of.
    :returns:   The node's ``Signal_Type_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_signal_type_flag")
def Set_Signal_Type_Flag(obj: Iir, value: Boolean) -> None:
    """
    Get/Set the signal_type flag of a type/subtype definition. This flags indicates whether the type can be used as a
    signal type. Access types, file types and composite types whose a sub-element is an access type cannot be used as a
    signal type.

    :param obj:   The node to write the ``Signal_Type_Flag`` field of.
    :param value: The value to write into the ``Signal_Type_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_signal_flag")
def Get_Has_Signal_Flag(obj: Iir) -> Boolean:
    """
    True if ATYPE is used to declare a signal or to handle a signal

     (such as slice or aliases).

    :param obj: The node to read the ``Has_Signal_Flag`` field of.
    :returns:   The node's ``Has_Signal_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_signal_flag")
def Set_Has_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if ATYPE is used to declare a signal or to handle a signal

     (such as slice or aliases).

    :param obj:   The node to write the ``Has_Signal_Flag`` field of.
    :param value: The value to write into the ``Has_Signal_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_purity_state")
def Get_Purity_State(obj: Iir) -> Iir:
    """
    Get/Set the purity status of a subprogram.

    :param obj: The node to read the ``Purity_State`` field of.
    :returns:   The node's ``Purity_State`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_purity_state")
def Set_Purity_State(obj: Iir, value: Iir) -> None:
    """
    Get/Set the purity status of a subprogram.

    :param obj:   The node to write the ``Purity_State`` field of.
    :param value: The value to write into the ``Purity_State`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_elab_flag")
def Get_Elab_Flag(obj: Iir) -> Boolean:
    """
    Set during binding when DESIGN is added in a list of file to bind.

    :param obj: The node to read the ``Elab_Flag`` field of.
    :returns:   The node's ``Elab_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_elab_flag")
def Set_Elab_Flag(obj: Iir, value: Boolean) -> None:
    """
    Set during binding when DESIGN is added in a list of file to bind.

    :param obj:   The node to write the ``Elab_Flag`` field of.
    :param value: The value to write into the ``Elab_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_vendor_library_flag")
def Get_Vendor_Library_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Vendor_Library_Flag`` field of.
    :returns:   The node's ``Vendor_Library_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_vendor_library_flag")
def Set_Vendor_Library_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Vendor_Library_Flag`` field of.
    :param value: The value to write into the ``Vendor_Library_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_mark_flag")
def Get_Configuration_Mark_Flag(obj: Iir) -> Boolean:
    """
    Used only by configuration to mark a design unit as already inserted in the list of units.  Used to avoid double
    insertion.

    :param obj: The node to read the ``Configuration_Mark_Flag`` field of.
    :returns:   The node's ``Configuration_Mark_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_mark_flag")
def Set_Configuration_Mark_Flag(obj: Iir, value: Boolean) -> None:
    """
    Used only by configuration to mark a design unit as already inserted in the list of units.  Used to avoid double
    insertion.

    :param obj:   The node to write the ``Configuration_Mark_Flag`` field of.
    :param value: The value to write into the ``Configuration_Mark_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_done_flag")
def Get_Configuration_Done_Flag(obj: Iir) -> Boolean:
    """
    Used only by configuration to flag units completely handled.  Used to detect recursion.

    :param obj: The node to read the ``Configuration_Done_Flag`` field of.
    :returns:   The node's ``Configuration_Done_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_done_flag")
def Set_Configuration_Done_Flag(obj: Iir, value: Boolean) -> None:
    """
    Used only by configuration to flag units completely handled.  Used to detect recursion.

    :param obj:   The node to write the ``Configuration_Done_Flag`` field of.
    :param value: The value to write into the ``Configuration_Done_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_index_constraint_flag")
def Get_Index_Constraint_Flag(obj: Iir) -> Boolean:
    """
    Set on an array_subtype if there is an index constraint. If not set, the subtype is unconstrained.

    :param obj: The node to read the ``Index_Constraint_Flag`` field of.
    :returns:   The node's ``Index_Constraint_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_index_constraint_flag")
def Set_Index_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """
    Set on an array_subtype if there is an index constraint. If not set, the subtype is unconstrained.

    :param obj:   The node to write the ``Index_Constraint_Flag`` field of.
    :param value: The value to write into the ``Index_Constraint_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_hide_implicit_flag")
def Get_Hide_Implicit_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Hide_Implicit_Flag`` field of.
    :returns:   The node's ``Hide_Implicit_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_hide_implicit_flag")
def Set_Hide_Implicit_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Hide_Implicit_Flag`` field of.
    :param value: The value to write into the ``Hide_Implicit_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_assertion_condition")
def Get_Assertion_Condition(obj: Iir) -> Iir:
    """
    Condition of an assertion.

    :param obj: The node to read the ``Assertion_Condition`` field of.
    :returns:   The node's ``Assertion_Condition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_assertion_condition")
def Set_Assertion_Condition(obj: Iir, value: Iir) -> None:
    """
    Condition of an assertion.

    :param obj:   The node to write the ``Assertion_Condition`` field of.
    :param value: The value to write into the ``Assertion_Condition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_report_expression")
def Get_Report_Expression(obj: Iir) -> Iir:
    """
    Report expression of an assertion or report statement.

    :param obj: The node to read the ``Report_Expression`` field of.
    :returns:   The node's ``Report_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_report_expression")
def Set_Report_Expression(obj: Iir, value: Iir) -> None:
    """
    Report expression of an assertion or report statement.

    :param obj:   The node to write the ``Report_Expression`` field of.
    :param value: The value to write into the ``Report_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_severity_expression")
def Get_Severity_Expression(obj: Iir) -> Iir:
    """
    Severity expression of an assertion or report statement.

    :param obj: The node to read the ``Severity_Expression`` field of.
    :returns:   The node's ``Severity_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_severity_expression")
def Set_Severity_Expression(obj: Iir, value: Iir) -> None:
    """
    Severity expression of an assertion or report statement.

    :param obj:   The node to write the ``Severity_Expression`` field of.
    :param value: The value to write into the ``Severity_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_instantiated_unit")
def Get_Instantiated_Unit(obj: Iir) -> Iir:
    """
    Instantiated unit of a component instantiation statement.

    :param obj: The node to read the ``Instantiated_Unit`` field of.
    :returns:   The node's ``Instantiated_Unit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_instantiated_unit")
def Set_Instantiated_Unit(obj: Iir, value: Iir) -> None:
    """
    Instantiated unit of a component instantiation statement.

    :param obj:   The node to write the ``Instantiated_Unit`` field of.
    :param value: The value to write into the ``Instantiated_Unit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_instantiated_header")
def Get_Instantiated_Header(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Instantiated_Header`` field of.
    :returns:   The node's ``Instantiated_Header`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_instantiated_header")
def Set_Instantiated_Header(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Instantiated_Header`` field of.
    :param value: The value to write into the ``Instantiated_Header`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_generic_map_aspect_chain")
def Get_Generic_Map_Aspect_Chain(obj: Iir) -> Iir:
    """
    Generic map aspect list.

    :param obj: The node to read the ``Generic_Map_Aspect_Chain`` field of.
    :returns:   The node's ``Generic_Map_Aspect_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_generic_map_aspect_chain")
def Set_Generic_Map_Aspect_Chain(obj: Iir, value: Iir) -> None:
    """
    Generic map aspect list.

    :param obj:   The node to write the ``Generic_Map_Aspect_Chain`` field of.
    :param value: The value to write into the ``Generic_Map_Aspect_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_port_map_aspect_chain")
def Get_Port_Map_Aspect_Chain(obj: Iir) -> Iir:
    """
    Port map aspect list.

    :param obj: The node to read the ``Port_Map_Aspect_Chain`` field of.
    :returns:   The node's ``Port_Map_Aspect_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_port_map_aspect_chain")
def Set_Port_Map_Aspect_Chain(obj: Iir, value: Iir) -> None:
    """
    Port map aspect list.

    :param obj:   The node to write the ``Port_Map_Aspect_Chain`` field of.
    :param value: The value to write into the ``Port_Map_Aspect_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_name")
def Get_Configuration_Name(obj: Iir) -> Iir:
    """
    Configuration of an entity_aspect_configuration.

    :param obj: The node to read the ``Configuration_Name`` field of.
    :returns:   The node's ``Configuration_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_name")
def Set_Configuration_Name(obj: Iir, value: Iir) -> None:
    """
    Configuration of an entity_aspect_configuration.

    :param obj:   The node to write the ``Configuration_Name`` field of.
    :param value: The value to write into the ``Configuration_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_component_configuration")
def Get_Component_Configuration(obj: Iir) -> Iir:
    """
    Component configuration for a component_instantiation_statement.

    :param obj: The node to read the ``Component_Configuration`` field of.
    :returns:   The node's ``Component_Configuration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_component_configuration")
def Set_Component_Configuration(obj: Iir, value: Iir) -> None:
    """
    Component configuration for a component_instantiation_statement.

    :param obj:   The node to write the ``Component_Configuration`` field of.
    :param value: The value to write into the ``Component_Configuration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_specification")
def Get_Configuration_Specification(obj: Iir) -> Iir:
    """
    Configuration specification for a component_instantiation_statement.

    :param obj: The node to read the ``Configuration_Specification`` field of.
    :returns:   The node's ``Configuration_Specification`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_specification")
def Set_Configuration_Specification(obj: Iir, value: Iir) -> None:
    """
    Configuration specification for a component_instantiation_statement.

    :param obj:   The node to write the ``Configuration_Specification`` field of.
    :param value: The value to write into the ``Configuration_Specification`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_default_binding_indication")
def Get_Default_Binding_Indication(obj: Iir) -> Iir:
    """
    Set/Get the default binding indication of a configuration specification or a component configuration.

    :param obj: The node to read the ``Default_Binding_Indication`` field of.
    :returns:   The node's ``Default_Binding_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_default_binding_indication")
def Set_Default_Binding_Indication(obj: Iir, value: Iir) -> None:
    """
    Set/Get the default binding indication of a configuration specification or a component configuration.

    :param obj:   The node to write the ``Default_Binding_Indication`` field of.
    :param value: The value to write into the ``Default_Binding_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_default_configuration_declaration")
def Get_Default_Configuration_Declaration(obj: Iir) -> Iir:
    """
    Set/Get the default configuration of an architecture.

    :param obj: The node to read the ``Default_Configuration_Declaration`` field of.
    :returns:   The node's ``Default_Configuration_Declaration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_default_configuration_declaration")
def Set_Default_Configuration_Declaration(obj: Iir, value: Iir) -> None:
    """
    Set/Get the default configuration of an architecture.

    :param obj:   The node to write the ``Default_Configuration_Declaration`` field of.
    :param value: The value to write into the ``Default_Configuration_Declaration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_expression")
def Get_Expression(obj: Iir) -> Iir:
    """
    Expression for an various nodes.

    :param obj: The node to read the ``Expression`` field of.
    :returns:   The node's ``Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_expression")
def Set_Expression(obj: Iir, value: Iir) -> None:
    """
    Expression for an various nodes.

    :param obj:   The node to write the ``Expression`` field of.
    :param value: The value to write into the ``Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_conditional_expression_chain")
def Get_Conditional_Expression_Chain(obj: Iir) -> Iir:
    """
    A conditional expression. Node kind is a Iir_Kind_Conditional_Expression.

    :param obj: The node to read the ``Conditional_Expression_Chain`` field of.
    :returns:   The node's ``Conditional_Expression_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_conditional_expression_chain")
def Set_Conditional_Expression_Chain(obj: Iir, value: Iir) -> None:
    """
    A conditional expression. Node kind is a Iir_Kind_Conditional_Expression.

    :param obj:   The node to write the ``Conditional_Expression_Chain`` field of.
    :param value: The value to write into the ``Conditional_Expression_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_allocator_designated_type")
def Get_Allocator_Designated_Type(obj: Iir) -> Iir:
    """
    Set to the designated type (either the type of the expression or the subtype) when the expression is analyzed.

    :param obj: The node to read the ``Allocator_Designated_Type`` field of.
    :returns:   The node's ``Allocator_Designated_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_allocator_designated_type")
def Set_Allocator_Designated_Type(obj: Iir, value: Iir) -> None:
    """
    Set to the designated type (either the type of the expression or the subtype) when the expression is analyzed.

    :param obj:   The node to write the ``Allocator_Designated_Type`` field of.
    :param value: The value to write into the ``Allocator_Designated_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_selected_waveform_chain")
def Get_Selected_Waveform_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Selected_Waveform_Chain`` field of.
    :returns:   The node's ``Selected_Waveform_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_selected_waveform_chain")
def Set_Selected_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Selected_Waveform_Chain`` field of.
    :param value: The value to write into the ``Selected_Waveform_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_selected_expressions_chain")
def Get_Selected_Expressions_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Selected_Expressions_Chain`` field of.
    :returns:   The node's ``Selected_Expressions_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_selected_expressions_chain")
def Set_Selected_Expressions_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Selected_Expressions_Chain`` field of.
    :param value: The value to write into the ``Selected_Expressions_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_conditional_waveform_chain")
def Get_Conditional_Waveform_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Conditional_Waveform_Chain`` field of.
    :returns:   The node's ``Conditional_Waveform_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_conditional_waveform_chain")
def Set_Conditional_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Conditional_Waveform_Chain`` field of.
    :param value: The value to write into the ``Conditional_Waveform_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_guard_expression")
def Get_Guard_Expression(obj: Iir) -> Iir:
    """
    Expression defining the value of the implicit guard signal.

    :param obj: The node to read the ``Guard_Expression`` field of.
    :returns:   The node's ``Guard_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_guard_expression")
def Set_Guard_Expression(obj: Iir, value: Iir) -> None:
    """
    Expression defining the value of the implicit guard signal.

    :param obj:   The node to write the ``Guard_Expression`` field of.
    :param value: The value to write into the ``Guard_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_guard_decl")
def Get_Guard_Decl(obj: Iir) -> Iir:
    """
    The declaration (if any) of the implicit guard signal of a block statement.

    :param obj: The node to read the ``Guard_Decl`` field of.
    :returns:   The node's ``Guard_Decl`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_guard_decl")
def Set_Guard_Decl(obj: Iir, value: Iir) -> None:
    """
    The declaration (if any) of the implicit guard signal of a block statement.

    :param obj:   The node to write the ``Guard_Decl`` field of.
    :param value: The value to write into the ``Guard_Decl`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_guard_sensitivity_list")
def Get_Guard_Sensitivity_List(obj: Iir) -> Iir:
    """
    Sensitivity list for the implicit guard signal.

    :param obj: The node to read the ``Guard_Sensitivity_List`` field of.
    :returns:   The node's ``Guard_Sensitivity_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_guard_sensitivity_list")
def Set_Guard_Sensitivity_List(obj: Iir, value: Iir) -> None:
    """
    Sensitivity list for the implicit guard signal.

    :param obj:   The node to write the ``Guard_Sensitivity_List`` field of.
    :param value: The value to write into the ``Guard_Sensitivity_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_implicit_chain")
def Get_Attribute_Implicit_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Attribute_Implicit_Chain`` field of.
    :returns:   The node's ``Attribute_Implicit_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_implicit_chain")
def Set_Attribute_Implicit_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Attribute_Implicit_Chain`` field of.
    :param value: The value to write into the ``Attribute_Implicit_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_block_block_configuration")
def Get_Block_Block_Configuration(obj: Iir) -> Iir:
    """
    Block_Configuration that applies to this block statement.

    :param obj: The node to read the ``Block_Block_Configuration`` field of.
    :returns:   The node's ``Block_Block_Configuration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_block_block_configuration")
def Set_Block_Block_Configuration(obj: Iir, value: Iir) -> None:
    """
    Block_Configuration that applies to this block statement.

    :param obj:   The node to write the ``Block_Block_Configuration`` field of.
    :param value: The value to write into the ``Block_Block_Configuration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_package_header")
def Get_Package_Header(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Package_Header`` field of.
    :returns:   The node's ``Package_Header`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_package_header")
def Set_Package_Header(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Package_Header`` field of.
    :param value: The value to write into the ``Package_Header`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_block_header")
def Get_Block_Header(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Block_Header`` field of.
    :returns:   The node's ``Block_Header`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_block_header")
def Set_Block_Header(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Block_Header`` field of.
    :param value: The value to write into the ``Block_Header`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_package_name")
def Get_Uninstantiated_Package_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Uninstantiated_Package_Name`` field of.
    :returns:   The node's ``Uninstantiated_Package_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_package_name")
def Set_Uninstantiated_Package_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Uninstantiated_Package_Name`` field of.
    :param value: The value to write into the ``Uninstantiated_Package_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_package_decl")
def Get_Uninstantiated_Package_Decl(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Uninstantiated_Package_Decl`` field of.
    :returns:   The node's ``Uninstantiated_Package_Decl`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_package_decl")
def Set_Uninstantiated_Package_Decl(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Uninstantiated_Package_Decl`` field of.
    :param value: The value to write into the ``Uninstantiated_Package_Decl`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_associated_package")
def Get_Associated_Package(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Associated_Package`` field of.
    :returns:   The node's ``Associated_Package`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_associated_package")
def Set_Associated_Package(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Associated_Package`` field of.
    :param value: The value to write into the ``Associated_Package`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_instance_source_file")
def Get_Instance_Source_File(obj: Iir) -> SourceFileEntry:
    """
    The created pseudo-file for relocating the instantiated nodes (generics and declarations).

    :param obj: The node to read the ``Instance_Source_File`` field of.
    :returns:   The node's ``Instance_Source_File`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_instance_source_file")
def Set_Instance_Source_File(obj: Iir, value: SourceFileEntry) -> None:
    """
    The created pseudo-file for relocating the instantiated nodes (generics and declarations).

    :param obj:   The node to write the ``Instance_Source_File`` field of.
    :param value: The value to write into the ``Instance_Source_File`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_generate_block_configuration")
def Get_Generate_Block_Configuration(obj: Iir) -> Iir:
    """
    Get/Set the block_configuration (there may be several block_configuration through the use of prev_configuration
    singly linked list) that apply to this generate statement.

    :param obj: The node to read the ``Generate_Block_Configuration`` field of.
    :returns:   The node's ``Generate_Block_Configuration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_generate_block_configuration")
def Set_Generate_Block_Configuration(obj: Iir, value: Iir) -> None:
    """
    Get/Set the block_configuration (there may be several block_configuration through the use of prev_configuration
    singly linked list) that apply to this generate statement.

    :param obj:   The node to write the ``Generate_Block_Configuration`` field of.
    :param value: The value to write into the ``Generate_Block_Configuration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_generate_statement_body")
def Get_Generate_Statement_Body(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Generate_Statement_Body`` field of.
    :returns:   The node's ``Generate_Statement_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_generate_statement_body")
def Set_Generate_Statement_Body(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Generate_Statement_Body`` field of.
    :param value: The value to write into the ``Generate_Statement_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_alternative_label")
def Get_Alternative_Label(obj: Iir) -> NameId:
    """
    :param obj: The node to read the ``Alternative_Label`` field of.
    :returns:   The node's ``Alternative_Label`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_alternative_label")
def Set_Alternative_Label(obj: Iir, value: NameId) -> None:
    """
    :param obj:   The node to write the ``Alternative_Label`` field of.
    :param value: The value to write into the ``Alternative_Label`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_generate_else_clause")
def Get_Generate_Else_Clause(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Generate_Else_Clause`` field of.
    :returns:   The node's ``Generate_Else_Clause`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_generate_else_clause")
def Set_Generate_Else_Clause(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Generate_Else_Clause`` field of.
    :param value: The value to write into the ``Generate_Else_Clause`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_condition")
def Get_Condition(obj: Iir) -> Iir:
    """
    Condition of a conditional_waveform, if_statement, elsif, while_loop_statement, next_statement or exit_statement.

    :param obj: The node to read the ``Condition`` field of.
    :returns:   The node's ``Condition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_condition")
def Set_Condition(obj: Iir, value: Iir) -> None:
    """
    Condition of a conditional_waveform, if_statement, elsif, while_loop_statement, next_statement or exit_statement.

    :param obj:   The node to write the ``Condition`` field of.
    :param value: The value to write into the ``Condition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_else_clause")
def Get_Else_Clause(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Else_Clause`` field of.
    :returns:   The node's ``Else_Clause`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_else_clause")
def Set_Else_Clause(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Else_Clause`` field of.
    :param value: The value to write into the ``Else_Clause`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_specification")
def Get_Parameter_Specification(obj: Iir) -> Iir:
    """
    Iterator of a for_loop_statement.

    :param obj: The node to read the ``Parameter_Specification`` field of.
    :returns:   The node's ``Parameter_Specification`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_specification")
def Set_Parameter_Specification(obj: Iir, value: Iir) -> None:
    """
    Iterator of a for_loop_statement.

    :param obj:   The node to write the ``Parameter_Specification`` field of.
    :param value: The value to write into the ``Parameter_Specification`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parent")
def Get_Parent(obj: Iir) -> Iir:
    """
    Get/Set the statement in which TARGET appears.  This is used to check if next/exit is in a loop.

    :param obj: The node to read the ``Parent`` field of.
    :returns:   The node's ``Parent`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parent")
def Set_Parent(obj: Iir, value: Iir) -> None:
    """
    Get/Set the statement in which TARGET appears.  This is used to check if next/exit is in a loop.

    :param obj:   The node to write the ``Parent`` field of.
    :param value: The value to write into the ``Parent`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_loop_label")
def Get_Loop_Label(obj: Iir) -> Iir:
    """
    Loop label for an exit_statement or next_statement.

    :param obj: The node to read the ``Loop_Label`` field of.
    :returns:   The node's ``Loop_Label`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_loop_label")
def Set_Loop_Label(obj: Iir, value: Iir) -> None:
    """
    Loop label for an exit_statement or next_statement.

    :param obj:   The node to write the ``Loop_Label`` field of.
    :param value: The value to write into the ``Loop_Label`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_exit_flag")
def Get_Exit_Flag(obj: Iir) -> Boolean:
    """
    True if there is an exit statement targeting this loop statement.

    :param obj: The node to read the ``Exit_Flag`` field of.
    :returns:   The node's ``Exit_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_exit_flag")
def Set_Exit_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if there is an exit statement targeting this loop statement.

    :param obj:   The node to write the ``Exit_Flag`` field of.
    :param value: The value to write into the ``Exit_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_next_flag")
def Get_Next_Flag(obj: Iir) -> Boolean:
    """
    True if there is a next statement targeting this loop statement.

    :param obj: The node to read the ``Next_Flag`` field of.
    :returns:   The node's ``Next_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_next_flag")
def Set_Next_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if there is a next statement targeting this loop statement.

    :param obj:   The node to write the ``Next_Flag`` field of.
    :param value: The value to write into the ``Next_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_component_name")
def Get_Component_Name(obj: Iir) -> Iir:
    """
    Component name for a component_configuration or a configuration_specification.

    :param obj: The node to read the ``Component_Name`` field of.
    :returns:   The node's ``Component_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_component_name")
def Set_Component_Name(obj: Iir, value: Iir) -> None:
    """
    Component name for a component_configuration or a configuration_specification.

    :param obj:   The node to write the ``Component_Name`` field of.
    :param value: The value to write into the ``Component_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_instantiation_list")
def Get_Instantiation_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Instantiation_List`` field of.
    :returns:   The node's ``Instantiation_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_instantiation_list")
def Set_Instantiation_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Instantiation_List`` field of.
    :param value: The value to write into the ``Instantiation_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_entity_aspect")
def Get_Entity_Aspect(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Entity_Aspect`` field of.
    :returns:   The node's ``Entity_Aspect`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_entity_aspect")
def Set_Entity_Aspect(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Entity_Aspect`` field of.
    :param value: The value to write into the ``Entity_Aspect`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_default_entity_aspect")
def Get_Default_Entity_Aspect(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Default_Entity_Aspect`` field of.
    :returns:   The node's ``Default_Entity_Aspect`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_default_entity_aspect")
def Set_Default_Entity_Aspect(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Default_Entity_Aspect`` field of.
    :param value: The value to write into the ``Default_Entity_Aspect`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_binding_indication")
def Get_Binding_Indication(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Binding_Indication`` field of.
    :returns:   The node's ``Binding_Indication`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_binding_indication")
def Set_Binding_Indication(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Binding_Indication`` field of.
    :param value: The value to write into the ``Binding_Indication`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_named_entity")
def Get_Named_Entity(obj: Iir) -> Iir:
    """
    The named entity designated by a name.

    :param obj: The node to read the ``Named_Entity`` field of.
    :returns:   The node's ``Named_Entity`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_named_entity")
def Set_Named_Entity(obj: Iir, value: Iir) -> None:
    """
    The named entity designated by a name.

    :param obj:   The node to write the ``Named_Entity`` field of.
    :param value: The value to write into the ``Named_Entity`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_referenced_name")
def Get_Referenced_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Referenced_Name`` field of.
    :returns:   The node's ``Referenced_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_referenced_name")
def Set_Referenced_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Referenced_Name`` field of.
    :param value: The value to write into the ``Referenced_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_expr_staticness")
def Get_Expr_Staticness(obj: Iir) -> Iir:
    """
    Expression staticness, defined by rules of LRM 7.4

    :param obj: The node to read the ``Expr_Staticness`` field of.
    :returns:   The node's ``Expr_Staticness`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_expr_staticness")
def Set_Expr_Staticness(obj: Iir, value: Iir) -> None:
    """
    Expression staticness, defined by rules of LRM 7.4

    :param obj:   The node to write the ``Expr_Staticness`` field of.
    :param value: The value to write into the ``Expr_Staticness`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_scalar_size")
def Get_Scalar_Size(obj: Iir) -> ScalarSize:
    """
    :param obj: The node to read the ``Scalar_Size`` field of.
    :returns:   The node's ``Scalar_Size`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_scalar_size")
def Set_Scalar_Size(obj: Iir, value: ScalarSize) -> None:
    """
    :param obj:   The node to write the ``Scalar_Size`` field of.
    :param value: The value to write into the ``Scalar_Size`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_error_origin")
def Get_Error_Origin(obj: Iir) -> Iir:
    """
    Node which couldn't be correctly analyzed.

    :param obj: The node to read the ``Error_Origin`` field of.
    :returns:   The node's ``Error_Origin`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_error_origin")
def Set_Error_Origin(obj: Iir, value: Iir) -> None:
    """
    Node which couldn't be correctly analyzed.

    :param obj:   The node to write the ``Error_Origin`` field of.
    :param value: The value to write into the ``Error_Origin`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_operand")
def Get_Operand(obj: Iir) -> Iir:
    """
    Operand of a monadic operator.

    :param obj: The node to read the ``Operand`` field of.
    :returns:   The node's ``Operand`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_operand")
def Set_Operand(obj: Iir, value: Iir) -> None:
    """
    Operand of a monadic operator.

    :param obj:   The node to write the ``Operand`` field of.
    :param value: The value to write into the ``Operand`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_left")
def Get_Left(obj: Iir) -> Iir:
    """
    Left operand of a dyadic operator.

    :param obj: The node to read the ``Left`` field of.
    :returns:   The node's ``Left`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_left")
def Set_Left(obj: Iir, value: Iir) -> None:
    """
    Left operand of a dyadic operator.

    :param obj:   The node to write the ``Left`` field of.
    :param value: The value to write into the ``Left`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_right")
def Get_Right(obj: Iir) -> Iir:
    """
    Right operand of a dyadic operator.

    :param obj: The node to read the ``Right`` field of.
    :returns:   The node's ``Right`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_right")
def Set_Right(obj: Iir, value: Iir) -> None:
    """
    Right operand of a dyadic operator.

    :param obj:   The node to write the ``Right`` field of.
    :param value: The value to write into the ``Right`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_unit_name")
def Get_Unit_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Unit_Name`` field of.
    :returns:   The node's ``Unit_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_unit_name")
def Set_Unit_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Unit_Name`` field of.
    :param value: The value to write into the ``Unit_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_name")
def Get_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Name`` field of.
    :returns:   The node's ``Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_name")
def Set_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Name`` field of.
    :param value: The value to write into the ``Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_group_template_name")
def Get_Group_Template_Name(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Group_Template_Name`` field of.
    :returns:   The node's ``Group_Template_Name`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_group_template_name")
def Set_Group_Template_Name(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Group_Template_Name`` field of.
    :param value: The value to write into the ``Group_Template_Name`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_name_staticness")
def Get_Name_Staticness(obj: Iir) -> Iir:
    """
    Staticness of a name, according to rules of LRM 6.1

    :param obj: The node to read the ``Name_Staticness`` field of.
    :returns:   The node's ``Name_Staticness`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_name_staticness")
def Set_Name_Staticness(obj: Iir, value: Iir) -> None:
    """
    Staticness of a name, according to rules of LRM 6.1

    :param obj:   The node to write the ``Name_Staticness`` field of.
    :param value: The value to write into the ``Name_Staticness`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_prefix")
def Get_Prefix(obj: Iir) -> Iir:
    """
    Prefix of a name.

    :param obj: The node to read the ``Prefix`` field of.
    :returns:   The node's ``Prefix`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_prefix")
def Set_Prefix(obj: Iir, value: Iir) -> None:
    """
    Prefix of a name.

    :param obj:   The node to write the ``Prefix`` field of.
    :param value: The value to write into the ``Prefix`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_signature_prefix")
def Get_Signature_Prefix(obj: Iir) -> Iir:
    """
    Prefix of a name signature

    :param obj: The node to read the ``Signature_Prefix`` field of.
    :returns:   The node's ``Signature_Prefix`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_signature_prefix")
def Set_Signature_Prefix(obj: Iir, value: Iir) -> None:
    """
    Prefix of a name signature

    :param obj:   The node to write the ``Signature_Prefix`` field of.
    :param value: The value to write into the ``Signature_Prefix`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_external_pathname")
def Get_External_Pathname(obj: Iir) -> Iir:
    """
    External pathname for an external name.

    :param obj: The node to read the ``External_Pathname`` field of.
    :returns:   The node's ``External_Pathname`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_external_pathname")
def Set_External_Pathname(obj: Iir, value: Iir) -> None:
    """
    External pathname for an external name.

    :param obj:   The node to write the ``External_Pathname`` field of.
    :param value: The value to write into the ``External_Pathname`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_pathname_suffix")
def Get_Pathname_Suffix(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Pathname_Suffix`` field of.
    :returns:   The node's ``Pathname_Suffix`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_pathname_suffix")
def Set_Pathname_Suffix(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Pathname_Suffix`` field of.
    :param value: The value to write into the ``Pathname_Suffix`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_pathname_expression")
def Get_Pathname_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Pathname_Expression`` field of.
    :returns:   The node's ``Pathname_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_pathname_expression")
def Set_Pathname_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Pathname_Expression`` field of.
    :param value: The value to write into the ``Pathname_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_in_formal_flag")
def Get_In_Formal_Flag(obj: Iir) -> Boolean:
    """
    True if the name appears in a formal_part.  In that case, some checks must be disabled (eg: the expression of a type
    conversion can be a write-only interface).

    :param obj: The node to read the ``In_Formal_Flag`` field of.
    :returns:   The node's ``In_Formal_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_in_formal_flag")
def Set_In_Formal_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if the name appears in a formal_part.  In that case, some checks must be disabled (eg: the expression of a type
    conversion can be a write-only interface).

    :param obj:   The node to write the ``In_Formal_Flag`` field of.
    :param value: The value to write into the ``In_Formal_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_inertial_flag")
def Get_Inertial_Flag(obj: Iir) -> Boolean:
    """
    True iff the association is an internal association.

    :param obj: The node to read the ``Inertial_Flag`` field of.
    :returns:   The node's ``Inertial_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_inertial_flag")
def Set_Inertial_Flag(obj: Iir, value: Boolean) -> None:
    """
    True iff the association is an internal association.

    :param obj:   The node to write the ``Inertial_Flag`` field of.
    :param value: The value to write into the ``Inertial_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_slice_subtype")
def Get_Slice_Subtype(obj: Iir) -> Iir:
    """
    The subtype of a slice.  Contrary to the Type field, this is not a reference.

    :param obj: The node to read the ``Slice_Subtype`` field of.
    :returns:   The node's ``Slice_Subtype`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_slice_subtype")
def Set_Slice_Subtype(obj: Iir, value: Iir) -> None:
    """
    The subtype of a slice.  Contrary to the Type field, this is not a reference.

    :param obj:   The node to write the ``Slice_Subtype`` field of.
    :param value: The value to write into the ``Slice_Subtype`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_suffix")
def Get_Suffix(obj: Iir) -> Iir:
    """
    Suffix of a slice.

    :param obj: The node to read the ``Suffix`` field of.
    :returns:   The node's ``Suffix`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_suffix")
def Set_Suffix(obj: Iir, value: Iir) -> None:
    """
    Suffix of a slice.

    :param obj:   The node to write the ``Suffix`` field of.
    :param value: The value to write into the ``Suffix`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype")
def Get_Index_Subtype(obj: Iir) -> Iir:
    """
    Set the designated index subtype of an array attribute.

    :param obj: The node to read the ``Index_Subtype`` field of.
    :returns:   The node's ``Index_Subtype`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype")
def Set_Index_Subtype(obj: Iir, value: Iir) -> None:
    """
    Set the designated index subtype of an array attribute.

    :param obj:   The node to write the ``Index_Subtype`` field of.
    :param value: The value to write into the ``Index_Subtype`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parameter")
def Get_Parameter(obj: Iir) -> Iir:
    """
    Parameter of an attribute.

    :param obj: The node to read the ``Parameter`` field of.
    :returns:   The node's ``Parameter`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parameter")
def Set_Parameter(obj: Iir, value: Iir) -> None:
    """
    Parameter of an attribute.

    :param obj:   The node to write the ``Parameter`` field of.
    :param value: The value to write into the ``Parameter`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_2")
def Get_Parameter_2(obj: Iir) -> Iir:
    """
    Second parameter of an attribute (for AMS VHDL).

    :param obj: The node to read the ``Parameter_2`` field of.
    :returns:   The node's ``Parameter_2`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_2")
def Set_Parameter_2(obj: Iir, value: Iir) -> None:
    """
    Second parameter of an attribute (for AMS VHDL).

    :param obj:   The node to write the ``Parameter_2`` field of.
    :param value: The value to write into the ``Parameter_2`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_3")
def Get_Parameter_3(obj: Iir) -> Iir:
    """
    Third parameter of an attribute (for AMS VHDL).

    :param obj: The node to read the ``Parameter_3`` field of.
    :returns:   The node's ``Parameter_3`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_3")
def Set_Parameter_3(obj: Iir, value: Iir) -> None:
    """
    Third parameter of an attribute (for AMS VHDL).

    :param obj:   The node to write the ``Parameter_3`` field of.
    :param value: The value to write into the ``Parameter_3`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_4")
def Get_Parameter_4(obj: Iir) -> Iir:
    """
    Fourth parameter of an attribute (for AMS VHDL).

    :param obj: The node to read the ``Parameter_4`` field of.
    :returns:   The node's ``Parameter_4`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_4")
def Set_Parameter_4(obj: Iir, value: Iir) -> None:
    """
    Fourth parameter of an attribute (for AMS VHDL).

    :param obj:   The node to write the ``Parameter_4`` field of.
    :param value: The value to write into the ``Parameter_4`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attr_chain")
def Get_Attr_Chain(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Attr_Chain`` field of.
    :returns:   The node's ``Attr_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attr_chain")
def Set_Attr_Chain(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Attr_Chain`` field of.
    :param value: The value to write into the ``Attr_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_actual_type")
def Get_Actual_Type(obj: Iir) -> Iir:
    """
    Type of the actual for an association by individual.

      Unless the formal is an unconstrained array type, this is the same as
      the formal type.
    Subtype indication for a type association.

    :param obj: The node to read the ``Actual_Type`` field of.
    :returns:   The node's ``Actual_Type`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_actual_type")
def Set_Actual_Type(obj: Iir, value: Iir) -> None:
    """
    Type of the actual for an association by individual.

      Unless the formal is an unconstrained array type, this is the same as
      the formal type.
    Subtype indication for a type association.

    :param obj:   The node to write the ``Actual_Type`` field of.
    :param value: The value to write into the ``Actual_Type`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_actual_type_definition")
def Get_Actual_Type_Definition(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Actual_Type_Definition`` field of.
    :returns:   The node's ``Actual_Type_Definition`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_actual_type_definition")
def Set_Actual_Type_Definition(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Actual_Type_Definition`` field of.
    :param value: The value to write into the ``Actual_Type_Definition`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_association_chain")
def Get_Association_Chain(obj: Iir) -> Iir:
    """
    List of individual associations for association_element_by_individual. Associations for parenthesis_name.

    :param obj: The node to read the ``Association_Chain`` field of.
    :returns:   The node's ``Association_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_association_chain")
def Set_Association_Chain(obj: Iir, value: Iir) -> None:
    """
    List of individual associations for association_element_by_individual. Associations for parenthesis_name.

    :param obj:   The node to write the ``Association_Chain`` field of.
    :param value: The value to write into the ``Association_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_individual_association_chain")
def Get_Individual_Association_Chain(obj: Iir) -> Iir:
    """
    List of choices for association_element_by_individual.

    :param obj: The node to read the ``Individual_Association_Chain`` field of.
    :returns:   The node's ``Individual_Association_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_individual_association_chain")
def Set_Individual_Association_Chain(obj: Iir, value: Iir) -> None:
    """
    List of choices for association_element_by_individual.

    :param obj:   The node to write the ``Individual_Association_Chain`` field of.
    :param value: The value to write into the ``Individual_Association_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_association_chain")
def Get_Subprogram_Association_Chain(obj: Iir) -> Iir:
    """
    Chain of implicit subprogram associations for a type association.

    :param obj: The node to read the ``Subprogram_Association_Chain`` field of.
    :returns:   The node's ``Subprogram_Association_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_association_chain")
def Set_Subprogram_Association_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of implicit subprogram associations for a type association.

    :param obj:   The node to write the ``Subprogram_Association_Chain`` field of.
    :param value: The value to write into the ``Subprogram_Association_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggregate_info")
def Get_Aggregate_Info(obj: Iir) -> Iir:
    """
    Get/Set info for the aggregate. There is one aggregate_info for for each dimension.

    :param obj: The node to read the ``Aggregate_Info`` field of.
    :returns:   The node's ``Aggregate_Info`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggregate_info")
def Set_Aggregate_Info(obj: Iir, value: Iir) -> None:
    """
    Get/Set info for the aggregate. There is one aggregate_info for for each dimension.

    :param obj:   The node to write the ``Aggregate_Info`` field of.
    :param value: The value to write into the ``Aggregate_Info`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_sub_aggregate_info")
def Get_Sub_Aggregate_Info(obj: Iir) -> Iir:
    """
    Get/Set the info node for the next dimension.

    :param obj: The node to read the ``Sub_Aggregate_Info`` field of.
    :returns:   The node's ``Sub_Aggregate_Info`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_sub_aggregate_info")
def Set_Sub_Aggregate_Info(obj: Iir, value: Iir) -> None:
    """
    Get/Set the info node for the next dimension.

    :param obj:   The node to write the ``Sub_Aggregate_Info`` field of.
    :param value: The value to write into the ``Sub_Aggregate_Info`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_dynamic_flag")
def Get_Aggr_Dynamic_Flag(obj: Iir) -> Boolean:
    """
    TRUE when the length of the aggregate is not locally static.

    :param obj: The node to read the ``Aggr_Dynamic_Flag`` field of.
    :returns:   The node's ``Aggr_Dynamic_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_dynamic_flag")
def Set_Aggr_Dynamic_Flag(obj: Iir, value: Boolean) -> None:
    """
    TRUE when the length of the aggregate is not locally static.

    :param obj:   The node to write the ``Aggr_Dynamic_Flag`` field of.
    :param value: The value to write into the ``Aggr_Dynamic_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_min_length")
def Get_Aggr_Min_Length(obj: Iir) -> Iir:
    """
    Get/Set the minimum number of elements for the lowest dimension of the aggregate or for the current dimension of a
    sub-aggregate. The real number of elements may be greater than this number if there is an 'other' choice.

    :param obj: The node to read the ``Aggr_Min_Length`` field of.
    :returns:   The node's ``Aggr_Min_Length`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_min_length")
def Set_Aggr_Min_Length(obj: Iir, value: Iir) -> None:
    """
    Get/Set the minimum number of elements for the lowest dimension of the aggregate or for the current dimension of a
    sub-aggregate. The real number of elements may be greater than this number if there is an 'other' choice.

    :param obj:   The node to write the ``Aggr_Min_Length`` field of.
    :param value: The value to write into the ``Aggr_Min_Length`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_low_limit")
def Get_Aggr_Low_Limit(obj: Iir) -> Iir:
    """
    Highest index choice, if any.

    :param obj: The node to read the ``Aggr_Low_Limit`` field of.
    :returns:   The node's ``Aggr_Low_Limit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_low_limit")
def Set_Aggr_Low_Limit(obj: Iir, value: Iir) -> None:
    """
    Highest index choice, if any.

    :param obj:   The node to write the ``Aggr_Low_Limit`` field of.
    :param value: The value to write into the ``Aggr_Low_Limit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_high_limit")
def Get_Aggr_High_Limit(obj: Iir) -> Iir:
    """
    Highest index choice, if any.

    :param obj: The node to read the ``Aggr_High_Limit`` field of.
    :returns:   The node's ``Aggr_High_Limit`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_high_limit")
def Set_Aggr_High_Limit(obj: Iir, value: Iir) -> None:
    """
    Highest index choice, if any.

    :param obj:   The node to write the ``Aggr_High_Limit`` field of.
    :param value: The value to write into the ``Aggr_High_Limit`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_others_flag")
def Get_Aggr_Others_Flag(obj: Iir) -> Boolean:
    """
    True if the aggregate has an 'others' choice.

    :param obj: The node to read the ``Aggr_Others_Flag`` field of.
    :returns:   The node's ``Aggr_Others_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_others_flag")
def Set_Aggr_Others_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if the aggregate has an 'others' choice.

    :param obj:   The node to write the ``Aggr_Others_Flag`` field of.
    :param value: The value to write into the ``Aggr_Others_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_named_flag")
def Get_Aggr_Named_Flag(obj: Iir) -> Boolean:
    """
    True if the aggregate have named associations.

    :param obj: The node to read the ``Aggr_Named_Flag`` field of.
    :returns:   The node's ``Aggr_Named_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_named_flag")
def Set_Aggr_Named_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if the aggregate have named associations.

    :param obj:   The node to write the ``Aggr_Named_Flag`` field of.
    :param value: The value to write into the ``Aggr_Named_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_aggregate_expand_flag")
def Get_Aggregate_Expand_Flag(obj: Iir) -> Boolean:
    """
    True if the aggregate can be statically built.

    :param obj: The node to read the ``Aggregate_Expand_Flag`` field of.
    :returns:   The node's ``Aggregate_Expand_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_aggregate_expand_flag")
def Set_Aggregate_Expand_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if the aggregate can be statically built.

    :param obj:   The node to write the ``Aggregate_Expand_Flag`` field of.
    :param value: The value to write into the ``Aggregate_Expand_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_determined_aggregate_flag")
def Get_Determined_Aggregate_Flag(obj: Iir) -> Boolean:
    """
    True if the bounds of the aggregated are determined by the context.

    :param obj: The node to read the ``Determined_Aggregate_Flag`` field of.
    :returns:   The node's ``Determined_Aggregate_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_determined_aggregate_flag")
def Set_Determined_Aggregate_Flag(obj: Iir, value: Boolean) -> None:
    """
    True if the bounds of the aggregated are determined by the context.

    :param obj:   The node to write the ``Determined_Aggregate_Flag`` field of.
    :param value: The value to write into the ``Determined_Aggregate_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_association_choices_chain")
def Get_Association_Choices_Chain(obj: Iir) -> Iir:
    """
    Chain of choices.

    :param obj: The node to read the ``Association_Choices_Chain`` field of.
    :returns:   The node's ``Association_Choices_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_association_choices_chain")
def Set_Association_Choices_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of choices.

    :param obj:   The node to write the ``Association_Choices_Chain`` field of.
    :param value: The value to write into the ``Association_Choices_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_case_statement_alternative_chain")
def Get_Case_Statement_Alternative_Chain(obj: Iir) -> Iir:
    """
    Chain of choices.

    :param obj: The node to read the ``Case_Statement_Alternative_Chain`` field of.
    :returns:   The node's ``Case_Statement_Alternative_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_case_statement_alternative_chain")
def Set_Case_Statement_Alternative_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of choices.

    :param obj:   The node to write the ``Case_Statement_Alternative_Chain`` field of.
    :param value: The value to write into the ``Case_Statement_Alternative_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_matching_flag")
def Get_Matching_Flag(obj: Iir) -> Boolean:
    """
    Matching condition for case statement.

    :param obj: The node to read the ``Matching_Flag`` field of.
    :returns:   The node's ``Matching_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_matching_flag")
def Set_Matching_Flag(obj: Iir, value: Boolean) -> None:
    """
    Matching condition for case statement.

    :param obj:   The node to write the ``Matching_Flag`` field of.
    :param value: The value to write into the ``Matching_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_choice_staticness")
def Get_Choice_Staticness(obj: Iir) -> Iir:
    """
    Staticness of the choice.

    :param obj: The node to read the ``Choice_Staticness`` field of.
    :returns:   The node's ``Choice_Staticness`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_choice_staticness")
def Set_Choice_Staticness(obj: Iir, value: Iir) -> None:
    """
    Staticness of the choice.

    :param obj:   The node to write the ``Choice_Staticness`` field of.
    :param value: The value to write into the ``Choice_Staticness`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_procedure_call")
def Get_Procedure_Call(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Procedure_Call`` field of.
    :returns:   The node's ``Procedure_Call`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_procedure_call")
def Set_Procedure_Call(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Procedure_Call`` field of.
    :param value: The value to write into the ``Procedure_Call`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_implementation")
def Get_Implementation(obj: Iir) -> Iir:
    """
    Subprogram to be called by a procedure, function call or operator.  This is the declaration of the subprogram (or a
    list of during analysis).

    :param obj: The node to read the ``Implementation`` field of.
    :returns:   The node's ``Implementation`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_implementation")
def Set_Implementation(obj: Iir, value: Iir) -> None:
    """
    Subprogram to be called by a procedure, function call or operator.  This is the declaration of the subprogram (or a
    list of during analysis).

    :param obj:   The node to write the ``Implementation`` field of.
    :param value: The value to write into the ``Implementation`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_association_chain")
def Get_Parameter_Association_Chain(obj: Iir) -> Iir:
    """
    Parameter associations for procedure and function call.

    :param obj: The node to read the ``Parameter_Association_Chain`` field of.
    :returns:   The node's ``Parameter_Association_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_association_chain")
def Set_Parameter_Association_Chain(obj: Iir, value: Iir) -> None:
    """
    Parameter associations for procedure and function call.

    :param obj:   The node to write the ``Parameter_Association_Chain`` field of.
    :param value: The value to write into the ``Parameter_Association_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_method_object")
def Get_Method_Object(obj: Iir) -> Iir:
    """
    Object of a method call.  NULL_IIR if the subprogram is not a method.

    :param obj: The node to read the ``Method_Object`` field of.
    :returns:   The node's ``Method_Object`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_method_object")
def Set_Method_Object(obj: Iir, value: Iir) -> None:
    """
    Object of a method call.  NULL_IIR if the subprogram is not a method.

    :param obj:   The node to write the ``Method_Object`` field of.
    :param value: The value to write into the ``Method_Object`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_type_mark")
def Get_Subtype_Type_Mark(obj: Iir) -> Iir:
    """
    The type_mark that appeared in the subtype indication.  This is a name. May be null_iir if there is no type mark (as
    in an iterator).

    :param obj: The node to read the ``Subtype_Type_Mark`` field of.
    :returns:   The node's ``Subtype_Type_Mark`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_type_mark")
def Set_Subtype_Type_Mark(obj: Iir, value: Iir) -> None:
    """
    The type_mark that appeared in the subtype indication.  This is a name. May be null_iir if there is no type mark (as
    in an iterator).

    :param obj:   The node to write the ``Subtype_Type_Mark`` field of.
    :param value: The value to write into the ``Subtype_Type_Mark`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_subnature_nature_mark")
def Get_Subnature_Nature_Mark(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Subnature_Nature_Mark`` field of.
    :returns:   The node's ``Subnature_Nature_Mark`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_subnature_nature_mark")
def Set_Subnature_Nature_Mark(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Subnature_Nature_Mark`` field of.
    :param value: The value to write into the ``Subnature_Nature_Mark`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_type_conversion_subtype")
def Get_Type_Conversion_Subtype(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Type_Conversion_Subtype`` field of.
    :returns:   The node's ``Type_Conversion_Subtype`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_type_conversion_subtype")
def Set_Type_Conversion_Subtype(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Type_Conversion_Subtype`` field of.
    :param value: The value to write into the ``Type_Conversion_Subtype`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_type_mark")
def Get_Type_Mark(obj: Iir) -> Iir:
    """
    The type_mark that appeared in qualified expressions or type conversions.

    :param obj: The node to read the ``Type_Mark`` field of.
    :returns:   The node's ``Type_Mark`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_type_mark")
def Set_Type_Mark(obj: Iir, value: Iir) -> None:
    """
    The type_mark that appeared in qualified expressions or type conversions.

    :param obj:   The node to write the ``Type_Mark`` field of.
    :param value: The value to write into the ``Type_Mark`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_file_type_mark")
def Get_File_Type_Mark(obj: Iir) -> Iir:
    """
    The type of values for a type file.

    :param obj: The node to read the ``File_Type_Mark`` field of.
    :returns:   The node's ``File_Type_Mark`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_file_type_mark")
def Set_File_Type_Mark(obj: Iir, value: Iir) -> None:
    """
    The type of values for a type file.

    :param obj:   The node to write the ``File_Type_Mark`` field of.
    :param value: The value to write into the ``File_Type_Mark`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_return_type_mark")
def Get_Return_Type_Mark(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Return_Type_Mark`` field of.
    :returns:   The node's ``Return_Type_Mark`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_return_type_mark")
def Set_Return_Type_Mark(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Return_Type_Mark`` field of.
    :param value: The value to write into the ``Return_Type_Mark`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_disconnect_flag")
def Get_Has_Disconnect_Flag(obj: Iir) -> Boolean:
    """
    This flag is set on a signal_declaration, when a disconnection specification applies to the signal (or a subelement
    of it). This is used to check 'others' and 'all' designators.

    :param obj: The node to read the ``Has_Disconnect_Flag`` field of.
    :returns:   The node's ``Has_Disconnect_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_disconnect_flag")
def Set_Has_Disconnect_Flag(obj: Iir, value: Boolean) -> None:
    """
    This flag is set on a signal_declaration, when a disconnection specification applies to the signal (or a subelement
    of it). This is used to check 'others' and 'all' designators.

    :param obj:   The node to write the ``Has_Disconnect_Flag`` field of.
    :param value: The value to write into the ``Has_Disconnect_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_active_flag")
def Get_Has_Active_Flag(obj: Iir) -> Boolean:
    """
    This flag is set on a signal when its activity is read by the user. Some signals handling can be optimized when this
    flag is set.

    :param obj: The node to read the ``Has_Active_Flag`` field of.
    :returns:   The node's ``Has_Active_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_active_flag")
def Set_Has_Active_Flag(obj: Iir, value: Boolean) -> None:
    """
    This flag is set on a signal when its activity is read by the user. Some signals handling can be optimized when this
    flag is set.

    :param obj:   The node to write the ``Has_Active_Flag`` field of.
    :param value: The value to write into the ``Has_Active_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_is_within_flag")
def Get_Is_Within_Flag(obj: Iir) -> Boolean:
    """
    This flag is set is code being analyzed is textually within TARGET. This is used for selected by name rule.

    :param obj: The node to read the ``Is_Within_Flag`` field of.
    :returns:   The node's ``Is_Within_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_is_within_flag")
def Set_Is_Within_Flag(obj: Iir, value: Boolean) -> None:
    """
    This flag is set is code being analyzed is textually within TARGET. This is used for selected by name rule.

    :param obj:   The node to write the ``Is_Within_Flag`` field of.
    :param value: The value to write into the ``Is_Within_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_type_marks_list")
def Get_Type_Marks_List(obj: Iir) -> Iir:
    """
    List of type_mark for an Iir_Kind_Signature

    :param obj: The node to read the ``Type_Marks_List`` field of.
    :returns:   The node's ``Type_Marks_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_type_marks_list")
def Set_Type_Marks_List(obj: Iir, value: Iir) -> None:
    """
    List of type_mark for an Iir_Kind_Signature

    :param obj:   The node to write the ``Type_Marks_List`` field of.
    :param value: The value to write into the ``Type_Marks_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_implicit_alias_flag")
def Get_Implicit_Alias_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Implicit_Alias_Flag`` field of.
    :returns:   The node's ``Implicit_Alias_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_implicit_alias_flag")
def Set_Implicit_Alias_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Implicit_Alias_Flag`` field of.
    :param value: The value to write into the ``Implicit_Alias_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_alias_signature")
def Get_Alias_Signature(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Alias_Signature`` field of.
    :returns:   The node's ``Alias_Signature`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_alias_signature")
def Set_Alias_Signature(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Alias_Signature`` field of.
    :param value: The value to write into the ``Alias_Signature`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_signature")
def Get_Attribute_Signature(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Attribute_Signature`` field of.
    :returns:   The node's ``Attribute_Signature`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_signature")
def Set_Attribute_Signature(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Attribute_Signature`` field of.
    :param value: The value to write into the ``Attribute_Signature`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_overload_list")
def Get_Overload_List(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Overload_List`` field of.
    :returns:   The node's ``Overload_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_overload_list")
def Set_Overload_List(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Overload_List`` field of.
    :param value: The value to write into the ``Overload_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_simple_name_identifier")
def Get_Simple_Name_Identifier(obj: Iir) -> NameId:
    """
    Identifier of the simple_name attribute.

    :param obj: The node to read the ``Simple_Name_Identifier`` field of.
    :returns:   The node's ``Simple_Name_Identifier`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_simple_name_identifier")
def Set_Simple_Name_Identifier(obj: Iir, value: NameId) -> None:
    """
    Identifier of the simple_name attribute.

    :param obj:   The node to write the ``Simple_Name_Identifier`` field of.
    :param value: The value to write into the ``Simple_Name_Identifier`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_simple_name_subtype")
def Get_Simple_Name_Subtype(obj: Iir) -> Iir:
    """
    Subtype for Simple_Name attribute.

    :param obj: The node to read the ``Simple_Name_Subtype`` field of.
    :returns:   The node's ``Simple_Name_Subtype`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_simple_name_subtype")
def Set_Simple_Name_Subtype(obj: Iir, value: Iir) -> None:
    """
    Subtype for Simple_Name attribute.

    :param obj:   The node to write the ``Simple_Name_Subtype`` field of.
    :param value: The value to write into the ``Simple_Name_Subtype`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_protected_type_body")
def Get_Protected_Type_Body(obj: Iir) -> Iir:
    """
    Body of a protected type declaration.

    :param obj: The node to read the ``Protected_Type_Body`` field of.
    :returns:   The node's ``Protected_Type_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_protected_type_body")
def Set_Protected_Type_Body(obj: Iir, value: Iir) -> None:
    """
    Body of a protected type declaration.

    :param obj:   The node to write the ``Protected_Type_Body`` field of.
    :param value: The value to write into the ``Protected_Type_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_protected_type_declaration")
def Get_Protected_Type_Declaration(obj: Iir) -> Iir:
    """
    Corresponding protected type declaration of a protected type body.

    :param obj: The node to read the ``Protected_Type_Declaration`` field of.
    :returns:   The node's ``Protected_Type_Declaration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_protected_type_declaration")
def Set_Protected_Type_Declaration(obj: Iir, value: Iir) -> None:
    """
    Corresponding protected type declaration of a protected type body.

    :param obj:   The node to write the ``Protected_Type_Declaration`` field of.
    :param value: The value to write into the ``Protected_Type_Declaration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_use_flag")
def Get_Use_Flag(obj: Iir) -> Boolean:
    """
    For a declaration: true if the declaration is used somewhere.

    :param obj: The node to read the ``Use_Flag`` field of.
    :returns:   The node's ``Use_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_use_flag")
def Set_Use_Flag(obj: Iir, value: Boolean) -> None:
    """
    For a declaration: true if the declaration is used somewhere.

    :param obj:   The node to write the ``Use_Flag`` field of.
    :param value: The value to write into the ``Use_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_elaborated_flag")
def Get_Elaborated_Flag(obj: Iir) -> Boolean:
    """
    For a subprogram declaration, constant declaration or protected type. Set when it could be used because fully
    elaborated.

    :param obj: The node to read the ``Elaborated_Flag`` field of.
    :returns:   The node's ``Elaborated_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_elaborated_flag")
def Set_Elaborated_Flag(obj: Iir, value: Boolean) -> None:
    """
    For a subprogram declaration, constant declaration or protected type. Set when it could be used because fully
    elaborated.

    :param obj:   The node to write the ``Elaborated_Flag`` field of.
    :param value: The value to write into the ``Elaborated_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_reserved_id")
def Get_End_Has_Reserved_Id(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'end' is followed by the reserved identifier.

    :param obj: The node to read the ``End_Has_Reserved_Id`` field of.
    :returns:   The node's ``End_Has_Reserved_Id`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_reserved_id")
def Set_End_Has_Reserved_Id(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'end' is followed by the reserved identifier.

    :param obj:   The node to write the ``End_Has_Reserved_Id`` field of.
    :param value: The value to write into the ``End_Has_Reserved_Id`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_identifier")
def Get_End_Has_Identifier(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'end' is followed by the identifier.

    :param obj: The node to read the ``End_Has_Identifier`` field of.
    :returns:   The node's ``End_Has_Identifier`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_identifier")
def Set_End_Has_Identifier(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'end' is followed by the identifier.

    :param obj:   The node to write the ``End_Has_Identifier`` field of.
    :param value: The value to write into the ``End_Has_Identifier`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_postponed")
def Get_End_Has_Postponed(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'end' is followed by 'postponed'.

    :param obj: The node to read the ``End_Has_Postponed`` field of.
    :returns:   The node's ``End_Has_Postponed`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_postponed")
def Set_End_Has_Postponed(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'end' is followed by 'postponed'.

    :param obj:   The node to write the ``End_Has_Postponed`` field of.
    :param value: The value to write into the ``End_Has_Postponed`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_begin")
def Get_Has_Begin(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'begin' is present.

    :param obj: The node to read the ``Has_Begin`` field of.
    :returns:   The node's ``Has_Begin`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_begin")
def Set_Has_Begin(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'begin' is present.

    :param obj:   The node to write the ``Has_Begin`` field of.
    :param value: The value to write into the ``Has_Begin`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_end")
def Get_Has_End(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'end' is present (only for generate body).

    :param obj: The node to read the ``Has_End`` field of.
    :returns:   The node's ``Has_End`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_end")
def Set_Has_End(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'end' is present (only for generate body).

    :param obj:   The node to write the ``Has_End`` field of.
    :param value: The value to write into the ``Has_End`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_is")
def Get_Has_Is(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'is' is present.

    :param obj: The node to read the ``Has_Is`` field of.
    :returns:   The node's ``Has_Is`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_is")
def Set_Has_Is(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'is' is present.

    :param obj:   The node to write the ``Has_Is`` field of.
    :param value: The value to write into the ``Has_Is`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_pure")
def Get_Has_Pure(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'pure' or 'impure' is present.

    :param obj: The node to read the ``Has_Pure`` field of.
    :returns:   The node's ``Has_Pure`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_pure")
def Set_Has_Pure(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'pure' or 'impure' is present.

    :param obj:   The node to write the ``Has_Pure`` field of.
    :param value: The value to write into the ``Has_Pure`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_body")
def Get_Has_Body(obj: Iir) -> Boolean:
    """
    Layout flag: true if body appears just after the specification.

    :param obj: The node to read the ``Has_Body`` field of.
    :returns:   The node's ``Has_Body`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_body")
def Set_Has_Body(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if body appears just after the specification.

    :param obj:   The node to write the ``Has_Body`` field of.
    :param value: The value to write into the ``Has_Body`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_parameter")
def Get_Has_Parameter(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'parameter' reserved identifier is present.

    :param obj: The node to read the ``Has_Parameter`` field of.
    :returns:   The node's ``Has_Parameter`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_parameter")
def Set_Has_Parameter(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'parameter' reserved identifier is present.

    :param obj:   The node to write the ``Has_Parameter`` field of.
    :param value: The value to write into the ``Has_Parameter`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_component")
def Get_Has_Component(obj: Iir) -> Boolean:
    """
    Layout flag: true if 'component' reserved identifier is present.

    :param obj: The node to read the ``Has_Component`` field of.
    :returns:   The node's ``Has_Component`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_component")
def Set_Has_Component(obj: Iir, value: Boolean) -> None:
    """
    Layout flag: true if 'component' reserved identifier is present.

    :param obj:   The node to write the ``Has_Component`` field of.
    :param value: The value to write into the ``Has_Component`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_identifier_list")
def Get_Has_Identifier_List(obj: Iir) -> Boolean:
    """
    Layout flag for object declaration.  If True, the identifier of this declaration is followed by an identifier (and
    separated by a comma). This flag is set on all but the last declarations. Eg: on 'signal A, B, C : Bit', the flag is
    set on A and B (but not C).

    :param obj: The node to read the ``Has_Identifier_List`` field of.
    :returns:   The node's ``Has_Identifier_List`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_identifier_list")
def Set_Has_Identifier_List(obj: Iir, value: Boolean) -> None:
    """
    Layout flag for object declaration.  If True, the identifier of this declaration is followed by an identifier (and
    separated by a comma). This flag is set on all but the last declarations. Eg: on 'signal A, B, C : Bit', the flag is
    set on A and B (but not C).

    :param obj:   The node to write the ``Has_Identifier_List`` field of.
    :param value: The value to write into the ``Has_Identifier_List`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_mode")
def Get_Has_Mode(obj: Iir) -> Boolean:
    """
    Layout flag for object declaration.  If True, the mode is present.

    :param obj: The node to read the ``Has_Mode`` field of.
    :returns:   The node's ``Has_Mode`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_mode")
def Set_Has_Mode(obj: Iir, value: Boolean) -> None:
    """
    Layout flag for object declaration.  If True, the mode is present.

    :param obj:   The node to write the ``Has_Mode`` field of.
    :param value: The value to write into the ``Has_Mode`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_class")
def Get_Has_Class(obj: Iir) -> Boolean:
    """
    Layout flag for object declaration.  If True, the object class is present.

    :param obj: The node to read the ``Has_Class`` field of.
    :returns:   The node's ``Has_Class`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_class")
def Set_Has_Class(obj: Iir, value: Boolean) -> None:
    """
    Layout flag for object declaration.  If True, the object class is present.

    :param obj:   The node to write the ``Has_Class`` field of.
    :param value: The value to write into the ``Has_Class`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_semicolon")
def Get_Has_Semicolon(obj: Iir) -> Boolean:
    """
    Layout flag for interface declaration.  If True, ';' is present after the interface.  In case of multiple names
    separated with colon, it cannot be set with Has_Identifier_List.

    :param obj: The node to read the ``Has_Semicolon`` field of.
    :returns:   The node's ``Has_Semicolon`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_semicolon")
def Set_Has_Semicolon(obj: Iir, value: Boolean) -> None:
    """
    Layout flag for interface declaration.  If True, ';' is present after the interface.  In case of multiple names
    separated with colon, it cannot be set with Has_Identifier_List.

    :param obj:   The node to write the ``Has_Semicolon`` field of.
    :param value: The value to write into the ``Has_Semicolon`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_has_delay_mechanism")
def Get_Has_Delay_Mechanism(obj: Iir) -> Boolean:
    """
    Layout flag for signal assignment.  If True, the delay mechanism is present.  This is obviously true for transport
    or inertial with reject, but the simple 'inertial' is optional.

    :param obj: The node to read the ``Has_Delay_Mechanism`` field of.
    :returns:   The node's ``Has_Delay_Mechanism`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_has_delay_mechanism")
def Set_Has_Delay_Mechanism(obj: Iir, value: Boolean) -> None:
    """
    Layout flag for signal assignment.  If True, the delay mechanism is present.  This is obviously true for transport
    or inertial with reject, but the simple 'inertial' is optional.

    :param obj:   The node to write the ``Has_Delay_Mechanism`` field of.
    :param value: The value to write into the ``Has_Delay_Mechanism`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_flag")
def Get_Suspend_Flag(obj: Iir) -> Boolean:
    """
    Set on wait, procedure call and composite statements when there is a sub-statement that can suspend a procedure or a
    process.  Also set on procedure declaration.  Note that the flag is conservative: it must be true if the node
    contains directly or indirectly a wait statement, but need not to be false otherwise.

    :param obj: The node to read the ``Suspend_Flag`` field of.
    :returns:   The node's ``Suspend_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_flag")
def Set_Suspend_Flag(obj: Iir, value: Boolean) -> None:
    """
    Set on wait, procedure call and composite statements when there is a sub-statement that can suspend a procedure or a
    process.  Also set on procedure declaration.  Note that the flag is conservative: it must be true if the node
    contains directly or indirectly a wait statement, but need not to be false otherwise.

    :param obj:   The node to write the ``Suspend_Flag`` field of.
    :param value: The value to write into the ``Suspend_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_covered_flag")
def Get_Covered_Flag(obj: Iir) -> Boolean:
    """
    Set during elaboration when the statement is executed.

    :param obj: The node to read the ``Covered_Flag`` field of.
    :returns:   The node's ``Covered_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_covered_flag")
def Set_Covered_Flag(obj: Iir, value: Boolean) -> None:
    """
    Set during elaboration when the statement is executed.

    :param obj:   The node to write the ``Covered_Flag`` field of.
    :param value: The value to write into the ``Covered_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_stop_flag")
def Get_Stop_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Stop_Flag`` field of.
    :returns:   The node's ``Stop_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_stop_flag")
def Set_Stop_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Stop_Flag`` field of.
    :param value: The value to write into the ``Stop_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_is_ref")
def Get_Is_Ref(obj: Iir) -> Boolean:
    """
    Set to True if Maybe_Ref fields are references.  This cannot be shared with Has_Identifier_List as: Is_Ref is set to
    True on all items but the first, while Has_Identifier_List is set to True on all items but the last.  Furthermore
    Is_Ref appears in nodes where Has_Identifier_List is not present.

    :param obj: The node to read the ``Is_Ref`` field of.
    :returns:   The node's ``Is_Ref`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_is_ref")
def Set_Is_Ref(obj: Iir, value: Boolean) -> None:
    """
    Set to True if Maybe_Ref fields are references.  This cannot be shared with Has_Identifier_List as: Is_Ref is set to
    True on all items but the first, while Has_Identifier_List is set to True on all items but the last.  Furthermore
    Is_Ref appears in nodes where Has_Identifier_List is not present.

    :param obj:   The node to write the ``Is_Ref`` field of.
    :param value: The value to write into the ``Is_Ref`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_is_forward_ref")
def Get_Is_Forward_Ref(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``Is_Forward_Ref`` field of.
    :returns:   The node's ``Is_Forward_Ref`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_is_forward_ref")
def Set_Is_Forward_Ref(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``Is_Forward_Ref`` field of.
    :param value: The value to write into the ``Is_Forward_Ref`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_property")
def Get_Psl_Property(obj: Iir) -> PSLNode:
    """
    :param obj: The node to read the ``Psl_Property`` field of.
    :returns:   The node's ``Psl_Property`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_property")
def Set_Psl_Property(obj: Iir, value: PSLNode) -> None:
    """
    :param obj:   The node to write the ``Psl_Property`` field of.
    :param value: The value to write into the ``Psl_Property`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_sequence")
def Get_Psl_Sequence(obj: Iir) -> PSLNode:
    """
    :param obj: The node to read the ``Psl_Sequence`` field of.
    :returns:   The node's ``Psl_Sequence`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_sequence")
def Set_Psl_Sequence(obj: Iir, value: PSLNode) -> None:
    """
    :param obj:   The node to write the ``Psl_Sequence`` field of.
    :param value: The value to write into the ``Psl_Sequence`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_declaration")
def Get_Psl_Declaration(obj: Iir) -> PSLNode:
    """
    :param obj: The node to read the ``Psl_Declaration`` field of.
    :returns:   The node's ``Psl_Declaration`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_declaration")
def Set_Psl_Declaration(obj: Iir, value: PSLNode) -> None:
    """
    :param obj:   The node to write the ``Psl_Declaration`` field of.
    :param value: The value to write into the ``Psl_Declaration`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_expression")
def Get_Psl_Expression(obj: Iir) -> PSLNode:
    """
    :param obj: The node to read the ``Psl_Expression`` field of.
    :returns:   The node's ``Psl_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_expression")
def Set_Psl_Expression(obj: Iir, value: PSLNode) -> None:
    """
    :param obj:   The node to write the ``Psl_Expression`` field of.
    :param value: The value to write into the ``Psl_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_boolean")
def Get_Psl_Boolean(obj: Iir) -> PSLNode:
    """
    :param obj: The node to read the ``Psl_Boolean`` field of.
    :returns:   The node's ``Psl_Boolean`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_boolean")
def Set_Psl_Boolean(obj: Iir, value: PSLNode) -> None:
    """
    :param obj:   The node to write the ``Psl_Boolean`` field of.
    :param value: The value to write into the ``Psl_Boolean`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_clock")
def Get_PSL_Clock(obj: Iir) -> PSLNode:
    """
    :param obj: The node to read the ``PSL_Clock`` field of.
    :returns:   The node's ``PSL_Clock`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_clock")
def Set_PSL_Clock(obj: Iir, value: PSLNode) -> None:
    """
    :param obj:   The node to write the ``PSL_Clock`` field of.
    :param value: The value to write into the ``PSL_Clock`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_abort")
def Get_PSL_Abort(obj: Iir) -> PSLNode:
    """
    :param obj: The node to read the ``PSL_Abort`` field of.
    :returns:   The node's ``PSL_Abort`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_abort")
def Set_PSL_Abort(obj: Iir, value: PSLNode) -> None:
    """
    :param obj:   The node to write the ``PSL_Abort`` field of.
    :param value: The value to write into the ``PSL_Abort`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_nfa")
def Get_PSL_NFA(obj: Iir) -> PSLNFA:
    """
    :param obj: The node to read the ``PSL_NFA`` field of.
    :returns:   The node's ``PSL_NFA`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_nfa")
def Set_PSL_NFA(obj: Iir, value: PSLNFA) -> None:
    """
    :param obj:   The node to write the ``PSL_NFA`` field of.
    :param value: The value to write into the ``PSL_NFA`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_nbr_states")
def Get_PSL_Nbr_States(obj: Iir) -> Int32:
    """
    :param obj: The node to read the ``PSL_Nbr_States`` field of.
    :returns:   The node's ``PSL_Nbr_States`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_nbr_states")
def Set_PSL_Nbr_States(obj: Iir, value: Int32) -> None:
    """
    :param obj:   The node to write the ``PSL_Nbr_States`` field of.
    :param value: The value to write into the ``PSL_Nbr_States`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_clock_sensitivity")
def Get_PSL_Clock_Sensitivity(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``PSL_Clock_Sensitivity`` field of.
    :returns:   The node's ``PSL_Clock_Sensitivity`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_clock_sensitivity")
def Set_PSL_Clock_Sensitivity(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``PSL_Clock_Sensitivity`` field of.
    :param value: The value to write into the ``PSL_Clock_Sensitivity`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_psl_eos_flag")
def Get_PSL_EOS_Flag(obj: Iir) -> Boolean:
    """
    :param obj: The node to read the ``PSL_EOS_Flag`` field of.
    :returns:   The node's ``PSL_EOS_Flag`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_psl_eos_flag")
def Set_PSL_EOS_Flag(obj: Iir, value: Boolean) -> None:
    """
    :param obj:   The node to write the ``PSL_EOS_Flag`` field of.
    :param value: The value to write into the ``PSL_EOS_Flag`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_count_expression")
def Get_Count_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Count_Expression`` field of.
    :returns:   The node's ``Count_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_count_expression")
def Set_Count_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Count_Expression`` field of.
    :param value: The value to write into the ``Count_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_clock_expression")
def Get_Clock_Expression(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Clock_Expression`` field of.
    :returns:   The node's ``Clock_Expression`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_clock_expression")
def Set_Clock_Expression(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Clock_Expression`` field of.
    :param value: The value to write into the ``Clock_Expression`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_default_clock")
def Get_Default_Clock(obj: Iir) -> PSLNode:
    """
    Reference to the default_clock node.

    :param obj: The node to read the ``Default_Clock`` field of.
    :returns:   The node's ``Default_Clock`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_default_clock")
def Set_Default_Clock(obj: Iir, value: PSLNode) -> None:
    """
    Reference to the default_clock node.

    :param obj:   The node to write the ``Default_Clock`` field of.
    :param value: The value to write into the ``Default_Clock`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_foreign_node")
def Get_Foreign_Node(obj: Iir) -> Int32:
    """
    :param obj: The node to read the ``Foreign_Node`` field of.
    :returns:   The node's ``Foreign_Node`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_foreign_node")
def Set_Foreign_Node(obj: Iir, value: Int32) -> None:
    """
    :param obj:   The node to write the ``Foreign_Node`` field of.
    :param value: The value to write into the ``Foreign_Node`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_state_index")
def Get_Suspend_State_Index(obj: Iir) -> Int32:
    """
    State index for the statement.

    :param obj: The node to read the ``Suspend_State_Index`` field of.
    :returns:   The node's ``Suspend_State_Index`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_state_index")
def Set_Suspend_State_Index(obj: Iir, value: Int32) -> None:
    """
    State index for the statement.

    :param obj:   The node to write the ``Suspend_State_Index`` field of.
    :param value: The value to write into the ``Suspend_State_Index`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_state_chain")
def Get_Suspend_State_Chain(obj: Iir) -> Iir:
    """
    Chain of suspend state statement.

    :param obj: The node to read the ``Suspend_State_Chain`` field of.
    :returns:   The node's ``Suspend_State_Chain`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_state_chain")
def Set_Suspend_State_Chain(obj: Iir, value: Iir) -> None:
    """
    Chain of suspend state statement.

    :param obj:   The node to write the ``Suspend_State_Chain`` field of.
    :param value: The value to write into the ``Suspend_State_Chain`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_state_last")
def Get_Suspend_State_Last(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Suspend_State_Last`` field of.
    :returns:   The node's ``Suspend_State_Last`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_state_last")
def Set_Suspend_State_Last(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Suspend_State_Last`` field of.
    :param value: The value to write into the ``Suspend_State_Last`` field.
    """


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_state_decl")
def Get_Suspend_State_Decl(obj: Iir) -> Iir:
    """
    :param obj: The node to read the ``Suspend_State_Decl`` field of.
    :returns:   The node's ``Suspend_State_Decl`` field.
    """
    return 0  # pragma: no cover


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_state_decl")
def Set_Suspend_State_Decl(obj: Iir, value: Iir) -> None:
    """
    :param obj:   The node to write the ``Suspend_State_Decl`` field of.
    :param value: The value to write into the ``Suspend_State_Decl`` field.
    """
