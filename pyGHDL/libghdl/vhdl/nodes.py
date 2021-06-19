# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
from enum import IntEnum, unique
from pydecor import export

from pyGHDL.libghdl._decorator import BindToLibGHDL
from typing import TypeVar
from ctypes import c_int32
from pyGHDL.libghdl import libghdl
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

Null_Iir = 0

Null_Iir_List = 0
Iir_List_All = 1

Null_Iir_Flist = 0
Iir_Flist_Others = 1
Iir_Flist_All = 2

DateType = TypeVar("DateType", bound=c_int32)


@export
@unique
class Iir_Kind(IntEnum):
    Unused = 0
    Error = 1
    Design_File = 2
    Design_Unit = 3
    Foreign_Module = 4
    Library_Clause = 5
    Use_Clause = 6
    Context_Reference = 7
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
    Association_Element_By_Individual = 21
    Association_Element_Open = 22
    Association_Element_Package = 23
    Association_Element_Type = 24
    Association_Element_Subprogram = 25
    Association_Element_Terminal = 26
    Choice_By_Range = 27
    Choice_By_Expression = 28
    Choice_By_Others = 29
    Choice_By_None = 30
    Choice_By_Name = 31
    Entity_Aspect_Entity = 32
    Entity_Aspect_Configuration = 33
    Entity_Aspect_Open = 34
    Psl_Hierarchical_Name = 35
    Block_Configuration = 36
    Block_Header = 37
    Component_Configuration = 38
    Binding_Indication = 39
    Entity_Class = 40
    Attribute_Value = 41
    Signature = 42
    Aggregate_Info = 43
    Procedure_Call = 44
    Record_Element_Constraint = 45
    Array_Element_Resolution = 46
    Record_Resolution = 47
    Record_Element_Resolution = 48
    Break_Element = 49
    Attribute_Specification = 50
    Disconnection_Specification = 51
    Step_Limit_Specification = 52
    Configuration_Specification = 53
    Access_Type_Definition = 54
    Incomplete_Type_Definition = 55
    Interface_Type_Definition = 56
    File_Type_Definition = 57
    Protected_Type_Declaration = 58
    Record_Type_Definition = 59
    Array_Type_Definition = 60
    Array_Subtype_Definition = 61
    Record_Subtype_Definition = 62
    Access_Subtype_Definition = 63
    Physical_Subtype_Definition = 64
    Floating_Subtype_Definition = 65
    Integer_Subtype_Definition = 66
    Enumeration_Subtype_Definition = 67
    Enumeration_Type_Definition = 68
    Integer_Type_Definition = 69
    Floating_Type_Definition = 70
    Physical_Type_Definition = 71
    Range_Expression = 72
    Protected_Type_Body = 73
    Wildcard_Type_Definition = 74
    Subtype_Definition = 75
    Scalar_Nature_Definition = 76
    Record_Nature_Definition = 77
    Array_Nature_Definition = 78
    Array_Subnature_Definition = 79
    Overload_List = 80
    Entity_Declaration = 81
    Configuration_Declaration = 82
    Context_Declaration = 83
    Package_Declaration = 84
    Package_Instantiation_Declaration = 85
    Vmode_Declaration = 86
    Vprop_Declaration = 87
    Vunit_Declaration = 88
    Package_Body = 89
    Architecture_Body = 90
    Type_Declaration = 91
    Anonymous_Type_Declaration = 92
    Subtype_Declaration = 93
    Nature_Declaration = 94
    Subnature_Declaration = 95
    Package_Header = 96
    Unit_Declaration = 97
    Library_Declaration = 98
    Component_Declaration = 99
    Attribute_Declaration = 100
    Group_Template_Declaration = 101
    Group_Declaration = 102
    Element_Declaration = 103
    Nature_Element_Declaration = 104
    Non_Object_Alias_Declaration = 105
    Psl_Declaration = 106
    Psl_Endpoint_Declaration = 107
    Enumeration_Literal = 108
    Function_Declaration = 109
    Procedure_Declaration = 110
    Function_Body = 111
    Procedure_Body = 112
    Function_Instantiation_Declaration = 113
    Procedure_Instantiation_Declaration = 114
    Terminal_Declaration = 115
    Object_Alias_Declaration = 116
    Free_Quantity_Declaration = 117
    Spectrum_Quantity_Declaration = 118
    Noise_Quantity_Declaration = 119
    Across_Quantity_Declaration = 120
    Through_Quantity_Declaration = 121
    File_Declaration = 122
    Guard_Signal_Declaration = 123
    Signal_Declaration = 124
    Variable_Declaration = 125
    Constant_Declaration = 126
    Iterator_Declaration = 127
    Interface_Constant_Declaration = 128
    Interface_Variable_Declaration = 129
    Interface_Signal_Declaration = 130
    Interface_File_Declaration = 131
    Interface_Quantity_Declaration = 132
    Interface_Terminal_Declaration = 133
    Interface_Type_Declaration = 134
    Interface_Package_Declaration = 135
    Interface_Function_Declaration = 136
    Interface_Procedure_Declaration = 137
    Anonymous_Signal_Declaration = 138
    Signal_Attribute_Declaration = 139
    Identity_Operator = 140
    Negation_Operator = 141
    Absolute_Operator = 142
    Not_Operator = 143
    Implicit_Condition_Operator = 144
    Condition_Operator = 145
    Reduction_And_Operator = 146
    Reduction_Or_Operator = 147
    Reduction_Nand_Operator = 148
    Reduction_Nor_Operator = 149
    Reduction_Xor_Operator = 150
    Reduction_Xnor_Operator = 151
    And_Operator = 152
    Or_Operator = 153
    Nand_Operator = 154
    Nor_Operator = 155
    Xor_Operator = 156
    Xnor_Operator = 157
    Equality_Operator = 158
    Inequality_Operator = 159
    Less_Than_Operator = 160
    Less_Than_Or_Equal_Operator = 161
    Greater_Than_Operator = 162
    Greater_Than_Or_Equal_Operator = 163
    Match_Equality_Operator = 164
    Match_Inequality_Operator = 165
    Match_Less_Than_Operator = 166
    Match_Less_Than_Or_Equal_Operator = 167
    Match_Greater_Than_Operator = 168
    Match_Greater_Than_Or_Equal_Operator = 169
    Sll_Operator = 170
    Sla_Operator = 171
    Srl_Operator = 172
    Sra_Operator = 173
    Rol_Operator = 174
    Ror_Operator = 175
    Addition_Operator = 176
    Substraction_Operator = 177
    Concatenation_Operator = 178
    Multiplication_Operator = 179
    Division_Operator = 180
    Modulus_Operator = 181
    Remainder_Operator = 182
    Exponentiation_Operator = 183
    Function_Call = 184
    Aggregate = 185
    Parenthesis_Expression = 186
    Qualified_Expression = 187
    Type_Conversion = 188
    Allocator_By_Expression = 189
    Allocator_By_Subtype = 190
    Selected_Element = 191
    Dereference = 192
    Implicit_Dereference = 193
    Slice_Name = 194
    Indexed_Name = 195
    Psl_Prev = 196
    Psl_Stable = 197
    Psl_Rose = 198
    Psl_Fell = 199
    Psl_Onehot = 200
    Psl_Onehot0 = 201
    Psl_Expression = 202
    Sensitized_Process_Statement = 203
    Process_Statement = 204
    Concurrent_Simple_Signal_Assignment = 205
    Concurrent_Conditional_Signal_Assignment = 206
    Concurrent_Selected_Signal_Assignment = 207
    Concurrent_Assertion_Statement = 208
    Concurrent_Procedure_Call_Statement = 209
    Concurrent_Break_Statement = 210
    Psl_Assert_Directive = 211
    Psl_Assume_Directive = 212
    Psl_Cover_Directive = 213
    Psl_Restrict_Directive = 214
    Block_Statement = 215
    If_Generate_Statement = 216
    Case_Generate_Statement = 217
    For_Generate_Statement = 218
    Component_Instantiation_Statement = 219
    Psl_Default_Clock = 220
    Generate_Statement_Body = 221
    If_Generate_Else_Clause = 222
    Simple_Simultaneous_Statement = 223
    Simultaneous_Null_Statement = 224
    Simultaneous_Procedural_Statement = 225
    Simultaneous_Case_Statement = 226
    Simultaneous_If_Statement = 227
    Simultaneous_Elsif = 228
    Simple_Signal_Assignment_Statement = 229
    Conditional_Signal_Assignment_Statement = 230
    Selected_Waveform_Assignment_Statement = 231
    Signal_Force_Assignment_Statement = 232
    Signal_Release_Assignment_Statement = 233
    Null_Statement = 234
    Assertion_Statement = 235
    Report_Statement = 236
    Wait_Statement = 237
    Variable_Assignment_Statement = 238
    Conditional_Variable_Assignment_Statement = 239
    Return_Statement = 240
    For_Loop_Statement = 241
    While_Loop_Statement = 242
    Next_Statement = 243
    Exit_Statement = 244
    Case_Statement = 245
    Procedure_Call_Statement = 246
    Break_Statement = 247
    If_Statement = 248
    Elsif = 249
    Character_Literal = 250
    Simple_Name = 251
    Selected_Name = 252
    Operator_Symbol = 253
    Reference_Name = 254
    External_Constant_Name = 255
    External_Signal_Name = 256
    External_Variable_Name = 257
    Selected_By_All_Name = 258
    Parenthesis_Name = 259
    Package_Pathname = 260
    Absolute_Pathname = 261
    Relative_Pathname = 262
    Pathname_Element = 263
    Base_Attribute = 264
    Subtype_Attribute = 265
    Element_Attribute = 266
    Across_Attribute = 267
    Through_Attribute = 268
    Nature_Reference_Attribute = 269
    Left_Type_Attribute = 270
    Right_Type_Attribute = 271
    High_Type_Attribute = 272
    Low_Type_Attribute = 273
    Ascending_Type_Attribute = 274
    Image_Attribute = 275
    Value_Attribute = 276
    Pos_Attribute = 277
    Val_Attribute = 278
    Succ_Attribute = 279
    Pred_Attribute = 280
    Leftof_Attribute = 281
    Rightof_Attribute = 282
    Signal_Slew_Attribute = 283
    Quantity_Slew_Attribute = 284
    Ramp_Attribute = 285
    Zoh_Attribute = 286
    Ltf_Attribute = 287
    Ztf_Attribute = 288
    Dot_Attribute = 289
    Integ_Attribute = 290
    Above_Attribute = 291
    Quantity_Delayed_Attribute = 292
    Delayed_Attribute = 293
    Stable_Attribute = 294
    Quiet_Attribute = 295
    Transaction_Attribute = 296
    Event_Attribute = 297
    Active_Attribute = 298
    Last_Event_Attribute = 299
    Last_Active_Attribute = 300
    Last_Value_Attribute = 301
    Driving_Attribute = 302
    Driving_Value_Attribute = 303
    Behavior_Attribute = 304
    Structure_Attribute = 305
    Simple_Name_Attribute = 306
    Instance_Name_Attribute = 307
    Path_Name_Attribute = 308
    Left_Array_Attribute = 309
    Right_Array_Attribute = 310
    High_Array_Attribute = 311
    Low_Array_Attribute = 312
    Length_Array_Attribute = 313
    Ascending_Array_Attribute = 314
    Range_Array_Attribute = 315
    Reverse_Range_Array_Attribute = 316
    Attribute_Name = 317


@export
class Iir_Kinds:
    Library_Unit = [
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

    Design_Unit = [
        Iir_Kind.Design_Unit,
        Iir_Kind.Foreign_Module,
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

    Process_Statement = [
        Iir_Kind.Sensitized_Process_Statement,
        Iir_Kind.Process_Statement,
    ]

    Interface_Object_Declaration = [
        Iir_Kind.Interface_Constant_Declaration,
        Iir_Kind.Interface_Variable_Declaration,
        Iir_Kind.Interface_Signal_Declaration,
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
        Iir_Kind.Interface_File_Declaration,
    ]

    Association_Element_Parameters = [
        Iir_Kind.Association_Element_By_Expression,
        Iir_Kind.Association_Element_By_Individual,
        Iir_Kind.Association_Element_Open,
    ]

    Association_Element = [
        Iir_Kind.Association_Element_By_Expression,
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
        Iir_Kind.Above_Attribute,
        Iir_Kind.Quantity_Delayed_Attribute,
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
        Iir_Kind.Above_Attribute,
        Iir_Kind.Quantity_Delayed_Attribute,
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
        Iir_Kind.Psl_Default_Clock,
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

    Generate_Statement = [
        Iir_Kind.If_Generate_Statement,
        Iir_Kind.Case_Generate_Statement,
        Iir_Kind.For_Generate_Statement,
    ]

    Concurrent_Signal_Assignment = [
        Iir_Kind.Concurrent_Simple_Signal_Assignment,
        Iir_Kind.Concurrent_Conditional_Signal_Assignment,
        Iir_Kind.Concurrent_Selected_Signal_Assignment,
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
        Iir_Kind.Null_Statement,
        Iir_Kind.Assertion_Statement,
        Iir_Kind.Report_Statement,
        Iir_Kind.Wait_Statement,
        Iir_Kind.Variable_Assignment_Statement,
        Iir_Kind.Conditional_Variable_Assignment_Statement,
        Iir_Kind.Return_Statement,
        Iir_Kind.For_Loop_Statement,
        Iir_Kind.While_Loop_Statement,
        Iir_Kind.Next_Statement,
        Iir_Kind.Exit_Statement,
        Iir_Kind.Case_Statement,
        Iir_Kind.Procedure_Call_Statement,
        Iir_Kind.Break_Statement,
        Iir_Kind.If_Statement,
    ]

    Next_Exit_Statement = [
        Iir_Kind.Next_Statement,
        Iir_Kind.Exit_Statement,
    ]

    Variable_Assignment_Statement = [
        Iir_Kind.Variable_Assignment_Statement,
        Iir_Kind.Conditional_Variable_Assignment_Statement,
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


@export
@unique
class Iir_Mode(IntEnum):
    Unknown_Mode = 0
    Linkage_Mode = 1
    Buffer_Mode = 2
    Out_Mode = 3
    Inout_Mode = 4
    In_Mode = 5


@export
@unique
class ScalarSize(IntEnum):
    Scalar_8 = 0
    Scalar_16 = 1
    Scalar_32 = 2
    Scalar_64 = 3


@export
@unique
class Iir_Staticness(IntEnum):
    Unknown = 0
    PNone = 1
    Globally = 2
    Locally = 3


@export
@unique
class Iir_Constraint(IntEnum):
    Unconstrained = 0
    Partially_Constrained = 1
    Fully_Constrained = 2


@export
@unique
class Iir_Delay_Mechanism(IntEnum):
    Inertial_Delay = 0
    Transport_Delay = 1


@export
@unique
class DateStateType(IntEnum):
    Extern = 0
    Disk = 1
    Parse = 2
    Analyze = 3


@export
@unique
class NumberBaseType(IntEnum):
    Base_None = 0
    Base_2 = 1
    Base_8 = 2
    Base_10 = 3
    Base_16 = 4


@export
@unique
class Iir_Predefined(IntEnum):
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
    Enum_Minimum = 16
    Enum_Maximum = 17
    Enum_To_String = 18
    Bit_And = 19
    Bit_Or = 20
    Bit_Nand = 21
    Bit_Nor = 22
    Bit_Xor = 23
    Bit_Xnor = 24
    Bit_Not = 25
    Bit_Match_Equality = 26
    Bit_Match_Inequality = 27
    Bit_Match_Less = 28
    Bit_Match_Less_Equal = 29
    Bit_Match_Greater = 30
    Bit_Match_Greater_Equal = 31
    Bit_Condition = 32
    Bit_Rising_Edge = 33
    Bit_Falling_Edge = 34
    Integer_Equality = 35
    Integer_Inequality = 36
    Integer_Less = 37
    Integer_Less_Equal = 38
    Integer_Greater = 39
    Integer_Greater_Equal = 40
    Integer_Identity = 41
    Integer_Negation = 42
    Integer_Absolute = 43
    Integer_Plus = 44
    Integer_Minus = 45
    Integer_Mul = 46
    Integer_Div = 47
    Integer_Mod = 48
    Integer_Rem = 49
    Integer_Exp = 50
    Integer_Minimum = 51
    Integer_Maximum = 52
    Integer_To_String = 53
    Floating_Equality = 54
    Floating_Inequality = 55
    Floating_Less = 56
    Floating_Less_Equal = 57
    Floating_Greater = 58
    Floating_Greater_Equal = 59
    Floating_Identity = 60
    Floating_Negation = 61
    Floating_Absolute = 62
    Floating_Plus = 63
    Floating_Minus = 64
    Floating_Mul = 65
    Floating_Div = 66
    Floating_Exp = 67
    Floating_Minimum = 68
    Floating_Maximum = 69
    Floating_To_String = 70
    Real_To_String_Digits = 71
    Real_To_String_Format = 72
    Universal_R_I_Mul = 73
    Universal_I_R_Mul = 74
    Universal_R_I_Div = 75
    Physical_Equality = 76
    Physical_Inequality = 77
    Physical_Less = 78
    Physical_Less_Equal = 79
    Physical_Greater = 80
    Physical_Greater_Equal = 81
    Physical_Identity = 82
    Physical_Negation = 83
    Physical_Absolute = 84
    Physical_Plus = 85
    Physical_Minus = 86
    Physical_Integer_Mul = 87
    Physical_Real_Mul = 88
    Integer_Physical_Mul = 89
    Real_Physical_Mul = 90
    Physical_Integer_Div = 91
    Physical_Real_Div = 92
    Physical_Physical_Div = 93
    Physical_Minimum = 94
    Physical_Maximum = 95
    Physical_To_String = 96
    Time_To_String_Unit = 97
    Access_Equality = 98
    Access_Inequality = 99
    Record_Equality = 100
    Record_Inequality = 101
    Array_Equality = 102
    Array_Inequality = 103
    Array_Less = 104
    Array_Less_Equal = 105
    Array_Greater = 106
    Array_Greater_Equal = 107
    Array_Array_Concat = 108
    Array_Element_Concat = 109
    Element_Array_Concat = 110
    Element_Element_Concat = 111
    Array_Minimum = 112
    Array_Maximum = 113
    Vector_Minimum = 114
    Vector_Maximum = 115
    Array_Sll = 116
    Array_Srl = 117
    Array_Sla = 118
    Array_Sra = 119
    Array_Rol = 120
    Array_Ror = 121
    TF_Array_And = 122
    TF_Array_Or = 123
    TF_Array_Nand = 124
    TF_Array_Nor = 125
    TF_Array_Xor = 126
    TF_Array_Xnor = 127
    TF_Array_Not = 128
    TF_Reduction_And = 129
    TF_Reduction_Or = 130
    TF_Reduction_Nand = 131
    TF_Reduction_Nor = 132
    TF_Reduction_Xor = 133
    TF_Reduction_Xnor = 134
    TF_Reduction_Not = 135
    TF_Array_Element_And = 136
    TF_Element_Array_And = 137
    TF_Array_Element_Or = 138
    TF_Element_Array_Or = 139
    TF_Array_Element_Nand = 140
    TF_Element_Array_Nand = 141
    TF_Array_Element_Nor = 142
    TF_Element_Array_Nor = 143
    TF_Array_Element_Xor = 144
    TF_Element_Array_Xor = 145
    TF_Array_Element_Xnor = 146
    TF_Element_Array_Xnor = 147
    Bit_Array_Match_Equality = 148
    Bit_Array_Match_Inequality = 149
    Array_Char_To_String = 150
    Bit_Vector_To_Ostring = 151
    Bit_Vector_To_Hstring = 152
    Std_Ulogic_Match_Equality = 153
    Std_Ulogic_Match_Inequality = 154
    Std_Ulogic_Match_Less = 155
    Std_Ulogic_Match_Less_Equal = 156
    Std_Ulogic_Match_Greater = 157
    Std_Ulogic_Match_Greater_Equal = 158
    Std_Ulogic_Array_Match_Equality = 159
    Std_Ulogic_Array_Match_Inequality = 160
    Deallocate = 161
    File_Open = 162
    File_Open_Status = 163
    File_Close = 164
    Read = 165
    Read_Length = 166
    Flush = 167
    Write = 168
    Endfile = 169
    Now_Function = 170
    Real_Now_Function = 171
    Frequency_Function = 172
    PNone = 173
    Foreign_Untruncated_Text_Read = 174
    Foreign_Textio_Read_Real = 175
    Foreign_Textio_Write_Real = 176
    Ieee_1164_Scalar_And = 177
    Ieee_1164_Scalar_Nand = 178
    Ieee_1164_Scalar_Or = 179
    Ieee_1164_Scalar_Nor = 180
    Ieee_1164_Scalar_Xor = 181
    Ieee_1164_Scalar_Xnor = 182
    Ieee_1164_Scalar_Not = 183
    Ieee_1164_Vector_And = 184
    Ieee_1164_Vector_Nand = 185
    Ieee_1164_Vector_Or = 186
    Ieee_1164_Vector_Nor = 187
    Ieee_1164_Vector_Xor = 188
    Ieee_1164_Vector_Xnor = 189
    Ieee_1164_Vector_Not = 190
    Ieee_1164_To_Bit = 191
    Ieee_1164_To_Bitvector = 192
    Ieee_1164_To_Stdulogic = 193
    Ieee_1164_To_Stdlogicvector_Bv = 194
    Ieee_1164_To_Stdlogicvector_Suv = 195
    Ieee_1164_To_Stdulogicvector_Bv = 196
    Ieee_1164_To_Stdulogicvector_Slv = 197
    Ieee_1164_To_X01_Slv = 198
    Ieee_1164_To_X01_Suv = 199
    Ieee_1164_To_X01_Log = 200
    Ieee_1164_To_X01_Bv_Slv = 201
    Ieee_1164_To_X01_Bv_Suv = 202
    Ieee_1164_To_X01_Bit_Log = 203
    Ieee_1164_To_X01Z_Slv = 204
    Ieee_1164_To_X01Z_Suv = 205
    Ieee_1164_To_X01Z_Log = 206
    Ieee_1164_To_X01Z_Bv_Slv = 207
    Ieee_1164_To_X01Z_Bv_Suv = 208
    Ieee_1164_To_X01Z_Bit_Log = 209
    Ieee_1164_To_UX01_Slv = 210
    Ieee_1164_To_UX01_Suv = 211
    Ieee_1164_To_UX01_Log = 212
    Ieee_1164_To_UX01_Bv_Slv = 213
    Ieee_1164_To_UX01_Bv_Suv = 214
    Ieee_1164_To_UX01_Bit_Log = 215
    Ieee_1164_Vector_Is_X = 216
    Ieee_1164_Scalar_Is_X = 217
    Ieee_1164_Rising_Edge = 218
    Ieee_1164_Falling_Edge = 219
    Ieee_1164_And_Suv_Log = 220
    Ieee_1164_And_Log_Suv = 221
    Ieee_1164_Nand_Suv_Log = 222
    Ieee_1164_Nand_Log_Suv = 223
    Ieee_1164_Or_Suv_Log = 224
    Ieee_1164_Or_Log_Suv = 225
    Ieee_1164_Nor_Suv_Log = 226
    Ieee_1164_Nor_Log_Suv = 227
    Ieee_1164_Xor_Suv_Log = 228
    Ieee_1164_Xor_Log_Suv = 229
    Ieee_1164_Xnor_Suv_Log = 230
    Ieee_1164_Xnor_Log_Suv = 231
    Ieee_1164_And_Suv = 232
    Ieee_1164_Nand_Suv = 233
    Ieee_1164_Or_Suv = 234
    Ieee_1164_Nor_Suv = 235
    Ieee_1164_Xor_Suv = 236
    Ieee_1164_Xnor_Suv = 237
    Ieee_1164_Vector_Sll = 238
    Ieee_1164_Vector_Srl = 239
    Ieee_1164_Vector_Rol = 240
    Ieee_1164_Vector_Ror = 241
    Ieee_1164_Condition_Operator = 242
    Ieee_Numeric_Std_Toint_Uns_Nat = 243
    Ieee_Numeric_Std_Toint_Sgn_Int = 244
    Ieee_Numeric_Std_Touns_Nat_Nat_Uns = 245
    Ieee_Numeric_Std_Touns_Nat_Uns_Uns = 246
    Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn = 247
    Ieee_Numeric_Std_Tosgn_Int_Sgn_Sgn = 248
    Ieee_Numeric_Std_Resize_Uns_Nat = 249
    Ieee_Numeric_Std_Resize_Sgn_Nat = 250
    Ieee_Numeric_Std_Resize_Uns_Uns = 251
    Ieee_Numeric_Std_Resize_Sgn_Sgn = 252
    Ieee_Numeric_Std_Add_Uns_Uns = 253
    Ieee_Numeric_Std_Add_Uns_Nat = 254
    Ieee_Numeric_Std_Add_Nat_Uns = 255
    Ieee_Numeric_Std_Add_Uns_Log = 256
    Ieee_Numeric_Std_Add_Log_Uns = 257
    Ieee_Numeric_Std_Add_Sgn_Sgn = 258
    Ieee_Numeric_Std_Add_Sgn_Int = 259
    Ieee_Numeric_Std_Add_Int_Sgn = 260
    Ieee_Numeric_Std_Add_Sgn_Log = 261
    Ieee_Numeric_Std_Add_Log_Sgn = 262
    Ieee_Numeric_Std_Sub_Uns_Uns = 263
    Ieee_Numeric_Std_Sub_Uns_Nat = 264
    Ieee_Numeric_Std_Sub_Nat_Uns = 265
    Ieee_Numeric_Std_Sub_Uns_Log = 266
    Ieee_Numeric_Std_Sub_Log_Uns = 267
    Ieee_Numeric_Std_Sub_Sgn_Sgn = 268
    Ieee_Numeric_Std_Sub_Sgn_Int = 269
    Ieee_Numeric_Std_Sub_Int_Sgn = 270
    Ieee_Numeric_Std_Sub_Sgn_Log = 271
    Ieee_Numeric_Std_Sub_Log_Sgn = 272
    Ieee_Numeric_Std_Mul_Uns_Uns = 273
    Ieee_Numeric_Std_Mul_Uns_Nat = 274
    Ieee_Numeric_Std_Mul_Nat_Uns = 275
    Ieee_Numeric_Std_Mul_Sgn_Sgn = 276
    Ieee_Numeric_Std_Mul_Sgn_Int = 277
    Ieee_Numeric_Std_Mul_Int_Sgn = 278
    Ieee_Numeric_Std_Div_Uns_Uns = 279
    Ieee_Numeric_Std_Div_Uns_Nat = 280
    Ieee_Numeric_Std_Div_Nat_Uns = 281
    Ieee_Numeric_Std_Div_Sgn_Sgn = 282
    Ieee_Numeric_Std_Div_Sgn_Int = 283
    Ieee_Numeric_Std_Div_Int_Sgn = 284
    Ieee_Numeric_Std_Rem_Uns_Uns = 285
    Ieee_Numeric_Std_Rem_Uns_Nat = 286
    Ieee_Numeric_Std_Rem_Nat_Uns = 287
    Ieee_Numeric_Std_Rem_Sgn_Sgn = 288
    Ieee_Numeric_Std_Rem_Sgn_Int = 289
    Ieee_Numeric_Std_Rem_Int_Sgn = 290
    Ieee_Numeric_Std_Mod_Uns_Uns = 291
    Ieee_Numeric_Std_Mod_Uns_Nat = 292
    Ieee_Numeric_Std_Mod_Nat_Uns = 293
    Ieee_Numeric_Std_Mod_Sgn_Sgn = 294
    Ieee_Numeric_Std_Mod_Sgn_Int = 295
    Ieee_Numeric_Std_Mod_Int_Sgn = 296
    Ieee_Numeric_Std_Gt_Uns_Uns = 297
    Ieee_Numeric_Std_Gt_Uns_Nat = 298
    Ieee_Numeric_Std_Gt_Nat_Uns = 299
    Ieee_Numeric_Std_Gt_Sgn_Sgn = 300
    Ieee_Numeric_Std_Gt_Sgn_Int = 301
    Ieee_Numeric_Std_Gt_Int_Sgn = 302
    Ieee_Numeric_Std_Lt_Uns_Uns = 303
    Ieee_Numeric_Std_Lt_Uns_Nat = 304
    Ieee_Numeric_Std_Lt_Nat_Uns = 305
    Ieee_Numeric_Std_Lt_Sgn_Sgn = 306
    Ieee_Numeric_Std_Lt_Sgn_Int = 307
    Ieee_Numeric_Std_Lt_Int_Sgn = 308
    Ieee_Numeric_Std_Le_Uns_Uns = 309
    Ieee_Numeric_Std_Le_Uns_Nat = 310
    Ieee_Numeric_Std_Le_Nat_Uns = 311
    Ieee_Numeric_Std_Le_Sgn_Sgn = 312
    Ieee_Numeric_Std_Le_Sgn_Int = 313
    Ieee_Numeric_Std_Le_Int_Sgn = 314
    Ieee_Numeric_Std_Ge_Uns_Uns = 315
    Ieee_Numeric_Std_Ge_Uns_Nat = 316
    Ieee_Numeric_Std_Ge_Nat_Uns = 317
    Ieee_Numeric_Std_Ge_Sgn_Sgn = 318
    Ieee_Numeric_Std_Ge_Sgn_Int = 319
    Ieee_Numeric_Std_Ge_Int_Sgn = 320
    Ieee_Numeric_Std_Eq_Uns_Uns = 321
    Ieee_Numeric_Std_Eq_Uns_Nat = 322
    Ieee_Numeric_Std_Eq_Nat_Uns = 323
    Ieee_Numeric_Std_Eq_Sgn_Sgn = 324
    Ieee_Numeric_Std_Eq_Sgn_Int = 325
    Ieee_Numeric_Std_Eq_Int_Sgn = 326
    Ieee_Numeric_Std_Ne_Uns_Uns = 327
    Ieee_Numeric_Std_Ne_Uns_Nat = 328
    Ieee_Numeric_Std_Ne_Nat_Uns = 329
    Ieee_Numeric_Std_Ne_Sgn_Sgn = 330
    Ieee_Numeric_Std_Ne_Sgn_Int = 331
    Ieee_Numeric_Std_Ne_Int_Sgn = 332
    Ieee_Numeric_Std_Match_Gt_Uns_Uns = 333
    Ieee_Numeric_Std_Match_Gt_Uns_Nat = 334
    Ieee_Numeric_Std_Match_Gt_Nat_Uns = 335
    Ieee_Numeric_Std_Match_Gt_Sgn_Sgn = 336
    Ieee_Numeric_Std_Match_Gt_Sgn_Int = 337
    Ieee_Numeric_Std_Match_Gt_Int_Sgn = 338
    Ieee_Numeric_Std_Match_Lt_Uns_Uns = 339
    Ieee_Numeric_Std_Match_Lt_Uns_Nat = 340
    Ieee_Numeric_Std_Match_Lt_Nat_Uns = 341
    Ieee_Numeric_Std_Match_Lt_Sgn_Sgn = 342
    Ieee_Numeric_Std_Match_Lt_Sgn_Int = 343
    Ieee_Numeric_Std_Match_Lt_Int_Sgn = 344
    Ieee_Numeric_Std_Match_Le_Uns_Uns = 345
    Ieee_Numeric_Std_Match_Le_Uns_Nat = 346
    Ieee_Numeric_Std_Match_Le_Nat_Uns = 347
    Ieee_Numeric_Std_Match_Le_Sgn_Sgn = 348
    Ieee_Numeric_Std_Match_Le_Sgn_Int = 349
    Ieee_Numeric_Std_Match_Le_Int_Sgn = 350
    Ieee_Numeric_Std_Match_Ge_Uns_Uns = 351
    Ieee_Numeric_Std_Match_Ge_Uns_Nat = 352
    Ieee_Numeric_Std_Match_Ge_Nat_Uns = 353
    Ieee_Numeric_Std_Match_Ge_Sgn_Sgn = 354
    Ieee_Numeric_Std_Match_Ge_Sgn_Int = 355
    Ieee_Numeric_Std_Match_Ge_Int_Sgn = 356
    Ieee_Numeric_Std_Match_Eq_Uns_Uns = 357
    Ieee_Numeric_Std_Match_Eq_Uns_Nat = 358
    Ieee_Numeric_Std_Match_Eq_Nat_Uns = 359
    Ieee_Numeric_Std_Match_Eq_Sgn_Sgn = 360
    Ieee_Numeric_Std_Match_Eq_Sgn_Int = 361
    Ieee_Numeric_Std_Match_Eq_Int_Sgn = 362
    Ieee_Numeric_Std_Match_Ne_Uns_Uns = 363
    Ieee_Numeric_Std_Match_Ne_Uns_Nat = 364
    Ieee_Numeric_Std_Match_Ne_Nat_Uns = 365
    Ieee_Numeric_Std_Match_Ne_Sgn_Sgn = 366
    Ieee_Numeric_Std_Match_Ne_Sgn_Int = 367
    Ieee_Numeric_Std_Match_Ne_Int_Sgn = 368
    Ieee_Numeric_Std_Sll_Uns_Int = 369
    Ieee_Numeric_Std_Sll_Sgn_Int = 370
    Ieee_Numeric_Std_Srl_Uns_Int = 371
    Ieee_Numeric_Std_Srl_Sgn_Int = 372
    Ieee_Numeric_Std_Sla_Uns_Int = 373
    Ieee_Numeric_Std_Sla_Sgn_Int = 374
    Ieee_Numeric_Std_Sra_Uns_Int = 375
    Ieee_Numeric_Std_Sra_Sgn_Int = 376
    Ieee_Numeric_Std_And_Uns_Uns = 377
    Ieee_Numeric_Std_And_Sgn_Sgn = 378
    Ieee_Numeric_Std_Or_Uns_Uns = 379
    Ieee_Numeric_Std_Or_Sgn_Sgn = 380
    Ieee_Numeric_Std_Nand_Uns_Uns = 381
    Ieee_Numeric_Std_Nand_Sgn_Sgn = 382
    Ieee_Numeric_Std_Nor_Uns_Uns = 383
    Ieee_Numeric_Std_Nor_Sgn_Sgn = 384
    Ieee_Numeric_Std_Xor_Uns_Uns = 385
    Ieee_Numeric_Std_Xor_Sgn_Sgn = 386
    Ieee_Numeric_Std_Xnor_Uns_Uns = 387
    Ieee_Numeric_Std_Xnor_Sgn_Sgn = 388
    Ieee_Numeric_Std_Not_Uns = 389
    Ieee_Numeric_Std_Not_Sgn = 390
    Ieee_Numeric_Std_Abs_Sgn = 391
    Ieee_Numeric_Std_Neg_Uns = 392
    Ieee_Numeric_Std_Neg_Sgn = 393
    Ieee_Numeric_Std_Min_Uns_Uns = 394
    Ieee_Numeric_Std_Min_Uns_Nat = 395
    Ieee_Numeric_Std_Min_Nat_Uns = 396
    Ieee_Numeric_Std_Min_Sgn_Sgn = 397
    Ieee_Numeric_Std_Min_Sgn_Int = 398
    Ieee_Numeric_Std_Min_Int_Sgn = 399
    Ieee_Numeric_Std_Max_Uns_Uns = 400
    Ieee_Numeric_Std_Max_Uns_Nat = 401
    Ieee_Numeric_Std_Max_Nat_Uns = 402
    Ieee_Numeric_Std_Max_Sgn_Sgn = 403
    Ieee_Numeric_Std_Max_Sgn_Int = 404
    Ieee_Numeric_Std_Max_Int_Sgn = 405
    Ieee_Numeric_Std_Shf_Left_Uns_Nat = 406
    Ieee_Numeric_Std_Shf_Right_Uns_Nat = 407
    Ieee_Numeric_Std_Shf_Left_Sgn_Nat = 408
    Ieee_Numeric_Std_Shf_Right_Sgn_Nat = 409
    Ieee_Numeric_Std_Rot_Left_Uns_Nat = 410
    Ieee_Numeric_Std_Rot_Right_Uns_Nat = 411
    Ieee_Numeric_Std_Rot_Left_Sgn_Nat = 412
    Ieee_Numeric_Std_Rot_Right_Sgn_Nat = 413
    Ieee_Numeric_Std_And_Sgn = 414
    Ieee_Numeric_Std_Nand_Sgn = 415
    Ieee_Numeric_Std_Or_Sgn = 416
    Ieee_Numeric_Std_Nor_Sgn = 417
    Ieee_Numeric_Std_Xor_Sgn = 418
    Ieee_Numeric_Std_Xnor_Sgn = 419
    Ieee_Numeric_Std_And_Uns = 420
    Ieee_Numeric_Std_Nand_Uns = 421
    Ieee_Numeric_Std_Or_Uns = 422
    Ieee_Numeric_Std_Nor_Uns = 423
    Ieee_Numeric_Std_Xor_Uns = 424
    Ieee_Numeric_Std_Xnor_Uns = 425
    Ieee_Numeric_Std_Find_Leftmost_Uns = 426
    Ieee_Numeric_Std_Find_Rightmost_Uns = 427
    Ieee_Numeric_Std_Find_Leftmost_Sgn = 428
    Ieee_Numeric_Std_Find_Rightmost_Sgn = 429
    Ieee_Numeric_Std_Match_Log = 430
    Ieee_Numeric_Std_Match_Uns = 431
    Ieee_Numeric_Std_Match_Sgn = 432
    Ieee_Numeric_Std_Match_Slv = 433
    Ieee_Numeric_Std_Match_Suv = 434
    Ieee_Numeric_Std_To_01_Uns = 435
    Ieee_Numeric_Std_To_01_Sgn = 436
    Ieee_Numeric_Std_Unsigned_To_Integer_Slv_Nat = 437
    Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Nat_Slv = 438
    Ieee_Math_Real_Ceil = 439
    Ieee_Math_Real_Floor = 440
    Ieee_Math_Real_Round = 441
    Ieee_Math_Real_Log2 = 442
    Ieee_Math_Real_Sin = 443
    Ieee_Math_Real_Cos = 444
    Ieee_Math_Real_Arctan = 445
    Ieee_Math_Real_Pow = 446
    Ieee_Std_Logic_Unsigned_Add_Slv_Slv = 447
    Ieee_Std_Logic_Unsigned_Add_Slv_Int = 448
    Ieee_Std_Logic_Unsigned_Add_Int_Slv = 449
    Ieee_Std_Logic_Unsigned_Add_Slv_Log = 450
    Ieee_Std_Logic_Unsigned_Add_Log_Slv = 451
    Ieee_Std_Logic_Unsigned_Sub_Slv_Slv = 452
    Ieee_Std_Logic_Unsigned_Sub_Slv_Int = 453
    Ieee_Std_Logic_Unsigned_Sub_Int_Slv = 454
    Ieee_Std_Logic_Unsigned_Sub_Slv_Log = 455
    Ieee_Std_Logic_Unsigned_Sub_Log_Slv = 456
    Ieee_Std_Logic_Unsigned_Id_Slv = 457
    Ieee_Std_Logic_Unsigned_Mul_Slv_Slv = 458
    Ieee_Std_Logic_Unsigned_Lt_Slv_Slv = 459
    Ieee_Std_Logic_Unsigned_Lt_Slv_Int = 460
    Ieee_Std_Logic_Unsigned_Lt_Int_Slv = 461
    Ieee_Std_Logic_Unsigned_Le_Slv_Slv = 462
    Ieee_Std_Logic_Unsigned_Le_Slv_Int = 463
    Ieee_Std_Logic_Unsigned_Le_Int_Slv = 464
    Ieee_Std_Logic_Unsigned_Gt_Slv_Slv = 465
    Ieee_Std_Logic_Unsigned_Gt_Slv_Int = 466
    Ieee_Std_Logic_Unsigned_Gt_Int_Slv = 467
    Ieee_Std_Logic_Unsigned_Ge_Slv_Slv = 468
    Ieee_Std_Logic_Unsigned_Ge_Slv_Int = 469
    Ieee_Std_Logic_Unsigned_Ge_Int_Slv = 470
    Ieee_Std_Logic_Unsigned_Eq_Slv_Slv = 471
    Ieee_Std_Logic_Unsigned_Eq_Slv_Int = 472
    Ieee_Std_Logic_Unsigned_Eq_Int_Slv = 473
    Ieee_Std_Logic_Unsigned_Ne_Slv_Slv = 474
    Ieee_Std_Logic_Unsigned_Ne_Slv_Int = 475
    Ieee_Std_Logic_Unsigned_Ne_Int_Slv = 476
    Ieee_Std_Logic_Unsigned_Conv_Integer = 477
    Ieee_Std_Logic_Unsigned_Shl = 478
    Ieee_Std_Logic_Unsigned_Shr = 479
    Ieee_Std_Logic_Signed_Add_Slv_Slv = 480
    Ieee_Std_Logic_Signed_Add_Slv_Int = 481
    Ieee_Std_Logic_Signed_Add_Int_Slv = 482
    Ieee_Std_Logic_Signed_Add_Slv_Log = 483
    Ieee_Std_Logic_Signed_Add_Log_Slv = 484
    Ieee_Std_Logic_Signed_Sub_Slv_Slv = 485
    Ieee_Std_Logic_Signed_Sub_Slv_Int = 486
    Ieee_Std_Logic_Signed_Sub_Int_Slv = 487
    Ieee_Std_Logic_Signed_Sub_Slv_Log = 488
    Ieee_Std_Logic_Signed_Sub_Log_Slv = 489
    Ieee_Std_Logic_Signed_Id_Slv = 490
    Ieee_Std_Logic_Signed_Neg_Slv = 491
    Ieee_Std_Logic_Signed_Abs_Slv = 492
    Ieee_Std_Logic_Signed_Mul_Slv_Slv = 493
    Ieee_Std_Logic_Signed_Lt_Slv_Slv = 494
    Ieee_Std_Logic_Signed_Lt_Slv_Int = 495
    Ieee_Std_Logic_Signed_Lt_Int_Slv = 496
    Ieee_Std_Logic_Signed_Le_Slv_Slv = 497
    Ieee_Std_Logic_Signed_Le_Slv_Int = 498
    Ieee_Std_Logic_Signed_Le_Int_Slv = 499
    Ieee_Std_Logic_Signed_Gt_Slv_Slv = 500
    Ieee_Std_Logic_Signed_Gt_Slv_Int = 501
    Ieee_Std_Logic_Signed_Gt_Int_Slv = 502
    Ieee_Std_Logic_Signed_Ge_Slv_Slv = 503
    Ieee_Std_Logic_Signed_Ge_Slv_Int = 504
    Ieee_Std_Logic_Signed_Ge_Int_Slv = 505
    Ieee_Std_Logic_Signed_Eq_Slv_Slv = 506
    Ieee_Std_Logic_Signed_Eq_Slv_Int = 507
    Ieee_Std_Logic_Signed_Eq_Int_Slv = 508
    Ieee_Std_Logic_Signed_Ne_Slv_Slv = 509
    Ieee_Std_Logic_Signed_Ne_Slv_Int = 510
    Ieee_Std_Logic_Signed_Ne_Int_Slv = 511
    Ieee_Std_Logic_Signed_Conv_Integer = 512
    Ieee_Std_Logic_Signed_Shl = 513
    Ieee_Std_Logic_Signed_Shr = 514
    Ieee_Std_Logic_Arith_Conv_Unsigned_Int = 515
    Ieee_Std_Logic_Arith_Conv_Unsigned_Uns = 516
    Ieee_Std_Logic_Arith_Conv_Unsigned_Sgn = 517
    Ieee_Std_Logic_Arith_Conv_Unsigned_Log = 518
    Ieee_Std_Logic_Arith_Conv_Integer_Int = 519
    Ieee_Std_Logic_Arith_Conv_Integer_Uns = 520
    Ieee_Std_Logic_Arith_Conv_Integer_Sgn = 521
    Ieee_Std_Logic_Arith_Conv_Integer_Log = 522
    Ieee_Std_Logic_Arith_Conv_Vector_Int = 523
    Ieee_Std_Logic_Arith_Conv_Vector_Uns = 524
    Ieee_Std_Logic_Arith_Conv_Vector_Sgn = 525
    Ieee_Std_Logic_Arith_Conv_Vector_Log = 526
    Ieee_Std_Logic_Arith_Ext = 527
    Ieee_Std_Logic_Arith_Sxt = 528
    Ieee_Std_Logic_Arith_Id_Uns_Uns = 529
    Ieee_Std_Logic_Arith_Id_Sgn_Sgn = 530
    Ieee_Std_Logic_Arith_Neg_Sgn_Sgn = 531
    Ieee_Std_Logic_Arith_Abs_Sgn_Sgn = 532
    Ieee_Std_Logic_Arith_Shl_Uns = 533
    Ieee_Std_Logic_Arith_Shl_Sgn = 534
    Ieee_Std_Logic_Arith_Shr_Uns = 535
    Ieee_Std_Logic_Arith_Shr_Sgn = 536
    Ieee_Std_Logic_Arith_Id_Uns_Slv = 537
    Ieee_Std_Logic_Arith_Id_Sgn_Slv = 538
    Ieee_Std_Logic_Arith_Neg_Sgn_Slv = 539
    Ieee_Std_Logic_Arith_Abs_Sgn_Slv = 540
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns = 541
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn = 542
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn = 543
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn = 544
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv = 545
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv = 546
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv = 547
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv = 548
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns = 549
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn = 550
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn = 551
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn = 552
    Ieee_Std_Logic_Arith_Add_Uns_Int_Uns = 553
    Ieee_Std_Logic_Arith_Add_Int_Uns_Uns = 554
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn = 555
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn = 556
    Ieee_Std_Logic_Arith_Add_Uns_Log_Uns = 557
    Ieee_Std_Logic_Arith_Add_Log_Uns_Uns = 558
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn = 559
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn = 560
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv = 561
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv = 562
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv = 563
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv = 564
    Ieee_Std_Logic_Arith_Add_Uns_Int_Slv = 565
    Ieee_Std_Logic_Arith_Add_Int_Uns_Slv = 566
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv = 567
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv = 568
    Ieee_Std_Logic_Arith_Add_Uns_Log_Slv = 569
    Ieee_Std_Logic_Arith_Add_Log_Uns_Slv = 570
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv = 571
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv = 572
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns = 573
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn = 574
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn = 575
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn = 576
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns = 577
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns = 578
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn = 579
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn = 580
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns = 581
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns = 582
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn = 583
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn = 584
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv = 585
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv = 586
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv = 587
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv = 588
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv = 589
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv = 590
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv = 591
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv = 592
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv = 593
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv = 594
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv = 595
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv = 596
    Ieee_Std_Logic_Arith_Lt_Uns_Uns = 597
    Ieee_Std_Logic_Arith_Lt_Sgn_Sgn = 598
    Ieee_Std_Logic_Arith_Lt_Uns_Sgn = 599
    Ieee_Std_Logic_Arith_Lt_Sgn_Uns = 600
    Ieee_Std_Logic_Arith_Lt_Uns_Int = 601
    Ieee_Std_Logic_Arith_Lt_Int_Uns = 602
    Ieee_Std_Logic_Arith_Lt_Sgn_Int = 603
    Ieee_Std_Logic_Arith_Lt_Int_Sgn = 604
    Ieee_Std_Logic_Arith_Le_Uns_Uns = 605
    Ieee_Std_Logic_Arith_Le_Sgn_Sgn = 606
    Ieee_Std_Logic_Arith_Le_Uns_Sgn = 607
    Ieee_Std_Logic_Arith_Le_Sgn_Uns = 608
    Ieee_Std_Logic_Arith_Le_Uns_Int = 609
    Ieee_Std_Logic_Arith_Le_Int_Uns = 610
    Ieee_Std_Logic_Arith_Le_Sgn_Int = 611
    Ieee_Std_Logic_Arith_Le_Int_Sgn = 612
    Ieee_Std_Logic_Arith_Gt_Uns_Uns = 613
    Ieee_Std_Logic_Arith_Gt_Sgn_Sgn = 614
    Ieee_Std_Logic_Arith_Gt_Uns_Sgn = 615
    Ieee_Std_Logic_Arith_Gt_Sgn_Uns = 616
    Ieee_Std_Logic_Arith_Gt_Uns_Int = 617
    Ieee_Std_Logic_Arith_Gt_Int_Uns = 618
    Ieee_Std_Logic_Arith_Gt_Sgn_Int = 619
    Ieee_Std_Logic_Arith_Gt_Int_Sgn = 620
    Ieee_Std_Logic_Arith_Ge_Uns_Uns = 621
    Ieee_Std_Logic_Arith_Ge_Sgn_Sgn = 622
    Ieee_Std_Logic_Arith_Ge_Uns_Sgn = 623
    Ieee_Std_Logic_Arith_Ge_Sgn_Uns = 624
    Ieee_Std_Logic_Arith_Ge_Uns_Int = 625
    Ieee_Std_Logic_Arith_Ge_Int_Uns = 626
    Ieee_Std_Logic_Arith_Ge_Sgn_Int = 627
    Ieee_Std_Logic_Arith_Ge_Int_Sgn = 628
    Ieee_Std_Logic_Arith_Eq_Uns_Uns = 629
    Ieee_Std_Logic_Arith_Eq_Sgn_Sgn = 630
    Ieee_Std_Logic_Arith_Eq_Uns_Sgn = 631
    Ieee_Std_Logic_Arith_Eq_Sgn_Uns = 632
    Ieee_Std_Logic_Arith_Eq_Uns_Int = 633
    Ieee_Std_Logic_Arith_Eq_Int_Uns = 634
    Ieee_Std_Logic_Arith_Eq_Sgn_Int = 635
    Ieee_Std_Logic_Arith_Eq_Int_Sgn = 636
    Ieee_Std_Logic_Arith_Ne_Uns_Uns = 637
    Ieee_Std_Logic_Arith_Ne_Sgn_Sgn = 638
    Ieee_Std_Logic_Arith_Ne_Uns_Sgn = 639
    Ieee_Std_Logic_Arith_Ne_Sgn_Uns = 640
    Ieee_Std_Logic_Arith_Ne_Uns_Int = 641
    Ieee_Std_Logic_Arith_Ne_Int_Uns = 642
    Ieee_Std_Logic_Arith_Ne_Sgn_Int = 643
    Ieee_Std_Logic_Arith_Ne_Int_Sgn = 644
    Ieee_Std_Logic_Misc_And_Reduce_Slv = 645
    Ieee_Std_Logic_Misc_And_Reduce_Suv = 646
    Ieee_Std_Logic_Misc_Nand_Reduce_Slv = 647
    Ieee_Std_Logic_Misc_Nand_Reduce_Suv = 648
    Ieee_Std_Logic_Misc_Or_Reduce_Slv = 649
    Ieee_Std_Logic_Misc_Or_Reduce_Suv = 650
    Ieee_Std_Logic_Misc_Nor_Reduce_Slv = 651
    Ieee_Std_Logic_Misc_Nor_Reduce_Suv = 652
    Ieee_Std_Logic_Misc_Xor_Reduce_Slv = 653
    Ieee_Std_Logic_Misc_Xor_Reduce_Suv = 654
    Ieee_Std_Logic_Misc_Xnor_Reduce_Slv = 655
    Ieee_Std_Logic_Misc_Xnor_Reduce_Suv = 656


@export
@BindToLibGHDL("vhdl__nodes__get_kind")
def Get_Kind(node: Iir) -> IirKind:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_location")
def Get_Location(node: Iir) -> LocationType:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_first_design_unit")
def Get_First_Design_Unit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_first_design_unit")
def Set_First_Design_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_last_design_unit")
def Get_Last_Design_Unit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_last_design_unit")
def Set_Last_Design_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library_declaration")
def Get_Library_Declaration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_library_declaration")
def Set_Library_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_checksum")
def Get_File_Checksum(obj: Iir) -> FileChecksumId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_file_checksum")
def Set_File_Checksum(obj: Iir, value: FileChecksumId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_analysis_time_stamp")
def Get_Analysis_Time_Stamp(obj: Iir) -> TimeStampId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_analysis_time_stamp")
def Set_Analysis_Time_Stamp(obj: Iir, value: TimeStampId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_source")
def Get_Design_File_Source(obj: Iir) -> SourceFileEntry:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_source")
def Set_Design_File_Source(obj: Iir, value: SourceFileEntry) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library")
def Get_Library(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_library")
def Set_Library(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_dependence_list")
def Get_File_Dependence_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_file_dependence_list")
def Set_File_Dependence_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_filename")
def Get_Design_File_Filename(obj: Iir) -> NameId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_filename")
def Set_Design_File_Filename(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_directory")
def Get_Design_File_Directory(obj: Iir) -> NameId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_directory")
def Set_Design_File_Directory(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file")
def Get_Design_File(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_file")
def Set_Design_File(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_chain")
def Get_Design_File_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_chain")
def Set_Design_File_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library_directory")
def Get_Library_Directory(obj: Iir) -> NameId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_library_directory")
def Set_Library_Directory(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_date")
def Get_Date(obj: Iir) -> DateType:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_date")
def Set_Date(obj: Iir, value: DateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_context_items")
def Get_Context_Items(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_context_items")
def Set_Context_Items(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_dependence_list")
def Get_Dependence_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_dependence_list")
def Set_Dependence_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_analysis_checks_list")
def Get_Analysis_Checks_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_analysis_checks_list")
def Set_Analysis_Checks_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_date_state")
def Get_Date_State(obj: Iir) -> DateStateType:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_date_state")
def Set_Date_State(obj: Iir, value: DateStateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guarded_target_state")
def Get_Guarded_Target_State(obj: Iir) -> TriStateType:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_guarded_target_state")
def Set_Guarded_Target_State(obj: Iir, value: TriStateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library_unit")
def Get_Library_Unit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_library_unit")
def Set_Library_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_hash_chain")
def Get_Hash_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_hash_chain")
def Set_Hash_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_pos")
def Get_Design_Unit_Source_Pos(obj: Iir) -> SourcePtr:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_pos")
def Set_Design_Unit_Source_Pos(obj: Iir, value: SourcePtr) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_line")
def Get_Design_Unit_Source_Line(obj: Iir) -> Int32:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_line")
def Set_Design_Unit_Source_Line(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_col")
def Get_Design_Unit_Source_Col(obj: Iir) -> Int32:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_col")
def Set_Design_Unit_Source_Col(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_value")
def Get_Value(obj: Iir) -> Int64:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_value")
def Set_Value(obj: Iir, value: Int64) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_enum_pos")
def Get_Enum_Pos(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_enum_pos")
def Set_Enum_Pos(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_physical_literal")
def Get_Physical_Literal(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_physical_literal")
def Set_Physical_Literal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_fp_value")
def Get_Fp_Value(obj: Iir) -> Fp64:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_fp_value")
def Set_Fp_Value(obj: Iir, value: Fp64) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_aggregate_list")
def Get_Simple_Aggregate_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_simple_aggregate_list")
def Set_Simple_Aggregate_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_string8_id")
def Get_String8_Id(obj: Iir) -> String8Id:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_string8_id")
def Set_String8_Id(obj: Iir, value: String8Id) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_string_length")
def Get_String_Length(obj: Iir) -> Int32:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_string_length")
def Set_String_Length(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_bit_string_base")
def Get_Bit_String_Base(obj: Iir) -> NumberBaseType:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_bit_string_base")
def Set_Bit_String_Base(obj: Iir, value: NumberBaseType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_signed")
def Get_Has_Signed(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_signed")
def Set_Has_Signed(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_sign")
def Get_Has_Sign(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_sign")
def Set_Has_Sign(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_length")
def Get_Has_Length(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_length")
def Set_Has_Length(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_literal_length")
def Get_Literal_Length(obj: Iir) -> Int32:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_literal_length")
def Set_Literal_Length(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_literal_origin")
def Get_Literal_Origin(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_literal_origin")
def Set_Literal_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_range_origin")
def Get_Range_Origin(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_range_origin")
def Set_Range_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_literal_subtype")
def Get_Literal_Subtype(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_literal_subtype")
def Set_Literal_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_allocator_subtype")
def Get_Allocator_Subtype(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_allocator_subtype")
def Set_Allocator_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_class")
def Get_Entity_Class(obj: Iir) -> Tok:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_entity_class")
def Set_Entity_Class(obj: Iir, value: Tok) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_name_list")
def Get_Entity_Name_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_entity_name_list")
def Set_Entity_Name_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_designator")
def Get_Attribute_Designator(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_designator")
def Set_Attribute_Designator(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_specification_chain")
def Get_Attribute_Specification_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_specification_chain")
def Set_Attribute_Specification_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_specification")
def Get_Attribute_Specification(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_specification")
def Set_Attribute_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_static_attribute_flag")
def Get_Static_Attribute_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_static_attribute_flag")
def Set_Static_Attribute_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_list")
def Get_Signal_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_signal_list")
def Set_Signal_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_quantity_list")
def Get_Quantity_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_quantity_list")
def Set_Quantity_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_designated_entity")
def Get_Designated_Entity(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_designated_entity")
def Set_Designated_Entity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_formal")
def Get_Formal(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_formal")
def Set_Formal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual")
def Get_Actual(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_actual")
def Set_Actual(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual_conversion")
def Get_Actual_Conversion(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_actual_conversion")
def Set_Actual_Conversion(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_formal_conversion")
def Get_Formal_Conversion(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_formal_conversion")
def Set_Formal_Conversion(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_whole_association_flag")
def Get_Whole_Association_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_whole_association_flag")
def Set_Whole_Association_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_collapse_signal_flag")
def Get_Collapse_Signal_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_collapse_signal_flag")
def Set_Collapse_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_artificial_flag")
def Get_Artificial_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_artificial_flag")
def Set_Artificial_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_open_flag")
def Get_Open_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_open_flag")
def Set_Open_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_after_drivers_flag")
def Get_After_Drivers_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_after_drivers_flag")
def Set_After_Drivers_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_we_value")
def Get_We_Value(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_we_value")
def Set_We_Value(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_time")
def Get_Time(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_time")
def Set_Time(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_expr")
def Get_Associated_Expr(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_associated_expr")
def Set_Associated_Expr(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_block")
def Get_Associated_Block(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_associated_block")
def Set_Associated_Block(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_chain")
def Get_Associated_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_associated_chain")
def Set_Associated_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_name")
def Get_Choice_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_choice_name")
def Set_Choice_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_expression")
def Get_Choice_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_choice_expression")
def Set_Choice_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_range")
def Get_Choice_Range(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_choice_range")
def Set_Choice_Range(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_same_alternative_flag")
def Get_Same_Alternative_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_same_alternative_flag")
def Set_Same_Alternative_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_type_flag")
def Get_Element_Type_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_element_type_flag")
def Set_Element_Type_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_architecture")
def Get_Architecture(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_architecture")
def Set_Architecture(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_specification")
def Get_Block_Specification(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_block_specification")
def Set_Block_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_prev_block_configuration")
def Get_Prev_Block_Configuration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_prev_block_configuration")
def Set_Prev_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_item_chain")
def Get_Configuration_Item_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_item_chain")
def Set_Configuration_Item_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_value_chain")
def Get_Attribute_Value_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_value_chain")
def Set_Attribute_Value_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_spec_chain")
def Get_Spec_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_spec_chain")
def Set_Spec_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_value_chain")
def Get_Value_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_value_chain")
def Set_Value_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_value_spec_chain")
def Get_Attribute_Value_Spec_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_value_spec_chain")
def Set_Attribute_Value_Spec_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_name")
def Get_Entity_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_entity_name")
def Set_Entity_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package")
def Get_Package(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_package")
def Set_Package(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package_body")
def Get_Package_Body(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_package_body")
def Set_Package_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instance_package_body")
def Get_Instance_Package_Body(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_instance_package_body")
def Set_Instance_Package_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_need_body")
def Get_Need_Body(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_need_body")
def Set_Need_Body(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_macro_expanded_flag")
def Get_Macro_Expanded_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_macro_expanded_flag")
def Set_Macro_Expanded_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_need_instance_bodies")
def Get_Need_Instance_Bodies(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_need_instance_bodies")
def Set_Need_Instance_Bodies(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_hierarchical_name")
def Get_Hierarchical_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_hierarchical_name")
def Set_Hierarchical_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_inherit_spec_chain")
def Get_Inherit_Spec_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_inherit_spec_chain")
def Set_Inherit_Spec_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_vunit_item_chain")
def Get_Vunit_Item_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_vunit_item_chain")
def Set_Vunit_Item_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_bound_vunit_chain")
def Get_Bound_Vunit_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_bound_vunit_chain")
def Set_Bound_Vunit_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_verification_block_configuration")
def Get_Verification_Block_Configuration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_verification_block_configuration")
def Set_Verification_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_configuration")
def Get_Block_Configuration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_block_configuration")
def Set_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_concurrent_statement_chain")
def Get_Concurrent_Statement_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_concurrent_statement_chain")
def Set_Concurrent_Statement_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_chain")
def Get_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_chain")
def Set_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_port_chain")
def Get_Port_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_port_chain")
def Set_Port_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generic_chain")
def Get_Generic_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_generic_chain")
def Set_Generic_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type")
def Get_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_type")
def Set_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_indication")
def Get_Subtype_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_indication")
def Set_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_discrete_range")
def Get_Discrete_Range(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_discrete_range")
def Set_Discrete_Range(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_definition")
def Get_Type_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_type_definition")
def Set_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_definition")
def Get_Subtype_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_definition")
def Set_Subtype_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_incomplete_type_declaration")
def Get_Incomplete_Type_Declaration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_incomplete_type_declaration")
def Set_Incomplete_Type_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_interface_type_subprograms")
def Get_Interface_Type_Subprograms(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_interface_type_subprograms")
def Set_Interface_Type_Subprograms(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature_definition")
def Get_Nature_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_nature_definition")
def Set_Nature_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature")
def Get_Nature(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_nature")
def Set_Nature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subnature_indication")
def Get_Subnature_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subnature_indication")
def Set_Subnature_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_mode")
def Get_Mode(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_mode")
def Set_Mode(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guarded_signal_flag")
def Get_Guarded_Signal_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_guarded_signal_flag")
def Set_Guarded_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_kind")
def Get_Signal_Kind(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_signal_kind")
def Set_Signal_Kind(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_base_name")
def Get_Base_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_base_name")
def Set_Base_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_interface_declaration_chain")
def Get_Interface_Declaration_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_interface_declaration_chain")
def Set_Interface_Declaration_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_specification")
def Get_Subprogram_Specification(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_specification")
def Set_Subprogram_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_sequential_statement_chain")
def Get_Sequential_Statement_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_sequential_statement_chain")
def Set_Sequential_Statement_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_statement_chain")
def Get_Simultaneous_Statement_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_statement_chain")
def Set_Simultaneous_Statement_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_body")
def Get_Subprogram_Body(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_body")
def Set_Subprogram_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_overload_number")
def Get_Overload_Number(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_overload_number")
def Set_Overload_Number(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_depth")
def Get_Subprogram_Depth(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_depth")
def Set_Subprogram_Depth(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_hash")
def Get_Subprogram_Hash(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_hash")
def Set_Subprogram_Hash(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_impure_depth")
def Get_Impure_Depth(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_impure_depth")
def Set_Impure_Depth(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_return_type")
def Get_Return_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_return_type")
def Set_Return_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_implicit_definition")
def Get_Implicit_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_implicit_definition")
def Set_Implicit_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_subprogram_name")
def Get_Uninstantiated_Subprogram_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_subprogram_name")
def Set_Uninstantiated_Subprogram_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_value")
def Get_Default_Value(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_default_value")
def Set_Default_Value(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_deferred_declaration")
def Get_Deferred_Declaration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_deferred_declaration")
def Set_Deferred_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_deferred_declaration_flag")
def Get_Deferred_Declaration_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_deferred_declaration_flag")
def Set_Deferred_Declaration_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_shared_flag")
def Get_Shared_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_shared_flag")
def Set_Shared_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit")
def Get_Design_Unit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit")
def Set_Design_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_statement")
def Get_Block_Statement(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_block_statement")
def Set_Block_Statement(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_driver")
def Get_Signal_Driver(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_signal_driver")
def Set_Signal_Driver(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_declaration_chain")
def Get_Declaration_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_declaration_chain")
def Set_Declaration_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_logical_name")
def Get_File_Logical_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_file_logical_name")
def Set_File_Logical_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_open_kind")
def Get_File_Open_Kind(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_file_open_kind")
def Set_File_Open_Kind(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_position")
def Get_Element_Position(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_element_position")
def Set_Element_Position(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_use_clause_chain")
def Get_Use_Clause_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_use_clause_chain")
def Set_Use_Clause_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_context_reference_chain")
def Get_Context_Reference_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_context_reference_chain")
def Set_Context_Reference_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_selected_name")
def Get_Selected_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_selected_name")
def Set_Selected_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_declarator")
def Get_Type_Declarator(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_type_declarator")
def Set_Type_Declarator(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_complete_type_definition")
def Get_Complete_Type_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_complete_type_definition")
def Set_Complete_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_incomplete_type_ref_chain")
def Get_Incomplete_Type_Ref_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_incomplete_type_ref_chain")
def Set_Incomplete_Type_Ref_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_type")
def Get_Associated_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_associated_type")
def Set_Associated_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_enumeration_literal_list")
def Get_Enumeration_Literal_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_enumeration_literal_list")
def Set_Enumeration_Literal_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_class_entry_chain")
def Get_Entity_Class_Entry_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_entity_class_entry_chain")
def Set_Entity_Class_Entry_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_group_constituent_list")
def Get_Group_Constituent_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_group_constituent_list")
def Set_Group_Constituent_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_unit_chain")
def Get_Unit_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_unit_chain")
def Set_Unit_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_primary_unit")
def Get_Primary_Unit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_primary_unit")
def Set_Primary_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_identifier")
def Get_Identifier(obj: Iir) -> NameId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_identifier")
def Set_Identifier(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_label")
def Get_Label(obj: Iir) -> NameId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_label")
def Set_Label(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_visible_flag")
def Get_Visible_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_visible_flag")
def Set_Visible_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_range_constraint")
def Get_Range_Constraint(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_range_constraint")
def Set_Range_Constraint(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_direction")
def Get_Direction(obj: Iir) -> DirectionType:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_direction")
def Set_Direction(obj: Iir, value: DirectionType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_left_limit")
def Get_Left_Limit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_left_limit")
def Set_Left_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_right_limit")
def Get_Right_Limit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_right_limit")
def Set_Right_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_left_limit_expr")
def Get_Left_Limit_Expr(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_left_limit_expr")
def Set_Left_Limit_Expr(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_right_limit_expr")
def Get_Right_Limit_Expr(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_right_limit_expr")
def Set_Right_Limit_Expr(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parent_type")
def Get_Parent_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parent_type")
def Set_Parent_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_nature")
def Get_Simple_Nature(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_simple_nature")
def Set_Simple_Nature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_base_nature")
def Get_Base_Nature(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_base_nature")
def Set_Base_Nature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_resolution_indication")
def Get_Resolution_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_resolution_indication")
def Set_Resolution_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_record_element_resolution_chain")
def Get_Record_Element_Resolution_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_record_element_resolution_chain")
def Set_Record_Element_Resolution_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_tolerance")
def Get_Tolerance(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_tolerance")
def Set_Tolerance(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_plus_terminal_name")
def Get_Plus_Terminal_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_plus_terminal_name")
def Set_Plus_Terminal_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_minus_terminal_name")
def Get_Minus_Terminal_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_minus_terminal_name")
def Set_Minus_Terminal_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_plus_terminal")
def Get_Plus_Terminal(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_plus_terminal")
def Set_Plus_Terminal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_minus_terminal")
def Get_Minus_Terminal(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_minus_terminal")
def Set_Minus_Terminal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_magnitude_expression")
def Get_Magnitude_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_magnitude_expression")
def Set_Magnitude_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_phase_expression")
def Get_Phase_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_phase_expression")
def Set_Phase_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_power_expression")
def Get_Power_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_power_expression")
def Set_Power_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_left")
def Get_Simultaneous_Left(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_left")
def Set_Simultaneous_Left(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_right")
def Get_Simultaneous_Right(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_right")
def Set_Simultaneous_Right(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_text_file_flag")
def Get_Text_File_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_text_file_flag")
def Set_Text_File_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_only_characters_flag")
def Get_Only_Characters_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_only_characters_flag")
def Set_Only_Characters_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_character_type")
def Get_Is_Character_Type(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_is_character_type")
def Set_Is_Character_Type(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature_staticness")
def Get_Nature_Staticness(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_nature_staticness")
def Set_Nature_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_staticness")
def Get_Type_Staticness(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_type_staticness")
def Set_Type_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_constraint_state")
def Get_Constraint_State(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_constraint_state")
def Set_Constraint_State(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype_list")
def Get_Index_Subtype_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype_list")
def Set_Index_Subtype_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype_definition_list")
def Get_Index_Subtype_Definition_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype_definition_list")
def Set_Index_Subtype_Definition_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subtype_indication")
def Get_Element_Subtype_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_element_subtype_indication")
def Set_Element_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subtype")
def Get_Element_Subtype(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_element_subtype")
def Set_Element_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subnature_indication")
def Get_Element_Subnature_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_element_subnature_indication")
def Set_Element_Subnature_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subnature")
def Get_Element_Subnature(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_element_subnature")
def Set_Element_Subnature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_constraint_list")
def Get_Index_Constraint_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_index_constraint_list")
def Set_Index_Constraint_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_array_element_constraint")
def Get_Array_Element_Constraint(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_array_element_constraint")
def Set_Array_Element_Constraint(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_array_constraint_flag")
def Get_Has_Array_Constraint_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_array_constraint_flag")
def Set_Has_Array_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_element_constraint_flag")
def Get_Has_Element_Constraint_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_element_constraint_flag")
def Set_Has_Element_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_elements_declaration_list")
def Get_Elements_Declaration_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_elements_declaration_list")
def Set_Elements_Declaration_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_owned_elements_chain")
def Get_Owned_Elements_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_owned_elements_chain")
def Set_Owned_Elements_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_designated_type")
def Get_Designated_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_designated_type")
def Set_Designated_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_designated_subtype_indication")
def Get_Designated_Subtype_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_designated_subtype_indication")
def Set_Designated_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_list")
def Get_Index_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_index_list")
def Set_Index_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_reference")
def Get_Reference(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_reference")
def Set_Reference(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature_declarator")
def Get_Nature_Declarator(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_nature_declarator")
def Set_Nature_Declarator(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_across_type_mark")
def Get_Across_Type_Mark(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_across_type_mark")
def Set_Across_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_through_type_mark")
def Get_Through_Type_Mark(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_through_type_mark")
def Set_Through_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_across_type_definition")
def Get_Across_Type_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_across_type_definition")
def Set_Across_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_through_type_definition")
def Get_Through_Type_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_through_type_definition")
def Set_Through_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_across_type")
def Get_Across_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_across_type")
def Set_Across_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_through_type")
def Get_Through_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_through_type")
def Set_Through_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_target")
def Get_Target(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_target")
def Set_Target(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_waveform_chain")
def Get_Waveform_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_waveform_chain")
def Set_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard")
def Get_Guard(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_guard")
def Set_Guard(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_delay_mechanism")
def Get_Delay_Mechanism(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_delay_mechanism")
def Set_Delay_Mechanism(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_reject_time_expression")
def Get_Reject_Time_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_reject_time_expression")
def Set_Reject_Time_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_force_mode")
def Get_Force_Mode(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_force_mode")
def Set_Force_Mode(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_force_mode")
def Get_Has_Force_Mode(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_force_mode")
def Set_Has_Force_Mode(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_sensitivity_list")
def Get_Sensitivity_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_sensitivity_list")
def Set_Sensitivity_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_process_origin")
def Get_Process_Origin(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_process_origin")
def Set_Process_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package_origin")
def Get_Package_Origin(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_package_origin")
def Set_Package_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_condition_clause")
def Get_Condition_Clause(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_condition_clause")
def Set_Condition_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_break_element")
def Get_Break_Element(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_break_element")
def Set_Break_Element(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_selector_quantity")
def Get_Selector_Quantity(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_selector_quantity")
def Set_Selector_Quantity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_break_quantity")
def Get_Break_Quantity(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_break_quantity")
def Set_Break_Quantity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_timeout_clause")
def Get_Timeout_Clause(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_timeout_clause")
def Set_Timeout_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_postponed_flag")
def Get_Postponed_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_postponed_flag")
def Set_Postponed_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_callees_list")
def Get_Callees_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_callees_list")
def Set_Callees_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_passive_flag")
def Get_Passive_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_passive_flag")
def Set_Passive_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_resolution_function_flag")
def Get_Resolution_Function_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_resolution_function_flag")
def Set_Resolution_Function_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_wait_state")
def Get_Wait_State(obj: Iir) -> TriStateType:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_wait_state")
def Set_Wait_State(obj: Iir, value: TriStateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_all_sensitized_state")
def Get_All_Sensitized_State(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_all_sensitized_state")
def Set_All_Sensitized_State(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_seen_flag")
def Get_Seen_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_seen_flag")
def Set_Seen_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_pure_flag")
def Get_Pure_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_pure_flag")
def Set_Pure_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_foreign_flag")
def Get_Foreign_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_foreign_flag")
def Set_Foreign_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_resolved_flag")
def Get_Resolved_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_resolved_flag")
def Set_Resolved_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_type_flag")
def Get_Signal_Type_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_signal_type_flag")
def Set_Signal_Type_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_signal_flag")
def Get_Has_Signal_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_signal_flag")
def Set_Has_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_purity_state")
def Get_Purity_State(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_purity_state")
def Set_Purity_State(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_elab_flag")
def Get_Elab_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_elab_flag")
def Set_Elab_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_vendor_library_flag")
def Get_Vendor_Library_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_vendor_library_flag")
def Set_Vendor_Library_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_mark_flag")
def Get_Configuration_Mark_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_mark_flag")
def Set_Configuration_Mark_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_done_flag")
def Get_Configuration_Done_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_done_flag")
def Set_Configuration_Done_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_constraint_flag")
def Get_Index_Constraint_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_index_constraint_flag")
def Set_Index_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_hide_implicit_flag")
def Get_Hide_Implicit_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_hide_implicit_flag")
def Set_Hide_Implicit_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_assertion_condition")
def Get_Assertion_Condition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_assertion_condition")
def Set_Assertion_Condition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_report_expression")
def Get_Report_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_report_expression")
def Set_Report_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_severity_expression")
def Get_Severity_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_severity_expression")
def Set_Severity_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instantiated_unit")
def Get_Instantiated_Unit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_instantiated_unit")
def Set_Instantiated_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generic_map_aspect_chain")
def Get_Generic_Map_Aspect_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_generic_map_aspect_chain")
def Set_Generic_Map_Aspect_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_port_map_aspect_chain")
def Get_Port_Map_Aspect_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_port_map_aspect_chain")
def Set_Port_Map_Aspect_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_name")
def Get_Configuration_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_name")
def Set_Configuration_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_component_configuration")
def Get_Component_Configuration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_component_configuration")
def Set_Component_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_specification")
def Get_Configuration_Specification(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_specification")
def Set_Configuration_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_binding_indication")
def Get_Default_Binding_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_default_binding_indication")
def Set_Default_Binding_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_configuration_declaration")
def Get_Default_Configuration_Declaration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_default_configuration_declaration")
def Set_Default_Configuration_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_expression")
def Get_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_expression")
def Set_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_conditional_expression_chain")
def Get_Conditional_Expression_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_conditional_expression_chain")
def Set_Conditional_Expression_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_allocator_designated_type")
def Get_Allocator_Designated_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_allocator_designated_type")
def Set_Allocator_Designated_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_selected_waveform_chain")
def Get_Selected_Waveform_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_selected_waveform_chain")
def Set_Selected_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_conditional_waveform_chain")
def Get_Conditional_Waveform_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_conditional_waveform_chain")
def Set_Conditional_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard_expression")
def Get_Guard_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_guard_expression")
def Set_Guard_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard_decl")
def Get_Guard_Decl(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_guard_decl")
def Set_Guard_Decl(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard_sensitivity_list")
def Get_Guard_Sensitivity_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_guard_sensitivity_list")
def Set_Guard_Sensitivity_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_attribute_chain")
def Get_Signal_Attribute_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_signal_attribute_chain")
def Set_Signal_Attribute_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_block_configuration")
def Get_Block_Block_Configuration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_block_block_configuration")
def Set_Block_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package_header")
def Get_Package_Header(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_package_header")
def Set_Package_Header(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_header")
def Get_Block_Header(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_block_header")
def Set_Block_Header(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_package_name")
def Get_Uninstantiated_Package_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_package_name")
def Set_Uninstantiated_Package_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_package_decl")
def Get_Uninstantiated_Package_Decl(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_package_decl")
def Set_Uninstantiated_Package_Decl(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instance_source_file")
def Get_Instance_Source_File(obj: Iir) -> SourceFileEntry:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_instance_source_file")
def Set_Instance_Source_File(obj: Iir, value: SourceFileEntry) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generate_block_configuration")
def Get_Generate_Block_Configuration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_generate_block_configuration")
def Set_Generate_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generate_statement_body")
def Get_Generate_Statement_Body(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_generate_statement_body")
def Set_Generate_Statement_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_alternative_label")
def Get_Alternative_Label(obj: Iir) -> NameId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_alternative_label")
def Set_Alternative_Label(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generate_else_clause")
def Get_Generate_Else_Clause(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_generate_else_clause")
def Set_Generate_Else_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_condition")
def Get_Condition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_condition")
def Set_Condition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_else_clause")
def Get_Else_Clause(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_else_clause")
def Set_Else_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_specification")
def Get_Parameter_Specification(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_specification")
def Set_Parameter_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parent")
def Get_Parent(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parent")
def Set_Parent(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_loop_label")
def Get_Loop_Label(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_loop_label")
def Set_Loop_Label(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_exit_flag")
def Get_Exit_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_exit_flag")
def Set_Exit_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_next_flag")
def Get_Next_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_next_flag")
def Set_Next_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_component_name")
def Get_Component_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_component_name")
def Set_Component_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instantiation_list")
def Get_Instantiation_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_instantiation_list")
def Set_Instantiation_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_aspect")
def Get_Entity_Aspect(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_entity_aspect")
def Set_Entity_Aspect(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_entity_aspect")
def Get_Default_Entity_Aspect(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_default_entity_aspect")
def Set_Default_Entity_Aspect(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_binding_indication")
def Get_Binding_Indication(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_binding_indication")
def Set_Binding_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_named_entity")
def Get_Named_Entity(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_named_entity")
def Set_Named_Entity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_referenced_name")
def Get_Referenced_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_referenced_name")
def Set_Referenced_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_expr_staticness")
def Get_Expr_Staticness(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_expr_staticness")
def Set_Expr_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_scalar_size")
def Get_Scalar_Size(obj: Iir) -> ScalarSize:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_scalar_size")
def Set_Scalar_Size(obj: Iir, value: ScalarSize) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_error_origin")
def Get_Error_Origin(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_error_origin")
def Set_Error_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_operand")
def Get_Operand(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_operand")
def Set_Operand(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_left")
def Get_Left(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_left")
def Set_Left(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_right")
def Get_Right(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_right")
def Set_Right(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_unit_name")
def Get_Unit_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_unit_name")
def Set_Unit_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_name")
def Get_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_name")
def Set_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_group_template_name")
def Get_Group_Template_Name(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_group_template_name")
def Set_Group_Template_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_name_staticness")
def Get_Name_Staticness(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_name_staticness")
def Set_Name_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_prefix")
def Get_Prefix(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_prefix")
def Set_Prefix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signature_prefix")
def Get_Signature_Prefix(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_signature_prefix")
def Set_Signature_Prefix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_external_pathname")
def Get_External_Pathname(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_external_pathname")
def Set_External_Pathname(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_pathname_suffix")
def Get_Pathname_Suffix(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_pathname_suffix")
def Set_Pathname_Suffix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_pathname_expression")
def Get_Pathname_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_pathname_expression")
def Set_Pathname_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_in_formal_flag")
def Get_In_Formal_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_in_formal_flag")
def Set_In_Formal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_slice_subtype")
def Get_Slice_Subtype(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_slice_subtype")
def Set_Slice_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_suffix")
def Get_Suffix(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_suffix")
def Set_Suffix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype")
def Get_Index_Subtype(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype")
def Set_Index_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter")
def Get_Parameter(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parameter")
def Set_Parameter(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_2")
def Get_Parameter_2(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_2")
def Set_Parameter_2(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_3")
def Get_Parameter_3(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_3")
def Set_Parameter_3(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_4")
def Get_Parameter_4(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_4")
def Set_Parameter_4(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attr_chain")
def Get_Attr_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_attr_chain")
def Set_Attr_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_attribute_declaration")
def Get_Signal_Attribute_Declaration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_signal_attribute_declaration")
def Set_Signal_Attribute_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual_type")
def Get_Actual_Type(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_actual_type")
def Set_Actual_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual_type_definition")
def Get_Actual_Type_Definition(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_actual_type_definition")
def Set_Actual_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_association_chain")
def Get_Association_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_association_chain")
def Set_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_individual_association_chain")
def Get_Individual_Association_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_individual_association_chain")
def Set_Individual_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_association_chain")
def Get_Subprogram_Association_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_association_chain")
def Set_Subprogram_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggregate_info")
def Get_Aggregate_Info(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggregate_info")
def Set_Aggregate_Info(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_sub_aggregate_info")
def Get_Sub_Aggregate_Info(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_sub_aggregate_info")
def Set_Sub_Aggregate_Info(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_dynamic_flag")
def Get_Aggr_Dynamic_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_dynamic_flag")
def Set_Aggr_Dynamic_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_min_length")
def Get_Aggr_Min_Length(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_min_length")
def Set_Aggr_Min_Length(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_low_limit")
def Get_Aggr_Low_Limit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_low_limit")
def Set_Aggr_Low_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_high_limit")
def Get_Aggr_High_Limit(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_high_limit")
def Set_Aggr_High_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_others_flag")
def Get_Aggr_Others_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_others_flag")
def Set_Aggr_Others_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_named_flag")
def Get_Aggr_Named_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_named_flag")
def Set_Aggr_Named_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggregate_expand_flag")
def Get_Aggregate_Expand_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_aggregate_expand_flag")
def Set_Aggregate_Expand_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_association_choices_chain")
def Get_Association_Choices_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_association_choices_chain")
def Set_Association_Choices_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_case_statement_alternative_chain")
def Get_Case_Statement_Alternative_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_case_statement_alternative_chain")
def Set_Case_Statement_Alternative_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_staticness")
def Get_Choice_Staticness(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_choice_staticness")
def Set_Choice_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_procedure_call")
def Get_Procedure_Call(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_procedure_call")
def Set_Procedure_Call(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_implementation")
def Get_Implementation(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_implementation")
def Set_Implementation(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_association_chain")
def Get_Parameter_Association_Chain(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_association_chain")
def Set_Parameter_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_method_object")
def Get_Method_Object(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_method_object")
def Set_Method_Object(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_type_mark")
def Get_Subtype_Type_Mark(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_type_mark")
def Set_Subtype_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subnature_nature_mark")
def Get_Subnature_Nature_Mark(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_subnature_nature_mark")
def Set_Subnature_Nature_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_conversion_subtype")
def Get_Type_Conversion_Subtype(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_type_conversion_subtype")
def Set_Type_Conversion_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_mark")
def Get_Type_Mark(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_type_mark")
def Set_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_type_mark")
def Get_File_Type_Mark(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_file_type_mark")
def Set_File_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_return_type_mark")
def Get_Return_Type_Mark(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_return_type_mark")
def Set_Return_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_disconnect_flag")
def Get_Has_Disconnect_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_disconnect_flag")
def Set_Has_Disconnect_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_active_flag")
def Get_Has_Active_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_active_flag")
def Set_Has_Active_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_within_flag")
def Get_Is_Within_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_is_within_flag")
def Set_Is_Within_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_marks_list")
def Get_Type_Marks_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_type_marks_list")
def Set_Type_Marks_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_implicit_alias_flag")
def Get_Implicit_Alias_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_implicit_alias_flag")
def Set_Implicit_Alias_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_alias_signature")
def Get_Alias_Signature(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_alias_signature")
def Set_Alias_Signature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_signature")
def Get_Attribute_Signature(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_signature")
def Set_Attribute_Signature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_overload_list")
def Get_Overload_List(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_overload_list")
def Set_Overload_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_name_identifier")
def Get_Simple_Name_Identifier(obj: Iir) -> NameId:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_simple_name_identifier")
def Set_Simple_Name_Identifier(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_name_subtype")
def Get_Simple_Name_Subtype(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_simple_name_subtype")
def Set_Simple_Name_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_protected_type_body")
def Get_Protected_Type_Body(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_protected_type_body")
def Set_Protected_Type_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_protected_type_declaration")
def Get_Protected_Type_Declaration(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_protected_type_declaration")
def Set_Protected_Type_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_use_flag")
def Get_Use_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_use_flag")
def Set_Use_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_reserved_id")
def Get_End_Has_Reserved_Id(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_reserved_id")
def Set_End_Has_Reserved_Id(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_identifier")
def Get_End_Has_Identifier(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_identifier")
def Set_End_Has_Identifier(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_postponed")
def Get_End_Has_Postponed(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_postponed")
def Set_End_Has_Postponed(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_label")
def Get_Has_Label(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_label")
def Set_Has_Label(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_begin")
def Get_Has_Begin(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_begin")
def Set_Has_Begin(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_end")
def Get_Has_End(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_end")
def Set_Has_End(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_is")
def Get_Has_Is(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_is")
def Set_Has_Is(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_pure")
def Get_Has_Pure(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_pure")
def Set_Has_Pure(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_body")
def Get_Has_Body(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_body")
def Set_Has_Body(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_parameter")
def Get_Has_Parameter(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_parameter")
def Set_Has_Parameter(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_component")
def Get_Has_Component(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_component")
def Set_Has_Component(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_identifier_list")
def Get_Has_Identifier_List(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_identifier_list")
def Set_Has_Identifier_List(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_mode")
def Get_Has_Mode(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_mode")
def Set_Has_Mode(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_class")
def Get_Has_Class(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_class")
def Set_Has_Class(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_delay_mechanism")
def Get_Has_Delay_Mechanism(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_has_delay_mechanism")
def Set_Has_Delay_Mechanism(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_flag")
def Get_Suspend_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_flag")
def Set_Suspend_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_ref")
def Get_Is_Ref(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_is_ref")
def Set_Is_Ref(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_forward_ref")
def Get_Is_Forward_Ref(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_is_forward_ref")
def Set_Is_Forward_Ref(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_property")
def Get_Psl_Property(obj: Iir) -> PSLNode:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_property")
def Set_Psl_Property(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_sequence")
def Get_Psl_Sequence(obj: Iir) -> PSLNode:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_sequence")
def Set_Psl_Sequence(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_declaration")
def Get_Psl_Declaration(obj: Iir) -> PSLNode:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_declaration")
def Set_Psl_Declaration(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_expression")
def Get_Psl_Expression(obj: Iir) -> PSLNode:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_expression")
def Set_Psl_Expression(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_boolean")
def Get_Psl_Boolean(obj: Iir) -> PSLNode:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_boolean")
def Set_Psl_Boolean(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_clock")
def Get_PSL_Clock(obj: Iir) -> PSLNode:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_clock")
def Set_PSL_Clock(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_nfa")
def Get_PSL_NFA(obj: Iir) -> PSLNFA:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_nfa")
def Set_PSL_NFA(obj: Iir, value: PSLNFA) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_nbr_states")
def Get_PSL_Nbr_States(obj: Iir) -> Int32:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_nbr_states")
def Set_PSL_Nbr_States(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_clock_sensitivity")
def Get_PSL_Clock_Sensitivity(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_clock_sensitivity")
def Set_PSL_Clock_Sensitivity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_eos_flag")
def Get_PSL_EOS_Flag(obj: Iir) -> Boolean:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_psl_eos_flag")
def Set_PSL_EOS_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_count_expression")
def Get_Count_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_count_expression")
def Set_Count_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_clock_expression")
def Get_Clock_Expression(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_clock_expression")
def Set_Clock_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_clock")
def Get_Default_Clock(obj: Iir) -> Iir:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_default_clock")
def Set_Default_Clock(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_foreign_node")
def Get_Foreign_Node(obj: Iir) -> Int32:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__set_foreign_node")
def Set_Foreign_Node(obj: Iir, value: Int32) -> None:
    """"""
