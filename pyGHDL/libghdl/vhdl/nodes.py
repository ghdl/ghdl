# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
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
    Break_Element = 50
    Attribute_Specification = 51
    Disconnection_Specification = 52
    Step_Limit_Specification = 53
    Configuration_Specification = 54
    Access_Type_Definition = 55
    Incomplete_Type_Definition = 56
    Interface_Type_Definition = 57
    File_Type_Definition = 58
    Protected_Type_Declaration = 59
    Record_Type_Definition = 60
    Array_Type_Definition = 61
    Array_Subtype_Definition = 62
    Record_Subtype_Definition = 63
    Access_Subtype_Definition = 64
    Physical_Subtype_Definition = 65
    Floating_Subtype_Definition = 66
    Integer_Subtype_Definition = 67
    Enumeration_Subtype_Definition = 68
    Enumeration_Type_Definition = 69
    Integer_Type_Definition = 70
    Floating_Type_Definition = 71
    Physical_Type_Definition = 72
    Range_Expression = 73
    Protected_Type_Body = 74
    Wildcard_Type_Definition = 75
    Foreign_Vector_Type_Definition = 76
    Subtype_Definition = 77
    Scalar_Nature_Definition = 78
    Record_Nature_Definition = 79
    Array_Nature_Definition = 80
    Array_Subnature_Definition = 81
    Overload_List = 82
    Foreign_Module = 83
    Entity_Declaration = 84
    Configuration_Declaration = 85
    Context_Declaration = 86
    Package_Declaration = 87
    Package_Instantiation_Declaration = 88
    Vmode_Declaration = 89
    Vprop_Declaration = 90
    Vunit_Declaration = 91
    Package_Body = 92
    Architecture_Body = 93
    Type_Declaration = 94
    Anonymous_Type_Declaration = 95
    Subtype_Declaration = 96
    Nature_Declaration = 97
    Subnature_Declaration = 98
    Package_Header = 99
    Unit_Declaration = 100
    Library_Declaration = 101
    Component_Declaration = 102
    Attribute_Declaration = 103
    Group_Template_Declaration = 104
    Group_Declaration = 105
    Element_Declaration = 106
    Nature_Element_Declaration = 107
    Non_Object_Alias_Declaration = 108
    Psl_Declaration = 109
    Psl_Endpoint_Declaration = 110
    Enumeration_Literal = 111
    Function_Declaration = 112
    Procedure_Declaration = 113
    Function_Body = 114
    Procedure_Body = 115
    Function_Instantiation_Declaration = 116
    Procedure_Instantiation_Declaration = 117
    Terminal_Declaration = 118
    Object_Alias_Declaration = 119
    Free_Quantity_Declaration = 120
    Spectrum_Quantity_Declaration = 121
    Noise_Quantity_Declaration = 122
    Across_Quantity_Declaration = 123
    Through_Quantity_Declaration = 124
    File_Declaration = 125
    Guard_Signal_Declaration = 126
    Signal_Declaration = 127
    Variable_Declaration = 128
    Constant_Declaration = 129
    Iterator_Declaration = 130
    Interface_Constant_Declaration = 131
    Interface_Variable_Declaration = 132
    Interface_Signal_Declaration = 133
    Interface_File_Declaration = 134
    Interface_Quantity_Declaration = 135
    Interface_Terminal_Declaration = 136
    Interface_Type_Declaration = 137
    Interface_Package_Declaration = 138
    Interface_Function_Declaration = 139
    Interface_Procedure_Declaration = 140
    Signal_Attribute_Declaration = 141
    Suspend_State_Declaration = 142
    Identity_Operator = 143
    Negation_Operator = 144
    Absolute_Operator = 145
    Not_Operator = 146
    Implicit_Condition_Operator = 147
    Condition_Operator = 148
    Reduction_And_Operator = 149
    Reduction_Or_Operator = 150
    Reduction_Nand_Operator = 151
    Reduction_Nor_Operator = 152
    Reduction_Xor_Operator = 153
    Reduction_Xnor_Operator = 154
    And_Operator = 155
    Or_Operator = 156
    Nand_Operator = 157
    Nor_Operator = 158
    Xor_Operator = 159
    Xnor_Operator = 160
    Equality_Operator = 161
    Inequality_Operator = 162
    Less_Than_Operator = 163
    Less_Than_Or_Equal_Operator = 164
    Greater_Than_Operator = 165
    Greater_Than_Or_Equal_Operator = 166
    Match_Equality_Operator = 167
    Match_Inequality_Operator = 168
    Match_Less_Than_Operator = 169
    Match_Less_Than_Or_Equal_Operator = 170
    Match_Greater_Than_Operator = 171
    Match_Greater_Than_Or_Equal_Operator = 172
    Sll_Operator = 173
    Sla_Operator = 174
    Srl_Operator = 175
    Sra_Operator = 176
    Rol_Operator = 177
    Ror_Operator = 178
    Addition_Operator = 179
    Substraction_Operator = 180
    Concatenation_Operator = 181
    Multiplication_Operator = 182
    Division_Operator = 183
    Modulus_Operator = 184
    Remainder_Operator = 185
    Exponentiation_Operator = 186
    Function_Call = 187
    Aggregate = 188
    Parenthesis_Expression = 189
    Qualified_Expression = 190
    Type_Conversion = 191
    Allocator_By_Expression = 192
    Allocator_By_Subtype = 193
    Selected_Element = 194
    Dereference = 195
    Implicit_Dereference = 196
    Slice_Name = 197
    Indexed_Name = 198
    Psl_Prev = 199
    Psl_Stable = 200
    Psl_Rose = 201
    Psl_Fell = 202
    Psl_Onehot = 203
    Psl_Onehot0 = 204
    Psl_Expression = 205
    Sensitized_Process_Statement = 206
    Process_Statement = 207
    Concurrent_Simple_Signal_Assignment = 208
    Concurrent_Conditional_Signal_Assignment = 209
    Concurrent_Selected_Signal_Assignment = 210
    Concurrent_Assertion_Statement = 211
    Concurrent_Procedure_Call_Statement = 212
    Concurrent_Break_Statement = 213
    Psl_Assert_Directive = 214
    Psl_Assume_Directive = 215
    Psl_Cover_Directive = 216
    Psl_Restrict_Directive = 217
    Block_Statement = 218
    If_Generate_Statement = 219
    Case_Generate_Statement = 220
    For_Generate_Statement = 221
    Component_Instantiation_Statement = 222
    Psl_Default_Clock = 223
    Generate_Statement_Body = 224
    If_Generate_Else_Clause = 225
    Simple_Simultaneous_Statement = 226
    Simultaneous_Null_Statement = 227
    Simultaneous_Procedural_Statement = 228
    Simultaneous_Case_Statement = 229
    Simultaneous_If_Statement = 230
    Simultaneous_Elsif = 231
    Simple_Signal_Assignment_Statement = 232
    Conditional_Signal_Assignment_Statement = 233
    Selected_Waveform_Assignment_Statement = 234
    Signal_Force_Assignment_Statement = 235
    Signal_Release_Assignment_Statement = 236
    Null_Statement = 237
    Assertion_Statement = 238
    Report_Statement = 239
    Wait_Statement = 240
    Variable_Assignment_Statement = 241
    Conditional_Variable_Assignment_Statement = 242
    Return_Statement = 243
    For_Loop_Statement = 244
    While_Loop_Statement = 245
    Next_Statement = 246
    Exit_Statement = 247
    Case_Statement = 248
    Procedure_Call_Statement = 249
    Break_Statement = 250
    If_Statement = 251
    Suspend_State_Statement = 252
    Elsif = 253
    Character_Literal = 254
    Simple_Name = 255
    Selected_Name = 256
    Operator_Symbol = 257
    Reference_Name = 258
    External_Constant_Name = 259
    External_Signal_Name = 260
    External_Variable_Name = 261
    Selected_By_All_Name = 262
    Parenthesis_Name = 263
    Package_Pathname = 264
    Absolute_Pathname = 265
    Relative_Pathname = 266
    Pathname_Element = 267
    Base_Attribute = 268
    Subtype_Attribute = 269
    Element_Attribute = 270
    Across_Attribute = 271
    Through_Attribute = 272
    Nature_Reference_Attribute = 273
    Left_Type_Attribute = 274
    Right_Type_Attribute = 275
    High_Type_Attribute = 276
    Low_Type_Attribute = 277
    Ascending_Type_Attribute = 278
    Image_Attribute = 279
    Value_Attribute = 280
    Pos_Attribute = 281
    Val_Attribute = 282
    Succ_Attribute = 283
    Pred_Attribute = 284
    Leftof_Attribute = 285
    Rightof_Attribute = 286
    Signal_Slew_Attribute = 287
    Quantity_Slew_Attribute = 288
    Ramp_Attribute = 289
    Zoh_Attribute = 290
    Ltf_Attribute = 291
    Ztf_Attribute = 292
    Dot_Attribute = 293
    Integ_Attribute = 294
    Above_Attribute = 295
    Quantity_Delayed_Attribute = 296
    Delayed_Attribute = 297
    Stable_Attribute = 298
    Quiet_Attribute = 299
    Transaction_Attribute = 300
    Event_Attribute = 301
    Active_Attribute = 302
    Last_Event_Attribute = 303
    Last_Active_Attribute = 304
    Last_Value_Attribute = 305
    Driving_Attribute = 306
    Driving_Value_Attribute = 307
    Behavior_Attribute = 308
    Structure_Attribute = 309
    Simple_Name_Attribute = 310
    Instance_Name_Attribute = 311
    Path_Name_Attribute = 312
    Left_Array_Attribute = 313
    Right_Array_Attribute = 314
    High_Array_Attribute = 315
    Low_Array_Attribute = 316
    Length_Array_Attribute = 317
    Ascending_Array_Attribute = 318
    Range_Array_Attribute = 319
    Reverse_Range_Array_Attribute = 320
    Attribute_Name = 321


@export
class Iir_Kinds:
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

    Sequential_Statement_Ext = [
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
        Iir_Kind.Suspend_State_Statement,
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
    Ieee_1164_Vector_Is_X = 223
    Ieee_1164_Scalar_Is_X = 224
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
    Ieee_Numeric_Bit_Toint_Uns_Nat = 488
    Ieee_Numeric_Bit_Toint_Sgn_Int = 489
    Ieee_Numeric_Bit_Touns_Nat_Nat_Uns = 490
    Ieee_Numeric_Bit_Touns_Nat_Uns_Uns = 491
    Ieee_Numeric_Bit_Tosgn_Int_Nat_Sgn = 492
    Ieee_Numeric_Bit_Tosgn_Int_Sgn_Sgn = 493
    Ieee_Numeric_Std_Unsigned_Add_Slv_Slv = 494
    Ieee_Numeric_Std_Unsigned_Add_Slv_Nat = 495
    Ieee_Numeric_Std_Unsigned_Add_Nat_Slv = 496
    Ieee_Numeric_Std_Unsigned_Sub_Slv_Slv = 497
    Ieee_Numeric_Std_Unsigned_Sub_Slv_Nat = 498
    Ieee_Numeric_Std_Unsigned_Sub_Nat_Slv = 499
    Ieee_Numeric_Std_Unsigned_Find_Rightmost = 500
    Ieee_Numeric_Std_Unsigned_Find_Leftmost = 501
    Ieee_Numeric_Std_Unsigned_Shift_Left = 502
    Ieee_Numeric_Std_Unsigned_Shift_Right = 503
    Ieee_Numeric_Std_Unsigned_Rotate_Left = 504
    Ieee_Numeric_Std_Unsigned_Rotate_Right = 505
    Ieee_Numeric_Std_Unsigned_To_Integer_Slv_Nat = 506
    Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Nat = 507
    Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Slv = 508
    Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Nat = 509
    Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Suv = 510
    Ieee_Numeric_Std_Unsigned_Resize_Slv_Nat = 511
    Ieee_Numeric_Std_Unsigned_Resize_Slv_Slv = 512
    Ieee_Numeric_Std_Unsigned_Maximum_Slv_Slv = 513
    Ieee_Numeric_Std_Unsigned_Minimum_Slv_Slv = 514
    Ieee_Math_Real_Sign = 515
    Ieee_Math_Real_Ceil = 516
    Ieee_Math_Real_Floor = 517
    Ieee_Math_Real_Round = 518
    Ieee_Math_Real_Trunc = 519
    Ieee_Math_Real_Mod = 520
    Ieee_Math_Real_Realmax = 521
    Ieee_Math_Real_Realmin = 522
    Ieee_Math_Real_Sqrt = 523
    Ieee_Math_Real_Cbrt = 524
    Ieee_Math_Real_Pow_Int_Real = 525
    Ieee_Math_Real_Pow_Real_Real = 526
    Ieee_Math_Real_Exp = 527
    Ieee_Math_Real_Log = 528
    Ieee_Math_Real_Log2 = 529
    Ieee_Math_Real_Log10 = 530
    Ieee_Math_Real_Log_Real_Real = 531
    Ieee_Math_Real_Sin = 532
    Ieee_Math_Real_Cos = 533
    Ieee_Math_Real_Tan = 534
    Ieee_Math_Real_Arcsin = 535
    Ieee_Math_Real_Arccos = 536
    Ieee_Math_Real_Arctan = 537
    Ieee_Math_Real_Arctan_Real_Real = 538
    Ieee_Math_Real_Sinh = 539
    Ieee_Math_Real_Cosh = 540
    Ieee_Math_Real_Tanh = 541
    Ieee_Math_Real_Arcsinh = 542
    Ieee_Math_Real_Arccosh = 543
    Ieee_Math_Real_Arctanh = 544
    Ieee_Std_Logic_Unsigned_Add_Slv_Slv = 545
    Ieee_Std_Logic_Unsigned_Add_Slv_Int = 546
    Ieee_Std_Logic_Unsigned_Add_Int_Slv = 547
    Ieee_Std_Logic_Unsigned_Add_Slv_Log = 548
    Ieee_Std_Logic_Unsigned_Add_Log_Slv = 549
    Ieee_Std_Logic_Unsigned_Sub_Slv_Slv = 550
    Ieee_Std_Logic_Unsigned_Sub_Slv_Int = 551
    Ieee_Std_Logic_Unsigned_Sub_Int_Slv = 552
    Ieee_Std_Logic_Unsigned_Sub_Slv_Log = 553
    Ieee_Std_Logic_Unsigned_Sub_Log_Slv = 554
    Ieee_Std_Logic_Unsigned_Id_Slv = 555
    Ieee_Std_Logic_Unsigned_Mul_Slv_Slv = 556
    Ieee_Std_Logic_Unsigned_Lt_Slv_Slv = 557
    Ieee_Std_Logic_Unsigned_Lt_Slv_Int = 558
    Ieee_Std_Logic_Unsigned_Lt_Int_Slv = 559
    Ieee_Std_Logic_Unsigned_Le_Slv_Slv = 560
    Ieee_Std_Logic_Unsigned_Le_Slv_Int = 561
    Ieee_Std_Logic_Unsigned_Le_Int_Slv = 562
    Ieee_Std_Logic_Unsigned_Gt_Slv_Slv = 563
    Ieee_Std_Logic_Unsigned_Gt_Slv_Int = 564
    Ieee_Std_Logic_Unsigned_Gt_Int_Slv = 565
    Ieee_Std_Logic_Unsigned_Ge_Slv_Slv = 566
    Ieee_Std_Logic_Unsigned_Ge_Slv_Int = 567
    Ieee_Std_Logic_Unsigned_Ge_Int_Slv = 568
    Ieee_Std_Logic_Unsigned_Eq_Slv_Slv = 569
    Ieee_Std_Logic_Unsigned_Eq_Slv_Int = 570
    Ieee_Std_Logic_Unsigned_Eq_Int_Slv = 571
    Ieee_Std_Logic_Unsigned_Ne_Slv_Slv = 572
    Ieee_Std_Logic_Unsigned_Ne_Slv_Int = 573
    Ieee_Std_Logic_Unsigned_Ne_Int_Slv = 574
    Ieee_Std_Logic_Unsigned_Conv_Integer = 575
    Ieee_Std_Logic_Unsigned_Shl = 576
    Ieee_Std_Logic_Unsigned_Shr = 577
    Ieee_Std_Logic_Signed_Add_Slv_Slv = 578
    Ieee_Std_Logic_Signed_Add_Slv_Int = 579
    Ieee_Std_Logic_Signed_Add_Int_Slv = 580
    Ieee_Std_Logic_Signed_Add_Slv_Log = 581
    Ieee_Std_Logic_Signed_Add_Log_Slv = 582
    Ieee_Std_Logic_Signed_Sub_Slv_Slv = 583
    Ieee_Std_Logic_Signed_Sub_Slv_Int = 584
    Ieee_Std_Logic_Signed_Sub_Int_Slv = 585
    Ieee_Std_Logic_Signed_Sub_Slv_Log = 586
    Ieee_Std_Logic_Signed_Sub_Log_Slv = 587
    Ieee_Std_Logic_Signed_Id_Slv = 588
    Ieee_Std_Logic_Signed_Neg_Slv = 589
    Ieee_Std_Logic_Signed_Abs_Slv = 590
    Ieee_Std_Logic_Signed_Mul_Slv_Slv = 591
    Ieee_Std_Logic_Signed_Lt_Slv_Slv = 592
    Ieee_Std_Logic_Signed_Lt_Slv_Int = 593
    Ieee_Std_Logic_Signed_Lt_Int_Slv = 594
    Ieee_Std_Logic_Signed_Le_Slv_Slv = 595
    Ieee_Std_Logic_Signed_Le_Slv_Int = 596
    Ieee_Std_Logic_Signed_Le_Int_Slv = 597
    Ieee_Std_Logic_Signed_Gt_Slv_Slv = 598
    Ieee_Std_Logic_Signed_Gt_Slv_Int = 599
    Ieee_Std_Logic_Signed_Gt_Int_Slv = 600
    Ieee_Std_Logic_Signed_Ge_Slv_Slv = 601
    Ieee_Std_Logic_Signed_Ge_Slv_Int = 602
    Ieee_Std_Logic_Signed_Ge_Int_Slv = 603
    Ieee_Std_Logic_Signed_Eq_Slv_Slv = 604
    Ieee_Std_Logic_Signed_Eq_Slv_Int = 605
    Ieee_Std_Logic_Signed_Eq_Int_Slv = 606
    Ieee_Std_Logic_Signed_Ne_Slv_Slv = 607
    Ieee_Std_Logic_Signed_Ne_Slv_Int = 608
    Ieee_Std_Logic_Signed_Ne_Int_Slv = 609
    Ieee_Std_Logic_Signed_Conv_Integer = 610
    Ieee_Std_Logic_Signed_Shl = 611
    Ieee_Std_Logic_Signed_Shr = 612
    Ieee_Std_Logic_Arith_Conv_Unsigned_Int = 613
    Ieee_Std_Logic_Arith_Conv_Unsigned_Uns = 614
    Ieee_Std_Logic_Arith_Conv_Unsigned_Sgn = 615
    Ieee_Std_Logic_Arith_Conv_Unsigned_Log = 616
    Ieee_Std_Logic_Arith_Conv_Integer_Int = 617
    Ieee_Std_Logic_Arith_Conv_Integer_Uns = 618
    Ieee_Std_Logic_Arith_Conv_Integer_Sgn = 619
    Ieee_Std_Logic_Arith_Conv_Integer_Log = 620
    Ieee_Std_Logic_Arith_Conv_Vector_Int = 621
    Ieee_Std_Logic_Arith_Conv_Vector_Uns = 622
    Ieee_Std_Logic_Arith_Conv_Vector_Sgn = 623
    Ieee_Std_Logic_Arith_Conv_Vector_Log = 624
    Ieee_Std_Logic_Arith_Ext = 625
    Ieee_Std_Logic_Arith_Sxt = 626
    Ieee_Std_Logic_Arith_Id_Uns_Uns = 627
    Ieee_Std_Logic_Arith_Id_Sgn_Sgn = 628
    Ieee_Std_Logic_Arith_Neg_Sgn_Sgn = 629
    Ieee_Std_Logic_Arith_Abs_Sgn_Sgn = 630
    Ieee_Std_Logic_Arith_Shl_Uns = 631
    Ieee_Std_Logic_Arith_Shl_Sgn = 632
    Ieee_Std_Logic_Arith_Shr_Uns = 633
    Ieee_Std_Logic_Arith_Shr_Sgn = 634
    Ieee_Std_Logic_Arith_Id_Uns_Slv = 635
    Ieee_Std_Logic_Arith_Id_Sgn_Slv = 636
    Ieee_Std_Logic_Arith_Neg_Sgn_Slv = 637
    Ieee_Std_Logic_Arith_Abs_Sgn_Slv = 638
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns = 639
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn = 640
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn = 641
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn = 642
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv = 643
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv = 644
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv = 645
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv = 646
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns = 647
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn = 648
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn = 649
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn = 650
    Ieee_Std_Logic_Arith_Add_Uns_Int_Uns = 651
    Ieee_Std_Logic_Arith_Add_Int_Uns_Uns = 652
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn = 653
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn = 654
    Ieee_Std_Logic_Arith_Add_Uns_Log_Uns = 655
    Ieee_Std_Logic_Arith_Add_Log_Uns_Uns = 656
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn = 657
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn = 658
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv = 659
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv = 660
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv = 661
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv = 662
    Ieee_Std_Logic_Arith_Add_Uns_Int_Slv = 663
    Ieee_Std_Logic_Arith_Add_Int_Uns_Slv = 664
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv = 665
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv = 666
    Ieee_Std_Logic_Arith_Add_Uns_Log_Slv = 667
    Ieee_Std_Logic_Arith_Add_Log_Uns_Slv = 668
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv = 669
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv = 670
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns = 671
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn = 672
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn = 673
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn = 674
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns = 675
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns = 676
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn = 677
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn = 678
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns = 679
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns = 680
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn = 681
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn = 682
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv = 683
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv = 684
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv = 685
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv = 686
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv = 687
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv = 688
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv = 689
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv = 690
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv = 691
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv = 692
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv = 693
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv = 694
    Ieee_Std_Logic_Arith_Lt_Uns_Uns = 695
    Ieee_Std_Logic_Arith_Lt_Sgn_Sgn = 696
    Ieee_Std_Logic_Arith_Lt_Uns_Sgn = 697
    Ieee_Std_Logic_Arith_Lt_Sgn_Uns = 698
    Ieee_Std_Logic_Arith_Lt_Uns_Int = 699
    Ieee_Std_Logic_Arith_Lt_Int_Uns = 700
    Ieee_Std_Logic_Arith_Lt_Sgn_Int = 701
    Ieee_Std_Logic_Arith_Lt_Int_Sgn = 702
    Ieee_Std_Logic_Arith_Le_Uns_Uns = 703
    Ieee_Std_Logic_Arith_Le_Sgn_Sgn = 704
    Ieee_Std_Logic_Arith_Le_Uns_Sgn = 705
    Ieee_Std_Logic_Arith_Le_Sgn_Uns = 706
    Ieee_Std_Logic_Arith_Le_Uns_Int = 707
    Ieee_Std_Logic_Arith_Le_Int_Uns = 708
    Ieee_Std_Logic_Arith_Le_Sgn_Int = 709
    Ieee_Std_Logic_Arith_Le_Int_Sgn = 710
    Ieee_Std_Logic_Arith_Gt_Uns_Uns = 711
    Ieee_Std_Logic_Arith_Gt_Sgn_Sgn = 712
    Ieee_Std_Logic_Arith_Gt_Uns_Sgn = 713
    Ieee_Std_Logic_Arith_Gt_Sgn_Uns = 714
    Ieee_Std_Logic_Arith_Gt_Uns_Int = 715
    Ieee_Std_Logic_Arith_Gt_Int_Uns = 716
    Ieee_Std_Logic_Arith_Gt_Sgn_Int = 717
    Ieee_Std_Logic_Arith_Gt_Int_Sgn = 718
    Ieee_Std_Logic_Arith_Ge_Uns_Uns = 719
    Ieee_Std_Logic_Arith_Ge_Sgn_Sgn = 720
    Ieee_Std_Logic_Arith_Ge_Uns_Sgn = 721
    Ieee_Std_Logic_Arith_Ge_Sgn_Uns = 722
    Ieee_Std_Logic_Arith_Ge_Uns_Int = 723
    Ieee_Std_Logic_Arith_Ge_Int_Uns = 724
    Ieee_Std_Logic_Arith_Ge_Sgn_Int = 725
    Ieee_Std_Logic_Arith_Ge_Int_Sgn = 726
    Ieee_Std_Logic_Arith_Eq_Uns_Uns = 727
    Ieee_Std_Logic_Arith_Eq_Sgn_Sgn = 728
    Ieee_Std_Logic_Arith_Eq_Uns_Sgn = 729
    Ieee_Std_Logic_Arith_Eq_Sgn_Uns = 730
    Ieee_Std_Logic_Arith_Eq_Uns_Int = 731
    Ieee_Std_Logic_Arith_Eq_Int_Uns = 732
    Ieee_Std_Logic_Arith_Eq_Sgn_Int = 733
    Ieee_Std_Logic_Arith_Eq_Int_Sgn = 734
    Ieee_Std_Logic_Arith_Ne_Uns_Uns = 735
    Ieee_Std_Logic_Arith_Ne_Sgn_Sgn = 736
    Ieee_Std_Logic_Arith_Ne_Uns_Sgn = 737
    Ieee_Std_Logic_Arith_Ne_Sgn_Uns = 738
    Ieee_Std_Logic_Arith_Ne_Uns_Int = 739
    Ieee_Std_Logic_Arith_Ne_Int_Uns = 740
    Ieee_Std_Logic_Arith_Ne_Sgn_Int = 741
    Ieee_Std_Logic_Arith_Ne_Int_Sgn = 742
    Ieee_Std_Logic_Misc_And_Reduce_Slv = 743
    Ieee_Std_Logic_Misc_And_Reduce_Suv = 744
    Ieee_Std_Logic_Misc_Nand_Reduce_Slv = 745
    Ieee_Std_Logic_Misc_Nand_Reduce_Suv = 746
    Ieee_Std_Logic_Misc_Or_Reduce_Slv = 747
    Ieee_Std_Logic_Misc_Or_Reduce_Suv = 748
    Ieee_Std_Logic_Misc_Nor_Reduce_Slv = 749
    Ieee_Std_Logic_Misc_Nor_Reduce_Suv = 750
    Ieee_Std_Logic_Misc_Xor_Reduce_Slv = 751
    Ieee_Std_Logic_Misc_Xor_Reduce_Suv = 752
    Ieee_Std_Logic_Misc_Xnor_Reduce_Slv = 753
    Ieee_Std_Logic_Misc_Xnor_Reduce_Suv = 754


@export
@BindToLibGHDL("vhdl__nodes__get_kind")
def Get_Kind(node: Iir) -> IirKind:
    """Get node kind."""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__get_location")
def Get_Location(node: Iir) -> LocationType:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__get_first_design_unit")
def Get_First_Design_Unit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_first_design_unit")
def Set_First_Design_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_last_design_unit")
def Get_Last_Design_Unit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_last_design_unit")
def Set_Last_Design_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library_declaration")
def Get_Library_Declaration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_library_declaration")
def Set_Library_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_checksum")
def Get_File_Checksum(obj: Iir) -> FileChecksumId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_file_checksum")
def Set_File_Checksum(obj: Iir, value: FileChecksumId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_analysis_time_stamp")
def Get_Analysis_Time_Stamp(obj: Iir) -> TimeStampId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_analysis_time_stamp")
def Set_Analysis_Time_Stamp(obj: Iir, value: TimeStampId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_source")
def Get_Design_File_Source(obj: Iir) -> SourceFileEntry:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_source")
def Set_Design_File_Source(obj: Iir, value: SourceFileEntry) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library")
def Get_Library(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_library")
def Set_Library(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_dependence_list")
def Get_File_Dependence_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_file_dependence_list")
def Set_File_Dependence_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_filename")
def Get_Design_File_Filename(obj: Iir) -> NameId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_filename")
def Set_Design_File_Filename(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_directory")
def Get_Design_File_Directory(obj: Iir) -> NameId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_directory")
def Set_Design_File_Directory(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file")
def Get_Design_File(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_file")
def Set_Design_File(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_file_chain")
def Get_Design_File_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_file_chain")
def Set_Design_File_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library_directory")
def Get_Library_Directory(obj: Iir) -> NameId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_library_directory")
def Set_Library_Directory(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_date")
def Get_Date(obj: Iir) -> DateType:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_date")
def Set_Date(obj: Iir, value: DateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_context_items")
def Get_Context_Items(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_context_items")
def Set_Context_Items(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_dependence_list")
def Get_Dependence_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_dependence_list")
def Set_Dependence_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_analysis_checks_list")
def Get_Analysis_Checks_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_analysis_checks_list")
def Set_Analysis_Checks_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_date_state")
def Get_Date_State(obj: Iir) -> DateStateType:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_date_state")
def Set_Date_State(obj: Iir, value: DateStateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guarded_target_state")
def Get_Guarded_Target_State(obj: Iir) -> TriStateType:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_guarded_target_state")
def Set_Guarded_Target_State(obj: Iir, value: TriStateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_library_unit")
def Get_Library_Unit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_library_unit")
def Set_Library_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_hash_chain")
def Get_Hash_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_hash_chain")
def Set_Hash_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_pos")
def Get_Design_Unit_Source_Pos(obj: Iir) -> SourcePtr:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_pos")
def Set_Design_Unit_Source_Pos(obj: Iir, value: SourcePtr) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_line")
def Get_Design_Unit_Source_Line(obj: Iir) -> Int32:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_line")
def Set_Design_Unit_Source_Line(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit_source_col")
def Get_Design_Unit_Source_Col(obj: Iir) -> Int32:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit_source_col")
def Set_Design_Unit_Source_Col(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_value")
def Get_Value(obj: Iir) -> Int64:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_value")
def Set_Value(obj: Iir, value: Int64) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_enum_pos")
def Get_Enum_Pos(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_enum_pos")
def Set_Enum_Pos(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_physical_literal")
def Get_Physical_Literal(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_physical_literal")
def Set_Physical_Literal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_fp_value")
def Get_Fp_Value(obj: Iir) -> Fp64:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_fp_value")
def Set_Fp_Value(obj: Iir, value: Fp64) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_aggregate_list")
def Get_Simple_Aggregate_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_simple_aggregate_list")
def Set_Simple_Aggregate_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_string8_id")
def Get_String8_Id(obj: Iir) -> String8Id:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_string8_id")
def Set_String8_Id(obj: Iir, value: String8Id) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_string_length")
def Get_String_Length(obj: Iir) -> Int32:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_string_length")
def Set_String_Length(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_bit_string_base")
def Get_Bit_String_Base(obj: Iir) -> NumberBaseType:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_bit_string_base")
def Set_Bit_String_Base(obj: Iir, value: NumberBaseType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_signed")
def Get_Has_Signed(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_signed")
def Set_Has_Signed(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_sign")
def Get_Has_Sign(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_sign")
def Set_Has_Sign(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_length")
def Get_Has_Length(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_length")
def Set_Has_Length(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_literal_length")
def Get_Literal_Length(obj: Iir) -> Int32:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_literal_length")
def Set_Literal_Length(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_literal_origin")
def Get_Literal_Origin(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_literal_origin")
def Set_Literal_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_range_origin")
def Get_Range_Origin(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_range_origin")
def Set_Range_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_literal_subtype")
def Get_Literal_Subtype(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_literal_subtype")
def Set_Literal_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_allocator_subtype")
def Get_Allocator_Subtype(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_allocator_subtype")
def Set_Allocator_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_class")
def Get_Entity_Class(obj: Iir) -> Tok:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_entity_class")
def Set_Entity_Class(obj: Iir, value: Tok) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_name_list")
def Get_Entity_Name_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_entity_name_list")
def Set_Entity_Name_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_designator")
def Get_Attribute_Designator(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_designator")
def Set_Attribute_Designator(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_specification_chain")
def Get_Attribute_Specification_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_specification_chain")
def Set_Attribute_Specification_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_specification")
def Get_Attribute_Specification(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_specification")
def Set_Attribute_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_static_attribute_flag")
def Get_Static_Attribute_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_static_attribute_flag")
def Set_Static_Attribute_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_list")
def Get_Signal_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_signal_list")
def Set_Signal_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_quantity_list")
def Get_Quantity_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_quantity_list")
def Set_Quantity_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_designated_entity")
def Get_Designated_Entity(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_designated_entity")
def Set_Designated_Entity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_formal")
def Get_Formal(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_formal")
def Set_Formal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual")
def Get_Actual(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_actual")
def Set_Actual(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual_conversion")
def Get_Actual_Conversion(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_actual_conversion")
def Set_Actual_Conversion(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_formal_conversion")
def Get_Formal_Conversion(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_formal_conversion")
def Set_Formal_Conversion(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_whole_association_flag")
def Get_Whole_Association_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_whole_association_flag")
def Set_Whole_Association_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_collapse_signal_flag")
def Get_Collapse_Signal_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_collapse_signal_flag")
def Set_Collapse_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_artificial_flag")
def Get_Artificial_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_artificial_flag")
def Set_Artificial_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_open_flag")
def Get_Open_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_open_flag")
def Set_Open_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_after_drivers_flag")
def Get_After_Drivers_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_after_drivers_flag")
def Set_After_Drivers_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_we_value")
def Get_We_Value(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_we_value")
def Set_We_Value(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_time")
def Get_Time(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_time")
def Set_Time(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_expr")
def Get_Associated_Expr(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_associated_expr")
def Set_Associated_Expr(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_block")
def Get_Associated_Block(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_associated_block")
def Set_Associated_Block(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_chain")
def Get_Associated_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_associated_chain")
def Set_Associated_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_name")
def Get_Choice_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_choice_name")
def Set_Choice_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_expression")
def Get_Choice_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_choice_expression")
def Set_Choice_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_range")
def Get_Choice_Range(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_choice_range")
def Set_Choice_Range(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_same_alternative_flag")
def Get_Same_Alternative_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_same_alternative_flag")
def Set_Same_Alternative_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_type_flag")
def Get_Element_Type_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_element_type_flag")
def Set_Element_Type_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_architecture")
def Get_Architecture(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_architecture")
def Set_Architecture(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_specification")
def Get_Block_Specification(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_block_specification")
def Set_Block_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_prev_block_configuration")
def Get_Prev_Block_Configuration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_prev_block_configuration")
def Set_Prev_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_item_chain")
def Get_Configuration_Item_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_item_chain")
def Set_Configuration_Item_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_value_chain")
def Get_Attribute_Value_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_value_chain")
def Set_Attribute_Value_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_spec_chain")
def Get_Spec_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_spec_chain")
def Set_Spec_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_value_chain")
def Get_Value_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_value_chain")
def Set_Value_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_value_spec_chain")
def Get_Attribute_Value_Spec_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_value_spec_chain")
def Set_Attribute_Value_Spec_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_name")
def Get_Entity_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_entity_name")
def Set_Entity_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package")
def Get_Package(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_package")
def Set_Package(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package_body")
def Get_Package_Body(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_package_body")
def Set_Package_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instance_package_body")
def Get_Instance_Package_Body(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_instance_package_body")
def Set_Instance_Package_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_need_body")
def Get_Need_Body(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_need_body")
def Set_Need_Body(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_macro_expanded_flag")
def Get_Macro_Expanded_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_macro_expanded_flag")
def Set_Macro_Expanded_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_need_instance_bodies")
def Get_Need_Instance_Bodies(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_need_instance_bodies")
def Set_Need_Instance_Bodies(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_hierarchical_name")
def Get_Hierarchical_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_hierarchical_name")
def Set_Hierarchical_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_vunit_item_chain")
def Get_Vunit_Item_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_vunit_item_chain")
def Set_Vunit_Item_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_bound_vunit_chain")
def Get_Bound_Vunit_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_bound_vunit_chain")
def Set_Bound_Vunit_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_verification_block_configuration")
def Get_Verification_Block_Configuration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_verification_block_configuration")
def Set_Verification_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_configuration")
def Get_Block_Configuration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_block_configuration")
def Set_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_concurrent_statement_chain")
def Get_Concurrent_Statement_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_concurrent_statement_chain")
def Set_Concurrent_Statement_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_chain")
def Get_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_chain")
def Set_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_port_chain")
def Get_Port_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_port_chain")
def Set_Port_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generic_chain")
def Get_Generic_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_generic_chain")
def Set_Generic_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type")
def Get_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_type")
def Set_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_indication")
def Get_Subtype_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_indication")
def Set_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_discrete_range")
def Get_Discrete_Range(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_discrete_range")
def Set_Discrete_Range(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_definition")
def Get_Type_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_type_definition")
def Set_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_definition")
def Get_Subtype_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_definition")
def Set_Subtype_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_incomplete_type_declaration")
def Get_Incomplete_Type_Declaration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_incomplete_type_declaration")
def Set_Incomplete_Type_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_interface_type_subprograms")
def Get_Interface_Type_Subprograms(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_interface_type_subprograms")
def Set_Interface_Type_Subprograms(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature_definition")
def Get_Nature_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_nature_definition")
def Set_Nature_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature")
def Get_Nature(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_nature")
def Set_Nature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subnature_indication")
def Get_Subnature_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subnature_indication")
def Set_Subnature_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_mode")
def Get_Mode(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_mode")
def Set_Mode(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guarded_signal_flag")
def Get_Guarded_Signal_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_guarded_signal_flag")
def Set_Guarded_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_kind")
def Get_Signal_Kind(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_signal_kind")
def Set_Signal_Kind(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_base_name")
def Get_Base_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_base_name")
def Set_Base_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_interface_declaration_chain")
def Get_Interface_Declaration_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_interface_declaration_chain")
def Set_Interface_Declaration_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_specification")
def Get_Subprogram_Specification(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_specification")
def Set_Subprogram_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_sequential_statement_chain")
def Get_Sequential_Statement_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_sequential_statement_chain")
def Set_Sequential_Statement_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_statement_chain")
def Get_Simultaneous_Statement_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_statement_chain")
def Set_Simultaneous_Statement_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_body")
def Get_Subprogram_Body(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_body")
def Set_Subprogram_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_overload_number")
def Get_Overload_Number(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_overload_number")
def Set_Overload_Number(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_depth")
def Get_Subprogram_Depth(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_depth")
def Set_Subprogram_Depth(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_hash")
def Get_Subprogram_Hash(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_hash")
def Set_Subprogram_Hash(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_impure_depth")
def Get_Impure_Depth(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_impure_depth")
def Set_Impure_Depth(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_return_type")
def Get_Return_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_return_type")
def Set_Return_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_implicit_definition")
def Get_Implicit_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_implicit_definition")
def Set_Implicit_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_subprogram_name")
def Get_Uninstantiated_Subprogram_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_subprogram_name")
def Set_Uninstantiated_Subprogram_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_value")
def Get_Default_Value(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_default_value")
def Set_Default_Value(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_deferred_declaration")
def Get_Deferred_Declaration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_deferred_declaration")
def Set_Deferred_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_deferred_declaration_flag")
def Get_Deferred_Declaration_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_deferred_declaration_flag")
def Set_Deferred_Declaration_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_shared_flag")
def Get_Shared_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_shared_flag")
def Set_Shared_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_design_unit")
def Get_Design_Unit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_design_unit")
def Set_Design_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_statement")
def Get_Block_Statement(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_block_statement")
def Set_Block_Statement(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_driver")
def Get_Signal_Driver(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_signal_driver")
def Set_Signal_Driver(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_declaration_chain")
def Get_Declaration_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_declaration_chain")
def Set_Declaration_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_logical_name")
def Get_File_Logical_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_file_logical_name")
def Set_File_Logical_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_open_kind")
def Get_File_Open_Kind(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_file_open_kind")
def Set_File_Open_Kind(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_position")
def Get_Element_Position(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_element_position")
def Set_Element_Position(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_use_clause_chain")
def Get_Use_Clause_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_use_clause_chain")
def Set_Use_Clause_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_context_reference_chain")
def Get_Context_Reference_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_context_reference_chain")
def Set_Context_Reference_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_inherit_spec_chain")
def Get_Inherit_Spec_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_inherit_spec_chain")
def Set_Inherit_Spec_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_selected_name")
def Get_Selected_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_selected_name")
def Set_Selected_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_declarator")
def Get_Type_Declarator(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_type_declarator")
def Set_Type_Declarator(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_complete_type_definition")
def Get_Complete_Type_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_complete_type_definition")
def Set_Complete_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_incomplete_type_ref_chain")
def Get_Incomplete_Type_Ref_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_incomplete_type_ref_chain")
def Set_Incomplete_Type_Ref_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_associated_type")
def Get_Associated_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_associated_type")
def Set_Associated_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_enumeration_literal_list")
def Get_Enumeration_Literal_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_enumeration_literal_list")
def Set_Enumeration_Literal_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_class_entry_chain")
def Get_Entity_Class_Entry_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_entity_class_entry_chain")
def Set_Entity_Class_Entry_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_group_constituent_list")
def Get_Group_Constituent_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_group_constituent_list")
def Set_Group_Constituent_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_unit_chain")
def Get_Unit_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_unit_chain")
def Set_Unit_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_primary_unit")
def Get_Primary_Unit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_primary_unit")
def Set_Primary_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_identifier")
def Get_Identifier(obj: Iir) -> NameId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_identifier")
def Set_Identifier(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_label")
def Get_Label(obj: Iir) -> NameId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_label")
def Set_Label(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_return_identifier")
def Get_Return_Identifier(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_return_identifier")
def Set_Return_Identifier(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_visible_flag")
def Get_Visible_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_visible_flag")
def Set_Visible_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_range_constraint")
def Get_Range_Constraint(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_range_constraint")
def Set_Range_Constraint(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_direction")
def Get_Direction(obj: Iir) -> DirectionType:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_direction")
def Set_Direction(obj: Iir, value: DirectionType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_left_limit")
def Get_Left_Limit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_left_limit")
def Set_Left_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_right_limit")
def Get_Right_Limit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_right_limit")
def Set_Right_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_left_limit_expr")
def Get_Left_Limit_Expr(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_left_limit_expr")
def Set_Left_Limit_Expr(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_right_limit_expr")
def Get_Right_Limit_Expr(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_right_limit_expr")
def Set_Right_Limit_Expr(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parent_type")
def Get_Parent_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parent_type")
def Set_Parent_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_nature")
def Get_Simple_Nature(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_simple_nature")
def Set_Simple_Nature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_base_nature")
def Get_Base_Nature(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_base_nature")
def Set_Base_Nature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_resolution_indication")
def Get_Resolution_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_resolution_indication")
def Set_Resolution_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_record_element_resolution_chain")
def Get_Record_Element_Resolution_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_record_element_resolution_chain")
def Set_Record_Element_Resolution_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_tolerance")
def Get_Tolerance(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_tolerance")
def Set_Tolerance(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_plus_terminal_name")
def Get_Plus_Terminal_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_plus_terminal_name")
def Set_Plus_Terminal_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_minus_terminal_name")
def Get_Minus_Terminal_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_minus_terminal_name")
def Set_Minus_Terminal_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_plus_terminal")
def Get_Plus_Terminal(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_plus_terminal")
def Set_Plus_Terminal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_minus_terminal")
def Get_Minus_Terminal(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_minus_terminal")
def Set_Minus_Terminal(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_magnitude_expression")
def Get_Magnitude_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_magnitude_expression")
def Set_Magnitude_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_phase_expression")
def Get_Phase_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_phase_expression")
def Set_Phase_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_power_expression")
def Get_Power_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_power_expression")
def Set_Power_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_left")
def Get_Simultaneous_Left(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_left")
def Set_Simultaneous_Left(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simultaneous_right")
def Get_Simultaneous_Right(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_simultaneous_right")
def Set_Simultaneous_Right(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_text_file_flag")
def Get_Text_File_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_text_file_flag")
def Set_Text_File_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_only_characters_flag")
def Get_Only_Characters_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_only_characters_flag")
def Set_Only_Characters_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_character_type")
def Get_Is_Character_Type(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_is_character_type")
def Set_Is_Character_Type(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature_staticness")
def Get_Nature_Staticness(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_nature_staticness")
def Set_Nature_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_staticness")
def Get_Type_Staticness(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_type_staticness")
def Set_Type_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_constraint_state")
def Get_Constraint_State(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_constraint_state")
def Set_Constraint_State(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype_list")
def Get_Index_Subtype_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype_list")
def Set_Index_Subtype_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype_definition_list")
def Get_Index_Subtype_Definition_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype_definition_list")
def Set_Index_Subtype_Definition_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subtype_indication")
def Get_Element_Subtype_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_element_subtype_indication")
def Set_Element_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subtype")
def Get_Element_Subtype(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_element_subtype")
def Set_Element_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subnature_indication")
def Get_Element_Subnature_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_element_subnature_indication")
def Set_Element_Subnature_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_element_subnature")
def Get_Element_Subnature(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_element_subnature")
def Set_Element_Subnature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_constraint_list")
def Get_Index_Constraint_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_index_constraint_list")
def Set_Index_Constraint_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_array_element_constraint")
def Get_Array_Element_Constraint(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_array_element_constraint")
def Set_Array_Element_Constraint(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_array_constraint_flag")
def Get_Has_Array_Constraint_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_array_constraint_flag")
def Set_Has_Array_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_element_constraint_flag")
def Get_Has_Element_Constraint_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_element_constraint_flag")
def Set_Has_Element_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_elements_declaration_list")
def Get_Elements_Declaration_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_elements_declaration_list")
def Set_Elements_Declaration_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_owned_elements_chain")
def Get_Owned_Elements_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_owned_elements_chain")
def Set_Owned_Elements_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_designated_type")
def Get_Designated_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_designated_type")
def Set_Designated_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_designated_subtype_indication")
def Get_Designated_Subtype_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_designated_subtype_indication")
def Set_Designated_Subtype_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_list")
def Get_Index_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_index_list")
def Set_Index_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_reference")
def Get_Reference(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_reference")
def Set_Reference(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_nature_declarator")
def Get_Nature_Declarator(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_nature_declarator")
def Set_Nature_Declarator(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_across_type_mark")
def Get_Across_Type_Mark(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_across_type_mark")
def Set_Across_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_through_type_mark")
def Get_Through_Type_Mark(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_through_type_mark")
def Set_Through_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_across_type_definition")
def Get_Across_Type_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_across_type_definition")
def Set_Across_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_through_type_definition")
def Get_Through_Type_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_through_type_definition")
def Set_Through_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_across_type")
def Get_Across_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_across_type")
def Set_Across_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_through_type")
def Get_Through_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_through_type")
def Set_Through_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_target")
def Get_Target(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_target")
def Set_Target(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_waveform_chain")
def Get_Waveform_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_waveform_chain")
def Set_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard")
def Get_Guard(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_guard")
def Set_Guard(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_delay_mechanism")
def Get_Delay_Mechanism(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_delay_mechanism")
def Set_Delay_Mechanism(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_reject_time_expression")
def Get_Reject_Time_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_reject_time_expression")
def Set_Reject_Time_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_force_mode")
def Get_Force_Mode(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_force_mode")
def Set_Force_Mode(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_force_mode")
def Get_Has_Force_Mode(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_force_mode")
def Set_Has_Force_Mode(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_sensitivity_list")
def Get_Sensitivity_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_sensitivity_list")
def Set_Sensitivity_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_process_origin")
def Get_Process_Origin(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_process_origin")
def Set_Process_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package_origin")
def Get_Package_Origin(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_package_origin")
def Set_Package_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_condition_clause")
def Get_Condition_Clause(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_condition_clause")
def Set_Condition_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_break_element")
def Get_Break_Element(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_break_element")
def Set_Break_Element(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_selector_quantity")
def Get_Selector_Quantity(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_selector_quantity")
def Set_Selector_Quantity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_break_quantity")
def Get_Break_Quantity(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_break_quantity")
def Set_Break_Quantity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_timeout_clause")
def Get_Timeout_Clause(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_timeout_clause")
def Set_Timeout_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_postponed_flag")
def Get_Postponed_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_postponed_flag")
def Set_Postponed_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_callees_list")
def Get_Callees_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_callees_list")
def Set_Callees_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_passive_flag")
def Get_Passive_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_passive_flag")
def Set_Passive_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_resolution_function_flag")
def Get_Resolution_Function_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_resolution_function_flag")
def Set_Resolution_Function_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_wait_state")
def Get_Wait_State(obj: Iir) -> TriStateType:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_wait_state")
def Set_Wait_State(obj: Iir, value: TriStateType) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_all_sensitized_state")
def Get_All_Sensitized_State(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_all_sensitized_state")
def Set_All_Sensitized_State(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_seen_flag")
def Get_Seen_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_seen_flag")
def Set_Seen_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_pure_flag")
def Get_Pure_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_pure_flag")
def Set_Pure_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_foreign_flag")
def Get_Foreign_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_foreign_flag")
def Set_Foreign_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_resolved_flag")
def Get_Resolved_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_resolved_flag")
def Set_Resolved_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_type_flag")
def Get_Signal_Type_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_signal_type_flag")
def Set_Signal_Type_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_signal_flag")
def Get_Has_Signal_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_signal_flag")
def Set_Has_Signal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_purity_state")
def Get_Purity_State(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_purity_state")
def Set_Purity_State(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_elab_flag")
def Get_Elab_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_elab_flag")
def Set_Elab_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_vendor_library_flag")
def Get_Vendor_Library_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_vendor_library_flag")
def Set_Vendor_Library_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_mark_flag")
def Get_Configuration_Mark_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_mark_flag")
def Set_Configuration_Mark_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_done_flag")
def Get_Configuration_Done_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_done_flag")
def Set_Configuration_Done_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_constraint_flag")
def Get_Index_Constraint_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_index_constraint_flag")
def Set_Index_Constraint_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_hide_implicit_flag")
def Get_Hide_Implicit_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_hide_implicit_flag")
def Set_Hide_Implicit_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_assertion_condition")
def Get_Assertion_Condition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_assertion_condition")
def Set_Assertion_Condition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_report_expression")
def Get_Report_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_report_expression")
def Set_Report_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_severity_expression")
def Get_Severity_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_severity_expression")
def Set_Severity_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instantiated_unit")
def Get_Instantiated_Unit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_instantiated_unit")
def Set_Instantiated_Unit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generic_map_aspect_chain")
def Get_Generic_Map_Aspect_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_generic_map_aspect_chain")
def Set_Generic_Map_Aspect_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_port_map_aspect_chain")
def Get_Port_Map_Aspect_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_port_map_aspect_chain")
def Set_Port_Map_Aspect_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_name")
def Get_Configuration_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_name")
def Set_Configuration_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_component_configuration")
def Get_Component_Configuration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_component_configuration")
def Set_Component_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_configuration_specification")
def Get_Configuration_Specification(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_configuration_specification")
def Set_Configuration_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_binding_indication")
def Get_Default_Binding_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_default_binding_indication")
def Set_Default_Binding_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_configuration_declaration")
def Get_Default_Configuration_Declaration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_default_configuration_declaration")
def Set_Default_Configuration_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_expression")
def Get_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_expression")
def Set_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_conditional_expression_chain")
def Get_Conditional_Expression_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_conditional_expression_chain")
def Set_Conditional_Expression_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_allocator_designated_type")
def Get_Allocator_Designated_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_allocator_designated_type")
def Set_Allocator_Designated_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_selected_waveform_chain")
def Get_Selected_Waveform_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_selected_waveform_chain")
def Set_Selected_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_conditional_waveform_chain")
def Get_Conditional_Waveform_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_conditional_waveform_chain")
def Set_Conditional_Waveform_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard_expression")
def Get_Guard_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_guard_expression")
def Set_Guard_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard_decl")
def Get_Guard_Decl(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_guard_decl")
def Set_Guard_Decl(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_guard_sensitivity_list")
def Get_Guard_Sensitivity_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_guard_sensitivity_list")
def Set_Guard_Sensitivity_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_attribute_chain")
def Get_Signal_Attribute_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_signal_attribute_chain")
def Set_Signal_Attribute_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_block_configuration")
def Get_Block_Block_Configuration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_block_block_configuration")
def Set_Block_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_package_header")
def Get_Package_Header(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_package_header")
def Set_Package_Header(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_block_header")
def Get_Block_Header(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_block_header")
def Set_Block_Header(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_package_name")
def Get_Uninstantiated_Package_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_package_name")
def Set_Uninstantiated_Package_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_uninstantiated_package_decl")
def Get_Uninstantiated_Package_Decl(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_uninstantiated_package_decl")
def Set_Uninstantiated_Package_Decl(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instance_source_file")
def Get_Instance_Source_File(obj: Iir) -> SourceFileEntry:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_instance_source_file")
def Set_Instance_Source_File(obj: Iir, value: SourceFileEntry) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generate_block_configuration")
def Get_Generate_Block_Configuration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_generate_block_configuration")
def Set_Generate_Block_Configuration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generate_statement_body")
def Get_Generate_Statement_Body(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_generate_statement_body")
def Set_Generate_Statement_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_alternative_label")
def Get_Alternative_Label(obj: Iir) -> NameId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_alternative_label")
def Set_Alternative_Label(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_generate_else_clause")
def Get_Generate_Else_Clause(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_generate_else_clause")
def Set_Generate_Else_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_condition")
def Get_Condition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_condition")
def Set_Condition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_else_clause")
def Get_Else_Clause(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_else_clause")
def Set_Else_Clause(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_specification")
def Get_Parameter_Specification(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_specification")
def Set_Parameter_Specification(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parent")
def Get_Parent(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parent")
def Set_Parent(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_loop_label")
def Get_Loop_Label(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_loop_label")
def Set_Loop_Label(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_exit_flag")
def Get_Exit_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_exit_flag")
def Set_Exit_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_next_flag")
def Get_Next_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_next_flag")
def Set_Next_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_component_name")
def Get_Component_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_component_name")
def Set_Component_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_instantiation_list")
def Get_Instantiation_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_instantiation_list")
def Set_Instantiation_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_entity_aspect")
def Get_Entity_Aspect(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_entity_aspect")
def Set_Entity_Aspect(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_entity_aspect")
def Get_Default_Entity_Aspect(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_default_entity_aspect")
def Set_Default_Entity_Aspect(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_binding_indication")
def Get_Binding_Indication(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_binding_indication")
def Set_Binding_Indication(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_named_entity")
def Get_Named_Entity(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_named_entity")
def Set_Named_Entity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_referenced_name")
def Get_Referenced_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_referenced_name")
def Set_Referenced_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_expr_staticness")
def Get_Expr_Staticness(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_expr_staticness")
def Set_Expr_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_scalar_size")
def Get_Scalar_Size(obj: Iir) -> ScalarSize:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_scalar_size")
def Set_Scalar_Size(obj: Iir, value: ScalarSize) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_error_origin")
def Get_Error_Origin(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_error_origin")
def Set_Error_Origin(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_operand")
def Get_Operand(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_operand")
def Set_Operand(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_left")
def Get_Left(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_left")
def Set_Left(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_right")
def Get_Right(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_right")
def Set_Right(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_unit_name")
def Get_Unit_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_unit_name")
def Set_Unit_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_name")
def Get_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_name")
def Set_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_group_template_name")
def Get_Group_Template_Name(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_group_template_name")
def Set_Group_Template_Name(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_name_staticness")
def Get_Name_Staticness(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_name_staticness")
def Set_Name_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_prefix")
def Get_Prefix(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_prefix")
def Set_Prefix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signature_prefix")
def Get_Signature_Prefix(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_signature_prefix")
def Set_Signature_Prefix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_external_pathname")
def Get_External_Pathname(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_external_pathname")
def Set_External_Pathname(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_pathname_suffix")
def Get_Pathname_Suffix(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_pathname_suffix")
def Set_Pathname_Suffix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_pathname_expression")
def Get_Pathname_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_pathname_expression")
def Set_Pathname_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_in_formal_flag")
def Get_In_Formal_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_in_formal_flag")
def Set_In_Formal_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_slice_subtype")
def Get_Slice_Subtype(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_slice_subtype")
def Set_Slice_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_suffix")
def Get_Suffix(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_suffix")
def Set_Suffix(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_index_subtype")
def Get_Index_Subtype(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_index_subtype")
def Set_Index_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter")
def Get_Parameter(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parameter")
def Set_Parameter(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_2")
def Get_Parameter_2(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_2")
def Set_Parameter_2(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_3")
def Get_Parameter_3(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_3")
def Set_Parameter_3(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_4")
def Get_Parameter_4(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_4")
def Set_Parameter_4(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attr_chain")
def Get_Attr_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_attr_chain")
def Set_Attr_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_signal_attribute_declaration")
def Get_Signal_Attribute_Declaration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_signal_attribute_declaration")
def Set_Signal_Attribute_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual_type")
def Get_Actual_Type(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_actual_type")
def Set_Actual_Type(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_actual_type_definition")
def Get_Actual_Type_Definition(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_actual_type_definition")
def Set_Actual_Type_Definition(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_association_chain")
def Get_Association_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_association_chain")
def Set_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_individual_association_chain")
def Get_Individual_Association_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_individual_association_chain")
def Set_Individual_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subprogram_association_chain")
def Get_Subprogram_Association_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subprogram_association_chain")
def Set_Subprogram_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggregate_info")
def Get_Aggregate_Info(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggregate_info")
def Set_Aggregate_Info(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_sub_aggregate_info")
def Get_Sub_Aggregate_Info(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_sub_aggregate_info")
def Set_Sub_Aggregate_Info(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_dynamic_flag")
def Get_Aggr_Dynamic_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_dynamic_flag")
def Set_Aggr_Dynamic_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_min_length")
def Get_Aggr_Min_Length(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_min_length")
def Set_Aggr_Min_Length(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_low_limit")
def Get_Aggr_Low_Limit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_low_limit")
def Set_Aggr_Low_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_high_limit")
def Get_Aggr_High_Limit(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_high_limit")
def Set_Aggr_High_Limit(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_others_flag")
def Get_Aggr_Others_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_others_flag")
def Set_Aggr_Others_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggr_named_flag")
def Get_Aggr_Named_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggr_named_flag")
def Set_Aggr_Named_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_aggregate_expand_flag")
def Get_Aggregate_Expand_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_aggregate_expand_flag")
def Set_Aggregate_Expand_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_association_choices_chain")
def Get_Association_Choices_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_association_choices_chain")
def Set_Association_Choices_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_case_statement_alternative_chain")
def Get_Case_Statement_Alternative_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_case_statement_alternative_chain")
def Set_Case_Statement_Alternative_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_matching_flag")
def Get_Matching_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_matching_flag")
def Set_Matching_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_choice_staticness")
def Get_Choice_Staticness(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_choice_staticness")
def Set_Choice_Staticness(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_procedure_call")
def Get_Procedure_Call(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_procedure_call")
def Set_Procedure_Call(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_implementation")
def Get_Implementation(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_implementation")
def Set_Implementation(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_parameter_association_chain")
def Get_Parameter_Association_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_parameter_association_chain")
def Set_Parameter_Association_Chain(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_method_object")
def Get_Method_Object(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_method_object")
def Set_Method_Object(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subtype_type_mark")
def Get_Subtype_Type_Mark(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subtype_type_mark")
def Set_Subtype_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_subnature_nature_mark")
def Get_Subnature_Nature_Mark(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_subnature_nature_mark")
def Set_Subnature_Nature_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_conversion_subtype")
def Get_Type_Conversion_Subtype(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_type_conversion_subtype")
def Set_Type_Conversion_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_mark")
def Get_Type_Mark(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_type_mark")
def Set_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_file_type_mark")
def Get_File_Type_Mark(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_file_type_mark")
def Set_File_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_return_type_mark")
def Get_Return_Type_Mark(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_return_type_mark")
def Set_Return_Type_Mark(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_disconnect_flag")
def Get_Has_Disconnect_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_disconnect_flag")
def Set_Has_Disconnect_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_active_flag")
def Get_Has_Active_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_active_flag")
def Set_Has_Active_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_within_flag")
def Get_Is_Within_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_is_within_flag")
def Set_Is_Within_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_type_marks_list")
def Get_Type_Marks_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_type_marks_list")
def Set_Type_Marks_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_implicit_alias_flag")
def Get_Implicit_Alias_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_implicit_alias_flag")
def Set_Implicit_Alias_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_alias_signature")
def Get_Alias_Signature(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_alias_signature")
def Set_Alias_Signature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_attribute_signature")
def Get_Attribute_Signature(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_attribute_signature")
def Set_Attribute_Signature(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_overload_list")
def Get_Overload_List(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_overload_list")
def Set_Overload_List(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_name_identifier")
def Get_Simple_Name_Identifier(obj: Iir) -> NameId:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_simple_name_identifier")
def Set_Simple_Name_Identifier(obj: Iir, value: NameId) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_simple_name_subtype")
def Get_Simple_Name_Subtype(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_simple_name_subtype")
def Set_Simple_Name_Subtype(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_protected_type_body")
def Get_Protected_Type_Body(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_protected_type_body")
def Set_Protected_Type_Body(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_protected_type_declaration")
def Get_Protected_Type_Declaration(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_protected_type_declaration")
def Set_Protected_Type_Declaration(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_use_flag")
def Get_Use_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_use_flag")
def Set_Use_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_reserved_id")
def Get_End_Has_Reserved_Id(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_reserved_id")
def Set_End_Has_Reserved_Id(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_identifier")
def Get_End_Has_Identifier(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_identifier")
def Set_End_Has_Identifier(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_end_has_postponed")
def Get_End_Has_Postponed(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_end_has_postponed")
def Set_End_Has_Postponed(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_label")
def Get_Has_Label(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_label")
def Set_Has_Label(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_begin")
def Get_Has_Begin(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_begin")
def Set_Has_Begin(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_end")
def Get_Has_End(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_end")
def Set_Has_End(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_is")
def Get_Has_Is(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_is")
def Set_Has_Is(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_pure")
def Get_Has_Pure(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_pure")
def Set_Has_Pure(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_body")
def Get_Has_Body(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_body")
def Set_Has_Body(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_parameter")
def Get_Has_Parameter(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_parameter")
def Set_Has_Parameter(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_component")
def Get_Has_Component(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_component")
def Set_Has_Component(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_identifier_list")
def Get_Has_Identifier_List(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_identifier_list")
def Set_Has_Identifier_List(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_mode")
def Get_Has_Mode(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_mode")
def Set_Has_Mode(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_class")
def Get_Has_Class(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_class")
def Set_Has_Class(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_has_delay_mechanism")
def Get_Has_Delay_Mechanism(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_has_delay_mechanism")
def Set_Has_Delay_Mechanism(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_flag")
def Get_Suspend_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_flag")
def Set_Suspend_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_ref")
def Get_Is_Ref(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_is_ref")
def Set_Is_Ref(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_is_forward_ref")
def Get_Is_Forward_Ref(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_is_forward_ref")
def Set_Is_Forward_Ref(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_property")
def Get_Psl_Property(obj: Iir) -> PSLNode:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_property")
def Set_Psl_Property(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_sequence")
def Get_Psl_Sequence(obj: Iir) -> PSLNode:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_sequence")
def Set_Psl_Sequence(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_declaration")
def Get_Psl_Declaration(obj: Iir) -> PSLNode:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_declaration")
def Set_Psl_Declaration(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_expression")
def Get_Psl_Expression(obj: Iir) -> PSLNode:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_expression")
def Set_Psl_Expression(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_boolean")
def Get_Psl_Boolean(obj: Iir) -> PSLNode:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_boolean")
def Set_Psl_Boolean(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_clock")
def Get_PSL_Clock(obj: Iir) -> PSLNode:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_clock")
def Set_PSL_Clock(obj: Iir, value: PSLNode) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_nfa")
def Get_PSL_NFA(obj: Iir) -> PSLNFA:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_nfa")
def Set_PSL_NFA(obj: Iir, value: PSLNFA) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_nbr_states")
def Get_PSL_Nbr_States(obj: Iir) -> Int32:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_nbr_states")
def Set_PSL_Nbr_States(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_clock_sensitivity")
def Get_PSL_Clock_Sensitivity(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_clock_sensitivity")
def Set_PSL_Clock_Sensitivity(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_eos_flag")
def Get_PSL_EOS_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_eos_flag")
def Set_PSL_EOS_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_psl_abort_flag")
def Get_PSL_Abort_Flag(obj: Iir) -> Boolean:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_psl_abort_flag")
def Set_PSL_Abort_Flag(obj: Iir, value: Boolean) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_count_expression")
def Get_Count_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_count_expression")
def Set_Count_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_clock_expression")
def Get_Clock_Expression(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_clock_expression")
def Set_Clock_Expression(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_default_clock")
def Get_Default_Clock(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_default_clock")
def Set_Default_Clock(obj: Iir, value: Iir) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_foreign_node")
def Get_Foreign_Node(obj: Iir) -> Int32:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_foreign_node")
def Set_Foreign_Node(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_state_index")
def Get_Suspend_State_Index(obj: Iir) -> Int32:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_state_index")
def Set_Suspend_State_Index(obj: Iir, value: Int32) -> None:
    """"""


@export
@BindToLibGHDL("vhdl__nodes__get_suspend_state_chain")
def Get_Suspend_State_Chain(obj: Iir) -> Iir:
    """"""
    return 0


@export
@BindToLibGHDL("vhdl__nodes__set_suspend_state_chain")
def Set_Suspend_State_Chain(obj: Iir, value: Iir) -> None:
    """"""
