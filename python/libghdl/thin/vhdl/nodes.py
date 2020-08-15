from libghdl import libghdl

Null_Iir = 0

Null_Iir_List = 0
Iir_List_All = 1

Null_Iir_Flist = 0
Iir_Flist_Others = 1
Iir_Flist_All = 2


class Iir_Kind:
    Unused = 0
    Error = 1
    Design_File = 2
    Design_Unit = 3
    Library_Clause = 4
    Use_Clause = 5
    Context_Reference = 6
    Integer_Literal = 7
    Floating_Point_Literal = 8
    Null_Literal = 9
    String_Literal8 = 10
    Physical_Int_Literal = 11
    Physical_Fp_Literal = 12
    Simple_Aggregate = 13
    Overflow_Literal = 14
    Unaffected_Waveform = 15
    Waveform_Element = 16
    Conditional_Waveform = 17
    Conditional_Expression = 18
    Association_Element_By_Expression = 19
    Association_Element_By_Individual = 20
    Association_Element_Open = 21
    Association_Element_Package = 22
    Association_Element_Type = 23
    Association_Element_Subprogram = 24
    Association_Element_Terminal = 25
    Choice_By_Range = 26
    Choice_By_Expression = 27
    Choice_By_Others = 28
    Choice_By_None = 29
    Choice_By_Name = 30
    Entity_Aspect_Entity = 31
    Entity_Aspect_Configuration = 32
    Entity_Aspect_Open = 33
    Psl_Hierarchical_Name = 34
    Block_Configuration = 35
    Block_Header = 36
    Component_Configuration = 37
    Binding_Indication = 38
    Entity_Class = 39
    Attribute_Value = 40
    Signature = 41
    Aggregate_Info = 42
    Procedure_Call = 43
    Record_Element_Constraint = 44
    Array_Element_Resolution = 45
    Record_Resolution = 46
    Record_Element_Resolution = 47
    Break_Element = 48
    Attribute_Specification = 49
    Disconnection_Specification = 50
    Step_Limit_Specification = 51
    Configuration_Specification = 52
    Access_Type_Definition = 53
    Incomplete_Type_Definition = 54
    Interface_Type_Definition = 55
    File_Type_Definition = 56
    Protected_Type_Declaration = 57
    Record_Type_Definition = 58
    Array_Type_Definition = 59
    Array_Subtype_Definition = 60
    Record_Subtype_Definition = 61
    Access_Subtype_Definition = 62
    Physical_Subtype_Definition = 63
    Floating_Subtype_Definition = 64
    Integer_Subtype_Definition = 65
    Enumeration_Subtype_Definition = 66
    Enumeration_Type_Definition = 67
    Integer_Type_Definition = 68
    Floating_Type_Definition = 69
    Physical_Type_Definition = 70
    Range_Expression = 71
    Protected_Type_Body = 72
    Wildcard_Type_Definition = 73
    Subtype_Definition = 74
    Scalar_Nature_Definition = 75
    Record_Nature_Definition = 76
    Array_Nature_Definition = 77
    Array_Subnature_Definition = 78
    Overload_List = 79
    Entity_Declaration = 80
    Configuration_Declaration = 81
    Context_Declaration = 82
    Package_Declaration = 83
    Package_Instantiation_Declaration = 84
    Vmode_Declaration = 85
    Vprop_Declaration = 86
    Vunit_Declaration = 87
    Package_Body = 88
    Architecture_Body = 89
    Type_Declaration = 90
    Anonymous_Type_Declaration = 91
    Subtype_Declaration = 92
    Nature_Declaration = 93
    Subnature_Declaration = 94
    Package_Header = 95
    Unit_Declaration = 96
    Library_Declaration = 97
    Component_Declaration = 98
    Attribute_Declaration = 99
    Group_Template_Declaration = 100
    Group_Declaration = 101
    Element_Declaration = 102
    Nature_Element_Declaration = 103
    Non_Object_Alias_Declaration = 104
    Psl_Declaration = 105
    Psl_Endpoint_Declaration = 106
    Enumeration_Literal = 107
    Function_Declaration = 108
    Procedure_Declaration = 109
    Function_Body = 110
    Procedure_Body = 111
    Terminal_Declaration = 112
    Object_Alias_Declaration = 113
    Free_Quantity_Declaration = 114
    Spectrum_Quantity_Declaration = 115
    Noise_Quantity_Declaration = 116
    Across_Quantity_Declaration = 117
    Through_Quantity_Declaration = 118
    File_Declaration = 119
    Guard_Signal_Declaration = 120
    Signal_Declaration = 121
    Variable_Declaration = 122
    Constant_Declaration = 123
    Iterator_Declaration = 124
    Interface_Constant_Declaration = 125
    Interface_Variable_Declaration = 126
    Interface_Signal_Declaration = 127
    Interface_File_Declaration = 128
    Interface_Quantity_Declaration = 129
    Interface_Terminal_Declaration = 130
    Interface_Type_Declaration = 131
    Interface_Package_Declaration = 132
    Interface_Function_Declaration = 133
    Interface_Procedure_Declaration = 134
    Anonymous_Signal_Declaration = 135
    Signal_Attribute_Declaration = 136
    Identity_Operator = 137
    Negation_Operator = 138
    Absolute_Operator = 139
    Not_Operator = 140
    Implicit_Condition_Operator = 141
    Condition_Operator = 142
    Reduction_And_Operator = 143
    Reduction_Or_Operator = 144
    Reduction_Nand_Operator = 145
    Reduction_Nor_Operator = 146
    Reduction_Xor_Operator = 147
    Reduction_Xnor_Operator = 148
    And_Operator = 149
    Or_Operator = 150
    Nand_Operator = 151
    Nor_Operator = 152
    Xor_Operator = 153
    Xnor_Operator = 154
    Equality_Operator = 155
    Inequality_Operator = 156
    Less_Than_Operator = 157
    Less_Than_Or_Equal_Operator = 158
    Greater_Than_Operator = 159
    Greater_Than_Or_Equal_Operator = 160
    Match_Equality_Operator = 161
    Match_Inequality_Operator = 162
    Match_Less_Than_Operator = 163
    Match_Less_Than_Or_Equal_Operator = 164
    Match_Greater_Than_Operator = 165
    Match_Greater_Than_Or_Equal_Operator = 166
    Sll_Operator = 167
    Sla_Operator = 168
    Srl_Operator = 169
    Sra_Operator = 170
    Rol_Operator = 171
    Ror_Operator = 172
    Addition_Operator = 173
    Substraction_Operator = 174
    Concatenation_Operator = 175
    Multiplication_Operator = 176
    Division_Operator = 177
    Modulus_Operator = 178
    Remainder_Operator = 179
    Exponentiation_Operator = 180
    Function_Call = 181
    Aggregate = 182
    Parenthesis_Expression = 183
    Qualified_Expression = 184
    Type_Conversion = 185
    Allocator_By_Expression = 186
    Allocator_By_Subtype = 187
    Selected_Element = 188
    Dereference = 189
    Implicit_Dereference = 190
    Slice_Name = 191
    Indexed_Name = 192
    Psl_Prev = 193
    Psl_Stable = 194
    Psl_Rose = 195
    Psl_Fell = 196
    Psl_Expression = 197
    Sensitized_Process_Statement = 198
    Process_Statement = 199
    Concurrent_Simple_Signal_Assignment = 200
    Concurrent_Conditional_Signal_Assignment = 201
    Concurrent_Selected_Signal_Assignment = 202
    Concurrent_Assertion_Statement = 203
    Concurrent_Procedure_Call_Statement = 204
    Concurrent_Break_Statement = 205
    Psl_Assert_Directive = 206
    Psl_Assume_Directive = 207
    Psl_Cover_Directive = 208
    Psl_Restrict_Directive = 209
    Block_Statement = 210
    If_Generate_Statement = 211
    Case_Generate_Statement = 212
    For_Generate_Statement = 213
    Component_Instantiation_Statement = 214
    Psl_Default_Clock = 215
    Generate_Statement_Body = 216
    If_Generate_Else_Clause = 217
    Simple_Simultaneous_Statement = 218
    Simultaneous_Null_Statement = 219
    Simultaneous_Procedural_Statement = 220
    Simultaneous_Case_Statement = 221
    Simultaneous_If_Statement = 222
    Simultaneous_Elsif = 223
    Simple_Signal_Assignment_Statement = 224
    Conditional_Signal_Assignment_Statement = 225
    Selected_Waveform_Assignment_Statement = 226
    Signal_Force_Assignment_Statement = 227
    Signal_Release_Assignment_Statement = 228
    Null_Statement = 229
    Assertion_Statement = 230
    Report_Statement = 231
    Wait_Statement = 232
    Variable_Assignment_Statement = 233
    Conditional_Variable_Assignment_Statement = 234
    Return_Statement = 235
    For_Loop_Statement = 236
    While_Loop_Statement = 237
    Next_Statement = 238
    Exit_Statement = 239
    Case_Statement = 240
    Procedure_Call_Statement = 241
    Break_Statement = 242
    If_Statement = 243
    Elsif = 244
    Character_Literal = 245
    Simple_Name = 246
    Selected_Name = 247
    Operator_Symbol = 248
    Reference_Name = 249
    External_Constant_Name = 250
    External_Signal_Name = 251
    External_Variable_Name = 252
    Selected_By_All_Name = 253
    Parenthesis_Name = 254
    Package_Pathname = 255
    Absolute_Pathname = 256
    Relative_Pathname = 257
    Pathname_Element = 258
    Base_Attribute = 259
    Subtype_Attribute = 260
    Element_Attribute = 261
    Across_Attribute = 262
    Through_Attribute = 263
    Nature_Reference_Attribute = 264
    Left_Type_Attribute = 265
    Right_Type_Attribute = 266
    High_Type_Attribute = 267
    Low_Type_Attribute = 268
    Ascending_Type_Attribute = 269
    Image_Attribute = 270
    Value_Attribute = 271
    Pos_Attribute = 272
    Val_Attribute = 273
    Succ_Attribute = 274
    Pred_Attribute = 275
    Leftof_Attribute = 276
    Rightof_Attribute = 277
    Signal_Slew_Attribute = 278
    Quantity_Slew_Attribute = 279
    Ramp_Attribute = 280
    Zoh_Attribute = 281
    Ltf_Attribute = 282
    Ztf_Attribute = 283
    Dot_Attribute = 284
    Integ_Attribute = 285
    Above_Attribute = 286
    Quantity_Delayed_Attribute = 287
    Delayed_Attribute = 288
    Stable_Attribute = 289
    Quiet_Attribute = 290
    Transaction_Attribute = 291
    Event_Attribute = 292
    Active_Attribute = 293
    Last_Event_Attribute = 294
    Last_Active_Attribute = 295
    Last_Value_Attribute = 296
    Driving_Attribute = 297
    Driving_Value_Attribute = 298
    Behavior_Attribute = 299
    Structure_Attribute = 300
    Simple_Name_Attribute = 301
    Instance_Name_Attribute = 302
    Path_Name_Attribute = 303
    Left_Array_Attribute = 304
    Right_Array_Attribute = 305
    High_Array_Attribute = 306
    Low_Array_Attribute = 307
    Length_Array_Attribute = 308
    Ascending_Array_Attribute = 309
    Range_Array_Attribute = 310
    Reverse_Range_Array_Attribute = 311
    Attribute_Name = 312


class Iir_Kinds:
    Variable_Assignment_Statement = [
        Iir_Kind.Variable_Assignment_Statement,
        Iir_Kind.Conditional_Variable_Assignment_Statement,
    ]

    Denoting_Name = [
        Iir_Kind.Character_Literal,
        Iir_Kind.Simple_Name,
        Iir_Kind.Selected_Name,
        Iir_Kind.Operator_Symbol,
        Iir_Kind.Reference_Name,
    ]

    Case_Choice = [
        Iir_Kind.Choice_By_Range,
        Iir_Kind.Choice_By_Expression,
        Iir_Kind.Choice_By_Others,
    ]

    Array_Type_Definition = [
        Iir_Kind.Array_Type_Definition,
        Iir_Kind.Array_Subtype_Definition,
    ]

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

    Array_Choice = [
        Iir_Kind.Choice_By_Range,
        Iir_Kind.Choice_By_Expression,
        Iir_Kind.Choice_By_Others,
        Iir_Kind.Choice_By_None,
    ]

    Subprogram_Declaration = [
        Iir_Kind.Function_Declaration,
        Iir_Kind.Procedure_Declaration,
    ]

    Subtype_Attribute = [
        Iir_Kind.Base_Attribute,
        Iir_Kind.Subtype_Attribute,
        Iir_Kind.Element_Attribute,
    ]

    Scalar_Subtype_Definition = [
        Iir_Kind.Physical_Subtype_Definition,
        Iir_Kind.Floating_Subtype_Definition,
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
    ]

    Subnature_Definition = [Iir_Kind.Array_Subnature_Definition]

    Literal = [
        Iir_Kind.Integer_Literal,
        Iir_Kind.Floating_Point_Literal,
        Iir_Kind.Null_Literal,
        Iir_Kind.String_Literal8,
        Iir_Kind.Physical_Int_Literal,
        Iir_Kind.Physical_Fp_Literal,
    ]

    Nature_Indication = [
        Iir_Kind.Scalar_Nature_Definition,
        Iir_Kind.Record_Nature_Definition,
        Iir_Kind.Array_Nature_Definition,
        Iir_Kind.Array_Subnature_Definition,
    ]

    Process_Statement = [
        Iir_Kind.Sensitized_Process_Statement,
        Iir_Kind.Process_Statement,
    ]

    Nature_Definition = [
        Iir_Kind.Scalar_Nature_Definition,
        Iir_Kind.Record_Nature_Definition,
        Iir_Kind.Array_Nature_Definition,
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

    Clause = [Iir_Kind.Library_Clause, Iir_Kind.Use_Clause, Iir_Kind.Context_Reference]

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

    External_Name = [
        Iir_Kind.External_Constant_Name,
        Iir_Kind.External_Signal_Name,
        Iir_Kind.External_Variable_Name,
    ]

    Dereference = [Iir_Kind.Dereference, Iir_Kind.Implicit_Dereference]

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

    Record_Choice = [
        Iir_Kind.Choice_By_Others,
        Iir_Kind.Choice_By_None,
        Iir_Kind.Choice_By_Name,
    ]

    Functions_And_Literals = [
        Iir_Kind.Enumeration_Literal,
        Iir_Kind.Function_Declaration,
    ]

    Verification_Unit = [
        Iir_Kind.Vmode_Declaration,
        Iir_Kind.Vprop_Declaration,
        Iir_Kind.Vunit_Declaration,
    ]

    Secondary_Unit = [Iir_Kind.Package_Body, Iir_Kind.Architecture_Body]

    Package_Declaration = [
        Iir_Kind.Package_Declaration,
        Iir_Kind.Package_Instantiation_Declaration,
    ]

    Psl_Builtin = [
        Iir_Kind.Psl_Prev,
        Iir_Kind.Psl_Stable,
        Iir_Kind.Psl_Rose,
        Iir_Kind.Psl_Fell,
    ]

    Generate_Statement = [
        Iir_Kind.If_Generate_Statement,
        Iir_Kind.Case_Generate_Statement,
        Iir_Kind.For_Generate_Statement,
    ]

    Composite_Subtype_Definition = [
        Iir_Kind.Array_Subtype_Definition,
        Iir_Kind.Record_Subtype_Definition,
    ]

    Choice = [
        Iir_Kind.Choice_By_Range,
        Iir_Kind.Choice_By_Expression,
        Iir_Kind.Choice_By_Others,
        Iir_Kind.Choice_By_None,
        Iir_Kind.Choice_By_Name,
    ]

    If_Case_Generate_Statement = [
        Iir_Kind.If_Generate_Statement,
        Iir_Kind.Case_Generate_Statement,
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

    Entity_Aspect = [
        Iir_Kind.Entity_Aspect_Entity,
        Iir_Kind.Entity_Aspect_Configuration,
        Iir_Kind.Entity_Aspect_Open,
    ]

    Subprogram_Body = [Iir_Kind.Function_Body, Iir_Kind.Procedure_Body]

    Source_Quantity_Declaration = [
        Iir_Kind.Spectrum_Quantity_Declaration,
        Iir_Kind.Noise_Quantity_Declaration,
    ]

    Specification = [
        Iir_Kind.Attribute_Specification,
        Iir_Kind.Disconnection_Specification,
        Iir_Kind.Step_Limit_Specification,
        Iir_Kind.Configuration_Specification,
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

    Association_Element_Parameters = [
        Iir_Kind.Association_Element_By_Expression,
        Iir_Kind.Association_Element_By_Individual,
        Iir_Kind.Association_Element_Open,
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

    Signal_Attribute = [
        Iir_Kind.Delayed_Attribute,
        Iir_Kind.Stable_Attribute,
        Iir_Kind.Quiet_Attribute,
        Iir_Kind.Transaction_Attribute,
    ]

    Type_Declaration = [
        Iir_Kind.Type_Declaration,
        Iir_Kind.Anonymous_Type_Declaration,
        Iir_Kind.Subtype_Declaration,
    ]

    Next_Exit_Statement = [Iir_Kind.Next_Statement, Iir_Kind.Exit_Statement]

    Association_Element = [
        Iir_Kind.Association_Element_By_Expression,
        Iir_Kind.Association_Element_By_Individual,
        Iir_Kind.Association_Element_Open,
        Iir_Kind.Association_Element_Package,
        Iir_Kind.Association_Element_Type,
        Iir_Kind.Association_Element_Subprogram,
        Iir_Kind.Association_Element_Terminal,
    ]

    Interface_Object_Declaration = [
        Iir_Kind.Interface_Constant_Declaration,
        Iir_Kind.Interface_Variable_Declaration,
        Iir_Kind.Interface_Signal_Declaration,
        Iir_Kind.Interface_File_Declaration,
        Iir_Kind.Interface_Quantity_Declaration,
    ]

    Composite_Type_Definition = [
        Iir_Kind.Record_Type_Definition,
        Iir_Kind.Array_Type_Definition,
        Iir_Kind.Array_Subtype_Definition,
        Iir_Kind.Record_Subtype_Definition,
    ]

    Interface_Subprogram_Declaration = [
        Iir_Kind.Interface_Function_Declaration,
        Iir_Kind.Interface_Procedure_Declaration,
    ]

    Branch_Quantity_Declaration = [
        Iir_Kind.Across_Quantity_Declaration,
        Iir_Kind.Through_Quantity_Declaration,
    ]

    Type_Attribute = [
        Iir_Kind.Left_Type_Attribute,
        Iir_Kind.Right_Type_Attribute,
        Iir_Kind.High_Type_Attribute,
        Iir_Kind.Low_Type_Attribute,
        Iir_Kind.Ascending_Type_Attribute,
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

    Quantity_Declaration = [
        Iir_Kind.Free_Quantity_Declaration,
        Iir_Kind.Spectrum_Quantity_Declaration,
        Iir_Kind.Noise_Quantity_Declaration,
        Iir_Kind.Across_Quantity_Declaration,
        Iir_Kind.Through_Quantity_Declaration,
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

    Physical_Literal = [Iir_Kind.Physical_Int_Literal, Iir_Kind.Physical_Fp_Literal]

    Simultaneous_Statement = [
        Iir_Kind.Simple_Simultaneous_Statement,
        Iir_Kind.Simultaneous_Null_Statement,
        Iir_Kind.Simultaneous_Procedural_Statement,
        Iir_Kind.Simultaneous_Case_Statement,
        Iir_Kind.Simultaneous_If_Statement,
    ]

    Concurrent_Signal_Assignment = [
        Iir_Kind.Concurrent_Simple_Signal_Assignment,
        Iir_Kind.Concurrent_Conditional_Signal_Assignment,
        Iir_Kind.Concurrent_Selected_Signal_Assignment,
    ]

    Range_Attribute = [
        Iir_Kind.Range_Array_Attribute,
        Iir_Kind.Reverse_Range_Array_Attribute,
    ]

    Name_Attribute = [
        Iir_Kind.Simple_Name_Attribute,
        Iir_Kind.Instance_Name_Attribute,
        Iir_Kind.Path_Name_Attribute,
    ]

    Scalar_Type_Attribute = [
        Iir_Kind.Pos_Attribute,
        Iir_Kind.Val_Attribute,
        Iir_Kind.Succ_Attribute,
        Iir_Kind.Pred_Attribute,
        Iir_Kind.Leftof_Attribute,
        Iir_Kind.Rightof_Attribute,
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

    Subtype_Definition = [
        Iir_Kind.Array_Subtype_Definition,
        Iir_Kind.Record_Subtype_Definition,
        Iir_Kind.Access_Subtype_Definition,
        Iir_Kind.Physical_Subtype_Definition,
        Iir_Kind.Floating_Subtype_Definition,
        Iir_Kind.Integer_Subtype_Definition,
        Iir_Kind.Enumeration_Subtype_Definition,
    ]

    Allocator = [Iir_Kind.Allocator_By_Expression, Iir_Kind.Allocator_By_Subtype]


class Iir_Mode:
    Unknown_Mode = 0
    Linkage_Mode = 1
    Buffer_Mode = 2
    Out_Mode = 3
    Inout_Mode = 4
    In_Mode = 5


class Iir_Staticness:
    Unknown = 0
    PNone = 1
    Globally = 2
    Locally = 3


class Iir_Constraint:
    Unconstrained = 0
    Partially_Constrained = 1
    Fully_Constrained = 2


class Iir_Delay_Mechanism:
    Inertial_Delay = 0
    Transport_Delay = 1


class Date_State:
    Extern = 0
    Disk = 1
    Parse = 2
    Analyze = 3


class Iir_Predefined:
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
    Ieee_1164_And_Suv = 220
    Ieee_1164_Nand_Suv = 221
    Ieee_1164_Or_Suv = 222
    Ieee_1164_Nor_Suv = 223
    Ieee_1164_Xor_Suv = 224
    Ieee_1164_Xnor_Suv = 225
    Ieee_1164_Vector_Sll = 226
    Ieee_1164_Vector_Srl = 227
    Ieee_1164_Vector_Rol = 228
    Ieee_1164_Vector_Ror = 229
    Ieee_1164_Condition_Operator = 230
    Ieee_Numeric_Std_Toint_Uns_Nat = 231
    Ieee_Numeric_Std_Toint_Sgn_Int = 232
    Ieee_Numeric_Std_Touns_Nat_Nat_Uns = 233
    Ieee_Numeric_Std_Touns_Nat_Uns_Uns = 234
    Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn = 235
    Ieee_Numeric_Std_Tosgn_Int_Sgn_Sgn = 236
    Ieee_Numeric_Std_Resize_Uns_Nat = 237
    Ieee_Numeric_Std_Resize_Sgn_Nat = 238
    Ieee_Numeric_Std_Resize_Uns_Uns = 239
    Ieee_Numeric_Std_Resize_Sgn_Sgn = 240
    Ieee_Numeric_Std_Add_Uns_Uns = 241
    Ieee_Numeric_Std_Add_Uns_Nat = 242
    Ieee_Numeric_Std_Add_Nat_Uns = 243
    Ieee_Numeric_Std_Add_Uns_Log = 244
    Ieee_Numeric_Std_Add_Log_Uns = 245
    Ieee_Numeric_Std_Add_Sgn_Sgn = 246
    Ieee_Numeric_Std_Add_Sgn_Int = 247
    Ieee_Numeric_Std_Add_Int_Sgn = 248
    Ieee_Numeric_Std_Add_Sgn_Log = 249
    Ieee_Numeric_Std_Add_Log_Sgn = 250
    Ieee_Numeric_Std_Sub_Uns_Uns = 251
    Ieee_Numeric_Std_Sub_Uns_Nat = 252
    Ieee_Numeric_Std_Sub_Nat_Uns = 253
    Ieee_Numeric_Std_Sub_Uns_Log = 254
    Ieee_Numeric_Std_Sub_Log_Uns = 255
    Ieee_Numeric_Std_Sub_Sgn_Sgn = 256
    Ieee_Numeric_Std_Sub_Sgn_Int = 257
    Ieee_Numeric_Std_Sub_Int_Sgn = 258
    Ieee_Numeric_Std_Sub_Sgn_Log = 259
    Ieee_Numeric_Std_Sub_Log_Sgn = 260
    Ieee_Numeric_Std_Mul_Uns_Uns = 261
    Ieee_Numeric_Std_Mul_Uns_Nat = 262
    Ieee_Numeric_Std_Mul_Nat_Uns = 263
    Ieee_Numeric_Std_Mul_Sgn_Sgn = 264
    Ieee_Numeric_Std_Mul_Sgn_Int = 265
    Ieee_Numeric_Std_Mul_Int_Sgn = 266
    Ieee_Numeric_Std_Div_Uns_Uns = 267
    Ieee_Numeric_Std_Div_Uns_Nat = 268
    Ieee_Numeric_Std_Div_Nat_Uns = 269
    Ieee_Numeric_Std_Div_Sgn_Sgn = 270
    Ieee_Numeric_Std_Div_Sgn_Int = 271
    Ieee_Numeric_Std_Div_Int_Sgn = 272
    Ieee_Numeric_Std_Rem_Uns_Uns = 273
    Ieee_Numeric_Std_Rem_Uns_Nat = 274
    Ieee_Numeric_Std_Rem_Nat_Uns = 275
    Ieee_Numeric_Std_Rem_Sgn_Sgn = 276
    Ieee_Numeric_Std_Rem_Sgn_Int = 277
    Ieee_Numeric_Std_Rem_Int_Sgn = 278
    Ieee_Numeric_Std_Mod_Uns_Uns = 279
    Ieee_Numeric_Std_Mod_Uns_Nat = 280
    Ieee_Numeric_Std_Mod_Nat_Uns = 281
    Ieee_Numeric_Std_Mod_Sgn_Sgn = 282
    Ieee_Numeric_Std_Mod_Sgn_Int = 283
    Ieee_Numeric_Std_Mod_Int_Sgn = 284
    Ieee_Numeric_Std_Gt_Uns_Uns = 285
    Ieee_Numeric_Std_Gt_Uns_Nat = 286
    Ieee_Numeric_Std_Gt_Nat_Uns = 287
    Ieee_Numeric_Std_Gt_Sgn_Sgn = 288
    Ieee_Numeric_Std_Gt_Sgn_Int = 289
    Ieee_Numeric_Std_Gt_Int_Sgn = 290
    Ieee_Numeric_Std_Lt_Uns_Uns = 291
    Ieee_Numeric_Std_Lt_Uns_Nat = 292
    Ieee_Numeric_Std_Lt_Nat_Uns = 293
    Ieee_Numeric_Std_Lt_Sgn_Sgn = 294
    Ieee_Numeric_Std_Lt_Sgn_Int = 295
    Ieee_Numeric_Std_Lt_Int_Sgn = 296
    Ieee_Numeric_Std_Le_Uns_Uns = 297
    Ieee_Numeric_Std_Le_Uns_Nat = 298
    Ieee_Numeric_Std_Le_Nat_Uns = 299
    Ieee_Numeric_Std_Le_Sgn_Sgn = 300
    Ieee_Numeric_Std_Le_Sgn_Int = 301
    Ieee_Numeric_Std_Le_Int_Sgn = 302
    Ieee_Numeric_Std_Ge_Uns_Uns = 303
    Ieee_Numeric_Std_Ge_Uns_Nat = 304
    Ieee_Numeric_Std_Ge_Nat_Uns = 305
    Ieee_Numeric_Std_Ge_Sgn_Sgn = 306
    Ieee_Numeric_Std_Ge_Sgn_Int = 307
    Ieee_Numeric_Std_Ge_Int_Sgn = 308
    Ieee_Numeric_Std_Eq_Uns_Uns = 309
    Ieee_Numeric_Std_Eq_Uns_Nat = 310
    Ieee_Numeric_Std_Eq_Nat_Uns = 311
    Ieee_Numeric_Std_Eq_Sgn_Sgn = 312
    Ieee_Numeric_Std_Eq_Sgn_Int = 313
    Ieee_Numeric_Std_Eq_Int_Sgn = 314
    Ieee_Numeric_Std_Ne_Uns_Uns = 315
    Ieee_Numeric_Std_Ne_Uns_Nat = 316
    Ieee_Numeric_Std_Ne_Nat_Uns = 317
    Ieee_Numeric_Std_Ne_Sgn_Sgn = 318
    Ieee_Numeric_Std_Ne_Sgn_Int = 319
    Ieee_Numeric_Std_Ne_Int_Sgn = 320
    Ieee_Numeric_Std_Match_Gt_Uns_Uns = 321
    Ieee_Numeric_Std_Match_Gt_Uns_Nat = 322
    Ieee_Numeric_Std_Match_Gt_Nat_Uns = 323
    Ieee_Numeric_Std_Match_Gt_Sgn_Sgn = 324
    Ieee_Numeric_Std_Match_Gt_Sgn_Int = 325
    Ieee_Numeric_Std_Match_Gt_Int_Sgn = 326
    Ieee_Numeric_Std_Match_Lt_Uns_Uns = 327
    Ieee_Numeric_Std_Match_Lt_Uns_Nat = 328
    Ieee_Numeric_Std_Match_Lt_Nat_Uns = 329
    Ieee_Numeric_Std_Match_Lt_Sgn_Sgn = 330
    Ieee_Numeric_Std_Match_Lt_Sgn_Int = 331
    Ieee_Numeric_Std_Match_Lt_Int_Sgn = 332
    Ieee_Numeric_Std_Match_Le_Uns_Uns = 333
    Ieee_Numeric_Std_Match_Le_Uns_Nat = 334
    Ieee_Numeric_Std_Match_Le_Nat_Uns = 335
    Ieee_Numeric_Std_Match_Le_Sgn_Sgn = 336
    Ieee_Numeric_Std_Match_Le_Sgn_Int = 337
    Ieee_Numeric_Std_Match_Le_Int_Sgn = 338
    Ieee_Numeric_Std_Match_Ge_Uns_Uns = 339
    Ieee_Numeric_Std_Match_Ge_Uns_Nat = 340
    Ieee_Numeric_Std_Match_Ge_Nat_Uns = 341
    Ieee_Numeric_Std_Match_Ge_Sgn_Sgn = 342
    Ieee_Numeric_Std_Match_Ge_Sgn_Int = 343
    Ieee_Numeric_Std_Match_Ge_Int_Sgn = 344
    Ieee_Numeric_Std_Match_Eq_Uns_Uns = 345
    Ieee_Numeric_Std_Match_Eq_Uns_Nat = 346
    Ieee_Numeric_Std_Match_Eq_Nat_Uns = 347
    Ieee_Numeric_Std_Match_Eq_Sgn_Sgn = 348
    Ieee_Numeric_Std_Match_Eq_Sgn_Int = 349
    Ieee_Numeric_Std_Match_Eq_Int_Sgn = 350
    Ieee_Numeric_Std_Match_Ne_Uns_Uns = 351
    Ieee_Numeric_Std_Match_Ne_Uns_Nat = 352
    Ieee_Numeric_Std_Match_Ne_Nat_Uns = 353
    Ieee_Numeric_Std_Match_Ne_Sgn_Sgn = 354
    Ieee_Numeric_Std_Match_Ne_Sgn_Int = 355
    Ieee_Numeric_Std_Match_Ne_Int_Sgn = 356
    Ieee_Numeric_Std_Sll_Uns_Int = 357
    Ieee_Numeric_Std_Sll_Sgn_Int = 358
    Ieee_Numeric_Std_Srl_Uns_Int = 359
    Ieee_Numeric_Std_Srl_Sgn_Int = 360
    Ieee_Numeric_Std_Sla_Uns_Int = 361
    Ieee_Numeric_Std_Sla_Sgn_Int = 362
    Ieee_Numeric_Std_Sra_Uns_Int = 363
    Ieee_Numeric_Std_Sra_Sgn_Int = 364
    Ieee_Numeric_Std_And_Uns_Uns = 365
    Ieee_Numeric_Std_And_Sgn_Sgn = 366
    Ieee_Numeric_Std_Or_Uns_Uns = 367
    Ieee_Numeric_Std_Or_Sgn_Sgn = 368
    Ieee_Numeric_Std_Nand_Uns_Uns = 369
    Ieee_Numeric_Std_Nand_Sgn_Sgn = 370
    Ieee_Numeric_Std_Nor_Uns_Uns = 371
    Ieee_Numeric_Std_Nor_Sgn_Sgn = 372
    Ieee_Numeric_Std_Xor_Uns_Uns = 373
    Ieee_Numeric_Std_Xor_Sgn_Sgn = 374
    Ieee_Numeric_Std_Xnor_Uns_Uns = 375
    Ieee_Numeric_Std_Xnor_Sgn_Sgn = 376
    Ieee_Numeric_Std_Not_Uns = 377
    Ieee_Numeric_Std_Not_Sgn = 378
    Ieee_Numeric_Std_Abs_Sgn = 379
    Ieee_Numeric_Std_Neg_Uns = 380
    Ieee_Numeric_Std_Neg_Sgn = 381
    Ieee_Numeric_Std_Min_Uns_Uns = 382
    Ieee_Numeric_Std_Min_Uns_Nat = 383
    Ieee_Numeric_Std_Min_Nat_Uns = 384
    Ieee_Numeric_Std_Min_Sgn_Sgn = 385
    Ieee_Numeric_Std_Min_Sgn_Int = 386
    Ieee_Numeric_Std_Min_Int_Sgn = 387
    Ieee_Numeric_Std_Max_Uns_Uns = 388
    Ieee_Numeric_Std_Max_Uns_Nat = 389
    Ieee_Numeric_Std_Max_Nat_Uns = 390
    Ieee_Numeric_Std_Max_Sgn_Sgn = 391
    Ieee_Numeric_Std_Max_Sgn_Int = 392
    Ieee_Numeric_Std_Max_Int_Sgn = 393
    Ieee_Numeric_Std_Shf_Left_Uns_Nat = 394
    Ieee_Numeric_Std_Shf_Right_Uns_Nat = 395
    Ieee_Numeric_Std_Shf_Left_Sgn_Nat = 396
    Ieee_Numeric_Std_Shf_Right_Sgn_Nat = 397
    Ieee_Numeric_Std_Rot_Left_Uns_Nat = 398
    Ieee_Numeric_Std_Rot_Right_Uns_Nat = 399
    Ieee_Numeric_Std_Rot_Left_Sgn_Nat = 400
    Ieee_Numeric_Std_Rot_Right_Sgn_Nat = 401
    Ieee_Numeric_Std_Match_Log = 402
    Ieee_Numeric_Std_Match_Uns = 403
    Ieee_Numeric_Std_Match_Sgn = 404
    Ieee_Numeric_Std_Match_Slv = 405
    Ieee_Numeric_Std_Match_Suv = 406
    Ieee_Numeric_Std_To_01_Uns = 407
    Ieee_Numeric_Std_To_01_Sgn = 408
    Ieee_Math_Real_Ceil = 409
    Ieee_Math_Real_Floor = 410
    Ieee_Math_Real_Round = 411
    Ieee_Math_Real_Log2 = 412
    Ieee_Math_Real_Sin = 413
    Ieee_Math_Real_Cos = 414
    Ieee_Std_Logic_Unsigned_Add_Slv_Slv = 415
    Ieee_Std_Logic_Unsigned_Add_Slv_Int = 416
    Ieee_Std_Logic_Unsigned_Add_Int_Slv = 417
    Ieee_Std_Logic_Unsigned_Add_Slv_Log = 418
    Ieee_Std_Logic_Unsigned_Add_Log_Slv = 419
    Ieee_Std_Logic_Unsigned_Sub_Slv_Slv = 420
    Ieee_Std_Logic_Unsigned_Sub_Slv_Int = 421
    Ieee_Std_Logic_Unsigned_Sub_Int_Slv = 422
    Ieee_Std_Logic_Unsigned_Sub_Slv_Log = 423
    Ieee_Std_Logic_Unsigned_Sub_Log_Slv = 424
    Ieee_Std_Logic_Unsigned_Id_Slv = 425
    Ieee_Std_Logic_Unsigned_Mul_Slv_Slv = 426
    Ieee_Std_Logic_Unsigned_Lt_Slv_Slv = 427
    Ieee_Std_Logic_Unsigned_Lt_Slv_Int = 428
    Ieee_Std_Logic_Unsigned_Lt_Int_Slv = 429
    Ieee_Std_Logic_Unsigned_Le_Slv_Slv = 430
    Ieee_Std_Logic_Unsigned_Le_Slv_Int = 431
    Ieee_Std_Logic_Unsigned_Le_Int_Slv = 432
    Ieee_Std_Logic_Unsigned_Gt_Slv_Slv = 433
    Ieee_Std_Logic_Unsigned_Gt_Slv_Int = 434
    Ieee_Std_Logic_Unsigned_Gt_Int_Slv = 435
    Ieee_Std_Logic_Unsigned_Ge_Slv_Slv = 436
    Ieee_Std_Logic_Unsigned_Ge_Slv_Int = 437
    Ieee_Std_Logic_Unsigned_Ge_Int_Slv = 438
    Ieee_Std_Logic_Unsigned_Eq_Slv_Slv = 439
    Ieee_Std_Logic_Unsigned_Eq_Slv_Int = 440
    Ieee_Std_Logic_Unsigned_Eq_Int_Slv = 441
    Ieee_Std_Logic_Unsigned_Ne_Slv_Slv = 442
    Ieee_Std_Logic_Unsigned_Ne_Slv_Int = 443
    Ieee_Std_Logic_Unsigned_Ne_Int_Slv = 444
    Ieee_Std_Logic_Unsigned_Conv_Integer = 445
    Ieee_Std_Logic_Unsigned_Shl = 446
    Ieee_Std_Logic_Unsigned_Shr = 447
    Ieee_Std_Logic_Signed_Add_Slv_Slv = 448
    Ieee_Std_Logic_Signed_Add_Slv_Int = 449
    Ieee_Std_Logic_Signed_Add_Int_Slv = 450
    Ieee_Std_Logic_Signed_Add_Slv_Log = 451
    Ieee_Std_Logic_Signed_Add_Log_Slv = 452
    Ieee_Std_Logic_Signed_Sub_Slv_Slv = 453
    Ieee_Std_Logic_Signed_Sub_Slv_Int = 454
    Ieee_Std_Logic_Signed_Sub_Int_Slv = 455
    Ieee_Std_Logic_Signed_Sub_Slv_Log = 456
    Ieee_Std_Logic_Signed_Sub_Log_Slv = 457
    Ieee_Std_Logic_Signed_Id_Slv = 458
    Ieee_Std_Logic_Signed_Neg_Slv = 459
    Ieee_Std_Logic_Signed_Abs_Slv = 460
    Ieee_Std_Logic_Signed_Mul_Slv_Slv = 461
    Ieee_Std_Logic_Signed_Lt_Slv_Slv = 462
    Ieee_Std_Logic_Signed_Lt_Slv_Int = 463
    Ieee_Std_Logic_Signed_Lt_Int_Slv = 464
    Ieee_Std_Logic_Signed_Le_Slv_Slv = 465
    Ieee_Std_Logic_Signed_Le_Slv_Int = 466
    Ieee_Std_Logic_Signed_Le_Int_Slv = 467
    Ieee_Std_Logic_Signed_Gt_Slv_Slv = 468
    Ieee_Std_Logic_Signed_Gt_Slv_Int = 469
    Ieee_Std_Logic_Signed_Gt_Int_Slv = 470
    Ieee_Std_Logic_Signed_Ge_Slv_Slv = 471
    Ieee_Std_Logic_Signed_Ge_Slv_Int = 472
    Ieee_Std_Logic_Signed_Ge_Int_Slv = 473
    Ieee_Std_Logic_Signed_Eq_Slv_Slv = 474
    Ieee_Std_Logic_Signed_Eq_Slv_Int = 475
    Ieee_Std_Logic_Signed_Eq_Int_Slv = 476
    Ieee_Std_Logic_Signed_Ne_Slv_Slv = 477
    Ieee_Std_Logic_Signed_Ne_Slv_Int = 478
    Ieee_Std_Logic_Signed_Ne_Int_Slv = 479
    Ieee_Std_Logic_Signed_Conv_Integer = 480
    Ieee_Std_Logic_Signed_Shl = 481
    Ieee_Std_Logic_Signed_Shr = 482
    Ieee_Std_Logic_Arith_Conv_Unsigned_Int = 483
    Ieee_Std_Logic_Arith_Conv_Unsigned_Uns = 484
    Ieee_Std_Logic_Arith_Conv_Unsigned_Sgn = 485
    Ieee_Std_Logic_Arith_Conv_Unsigned_Log = 486
    Ieee_Std_Logic_Arith_Conv_Integer_Int = 487
    Ieee_Std_Logic_Arith_Conv_Integer_Uns = 488
    Ieee_Std_Logic_Arith_Conv_Integer_Sgn = 489
    Ieee_Std_Logic_Arith_Conv_Integer_Log = 490
    Ieee_Std_Logic_Arith_Conv_Vector_Int = 491
    Ieee_Std_Logic_Arith_Conv_Vector_Uns = 492
    Ieee_Std_Logic_Arith_Conv_Vector_Sgn = 493
    Ieee_Std_Logic_Arith_Conv_Vector_Log = 494
    Ieee_Std_Logic_Arith_Ext = 495
    Ieee_Std_Logic_Arith_Sxt = 496
    Ieee_Std_Logic_Arith_Id_Uns_Uns = 497
    Ieee_Std_Logic_Arith_Id_Sgn_Sgn = 498
    Ieee_Std_Logic_Arith_Neg_Sgn_Sgn = 499
    Ieee_Std_Logic_Arith_Abs_Sgn_Sgn = 500
    Ieee_Std_Logic_Arith_Shl_Uns = 501
    Ieee_Std_Logic_Arith_Shl_Sgn = 502
    Ieee_Std_Logic_Arith_Shr_Uns = 503
    Ieee_Std_Logic_Arith_Shr_Sgn = 504
    Ieee_Std_Logic_Arith_Id_Uns_Slv = 505
    Ieee_Std_Logic_Arith_Id_Sgn_Slv = 506
    Ieee_Std_Logic_Arith_Neg_Sgn_Slv = 507
    Ieee_Std_Logic_Arith_Abs_Sgn_Slv = 508
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns = 509
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn = 510
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn = 511
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn = 512
    Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv = 513
    Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv = 514
    Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv = 515
    Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv = 516
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns = 517
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn = 518
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn = 519
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn = 520
    Ieee_Std_Logic_Arith_Add_Uns_Int_Uns = 521
    Ieee_Std_Logic_Arith_Add_Int_Uns_Uns = 522
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn = 523
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn = 524
    Ieee_Std_Logic_Arith_Add_Uns_Log_Uns = 525
    Ieee_Std_Logic_Arith_Add_Log_Uns_Uns = 526
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn = 527
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn = 528
    Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv = 529
    Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv = 530
    Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv = 531
    Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv = 532
    Ieee_Std_Logic_Arith_Add_Uns_Int_Slv = 533
    Ieee_Std_Logic_Arith_Add_Int_Uns_Slv = 534
    Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv = 535
    Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv = 536
    Ieee_Std_Logic_Arith_Add_Uns_Log_Slv = 537
    Ieee_Std_Logic_Arith_Add_Log_Uns_Slv = 538
    Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv = 539
    Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv = 540
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns = 541
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn = 542
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn = 543
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn = 544
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns = 545
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns = 546
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn = 547
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn = 548
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns = 549
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns = 550
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn = 551
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn = 552
    Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv = 553
    Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv = 554
    Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv = 555
    Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv = 556
    Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv = 557
    Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv = 558
    Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv = 559
    Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv = 560
    Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv = 561
    Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv = 562
    Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv = 563
    Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv = 564
    Ieee_Std_Logic_Arith_Lt_Uns_Uns = 565
    Ieee_Std_Logic_Arith_Lt_Sgn_Sgn = 566
    Ieee_Std_Logic_Arith_Lt_Uns_Sgn = 567
    Ieee_Std_Logic_Arith_Lt_Sgn_Uns = 568
    Ieee_Std_Logic_Arith_Lt_Uns_Int = 569
    Ieee_Std_Logic_Arith_Lt_Int_Uns = 570
    Ieee_Std_Logic_Arith_Lt_Sgn_Int = 571
    Ieee_Std_Logic_Arith_Lt_Int_Sgn = 572
    Ieee_Std_Logic_Arith_Le_Uns_Uns = 573
    Ieee_Std_Logic_Arith_Le_Sgn_Sgn = 574
    Ieee_Std_Logic_Arith_Le_Uns_Sgn = 575
    Ieee_Std_Logic_Arith_Le_Sgn_Uns = 576
    Ieee_Std_Logic_Arith_Le_Uns_Int = 577
    Ieee_Std_Logic_Arith_Le_Int_Uns = 578
    Ieee_Std_Logic_Arith_Le_Sgn_Int = 579
    Ieee_Std_Logic_Arith_Le_Int_Sgn = 580
    Ieee_Std_Logic_Arith_Gt_Uns_Uns = 581
    Ieee_Std_Logic_Arith_Gt_Sgn_Sgn = 582
    Ieee_Std_Logic_Arith_Gt_Uns_Sgn = 583
    Ieee_Std_Logic_Arith_Gt_Sgn_Uns = 584
    Ieee_Std_Logic_Arith_Gt_Uns_Int = 585
    Ieee_Std_Logic_Arith_Gt_Int_Uns = 586
    Ieee_Std_Logic_Arith_Gt_Sgn_Int = 587
    Ieee_Std_Logic_Arith_Gt_Int_Sgn = 588
    Ieee_Std_Logic_Arith_Ge_Uns_Uns = 589
    Ieee_Std_Logic_Arith_Ge_Sgn_Sgn = 590
    Ieee_Std_Logic_Arith_Ge_Uns_Sgn = 591
    Ieee_Std_Logic_Arith_Ge_Sgn_Uns = 592
    Ieee_Std_Logic_Arith_Ge_Uns_Int = 593
    Ieee_Std_Logic_Arith_Ge_Int_Uns = 594
    Ieee_Std_Logic_Arith_Ge_Sgn_Int = 595
    Ieee_Std_Logic_Arith_Ge_Int_Sgn = 596
    Ieee_Std_Logic_Arith_Eq_Uns_Uns = 597
    Ieee_Std_Logic_Arith_Eq_Sgn_Sgn = 598
    Ieee_Std_Logic_Arith_Eq_Uns_Sgn = 599
    Ieee_Std_Logic_Arith_Eq_Sgn_Uns = 600
    Ieee_Std_Logic_Arith_Eq_Uns_Int = 601
    Ieee_Std_Logic_Arith_Eq_Int_Uns = 602
    Ieee_Std_Logic_Arith_Eq_Sgn_Int = 603
    Ieee_Std_Logic_Arith_Eq_Int_Sgn = 604
    Ieee_Std_Logic_Arith_Ne_Uns_Uns = 605
    Ieee_Std_Logic_Arith_Ne_Sgn_Sgn = 606
    Ieee_Std_Logic_Arith_Ne_Uns_Sgn = 607
    Ieee_Std_Logic_Arith_Ne_Sgn_Uns = 608
    Ieee_Std_Logic_Arith_Ne_Uns_Int = 609
    Ieee_Std_Logic_Arith_Ne_Int_Uns = 610
    Ieee_Std_Logic_Arith_Ne_Sgn_Int = 611
    Ieee_Std_Logic_Arith_Ne_Int_Sgn = 612
    Ieee_Std_Logic_Misc_And_Reduce_Slv = 613
    Ieee_Std_Logic_Misc_And_Reduce_Suv = 614
    Ieee_Std_Logic_Misc_Nand_Reduce_Slv = 615
    Ieee_Std_Logic_Misc_Nand_Reduce_Suv = 616
    Ieee_Std_Logic_Misc_Or_Reduce_Slv = 617
    Ieee_Std_Logic_Misc_Or_Reduce_Suv = 618
    Ieee_Std_Logic_Misc_Nor_Reduce_Slv = 619
    Ieee_Std_Logic_Misc_Nor_Reduce_Suv = 620
    Ieee_Std_Logic_Misc_Xor_Reduce_Slv = 621
    Ieee_Std_Logic_Misc_Xor_Reduce_Suv = 622
    Ieee_Std_Logic_Misc_Xnor_Reduce_Slv = 623
    Ieee_Std_Logic_Misc_Xnor_Reduce_Suv = 624


Get_Kind = libghdl.vhdl__nodes__get_kind
Get_Location = libghdl.vhdl__nodes__get_location

Get_First_Design_Unit = libghdl.vhdl__nodes__get_first_design_unit

Set_First_Design_Unit = libghdl.vhdl__nodes__set_first_design_unit

Get_Last_Design_Unit = libghdl.vhdl__nodes__get_last_design_unit

Set_Last_Design_Unit = libghdl.vhdl__nodes__set_last_design_unit

Get_Library_Declaration = libghdl.vhdl__nodes__get_library_declaration

Set_Library_Declaration = libghdl.vhdl__nodes__set_library_declaration

Get_File_Checksum = libghdl.vhdl__nodes__get_file_checksum

Set_File_Checksum = libghdl.vhdl__nodes__set_file_checksum

Get_Analysis_Time_Stamp = libghdl.vhdl__nodes__get_analysis_time_stamp

Set_Analysis_Time_Stamp = libghdl.vhdl__nodes__set_analysis_time_stamp

Get_Design_File_Source = libghdl.vhdl__nodes__get_design_file_source

Set_Design_File_Source = libghdl.vhdl__nodes__set_design_file_source

Get_Library = libghdl.vhdl__nodes__get_library

Set_Library = libghdl.vhdl__nodes__set_library

Get_File_Dependence_List = libghdl.vhdl__nodes__get_file_dependence_list

Set_File_Dependence_List = libghdl.vhdl__nodes__set_file_dependence_list

Get_Design_File_Filename = libghdl.vhdl__nodes__get_design_file_filename

Set_Design_File_Filename = libghdl.vhdl__nodes__set_design_file_filename

Get_Design_File_Directory = libghdl.vhdl__nodes__get_design_file_directory

Set_Design_File_Directory = libghdl.vhdl__nodes__set_design_file_directory

Get_Design_File = libghdl.vhdl__nodes__get_design_file

Set_Design_File = libghdl.vhdl__nodes__set_design_file

Get_Design_File_Chain = libghdl.vhdl__nodes__get_design_file_chain

Set_Design_File_Chain = libghdl.vhdl__nodes__set_design_file_chain

Get_Library_Directory = libghdl.vhdl__nodes__get_library_directory

Set_Library_Directory = libghdl.vhdl__nodes__set_library_directory

Get_Date = libghdl.vhdl__nodes__get_date

Set_Date = libghdl.vhdl__nodes__set_date

Get_Context_Items = libghdl.vhdl__nodes__get_context_items

Set_Context_Items = libghdl.vhdl__nodes__set_context_items

Get_Dependence_List = libghdl.vhdl__nodes__get_dependence_list

Set_Dependence_List = libghdl.vhdl__nodes__set_dependence_list

Get_Analysis_Checks_List = libghdl.vhdl__nodes__get_analysis_checks_list

Set_Analysis_Checks_List = libghdl.vhdl__nodes__set_analysis_checks_list

Get_Date_State = libghdl.vhdl__nodes__get_date_state

Set_Date_State = libghdl.vhdl__nodes__set_date_state

Get_Guarded_Target_State = libghdl.vhdl__nodes__get_guarded_target_state

Set_Guarded_Target_State = libghdl.vhdl__nodes__set_guarded_target_state

Get_Library_Unit = libghdl.vhdl__nodes__get_library_unit

Set_Library_Unit = libghdl.vhdl__nodes__set_library_unit

Get_Hash_Chain = libghdl.vhdl__nodes__get_hash_chain

Set_Hash_Chain = libghdl.vhdl__nodes__set_hash_chain

Get_Design_Unit_Source_Pos = libghdl.vhdl__nodes__get_design_unit_source_pos

Set_Design_Unit_Source_Pos = libghdl.vhdl__nodes__set_design_unit_source_pos

Get_Design_Unit_Source_Line = libghdl.vhdl__nodes__get_design_unit_source_line

Set_Design_Unit_Source_Line = libghdl.vhdl__nodes__set_design_unit_source_line

Get_Design_Unit_Source_Col = libghdl.vhdl__nodes__get_design_unit_source_col

Set_Design_Unit_Source_Col = libghdl.vhdl__nodes__set_design_unit_source_col

Get_Value = libghdl.vhdl__nodes__get_value

Set_Value = libghdl.vhdl__nodes__set_value

Get_Enum_Pos = libghdl.vhdl__nodes__get_enum_pos

Set_Enum_Pos = libghdl.vhdl__nodes__set_enum_pos

Get_Physical_Literal = libghdl.vhdl__nodes__get_physical_literal

Set_Physical_Literal = libghdl.vhdl__nodes__set_physical_literal

Get_Fp_Value = libghdl.vhdl__nodes__get_fp_value

Set_Fp_Value = libghdl.vhdl__nodes__set_fp_value

Get_Simple_Aggregate_List = libghdl.vhdl__nodes__get_simple_aggregate_list

Set_Simple_Aggregate_List = libghdl.vhdl__nodes__set_simple_aggregate_list

Get_String8_Id = libghdl.vhdl__nodes__get_string8_id

Set_String8_Id = libghdl.vhdl__nodes__set_string8_id

Get_String_Length = libghdl.vhdl__nodes__get_string_length

Set_String_Length = libghdl.vhdl__nodes__set_string_length

Get_Bit_String_Base = libghdl.vhdl__nodes__get_bit_string_base

Set_Bit_String_Base = libghdl.vhdl__nodes__set_bit_string_base

Get_Has_Signed = libghdl.vhdl__nodes__get_has_signed

Set_Has_Signed = libghdl.vhdl__nodes__set_has_signed

Get_Has_Sign = libghdl.vhdl__nodes__get_has_sign

Set_Has_Sign = libghdl.vhdl__nodes__set_has_sign

Get_Has_Length = libghdl.vhdl__nodes__get_has_length

Set_Has_Length = libghdl.vhdl__nodes__set_has_length

Get_Literal_Length = libghdl.vhdl__nodes__get_literal_length

Set_Literal_Length = libghdl.vhdl__nodes__set_literal_length

Get_Literal_Origin = libghdl.vhdl__nodes__get_literal_origin

Set_Literal_Origin = libghdl.vhdl__nodes__set_literal_origin

Get_Range_Origin = libghdl.vhdl__nodes__get_range_origin

Set_Range_Origin = libghdl.vhdl__nodes__set_range_origin

Get_Literal_Subtype = libghdl.vhdl__nodes__get_literal_subtype

Set_Literal_Subtype = libghdl.vhdl__nodes__set_literal_subtype

Get_Allocator_Subtype = libghdl.vhdl__nodes__get_allocator_subtype

Set_Allocator_Subtype = libghdl.vhdl__nodes__set_allocator_subtype

Get_Entity_Class = libghdl.vhdl__nodes__get_entity_class

Set_Entity_Class = libghdl.vhdl__nodes__set_entity_class

Get_Entity_Name_List = libghdl.vhdl__nodes__get_entity_name_list

Set_Entity_Name_List = libghdl.vhdl__nodes__set_entity_name_list

Get_Attribute_Designator = libghdl.vhdl__nodes__get_attribute_designator

Set_Attribute_Designator = libghdl.vhdl__nodes__set_attribute_designator

Get_Attribute_Specification_Chain = (
    libghdl.vhdl__nodes__get_attribute_specification_chain
)

Set_Attribute_Specification_Chain = (
    libghdl.vhdl__nodes__set_attribute_specification_chain
)

Get_Attribute_Specification = libghdl.vhdl__nodes__get_attribute_specification

Set_Attribute_Specification = libghdl.vhdl__nodes__set_attribute_specification

Get_Signal_List = libghdl.vhdl__nodes__get_signal_list

Set_Signal_List = libghdl.vhdl__nodes__set_signal_list

Get_Quantity_List = libghdl.vhdl__nodes__get_quantity_list

Set_Quantity_List = libghdl.vhdl__nodes__set_quantity_list

Get_Designated_Entity = libghdl.vhdl__nodes__get_designated_entity

Set_Designated_Entity = libghdl.vhdl__nodes__set_designated_entity

Get_Formal = libghdl.vhdl__nodes__get_formal

Set_Formal = libghdl.vhdl__nodes__set_formal

Get_Actual = libghdl.vhdl__nodes__get_actual

Set_Actual = libghdl.vhdl__nodes__set_actual

Get_Actual_Conversion = libghdl.vhdl__nodes__get_actual_conversion

Set_Actual_Conversion = libghdl.vhdl__nodes__set_actual_conversion

Get_Formal_Conversion = libghdl.vhdl__nodes__get_formal_conversion

Set_Formal_Conversion = libghdl.vhdl__nodes__set_formal_conversion

Get_Whole_Association_Flag = libghdl.vhdl__nodes__get_whole_association_flag

Set_Whole_Association_Flag = libghdl.vhdl__nodes__set_whole_association_flag

Get_Collapse_Signal_Flag = libghdl.vhdl__nodes__get_collapse_signal_flag

Set_Collapse_Signal_Flag = libghdl.vhdl__nodes__set_collapse_signal_flag

Get_Artificial_Flag = libghdl.vhdl__nodes__get_artificial_flag

Set_Artificial_Flag = libghdl.vhdl__nodes__set_artificial_flag

Get_Open_Flag = libghdl.vhdl__nodes__get_open_flag

Set_Open_Flag = libghdl.vhdl__nodes__set_open_flag

Get_After_Drivers_Flag = libghdl.vhdl__nodes__get_after_drivers_flag

Set_After_Drivers_Flag = libghdl.vhdl__nodes__set_after_drivers_flag

Get_We_Value = libghdl.vhdl__nodes__get_we_value

Set_We_Value = libghdl.vhdl__nodes__set_we_value

Get_Time = libghdl.vhdl__nodes__get_time

Set_Time = libghdl.vhdl__nodes__set_time

Get_Associated_Expr = libghdl.vhdl__nodes__get_associated_expr

Set_Associated_Expr = libghdl.vhdl__nodes__set_associated_expr

Get_Associated_Block = libghdl.vhdl__nodes__get_associated_block

Set_Associated_Block = libghdl.vhdl__nodes__set_associated_block

Get_Associated_Chain = libghdl.vhdl__nodes__get_associated_chain

Set_Associated_Chain = libghdl.vhdl__nodes__set_associated_chain

Get_Choice_Name = libghdl.vhdl__nodes__get_choice_name

Set_Choice_Name = libghdl.vhdl__nodes__set_choice_name

Get_Choice_Expression = libghdl.vhdl__nodes__get_choice_expression

Set_Choice_Expression = libghdl.vhdl__nodes__set_choice_expression

Get_Choice_Range = libghdl.vhdl__nodes__get_choice_range

Set_Choice_Range = libghdl.vhdl__nodes__set_choice_range

Get_Same_Alternative_Flag = libghdl.vhdl__nodes__get_same_alternative_flag

Set_Same_Alternative_Flag = libghdl.vhdl__nodes__set_same_alternative_flag

Get_Element_Type_Flag = libghdl.vhdl__nodes__get_element_type_flag

Set_Element_Type_Flag = libghdl.vhdl__nodes__set_element_type_flag

Get_Architecture = libghdl.vhdl__nodes__get_architecture

Set_Architecture = libghdl.vhdl__nodes__set_architecture

Get_Block_Specification = libghdl.vhdl__nodes__get_block_specification

Set_Block_Specification = libghdl.vhdl__nodes__set_block_specification

Get_Prev_Block_Configuration = libghdl.vhdl__nodes__get_prev_block_configuration

Set_Prev_Block_Configuration = libghdl.vhdl__nodes__set_prev_block_configuration

Get_Configuration_Item_Chain = libghdl.vhdl__nodes__get_configuration_item_chain

Set_Configuration_Item_Chain = libghdl.vhdl__nodes__set_configuration_item_chain

Get_Attribute_Value_Chain = libghdl.vhdl__nodes__get_attribute_value_chain

Set_Attribute_Value_Chain = libghdl.vhdl__nodes__set_attribute_value_chain

Get_Spec_Chain = libghdl.vhdl__nodes__get_spec_chain

Set_Spec_Chain = libghdl.vhdl__nodes__set_spec_chain

Get_Value_Chain = libghdl.vhdl__nodes__get_value_chain

Set_Value_Chain = libghdl.vhdl__nodes__set_value_chain

Get_Attribute_Value_Spec_Chain = libghdl.vhdl__nodes__get_attribute_value_spec_chain

Set_Attribute_Value_Spec_Chain = libghdl.vhdl__nodes__set_attribute_value_spec_chain

Get_Entity_Name = libghdl.vhdl__nodes__get_entity_name

Set_Entity_Name = libghdl.vhdl__nodes__set_entity_name

Get_Package = libghdl.vhdl__nodes__get_package

Set_Package = libghdl.vhdl__nodes__set_package

Get_Package_Body = libghdl.vhdl__nodes__get_package_body

Set_Package_Body = libghdl.vhdl__nodes__set_package_body

Get_Instance_Package_Body = libghdl.vhdl__nodes__get_instance_package_body

Set_Instance_Package_Body = libghdl.vhdl__nodes__set_instance_package_body

Get_Need_Body = libghdl.vhdl__nodes__get_need_body

Set_Need_Body = libghdl.vhdl__nodes__set_need_body

Get_Macro_Expanded_Flag = libghdl.vhdl__nodes__get_macro_expanded_flag

Set_Macro_Expanded_Flag = libghdl.vhdl__nodes__set_macro_expanded_flag

Get_Need_Instance_Bodies = libghdl.vhdl__nodes__get_need_instance_bodies

Set_Need_Instance_Bodies = libghdl.vhdl__nodes__set_need_instance_bodies

Get_Hierarchical_Name = libghdl.vhdl__nodes__get_hierarchical_name

Set_Hierarchical_Name = libghdl.vhdl__nodes__set_hierarchical_name

Get_Inherit_Spec_Chain = libghdl.vhdl__nodes__get_inherit_spec_chain

Set_Inherit_Spec_Chain = libghdl.vhdl__nodes__set_inherit_spec_chain

Get_Vunit_Item_Chain = libghdl.vhdl__nodes__get_vunit_item_chain

Set_Vunit_Item_Chain = libghdl.vhdl__nodes__set_vunit_item_chain

Get_Bound_Vunit_Chain = libghdl.vhdl__nodes__get_bound_vunit_chain

Set_Bound_Vunit_Chain = libghdl.vhdl__nodes__set_bound_vunit_chain

Get_Verification_Block_Configuration = (
    libghdl.vhdl__nodes__get_verification_block_configuration
)

Set_Verification_Block_Configuration = (
    libghdl.vhdl__nodes__set_verification_block_configuration
)

Get_Block_Configuration = libghdl.vhdl__nodes__get_block_configuration

Set_Block_Configuration = libghdl.vhdl__nodes__set_block_configuration

Get_Concurrent_Statement_Chain = libghdl.vhdl__nodes__get_concurrent_statement_chain

Set_Concurrent_Statement_Chain = libghdl.vhdl__nodes__set_concurrent_statement_chain

Get_Chain = libghdl.vhdl__nodes__get_chain

Set_Chain = libghdl.vhdl__nodes__set_chain

Get_Port_Chain = libghdl.vhdl__nodes__get_port_chain

Set_Port_Chain = libghdl.vhdl__nodes__set_port_chain

Get_Generic_Chain = libghdl.vhdl__nodes__get_generic_chain

Set_Generic_Chain = libghdl.vhdl__nodes__set_generic_chain

Get_Type = libghdl.vhdl__nodes__get_type

Set_Type = libghdl.vhdl__nodes__set_type

Get_Subtype_Indication = libghdl.vhdl__nodes__get_subtype_indication

Set_Subtype_Indication = libghdl.vhdl__nodes__set_subtype_indication

Get_Discrete_Range = libghdl.vhdl__nodes__get_discrete_range

Set_Discrete_Range = libghdl.vhdl__nodes__set_discrete_range

Get_Type_Definition = libghdl.vhdl__nodes__get_type_definition

Set_Type_Definition = libghdl.vhdl__nodes__set_type_definition

Get_Subtype_Definition = libghdl.vhdl__nodes__get_subtype_definition

Set_Subtype_Definition = libghdl.vhdl__nodes__set_subtype_definition

Get_Incomplete_Type_Declaration = libghdl.vhdl__nodes__get_incomplete_type_declaration

Set_Incomplete_Type_Declaration = libghdl.vhdl__nodes__set_incomplete_type_declaration

Get_Interface_Type_Subprograms = libghdl.vhdl__nodes__get_interface_type_subprograms

Set_Interface_Type_Subprograms = libghdl.vhdl__nodes__set_interface_type_subprograms

Get_Nature_Definition = libghdl.vhdl__nodes__get_nature_definition

Set_Nature_Definition = libghdl.vhdl__nodes__set_nature_definition

Get_Nature = libghdl.vhdl__nodes__get_nature

Set_Nature = libghdl.vhdl__nodes__set_nature

Get_Subnature_Indication = libghdl.vhdl__nodes__get_subnature_indication

Set_Subnature_Indication = libghdl.vhdl__nodes__set_subnature_indication

Get_Mode = libghdl.vhdl__nodes__get_mode

Set_Mode = libghdl.vhdl__nodes__set_mode

Get_Guarded_Signal_Flag = libghdl.vhdl__nodes__get_guarded_signal_flag

Set_Guarded_Signal_Flag = libghdl.vhdl__nodes__set_guarded_signal_flag

Get_Signal_Kind = libghdl.vhdl__nodes__get_signal_kind

Set_Signal_Kind = libghdl.vhdl__nodes__set_signal_kind

Get_Base_Name = libghdl.vhdl__nodes__get_base_name

Set_Base_Name = libghdl.vhdl__nodes__set_base_name

Get_Interface_Declaration_Chain = libghdl.vhdl__nodes__get_interface_declaration_chain

Set_Interface_Declaration_Chain = libghdl.vhdl__nodes__set_interface_declaration_chain

Get_Subprogram_Specification = libghdl.vhdl__nodes__get_subprogram_specification

Set_Subprogram_Specification = libghdl.vhdl__nodes__set_subprogram_specification

Get_Sequential_Statement_Chain = libghdl.vhdl__nodes__get_sequential_statement_chain

Set_Sequential_Statement_Chain = libghdl.vhdl__nodes__set_sequential_statement_chain

Get_Simultaneous_Statement_Chain = libghdl.vhdl__nodes__get_simultaneous_statement_chain

Set_Simultaneous_Statement_Chain = libghdl.vhdl__nodes__set_simultaneous_statement_chain

Get_Subprogram_Body = libghdl.vhdl__nodes__get_subprogram_body

Set_Subprogram_Body = libghdl.vhdl__nodes__set_subprogram_body

Get_Overload_Number = libghdl.vhdl__nodes__get_overload_number

Set_Overload_Number = libghdl.vhdl__nodes__set_overload_number

Get_Subprogram_Depth = libghdl.vhdl__nodes__get_subprogram_depth

Set_Subprogram_Depth = libghdl.vhdl__nodes__set_subprogram_depth

Get_Subprogram_Hash = libghdl.vhdl__nodes__get_subprogram_hash

Set_Subprogram_Hash = libghdl.vhdl__nodes__set_subprogram_hash

Get_Impure_Depth = libghdl.vhdl__nodes__get_impure_depth

Set_Impure_Depth = libghdl.vhdl__nodes__set_impure_depth

Get_Return_Type = libghdl.vhdl__nodes__get_return_type

Set_Return_Type = libghdl.vhdl__nodes__set_return_type

Get_Implicit_Definition = libghdl.vhdl__nodes__get_implicit_definition

Set_Implicit_Definition = libghdl.vhdl__nodes__set_implicit_definition

Get_Default_Value = libghdl.vhdl__nodes__get_default_value

Set_Default_Value = libghdl.vhdl__nodes__set_default_value

Get_Deferred_Declaration = libghdl.vhdl__nodes__get_deferred_declaration

Set_Deferred_Declaration = libghdl.vhdl__nodes__set_deferred_declaration

Get_Deferred_Declaration_Flag = libghdl.vhdl__nodes__get_deferred_declaration_flag

Set_Deferred_Declaration_Flag = libghdl.vhdl__nodes__set_deferred_declaration_flag

Get_Shared_Flag = libghdl.vhdl__nodes__get_shared_flag

Set_Shared_Flag = libghdl.vhdl__nodes__set_shared_flag

Get_Design_Unit = libghdl.vhdl__nodes__get_design_unit

Set_Design_Unit = libghdl.vhdl__nodes__set_design_unit

Get_Block_Statement = libghdl.vhdl__nodes__get_block_statement

Set_Block_Statement = libghdl.vhdl__nodes__set_block_statement

Get_Signal_Driver = libghdl.vhdl__nodes__get_signal_driver

Set_Signal_Driver = libghdl.vhdl__nodes__set_signal_driver

Get_Declaration_Chain = libghdl.vhdl__nodes__get_declaration_chain

Set_Declaration_Chain = libghdl.vhdl__nodes__set_declaration_chain

Get_File_Logical_Name = libghdl.vhdl__nodes__get_file_logical_name

Set_File_Logical_Name = libghdl.vhdl__nodes__set_file_logical_name

Get_File_Open_Kind = libghdl.vhdl__nodes__get_file_open_kind

Set_File_Open_Kind = libghdl.vhdl__nodes__set_file_open_kind

Get_Element_Position = libghdl.vhdl__nodes__get_element_position

Set_Element_Position = libghdl.vhdl__nodes__set_element_position

Get_Use_Clause_Chain = libghdl.vhdl__nodes__get_use_clause_chain

Set_Use_Clause_Chain = libghdl.vhdl__nodes__set_use_clause_chain

Get_Context_Reference_Chain = libghdl.vhdl__nodes__get_context_reference_chain

Set_Context_Reference_Chain = libghdl.vhdl__nodes__set_context_reference_chain

Get_Selected_Name = libghdl.vhdl__nodes__get_selected_name

Set_Selected_Name = libghdl.vhdl__nodes__set_selected_name

Get_Type_Declarator = libghdl.vhdl__nodes__get_type_declarator

Set_Type_Declarator = libghdl.vhdl__nodes__set_type_declarator

Get_Complete_Type_Definition = libghdl.vhdl__nodes__get_complete_type_definition

Set_Complete_Type_Definition = libghdl.vhdl__nodes__set_complete_type_definition

Get_Incomplete_Type_Ref_Chain = libghdl.vhdl__nodes__get_incomplete_type_ref_chain

Set_Incomplete_Type_Ref_Chain = libghdl.vhdl__nodes__set_incomplete_type_ref_chain

Get_Associated_Type = libghdl.vhdl__nodes__get_associated_type

Set_Associated_Type = libghdl.vhdl__nodes__set_associated_type

Get_Enumeration_Literal_List = libghdl.vhdl__nodes__get_enumeration_literal_list

Set_Enumeration_Literal_List = libghdl.vhdl__nodes__set_enumeration_literal_list

Get_Entity_Class_Entry_Chain = libghdl.vhdl__nodes__get_entity_class_entry_chain

Set_Entity_Class_Entry_Chain = libghdl.vhdl__nodes__set_entity_class_entry_chain

Get_Group_Constituent_List = libghdl.vhdl__nodes__get_group_constituent_list

Set_Group_Constituent_List = libghdl.vhdl__nodes__set_group_constituent_list

Get_Unit_Chain = libghdl.vhdl__nodes__get_unit_chain

Set_Unit_Chain = libghdl.vhdl__nodes__set_unit_chain

Get_Primary_Unit = libghdl.vhdl__nodes__get_primary_unit

Set_Primary_Unit = libghdl.vhdl__nodes__set_primary_unit

Get_Identifier = libghdl.vhdl__nodes__get_identifier

Set_Identifier = libghdl.vhdl__nodes__set_identifier

Get_Label = libghdl.vhdl__nodes__get_label

Set_Label = libghdl.vhdl__nodes__set_label

Get_Visible_Flag = libghdl.vhdl__nodes__get_visible_flag

Set_Visible_Flag = libghdl.vhdl__nodes__set_visible_flag

Get_Range_Constraint = libghdl.vhdl__nodes__get_range_constraint

Set_Range_Constraint = libghdl.vhdl__nodes__set_range_constraint

Get_Direction = libghdl.vhdl__nodes__get_direction

Set_Direction = libghdl.vhdl__nodes__set_direction

Get_Left_Limit = libghdl.vhdl__nodes__get_left_limit

Set_Left_Limit = libghdl.vhdl__nodes__set_left_limit

Get_Right_Limit = libghdl.vhdl__nodes__get_right_limit

Set_Right_Limit = libghdl.vhdl__nodes__set_right_limit

Get_Left_Limit_Expr = libghdl.vhdl__nodes__get_left_limit_expr

Set_Left_Limit_Expr = libghdl.vhdl__nodes__set_left_limit_expr

Get_Right_Limit_Expr = libghdl.vhdl__nodes__get_right_limit_expr

Set_Right_Limit_Expr = libghdl.vhdl__nodes__set_right_limit_expr

Get_Parent_Type = libghdl.vhdl__nodes__get_parent_type

Set_Parent_Type = libghdl.vhdl__nodes__set_parent_type

Get_Simple_Nature = libghdl.vhdl__nodes__get_simple_nature

Set_Simple_Nature = libghdl.vhdl__nodes__set_simple_nature

Get_Base_Nature = libghdl.vhdl__nodes__get_base_nature

Set_Base_Nature = libghdl.vhdl__nodes__set_base_nature

Get_Resolution_Indication = libghdl.vhdl__nodes__get_resolution_indication

Set_Resolution_Indication = libghdl.vhdl__nodes__set_resolution_indication

Get_Record_Element_Resolution_Chain = (
    libghdl.vhdl__nodes__get_record_element_resolution_chain
)

Set_Record_Element_Resolution_Chain = (
    libghdl.vhdl__nodes__set_record_element_resolution_chain
)

Get_Tolerance = libghdl.vhdl__nodes__get_tolerance

Set_Tolerance = libghdl.vhdl__nodes__set_tolerance

Get_Plus_Terminal_Name = libghdl.vhdl__nodes__get_plus_terminal_name

Set_Plus_Terminal_Name = libghdl.vhdl__nodes__set_plus_terminal_name

Get_Minus_Terminal_Name = libghdl.vhdl__nodes__get_minus_terminal_name

Set_Minus_Terminal_Name = libghdl.vhdl__nodes__set_minus_terminal_name

Get_Plus_Terminal = libghdl.vhdl__nodes__get_plus_terminal

Set_Plus_Terminal = libghdl.vhdl__nodes__set_plus_terminal

Get_Minus_Terminal = libghdl.vhdl__nodes__get_minus_terminal

Set_Minus_Terminal = libghdl.vhdl__nodes__set_minus_terminal

Get_Magnitude_Expression = libghdl.vhdl__nodes__get_magnitude_expression

Set_Magnitude_Expression = libghdl.vhdl__nodes__set_magnitude_expression

Get_Phase_Expression = libghdl.vhdl__nodes__get_phase_expression

Set_Phase_Expression = libghdl.vhdl__nodes__set_phase_expression

Get_Power_Expression = libghdl.vhdl__nodes__get_power_expression

Set_Power_Expression = libghdl.vhdl__nodes__set_power_expression

Get_Simultaneous_Left = libghdl.vhdl__nodes__get_simultaneous_left

Set_Simultaneous_Left = libghdl.vhdl__nodes__set_simultaneous_left

Get_Simultaneous_Right = libghdl.vhdl__nodes__get_simultaneous_right

Set_Simultaneous_Right = libghdl.vhdl__nodes__set_simultaneous_right

Get_Text_File_Flag = libghdl.vhdl__nodes__get_text_file_flag

Set_Text_File_Flag = libghdl.vhdl__nodes__set_text_file_flag

Get_Only_Characters_Flag = libghdl.vhdl__nodes__get_only_characters_flag

Set_Only_Characters_Flag = libghdl.vhdl__nodes__set_only_characters_flag

Get_Is_Character_Type = libghdl.vhdl__nodes__get_is_character_type

Set_Is_Character_Type = libghdl.vhdl__nodes__set_is_character_type

Get_Nature_Staticness = libghdl.vhdl__nodes__get_nature_staticness

Set_Nature_Staticness = libghdl.vhdl__nodes__set_nature_staticness

Get_Type_Staticness = libghdl.vhdl__nodes__get_type_staticness

Set_Type_Staticness = libghdl.vhdl__nodes__set_type_staticness

Get_Constraint_State = libghdl.vhdl__nodes__get_constraint_state

Set_Constraint_State = libghdl.vhdl__nodes__set_constraint_state

Get_Index_Subtype_List = libghdl.vhdl__nodes__get_index_subtype_list

Set_Index_Subtype_List = libghdl.vhdl__nodes__set_index_subtype_list

Get_Index_Subtype_Definition_List = (
    libghdl.vhdl__nodes__get_index_subtype_definition_list
)

Set_Index_Subtype_Definition_List = (
    libghdl.vhdl__nodes__set_index_subtype_definition_list
)

Get_Element_Subtype_Indication = libghdl.vhdl__nodes__get_element_subtype_indication

Set_Element_Subtype_Indication = libghdl.vhdl__nodes__set_element_subtype_indication

Get_Element_Subtype = libghdl.vhdl__nodes__get_element_subtype

Set_Element_Subtype = libghdl.vhdl__nodes__set_element_subtype

Get_Element_Subnature_Indication = libghdl.vhdl__nodes__get_element_subnature_indication

Set_Element_Subnature_Indication = libghdl.vhdl__nodes__set_element_subnature_indication

Get_Element_Subnature = libghdl.vhdl__nodes__get_element_subnature

Set_Element_Subnature = libghdl.vhdl__nodes__set_element_subnature

Get_Index_Constraint_List = libghdl.vhdl__nodes__get_index_constraint_list

Set_Index_Constraint_List = libghdl.vhdl__nodes__set_index_constraint_list

Get_Array_Element_Constraint = libghdl.vhdl__nodes__get_array_element_constraint

Set_Array_Element_Constraint = libghdl.vhdl__nodes__set_array_element_constraint

Get_Elements_Declaration_List = libghdl.vhdl__nodes__get_elements_declaration_list

Set_Elements_Declaration_List = libghdl.vhdl__nodes__set_elements_declaration_list

Get_Owned_Elements_Chain = libghdl.vhdl__nodes__get_owned_elements_chain

Set_Owned_Elements_Chain = libghdl.vhdl__nodes__set_owned_elements_chain

Get_Designated_Type = libghdl.vhdl__nodes__get_designated_type

Set_Designated_Type = libghdl.vhdl__nodes__set_designated_type

Get_Designated_Subtype_Indication = (
    libghdl.vhdl__nodes__get_designated_subtype_indication
)

Set_Designated_Subtype_Indication = (
    libghdl.vhdl__nodes__set_designated_subtype_indication
)

Get_Index_List = libghdl.vhdl__nodes__get_index_list

Set_Index_List = libghdl.vhdl__nodes__set_index_list

Get_Reference = libghdl.vhdl__nodes__get_reference

Set_Reference = libghdl.vhdl__nodes__set_reference

Get_Nature_Declarator = libghdl.vhdl__nodes__get_nature_declarator

Set_Nature_Declarator = libghdl.vhdl__nodes__set_nature_declarator

Get_Across_Type_Mark = libghdl.vhdl__nodes__get_across_type_mark

Set_Across_Type_Mark = libghdl.vhdl__nodes__set_across_type_mark

Get_Through_Type_Mark = libghdl.vhdl__nodes__get_through_type_mark

Set_Through_Type_Mark = libghdl.vhdl__nodes__set_through_type_mark

Get_Across_Type_Definition = libghdl.vhdl__nodes__get_across_type_definition

Set_Across_Type_Definition = libghdl.vhdl__nodes__set_across_type_definition

Get_Through_Type_Definition = libghdl.vhdl__nodes__get_through_type_definition

Set_Through_Type_Definition = libghdl.vhdl__nodes__set_through_type_definition

Get_Across_Type = libghdl.vhdl__nodes__get_across_type

Set_Across_Type = libghdl.vhdl__nodes__set_across_type

Get_Through_Type = libghdl.vhdl__nodes__get_through_type

Set_Through_Type = libghdl.vhdl__nodes__set_through_type

Get_Target = libghdl.vhdl__nodes__get_target

Set_Target = libghdl.vhdl__nodes__set_target

Get_Waveform_Chain = libghdl.vhdl__nodes__get_waveform_chain

Set_Waveform_Chain = libghdl.vhdl__nodes__set_waveform_chain

Get_Guard = libghdl.vhdl__nodes__get_guard

Set_Guard = libghdl.vhdl__nodes__set_guard

Get_Delay_Mechanism = libghdl.vhdl__nodes__get_delay_mechanism

Set_Delay_Mechanism = libghdl.vhdl__nodes__set_delay_mechanism

Get_Reject_Time_Expression = libghdl.vhdl__nodes__get_reject_time_expression

Set_Reject_Time_Expression = libghdl.vhdl__nodes__set_reject_time_expression

Get_Force_Mode = libghdl.vhdl__nodes__get_force_mode

Set_Force_Mode = libghdl.vhdl__nodes__set_force_mode

Get_Has_Force_Mode = libghdl.vhdl__nodes__get_has_force_mode

Set_Has_Force_Mode = libghdl.vhdl__nodes__set_has_force_mode

Get_Sensitivity_List = libghdl.vhdl__nodes__get_sensitivity_list

Set_Sensitivity_List = libghdl.vhdl__nodes__set_sensitivity_list

Get_Process_Origin = libghdl.vhdl__nodes__get_process_origin

Set_Process_Origin = libghdl.vhdl__nodes__set_process_origin

Get_Package_Origin = libghdl.vhdl__nodes__get_package_origin

Set_Package_Origin = libghdl.vhdl__nodes__set_package_origin

Get_Condition_Clause = libghdl.vhdl__nodes__get_condition_clause

Set_Condition_Clause = libghdl.vhdl__nodes__set_condition_clause

Get_Break_Element = libghdl.vhdl__nodes__get_break_element

Set_Break_Element = libghdl.vhdl__nodes__set_break_element

Get_Selector_Quantity = libghdl.vhdl__nodes__get_selector_quantity

Set_Selector_Quantity = libghdl.vhdl__nodes__set_selector_quantity

Get_Break_Quantity = libghdl.vhdl__nodes__get_break_quantity

Set_Break_Quantity = libghdl.vhdl__nodes__set_break_quantity

Get_Timeout_Clause = libghdl.vhdl__nodes__get_timeout_clause

Set_Timeout_Clause = libghdl.vhdl__nodes__set_timeout_clause

Get_Postponed_Flag = libghdl.vhdl__nodes__get_postponed_flag

Set_Postponed_Flag = libghdl.vhdl__nodes__set_postponed_flag

Get_Callees_List = libghdl.vhdl__nodes__get_callees_list

Set_Callees_List = libghdl.vhdl__nodes__set_callees_list

Get_Passive_Flag = libghdl.vhdl__nodes__get_passive_flag

Set_Passive_Flag = libghdl.vhdl__nodes__set_passive_flag

Get_Resolution_Function_Flag = libghdl.vhdl__nodes__get_resolution_function_flag

Set_Resolution_Function_Flag = libghdl.vhdl__nodes__set_resolution_function_flag

Get_Wait_State = libghdl.vhdl__nodes__get_wait_state

Set_Wait_State = libghdl.vhdl__nodes__set_wait_state

Get_All_Sensitized_State = libghdl.vhdl__nodes__get_all_sensitized_state

Set_All_Sensitized_State = libghdl.vhdl__nodes__set_all_sensitized_state

Get_Seen_Flag = libghdl.vhdl__nodes__get_seen_flag

Set_Seen_Flag = libghdl.vhdl__nodes__set_seen_flag

Get_Pure_Flag = libghdl.vhdl__nodes__get_pure_flag

Set_Pure_Flag = libghdl.vhdl__nodes__set_pure_flag

Get_Foreign_Flag = libghdl.vhdl__nodes__get_foreign_flag

Set_Foreign_Flag = libghdl.vhdl__nodes__set_foreign_flag

Get_Resolved_Flag = libghdl.vhdl__nodes__get_resolved_flag

Set_Resolved_Flag = libghdl.vhdl__nodes__set_resolved_flag

Get_Signal_Type_Flag = libghdl.vhdl__nodes__get_signal_type_flag

Set_Signal_Type_Flag = libghdl.vhdl__nodes__set_signal_type_flag

Get_Has_Signal_Flag = libghdl.vhdl__nodes__get_has_signal_flag

Set_Has_Signal_Flag = libghdl.vhdl__nodes__set_has_signal_flag

Get_Purity_State = libghdl.vhdl__nodes__get_purity_state

Set_Purity_State = libghdl.vhdl__nodes__set_purity_state

Get_Elab_Flag = libghdl.vhdl__nodes__get_elab_flag

Set_Elab_Flag = libghdl.vhdl__nodes__set_elab_flag

Get_Vendor_Library_Flag = libghdl.vhdl__nodes__get_vendor_library_flag

Set_Vendor_Library_Flag = libghdl.vhdl__nodes__set_vendor_library_flag

Get_Configuration_Mark_Flag = libghdl.vhdl__nodes__get_configuration_mark_flag

Set_Configuration_Mark_Flag = libghdl.vhdl__nodes__set_configuration_mark_flag

Get_Configuration_Done_Flag = libghdl.vhdl__nodes__get_configuration_done_flag

Set_Configuration_Done_Flag = libghdl.vhdl__nodes__set_configuration_done_flag

Get_Index_Constraint_Flag = libghdl.vhdl__nodes__get_index_constraint_flag

Set_Index_Constraint_Flag = libghdl.vhdl__nodes__set_index_constraint_flag

Get_Hide_Implicit_Flag = libghdl.vhdl__nodes__get_hide_implicit_flag

Set_Hide_Implicit_Flag = libghdl.vhdl__nodes__set_hide_implicit_flag

Get_Assertion_Condition = libghdl.vhdl__nodes__get_assertion_condition

Set_Assertion_Condition = libghdl.vhdl__nodes__set_assertion_condition

Get_Report_Expression = libghdl.vhdl__nodes__get_report_expression

Set_Report_Expression = libghdl.vhdl__nodes__set_report_expression

Get_Severity_Expression = libghdl.vhdl__nodes__get_severity_expression

Set_Severity_Expression = libghdl.vhdl__nodes__set_severity_expression

Get_Instantiated_Unit = libghdl.vhdl__nodes__get_instantiated_unit

Set_Instantiated_Unit = libghdl.vhdl__nodes__set_instantiated_unit

Get_Generic_Map_Aspect_Chain = libghdl.vhdl__nodes__get_generic_map_aspect_chain

Set_Generic_Map_Aspect_Chain = libghdl.vhdl__nodes__set_generic_map_aspect_chain

Get_Port_Map_Aspect_Chain = libghdl.vhdl__nodes__get_port_map_aspect_chain

Set_Port_Map_Aspect_Chain = libghdl.vhdl__nodes__set_port_map_aspect_chain

Get_Configuration_Name = libghdl.vhdl__nodes__get_configuration_name

Set_Configuration_Name = libghdl.vhdl__nodes__set_configuration_name

Get_Component_Configuration = libghdl.vhdl__nodes__get_component_configuration

Set_Component_Configuration = libghdl.vhdl__nodes__set_component_configuration

Get_Configuration_Specification = libghdl.vhdl__nodes__get_configuration_specification

Set_Configuration_Specification = libghdl.vhdl__nodes__set_configuration_specification

Get_Default_Binding_Indication = libghdl.vhdl__nodes__get_default_binding_indication

Set_Default_Binding_Indication = libghdl.vhdl__nodes__set_default_binding_indication

Get_Default_Configuration_Declaration = (
    libghdl.vhdl__nodes__get_default_configuration_declaration
)

Set_Default_Configuration_Declaration = (
    libghdl.vhdl__nodes__set_default_configuration_declaration
)

Get_Expression = libghdl.vhdl__nodes__get_expression

Set_Expression = libghdl.vhdl__nodes__set_expression

Get_Conditional_Expression_Chain = libghdl.vhdl__nodes__get_conditional_expression_chain

Set_Conditional_Expression_Chain = libghdl.vhdl__nodes__set_conditional_expression_chain

Get_Allocator_Designated_Type = libghdl.vhdl__nodes__get_allocator_designated_type

Set_Allocator_Designated_Type = libghdl.vhdl__nodes__set_allocator_designated_type

Get_Selected_Waveform_Chain = libghdl.vhdl__nodes__get_selected_waveform_chain

Set_Selected_Waveform_Chain = libghdl.vhdl__nodes__set_selected_waveform_chain

Get_Conditional_Waveform_Chain = libghdl.vhdl__nodes__get_conditional_waveform_chain

Set_Conditional_Waveform_Chain = libghdl.vhdl__nodes__set_conditional_waveform_chain

Get_Guard_Expression = libghdl.vhdl__nodes__get_guard_expression

Set_Guard_Expression = libghdl.vhdl__nodes__set_guard_expression

Get_Guard_Decl = libghdl.vhdl__nodes__get_guard_decl

Set_Guard_Decl = libghdl.vhdl__nodes__set_guard_decl

Get_Guard_Sensitivity_List = libghdl.vhdl__nodes__get_guard_sensitivity_list

Set_Guard_Sensitivity_List = libghdl.vhdl__nodes__set_guard_sensitivity_list

Get_Signal_Attribute_Chain = libghdl.vhdl__nodes__get_signal_attribute_chain

Set_Signal_Attribute_Chain = libghdl.vhdl__nodes__set_signal_attribute_chain

Get_Block_Block_Configuration = libghdl.vhdl__nodes__get_block_block_configuration

Set_Block_Block_Configuration = libghdl.vhdl__nodes__set_block_block_configuration

Get_Package_Header = libghdl.vhdl__nodes__get_package_header

Set_Package_Header = libghdl.vhdl__nodes__set_package_header

Get_Block_Header = libghdl.vhdl__nodes__get_block_header

Set_Block_Header = libghdl.vhdl__nodes__set_block_header

Get_Uninstantiated_Package_Name = libghdl.vhdl__nodes__get_uninstantiated_package_name

Set_Uninstantiated_Package_Name = libghdl.vhdl__nodes__set_uninstantiated_package_name

Get_Uninstantiated_Package_Decl = libghdl.vhdl__nodes__get_uninstantiated_package_decl

Set_Uninstantiated_Package_Decl = libghdl.vhdl__nodes__set_uninstantiated_package_decl

Get_Instance_Source_File = libghdl.vhdl__nodes__get_instance_source_file

Set_Instance_Source_File = libghdl.vhdl__nodes__set_instance_source_file

Get_Generate_Block_Configuration = libghdl.vhdl__nodes__get_generate_block_configuration

Set_Generate_Block_Configuration = libghdl.vhdl__nodes__set_generate_block_configuration

Get_Generate_Statement_Body = libghdl.vhdl__nodes__get_generate_statement_body

Set_Generate_Statement_Body = libghdl.vhdl__nodes__set_generate_statement_body

Get_Alternative_Label = libghdl.vhdl__nodes__get_alternative_label

Set_Alternative_Label = libghdl.vhdl__nodes__set_alternative_label

Get_Generate_Else_Clause = libghdl.vhdl__nodes__get_generate_else_clause

Set_Generate_Else_Clause = libghdl.vhdl__nodes__set_generate_else_clause

Get_Condition = libghdl.vhdl__nodes__get_condition

Set_Condition = libghdl.vhdl__nodes__set_condition

Get_Else_Clause = libghdl.vhdl__nodes__get_else_clause

Set_Else_Clause = libghdl.vhdl__nodes__set_else_clause

Get_Parameter_Specification = libghdl.vhdl__nodes__get_parameter_specification

Set_Parameter_Specification = libghdl.vhdl__nodes__set_parameter_specification

Get_Parent = libghdl.vhdl__nodes__get_parent

Set_Parent = libghdl.vhdl__nodes__set_parent

Get_Loop_Label = libghdl.vhdl__nodes__get_loop_label

Set_Loop_Label = libghdl.vhdl__nodes__set_loop_label

Get_Exit_Flag = libghdl.vhdl__nodes__get_exit_flag

Set_Exit_Flag = libghdl.vhdl__nodes__set_exit_flag

Get_Next_Flag = libghdl.vhdl__nodes__get_next_flag

Set_Next_Flag = libghdl.vhdl__nodes__set_next_flag

Get_Component_Name = libghdl.vhdl__nodes__get_component_name

Set_Component_Name = libghdl.vhdl__nodes__set_component_name

Get_Instantiation_List = libghdl.vhdl__nodes__get_instantiation_list

Set_Instantiation_List = libghdl.vhdl__nodes__set_instantiation_list

Get_Entity_Aspect = libghdl.vhdl__nodes__get_entity_aspect

Set_Entity_Aspect = libghdl.vhdl__nodes__set_entity_aspect

Get_Default_Entity_Aspect = libghdl.vhdl__nodes__get_default_entity_aspect

Set_Default_Entity_Aspect = libghdl.vhdl__nodes__set_default_entity_aspect

Get_Binding_Indication = libghdl.vhdl__nodes__get_binding_indication

Set_Binding_Indication = libghdl.vhdl__nodes__set_binding_indication

Get_Named_Entity = libghdl.vhdl__nodes__get_named_entity

Set_Named_Entity = libghdl.vhdl__nodes__set_named_entity

Get_Alias_Declaration = libghdl.vhdl__nodes__get_alias_declaration

Set_Alias_Declaration = libghdl.vhdl__nodes__set_alias_declaration

Get_Referenced_Name = libghdl.vhdl__nodes__get_referenced_name

Set_Referenced_Name = libghdl.vhdl__nodes__set_referenced_name

Get_Expr_Staticness = libghdl.vhdl__nodes__get_expr_staticness

Set_Expr_Staticness = libghdl.vhdl__nodes__set_expr_staticness

Get_Scalar_Size = libghdl.vhdl__nodes__get_scalar_size

Set_Scalar_Size = libghdl.vhdl__nodes__set_scalar_size

Get_Error_Origin = libghdl.vhdl__nodes__get_error_origin

Set_Error_Origin = libghdl.vhdl__nodes__set_error_origin

Get_Operand = libghdl.vhdl__nodes__get_operand

Set_Operand = libghdl.vhdl__nodes__set_operand

Get_Left = libghdl.vhdl__nodes__get_left

Set_Left = libghdl.vhdl__nodes__set_left

Get_Right = libghdl.vhdl__nodes__get_right

Set_Right = libghdl.vhdl__nodes__set_right

Get_Unit_Name = libghdl.vhdl__nodes__get_unit_name

Set_Unit_Name = libghdl.vhdl__nodes__set_unit_name

Get_Name = libghdl.vhdl__nodes__get_name

Set_Name = libghdl.vhdl__nodes__set_name

Get_Group_Template_Name = libghdl.vhdl__nodes__get_group_template_name

Set_Group_Template_Name = libghdl.vhdl__nodes__set_group_template_name

Get_Name_Staticness = libghdl.vhdl__nodes__get_name_staticness

Set_Name_Staticness = libghdl.vhdl__nodes__set_name_staticness

Get_Prefix = libghdl.vhdl__nodes__get_prefix

Set_Prefix = libghdl.vhdl__nodes__set_prefix

Get_Signature_Prefix = libghdl.vhdl__nodes__get_signature_prefix

Set_Signature_Prefix = libghdl.vhdl__nodes__set_signature_prefix

Get_External_Pathname = libghdl.vhdl__nodes__get_external_pathname

Set_External_Pathname = libghdl.vhdl__nodes__set_external_pathname

Get_Pathname_Suffix = libghdl.vhdl__nodes__get_pathname_suffix

Set_Pathname_Suffix = libghdl.vhdl__nodes__set_pathname_suffix

Get_Pathname_Expression = libghdl.vhdl__nodes__get_pathname_expression

Set_Pathname_Expression = libghdl.vhdl__nodes__set_pathname_expression

Get_In_Formal_Flag = libghdl.vhdl__nodes__get_in_formal_flag

Set_In_Formal_Flag = libghdl.vhdl__nodes__set_in_formal_flag

Get_Slice_Subtype = libghdl.vhdl__nodes__get_slice_subtype

Set_Slice_Subtype = libghdl.vhdl__nodes__set_slice_subtype

Get_Suffix = libghdl.vhdl__nodes__get_suffix

Set_Suffix = libghdl.vhdl__nodes__set_suffix

Get_Index_Subtype = libghdl.vhdl__nodes__get_index_subtype

Set_Index_Subtype = libghdl.vhdl__nodes__set_index_subtype

Get_Parameter = libghdl.vhdl__nodes__get_parameter

Set_Parameter = libghdl.vhdl__nodes__set_parameter

Get_Parameter_2 = libghdl.vhdl__nodes__get_parameter_2

Set_Parameter_2 = libghdl.vhdl__nodes__set_parameter_2

Get_Parameter_3 = libghdl.vhdl__nodes__get_parameter_3

Set_Parameter_3 = libghdl.vhdl__nodes__set_parameter_3

Get_Parameter_4 = libghdl.vhdl__nodes__get_parameter_4

Set_Parameter_4 = libghdl.vhdl__nodes__set_parameter_4

Get_Attr_Chain = libghdl.vhdl__nodes__get_attr_chain

Set_Attr_Chain = libghdl.vhdl__nodes__set_attr_chain

Get_Signal_Attribute_Declaration = libghdl.vhdl__nodes__get_signal_attribute_declaration

Set_Signal_Attribute_Declaration = libghdl.vhdl__nodes__set_signal_attribute_declaration

Get_Actual_Type = libghdl.vhdl__nodes__get_actual_type

Set_Actual_Type = libghdl.vhdl__nodes__set_actual_type

Get_Actual_Type_Definition = libghdl.vhdl__nodes__get_actual_type_definition

Set_Actual_Type_Definition = libghdl.vhdl__nodes__set_actual_type_definition

Get_Association_Chain = libghdl.vhdl__nodes__get_association_chain

Set_Association_Chain = libghdl.vhdl__nodes__set_association_chain

Get_Individual_Association_Chain = libghdl.vhdl__nodes__get_individual_association_chain

Set_Individual_Association_Chain = libghdl.vhdl__nodes__set_individual_association_chain

Get_Subprogram_Association_Chain = libghdl.vhdl__nodes__get_subprogram_association_chain

Set_Subprogram_Association_Chain = libghdl.vhdl__nodes__set_subprogram_association_chain

Get_Aggregate_Info = libghdl.vhdl__nodes__get_aggregate_info

Set_Aggregate_Info = libghdl.vhdl__nodes__set_aggregate_info

Get_Sub_Aggregate_Info = libghdl.vhdl__nodes__get_sub_aggregate_info

Set_Sub_Aggregate_Info = libghdl.vhdl__nodes__set_sub_aggregate_info

Get_Aggr_Dynamic_Flag = libghdl.vhdl__nodes__get_aggr_dynamic_flag

Set_Aggr_Dynamic_Flag = libghdl.vhdl__nodes__set_aggr_dynamic_flag

Get_Aggr_Min_Length = libghdl.vhdl__nodes__get_aggr_min_length

Set_Aggr_Min_Length = libghdl.vhdl__nodes__set_aggr_min_length

Get_Aggr_Low_Limit = libghdl.vhdl__nodes__get_aggr_low_limit

Set_Aggr_Low_Limit = libghdl.vhdl__nodes__set_aggr_low_limit

Get_Aggr_High_Limit = libghdl.vhdl__nodes__get_aggr_high_limit

Set_Aggr_High_Limit = libghdl.vhdl__nodes__set_aggr_high_limit

Get_Aggr_Others_Flag = libghdl.vhdl__nodes__get_aggr_others_flag

Set_Aggr_Others_Flag = libghdl.vhdl__nodes__set_aggr_others_flag

Get_Aggr_Named_Flag = libghdl.vhdl__nodes__get_aggr_named_flag

Set_Aggr_Named_Flag = libghdl.vhdl__nodes__set_aggr_named_flag

Get_Aggregate_Expand_Flag = libghdl.vhdl__nodes__get_aggregate_expand_flag

Set_Aggregate_Expand_Flag = libghdl.vhdl__nodes__set_aggregate_expand_flag

Get_Association_Choices_Chain = libghdl.vhdl__nodes__get_association_choices_chain

Set_Association_Choices_Chain = libghdl.vhdl__nodes__set_association_choices_chain

Get_Case_Statement_Alternative_Chain = (
    libghdl.vhdl__nodes__get_case_statement_alternative_chain
)

Set_Case_Statement_Alternative_Chain = (
    libghdl.vhdl__nodes__set_case_statement_alternative_chain
)

Get_Choice_Staticness = libghdl.vhdl__nodes__get_choice_staticness

Set_Choice_Staticness = libghdl.vhdl__nodes__set_choice_staticness

Get_Procedure_Call = libghdl.vhdl__nodes__get_procedure_call

Set_Procedure_Call = libghdl.vhdl__nodes__set_procedure_call

Get_Implementation = libghdl.vhdl__nodes__get_implementation

Set_Implementation = libghdl.vhdl__nodes__set_implementation

Get_Parameter_Association_Chain = libghdl.vhdl__nodes__get_parameter_association_chain

Set_Parameter_Association_Chain = libghdl.vhdl__nodes__set_parameter_association_chain

Get_Method_Object = libghdl.vhdl__nodes__get_method_object

Set_Method_Object = libghdl.vhdl__nodes__set_method_object

Get_Subtype_Type_Mark = libghdl.vhdl__nodes__get_subtype_type_mark

Set_Subtype_Type_Mark = libghdl.vhdl__nodes__set_subtype_type_mark

Get_Subnature_Nature_Mark = libghdl.vhdl__nodes__get_subnature_nature_mark

Set_Subnature_Nature_Mark = libghdl.vhdl__nodes__set_subnature_nature_mark

Get_Type_Conversion_Subtype = libghdl.vhdl__nodes__get_type_conversion_subtype

Set_Type_Conversion_Subtype = libghdl.vhdl__nodes__set_type_conversion_subtype

Get_Type_Mark = libghdl.vhdl__nodes__get_type_mark

Set_Type_Mark = libghdl.vhdl__nodes__set_type_mark

Get_File_Type_Mark = libghdl.vhdl__nodes__get_file_type_mark

Set_File_Type_Mark = libghdl.vhdl__nodes__set_file_type_mark

Get_Return_Type_Mark = libghdl.vhdl__nodes__get_return_type_mark

Set_Return_Type_Mark = libghdl.vhdl__nodes__set_return_type_mark

Get_Has_Disconnect_Flag = libghdl.vhdl__nodes__get_has_disconnect_flag

Set_Has_Disconnect_Flag = libghdl.vhdl__nodes__set_has_disconnect_flag

Get_Has_Active_Flag = libghdl.vhdl__nodes__get_has_active_flag

Set_Has_Active_Flag = libghdl.vhdl__nodes__set_has_active_flag

Get_Is_Within_Flag = libghdl.vhdl__nodes__get_is_within_flag

Set_Is_Within_Flag = libghdl.vhdl__nodes__set_is_within_flag

Get_Type_Marks_List = libghdl.vhdl__nodes__get_type_marks_list

Set_Type_Marks_List = libghdl.vhdl__nodes__set_type_marks_list

Get_Implicit_Alias_Flag = libghdl.vhdl__nodes__get_implicit_alias_flag

Set_Implicit_Alias_Flag = libghdl.vhdl__nodes__set_implicit_alias_flag

Get_Alias_Signature = libghdl.vhdl__nodes__get_alias_signature

Set_Alias_Signature = libghdl.vhdl__nodes__set_alias_signature

Get_Attribute_Signature = libghdl.vhdl__nodes__get_attribute_signature

Set_Attribute_Signature = libghdl.vhdl__nodes__set_attribute_signature

Get_Overload_List = libghdl.vhdl__nodes__get_overload_list

Set_Overload_List = libghdl.vhdl__nodes__set_overload_list

Get_Simple_Name_Identifier = libghdl.vhdl__nodes__get_simple_name_identifier

Set_Simple_Name_Identifier = libghdl.vhdl__nodes__set_simple_name_identifier

Get_Simple_Name_Subtype = libghdl.vhdl__nodes__get_simple_name_subtype

Set_Simple_Name_Subtype = libghdl.vhdl__nodes__set_simple_name_subtype

Get_Protected_Type_Body = libghdl.vhdl__nodes__get_protected_type_body

Set_Protected_Type_Body = libghdl.vhdl__nodes__set_protected_type_body

Get_Protected_Type_Declaration = libghdl.vhdl__nodes__get_protected_type_declaration

Set_Protected_Type_Declaration = libghdl.vhdl__nodes__set_protected_type_declaration

Get_Use_Flag = libghdl.vhdl__nodes__get_use_flag

Set_Use_Flag = libghdl.vhdl__nodes__set_use_flag

Get_End_Has_Reserved_Id = libghdl.vhdl__nodes__get_end_has_reserved_id

Set_End_Has_Reserved_Id = libghdl.vhdl__nodes__set_end_has_reserved_id

Get_End_Has_Identifier = libghdl.vhdl__nodes__get_end_has_identifier

Set_End_Has_Identifier = libghdl.vhdl__nodes__set_end_has_identifier

Get_End_Has_Postponed = libghdl.vhdl__nodes__get_end_has_postponed

Set_End_Has_Postponed = libghdl.vhdl__nodes__set_end_has_postponed

Get_Has_Label = libghdl.vhdl__nodes__get_has_label

Set_Has_Label = libghdl.vhdl__nodes__set_has_label

Get_Has_Begin = libghdl.vhdl__nodes__get_has_begin

Set_Has_Begin = libghdl.vhdl__nodes__set_has_begin

Get_Has_End = libghdl.vhdl__nodes__get_has_end

Set_Has_End = libghdl.vhdl__nodes__set_has_end

Get_Has_Is = libghdl.vhdl__nodes__get_has_is

Set_Has_Is = libghdl.vhdl__nodes__set_has_is

Get_Has_Pure = libghdl.vhdl__nodes__get_has_pure

Set_Has_Pure = libghdl.vhdl__nodes__set_has_pure

Get_Has_Body = libghdl.vhdl__nodes__get_has_body

Set_Has_Body = libghdl.vhdl__nodes__set_has_body

Get_Has_Parameter = libghdl.vhdl__nodes__get_has_parameter

Set_Has_Parameter = libghdl.vhdl__nodes__set_has_parameter

Get_Has_Component = libghdl.vhdl__nodes__get_has_component

Set_Has_Component = libghdl.vhdl__nodes__set_has_component

Get_Has_Identifier_List = libghdl.vhdl__nodes__get_has_identifier_list

Set_Has_Identifier_List = libghdl.vhdl__nodes__set_has_identifier_list

Get_Has_Mode = libghdl.vhdl__nodes__get_has_mode

Set_Has_Mode = libghdl.vhdl__nodes__set_has_mode

Get_Has_Class = libghdl.vhdl__nodes__get_has_class

Set_Has_Class = libghdl.vhdl__nodes__set_has_class

Get_Has_Delay_Mechanism = libghdl.vhdl__nodes__get_has_delay_mechanism

Set_Has_Delay_Mechanism = libghdl.vhdl__nodes__set_has_delay_mechanism

Get_Suspend_Flag = libghdl.vhdl__nodes__get_suspend_flag

Set_Suspend_Flag = libghdl.vhdl__nodes__set_suspend_flag

Get_Is_Ref = libghdl.vhdl__nodes__get_is_ref

Set_Is_Ref = libghdl.vhdl__nodes__set_is_ref

Get_Is_Forward_Ref = libghdl.vhdl__nodes__get_is_forward_ref

Set_Is_Forward_Ref = libghdl.vhdl__nodes__set_is_forward_ref

Get_Psl_Property = libghdl.vhdl__nodes__get_psl_property

Set_Psl_Property = libghdl.vhdl__nodes__set_psl_property

Get_Psl_Sequence = libghdl.vhdl__nodes__get_psl_sequence

Set_Psl_Sequence = libghdl.vhdl__nodes__set_psl_sequence

Get_Psl_Declaration = libghdl.vhdl__nodes__get_psl_declaration

Set_Psl_Declaration = libghdl.vhdl__nodes__set_psl_declaration

Get_Psl_Expression = libghdl.vhdl__nodes__get_psl_expression

Set_Psl_Expression = libghdl.vhdl__nodes__set_psl_expression

Get_Psl_Boolean = libghdl.vhdl__nodes__get_psl_boolean

Set_Psl_Boolean = libghdl.vhdl__nodes__set_psl_boolean

Get_PSL_Clock = libghdl.vhdl__nodes__get_psl_clock

Set_PSL_Clock = libghdl.vhdl__nodes__set_psl_clock

Get_PSL_NFA = libghdl.vhdl__nodes__get_psl_nfa

Set_PSL_NFA = libghdl.vhdl__nodes__set_psl_nfa

Get_PSL_Nbr_States = libghdl.vhdl__nodes__get_psl_nbr_states

Set_PSL_Nbr_States = libghdl.vhdl__nodes__set_psl_nbr_states

Get_PSL_Clock_Sensitivity = libghdl.vhdl__nodes__get_psl_clock_sensitivity

Set_PSL_Clock_Sensitivity = libghdl.vhdl__nodes__set_psl_clock_sensitivity

Get_PSL_EOS_Flag = libghdl.vhdl__nodes__get_psl_eos_flag

Set_PSL_EOS_Flag = libghdl.vhdl__nodes__set_psl_eos_flag

Get_Count_Expression = libghdl.vhdl__nodes__get_count_expression

Set_Count_Expression = libghdl.vhdl__nodes__set_count_expression

Get_Clock_Expression = libghdl.vhdl__nodes__get_clock_expression

Set_Clock_Expression = libghdl.vhdl__nodes__set_clock_expression

Get_Default_Clock = libghdl.vhdl__nodes__get_default_clock

Set_Default_Clock = libghdl.vhdl__nodes__set_default_clock
