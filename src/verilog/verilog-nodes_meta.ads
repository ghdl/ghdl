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

with Types; use Types;
with Verilog.Types; use Verilog.Types;
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Nodes_Meta is
   --  The enumeration of all possible types in the nodes.
   type Types_Enum is
     (
      Type_Base_Type,
      Type_Binary_Ops,
      Type_Bn_Index,
      Type_Boolean,
      Type_Conv_Ops,
      Type_DPI_Spec_Type,
      Type_Edge_Type,
      Type_Fp64,
      Type_Int32,
      Type_Join_Type,
      Type_Lifetime_Type,
      Type_Lit_Id,
      Type_Name_Id,
      Type_Node,
      Type_Obj_Id,
      Type_Polarity_Type,
      Type_Proc_Id,
      Type_Scope_Id,
      Type_String8_Id,
      Type_Sys_Tf_Id,
      Type_Tsize_Type,
      Type_Udp_Kind,
      Type_Udp_Symbol,
      Type_Unary_Ops,
      Type_Uns32,
      Type_Violation_Type,
      Type_Visibility_Type,
      Type_Width_Type
     );

   --  The enumeration of all fields defined in iirs.
   type Fields_Enum is
     (
      Field_Parent,
      Field_Call_Scope,
      Field_Identifier,
      Field_C_Identifier,
      Field_Ports_Chain,
      Field_Tf_Ports_Chain,
      Field_Package_Import_Chain,
      Field_Parameter_Port_Chain,
      Field_Parameter,
      Field_Foreign_Node,
      Field_Descriptions,
      Field_Class_Item_Chain,
      Field_Package_Item_Chain,
      Field_Items_Chain,
      Field_Clocking_Item_Chain,
      Field_Tf_Item_Declaration_Chain,
      Field_Block_Item_Declaration_Chain,
      Field_Generate_Item_Chain,
      Field_Specify_Item_Chain,
      Field_Statements_Chain,
      Field_Modport_Ports_Chain,
      Field_Chain,
      Field_Constraint_Block_Chain,
      Field_Constraint_Set,
      Field_OOB_Prefix,
      Field_Out_Of_Block_Declaration,
      Field_Generate_Index,
      Field_Return_Variable,
      Field_Return_Variable_Ref,
      Field_This_Variable,
      Field_Expression,
      Field_Reject_Limit,
      Field_Error_Limit,
      Field_Sequence,
      Field_Init_Expression,
      Field_Size_Expression,
      Field_Override_Stmt,
      Field_Parameter_Expression,
      Field_Parameter_Type,
      Field_Value_Range,
      Field_Lsb_Include_Flag,
      Field_Msb_Include_Flag,
      Field_Range,
      Field_Msb,
      Field_Lsb,
      Field_Msb_Cst,
      Field_Lsb_Cst,
      Field_Base_Expr,
      Field_Width_Expr,
      Field_Width_Cst,
      Field_Type_Width,
      Field_Type_Size,
      Field_Stride_Width,
      Field_Stride_Size,
      Field_Type_Hash,
      Field_Maximum_Size_Expr,
      Field_Maximum_Size_Cst,
      Field_Lvalue,
      Field_Name,
      Field_Item_Name,
      Field_Pattern_Key,
      Field_Left,
      Field_Right,
      Field_Repeat_Expression,
      Field_Op_Attributes,
      Field_Attributes_Chain,
      Field_Condition,
      Field_Cond_True,
      Field_Cond_False,
      Field_True_Stmt,
      Field_False_Stmt,
      Field_Pass_Stmt,
      Field_Else_Stmt,
      Field_Clocking_Event,
      Field_Disable_Expression,
      Field_Property_Expression,
      Field_True_Block,
      Field_False_Block,
      Field_Statement,
      Field_Foreach_Array,
      Field_Foreach_Variables,
      Field_Control,
      Field_Replication,
      Field_Replication_Cst,
      Field_Expressions,
      Field_Elements,
      Field_Slice_Size_Expr,
      Field_Slice_Size_Type,
      Field_Members,
      Field_Nbr_Members,
      Field_Member_Index,
      Field_Packed_Member_Offset,
      Field_Nature_Items,
      Field_Discipline_Items,
      Field_Continuous_Flag,
      Field_Potential_Flag,
      Field_Nature,
      Field_Connections,
      Field_Gate_Terminals,
      Field_Parameter_Values,
      Field_Case_Items,
      Field_Delay,
      Field_Net_Delay,
      Field_Gate_Delay,
      Field_Assign_Delay,
      Field_Rising_Delay,
      Field_Falling_Delay,
      Field_Highz_Delay,
      Field_For_Initialization,
      Field_Step_Assign,
      Field_Arguments,
      Field_Iterator_Argument,
      Field_Task,
      Field_Signed_Flag,
      Field_Scope_Flag,
      Field_Number_Base,
      Field_Number_Hi_Val,
      Field_Number_Lo_Val,
      Field_Number_Hi_Zx,
      Field_Number_Lo_Zx,
      Field_Number_Size,
      Field_Expr_Origin,
      Field_Bignum_Index,
      Field_Bignum_Len,
      Field_Real_Number,
      Field_Time_Unit,
      Field_Scale_Factor,
      Field_Time_Precision,
      Field_Timescale,
      Field_String_Size,
      Field_Data_Type,
      Field_Expr_Type,
      Field_Param_Type,
      Field_Element_Data_Type,
      Field_Type_Element_Type,
      Field_Cast_Data_Type,
      Field_Base_Class_Type,
      Field_Class_Constructor,
      Field_Inheritance_Depth,
      Field_Enum_Base_Data_Type,
      Field_Enum_Base_Type,
      Field_Packed_Base_Type,
      Field_Default_Type,
      Field_Type_Owner,
      Field_Type_Owner_2,
      Field_Forward_Type,
      Field_Enum_Names,
      Field_Index_Data_Type,
      Field_Type_Index_Type,
      Field_Type_Argument,
      Field_Type_Signed,
      Field_Subroutine,
      Field_Object,
      Field_With_Expression,
      Field_Drive_Strength,
      Field_Net_Drive_Strength,
      Field_Charge_Strength,
      Field_Module,
      Field_Class_Name,
      Field_Interface,
      Field_Interface_Name,
      Field_Instance,
      Field_Instance_Ref,
      Field_Port,
      Field_Collapse_Flag,
      Field_Unary_Op,
      Field_Binary_Op,
      Field_Conversion_Op,
      Field_Declaration,
      Field_Redeclaration,
      Field_This_Declaration,
      Field_Default_Value,
      Field_Instantiated_Flag,
      Field_Ansi_Port_Flag,
      Field_Event,
      Field_Min_Expr,
      Field_Typ_Expr,
      Field_Max_Expr,
      Field_Udp_Port_Declaration_Chain,
      Field_Udp_Kind,
      Field_Udp_Initial,
      Field_Udp_Entries_Chain,
      Field_Input_Chain,
      Field_Output_Symbol,
      Field_Current_State,
      Field_Next_State,
      Field_Symbol,
      Field_From_Symbol,
      Field_To_Symbol,
      Field_Specify_Input,
      Field_Specify_Output,
      Field_Path_Delay,
      Field_Data_Source,
      Field_Polarity,
      Field_Delay_Rise,
      Field_Delay_Fall,
      Field_Delay_Z,
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
      Field_String_Id,
      Field_Label,
      Field_Label_Number,
      Field_Label_Chain,
      Field_Label_Use,
      Field_Suspend_Flag,
      Field_Same_Case_Flag,
      Field_Obj_Id,
      Field_Scope_Id,
      Field_Process_Id,
      Field_Sys_Tf_Id,
      Field_Lit_Id,
      Field_Generate_Block,
      Field_Input_Skew,
      Field_Output_Skew,
      Field_Delay_Control,
      Field_Attribute_Item,
      Field_Has_Identifier_List,
      Field_Has_Sign,
      Field_Connected_Flag,
      Field_Complete_Flag,
      Field_Implicit_Flag,
      Field_Redeclaration_Flag,
      Field_Is_Automatic,
      Field_Lifetime,
      Field_Has_Lifetime,
      Field_Has_End_Name,
      Field_Call,
      Field_Timeunit,
      Field_Timeprecision,
      Field_Error_Origin,
      Field_Has_Void_Cast,
      Field_Is_Const,
      Field_Has_Var,
      Field_Has_Type,
      Field_Has_Direction,
      Field_Has_Parenthesis,
      Field_Has_Argument,
      Field_Fully_Analyzed_Flag,
      Field_Resolved_Flag,
      Field_Mark_Flag,
      Field_Is_Constant,
      Field_Static_Flag,
      Field_Has_Attribute,
      Field_Attribute_Full,
      Field_Attribute_Parallel,
      Field_Other_Attributes,
      Field_Pure_Property,
      Field_Context_Property,
      Field_Has_Extern_Flag,
      Field_Virtual_Flag,
      Field_Pure_Flag,
      Field_Join_Option,
      Field_Edge_Identifier,
      Field_DPI_Spec,
      Field_Visibility,
      Field_Class_Visibility,
      Field_Has_Visibility,
      Field_Violation,
      Field_Random_Flag,
      Field_Randc_Flag,
      Field_Size_Flag,
      Field_Type_Analyzed_Flag,
      Field_Forward_Typedef_Flag,
      Field_Access,
      Field_Arg1,
      Field_Arg2
     );
   pragma Discard_Names (Fields_Enum);

   --  Return the type of field F.
   function Get_Field_Type (F : Fields_Enum) return Types_Enum;

   --  Get the name of a field.
   function Get_Field_Image (F : Fields_Enum) return String;

   --  Get the name of a kind.
   function Get_Nkind_Image (K : Nkind) return String;

   --  Possible attributes of a field.
   type Field_Attribute is
     (
      Attr_Maybe_Ref, Attr_Maybe_Ref2,
      Attr_None,
      Attr_Ref, Attr_Forward_Ref,
      Attr_Chain, Attr_Chain_Next
     );

   --  Attributes without Maybe_Ref*
   subtype Field_Actual_Attribute is Field_Attribute range
     Attr_None .. Field_Attribute'Last;

   --  Get the attribute of a field.
   function Get_Field_Attribute (F : Fields_Enum) return Field_Attribute;

   function Get_Field_Actual_Attribute (N : Node; F : Fields_Enum)
                                       return Field_Actual_Attribute;

   type Fields_Array is array (Natural range <>) of Fields_Enum;

   --  Return the list of fields for node K.  The fields are sorted: first
   --  the non nodes/list of nodes, then the nodes/lists that aren't reference,
   --  and then the reference.
   function Get_Fields (K : Nkind) return Fields_Array;

   --  Get/Set a field.
   function Get_Base_Type
      (N : Node; F : Fields_Enum) return Base_Type;
   procedure Set_Base_Type
      (N : Node; F : Fields_Enum; V: Base_Type);

   function Get_Binary_Ops
      (N : Node; F : Fields_Enum) return Binary_Ops;
   procedure Set_Binary_Ops
      (N : Node; F : Fields_Enum; V: Binary_Ops);

   function Get_Bn_Index
      (N : Node; F : Fields_Enum) return Bn_Index;
   procedure Set_Bn_Index
      (N : Node; F : Fields_Enum; V: Bn_Index);

   function Get_Boolean
      (N : Node; F : Fields_Enum) return Boolean;
   procedure Set_Boolean
      (N : Node; F : Fields_Enum; V: Boolean);

   function Get_Conv_Ops
      (N : Node; F : Fields_Enum) return Conv_Ops;
   procedure Set_Conv_Ops
      (N : Node; F : Fields_Enum; V: Conv_Ops);

   function Get_DPI_Spec_Type
      (N : Node; F : Fields_Enum) return DPI_Spec_Type;
   procedure Set_DPI_Spec_Type
      (N : Node; F : Fields_Enum; V: DPI_Spec_Type);

   function Get_Edge_Type
      (N : Node; F : Fields_Enum) return Edge_Type;
   procedure Set_Edge_Type
      (N : Node; F : Fields_Enum; V: Edge_Type);

   function Get_Fp64
      (N : Node; F : Fields_Enum) return Fp64;
   procedure Set_Fp64
      (N : Node; F : Fields_Enum; V: Fp64);

   function Get_Int32
      (N : Node; F : Fields_Enum) return Int32;
   procedure Set_Int32
      (N : Node; F : Fields_Enum; V: Int32);

   function Get_Join_Type
      (N : Node; F : Fields_Enum) return Join_Type;
   procedure Set_Join_Type
      (N : Node; F : Fields_Enum; V: Join_Type);

   function Get_Lifetime_Type
      (N : Node; F : Fields_Enum) return Lifetime_Type;
   procedure Set_Lifetime_Type
      (N : Node; F : Fields_Enum; V: Lifetime_Type);

   function Get_Lit_Id
      (N : Node; F : Fields_Enum) return Lit_Id;
   procedure Set_Lit_Id
      (N : Node; F : Fields_Enum; V: Lit_Id);

   function Get_Name_Id
      (N : Node; F : Fields_Enum) return Name_Id;
   procedure Set_Name_Id
      (N : Node; F : Fields_Enum; V: Name_Id);

   function Get_Node
      (N : Node; F : Fields_Enum) return Node;
   procedure Set_Node
      (N : Node; F : Fields_Enum; V: Node);

   function Get_Obj_Id
      (N : Node; F : Fields_Enum) return Obj_Id;
   procedure Set_Obj_Id
      (N : Node; F : Fields_Enum; V: Obj_Id);

   function Get_Polarity_Type
      (N : Node; F : Fields_Enum) return Polarity_Type;
   procedure Set_Polarity_Type
      (N : Node; F : Fields_Enum; V: Polarity_Type);

   function Get_Proc_Id
      (N : Node; F : Fields_Enum) return Proc_Id;
   procedure Set_Proc_Id
      (N : Node; F : Fields_Enum; V: Proc_Id);

   function Get_Scope_Id
      (N : Node; F : Fields_Enum) return Scope_Id;
   procedure Set_Scope_Id
      (N : Node; F : Fields_Enum; V: Scope_Id);

   function Get_String8_Id
      (N : Node; F : Fields_Enum) return String8_Id;
   procedure Set_String8_Id
      (N : Node; F : Fields_Enum; V: String8_Id);

   function Get_Sys_Tf_Id
      (N : Node; F : Fields_Enum) return Sys_Tf_Id;
   procedure Set_Sys_Tf_Id
      (N : Node; F : Fields_Enum; V: Sys_Tf_Id);

   function Get_Tsize_Type
      (N : Node; F : Fields_Enum) return Tsize_Type;
   procedure Set_Tsize_Type
      (N : Node; F : Fields_Enum; V: Tsize_Type);

   function Get_Udp_Kind
      (N : Node; F : Fields_Enum) return Udp_Kind;
   procedure Set_Udp_Kind
      (N : Node; F : Fields_Enum; V: Udp_Kind);

   function Get_Udp_Symbol
      (N : Node; F : Fields_Enum) return Udp_Symbol;
   procedure Set_Udp_Symbol
      (N : Node; F : Fields_Enum; V: Udp_Symbol);

   function Get_Unary_Ops
      (N : Node; F : Fields_Enum) return Unary_Ops;
   procedure Set_Unary_Ops
      (N : Node; F : Fields_Enum; V: Unary_Ops);

   function Get_Uns32
      (N : Node; F : Fields_Enum) return Uns32;
   procedure Set_Uns32
      (N : Node; F : Fields_Enum; V: Uns32);

   function Get_Violation_Type
      (N : Node; F : Fields_Enum) return Violation_Type;
   procedure Set_Violation_Type
      (N : Node; F : Fields_Enum; V: Violation_Type);

   function Get_Visibility_Type
      (N : Node; F : Fields_Enum) return Visibility_Type;
   procedure Set_Visibility_Type
      (N : Node; F : Fields_Enum; V: Visibility_Type);

   function Get_Width_Type
      (N : Node; F : Fields_Enum) return Width_Type;
   procedure Set_Width_Type
      (N : Node; F : Fields_Enum; V: Width_Type);

   function Has_Parent (K : Nkind) return Boolean;
   function Has_Call_Scope (K : Nkind) return Boolean;
   function Has_Identifier (K : Nkind) return Boolean;
   function Has_C_Identifier (K : Nkind) return Boolean;
   function Has_Ports_Chain (K : Nkind) return Boolean;
   function Has_Tf_Ports_Chain (K : Nkind) return Boolean;
   function Has_Package_Import_Chain (K : Nkind) return Boolean;
   function Has_Parameter_Port_Chain (K : Nkind) return Boolean;
   function Has_Parameter (K : Nkind) return Boolean;
   function Has_Foreign_Node (K : Nkind) return Boolean;
   function Has_Descriptions (K : Nkind) return Boolean;
   function Has_Class_Item_Chain (K : Nkind) return Boolean;
   function Has_Package_Item_Chain (K : Nkind) return Boolean;
   function Has_Items_Chain (K : Nkind) return Boolean;
   function Has_Clocking_Item_Chain (K : Nkind) return Boolean;
   function Has_Tf_Item_Declaration_Chain (K : Nkind) return Boolean;
   function Has_Block_Item_Declaration_Chain (K : Nkind) return Boolean;
   function Has_Generate_Item_Chain (K : Nkind) return Boolean;
   function Has_Specify_Item_Chain (K : Nkind) return Boolean;
   function Has_Statements_Chain (K : Nkind) return Boolean;
   function Has_Modport_Ports_Chain (K : Nkind) return Boolean;
   function Has_Chain (K : Nkind) return Boolean;
   function Has_Constraint_Block_Chain (K : Nkind) return Boolean;
   function Has_Constraint_Set (K : Nkind) return Boolean;
   function Has_OOB_Prefix (K : Nkind) return Boolean;
   function Has_Out_Of_Block_Declaration (K : Nkind) return Boolean;
   function Has_Generate_Index (K : Nkind) return Boolean;
   function Has_Return_Variable (K : Nkind) return Boolean;
   function Has_Return_Variable_Ref (K : Nkind) return Boolean;
   function Has_This_Variable (K : Nkind) return Boolean;
   function Has_Expression (K : Nkind) return Boolean;
   function Has_Reject_Limit (K : Nkind) return Boolean;
   function Has_Error_Limit (K : Nkind) return Boolean;
   function Has_Sequence (K : Nkind) return Boolean;
   function Has_Init_Expression (K : Nkind) return Boolean;
   function Has_Size_Expression (K : Nkind) return Boolean;
   function Has_Override_Stmt (K : Nkind) return Boolean;
   function Has_Parameter_Expression (K : Nkind) return Boolean;
   function Has_Parameter_Type (K : Nkind) return Boolean;
   function Has_Value_Range (K : Nkind) return Boolean;
   function Has_Lsb_Include_Flag (K : Nkind) return Boolean;
   function Has_Msb_Include_Flag (K : Nkind) return Boolean;
   function Has_Range (K : Nkind) return Boolean;
   function Has_Msb (K : Nkind) return Boolean;
   function Has_Lsb (K : Nkind) return Boolean;
   function Has_Msb_Cst (K : Nkind) return Boolean;
   function Has_Lsb_Cst (K : Nkind) return Boolean;
   function Has_Base_Expr (K : Nkind) return Boolean;
   function Has_Width_Expr (K : Nkind) return Boolean;
   function Has_Width_Cst (K : Nkind) return Boolean;
   function Has_Type_Width (K : Nkind) return Boolean;
   function Has_Type_Size (K : Nkind) return Boolean;
   function Has_Stride_Width (K : Nkind) return Boolean;
   function Has_Stride_Size (K : Nkind) return Boolean;
   function Has_Type_Hash (K : Nkind) return Boolean;
   function Has_Maximum_Size_Expr (K : Nkind) return Boolean;
   function Has_Maximum_Size_Cst (K : Nkind) return Boolean;
   function Has_Lvalue (K : Nkind) return Boolean;
   function Has_Name (K : Nkind) return Boolean;
   function Has_Item_Name (K : Nkind) return Boolean;
   function Has_Pattern_Key (K : Nkind) return Boolean;
   function Has_Left (K : Nkind) return Boolean;
   function Has_Right (K : Nkind) return Boolean;
   function Has_Repeat_Expression (K : Nkind) return Boolean;
   function Has_Op_Attributes (K : Nkind) return Boolean;
   function Has_Attributes_Chain (K : Nkind) return Boolean;
   function Has_Condition (K : Nkind) return Boolean;
   function Has_Cond_True (K : Nkind) return Boolean;
   function Has_Cond_False (K : Nkind) return Boolean;
   function Has_True_Stmt (K : Nkind) return Boolean;
   function Has_False_Stmt (K : Nkind) return Boolean;
   function Has_Pass_Stmt (K : Nkind) return Boolean;
   function Has_Else_Stmt (K : Nkind) return Boolean;
   function Has_Clocking_Event (K : Nkind) return Boolean;
   function Has_Disable_Expression (K : Nkind) return Boolean;
   function Has_Property_Expression (K : Nkind) return Boolean;
   function Has_True_Block (K : Nkind) return Boolean;
   function Has_False_Block (K : Nkind) return Boolean;
   function Has_Statement (K : Nkind) return Boolean;
   function Has_Foreach_Array (K : Nkind) return Boolean;
   function Has_Foreach_Variables (K : Nkind) return Boolean;
   function Has_Control (K : Nkind) return Boolean;
   function Has_Replication (K : Nkind) return Boolean;
   function Has_Replication_Cst (K : Nkind) return Boolean;
   function Has_Expressions (K : Nkind) return Boolean;
   function Has_Elements (K : Nkind) return Boolean;
   function Has_Slice_Size_Expr (K : Nkind) return Boolean;
   function Has_Slice_Size_Type (K : Nkind) return Boolean;
   function Has_Members (K : Nkind) return Boolean;
   function Has_Nbr_Members (K : Nkind) return Boolean;
   function Has_Member_Index (K : Nkind) return Boolean;
   function Has_Packed_Member_Offset (K : Nkind) return Boolean;
   function Has_Nature_Items (K : Nkind) return Boolean;
   function Has_Discipline_Items (K : Nkind) return Boolean;
   function Has_Continuous_Flag (K : Nkind) return Boolean;
   function Has_Potential_Flag (K : Nkind) return Boolean;
   function Has_Nature (K : Nkind) return Boolean;
   function Has_Connections (K : Nkind) return Boolean;
   function Has_Gate_Terminals (K : Nkind) return Boolean;
   function Has_Parameter_Values (K : Nkind) return Boolean;
   function Has_Case_Items (K : Nkind) return Boolean;
   function Has_Delay (K : Nkind) return Boolean;
   function Has_Net_Delay (K : Nkind) return Boolean;
   function Has_Gate_Delay (K : Nkind) return Boolean;
   function Has_Assign_Delay (K : Nkind) return Boolean;
   function Has_Rising_Delay (K : Nkind) return Boolean;
   function Has_Falling_Delay (K : Nkind) return Boolean;
   function Has_Highz_Delay (K : Nkind) return Boolean;
   function Has_For_Initialization (K : Nkind) return Boolean;
   function Has_Step_Assign (K : Nkind) return Boolean;
   function Has_Arguments (K : Nkind) return Boolean;
   function Has_Iterator_Argument (K : Nkind) return Boolean;
   function Has_Task (K : Nkind) return Boolean;
   function Has_Signed_Flag (K : Nkind) return Boolean;
   function Has_Scope_Flag (K : Nkind) return Boolean;
   function Has_Number_Base (K : Nkind) return Boolean;
   function Has_Number_Hi_Val (K : Nkind) return Boolean;
   function Has_Number_Lo_Val (K : Nkind) return Boolean;
   function Has_Number_Hi_Zx (K : Nkind) return Boolean;
   function Has_Number_Lo_Zx (K : Nkind) return Boolean;
   function Has_Number_Size (K : Nkind) return Boolean;
   function Has_Expr_Origin (K : Nkind) return Boolean;
   function Has_Bignum_Index (K : Nkind) return Boolean;
   function Has_Bignum_Len (K : Nkind) return Boolean;
   function Has_Real_Number (K : Nkind) return Boolean;
   function Has_Time_Unit (K : Nkind) return Boolean;
   function Has_Scale_Factor (K : Nkind) return Boolean;
   function Has_Time_Precision (K : Nkind) return Boolean;
   function Has_Timescale (K : Nkind) return Boolean;
   function Has_String_Size (K : Nkind) return Boolean;
   function Has_Data_Type (K : Nkind) return Boolean;
   function Has_Expr_Type (K : Nkind) return Boolean;
   function Has_Param_Type (K : Nkind) return Boolean;
   function Has_Element_Data_Type (K : Nkind) return Boolean;
   function Has_Type_Element_Type (K : Nkind) return Boolean;
   function Has_Cast_Data_Type (K : Nkind) return Boolean;
   function Has_Base_Class_Type (K : Nkind) return Boolean;
   function Has_Class_Constructor (K : Nkind) return Boolean;
   function Has_Inheritance_Depth (K : Nkind) return Boolean;
   function Has_Enum_Base_Data_Type (K : Nkind) return Boolean;
   function Has_Enum_Base_Type (K : Nkind) return Boolean;
   function Has_Packed_Base_Type (K : Nkind) return Boolean;
   function Has_Default_Type (K : Nkind) return Boolean;
   function Has_Type_Owner (K : Nkind) return Boolean;
   function Has_Type_Owner_2 (K : Nkind) return Boolean;
   function Has_Forward_Type (K : Nkind) return Boolean;
   function Has_Enum_Names (K : Nkind) return Boolean;
   function Has_Index_Data_Type (K : Nkind) return Boolean;
   function Has_Type_Index_Type (K : Nkind) return Boolean;
   function Has_Type_Argument (K : Nkind) return Boolean;
   function Has_Type_Signed (K : Nkind) return Boolean;
   function Has_Subroutine (K : Nkind) return Boolean;
   function Has_Object (K : Nkind) return Boolean;
   function Has_With_Expression (K : Nkind) return Boolean;
   function Has_Drive_Strength (K : Nkind) return Boolean;
   function Has_Net_Drive_Strength (K : Nkind) return Boolean;
   function Has_Charge_Strength (K : Nkind) return Boolean;
   function Has_Module (K : Nkind) return Boolean;
   function Has_Class_Name (K : Nkind) return Boolean;
   function Has_Interface (K : Nkind) return Boolean;
   function Has_Interface_Name (K : Nkind) return Boolean;
   function Has_Instance (K : Nkind) return Boolean;
   function Has_Instance_Ref (K : Nkind) return Boolean;
   function Has_Port (K : Nkind) return Boolean;
   function Has_Collapse_Flag (K : Nkind) return Boolean;
   function Has_Unary_Op (K : Nkind) return Boolean;
   function Has_Binary_Op (K : Nkind) return Boolean;
   function Has_Conversion_Op (K : Nkind) return Boolean;
   function Has_Declaration (K : Nkind) return Boolean;
   function Has_Redeclaration (K : Nkind) return Boolean;
   function Has_This_Declaration (K : Nkind) return Boolean;
   function Has_Default_Value (K : Nkind) return Boolean;
   function Has_Instantiated_Flag (K : Nkind) return Boolean;
   function Has_Ansi_Port_Flag (K : Nkind) return Boolean;
   function Has_Event (K : Nkind) return Boolean;
   function Has_Min_Expr (K : Nkind) return Boolean;
   function Has_Typ_Expr (K : Nkind) return Boolean;
   function Has_Max_Expr (K : Nkind) return Boolean;
   function Has_Udp_Port_Declaration_Chain (K : Nkind) return Boolean;
   function Has_Udp_Kind (K : Nkind) return Boolean;
   function Has_Udp_Initial (K : Nkind) return Boolean;
   function Has_Udp_Entries_Chain (K : Nkind) return Boolean;
   function Has_Input_Chain (K : Nkind) return Boolean;
   function Has_Output_Symbol (K : Nkind) return Boolean;
   function Has_Current_State (K : Nkind) return Boolean;
   function Has_Next_State (K : Nkind) return Boolean;
   function Has_Symbol (K : Nkind) return Boolean;
   function Has_From_Symbol (K : Nkind) return Boolean;
   function Has_To_Symbol (K : Nkind) return Boolean;
   function Has_Specify_Input (K : Nkind) return Boolean;
   function Has_Specify_Output (K : Nkind) return Boolean;
   function Has_Path_Delay (K : Nkind) return Boolean;
   function Has_Data_Source (K : Nkind) return Boolean;
   function Has_Polarity (K : Nkind) return Boolean;
   function Has_Delay_Rise (K : Nkind) return Boolean;
   function Has_Delay_Fall (K : Nkind) return Boolean;
   function Has_Delay_Z (K : Nkind) return Boolean;
   function Has_Delay_01 (K : Nkind) return Boolean;
   function Has_Delay_10 (K : Nkind) return Boolean;
   function Has_Delay_0z (K : Nkind) return Boolean;
   function Has_Delay_z1 (K : Nkind) return Boolean;
   function Has_Delay_1z (K : Nkind) return Boolean;
   function Has_Delay_z0 (K : Nkind) return Boolean;
   function Has_Delay_0x (K : Nkind) return Boolean;
   function Has_Delay_x1 (K : Nkind) return Boolean;
   function Has_Delay_1x (K : Nkind) return Boolean;
   function Has_Delay_x0 (K : Nkind) return Boolean;
   function Has_Delay_xz (K : Nkind) return Boolean;
   function Has_Delay_zx (K : Nkind) return Boolean;
   function Has_String_Id (K : Nkind) return Boolean;
   function Has_Label (K : Nkind) return Boolean;
   function Has_Label_Number (K : Nkind) return Boolean;
   function Has_Label_Chain (K : Nkind) return Boolean;
   function Has_Label_Use (K : Nkind) return Boolean;
   function Has_Suspend_Flag (K : Nkind) return Boolean;
   function Has_Same_Case_Flag (K : Nkind) return Boolean;
   function Has_Obj_Id (K : Nkind) return Boolean;
   function Has_Scope_Id (K : Nkind) return Boolean;
   function Has_Process_Id (K : Nkind) return Boolean;
   function Has_Sys_Tf_Id (K : Nkind) return Boolean;
   function Has_Lit_Id (K : Nkind) return Boolean;
   function Has_Generate_Block (K : Nkind) return Boolean;
   function Has_Input_Skew (K : Nkind) return Boolean;
   function Has_Output_Skew (K : Nkind) return Boolean;
   function Has_Delay_Control (K : Nkind) return Boolean;
   function Has_Attribute_Item (K : Nkind) return Boolean;
   function Has_Has_Identifier_List (K : Nkind) return Boolean;
   function Has_Has_Sign (K : Nkind) return Boolean;
   function Has_Connected_Flag (K : Nkind) return Boolean;
   function Has_Complete_Flag (K : Nkind) return Boolean;
   function Has_Implicit_Flag (K : Nkind) return Boolean;
   function Has_Redeclaration_Flag (K : Nkind) return Boolean;
   function Has_Is_Automatic (K : Nkind) return Boolean;
   function Has_Lifetime (K : Nkind) return Boolean;
   function Has_Has_Lifetime (K : Nkind) return Boolean;
   function Has_Has_End_Name (K : Nkind) return Boolean;
   function Has_Call (K : Nkind) return Boolean;
   function Has_Timeunit (K : Nkind) return Boolean;
   function Has_Timeprecision (K : Nkind) return Boolean;
   function Has_Error_Origin (K : Nkind) return Boolean;
   function Has_Has_Void_Cast (K : Nkind) return Boolean;
   function Has_Is_Const (K : Nkind) return Boolean;
   function Has_Has_Var (K : Nkind) return Boolean;
   function Has_Has_Type (K : Nkind) return Boolean;
   function Has_Has_Direction (K : Nkind) return Boolean;
   function Has_Has_Parenthesis (K : Nkind) return Boolean;
   function Has_Has_Argument (K : Nkind) return Boolean;
   function Has_Fully_Analyzed_Flag (K : Nkind) return Boolean;
   function Has_Resolved_Flag (K : Nkind) return Boolean;
   function Has_Mark_Flag (K : Nkind) return Boolean;
   function Has_Is_Constant (K : Nkind) return Boolean;
   function Has_Static_Flag (K : Nkind) return Boolean;
   function Has_Has_Attribute (K : Nkind) return Boolean;
   function Has_Attribute_Full (K : Nkind) return Boolean;
   function Has_Attribute_Parallel (K : Nkind) return Boolean;
   function Has_Other_Attributes (K : Nkind) return Boolean;
   function Has_Pure_Property (K : Nkind) return Boolean;
   function Has_Context_Property (K : Nkind) return Boolean;
   function Has_Has_Extern_Flag (K : Nkind) return Boolean;
   function Has_Virtual_Flag (K : Nkind) return Boolean;
   function Has_Pure_Flag (K : Nkind) return Boolean;
   function Has_Join_Option (K : Nkind) return Boolean;
   function Has_Edge_Identifier (K : Nkind) return Boolean;
   function Has_DPI_Spec (K : Nkind) return Boolean;
   function Has_Visibility (K : Nkind) return Boolean;
   function Has_Class_Visibility (K : Nkind) return Boolean;
   function Has_Has_Visibility (K : Nkind) return Boolean;
   function Has_Violation (K : Nkind) return Boolean;
   function Has_Random_Flag (K : Nkind) return Boolean;
   function Has_Randc_Flag (K : Nkind) return Boolean;
   function Has_Size_Flag (K : Nkind) return Boolean;
   function Has_Type_Analyzed_Flag (K : Nkind) return Boolean;
   function Has_Forward_Typedef_Flag (K : Nkind) return Boolean;
   function Has_Access (K : Nkind) return Boolean;
   function Has_Arg1 (K : Nkind) return Boolean;
   function Has_Arg2 (K : Nkind) return Boolean;
end Verilog.Nodes_Meta;
