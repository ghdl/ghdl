--  Meta description of nodes.
--  Copyright (C) 2014 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Tokens; use Vhdl.Tokens;
with PSL.Types; use PSL.Types;

package Vhdl.Nodes_Meta is
   --  The enumeration of all possible types in the nodes.
   type Types_Enum is
     (
      Type_Boolean,
      Type_Date_State_Type,
      Type_Date_Type,
      Type_Direction_Type,
      Type_File_Checksum_Id,
      Type_Fp64,
      Type_Iir,
      Type_Iir_All_Sensitized,
      Type_Iir_Constraint,
      Type_Iir_Delay_Mechanism,
      Type_Iir_Flist,
      Type_Iir_Force_Mode,
      Type_Iir_Index32,
      Type_Iir_Int32,
      Type_Iir_List,
      Type_Iir_Mode,
      Type_Iir_Predefined_Functions,
      Type_Iir_Pure_State,
      Type_Iir_Signal_Kind,
      Type_Iir_Staticness,
      Type_Int32,
      Type_Int64,
      Type_Name_Id,
      Type_Number_Base_Type,
      Type_PSL_NFA,
      Type_PSL_Node,
      Type_Scalar_Size,
      Type_Source_File_Entry,
      Type_Source_Ptr,
      Type_String8_Id,
      Type_Time_Stamp_Id,
      Type_Token_Type,
      Type_Tri_State_Type
     );

   --  The enumeration of all fields defined in iirs.
   type Fields_Enum is
     (
      Field_First_Design_Unit,
      Field_Last_Design_Unit,
      Field_Library_Declaration,
      Field_File_Checksum,
      Field_Analysis_Time_Stamp,
      Field_Design_File_Source,
      Field_Library,
      Field_File_Dependence_List,
      Field_Design_File_Filename,
      Field_Design_File_Directory,
      Field_Design_File,
      Field_Design_File_Chain,
      Field_Library_Directory,
      Field_Date,
      Field_Context_Items,
      Field_Dependence_List,
      Field_Analysis_Checks_List,
      Field_Date_State,
      Field_Guarded_Target_State,
      Field_Library_Unit,
      Field_Hash_Chain,
      Field_Design_Unit_Source_Pos,
      Field_Design_Unit_Source_Line,
      Field_Design_Unit_Source_Col,
      Field_Value,
      Field_Enum_Pos,
      Field_Physical_Literal,
      Field_Fp_Value,
      Field_Simple_Aggregate_List,
      Field_String8_Id,
      Field_String_Length,
      Field_Bit_String_Base,
      Field_Has_Signed,
      Field_Has_Sign,
      Field_Has_Length,
      Field_Literal_Length,
      Field_Literal_Origin,
      Field_Range_Origin,
      Field_Literal_Subtype,
      Field_Allocator_Subtype,
      Field_Entity_Class,
      Field_Entity_Name_List,
      Field_Attribute_Designator,
      Field_Attribute_Specification_Chain,
      Field_Attribute_Specification,
      Field_Static_Attribute_Flag,
      Field_Signal_List,
      Field_Quantity_List,
      Field_Designated_Entity,
      Field_Formal,
      Field_Actual,
      Field_Open_Actual,
      Field_Actual_Conversion,
      Field_Formal_Conversion,
      Field_Whole_Association_Flag,
      Field_Collapse_Signal_Flag,
      Field_Artificial_Flag,
      Field_Open_Flag,
      Field_After_Drivers_Flag,
      Field_We_Value,
      Field_Time,
      Field_Associated_Expr,
      Field_Associated_Block,
      Field_Associated_Chain,
      Field_Choice_Name,
      Field_Choice_Expression,
      Field_Choice_Range,
      Field_Same_Alternative_Flag,
      Field_Element_Type_Flag,
      Field_Architecture,
      Field_Block_Specification,
      Field_Prev_Block_Configuration,
      Field_Configuration_Item_Chain,
      Field_Attribute_Value_Chain,
      Field_Spec_Chain,
      Field_Value_Chain,
      Field_Attribute_Value_Spec_Chain,
      Field_Entity_Name,
      Field_Package,
      Field_Package_Body,
      Field_Instance_Package_Body,
      Field_Need_Body,
      Field_Macro_Expanded_Flag,
      Field_Need_Instance_Bodies,
      Field_Hierarchical_Name,
      Field_Vunit_Item_Chain,
      Field_Bound_Vunit_Chain,
      Field_Verification_Block_Configuration,
      Field_Block_Configuration,
      Field_Concurrent_Statement_Chain,
      Field_Chain,
      Field_Port_Chain,
      Field_Generic_Chain,
      Field_Type,
      Field_Subtype_Indication,
      Field_Discrete_Range,
      Field_Type_Definition,
      Field_Subtype_Definition,
      Field_Incomplete_Type_Declaration,
      Field_Interface_Type_Subprograms,
      Field_Interface_Type_Definition,
      Field_Nature_Definition,
      Field_Nature,
      Field_Subnature_Indication,
      Field_Reference_Terminal_Flag,
      Field_Mode,
      Field_Guarded_Signal_Flag,
      Field_Signal_Kind,
      Field_Base_Name,
      Field_Interface_Declaration_Chain,
      Field_Default_Subprogram,
      Field_Associated_Subprogram,
      Field_Subprogram_Specification,
      Field_Sequential_Statement_Chain,
      Field_Simultaneous_Statement_Chain,
      Field_Subprogram_Body,
      Field_Overload_Number,
      Field_Subprogram_Depth,
      Field_Subprogram_Hash,
      Field_Impure_Depth,
      Field_Return_Type,
      Field_Implicit_Definition,
      Field_Uninstantiated_Subprogram_Name,
      Field_Default_Value,
      Field_Deferred_Declaration,
      Field_Deferred_Declaration_Flag,
      Field_Shared_Flag,
      Field_Design_Unit,
      Field_Block_Statement,
      Field_Signal_Driver,
      Field_Declaration_Chain,
      Field_File_Logical_Name,
      Field_File_Open_Kind,
      Field_Element_Position,
      Field_Use_Clause_Chain,
      Field_Context_Reference_Chain,
      Field_Inherit_Spec_Chain,
      Field_Selected_Name,
      Field_Type_Declarator,
      Field_Complete_Type_Definition,
      Field_Incomplete_Type_Ref_Chain,
      Field_Associated_Type,
      Field_Enumeration_Literal_List,
      Field_Entity_Class_Entry_Chain,
      Field_Group_Constituent_List,
      Field_Unit_Chain,
      Field_Primary_Unit,
      Field_Identifier,
      Field_Label,
      Field_Return_Identifier,
      Field_Visible_Flag,
      Field_Range_Constraint,
      Field_Direction,
      Field_Left_Limit,
      Field_Right_Limit,
      Field_Left_Limit_Expr,
      Field_Right_Limit_Expr,
      Field_Parent_Type,
      Field_Simple_Nature,
      Field_Base_Nature,
      Field_Resolution_Indication,
      Field_Record_Element_Resolution_Chain,
      Field_Tolerance,
      Field_Plus_Terminal_Name,
      Field_Minus_Terminal_Name,
      Field_Plus_Terminal,
      Field_Minus_Terminal,
      Field_Magnitude_Expression,
      Field_Phase_Expression,
      Field_Power_Expression,
      Field_Simultaneous_Left,
      Field_Simultaneous_Right,
      Field_Text_File_Flag,
      Field_Only_Characters_Flag,
      Field_Is_Character_Type,
      Field_Nature_Staticness,
      Field_Type_Staticness,
      Field_Constraint_State,
      Field_Index_Subtype_List,
      Field_Index_Subtype_Definition_List,
      Field_Element_Subtype_Indication,
      Field_Element_Subtype,
      Field_Element_Subnature_Indication,
      Field_Element_Subnature,
      Field_Index_Constraint_List,
      Field_Array_Element_Constraint,
      Field_Has_Array_Constraint_Flag,
      Field_Has_Element_Constraint_Flag,
      Field_Elements_Declaration_List,
      Field_Owned_Elements_Chain,
      Field_Designated_Type,
      Field_Designated_Subtype_Indication,
      Field_Index_List,
      Field_Reference,
      Field_Nature_Declarator,
      Field_Across_Type_Mark,
      Field_Through_Type_Mark,
      Field_Across_Type_Definition,
      Field_Through_Type_Definition,
      Field_Across_Type,
      Field_Through_Type,
      Field_Target,
      Field_Waveform_Chain,
      Field_Guard,
      Field_Delay_Mechanism,
      Field_Reject_Time_Expression,
      Field_Force_Mode,
      Field_Has_Force_Mode,
      Field_Sensitivity_List,
      Field_Process_Origin,
      Field_Package_Origin,
      Field_Condition_Clause,
      Field_Break_Element,
      Field_Selector_Quantity,
      Field_Break_Quantity,
      Field_Timeout_Clause,
      Field_Postponed_Flag,
      Field_Callees_List,
      Field_Passive_Flag,
      Field_Resolution_Function_Flag,
      Field_Wait_State,
      Field_All_Sensitized_State,
      Field_Seen_Flag,
      Field_Pure_Flag,
      Field_Foreign_Flag,
      Field_Resolved_Flag,
      Field_Signal_Type_Flag,
      Field_Has_Signal_Flag,
      Field_Purity_State,
      Field_Elab_Flag,
      Field_Vendor_Library_Flag,
      Field_Configuration_Mark_Flag,
      Field_Configuration_Done_Flag,
      Field_Index_Constraint_Flag,
      Field_Hide_Implicit_Flag,
      Field_Assertion_Condition,
      Field_Report_Expression,
      Field_Severity_Expression,
      Field_Instantiated_Unit,
      Field_Instantiated_Header,
      Field_Generic_Map_Aspect_Chain,
      Field_Port_Map_Aspect_Chain,
      Field_Configuration_Name,
      Field_Component_Configuration,
      Field_Configuration_Specification,
      Field_Default_Binding_Indication,
      Field_Default_Configuration_Declaration,
      Field_Expression,
      Field_Conditional_Expression_Chain,
      Field_Allocator_Designated_Type,
      Field_Selected_Waveform_Chain,
      Field_Conditional_Waveform_Chain,
      Field_Guard_Expression,
      Field_Guard_Decl,
      Field_Guard_Sensitivity_List,
      Field_Attribute_Implicit_Chain,
      Field_Block_Block_Configuration,
      Field_Package_Header,
      Field_Block_Header,
      Field_Uninstantiated_Package_Name,
      Field_Uninstantiated_Package_Decl,
      Field_Instance_Source_File,
      Field_Generate_Block_Configuration,
      Field_Generate_Statement_Body,
      Field_Alternative_Label,
      Field_Generate_Else_Clause,
      Field_Condition,
      Field_Else_Clause,
      Field_Parameter_Specification,
      Field_Parent,
      Field_Loop_Label,
      Field_Exit_Flag,
      Field_Next_Flag,
      Field_Component_Name,
      Field_Instantiation_List,
      Field_Entity_Aspect,
      Field_Default_Entity_Aspect,
      Field_Binding_Indication,
      Field_Named_Entity,
      Field_Referenced_Name,
      Field_Expr_Staticness,
      Field_Scalar_Size,
      Field_Error_Origin,
      Field_Operand,
      Field_Left,
      Field_Right,
      Field_Unit_Name,
      Field_Name,
      Field_Group_Template_Name,
      Field_Name_Staticness,
      Field_Prefix,
      Field_Signature_Prefix,
      Field_External_Pathname,
      Field_Pathname_Suffix,
      Field_Pathname_Expression,
      Field_In_Formal_Flag,
      Field_Inertial_Flag,
      Field_Slice_Subtype,
      Field_Suffix,
      Field_Index_Subtype,
      Field_Parameter,
      Field_Parameter_2,
      Field_Parameter_3,
      Field_Parameter_4,
      Field_Attr_Chain,
      Field_Attribute_Implicit_Declaration,
      Field_Actual_Type,
      Field_Actual_Type_Definition,
      Field_Association_Chain,
      Field_Individual_Association_Chain,
      Field_Subprogram_Association_Chain,
      Field_Aggregate_Info,
      Field_Sub_Aggregate_Info,
      Field_Aggr_Dynamic_Flag,
      Field_Aggr_Min_Length,
      Field_Aggr_Low_Limit,
      Field_Aggr_High_Limit,
      Field_Aggr_Others_Flag,
      Field_Aggr_Named_Flag,
      Field_Aggregate_Expand_Flag,
      Field_Determined_Aggregate_Flag,
      Field_Association_Choices_Chain,
      Field_Case_Statement_Alternative_Chain,
      Field_Matching_Flag,
      Field_Choice_Staticness,
      Field_Procedure_Call,
      Field_Implementation,
      Field_Parameter_Association_Chain,
      Field_Method_Object,
      Field_Subtype_Type_Mark,
      Field_Subnature_Nature_Mark,
      Field_Type_Conversion_Subtype,
      Field_Type_Mark,
      Field_File_Type_Mark,
      Field_Return_Type_Mark,
      Field_Has_Disconnect_Flag,
      Field_Has_Active_Flag,
      Field_Is_Within_Flag,
      Field_Type_Marks_List,
      Field_Implicit_Alias_Flag,
      Field_Alias_Signature,
      Field_Attribute_Signature,
      Field_Overload_List,
      Field_Simple_Name_Identifier,
      Field_Simple_Name_Subtype,
      Field_Protected_Type_Body,
      Field_Protected_Type_Declaration,
      Field_Use_Flag,
      Field_End_Has_Reserved_Id,
      Field_End_Has_Identifier,
      Field_End_Has_Postponed,
      Field_Has_Label,
      Field_Has_Begin,
      Field_Has_End,
      Field_Has_Is,
      Field_Has_Pure,
      Field_Has_Body,
      Field_Has_Parameter,
      Field_Has_Component,
      Field_Has_Identifier_List,
      Field_Has_Mode,
      Field_Has_Class,
      Field_Has_Delay_Mechanism,
      Field_Suspend_Flag,
      Field_Stop_Flag,
      Field_Is_Ref,
      Field_Is_Forward_Ref,
      Field_Psl_Property,
      Field_Psl_Sequence,
      Field_Psl_Declaration,
      Field_Psl_Expression,
      Field_Psl_Boolean,
      Field_PSL_Clock,
      Field_PSL_NFA,
      Field_PSL_Nbr_States,
      Field_PSL_Clock_Sensitivity,
      Field_PSL_EOS_Flag,
      Field_PSL_Abort_Flag,
      Field_Count_Expression,
      Field_Clock_Expression,
      Field_Default_Clock,
      Field_Foreign_Node,
      Field_Suspend_State_Index,
      Field_Suspend_State_Chain
     );
   pragma Discard_Names (Fields_Enum);

   --  Return the type of field F.
   function Get_Field_Type (F : Fields_Enum) return Types_Enum;

   --  Get the name of a field.
   function Get_Field_Image (F : Fields_Enum) return String;

   --  Get the name of a kind.
   function Get_Iir_Image (K : Iir_Kind) return String;

   --  Possible attributes of a field.
   type Field_Attribute is
     (
      Attr_None,
      Attr_Chain,
      Attr_Chain_Next,
      Attr_Forward_Ref,
      Attr_Maybe_Forward_Ref,
      Attr_Maybe_Ref,
      Attr_Of_Maybe_Ref,
      Attr_Of_Ref,
      Attr_Ref
     );

   --  Get the attribute of a field.
   function Get_Field_Attribute (F : Fields_Enum) return Field_Attribute;

   type Fields_Index_Extended is new Int32;
   subtype Fields_Index is Fields_Index_Extended
     range 0 .. Fields_Index_Extended'Last;

   type Fields_Array is array (Fields_Index range <>) of Fields_Enum;

   --  Return the list of fields for node K.  The fields are sorted: first
   --  the non nodes/list of nodes, then the nodes/lists that aren't reference,
   --  and then the reference.
   function Get_Fields (K : Iir_Kind) return Fields_Array;

   --  Likewise, but without using arrays (for interfacing with C).
   function Get_Fields_First (K : Iir_Kind) return Fields_Index;
   function Get_Fields_Last (K : Iir_Kind) return Fields_Index;
   function Get_Field_By_Index (Idx : Fields_Index) return Fields_Enum;

   --  Get/Set a field.
   function Get_Boolean
      (N : Iir; F : Fields_Enum) return Boolean;
   procedure Set_Boolean
      (N : Iir; F : Fields_Enum; V: Boolean);

   function Get_Date_State_Type
      (N : Iir; F : Fields_Enum) return Date_State_Type;
   procedure Set_Date_State_Type
      (N : Iir; F : Fields_Enum; V: Date_State_Type);

   function Get_Date_Type
      (N : Iir; F : Fields_Enum) return Date_Type;
   procedure Set_Date_Type
      (N : Iir; F : Fields_Enum; V: Date_Type);

   function Get_Direction_Type
      (N : Iir; F : Fields_Enum) return Direction_Type;
   procedure Set_Direction_Type
      (N : Iir; F : Fields_Enum; V: Direction_Type);

   function Get_File_Checksum_Id
      (N : Iir; F : Fields_Enum) return File_Checksum_Id;
   procedure Set_File_Checksum_Id
      (N : Iir; F : Fields_Enum; V: File_Checksum_Id);

   function Get_Fp64
      (N : Iir; F : Fields_Enum) return Fp64;
   procedure Set_Fp64
      (N : Iir; F : Fields_Enum; V: Fp64);

   function Get_Iir
      (N : Iir; F : Fields_Enum) return Iir;
   procedure Set_Iir
      (N : Iir; F : Fields_Enum; V: Iir);

   function Get_Iir_All_Sensitized
      (N : Iir; F : Fields_Enum) return Iir_All_Sensitized;
   procedure Set_Iir_All_Sensitized
      (N : Iir; F : Fields_Enum; V: Iir_All_Sensitized);

   function Get_Iir_Constraint
      (N : Iir; F : Fields_Enum) return Iir_Constraint;
   procedure Set_Iir_Constraint
      (N : Iir; F : Fields_Enum; V: Iir_Constraint);

   function Get_Iir_Delay_Mechanism
      (N : Iir; F : Fields_Enum) return Iir_Delay_Mechanism;
   procedure Set_Iir_Delay_Mechanism
      (N : Iir; F : Fields_Enum; V: Iir_Delay_Mechanism);

   function Get_Iir_Flist
      (N : Iir; F : Fields_Enum) return Iir_Flist;
   procedure Set_Iir_Flist
      (N : Iir; F : Fields_Enum; V: Iir_Flist);

   function Get_Iir_Force_Mode
      (N : Iir; F : Fields_Enum) return Iir_Force_Mode;
   procedure Set_Iir_Force_Mode
      (N : Iir; F : Fields_Enum; V: Iir_Force_Mode);

   function Get_Iir_Index32
      (N : Iir; F : Fields_Enum) return Iir_Index32;
   procedure Set_Iir_Index32
      (N : Iir; F : Fields_Enum; V: Iir_Index32);

   function Get_Iir_Int32
      (N : Iir; F : Fields_Enum) return Iir_Int32;
   procedure Set_Iir_Int32
      (N : Iir; F : Fields_Enum; V: Iir_Int32);

   function Get_Iir_List
      (N : Iir; F : Fields_Enum) return Iir_List;
   procedure Set_Iir_List
      (N : Iir; F : Fields_Enum; V: Iir_List);

   function Get_Iir_Mode
      (N : Iir; F : Fields_Enum) return Iir_Mode;
   procedure Set_Iir_Mode
      (N : Iir; F : Fields_Enum; V: Iir_Mode);

   function Get_Iir_Predefined_Functions
      (N : Iir; F : Fields_Enum) return Iir_Predefined_Functions;
   procedure Set_Iir_Predefined_Functions
      (N : Iir; F : Fields_Enum; V: Iir_Predefined_Functions);

   function Get_Iir_Pure_State
      (N : Iir; F : Fields_Enum) return Iir_Pure_State;
   procedure Set_Iir_Pure_State
      (N : Iir; F : Fields_Enum; V: Iir_Pure_State);

   function Get_Iir_Signal_Kind
      (N : Iir; F : Fields_Enum) return Iir_Signal_Kind;
   procedure Set_Iir_Signal_Kind
      (N : Iir; F : Fields_Enum; V: Iir_Signal_Kind);

   function Get_Iir_Staticness
      (N : Iir; F : Fields_Enum) return Iir_Staticness;
   procedure Set_Iir_Staticness
      (N : Iir; F : Fields_Enum; V: Iir_Staticness);

   function Get_Int32
      (N : Iir; F : Fields_Enum) return Int32;
   procedure Set_Int32
      (N : Iir; F : Fields_Enum; V: Int32);

   function Get_Int64
      (N : Iir; F : Fields_Enum) return Int64;
   procedure Set_Int64
      (N : Iir; F : Fields_Enum; V: Int64);

   function Get_Name_Id
      (N : Iir; F : Fields_Enum) return Name_Id;
   procedure Set_Name_Id
      (N : Iir; F : Fields_Enum; V: Name_Id);

   function Get_Number_Base_Type
      (N : Iir; F : Fields_Enum) return Number_Base_Type;
   procedure Set_Number_Base_Type
      (N : Iir; F : Fields_Enum; V: Number_Base_Type);

   function Get_PSL_NFA
      (N : Iir; F : Fields_Enum) return PSL_NFA;
   procedure Set_PSL_NFA
      (N : Iir; F : Fields_Enum; V: PSL_NFA);

   function Get_PSL_Node
      (N : Iir; F : Fields_Enum) return PSL_Node;
   procedure Set_PSL_Node
      (N : Iir; F : Fields_Enum; V: PSL_Node);

   function Get_Scalar_Size
      (N : Iir; F : Fields_Enum) return Scalar_Size;
   procedure Set_Scalar_Size
      (N : Iir; F : Fields_Enum; V: Scalar_Size);

   function Get_Source_File_Entry
      (N : Iir; F : Fields_Enum) return Source_File_Entry;
   procedure Set_Source_File_Entry
      (N : Iir; F : Fields_Enum; V: Source_File_Entry);

   function Get_Source_Ptr
      (N : Iir; F : Fields_Enum) return Source_Ptr;
   procedure Set_Source_Ptr
      (N : Iir; F : Fields_Enum; V: Source_Ptr);

   function Get_String8_Id
      (N : Iir; F : Fields_Enum) return String8_Id;
   procedure Set_String8_Id
      (N : Iir; F : Fields_Enum; V: String8_Id);

   function Get_Time_Stamp_Id
      (N : Iir; F : Fields_Enum) return Time_Stamp_Id;
   procedure Set_Time_Stamp_Id
      (N : Iir; F : Fields_Enum; V: Time_Stamp_Id);

   function Get_Token_Type
      (N : Iir; F : Fields_Enum) return Token_Type;
   procedure Set_Token_Type
      (N : Iir; F : Fields_Enum; V: Token_Type);

   function Get_Tri_State_Type
      (N : Iir; F : Fields_Enum) return Tri_State_Type;
   procedure Set_Tri_State_Type
      (N : Iir; F : Fields_Enum; V: Tri_State_Type);

   function Has_First_Design_Unit (K : Iir_Kind) return Boolean;
   function Has_Last_Design_Unit (K : Iir_Kind) return Boolean;
   function Has_Library_Declaration (K : Iir_Kind) return Boolean;
   function Has_File_Checksum (K : Iir_Kind) return Boolean;
   function Has_Analysis_Time_Stamp (K : Iir_Kind) return Boolean;
   function Has_Design_File_Source (K : Iir_Kind) return Boolean;
   function Has_Library (K : Iir_Kind) return Boolean;
   function Has_File_Dependence_List (K : Iir_Kind) return Boolean;
   function Has_Design_File_Filename (K : Iir_Kind) return Boolean;
   function Has_Design_File_Directory (K : Iir_Kind) return Boolean;
   function Has_Design_File (K : Iir_Kind) return Boolean;
   function Has_Design_File_Chain (K : Iir_Kind) return Boolean;
   function Has_Library_Directory (K : Iir_Kind) return Boolean;
   function Has_Date (K : Iir_Kind) return Boolean;
   function Has_Context_Items (K : Iir_Kind) return Boolean;
   function Has_Dependence_List (K : Iir_Kind) return Boolean;
   function Has_Analysis_Checks_List (K : Iir_Kind) return Boolean;
   function Has_Date_State (K : Iir_Kind) return Boolean;
   function Has_Guarded_Target_State (K : Iir_Kind) return Boolean;
   function Has_Library_Unit (K : Iir_Kind) return Boolean;
   function Has_Hash_Chain (K : Iir_Kind) return Boolean;
   function Has_Design_Unit_Source_Pos (K : Iir_Kind) return Boolean;
   function Has_Design_Unit_Source_Line (K : Iir_Kind) return Boolean;
   function Has_Design_Unit_Source_Col (K : Iir_Kind) return Boolean;
   function Has_Value (K : Iir_Kind) return Boolean;
   function Has_Enum_Pos (K : Iir_Kind) return Boolean;
   function Has_Physical_Literal (K : Iir_Kind) return Boolean;
   function Has_Fp_Value (K : Iir_Kind) return Boolean;
   function Has_Simple_Aggregate_List (K : Iir_Kind) return Boolean;
   function Has_String8_Id (K : Iir_Kind) return Boolean;
   function Has_String_Length (K : Iir_Kind) return Boolean;
   function Has_Bit_String_Base (K : Iir_Kind) return Boolean;
   function Has_Has_Signed (K : Iir_Kind) return Boolean;
   function Has_Has_Sign (K : Iir_Kind) return Boolean;
   function Has_Has_Length (K : Iir_Kind) return Boolean;
   function Has_Literal_Length (K : Iir_Kind) return Boolean;
   function Has_Literal_Origin (K : Iir_Kind) return Boolean;
   function Has_Range_Origin (K : Iir_Kind) return Boolean;
   function Has_Literal_Subtype (K : Iir_Kind) return Boolean;
   function Has_Allocator_Subtype (K : Iir_Kind) return Boolean;
   function Has_Entity_Class (K : Iir_Kind) return Boolean;
   function Has_Entity_Name_List (K : Iir_Kind) return Boolean;
   function Has_Attribute_Designator (K : Iir_Kind) return Boolean;
   function Has_Attribute_Specification_Chain (K : Iir_Kind)
      return Boolean;
   function Has_Attribute_Specification (K : Iir_Kind) return Boolean;
   function Has_Static_Attribute_Flag (K : Iir_Kind) return Boolean;
   function Has_Signal_List (K : Iir_Kind) return Boolean;
   function Has_Quantity_List (K : Iir_Kind) return Boolean;
   function Has_Designated_Entity (K : Iir_Kind) return Boolean;
   function Has_Formal (K : Iir_Kind) return Boolean;
   function Has_Actual (K : Iir_Kind) return Boolean;
   function Has_Open_Actual (K : Iir_Kind) return Boolean;
   function Has_Actual_Conversion (K : Iir_Kind) return Boolean;
   function Has_Formal_Conversion (K : Iir_Kind) return Boolean;
   function Has_Whole_Association_Flag (K : Iir_Kind) return Boolean;
   function Has_Collapse_Signal_Flag (K : Iir_Kind) return Boolean;
   function Has_Artificial_Flag (K : Iir_Kind) return Boolean;
   function Has_Open_Flag (K : Iir_Kind) return Boolean;
   function Has_After_Drivers_Flag (K : Iir_Kind) return Boolean;
   function Has_We_Value (K : Iir_Kind) return Boolean;
   function Has_Time (K : Iir_Kind) return Boolean;
   function Has_Associated_Expr (K : Iir_Kind) return Boolean;
   function Has_Associated_Block (K : Iir_Kind) return Boolean;
   function Has_Associated_Chain (K : Iir_Kind) return Boolean;
   function Has_Choice_Name (K : Iir_Kind) return Boolean;
   function Has_Choice_Expression (K : Iir_Kind) return Boolean;
   function Has_Choice_Range (K : Iir_Kind) return Boolean;
   function Has_Same_Alternative_Flag (K : Iir_Kind) return Boolean;
   function Has_Element_Type_Flag (K : Iir_Kind) return Boolean;
   function Has_Architecture (K : Iir_Kind) return Boolean;
   function Has_Block_Specification (K : Iir_Kind) return Boolean;
   function Has_Prev_Block_Configuration (K : Iir_Kind) return Boolean;
   function Has_Configuration_Item_Chain (K : Iir_Kind) return Boolean;
   function Has_Attribute_Value_Chain (K : Iir_Kind) return Boolean;
   function Has_Spec_Chain (K : Iir_Kind) return Boolean;
   function Has_Value_Chain (K : Iir_Kind) return Boolean;
   function Has_Attribute_Value_Spec_Chain (K : Iir_Kind) return Boolean;
   function Has_Entity_Name (K : Iir_Kind) return Boolean;
   function Has_Package (K : Iir_Kind) return Boolean;
   function Has_Package_Body (K : Iir_Kind) return Boolean;
   function Has_Instance_Package_Body (K : Iir_Kind) return Boolean;
   function Has_Need_Body (K : Iir_Kind) return Boolean;
   function Has_Macro_Expanded_Flag (K : Iir_Kind) return Boolean;
   function Has_Need_Instance_Bodies (K : Iir_Kind) return Boolean;
   function Has_Hierarchical_Name (K : Iir_Kind) return Boolean;
   function Has_Vunit_Item_Chain (K : Iir_Kind) return Boolean;
   function Has_Bound_Vunit_Chain (K : Iir_Kind) return Boolean;
   function Has_Verification_Block_Configuration (K : Iir_Kind)
      return Boolean;
   function Has_Block_Configuration (K : Iir_Kind) return Boolean;
   function Has_Concurrent_Statement_Chain (K : Iir_Kind) return Boolean;
   function Has_Chain (K : Iir_Kind) return Boolean;
   function Has_Port_Chain (K : Iir_Kind) return Boolean;
   function Has_Generic_Chain (K : Iir_Kind) return Boolean;
   function Has_Type (K : Iir_Kind) return Boolean;
   function Has_Subtype_Indication (K : Iir_Kind) return Boolean;
   function Has_Discrete_Range (K : Iir_Kind) return Boolean;
   function Has_Type_Definition (K : Iir_Kind) return Boolean;
   function Has_Subtype_Definition (K : Iir_Kind) return Boolean;
   function Has_Incomplete_Type_Declaration (K : Iir_Kind) return Boolean;
   function Has_Interface_Type_Subprograms (K : Iir_Kind) return Boolean;
   function Has_Interface_Type_Definition (K : Iir_Kind) return Boolean;
   function Has_Nature_Definition (K : Iir_Kind) return Boolean;
   function Has_Nature (K : Iir_Kind) return Boolean;
   function Has_Subnature_Indication (K : Iir_Kind) return Boolean;
   function Has_Reference_Terminal_Flag (K : Iir_Kind) return Boolean;
   function Has_Mode (K : Iir_Kind) return Boolean;
   function Has_Guarded_Signal_Flag (K : Iir_Kind) return Boolean;
   function Has_Signal_Kind (K : Iir_Kind) return Boolean;
   function Has_Base_Name (K : Iir_Kind) return Boolean;
   function Has_Interface_Declaration_Chain (K : Iir_Kind) return Boolean;
   function Has_Default_Subprogram (K : Iir_Kind) return Boolean;
   function Has_Associated_Subprogram (K : Iir_Kind) return Boolean;
   function Has_Subprogram_Specification (K : Iir_Kind) return Boolean;
   function Has_Sequential_Statement_Chain (K : Iir_Kind) return Boolean;
   function Has_Simultaneous_Statement_Chain (K : Iir_Kind) return Boolean;
   function Has_Subprogram_Body (K : Iir_Kind) return Boolean;
   function Has_Overload_Number (K : Iir_Kind) return Boolean;
   function Has_Subprogram_Depth (K : Iir_Kind) return Boolean;
   function Has_Subprogram_Hash (K : Iir_Kind) return Boolean;
   function Has_Impure_Depth (K : Iir_Kind) return Boolean;
   function Has_Return_Type (K : Iir_Kind) return Boolean;
   function Has_Implicit_Definition (K : Iir_Kind) return Boolean;
   function Has_Uninstantiated_Subprogram_Name (K : Iir_Kind)
      return Boolean;
   function Has_Default_Value (K : Iir_Kind) return Boolean;
   function Has_Deferred_Declaration (K : Iir_Kind) return Boolean;
   function Has_Deferred_Declaration_Flag (K : Iir_Kind) return Boolean;
   function Has_Shared_Flag (K : Iir_Kind) return Boolean;
   function Has_Design_Unit (K : Iir_Kind) return Boolean;
   function Has_Block_Statement (K : Iir_Kind) return Boolean;
   function Has_Signal_Driver (K : Iir_Kind) return Boolean;
   function Has_Declaration_Chain (K : Iir_Kind) return Boolean;
   function Has_File_Logical_Name (K : Iir_Kind) return Boolean;
   function Has_File_Open_Kind (K : Iir_Kind) return Boolean;
   function Has_Element_Position (K : Iir_Kind) return Boolean;
   function Has_Use_Clause_Chain (K : Iir_Kind) return Boolean;
   function Has_Context_Reference_Chain (K : Iir_Kind) return Boolean;
   function Has_Inherit_Spec_Chain (K : Iir_Kind) return Boolean;
   function Has_Selected_Name (K : Iir_Kind) return Boolean;
   function Has_Type_Declarator (K : Iir_Kind) return Boolean;
   function Has_Complete_Type_Definition (K : Iir_Kind) return Boolean;
   function Has_Incomplete_Type_Ref_Chain (K : Iir_Kind) return Boolean;
   function Has_Associated_Type (K : Iir_Kind) return Boolean;
   function Has_Enumeration_Literal_List (K : Iir_Kind) return Boolean;
   function Has_Entity_Class_Entry_Chain (K : Iir_Kind) return Boolean;
   function Has_Group_Constituent_List (K : Iir_Kind) return Boolean;
   function Has_Unit_Chain (K : Iir_Kind) return Boolean;
   function Has_Primary_Unit (K : Iir_Kind) return Boolean;
   function Has_Identifier (K : Iir_Kind) return Boolean;
   function Has_Label (K : Iir_Kind) return Boolean;
   function Has_Return_Identifier (K : Iir_Kind) return Boolean;
   function Has_Visible_Flag (K : Iir_Kind) return Boolean;
   function Has_Range_Constraint (K : Iir_Kind) return Boolean;
   function Has_Direction (K : Iir_Kind) return Boolean;
   function Has_Left_Limit (K : Iir_Kind) return Boolean;
   function Has_Right_Limit (K : Iir_Kind) return Boolean;
   function Has_Left_Limit_Expr (K : Iir_Kind) return Boolean;
   function Has_Right_Limit_Expr (K : Iir_Kind) return Boolean;
   function Has_Parent_Type (K : Iir_Kind) return Boolean;
   function Has_Simple_Nature (K : Iir_Kind) return Boolean;
   function Has_Base_Nature (K : Iir_Kind) return Boolean;
   function Has_Resolution_Indication (K : Iir_Kind) return Boolean;
   function Has_Record_Element_Resolution_Chain (K : Iir_Kind)
      return Boolean;
   function Has_Tolerance (K : Iir_Kind) return Boolean;
   function Has_Plus_Terminal_Name (K : Iir_Kind) return Boolean;
   function Has_Minus_Terminal_Name (K : Iir_Kind) return Boolean;
   function Has_Plus_Terminal (K : Iir_Kind) return Boolean;
   function Has_Minus_Terminal (K : Iir_Kind) return Boolean;
   function Has_Magnitude_Expression (K : Iir_Kind) return Boolean;
   function Has_Phase_Expression (K : Iir_Kind) return Boolean;
   function Has_Power_Expression (K : Iir_Kind) return Boolean;
   function Has_Simultaneous_Left (K : Iir_Kind) return Boolean;
   function Has_Simultaneous_Right (K : Iir_Kind) return Boolean;
   function Has_Text_File_Flag (K : Iir_Kind) return Boolean;
   function Has_Only_Characters_Flag (K : Iir_Kind) return Boolean;
   function Has_Is_Character_Type (K : Iir_Kind) return Boolean;
   function Has_Nature_Staticness (K : Iir_Kind) return Boolean;
   function Has_Type_Staticness (K : Iir_Kind) return Boolean;
   function Has_Constraint_State (K : Iir_Kind) return Boolean;
   function Has_Index_Subtype_List (K : Iir_Kind) return Boolean;
   function Has_Index_Subtype_Definition_List (K : Iir_Kind)
      return Boolean;
   function Has_Element_Subtype_Indication (K : Iir_Kind) return Boolean;
   function Has_Element_Subtype (K : Iir_Kind) return Boolean;
   function Has_Element_Subnature_Indication (K : Iir_Kind) return Boolean;
   function Has_Element_Subnature (K : Iir_Kind) return Boolean;
   function Has_Index_Constraint_List (K : Iir_Kind) return Boolean;
   function Has_Array_Element_Constraint (K : Iir_Kind) return Boolean;
   function Has_Has_Array_Constraint_Flag (K : Iir_Kind) return Boolean;
   function Has_Has_Element_Constraint_Flag (K : Iir_Kind) return Boolean;
   function Has_Elements_Declaration_List (K : Iir_Kind) return Boolean;
   function Has_Owned_Elements_Chain (K : Iir_Kind) return Boolean;
   function Has_Designated_Type (K : Iir_Kind) return Boolean;
   function Has_Designated_Subtype_Indication (K : Iir_Kind)
      return Boolean;
   function Has_Index_List (K : Iir_Kind) return Boolean;
   function Has_Reference (K : Iir_Kind) return Boolean;
   function Has_Nature_Declarator (K : Iir_Kind) return Boolean;
   function Has_Across_Type_Mark (K : Iir_Kind) return Boolean;
   function Has_Through_Type_Mark (K : Iir_Kind) return Boolean;
   function Has_Across_Type_Definition (K : Iir_Kind) return Boolean;
   function Has_Through_Type_Definition (K : Iir_Kind) return Boolean;
   function Has_Across_Type (K : Iir_Kind) return Boolean;
   function Has_Through_Type (K : Iir_Kind) return Boolean;
   function Has_Target (K : Iir_Kind) return Boolean;
   function Has_Waveform_Chain (K : Iir_Kind) return Boolean;
   function Has_Guard (K : Iir_Kind) return Boolean;
   function Has_Delay_Mechanism (K : Iir_Kind) return Boolean;
   function Has_Reject_Time_Expression (K : Iir_Kind) return Boolean;
   function Has_Force_Mode (K : Iir_Kind) return Boolean;
   function Has_Has_Force_Mode (K : Iir_Kind) return Boolean;
   function Has_Sensitivity_List (K : Iir_Kind) return Boolean;
   function Has_Process_Origin (K : Iir_Kind) return Boolean;
   function Has_Package_Origin (K : Iir_Kind) return Boolean;
   function Has_Condition_Clause (K : Iir_Kind) return Boolean;
   function Has_Break_Element (K : Iir_Kind) return Boolean;
   function Has_Selector_Quantity (K : Iir_Kind) return Boolean;
   function Has_Break_Quantity (K : Iir_Kind) return Boolean;
   function Has_Timeout_Clause (K : Iir_Kind) return Boolean;
   function Has_Postponed_Flag (K : Iir_Kind) return Boolean;
   function Has_Callees_List (K : Iir_Kind) return Boolean;
   function Has_Passive_Flag (K : Iir_Kind) return Boolean;
   function Has_Resolution_Function_Flag (K : Iir_Kind) return Boolean;
   function Has_Wait_State (K : Iir_Kind) return Boolean;
   function Has_All_Sensitized_State (K : Iir_Kind) return Boolean;
   function Has_Seen_Flag (K : Iir_Kind) return Boolean;
   function Has_Pure_Flag (K : Iir_Kind) return Boolean;
   function Has_Foreign_Flag (K : Iir_Kind) return Boolean;
   function Has_Resolved_Flag (K : Iir_Kind) return Boolean;
   function Has_Signal_Type_Flag (K : Iir_Kind) return Boolean;
   function Has_Has_Signal_Flag (K : Iir_Kind) return Boolean;
   function Has_Purity_State (K : Iir_Kind) return Boolean;
   function Has_Elab_Flag (K : Iir_Kind) return Boolean;
   function Has_Vendor_Library_Flag (K : Iir_Kind) return Boolean;
   function Has_Configuration_Mark_Flag (K : Iir_Kind) return Boolean;
   function Has_Configuration_Done_Flag (K : Iir_Kind) return Boolean;
   function Has_Index_Constraint_Flag (K : Iir_Kind) return Boolean;
   function Has_Hide_Implicit_Flag (K : Iir_Kind) return Boolean;
   function Has_Assertion_Condition (K : Iir_Kind) return Boolean;
   function Has_Report_Expression (K : Iir_Kind) return Boolean;
   function Has_Severity_Expression (K : Iir_Kind) return Boolean;
   function Has_Instantiated_Unit (K : Iir_Kind) return Boolean;
   function Has_Instantiated_Header (K : Iir_Kind) return Boolean;
   function Has_Generic_Map_Aspect_Chain (K : Iir_Kind) return Boolean;
   function Has_Port_Map_Aspect_Chain (K : Iir_Kind) return Boolean;
   function Has_Configuration_Name (K : Iir_Kind) return Boolean;
   function Has_Component_Configuration (K : Iir_Kind) return Boolean;
   function Has_Configuration_Specification (K : Iir_Kind) return Boolean;
   function Has_Default_Binding_Indication (K : Iir_Kind) return Boolean;
   function Has_Default_Configuration_Declaration (K : Iir_Kind)
      return Boolean;
   function Has_Expression (K : Iir_Kind) return Boolean;
   function Has_Conditional_Expression_Chain (K : Iir_Kind) return Boolean;
   function Has_Allocator_Designated_Type (K : Iir_Kind) return Boolean;
   function Has_Selected_Waveform_Chain (K : Iir_Kind) return Boolean;
   function Has_Conditional_Waveform_Chain (K : Iir_Kind) return Boolean;
   function Has_Guard_Expression (K : Iir_Kind) return Boolean;
   function Has_Guard_Decl (K : Iir_Kind) return Boolean;
   function Has_Guard_Sensitivity_List (K : Iir_Kind) return Boolean;
   function Has_Attribute_Implicit_Chain (K : Iir_Kind) return Boolean;
   function Has_Block_Block_Configuration (K : Iir_Kind) return Boolean;
   function Has_Package_Header (K : Iir_Kind) return Boolean;
   function Has_Block_Header (K : Iir_Kind) return Boolean;
   function Has_Uninstantiated_Package_Name (K : Iir_Kind) return Boolean;
   function Has_Uninstantiated_Package_Decl (K : Iir_Kind) return Boolean;
   function Has_Instance_Source_File (K : Iir_Kind) return Boolean;
   function Has_Generate_Block_Configuration (K : Iir_Kind) return Boolean;
   function Has_Generate_Statement_Body (K : Iir_Kind) return Boolean;
   function Has_Alternative_Label (K : Iir_Kind) return Boolean;
   function Has_Generate_Else_Clause (K : Iir_Kind) return Boolean;
   function Has_Condition (K : Iir_Kind) return Boolean;
   function Has_Else_Clause (K : Iir_Kind) return Boolean;
   function Has_Parameter_Specification (K : Iir_Kind) return Boolean;
   function Has_Parent (K : Iir_Kind) return Boolean;
   function Has_Loop_Label (K : Iir_Kind) return Boolean;
   function Has_Exit_Flag (K : Iir_Kind) return Boolean;
   function Has_Next_Flag (K : Iir_Kind) return Boolean;
   function Has_Component_Name (K : Iir_Kind) return Boolean;
   function Has_Instantiation_List (K : Iir_Kind) return Boolean;
   function Has_Entity_Aspect (K : Iir_Kind) return Boolean;
   function Has_Default_Entity_Aspect (K : Iir_Kind) return Boolean;
   function Has_Binding_Indication (K : Iir_Kind) return Boolean;
   function Has_Named_Entity (K : Iir_Kind) return Boolean;
   function Has_Referenced_Name (K : Iir_Kind) return Boolean;
   function Has_Expr_Staticness (K : Iir_Kind) return Boolean;
   function Has_Scalar_Size (K : Iir_Kind) return Boolean;
   function Has_Error_Origin (K : Iir_Kind) return Boolean;
   function Has_Operand (K : Iir_Kind) return Boolean;
   function Has_Left (K : Iir_Kind) return Boolean;
   function Has_Right (K : Iir_Kind) return Boolean;
   function Has_Unit_Name (K : Iir_Kind) return Boolean;
   function Has_Name (K : Iir_Kind) return Boolean;
   function Has_Group_Template_Name (K : Iir_Kind) return Boolean;
   function Has_Name_Staticness (K : Iir_Kind) return Boolean;
   function Has_Prefix (K : Iir_Kind) return Boolean;
   function Has_Signature_Prefix (K : Iir_Kind) return Boolean;
   function Has_External_Pathname (K : Iir_Kind) return Boolean;
   function Has_Pathname_Suffix (K : Iir_Kind) return Boolean;
   function Has_Pathname_Expression (K : Iir_Kind) return Boolean;
   function Has_In_Formal_Flag (K : Iir_Kind) return Boolean;
   function Has_Inertial_Flag (K : Iir_Kind) return Boolean;
   function Has_Slice_Subtype (K : Iir_Kind) return Boolean;
   function Has_Suffix (K : Iir_Kind) return Boolean;
   function Has_Index_Subtype (K : Iir_Kind) return Boolean;
   function Has_Parameter (K : Iir_Kind) return Boolean;
   function Has_Parameter_2 (K : Iir_Kind) return Boolean;
   function Has_Parameter_3 (K : Iir_Kind) return Boolean;
   function Has_Parameter_4 (K : Iir_Kind) return Boolean;
   function Has_Attr_Chain (K : Iir_Kind) return Boolean;
   function Has_Attribute_Implicit_Declaration (K : Iir_Kind)
      return Boolean;
   function Has_Actual_Type (K : Iir_Kind) return Boolean;
   function Has_Actual_Type_Definition (K : Iir_Kind) return Boolean;
   function Has_Association_Chain (K : Iir_Kind) return Boolean;
   function Has_Individual_Association_Chain (K : Iir_Kind) return Boolean;
   function Has_Subprogram_Association_Chain (K : Iir_Kind) return Boolean;
   function Has_Aggregate_Info (K : Iir_Kind) return Boolean;
   function Has_Sub_Aggregate_Info (K : Iir_Kind) return Boolean;
   function Has_Aggr_Dynamic_Flag (K : Iir_Kind) return Boolean;
   function Has_Aggr_Min_Length (K : Iir_Kind) return Boolean;
   function Has_Aggr_Low_Limit (K : Iir_Kind) return Boolean;
   function Has_Aggr_High_Limit (K : Iir_Kind) return Boolean;
   function Has_Aggr_Others_Flag (K : Iir_Kind) return Boolean;
   function Has_Aggr_Named_Flag (K : Iir_Kind) return Boolean;
   function Has_Aggregate_Expand_Flag (K : Iir_Kind) return Boolean;
   function Has_Determined_Aggregate_Flag (K : Iir_Kind) return Boolean;
   function Has_Association_Choices_Chain (K : Iir_Kind) return Boolean;
   function Has_Case_Statement_Alternative_Chain (K : Iir_Kind)
      return Boolean;
   function Has_Matching_Flag (K : Iir_Kind) return Boolean;
   function Has_Choice_Staticness (K : Iir_Kind) return Boolean;
   function Has_Procedure_Call (K : Iir_Kind) return Boolean;
   function Has_Implementation (K : Iir_Kind) return Boolean;
   function Has_Parameter_Association_Chain (K : Iir_Kind) return Boolean;
   function Has_Method_Object (K : Iir_Kind) return Boolean;
   function Has_Subtype_Type_Mark (K : Iir_Kind) return Boolean;
   function Has_Subnature_Nature_Mark (K : Iir_Kind) return Boolean;
   function Has_Type_Conversion_Subtype (K : Iir_Kind) return Boolean;
   function Has_Type_Mark (K : Iir_Kind) return Boolean;
   function Has_File_Type_Mark (K : Iir_Kind) return Boolean;
   function Has_Return_Type_Mark (K : Iir_Kind) return Boolean;
   function Has_Has_Disconnect_Flag (K : Iir_Kind) return Boolean;
   function Has_Has_Active_Flag (K : Iir_Kind) return Boolean;
   function Has_Is_Within_Flag (K : Iir_Kind) return Boolean;
   function Has_Type_Marks_List (K : Iir_Kind) return Boolean;
   function Has_Implicit_Alias_Flag (K : Iir_Kind) return Boolean;
   function Has_Alias_Signature (K : Iir_Kind) return Boolean;
   function Has_Attribute_Signature (K : Iir_Kind) return Boolean;
   function Has_Overload_List (K : Iir_Kind) return Boolean;
   function Has_Simple_Name_Identifier (K : Iir_Kind) return Boolean;
   function Has_Simple_Name_Subtype (K : Iir_Kind) return Boolean;
   function Has_Protected_Type_Body (K : Iir_Kind) return Boolean;
   function Has_Protected_Type_Declaration (K : Iir_Kind) return Boolean;
   function Has_Use_Flag (K : Iir_Kind) return Boolean;
   function Has_End_Has_Reserved_Id (K : Iir_Kind) return Boolean;
   function Has_End_Has_Identifier (K : Iir_Kind) return Boolean;
   function Has_End_Has_Postponed (K : Iir_Kind) return Boolean;
   function Has_Has_Label (K : Iir_Kind) return Boolean;
   function Has_Has_Begin (K : Iir_Kind) return Boolean;
   function Has_Has_End (K : Iir_Kind) return Boolean;
   function Has_Has_Is (K : Iir_Kind) return Boolean;
   function Has_Has_Pure (K : Iir_Kind) return Boolean;
   function Has_Has_Body (K : Iir_Kind) return Boolean;
   function Has_Has_Parameter (K : Iir_Kind) return Boolean;
   function Has_Has_Component (K : Iir_Kind) return Boolean;
   function Has_Has_Identifier_List (K : Iir_Kind) return Boolean;
   function Has_Has_Mode (K : Iir_Kind) return Boolean;
   function Has_Has_Class (K : Iir_Kind) return Boolean;
   function Has_Has_Delay_Mechanism (K : Iir_Kind) return Boolean;
   function Has_Suspend_Flag (K : Iir_Kind) return Boolean;
   function Has_Stop_Flag (K : Iir_Kind) return Boolean;
   function Has_Is_Ref (K : Iir_Kind) return Boolean;
   function Has_Is_Forward_Ref (K : Iir_Kind) return Boolean;
   function Has_Psl_Property (K : Iir_Kind) return Boolean;
   function Has_Psl_Sequence (K : Iir_Kind) return Boolean;
   function Has_Psl_Declaration (K : Iir_Kind) return Boolean;
   function Has_Psl_Expression (K : Iir_Kind) return Boolean;
   function Has_Psl_Boolean (K : Iir_Kind) return Boolean;
   function Has_PSL_Clock (K : Iir_Kind) return Boolean;
   function Has_PSL_NFA (K : Iir_Kind) return Boolean;
   function Has_PSL_Nbr_States (K : Iir_Kind) return Boolean;
   function Has_PSL_Clock_Sensitivity (K : Iir_Kind) return Boolean;
   function Has_PSL_EOS_Flag (K : Iir_Kind) return Boolean;
   function Has_PSL_Abort_Flag (K : Iir_Kind) return Boolean;
   function Has_Count_Expression (K : Iir_Kind) return Boolean;
   function Has_Clock_Expression (K : Iir_Kind) return Boolean;
   function Has_Default_Clock (K : Iir_Kind) return Boolean;
   function Has_Foreign_Node (K : Iir_Kind) return Boolean;
   function Has_Suspend_State_Index (K : Iir_Kind) return Boolean;
   function Has_Suspend_State_Chain (K : Iir_Kind) return Boolean;
end Vhdl.Nodes_Meta;
