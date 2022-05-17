--  Extended locations for iir nodes
--  Copyright (C) 2017 Tristan Gingold
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

package Vhdl.Elocations is

   -- Start of Iir_Kind.

   -- Iir_Kind_Design_File (None)

   -- Iir_Kind_Design_Unit (None)

   -- Iir_Kind_Library_Clause (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_String_Literal8 (None)

   -- Iir_Kind_Integer_Literal (None)

   -- Iir_Kind_Floating_Point_Literal (None)

   -- Iir_Kind_Null_Literal (None)

   -- Iir_Kind_Physical_Int_Literal (None)
   -- Iir_Kind_Physical_Fp_Literal (None)

   -- Iir_Kind_Simple_Aggregate (None)

   -- Iir_Kind_Overflow_Literal (None)

   -- Iir_Kind_Unaffected_Waveform (None)

   -------------
   --  Tuples --
   -------------

   -- Iir_Kind_Association_Element_By_Expression (L1)
   -- Iir_Kind_Association_Element_By_Name (L1)
   -- Iir_Kind_Association_Element_Open (L1)
   -- Iir_Kind_Association_Element_By_Individual (L1)
   -- Iir_Kind_Association_Element_Package (L1)
   -- Iir_Kind_Association_Element_Type (L1)
   -- Iir_Kind_Association_Element_Subprogram (L1)
   -- Iir_Kind_Association_Element_Terminal (L1)
   --
   --   Get/Set_Arrow_Location (Field1)

   -- Iir_Kind_Waveform_Element (None)

   -- Iir_Kind_Conditional_Waveform (None)

   -- Iir_Kind_Conditional_Expression (None)

   -- Iir_Kind_Choice_By_Others (None)
   -- Iir_Kind_Choice_By_None (None)
   -- Iir_Kind_Choice_By_Range (None)
   -- Iir_Kind_Choice_By_Name (None)
   -- Iir_Kind_Choice_By_Expression (None)

   -- Iir_Kind_Entity_Aspect_Entity (None)

   -- Iir_Kind_Entity_Aspect_Open (None)

   -- Iir_Kind_Entity_Aspect_Configuration (None)

   -- Iir_Kind_Psl_Hierarchical_Name (None)

   -- Iir_Kind_Block_Configuration (None)

   -- Iir_Kind_Binding_Indication (None)

   -- Iir_Kind_Component_Configuration (None)
   -- Iir_Kind_Configuration_Specification (None)

   -- Iir_Kind_Disconnection_Specification (None)
   -- Iir_Kind_Step_Limit_Specification (None)

   -- Iir_Kind_Block_Header (L6)
   --
   --   Get/Set_Generic_Location (Field5)
   --
   --   Get/Set_Port_Location (Field6)
   --
   --   Get/Set_Generic_Map_Location (Field3)
   --
   --   Get/Set_Port_Map_Location (Field2)

   -- Iir_Kind_Entity_Class (None)

   -- Iir_Kind_Attribute_Specification (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Attribute_Value (None)

   -- Iir_Kind_Psl_Expression (None)
   -- Iir_Kind_Psl_Prev (None)
   -- Iir_Kind_Psl_Stable (None)
   -- Iir_Kind_Psl_Rose (None)
   -- Iir_Kind_Psl_Fell (None)
   -- Iir_Kind_Psl_Onehot (None)
   -- Iir_Kind_Psl_Onehot0 (None)

   -- Iir_Kind_Signature (None)

   -- Iir_Kind_Overload_List (None)

   -------------------
   --  Declarations --
   -------------------

   -- Iir_Kind_Foreign_Module (None)

   -- Iir_Kind_Entity_Declaration (L6)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Generic_Location (Field5)
   --
   --   Get/Set_Port_Location (Field6)
   --
   --   Get/Set_Begin_Location (Field3)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Architecture_Body (L4)
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Begin_Location (Field3)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Configuration_Declaration (L2)
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Package_Header (L5)
   --
   --   Get/Set_Generic_Location (Field5)
   --
   --   Get/Set_Generic_Map_Location (Field3)

   -- Iir_Kind_Package_Declaration (L2)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Package_Body (L2)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Package_Instantiation_Declaration (L3)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --  Correspond to the final ';'.
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Generic_Map_Location (Field3)

   -- Iir_Kind_Context_Declaration (L2)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Vunit_Declaration (None)
   -- Iir_Kind_Vmode_Declaration (None)
   -- Iir_Kind_Vprop_Declaration (None)

   -- Iir_Kind_Library_Declaration (None)

   -- Iir_Kind_Component_Declaration (L6)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Generic_Location (Field5)
   --
   --   Get/Set_Port_Location (Field6)

   -- Iir_Kind_Object_Alias_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Non_Object_Alias_Declaration (None)

   -- Iir_Kind_Anonymous_Type_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Type_Declaration (L4)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Subtype_Declaration (L4)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Nature_Declaration (None)

   -- Iir_Kind_Subnature_Declaration (None)

   -- Iir_Kind_Interface_Signal_Declaration (L3)
   -- Iir_Kind_Interface_Constant_Declaration (L3)
   -- Iir_Kind_Interface_Variable_Declaration (L3)
   -- Iir_Kind_Interface_File_Declaration (L3)
   -- Iir_Kind_Interface_Quantity_Declaration (L3)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_Colon_Location (Field2)
   --
   --   Get/Set_Assign_Location (Field3)

   -- Iir_Kind_Interface_Type_Declaration (L1)
   -- Iir_Kind_Interface_Terminal_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Interface_Package_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Function_Declaration (L1)
   -- Iir_Kind_Procedure_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Function_Body (L4)
   -- Iir_Kind_Procedure_Body (L4)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Begin_Location (Field3)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Function_Instantiation_Declaration (None)
   -- Iir_Kind_Procedure_Instantiation_Declaration (None)

   -- Iir_Kind_Interface_Function_Declaration (None)
   -- Iir_Kind_Interface_Procedure_Declaration (None)

   -- Iir_Kind_Signal_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Guard_Signal_Declaration (None)

   -- Iir_Kind_Signal_Attribute_Declaration (None)
   -- Iir_Kind_Suspend_State_Declaration (None)

   -- Iir_Kind_Constant_Declaration (L1)
   -- Iir_Kind_Iterator_Declaration (L1)
   -- Iir_Kind_Variable_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_File_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Element_Declaration (None)
   -- Iir_Kind_Nature_Element_Declaration (None)

   -- Iir_Kind_Record_Resolution (None)

   -- Iir_Kind_Record_Element_Constraint (None)

   -- Iir_Kind_Attribute_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Group_Template_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Group_Declaration (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Psl_Endpoint_Declaration (None)

   -- Iir_Kind_Psl_Declaration (None)

   -- Iir_Kind_Terminal_Declaration (None)

   -- Iir_Kind_Free_Quantity_Declaration (None)
   -- Iir_Kind_Spectrum_Quantity_Declaration (None)
   -- Iir_Kind_Noise_Quantity_Declaration (None)

   -- Iir_Kind_Across_Quantity_Declaration (None)
   -- Iir_Kind_Through_Quantity_Declaration (None)

   -- Iir_Kind_Use_Clause (None)

   -- Iir_Kind_Context_Reference (None)

   -- Iir_Kind_PSL_Inherit_Spec (None)

   -----------------------
   --  type definitions --
   -----------------------

   -- Iir_Kind_Enumeration_Type_Definition (None)

   -- Iir_Kind_Enumeration_Literal (None)

   -- Iir_Kind_Physical_Type_Definition (None)

   -- Iir_Kind_Unit_Declaration (None)

   -- Iir_Kind_Integer_Type_Definition (None)
   -- Iir_Kind_Floating_Type_Definition (None)

   -- Iir_Kind_Array_Type_Definition (None)
   -- Iir_Kind_Array_Nature_Definition (None)

   -- Iir_Kind_Record_Type_Definition (L2)
   -- Iir_Kind_Record_Nature_Definition (L2)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Access_Type_Definition (None)

   -- Iir_Kind_File_Type_Definition (None)

   -- Iir_Kind_Incomplete_Type_Definition (None)

   -- Iir_Kind_Interface_Type_Definition (None)

   -- Iir_Kind_Protected_Type_Declaration (L2)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Protected_Type_Body (L2)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Wildcard_Type_Definition (None)

   -- Iir_Kind_Foreign_Vector_Type_Definition (None)

   --------------------------
   --  subtype definitions --
   --------------------------

   -- Iir_Kind_Enumeration_Subtype_Definition (None)
   -- Iir_Kind_Integer_Subtype_Definition (None)
   -- Iir_Kind_Physical_Subtype_Definition (None)

   -- Iir_Kind_Floating_Subtype_Definition (None)

   -- Iir_Kind_Access_Subtype_Definition (None)

   -- Iir_Kind_Array_Element_Resolution (None)

   -- Iir_Kind_Record_Element_Resolution (None)

   -- Iir_Kind_Record_Subtype_Definition (None)

   -- Iir_Kind_Array_Subtype_Definition (None)
   -- Iir_Kind_Array_Subnature_Definition (None)

   -- Iir_Kind_Range_Expression (None)

   -- Iir_Kind_Subtype_Definition (None)

   -------------------------
   --  Nature definitions --
   -------------------------

   -- Iir_Kind_Scalar_Nature_Definition (None)

   ----------------------------
   --  concurrent statements --
   ----------------------------

   -- Iir_Kind_Concurrent_Conditional_Signal_Assignment (L1)
   -- Iir_Kind_Concurrent_Selected_Signal_Assignment (L1)
   -- Iir_Kind_Concurrent_Simple_Signal_Assignment (L1)
   --
   --   Get/Set_Start_Location (Field1)

   -- Iir_Kind_Signal_Force_Assignment_Statement (None)
   -- Iir_Kind_Signal_Release_Assignment_Statement (None)

   -- Iir_Kind_Sensitized_Process_Statement (L4)
   -- Iir_Kind_Process_Statement (L4)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Begin_Location (Field3)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Concurrent_Assertion_Statement (None)

   -- Iir_Kind_Concurrent_Break_Statement (None)

   -- Iir_Kind_Psl_Default_Clock (None)

   -- Iir_Kind_Psl_Assert_Directive (None)
   -- Iir_Kind_Psl_Assume_Directive (None)
   -- Iir_Kind_Psl_Cover_Directive (None)
   -- Iir_Kind_Psl_Restrict_Directive (None)

   -- Iir_Kind_Component_Instantiation_Statement (L3)
   --
   --   Get/Set_Generic_Map_Location (Field3)
   --
   --   Get/Set_Port_Map_Location (Field2)

   -- Iir_Kind_Block_Statement (L4)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Begin_Location (Field3)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Generate_Statement_Body (L3)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Begin_Location (Field3)

   -- Iir_Kind_For_Generate_Statement (L3)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Generate_Location (Field3)

   -- Iir_Kind_If_Generate_Else_Clause (L3)
   -- Iir_Kind_If_Generate_Statement (L3)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Generate_Location (Field3)

   -- Iir_Kind_Case_Generate_Statement (None)

   -- Iir_Kind_Simple_Simultaneous_Statement (None)
   -- Iir_Kind_Simultaneous_Null_Statement (None)

   -- Iir_Kind_Simultaneous_Procedural_Statement (L4)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Begin_Location (Field3)
   --
   --   Get/Set_Is_Location (Field4)

   -- Iir_Kind_Simultaneous_If_Statement (L3)
   -- Iir_Kind_Simultaneous_Elsif (L3)
   --
   --  Location of 'if', 'else' or 'elsif'.
   --   Get/Set_Start_Location (Field1)
   --
   --  Location of the next 'elsif', 'else' or 'end if'.
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Use_Location (Field3)

   ----------------------------
   --  sequential statements --
   ----------------------------

   -- Iir_Kind_If_Statement (L3)
   -- Iir_Kind_Elsif (L3)
   --
   --  Location of 'if', 'else' or 'elsif'.
   --   Get/Set_Start_Location (Field1)
   --
   --  Location of the next 'elsif', 'else' or 'end if'.
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Then_Location (Field3)

   -- Iir_Kind_For_Loop_Statement (L3)
   -- Iir_Kind_While_Loop_Statement (L3)
   --
   --   Get/Set_Start_Location (Field1)
   --
   --   Get/Set_End_Location (Field2)
   --
   --   Get/Set_Loop_Location (Field3)

   -- Iir_Kind_Exit_Statement (None)
   -- Iir_Kind_Next_Statement (None)

   -- Iir_Kind_Simple_Signal_Assignment_Statement (None)
   -- Iir_Kind_Conditional_Signal_Assignment_Statement (None)
   -- Iir_Kind_Selected_Waveform_Assignment_Statement (None)

   -- Iir_Kind_Variable_Assignment_Statement (None)

   -- Iir_Kind_Conditional_Variable_Assignment_Statement (None)

   -- Iir_Kind_Assertion_Statement (None)

   -- Iir_Kind_Report_Statement (None)

   -- Iir_Kind_Wait_Statement (None)

   -- Iir_Kind_Return_Statement (None)

   -- Iir_Kind_Case_Statement (L2)
   -- Iir_Kind_Simultaneous_Case_Statement (L2)
   --
   --   Get/Set_End_Location (Field2)

   -- Iir_Kind_Procedure_Call_Statement (None)
   -- Iir_Kind_Concurrent_Procedure_Call_Statement (None)

   -- Iir_Kind_Procedure_Call (None)

   -- Iir_Kind_Null_Statement (None)

   -- Iir_Kind_Break_Statement (None)

   -- Iir_Kind_Break_Element (None)

   -- Iir_Kind_Suspend_State_Statement (None)

   ----------------
   --  operators --
   ----------------

   -- Iir_Kinds_Monadic_Operator (None)

   -- Iir_Kinds_Dyadic_Operator (None)

   -- Iir_Kind_Function_Call (None)

   -- Iir_Kind_Aggregate (None)

   -- Iir_Kind_Aggregate_Info (None)

   -- Iir_Kind_Parenthesis_Expression (L1)
   --
   --   Get/Set_Right_Paren_Location (Field1)

   -- Iir_Kind_Qualified_Expression (None)

   -- Iir_Kind_Type_Conversion (None)

   -- Iir_Kind_Allocator_By_Expression (None)
   -- Iir_Kind_Allocator_By_Subtype (None)

   ------------
   --  Names --
   ------------

   -- Iir_Kind_Simple_Name (None)
   -- Iir_Kind_Character_Literal (None)

   -- Iir_Kind_Operator_Symbol (None)

   -- Iir_Kind_Reference_Name (None)

   -- Iir_Kind_Selected_Name (None)

   -- Iir_Kind_Selected_By_All_Name (None)

   -- Iir_Kind_Indexed_Name (None)

   -- Iir_Kind_Slice_Name (None)

   -- Iir_Kind_Parenthesis_Name (None)

   -- Iir_Kind_Selected_Element (None)

   -- Iir_Kind_Implicit_Dereference (None)
   -- Iir_Kind_Dereference (None)

   -- Iir_Kind_External_Constant_Name (None)
   -- Iir_Kind_External_Signal_Name (None)
   -- Iir_Kind_External_Variable_Name (None)

   -- Iir_Kind_Package_Pathname (None)

   -- Iir_Kind_Absolute_Pathname (None)

   -- Iir_Kind_Relative_Pathname (None)

   -- Iir_Kind_Pathname_Element (None)

   -----------------
   --  Attributes --
   -----------------

   -- Iir_Kind_Attribute_Name (None)

   -- Iir_Kind_Base_Attribute (None)
   -- Iir_Kind_Across_Attribute (None)
   -- Iir_Kind_Through_Attribute (None)
   -- Iir_Kind_Nature_Reference_Attribute (None)
   -- Iir_Kind_Left_Type_Attribute (None)
   -- Iir_Kind_Right_Type_Attribute (None)
   -- Iir_Kind_High_Type_Attribute (None)
   -- Iir_Kind_Low_Type_Attribute (None)
   -- Iir_Kind_Ascending_Type_Attribute (None)

   -- Iir_Kind_Range_Array_Attribute (None)
   -- Iir_Kind_Reverse_Range_Array_Attribute (None)
   -- Iir_Kind_Left_Array_Attribute (None)
   -- Iir_Kind_Right_Array_Attribute (None)
   -- Iir_Kind_High_Array_Attribute (None)
   -- Iir_Kind_Low_Array_Attribute (None)
   -- Iir_Kind_Ascending_Array_Attribute (None)
   -- Iir_Kind_Length_Array_Attribute (None)

   -- Iir_Kind_Subtype_Attribute (None)
   -- Iir_Kind_Element_Attribute (None)

   -- Iir_Kind_Signal_Slew_Attribute (None)
   -- Iir_Kind_Quantity_Slew_Attribute (None)
   -- Iir_Kind_Zoh_Attribute (None)
   -- Iir_Kind_Ltf_Attribute (None)
   -- Iir_Kind_Ztf_Attribute (None)
   -- Iir_Kind_Dot_Attribute (None)
   -- Iir_Kind_Integ_Attribute (None)
   -- Iir_Kind_Quantity_Delayed_Attribute (None)

   -- Iir_Kind_Ramp_Attribute (None)
   -- Iir_Kind_Above_Attribute (None)
   -- Iir_Kind_Stable_Attribute (None)
   -- Iir_Kind_Delayed_Attribute (None)
   -- Iir_Kind_Quiet_Attribute (None)
   -- Iir_Kind_Transaction_Attribute (None)

   -- Iir_Kind_Event_Attribute (None)
   -- Iir_Kind_Last_Event_Attribute (None)
   -- Iir_Kind_Last_Value_Attribute (None)
   -- Iir_Kind_Active_Attribute (None)
   -- Iir_Kind_Last_Active_Attribute (None)
   -- Iir_Kind_Driving_Attribute (None)
   -- Iir_Kind_Driving_Value_Attribute (None)

   -- Iir_Kind_Pos_Attribute (None)
   -- Iir_Kind_Val_Attribute (None)
   -- Iir_Kind_Succ_Attribute (None)
   -- Iir_Kind_Pred_Attribute (None)
   -- Iir_Kind_Leftof_Attribute (None)
   -- Iir_Kind_Rightof_Attribute (None)

   -- Iir_Kind_Image_Attribute (None)
   -- Iir_Kind_Value_Attribute (None)

   -- Iir_Kind_Simple_Name_Attribute (None)
   -- Iir_Kind_Instance_Name_Attribute (None)
   -- Iir_Kind_Path_Name_Attribute (None)

   -- Iir_Kind_Behavior_Attribute (None)
   -- Iir_Kind_Structure_Attribute (None)
   --  FIXME: to describe (None)

   -- Iir_Kind_Error (None)

   -- Iir_Kind_Unused (None)

   -- End of Iir_Kind.

   --  Allocate memory to store elocations for node N.  Must be called once.
   procedure Create_Elocations (N : Iir);

   -- General methods.

   --  Field: Field1
   function Get_Start_Location (N : Iir) return Location_Type;
   procedure Set_Start_Location (N : Iir; Loc : Location_Type);

   --  Field: Field1
   function Get_Right_Paren_Location (N : Iir) return Location_Type;
   procedure Set_Right_Paren_Location (N : Iir; Loc : Location_Type);

   --  Field: Field2
   function Get_End_Location (N : Iir) return Location_Type;
   procedure Set_End_Location (N : Iir; Loc : Location_Type);

   --  Field: Field4
   function Get_Is_Location (N : Iir) return Location_Type;
   procedure Set_Is_Location (N : Iir; Loc : Location_Type);

   --  Field: Field3
   function Get_Begin_Location (N : Iir) return Location_Type;
   procedure Set_Begin_Location (N : Iir; Loc : Location_Type);

   --  Field: Field3
   function Get_Then_Location (N : Iir) return Location_Type;
   procedure Set_Then_Location (N : Iir; Loc : Location_Type);

   --  Field: Field3
   function Get_Use_Location (N : Iir) return Location_Type;
   procedure Set_Use_Location (N : Iir; Loc : Location_Type);

   --  Field: Field3
   function Get_Loop_Location (N : Iir) return Location_Type;
   procedure Set_Loop_Location (N : Iir; Loc : Location_Type);

   --  Field: Field3
   function Get_Generate_Location (N : Iir) return Location_Type;
   procedure Set_Generate_Location (N : Iir; Loc : Location_Type);

   --  Field: Field5
   function Get_Generic_Location (N : Iir) return Location_Type;
   procedure Set_Generic_Location (N : Iir; Loc : Location_Type);

   --  Field: Field6
   function Get_Port_Location (N : Iir) return Location_Type;
   procedure Set_Port_Location (N : Iir; Loc : Location_Type);

   --  Field: Field3
   function Get_Generic_Map_Location (N : Iir) return Location_Type;
   procedure Set_Generic_Map_Location (N : Iir; Loc : Location_Type);

   --  Field: Field2
   function Get_Port_Map_Location (N : Iir) return Location_Type;
   procedure Set_Port_Map_Location (N : Iir; Loc : Location_Type);

   --  Field: Field1
   function Get_Arrow_Location (N : Iir) return Location_Type;
   procedure Set_Arrow_Location (N : Iir; Loc : Location_Type);

   --  Field: Field2
   function Get_Colon_Location (N : Iir) return Location_Type;
   procedure Set_Colon_Location (N : Iir; Loc : Location_Type);

   --  Field: Field3
   function Get_Assign_Location (N : Iir) return Location_Type;
   procedure Set_Assign_Location (N : Iir; Loc : Location_Type);
end Vhdl.Elocations;
