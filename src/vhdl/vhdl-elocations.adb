--  Extended locations for iir nodes
--  Copyright (C) 2017-2021 Tristan Gingold
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

with Tables;
with Vhdl.Nodes_Priv;
with Vhdl.Elocations_Meta; use Vhdl.Elocations_Meta;

package body Vhdl.Elocations is

   --  Format of a node.
   type Format_Type is
     (
      Format_None,
      Format_L1,
      Format_L2,
      Format_L3,
      Format_L4,
      Format_L5,
      Format_L6
     );

   -- Common fields are:

   -- Fields of Format_None:

   -- Fields of Format_L1:
   --   Field1 : Location_Type

   -- Fields of Format_L2:
   --   Field1 : Location_Type
   --   Field2 : Location_Type

   -- Fields of Format_L3:
   --   Field1 : Location_Type
   --   Field2 : Location_Type
   --   Field3 : Location_Type

   -- Fields of Format_L4:
   --   Field1 : Location_Type
   --   Field2 : Location_Type
   --   Field3 : Location_Type
   --   Field4 : Location_Type

   -- Fields of Format_L5:
   --   Field1 : Location_Type
   --   Field2 : Location_Type
   --   Field3 : Location_Type
   --   Field4 : Location_Type
   --   Field5 : Location_Type

   -- Fields of Format_L6:
   --   Field1 : Location_Type
   --   Field2 : Location_Type
   --   Field3 : Location_Type
   --   Field4 : Location_Type
   --   Field5 : Location_Type
   --   Field6 : Location_Type

   function Get_Format (Kind : Iir_Kind) return Format_Type;

   type Location_Index_Type is new Types.Nat32;
   No_Location_Index : constant Location_Index_Type := 0;

   package Elocations_Index_Table is new Tables
     (Table_Component_Type => Location_Index_Type,
      Table_Index_Type => Iir,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   package Elocations_Table is new Tables
     (Table_Component_Type => Location_Type,
      Table_Index_Type => Location_Index_Type,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   procedure Create_Elocations (N : Iir)
   is
      use Vhdl.Nodes_Priv;
      Format : constant Format_Type := Get_Format (Get_Kind (N));
      El : constant Iir := Elocations_Index_Table.Last;
      Len : Location_Index_Type;
      Idx : Location_Index_Type;
   begin
      pragma Assert (Format /= Format_None);

      if El < N then
         Elocations_Index_Table.Set_Last (N);
         Elocations_Index_Table.Table (El + 1 .. N) :=
           (others => No_Location_Index);
      end if;

      --  Must be called once.
      pragma Assert (Elocations_Index_Table.Table (N) = No_Location_Index);

      case Format is
         when Format_None =>
            raise Program_Error;
         when Format_L1 =>
            Len := 1;
         when Format_L2 =>
            Len := 2;
         when Format_L3 =>
            Len := 3;
         when Format_L4 =>
            Len := 4;
         when Format_L5 =>
            Len := 5;
         when Format_L6 =>
            Len := 6;
      end case;

      Idx := Elocations_Table.Last + 1;
      Elocations_Index_Table.Table (N) := Idx;
      Elocations_Table.Set_Last (Idx + Len - 1);
      Elocations_Table.Table (Idx .. Idx + Len - 1) := (others => No_Location);
   end Create_Elocations;

   procedure Delete_Elocations (N : Iir)
   is
      use Vhdl.Nodes_Priv;
      Old : Location_Index_Type;
   begin
      --  Cannot delete an already deleted location.
      if N > Elocations_Index_Table.Last then
         return;
      end if;
      Old := Elocations_Index_Table.Table (N);
      if Old = No_Location_Index then
         return;
      end if;

      --  Clear the corresponding index.
      Elocations_Index_Table.Table (N) := No_Location_Index;

      --  FIXME: keep free slots in chained list ?
   end Delete_Elocations;

   procedure Free_Hook (N : Iir) is
   begin
      Delete_Elocations (N);
   end Free_Hook;

   generic
      Off : Location_Index_Type;
   function Get_FieldX (N : Iir) return Location_Type;

   generic
      Off : Location_Index_Type;
   procedure Set_FieldX (N : Iir; Loc : Location_Type);

   function Get_FieldX (N : Iir) return Location_Type
   is
      use Vhdl.Nodes_Priv;
      Idx : Location_Index_Type;
   begin
      pragma Assert (N <= Elocations_Index_Table.Last);
      Idx := Elocations_Index_Table.Table (N);
      return Elocations_Table.Table (Idx + Off - 1);
   end Get_FieldX;

   procedure Set_FieldX (N : Iir; Loc : Location_Type)
   is
      use Vhdl.Nodes_Priv;
      Idx : Location_Index_Type;
   begin
      pragma Assert (N <= Elocations_Index_Table.Last);
      Idx := Elocations_Index_Table.Table (N);
      Elocations_Table.Table (Idx + Off - 1) := Loc;
   end Set_FieldX;

   function Get_Field1 is new Get_FieldX (1);
   procedure Set_Field1 is new Set_FieldX (1);

   function Get_Field2 is new Get_FieldX (2);
   procedure Set_Field2 is new Set_FieldX (2);

   function Get_Field3 is new Get_FieldX (3);
   procedure Set_Field3 is new Set_FieldX (3);

   function Get_Field4 is new Get_FieldX (4);
   procedure Set_Field4 is new Set_FieldX (4);

   function Get_Field5 is new Get_FieldX (5);
   procedure Set_Field5 is new Set_FieldX (5);

   function Get_Field6 is new Get_FieldX (6);
   procedure Set_Field6 is new Set_FieldX (6);

   --  Subprograms
   function Get_Format (Kind : Iir_Kind) return Format_Type is
   begin
      case Kind is
         when Iir_Kind_Unused
           | Iir_Kind_Error
           | Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Use_Clause
           | Iir_Kind_Context_Reference
           | Iir_Kind_PSL_Inherit_Spec
           | Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Null_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Overflow_Literal
           | Iir_Kind_Unaffected_Waveform
           | Iir_Kind_Waveform_Element
           | Iir_Kind_Conditional_Waveform
           | Iir_Kind_Conditional_Expression
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Entity_Aspect_Configuration
           | Iir_Kind_Entity_Aspect_Open
           | Iir_Kind_Psl_Hierarchical_Name
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Entity_Class
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Signature
           | Iir_Kind_Aggregate_Info
           | Iir_Kind_Procedure_Call
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Array_Element_Resolution
           | Iir_Kind_Record_Resolution
           | Iir_Kind_Record_Element_Resolution
           | Iir_Kind_Break_Element
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Step_Limit_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_File_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Range_Expression
           | Iir_Kind_Wildcard_Type_Definition
           | Iir_Kind_Foreign_Vector_Type_Definition
           | Iir_Kind_Subtype_Definition
           | Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition
           | Iir_Kind_Overload_List
           | Iir_Kind_Foreign_Module
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Boolean_Parameter
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Attribute_Implicit_Declaration
           | Iir_Kind_Suspend_State_Declaration
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Implicit_Condition_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator
           | Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Match_Equality_Operator
           | Iir_Kind_Match_Inequality_Operator
           | Iir_Kind_Match_Less_Than_Operator
           | Iir_Kind_Match_Less_Than_Or_Equal_Operator
           | Iir_Kind_Match_Greater_Than_Operator
           | Iir_Kind_Match_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call
           | Iir_Kind_Aggregate
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell
           | Iir_Kind_Psl_Onehot
           | Iir_Kind_Psl_Onehot0
           | Iir_Kind_Psl_Expression
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Simultaneous_Null_Statement
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_Suspend_State_Statement
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Reference_Name
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Package_Pathname
           | Iir_Kind_Absolute_Pathname
           | Iir_Kind_Relative_Pathname
           | Iir_Kind_Pathname_Element
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute
           | Iir_Kind_Nature_Reference_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Behavior_Attribute
           | Iir_Kind_Structure_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return Format_None;
         when Iir_Kind_Library_Clause
           | Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Parenthesis_Expression
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            return Format_L1;
         when Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Case_Statement =>
            return Format_L2;
         when Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return Format_L3;
         when Iir_Kind_Architecture_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement =>
            return Format_L4;
         when Iir_Kind_Package_Header =>
            return Format_L5;
         when Iir_Kind_Block_Header
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Component_Declaration =>
            return Format_L6;
      end case;
   end Get_Format;

   function Get_Start_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Start_Location (Get_Kind (N)),
                     "no field Start_Location");
      return Get_Field1 (N);
   end Get_Start_Location;

   procedure Set_Start_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Start_Location (Get_Kind (N)),
                     "no field Start_Location");
      Set_Field1 (N, Loc);
   end Set_Start_Location;

   function Get_Right_Paren_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Right_Paren_Location (Get_Kind (N)),
                     "no field Right_Paren_Location");
      return Get_Field1 (N);
   end Get_Right_Paren_Location;

   procedure Set_Right_Paren_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Right_Paren_Location (Get_Kind (N)),
                     "no field Right_Paren_Location");
      Set_Field1 (N, Loc);
   end Set_Right_Paren_Location;

   function Get_End_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_End_Location (Get_Kind (N)),
                     "no field End_Location");
      return Get_Field2 (N);
   end Get_End_Location;

   procedure Set_End_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_End_Location (Get_Kind (N)),
                     "no field End_Location");
      Set_Field2 (N, Loc);
   end Set_End_Location;

   function Get_Is_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Location (Get_Kind (N)),
                     "no field Is_Location");
      return Get_Field4 (N);
   end Get_Is_Location;

   procedure Set_Is_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Location (Get_Kind (N)),
                     "no field Is_Location");
      Set_Field4 (N, Loc);
   end Set_Is_Location;

   function Get_Begin_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Begin_Location (Get_Kind (N)),
                     "no field Begin_Location");
      return Get_Field3 (N);
   end Get_Begin_Location;

   procedure Set_Begin_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Begin_Location (Get_Kind (N)),
                     "no field Begin_Location");
      Set_Field3 (N, Loc);
   end Set_Begin_Location;

   function Get_Then_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Then_Location (Get_Kind (N)),
                     "no field Then_Location");
      return Get_Field3 (N);
   end Get_Then_Location;

   procedure Set_Then_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Then_Location (Get_Kind (N)),
                     "no field Then_Location");
      Set_Field3 (N, Loc);
   end Set_Then_Location;

   function Get_Use_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Use_Location (Get_Kind (N)),
                     "no field Use_Location");
      return Get_Field3 (N);
   end Get_Use_Location;

   procedure Set_Use_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Use_Location (Get_Kind (N)),
                     "no field Use_Location");
      Set_Field3 (N, Loc);
   end Set_Use_Location;

   function Get_Loop_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Loop_Location (Get_Kind (N)),
                     "no field Loop_Location");
      return Get_Field3 (N);
   end Get_Loop_Location;

   procedure Set_Loop_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Loop_Location (Get_Kind (N)),
                     "no field Loop_Location");
      Set_Field3 (N, Loc);
   end Set_Loop_Location;

   function Get_Generate_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Generate_Location (Get_Kind (N)),
                     "no field Generate_Location");
      return Get_Field3 (N);
   end Get_Generate_Location;

   procedure Set_Generate_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Generate_Location (Get_Kind (N)),
                     "no field Generate_Location");
      Set_Field3 (N, Loc);
   end Set_Generate_Location;

   function Get_Generic_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Generic_Location (Get_Kind (N)),
                     "no field Generic_Location");
      return Get_Field5 (N);
   end Get_Generic_Location;

   procedure Set_Generic_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Generic_Location (Get_Kind (N)),
                     "no field Generic_Location");
      Set_Field5 (N, Loc);
   end Set_Generic_Location;

   function Get_Port_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Port_Location (Get_Kind (N)),
                     "no field Port_Location");
      return Get_Field6 (N);
   end Get_Port_Location;

   procedure Set_Port_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Port_Location (Get_Kind (N)),
                     "no field Port_Location");
      Set_Field6 (N, Loc);
   end Set_Port_Location;

   function Get_Generic_Map_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Generic_Map_Location (Get_Kind (N)),
                     "no field Generic_Map_Location");
      return Get_Field3 (N);
   end Get_Generic_Map_Location;

   procedure Set_Generic_Map_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Generic_Map_Location (Get_Kind (N)),
                     "no field Generic_Map_Location");
      Set_Field3 (N, Loc);
   end Set_Generic_Map_Location;

   function Get_Port_Map_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Port_Map_Location (Get_Kind (N)),
                     "no field Port_Map_Location");
      return Get_Field2 (N);
   end Get_Port_Map_Location;

   procedure Set_Port_Map_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Port_Map_Location (Get_Kind (N)),
                     "no field Port_Map_Location");
      Set_Field2 (N, Loc);
   end Set_Port_Map_Location;

   function Get_Arrow_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Arrow_Location (Get_Kind (N)),
                     "no field Arrow_Location");
      return Get_Field1 (N);
   end Get_Arrow_Location;

   procedure Set_Arrow_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Arrow_Location (Get_Kind (N)),
                     "no field Arrow_Location");
      Set_Field1 (N, Loc);
   end Set_Arrow_Location;

   function Get_Colon_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Colon_Location (Get_Kind (N)),
                     "no field Colon_Location");
      return Get_Field2 (N);
   end Get_Colon_Location;

   procedure Set_Colon_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Colon_Location (Get_Kind (N)),
                     "no field Colon_Location");
      Set_Field2 (N, Loc);
   end Set_Colon_Location;

   function Get_Assign_Location (N : Iir) return Location_Type is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Assign_Location (Get_Kind (N)),
                     "no field Assign_Location");
      return Get_Field3 (N);
   end Get_Assign_Location;

   procedure Set_Assign_Location (N : Iir; Loc : Location_Type) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Assign_Location (Get_Kind (N)),
                     "no field Assign_Location");
      Set_Field3 (N, Loc);
   end Set_Assign_Location;


begin
   Vhdl.Nodes.Register_Free_Hook (Free_Hook'Access);
end Vhdl.Elocations;
