--  Tree node definitions.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Nodes; use Nodes;
with Lists; use Lists;
with Nodes_Meta; use Nodes_Meta;

package body Iirs is
   function Is_Null (Node : Iir) return Boolean is
   begin
      return Node = Null_Iir;
   end Is_Null;

   function Is_Null_List (Node : Iir_List) return Boolean is
   begin
      return Node = Null_Iir_List;
   end Is_Null_List;

   function Is_Valid (Node : Iir) return Boolean is
   begin
      return Node /= Null_Iir;
   end Is_Valid;

   ---------------------------------------------------
   -- General subprograms that operate on every iir --
   ---------------------------------------------------

   function Get_Format (Kind : Iir_Kind) return Format_Type;

   function Create_Iir (Kind : Iir_Kind) return Iir
   is
      Res : Iir;
      Format : Format_Type;
   begin
      Format := Get_Format (Kind);
      Res := Create_Node (Format);
      Set_Nkind (Res, Iir_Kind'Pos (Kind));
      return Res;
   end Create_Iir;

   --  Statistics.
   procedure Disp_Stats
   is
      use Ada.Text_IO;
      type Num_Array is array (Iir_Kind) of Natural;
      Num : Num_Array := (others => 0);
      type Format_Array is array (Format_Type) of Natural;
      Formats : Format_Array := (others => 0);
      Kind : Iir_Kind;
      I : Iir;
      Last_I : Iir;
      Format : Format_Type;
   begin
      I := Error_Node + 1;
      Last_I := Get_Last_Node;
      while I < Last_I loop
         Kind := Get_Kind (I);
         Num (Kind) := Num (Kind) + 1;
         Format := Get_Format (Kind);
         Formats (Format) := Formats (Format) + 1;
         I := Next_Node (I);
      end loop;

      Put_Line ("Stats per iir_kind:");
      for J in Iir_Kind loop
         if Num (J) /= 0 then
            Put_Line (' ' & Iir_Kind'Image (J) & ':'
                      & Natural'Image (Num (J)));
         end if;
      end loop;
      Put_Line ("Stats per formats:");
      for J in Format_Type loop
         Put_Line (' ' & Format_Type'Image (J) & ':'
                   & Natural'Image (Formats (J)));
      end loop;
   end Disp_Stats;

   function Kind_In (K : Iir_Kind; K1, K2 : Iir_Kind) return Boolean is
   begin
      return K = K1 or K = K2;
   end Kind_In;

   function Iir_Predefined_Shortcut_P (Func : Iir_Predefined_Functions)
     return Boolean is
   begin
      case Func is
         when Iir_Predefined_Bit_And
           | Iir_Predefined_Bit_Or
           | Iir_Predefined_Bit_Nand
           | Iir_Predefined_Bit_Nor
           | Iir_Predefined_Boolean_And
           | Iir_Predefined_Boolean_Or
           | Iir_Predefined_Boolean_Nand
           | Iir_Predefined_Boolean_Nor =>
            return True;
         when others =>
            return False;
      end case;
   end Iir_Predefined_Shortcut_P;

   function Create_Iir_Error return Iir
   is
      Res : Iir;
   begin
      Res := Create_Node (Format_Short);
      Set_Nkind (Res, Iir_Kind'Pos (Iir_Kind_Error));
      Set_Base_Type (Res, Res);
      return Res;
   end Create_Iir_Error;

   procedure Location_Copy (Target : Iir; Src : Iir) is
   begin
      Set_Location (Target, Get_Location (Src));
   end Location_Copy;

   -- Get kind
   function Get_Kind (N : Iir) return Iir_Kind
   is
      --  Speed up: avoid to check that nkind is in the bounds of Iir_Kind.
      pragma Suppress (Range_Check);
   begin
      pragma Assert (N /= Null_Iir);
      return Iir_Kind'Val (Get_Nkind (N));
   end Get_Kind;

   function Time_Stamp_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => Time_Stamp_Id, Target => Iir);

   function Iir_To_Time_Stamp_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Time_Stamp_Id);

   function File_Checksum_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => File_Checksum_Id, Target => Iir);

   function Iir_To_File_Checksum_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => File_Checksum_Id);

   function Iir_To_Iir_List is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_List);
   function Iir_List_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_List, Target => Iir);

   function Iir_To_Iir_Flist is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_Flist);
   function Iir_Flist_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_Flist, Target => Iir);

   function Iir_To_Token_Type (N : Iir) return Token_Type is
   begin
      return Token_Type'Val (N);
   end Iir_To_Token_Type;

   function Token_Type_To_Iir (T : Token_Type) return Iir is
   begin
      return Token_Type'Pos (T);
   end Token_Type_To_Iir;

--     function Iir_To_Iir_Index32 (N : Iir) return Iir_Index32 is
--     begin
--        return Iir_Index32 (N);
--     end Iir_To_Iir_Index32;

--     function Iir_Index32_To_Iir (V : Iir_Index32) return Iir is
--     begin
--        return Iir_Index32'Pos (V);
--     end Iir_Index32_To_Iir;

   function Iir_To_Name_Id (N : Iir) return Name_Id is
   begin
      return Iir'Pos (N);
   end Iir_To_Name_Id;
   pragma Inline (Iir_To_Name_Id);

   function Name_Id_To_Iir (V : Name_Id) return Iir is
   begin
      return Name_Id'Pos (V);
   end Name_Id_To_Iir;

   function Iir_To_Iir_Int32 is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_Int32);

   function Iir_Int32_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_Int32, Target => Iir);

   function Iir_To_Source_Ptr (N : Iir) return Source_Ptr is
   begin
      return Source_Ptr (N);
   end Iir_To_Source_Ptr;

   function Source_Ptr_To_Iir (P : Source_Ptr) return Iir is
   begin
      return Iir (P);
   end Source_Ptr_To_Iir;

   function Iir_To_Source_File_Entry is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Source_File_Entry);
   function Source_File_Entry_To_Iir is new Ada.Unchecked_Conversion
     (Source => Source_File_Entry, Target => Iir);

   function Boolean_To_Iir_Delay_Mechanism is new Ada.Unchecked_Conversion
     (Source => Boolean, Target => Iir_Delay_Mechanism);
   function Iir_Delay_Mechanism_To_Boolean is new Ada.Unchecked_Conversion
     (Source => Iir_Delay_Mechanism, Target => Boolean);

   function Boolean_To_Iir_Signal_Kind is new Ada.Unchecked_Conversion
     (Source => Boolean, Target => Iir_Signal_Kind);
   function Iir_Signal_Kind_To_Boolean is new Ada.Unchecked_Conversion
     (Source => Iir_Signal_Kind, Target => Boolean);

   function Iir_To_String8_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => String8_Id);
   function String8_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => String8_Id, Target => Iir);

   function Iir_To_Int32 is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Int32);
   function Int32_To_Iir is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Iir);

   function Iir_To_PSL_Node is new Ada.Unchecked_Conversion
     (Source => Iir, Target => PSL_Node);

   function PSL_Node_To_Iir is new Ada.Unchecked_Conversion
     (Source => PSL_Node, Target => Iir);

   function Iir_To_PSL_NFA is new Ada.Unchecked_Conversion
     (Source => Iir, Target => PSL_NFA);

   function PSL_NFA_To_Iir is new Ada.Unchecked_Conversion
     (Source => PSL_NFA, Target => Iir);

   --  Subprograms
   function Get_Format (Kind : Iir_Kind) return Format_Type is
   begin
      case Kind is
         when Iir_Kind_Unused
           | Iir_Kind_Error
           | Iir_Kind_Library_Clause
           | Iir_Kind_Use_Clause
           | Iir_Kind_Context_Reference
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
           | Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Entity_Aspect_Configuration
           | Iir_Kind_Entity_Aspect_Open
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Entity_Class
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Aggregate_Info
           | Iir_Kind_Procedure_Call
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Array_Element_Resolution
           | Iir_Kind_Record_Resolution
           | Iir_Kind_Record_Element_Resolution
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Range_Expression
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Wildcard_Type_Definition
           | Iir_Kind_Overload_List
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Signal_Attribute_Declaration
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
           | Iir_Kind_Parenthesis_Expression
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Psl_Expression
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif
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
            return Format_Short;
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Block_Header
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Signature
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Subtype_Definition
           | Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Package_Header
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_File_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Psl_Cover_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Wait_Statement =>
            return Format_Medium;
      end case;
   end Get_Format;

   function Get_First_Design_Unit (Design : Iir) return Iir is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_First_Design_Unit (Get_Kind (Design)),
                     "no field First_Design_Unit");
      return Get_Field5 (Design);
   end Get_First_Design_Unit;

   procedure Set_First_Design_Unit (Design : Iir; Chain : Iir) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_First_Design_Unit (Get_Kind (Design)),
                     "no field First_Design_Unit");
      Set_Field5 (Design, Chain);
   end Set_First_Design_Unit;

   function Get_Last_Design_Unit (Design : Iir) return Iir is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Last_Design_Unit (Get_Kind (Design)),
                     "no field Last_Design_Unit");
      return Get_Field6 (Design);
   end Get_Last_Design_Unit;

   procedure Set_Last_Design_Unit (Design : Iir; Chain : Iir) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Last_Design_Unit (Get_Kind (Design)),
                     "no field Last_Design_Unit");
      Set_Field6 (Design, Chain);
   end Set_Last_Design_Unit;

   function Get_Library_Declaration (Design : Iir) return Iir is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Library_Declaration (Get_Kind (Design)),
                     "no field Library_Declaration");
      return Get_Field1 (Design);
   end Get_Library_Declaration;

   procedure Set_Library_Declaration (Design : Iir; Library : Iir) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Library_Declaration (Get_Kind (Design)),
                     "no field Library_Declaration");
      Set_Field1 (Design, Library);
   end Set_Library_Declaration;

   function Get_File_Checksum (Design : Iir) return File_Checksum_Id is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_File_Checksum (Get_Kind (Design)),
                     "no field File_Checksum");
      return Iir_To_File_Checksum_Id (Get_Field4 (Design));
   end Get_File_Checksum;

   procedure Set_File_Checksum (Design : Iir; Checksum : File_Checksum_Id) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_File_Checksum (Get_Kind (Design)),
                     "no field File_Checksum");
      Set_Field4 (Design, File_Checksum_Id_To_Iir (Checksum));
   end Set_File_Checksum;

   function Get_Analysis_Time_Stamp (Design : Iir) return Time_Stamp_Id is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Analysis_Time_Stamp (Get_Kind (Design)),
                     "no field Analysis_Time_Stamp");
      return Iir_To_Time_Stamp_Id (Get_Field3 (Design));
   end Get_Analysis_Time_Stamp;

   procedure Set_Analysis_Time_Stamp (Design : Iir; Stamp : Time_Stamp_Id) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Analysis_Time_Stamp (Get_Kind (Design)),
                     "no field Analysis_Time_Stamp");
      Set_Field3 (Design, Time_Stamp_Id_To_Iir (Stamp));
   end Set_Analysis_Time_Stamp;

   function Get_Library (File : Iir_Design_File) return Iir is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Library (Get_Kind (File)),
                     "no field Library");
      return Get_Field0 (File);
   end Get_Library;

   procedure Set_Library (File : Iir_Design_File; Lib : Iir) is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Library (Get_Kind (File)),
                     "no field Library");
      Set_Field0 (File, Lib);
   end Set_Library;

   function Get_File_Dependence_List (File : Iir_Design_File) return Iir_List
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_File_Dependence_List (Get_Kind (File)),
                     "no field File_Dependence_List");
      return Iir_To_Iir_List (Get_Field1 (File));
   end Get_File_Dependence_List;

   procedure Set_File_Dependence_List (File : Iir_Design_File; Lst : Iir_List)
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_File_Dependence_List (Get_Kind (File)),
                     "no field File_Dependence_List");
      Set_Field1 (File, Iir_List_To_Iir (Lst));
   end Set_File_Dependence_List;

   function Get_Design_File_Filename (File : Iir_Design_File) return Name_Id
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Filename (Get_Kind (File)),
                     "no field Design_File_Filename");
      return Name_Id'Val (Get_Field12 (File));
   end Get_Design_File_Filename;

   procedure Set_Design_File_Filename (File : Iir_Design_File; Name : Name_Id)
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Filename (Get_Kind (File)),
                     "no field Design_File_Filename");
      Set_Field12 (File, Name_Id'Pos (Name));
   end Set_Design_File_Filename;

   function Get_Design_File_Directory (File : Iir_Design_File) return Name_Id
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Directory (Get_Kind (File)),
                     "no field Design_File_Directory");
      return Name_Id'Val (Get_Field11 (File));
   end Get_Design_File_Directory;

   procedure Set_Design_File_Directory (File : Iir_Design_File; Dir : Name_Id)
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Directory (Get_Kind (File)),
                     "no field Design_File_Directory");
      Set_Field11 (File, Name_Id'Pos (Dir));
   end Set_Design_File_Directory;

   function Get_Design_File (Unit : Iir_Design_Unit) return Iir is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Design_File (Get_Kind (Unit)),
                     "no field Design_File");
      return Get_Field0 (Unit);
   end Get_Design_File;

   procedure Set_Design_File (Unit : Iir_Design_Unit; File : Iir) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Design_File (Get_Kind (Unit)),
                     "no field Design_File");
      Set_Field0 (Unit, File);
   end Set_Design_File;

   function Get_Design_File_Chain (Library : Iir) return Iir is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Design_File_Chain (Get_Kind (Library)),
                     "no field Design_File_Chain");
      return Get_Field1 (Library);
   end Get_Design_File_Chain;

   procedure Set_Design_File_Chain (Library : Iir; Chain : Iir) is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Design_File_Chain (Get_Kind (Library)),
                     "no field Design_File_Chain");
      Set_Field1 (Library, Chain);
   end Set_Design_File_Chain;

   function Get_Library_Directory (Library : Iir) return Name_Id is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Library_Directory (Get_Kind (Library)),
                     "no field Library_Directory");
      return Name_Id'Val (Get_Field5 (Library));
   end Get_Library_Directory;

   procedure Set_Library_Directory (Library : Iir; Dir : Name_Id) is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Library_Directory (Get_Kind (Library)),
                     "no field Library_Directory");
      Set_Field5 (Library, Name_Id'Pos (Dir));
   end Set_Library_Directory;

   function Get_Date (Target : Iir) return Date_Type is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Date (Get_Kind (Target)),
                     "no field Date");
      return Date_Type'Val (Get_Field4 (Target));
   end Get_Date;

   procedure Set_Date (Target : Iir; Date : Date_Type) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Date (Get_Kind (Target)),
                     "no field Date");
      Set_Field4 (Target, Date_Type'Pos (Date));
   end Set_Date;

   function Get_Context_Items (Design_Unit : Iir) return Iir is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Context_Items (Get_Kind (Design_Unit)),
                     "no field Context_Items");
      return Get_Field1 (Design_Unit);
   end Get_Context_Items;

   procedure Set_Context_Items (Design_Unit : Iir; Items_Chain : Iir) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Context_Items (Get_Kind (Design_Unit)),
                     "no field Context_Items");
      Set_Field1 (Design_Unit, Items_Chain);
   end Set_Context_Items;

   function Get_Dependence_List (Unit : Iir) return Iir_List is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Dependence_List (Get_Kind (Unit)),
                     "no field Dependence_List");
      return Iir_To_Iir_List (Get_Field8 (Unit));
   end Get_Dependence_List;

   procedure Set_Dependence_List (Unit : Iir; List : Iir_List) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Dependence_List (Get_Kind (Unit)),
                     "no field Dependence_List");
      Set_Field8 (Unit, Iir_List_To_Iir (List));
   end Set_Dependence_List;

   function Get_Analysis_Checks_List (Unit : Iir) return Iir_List is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Analysis_Checks_List (Get_Kind (Unit)),
                     "no field Analysis_Checks_List");
      return Iir_To_Iir_List (Get_Field9 (Unit));
   end Get_Analysis_Checks_List;

   procedure Set_Analysis_Checks_List (Unit : Iir; List : Iir_List) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Analysis_Checks_List (Get_Kind (Unit)),
                     "no field Analysis_Checks_List");
      Set_Field9 (Unit, Iir_List_To_Iir (List));
   end Set_Analysis_Checks_List;

   function Get_Date_State (Unit : Iir_Design_Unit) return Date_State_Type is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Date_State (Get_Kind (Unit)),
                     "no field Date_State");
      return Date_State_Type'Val (Get_State1 (Unit));
   end Get_Date_State;

   procedure Set_Date_State (Unit : Iir_Design_Unit; State : Date_State_Type)
   is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Date_State (Get_Kind (Unit)),
                     "no field Date_State");
      Set_State1 (Unit, Date_State_Type'Pos (State));
   end Set_Date_State;

   function Get_Guarded_Target_State (Stmt : Iir) return Tri_State_Type is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Guarded_Target_State (Get_Kind (Stmt)),
                     "no field Guarded_Target_State");
      return Tri_State_Type'Val (Get_State1 (Stmt));
   end Get_Guarded_Target_State;

   procedure Set_Guarded_Target_State (Stmt : Iir; State : Tri_State_Type) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Guarded_Target_State (Get_Kind (Stmt)),
                     "no field Guarded_Target_State");
      Set_State1 (Stmt, Tri_State_Type'Pos (State));
   end Set_Guarded_Target_State;

   function Get_Library_Unit (Design_Unit : Iir_Design_Unit) return Iir is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Library_Unit (Get_Kind (Design_Unit)),
                     "no field Library_Unit");
      return Get_Field5 (Design_Unit);
   end Get_Library_Unit;

   procedure Set_Library_Unit (Design_Unit : Iir_Design_Unit; Lib_Unit : Iir)
   is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Library_Unit (Get_Kind (Design_Unit)),
                     "no field Library_Unit");
      Set_Field5 (Design_Unit, Lib_Unit);
   end Set_Library_Unit;

   function Get_Hash_Chain (Design_Unit : Iir_Design_Unit) return Iir is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Hash_Chain (Get_Kind (Design_Unit)),
                     "no field Hash_Chain");
      return Get_Field7 (Design_Unit);
   end Get_Hash_Chain;

   procedure Set_Hash_Chain (Design_Unit : Iir_Design_Unit; Chain : Iir) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Hash_Chain (Get_Kind (Design_Unit)),
                     "no field Hash_Chain");
      Set_Field7 (Design_Unit, Chain);
   end Set_Hash_Chain;

   function Get_Design_Unit_Source_Pos (Design_Unit : Iir) return Source_Ptr
   is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Pos (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Pos");
      return Iir_To_Source_Ptr (Get_Field10 (Design_Unit));
   end Get_Design_Unit_Source_Pos;

   procedure Set_Design_Unit_Source_Pos (Design_Unit : Iir; Pos : Source_Ptr)
   is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Pos (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Pos");
      Set_Field10 (Design_Unit, Source_Ptr_To_Iir (Pos));
   end Set_Design_Unit_Source_Pos;

   function Get_Design_Unit_Source_Line (Design_Unit : Iir) return Int32 is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Line (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Line");
      return Iir_To_Int32 (Get_Field11 (Design_Unit));
   end Get_Design_Unit_Source_Line;

   procedure Set_Design_Unit_Source_Line (Design_Unit : Iir; Line : Int32) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Line (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Line");
      Set_Field11 (Design_Unit, Int32_To_Iir (Line));
   end Set_Design_Unit_Source_Line;

   function Get_Design_Unit_Source_Col (Design_Unit : Iir) return Int32 is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Col (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Col");
      return Iir_To_Int32 (Get_Field12 (Design_Unit));
   end Get_Design_Unit_Source_Col;

   procedure Set_Design_Unit_Source_Col (Design_Unit : Iir; Line : Int32) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Col (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Col");
      Set_Field12 (Design_Unit, Int32_To_Iir (Line));
   end Set_Design_Unit_Source_Col;

   type Iir_Int64_Conv is record
      Field4: Iir;
      Field5: Iir;
   end record;
   pragma Pack (Iir_Int64_Conv);
   pragma Assert (Iir_Int64_Conv'Size = Iir_Int64'Size);

   function Get_Value (Lit : Iir) return Iir_Int64
   is
      function To_Iir_Int64 is new Ada.Unchecked_Conversion
         (Iir_Int64_Conv, Iir_Int64);
      Conv : Iir_Int64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Value (Get_Kind (Lit)),
                     "no field Value");
      Conv.Field4 := Get_Field4 (Lit);
      Conv.Field5 := Get_Field5 (Lit);
      return To_Iir_Int64 (Conv);
   end Get_Value;

   procedure Set_Value (Lit : Iir; Val : Iir_Int64)
   is
      function To_Iir_Int64_Conv is new Ada.Unchecked_Conversion
         (Iir_Int64, Iir_Int64_Conv);
      Conv : Iir_Int64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Value (Get_Kind (Lit)),
                     "no field Value");
      Conv := To_Iir_Int64_Conv (Val);
      Set_Field4 (Lit, Conv.Field4);
      Set_Field5 (Lit, Conv.Field5);
   end Set_Value;

   function Get_Enum_Pos (Lit : Iir) return Iir_Int32 is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Enum_Pos (Get_Kind (Lit)),
                     "no field Enum_Pos");
      return Iir_Int32'Val (Get_Field5 (Lit));
   end Get_Enum_Pos;

   procedure Set_Enum_Pos (Lit : Iir; Val : Iir_Int32) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Enum_Pos (Get_Kind (Lit)),
                     "no field Enum_Pos");
      Set_Field5 (Lit, Iir_Int32'Pos (Val));
   end Set_Enum_Pos;

   function Get_Physical_Literal (Unit : Iir) return Iir is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Physical_Literal (Get_Kind (Unit)),
                     "no field Physical_Literal");
      return Get_Field4 (Unit);
   end Get_Physical_Literal;

   procedure Set_Physical_Literal (Unit : Iir; Lit : Iir) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Physical_Literal (Get_Kind (Unit)),
                     "no field Physical_Literal");
      Set_Field4 (Unit, Lit);
   end Set_Physical_Literal;

   type Iir_Fp64_Conv is record
      Field4: Iir;
      Field5: Iir;
   end record;
   pragma Pack (Iir_Fp64_Conv);
   pragma Assert (Iir_Fp64_Conv'Size = Iir_Fp64'Size);

   function Get_Fp_Value (Lit : Iir) return Iir_Fp64
   is
      function To_Iir_Fp64 is new Ada.Unchecked_Conversion
         (Iir_Fp64_Conv, Iir_Fp64);
      Conv : Iir_Fp64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Fp_Value (Get_Kind (Lit)),
                     "no field Fp_Value");
      Conv.Field4 := Get_Field4 (Lit);
      Conv.Field5 := Get_Field5 (Lit);
      return To_Iir_Fp64 (Conv);
   end Get_Fp_Value;

   procedure Set_Fp_Value (Lit : Iir; Val : Iir_Fp64)
   is
      function To_Iir_Fp64_Conv is new Ada.Unchecked_Conversion
         (Iir_Fp64, Iir_Fp64_Conv);
      Conv : Iir_Fp64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Fp_Value (Get_Kind (Lit)),
                     "no field Fp_Value");
      Conv := To_Iir_Fp64_Conv (Val);
      Set_Field4 (Lit, Conv.Field4);
      Set_Field5 (Lit, Conv.Field5);
   end Set_Fp_Value;

   function Get_Simple_Aggregate_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Aggregate_List (Get_Kind (Target)),
                     "no field Simple_Aggregate_List");
      return Iir_To_Iir_Flist (Get_Field4 (Target));
   end Get_Simple_Aggregate_List;

   procedure Set_Simple_Aggregate_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Aggregate_List (Get_Kind (Target)),
                     "no field Simple_Aggregate_List");
      Set_Field4 (Target, Iir_Flist_To_Iir (List));
   end Set_Simple_Aggregate_List;

   function Get_String8_Id (Lit : Iir) return String8_Id is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String8_Id (Get_Kind (Lit)),
                     "no field String8_Id");
      return Iir_To_String8_Id (Get_Field5 (Lit));
   end Get_String8_Id;

   procedure Set_String8_Id (Lit : Iir; Id : String8_Id) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String8_Id (Get_Kind (Lit)),
                     "no field String8_Id");
      Set_Field5 (Lit, String8_Id_To_Iir (Id));
   end Set_String8_Id;

   function Get_String_Length (Lit : Iir) return Int32 is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String_Length (Get_Kind (Lit)),
                     "no field String_Length");
      return Iir_To_Int32 (Get_Field4 (Lit));
   end Get_String_Length;

   procedure Set_String_Length (Lit : Iir; Len : Int32) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String_Length (Get_Kind (Lit)),
                     "no field String_Length");
      Set_Field4 (Lit, Int32_To_Iir (Len));
   end Set_String_Length;

   type Number_Base_Type_Conv is record
      Flag12: Boolean;
      Flag13: Boolean;
      Flag14: Boolean;
   end record;
   pragma Pack (Number_Base_Type_Conv);
   pragma Assert (Number_Base_Type_Conv'Size = Number_Base_Type'Size);

   function Get_Bit_String_Base (Lit : Iir) return Number_Base_Type
   is
      function To_Number_Base_Type is new Ada.Unchecked_Conversion
         (Number_Base_Type_Conv, Number_Base_Type);
      Conv : Number_Base_Type_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Bit_String_Base (Get_Kind (Lit)),
                     "no field Bit_String_Base");
      Conv.Flag12 := Get_Flag12 (Lit);
      Conv.Flag13 := Get_Flag13 (Lit);
      Conv.Flag14 := Get_Flag14 (Lit);
      return To_Number_Base_Type (Conv);
   end Get_Bit_String_Base;

   procedure Set_Bit_String_Base (Lit : Iir; Base : Number_Base_Type)
   is
      function To_Number_Base_Type_Conv is new Ada.Unchecked_Conversion
         (Number_Base_Type, Number_Base_Type_Conv);
      Conv : Number_Base_Type_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Bit_String_Base (Get_Kind (Lit)),
                     "no field Bit_String_Base");
      Conv := To_Number_Base_Type_Conv (Base);
      Set_Flag12 (Lit, Conv.Flag12);
      Set_Flag13 (Lit, Conv.Flag13);
      Set_Flag14 (Lit, Conv.Flag14);
   end Set_Bit_String_Base;

   function Get_Has_Signed (Lit : Iir) return Boolean is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Signed (Get_Kind (Lit)),
                     "no field Has_Signed");
      return Get_Flag1 (Lit);
   end Get_Has_Signed;

   procedure Set_Has_Signed (Lit : Iir; Flag : Boolean) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Signed (Get_Kind (Lit)),
                     "no field Has_Signed");
      Set_Flag1 (Lit, Flag);
   end Set_Has_Signed;

   function Get_Has_Sign (Lit : Iir) return Boolean is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Sign (Get_Kind (Lit)),
                     "no field Has_Sign");
      return Get_Flag2 (Lit);
   end Get_Has_Sign;

   procedure Set_Has_Sign (Lit : Iir; Flag : Boolean) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Sign (Get_Kind (Lit)),
                     "no field Has_Sign");
      Set_Flag2 (Lit, Flag);
   end Set_Has_Sign;

   function Get_Has_Length (Lit : Iir) return Boolean is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Length (Get_Kind (Lit)),
                     "no field Has_Length");
      return Get_Flag3 (Lit);
   end Get_Has_Length;

   procedure Set_Has_Length (Lit : Iir; Flag : Boolean) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Length (Get_Kind (Lit)),
                     "no field Has_Length");
      Set_Flag3 (Lit, Flag);
   end Set_Has_Length;

   function Get_Literal_Origin (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Origin (Get_Kind (Lit)),
                     "no field Literal_Origin");
      return Get_Field2 (Lit);
   end Get_Literal_Origin;

   procedure Set_Literal_Origin (Lit : Iir; Orig : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Origin (Get_Kind (Lit)),
                     "no field Literal_Origin");
      Set_Field2 (Lit, Orig);
   end Set_Literal_Origin;

   function Get_Range_Origin (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Range_Origin (Get_Kind (Lit)),
                     "no field Range_Origin");
      return Get_Field0 (Lit);
   end Get_Range_Origin;

   procedure Set_Range_Origin (Lit : Iir; Orig : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Range_Origin (Get_Kind (Lit)),
                     "no field Range_Origin");
      Set_Field0 (Lit, Orig);
   end Set_Range_Origin;

   function Get_Literal_Subtype (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Subtype (Get_Kind (Lit)),
                     "no field Literal_Subtype");
      return Get_Field3 (Lit);
   end Get_Literal_Subtype;

   procedure Set_Literal_Subtype (Lit : Iir; Atype : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Subtype (Get_Kind (Lit)),
                     "no field Literal_Subtype");
      Set_Field3 (Lit, Atype);
   end Set_Literal_Subtype;

   function Get_Allocator_Subtype (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Allocator_Subtype (Get_Kind (Lit)),
                     "no field Allocator_Subtype");
      return Get_Field3 (Lit);
   end Get_Allocator_Subtype;

   procedure Set_Allocator_Subtype (Lit : Iir; Atype : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Allocator_Subtype (Get_Kind (Lit)),
                     "no field Allocator_Subtype");
      Set_Field3 (Lit, Atype);
   end Set_Allocator_Subtype;

   function Get_Entity_Class (Target : Iir) return Token_Type is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class (Get_Kind (Target)),
                     "no field Entity_Class");
      return Iir_To_Token_Type (Get_Field3 (Target));
   end Get_Entity_Class;

   procedure Set_Entity_Class (Target : Iir; Kind : Token_Type) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class (Get_Kind (Target)),
                     "no field Entity_Class");
      Set_Field3 (Target, Token_Type_To_Iir (Kind));
   end Set_Entity_Class;

   function Get_Entity_Name_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Name_List (Get_Kind (Target)),
                     "no field Entity_Name_List");
      return Iir_To_Iir_Flist (Get_Field8 (Target));
   end Get_Entity_Name_List;

   procedure Set_Entity_Name_List (Target : Iir; Names : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Name_List (Get_Kind (Target)),
                     "no field Entity_Name_List");
      Set_Field8 (Target, Iir_Flist_To_Iir (Names));
   end Set_Entity_Name_List;

   function Get_Attribute_Designator (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Designator (Get_Kind (Target)),
                     "no field Attribute_Designator");
      return Get_Field6 (Target);
   end Get_Attribute_Designator;

   procedure Set_Attribute_Designator (Target : Iir; Designator : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Designator (Get_Kind (Target)),
                     "no field Attribute_Designator");
      Set_Field6 (Target, Designator);
   end Set_Attribute_Designator;

   function Get_Attribute_Specification_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Specification_Chain (Get_Kind (Target)),
                     "no field Attribute_Specification_Chain");
      return Get_Field7 (Target);
   end Get_Attribute_Specification_Chain;

   procedure Set_Attribute_Specification_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Specification_Chain (Get_Kind (Target)),
                     "no field Attribute_Specification_Chain");
      Set_Field7 (Target, Chain);
   end Set_Attribute_Specification_Chain;

   function Get_Attribute_Specification (Val : Iir) return Iir is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Attribute_Specification (Get_Kind (Val)),
                     "no field Attribute_Specification");
      return Get_Field4 (Val);
   end Get_Attribute_Specification;

   procedure Set_Attribute_Specification (Val : Iir; Attr : Iir) is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Attribute_Specification (Get_Kind (Val)),
                     "no field Attribute_Specification");
      Set_Field4 (Val, Attr);
   end Set_Attribute_Specification;

   function Get_Signal_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_List (Get_Kind (Target)),
                     "no field Signal_List");
      return Iir_To_Iir_Flist (Get_Field3 (Target));
   end Get_Signal_List;

   procedure Set_Signal_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_List (Get_Kind (Target)),
                     "no field Signal_List");
      Set_Field3 (Target, Iir_Flist_To_Iir (List));
   end Set_Signal_List;

   function Get_Designated_Entity (Val : Iir_Attribute_Value) return Iir is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Designated_Entity (Get_Kind (Val)),
                     "no field Designated_Entity");
      return Get_Field3 (Val);
   end Get_Designated_Entity;

   procedure Set_Designated_Entity (Val : Iir_Attribute_Value; Entity : Iir)
   is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Designated_Entity (Get_Kind (Val)),
                     "no field Designated_Entity");
      Set_Field3 (Val, Entity);
   end Set_Designated_Entity;

   function Get_Formal (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal (Get_Kind (Target)),
                     "no field Formal");
      return Get_Field1 (Target);
   end Get_Formal;

   procedure Set_Formal (Target : Iir; Formal : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal (Get_Kind (Target)),
                     "no field Formal");
      Set_Field1 (Target, Formal);
   end Set_Formal;

   function Get_Actual (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual (Get_Kind (Target)),
                     "no field Actual");
      return Get_Field3 (Target);
   end Get_Actual;

   procedure Set_Actual (Target : Iir; Actual : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual (Get_Kind (Target)),
                     "no field Actual");
      Set_Field3 (Target, Actual);
   end Set_Actual;

   function Get_Actual_Conversion (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Conversion (Get_Kind (Target)),
                     "no field Actual_Conversion");
      return Get_Field4 (Target);
   end Get_Actual_Conversion;

   procedure Set_Actual_Conversion (Target : Iir; Conv : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Conversion (Get_Kind (Target)),
                     "no field Actual_Conversion");
      Set_Field4 (Target, Conv);
   end Set_Actual_Conversion;

   function Get_Formal_Conversion (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal_Conversion (Get_Kind (Target)),
                     "no field Formal_Conversion");
      return Get_Field5 (Target);
   end Get_Formal_Conversion;

   procedure Set_Formal_Conversion (Target : Iir; Conv : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal_Conversion (Get_Kind (Target)),
                     "no field Formal_Conversion");
      Set_Field5 (Target, Conv);
   end Set_Formal_Conversion;

   function Get_Whole_Association_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Whole_Association_Flag (Get_Kind (Target)),
                     "no field Whole_Association_Flag");
      return Get_Flag1 (Target);
   end Get_Whole_Association_Flag;

   procedure Set_Whole_Association_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Whole_Association_Flag (Get_Kind (Target)),
                     "no field Whole_Association_Flag");
      Set_Flag1 (Target, Flag);
   end Set_Whole_Association_Flag;

   function Get_Collapse_Signal_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Collapse_Signal_Flag (Get_Kind (Target)),
                     "no field Collapse_Signal_Flag");
      return Get_Flag2 (Target);
   end Get_Collapse_Signal_Flag;

   procedure Set_Collapse_Signal_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Collapse_Signal_Flag (Get_Kind (Target)),
                     "no field Collapse_Signal_Flag");
      Set_Flag2 (Target, Flag);
   end Set_Collapse_Signal_Flag;

   function Get_Artificial_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Artificial_Flag (Get_Kind (Target)),
                     "no field Artificial_Flag");
      return Get_Flag3 (Target);
   end Get_Artificial_Flag;

   procedure Set_Artificial_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Artificial_Flag (Get_Kind (Target)),
                     "no field Artificial_Flag");
      Set_Flag3 (Target, Flag);
   end Set_Artificial_Flag;

   function Get_Open_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Open_Flag (Get_Kind (Target)),
                     "no field Open_Flag");
      return Get_Flag7 (Target);
   end Get_Open_Flag;

   procedure Set_Open_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Open_Flag (Get_Kind (Target)),
                     "no field Open_Flag");
      Set_Flag7 (Target, Flag);
   end Set_Open_Flag;

   function Get_After_Drivers_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_After_Drivers_Flag (Get_Kind (Target)),
                     "no field After_Drivers_Flag");
      return Get_Flag5 (Target);
   end Get_After_Drivers_Flag;

   procedure Set_After_Drivers_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_After_Drivers_Flag (Get_Kind (Target)),
                     "no field After_Drivers_Flag");
      Set_Flag5 (Target, Flag);
   end Set_After_Drivers_Flag;

   function Get_We_Value (We : Iir_Waveform_Element) return Iir is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_We_Value (Get_Kind (We)),
                     "no field We_Value");
      return Get_Field1 (We);
   end Get_We_Value;

   procedure Set_We_Value (We : Iir_Waveform_Element; An_Iir : Iir) is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_We_Value (Get_Kind (We)),
                     "no field We_Value");
      Set_Field1 (We, An_Iir);
   end Set_We_Value;

   function Get_Time (We : Iir_Waveform_Element) return Iir is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_Time (Get_Kind (We)),
                     "no field Time");
      return Get_Field3 (We);
   end Get_Time;

   procedure Set_Time (We : Iir_Waveform_Element; An_Iir : Iir) is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_Time (Get_Kind (We)),
                     "no field Time");
      Set_Field3 (We, An_Iir);
   end Set_Time;

   function Get_Choice_Position (Choice : Iir) return Int32 is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Position (Get_Kind (Choice)),
                     "no field Choice_Position");
      return Int32'Val (Get_Field1 (Choice));
   end Get_Choice_Position;

   procedure Set_Choice_Position (Choice : Iir; Pos : Int32) is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Position (Get_Kind (Choice)),
                     "no field Choice_Position");
      Set_Field1 (Choice, Int32'Pos (Pos));
   end Set_Choice_Position;

   function Get_Associated_Expr (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Expr (Get_Kind (Target)),
                     "no field Associated_Expr");
      return Get_Field3 (Target);
   end Get_Associated_Expr;

   procedure Set_Associated_Expr (Target : Iir; Associated : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Expr (Get_Kind (Target)),
                     "no field Associated_Expr");
      Set_Field3 (Target, Associated);
   end Set_Associated_Expr;

   function Get_Associated_Block (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Block (Get_Kind (Target)),
                     "no field Associated_Block");
      return Get_Field3 (Target);
   end Get_Associated_Block;

   procedure Set_Associated_Block (Target : Iir; Associated : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Block (Get_Kind (Target)),
                     "no field Associated_Block");
      Set_Field3 (Target, Associated);
   end Set_Associated_Block;

   function Get_Associated_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Chain (Get_Kind (Target)),
                     "no field Associated_Chain");
      return Get_Field4 (Target);
   end Get_Associated_Chain;

   procedure Set_Associated_Chain (Target : Iir; Associated : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Chain (Get_Kind (Target)),
                     "no field Associated_Chain");
      Set_Field4 (Target, Associated);
   end Set_Associated_Chain;

   function Get_Choice_Name (Choice : Iir) return Iir is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Name (Get_Kind (Choice)),
                     "no field Choice_Name");
      return Get_Field5 (Choice);
   end Get_Choice_Name;

   procedure Set_Choice_Name (Choice : Iir; Name : Iir) is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Name (Get_Kind (Choice)),
                     "no field Choice_Name");
      Set_Field5 (Choice, Name);
   end Set_Choice_Name;

   function Get_Choice_Expression (Choice : Iir) return Iir is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Expression (Get_Kind (Choice)),
                     "no field Choice_Expression");
      return Get_Field5 (Choice);
   end Get_Choice_Expression;

   procedure Set_Choice_Expression (Choice : Iir; Name : Iir) is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Expression (Get_Kind (Choice)),
                     "no field Choice_Expression");
      Set_Field5 (Choice, Name);
   end Set_Choice_Expression;

   function Get_Choice_Range (Choice : Iir) return Iir is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Range (Get_Kind (Choice)),
                     "no field Choice_Range");
      return Get_Field5 (Choice);
   end Get_Choice_Range;

   procedure Set_Choice_Range (Choice : Iir; Name : Iir) is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Range (Get_Kind (Choice)),
                     "no field Choice_Range");
      Set_Field5 (Choice, Name);
   end Set_Choice_Range;

   function Get_Same_Alternative_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Same_Alternative_Flag (Get_Kind (Target)),
                     "no field Same_Alternative_Flag");
      return Get_Flag1 (Target);
   end Get_Same_Alternative_Flag;

   procedure Set_Same_Alternative_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Same_Alternative_Flag (Get_Kind (Target)),
                     "no field Same_Alternative_Flag");
      Set_Flag1 (Target, Val);
   end Set_Same_Alternative_Flag;

   function Get_Element_Type_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Type_Flag (Get_Kind (Target)),
                     "no field Element_Type_Flag");
      return Get_Flag2 (Target);
   end Get_Element_Type_Flag;

   procedure Set_Element_Type_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Type_Flag (Get_Kind (Target)),
                     "no field Element_Type_Flag");
      Set_Flag2 (Target, Val);
   end Set_Element_Type_Flag;

   function Get_Architecture (Target : Iir_Entity_Aspect_Entity) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Architecture (Get_Kind (Target)),
                     "no field Architecture");
      return Get_Field3 (Target);
   end Get_Architecture;

   procedure Set_Architecture (Target : Iir_Entity_Aspect_Entity; Arch : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Architecture (Get_Kind (Target)),
                     "no field Architecture");
      Set_Field3 (Target, Arch);
   end Set_Architecture;

   function Get_Block_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Specification (Get_Kind (Target)),
                     "no field Block_Specification");
      return Get_Field5 (Target);
   end Get_Block_Specification;

   procedure Set_Block_Specification (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Specification (Get_Kind (Target)),
                     "no field Block_Specification");
      Set_Field5 (Target, Block);
   end Set_Block_Specification;

   function Get_Prev_Block_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prev_Block_Configuration (Get_Kind (Target)),
                     "no field Prev_Block_Configuration");
      return Get_Field4 (Target);
   end Get_Prev_Block_Configuration;

   procedure Set_Prev_Block_Configuration (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prev_Block_Configuration (Get_Kind (Target)),
                     "no field Prev_Block_Configuration");
      Set_Field4 (Target, Block);
   end Set_Prev_Block_Configuration;

   function Get_Configuration_Item_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Item_Chain (Get_Kind (Target)),
                     "no field Configuration_Item_Chain");
      return Get_Field3 (Target);
   end Get_Configuration_Item_Chain;

   procedure Set_Configuration_Item_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Item_Chain (Get_Kind (Target)),
                     "no field Configuration_Item_Chain");
      Set_Field3 (Target, Chain);
   end Set_Configuration_Item_Chain;

   function Get_Attribute_Value_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Chain");
      return Get_Field4 (Target);
   end Get_Attribute_Value_Chain;

   procedure Set_Attribute_Value_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Chain");
      Set_Field4 (Target, Chain);
   end Set_Attribute_Value_Chain;

   function Get_Spec_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Spec_Chain (Get_Kind (Target)),
                     "no field Spec_Chain");
      return Get_Field2 (Target);
   end Get_Spec_Chain;

   procedure Set_Spec_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Spec_Chain (Get_Kind (Target)),
                     "no field Spec_Chain");
      Set_Field2 (Target, Chain);
   end Set_Spec_Chain;

   function Get_Value_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Value_Chain (Get_Kind (Target)),
                     "no field Value_Chain");
      return Get_Field0 (Target);
   end Get_Value_Chain;

   procedure Set_Value_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Value_Chain (Get_Kind (Target)),
                     "no field Value_Chain");
      Set_Field0 (Target, Chain);
   end Set_Value_Chain;

   function Get_Attribute_Value_Spec_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Spec_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Spec_Chain");
      return Get_Field4 (Target);
   end Get_Attribute_Value_Spec_Chain;

   procedure Set_Attribute_Value_Spec_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Spec_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Spec_Chain");
      Set_Field4 (Target, Chain);
   end Set_Attribute_Value_Spec_Chain;

   function Get_Entity_Name (Arch : Iir) return Iir is
   begin
      pragma Assert (Arch /= Null_Iir);
      pragma Assert (Has_Entity_Name (Get_Kind (Arch)),
                     "no field Entity_Name");
      return Get_Field2 (Arch);
   end Get_Entity_Name;

   procedure Set_Entity_Name (Arch : Iir; Entity : Iir) is
   begin
      pragma Assert (Arch /= Null_Iir);
      pragma Assert (Has_Entity_Name (Get_Kind (Arch)),
                     "no field Entity_Name");
      Set_Field2 (Arch, Entity);
   end Set_Entity_Name;

   function Get_Package (Package_Body : Iir) return Iir is
   begin
      pragma Assert (Package_Body /= Null_Iir);
      pragma Assert (Has_Package (Get_Kind (Package_Body)),
                     "no field Package");
      return Get_Field5 (Package_Body);
   end Get_Package;

   procedure Set_Package (Package_Body : Iir; Decl : Iir) is
   begin
      pragma Assert (Package_Body /= Null_Iir);
      pragma Assert (Has_Package (Get_Kind (Package_Body)),
                     "no field Package");
      Set_Field5 (Package_Body, Decl);
   end Set_Package;

   function Get_Package_Body (Pkg : Iir) return Iir is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Body (Get_Kind (Pkg)),
                     "no field Package_Body");
      return Get_Field5 (Pkg);
   end Get_Package_Body;

   procedure Set_Package_Body (Pkg : Iir; Decl : Iir) is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Body (Get_Kind (Pkg)),
                     "no field Package_Body");
      Set_Field5 (Pkg, Decl);
   end Set_Package_Body;

   function Get_Need_Body (Decl : Iir_Package_Declaration) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Body (Get_Kind (Decl)),
                     "no field Need_Body");
      return Get_Flag1 (Decl);
   end Get_Need_Body;

   procedure Set_Need_Body (Decl : Iir_Package_Declaration; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Body (Get_Kind (Decl)),
                     "no field Need_Body");
      Set_Flag1 (Decl, Flag);
   end Set_Need_Body;

   function Get_Macro_Expanded_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Macro_Expanded_Flag (Get_Kind (Decl)),
                     "no field Macro_Expanded_Flag");
      return Get_Flag2 (Decl);
   end Get_Macro_Expanded_Flag;

   procedure Set_Macro_Expanded_Flag (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Macro_Expanded_Flag (Get_Kind (Decl)),
                     "no field Macro_Expanded_Flag");
      Set_Flag2 (Decl, Flag);
   end Set_Macro_Expanded_Flag;

   function Get_Need_Instance_Bodies (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Instance_Bodies (Get_Kind (Decl)),
                     "no field Need_Instance_Bodies");
      return Get_Flag3 (Decl);
   end Get_Need_Instance_Bodies;

   procedure Set_Need_Instance_Bodies (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Instance_Bodies (Get_Kind (Decl)),
                     "no field Need_Instance_Bodies");
      Set_Flag3 (Decl, Flag);
   end Set_Need_Instance_Bodies;

   function Get_Block_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Configuration (Get_Kind (Target)),
                     "no field Block_Configuration");
      return Get_Field5 (Target);
   end Get_Block_Configuration;

   procedure Set_Block_Configuration (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Configuration (Get_Kind (Target)),
                     "no field Block_Configuration");
      Set_Field5 (Target, Block);
   end Set_Block_Configuration;

   function Get_Concurrent_Statement_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Concurrent_Statement_Chain (Get_Kind (Target)),
                     "no field Concurrent_Statement_Chain");
      return Get_Field5 (Target);
   end Get_Concurrent_Statement_Chain;

   procedure Set_Concurrent_Statement_Chain (Target : Iir; First : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Concurrent_Statement_Chain (Get_Kind (Target)),
                     "no field Concurrent_Statement_Chain");
      Set_Field5 (Target, First);
   end Set_Concurrent_Statement_Chain;

   function Get_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Chain (Get_Kind (Target)),
                     "no field Chain");
      return Get_Field2 (Target);
   end Get_Chain;

   procedure Set_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Chain (Get_Kind (Target)),
                     "no field Chain");
      Set_Field2 (Target, Chain);
   end Set_Chain;

   function Get_Port_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Chain (Get_Kind (Target)),
                     "no field Port_Chain");
      return Get_Field7 (Target);
   end Get_Port_Chain;

   procedure Set_Port_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Chain (Get_Kind (Target)),
                     "no field Port_Chain");
      Set_Field7 (Target, Chain);
   end Set_Port_Chain;

   function Get_Generic_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Chain (Get_Kind (Target)),
                     "no field Generic_Chain");
      return Get_Field6 (Target);
   end Get_Generic_Chain;

   procedure Set_Generic_Chain (Target : Iir; Generics : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Chain (Get_Kind (Target)),
                     "no field Generic_Chain");
      Set_Field6 (Target, Generics);
   end Set_Generic_Chain;

   function Get_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type (Get_Kind (Target)),
                     "no field Type");
      return Get_Field1 (Target);
   end Get_Type;

   procedure Set_Type (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type (Get_Kind (Target)),
                     "no field Type");
      Set_Field1 (Target, Atype);
   end Set_Type;

   function Get_Subtype_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Indication (Get_Kind (Target)),
                     "no field Subtype_Indication");
      return Get_Field5 (Target);
   end Get_Subtype_Indication;

   procedure Set_Subtype_Indication (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Indication (Get_Kind (Target)),
                     "no field Subtype_Indication");
      Set_Field5 (Target, Atype);
   end Set_Subtype_Indication;

   function Get_Discrete_Range (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Discrete_Range (Get_Kind (Target)),
                     "no field Discrete_Range");
      return Get_Field6 (Target);
   end Get_Discrete_Range;

   procedure Set_Discrete_Range (Target : Iir; Rng : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Discrete_Range (Get_Kind (Target)),
                     "no field Discrete_Range");
      Set_Field6 (Target, Rng);
   end Set_Discrete_Range;

   function Get_Type_Definition (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Type_Definition (Get_Kind (Decl)),
                     "no field Type_Definition");
      return Get_Field1 (Decl);
   end Get_Type_Definition;

   procedure Set_Type_Definition (Decl : Iir; Atype : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Type_Definition (Get_Kind (Decl)),
                     "no field Type_Definition");
      Set_Field1 (Decl, Atype);
   end Set_Type_Definition;

   function Get_Subtype_Definition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Definition (Get_Kind (Target)),
                     "no field Subtype_Definition");
      return Get_Field4 (Target);
   end Get_Subtype_Definition;

   procedure Set_Subtype_Definition (Target : Iir; Def : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Definition (Get_Kind (Target)),
                     "no field Subtype_Definition");
      Set_Field4 (Target, Def);
   end Set_Subtype_Definition;

   function Get_Incomplete_Type_Declaration (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Declaration (Get_Kind (N)),
                     "no field Incomplete_Type_Declaration");
      return Get_Field5 (N);
   end Get_Incomplete_Type_Declaration;

   procedure Set_Incomplete_Type_Declaration (N : Iir; Decl : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Declaration (Get_Kind (N)),
                     "no field Incomplete_Type_Declaration");
      Set_Field5 (N, Decl);
   end Set_Incomplete_Type_Declaration;

   function Get_Interface_Type_Subprograms (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Type_Subprograms (Get_Kind (Target)),
                     "no field Interface_Type_Subprograms");
      return Get_Field4 (Target);
   end Get_Interface_Type_Subprograms;

   procedure Set_Interface_Type_Subprograms (Target : Iir; Subprg : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Type_Subprograms (Get_Kind (Target)),
                     "no field Interface_Type_Subprograms");
      Set_Field4 (Target, Subprg);
   end Set_Interface_Type_Subprograms;

   function Get_Nature (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Nature (Get_Kind (Target)),
                     "no field Nature");
      return Get_Field1 (Target);
   end Get_Nature;

   procedure Set_Nature (Target : Iir; Nature : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Nature (Get_Kind (Target)),
                     "no field Nature");
      Set_Field1 (Target, Nature);
   end Set_Nature;

   type Iir_Mode_Conv is record
      Flag13: Boolean;
      Flag14: Boolean;
      Flag15: Boolean;
   end record;
   pragma Pack (Iir_Mode_Conv);
   pragma Assert (Iir_Mode_Conv'Size = Iir_Mode'Size);

   function Get_Mode (Target : Iir) return Iir_Mode
   is
      function To_Iir_Mode is new Ada.Unchecked_Conversion
         (Iir_Mode_Conv, Iir_Mode);
      Conv : Iir_Mode_Conv;
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Mode (Get_Kind (Target)),
                     "no field Mode");
      Conv.Flag13 := Get_Flag13 (Target);
      Conv.Flag14 := Get_Flag14 (Target);
      Conv.Flag15 := Get_Flag15 (Target);
      return To_Iir_Mode (Conv);
   end Get_Mode;

   procedure Set_Mode (Target : Iir; Mode : Iir_Mode)
   is
      function To_Iir_Mode_Conv is new Ada.Unchecked_Conversion
         (Iir_Mode, Iir_Mode_Conv);
      Conv : Iir_Mode_Conv;
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Mode (Get_Kind (Target)),
                     "no field Mode");
      Conv := To_Iir_Mode_Conv (Mode);
      Set_Flag13 (Target, Conv.Flag13);
      Set_Flag14 (Target, Conv.Flag14);
      Set_Flag15 (Target, Conv.Flag15);
   end Set_Mode;

   function Get_Guarded_Signal_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guarded_Signal_Flag (Get_Kind (Target)),
                     "no field Guarded_Signal_Flag");
      return Get_Flag8 (Target);
   end Get_Guarded_Signal_Flag;

   procedure Set_Guarded_Signal_Flag (Target : Iir; Guarded : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guarded_Signal_Flag (Get_Kind (Target)),
                     "no field Guarded_Signal_Flag");
      Set_Flag8 (Target, Guarded);
   end Set_Guarded_Signal_Flag;

   function Get_Signal_Kind (Target : Iir) return Iir_Signal_Kind is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Kind (Get_Kind (Target)),
                     "no field Signal_Kind");
      return Boolean_To_Iir_Signal_Kind (Get_Flag9 (Target));
   end Get_Signal_Kind;

   procedure Set_Signal_Kind (Target : Iir; Signal_Kind : Iir_Signal_Kind) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Kind (Get_Kind (Target)),
                     "no field Signal_Kind");
      Set_Flag9 (Target, Iir_Signal_Kind_To_Boolean (Signal_Kind));
   end Set_Signal_Kind;

   function Get_Base_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Base_Name (Get_Kind (Target)),
                     "no field Base_Name");
      return Get_Field5 (Target);
   end Get_Base_Name;

   procedure Set_Base_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Base_Name (Get_Kind (Target)),
                     "no field Base_Name");
      Set_Field5 (Target, Name);
   end Set_Base_Name;

   function Get_Interface_Declaration_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Declaration_Chain (Get_Kind (Target)),
                     "no field Interface_Declaration_Chain");
      return Get_Field5 (Target);
   end Get_Interface_Declaration_Chain;

   procedure Set_Interface_Declaration_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Declaration_Chain (Get_Kind (Target)),
                     "no field Interface_Declaration_Chain");
      Set_Field5 (Target, Chain);
   end Set_Interface_Declaration_Chain;

   function Get_Subprogram_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Specification (Get_Kind (Target)),
                     "no field Subprogram_Specification");
      return Get_Field6 (Target);
   end Get_Subprogram_Specification;

   procedure Set_Subprogram_Specification (Target : Iir; Spec : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Specification (Get_Kind (Target)),
                     "no field Subprogram_Specification");
      Set_Field6 (Target, Spec);
   end Set_Subprogram_Specification;

   function Get_Sequential_Statement_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sequential_Statement_Chain (Get_Kind (Target)),
                     "no field Sequential_Statement_Chain");
      return Get_Field5 (Target);
   end Get_Sequential_Statement_Chain;

   procedure Set_Sequential_Statement_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sequential_Statement_Chain (Get_Kind (Target)),
                     "no field Sequential_Statement_Chain");
      Set_Field5 (Target, Chain);
   end Set_Sequential_Statement_Chain;

   function Get_Subprogram_Body (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Body (Get_Kind (Target)),
                     "no field Subprogram_Body");
      return Get_Field9 (Target);
   end Get_Subprogram_Body;

   procedure Set_Subprogram_Body (Target : Iir; A_Body : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Body (Get_Kind (Target)),
                     "no field Subprogram_Body");
      Set_Field9 (Target, A_Body);
   end Set_Subprogram_Body;

   function Get_Overload_Number (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_Number (Get_Kind (Target)),
                     "no field Overload_Number");
      return Iir_Int32'Val (Get_Field12 (Target));
   end Get_Overload_Number;

   procedure Set_Overload_Number (Target : Iir; Val : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_Number (Get_Kind (Target)),
                     "no field Overload_Number");
      Set_Field12 (Target, Iir_Int32'Pos (Val));
   end Set_Overload_Number;

   function Get_Subprogram_Depth (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Depth (Get_Kind (Target)),
                     "no field Subprogram_Depth");
      return Iir_Int32'Val (Get_Field10 (Target));
   end Get_Subprogram_Depth;

   procedure Set_Subprogram_Depth (Target : Iir; Depth : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Depth (Get_Kind (Target)),
                     "no field Subprogram_Depth");
      Set_Field10 (Target, Iir_Int32'Pos (Depth));
   end Set_Subprogram_Depth;

   function Get_Subprogram_Hash (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Hash (Get_Kind (Target)),
                     "no field Subprogram_Hash");
      return Iir_Int32'Val (Get_Field4 (Target));
   end Get_Subprogram_Hash;

   procedure Set_Subprogram_Hash (Target : Iir; Val : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Hash (Get_Kind (Target)),
                     "no field Subprogram_Hash");
      Set_Field4 (Target, Iir_Int32'Pos (Val));
   end Set_Subprogram_Hash;

   function Get_Impure_Depth (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Impure_Depth (Get_Kind (Target)),
                     "no field Impure_Depth");
      return Iir_To_Iir_Int32 (Get_Field3 (Target));
   end Get_Impure_Depth;

   procedure Set_Impure_Depth (Target : Iir; Depth : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Impure_Depth (Get_Kind (Target)),
                     "no field Impure_Depth");
      Set_Field3 (Target, Iir_Int32_To_Iir (Depth));
   end Set_Impure_Depth;

   function Get_Return_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type (Get_Kind (Target)),
                     "no field Return_Type");
      return Get_Field1 (Target);
   end Get_Return_Type;

   procedure Set_Return_Type (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type (Get_Kind (Target)),
                     "no field Return_Type");
      Set_Field1 (Target, Decl);
   end Set_Return_Type;

   function Get_Implicit_Definition (D : Iir) return Iir_Predefined_Functions
   is
   begin
      pragma Assert (D /= Null_Iir);
      pragma Assert (Has_Implicit_Definition (Get_Kind (D)),
                     "no field Implicit_Definition");
      return Iir_Predefined_Functions'Val (Get_Field7 (D));
   end Get_Implicit_Definition;

   procedure Set_Implicit_Definition (D : Iir; Def : Iir_Predefined_Functions)
   is
   begin
      pragma Assert (D /= Null_Iir);
      pragma Assert (Has_Implicit_Definition (Get_Kind (D)),
                     "no field Implicit_Definition");
      Set_Field7 (D, Iir_Predefined_Functions'Pos (Def));
   end Set_Implicit_Definition;

   function Get_Default_Value (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Value (Get_Kind (Target)),
                     "no field Default_Value");
      return Get_Field4 (Target);
   end Get_Default_Value;

   procedure Set_Default_Value (Target : Iir; Value : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Value (Get_Kind (Target)),
                     "no field Default_Value");
      Set_Field4 (Target, Value);
   end Set_Default_Value;

   function Get_Deferred_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration (Get_Kind (Target)),
                     "no field Deferred_Declaration");
      return Get_Field6 (Target);
   end Get_Deferred_Declaration;

   procedure Set_Deferred_Declaration (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration (Get_Kind (Target)),
                     "no field Deferred_Declaration");
      Set_Field6 (Target, Decl);
   end Set_Deferred_Declaration;

   function Get_Deferred_Declaration_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration_Flag (Get_Kind (Target)),
                     "no field Deferred_Declaration_Flag");
      return Get_Flag1 (Target);
   end Get_Deferred_Declaration_Flag;

   procedure Set_Deferred_Declaration_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration_Flag (Get_Kind (Target)),
                     "no field Deferred_Declaration_Flag");
      Set_Flag1 (Target, Flag);
   end Set_Deferred_Declaration_Flag;

   function Get_Shared_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Shared_Flag (Get_Kind (Target)),
                     "no field Shared_Flag");
      return Get_Flag2 (Target);
   end Get_Shared_Flag;

   procedure Set_Shared_Flag (Target : Iir; Shared : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Shared_Flag (Get_Kind (Target)),
                     "no field Shared_Flag");
      Set_Flag2 (Target, Shared);
   end Set_Shared_Flag;

   function Get_Design_Unit (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Design_Unit (Get_Kind (Target)),
                     "no field Design_Unit");
      return Get_Field0 (Target);
   end Get_Design_Unit;

   procedure Set_Design_Unit (Target : Iir; Unit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Design_Unit (Get_Kind (Target)),
                     "no field Design_Unit");
      Set_Field0 (Target, Unit);
   end Set_Design_Unit;

   function Get_Block_Statement (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Statement (Get_Kind (Target)),
                     "no field Block_Statement");
      return Get_Field5 (Target);
   end Get_Block_Statement;

   procedure Set_Block_Statement (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Statement (Get_Kind (Target)),
                     "no field Block_Statement");
      Set_Field5 (Target, Block);
   end Set_Block_Statement;

   function Get_Signal_Driver (Target : Iir_Signal_Declaration) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Driver (Get_Kind (Target)),
                     "no field Signal_Driver");
      return Get_Field7 (Target);
   end Get_Signal_Driver;

   procedure Set_Signal_Driver (Target : Iir_Signal_Declaration; Driver : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Driver (Get_Kind (Target)),
                     "no field Signal_Driver");
      Set_Field7 (Target, Driver);
   end Set_Signal_Driver;

   function Get_Declaration_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Declaration_Chain (Get_Kind (Target)),
                     "no field Declaration_Chain");
      return Get_Field1 (Target);
   end Get_Declaration_Chain;

   procedure Set_Declaration_Chain (Target : Iir; Decls : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Declaration_Chain (Get_Kind (Target)),
                     "no field Declaration_Chain");
      Set_Field1 (Target, Decls);
   end Set_Declaration_Chain;

   function Get_File_Logical_Name (Target : Iir_File_Declaration) return Iir
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Logical_Name (Get_Kind (Target)),
                     "no field File_Logical_Name");
      return Get_Field6 (Target);
   end Get_File_Logical_Name;

   procedure Set_File_Logical_Name (Target : Iir_File_Declaration; Name : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Logical_Name (Get_Kind (Target)),
                     "no field File_Logical_Name");
      Set_Field6 (Target, Name);
   end Set_File_Logical_Name;

   function Get_File_Open_Kind (Target : Iir_File_Declaration) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Open_Kind (Get_Kind (Target)),
                     "no field File_Open_Kind");
      return Get_Field7 (Target);
   end Get_File_Open_Kind;

   procedure Set_File_Open_Kind (Target : Iir_File_Declaration; Kind : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Open_Kind (Get_Kind (Target)),
                     "no field File_Open_Kind");
      Set_Field7 (Target, Kind);
   end Set_File_Open_Kind;

   function Get_Element_Position (Target : Iir) return Iir_Index32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Position (Get_Kind (Target)),
                     "no field Element_Position");
      return Iir_Index32'Val (Get_Field4 (Target));
   end Get_Element_Position;

   procedure Set_Element_Position (Target : Iir; Pos : Iir_Index32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Position (Get_Kind (Target)),
                     "no field Element_Position");
      Set_Field4 (Target, Iir_Index32'Pos (Pos));
   end Set_Element_Position;

   function Get_Base_Element_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Base_Element_Declaration (Get_Kind (Target)),
                     "no field Base_Element_Declaration");
      return Get_Field2 (Target);
   end Get_Base_Element_Declaration;

   procedure Set_Base_Element_Declaration (Target : Iir; El : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Base_Element_Declaration (Get_Kind (Target)),
                     "no field Base_Element_Declaration");
      Set_Field2 (Target, El);
   end Set_Base_Element_Declaration;

   function Get_Element_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Declaration (Get_Kind (Target)),
                     "no field Element_Declaration");
      return Get_Field5 (Target);
   end Get_Element_Declaration;

   procedure Set_Element_Declaration (Target : Iir; El : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Declaration (Get_Kind (Target)),
                     "no field Element_Declaration");
      Set_Field5 (Target, El);
   end Set_Element_Declaration;

   function Get_Selected_Element (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Element (Get_Kind (Target)),
                     "no field Selected_Element");
      return Get_Field2 (Target);
   end Get_Selected_Element;

   procedure Set_Selected_Element (Target : Iir; El : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Element (Get_Kind (Target)),
                     "no field Selected_Element");
      Set_Field2 (Target, El);
   end Set_Selected_Element;

   function Get_Use_Clause_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Use_Clause_Chain (Get_Kind (Target)),
                     "no field Use_Clause_Chain");
      return Get_Field3 (Target);
   end Get_Use_Clause_Chain;

   procedure Set_Use_Clause_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Use_Clause_Chain (Get_Kind (Target)),
                     "no field Use_Clause_Chain");
      Set_Field3 (Target, Chain);
   end Set_Use_Clause_Chain;

   function Get_Context_Reference_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Context_Reference_Chain (Get_Kind (Target)),
                     "no field Context_Reference_Chain");
      return Get_Field3 (Target);
   end Get_Context_Reference_Chain;

   procedure Set_Context_Reference_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Context_Reference_Chain (Get_Kind (Target)),
                     "no field Context_Reference_Chain");
      Set_Field3 (Target, Chain);
   end Set_Context_Reference_Chain;

   function Get_Selected_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Name (Get_Kind (Target)),
                     "no field Selected_Name");
      return Get_Field1 (Target);
   end Get_Selected_Name;

   procedure Set_Selected_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Name (Get_Kind (Target)),
                     "no field Selected_Name");
      Set_Field1 (Target, Name);
   end Set_Selected_Name;

   function Get_Type_Declarator (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Type_Declarator (Get_Kind (Def)),
                     "no field Type_Declarator");
      return Get_Field3 (Def);
   end Get_Type_Declarator;

   procedure Set_Type_Declarator (Def : Iir; Decl : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Type_Declarator (Get_Kind (Def)),
                     "no field Type_Declarator");
      Set_Field3 (Def, Decl);
   end Set_Type_Declarator;

   function Get_Complete_Type_Definition (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Complete_Type_Definition (Get_Kind (N)),
                     "no field Complete_Type_Definition");
      return Get_Field5 (N);
   end Get_Complete_Type_Definition;

   procedure Set_Complete_Type_Definition (N : Iir; Def : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Complete_Type_Definition (Get_Kind (N)),
                     "no field Complete_Type_Definition");
      Set_Field5 (N, Def);
   end Set_Complete_Type_Definition;

   function Get_Incomplete_Type_Ref_Chain (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Ref_Chain (Get_Kind (N)),
                     "no field Incomplete_Type_Ref_Chain");
      return Get_Field0 (N);
   end Get_Incomplete_Type_Ref_Chain;

   procedure Set_Incomplete_Type_Ref_Chain (N : Iir; Def : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Ref_Chain (Get_Kind (N)),
                     "no field Incomplete_Type_Ref_Chain");
      Set_Field0 (N, Def);
   end Set_Incomplete_Type_Ref_Chain;

   function Get_Associated_Type (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Associated_Type (Get_Kind (Def)),
                     "no field Associated_Type");
      return Get_Field5 (Def);
   end Get_Associated_Type;

   procedure Set_Associated_Type (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Associated_Type (Get_Kind (Def)),
                     "no field Associated_Type");
      Set_Field5 (Def, Atype);
   end Set_Associated_Type;

   function Get_Enumeration_Literal_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Enumeration_Literal_List (Get_Kind (Target)),
                     "no field Enumeration_Literal_List");
      return Iir_To_Iir_Flist (Get_Field2 (Target));
   end Get_Enumeration_Literal_List;

   procedure Set_Enumeration_Literal_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Enumeration_Literal_List (Get_Kind (Target)),
                     "no field Enumeration_Literal_List");
      Set_Field2 (Target, Iir_Flist_To_Iir (List));
   end Set_Enumeration_Literal_List;

   function Get_Entity_Class_Entry_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class_Entry_Chain (Get_Kind (Target)),
                     "no field Entity_Class_Entry_Chain");
      return Get_Field1 (Target);
   end Get_Entity_Class_Entry_Chain;

   procedure Set_Entity_Class_Entry_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class_Entry_Chain (Get_Kind (Target)),
                     "no field Entity_Class_Entry_Chain");
      Set_Field1 (Target, Chain);
   end Set_Entity_Class_Entry_Chain;

   function Get_Group_Constituent_List (Group : Iir) return Iir_Flist is
   begin
      pragma Assert (Group /= Null_Iir);
      pragma Assert (Has_Group_Constituent_List (Get_Kind (Group)),
                     "no field Group_Constituent_List");
      return Iir_To_Iir_Flist (Get_Field1 (Group));
   end Get_Group_Constituent_List;

   procedure Set_Group_Constituent_List (Group : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Group /= Null_Iir);
      pragma Assert (Has_Group_Constituent_List (Get_Kind (Group)),
                     "no field Group_Constituent_List");
      Set_Field1 (Group, Iir_Flist_To_Iir (List));
   end Set_Group_Constituent_List;

   function Get_Unit_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Chain (Get_Kind (Target)),
                     "no field Unit_Chain");
      return Get_Field2 (Target);
   end Get_Unit_Chain;

   procedure Set_Unit_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Chain (Get_Kind (Target)),
                     "no field Unit_Chain");
      Set_Field2 (Target, Chain);
   end Set_Unit_Chain;

   function Get_Primary_Unit (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Primary_Unit (Get_Kind (Target)),
                     "no field Primary_Unit");
      return Get_Field2 (Target);
   end Get_Primary_Unit;

   procedure Set_Primary_Unit (Target : Iir; Unit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Primary_Unit (Get_Kind (Target)),
                     "no field Primary_Unit");
      Set_Field2 (Target, Unit);
   end Set_Primary_Unit;

   function Get_Identifier (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Identifier (Get_Kind (Target)),
                     "no field Identifier");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Identifier;

   procedure Set_Identifier (Target : Iir; Identifier : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Identifier (Get_Kind (Target)),
                     "no field Identifier");
      Set_Field3 (Target, Name_Id_To_Iir (Identifier));
   end Set_Identifier;

   function Get_Label (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Label (Get_Kind (Target)),
                     "no field Label");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Label;

   procedure Set_Label (Target : Iir; Label : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Label (Get_Kind (Target)),
                     "no field Label");
      Set_Field3 (Target, Name_Id_To_Iir (Label));
   end Set_Label;

   function Get_Visible_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Visible_Flag (Get_Kind (Target)),
                     "no field Visible_Flag");
      return Get_Flag4 (Target);
   end Get_Visible_Flag;

   procedure Set_Visible_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Visible_Flag (Get_Kind (Target)),
                     "no field Visible_Flag");
      Set_Flag4 (Target, Flag);
   end Set_Visible_Flag;

   function Get_Range_Constraint (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Range_Constraint (Get_Kind (Target)),
                     "no field Range_Constraint");
      return Get_Field1 (Target);
   end Get_Range_Constraint;

   procedure Set_Range_Constraint (Target : Iir; Constraint : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Range_Constraint (Get_Kind (Target)),
                     "no field Range_Constraint");
      Set_Field1 (Target, Constraint);
   end Set_Range_Constraint;

   function Get_Direction (Decl : Iir) return Iir_Direction is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Direction (Get_Kind (Decl)),
                     "no field Direction");
      return Iir_Direction'Val (Get_State2 (Decl));
   end Get_Direction;

   procedure Set_Direction (Decl : Iir; Dir : Iir_Direction) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Direction (Get_Kind (Decl)),
                     "no field Direction");
      Set_State2 (Decl, Iir_Direction'Pos (Dir));
   end Set_Direction;

   function Get_Left_Limit (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit (Get_Kind (Decl)),
                     "no field Left_Limit");
      return Get_Field4 (Decl);
   end Get_Left_Limit;

   procedure Set_Left_Limit (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit (Get_Kind (Decl)),
                     "no field Left_Limit");
      Set_Field4 (Decl, Limit);
   end Set_Left_Limit;

   function Get_Right_Limit (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit (Get_Kind (Decl)),
                     "no field Right_Limit");
      return Get_Field5 (Decl);
   end Get_Right_Limit;

   procedure Set_Right_Limit (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit (Get_Kind (Decl)),
                     "no field Right_Limit");
      Set_Field5 (Decl, Limit);
   end Set_Right_Limit;

   function Get_Left_Limit_Expr (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit_Expr (Get_Kind (Decl)),
                     "no field Left_Limit_Expr");
      return Get_Field2 (Decl);
   end Get_Left_Limit_Expr;

   procedure Set_Left_Limit_Expr (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit_Expr (Get_Kind (Decl)),
                     "no field Left_Limit_Expr");
      Set_Field2 (Decl, Limit);
   end Set_Left_Limit_Expr;

   function Get_Right_Limit_Expr (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit_Expr (Get_Kind (Decl)),
                     "no field Right_Limit_Expr");
      return Get_Field3 (Decl);
   end Get_Right_Limit_Expr;

   procedure Set_Right_Limit_Expr (Decl : Iir_Range_Expression; Limit : Iir)
   is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit_Expr (Get_Kind (Decl)),
                     "no field Right_Limit_Expr");
      Set_Field3 (Decl, Limit);
   end Set_Right_Limit_Expr;

   function Get_Base_Type (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Base_Type (Get_Kind (Decl)),
                     "no field Base_Type");
      return Get_Field4 (Decl);
   end Get_Base_Type;

   procedure Set_Base_Type (Decl : Iir; Base_Type : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Base_Type (Get_Kind (Decl)),
                     "no field Base_Type");
      Set_Field4 (Decl, Base_Type);
   end Set_Base_Type;

   function Get_Resolution_Indication (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Resolution_Indication (Get_Kind (Decl)),
                     "no field Resolution_Indication");
      return Get_Field5 (Decl);
   end Get_Resolution_Indication;

   procedure Set_Resolution_Indication (Decl : Iir; Ind : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Resolution_Indication (Get_Kind (Decl)),
                     "no field Resolution_Indication");
      Set_Field5 (Decl, Ind);
   end Set_Resolution_Indication;

   function Get_Record_Element_Resolution_Chain (Res : Iir) return Iir is
   begin
      pragma Assert (Res /= Null_Iir);
      pragma Assert (Has_Record_Element_Resolution_Chain (Get_Kind (Res)),
                     "no field Record_Element_Resolution_Chain");
      return Get_Field1 (Res);
   end Get_Record_Element_Resolution_Chain;

   procedure Set_Record_Element_Resolution_Chain (Res : Iir; Chain : Iir) is
   begin
      pragma Assert (Res /= Null_Iir);
      pragma Assert (Has_Record_Element_Resolution_Chain (Get_Kind (Res)),
                     "no field Record_Element_Resolution_Chain");
      Set_Field1 (Res, Chain);
   end Set_Record_Element_Resolution_Chain;

   function Get_Tolerance (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Tolerance (Get_Kind (Def)),
                     "no field Tolerance");
      return Get_Field7 (Def);
   end Get_Tolerance;

   procedure Set_Tolerance (Def : Iir; Tol : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Tolerance (Get_Kind (Def)),
                     "no field Tolerance");
      Set_Field7 (Def, Tol);
   end Set_Tolerance;

   function Get_Plus_Terminal (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Plus_Terminal (Get_Kind (Def)),
                     "no field Plus_Terminal");
      return Get_Field8 (Def);
   end Get_Plus_Terminal;

   procedure Set_Plus_Terminal (Def : Iir; Terminal : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Plus_Terminal (Get_Kind (Def)),
                     "no field Plus_Terminal");
      Set_Field8 (Def, Terminal);
   end Set_Plus_Terminal;

   function Get_Minus_Terminal (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Minus_Terminal (Get_Kind (Def)),
                     "no field Minus_Terminal");
      return Get_Field9 (Def);
   end Get_Minus_Terminal;

   procedure Set_Minus_Terminal (Def : Iir; Terminal : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Minus_Terminal (Get_Kind (Def)),
                     "no field Minus_Terminal");
      Set_Field9 (Def, Terminal);
   end Set_Minus_Terminal;

   function Get_Simultaneous_Left (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Left (Get_Kind (Def)),
                     "no field Simultaneous_Left");
      return Get_Field5 (Def);
   end Get_Simultaneous_Left;

   procedure Set_Simultaneous_Left (Def : Iir; Expr : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Left (Get_Kind (Def)),
                     "no field Simultaneous_Left");
      Set_Field5 (Def, Expr);
   end Set_Simultaneous_Left;

   function Get_Simultaneous_Right (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Right (Get_Kind (Def)),
                     "no field Simultaneous_Right");
      return Get_Field6 (Def);
   end Get_Simultaneous_Right;

   procedure Set_Simultaneous_Right (Def : Iir; Expr : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Right (Get_Kind (Def)),
                     "no field Simultaneous_Right");
      Set_Field6 (Def, Expr);
   end Set_Simultaneous_Right;

   function Get_Text_File_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Text_File_Flag (Get_Kind (Atype)),
                     "no field Text_File_Flag");
      return Get_Flag4 (Atype);
   end Get_Text_File_Flag;

   procedure Set_Text_File_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Text_File_Flag (Get_Kind (Atype)),
                     "no field Text_File_Flag");
      Set_Flag4 (Atype, Flag);
   end Set_Text_File_Flag;

   function Get_Only_Characters_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Only_Characters_Flag (Get_Kind (Atype)),
                     "no field Only_Characters_Flag");
      return Get_Flag4 (Atype);
   end Get_Only_Characters_Flag;

   procedure Set_Only_Characters_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Only_Characters_Flag (Get_Kind (Atype)),
                     "no field Only_Characters_Flag");
      Set_Flag4 (Atype, Flag);
   end Set_Only_Characters_Flag;

   function Get_Is_Character_Type (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Is_Character_Type (Get_Kind (Atype)),
                     "no field Is_Character_Type");
      return Get_Flag5 (Atype);
   end Get_Is_Character_Type;

   procedure Set_Is_Character_Type (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Is_Character_Type (Get_Kind (Atype)),
                     "no field Is_Character_Type");
      Set_Flag5 (Atype, Flag);
   end Set_Is_Character_Type;

   function Get_Type_Staticness (Atype : Iir) return Iir_Staticness is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Type_Staticness (Get_Kind (Atype)),
                     "no field Type_Staticness");
      return Iir_Staticness'Val (Get_State1 (Atype));
   end Get_Type_Staticness;

   procedure Set_Type_Staticness (Atype : Iir; Static : Iir_Staticness) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Type_Staticness (Get_Kind (Atype)),
                     "no field Type_Staticness");
      Set_State1 (Atype, Iir_Staticness'Pos (Static));
   end Set_Type_Staticness;

   function Get_Constraint_State (Atype : Iir) return Iir_Constraint is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Constraint_State (Get_Kind (Atype)),
                     "no field Constraint_State");
      return Iir_Constraint'Val (Get_State2 (Atype));
   end Get_Constraint_State;

   procedure Set_Constraint_State (Atype : Iir; State : Iir_Constraint) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Constraint_State (Get_Kind (Atype)),
                     "no field Constraint_State");
      Set_State2 (Atype, Iir_Constraint'Pos (State));
   end Set_Constraint_State;

   function Get_Index_Subtype_List (Decl : Iir) return Iir_Flist is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_Subtype_List (Get_Kind (Decl)),
                     "no field Index_Subtype_List");
      return Iir_To_Iir_Flist (Get_Field9 (Decl));
   end Get_Index_Subtype_List;

   procedure Set_Index_Subtype_List (Decl : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_Subtype_List (Get_Kind (Decl)),
                     "no field Index_Subtype_List");
      Set_Field9 (Decl, Iir_Flist_To_Iir (List));
   end Set_Index_Subtype_List;

   function Get_Index_Subtype_Definition_List (Def : Iir) return Iir_Flist is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Subtype_Definition_List (Get_Kind (Def)),
                     "no field Index_Subtype_Definition_List");
      return Iir_To_Iir_Flist (Get_Field6 (Def));
   end Get_Index_Subtype_Definition_List;

   procedure Set_Index_Subtype_Definition_List (Def : Iir; Idx : Iir_Flist) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Subtype_Definition_List (Get_Kind (Def)),
                     "no field Index_Subtype_Definition_List");
      Set_Field6 (Def, Iir_Flist_To_Iir (Idx));
   end Set_Index_Subtype_Definition_List;

   function Get_Element_Subtype_Indication (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype_Indication (Get_Kind (Decl)),
                     "no field Element_Subtype_Indication");
      return Get_Field2 (Decl);
   end Get_Element_Subtype_Indication;

   procedure Set_Element_Subtype_Indication (Decl : Iir; Sub_Type : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype_Indication (Get_Kind (Decl)),
                     "no field Element_Subtype_Indication");
      Set_Field2 (Decl, Sub_Type);
   end Set_Element_Subtype_Indication;

   function Get_Element_Subtype (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype (Get_Kind (Decl)),
                     "no field Element_Subtype");
      return Get_Field1 (Decl);
   end Get_Element_Subtype;

   procedure Set_Element_Subtype (Decl : Iir; Sub_Type : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype (Get_Kind (Decl)),
                     "no field Element_Subtype");
      Set_Field1 (Decl, Sub_Type);
   end Set_Element_Subtype;

   function Get_Index_Constraint_List (Def : Iir) return Iir_Flist is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Constraint_List (Get_Kind (Def)),
                     "no field Index_Constraint_List");
      return Iir_To_Iir_Flist (Get_Field6 (Def));
   end Get_Index_Constraint_List;

   procedure Set_Index_Constraint_List (Def : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Constraint_List (Get_Kind (Def)),
                     "no field Index_Constraint_List");
      Set_Field6 (Def, Iir_Flist_To_Iir (List));
   end Set_Index_Constraint_List;

   function Get_Array_Element_Constraint (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Array_Element_Constraint (Get_Kind (Def)),
                     "no field Array_Element_Constraint");
      return Get_Field8 (Def);
   end Get_Array_Element_Constraint;

   procedure Set_Array_Element_Constraint (Def : Iir; El : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Array_Element_Constraint (Get_Kind (Def)),
                     "no field Array_Element_Constraint");
      Set_Field8 (Def, El);
   end Set_Array_Element_Constraint;

   function Get_Elements_Declaration_List (Decl : Iir) return Iir_Flist is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Elements_Declaration_List (Get_Kind (Decl)),
                     "no field Elements_Declaration_List");
      return Iir_To_Iir_Flist (Get_Field1 (Decl));
   end Get_Elements_Declaration_List;

   procedure Set_Elements_Declaration_List (Decl : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Elements_Declaration_List (Get_Kind (Decl)),
                     "no field Elements_Declaration_List");
      Set_Field1 (Decl, Iir_Flist_To_Iir (List));
   end Set_Elements_Declaration_List;

   function Get_Designated_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Type (Get_Kind (Target)),
                     "no field Designated_Type");
      return Get_Field1 (Target);
   end Get_Designated_Type;

   procedure Set_Designated_Type (Target : Iir; Dtype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Type (Get_Kind (Target)),
                     "no field Designated_Type");
      Set_Field1 (Target, Dtype);
   end Set_Designated_Type;

   function Get_Designated_Subtype_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Subtype_Indication (Get_Kind (Target)),
                     "no field Designated_Subtype_Indication");
      return Get_Field5 (Target);
   end Get_Designated_Subtype_Indication;

   procedure Set_Designated_Subtype_Indication (Target : Iir; Dtype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Subtype_Indication (Get_Kind (Target)),
                     "no field Designated_Subtype_Indication");
      Set_Field5 (Target, Dtype);
   end Set_Designated_Subtype_Indication;

   function Get_Index_List (Decl : Iir) return Iir_Flist is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_List (Get_Kind (Decl)),
                     "no field Index_List");
      return Iir_To_Iir_Flist (Get_Field2 (Decl));
   end Get_Index_List;

   procedure Set_Index_List (Decl : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_List (Get_Kind (Decl)),
                     "no field Index_List");
      Set_Field2 (Decl, Iir_Flist_To_Iir (List));
   end Set_Index_List;

   function Get_Reference (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Reference (Get_Kind (Def)),
                     "no field Reference");
      return Get_Field2 (Def);
   end Get_Reference;

   procedure Set_Reference (Def : Iir; Ref : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Reference (Get_Kind (Def)),
                     "no field Reference");
      Set_Field2 (Def, Ref);
   end Set_Reference;

   function Get_Nature_Declarator (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Nature_Declarator (Get_Kind (Def)),
                     "no field Nature_Declarator");
      return Get_Field3 (Def);
   end Get_Nature_Declarator;

   procedure Set_Nature_Declarator (Def : Iir; Decl : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Nature_Declarator (Get_Kind (Def)),
                     "no field Nature_Declarator");
      Set_Field3 (Def, Decl);
   end Set_Nature_Declarator;

   function Get_Across_Type (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type (Get_Kind (Def)),
                     "no field Across_Type");
      return Get_Field7 (Def);
   end Get_Across_Type;

   procedure Set_Across_Type (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type (Get_Kind (Def)),
                     "no field Across_Type");
      Set_Field7 (Def, Atype);
   end Set_Across_Type;

   function Get_Through_Type (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type (Get_Kind (Def)),
                     "no field Through_Type");
      return Get_Field8 (Def);
   end Get_Through_Type;

   procedure Set_Through_Type (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type (Get_Kind (Def)),
                     "no field Through_Type");
      Set_Field8 (Def, Atype);
   end Set_Through_Type;

   function Get_Target (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Target (Get_Kind (Target)),
                     "no field Target");
      return Get_Field1 (Target);
   end Get_Target;

   procedure Set_Target (Target : Iir; Atarget : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Target (Get_Kind (Target)),
                     "no field Target");
      Set_Field1 (Target, Atarget);
   end Set_Target;

   function Get_Waveform_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Waveform_Chain (Get_Kind (Target)),
                     "no field Waveform_Chain");
      return Get_Field5 (Target);
   end Get_Waveform_Chain;

   procedure Set_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Waveform_Chain (Get_Kind (Target)),
                     "no field Waveform_Chain");
      Set_Field5 (Target, Chain);
   end Set_Waveform_Chain;

   function Get_Guard (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard (Get_Kind (Target)),
                     "no field Guard");
      return Get_Field8 (Target);
   end Get_Guard;

   procedure Set_Guard (Target : Iir; Guard : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard (Get_Kind (Target)),
                     "no field Guard");
      Set_Field8 (Target, Guard);
   end Set_Guard;

   function Get_Delay_Mechanism (Target : Iir) return Iir_Delay_Mechanism is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Delay_Mechanism (Get_Kind (Target)),
                     "no field Delay_Mechanism");
      return Boolean_To_Iir_Delay_Mechanism (Get_Flag1 (Target));
   end Get_Delay_Mechanism;

   procedure Set_Delay_Mechanism (Target : Iir; Kind : Iir_Delay_Mechanism) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Delay_Mechanism (Get_Kind (Target)),
                     "no field Delay_Mechanism");
      Set_Flag1 (Target, Iir_Delay_Mechanism_To_Boolean (Kind));
   end Set_Delay_Mechanism;

   function Get_Reject_Time_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Reject_Time_Expression (Get_Kind (Target)),
                     "no field Reject_Time_Expression");
      return Get_Field4 (Target);
   end Get_Reject_Time_Expression;

   procedure Set_Reject_Time_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Reject_Time_Expression (Get_Kind (Target)),
                     "no field Reject_Time_Expression");
      Set_Field4 (Target, Expr);
   end Set_Reject_Time_Expression;

   function Get_Sensitivity_List (Wait : Iir) return Iir_List is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Sensitivity_List (Get_Kind (Wait)),
                     "no field Sensitivity_List");
      return Iir_To_Iir_List (Get_Field6 (Wait));
   end Get_Sensitivity_List;

   procedure Set_Sensitivity_List (Wait : Iir; List : Iir_List) is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Sensitivity_List (Get_Kind (Wait)),
                     "no field Sensitivity_List");
      Set_Field6 (Wait, Iir_List_To_Iir (List));
   end Set_Sensitivity_List;

   function Get_Process_Origin (Proc : Iir) return Iir is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Process_Origin (Get_Kind (Proc)),
                     "no field Process_Origin");
      return Get_Field8 (Proc);
   end Get_Process_Origin;

   procedure Set_Process_Origin (Proc : Iir; Orig : Iir) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Process_Origin (Get_Kind (Proc)),
                     "no field Process_Origin");
      Set_Field8 (Proc, Orig);
   end Set_Process_Origin;

   function Get_Package_Origin (Pkg : Iir) return Iir is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Origin (Get_Kind (Pkg)),
                     "no field Package_Origin");
      return Get_Field7 (Pkg);
   end Get_Package_Origin;

   procedure Set_Package_Origin (Pkg : Iir; Orig : Iir) is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Origin (Get_Kind (Pkg)),
                     "no field Package_Origin");
      Set_Field7 (Pkg, Orig);
   end Set_Package_Origin;

   function Get_Condition_Clause (Wait : Iir_Wait_Statement) return Iir is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Condition_Clause (Get_Kind (Wait)),
                     "no field Condition_Clause");
      return Get_Field5 (Wait);
   end Get_Condition_Clause;

   procedure Set_Condition_Clause (Wait : Iir_Wait_Statement; Cond : Iir) is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Condition_Clause (Get_Kind (Wait)),
                     "no field Condition_Clause");
      Set_Field5 (Wait, Cond);
   end Set_Condition_Clause;

   function Get_Timeout_Clause (Wait : Iir_Wait_Statement) return Iir is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Timeout_Clause (Get_Kind (Wait)),
                     "no field Timeout_Clause");
      return Get_Field1 (Wait);
   end Get_Timeout_Clause;

   procedure Set_Timeout_Clause (Wait : Iir_Wait_Statement; Timeout : Iir) is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Timeout_Clause (Get_Kind (Wait)),
                     "no field Timeout_Clause");
      Set_Field1 (Wait, Timeout);
   end Set_Timeout_Clause;

   function Get_Postponed_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Postponed_Flag (Get_Kind (Target)),
                     "no field Postponed_Flag");
      return Get_Flag3 (Target);
   end Get_Postponed_Flag;

   procedure Set_Postponed_Flag (Target : Iir; Value : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Postponed_Flag (Get_Kind (Target)),
                     "no field Postponed_Flag");
      Set_Flag3 (Target, Value);
   end Set_Postponed_Flag;

   function Get_Callees_List (Proc : Iir) return Iir_List is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Callees_List (Get_Kind (Proc)),
                     "no field Callees_List");
      return Iir_To_Iir_List (Get_Field7 (Proc));
   end Get_Callees_List;

   procedure Set_Callees_List (Proc : Iir; List : Iir_List) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Callees_List (Get_Kind (Proc)),
                     "no field Callees_List");
      Set_Field7 (Proc, Iir_List_To_Iir (List));
   end Set_Callees_List;

   function Get_Passive_Flag (Proc : Iir) return Boolean is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Passive_Flag (Get_Kind (Proc)),
                     "no field Passive_Flag");
      return Get_Flag2 (Proc);
   end Get_Passive_Flag;

   procedure Set_Passive_Flag (Proc : Iir; Flag : Boolean) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Passive_Flag (Get_Kind (Proc)),
                     "no field Passive_Flag");
      Set_Flag2 (Proc, Flag);
   end Set_Passive_Flag;

   function Get_Resolution_Function_Flag (Func : Iir) return Boolean is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Resolution_Function_Flag (Get_Kind (Func)),
                     "no field Resolution_Function_Flag");
      return Get_Flag7 (Func);
   end Get_Resolution_Function_Flag;

   procedure Set_Resolution_Function_Flag (Func : Iir; Flag : Boolean) is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Resolution_Function_Flag (Get_Kind (Func)),
                     "no field Resolution_Function_Flag");
      Set_Flag7 (Func, Flag);
   end Set_Resolution_Function_Flag;

   function Get_Wait_State (Proc : Iir) return Tri_State_Type is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Wait_State (Get_Kind (Proc)),
                     "no field Wait_State");
      return Tri_State_Type'Val (Get_State1 (Proc));
   end Get_Wait_State;

   procedure Set_Wait_State (Proc : Iir; State : Tri_State_Type) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Wait_State (Get_Kind (Proc)),
                     "no field Wait_State");
      Set_State1 (Proc, Tri_State_Type'Pos (State));
   end Set_Wait_State;

   function Get_All_Sensitized_State (Proc : Iir) return Iir_All_Sensitized is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_All_Sensitized_State (Get_Kind (Proc)),
                     "no field All_Sensitized_State");
      return Iir_All_Sensitized'Val (Get_State3 (Proc));
   end Get_All_Sensitized_State;

   procedure Set_All_Sensitized_State (Proc : Iir; State : Iir_All_Sensitized)
   is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_All_Sensitized_State (Get_Kind (Proc)),
                     "no field All_Sensitized_State");
      Set_State3 (Proc, Iir_All_Sensitized'Pos (State));
   end Set_All_Sensitized_State;

   function Get_Seen_Flag (Proc : Iir) return Boolean is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Seen_Flag (Get_Kind (Proc)),
                     "no field Seen_Flag");
      return Get_Flag1 (Proc);
   end Get_Seen_Flag;

   procedure Set_Seen_Flag (Proc : Iir; Flag : Boolean) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Seen_Flag (Get_Kind (Proc)),
                     "no field Seen_Flag");
      Set_Flag1 (Proc, Flag);
   end Set_Seen_Flag;

   function Get_Pure_Flag (Func : Iir) return Boolean is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Pure_Flag (Get_Kind (Func)),
                     "no field Pure_Flag");
      return Get_Flag2 (Func);
   end Get_Pure_Flag;

   procedure Set_Pure_Flag (Func : Iir; Flag : Boolean) is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Pure_Flag (Get_Kind (Func)),
                     "no field Pure_Flag");
      Set_Flag2 (Func, Flag);
   end Set_Pure_Flag;

   function Get_Foreign_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Foreign_Flag (Get_Kind (Decl)),
                     "no field Foreign_Flag");
      return Get_Flag3 (Decl);
   end Get_Foreign_Flag;

   procedure Set_Foreign_Flag (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Foreign_Flag (Get_Kind (Decl)),
                     "no field Foreign_Flag");
      Set_Flag3 (Decl, Flag);
   end Set_Foreign_Flag;

   function Get_Resolved_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Resolved_Flag (Get_Kind (Atype)),
                     "no field Resolved_Flag");
      return Get_Flag1 (Atype);
   end Get_Resolved_Flag;

   procedure Set_Resolved_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Resolved_Flag (Get_Kind (Atype)),
                     "no field Resolved_Flag");
      Set_Flag1 (Atype, Flag);
   end Set_Resolved_Flag;

   function Get_Signal_Type_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Signal_Type_Flag (Get_Kind (Atype)),
                     "no field Signal_Type_Flag");
      return Get_Flag2 (Atype);
   end Get_Signal_Type_Flag;

   procedure Set_Signal_Type_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Signal_Type_Flag (Get_Kind (Atype)),
                     "no field Signal_Type_Flag");
      Set_Flag2 (Atype, Flag);
   end Set_Signal_Type_Flag;

   function Get_Has_Signal_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Has_Signal_Flag (Get_Kind (Atype)),
                     "no field Has_Signal_Flag");
      return Get_Flag3 (Atype);
   end Get_Has_Signal_Flag;

   procedure Set_Has_Signal_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Has_Signal_Flag (Get_Kind (Atype)),
                     "no field Has_Signal_Flag");
      Set_Flag3 (Atype, Flag);
   end Set_Has_Signal_Flag;

   function Get_Purity_State (Proc : Iir) return Iir_Pure_State is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Purity_State (Get_Kind (Proc)),
                     "no field Purity_State");
      return Iir_Pure_State'Val (Get_State2 (Proc));
   end Get_Purity_State;

   procedure Set_Purity_State (Proc : Iir; State : Iir_Pure_State) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Purity_State (Get_Kind (Proc)),
                     "no field Purity_State");
      Set_State2 (Proc, Iir_Pure_State'Pos (State));
   end Set_Purity_State;

   function Get_Elab_Flag (Design : Iir) return Boolean is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Elab_Flag (Get_Kind (Design)),
                     "no field Elab_Flag");
      return Get_Flag3 (Design);
   end Get_Elab_Flag;

   procedure Set_Elab_Flag (Design : Iir; Flag : Boolean) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Elab_Flag (Get_Kind (Design)),
                     "no field Elab_Flag");
      Set_Flag3 (Design, Flag);
   end Set_Elab_Flag;

   function Get_Configuration_Mark_Flag (Design : Iir) return Boolean is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Mark_Flag (Get_Kind (Design)),
                     "no field Configuration_Mark_Flag");
      return Get_Flag4 (Design);
   end Get_Configuration_Mark_Flag;

   procedure Set_Configuration_Mark_Flag (Design : Iir; Flag : Boolean) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Mark_Flag (Get_Kind (Design)),
                     "no field Configuration_Mark_Flag");
      Set_Flag4 (Design, Flag);
   end Set_Configuration_Mark_Flag;

   function Get_Configuration_Done_Flag (Design : Iir) return Boolean is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Done_Flag (Get_Kind (Design)),
                     "no field Configuration_Done_Flag");
      return Get_Flag5 (Design);
   end Get_Configuration_Done_Flag;

   procedure Set_Configuration_Done_Flag (Design : Iir; Flag : Boolean) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Done_Flag (Get_Kind (Design)),
                     "no field Configuration_Done_Flag");
      Set_Flag5 (Design, Flag);
   end Set_Configuration_Done_Flag;

   function Get_Index_Constraint_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Index_Constraint_Flag (Get_Kind (Atype)),
                     "no field Index_Constraint_Flag");
      return Get_Flag4 (Atype);
   end Get_Index_Constraint_Flag;

   procedure Set_Index_Constraint_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Index_Constraint_Flag (Get_Kind (Atype)),
                     "no field Index_Constraint_Flag");
      Set_Flag4 (Atype, Flag);
   end Set_Index_Constraint_Flag;

   function Get_Hide_Implicit_Flag (Subprg : Iir) return Boolean is
   begin
      pragma Assert (Subprg /= Null_Iir);
      pragma Assert (Has_Hide_Implicit_Flag (Get_Kind (Subprg)),
                     "no field Hide_Implicit_Flag");
      return Get_Flag12 (Subprg);
   end Get_Hide_Implicit_Flag;

   procedure Set_Hide_Implicit_Flag (Subprg : Iir; Flag : Boolean) is
   begin
      pragma Assert (Subprg /= Null_Iir);
      pragma Assert (Has_Hide_Implicit_Flag (Get_Kind (Subprg)),
                     "no field Hide_Implicit_Flag");
      Set_Flag12 (Subprg, Flag);
   end Set_Hide_Implicit_Flag;

   function Get_Assertion_Condition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Assertion_Condition (Get_Kind (Target)),
                     "no field Assertion_Condition");
      return Get_Field1 (Target);
   end Get_Assertion_Condition;

   procedure Set_Assertion_Condition (Target : Iir; Cond : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Assertion_Condition (Get_Kind (Target)),
                     "no field Assertion_Condition");
      Set_Field1 (Target, Cond);
   end Set_Assertion_Condition;

   function Get_Report_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Report_Expression (Get_Kind (Target)),
                     "no field Report_Expression");
      return Get_Field5 (Target);
   end Get_Report_Expression;

   procedure Set_Report_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Report_Expression (Get_Kind (Target)),
                     "no field Report_Expression");
      Set_Field5 (Target, Expr);
   end Set_Report_Expression;

   function Get_Severity_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Severity_Expression (Get_Kind (Target)),
                     "no field Severity_Expression");
      return Get_Field4 (Target);
   end Get_Severity_Expression;

   procedure Set_Severity_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Severity_Expression (Get_Kind (Target)),
                     "no field Severity_Expression");
      Set_Field4 (Target, Expr);
   end Set_Severity_Expression;

   function Get_Instantiated_Unit (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiated_Unit (Get_Kind (Target)),
                     "no field Instantiated_Unit");
      return Get_Field1 (Target);
   end Get_Instantiated_Unit;

   procedure Set_Instantiated_Unit (Target : Iir; Unit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiated_Unit (Get_Kind (Target)),
                     "no field Instantiated_Unit");
      Set_Field1 (Target, Unit);
   end Set_Instantiated_Unit;

   function Get_Generic_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Generic_Map_Aspect_Chain");
      return Get_Field8 (Target);
   end Get_Generic_Map_Aspect_Chain;

   procedure Set_Generic_Map_Aspect_Chain (Target : Iir; Generics : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Generic_Map_Aspect_Chain");
      Set_Field8 (Target, Generics);
   end Set_Generic_Map_Aspect_Chain;

   function Get_Port_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Port_Map_Aspect_Chain");
      return Get_Field9 (Target);
   end Get_Port_Map_Aspect_Chain;

   procedure Set_Port_Map_Aspect_Chain (Target : Iir; Port : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Port_Map_Aspect_Chain");
      Set_Field9 (Target, Port);
   end Set_Port_Map_Aspect_Chain;

   function Get_Configuration_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Name (Get_Kind (Target)),
                     "no field Configuration_Name");
      return Get_Field1 (Target);
   end Get_Configuration_Name;

   procedure Set_Configuration_Name (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Name (Get_Kind (Target)),
                     "no field Configuration_Name");
      Set_Field1 (Target, Conf);
   end Set_Configuration_Name;

   function Get_Component_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Configuration (Get_Kind (Target)),
                     "no field Component_Configuration");
      return Get_Field6 (Target);
   end Get_Component_Configuration;

   procedure Set_Component_Configuration (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Configuration (Get_Kind (Target)),
                     "no field Component_Configuration");
      Set_Field6 (Target, Conf);
   end Set_Component_Configuration;

   function Get_Configuration_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Specification (Get_Kind (Target)),
                     "no field Configuration_Specification");
      return Get_Field7 (Target);
   end Get_Configuration_Specification;

   procedure Set_Configuration_Specification (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Specification (Get_Kind (Target)),
                     "no field Configuration_Specification");
      Set_Field7 (Target, Conf);
   end Set_Configuration_Specification;

   function Get_Default_Binding_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Binding_Indication (Get_Kind (Target)),
                     "no field Default_Binding_Indication");
      return Get_Field5 (Target);
   end Get_Default_Binding_Indication;

   procedure Set_Default_Binding_Indication (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Binding_Indication (Get_Kind (Target)),
                     "no field Default_Binding_Indication");
      Set_Field5 (Target, Conf);
   end Set_Default_Binding_Indication;

   function Get_Default_Configuration_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Configuration_Declaration (Get_Kind (Target)),
                     "no field Default_Configuration_Declaration");
      return Get_Field6 (Target);
   end Get_Default_Configuration_Declaration;

   procedure Set_Default_Configuration_Declaration (Target : Iir; Conf : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Configuration_Declaration (Get_Kind (Target)),
                     "no field Default_Configuration_Declaration");
      Set_Field6 (Target, Conf);
   end Set_Default_Configuration_Declaration;

   function Get_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expression (Get_Kind (Target)),
                     "no field Expression");
      return Get_Field5 (Target);
   end Get_Expression;

   procedure Set_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expression (Get_Kind (Target)),
                     "no field Expression");
      Set_Field5 (Target, Expr);
   end Set_Expression;

   function Get_Conditional_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Expression (Get_Kind (Target)),
                     "no field Conditional_Expression");
      return Get_Field5 (Target);
   end Get_Conditional_Expression;

   procedure Set_Conditional_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Expression (Get_Kind (Target)),
                     "no field Conditional_Expression");
      Set_Field5 (Target, Expr);
   end Set_Conditional_Expression;

   function Get_Allocator_Designated_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Allocator_Designated_Type (Get_Kind (Target)),
                     "no field Allocator_Designated_Type");
      return Get_Field2 (Target);
   end Get_Allocator_Designated_Type;

   procedure Set_Allocator_Designated_Type (Target : Iir; A_Type : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Allocator_Designated_Type (Get_Kind (Target)),
                     "no field Allocator_Designated_Type");
      Set_Field2 (Target, A_Type);
   end Set_Allocator_Designated_Type;

   function Get_Selected_Waveform_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Waveform_Chain (Get_Kind (Target)),
                     "no field Selected_Waveform_Chain");
      return Get_Field7 (Target);
   end Get_Selected_Waveform_Chain;

   procedure Set_Selected_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Waveform_Chain (Get_Kind (Target)),
                     "no field Selected_Waveform_Chain");
      Set_Field7 (Target, Chain);
   end Set_Selected_Waveform_Chain;

   function Get_Conditional_Waveform_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Waveform_Chain (Get_Kind (Target)),
                     "no field Conditional_Waveform_Chain");
      return Get_Field5 (Target);
   end Get_Conditional_Waveform_Chain;

   procedure Set_Conditional_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Waveform_Chain (Get_Kind (Target)),
                     "no field Conditional_Waveform_Chain");
      Set_Field5 (Target, Chain);
   end Set_Conditional_Waveform_Chain;

   function Get_Guard_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Expression (Get_Kind (Target)),
                     "no field Guard_Expression");
      return Get_Field2 (Target);
   end Get_Guard_Expression;

   procedure Set_Guard_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Expression (Get_Kind (Target)),
                     "no field Guard_Expression");
      Set_Field2 (Target, Expr);
   end Set_Guard_Expression;

   function Get_Guard_Decl (Target : Iir_Block_Statement) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Decl (Get_Kind (Target)),
                     "no field Guard_Decl");
      return Get_Field8 (Target);
   end Get_Guard_Decl;

   procedure Set_Guard_Decl (Target : Iir_Block_Statement; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Decl (Get_Kind (Target)),
                     "no field Guard_Decl");
      Set_Field8 (Target, Decl);
   end Set_Guard_Decl;

   function Get_Guard_Sensitivity_List (Guard : Iir) return Iir_List is
   begin
      pragma Assert (Guard /= Null_Iir);
      pragma Assert (Has_Guard_Sensitivity_List (Get_Kind (Guard)),
                     "no field Guard_Sensitivity_List");
      return Iir_To_Iir_List (Get_Field4 (Guard));
   end Get_Guard_Sensitivity_List;

   procedure Set_Guard_Sensitivity_List (Guard : Iir; List : Iir_List) is
   begin
      pragma Assert (Guard /= Null_Iir);
      pragma Assert (Has_Guard_Sensitivity_List (Get_Kind (Guard)),
                     "no field Guard_Sensitivity_List");
      Set_Field4 (Guard, Iir_List_To_Iir (List));
   end Set_Guard_Sensitivity_List;

   function Get_Signal_Attribute_Chain (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Signal_Attribute_Chain (Get_Kind (Decl)),
                     "no field Signal_Attribute_Chain");
      return Get_Field3 (Decl);
   end Get_Signal_Attribute_Chain;

   procedure Set_Signal_Attribute_Chain (Decl : Iir; Chain : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Signal_Attribute_Chain (Get_Kind (Decl)),
                     "no field Signal_Attribute_Chain");
      Set_Field3 (Decl, Chain);
   end Set_Signal_Attribute_Chain;

   function Get_Block_Block_Configuration (Block : Iir) return Iir is
   begin
      pragma Assert (Block /= Null_Iir);
      pragma Assert (Has_Block_Block_Configuration (Get_Kind (Block)),
                     "no field Block_Block_Configuration");
      return Get_Field6 (Block);
   end Get_Block_Block_Configuration;

   procedure Set_Block_Block_Configuration (Block : Iir; Conf : Iir) is
   begin
      pragma Assert (Block /= Null_Iir);
      pragma Assert (Has_Block_Block_Configuration (Get_Kind (Block)),
                     "no field Block_Block_Configuration");
      Set_Field6 (Block, Conf);
   end Set_Block_Block_Configuration;

   function Get_Package_Header (Pkg : Iir) return Iir is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Header (Get_Kind (Pkg)),
                     "no field Package_Header");
      return Get_Field6 (Pkg);
   end Get_Package_Header;

   procedure Set_Package_Header (Pkg : Iir; Header : Iir) is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Header (Get_Kind (Pkg)),
                     "no field Package_Header");
      Set_Field6 (Pkg, Header);
   end Set_Package_Header;

   function Get_Block_Header (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Header (Get_Kind (Target)),
                     "no field Block_Header");
      return Get_Field7 (Target);
   end Get_Block_Header;

   procedure Set_Block_Header (Target : Iir; Header : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Header (Get_Kind (Target)),
                     "no field Block_Header");
      Set_Field7 (Target, Header);
   end Set_Block_Header;

   function Get_Uninstantiated_Package_Name (Inst : Iir) return Iir is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Name (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Name");
      return Get_Field7 (Inst);
   end Get_Uninstantiated_Package_Name;

   procedure Set_Uninstantiated_Package_Name (Inst : Iir; Name : Iir) is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Name (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Name");
      Set_Field7 (Inst, Name);
   end Set_Uninstantiated_Package_Name;

   function Get_Uninstantiated_Package_Decl (Inst : Iir) return Iir is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Decl (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Decl");
      return Get_Field9 (Inst);
   end Get_Uninstantiated_Package_Decl;

   procedure Set_Uninstantiated_Package_Decl (Inst : Iir; Pkg : Iir) is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Decl (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Decl");
      Set_Field9 (Inst, Pkg);
   end Set_Uninstantiated_Package_Decl;

   function Get_Instance_Source_File (Inst : Iir) return Source_File_Entry is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Instance_Source_File (Get_Kind (Inst)),
                     "no field Instance_Source_File");
      return Iir_To_Source_File_Entry (Get_Field10 (Inst));
   end Get_Instance_Source_File;

   procedure Set_Instance_Source_File (Inst : Iir; File : Source_File_Entry)
   is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Instance_Source_File (Get_Kind (Inst)),
                     "no field Instance_Source_File");
      Set_Field10 (Inst, Source_File_Entry_To_Iir (File));
   end Set_Instance_Source_File;

   function Get_Generate_Block_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Block_Configuration (Get_Kind (Target)),
                     "no field Generate_Block_Configuration");
      return Get_Field2 (Target);
   end Get_Generate_Block_Configuration;

   procedure Set_Generate_Block_Configuration (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Block_Configuration (Get_Kind (Target)),
                     "no field Generate_Block_Configuration");
      Set_Field2 (Target, Conf);
   end Set_Generate_Block_Configuration;

   function Get_Generate_Statement_Body (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Statement_Body (Get_Kind (Target)),
                     "no field Generate_Statement_Body");
      return Get_Field4 (Target);
   end Get_Generate_Statement_Body;

   procedure Set_Generate_Statement_Body (Target : Iir; Bod : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Statement_Body (Get_Kind (Target)),
                     "no field Generate_Statement_Body");
      Set_Field4 (Target, Bod);
   end Set_Generate_Statement_Body;

   function Get_Alternative_Label (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Alternative_Label (Get_Kind (Target)),
                     "no field Alternative_Label");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Alternative_Label;

   procedure Set_Alternative_Label (Target : Iir; Label : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Alternative_Label (Get_Kind (Target)),
                     "no field Alternative_Label");
      Set_Field3 (Target, Name_Id_To_Iir (Label));
   end Set_Alternative_Label;

   function Get_Generate_Else_Clause (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Else_Clause (Get_Kind (Target)),
                     "no field Generate_Else_Clause");
      return Get_Field5 (Target);
   end Get_Generate_Else_Clause;

   procedure Set_Generate_Else_Clause (Target : Iir; Clause : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Else_Clause (Get_Kind (Target)),
                     "no field Generate_Else_Clause");
      Set_Field5 (Target, Clause);
   end Set_Generate_Else_Clause;

   function Get_Condition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Condition (Get_Kind (Target)),
                     "no field Condition");
      return Get_Field1 (Target);
   end Get_Condition;

   procedure Set_Condition (Target : Iir; Condition : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Condition (Get_Kind (Target)),
                     "no field Condition");
      Set_Field1 (Target, Condition);
   end Set_Condition;

   function Get_Else_Clause (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Else_Clause (Get_Kind (Target)),
                     "no field Else_Clause");
      return Get_Field4 (Target);
   end Get_Else_Clause;

   procedure Set_Else_Clause (Target : Iir; Clause : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Else_Clause (Get_Kind (Target)),
                     "no field Else_Clause");
      Set_Field4 (Target, Clause);
   end Set_Else_Clause;

   function Get_Parameter_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Specification (Get_Kind (Target)),
                     "no field Parameter_Specification");
      return Get_Field1 (Target);
   end Get_Parameter_Specification;

   procedure Set_Parameter_Specification (Target : Iir; Param : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Specification (Get_Kind (Target)),
                     "no field Parameter_Specification");
      Set_Field1 (Target, Param);
   end Set_Parameter_Specification;

   function Get_Parent (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parent (Get_Kind (Target)),
                     "no field Parent");
      return Get_Field0 (Target);
   end Get_Parent;

   procedure Set_Parent (Target : Iir; Parent : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parent (Get_Kind (Target)),
                     "no field Parent");
      Set_Field0 (Target, Parent);
   end Set_Parent;

   function Get_Loop_Label (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Loop_Label (Get_Kind (Target)),
                     "no field Loop_Label");
      return Get_Field5 (Target);
   end Get_Loop_Label;

   procedure Set_Loop_Label (Target : Iir; Stmt : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Loop_Label (Get_Kind (Target)),
                     "no field Loop_Label");
      Set_Field5 (Target, Stmt);
   end Set_Loop_Label;

   function Get_Component_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Name (Get_Kind (Target)),
                     "no field Component_Name");
      return Get_Field4 (Target);
   end Get_Component_Name;

   procedure Set_Component_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Name (Get_Kind (Target)),
                     "no field Component_Name");
      Set_Field4 (Target, Name);
   end Set_Component_Name;

   function Get_Instantiation_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiation_List (Get_Kind (Target)),
                     "no field Instantiation_List");
      return Iir_To_Iir_Flist (Get_Field1 (Target));
   end Get_Instantiation_List;

   procedure Set_Instantiation_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiation_List (Get_Kind (Target)),
                     "no field Instantiation_List");
      Set_Field1 (Target, Iir_Flist_To_Iir (List));
   end Set_Instantiation_List;

   function Get_Entity_Aspect (Target : Iir_Binding_Indication) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Aspect (Get_Kind (Target)),
                     "no field Entity_Aspect");
      return Get_Field3 (Target);
   end Get_Entity_Aspect;

   procedure Set_Entity_Aspect (Target : Iir_Binding_Indication; Entity : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Aspect (Get_Kind (Target)),
                     "no field Entity_Aspect");
      Set_Field3 (Target, Entity);
   end Set_Entity_Aspect;

   function Get_Default_Entity_Aspect (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Entity_Aspect (Get_Kind (Target)),
                     "no field Default_Entity_Aspect");
      return Get_Field1 (Target);
   end Get_Default_Entity_Aspect;

   procedure Set_Default_Entity_Aspect (Target : Iir; Aspect : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Entity_Aspect (Get_Kind (Target)),
                     "no field Default_Entity_Aspect");
      Set_Field1 (Target, Aspect);
   end Set_Default_Entity_Aspect;

   function Get_Binding_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Binding_Indication (Get_Kind (Target)),
                     "no field Binding_Indication");
      return Get_Field3 (Target);
   end Get_Binding_Indication;

   procedure Set_Binding_Indication (Target : Iir; Binding : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Binding_Indication (Get_Kind (Target)),
                     "no field Binding_Indication");
      Set_Field3 (Target, Binding);
   end Set_Binding_Indication;

   function Get_Named_Entity (Name : Iir) return Iir is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Named_Entity (Get_Kind (Name)),
                     "no field Named_Entity");
      return Get_Field4 (Name);
   end Get_Named_Entity;

   procedure Set_Named_Entity (Name : Iir; Val : Iir) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Named_Entity (Get_Kind (Name)),
                     "no field Named_Entity");
      Set_Field4 (Name, Val);
   end Set_Named_Entity;

   function Get_Alias_Declaration (Name : Iir) return Iir is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Alias_Declaration (Get_Kind (Name)),
                     "no field Alias_Declaration");
      return Get_Field2 (Name);
   end Get_Alias_Declaration;

   procedure Set_Alias_Declaration (Name : Iir; Val : Iir) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Alias_Declaration (Get_Kind (Name)),
                     "no field Alias_Declaration");
      Set_Field2 (Name, Val);
   end Set_Alias_Declaration;

   function Get_Referenced_Name (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Referenced_Name (Get_Kind (N)),
                     "no field Referenced_Name");
      return Get_Field2 (N);
   end Get_Referenced_Name;

   procedure Set_Referenced_Name (N : Iir; Name : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Referenced_Name (Get_Kind (N)),
                     "no field Referenced_Name");
      Set_Field2 (N, Name);
   end Set_Referenced_Name;

   function Get_Expr_Staticness (Target : Iir) return Iir_Staticness is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expr_Staticness (Get_Kind (Target)),
                     "no field Expr_Staticness");
      return Iir_Staticness'Val (Get_State1 (Target));
   end Get_Expr_Staticness;

   procedure Set_Expr_Staticness (Target : Iir; Static : Iir_Staticness) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expr_Staticness (Get_Kind (Target)),
                     "no field Expr_Staticness");
      Set_State1 (Target, Iir_Staticness'Pos (Static));
   end Set_Expr_Staticness;

   function Get_Error_Origin (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Error_Origin (Get_Kind (Target)),
                     "no field Error_Origin");
      return Get_Field2 (Target);
   end Get_Error_Origin;

   procedure Set_Error_Origin (Target : Iir; Origin : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Error_Origin (Get_Kind (Target)),
                     "no field Error_Origin");
      Set_Field2 (Target, Origin);
   end Set_Error_Origin;

   function Get_Operand (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Operand (Get_Kind (Target)),
                     "no field Operand");
      return Get_Field2 (Target);
   end Get_Operand;

   procedure Set_Operand (Target : Iir; An_Iir : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Operand (Get_Kind (Target)),
                     "no field Operand");
      Set_Field2 (Target, An_Iir);
   end Set_Operand;

   function Get_Left (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Left (Get_Kind (Target)),
                     "no field Left");
      return Get_Field2 (Target);
   end Get_Left;

   procedure Set_Left (Target : Iir; An_Iir : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Left (Get_Kind (Target)),
                     "no field Left");
      Set_Field2 (Target, An_Iir);
   end Set_Left;

   function Get_Right (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Right (Get_Kind (Target)),
                     "no field Right");
      return Get_Field4 (Target);
   end Get_Right;

   procedure Set_Right (Target : Iir; An_Iir : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Right (Get_Kind (Target)),
                     "no field Right");
      Set_Field4 (Target, An_Iir);
   end Set_Right;

   function Get_Physical_Unit (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Physical_Unit (Get_Kind (Lit)),
                     "no field Physical_Unit");
      return Get_Field3 (Lit);
   end Get_Physical_Unit;

   procedure Set_Physical_Unit (Lit : Iir; Name : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Physical_Unit (Get_Kind (Lit)),
                     "no field Physical_Unit");
      Set_Field3 (Lit, Name);
   end Set_Physical_Unit;

   function Get_Unit_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Name (Get_Kind (Target)),
                     "no field Unit_Name");
      return Get_Field0 (Target);
   end Get_Unit_Name;

   procedure Set_Unit_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Name (Get_Kind (Target)),
                     "no field Unit_Name");
      Set_Field0 (Target, Name);
   end Set_Unit_Name;

   function Get_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name (Get_Kind (Target)),
                     "no field Name");
      return Get_Field4 (Target);
   end Get_Name;

   procedure Set_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name (Get_Kind (Target)),
                     "no field Name");
      Set_Field4 (Target, Name);
   end Set_Name;

   function Get_Group_Template_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Group_Template_Name (Get_Kind (Target)),
                     "no field Group_Template_Name");
      return Get_Field5 (Target);
   end Get_Group_Template_Name;

   procedure Set_Group_Template_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Group_Template_Name (Get_Kind (Target)),
                     "no field Group_Template_Name");
      Set_Field5 (Target, Name);
   end Set_Group_Template_Name;

   function Get_Name_Staticness (Target : Iir) return Iir_Staticness is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name_Staticness (Get_Kind (Target)),
                     "no field Name_Staticness");
      return Iir_Staticness'Val (Get_State2 (Target));
   end Get_Name_Staticness;

   procedure Set_Name_Staticness (Target : Iir; Static : Iir_Staticness) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name_Staticness (Get_Kind (Target)),
                     "no field Name_Staticness");
      Set_State2 (Target, Iir_Staticness'Pos (Static));
   end Set_Name_Staticness;

   function Get_Prefix (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prefix (Get_Kind (Target)),
                     "no field Prefix");
      return Get_Field0 (Target);
   end Get_Prefix;

   procedure Set_Prefix (Target : Iir; Prefix : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prefix (Get_Kind (Target)),
                     "no field Prefix");
      Set_Field0 (Target, Prefix);
   end Set_Prefix;

   function Get_Signature_Prefix (Sign : Iir) return Iir is
   begin
      pragma Assert (Sign /= Null_Iir);
      pragma Assert (Has_Signature_Prefix (Get_Kind (Sign)),
                     "no field Signature_Prefix");
      return Get_Field1 (Sign);
   end Get_Signature_Prefix;

   procedure Set_Signature_Prefix (Sign : Iir; Prefix : Iir) is
   begin
      pragma Assert (Sign /= Null_Iir);
      pragma Assert (Has_Signature_Prefix (Get_Kind (Sign)),
                     "no field Signature_Prefix");
      Set_Field1 (Sign, Prefix);
   end Set_Signature_Prefix;

   function Get_External_Pathname (Name : Iir) return Iir is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_External_Pathname (Get_Kind (Name)),
                     "no field External_Pathname");
      return Get_Field3 (Name);
   end Get_External_Pathname;

   procedure Set_External_Pathname (Name : Iir; Path : Iir) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_External_Pathname (Get_Kind (Name)),
                     "no field External_Pathname");
      Set_Field3 (Name, Path);
   end Set_External_Pathname;

   function Get_Pathname_Suffix (Path : Iir) return Iir is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Suffix (Get_Kind (Path)),
                     "no field Pathname_Suffix");
      return Get_Field2 (Path);
   end Get_Pathname_Suffix;

   procedure Set_Pathname_Suffix (Path : Iir; Suffix : Iir) is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Suffix (Get_Kind (Path)),
                     "no field Pathname_Suffix");
      Set_Field2 (Path, Suffix);
   end Set_Pathname_Suffix;

   function Get_Pathname_Expression (Path : Iir) return Iir is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Expression (Get_Kind (Path)),
                     "no field Pathname_Expression");
      return Get_Field5 (Path);
   end Get_Pathname_Expression;

   procedure Set_Pathname_Expression (Path : Iir; Expr : Iir) is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Expression (Get_Kind (Path)),
                     "no field Pathname_Expression");
      Set_Field5 (Path, Expr);
   end Set_Pathname_Expression;

   function Get_In_Formal_Flag (Name : Iir) return Boolean is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_In_Formal_Flag (Get_Kind (Name)),
                     "no field In_Formal_Flag");
      return Get_Flag4 (Name);
   end Get_In_Formal_Flag;

   procedure Set_In_Formal_Flag (Name : Iir; Flag : Boolean) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_In_Formal_Flag (Get_Kind (Name)),
                     "no field In_Formal_Flag");
      Set_Flag4 (Name, Flag);
   end Set_In_Formal_Flag;

   function Get_Slice_Subtype (Slice : Iir) return Iir is
   begin
      pragma Assert (Slice /= Null_Iir);
      pragma Assert (Has_Slice_Subtype (Get_Kind (Slice)),
                     "no field Slice_Subtype");
      return Get_Field3 (Slice);
   end Get_Slice_Subtype;

   procedure Set_Slice_Subtype (Slice : Iir; Atype : Iir) is
   begin
      pragma Assert (Slice /= Null_Iir);
      pragma Assert (Has_Slice_Subtype (Get_Kind (Slice)),
                     "no field Slice_Subtype");
      Set_Field3 (Slice, Atype);
   end Set_Slice_Subtype;

   function Get_Suffix (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Suffix (Get_Kind (Target)),
                     "no field Suffix");
      return Get_Field2 (Target);
   end Get_Suffix;

   procedure Set_Suffix (Target : Iir; Suffix : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Suffix (Get_Kind (Target)),
                     "no field Suffix");
      Set_Field2 (Target, Suffix);
   end Set_Suffix;

   function Get_Index_Subtype (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Index_Subtype (Get_Kind (Attr)),
                     "no field Index_Subtype");
      return Get_Field2 (Attr);
   end Get_Index_Subtype;

   procedure Set_Index_Subtype (Attr : Iir; St : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Index_Subtype (Get_Kind (Attr)),
                     "no field Index_Subtype");
      Set_Field2 (Attr, St);
   end Set_Index_Subtype;

   function Get_Parameter (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter (Get_Kind (Target)),
                     "no field Parameter");
      return Get_Field4 (Target);
   end Get_Parameter;

   procedure Set_Parameter (Target : Iir; Param : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter (Get_Kind (Target)),
                     "no field Parameter");
      Set_Field4 (Target, Param);
   end Set_Parameter;

   function Get_Attr_Chain (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attr_Chain (Get_Kind (Attr)),
                     "no field Attr_Chain");
      return Get_Field2 (Attr);
   end Get_Attr_Chain;

   procedure Set_Attr_Chain (Attr : Iir; Chain : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attr_Chain (Get_Kind (Attr)),
                     "no field Attr_Chain");
      Set_Field2 (Attr, Chain);
   end Set_Attr_Chain;

   function Get_Signal_Attribute_Declaration (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Signal_Attribute_Declaration (Get_Kind (Attr)),
                     "no field Signal_Attribute_Declaration");
      return Get_Field3 (Attr);
   end Get_Signal_Attribute_Declaration;

   procedure Set_Signal_Attribute_Declaration (Attr : Iir; Decl : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Signal_Attribute_Declaration (Get_Kind (Attr)),
                     "no field Signal_Attribute_Declaration");
      Set_Field3 (Attr, Decl);
   end Set_Signal_Attribute_Declaration;

   function Get_Actual_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type (Get_Kind (Target)),
                     "no field Actual_Type");
      return Get_Field5 (Target);
   end Get_Actual_Type;

   procedure Set_Actual_Type (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type (Get_Kind (Target)),
                     "no field Actual_Type");
      Set_Field5 (Target, Atype);
   end Set_Actual_Type;

   function Get_Actual_Type_Definition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type_Definition (Get_Kind (Target)),
                     "no field Actual_Type_Definition");
      return Get_Field3 (Target);
   end Get_Actual_Type_Definition;

   procedure Set_Actual_Type_Definition (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type_Definition (Get_Kind (Target)),
                     "no field Actual_Type_Definition");
      Set_Field3 (Target, Atype);
   end Set_Actual_Type_Definition;

   function Get_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Chain (Get_Kind (Target)),
                     "no field Association_Chain");
      return Get_Field2 (Target);
   end Get_Association_Chain;

   procedure Set_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Chain (Get_Kind (Target)),
                     "no field Association_Chain");
      Set_Field2 (Target, Chain);
   end Set_Association_Chain;

   function Get_Individual_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Individual_Association_Chain (Get_Kind (Target)),
                     "no field Individual_Association_Chain");
      return Get_Field4 (Target);
   end Get_Individual_Association_Chain;

   procedure Set_Individual_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Individual_Association_Chain (Get_Kind (Target)),
                     "no field Individual_Association_Chain");
      Set_Field4 (Target, Chain);
   end Set_Individual_Association_Chain;

   function Get_Subprogram_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Association_Chain (Get_Kind (Target)),
                     "no field Subprogram_Association_Chain");
      return Get_Field4 (Target);
   end Get_Subprogram_Association_Chain;

   procedure Set_Subprogram_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Association_Chain (Get_Kind (Target)),
                     "no field Subprogram_Association_Chain");
      Set_Field4 (Target, Chain);
   end Set_Subprogram_Association_Chain;

   function Get_Aggregate_Info (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggregate_Info (Get_Kind (Target)),
                     "no field Aggregate_Info");
      return Get_Field5 (Target);
   end Get_Aggregate_Info;

   procedure Set_Aggregate_Info (Target : Iir; Info : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggregate_Info (Get_Kind (Target)),
                     "no field Aggregate_Info");
      Set_Field5 (Target, Info);
   end Set_Aggregate_Info;

   function Get_Sub_Aggregate_Info (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sub_Aggregate_Info (Get_Kind (Target)),
                     "no field Sub_Aggregate_Info");
      return Get_Field1 (Target);
   end Get_Sub_Aggregate_Info;

   procedure Set_Sub_Aggregate_Info (Target : Iir; Info : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sub_Aggregate_Info (Get_Kind (Target)),
                     "no field Sub_Aggregate_Info");
      Set_Field1 (Target, Info);
   end Set_Sub_Aggregate_Info;

   function Get_Aggr_Dynamic_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Dynamic_Flag (Get_Kind (Target)),
                     "no field Aggr_Dynamic_Flag");
      return Get_Flag3 (Target);
   end Get_Aggr_Dynamic_Flag;

   procedure Set_Aggr_Dynamic_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Dynamic_Flag (Get_Kind (Target)),
                     "no field Aggr_Dynamic_Flag");
      Set_Flag3 (Target, Val);
   end Set_Aggr_Dynamic_Flag;

   function Get_Aggr_Min_Length (Info : Iir_Aggregate_Info) return Iir_Int32
   is
   begin
      pragma Assert (Info /= Null_Iir);
      pragma Assert (Has_Aggr_Min_Length (Get_Kind (Info)),
                     "no field Aggr_Min_Length");
      return Iir_To_Iir_Int32 (Get_Field4 (Info));
   end Get_Aggr_Min_Length;

   procedure Set_Aggr_Min_Length (Info : Iir_Aggregate_Info; Nbr : Iir_Int32)
   is
   begin
      pragma Assert (Info /= Null_Iir);
      pragma Assert (Has_Aggr_Min_Length (Get_Kind (Info)),
                     "no field Aggr_Min_Length");
      Set_Field4 (Info, Iir_Int32_To_Iir (Nbr));
   end Set_Aggr_Min_Length;

   function Get_Aggr_Low_Limit (Target : Iir_Aggregate_Info) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Low_Limit (Get_Kind (Target)),
                     "no field Aggr_Low_Limit");
      return Get_Field2 (Target);
   end Get_Aggr_Low_Limit;

   procedure Set_Aggr_Low_Limit (Target : Iir_Aggregate_Info; Limit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Low_Limit (Get_Kind (Target)),
                     "no field Aggr_Low_Limit");
      Set_Field2 (Target, Limit);
   end Set_Aggr_Low_Limit;

   function Get_Aggr_High_Limit (Target : Iir_Aggregate_Info) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_High_Limit (Get_Kind (Target)),
                     "no field Aggr_High_Limit");
      return Get_Field3 (Target);
   end Get_Aggr_High_Limit;

   procedure Set_Aggr_High_Limit (Target : Iir_Aggregate_Info; Limit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_High_Limit (Get_Kind (Target)),
                     "no field Aggr_High_Limit");
      Set_Field3 (Target, Limit);
   end Set_Aggr_High_Limit;

   function Get_Aggr_Others_Flag (Target : Iir_Aggregate_Info) return Boolean
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Others_Flag (Get_Kind (Target)),
                     "no field Aggr_Others_Flag");
      return Get_Flag2 (Target);
   end Get_Aggr_Others_Flag;

   procedure Set_Aggr_Others_Flag (Target : Iir_Aggregate_Info; Val : Boolean)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Others_Flag (Get_Kind (Target)),
                     "no field Aggr_Others_Flag");
      Set_Flag2 (Target, Val);
   end Set_Aggr_Others_Flag;

   function Get_Aggr_Named_Flag (Target : Iir_Aggregate_Info) return Boolean
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Named_Flag (Get_Kind (Target)),
                     "no field Aggr_Named_Flag");
      return Get_Flag4 (Target);
   end Get_Aggr_Named_Flag;

   procedure Set_Aggr_Named_Flag (Target : Iir_Aggregate_Info; Val : Boolean)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Named_Flag (Get_Kind (Target)),
                     "no field Aggr_Named_Flag");
      Set_Flag4 (Target, Val);
   end Set_Aggr_Named_Flag;

   function Get_Aggregate_Expand_Flag (Aggr : Iir) return Boolean is
   begin
      pragma Assert (Aggr /= Null_Iir);
      pragma Assert (Has_Aggregate_Expand_Flag (Get_Kind (Aggr)),
                     "no field Aggregate_Expand_Flag");
      return Get_Flag1 (Aggr);
   end Get_Aggregate_Expand_Flag;

   procedure Set_Aggregate_Expand_Flag (Aggr : Iir; Flag : Boolean) is
   begin
      pragma Assert (Aggr /= Null_Iir);
      pragma Assert (Has_Aggregate_Expand_Flag (Get_Kind (Aggr)),
                     "no field Aggregate_Expand_Flag");
      Set_Flag1 (Aggr, Flag);
   end Set_Aggregate_Expand_Flag;

   function Get_Association_Choices_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Choices_Chain (Get_Kind (Target)),
                     "no field Association_Choices_Chain");
      return Get_Field4 (Target);
   end Get_Association_Choices_Chain;

   procedure Set_Association_Choices_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Choices_Chain (Get_Kind (Target)),
                     "no field Association_Choices_Chain");
      Set_Field4 (Target, Chain);
   end Set_Association_Choices_Chain;

   function Get_Case_Statement_Alternative_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Case_Statement_Alternative_Chain (Get_Kind (Target)),
                     "no field Case_Statement_Alternative_Chain");
      return Get_Field1 (Target);
   end Get_Case_Statement_Alternative_Chain;

   procedure Set_Case_Statement_Alternative_Chain (Target : Iir; Chain : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Case_Statement_Alternative_Chain (Get_Kind (Target)),
                     "no field Case_Statement_Alternative_Chain");
      Set_Field1 (Target, Chain);
   end Set_Case_Statement_Alternative_Chain;

   function Get_Choice_Staticness (Target : Iir) return Iir_Staticness is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Choice_Staticness (Get_Kind (Target)),
                     "no field Choice_Staticness");
      return Iir_Staticness'Val (Get_State1 (Target));
   end Get_Choice_Staticness;

   procedure Set_Choice_Staticness (Target : Iir; Staticness : Iir_Staticness)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Choice_Staticness (Get_Kind (Target)),
                     "no field Choice_Staticness");
      Set_State1 (Target, Iir_Staticness'Pos (Staticness));
   end Set_Choice_Staticness;

   function Get_Procedure_Call (Stmt : Iir) return Iir is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Procedure_Call (Get_Kind (Stmt)),
                     "no field Procedure_Call");
      return Get_Field1 (Stmt);
   end Get_Procedure_Call;

   procedure Set_Procedure_Call (Stmt : Iir; Call : Iir) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Procedure_Call (Get_Kind (Stmt)),
                     "no field Procedure_Call");
      Set_Field1 (Stmt, Call);
   end Set_Procedure_Call;

   function Get_Implementation (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Implementation (Get_Kind (Target)),
                     "no field Implementation");
      return Get_Field3 (Target);
   end Get_Implementation;

   procedure Set_Implementation (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Implementation (Get_Kind (Target)),
                     "no field Implementation");
      Set_Field3 (Target, Decl);
   end Set_Implementation;

   function Get_Parameter_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Association_Chain (Get_Kind (Target)),
                     "no field Parameter_Association_Chain");
      return Get_Field2 (Target);
   end Get_Parameter_Association_Chain;

   procedure Set_Parameter_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Association_Chain (Get_Kind (Target)),
                     "no field Parameter_Association_Chain");
      Set_Field2 (Target, Chain);
   end Set_Parameter_Association_Chain;

   function Get_Method_Object (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Method_Object (Get_Kind (Target)),
                     "no field Method_Object");
      return Get_Field4 (Target);
   end Get_Method_Object;

   procedure Set_Method_Object (Target : Iir; Object : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Method_Object (Get_Kind (Target)),
                     "no field Method_Object");
      Set_Field4 (Target, Object);
   end Set_Method_Object;

   function Get_Subtype_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Type_Mark (Get_Kind (Target)),
                     "no field Subtype_Type_Mark");
      return Get_Field2 (Target);
   end Get_Subtype_Type_Mark;

   procedure Set_Subtype_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Type_Mark (Get_Kind (Target)),
                     "no field Subtype_Type_Mark");
      Set_Field2 (Target, Mark);
   end Set_Subtype_Type_Mark;

   function Get_Type_Conversion_Subtype (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Conversion_Subtype (Get_Kind (Target)),
                     "no field Type_Conversion_Subtype");
      return Get_Field3 (Target);
   end Get_Type_Conversion_Subtype;

   procedure Set_Type_Conversion_Subtype (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Conversion_Subtype (Get_Kind (Target)),
                     "no field Type_Conversion_Subtype");
      Set_Field3 (Target, Atype);
   end Set_Type_Conversion_Subtype;

   function Get_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Mark (Get_Kind (Target)),
                     "no field Type_Mark");
      return Get_Field4 (Target);
   end Get_Type_Mark;

   procedure Set_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Mark (Get_Kind (Target)),
                     "no field Type_Mark");
      Set_Field4 (Target, Mark);
   end Set_Type_Mark;

   function Get_File_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Type_Mark (Get_Kind (Target)),
                     "no field File_Type_Mark");
      return Get_Field2 (Target);
   end Get_File_Type_Mark;

   procedure Set_File_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Type_Mark (Get_Kind (Target)),
                     "no field File_Type_Mark");
      Set_Field2 (Target, Mark);
   end Set_File_Type_Mark;

   function Get_Return_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type_Mark (Get_Kind (Target)),
                     "no field Return_Type_Mark");
      return Get_Field8 (Target);
   end Get_Return_Type_Mark;

   procedure Set_Return_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type_Mark (Get_Kind (Target)),
                     "no field Return_Type_Mark");
      Set_Field8 (Target, Mark);
   end Set_Return_Type_Mark;

   function Get_Has_Disconnect_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Disconnect_Flag (Get_Kind (Target)),
                     "no field Has_Disconnect_Flag");
      return Get_Flag1 (Target);
   end Get_Has_Disconnect_Flag;

   procedure Set_Has_Disconnect_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Disconnect_Flag (Get_Kind (Target)),
                     "no field Has_Disconnect_Flag");
      Set_Flag1 (Target, Val);
   end Set_Has_Disconnect_Flag;

   function Get_Has_Active_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Active_Flag (Get_Kind (Target)),
                     "no field Has_Active_Flag");
      return Get_Flag2 (Target);
   end Get_Has_Active_Flag;

   procedure Set_Has_Active_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Active_Flag (Get_Kind (Target)),
                     "no field Has_Active_Flag");
      Set_Flag2 (Target, Val);
   end Set_Has_Active_Flag;

   function Get_Is_Within_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Is_Within_Flag (Get_Kind (Target)),
                     "no field Is_Within_Flag");
      return Get_Flag5 (Target);
   end Get_Is_Within_Flag;

   procedure Set_Is_Within_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Is_Within_Flag (Get_Kind (Target)),
                     "no field Is_Within_Flag");
      Set_Flag5 (Target, Val);
   end Set_Is_Within_Flag;

   function Get_Type_Marks_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Marks_List (Get_Kind (Target)),
                     "no field Type_Marks_List");
      return Iir_To_Iir_Flist (Get_Field2 (Target));
   end Get_Type_Marks_List;

   procedure Set_Type_Marks_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Marks_List (Get_Kind (Target)),
                     "no field Type_Marks_List");
      Set_Field2 (Target, Iir_Flist_To_Iir (List));
   end Set_Type_Marks_List;

   function Get_Implicit_Alias_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Implicit_Alias_Flag (Get_Kind (Decl)),
                     "no field Implicit_Alias_Flag");
      return Get_Flag1 (Decl);
   end Get_Implicit_Alias_Flag;

   procedure Set_Implicit_Alias_Flag (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Implicit_Alias_Flag (Get_Kind (Decl)),
                     "no field Implicit_Alias_Flag");
      Set_Flag1 (Decl, Flag);
   end Set_Implicit_Alias_Flag;

   function Get_Alias_Signature (Alias : Iir) return Iir is
   begin
      pragma Assert (Alias /= Null_Iir);
      pragma Assert (Has_Alias_Signature (Get_Kind (Alias)),
                     "no field Alias_Signature");
      return Get_Field5 (Alias);
   end Get_Alias_Signature;

   procedure Set_Alias_Signature (Alias : Iir; Signature : Iir) is
   begin
      pragma Assert (Alias /= Null_Iir);
      pragma Assert (Has_Alias_Signature (Get_Kind (Alias)),
                     "no field Alias_Signature");
      Set_Field5 (Alias, Signature);
   end Set_Alias_Signature;

   function Get_Attribute_Signature (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attribute_Signature (Get_Kind (Attr)),
                     "no field Attribute_Signature");
      return Get_Field2 (Attr);
   end Get_Attribute_Signature;

   procedure Set_Attribute_Signature (Attr : Iir; Signature : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attribute_Signature (Get_Kind (Attr)),
                     "no field Attribute_Signature");
      Set_Field2 (Attr, Signature);
   end Set_Attribute_Signature;

   function Get_Overload_List (Target : Iir) return Iir_List is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_List (Get_Kind (Target)),
                     "no field Overload_List");
      return Iir_To_Iir_List (Get_Field1 (Target));
   end Get_Overload_List;

   procedure Set_Overload_List (Target : Iir; List : Iir_List) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_List (Get_Kind (Target)),
                     "no field Overload_List");
      Set_Field1 (Target, Iir_List_To_Iir (List));
   end Set_Overload_List;

   function Get_Simple_Name_Identifier (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Identifier (Get_Kind (Target)),
                     "no field Simple_Name_Identifier");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Simple_Name_Identifier;

   procedure Set_Simple_Name_Identifier (Target : Iir; Ident : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Identifier (Get_Kind (Target)),
                     "no field Simple_Name_Identifier");
      Set_Field3 (Target, Name_Id_To_Iir (Ident));
   end Set_Simple_Name_Identifier;

   function Get_Simple_Name_Subtype (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Subtype (Get_Kind (Target)),
                     "no field Simple_Name_Subtype");
      return Get_Field4 (Target);
   end Get_Simple_Name_Subtype;

   procedure Set_Simple_Name_Subtype (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Subtype (Get_Kind (Target)),
                     "no field Simple_Name_Subtype");
      Set_Field4 (Target, Atype);
   end Set_Simple_Name_Subtype;

   function Get_Protected_Type_Body (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Body (Get_Kind (Target)),
                     "no field Protected_Type_Body");
      return Get_Field2 (Target);
   end Get_Protected_Type_Body;

   procedure Set_Protected_Type_Body (Target : Iir; Bod : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Body (Get_Kind (Target)),
                     "no field Protected_Type_Body");
      Set_Field2 (Target, Bod);
   end Set_Protected_Type_Body;

   function Get_Protected_Type_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Declaration (Get_Kind (Target)),
                     "no field Protected_Type_Declaration");
      return Get_Field4 (Target);
   end Get_Protected_Type_Declaration;

   procedure Set_Protected_Type_Declaration (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Declaration (Get_Kind (Target)),
                     "no field Protected_Type_Declaration");
      Set_Field4 (Target, Decl);
   end Set_Protected_Type_Declaration;

   function Get_Use_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Use_Flag (Get_Kind (Decl)),
                     "no field Use_Flag");
      return Get_Flag6 (Decl);
   end Get_Use_Flag;

   procedure Set_Use_Flag (Decl : Iir; Val : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Use_Flag (Get_Kind (Decl)),
                     "no field Use_Flag");
      Set_Flag6 (Decl, Val);
   end Set_Use_Flag;

   function Get_End_Has_Reserved_Id (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Reserved_Id (Get_Kind (Decl)),
                     "no field End_Has_Reserved_Id");
      return Get_Flag8 (Decl);
   end Get_End_Has_Reserved_Id;

   procedure Set_End_Has_Reserved_Id (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Reserved_Id (Get_Kind (Decl)),
                     "no field End_Has_Reserved_Id");
      Set_Flag8 (Decl, Flag);
   end Set_End_Has_Reserved_Id;

   function Get_End_Has_Identifier (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Identifier (Get_Kind (Decl)),
                     "no field End_Has_Identifier");
      return Get_Flag9 (Decl);
   end Get_End_Has_Identifier;

   procedure Set_End_Has_Identifier (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Identifier (Get_Kind (Decl)),
                     "no field End_Has_Identifier");
      Set_Flag9 (Decl, Flag);
   end Set_End_Has_Identifier;

   function Get_End_Has_Postponed (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Postponed (Get_Kind (Decl)),
                     "no field End_Has_Postponed");
      return Get_Flag10 (Decl);
   end Get_End_Has_Postponed;

   procedure Set_End_Has_Postponed (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Postponed (Get_Kind (Decl)),
                     "no field End_Has_Postponed");
      Set_Flag10 (Decl, Flag);
   end Set_End_Has_Postponed;

   function Get_Has_Label (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Label (Get_Kind (Decl)),
                     "no field Has_Label");
      return Get_Flag6 (Decl);
   end Get_Has_Label;

   procedure Set_Has_Label (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Label (Get_Kind (Decl)),
                     "no field Has_Label");
      Set_Flag6 (Decl, Flag);
   end Set_Has_Label;

   function Get_Has_Begin (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Begin (Get_Kind (Decl)),
                     "no field Has_Begin");
      return Get_Flag10 (Decl);
   end Get_Has_Begin;

   procedure Set_Has_Begin (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Begin (Get_Kind (Decl)),
                     "no field Has_Begin");
      Set_Flag10 (Decl, Flag);
   end Set_Has_Begin;

   function Get_Has_End (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_End (Get_Kind (Decl)),
                     "no field Has_End");
      return Get_Flag11 (Decl);
   end Get_Has_End;

   procedure Set_Has_End (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_End (Get_Kind (Decl)),
                     "no field Has_End");
      Set_Flag11 (Decl, Flag);
   end Set_Has_End;

   function Get_Has_Is (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Is (Get_Kind (Decl)),
                     "no field Has_Is");
      return Get_Flag7 (Decl);
   end Get_Has_Is;

   procedure Set_Has_Is (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Is (Get_Kind (Decl)),
                     "no field Has_Is");
      Set_Flag7 (Decl, Flag);
   end Set_Has_Is;

   function Get_Has_Pure (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Pure (Get_Kind (Decl)),
                     "no field Has_Pure");
      return Get_Flag8 (Decl);
   end Get_Has_Pure;

   procedure Set_Has_Pure (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Pure (Get_Kind (Decl)),
                     "no field Has_Pure");
      Set_Flag8 (Decl, Flag);
   end Set_Has_Pure;

   function Get_Has_Body (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Body (Get_Kind (Decl)),
                     "no field Has_Body");
      return Get_Flag9 (Decl);
   end Get_Has_Body;

   procedure Set_Has_Body (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Body (Get_Kind (Decl)),
                     "no field Has_Body");
      Set_Flag9 (Decl, Flag);
   end Set_Has_Body;

   function Get_Has_Parameter (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Parameter (Get_Kind (Decl)),
                     "no field Has_Parameter");
      return Get_Flag10 (Decl);
   end Get_Has_Parameter;

   procedure Set_Has_Parameter (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Parameter (Get_Kind (Decl)),
                     "no field Has_Parameter");
      Set_Flag10 (Decl, Flag);
   end Set_Has_Parameter;

   function Get_Has_Component (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Component (Get_Kind (Decl)),
                     "no field Has_Component");
      return Get_Flag5 (Decl);
   end Get_Has_Component;

   procedure Set_Has_Component (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Component (Get_Kind (Decl)),
                     "no field Has_Component");
      Set_Flag5 (Decl, Flag);
   end Set_Has_Component;

   function Get_Has_Identifier_List (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (Decl)),
                     "no field Has_Identifier_List");
      return Get_Flag3 (Decl);
   end Get_Has_Identifier_List;

   procedure Set_Has_Identifier_List (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (Decl)),
                     "no field Has_Identifier_List");
      Set_Flag3 (Decl, Flag);
   end Set_Has_Identifier_List;

   function Get_Has_Mode (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Mode (Get_Kind (Decl)),
                     "no field Has_Mode");
      return Get_Flag10 (Decl);
   end Get_Has_Mode;

   procedure Set_Has_Mode (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Mode (Get_Kind (Decl)),
                     "no field Has_Mode");
      Set_Flag10 (Decl, Flag);
   end Set_Has_Mode;

   function Get_Has_Class (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Class (Get_Kind (Decl)),
                     "no field Has_Class");
      return Get_Flag11 (Decl);
   end Get_Has_Class;

   procedure Set_Has_Class (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Class (Get_Kind (Decl)),
                     "no field Has_Class");
      Set_Flag11 (Decl, Flag);
   end Set_Has_Class;

   function Get_Suspend_Flag (Stmt : Iir) return Boolean is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Suspend_Flag (Get_Kind (Stmt)),
                     "no field Suspend_Flag");
      return Get_Flag11 (Stmt);
   end Get_Suspend_Flag;

   procedure Set_Suspend_Flag (Stmt : Iir; Flag : Boolean) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Suspend_Flag (Get_Kind (Stmt)),
                     "no field Suspend_Flag");
      Set_Flag11 (Stmt, Flag);
   end Set_Suspend_Flag;

   function Get_Is_Ref (N : Iir) return Boolean is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Ref (Get_Kind (N)),
                     "no field Is_Ref");
      return Get_Flag12 (N);
   end Get_Is_Ref;

   procedure Set_Is_Ref (N : Iir; Ref : Boolean) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Ref (Get_Kind (N)),
                     "no field Is_Ref");
      Set_Flag12 (N, Ref);
   end Set_Is_Ref;

   function Get_Is_Forward_Ref (N : Iir) return Boolean is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Forward_Ref (Get_Kind (N)),
                     "no field Is_Forward_Ref");
      return Get_Flag1 (N);
   end Get_Is_Forward_Ref;

   procedure Set_Is_Forward_Ref (N : Iir; Ref : Boolean) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Forward_Ref (Get_Kind (N)),
                     "no field Is_Forward_Ref");
      Set_Flag1 (N, Ref);
   end Set_Is_Forward_Ref;

   function Get_Psl_Property (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Property (Get_Kind (Decl)),
                     "no field Psl_Property");
      return Iir_To_PSL_Node (Get_Field1 (Decl));
   end Get_Psl_Property;

   procedure Set_Psl_Property (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Property (Get_Kind (Decl)),
                     "no field Psl_Property");
      Set_Field1 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Property;

   function Get_Psl_Sequence (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Sequence (Get_Kind (Decl)),
                     "no field Psl_Sequence");
      return Iir_To_PSL_Node (Get_Field1 (Decl));
   end Get_Psl_Sequence;

   procedure Set_Psl_Sequence (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Sequence (Get_Kind (Decl)),
                     "no field Psl_Sequence");
      Set_Field1 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Sequence;

   function Get_Psl_Declaration (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Declaration (Get_Kind (Decl)),
                     "no field Psl_Declaration");
      return Iir_To_PSL_Node (Get_Field6 (Decl));
   end Get_Psl_Declaration;

   procedure Set_Psl_Declaration (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Declaration (Get_Kind (Decl)),
                     "no field Psl_Declaration");
      Set_Field6 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Declaration;

   function Get_Psl_Expression (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Expression (Get_Kind (Decl)),
                     "no field Psl_Expression");
      return Iir_To_PSL_Node (Get_Field3 (Decl));
   end Get_Psl_Expression;

   procedure Set_Psl_Expression (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Expression (Get_Kind (Decl)),
                     "no field Psl_Expression");
      Set_Field3 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Expression;

   function Get_Psl_Boolean (N : Iir) return PSL_Node is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Psl_Boolean (Get_Kind (N)),
                     "no field Psl_Boolean");
      return Iir_To_PSL_Node (Get_Field1 (N));
   end Get_Psl_Boolean;

   procedure Set_Psl_Boolean (N : Iir; Bool : PSL_Node) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Psl_Boolean (Get_Kind (N)),
                     "no field Psl_Boolean");
      Set_Field1 (N, PSL_Node_To_Iir (Bool));
   end Set_Psl_Boolean;

   function Get_PSL_Clock (N : Iir) return PSL_Node is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock (Get_Kind (N)),
                     "no field PSL_Clock");
      return Iir_To_PSL_Node (Get_Field7 (N));
   end Get_PSL_Clock;

   procedure Set_PSL_Clock (N : Iir; Clock : PSL_Node) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock (Get_Kind (N)),
                     "no field PSL_Clock");
      Set_Field7 (N, PSL_Node_To_Iir (Clock));
   end Set_PSL_Clock;

   function Get_PSL_NFA (N : Iir) return PSL_NFA is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_NFA (Get_Kind (N)),
                     "no field PSL_NFA");
      return Iir_To_PSL_NFA (Get_Field8 (N));
   end Get_PSL_NFA;

   procedure Set_PSL_NFA (N : Iir; Fa : PSL_NFA) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_NFA (Get_Kind (N)),
                     "no field PSL_NFA");
      Set_Field8 (N, PSL_NFA_To_Iir (Fa));
   end Set_PSL_NFA;

   function Get_PSL_Nbr_States (N : Iir) return Int32 is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Nbr_States (Get_Kind (N)),
                     "no field PSL_Nbr_States");
      return Iir_To_Int32 (Get_Field9 (N));
   end Get_PSL_Nbr_States;

   procedure Set_PSL_Nbr_States (N : Iir; Nbr : Int32) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Nbr_States (Get_Kind (N)),
                     "no field PSL_Nbr_States");
      Set_Field9 (N, Int32_To_Iir (Nbr));
   end Set_PSL_Nbr_States;

   function Get_PSL_Clock_Sensitivity (N : Iir) return Iir_List is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock_Sensitivity (Get_Kind (N)),
                     "no field PSL_Clock_Sensitivity");
      return Iir_To_Iir_List (Get_Field10 (N));
   end Get_PSL_Clock_Sensitivity;

   procedure Set_PSL_Clock_Sensitivity (N : Iir; List : Iir_List) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock_Sensitivity (Get_Kind (N)),
                     "no field PSL_Clock_Sensitivity");
      Set_Field10 (N, Iir_List_To_Iir (List));
   end Set_PSL_Clock_Sensitivity;

   function Get_PSL_EOS_Flag (N : Iir) return Boolean is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_EOS_Flag (Get_Kind (N)),
                     "no field PSL_EOS_Flag");
      return Get_Flag1 (N);
   end Get_PSL_EOS_Flag;

   procedure Set_PSL_EOS_Flag (N : Iir; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_EOS_Flag (Get_Kind (N)),
                     "no field PSL_EOS_Flag");
      Set_Flag1 (N, Flag);
   end Set_PSL_EOS_Flag;

end Iirs;
