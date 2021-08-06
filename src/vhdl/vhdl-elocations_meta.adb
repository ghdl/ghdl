--  Meta description of Elocations.
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

with Vhdl.Elocations; use Vhdl.Elocations;

package body Vhdl.Elocations_Meta is
   function Get_Field_Image (F : Fields_Enum) return String is
   begin
      case F is
         when Field_Start_Location =>
            return "start_location";
         when Field_Right_Paren_Location =>
            return "right_paren_location";
         when Field_End_Location =>
            return "end_location";
         when Field_Is_Location =>
            return "is_location";
         when Field_Begin_Location =>
            return "begin_location";
         when Field_Then_Location =>
            return "then_location";
         when Field_Use_Location =>
            return "use_location";
         when Field_Loop_Location =>
            return "loop_location";
         when Field_Generate_Location =>
            return "generate_location";
         when Field_Generic_Location =>
            return "generic_location";
         when Field_Port_Location =>
            return "port_location";
         when Field_Generic_Map_Location =>
            return "generic_map_location";
         when Field_Port_Map_Location =>
            return "port_map_location";
         when Field_Arrow_Location =>
            return "arrow_location";
         when Field_Colon_Location =>
            return "colon_location";
         when Field_Assign_Location =>
            return "assign_location";
      end case;
   end Get_Field_Image;

   type Field_Type is (Type_Location_Type);

   function Fields_Type (F : Fields_Enum) return Field_Type
   is
      pragma Unreferenced (F);
   begin
      return Type_Location_Type;
   end Fields_Type;

   pragma Warnings (Off, """others"" choice is redundant");

   function Get_Location_Type
      (N : Iir; F : Fields_Enum) return Location_Type is
   begin
      pragma Assert (Fields_Type (F) = Type_Location_Type);
      case F is
         when Field_Start_Location =>
            return Get_Start_Location (N);
         when Field_Right_Paren_Location =>
            return Get_Right_Paren_Location (N);
         when Field_End_Location =>
            return Get_End_Location (N);
         when Field_Is_Location =>
            return Get_Is_Location (N);
         when Field_Begin_Location =>
            return Get_Begin_Location (N);
         when Field_Then_Location =>
            return Get_Then_Location (N);
         when Field_Use_Location =>
            return Get_Use_Location (N);
         when Field_Loop_Location =>
            return Get_Loop_Location (N);
         when Field_Generate_Location =>
            return Get_Generate_Location (N);
         when Field_Generic_Location =>
            return Get_Generic_Location (N);
         when Field_Port_Location =>
            return Get_Port_Location (N);
         when Field_Generic_Map_Location =>
            return Get_Generic_Map_Location (N);
         when Field_Port_Map_Location =>
            return Get_Port_Map_Location (N);
         when Field_Arrow_Location =>
            return Get_Arrow_Location (N);
         when Field_Colon_Location =>
            return Get_Colon_Location (N);
         when Field_Assign_Location =>
            return Get_Assign_Location (N);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Location_Type;

   procedure Set_Location_Type
      (N : Iir; F : Fields_Enum; V: Location_Type) is
   begin
      pragma Assert (Fields_Type (F) = Type_Location_Type);
      case F is
         when Field_Start_Location =>
            Set_Start_Location (N, V);
         when Field_Right_Paren_Location =>
            Set_Right_Paren_Location (N, V);
         when Field_End_Location =>
            Set_End_Location (N, V);
         when Field_Is_Location =>
            Set_Is_Location (N, V);
         when Field_Begin_Location =>
            Set_Begin_Location (N, V);
         when Field_Then_Location =>
            Set_Then_Location (N, V);
         when Field_Use_Location =>
            Set_Use_Location (N, V);
         when Field_Loop_Location =>
            Set_Loop_Location (N, V);
         when Field_Generate_Location =>
            Set_Generate_Location (N, V);
         when Field_Generic_Location =>
            Set_Generic_Location (N, V);
         when Field_Port_Location =>
            Set_Port_Location (N, V);
         when Field_Generic_Map_Location =>
            Set_Generic_Map_Location (N, V);
         when Field_Port_Map_Location =>
            Set_Port_Map_Location (N, V);
         when Field_Arrow_Location =>
            Set_Arrow_Location (N, V);
         when Field_Colon_Location =>
            Set_Colon_Location (N, V);
         when Field_Assign_Location =>
            Set_Assign_Location (N, V);
         when others =>
            raise Internal_Error;
      end case;
   end Set_Location_Type;

   function Has_Start_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Library_Clause
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Start_Location;

   function Has_Right_Paren_Location (K : Iir_Kind) return Boolean is
   begin
      return K = Iir_Kind_Parenthesis_Expression;
   end Has_Right_Paren_Location;

   function Has_End_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_End_Location;

   function Has_Is_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Is_Location;

   function Has_Begin_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Simultaneous_Procedural_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Begin_Location;

   function Has_Then_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Then_Location;

   function Has_Use_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Use_Location;

   function Has_Loop_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Loop_Location;

   function Has_Generate_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_If_Generate_Else_Clause =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generate_Location;

   function Has_Generic_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Header
           | Iir_Kind_Component_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generic_Location;

   function Has_Port_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Component_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Port_Location;

   function Has_Generic_Map_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Package_Header
           | Iir_Kind_Component_Instantiation_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Generic_Map_Location;

   function Has_Port_Map_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Block_Header
           | Iir_Kind_Component_Instantiation_Statement =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Port_Map_Location;

   function Has_Arrow_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Arrow_Location;

   function Has_Colon_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Colon_Location;

   function Has_Assign_Location (K : Iir_Kind) return Boolean is
   begin
      case K is
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Has_Assign_Location;


   pragma Warnings (On, """others"" choice is redundant");
end Vhdl.Elocations_Meta;
